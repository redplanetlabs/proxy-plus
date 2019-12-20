(ns com.rpl.proxy-plus
  (:use [com.rpl.specter])
  (:require [com.rpl [asm :as asm]])
  (:import [java.util Map]
           [clojure.lang IFn]
           [java.lang.reflect Modifier Constructor Method])
  )

(defmacro dofor-indexed [[b aseq] & body]
  `(->> ~aseq
        (map-indexed (fn [~@b] ~@body))
        doall))

(defn- partition-when
  "Collects values for a partition until f returns true.
    (partition-when keyword? [:a 1 2 3 :b :c 4 5] => [[:a 1 2 3] [:b] [:c 4 5]])"
  [f coll]
  (let [[all curr] (reduce
                      (fn [[all curr] elem]
                        (if (f elem)
                          [(if (some? curr) (conj all curr) all) [elem]]
                          [all (conj (if curr curr []) elem)]
                          ))
                      [[] nil]
                      coll
                      )]
    (if (empty? curr) all (conj all curr))
    ))

(defmacro dofor [& body]
  `(doall (for ~@body)))

(defn- resolve! [sym]
  (if-let [ret (resolve sym)]
    ret
    (throw (ex-info "Could not resolve symbol" {:sym sym}))
    ))

(defn- get-super-and-interfaces [bases]
  (if (or (empty? bases) (.isInterface ^Class (first bases)))
    [Object bases]
    [(first bases) (next bases)]))

(defn- get-protected-methods [^Class klass]
  (if-not klass
    []
    (concat
      (get-protected-methods (.getSuperclass klass))
      (filter
        (fn [^Method m] (Modifier/isProtected (.getModifiers m)))
        (.getDeclaredMethods klass)
        ))))

(defn- find-matching-method [^Class klass name num-params]
  (let [matching (filter
                    (fn [^Method m]
                      (and (= (.getName m) name)
                           (= num-params (-> m .getParameterTypes count))
                           ))
                    (concat
                      (get-protected-methods klass)
                      (.getMethods klass)))]
    (cond
      (> (count matching) 1)
      (throw
        (ex-info
          "Too many matching methods"
          {:base klass :name name :methods (seq matching)}))

      (= (count matching) 0)
      (throw (ex-info "No matching methods" {:base klass :name name}))

      :else
      (first matching)
      )))


(defn- box-arg [ga klass]
  (let [[klass meth]
        (cond
          (identical? klass Boolean/TYPE)
          [Boolean (asm/desc->method "Boolean valueOf(boolean)")]

          (identical? klass Byte/TYPE)
          [Byte (asm/desc->method "Byte valueOf(byte)")]

          (identical? klass Short/TYPE)
          [Short (asm/desc->method "Short valueOf(short)")]

          (identical? klass Integer/TYPE)
          [Integer (asm/desc->method "Integer valueOf(int)")]

          (identical? klass Long/TYPE)
          [Long (asm/desc->method "Long valueOf(long)")]

          (identical? klass Float/TYPE)
          [Float (asm/desc->method "Float valueOf(float)")]

          (identical? klass Double/TYPE)
          [Double (asm/desc->method "Double valueOf(double)")]

          (identical? klass Character/TYPE)
          [Character (asm/desc->method "Character valueOf(char)")]

          :else
          (throw (ex-info "Unexpected primitive type" {:class klass}))
          )]
    (asm/invoke-static
      ga
      klass
      meth
      )))

(defn- unbox-arg [ga klass]
  (let [[klass meth]
        (cond
          (identical? klass Boolean/TYPE)
          [Boolean (asm/desc->method "boolean booleanValue()")]

          (identical? klass Byte/TYPE)
          [Number (asm/desc->method "byte byteValue()")]

          (identical? klass Short/TYPE)
          [Number (asm/desc->method "short shortValue()")]

          (identical? klass Integer/TYPE)
          [Number (asm/desc->method "int intValue()")]

          (identical? klass Long/TYPE)
          [Number (asm/desc->method "long longValue()")]

          (identical? klass Float/TYPE)
          [Number (asm/desc->method "float floatValue()")]

          (identical? klass Double/TYPE)
          [Number (asm/desc->method "double doubleValue()")]

          (identical? klass Character/TYPE)
          [Character (asm/desc->method "char charValue()")]

          :else
          (throw (ex-info "Unexpected primitive type" {:class klass}))
          )]
    (asm/check-cast ga klass)
    (asm/invoke-virtual
      ga
      klass
      meth
      )))

(defmacro proxy+ [super-args & impls]
  (let [decls (dofor [[base-sym & overrides] (partition-when symbol? impls)]
                {:base
                 (resolve! base-sym)

                 :override-info
                 (dofor [[name params & body] overrides]
                   (let [sname (str name)]
                     {:name sname
                      :fn `(fn ~params ~@body)
                      :field (munge (str (gensym sname)))
                      :num-params (count params)
                      }))
                 })
        [^Class super interfaces] (get-super-and-interfaces (mapv :base decls))

        class-name (.replace (str *ns* "." (gensym "proxy_plus")) \- \_)

        set-fns-meth (asm/get-method asm/TVOID-TYPE "___setFns" [Map])
        map-get-meth (asm/get-method Object "get" [Object])

        this-type (asm/class-name->type-descriptor class-name)

        cw (asm/class-writer-auto)
        ]
    (apply asm/visit
      cw
      (asm/class-name->internal-name class-name)
      (asm/type-internal-name super)
      (mapv asm/type-internal-name interfaces))
    (doseq [f (select [ALL :override-info ALL :field] decls)]
      (asm/visit-field cw f (asm/type-descriptor IFn)))

    (let [ga (asm/generator-adapter set-fns-meth cw)]
      (doseq [f (select [ALL :override-info ALL :field] decls)]
        (asm/load-this ga)
        (asm/load-arg ga 0)
        (asm/push-string ga f)
        (asm/invoke-interface ga Map map-get-meth)
        (asm/check-cast ga IFn)
        (asm/put-field
          ga
          this-type
          f
          IFn)
        )
      (asm/return-value ga)
      (asm/end-method ga)
      )

    (doseq [^Constructor c (.getDeclaredConstructors super)]
      (when-not (Modifier/isPrivate (.getModifiers c))
        (let [ptypes (.getParameterTypes c)
              ga (asm/generator-adapter
                   (asm/get-method
                     asm/TVOID-TYPE
                     "<init>"
                     (cons Map ptypes))
                   cw)]
          (asm/load-this ga)
          (doseq [i (range (count ptypes))]
            (asm/load-arg ga (inc i))
            )
          (asm/invoke-constructor
            ga
            super
            (asm/get-method
              asm/TVOID-TYPE
              "<init>"
              ptypes)
            )
          (asm/load-this ga)
          (asm/load-arg ga 0)
          (asm/invoke-virtual
            ga
            this-type
            set-fns-meth
            )
          (asm/return-value ga)
          (asm/end-method ga)
          )))

    (doseq [{:keys [base override-info]} decls
            {:keys [name field num-params]} override-info]
      (let [^Method jmeth (find-matching-method
                            base
                            name
                            ;; dec because "this" not relevant for java
                            (dec num-params))
            rtype (.getReturnType jmeth)
            ptypes (.getParameterTypes jmeth)
            ga (asm/generator-adapter
                 (asm/get-method
                   (asm/asm-type rtype)
                   name
                   ptypes)
                 cw)]
        (asm/load-this ga)
        (asm/get-field ga this-type field IFn)
        (asm/load-this ga)
        (dofor-indexed [[i ^Class p] ptypes]
          (do
            (asm/load-arg ga i)
            (if (.isPrimitive p)
              (box-arg ga p))
            ))
        (asm/invoke-interface
          ga
          IFn
          (asm/get-method
            Object
            "invoke"
            ;; one more for "this"
            (repeat (-> ptypes count inc) Object)
            ))
        (if (not= Void/TYPE rtype)
          (if (.isPrimitive rtype)
            (unbox-arg ga rtype)
            (asm/check-cast ga rtype)
            ))
        (asm/return-value ga)
        (asm/end-method ga)
        ))

    (asm/define-class
      (asm/dynamic-class-loader)
      class-name
      cw)
    `(do
       (import (quote ~(symbol class-name)))
       (let [fn-map# (hash-map
                      ~@(select [ALL
                                 :override-info
                                 ALL
                                 (multi-path :field :fn)]
                           decls))]
         (new ~(symbol class-name) fn-map# ~@super-args)
         ))
  ))
