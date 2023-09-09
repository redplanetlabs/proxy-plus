(ns com.rpl.proxy-plus
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

(defn- tag-matches [tag param-class]
  (or
   (nil? tag)
   (let [compare-class
     (case tag
       ;; map primitive type hints to their equivalent classes
       byte Byte/TYPE
       short Short/TYPE
       int Integer/TYPE
       long Long/TYPE
       float Float/TYPE
       double Double/TYPE
       boolean Boolean/TYPE
       char Character/TYPE
       bytes (type (byte-array []))
       shorts (type (short-array []))
       ints (type (int-array []))
       longs (type (long-array []))
       floats (type (float-array []))
       doubles (type (double-array []))
       booleans (type (boolean-array []))
       chars (type (char-array []))
       (resolve tag) ; default, just return the resolved tag (which should be a Class)
       )]
     (when (nil? compare-class)
       (throw (ex-info (str "Type hint "
                            tag
                            " resolved to nil. Make sure that type is imported!")
                       {})))
     (.isAssignableFrom param-class compare-class))
  )
)

(defn- type-hints-match [l1 l2]
  (and (= (count l1) (count l2))
  (let [[l1h & l1t] l1
        [l2h & l2t] l2]
      (and
        (tag-matches l1h l2h)
        (or (empty? l1t) (type-hints-match l1t l2t))
      ))
))

(defn characterize
  [^Method m]
  (-> {:name (.getName m)
       :params (vec (.getParameterTypes m))
       :returns (.getReturnType m)}))

(defn- find-matching-method [^Class klass method-name all-param-types]
  (let [available-methods (concat
                           (get-protected-methods klass)
                           (.getMethods klass))
        matching (filter
                    (fn [^Method m]
                      (and (= (.getName m) method-name)
                           (type-hints-match (rest all-param-types)
                                             (-> m .getParameterTypes))
                           ))
                    available-methods)]
    (cond
      ;; there was more than one match, but the last one is the one that is
      ;; closest to the base class we specified in the proxy+ decl block, so
      ;; let's just use that one.
      (> (count matching) 1)
      (last matching)

      (= (count matching) 0)
      (throw (ex-info "No matching methods" {:base klass
                                             :name method-name
                                             :params (vec (rest all-param-types))
                                             :methods-available (map characterize available-methods)}))

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

(defn define-proxy-class [proxy-name-sym decls]
  (let [[^Class super interfaces] (get-super-and-interfaces (mapv :base decls))

        class-name (.replace (str *ns* "." proxy-name-sym) \- \_)
        this-type (asm/class-name->type-descriptor class-name)

        cw (asm/class-writer-auto)]

    ;; class declaration
    (apply asm/visit
      cw
      (asm/class-name->internal-name class-name)
      (asm/type-internal-name super)
      (mapv asm/type-internal-name interfaces))

    ;; generate instance fields for all fn impls. note that these will start
    ;; nil and be set to impls (with their closures!) at instantiation time.
    (doseq [{:keys [field]}
            (->> decls
                 (mapv :override-info)
                 (apply concat))]
      (asm/visit-field cw field (asm/type-descriptor IFn)))

    ;; generate constructors (one per parent class constructor arity])
    (doseq [^Constructor c (.getDeclaredConstructors super)]
      (when-not (Modifier/isPrivate (.getModifiers c))
        (let [ptypes (.getParameterTypes c)
              ga (asm/generator-adapter
                  (asm/get-method
                   asm/TVOID-TYPE
                   "<init>"
                   ptypes)
                  cw)]
          (asm/load-this ga)
          ;; load all the args onto stack
          (doseq [i (range (count ptypes))]
            (asm/load-arg ga i))
          ;; invoke superclass constructor
          (asm/invoke-constructor ga
                                  super
                                  (asm/get-method
                                   asm/TVOID-TYPE
                                   "<init>"
                                   ptypes))
          (asm/return-value ga)
          (asm/end-method ga))))

    ;; generate "real" method impls
    (doseq [{:keys [base override-info]} decls
            {:keys [method-name field all-param-types]} override-info]
      (let [^Method jmeth (find-matching-method base
                                                method-name
                                                all-param-types)
            rtype (.getReturnType jmeth)
            ptypes (.getParameterTypes jmeth)
            ga (asm/generator-adapter
                (asm/get-method
                 (asm/asm-type rtype)
                 method-name
                 ptypes)
                cw)]
        (asm/load-this ga)
        ;; load the fn for the impl of this fn
        (asm/get-field ga this-type field IFn)
        (asm/load-this ga)
        ;; re-load all the arguments so we can call the clj fn impl
        (dofor-indexed [[i ^Class p] ptypes]
                       (asm/load-arg ga i)
                       (if (.isPrimitive p)
                         (box-arg ga p)))
        ;; invoke the clj fn
        (asm/invoke-interface ga
                              IFn
                              (asm/get-method
                               Object
                               "invoke"
                               ;; one more for "this"
                               (repeat (-> ptypes count inc) Object)))
        ;; manage return value if it's not void
        (when (not= Void/TYPE rtype)
          (if (.isPrimitive rtype)
            (unbox-arg ga rtype)
            (asm/check-cast ga rtype)))
        (asm/return-value ga)
        (asm/end-method ga)
        ))

    ;; return the class
    (asm/define-class
      (asm/dynamic-class-loader)
      class-name
      cw)))

(defn strip-unimplemented-hints [args]
  ;; Clojure has arbitrary limits on primative type hints. Adjust :tag in arg
  ;; meta to meet them by stripping out unimplemented hints.
  ;; See Issue #20
  (let [clean-meta (fn [{:keys [tag] :as m}]
                     (cond
                       (nil? tag) m
                       (-> tag resolve class?) m
                       ;; max. 4 args with ^double/^long hints
                       (> (count args) 4) (dissoc m :tag)
                       ;; Primatives other than ^double/^long banned
                       (contains? #{'double 'long} tag) m
                       :else (dissoc m :tag)))]
    (mapv #(vary-meta % clean-meta) args)))

(defmacro proxy+
  "A replacement for clojure.core/proxy. Return an object implementing the class
   and interfaces. The class will be named `ClassNameSymbol` if provided;
   otherwise uses a unique generated name.

   super-args is a (possibly empty) vector of arguments to the superclass
   constructor.

   impl-body specifies the superclass, any interfaces, and their method
   implementations, using the same syntax as clojure.core/proxy.

   The first implementation body also specifies the superclass, if it refers to
   a class; if it is an interface, then the superclass will be Object. All other
   implementation bodies must refer to interfaces.

   Clojure has certain arbitary restrictions on primative hints (^long, ^int,
   etc). If the type signature given to proxy does not allow a legal Clojure
   function to be constructed then the primative type hints will be used to
   match the method being over-ridden but otherwise ignored for the purposes of
   constructing the Clojure function."
  {:arglists '([[super-args] & impl-body]
               [ClassNameSymbol [super-args] & impl-body])}
  [& args]
  (let [[proxy-name-sym super-args impls]
        (if (symbol? (first args))
          [(first args)
           (first (rest args))
           (rest (rest args))]
          [(gensym "proxy_plus")
           (first args)
           (rest args)])

        decls (dofor [[base-sym & overrides] (partition-when symbol? impls)]
                {:base (resolve! base-sym)

                 :override-info
                 (dofor [[method-name params & body] overrides]
                        (let [param-types (mapv (comp :tag meta) params)
                              clean-params (strip-unimplemented-hints params)
                              sname (str method-name)
                              field-name (munge (str (gensym sname)))
                              impl-sym (symbol (str field-name "-impl"))]
                          {:method-name sname
                           :impl-sym impl-sym
                           :impl-form `(fn ~clean-params ~@body)
                           :field field-name
                           :all-param-types param-types
                           }))
                 })
        klass (define-proxy-class proxy-name-sym decls)
        class-name (.getName klass)]

    (.importClass ^clojure.lang.Namespace *ns* klass)
    (let [inst-sym (with-meta (gensym "inst")
                     {:tag (symbol class-name)})
          all-impls (->> decls
                         (mapv :override-info)
                         (apply concat))]
      `(let [;; declare each of the clj fn impls here. crucially, they capture
             ;; their closures at this callsite.
             ~@(->> all-impls
                    (mapv (juxt :impl-sym :impl-form))
                    (reduce concat))
             ;; instantiate actual proxied type. note that none of the clj fn
             ;; impls will be assigned yet!
             ~inst-sym (new ~(symbol class-name) ~@super-args)]
         ;; iterate over all impls and set the impl fields to the bound impl fn
         ;; instances.
         ~@(->> all-impls
                (mapv (fn [{:keys [impl-sym field]}]
                        `(set! (. ~inst-sym ~(symbol field)) ~impl-sym))))
         ;; the instance is now actually usable.
         ~inst-sym
         ))))
