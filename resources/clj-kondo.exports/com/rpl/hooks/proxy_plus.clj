(ns hooks.proxy-plus
  (:require [clj-kondo.hooks-api :as api]))

(defn proxy+
  [{:keys [node]}]
  (let [{:keys [children]}
        node

        ;; Consume `proxy+` token
        children (rest children)

        ;; Consume ClassNameSymbol. Not used for anything
        name (if (-> children first api/token-node?)
                (first children)
                nil)
        children (if name (rest children) children)

        ;; Consume super-args. We require:
        superclass-args (first children)
        impl-body (rest children)

        new-superclass
        (api/list-node
          ;; Check with (new Baseclass arg1 arg2 ...)
          (list*
            (api/token-node 'new)
            (api/token-node 'java.lang.Object)
            (:children superclass-args)))

        reify-node
        (api/list-node
          (list*
            (api/token-node 'reify)
            impl-body))

        new-node (api/list-node
                   (list
                     (api/token-node 'do)
                     new-superclass
                     reify-node))]
    ;; For linting purposes we macroexpand the proxy+ call to:
    ;;
    ;; (do
    ;;   (new java.lang.Object ~@super-args)
    ;;   (reify ~@impl-body))
    ;;
    ;; generated code can be viewed with:
    ;; (prn (api/sexpr new-node))
    ;;
    ;; # Notes
    ;;
    ;; We also check that super-args is a vector.
    ;;
    ;; We assume java.lang.Object as the superclass, at the time of writing
    ;; linters don't check the arguments passed to java constructors for
    ;; correctness and it is not easy to tell if there is a real base class
    ;; or if we are proxying an interface.

    (when-not (api/vector-node? superclass-args)
      (api/reg-finding! (assoc (-> children first meta)
                          :message (format "Superclass arguments '%s' should be a vector" superclass-args)
                          :type :proxy-plus/superclass-args)))

    {:node new-node}))
