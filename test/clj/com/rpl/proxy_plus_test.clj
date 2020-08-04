(ns com.rpl.proxy-plus-test
  (:use [clojure.test]
        [com.rpl.proxy-plus])
  (:import [com.rpl TestBaseClass TestBaseClass2 InterfaceAB]))

(deftest nothing-test
  (let [o (proxy+ [])
        o2 (proxy+ [] Object)]
    (is (instance? Object o))
    (is (not= (class o) Object))
    (is (instance? Object o2))
    (is (not= (class o2) Object))
    ))

(deftest base-class-test
  (let [o (proxy+ ["a"]
            TestBaseClass
            (foo [this s] (count s))
            (doSomething [this c l s d]
              (str c l s d))
              )]
    (is (instance? TestBaseClass o))
    (is (= "a 11" (.getVal o)))
    (is (= "a1024.5" (.doSomething o \a 10 2 4.5)))
    (is (thrown? Exception (.foo2 o)))
    (is (= 3 (.foo o "aaa")))
    (is (= 5 (.foo o "edcba")))
    ))

(definterface I1
  (^String foo [^Long l ^long l2])
  (^void bar [^Object o])
  )

(definterface I2
  (^Object foo [])
  (^void car [])
  )

(definterface I3
  (^String foo [^Long l ^long l2])
  )

(deftest only-interfaces-test
  (let [v (volatile! 0)
        o (proxy+ []
            I1
            (foo [this l l2] (str (+ l l2)))
            (bar [this o] (vswap! v inc))
            I2
            (foo [this] 99)
            (car [this] (vswap! v #(* 10 %)))
            )]
    (is (instance? I1 o))
    (is (instance? I2 o))
    (is (= "13" (.foo o 10 3)))
    (is (= 0 @v))
    (.bar o nil)
    (is (= 1 @v))
    (is (= 99 (.foo o)))
    (.car o)
    (is (= 10 @v))
    ))

(deftest superclass-and-interface-test
  (let [v (volatile! 0)
        o (proxy+ []
            TestBaseClass
            (foo2 [this] (vswap! v inc))
            I1
            (bar [this o] (vreset! v o) (str o "!"))
            )]
    (is (instance? TestBaseClass o))
    (is (instance? I1 o))
    (is (not (instance? I2 o)))
    (is (= 0 @v))
    (.foo2 o)
    (is (= 1 @v))
    (is (nil? (.bar o "a")))
    (is (= "a" @v))
    ))

(deftest super-args-test
  (let [o (proxy+ []
            TestBaseClass
            )
        o2 (proxy+ ["bbb"]
             TestBaseClass
             )
        o3 (proxy+ ["z" -1]
             TestBaseClass
             )]
    (is (= "s 10" (.getVal o)))
    (is (= "bbb 11" (.getVal o2)))
    (is (= "z -1" (.getVal o3)))
    ))

(deftest overlapping-interfaces-test
  (let [o (proxy+ []
            I1
            (foo [this l1 l2] (str (- l1 l2)))
            I3
            )
        o2 ((fn [x] x) o)
        ]
    (is (instance? I1 o))
    (is (instance? I3 o))
    (is (= "-7" (.foo ^I1 o2 3 10)))
    (is (= "-7" (.foo ^I3 o2 3 10)))
    ))

(definterface I4
  (^Object foo [])
  (^Object foo [arg]))

(deftest multiple-arity-same-method-name-test
  (let [o (proxy+ []
            I4
            (foo [this] :a)
            (foo [this arg] (str arg "?"))
            )]
    (is (= :a (.foo o)))
    (is (= "hello?" (.foo o "hello")))
    ))

(definterface I5
  (^String foo [^String str])
  (^String foo [^Integer n])
  )

(deftest same-method-name-different-param-types
   (let [o1 (proxy+ []
              I5
              (foo [this ^String strArg] "other")
              (foo [this ^Integer integerArg] "first")
              )
         sb (new StringBuilder)
         o2 (proxy+ []
              java.io.Writer
              (write [this ^String str offset len] (do (.append sb "String overload") nil))
              (write [this ^chars cbuf offset len] (do (.append sb "char[] overload") nil))
              )
         o3 (proxy+ []
              InterfaceAB
              (bar [this ^Integer x ^Integer y ^long z] "barA") ; from InterfaceA
              (bar [this ^Integer x ^Integer y ^Integer z] "barB") ; from InterfaceB
              (baz [this] 0)
              (baz [this ^long p] 1)
              (baz [this ^double p] 2)
              (baz [this ^ints p] 3)
              (baz [this ^chars p] 4)
              )]
      (is (= "other" (.foo o1 "bar")))
      (is (= "first" (.foo o1 (int 3))))
      ;; overloads of concrete Java Writer class
      (is (= nil (.write o2 "hello" 0 5)))
      (is (= "String overload" (.toString sb)))
      (.setLength sb 0)
      (is (= nil (.write o2 (char-array [\f \o \o]) 1 1)))
      (is (= "char[] overload" (.toString sb)))
      ;; same method and arity, but from two different super interfaces
      (is (= "barA" (.bar o3 (int 1) (int 1) (long 2)))) ; from InterfaceA
      (is (= "barB" (.bar o3 (int 1) (int 1) (int 2)))) ; from InterfaceB
      (is (= 0 (.baz o3))) ; 0-arity overload
      (is (= 1 (.baz o3 (long 1)))) ; 1-arity (long) overload
      (is (= 2 (.baz o3 (double 1.0)))) ; 1-arity (double) overload
      (is (= 3 (.baz o3 (int-array [1 2 3])))) ; 1-arity (int[]) overload
      (is (= 4 (.baz o3 (char-array "zyx")))) ; 1-arity (char[]) overload
    ))

(deftest inherited-test
  (let [o (proxy+ []
            TestBaseClass2
            (foo [this arg] (* 2 (count arg)))
            (car [this] "inherited")
            )]
    (is (= 12 (.foo o "biubiu")))
    (is (= "inherited" (.car o)))
    ))
