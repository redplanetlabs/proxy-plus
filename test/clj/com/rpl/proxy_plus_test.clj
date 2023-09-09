(ns com.rpl.proxy-plus-test
  (:use [clojure.test]
        [com.rpl.proxy-plus])
  (:import [com.rpl TestBaseClass TestBaseClass2
            InterfaceA InterfaceB InterfaceC InterfaceD
            AbstractBaseClass1 AbstractBaseClass2]))

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
    ;; NOTE generates a reflection warning because this isn't a real field / fn!
    (is (thrown? Exception (.foo2 o)))
    (is (= 3 (.foo o "aaa")))
    (is (= 5 (.foo o "edcba")))
    ))

(deftest multiple-levels-of-base-classes-test
  (let [o (proxy+
           []
           AbstractBaseClass2
           (foo [_this ^Integer x ^String y])
           (bar [_this ^Integer x ^Integer y ^long z])
           )])
  )

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
              (write [this ^String str offset len]
                     (.append sb "String overload")
                     nil)
              (write [this ^chars cbuf offset len]
                     (.append sb "char[] overload")
                     nil))
         o3 (proxy+ []
              InterfaceA
              (bar [this ^Integer x ^Integer y ^long z] "barA")

              InterfaceB
              (bar [this ^Integer x ^Integer y ^Integer z] "barB")

              InterfaceC
              (baz [this] 0)
              (baz [this ^long p] 1)
              (baz [this ^double p] 2)
              (baz [this ^ints p] 3)
              (baz [this ^chars p] 4)
              )]
      (is (= "other" (.foo ^I5 o1 "bar")))
      (is (= "first" (.foo ^I5 o1 ^Integer (int 3))))
      ;; overloads of concrete Java Writer class
      (is (= nil (.write o2 "hello" 0 5)))
      (is (= "String overload" (.toString sb)))
      (.setLength sb 0)
      (is (= nil (.write o2 (char-array [\f \o \o]) 1 1)))
      (is (= "char[] overload" (.toString sb)))
      ;; same method and arity, but from two different super interfaces
      ; from InterfaceA
      (is (= "barA" (.bar ^InterfaceA o3 (int 1) (int 1) (long 2))))
      ; from InterfaceB
      (is (= "barB" (.bar ^InterfaceB o3 (int 1) (int 1) (int 2))))
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

(deftest named-proxy-test
  (let [o (proxy+ my-proxy [])]
    (is (= (.getName (class o))
           "com.rpl.proxy_plus_test.my_proxy"))
    ))

(definterface I6
  (^String foo [^java.util.Map m])
  (^String foo [^java.util.HashMap m])
  );

(deftest assignable-from-test
  (let [o (proxy+
           []
           I6
           (foo [_this ^java.util.HashMap m] "woo")
           (foo [_this ^java.util.Map m] "wee")

           )]
    (is (= "wee" (.foo o (java.util.TreeMap.))))
    (is (= "woo" (.foo o (java.util.HashMap.))))
    )
  )

(deftest throws-on-busted-type-hint-test
  (is (thrown? Exception
               ;; eval here so that the test namespace as a whole compiles, even
               ;; though this produces a compile-time error!
               (eval '(proxy+
                       []
                       I6
                       (foo [_this ^SuperDuperMap m] "woo")
                       ))))
  )

(deftest strip-unimplemented-hints-test
  (let [x nil]
    (is (= '[nil nil nil]
           (mapv meta (strip-unimplemented-hints '[x x x]))))
    (is (= '[nil nil {:not-disturbed true}]
           (mapv meta (strip-unimplemented-hints '[x x ^{:not-disturbed true} x]))))
    (is (= '[{:tag java.lang.Object}]
           (mapv meta (strip-unimplemented-hints '[^java.lang.Object x]))))
    (is (= '[{:tag java.lang.Object} {:tag long} nil]
           (mapv meta (strip-unimplemented-hints '[^java.lang.Object x ^long x x]))))
    (is (= '[{:tag java.lang.Object} {:tag long} {}]
           (mapv meta (strip-unimplemented-hints '[^java.lang.Object x ^long x ^bet.this.isnt.loaded x]))))
    (is (= '[{:tag java.lang.Object} {} {} nil nil]
           (mapv meta (strip-unimplemented-hints '[^java.lang.Object x ^long x ^bet.this.isnt.loaded x x x]))))
    (is (= '[{} {} {} {} {}]
           (mapv meta (strip-unimplemented-hints '[^int x ^long x ^double x ^char x ^boolean x]))))
    (is (= '[{} {:tag long} {:tag double} {}]
           (mapv meta (strip-unimplemented-hints '[^int x ^long x ^double x ^char x]))))
    (is (= '[{} {:tag long, :not-disturbed true} {:tag double}]
           (mapv meta (strip-unimplemented-hints '[^int x ^{:tag long :not-disturbed true} x ^double x]))))
    (is (= '[{} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {:tag Object}]
           (mapv meta (strip-unimplemented-hints
                       '[^byte x ^bytes x ^short x ^shorts x ^int x ^ints x ^long x ^longs x
                         ^float x ^floats x ^double x ^doubles x ^boolean x ^booleans x ^char x ^chars x
                         ^void x ^objects x ^Object x]))))))

(deftest hard-primative-signature-test
  (testing "There are complications from Clojure's compiler. It doesn't support
functions with certain primative type hints and only supports 4 args if any
primatives are present. Check such superclass methods can still be overridden.

Issue #20"

    (let [o
          (proxy+ [] TestBaseClass
                  ;; Hard to test, but this is a demo of what an illegal method
                  ;; override does.
                  #_(hardSignature [this ^char b s i l bo st] 11)

                  ;; Note that seeing ^long or ^double in the type signature
                  ;; triggers special behaviour in the Clojure compiler.
                  ;; This code would work without those hints, but the test
                  ;; would not be thorough.
                  (hardSignature [this ^byte b ^short s ^int i ^long l ^boolean bo ^String st] 12)
                  (hardSignature [this ^java.lang.Byte b ^Short s ^Integer i ^Long l ^Boolean bo ^String st] (.intValue 13)))]
      (is (= 12 (.hardSignature o
                               ^byte (.byteValue 1)
                               ^short (.shortValue 1)
                               ^int (.intValue 1)
                               ^long (identity 1)
                               ^boolean (identity true)
                               "Two")))
      (is (= 13 (.hardSignature o
                               ^Byte (.byteValue 1)
                               ^Short (.shortValue 1)
                               ^Integer (.intValue 1)
                               ^Long (identity 1)
                               ^Boolean (identity true)
                                "Two"))))))
