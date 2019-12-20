(ns com.rpl.proxy-plus.benchmarks
  (:use [com.rpl.specter]
        [com.rpl.proxy-plus])
  (:require [criterium.core :as bench]))

(definterface Foo
  (^Long foo [^long arg]))

(def p1 (proxy [Foo] [] (foo [arg] (inc arg))))
(def p2 (proxy+ [] Foo (foo [this arg] (inc arg))))


(println "proxy one override dispatch performance (10,000 iterations):")
(println "------------------------------------------------------------")
(bench/bench
  (dotimes [i 10000]
    (.foo ^Foo p1 i)))

(println "\n")
(println "proxy+ one override dispatch performance (10,000 iterations):")
(println "-------------------------------------------------------------")
(bench/bench
  (dotimes [i 10000]
    (.foo ^Foo p2 i)))

(definterface Foo2
  (m1 [arg])
  (m2 [arg])
  (m3 [arg])
  (m4 [arg])
  (m5 [arg])
  (m6 [arg])
  (m7 [arg])
  (m8 [arg])
  (m9 [arg])
  (m10 [arg])
  )

(def p11
  (proxy [Foo2] []
    (m1 [arg] 1)
    (m2 [arg] 2)
    (m3 [arg] 3)
    (m4 [arg] 4)
    (m5 [arg] 5)
    (m6 [arg] 6)
    (m7 [arg] 7)
    (m8 [arg] 8)
    (m9 [arg] 9)
    (m10 [arg] 10)
    ))

(def p22
  (proxy+ [] Foo2
    (m1 [this arg] 1)
    (m2 [this arg] 2)
    (m3 [this arg] 3)
    (m4 [this arg] 4)
    (m5 [this arg] 5)
    (m6 [this arg] 6)
    (m7 [this arg] 7)
    (m8 [this arg] 8)
    (m9 [this arg] 9)
    (m10 [this arg] 10)
    ))

(println "\n\n")
(println "proxy ten overrides dispatch performance (10,000 iterations):")
(println "-------------------------------------------------------------")
(bench/bench
  (dotimes [i 10000]
    (.m1 ^Foo2 p11 i)
    (.m2 ^Foo2 p11 i)
    (.m3 ^Foo2 p11 i)
    (.m4 ^Foo2 p11 i)
    (.m5 ^Foo2 p11 i)
    (.m6 ^Foo2 p11 i)
    (.m7 ^Foo2 p11 i)
    (.m8 ^Foo2 p11 i)
    (.m9 ^Foo2 p11 i)
    (.m10 ^Foo2 p11 i)
    ))

(println "\n")
(println "proxy+ ten overrides dispatch performance (10,000 iterations):")
(println "--------------------------------------------------------------")
(bench/bench
  (dotimes [i 10000]
    (.m1 ^Foo2 p22 i)
    (.m2 ^Foo2 p22 i)
    (.m3 ^Foo2 p22 i)
    (.m4 ^Foo2 p22 i)
    (.m5 ^Foo2 p22 i)
    (.m6 ^Foo2 p22 i)
    (.m7 ^Foo2 p22 i)
    (.m8 ^Foo2 p22 i)
    (.m9 ^Foo2 p22 i)
    (.m10 ^Foo2 p22 i)
    ))
