(defproject com.rpl/proxy-plus "0.0.9-SNAPSHOT"
  :description "A faster and more usable replacement for Clojure's proxy."
  :java-source-paths ["test/java"]
  :test-paths ["test/clj"]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.rpl/specter "1.1.3"]
                 [org.ow2.asm/asm "5.1"]
                 [org.ow2.asm/asm-commons "5.1"]
                 [org.ow2.asm/asm-util "5.1"]
                 [org.ow2.asm/asm-analysis "5.1"]]

  :profiles {:bench {:dependencies [[criterium "0.4.5"]]}
             }
  )
