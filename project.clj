(defproject com.rpl/proxy-plus "0.0.10-SNAPSHOT"
  :description "A faster and more usable replacement for Clojure's proxy."
  :java-source-paths ["test/java"]
  :test-paths ["test/clj"]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.rpl/specter "1.1.3"]
                 [org.ow2.asm/asm "6.2"]
                 [org.ow2.asm/asm-commons "6.2"]
                 [org.ow2.asm/asm-util "6.2"]
                 [org.ow2.asm/asm-analysis "6.2"]]

  :profiles {:bench {:dependencies [[criterium "0.4.5"]]}
             }
  )
