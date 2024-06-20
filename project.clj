(defproject com.rpl/proxy-plus "0.0.12-SNAPSHOT"
  :description "A faster and more usable replacement for Clojure's proxy."
  :test-paths ["test/clj"]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.rpl/rama-shaded-asm "4.2"]]

  :repositories
  {"nexus-releases"
   {:url
    "https://nexus.redplanetlabs.com/repository/maven-public-releases"}}

  :profiles {:dev {:java-source-paths ["test/java"]}
             :bench {:dependencies [[criterium "0.4.5"]]}
             }
  )
