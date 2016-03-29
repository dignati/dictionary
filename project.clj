(defproject dignati/dictionary "0.1.0"
  :description "Simple dictionary matching"
  :url "https://github.com/dignati/dictionary"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
