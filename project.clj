(defproject bitfinex-api "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-http "3.12.3"]
                 [org.clojure/data.json "2.2.1"]
                 [org.clojure/algo.generic "0.1.3"]
                 [buddy/buddy-core "1.10.1"]
                 [clojure.java-time "0.3.3"]
                 [spec-dict "0.2.1"]]
  :main ^:skip-aot bitfinex-api.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
