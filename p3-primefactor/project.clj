(defproject p3-primefactor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"], [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot p3-primefactor.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

  :repl-options {
             ;; If nREPL takes too long to load it may timeout,
             ;; increase this to wait longer before timing out.
             ;; Defaults to 30000 (30 seconds)
             :timeout 120000
             }

  :jvm-opts ["-Xmx2g" "-server"] 
