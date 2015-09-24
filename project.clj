(defproject wat "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.3"]
                 ;[ring/ring-core "1.3.2"]
                 [ring/ring-jetty-adapter "1.3.2"]
                 [ring/ring-defaults "0.1.2"]
                 [compojure "1.4.0"]
                 [buddy "0.6.2"]
                 [buddy/buddy-hashers "0.6.0"]
                 [bouncer "0.3.3"]
                 [com.datomic/datomic-free "0.9.5186" :exclusions [joda-time]]
                 [cheshire "5.5.0"]]
  :main wat.core)
