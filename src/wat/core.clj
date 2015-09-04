(ns wat.core
  (:require
   [ring.adapter.jetty :as jetty]
   [ring.util.response :refer :all]
   [ring.middleware.resource :refer :all]
   [compojure.core :refer :all]
   [compojure.route :as route]))

(use '[wat.database :as db])

(defn handler [request]
  (let [url (:uri request)]
    (cond
     (= "/lol" url)
       (redirect "https://www.youtube.com/watch?v=dQw4w9WgXcQ")
     (= "/dummy" url) {:status 200
            :headers {"Content-Type" "text/html"}
            :body "Welcome on dummy page!"}
     :else {:status 200
            :headers {"Content-Type" "text/html"}
            :body (str "<script>
                        function ahtung(){
                            alert (\"" (db/get-project-list) "\")
                        }
                        </script>
                        <center>
                          <a href=\"/dummy\"> <img src=\"favicon.ico\" onclick=\"ahtung()\" > </a> <br>
                        " (db/get-dummy-info) " 
                        </center>
                        "
                        ) }
     )))

(def app
  (-> #'handler
      (wrap-resource "public")))

(defonce server (jetty/run-jetty #'app {:port 3030 :join? false}))
