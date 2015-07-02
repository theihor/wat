(ns wat.core)

(use 'ring.adapter.jetty)
(use 'ring.util.response)
(use 'ring.middleware.resource)

(defn handler [request]
  (let [url (:uri request)]
    (println url)
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
                            alert (\"Shit happened!\")
                        }
                        </script>
                        <center>
                          <a href=\"/dummy\"> <img src=\"favicon.ico\" onclick=\"ahtung()\" > </a> <br>
                        Click me! 
                        </center>
                        "
                        ) }
     )))

(def app
  (-> #'handler
   (wrap-resource "public")))

(defonce server (ring.adapter.jetty/run-jetty #'app {:port 3030 :join? false}))
