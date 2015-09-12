(ns wat.core
  (:require
   [ring.adapter.jetty :as jetty]
   [ring.util.response :refer :all]
   [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.session.cookie :refer [cookie-store]]
   [compojure.core :refer :all]
   [compojure.route :as route]))

(use '[wat.database :as db])
(use '[wat.handlers :as handlers])

(defroutes app-routes
  (GET "/" [] handlers/workspace-handler)
  ;; Authenticate
  (GET "/login" [] (redirect "login.html"))
  (GET "/register" [] (redirect "register.html"))
  (POST "/login" [username password] (handlers/login-handler username password))
  (POST "/register" [username password token] (handlers/registration-handler username password token))
  (ANY "/logout" [] handlers/logout-handler)
  ;; User
  (GET "/workspace" [] handlers/workspace-handler)
  ;; Admin
  (route/not-found "<center><br><h1>Nothing here, go and do something useful.</h1></center>"))

(def app
  (-> #'app-routes
      (wrap-session {:store (cookie-store {:key "ljvoow43kfk34pkf"})})
      (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))))

(defonce server (jetty/run-jetty #'app {:port 3030 :join? false}))

 
