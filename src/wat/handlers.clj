(ns wat.handlers
  (:require [buddy.hashers :as hash]
            [ring.util.response :refer :all]))

(use '[wat.database :as db])
(use '[wat.util :as util :refer [aif]])

(defn authenticated? [req role]
  (let [user (user-exist? (:user (:session req)))]
    (if (and user (<= (:role user) role))
      user)))

;; ===============================================
;; User handlers
;; ===============================================

(defn workspace-handler [req] ;; TODO: if user already got chunk - load it to workspace instantly
  (aif (authenticated? req 1)
       (let [user it]
         {:status 200
          :headers {"Content-Type" "text/html"}
          :body (str "<center>Welcome to workspace,<b>" (:name user) "</b>.</center>")})
       (redirect "/login")))

(defn get-text-to-translate [req]
  (aif (authenticated? req 1)
       (let [user it
             pname (:project-name req)
             size (:number-of-words req)
             [chunk chunk-size] (db/get-project-chunk-to-translate pname size user)]
         ;; Some kind of response         
     )
   (redirect "/login")))

(defn get-text-to-redact [req]
  (aif (authenticated? req 1)
       (let [user it
             pname (:project-name req)
             size (:number-of-words req)
             [chunk chunk-size] (db/get-project-chunk-to-redact pname size user)]
         ;; Some kind of response      
     )
   (redirect "/login")))

;; ===============================================
;; Admin handlers
;; ===============================================

(defn dashboard-handler [req]
  (aif (authenticated? req 0)
       (let [user it]
         {:status 200
          :headers {"Content-Type" "text/html"}
          :body (str "<center>Welcome to workspace,<b>" (:name user) "</b>.</center>")})
       {:status  403
        :headers {"Content-Type" "text/html"}
        :body    "<center>Only for admins, buddy.</center>"}))

;; ===============================================

(defn logout-handler [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<center>Farewell!</center>"
   :session nil})

(defn login-handler [username password]
  (aif (db/user-exist? username)
       (if (hash/check password (:password it))
         (do
           (assoc (redirect "/workspace")
                  :session {:user (:name it)}))
         {:status  403
          :headers {"Content-Type" "text/html"}
          :body    "<center>Wrong password.</center>"})
       {:status  403
        :headers {"Content-Type" "text/html"}
        :body    "<center>No such user.</center>"}))

(defn registration-handler [username password token]
  (if (db/user-exist? username)
    {:status  403
     :headers {"Content-Type" "text/html"}
     :body    (str "<center>Username <b>" username "</b> is taken, try again.</center>")}
    (if (= "mvks-fdnv-8r30-49ik" token) ;; TODO: make adequate tokens or smth
      (do
        (db/add-user username (hash/encrypt password) 1)
        (login-handler username password))
      {:status  403
       :headers {"Content-Type" "text/html"}
       :body    "<center>Invalid token, try again.</center>"})))
