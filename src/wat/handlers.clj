(ns wat.handlers
  (:require [buddy.hashers :as hash]
            [ring.util.response :refer :all]))

(use '[wat.database :as db])
(use '[wat.util :refer :all])

(defn authenticated? [req role]
  (let [user (user-exist? (:user (:session req)))]
    (if (and user (<= (:role user) role))
      user)))

(defn workspace-handler [req]
  (aif (authenticated? req 1)
   (let [user it]
     {:status 200
      :headers {"Content-Type" "text/html"}
      :body (str "<center>Welcome to workspace,<b>" (:name user) "</b>.</center>")})
   (redirect "/login")))

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

#_(defn handler [request]
  {:status 200
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
                        ) })
