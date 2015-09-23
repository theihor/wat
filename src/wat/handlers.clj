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
          :body (str "<div class=\"translate\">
                      <form method=\"post\" action=\"get-text-to-translate\">
                       <h2>Get text to translate</h2>
                       <p><input type=\"number\" name=\"chunk-size\" required=\"required\" value=\"2000\" ></p>
                       <p><input type=\"text\" name=\"project-name\" required=\"required\" placeholder= \"Project name\" ></p>
                       <p class=\"submit\"><input type=\"submit\" name=\"get-chunk\" value=\"Get!\"></p>
                      </form>
                      <form method=\"post\" action=\"get-text-to-redact\">
                       <h2>Get text to redact</h2>
                       <p><input type=\"number\" name=\"chunk-size\" required=\"required\" value=\"2000\" ></p>
                       <p><input type=\"text\" name=\"project-name\" required=\"required\" placeholder= \"Project name\" ></p>
                       <p class=\"submit\"><input type=\"submit\" name=\"get-chunk\" value=\"Get!\"></p>
                      </form>
                      <form action=\"return-text\" method=\"post\" enctype=\"multipart/form-data\">
                       <h2>Return text</h2>
                       <p><input name=\"file\" type=\"file\" required=\"required\" /></p>
                       <p><input type=\"text\" name=\"project-name\" required=\"required\" placeholder= \"Project name\" ></p>
                       <p><input type=\"submit\" name=\"submit\" value=\"Submit as translated\" />
                          <input type=\"submit\" name=\"submit\" value=\"Submit as redacted\" /></p>
                     </form>
                     <form action=\"logout\" method=\"get\">
                      <br> <br> <br>
                      <input type=\"submit\" name=\"submit\" value=\"Logout\"/>
                     </form>
                     </div> ")})
       (redirect "/login")))

(defn get-text-to-translate [req]
  (aif (authenticated? req 1)
       (let [params (:params req)
             user it
             pname (:project-name params)
             proj (db/get-project pname)
             size (read-string (:chunk-size params))
             [chunk chunk-size] (db/get-chunk-to-translate pname size user)
             dir (str (System/getProperty "user.dir")  "/resources/public/")
             fname (str (:name user) "-" pname "-" chunk-size ".csv")
             path (str dir fname)]
         (if (:worklist user)
           {:status 403
            :headers {"Content-Type" "text/html"}
            :body "You already have a chunk"}
           (do (util/dump-lines path chunk (into (:project/input-attrs proj)
                                                 (:project/work-attrs proj)))
             (redirect fname))))
       (redirect "/login")))

(defn get-text-to-redact [req]
  (aif (authenticated? req 1)
       (let [params (:params req)
             user it
             pname (:project-name params)
             proj (db/get-project pname)
             size (read-string (:chunk-size params))
             [chunk chunk-size] (db/get-chunk-to-redact pname size user)
             dir (str (System/getProperty "user.dir")  "/resources/public/")
             fname (str (:name user) "-" pname "-" chunk-size ".csv")
             path (str dir fname)]
         (if (:worklist user)
           {:status 403
            :headers {"Content-Type" "text/html"}
            :body "You already have a chunk"}
           (do (util/dump-lines path chunk (into (:project/input-attrs proj)
                                                 (:project/work-attrs proj)))
             (redirect fname))))
       (redirect "/login")))

(defn return-text [req]
  (aif (authenticated? req 1)
       (let [params (:params req)
             user it
             file (:tempfile (:file params))
             pname (:project-name params)
             proj (db/get-project pname)
             data (util/load-lines file (into (:project/input-attrs proj)
                                              (:project/work-attrs proj)))]
         
         (when (= (:submit params) "Submit as translated")
           (db/return-translated-chunk proj data user))
         (when (= (:submit params) "Submit as redacted")
           (db/return-redacted-chunk proj data user))
         {:status 200
          :headers {"Content-Type" "text/html"}
          :body (str "<center>Success! Lines for <b>" pname
                     "</b> uploaded. <a href=\"workspace\">Go back to workspace</a></center>")})
       (redirect "/login")))

;; ===============================================
;; Admin handlers
;; ===============================================

(defn dashboard-handler [req]
  (aif (authenticated? req 0)
       (let [user it]
         {:status 200
          :headers {"Content-Type" "text/html"}
          :body (str "<center>Welcome to dashboard,<b>" (:name user) "</b>.</center>")})
       {:status  403
        :headers {"Content-Type" "text/html"}
        :body    "<center>Only for admins, buddy.</center>"}))

;; ===============================================

(defn logout-handler [req]
  (assoc (redirect "login")
         :session nil))

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
    (if (util/check-token token)
      (do
        (db/add-user username (hash/encrypt password) 1)
        (login-handler username password))
      {:status  403
       :headers {"Content-Type" "text/html"}
       :body    "<center>Invalid token, try again.</center>"})))
