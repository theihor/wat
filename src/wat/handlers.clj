(ns wat.handlers
  (:require [buddy.hashers :as hash]
            [ring.util.response :refer :all]))

(use '[wat.database :as db])
(use '[wat.util :as util :refer [aif]])
(use '[wat.handlers-html :as html])

(defn authenticated? [req role]
  (let [user (user-exist? (:user (:session req)))]
    (if (and user (<= (:role user) role))
      user)))

(def ^:const TRANSLATOR 3)
(def ^:const REDACTOR 2)
(def ^:const G-REDACTOR 1)
(def ^:const ADMIN 0)

;; ===============================================
;; User handlers
;; ===============================================

(defn workspace-handler [req] ;; TODO: if user already got chunk - load it to workspace instantly
  (aif (authenticated? req TRANSLATOR)
       (let [user it]
         {:status 200
          :headers {"Content-Type" "text/html" "Title" "WAT"}
          :body (html/workspace-body)})
       (redirect "/login")))

(defn get-text-to-translate [req]
  (aif (authenticated? req TRANSLATOR)
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
  (aif (authenticated? req REDACTOR)
       (let [params (:params req)
             user it
             pname (:project-name params)
             proj (db/get-project pname)
             size (read-string (:chunk-size params))
             [chunk chunk-size] (cond
                                  (= (:submit params) "Redact translated")
                                    (db/get-chunk-to-redact pname size user)
                                  (= (:submit params) "Redact redacted")
                                    (db/get-chunk-to-redact-2 pname size user))
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
  (aif (authenticated? req TRANSLATOR)
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
  (aif (authenticated? req ADMIN)
       (let [user it]
         {:status 200
          :headers {"Content-Type" "text/html"}
          :body (html/dashboard-body)})
       {:status  403
        :headers {"Content-Type" "text/html"}
        :body    "<center>Only for admins, buddy.</center>"}))

(defn get-line-by-identifier [req]
  (aif (authenticated? req ADMIN)
       (let [params (:params req)
             user it
             pname (:project-name params)
             proj (db/get-project pname)
             id (:line-id params)
             line (into {} (db/get-line-by-id pname id))
             dir (str (System/getProperty "user.dir")  "/resources/public/")
             fname (str pname "-line-" id ".csv")
             path (str dir fname)]
         (util/dump-lines path [line] (into (:project/input-attrs proj)
                                            (:project/work-attrs proj)))
         (redirect fname))))

(defn search-by-string [req]
  (aif (authenticated? req ADMIN)
       (let [params (:params req)
             user it
             pname (:project-name params)
             string (:string-to-search params)
             attr (keyword (:line-attribute params))
             proj (db/get-project pname)
             chunk (db/search-in-project pname string attr)
             dir (str (System/getProperty "user.dir")  "/resources/public/")
             fname (str pname "-search.csv")
             path (str dir fname)]
         (util/dump-lines path chunk (into (:project/input-attrs proj)
                                           (:project/work-attrs proj)))
         (redirect fname)
         )))

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
    (aif (util/check-token token)
      (do
        (db/add-user username (hash/encrypt password) it)
        (login-handler username password))
      {:status  403
       :headers {"Content-Type" "text/html"}
       :body    "<center>Invalid token, try again.</center>"})))
