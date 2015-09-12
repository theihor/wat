(ns wat.database)

(require '[datomic.api :as d]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clj-time.core :as time]
         '[clj-time.coerce :as tc])

(use '[wat.util :refer :all])

(declare define-attributes)

(defonce connect-to-db-server
  (let [db-name "test-db"
        uri (str "datomic:free://localhost:3131/" db-name)
        res (d/create-database uri)]
    (def ^:dynamic *conn* (d/connect uri))
    (if res
      (do (define-attributes)
          (println "Created database" db-name "!"))
      (println "Database" db-name "already exist, its fine."))
    (println "Connection established!")))

(defn add-attribute
  ([name type cardinality doc]
   (future
     (d/transact *conn*
                 [{:db/id (d/tempid :db.part/db)
                   :db/ident name
                   :db/valueType type
                   :db/cardinality cardinality
                   :db/doc doc
                   :db.install/_attribute :db.part/db}])))
  ([[name type cardinality doc]]
   (future
     (d/transact *conn*
                 [{:db/id (d/tempid :db.part/db)
                   :db/ident name
                   :db/valueType type
                   :db/cardinality cardinality
                   :db/doc doc
                   :db.install/_attribute :db.part/db}]))))

(defn define-attributes
  "Basic attributes, call only for newly created database."
  []
  (doall
   (map add-attribute ;; Project attributes
        [[:project/name :db.type/string :db.cardinality/one "Project name"]
         [:project/input-attrs :db.type/string :db.cardinality/one "Line attributes used to load project."]
         [:project/work-attrs :db.type/string :db.cardinality/one "Line attributes used in work."]]))
  (doall
   (map add-attribute ;; User attributes
        [[:user/name :db.type/string :db.cardinality/one "Username"]
         [:user/password :db.type/string :db.cardinality/one "Encrypted user password."]
         [:user/role :db.type/long :db.cardinality/one "Defines user rights (admin=0, user=1 etc.)"]
         [:user/uid :db.type/long :db.cardinality/one "UID"]
         [:user/rating :db.type/long :db.cardinality/one "User work rating."]
         [:user/worklist :db.type/ref :db.cardinality/many "List of lines user working on."]]))
  (doall
   (map add-attribute ;; Basic line attributes
        [[:line/pname :db.type/string :db.cardinality/one "Project name text line belongs to."]
         [:line/id :db.type/string :db.cardinality/one "Text line id."]
         [:line/text :db.type/string :db.cardinality/one "Original text line."]
         [:line/translator :db.type/string :db.cardinality/one "Text line translator."]
         [:line/redactor :db.type/string :db.cardinality/one "Text line redactor."]
         [:line/reserved :db.type/string :db.cardinality/one "Person that is currently working on this line."]
         [:line/num :db.type/long :db.cardinality/one "Line number for proper ordering."]]))
  nil)

(defn- db-id-by-name [name]
  (when name
    (d/q '[:find ?ident .
           :in $ ?name
           :where [?ident :user/name ?name]] (d/db *conn*) name)))

(defn get-n-words [n coll]
  (let [sum (atom 0)
        db (d/db *conn*)]
    [(reduce (fn [x y]
               (if (< @sum n)
                  (do
                    (swap! sum + (count (clojure.string/split (:line/text (d/touch (d/entity db y))) #"\s")))
                    (conj x y))
                  x))
              []
              coll) @sum]))

(defn create-project [pname input-attrs work-attrs]
  (d/transact *conn*
              [{:db/id (d/tempid :db.part/user)
                :project/name pname
                :project/input-attrs (str input-attrs)
                :project/work-attrs (str work-attrs)}]))

(defn get-project [pname]
  (let [proj
        (into {} (d/touch (d/entity (d/db *conn*)
                                    (d/q '[:find ?proj .
                                           :in $ ?pname
                                           :where [?proj :project/name ?pname]]
                                         (d/db *conn*) pname))))]
    (assoc proj :project/input-attrs (load-string (:project/input-attrs proj))
                :project/work-attrs (load-string (:project/work-attrs proj)))))

(defn dump-project
  "Dump project sorted by line number into file specified or just return lines if no file specified.
   Header is not printed now."
  ([pname fname]
   (let [db (d/db *conn*)
         proj (get-project pname)
         attrs (vec (concat (:project/input-attrs proj)
                            (:project/work-attrs proj)))
         content (d/q '[:find [?line ...]
                        :in $ ?pname
                        :where [?line :line/pname ?pname]]
                      db pname)]
     (with-open [out-file (io/writer fname)]
       (csv/write-csv out-file
                      (map #(map % attrs)
                           (sort-by :line/num (map #(into {} (d/touch (d/entity db %))) content)))))))
  ([pname]
    (let [db (d/db *conn*)
          content (d/q '[:find [?line ...]
                         :in $ ?pname
                         :where [?line :line/pname ?pname]]
                       db pname)]
      (map #(d/touch (d/entity db %)) content))))

(defn load-project
  "Load project from specified file."
  [pname source]
  (let [proj (get-project pname)
        attrs (concat (:project/input-attrs proj)
                      (:project/work-attrs proj)
                      [:line/reserved :line/pname :line/num :db/id])
        stub (concat (repeat (inc (count (:project/work-attrs proj))) "") [pname])
        data (rest (with-open [in-file (io/reader source)]
                     (doall
                      (csv/read-csv in-file))))]
    
    (loop [line data
           num 0]
      (aif (first line)
           (do
             (d/transact *conn* [(assoc (zipmap attrs (concat it stub))
                                        :db/id (d/tempid :db.part/user)
                                        :line/num num)])
             (recur (rest line)
                    (inc num)))))) true)

(defn cleanup-project
  "Delete all entries related to specified project.
   Good to wait for ~3 seconds after it to let indexing event occur."
  [pname]
  (let [data (d/q '[:find [?line ...]
                    :in $ ?pname
                    :where [?line :line/pname ?pname]]
                  (d/db *conn*) pname)]
    (doseq [e data]
      (d/transact
       *conn*
       [{:db/id #db/id[db.part/user], :db/excise e}]))))

(defn get-project-chunk-to-translate [pname size user]
  (let [uname (:name user)
        [chunk actual-size] (get-n-words size
                                         (d/q '[:find [?e ...]
                                                :in $ ?pname
                                                :where [?e :line/pname ?pname]
                                                       [?e :line/translator ""]
                                                       [?e :line/reserved ""]]
                                              (d/db *conn*) pname))]
    (doseq [e chunk]
      (d/transact *conn*
                  [{:db/id e
                    :line/reserved uname}]))
    (d/transact *conn*
                  [{:db/id (db-id-by-name uname)
                    :user/worklist chunk}])
    [(doall (map #(d/touch (d/entity (d/db *conn*) %)) chunk)) actual-size]))

(defn get-project-chunk-to-redact [pname size user]
  (let [uname (:name user)
        [chunk actual-size] (get-n-words size
                                         (d/q '[:find [?e ...]
                                                :in $ ?pname
                                                :where [?e :line/pname ?pname]
                                                       (not [?e :line/translator ""])
                                                       [?e :line/reserved ""]]
                                              (d/db *conn*) pname))]
    (doseq [e chunk]
      (d/transact *conn*
                  [{:db/id e
                    :line/reserved uname}]))
    (d/transact *conn*
                [{:db/id (db-id-by-name uname)
                  :user/worklist chunk}])
    [(doall (map #(d/touch (d/entity (d/db *conn*) %)) chunk)) actual-size]))

(defn return-translated-chunk [chunk user]
  (let [uname (:name user)]
   (doseq [e chunk]
     (d/transact *conn*
                 [{:db/id e
                   :line/reserved ""
                   :line/translator uname}]))
   (d/transact *conn*
               [{:db/id (db-id-by-name uname)
                 :user/worklist []}])))

(defn return-redacted-chunk [chunk user]
  (let [uname (:uname user)]
    (doseq [e chunk]
            (d/transact *conn*
                        [{:db/id e
                          :line/reserved ""
                          :line/redactor uname}]))
    (d/transact *conn*
                [{:db/id (db-id-by-name uname)
                  :user/worklist []}])))

(defn update-chunk [data]
  (doseq [e data]
    (when-not (= e (d/touch (d/entity (d/db *conn*) (:db/id e))))
      (d/transact *conn* [e]))))

(defn add-user [username password role]
  (let [uid (tc/to-long (time/now))]
   (d/transact *conn*
               [{:db/id (d/tempid :db.part/user)
                 :user/name username
                 :user/uid (java.lang.Long. uid)
                 :user/password password
                 :user/rating (java.lang.Long. 0)
                 :user/worklist []
                 :user/role (java.lang.Long. role)}])))

(defn set-password
  "Password should be provided in hashed form."
  [username password]
  (aif (d/q '[:find ?ident .
              :in $ ?name
              :where [?ident :user/name ?name]] (d/db *conn*) username)
       (d/transact *conn*
                   [{:db/id (:db/id (d/touch (d/entity (d/db *conn*) it)))
                     :user/password password}])))

(defn user-exist? [name]
  (aif (db-id-by-name name)
       (let [user (d/touch (d/entity (d/db *conn*) it))]
         {:name (:user/name user) :uid (:user/uid user) :password (:user/password user) :role (:user/role user)})))

(defn get-user-list []
  (doall
   (map #(d/touch (d/entity (d/db *conn*) %))
        (d/q '[:find [?user ...]
               :where [?user :user/name _]] (d/db *conn*)))))

(defn get-line-attributes []
  (d/q '[:find [?ident ...]
         :where [?e :db/ident ?ident]
         [_ :db.install/attribute ?e]
         [(str ?ident) ?val]
         [(.startsWith ^String ?val ":line")]] (d/db *conn*)))

