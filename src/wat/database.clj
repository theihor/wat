(ns wat.database)

(require '[datomic.api :as d]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clj-time.core :as time]
         '[clj-time.coerce :as tc])

(use '[wat.util :refer :all])

(declare define-attributes)
(declare get-entity)

(defonce connect-to-db-server
  (let [db-name "test-db"
        uri (str "datomic:free://localhost:3131/" db-name)
        created? (d/create-database uri)]
    (future
      (Thread/sleep 5000)
      (def ^:dynamic *conn* (d/connect uri))
     (if created?
       (do
         (define-attributes)
         (println "\nCreated database" db-name "!"))
       (println "\nDatabase" db-name "already exist, its fine."))
     (println "Connection established!"))))

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
  (aif (d/q '[:find ?proj .
              :in $ ?pname
              :where [?proj :project/name ?pname]]
            (d/db *conn*) pname)
       (let [proj (into {} (d/touch (d/entity (d/db *conn*) it)))]
        (assoc proj
                :project/input-attrs (load-string (:project/input-attrs proj))
                :project/work-attrs (load-string (:project/work-attrs proj))
                :project/inner-attrs [:line/reserved :line/pname :line/num :db/id]))))

(defn dump-project
  "Dump project sorted by line number into file specified or just return lines if no file specified.
   Header is not printed now."
  ([pname fname]
   (let [db (d/db *conn*)
         proj (get-project pname)
         attrs (into (:project/input-attrs proj)
                     (:project/work-attrs proj))
         content (d/q '[:find [?line ...]
                        :in $ ?pname
                        :where [?line :line/pname ?pname]]
                      db pname)]
     (with-open [out-file (io/writer fname)]
       (csv/write-csv out-file
                      (cons attrs
                            (map #(map % attrs)
                                 (sort-by :line/num (map #(into {} (d/touch (d/entity db %))) content))))))))
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
    (d/transact-async
     *conn*
     (loop [line data
            num 0
            coll []]
       (aif (first line)
            (recur (rest line)
                   (inc num)
                   (conj coll (assoc (zipmap attrs (concat it stub))
                                     :db/id (d/tempid :db.part/user)
                                     :line/num num)))
            coll)))))

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

(defn get-chunk-to-translate [pname size user]
  (when-not (:worklist user)
   (let [uname (:name user)
         db (d/db *conn*)
         [chunk actual-size] (get-n-words size
                                          (d/q '[:find [?e ...]
                                                 :in $ ?pname
                                                 :where [?e :line/pname ?pname]
                                                 [?e :line/translator ""]
                                                 [?e :line/reserved ""]]
                                               db pname))]
     (doseq [e chunk]
       (d/transact *conn*
                   [{:db/id e
                     :line/reserved uname}]))
     (d/transact *conn*
                 [{:db/id (db-id-by-name uname)
                   :user/worklist chunk}])
     [(doall (map #(into {} (d/touch (d/entity db %))) chunk)) actual-size])))

(defn get-chunk-to-redact [pname size user]
  (when-not (:worklist user)
   (let [uname (:name user)
         db (d/db *conn*)
         translator (:line/translator (get-entity db (d/q '[:find ?e .
                                                          :in $ ?pname
                                                          :where [?e :line/pname ?pname]
                                                          (not [?e :line/translator ""])
                                                          [?e :line/reserved ""]]
                                                        db pname)))
         [chunk actual-size] (get-n-words size
                                          (d/q '[:find [?e ...]
                                                 :in $ ?pname ?tname
                                                 :where [?e :line/pname ?pname]
                                                 [?e :line/translator ?tname]
                                                 [?e :line/redactor ""] 
                                                 [?e :line/reserved ""]]
                                               (d/db *conn*) pname translator))]
     (doseq [e chunk]
       (d/transact *conn*
                   [{:db/id e
                     :line/reserved uname}]))
     (d/transact *conn*
                 [{:db/id (db-id-by-name uname)
                   :user/worklist chunk}])
     [(doall (map #(into {} (d/touch (d/entity db %))) chunk)) actual-size])))

(defn get-chunk-to-redact-2 [pname size user]
  (when-not (:worklist user)
    (let [uname (:name user)
          urole (:role user)
          db (d/db *conn*)
          redactor (:line/redactor
                    (get-entity db (d/q '[:find ?e .
                                         :in $ ?pname ?uname ?urole
                                         :where [?e :line/pname ?pname]
                                                [?e :line/redactor ?rname]
                                          [(#(and (not= %1 %2)
                                                  (not= ?rname "")
                                                        (not= (:role (wat.database/user-exist? %1)) %3))
                                                  ?rname ?uname ?urole)]
                                                [?e :line/reserved ""]]
                                       db pname uname urole)))
          [chunk actual-size] (when redactor (get-n-words size
                                             (d/q '[:find [?e ...]
                                                    :in $ ?pname ?rname
                                                    :where [?e :line/pname ?pname]
                                                    [?e :line/redactor ?rname] 
                                                    [?e :line/reserved ""]]
                                                  db pname redactor)))]
      (doseq [e chunk]
       (d/transact *conn*
                   [{:db/id e
                     :line/reserved uname}]))
     (d/transact *conn*
                 [{:db/id (db-id-by-name uname)
                   :user/worklist chunk}])
     [(doall (map #(into {} (get-entity db %)) chunk)) actual-size])))

(defn return-translated-chunk [proj data user]
  (let [uname (:name user)
        worklist (:worklist user)
        db (d/db *conn*)
        number-translated ;; Use it somehow
        (loop [old-lines worklist
               num-modified 0]
          (aif (:db/id (first old-lines))
               (let [original (apply dissoc
                                     (into {} (get-entity db it))
                                     (:project/inner-attrs proj))
                     id (:line/id original)
                     modified (first (filter #(= id (:line/id %)) data))]
                 (if (and original (= original modified))
                   (do ;; If line was not changed
                     (d/transact *conn*
                                   [{:db/id it
                                     :line/reserved ""}])
                     (recur (rest old-lines)
                            num-modified))
                   (do ;; If line actually has changes
                     (d/transact *conn*
                                   [(assoc modified
                                           :db/id it
                                           :line/reserved ""
                                           :line/translator uname)])
                     (recur (rest old-lines)
                            (+ num-modified
                               (count (clojure.string/split (:line/text modified) #"\s")))))))
               num-modified))]
    (doseq [e worklist]
      (d/transact *conn*
                  [[:db/retract (db-id-by-name uname) :user/worklist (:db/id e)]]))))

(defn return-redacted-chunk [proj data user]
  (let [uname (:name user)
        worklist (:worklist user)
        db (d/db *conn*)
        number-redacted ;; Use it somehow
        (loop [old-lines worklist
               num-modified 0]
          (aif (:db/id (first old-lines))
               (let [original (apply dissoc
                                     (into {} (get-entity db it))
                                     (:project/inner-attrs proj))
                     id (:line/id original)
                     modified (first (filter #(= id (:line/id %)) data))] 
                 (d/transact *conn*
                             [(assoc modified
                                     :db/id it
                                     :line/reserved ""
                                     :line/redactor uname)])
                 (recur (rest old-lines)
                        (+ num-modified
                           (count (clojure.string/split (:line/text modified) #"\s")))))
               num-modified))]
    (doseq [e worklist]
      (d/transact *conn*
                  [[:db/retract (db-id-by-name uname) :user/worklist (:db/id e)]]))))

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
       (let [user (get-entity (d/db *conn*) it)]
         {:name (:user/name user) :uid (:user/uid user) :password (:user/password user) :role (:user/role user) :worklist (:user/worklist user)})))

(defn get-user-list []
  (doall
   (map #(get-entity (d/db *conn*) %)
        (d/q '[:find [?user ...]
               :where [?user :user/name _]] (d/db *conn*)))))

(defn get-project-list []
  (doall
   (map #(d/touch (d/entity (d/db *conn*) %))
        (d/q '[:find [?user ...]
               :where [?user :project/name _]] (d/db *conn*)))))

(defn get-line-attributes []
  (d/q '[:find [?ident ...]
         :where [?e :db/ident ?ident]
         [_ :db.install/attribute ?e]
         [(str ?ident) ?val]
         [(.startsWith ^String ?val ":line")]] (d/db *conn*)))

(defn get-attribute-history [entity-id attr-id]
  (vec (d/q '[:find ?tx-time ?v
              :in $ ?e ?attr
              :where [?e ?a ?v ?tx ?added]
                     [(= ?a ?attr)]
                     [?tx :db/txInstant ?tx-time]
                     [(= true ?added)]] 
            (d/history (d/db *conn*)) 
            entity-id
            attr-id)))

(defn get-attribute-id [attr]
  (d/q '[:find ?e .
         :in $ ?a
         :where [?e :db/ident ?ident]
                [_ :db.install/attribute ?e]
                [(= ?ident ?a)]]
       (d/db *conn*) attr))

(defn get-line-by-id [pname id]
  (aif (d/q '[:find ?e .
              :in $ ?pname ?id
              :where [?e :line/id ?id]
                     [?e :line/pname ?pname]]
            (d/db *conn*) pname id)
   (d/touch (d/entity (d/db *conn*) it))))

(defn get-entity [db id]
  (aif (d/entity db id)
       (d/touch it)))
