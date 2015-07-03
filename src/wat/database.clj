(ns wat.database)

(require '[datomic.api :as d])

(defn define-attributes [c]
  (d/transact c
              [{:db/id (d/tempid :db.part/db)
                :db/ident :line/translator
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/doc "Text line translator."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :line/original
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/doc "Original text line."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :line/male
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/doc "Male translated text line."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :line/female
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/doc "Female translated text line."
                :db.install/_attribute :db.part/db}]))

(defn create-line [original-text male-text female-text translator]
  {:db/id (d/tempid :db.part/user)
   :line/original original-text
   :line/male male-text
   :line/female female-text
   :line/translator translator})

(defn get-2000 [coll]
  (println coll)
  (let [sum (atom 0)]
    (reduce (fn [x y] (println (:line/original (d/touch (d/entity (d/db conn) y))))
              (if (< @sum 1980)
                (do
                  (swap! sum + (count (:line/original (d/touch (d/entity (d/db conn) y)))))
                  (conj x y))
              x))
          []
          coll)))

(defonce connect-to-db-server
  (let [db-name "test-db"
        uri (str "datomic:free://localhost:3131/" db-name)
        res (d/create-database uri)]
    (def conn (d/connect uri))
    (if res
      (do (define-attributes conn)
          (println "Created database" db-name "!"))
      (println "Database" db-name "already exist, its fine."))
    (println "Connection established!")))

(defn get-dummy-info []
  (define-attributes conn)
  @(d/transact ; add entry
    conn
    [(create-line "wolololol" "" "" "noone")])
  (println ">")
  (println (str (d/q '[:find (wat.database/get-2000 ?e) . ;; find all untranslated entries and collect till have 2000 symbols or shit
                       :where [?e :line/translator "noone"]]
                     (d/db conn))))
  (println "<")
  (str "messing with datomic"))

(defn get-project-list []
  "Project list, duh.")


