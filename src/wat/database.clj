(ns wat.database)

(require '[datomic.api :as d])

(defn define-attributes [c]
  (d/transact c
              [{:db/id (d/tempid :db.part/db)
                :db/ident :project/name
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/doc "Project name text line belongs to."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :line/id
                :db/valueType :db.type/bigint
                :db/cardinality :db.cardinality/one
                :db/doc "Text line number."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
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

(defn create-line [id original-text male-text female-text translator]
  {:db/id (d/tempid :db.part/user)
   :project/name "default"
   :line/id id
   :line/original original-text
   :line/male male-text
   :line/female female-text
   :line/translator translator})

(defn get-n-words [n coll]
  (let [sum (atom 0)
        db (d/db conn)]
    (reduce (fn [x y]
              (if (< @sum n)
                (do
                  (swap! sum + (count (clojure.string/split (:line/original (d/touch (d/entity db y))) #"\s")))
                  (conj x y))
              x))
          []
          coll)))

(defn get-dummy-info []
  #_ 
  @(d/transact ; add entry
    conn
    [(create-line "wolololol" "" "" "noone")])
  (println ">")
  (println (str (d/q '[:find (wat.database/get-n-words 3 ?e) . ;; find all untranslated entries and collect till have n words 
                       :where [?e :line/translator "noone"]]
                     (d/db conn))))
  (println "<")
  (str "Hello there, big brother greets you!"))

(defn get-project-list []
  "Project list, duh.")


