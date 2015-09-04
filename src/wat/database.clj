(ns wat.database)

(require '[datomic.api :as d]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(defn define-attributes [c]
  (d/transact c
              [{:db/id (d/tempid :db.part/db)
                :db/ident :line/pname
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/doc "Project name text line belongs to."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :line/num
                :db/valueType :db.type/long
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

(defn create-line [& {:keys [pname id text male-text female-text translator], :or {male-text "" female-text "" translator ""}
                      }]
  (println (str "wtf" pname id text male-text female-text translator))
  {:db/id (d/tempid :db.part/user)
   :line/pname pname
   :line/num (java.lang.Long. id)
   :line/original text
   :line/male male-text
   :line/female female-text
   :line/translator translator
   })

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

(defn dump-project [project-name]
  (let [db (d/db conn)
        content (d/q '[:find [?line ...]
                       :in $ ?pname
                       :where [?line :line/pname ?pname]]
                     db project-name)]
    (with-open [out-file (io/writer (str "/tmp/" project-name ".csv"))]
      (csv/write-csv out-file
                     (sort #(compare (first %1) (first %2))
                             (map #(let [ent (d/touch (d/entity db %))]
                                     [(:line/num ent)
                                      (:line/original ent)
                                      (:line/male ent)
                                      (:line/female ent)
                                      (:line/translator ent)])
                                  content))))))

(defn load-project [pname source]
  (let [data (with-open [in-file (io/reader source)]
               (doall
                (csv/read-csv in-file)))]
    (doseq [[n o m f t] data]
        @(d/transact conn [(create-line :pname pname :id n :text o)]))))

(defn get-dummy-info []
  (println "====================")
  ;(define-attributes conn)
  #_@(d/transact ; add entry
    conn
    [(create-line :pname "default" :id 2 :text "qwerty sdfg")])
  (println ">")
  ;(dump-project "default")
  ;(load-project "tududu" "/tmp/default")
  (dump-project "tududu")
  (println (str (d/q '[:find (wat.database/get-n-words 8 ?e) .;; find all untranslated entries and collect till have n words 
                       :where [?e :line/pname "tududu"]]
                     (d/db conn))))
  (println "<")
  (str "Hello there, big brother greets you!"))

(defn get-project-list []
  "Project list, duh.")


