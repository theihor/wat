(ns wat.database)

(require '[datomic.api :as d]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(defn define-attributes [c]
  (d/transact c ; User attributes
              [{:db/id (d/tempid :db.part/db)
                :db/ident :user/name
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one
                :db/doc "Username"
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :user/properties
                :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one
                :db/doc "Defines user rights (admin/user etc.)"
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :user/uid
                :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one
                :db/doc "UID"
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :user/rating
                :db/valueType :db.type/double
                :db/cardinality :db.cardinality/one
                :db/doc "User work rating."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :user/worklist
                :db/valueType :db.type/ref
                :db/cardinality :db.cardinality/many
                :db/doc "List of lines user working on."
                :db.install/_attribute :db.part/db}])
  (d/transact c ; Line attributes
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
                :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one
                :db/doc "Text line translator."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :line/redactor
                :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one
                :db/doc "Text line translator."
                :db.install/_attribute :db.part/db}
               {:db/id (d/tempid :db.part/db)
                :db/ident :line/reserved
                :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one
                :db/doc "UID of person that is currently working on this line."
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

(defn create-line [& {:keys [pname id text male-text female-text translator redactor reserved],
                      :or {pname "" id -1 text "" male-text "" female-text ""
                           translator 0 redactor 0 reserved 0}}]
  {:db/id (d/tempid :db.part/user)
   :line/pname pname
   :line/num (java.lang.Long. id)
   :line/original text
   :line/male male-text
   :line/female female-text
   :line/translator (java.lang.Long. translator)
   :line/redactor (java.lang.Long. redactor)
   :line/reserved (java.lang.Long. reserved)
   })

(defn get-n-words [n coll]
  (let [sum (atom 0)
        db (d/db conn)]
    [(reduce (fn [x y]
               (if (< @sum n)
                  (do
                    (swap! sum + (count (clojure.string/split (:line/original (d/touch (d/entity db y))) #"\s")))
                    (conj x y))
                  x))
              []
              coll) @sum]))

(defn dump-project
  "Dump project sorted by line number into file specified.
   Header is not printed now."
  [project-name fname]
  (let [db (d/db conn)
        content (d/q '[:find [?line ...]
                       :in $ ?pname
                       :where [?line :line/pname ?pname]]
                     db project-name)]
    (with-open [out-file (io/writer fname)]
      (csv/write-csv out-file
                     (sort #(compare (first %1) (first %2))
                             (map #(let [ent (d/touch (d/entity db %))]
                                     [(:line/num ent)
                                      (:line/original ent)
                                      (:line/male ent)
                                      (:line/female ent)
                                      (:line/translator ent)])
                                  content))))))

(defn load-project
  "Load project from specified file.
   Now it is considered that header does not exist."
  [pname source]
  (let [data (with-open [in-file (io/reader source)]
               (doall
                (csv/read-csv in-file)))]
    (doseq [[n o m f t] data]
      (d/transact conn [(create-line :pname pname :id n :text o)]))))

(defn cleanup-project
  "Delete all entries related to specified project.
   Good to wait for ~3 seconds after it to let indexing event occur."
  [pname]
  (let [data (d/q '[:find [?line ...]
                    :in $ ?pname
                    :where [?line :line/pname ?pname]]
                  (d/db conn) pname)]
    (doseq [e data]
      (d/transact
       conn
       [{:db/id #db/id[db.part/user], :db/excise e}]))))

(defn get-project-chunk-to-translate [pname size uid]
  (let [[chunk actual-size] (get-n-words size
                                         (d/q '[:find [?e ...]
                                                :in $ ?pname
                                                :where [?e :line/pname ?pname]
                                                       [?e :line/translator 0]
                                                       [?e :line/reserved 0]]
                                              (d/db conn) pname))]
    (doseq [e chunk]
      (d/transact conn
                  [{:db/id e
                    :line/reserved uid}]))
    [chunk actual-size]))

(defn get-project-chunk-to-redact [pname size uid]
  (let [[chunk actual-size] (get-n-words size
                                         (d/q '[:find [?e ...]
                                                :in $ ?pname
                                                :where [?e :line/pname ?pname]
                                                       (not [?e :line/translator 0])
                                                       [?e :line/reserved 0]]
                                              (d/db conn) pname))]
    (doseq [e chunk]
      (d/transact conn
                  [{:db/id e
                    :line/reserved uid}]))
    [chunk actual-size]))

(defn return-translated-chunk [chunk uid]
  (doseq [e chunk]
    (d/transact conn
                  [{:db/id e
                    :line/reserved 0}
                    :line/translator uid])))

(defn return-redacted-chunk [chunk uid]
  (doseq [e chunk]
    (d/transact conn
                  [{:db/id e
                    :line/reserved 0}
                    :line/redactor uid])))

(defn update-chunk [data]
  (doseq [e data]
    (when-not (= e (d/touch (d/entity (d/db conn) (:db/id e))))
      (d/transact conn [e]))))

(defn get-dummy-info []
  (println "====================")
  ;(define-attributes conn)
  #_(println (str (d/transact ; add entry
                 conn
                 [(create-line :pname "default" :id 1 :text "qwerty sdfg")])))
  (println (d/q '[:find [?ident ...]
                  :where
                  [?e :db/ident ?ident]
                  [_ :db.install/attribute ?e]
                  [(.toString ?ident) ?val]
                  [(.startsWith ?val ":line")]] (d/db conn)))
  (println ">")
  ;(load-project "tududu" "/tmp/default")
  ;(dump-project "tududu" "/tmp/atatta.csv")
  ;; (future
  ;;   (cleanup-project "tududu")
  ;;   (Thread/sleep 3000)
  ;;   (dump-project "tududu" "/tmp/atatta.csv"))
                                        ;(println (get-project-chunk "tududu" 12))
  #_(d/transact conn ; User attributes
                [{:db/id (d/tempid :db.part/db)
                  :db/ident :user/name
                  :db/valueType :db.type/string
                  :db/cardinality :db.cardinality/one
                  :db/doc "Username"
                  :db.install/_attribute :db.part/db}
                 {:db/id (d/tempid :db.part/db)
                  :db/ident :user/uid
                  :db/valueType :db.type/ref
                  :db/cardinality :db.cardinality/many
                  :db/doc "UID"
                  :db.install/_attribute :db.part/db}])
  #_(d/transact
     conn
     [{:db/id 17592186045973
       :user/name "dummy"
       :user/uid [17592186045969]}])

  ;(get-project-chunk-to-translate "tududu" 11 1)
  
  #_(println (d/touch (d/entity (d/db conn) (d/q '[:find ?e .
                                                 :where [?e :user/name "dummy"]]
                                               (d/db conn)))))
  (println "<")
  (str "Hello there, big brother greets you!"))

(defn get-project-list []
  "Project list, duh.")


