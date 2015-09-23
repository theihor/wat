(ns wat.util
  (:require [cheshire.core :as json]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defmacro aif
  ([cond then else]
   (let [its 'it]
    `(let [~its ~cond]
       (if ~its
         ~then
         ~else))))
  ([cond then]
    (let [its 'it]
    `(let [~its ~cond]
       (if ~its
         ~then)))))

(defn encode
  "Convert map to json format."
  [map]
  (json/generate-string map))

(defn decode
  "Convert json to map."
  [json]
  (json/parse-string json true))

(defn dump-lines [fname line-list attrs]
  (with-open [out-file (io/writer fname)]
    (csv/write-csv out-file
                   (cons attrs
                         (map #(map % attrs)
                              (sort-by :line/num line-list))))))

(defn load-lines [source attrs]
  (let [data (rest (with-open [in-file (io/reader source)]
                     (doall
                      (csv/read-csv in-file))))]
   (loop [line data
          num 0
          coll []]
     (aif (first line)
          (recur (rest line)
                 (inc num)
                 (conj coll (zipmap attrs it)))
          coll))))
