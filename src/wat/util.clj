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
  (println fname line-list attrs)
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

(let [used (atom [])] ;; Resets each time you restart system or reload this file
  (defn check-token [token]
    (aif (and (= 24 (count token))
               (not (some #{token} @used))
               (apply bit-xor (map int (into [] token))))
         (when (< it 4)
             (swap! used conj token) it)))

  (defn generate-token [role]
    (let [chars (concat (range 48 58) (range 66 91) (range 97 123))]
      (loop []
        (let [p1 (take 3 (repeatedly #(rand-nth chars)))
              p2 (take 4 (repeatedly #(rand-nth chars)))
              p3 (take 4 (repeatedly #(rand-nth chars)))
              p4 (take 4 (repeatedly #(rand-nth chars)))
              p5 (take 4 (repeatedly #(rand-nth chars)))
              unbalanced (concat p1 [45] p2 [45] p3 [45] p4 [45] p5)
              val (apply bit-xor unbalanced)
              comp (cond
                       (= role 0) val ;; admin
                       (= role 1) (bit-flip val 0) ;; second redactor
                       (= role 2) (bit-flip val 1) ;; first redactor
                       true (-> val (bit-flip 0) (bit-flip 1))) ;; translator 
              token (reduce str (map char (conj unbalanced comp)))]
          (if (and (not (some #{token} @used))
                   (some #{comp} chars))
            token
            (recur)))))))

