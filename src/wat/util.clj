(ns wat.util
  (:require [cheshire.core :as json]))

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

