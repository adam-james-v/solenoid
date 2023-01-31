(ns solenoid.utils
  (:require [clojure.string :as str])
  (:import [java.net URLDecoder]))

(defn parse-body [body]
  (-> body
      slurp
      (str/split #"=")
      second
      URLDecoder/decode))

(defn url-encoded-str->str
  [s]
  (URLDecoder/decode s))

(defn maybe-read-string
  [s]
  (try (read-string s)
       (catch Exception _e nil)))

(defn get-string-value [query-string]
  (let [m (->> (str/split query-string #"[=&]")
               (partition 2)
               (mapv vec)
               (into {}))]
    (-> (get m "value")
        url-encoded-str->str)))

(defn query-string->map [query-string]
  (if query-string
    (let [m (->> (str/split query-string #"[=&]")
                 (partition 2)
                 (mapv vec)
                 (into {}))]
      (-> m
          (update-vals url-encoded-str->str)
          (update-vals maybe-read-string)
          (update-keys keyword)))
    {}))

(defn stringified-key->keyword
  [sk]
  (keyword (str/replace sk #":" "")))

;; https://stackoverflow.com/a/12814087
(defn get-watches
  "Returns list of keys corresponding to watchers of the reference."
  [^clojure.lang.IRef reference]
  (keys (.getWatches reference)))
