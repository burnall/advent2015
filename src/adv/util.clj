(ns adv.util)

(defn parse-int [s] (Integer/parseInt s))

(def split clojure.string/split)

(defn split-lines [text]
  (split text #"\n"))

(defn is-digit [c]
  (and (>= (int c) (int \0)) (<= (int c) (int \9))))


