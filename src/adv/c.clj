(ns adv.c
  (:require [adv.util :as util :refer [split-lines]]))

; DAY 8
(def input8 
  (->> "data/input8.txt"
       (slurp)
       (split-lines)))

(defn get-in-memory-size [s]
  (let [patterns (re-seq #"\\." s)]
    (- (count s)
       (reduce + 
               2
               (map #(case (second %) 
                      \\ 1
                      \" 1
                      3)
                      patterns)))))

(defn get-code-representation-size [s]
  (count s))

(defn day8
  ([lines] (->> lines
                (map #(- (get-code-representation-size %) (get-in-memory-size %)))
                (reduce + 0)))
  ([] (day8 input8)))
 
