(ns adv.l
  (:require [adv.util :refer [split split-lines parse-int is-digit]]))

; DAY 23

(defn get-value [s]
  (if (or (= (first s) \+) (= (first s) \-)) 
    (parse-int s)
    (keyword s)))

(defn make-command 
  ([cmd op1] {:cmd cmd, :op1 (get-value op1)})
  ([cmd op1 op2] (assoc (make-command cmd op1) :op2 (get-value op2)))) 

(defn parse-command [line]
  (-> line 
      (split #"[ ,]+") 
      (#(apply make-command %)))) 

(def input23 
  (->> "data/input23.txt"
       (slurp)
       (split-lines)
       (map parse-command)))




