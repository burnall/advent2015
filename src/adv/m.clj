(ns adv.m
  (:require [adv.util :refer [split split-lines parse-int is-digit]]))

; DAY 24

(def input 
  (->> "data/input24.txt"
       (slurp)
       (split-lines)
       (mapv parse-int)
       (reverse)
       ))

(defn better-solution? [{:keys [cnt-a qe-a]} 
                       {:keys [cnt-b qe-b]}]
  (or (< cnt-a cnt-b)
      (and (= cnt-a cnt-b) (< (qe-a) (qe-b)))))

(declare evaluate-branches)
(declare split-sum-in-two)

(defn search-depth [packages current best] 
  (cond 
    (= (:pos current) (count packages)) best
    (not (better-solution? current best)) best
    (and (= (:spent current) (:goal current)) 
         (split-sum-in-two packages current)) current
    :else (evaluate-branches packages current best)))

(defn evaluate-branches [packages current best]
)

(defn split-sum-in-two [packages best])

(defn get-goal [packages]
  (->> packages 
       (reduce +)
       (#(quot % 3))))

(defn day24 
  ([packages] (search-depth packages {:cnt 0, :els [], :qe 0, :pos 0, :spent 0, :goal (get-goal packages)} {:cnt 10}))
  ([] (day24 input)))


