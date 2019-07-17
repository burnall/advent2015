(ns adv.m
  (:require [adv.util :refer [split split-lines parse-int is-digit]]))

; DAY 24

(def input 
  (->> "data/input24.txt"
       (slurp)
       (split-lines)
       (map parse-int)
       (reverse)
       (vec)
       ))

(defn better-solution? [{cnt-a :cnt, qe-a :qe} 
                       {cnt-b :cnt, qe-b :qe}]
  (or (< cnt-a cnt-b)
      (and (= cnt-a cnt-b) (< qe-a qe-b))))

(declare evaluate-branches)
(declare splittable-in-two?)

(defn search-depth [packages current-solution best] 
  (cond 
    (not (better-solution? current-solution best)) best
    ;(and (= (:spent current-solution) (:goal current-solution)) 
    ;     (splittable-in-two? packages (:els current-solution))) current-solution
    (= (:spent current-solution) (:goal current-solution)) current-solution
    (= (:pos current-solution) (count packages)) best
    :else (evaluate-branches packages current-solution best)))

(defn add-next-package [packages 
                        {:keys [cnt els qe spent pos goal]}]
  (let [el (packages pos)]
    {:cnt (inc cnt)
     :els (conj els el)
     :qe (* qe el)
     :spent (+ spent el)
     :pos (inc pos)
     :goal goal}))

(defn evaluate-branches [packages {:keys [pos spent goal] :as solution} best]
  (let [el (packages pos)
        best (if (> (+ spent el) goal) 
               best
               (search-depth packages (add-next-package packages solution) best))]
    (search-depth packages (update solution :pos inc) best)))   

(defn filter-out-used [res packages elements]
  (cond
    (empty? packages) res
    (empty? elements) (into res packages)
    :else 
      (let [[p & ps] packages
            [e & es] elements]
        (if (= p e) 
          (recur res ps es)
          (recur (conj res p) ps elements))))) 

(declare evaluate-branches-two)

(defn split-sum-in-two [packages 
                        {:keys [spent pos goal els] :as current-solution}]
  (cond 
    (= spent goal) els
    (= pos (count packages)) false
    :else (evaluate-branches-two packages current-solution)))

(defn add-next-package-two [packages 
                            {:keys [els spent pos goal]}]
  (let [el (packages pos)]
    {:els (conj els el)
     :spent (+ spent el)
     :pos (inc pos)
     :goal goal}))

(defn evaluate-branches-two [packages {:keys [pos spent goal] :as solution}]
  (let [el (packages pos)
        sol (when (<= (+ spent el) goal) 
               (split-sum-in-two packages (add-next-package-two packages solution)))]
    (or sol (split-sum-in-two packages (update solution :pos inc)))))   

(defn splittable-in-two? [packages elements]
  (let [ps (filter-out-used [] packages elements)
        goal (quot (reduce + ps) 2)
        one (split-sum-in-two packages {:els [], :goal goal, :pos 0, :spent 0})]
    (boolean (seq one))))      

(defn get-goal [packages]
  (->> packages 
       (reduce +)
       (#(quot % 4))))

(defn day24 
  ([packages] (search-depth packages {:cnt 0, :els [], :qe 1, :pos 0, :spent 0, :goal (get-goal packages)} {:cnt 1000}))
  ([] (day24 input)))


