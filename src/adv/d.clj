(ns adv.d
  (:require [adv.util :as util :refer [split-lines parse-int]]))

; DAY 11

(defn to-base-26 [s]
  (->> s
       (mapv (fn [ch] (- (int ch) (int \a))))))

(defn base-26-to-string [ds]
  (->> ds
       (map (fn [d] (char (+ d (int \a)))))
       (apply str)))

(defn inc-base-26 [ds]
  (let [n (count ds)
        index-not-25 (->> (range (dec n) -1 -1)
                          (filter (fn [i] (not= (ds i) 25)))
                (first))]
    (vec (if (nil? index-not-25) 
           (repeat n 0)
           (concat (subvec ds 0 index-not-25) 
                   [(inc (ds index-not-25))] 
                   (repeat (- n index-not-25 1) 0))))))

(defn has-straight-3 [ds]
  (->> ds
       (reduce (fn [{:keys [cnt prev]} d]
                 (if (= (inc prev) d) 
                   (if (= cnt 1)
                     (reduced {:res true})
                     {:prev d, :cnt 1})
                   {:prev d, :cnt 0}))  
               {:cnt 0, :prev -1})
        :res))

(defn has-no-iol [ds]
  (->> ds
       (filter #(or (= % (- (int \i) (int \a)))
               (= % (- (int \o) (int \a)))
               (= % (- (int \l) (int \a)))))
      (first)
      (nil?)))

(defn has-two-pairs [ds]
  (->> ds 
       (reduce (fn [{:keys [prev cnt pair]} d]
                 (if (= prev d)
                   (if (= cnt 1) 
                     (if (= d pair)
                       {:prev d, :cnt 1, :pair pair}
                       (reduced {:res true}))
                     {:prev d, :cnt 1, :pair d})
                   {:prev d, :cnt cnt, :pair pair}))  
               {:prev -1, :cnt 0, :pair nil})
      (:res)))         

(defn day11 
  ([s] (->> s
            (to-base-26)
            (iterate inc-base-26)
            (drop 1)
            (filter (fn [ds] (and (has-straight-3 ds) (has-no-iol ds) (has-two-pairs ds))))
            (first)
            (base-26-to-string)))
  ([] (day11 "hepxcrrq")))

(defn day11-2 []
  (day11 (day11)))

; DAY 12

(def input12 (slurp "data/input12.txt"))

(defn json-sum [json]
  (->> json
       (re-seq #"(-?\d+)")
       (map first)
       (map parse-int)
       (reduce +)))

(defn day12
  ([json] (json-sum json))
  ([] (day12 input12)))

