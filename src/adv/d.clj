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

; Day 12 - part 2
(defn char-to-int [c] (- (int c) (int \0)))

(defn digit? [c] (and (>= (int c) (int \0)) (<= (int c) (int \9)))) 

(defn parse-integer-int [acc xs]
  (if (digit? (first xs))
    (recur (+ (* 10 acc) (char-to-int (first xs))) (rest xs))
    {:rst xs, :value acc}))

(defn parse-integer [negative? acc xs] 
  (let [{xs :rst, value :value} (parse-integer-int acc xs)]
    {:rst xs, :value ((if negative? - identity) value)})) 

(defn parse-string-int [acc [x & xs]]
  (if (= x \")
    {:rst xs, :value acc}
    (recur (conj acc x) xs)))

(defn parse-string [xs]
  (let [{rst :rst, value :value} (parse-string-int [] xs)]
    {:rst rst, :value (apply str value)}))

(declare parse-object-int)
(declare parse-array-int)

(defn parse-value [x xs]
  (cond
    (digit? x) (parse-integer false (char-to-int x) xs)
    (= x \-) (parse-integer true 0 xs)
    (= x \{) (parse-object-int {} xs)
    (= x \[) (parse-array-int [] xs)
    (= x \") (parse-string xs)))

(defn parse-array-int [acc [x & xs]]
  (if (= x \]) 
    {:rst xs, :value acc}
    (let [{[x & xs] :rst, value :value} (parse-value x xs)
          acc (conj acc value)]
      (if (= x \,)
        (recur acc xs)
        {:rst xs, :value acc})))) ; Skip implied ']'

(defn parse-object-int [acc [x & xs]] 
  (if (= x \})
    {:rst xs, :value acc}
    (let [{xs :rst, property-key :value} (parse-string xs) ; Ignore x, should be '"'
          [x & xs] (rest xs) ; Skip implied ':' 
          {[x & xs] :rst, property-value :value} (parse-value x xs)
          acc (assoc acc property-key property-value)]
      (if (= x \,)
        (recur acc xs)
        {:rst xs, :value acc})))) ; Skip implied '}'

(defn parse-json [[x & xs]]
  (condp = x 
    \{ (parse-object-int {} xs)
    \[ (parse-array-int [] xs)))

(defn has-red? [obj]
  (->> obj
      (keys)
      (filter (fn [a-key] (= (obj a-key) "red")))
      (seq)))

(defn traverse-json [json]
  (cond 
    (int? json) json
    (string? json) 0
    (vector? json) (reduce + (map traverse-json json))
    (map? json) 
      (if (has-red? json) 
        0
        (reduce + (map #(traverse-json (json %)) (keys json)))))) 

(defn day12-2
  ([json-string] 
    (->> json-string
         (parse-json)
         (:value)
         (traverse-json)))
  ([] (day12-2 input12)))
  
