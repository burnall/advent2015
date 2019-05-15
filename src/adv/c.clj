(ns adv.c
  (:require [adv.util :as util :refer [split-lines parse-int permutations]]))

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

; Part 1

(defn get-code-representation-size [s]
  (count s))

(defn day8
  ([lines] (->> lines
                (map #(- (get-code-representation-size %) (get-in-memory-size %)))
                (reduce + 0)))
  ([] (day8 input8)))

; Part 2
(defn get-encoded-code-representation-size [s]
  (let [patterns (re-seq #"\\." s)]
    (+ (count s)
       4
       (reduce + 
               0
               (map #(case (second %) 
                      \\ 2
                      \" 2
                      1)
                      patterns)))))

(defn day8-2
  ([lines] (->> lines
                (map #(- (get-encoded-code-representation-size %) (get-code-representation-size %)))
                (reduce + 0)))

  ([] (day8-2 input8)))

 ; DAY 9

(defn parse-distance [line] 
  (let [[_ town1 town2 distance]  (re-find #"(\w+) to (\w+) = (\d+)" line)]
    {:town1 town1, :town2 town2, :distance (parse-int distance)}))

(defn add-to-town-ids [town-ids town] 
  (if (get town-ids town)
    town-ids
    (assoc town-ids town (count town-ids))))

(defn get-distance-config [distance-objs]
  (->> distance-objs
      (reduce (fn [{:keys [town-ids distances]} {:keys [town1 town2 distance]}]
                (let [town-ids (add-to-town-ids (add-to-town-ids town-ids town1) town2)
                      id1 (get town-ids town1)
                      id2 (get town-ids town2)]
                  {:town-ids town-ids
                  :distances (assoc-in (assoc-in distances [id1 id2] distance) [id2 id1] distance)}))
              {:town-ids {}, 
              :distances (mapv (fn [_] (apply vector (repeat 10 0))) (range 10))})))

(def input9 
   (->>  "data/input9.txt"
         (slurp)
         (split-lines)
         (map parse-distance)
         (get-distance-config)))

(defn total-distance [distances route]
  (->> (dec (count route))
       (range)
       (map #(get-in distances [(route %) (route (inc %))]))
       (reduce +))) 

(defn day9 
  ([{:keys [town-ids distances]} agg-func] 
    (->> town-ids
         (count)
         (range)
         (permutations)
         (map (partial total-distance distances))
         (apply agg-func)))

  ([] (day9 input9 min)))

; Part 2
(defn day9-2 [] (day9 input9 max)) 

; Day 10

(defn number-to-digits [n]
  (->> n
       (str)
       (map #(- (int %) (int (char \0))))))  

(defn look-and-say [digits]
  (->> digits
       (partition-by identity)
       (mapcat (fn [ds] [(count ds) (first ds)]))))

(defn day10 
  ([n times] 
    (->> n
         (number-to-digits)
         (iterate look-and-say)
         (#(nth % times))
         (count)))
  ([] (day10 3113322113 40)))

(defn day10-2 [] 
  (day10 3113322113 50))


