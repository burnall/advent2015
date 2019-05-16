(ns adv.e
  (:require [adv.util :as util :refer [split-lines parse-int permutations]]))

; DAY 13
(defn parse-relations [line]
  (->> line
       (re-seq #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)")
       (first)
       ((fn [[_ person1 kind value person2]]
          {:person1 person1 
          :person2 person2
          :rel ((if (= kind "gain") identity -) (parse-int value))}))))  

(defn add-to-person-ids [person-rels person] 
  (if (get person-rels person)
    person-rels
    (assoc person-rels person (count person-rels))))

(defn get-relation-config [relations]
  (->> relations
       (reduce (fn [{:keys [person-ids rels]} {:keys [person1 person2 rel]}]
                 (let [person-ids (add-to-person-ids (add-to-person-ids person-ids person1) person2)
                      id1 (get person-ids person1)
                      id2 (get person-ids person2)]
                   {:person-ids person-ids
                   :rels (assoc-in rels [id1 id2] rel)}))
                {:person-ids {}, 
                :rels (mapv (fn [_] (apply vector (repeat 10 0))) (range 10))})))

(def input13
   (->> "data/input13.txt"
         (slurp)
         (split-lines)
         (map parse-relations)
         (get-relation-config)))

(defn total-happiness [rels sitting]
  (let [last-index (dec (count sitting))
        get-rel (fn [a b] (get-in rels [(sitting a) (sitting b)]))]
    (->> last-index
         (range)
         (map #(+ (get-rel % (inc %))
                  (get-rel (inc %) %)))
         (reduce +)
         (+ (get-rel 0 last-index)
            (get-rel last-index 0)))))

(defn day13 
  ([{:keys [person-ids rels]} agg-func] 
    (->> person-ids
         (count)
         (range)
         (permutations)
         (map (partial total-happiness rels))
         (apply agg-func)))

  ([] (day13 input13 max)))

; Day 13 - Part 2
(defn tweak-person-config [{:keys [person-ids rels]}]
  {:person-ids (assoc person-ids "me" (count person-ids)) 
  :rels rels}) ; Dirty trick

(defn day13-2 
  ([config] (day13 (tweak-person-config config) max))
  ([] (day13-2 input13)))

; Day 14

(def input14 
   (->> "data/input14.txt"
         (slurp)
         (split-lines)
         (map #(re-seq #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds." %))
         (map first)
         (map (fn [[_ nickname speed span pause]] 
                {:nickname nickname
                :speed (parse-int speed)
                :span (parse-int span)
                :pause (parse-int pause)})))) 

(defn get-distance [duration {:keys [speed span pause]}]
  (+ (* speed span (quot duration (+ span pause)))
     (* speed (min (mod duration (+ span pause)) span))))       

(defn day14 
  ([reindeers duration] 
    (->> reindeers 
         (map (partial get-distance duration))
         (apply max)))
  ([] (day14 input14 2503)))

; Day 14 - part 2
(defn get-leaders [reindeers duration]
  (let [results (map (fn [r] [(:nickname r) (get-distance duration r)]) 
                      reindeers)
       max-distance (apply max (map second results))]                 
    (->> results
         (filter #(= max-distance (second %)))
         (map first))))

(defn day14-2 
  ([reindeers duration] 
    (->> (inc duration)
         (range 1)
         (mapcat (partial get-leaders reindeers))
         (frequencies)))
        
  ([] (day14-2 input14 2503)))



