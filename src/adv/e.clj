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


