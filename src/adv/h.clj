(ns adv.h
  (:require [adv.util :as util :refer [split-lines parse-int]]))

(defn parse-molecule [s]
  (->> s
       (re-seq #"([A-Z][a-z]*)")
       (mapv first)))

(defn get-ruleset [xs]
  (->> xs 
       (map (partial re-seq #"(.+) => (.+)"))
       (map first)
       (map (fn [[_ from to]] {:from from, :to (parse-molecule to)}))
       (group-by :from)
       (map (fn [[from objs]] [from (map :to objs)]))
       (into {})))

(defn build-config [xs]
  (let [last-index (dec (count xs))]
    {:molecule (parse-molecule (xs last-index))
     :ruleset (get-ruleset (subvec xs 0 (dec last-index)))})) 

(def input19
  (->> "data/input19-small.txt"
       (slurp)
       (split-lines)
       (build-config)))

(defn mutate-position [molecule ruleset i]
  (->> (molecule i)
       (get ruleset)
       (map (fn [to] (vec (concat (subvec molecule 0 i) 
                                  to
                                  (subvec molecule (inc i))))))))

(defn mutate [ruleset molecule]
  (->> (count molecule)
       (range)
       (mapcat (partial mutate-position molecule ruleset))
       (distinct)))

(defn day19 
  ([{:keys [molecule ruleset]}] (count (mutate ruleset molecule)))
  ([] (day19 input19)))

; Part 2

(defn mutate-many [ruleset molecules]
  (->> molecules
       (mapcat (partial mutate ruleset))
       (distinct)))

; Appropriate for very small molecules only
(defn search-breadth [{:keys [molecule ruleset]}]
  (->> [["e"]]
       (iterate (partial mutate-many ruleset))
       (map (fn [i molecules] 
              (and (seq (filter (partial = molecule)  molecules)) i)) 
            (range))
       (filter boolean)
       (first)))

(defn day19-2 
  ([] (day19-2 input19)))



