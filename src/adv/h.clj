(ns adv.h
  (:require [adv.util :as util :refer [split-lines parse-int]]))

(defn parse-molecule [s]
  (->> s
       (re-seq #"([A-Z][a-z]*)")
       (mapv first)))

(defn get-rules [xs]
  (->> xs 
       (map (partial re-seq #"(.+) => (.+)"))
       (map first)
       (map (fn [[_ from to]] {:from from, :to (parse-molecule to)}))))
 
(defn get-ruleset [rules]
  (->> rules
       (group-by :from)
       (map (fn [[from objs]] [from (map :to objs)]))
       (into {})))

(defn build-config [xs]
  (let [last-index (dec (count xs))
        rules (get-rules (subvec xs 0 (dec last-index)))]
    {:molecule (parse-molecule (xs last-index))
     :rules rules
     :ruleset (get-ruleset rules)}))

(def input19
  (->> "data/input19.txt"
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

(defn match-rule? [molecule {to :to}] 
  (and (>= (count molecule) (count to))
       (= (subvec molecule 0 (count to))  
          to)))

(defn mutate-back [molecule i {from :from, to :to}]
  (->> [(subvec molecule 0 i)
        [from]
        (subvec molecule (+ i (count to)))]
        (apply concat)
        (vec))) 

(defn mutate-back-position [rules molecule i]
  (let [sub-molecule (subvec molecule i)]
    (->> rules
         (filter (partial match-rule? sub-molecule))
         (map (partial mutate-back molecule i)))))

(defn back-track-molecule [rules molecule]
  (->> molecule
       (count)
       (range)
       (mapcat (partial mutate-back-position rules molecule))))

(defn back-track-molecules [rules molecules]
  (->> molecules
       (mapcat (partial back-track-molecule rules))
       (distinct)))

; Also slow brute-force solution
(defn back-track-molecule-to-e [rules molecule]
  (->> [molecule]
       (iterate (partial back-track-molecules rules))
       (map (fn [i molecules] 
              (and (seq (filter (partial = ["e"])  molecules)) i)) 
            (range))
       (filter boolean)
       (first)))

(defn day19-2 
  ([{:keys [rules molecule]}] (back-track-molecule-to-e rules molecule))
  ([] (day19-2 input19)))

; Can be solved with no code - considering very particular data sample

