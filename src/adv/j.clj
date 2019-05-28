(ns adv.j)

; Day 21

(def items {
  1 {:name "Dagger", :cost 8, :damage 4, :armor 0}
  2 {:name "Shortsword", :cost 10, :damage 5, :armor 0}
  3 {:name "Warhammer", :cost 25, :damage 6, :armor 0}
  4 {:name "Longsword", :cost 40, :damage 7, :armor 0}
  5 {:name "Greataxe", :cost 74, :damage 8, :armor 0}
  
  6 {:name "Leather", :cost 13, :damage 0, :armor 1}
  7 {:name "Chainmail", :cost 31, :damage 0, :armor 2}
  8 {:name "Splintmail", :cost 53, :damage 0, :armor 3}
  9 {:name "Bandedmail", :cost 75, :damage 0, :armor 4}
  10 {:name "Platemail", :cost 102, :damage 0, :armor 5}

  11 {:name "Damage +1", :cost 25, :damage 1, :armor 0}
  12 {:name "Damage +2", :cost 50, :damage 2, :armor 0}
  13 {:name "Damage +3", :cost 100, :damage 3, :armor 0}
  14 {:name "Defense +1", :cost 20, :damage 0, :armor 1}
  15 {:name "Defense +2", :cost 40, :damage 0, :armor 2}
  16 {:name "Defense +3", :cost 80, :damage 0, :armor 3}})

(def weapons [1 2 3 4 5])
(def armor [6 7 8 9 10])
(def rings [11 12 13 14 15 16])

(defn carthesian [& params]
  (reduce (fn [acc p] (for [a acc, b p] (concat a b))) 
          params)) 

 (defn prep [v] 
   (mapv vector v))

(defn combine-rings [rs]
  (concat [[]]
          (prep rs)
          (->> (count rs)
               (dec)
               (range)
               (mapcat (fn [i] (carthesian [[(rs i)]] (prep (subvec rs (inc i))))))))) 

(def combinations
  (carthesian
    (prep weapons)
    (conj (prep armor) [])
    (combine-rings rings)))          

(defn join-items [a b]
  {:cost (+ (:cost a) (:cost b))
   :damage (+ (:damage a) (:damage b))
   :armor (+ (:armor a) (:armor b))})

(defn evaluate [combination]
  (->> combination
       (map items)
       (reduce join-items {:cost 0, :damage 0, :armor 0})
       (#(assoc % :ids combination))))

(defn moves-to-kill [hp damage]
  (Math/ceil (/ hp damage))) 

(defn win? [{boss-hp :hp, boss-damage :damage, boss-armor :armor} {:keys [hp damage armor]}]
  (let [afflicted (if (<= damage boss-armor) 1 (- damage boss-armor))
        received (if (<= boss-damage armor) 1 (- boss-damage armor))]
    (<= (moves-to-kill boss-hp afflicted) (moves-to-kill hp received))))      

(defn day21 
  ([hp-player boss]
    (->> combinations
         (map evaluate)
         (sort-by :cost)
         (map #(assoc % :hp hp-player))
         (filter (partial win? boss))
         (first)))
  ([] (day21 100 {:hp 104, :damage 8, :armor 1})))

; Part 2
(defn day21-2 
  ([hp-player boss]
    (->> combinations
         (map evaluate)
         (sort-by (comp - :cost))
         (map #(assoc % :hp hp-player))
         (filter (comp not (partial win? boss)))
         (first)))
  ([] (day21-2 100 {:hp 104, :damage 8, :armor 1})))
