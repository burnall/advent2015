(ns adv.f
  (:require [adv.util :as util :refer [split-lines parse-int permutations]]))

(defn parse-ingredients [line]
  (->> line
       (re-seq #"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)")
       (first)
       ((fn [[_ name capacity durability flavor texture calories]]
          {:name name 
          :capacity (parse-int capacity)
          :durability (parse-int durability)
          :flavor (parse-int flavor)
          :texture (parse-int texture)
          :calories (parse-int calories)}))))

(defn ingredient-to-vector [ingredient]
  (map ingredient [:capacity :durability :flavor :texture]))

(def input15 
  (->> "data/input15.txt"
       (slurp)
       (split-lines)
       (map parse-ingredients)))

(defn split-amount [amount boxes]
  (if (= boxes 1)
    [[amount]]
    (->> (inc amount)
         (range)
         (mapcat (fn [i] (map #(conj % i)    
                              (split-amount (- amount i) (dec boxes))))))))

(defn measure-properties [ingredients-v amounts] 
  (->> (map (fn [ingredient amount] (map (partial * amount) ingredient))  
            ingredients-v
            amounts)
       (apply map +)
       (map #(if (< % 0) 0 %))
       (reduce *)))

(defn day15
  ([ingredients amount] 
    (let [ingredients-v (map ingredient-to-vector ingredients)]
      (->>  (count ingredients-v)
            (split-amount amount)
            (map (partial measure-properties ingredients-v))
            (apply max))))


  ([] (day15 input15 100)))


; Day 15 - part 2
(defn calories? [ingredients amounts]
  (->> (map (fn [ingredient amount]
              (* (:calories ingredient) amount))
            ingredients
            amounts)
       (reduce +)
       (= 500)))

 (defn day15-2
  ([ingredients amount] 
    (let [ingredients-v (map ingredient-to-vector ingredients)]
      (->>  (count ingredients-v)
            (split-amount amount)
            (filter (partial calories? ingredients)) 
            (map (partial measure-properties ingredients-v))
            (apply max))))

  ([] (day15-2 input15 100))) 
