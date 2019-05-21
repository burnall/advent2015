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

; Day 16

(defn parse-sues [line]
  (->> line
       (re-seq #"Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)")
       (first)
       ((fn [[_ id sign1 amount1 sign2 amount2 sign3 amount3]]
         {:id id, :signs [[sign1 amount1] [sign2 amount2] [sign3 amount3]]}))
       ((fn [obj] 
          (reduce (fn [agg [sign amount]]
                    (assoc agg (keyword sign) (parse-int amount)))
                  {:id (:id obj)}
                  (:signs obj)))))) 

(def input16 
  (->> "data/input16.txt"
       (slurp)
       (split-lines)
       (map parse-sues)))

(def input16-2
  (->> "data/input16-2.txt"
       (slurp)
       (split-lines)
       (map (partial re-seq #"(\w+): (\d+)"))
       (map first)
       (map (fn [[_ sign amount]] [(keyword sign) (parse-int amount)]))))

(defn default-predicate-for-sign [sign]
  (fn [sue amount]
    (or (nil? (sign sue))
        (= (sign sue) amount))))

(defn build-predicate [signs predicate-for-sign]
  (fn [sue]
    (reduce (fn [res [sign amount]]
              (or ((predicate-for-sign sign) sue amount)
                  (reduced false)))
            true
            signs)))

(defn day16
  ([sues signs]
    (->> sues 
         (filter (build-predicate signs default-predicate-for-sign))))
  ([] (day16 input16 input16-2)))

; Day 16 - part 2
(defn other-predicate-for-sign [sign]
  (cond 
    (or (= sign :cats) (= sign :trees))
      (fn [sue amount]
        (or (nil? (sign sue))
            (> (sign sue) amount)))
    (or (= sign :pomeranians) (= sign :goldfish))
      (fn [sue amount]
        (or (nil? (sign sue))
            (< (sign sue) amount)))
    :default  
      (fn [sue amount]
        (or (nil? (sign sue))
            (= (sign sue) amount)))))

(defn day16-2
  ([sues signs]
    (->> sues 
         (filter (build-predicate signs other-predicate-for-sign))))
  ([] (day16-2 input16 input16-2)))

