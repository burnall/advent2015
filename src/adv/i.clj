(ns adv.i)

(defn divided? [factors n]
  (let [maxf (int (Math/sqrt n))]
    (->> factors
         (take-while #(<= % maxf))
         (map (partial mod n))
         (some zero?))))

(defn primes [limit] 
  (->> (range 2 limit)
       (reduce (fn [acc i] 
                 (if (divided? acc i)
                   acc
                   (conj acc i)))
               [])))    

(def prime-table (primes 1000000))
(def prime-set (set prime-table))

(defn get-prime-factors 
  ([n] (get-prime-factors n []))
  ([n factors]
    (if (prime-set n) 
      (conj factors n)
      (let [f (->> prime-table
                   (filter #(when (zero? (mod n %)) %))
                   (first))]
        (recur (quot n f) (conj factors f))))))

(defn join-factors [fa fb]
  (for [a fa 
        b fb]
    (* a b)))    

(defn get-exp-factors [base exp]
  (->> (iterate (partial * base) 1)
       (take (inc exp))))

(defn filter-factors-default [n factor]
  true)

(defn get-sum-all-factors [filter-factors n]
  (if (prime-set n)
    (inc n)
    (->> (get-prime-factors n)
         (frequencies)
         (map (partial apply get-exp-factors))
         (reduce join-factors [1])
         (filter (partial filter-factors n))
         (reduce +))))
              
(defn day20 
  ([goal limit filter-factors] 
    (->> limit
         (range 2)
         (map (juxt (partial get-sum-all-factors filter-factors) identity))
         (filter (fn [[sumf n]] (>= sumf goal)))
         (first)))
  ([goal limit] (day20 goal limit filter-factors-default)))       

; Part 2
(defn filter-factors-by-quote [quote n factor]
   (>= quote (/ n factor)))  

(defn day20-2 [goal limit quote]
  (day20 goal limit (partial filter-factors-by-quote quote)))


