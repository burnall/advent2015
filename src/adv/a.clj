(ns adv.a
  (:require [adv.util :refer [split split-lines parse-int is-digit]]))

; htps://adventofcode.com/2015/

; DAY 1

(def input1 (slurp "data/input1.txt"))

; DAY 1 - part 1
(defn day1
  ([s] (let [freq (frequencies s)]
         (- (get freq \( 0)
            (get freq \) 0)))) 
         
  ([] (day1 input1)))

; DAY 1 - part 2
(defn day1-2
  ([s] (->> s
            (map #(if (= % \() 1 -1))
            (reductions +)
            (take-while #(not= % -1))
            (count)
            (inc)))
           
  ([] (day1-2 input1)))

; DAY 2
(defn parse-dim-line [s] 
  (-> s
      (split #"x")
      (#(map parse-int %))))

(def input2 
  (->> "data/input2.txt"
       (slurp)
       (split-lines) 
       (map parse-dim-line)))

; DAY 2 - part 1
(defn get-total-wrapping [dimensions] 
  (let [[a b c] (sort dimensions)]
    (+ (* 2 (+ (* a b) (* a c) (* b c))) 
       (* a b))))

(defn day2 
  ([presents] (->> presents
                   (map get-total-wrapping)
                   (reduce +)))
  ([] (day2 input2))) 

; DAY 2 - part 2
(defn get-total-ribbon [dimensions] 
  (let [[a b c] (sort dimensions)]
    (+ (* a b c)
       (* 2 (+ a b)))))


(defn day2-2
  ([presents]  (->> presents
                    (map get-total-ribbon)
                    (reduce +)))

  ([] (day2-2 input2)))

; DAY 3
(def input3 (clojure.string/trim-newline (slurp "data/input3.txt")))

(defn get-initial-state []
  {:pos [0 0] 
  :visited {[0 0] 1}}) 

(def moves {\> [1 0],  \< [-1 0], \^ [0 -1], \v [0 1]})

(defn walk-a-trip [trip initial-state]
  (reduce (fn [{:keys [pos visited]} ch] 
            (let [new-pos (map + pos (moves ch))]
              {:pos new-pos
              :visited (update-in visited [new-pos] #(if % (inc %) 1))}))
          initial-state
          trip))

; DAY 3 - part 1
(defn day3 
  ([trip] (->> (get-initial-state)
               (walk-a-trip trip)
               (:visited)
               (count)))
  ([] (day3 input3)))             

; DAY 3 - part 2
(defn transpose [xs] (apply map list (partition 2 xs)))

(defn day3-2 
  ([trip] (let [[odd-trip even-trip] (transpose trip)
                state (assoc (walk-a-trip odd-trip (get-initial-state)) :pos [0 0])]
            (->> state    
                 (walk-a-trip even-trip)
                 (:visited)
                 (count))))
  ([] (day3-2 input3)))

; DAY 4 
(defn hash-string
  "Use java interop to flexibly hash strings"
  [string algo base]
  (let [hashed
        (doto (java.security.MessageDigest/getInstance algo)
          (.reset)
          (.update (.getBytes string)))]
    (format "%032x" (new java.math.BigInteger 1 (.digest hashed)))))

(defn hash-md5
  "Generate a md5 checksum for the given string"
  [string]
  (hash-string string "MD5" 16))

(defn day4 
  ([prefix, zero-count] (->> (range)
                 (map (fn [i] [i (hash-md5 (str prefix i))]))
                 (filter (fn [[i md5]] (= (apply str (repeat zero-count \0))  (subs md5 0 zero-count))))
                 (take 1)))
  ([zero-count] (day4 "bgvyzdsv" zero-count))) 
 
 ; DAY 5
(def input5 (split-lines (slurp "data/input5.txt")))
 
(defn pred1 [s] 
 (let [vowels #{\a \e \i \o \u}]
   (= 3
   (reduce (fn [cnt ch]
             (if (vowels ch) 
               (if (= cnt 2) 
                 (reduced 3)
                 (inc cnt))
               cnt))
           0
           s))))
 
 (defn pred2 [s]
   (= true
     (reduce (fn [prev ch] 
               (if (= prev ch) 
                 (reduced true)
                 ch))
             s)))

(defn pred3 [s]
  (not (re-find #"ab|cd|pq|xy" s)))

(defn day5 
   ([words] (->> words
                 (filter pred1)
                 (filter pred2)
                 (filter pred3)
                 (count)))
   ([] (day5 input5)))

; DAY 5 - part 2
(defn pred4 [s]
  (some? (re-find #"(..).*\1" s))) 

(defn pred5 [s]
  (some? (re-find #"(.).\1" s)))

(defn day5-2 
   ([words] (->> words
                 (filter pred4)
                 (filter pred5)
                 (count)))
   ([] (day5-2 input5)))


