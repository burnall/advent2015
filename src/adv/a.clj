(ns adv.a)

;https://adventofcode.com/2015/

; Utility
(defn parseInt[s] (Integer/parseInt s))

(def split clojure.string/split)

(defn split-lines [text]
  (split text #"\n"))

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
      (#(map parseInt %))))

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

; DAY2 - part 2

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

