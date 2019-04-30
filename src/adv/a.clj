(ns adv.a)

;https://adventofcode.com/2015/

; Utility
(defn parseInt[s] (Integer/parseInt s))

(def split clojure.string/split)

(defn split-lines [text]
  (split text #"\n"))

; DAY 1 - part 1
(def input1 (slurp "data/input1.txt"))

(defn day1 
  ([s] (let [freq (frequencies s)]
         (- (get freq \( 0)
            (get freq \) 0)))) 
         
  ([] (day1 input1)))

; DAY 1 - part 2
(defn day2
  ([s] (->> s
            (map #(if (= % \() 1 -1))
            (reductions +)
            (take-while #(not= % -1))
            (count)
            (inc)))
           
  ([] (day2 input1)))

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



