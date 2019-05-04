(ns adv.b)

; Utility
(defn parse-int [s] (Integer/parseInt s))

(def split clojure.string/split)

(defn split-lines [text]
  (split text #"\n"))

; DAY 6
(def re-command #"(.+) (\d+),(\d+) through (\d+),(\d+)")

(defn parse-command [[_ verb x1 y1 x2 y2]]
  {:verb verb
  :a [(parse-int x1) (parse-int y1)]
  :b [(parse-int x2) (parse-int y2)]})

(def input6 
  (->> "data/input6.txt"
       (slurp)
       (split-lines)
       (map (partial re-find re-command))
       (map parse-command)))

(defn get-initial-grid []
  (->> 1000
       (range)
       (mapv (fn [_] (vec(repeat 1000 0))))))
       
(defn apply-cmd [verb-func grid {:keys [a b verb]}]
  (let [[x1 y1] a
        [x2 y2] b
        f (verb-func verb)] 
    (reduce (fn [g [x y]] (update-in g [y x] f))
            grid
            (for [y (range y1 (inc y2))
                 x (range x1 (inc x2))] [x y]))))

; Part 1
(defn verb-func1 [verb]
  (case verb
    "turn off" (fn [v] 0)
    "turn on"  (fn [v] 1)
    (fn [v] (- 1 v))))

(defn count-lights [grid]
  (->> grid
       (flatten)
       (filter #(= 1 %))
       (count)))

(defn day6 
  ([cmds] (count-lights (reduce (partial apply-cmd verb-func1) 
                                (get-initial-grid) 
                                cmds)))
  ([] (day6 input6)))
  
; Part 2
(defn verb-func2 [verb]
  (case verb
    "turn off" (fn [v] (if (zero? v) 0 (dec v)))
    "turn on" inc
    (comp inc inc)))

(defn overall-brightness [grid]
  (->> grid
       (flatten)
       (reduce +)))

(defn day6-2 
  ([cmds] (overall-brightness (reduce (partial apply-cmd verb-func2) 
                                (get-initial-grid) 
                                cmds)))
  ([] (day6-2 input6)))
 
