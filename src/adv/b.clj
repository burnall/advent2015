(ns adv.b
  (:require [adv.util :refer [split-lines parse-int is-digit]]))

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

; DAY 7
(defn bit-not-16 [n]
  (- 65535 n))

(defn bit-shift-left-16 [n times]
  (-> n
     (bit-shift-left times)
     (bit-and 65535)))

(def clause-re #"((\d+)|(\S+) (AND|OR) (\S+)|NOT (\S+)|(\S+) (LSHIFT|RSHIFT) (\d+)|(\S+)) -> (\S+)")

(defn parse-var [clause v1 v2]
  (-> clause
      (#(if (is-digit (first v1)) 
          (assoc % :e1 (parse-int v1))
          (assoc % :w1 v1)))
       (#(if (is-digit (first v2)) 
          (assoc % :e2 (parse-int v2))
          (assoc % :w2 v2)))))

(defn read-clause [line]
  (let [m (re-find clause-re line)
        to (m 11)]
    (cond 
      (some? (m 2)) {:t :signal, :signal (parse-int (m 2)), :to to}
      (some? (m 6)) {:t :not, :w1 (m 6), :to to}
      (some? (m 3)) (parse-var {:t :binary, :op (if (= (m 4) "AND") bit-and bit-or), :to to} (m 3) (m 5))
      (some? (m 7)) {:t :func, :op (if (= (m 8) "RSHIFT") bit-shift-right bit-shift-left-16), :w1 (m 7), :p1 (parse-int (m 9)), :to to}
      (some? (m 10)) {:t :wire, :w1 (m 10), :to to})))

(def input7 
  (->> "data/input7.txt"
       (slurp)
       (split-lines)
       (map read-clause)))

(defn get-simple-wires [clauses]
  (->> clauses
       (filter #(= (:t %) :signal))
       (map #(vector (:to %) (:signal %)))
       (into {})))

(defn get-initial-circuit [clauses]
  {:solved-wires (get-simple-wires clauses) ; map
   :clauses (set clauses) ; set
  }) 

(defn solve-clause [clause solved-wires]
  (case (:t clause)
    :signal (:signal clause)
    :not (when-let [s1 (solved-wires (:w1 clause))] 
           (bit-not-16 s1))
    :binary (let [s1 (or (:e1 clause) (solved-wires (:w1 clause))) 
              s2 (or (:e2 clause) (solved-wires (:w2 clause)))]
              (when (and (some? s1) (some? s2)) ((:op clause) s1 s2)))
    :func (when-let [s1 (solved-wires (:w1 clause))]
            ((:op clause) s1 (:p1 clause)))  
    :wire (solved-wires (:w1 clause))))


(defn search-clauses [{:keys [solved-wires clauses]}]
  (reduce (fn [[sw active-clauses] clause] 
             (if-let [solved (solve-clause clause sw)]
               [(assoc sw (:to clause) solved) (disj active-clauses clause)]
               [sw active-clauses]))
          [solved-wires clauses]
          clauses))

(defn solve-circuit [{:keys [solved-wires clauses] :as circuit}]
  (let [[sw cls] (search-clauses circuit)]
    (if (= (count clauses) (count cls))
      solved-wires
      (recur {:solved-wires sw, :clauses cls}))))

; Part 1
(defn day7 
  ([clauses] (solve-circuit (get-initial-circuit clauses)))
  ([] (day7 input7)))

; Part 2
(defn day7-2 
  ([clauses] 
    (let [base (solve-circuit (get-initial-circuit clauses))
          a-signal (base "a")]
      (->> clauses
           (filter #(not= "b" (:to %)))
           (#(conj % {:t :signal, :signal a-signal, :to "b"}))
           (get-initial-circuit)
           (solve-circuit))))
      
  ([] (day7-2 input7)))
  
