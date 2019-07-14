(ns adv.l
  (:require [adv.util :refer [split split-lines parse-int is-digit]]))

; DAY 23

(defn get-value [s]
  (if (or (= (first s) \+) (= (first s) \-)) 
    (parse-int s)
    (keyword s)))

(defn make-instruction 
  ([cmd op1] {:cmd (keyword cmd), :op1 (get-value op1)})
  ([cmd op1 op2] (assoc (make-instruction cmd op1) :op2 (get-value op2)))) 

(defn parse-instruction [line]
  (-> line 
      (split #"[ ,]+") 
      (#(apply make-instruction %)))) 

(def input23 
  (->> "data/input23.txt"
       (slurp)
       (split-lines)
       (mapv parse-instruction)))

(defn exec-cmd [cmds {line :line, :as state}] 
  (prn state)
  (let [{:keys [cmd op1 op2]} (cmds line)]
    (case cmd
      :hlf (merge state {op1 (quot (op1 state) 2), :line (inc line)})
      :tpl (merge state {op1 (* 3 (op1 state)), :line (inc line)})
      :inc (merge state {op1 (inc (op1 state)), :line (inc line)})
      :jmp (assoc state :line (+ line op1))
      :jie (assoc state :line 
                  (if (zero? (mod (op1 state) 2))
                    (+ line op2)
                    (inc line)))
      :jio (assoc state :line 
                  (if (= 1 (op1 state))
                    (+ line op2)
                    (inc line))))))

(defn run-program
  ([cmds] (->> {:a 1N, :b 0N, :line 0}
               (iterate (partial exec-cmd cmds))
               (drop-while #(and (>= (:line %) 0) (> (count cmds) (:line %))))
               (first)))

  ([] (run-program input23)))

