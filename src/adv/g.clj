(ns adv.g
  (:require [adv.util :as util :refer [split-lines parse-int permutations]]))

(def input17
  (->> "data/input17.txt"
       (slurp)
       (split-lines)
       (map parse-int)))

(defn split-volume [[jar & more] vol]
  (cond
    (= vol 0) 1
    (nil? jar) 0
    (< vol 0) 0
    :else (+ (split-volume more vol)
             (split-volume more (- vol jar)))))

(defn day17
  ([jars vol] (split-volume jars vol))

  ([] (day17 input17 150)))

; Part 2
(defn split-volume-2 [[jar & more] vol used-jar]
  (cond
    (= vol 0) [used-jar]
    (nil? jar) []
    (< vol 0) []
    :else (concat (split-volume-2 more vol used-jar)
                  (split-volume-2 more (- vol jar) (inc used-jar)))))

(defn day17-2
  ([jars vol] (frequencies (split-volume-2 jars vol 0)))

  ([] (day17-2 input17 150)))

; DAY 18

(defn read-grid-row [line]
  (mapv (partial = \#) line))

(def input18
  (->> "data/input18.txt"
       (slurp)
       (split-lines)
       (mapv read-grid-row)))

(defn get-on-neighbors [grid row col]
  (letfn [(f [r c] 
            (if (get-in grid [r c])
               1
               0))]
    (reduce (fn [agg [dr dc]] (+ agg (f (+ row dr) (+ col dc))))
            0
            [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))

(defn next-cell [grid row col]
  (let [on-count (get-on-neighbors grid row col)
        state (get-in grid [row col])]
     (or (and state (= on-count 2)) 
         (= on-count 3)))) 

(defn next-grid-row [grid row]
  (mapv (partial next-cell grid row) 
        (range (count (grid row)))))

(defn next-grid [grid]
  (mapv (partial next-grid-row grid) 
        (range (count grid))))

(defn draw-grid [grid]
  (map (fn [row] 
         (->> row
              (map #(if % \# \.))
              (apply str)
              (prn)))
       grid))

(defn day18
  ([grid steps get-next-grid]
    (->> grid
         (iterate get-next-grid)
         (#(nth % steps))
         (mapcat identity)
         (filter boolean)
         (count)))

  ([] (day18 input18 100 next-grid)))

; Part 2

(defn next-weird-grid [grid]
  (let [last-row (dec (count grid))
        last-col (dec (count (grid 0)))]
    (-> grid 
        (next-grid)
        (assoc-in [0 0] true)
        (assoc-in [0 last-col] true)
        (assoc-in [last-row 0] true)
        (assoc-in [last-row last-col] true))))

(defn day18-2
  ([grid steps] (day18 grid steps next-weird-grid))
  ([] (day18-2 input18 100)))

