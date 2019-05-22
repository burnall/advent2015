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
 
