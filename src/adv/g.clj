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
  
