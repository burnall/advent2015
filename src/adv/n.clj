(ns adv.n)

; Day 25
(defn diag-number [row col]
  (let [row-start (+ row col -1)
        complete-rows (dec row-start)
        triangle (quot (* complete-rows (inc complete-rows)) 2)]
    (+ triangle col)))        

(defn next-val [n]
  (mod (* 252533 n) 33554393))

(defn day25 
  ([row col] (nth (iterate next-val 20151125)
                  (dec (diag-number row col))))

  ([] (day25 2978 3083)))

