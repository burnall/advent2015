(ns adv.k)

(def spells [
  {:id 1, :name "magic missile", :mana 53,  :damage 4, :effect 0}
  {:id 2, :name "drain", :mana 73, :damage 2, :effect 0, :heal 2}
  {:id 3, :name "shield", :mana 113, :armor-inc 7, :effect 6}
  {:id 4, :name "poison", :mana 173, :damage 3, :effect 6}
  {:id 5, :name "recharge", :mana 229, :mana-inc 101, :effect 5}])

(defn initial-state [boss player]
  {:turn :player
   :mana-spent 0 
   :boss boss
   :player player
   :effects #{}})

(defn apply-armor-inc [spell [boss player]]
  [boss player]
)

(defn apply-effect [[boss player] spell]
  (prn boss player spell)
  (->> [boss player]
       (apply-armor-inc spell)))

(defn apply-effects [{:keys [boss player effects]}]
  (prn boss player effects)
  (reduce apply-effect 
      [boss player]
       effects))

(defn search-best-variation [{:keys [min-mana-spent cnt]} {:keys [mana-spent turn boss player effects] :as state}]
  (cond 
    ;(>= mana-spent min-mana-spent) min-mana-spent
    ;(<= (:hp player) 0) min-mana-spent
    ;(<= (:hp boss) 0) mana-spent
    :else (apply-effects (assoc state :effects #{3 4 5}))
))

(defn day22
  ([boss player] 
    (search-best-variation {:cnt 0, :min-mana-spent Integer/MAX_VALUE}
                           (initial-state boss player)))

  ([] (day22 {:hp 58, :damage 9} {:hp 50, :mana 500, :armor 0})))
