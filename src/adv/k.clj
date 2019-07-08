(ns adv.k)

(def spells [
  {:id 1, :name "magic missile", :mana 53,  :damage 4 :heal 0}
  {:id 2, :name "drain", :mana 73, :damage 2, :heal 2}
  ;{:id 3, :name "shield", :mana 113, :armor-inc 7, :effect 6}
  ;{:id 4, :name "poison", :mana 173, :damage 3, :effect 6}
  ;{:id 5, :name "recharge", :mana 229, :mana-inc 101, :effect 5}
  ])

(defn get-initial-state [boss player]
  {:turn :player
   :boss boss
   :player (merge player {:mana-spent 0, :moves []})
   :effects #{}})

(defn do-player-spell [{player :player, boss :boss, moves :moves, :as state} 
                   current-best 
                   {:keys [heal damage mana effect] :as spell}]
  (when (nil? effect)  
    (let [player-changes {:hp (+ heal (:hp player))
                          :mana-left (- (:mana-left player) mana)
                          :mana-spent (+ (:mana-spent player) mana)
                          :moves (conj moves spell)}
          boss (update boss :hp (partial + (- damage)))
          state (merge state {:boss boss, :player player, :turn :boss})]
      (find-best-scenario current-best state))))   

(defn get-damage [damage armor]
  (if (>= armor damage)
    1
    (- damage armor))) 

(defn apply-effects [{:keys [player boss effects] :as state}]
  state)

(defn do-boss-turn [current-best state] 
  (let [{:keys [player boss]} (apply-effects state)
        damage (get-damage (:damage boss) (:armor player))
        player (update player :hp (partial + (- damage)))
        state (merge state {:player player, :boss boss, :turn :player})] 
    (find-best-scenario current-best state)))      

(defn do-player-turn [current-best state] 
  (let [{:keys [player boss]} (apply-effects state)]
    (->> spells 
         (reduce (partial do-player-spell state) 
                 current-best))))

(defn branch [current-best state]
  (if (= :boss (:turn state))
    (do-boss-turn current-best state)
    (do-player-turn current-best state)))

(defn find-best-scenario [{:keys [min-mana-spent moves] :as current-best} 
                          {:keys [turn boss player effects] :as state}]
  (prn state current-best)
  (cond 
    (>= (:mana-spent player) min-mana-spent) current-best
    (<= (:hp player) 0) current-best
    (<= (:hp boss) 0) {:min-mana-spent (:mana-spent player), :moves moves} 
    :else (branch current-best state)))
     
(defn day22
  ([boss player] 
    (find-best-scenario {:moves [], :min-mana-spent Integer/MAX_VALUE}
                         (get-initial-state boss player)))

  ([] (day22 {:hp 58, :damage 9} {:hp 50, :mana-left 500, :armor 0})))

