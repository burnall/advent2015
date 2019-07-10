(ns adv.k)

(def spells (sort-by :id [
  {:id 4, :name "magic missile", :mana 53,  :damage 4 :heal 0}
  {:id 5, :name "drain", :mana 73, :damage 2, :heal 2}
  {:id 3, :name "shield", :mana 113, :armor-inc 7, :effect 6}
  {:id 1, :name "poison", :mana 173, :damage 3, :effect 6}
  {:id 2, :name "recharge", :mana 229, :mana-inc 101, :effect 5}
  ]))

(defn get-initial-state [boss player]
  {:turn :player
   :boss boss
   :player (merge player {:mana-spent 0, :moves []})
   :effects []})   

(declare find-best-scenario)

(defn do-player-spell [{:keys [player boss effects], :as state} 
                   current-best 
                   {:keys [heal damage mana effect armor-inc] :as spell}]
  (let [player-changes {:hp (+ (:hp player) (or heal 0))
                        :mana-left (- (:mana-left player) mana)
                        :mana-spent (+ (:mana-spent player) mana)
                        :moves (conj (:moves player) spell)
                        :armor (+ (:armor player) (or armor-inc 0))} 
        effects (if (:effect spell) (conj effects spell) effects)
        boss (if (and (nil? effect) (:damage spell))    
               (update boss :hp (partial + (- (:damage spell))))
               boss)
        state {
            :boss boss
            :player (merge player player-changes)
            :turn :boss
            :effects effects}]
    (find-best-scenario current-best state)))

(defn get-damage [damage armor]
  (if (>= armor damage)
    1
    (- damage armor))) 

(defn apply-effect [{:keys [player boss], :as state}
                    {:keys [damage mana-inc mana effect]}]
  (let [boss (update boss :hp (partial + (- (or damage 0)))) 
        player (update player :mana-left (partial + (or mana-inc 0)))]
    (merge state {:boss boss, :player player})))    

(defn update-effects [{:keys [player effects], :as state}]
  (let [effects (map #(update % :effect dec) effects) 
        player (->> effects 
                    (filter (comp zero? :effect))
                    (filter :armor-inc)
                    (reduce (fn [p {armor-inc :armor-inc}] (update p :armor (partial + (- armor-inc))))
                            player))
        effects (filter (comp not zero? :effect) effects)]
    (merge state {:player player, :effects effects})))    

(defn apply-effects [state]
  (->> state
       :effects
       (reduce apply-effect state)
       (update-effects)))

(defn do-boss-turn [current-best state] 
  (let [{:keys [player boss effects]} (apply-effects state)
        damage (get-damage (:damage boss) (:armor player))
        player (update player :hp (partial + (- damage)))
        state {:player player, :boss boss, :turn :player, :effects effects}] 
    (find-best-scenario current-best state)))      

(defn filter-spells [active-effects spells]
  (filter (fn [spell] (every? (fn [act] (not= (:id act) (:id spell))) 
                              active-effects))
            spells))

(defn do-player-turn [current-best state]
  ; Here is changes for part 2
  (let [hp (dec (get-in state [:player :hp]))]
    (if (zero? hp)
      current-best
      (let [state (assoc-in state [:player :hp] hp) 
            state (apply-effects state)]
        (->> spells 
             (filter-spells (:effects state)) 
             (reduce (partial do-player-spell state) 
                     current-best))))))

(defn branch [current-best state]
  (if (= :boss (:turn state))
    (do-boss-turn current-best state)
    (do-player-turn current-best state)))

(defn find-best-scenario [{:keys [min-mana-spent] :as current-best} 
                          {:keys [turn boss player effects] :as state}]
  (prn 77 state) (newline)
  (prn 88 current-best) (newline)
  (cond
    ;(> (count (:moves player)) 12) current-best 
    (>= (:mana-spent player) min-mana-spent) current-best
    (<= (:hp player) 0) current-best
    (<= (:hp boss) 0) {:min-mana-spent (:mana-spent player), :moves (:moves player)} 
    (and (= turn :player) (<= (:mana-left player) 0)) current-best 
    :else (branch current-best state)))
     
(defn day22
  ([boss player] 
    (find-best-scenario {:moves [], :min-mana-spent Integer/MAX_VALUE}
                         (get-initial-state boss player)))
  ;([] (day22 {:hp 13, :damage 8} {:hp 10, :mana-left 250, :armor 0})))

  ([] (day22 {:hp 58, :damage 9} {:hp 50, :mana-left 500, :armor 0})))

