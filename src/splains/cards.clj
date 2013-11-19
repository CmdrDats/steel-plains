(ns splains.cards
  (:require [splains.board :as board]))

(def ^:dynamic context {})

(defn ctx [] context)
(defmacro bind-ctx
  [ctx & body]
  `(binding [context ~ctx]
     ~@body))

(defn update-unit [ctx unit f & args]
  (if-let [i (.indexOf (get-in ctx [:game :unitlist]) unit)]
    (do
      (apply update-in ctx [:game :unitlist i] f args))))

(defmacro defconstraint [name params & body]
  `(defn ~name ~params (fn [] ~@body)))

(defconstraint owner []
  (= (:player (ctx)) (get-in (ctx) [:unit :player])))

(defconstraint title [t]
  (= t (get-in (ctx) [:unit :title])))

(def adjacent-tiles [[0 0] [0 1] [1 0] [-1 0] [0 -1] [1 1] [-1 -1]])

;; Matches units that are adjacent to any of the given units 
(defconstraint adjacent [units]
  (let [loc (get-in (ctx) [:unit :location])]
    (loop [us units
           [t & ts] adjacent-tiles]
      (cond
       (empty? us) false
       (nil? t) (recur (rest us) adjacent-tiles)
       (= (map + loc t) (:location (first us))) true
       :else
       (recur us ts)
       ))))

(defconstraint c-or [& constraints]
  (loop [[c & cs] constraints]
    (cond
     (nil? c) false
     (c) true
     :else (recur cs))))

(defconstraint c-and [& constraints]
  (loop [[c & cs] constraints]
    (cond
     (nil? c) true
     (c) (recur (cs))
     :else false)))

(defconstraint property [property]
  (contains? (set (get-in (ctx) [:unit :properties] [])) property))

(defn properties [& properties]
  (apply c-and (map property properties)))

(defn current-player []
  [(:player (ctx))])

(defn unit-players [units]
  (set (map :player units)))

(defn buff-units [unitlist effect]
  (reduce (fn [c u] (update-unit c u effect)) (ctx) unitlist))

(defn buff [unitlist & effects]
  (reduce
   (fn [c e]
     (bind-ctx c (buff-units unitlist e)))
   (ctx) effects))

(defn matches-constraints [constraints unit]
  (bind-ctx
   (assoc-in (ctx) [:unit] unit)
   (empty? (filter #(false? (%)) constraints))))

(defn units [& constraints]
  (doall
   (filter (partial #'matches-constraints constraints) (get-in (ctx) [:game :unitlist]))))

(defn unit []
  (:unit (ctx) {}))

(defn targets []
  (:targets (ctx) []))

(defn lose-game [players]
  (reduce 
   #(update-in %1 [:game :players %2] assoc :killed true)
   (ctx) players))

;; Unit Effects
(defn add-property [type value]
  (fn [u]
    (update-in u [type] (fnil + 0) value)))

(defn damage [damage]
  (add-property :health (- 0 damage)))

(defn attack [attack]
  (add-property :attack attack))

(defn movement [movement]
  (add-property :movement movement))

(defn tmp-property [type value turncount]
  (fn [u]
    (update-in u [:temp-buffs] conj {:type type :value value :turncount turncount})))

(defn health [health]
  (fn [u]
    (-> (update-in u [:max-health] + health)
        (update-in [:health] + health))))

(defn poison [targets turncount]
  (buff targets (add-property :poisoned turncount)))

(defn heal [health]
  (fn [u] (update-in u [:health] #(min (:max-health u) (+ % health)))))
 
(defn destroy [targets]
  (let [t (set targets)]
    (update-in (ctx) [:game :unitlist] #(filter (complement (partial contains? t)) %))))

(defn remove-from-game [targets]
  (let [t (set targets)]
    (update-in (ctx) [:game :unitlist] #(filter (complement (partial contains? t)) %))))

(defn attacker []
  [(:attacker (ctx) [])])

(defn add-card [card pnum destination]
  (update-in
   (ctx) [:game :players pnum destination] conj
   (assoc card :ephemeral true)))

;;When enchanted creature is destroyed, it is returned to owners hand.
;;If it was killed by damage from another unit, that unit is returned
;;to its owner's hand.
;; {:killed
;;  (rule
;;   (if (attacker)
;;     (add-card (find-card (:title (unit))) (owner) :hand)))}

;; (rule
;;  (destroy (targets)))

;; {:damage
;;  (buff (unitlist (units (adjacent (this)))) (damage 1))

;;  :attacked
;;  (rule (poison (attacker) 3))}
;; When Bitter Root is attacked, attacking unit becomes poisoned for 3
;;  rounds.
;;
;;(rule (buff (property [:ranged :lobber) (movement 2)))
;; All units with Ranged or Lobber attack have their Countdown decreased by 2.


;; Deal 3 damage to target unit. If that unit is destroyed, draw 1
;; scroll.

;;Target creature gets +2 Attack until end of turn for each opponent
;;unit on the same row. It is then dealt 1 damage.
;; {:rules
;;  {:cast
;;   `(buff (targets) (eot-attack (* 2 (count (adjacent units)))))}}


;; Location
(defn location [unitlist]
  {:type :location
   :fn (fn [game unit]
         (let [k ((:fn unitlist) game unit)]
           (cond
            (coll? k) (first (shuffle k))
            :else k)))})

;; Effects
(defn summon [card location])

(defn find-card [title])

(defmacro rule [body]
  `(quote ~@body))

;; Card definition functions
(defn card [title type card-text flavour-text subtypes cost targets rules]
  (assoc rules
    :title title
    :card-text card-text
    :flavour-text flavour-text
    :type type
    :subtypes subtypes
    :targets targets
    :cost cost))

(defn unit [title card-text flavour-text subtypes cost health attack movement & {:as rules-and-abilities}]
  (assoc (card title :unit card-text flavour-text subtypes cost [:location] rules-and-abilities)
    :health health
    :attack attack
    :movement movement))

(defn structure [title card-text flavour-text subtypes cost health attack & {:as rules-and-abilities}]
  (assoc (card title :structure card-text flavour-text subtypes cost [:location] rules-and-abilities)
    :health health
    :attack attack))

(defn enchantment [title card-text flavour-text subtypes cost targets & {:as rules}]
  (card title :enchantment card-text flavour-text subtypes cost targets rules))

(defn spell [title card-text flavour-text subtypes cost targets & {:as rules}]
  (card title :enchantment card-text flavour-text subtypes cost targets rules))

;; These are rather direct ports from Scrolls - Only as illustration
;; and for making sure the basic mechanics work. Will be switched out later.
(def carddefs
  [(structure "Ancestral Totem" "While {this} is in play, all of its owners units get +1 attack" ""
              [:totem] {:green 4} 2 0
              :rules
              {:in-play
               (rule (buff (units (owner)) (attack 1)))})

   (enchantment "Bear Paw" "When {this} enters play, add 1 to movement of all owners movement. While {this} is in play add +2 attack and +2 health to all owners units" ""
                [] {:green 2} [:structure :unit]
                :rules
                {:on-enter (rule (buff (target) (movement 1)))
                 :in-play (rule (buff (target) (attack 2) (health 2)))})

   (unit "Brother of the Wolf" "Instead of moving, {this} can summon a Ragged Wolf instead"
             ""
             [:human :kinfolk] {:green 4} 4 3 2
             :abilities 
             [{:title "Summon Wolf" ;;:when (property :movement 2)
               :activated (rule (summon (find-card "Ragged Wolf") (location (this))))}])
   
   (unit "Ragged Wolf" "" "Ragged she comes." [:beast :wolf] {:green 1} 2 1 2)
   
   (board/land "Gold Mine" 16 16 [:gold] [128 32 128 128])
   
   (spell "Hymn" "Heal target unit by 3 health" ""
          [] {:green 1} [:unit]
          :rules {:on-cast (rule (buff (target) (heal 3)))})
   
   (unit "Wreaking Lion"
             "When {this} enters play, {this} deals 1 damage to all of its owners units. When leaving play, heals 1 to owners units. While {this} is in play, +1 attack on all of owners lions"
             "When a lion wreaks havoc" [:beast :lion] {:white 2 :red 1} 2 3 4
             :properties [:haste]
             :rules
             {:on-enter (rule (buff (units (owner)) (damage 1)))
              :in-play (rule (buff (units (and (owner) (subtype :lion))) (attack 1)))
              :on-remove (rule (buff (units (owner)) (heal 1)))})])

(defn deck [])



(def outpost-card
  (structure
   "Outpost" "If all your outposts are destroyed, you lose the game" "" [] {} 10 0
   :rules
   {:on-remove
    (rule (if (empty? (units (owner) (title "Outpost")))
            (lose-game (owner))))}))

(defn instance-card [game player card]
  (assoc card
    :owner player
    :played-turn (:turn game)))

(defn place-land [game player land location]
  (board/place-land land game location))

(defn land-cost [land-idx]
  (+ 10 (* land-idx 7)))

(defn pay-gold [game player-idx gold]
  (update-in game [:players player-idx :gold] - gold))

(defn replenish-lands [{hand :land-hand deck :land-deck :as game}]
  (let [count (- 5 (count hand))]
    (-> game
        (update-in [:land-hand] #(concat % (take count deck)))
        (update-in [:land-deck] #(drop count %)))))

(defn play-land [game player-idx land-idx location]
  (let [land (nth (:land-hand game) land-idx)
        {gold :gold :as player} (get-in game [:players player-idx])
        {type :type player :player} (get-in game [:map location])]
    (if (and (or (nil? player) (= player-idx player))
             (not= type :outland)
             (>= gold (land-cost land-idx)))
      (->
       (pay-gold game player-idx (land-cost land-idx))
       (place-land player-idx land location)
       (update-in [:land-hand] #(concat (take land-idx %) (drop (inc land-idx) %)))
       (replenish-lands))
      game)))

(defn place-unit [game player unit location]
  (update-in game [:unitlist] conj (assoc unit :location location)))

(defn play-card [game player-idx card-idx location])


(defn draw-card [game player-idx]
  (update-in
   game [:players player-idx]
   (fn [{:keys [deck discard hand] :as player}]
     (let [[[card & deck] discard] (if (empty? deck) [(shuffle discard) '()] [deck discard])]
       (assoc player :deck deck :discard discard :hand (conj hand card))))))

(defn draw-cards [game player-idx count]
  (case count
    0 game
    (recur (draw-card game player-idx) player-idx (dec count))))

(defn discard-card [game player card-idx]
  (update-in
   game [:players player]
   (fn [{:keys [discard hand] :as p}]
     (assoc p
       :discard (conj discard (nth hand card-idx))
       :hand (concat (take card-idx hand) (drop (inc card-idx) hand))))))

(defn add-player [game name deck]
  (update-in game [:players]
             #(conj % {:name name :hand '() :deck (shuffle deck) :discard '() :gold 50})))

(defn pick-starting-player [{players :players :as game}]
  (assoc-in game [:current-player] (Math/round (rand (dec (count players))))))

(defn draw-player-cards 
  ([game cnt] (draw-player-cards game (range (count (:players game))) cnt))
  ([game players cnt]
     (cond
      (empty? players) game
      :else (recur (draw-cards game (first players) cnt) (rest players) cnt))))

(defn place-outposts [game]
  game)

(defn game []
  (-> (board/initialize 7 2)
      (replenish-lands)
      (add-player "CmdrDats" (take (* 7 (count carddefs)) (cycle carddefs)))
      (add-player "Sanchymoo" (take (* 7 (count carddefs)) (cycle carddefs)))
      (draw-player-cards 5)
      (pick-starting-player)
      (place-outposts)))
