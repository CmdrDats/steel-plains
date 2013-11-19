(ns splains.board)

(defn land [title move size yield colour]
  {:title title
   :move move
   :size size
   :yield yield
   :colour colour})

(def lands
  {:steelplain (land "Steel Plain" 32 8 [:colourless] [128 128 128 48])
   :outland (land "Outland" 32 32 [] [96 96 128 128])
   :plain (land "Plains" 8 32 [:white :green] [36 96 128 128])
   :mountain (land "Mountains" 32 8 [:red :green] [128 96 96 128])
   :river (land "Rivers" 16 16 [:blue :white] [36 36 128 128])
   :hill (land "Hills" 16 16 [:white :red] [96 128 96 128])
   :swamp (land "Swamp" 32 32 [:red :blue] [96 96 96 128])
   :tundra (land "Tundra" 8 16 [:green :blue] [36 128 36 128])
   :forest (land "Forest" 32 16 [:green :red] [16 128 16 128])
   :mudflat (land "Mudflats" 32 8 [:blue :white] [96 32 16 128])})

(def default-land
  {:colour [56 128 56 128]
   :type :playable
   :land (:steelplain lands)})

(def hole {})

(def outpost
  {:colour [56 56 128 128]
   :type :outland
   :land (:outland lands)})

(def nomansland
  {:colour [128 56 56 128]
   :type :nomansland
   :land (:steelplain lands)})

(defn define-location [land map coord]
  (assoc-in map [:map coord] land))

(defn place-land [land map coord]
  (assoc-in map [:map coord :land] land))

(defn plain-map [size]
  (let [map (zipmap
             (for [x (range (dec (* 2 size)))
                   y (range (max (inc (- x size)) 0) (min (+ x size) (dec (* 2 size))))]
               [x, y])
             (repeat default-land))]
    {:map map
     :size size
     :unitlist []
     :land-hand []
     :players []
     :turn 0
     :land-deck (shuffle (take (count map) (cycle (vals (dissoc lands :steelplain :outland)))))}))

(defn edged [extent mult size]
  (apply concat
         (for [x (range size)]
           [[x 0] [0 x]
            [(+ extent x) mult] [mult (+ extent x)]
            [x (+ extent x)] [(+ extent x) x]] )))

(defn starting [extent mult size]
  (let [x (/ extent 2)]
    [[0 x] [mult (+ extent x)]
     [x (+ extent x)] [(+ extent x) x]
     [(+ extent x) mult] [x 0]]))

(defn cuts [extent mult size]
  (apply concat
         (for [x (range mult)]
           [[x extent] [extent x] [x x]])))

(defn player-cuts [size players]
  (let [t (= players 3)
        extent (dec size)
        mult (* extent 2)]
    (for [x (range size)
          y (range x size)]
      [(if t [y x] [x y]) [(+ extent y) (+ extent x)]
       [y (+ extent x)] [(+ extent x) y]
       [(+ extent x) (+ extent y)] (if t [x y] [y x])])))

(defn assign-player [players map [pnum coord]]
  (let [current (get-in map [:map coord :type] :hole)]
    (case current
      :hole map
      :nomansland map
      (if (< pnum players)
        (assoc-in map [:map coord :player] pnum)
        (if (= :outland current)
          (define-location hole map coord)
          (define-location nomansland map coord))))))

(defn assign-players [players map pcoords]
  (reduce (partial #'assign-player players) map (zipmap (range) pcoords)))

(defn setup-start [map pcoords]
  (if (not= {} (get-in map [:map pcoords] {}))
    (assoc-in map [:map pcoords :starting] true)
    map))

(defn initialize [size players]
  (let [extent (dec size)
        mult (* extent 2)
        m (plain-map size)
        m (reduce (partial define-location outpost) m (edged extent mult size))
        m (reduce (partial #'assign-players players) m (player-cuts size players))
        m (reduce (partial define-location nomansland) m (cuts extent mult size))
        corners [[0 0] [0 extent] [extent 0] [extent mult] [mult extent] [mult mult]]
        m (reduce (partial define-location hole) m corners)]
    (reduce setup-start m (starting extent mult size))))
