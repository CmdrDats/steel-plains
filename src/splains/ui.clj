(ns splains.ui
  (:use [quil.core])
  (:require [splains.game :as st])
  (:require [splains.cards :as cards]))

(defn hexagon [[x y] size]
  (let [c size
        a (/ c 2)
        b (* (sin (radians 60)) c)]
    (begin-shape)
    (let [vertex #(vertex (- (+ x %1) size) (- (+ y %2) size))]
      (vertex 0 b)
      (vertex a 0)
      (vertex (+ a c) 0)
      (vertex (* 2 c) b)
      (vertex (+ a c) (* 2 b))
      (vertex a (* 2 b))
      (vertex 0 b))
    (end-shape)))

(defn setup []
  (smooth)                         
  (frame-rate 2)
  (background 200)
  (set-state! :mouse-position (atom [0 0])))

(defn cell-state [state p]
  (get (:map state) p {}))

(def gamestate (atom (cards/game)))
(def selected (atom nil))

(defn draw-empty-cell [[x y] t size]
  (stroke-weight 0.1)
  (fill 0 0 0 0)
  (hexagon [x y] size)
  (stroke-weight 2)
  (fill 0 0 0 255)
  (text-size 9)
  (text t (- x 23) (- y 18)))

(defn draw-selected-cell [[x y] t size]
  (stroke-weight 1)
  (fill 56 128 128 128)
  (hexagon [x y] size)
  (stroke-weight 2)
  (fill 0 0 0 255)
  (text-size 9)
  (text t (- x 23) (- y 18)))

(defn draw-filled-cell [[posx posy] t size background]
  (stroke-weight 2)
  (apply fill background)
  (hexagon [posx posy] size)
  (fill 0 0 0 255)
  (text-size 9)
  (text t (- posx 23) (- posy 18)))

(defn text-cell [[posx posy] t]
  (fill 0 0 0 255)
  (text-size 9)
  (text t (- posx 23) (- posy 18)))

(defn selected-tile-coords [[x y] [size a b xsize]]
  (let [bx (/ (+ (/ (- x 500) xsize) (/ (- y 100) b)) 2)  
        by (/ (- (/ (- y 100) b) (/ (- x 500) xsize)) 2)]
    [(round bx) (round by)]))

(defn draw-board [game selected-tile {:keys [hand deck discard name gold res] :as player} [size a b xsize]]
  (text-cell
   [680 30]
   (str "Current player:\n\n" name "\n  "
        gold "g\n  " (apply str (map (fn [[k v]] (str k " " v "\n  ")) res))))
  (text-cell
   [800 30] (apply str "Players:\n\n" (interpose "\n" (map :name (:players game))))) 
  (doseq [x (range -4 18)
          y (range -4 18)]
    (let [p [x y]
          cell (cell-state game p)
          posx (+ 500 (- (* x xsize) (* y xsize)))
          posy (+ 100 (+ (* x b) (* y b)))]
      (if (= p selected-tile)
        (draw-selected-cell [posx posy] (str "\n" x ", " y "\n " (round posx) ":" (round posy)) size))
      (if (= p (first @selected))
        (draw-selected-cell [posx posy] (str "\n\n\n  Selected") size))
      (cond
       (= p [-3 3])
       (draw-empty-cell [posx posy] (str "\n  Lands") size)
       (and (<= y 5) (>= y 1) (= -2 x))
       (if-let [{:keys [colour title]} (nth (:land-hand game) (- 5 y))]
         (draw-filled-cell [posx posy] (str title "\n\n  " (cards/land-cost (- 5 y)) "g") size colour))
       (= p [8 15])
       (draw-empty-cell [posx posy] (str "\n  Hand: " (count hand)) size)
       (= p [9 15])
       (draw-empty-cell [posx posy] (str "\n Deck: " (count deck)) size)
       (= p [10 15])
       (draw-empty-cell [posx posy] (str "\nDiscard: " (count discard)) size)
       (and (= y 14) (>= x 7))
       (if-let [{:keys [colour title]} (nth hand (- x 7) nil)]
         (draw-filled-cell [posx posy] (str title "\n\n  ") size [128 96 96 128]))
       (= cell {}) nil
       ;(draw-empty-cell [posx posy] (str "\n" x ", " y) size)
       (contains? cell :colour)
       (draw-filled-cell
        [posx posy]
        (str (get-in cell [:land :title]) "\n\n" (:player cell) (if (:starting cell) " - S")) size (get-in cell [:land :colour] (:colour cell)))
       :else
       (draw-empty-cell [posx posy] "-" size)))))

(defn hex-calc [size]
  [size (/ size 2) (* (sin (radians 60)) size) (+ (/ size 2) size)])

(defn draw []
  (background-float 196)
  (let [precalc (hex-calc 40)]
    (let [[x y] @(state :mouse-position)
          {:keys [current-player players] :as game} @gamestate
          player (nth players current-player)]
      (stroke-weight 4)
      (ellipse x y 4 4)
      (draw-board game (selected-tile-coords [x y] precalc) player precalc))))

(defn mouse-moved []
  (let [x (mouse-x)  y (mouse-y)]
    (reset! (state :mouse-position) [x y])))

(defn place-unit [[[x y] selection] location]
  (cond 
   (= x -2) (swap! gamestate cards/play-land (:current-player @gamestate) (- 5 y) location)
   #_(= y 14 #_(swap! gamestate cards/play-car))
   ))

(defn mouse-clicked []
  (let [game @gamestate
        [x y :as p] (selected-tile-coords @(state :mouse-position) (hex-calc 40))]
    (cond
     (and (<= y 5) (>= y 1) (= -2 x))
     (reset! selected [p (nth (:land-hand game) (- 5 y))])
     (and (= y 14) (>= x 7))
     (let [card (nth (get-in game [:players (:current-player game) :hand] []) (- x 7) nil)]
       (reset! selected (if card [p card] nil)))
     (and @selected
          (>= x 0) (<= x 12) (>= y 0) (<= y 12))
     (do
       (place-unit @selected p)
       (reset! selected nil)))))

(defsketch example
  :title "Steel Plains"
  :setup setup
  :draw draw
  :mouse-moved mouse-moved
  :mouse-clicked mouse-clicked 
  :size [600 600])
