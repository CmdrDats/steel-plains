(ns splains.game
  (:require [splains.board :as board]))


(defn initialize [size players]
  (let [b (board/initialize size players)]
    b
    ))




