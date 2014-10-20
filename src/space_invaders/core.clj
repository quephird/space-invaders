(ns space-invaders.core
  (:import [ddf.minim Minim])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn make-invaders []
  (into []
    (for [i (range 24)]
      {:x (* 75 (rem i 8))
       :y (* 75 (quot i 8))})))

; TODO: load sprites for each entity
(defn create-board [w h]
  {:player         {:x (* 0.5 w)
                    :y (* 0.9 h)}
   :player-bullets []
   :patrol         {:invaders (make-invaders)
                    :x 75
                    :y 75
                    :dx 1}})

(defn setup []
  (let [w (q/width)
        h (q/height)]
    (q/smooth)
    (q/rect-mode :center)
    (q/frame-rate 60)
;    (q/no-loop)
    (create-board w h)))

(defn update-bullets [state]
  ; This:
  ;
  ; 1) gets rid of bullets that pass off screen, and
  ; 2) moves remaining bullets upward.
  (update-in state [:player-bullets]
    (fn [bullets]
      (->> bullets
        (filter (fn [bullet] (> (bullet :y) 0)))
        (map (fn [bullet] (update-in bullet [:y] (fn [y] (- y 5)))))
        )
      )
    )
  )

(defn update-patrol [state]
  (update-in state [:patrol]
    (fn [patrol]
      ; This seems awfully hacky but I couldn't figure
      ; out how better to capture current state
      (let [curr-x  (patrol :x)
            curr-dx (patrol :dx)
            new-dx (if (or (< curr-x 75)
                           (>= curr-x 200))
                     (- curr-dx)
                     curr-dx)]
        (-> patrol
          (update-in [:dx] (fn [dx] new-dx))
          (update-in [:x] (fn [x] (+ x new-dx)))
          )
        )
      )
    )
  )

(defn update-board [state]
  (-> state
    (update-bullets)
    (update-patrol)
    ))

(defn move-player [player dx]
  (update-in player [:x] (fn [x] (+ x dx))))

(defn key-pressed [state event]
  (let [key            (event :key)
        key-code       (event :key-code)
        player         (state :player)
        player-bullets (state :player-bullets)
        dx             ({:left -10 :right 10} key 0)
        new-bullet     {:x (player :x) :y (player :y)}]
    (-> state
      (update-in [:player :x] (fn [x] (+ x dx)))
      (update-in [:player-bullets] (fn [bullets] (if (= 32 key-code) (conj bullets new-bullet) bullets)))
      )))

; TODO: draw sprite
(defn draw-player [{x :x y :y}]
  (q/fill 0)
  (q/rect x y 50 50))

; TODO: draw sprites
(defn draw-bullets [bullets]
  (doseq [{x :x y :y} bullets]
    (q/fill 0)
    (q/rect x y 10 10)
    )
  )

; TODO: draw sprites
(defn draw-patrol [patrol]
  (let [{patrol-x :x
         patrol-y :y
         invaders :invaders} patrol]
    (doseq [{invader-x :x invader-y :y} invaders]
      (q/fill 0)
      (q/rect (+ patrol-x invader-x) (+ patrol-y invader-y) 50 50)
      )
    )
  )

(defn draw-board [state]
  (let [fc      (q/frame-count)
        w       (q/width)
        h       (q/height)
        player  (state :player)
        bullets (state :player-bullets)
        patrol  (state :patrol)
        ]
    (q/background 255)

    (draw-player player)
    (draw-bullets bullets)
    (draw-patrol patrol)
    )
  )

(q/defsketch space-invaders
  :title "space invaders"
  :setup setup
  :draw draw-board
  :size [800 800]
  :update update-board
  :key-pressed key-pressed
  :middleware [m/fun-mode])
