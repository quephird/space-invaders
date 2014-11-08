(ns space-invaders.core
  (:import [ddf.minim Minim])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn make-invaders []
  "Returns a vector of hashmaps each representing an invader"
  (into []
    (for [i (range 24)]
      {:x (* 75 (rem i 8))
       :y (* 75 (quot i 8))})))

; TODO: create and load sprites for digits
(defn create-board [w h]
  "Returns a nested hashmap representing the entire state of the game"
  {:player         {:x (* 0.5 w)
                    :y (* 0.9 h)
                    :sprite (q/load-image "resources/player.png")}
   :player-bullets {:locations []
                    :sprite (q/load-image "resources/bullet.png")}
   :patrol         {:invaders (make-invaders)
                    :x 75
                    :y 75
                    :dx 1
                    :sprite (q/load-image "resources/invader.png")}
   :score           0})

; TODO: Get rid of magic numbers
;       fix lying docstring here
(defn shot? [{entity-x :x entity-y :y}
             {bullet-x :x bullet-y :y}
             {patrol-x :x patrol-y :y}]
  "Returns true if the bullet is within 50 pixels in both directions of the entity"
  (and (< (Math/abs (- bullet-x (+ entity-x patrol-x))) 32)
       (< (Math/abs (- bullet-y (+ entity-y patrol-y))) 24)))

; TODO: The next three functions are smelly; there is definitely
;       code repetition here but I wanted to get something working first.
(defn no-hits? [bullet invaders patrol-coords]
  "Returns true if the bullet has hit none of the invaders"
  (let [hits (count (filter (fn [invader] (shot? invader bullet patrol-coords)) invaders))]
    (zero? hits)))

(defn not-hit? [invader bullets patrol-coords]
  "Returns true if the invader has not been hit by any of the bullets"
  (let [hits (count (filter (fn [bullet] (shot? invader bullet patrol-coords)) bullets))]
    (zero? hits)))

(defn check-for-collisions [state]
  "Returns a new version of game state removing all bullets
   and invaders involved in collisions"
  (let [{{invaders  :invaders
          patrol-x  :x
          patrol-y  :y}  :patrol
         {locations :locations} :player-bullets
         score                  :score} state
        patrol-coords         {:x patrol-x :y patrol-y }
        bullets-left-over     (filter (fn [bullet] (no-hits? bullet invaders patrol-coords)) locations)
        invaders-left-over    (filter (fn [invader] (not-hit? invader locations patrol-coords)) invaders)
        points-scored         (* (- (count invaders) (count invaders-left-over)) 100)]
    (-> state
      (assoc-in [:player-bullets :locations] bullets-left-over)
      (assoc-in [:patrol :invaders] invaders-left-over)
      (update-in [:score] (fn [score] (+ score points-scored))))))

(defn update-bullets [state]
  "Returns a new version of game state by:

   * getting rid of player bullets that pass off screen, and
   * moving remaining player bullets upward
  "
  (update-in state [:player-bullets :locations]
    (fn [bullets]
      (->> bullets
        (filter (fn [bullet] (> (bullet :y) 0)))
        (map (fn [bullet] (update-in bullet [:y] (fn [y] (- y 5)))))))))

(defn update-patrol [state]
  "Returns a new version of game state after moving the invader patrol"
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
          )))))

(defn update-board [state]
  "Primary hook which updates the entire game state before the next frame is drawn"
  (-> state
    (check-for-collisions)
    (update-bullets)
    (update-patrol)
    ))

(defn move-player [player dx]
  "Returns a new version of the player hashmap representing a change in position"
  (update-in player [:x] (fn [x] (+ x dx))))

; TODO: Figure out how to create new bullet _only_ when space key is pressed.
(defn key-pressed [state event]
  "Primary hook to return new version of game state taking into account:

  * moving the player left or right
  * generating a new bullet
  "
  (let [key            (event :key)
        key-code       (event :key-code)
        player         (state :player)
        dx             ({:left -10 :right 10} key 0)
        new-bullet     {:x (player :x) :y (- (player :y) 48)}]
    (-> state
      (update-in [:player :x] (fn [x] (+ x dx)))
      (update-in [:player-bullets :locations] (fn [bullets] (if (= 32 key-code) (conj bullets new-bullet) bullets)))
      )))

(defn draw-player [{x :x y :y sprite :sprite}]
  "Renders the player to the screen"
  (q/image sprite x y))

(defn draw-bullets [{bullets :locations sprite :sprite}]
  "Renders all player bullets to the screen"
  (doseq [{x :x y :y} bullets]
    (q/image sprite x y)))

(defn draw-patrol [patrol]
  "Renders the entire invader patrol to the screen"
  (let [{patrol-x :x
         patrol-y :y
         invaders :invaders
         sprite   :sprite} patrol]
    (doseq [{invader-x :x invader-y :y} invaders]
      (q/image sprite (+ patrol-x invader-x) (+ patrol-y invader-y)))))

; TODO: render digits using sprites
(defn draw-score [score]
  "Renders the current score to the screen"
  (let [w       (q/width)
        h       (q/height)]
    (q/fill 255)
    (q/text-size 32)
    (q/text (str score) 10 25)))

(defn draw-board [state]
  "Primary hook to render all entities to the screen"
  (let [fc      (q/frame-count)
        w       (q/width)
        h       (q/height)
        player  (state :player)
        bullets (state :player-bullets)
        patrol  (state :patrol)
        score   (state :score)
        ]
    (q/background 0)

    (draw-player player)
    (draw-bullets bullets)
    (draw-patrol patrol)
    (draw-score score)))

(defn setup []
  "Primary hook to configure parts of the environment
   and generate an initial game state"
  (let [w (q/width)
        h (q/height)]
    (q/smooth)
    (q/image-mode :center)
    (create-board w h)))

(q/defsketch space-invaders
  :title "space invaders"
  :setup setup
  :draw draw-board
  :size [800 800]
  :update update-board
  :key-pressed key-pressed
  :middleware [m/fun-mode])
