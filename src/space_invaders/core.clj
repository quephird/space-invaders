 (ns space-invaders.core
  (:import [ddf.minim Minim AudioPlayer])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn make-invaders []
  "Returns a vector of hashmaps each representing an invader"
  (into []
    (for [i (range 24)]
      {:idx i
       :x (* 75 (inc (rem i 8)))
       :y (+ 100 (* 75 (quot i 8)))})))

; TODO: Consider procedurally generating sprites.
(defn load-digit-sprites []
  (into {}
    (for [digit "0123456789"]
      [digit (q/load-image (str "resources/" digit ".png"))])))

(defn load-letter-sprites []
  (into {}
    (for [letter "GAMEOVR"]
      [letter (q/load-image (str "resources/" letter ".png"))])))

; TODO: Need to introduce level property which can be incremented.
;       Possibly introduce increasing difficulty by making speed a property
;         of the bullets too.
;       Make probability of generating a bullet a property
;         that can be "mutated" to increase difficulty
;       Consider reorganizing sprites.
;       Need provision for random big invader for extra points
(defn create-board [w h m]
  "Returns a nested hashmap representing the entire state of the game"
  {:board          {:w          w
                    :h          h}
   :player         {:x          (* 0.5 w)
                    :y          (* 0.9 h)
                    :sprite     (q/load-image "resources/player.png")
                    :sound      (.loadFile m "resources/explosion.wav")}
   :player-bullets {:locations  []
                    :sprite     (q/load-image "resources/pbullet.png")
                    :sound      (.loadFile m "resources/pew.mp3")}
   :patrol         {:invaders   (make-invaders)
                    :direction  1
                    :dx         1
                    :sprites    [(q/load-image "resources/invader1.png")
                                 (q/load-image "resources/invader2.png")]}
   :invader-bullets {:locations []
                     :sprite    (q/load-image "resources/ibullet.png")
                     :sound     (.loadFile m "resources/laser.wav")}
   :score           {:value     0
                     :sprites   (load-digit-sprites)}
   :lives           {:value     3
                     :sprite    (q/load-image "resources/playersm.png")}
   :letters         {:sprites   (load-letter-sprites)}})

(defn reset-board [{{w :w h :h} :board :as state}]
  "Returns a new version of the board with all 'mutable' values
   in their orignal state."
  (-> state
    (assoc-in [:player :x] (* 0.5 w))
    (assoc-in [:player :y] (* 0.9 h))
    (assoc-in [:patrol :invaders] (make-invaders))
    (assoc-in [:patrol :direction] 1)
    (assoc-in [:patrol :dx] 1)
    (assoc-in [:invader-bullets :locations] [])
    (assoc-in [:player-bullets :locations] [])
    (assoc-in [:score :value] 0)
    (assoc-in [:lives :value] 3)))

(defn game-over? [{{value :value} :lives :as state}]
  (zero? value))

(defn no-player-bullets? [{{locations :locations} :player-bullets}]
  (zero? (count locations)))

(defn within-player-hitbox? [{player-x :x player-y :y}
                             {bullet-x :x bullet-y :y}]
  "Returns true if the bullet's coordinates fall within a
   triangular region defined by range-x and range-y."
  (let [range-x 24
        range-y 32
        dy      (+ range-y bullet-y (- player-y))]
    (and (< (Math/abs (- bullet-y player-y)) range-y)
         (< (Math/abs (- bullet-x player-x)) (/ (* dy range-x) 2 range-y)))))

(defn within-invader-hitbox? [{invader-x :x invader-y :y}
                              {bullet-x :x  bullet-y :y}]
  "Returns true if the bullet is within the hitbox of the invader"
  (let [range-x 32
        range-y 24]
    (and (< (Math/abs (- bullet-x invader-x)) range-x)
         (< (Math/abs (- bullet-y invader-y)) range-y))))

(defn entity-shot? [entity bullet-locations hitbox-fn]
  "Returns true if any of bullets are is within the entity's hitbox"
  (->> bullet-locations
    (filter (fn [location] (hitbox-fn entity location)))
    count
    (< 0)))

(defn any-invader-shot? [bullet invaders]
  "Returns true if the bullet has hit any of the invaders"
  (->> invaders
    (filter (fn [invader] (within-invader-hitbox? invader bullet)))
    count
    (< 0)))

; TODO: Figure out how to play sound when invader is shot.
(defn check-invaders-shot [{{invaders  :invaders}  :patrol
                            {locations :locations} :player-bullets
                             score                 :score :as state}]
  "Returns a new version of game state removing all bullets
   and invaders involved in collisions"
  (let [bullets-left-over     (remove (fn [bullet] (any-invader-shot? bullet invaders)) locations)
        invaders-left-over    (remove (fn [invader] (entity-shot? invader locations within-invader-hitbox?)) invaders)
        points-scored         (* (- (count invaders) (count invaders-left-over)) 100)]
    (-> state
      (assoc-in [:player-bullets :locations] bullets-left-over)
      (assoc-in [:patrol :invaders] invaders-left-over)
      (update-in [:score :value] (fn [score] (+ score points-scored))))))

; TODO: Figure out how to destructure player and sound simulaneously.
;       Figure out how to refactor this to make this pure and do sound
;         output elsewhere.
;       Figure out how to give extra points for grazing bullets.
(defn check-player-shot [{{bullet-locations :locations} :invader-bullets
                          {w :w h :h} :board
                           player :player :as state}]
  (let [sound (player :sound)]
    (if (entity-shot? player bullet-locations within-player-hitbox?)
      (do
        (doto sound .rewind .play)
        (Thread/sleep 5000)
        (-> state
          (assoc-in  [:player :x] (* 0.5 w))
          (assoc-in  [:player :y] (* 0.9 h))
          (assoc-in  [:invader-bullets :locations] [])
          (assoc-in  [:player-bullets :locations] [])
          (update-in [:lives :value] dec)))
      state)))

(defn check-invaders-cleared [{{invaders :invaders} :patrol :as state}]
  "Returns a new version of game state with a brand new patrol
   if no invaders remain or the state unchanged."
  (if (zero? (count invaders))
    (-> state
      (assoc-in [:patrol :invaders] (make-invaders))
      (assoc-in [:patrol :direction] 1)
      (assoc-in [:patrol :dx] 1))
    state))

; TODO: Maybe make bullet-dy a property that can decrease to\
;         increase dificulty.
(defn move-player-bullets [state]
  "Returns a new version of game state by:

   * getting rid of player bullets that pass off screen, and
   * moving remaining player bullets upward"
  (let [bullet-dy 10]
    (update-in state [:player-bullets :locations]
      (fn [bullets]
        (->> bullets
          (filter (fn [bullet] (> (bullet :y) 0)))
          (map (fn [bullet] (update-in bullet [:y] (fn [y] (- y bullet-dy))))))))))

(defn move-invader-bullets [{{h :h} :board :as state}]
  "Returns a new version of game state by:

   * getting rid of invader bullets that pass off screen, and
   * moving remaining invader bullets downward"
  (update-in state [:invader-bullets :locations]
    (fn [bullets]
      (->> bullets
        (filter (fn [bullet] (< (bullet :y) h)))
        (map (fn [bullet] (update-in bullet [:y] (fn [y] (+ y 5)))))))))

; TODO: Deal better with magic numbers here
(defn change-direction? [invaders]
  (let [min-x (apply min (map #(:x %) invaders))
        max-x (apply max (map #(:x %) invaders))]
    (or (< min-x 75) (>= max-x 725))))

; TODO: Need to better manage magic numbers.
(defn move-patrol [{{curr-direction :direction
                     curr-dx        :dx
                     invaders       :invaders} :patrol :as state}]
  "Returns a new version of game state after moving the invader patrol"
  (let [change-direction (change-direction? invaders)
        new-direction (if change-direction (- curr-direction) curr-direction)
        dy            (if change-direction 32 0)
        new-dx        (/ 12 (count invaders))]
    (-> state
      (assoc-in [:patrol :direction] new-direction)
      (assoc-in [:patrol :dx] new-dx)
      (update-in [:patrol :invaders]
        (fn [invaders]
          (->> invaders
            (map (fn [invader] (update-in invader [:x] (fn [x] (+ x (* curr-dx new-direction))))))
            (map (fn [invader] (update-in invader [:y] (fn [y] (+ y dy))))))
            )))))

; TODO: Reconsider whether this needs to be a standalone function.
(defn make-invader-bullet [{x :x y :y}]
  "Randomly creates a new bullet located relative to
   the inbound invader and patrol coordinates"
  (if (> (q/random 1) 0.998)
    {:x x :y y}))

(defn generate-invader-bullets [state]
  (let [{{invaders :invaders} :patrol
         {sound :sound}       :invader-bullets} state
        new-bullets (->> invaders
                      (map (fn [invader] (make-invader-bullet invader)))
                      (filter #(not (nil? %)))
                      (into []))]
    (dotimes [_ (count new-bullets)]
      (doto sound .rewind .play))
    (-> state
      (update-in [:invader-bullets :locations] (fn [bullets] (concat bullets new-bullets))))))

; TODO: Need routine to check if invader have gotten too close to ground;
;         if so, player loses life.
(defn update-board [state]
  "Primary hook which updates the entire game state before the next frame is drawn"
  (if (game-over? state)
    state
    (-> state
      (check-player-shot)
      (check-invaders-shot)
      (check-invaders-cleared)
      (move-player-bullets)
      (move-invader-bullets)
      (move-patrol)
      (generate-invader-bullets)
      )))

(defn move-player [{{x   :x} :player
                    {w   :w} :board  :as state}
                    {key :key        :as event}]
  "Returns a new version of the board state representing
   a change in position of the player"
  (let [margin 75
        dx (cond
              (and (= key :left) (> x margin)) -10
              (and (= key :right) (< x (- w margin))) 10
              :else 0)]
    (update-in state [:player :x] (fn [x] (+ x dx)))))

(defn add-player-bullet [{{x :x y :y} :player :as state}]
  (let [pixels-above-player 48
        new-bullet {:x x :y (- y pixels-above-player)}]
    (update-in state [:player-bullets :locations] conj new-bullet)))

; TODO: Figure out why player stops moving after hitting the space key
;         with left or right key still depressed.
;       Figure out how to move sound clip playing out into main draw routine.
;       Figure out how to limit to one live bullet at a time.
(defn key-pressed [{{sound :sound} :player-bullets :as state}
                    {key           :key
                     key-code      :key-code       :as event}]
  "Primary hook to return new version of game state taking into account:

    * moving the player left or right
    * generating a new bullet"
  (cond
    (and (= :s key) (game-over? state))
      (reset-board state)
    (and (= 32 key-code) (no-player-bullets? state))
      (do
        (doto sound .rewind .play)
        (add-player-bullet state))
    (contains? #{:left :right} key)
      (move-player state event)
    :else
      state))

(defn draw-player [{{x      :x
                     y      :y
                     sprite :sprite} :player}]
  "Renders the player to the screen"
  (q/image sprite x y))

(defn draw-bullets [{bullets :locations sprite :sprite}]
  "Renders all player bullets to the screen"
  (doseq [{x :x y :y} bullets]
    (q/image sprite x y)))

; TODO: Think about how to draw exploded invaders,
;         possibly introduce :status property for each invader
;       Need to draw big ship.
(defn draw-patrol [{{invaders :invaders
                     sprites  :sprites} :patrol}]
  "Renders the entire invader patrol to the screen"
  ; This tick insures that sprites alternate over time...
  (let [tick (quot (q/frame-count) 30)]
    (doseq [{invader-idx :idx
             invader-x   :x
             invader-y   :y} invaders]
      ; ... and this logic insures that sprites alterate across the patrol.
      (let [sprite-idx (mod (+ tick invader-idx (quot invader-idx 8)) 2)]
        (q/image (sprites sprite-idx) invader-x invader-y)))))

(defn draw-score [{{value   :value
                    sprites :sprites} :score}]
  "Renders the current score to the screen"
  (q/push-matrix)
  (q/translate 25 25)
  (doseq [digit (str value)]
    (q/image (sprites digit) 0 0)
    (q/translate 32 0))
  (q/pop-matrix))

; TODO: Again with the magic numbers
(defn draw-lives [{{value  :value
                    sprite :sprite} :lives
                   {w      :w}      :board}]
  "Renders the number of lives left for the player"
  (q/push-matrix)
  (q/translate (- w 32) 32)
  (dotimes [_ value]
    (q/image sprite 0 0)
    (q/translate -32 0))
  (q/pop-matrix))

; TODO: Again with the magic numbers
;       Possibly play sad trombone clip. (BUT ONLY ONCE!)
(defn draw-game-over [{{sprites :sprites} :letters}]
  (q/background 0)
  (q/push-matrix)
  (q/translate 50 400)
  (doseq [letter "GAMEOVER"]
    (q/image (sprites letter) 0 0)
    (q/translate 100 0))
  (q/pop-matrix))

; TODO: Initalize board with set number of stars;
;         in update routine, move stars upward, randomly select whether or not to add
;         star to bottom, draw them here.
(defn draw-background [{{w :w h :h} :board}]
;  (let [])
  (q/background 0)
  (q/stroke-weight 4)
  (dotimes [_ 20]
    (q/stroke (q/random 255) 255 255)
    (q/point (q/random w) (q/random h))))

; TODO: Figure out how to implement background music.
;       Need start screen with directions.
;       Need background image.
(defn draw-board [{player-bullets  :player-bullets
                   invader-bullets :invader-bullets :as state}]
  "Primary hook to render all entities to the screen"
  (draw-background state)

  (if (game-over? state)
    (draw-game-over state)
    (do
      (draw-player state)
      (draw-bullets player-bullets)
      (draw-bullets invader-bullets)
      (draw-patrol state)
      (draw-score state)
      (draw-lives state))))

(defn setup []
  "Primary hook to configure parts of the environment
   and generate an initial game state"
  (let [w (q/width)
        h (q/height)
        m (Minim.)]
    (q/smooth)
    (q/color-mode :hsb)
    (q/image-mode :center)
    (create-board w h m)))

(q/defsketch space-invaders
  :title       "space invaders"
  :setup       setup
  :draw        draw-board
  :size        [800 800]
  :update      update-board
  :key-pressed key-pressed
  :middleware  [m/fun-mode])
