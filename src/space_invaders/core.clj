 (ns space-invaders.core
  (:import [ddf.minim Minim AudioPlayer])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn make-invaders []
  "Returns a vector of hashmaps each representing an invader"
  (into []
    (for [i (range 24)]
      {:x (* 75 (inc (rem i 8)))
       :y (+ 100 (* 75 (quot i 8)))})))

(defn load-digit-sprites []
  (into {}
    (for [digit "0123456789"]
      [digit (q/load-image (str "resources/" digit ".png"))])))

; TODO: Need to introduce level property which can be incremented.
;       Possibly introduce increasing difficulty by making speed a property
;         of the bullets too.
;       Make probability of generating a bullet a property
;         that can be "mutated" to increase difficulty
(defn create-board [w h m]
  "Returns a nested hashmap representing the entire state of the game"
  {:player         {:x (* 0.5 w)
                    :y (* 0.9 h)
                    :sprite (q/load-image "resources/player.png")}
   :player-bullets {:locations []
                    :sprite (q/load-image "resources/pbullet.png")
                    :sound (.loadFile m "resources/pew.mp3")}
   :patrol         {:invaders (make-invaders)
                    :direction 1
                    :dx 1
                    :sprite (q/load-image "resources/invader.png")}
   :invader-bullets {:locations []
                    :sprite (q/load-image "resources/ibullet.png")}
   :score           {:value 0
                     :sprites (load-digit-sprites)}
   :lives           {:value  3
                     :sprite (q/load-image "resources/playersm.png")}})

; TODO: Better manage magic numbers.
(defn shot? [{entity-x :x entity-y :y}
             {bullet-x :x bullet-y :y}]
  "Returns true if the bullet is within the hitbox of the entity"
  (and (< (Math/abs (- bullet-x entity-x)) 32)
       (< (Math/abs (- bullet-y entity-y)) 24)))

; TODO: The next three functions are smelly; there is definitely
;       code repetition here but I wanted to get something working first.
(defn no-hits? [bullet invaders]
  "Returns true if the bullet has hit none of the invaders"
  (let [hits (count (filter (fn [invader] (shot? invader bullet)) invaders))]
    (zero? hits)))

(defn not-hit? [invader bullets]
  "Returns true if the invader has not been hit by any of the bullets"
  (let [hits (count (filter (fn [bullet] (shot? invader bullet)) bullets))]
    (zero? hits)))

; TODO: Check to see if enemy bullet has hit player
(defn check-for-collisions [state]
  "Returns a new version of game state removing all bullets
   and invaders involved in collisions"
  (let [{{invaders  :invaders}  :patrol
         {locations :locations} :player-bullets
         score                  :score} state
        bullets-left-over     (filter (fn [bullet] (no-hits? bullet invaders)) locations)
        invaders-left-over    (filter (fn [invader] (not-hit? invader locations)) invaders)
        points-scored         (* (- (count invaders) (count invaders-left-over)) 100)]
    (-> state
      (assoc-in [:player-bullets :locations] bullets-left-over)
      (assoc-in [:patrol :invaders] invaders-left-over)
      (update-in [:score :value] (fn [score] (+ score points-scored))))))

(defn move-player-bullets [state]
  "Returns a new version of game state by:

   * getting rid of player bullets that pass off screen, and
   * moving remaining player bullets upward"
  (update-in state [:player-bullets :locations]
    (fn [bullets]
      (->> bullets
        (filter (fn [bullet] (> (bullet :y) 0)))
        (map (fn [bullet] (update-in bullet [:y] (fn [y] (- y 5)))))))))

; TODO: Think about how to make this pure
(defn move-invader-bullets [state]
  "Returns a new version of game state by:

   * getting rid of invader bullets that pass off screen, and
   * moving remaining invader bullets downward"
  (let [h (q/height)]
    (update-in state [:invader-bullets :locations]
      (fn [bullets]
        (->> bullets
          (filter (fn [bullet] (< (bullet :y) h)))
          (map (fn [bullet] (update-in bullet [:y] (fn [y] (+ y 5))))))))))

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
        new-direction (if change-direction
                        (- curr-direction)
                        curr-direction)
        dy            (if change-direction
                        32
                        0)]
    (-> state
      (assoc-in [:patrol :direction] new-direction)
      (update-in [:patrol :invaders]
        (fn [invaders]
          (->> invaders
            (map (fn [invader] (update-in invader [:x] (fn [x] (+ x (* curr-dx new-direction))))))
            (map (fn [invader] (update-in invader [:y] (fn [y] (+ y dy))))))
            )))))

; TODO: Reconsider whether this needs to be a standalone function.
(defn generate-new-bullet [{x :x y :y}]
  "Randomly creates a new bullet located relative to
   the inbound invader and patrol coordinates"
  (if (> (q/random 1) 0.995)
    {:x x :y y}))

(defn generate-invader-bullets [state]
  (let [{{invaders :invaders} :patrol} state
        new-bullets (->> invaders
                      (map (fn [invader] (generate-new-bullet invader)))
                      (filter #(not (nil? %)))
                      (into []))]
    (-> state
      (update-in [:invader-bullets :locations] (fn [bullets] (concat bullets new-bullets))))))

(defn update-board [state]
  "Primary hook which updates the entire game state before the next frame is drawn"
  (-> state
    (check-for-collisions)
    (move-player-bullets)
    (move-invader-bullets)
    (move-patrol)
    (generate-invader-bullets)
    ))

; TODO: Prevent the player from moving off screen
(defn move-player [player dx]
  "Returns a new version of the player hashmap representing a change in position"
  (update-in player [:x] (fn [x] (+ x dx))))

; TODO: Figure out why player stops moving after hitting the space key
;       with left or right key still depressed.
(defn key-pressed [{player :player
                   {sound :sound} :player-bullets :as state}
                   {key :key key-code :key-code}]
  "Primary hook to return new version of game state taking into account:

  * moving the player left or right
  * generating a new bullet"
  (let [dx             ({:left -10 :right 10} key 0)
        new-bullet     {:x (player :x) :y (- (player :y) 48)}]
    (cond
      (= 32 key-code)
        (do
          (doto sound .rewind .play)
          (update-in state [:player-bullets :locations] conj new-bullet))
      (contains? #{:left :right} key)
        (update-in state [:player :x] (fn [x] (+ x dx)))
      :else
        state)))

(defn draw-player [{x :x y :y sprite :sprite}]
  "Renders the player to the screen"
  (q/image sprite x y))

(defn draw-bullets [{bullets :locations sprite :sprite}]
  "Renders all player bullets to the screen"
  (doseq [{x :x y :y} bullets]
    (q/image sprite x y)))

(defn draw-patrol [patrol]
  "Renders the entire invader patrol to the screen"
  (let [{invaders :invaders
         sprite   :sprite} patrol]
    (doseq [{invader-x :x invader-y :y} invaders]
      (q/image sprite invader-x invader-y))))

(defn draw-score [{value :value sprites :sprites}]
  "Renders the current score to the screen"
  (q/push-matrix)
  (q/translate 25 25)
  (doseq [digit (str value)]
    (q/image (sprites digit) 0 0)
    (q/translate 32 0))
  (q/pop-matrix))

(defn draw-lives [{value :value sprite :sprite}]
  "Renders the number of lives left for the player"
  (let [w (q/width)]
    (q/push-matrix)
    (q/translate (- w 32) 32)
    (dotimes [_ value]
      (q/image sprite 0 0)
      (q/translate -32 0))
    (q/pop-matrix)))

(defn draw-board [state]
  "Primary hook to render all entities to the screen"
  (let [fc      (q/frame-count)
        w       (q/width)
        h       (q/height)
        {player          :player
         player-bullets  :player-bullets
         invader-bullets :invader-bullets
         patrol          :patrol
         score           :score
         lives           :lives} state]
    (q/background 0)

    (draw-player player)
    (draw-bullets player-bullets)
    (draw-bullets invader-bullets)
    (draw-patrol patrol)
    (draw-score score)
    (draw-lives lives)
    ))

(defn setup []
  "Primary hook to configure parts of the environment
   and generate an initial game state"
;  (q/frame-rate 1)
  (let [w (q/width)
        h (q/height)
        m (Minim.)]
    (q/smooth)
    (q/image-mode :center)
    (create-board w h m)))

(q/defsketch space-invaders
  :title "space invaders"
  :setup setup
  :draw draw-board
  :size [800 800]
  :update update-board
  :key-pressed key-pressed
  :middleware [m/fun-mode])
