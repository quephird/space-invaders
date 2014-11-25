(ns space-invaders.core
  (:import [ddf.minim Minim AudioPlayer])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn make-stars [w h]
  "Returns a vector of hashmaps each representing a background star"
  (into []
    (for [i (range 100)]
      {:x (q/random w)
       :y (q/random h)})))

(defn make-invaders []
  "Returns a vector of hashmaps each representing an invader"
  (let [start-x 0
        start-y 150]
    (into []
      (for [i (range 24)]
        {:idx i
         :x (+ start-x (* 75 (inc (rem i 8))))
         :y (+ start-y (* 75 (quot i 8)))}))))

; TODO: Consider procedurally generating sprites.
(defn load-digit-sprites []
  (into {}
    (for [digit "0123456789"]
      [digit (q/load-image (str "resources/" digit ".png"))])))

(defn load-letter-sprites []
  (into {}
    (for [letter "GAMEOVR"]
      [letter (q/load-image (str "resources/" letter ".png"))])))

(defn load-mystery-ship-sprites []
  (into []
    (for [i (range 1 6)]
      (q/load-image (str "resources/mystery" i ".png")))))

; TODO: Possibly introduce increasing difficulty by making speed a property
;         of the bullets too.
;       Make probability of generating a bullet a property
;         that can be "mutated" to increase difficulty
;       Consider reorganizing sprites.
;       Need provision for random big invader for extra points
(defn create-board [w h m]
  "Returns a nested hashmap representing the entire state of the game"
  {:board          {:w            w
                    :h            h
                    :music        (.loadFile m "resources/level.mp3")}
   :stars           (make-stars w h)
   :player         {:x            (* 0.5 w)
                    :y            (* 0.9 h)
                    :sprite       (q/load-image "resources/player.png")}
   :player-bullets {:locations    []
                    :sprite       (q/load-image "resources/pbullet.png")}
   :patrol         {:invaders     (make-invaders)
                    :direction    1
                    :dx           1
                    :sprites      [(q/load-image "resources/invader1.png")
                                   (q/load-image "resources/invader2.png")]}
   :invader-bullets {:locations   []
                     :sprite      (q/load-image "resources/ibullet.png")}
   :mystery-ship    {:location    nil
                     :sprites     (load-mystery-ship-sprites)}
   :boss-ship       {:location    {:x 400 :y 400}
                     :direction-x 1
                     :direction-y 1
                     :hits-left   25
                     :sound       (.loadFile m "resources/boom.wav")
                     :sprites     [(q/load-image "resources/boss1.png")
                                   (q/load-image "resources/boss2.png")]}
   :score           {:value       0
                     :sprites     (load-digit-sprites)}
   :level           {:value       1
                     :sprites     (load-digit-sprites)}
   :lives           {:value       3
                     :sprite      (q/load-image "resources/playersm.png")}
   :letters         {:sprites     (load-letter-sprites)}
   :sounds          {:new-player-bullet   (.loadFile m "resources/pew.mp3")
                     :new-invader-bullet  (.loadFile m "resources/laser.wav")
                     :new-mystery-ship    (.loadFile m "resources/klaxon.mp3")
                     :new-mystery-bullet  (.loadFile m "resources/mlaser.wav")
                     :new-boss-bullet     (.loadFile m "resources/blaser.wav")
                     :invader-dead        (.loadFile m "resources/boom.wav")
                     :invader-landed      (.loadFile m "resources/landing.wav")
                     :player-dead         (.loadFile m "resources/explosion.wav")}
   :events           []})

(defn reset-board [{{w :w h :h} :board :as state}]
  "Returns a new version of the board with all 'mutable' values
   in their orignal state."
  (-> state
    (assoc-in [:events] [])
    (assoc-in [:player :x] (* 0.5 w))
    (assoc-in [:player :y] (* 0.9 h))
    (assoc-in [:patrol :invaders] (make-invaders))
    (assoc-in [:patrol :direction] 1)
    (assoc-in [:patrol :dx] 1)
    (assoc-in [:invader-bullets :locations] [])
    (assoc-in [:player-bullets :locations] [])
    (assoc-in [:score :value] 0)
    (assoc-in [:level :value] 1)
    (assoc-in [:lives :value] 3)))

(defn game-over? [{{value :value} :lives :as state}]
  (zero? value))

(defn regular-level? [{{value :value} :level :as state}]
  (and (not (zero? (mod value 3)))
       (not (game-over? state))))

(defn boss-level? [{{value :value} :level :as state}]
  (and (zero? (mod value 3))
       (not (game-over? state))))

(defn invaders-reached-bottom? [{{h        :h}        :board
                                 {invaders :invaders} :patrol :as state}]
  (->> invaders (map :y) (apply max) (< (- h 100))))

(defn no-player-bullets? [{{locations :locations} :player-bullets}]
  (zero? (count locations)))

(defn clear-previous-events [state]
  (assoc-in state [:events] []))

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

(defn within-boss-hitbox? [{boss-x   :x boss-y :y}
                           {bullet-x :x bullet-y :y}]
  "Returns true if the bullet is within the hitbox of the invader"
  (let [range-x 150
        range-y 100]
    (and (< (Math/abs (- bullet-x boss-x)) range-x)
         (< (Math/abs (- bullet-y boss-y)) range-y))))

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

(defn check-invaders-shot [{{invaders  :invaders}  :patrol
                            {locations :locations} :player-bullets
                             score                 :score :as state}]
  "Returns a new version of game state removing all bullets
   and invaders involved in collisions"
  (let [bullets-left-over     (remove (fn [bullet] (any-invader-shot? bullet invaders)) locations)
        invaders-shot         (filter (fn [invader] (entity-shot? invader locations within-invader-hitbox?)) invaders)
        invaders-left-over    (remove (set invaders-shot) invaders)
        points-scored         (* (count invaders-shot) 100)]
    (-> state
      (assoc-in [:player-bullets :locations] bullets-left-over)
      (assoc-in [:patrol :invaders] invaders-left-over)
      (update-in [:events] concat (repeat (count invaders-shot) :invader-dead))
      (update-in [:score :value] (fn [score] (+ score points-scored))))))

(defn check-boss-shot [{{boss-location :location
                         hits-left     :hits-left
                         sound         :sound}    :boss-ship
                        {locations :locations}  :player-bullets
                         score                  :score :as state}]
  "Returns a new version of game state removing all hitting player bullets"
  (let [bullets-missed      (remove (fn [bullet] (entity-shot? boss-location locations within-boss-hitbox?)) locations)
        bullets-hit         (filter (fn [bullet] (entity-shot? boss-location locations within-boss-hitbox?)) locations)
        points-scored         (* (count bullets-hit) 100)]
    (doseq [_ bullets-hit]
      (doto sound .rewind .play))
    (-> state
      (assoc-in [:player-bullets :locations] bullets-missed)
      (assoc-in [:boss-ship :hits-left] (- hits-left (count bullets-hit)))
      (update-in [:score :value] (fn [score] (+ score points-scored))))))

; TODO: Figure out how to give extra points for grazing bullets.
(defn check-player-shot [{{w :w      h :h}       :board
                          {locations :locations} :invader-bullets
                           player                :player :as state}]
    (if (entity-shot? player locations within-player-hitbox?)
      (-> state
        (assoc-in  [:player :x] (* 0.5 w))
        (assoc-in  [:player :y] (* 0.9 h))
        (assoc-in  [:invader-bullets :locations] [])
        (assoc-in  [:player-bullets :locations] [])
        (assoc-in  [:mystery-ship :location] nil)
        (update-in [:events] conj :player-dead)
        (update-in [:lives :value] dec))
      state))

(defn check-invaders-reached-bottom [state]
  (if (invaders-reached-bottom? state)
      (-> state
        (update-in [:events] conj :invader-landed)
        (assoc-in [:lives :value] 0))
    state))

(defn check-invaders-cleared [{{invaders :invaders} :patrol :as state}]
  "Returns a new version of game state with a brand new patrol
   if no invaders remain or the state unchanged."
  (if (zero? (count invaders))
    (-> state
      (update-in [:level :value] inc)
      (assoc-in [:patrol :invaders] (make-invaders))
      (assoc-in [:patrol :direction] 1)
      (assoc-in [:patrol :dx] 1))
    state))

(defn check-boss-dead [{{hits-left :hits-left} :boss-ship :as state}]
  (if (zero? hits-left)
    (-> state
      (update-in [:level :value] inc)
      (assoc-in [:boss-ship :location] {:x 400 :y 400})
      (assoc-in [:boss-ship :hits-left] 25)
      (assoc-in [:patrol :invaders] (make-invaders))
      (assoc-in [:patrol :direction] 1)
      (assoc-in [:patrol :dx] 1))
    state))

(defn move-stars [{stars :stars :as state}]
  (assoc-in state [:stars]
    (into []
      (map (fn [star] (update-in star [:y] (fn [y] (- y 2)))) stars))))

(defn update-stars [{{w :w h :h} :board
                      stars      :stars :as state}]
  (let [new-star (if (< (q/random 1) 0.25) [{:x (q/random w) :y h}])]
    (-> state
      (assoc-in [:stars] (into [] (remove (fn [{y :y}] (< y 0)) stars)))
      (assoc-in [:stars] (concat stars new-star)))))

; TODO: Maybe make bullet-dy a property that can decrease to increase dificulty.
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

(defn change-patrol-direction? [{{w        :w}        :board
                                 {invaders :invaders} :patrol}]
  (let [margin 75
        min-x  (apply min (map #(:x %) invaders))
        max-x  (apply max (map #(:x %) invaders))]
    (or (< min-x margin) (>= max-x (- w margin)))))

(defn move-patrol [{{curr-direction :direction
                     curr-dx        :dx
                     invaders       :invaders} :patrol :as state}]
  "Returns a new version of game state after moving the invader patrol"
  (let [change-direction (change-patrol-direction? state)
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

(defn move-mystery-ship [{{location :location} :mystery-ship :as state}]
  (if (nil? location)
    state
    (update-in state [:mystery-ship :location :x] (fn [x] (+ x 5)))))

; TODO: Reconsider whether this needs to be a standalone function.
(defn make-invader-bullet [{x :x y :y}]
  "Randomly creates a new bullet located relative to
   the inbound invader and patrol coordinates"
  (if (> (q/random 1) 0.998)
    {:x x :y y}))

(defn move-boss-ship [{{w :w h :h} :board
                       {{x :x y :y} :location
                        direction-x :direction-x
                        direction-y :direction-y} :boss-ship :as state}]
  (let [margin-x 200
        margin-y 200
        new-direction-x (if (or (< (- x margin-x) 0) (> (+ x margin-x) w))
                             (- direction-x)
                             direction-x)
        new-direction-y (if (or (< (- y margin-y) 0) (> (+ y margin-y 100) h))
                             (- direction-y)
                             direction-y)
        new-x           (+ x (* 2 new-direction-x))
        new-y           (+ y (* 2 new-direction-y))]
    (-> state
      (assoc-in [:boss-ship :location :x] new-x)
      (assoc-in [:boss-ship :location :y] new-y)
      (assoc-in [:boss-ship :direction-x] new-direction-x)
      (assoc-in [:boss-ship :direction-y] new-direction-y))
    ))

(defn generate-invader-bullets [{{invaders :invaders} :patrol :as state}]
  (let [new-bullets (->> invaders
                      (map (fn [invader] (make-invader-bullet invader)))
                      (filter #(not (nil? %)))
                      (into []))]
    (-> state
      (update-in [:events] concat (repeat (count new-bullets) :new-invader-bullet))
      (update-in [:invader-bullets :locations] (fn [bullets] (concat bullets new-bullets))))))

(defn generate-mystery-ship-bullets [{{location :location} :mystery-ship
                                      {sound    :sound}    :invader-bullets :as state}]
  (cond
    (nil? location)
      state
    (> (q/random 1) 0.05)
      state
    :else
      (let [new-bullets (into [] (map (fn [n] (update-in location [:x] (fn [x] (+ x n)))) [-30 0 30]))]
        (-> state
          (update-in [:invader-bullets :locations] concat new-bullets)
          (update-in [:events] conj :new-mystery-bullet)))))

(defn generate-boss-bullets [{{{x :x y :y} :location} :boss-ship :as state}]
  (if (> (q/random 1) 0.1)
    state
    (let [bullet-x   (+ x -200 (q/random 255))
          bullet-y   (+ y (q/random 50))
          new-bullet {:x bullet-x :y bullet-y}]
      (-> state
        (update-in [:invader-bullets :locations] conj new-bullet)
        (update-in [:events] conj :new-boss-bullet)))))

(defn update-mystery-ship [{{w        :w}        :board
                            {location :location} :mystery-ship :as state}]
  "Returns a new version of the state with either a new mystery ship
   or it's being removed from the board."
  (cond
    (and (nil? location) (zero? (mod (q/frame-count) 1000)))
      (-> state
        (assoc-in [:mystery-ship :location] {:x -128 :y 75})
        (update-in [:events] conj :new-mystery-ship))
    (> (:x location 0) (+ w 128))
      (-> state
        (assoc-in [:mystery-ship :location] nil)
        (update-in [:events] conj :mystery-ship-gone))
    :else
      state))

(defn update-board [state]
  "Primary hook which updates the entire game state before the next frame is drawn"
  (cond
    (regular-level? state)
      (-> state
        clear-previous-events
        check-player-shot
        check-invaders-reached-bottom
        check-invaders-shot
        check-invaders-cleared
        move-stars
        move-invader-bullets
        move-player-bullets
        move-patrol
        move-mystery-ship
        update-stars
        generate-invader-bullets
        generate-mystery-ship-bullets
        update-mystery-ship
        )
    (boss-level? state)
      (-> state
        clear-previous-events
        check-player-shot
        check-boss-dead
        check-boss-shot
        move-boss-ship
        move-invader-bullets
        move-player-bullets
        generate-boss-bullets)
    :else
      (clear-previous-events state)))

(defn move-player [{{x   :x} :player
                    {w   :w} :board  :as state}
                    {key :key        :as event}]
  "Returns a new version of the board state representing
   a change in position of the player"
  (let [margin 125
        dx (cond
              (and (= key :left) (> x margin)) -10
              (and (= key :right) (< x (- w margin))) 10
              :else 0)]
    (update-in state [:player :x] (fn [x] (+ x dx)))))

; TODO: Figure out how to move sound clip playing out into main draw routine.
(defn add-player-bullet [{{x :x y :y} :player
                          sounds      :sounds :as state}]
  (let [pixels-above-player 48
        new-bullet {:x x :y (- y pixels-above-player)}]
    (doto (:new-player-bullet sounds) .rewind .play)
    (-> state
      (update-in [:player-bullets :locations] conj new-bullet))))

; TODO: Figure out why player stops moving after hitting the space key
;         with left or right key still depressed.
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
      (add-player-bullet state)
    (contains? #{:left :right} key)
      (move-player state event)
    :else
      state))

(defn play-background-music [{{music :music} :board :as state}]
  (if (not (.isLooping music))
    (doto music .unmute .rewind .loop))
  state)

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

(defn draw-mystery-ship [{{location :location
                           sprites  :sprites} :mystery-ship}]
  (if (not (nil? location))
    (let [tick (quot (q/frame-count) 10)
          idx  (mod tick (count sprites))]
      (q/image (sprites idx) (:x location) (:y location)))))

(defn draw-boss-ship [{{{x :x y :y} :location
                        sprites     :sprites} :boss-ship}]
  (let [tick (quot (q/frame-count) 30)
        idx  (mod tick (count sprites))]
    (q/image (sprites idx) x y)))

(defn draw-score [{{value   :value
                    sprites :sprites} :score}]
  "Renders the current score to the screen"
  (q/push-matrix)
  (q/translate 25 25)
  (doseq [digit (str value)]
    (q/image (sprites digit) 0 0)
    (q/translate 32 0))
  (q/pop-matrix))

(defn draw-lives [{{value  :value
                    sprite :sprite} :lives
                   {w      :w}      :board}]
  "Renders the number of lives left for the player"
  (let [sprite-width 32]
    (q/push-matrix)
    (q/translate (- w sprite-width) sprite-width)
    (dotimes [_ value]
      (q/image sprite 0 0)
      (q/translate (- sprite-width) 0))
    (q/pop-matrix)))

(defn draw-level [{{value   :value
                    sprites :sprites} :level
                   {w       :w
                    h       :h}     :board}]
  "Renders the current level to the screen"
  (let [sprite-width 32]
    (q/push-matrix)
    (q/translate (- w sprite-width) (- h sprite-width))
    (doseq [digit (str value)]
      (q/image (sprites digit) 0 0)
      (q/translate (- sprite-width) 0))
    (q/pop-matrix)))

; TODO: Possibly play some thing amusing like a sad trombone clip. (BUT ONLY ONCE!)
(defn draw-game-over [{{music   :music
                        h       :h} :board
                       {sprites :sprites} :letters}]
  (let [letter-width 100]
    (q/background 0)
    (q/push-matrix)
    (q/translate (* 0.5 letter-width) (* 0.5 h))
    (doto music .mute .play)
    (doseq [letter "GAMEOVER"]
      (q/image (sprites letter) 0 0)
      (q/translate letter-width 0))
    (q/pop-matrix)))

(defn draw-stars [{{w :w h :h} :board
                   stars       :stars}]
  (q/background 0)
  (q/stroke-weight 4)
  (doseq [{x :x y :y} stars]
    (q/stroke (q/random 255) 255 255)
    (q/point x y)))

(defn handle-sounds [{sounds :sounds
                      events :events :as state}]
  (doseq [event events]
    (cond
      (= event :new-invader-bullet)
        (doto (event sounds) .rewind .play)
      (= event :new-mystery-bullet)
        (doto (event sounds) .rewind .play)
      (= event :new-boss-bullet)
        (doto (event sounds) .rewind .play)
      (= event :invader-dead)
        (doto (event sounds) .rewind .play)
      (= event :new-mystery-ship)
        (doto (event sounds) .rewind .unmute .loop)
      (= event :mystery-ship-gone)
        (doto (:new-mystery-ship sounds) .mute .play)
      (= event :invader-landed)
        (do
          (doto (event sounds) .rewind .play)
          (Thread/sleep 5000))
      (= event :player-dead)
        (do
;        (doto music .mute .play)
;        (doto mystery-sound .mute .play)
;        (doto death-sound .rewind .play)
;        (Thread/sleep 5000)
;        (doto music .unmute .rewind .play)
          (doto (event sounds) .rewind .play)
          (Thread/sleep 5000))
      :else
        nil)))

(defn draw-regular-level [{player-bullets  :player-bullets
                           invader-bullets :invader-bullets :as state}]
;  (play-background-music state)
  (draw-stars state)
  (draw-player state)
  (draw-bullets player-bullets)
  (draw-bullets invader-bullets)
  (draw-patrol state)
  (draw-mystery-ship state)
  (draw-score state)
  (draw-lives state)
  (draw-level state)
  )

(defn draw-boss-level [{player-bullets  :player-bullets
                        invader-bullets :invader-bullets :as state}]
  (draw-stars state)
  (draw-player state)
  (draw-boss-ship state)
  (draw-bullets player-bullets)
  (draw-bullets invader-bullets)
  (draw-score state)
  (draw-lives state)
  (draw-level state)
  )

; TODO: Need start screen with directions.
;       Need to implement levels
;       Need to implement boss level
(defn draw-board [state]
  "Primary hook to render all entities to the screen"
  (handle-sounds state)
  (cond
    (regular-level? state)
      (draw-regular-level state)
    (boss-level? state)
      (draw-boss-level state)
    :else
      (draw-game-over state)))

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

(defn stop-all-sounds [{{music :music} :board
                        {sound :sound} :mystery-ship :as state}]
  "Primary hook when sketch is closed to insure that sounds are stopped
   without shutting down the entire JVM."
  (doto music .mute .play)
  (doto sound .mute .play))

; TODO: Figure out how to configure this project such that
;         lein uberjar produces an executable jar.
(q/defsketch space-invaders
  :title       "space invaders"
  :size        [800 800]
  :setup       setup
  :draw        draw-board
  :key-pressed key-pressed
  :update      update-board
  :on-close    stop-all-sounds
  :middleware  [m/fun-mode])
