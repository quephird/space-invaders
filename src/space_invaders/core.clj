(ns space-invaders.core
  (:import [ddf.minim Minim])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn make-stars [w h]
  "Returns a vector of hashmaps each representing a background star"
  (into []
    (for [i (range 50)]
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

(defn load-mystery-ship-sprites []
  (into []
    (for [i (range 1 4)]
      (q/load-image (str "resources/mystery" i ".png")))))

; TODO: Possibly introduce increasing difficulty by making speed a property
;         of the bullets too.
;       Make probability of generating a bullet a property
;         that can be "mutated" to increase difficulty
(defn create-board [w h m]
  "Returns a nested hashmap representing the entire state of the game"
  {:board          {:w            w
                    :h            h}
   :stars           (make-stars w h)
   :player         {:x            (* 0.5 w)
                    :y            (* 0.9 h)
                    :direction    0}
   :player-bullets  []
   :patrol         {:invaders     (make-invaders)
                    :invader-explosions []
                    :direction    1
                    :dx           1}
   :invader-bullets  []
   :mystery-ship     nil
   :boss-ship       {:location    {:x 400 :y 400}
                     :direction-x 1
                     :direction-y 1
                     :hits-left   25}
   :score            0
   :status           :waiting
   :level            1
   :lives            3
   :events           []
   :fonts           {:start           (q/create-font "Courier" 48)
                     :game            (q/create-font "Courier" 32)
                     :game-over       (q/create-font "Courier" 60)}
;   :loops           (.loadFile m "resources/regular.wav")
   :sprites         {:player          (q/load-image "resources/player.png")
                     :player-bullet   (q/load-image "resources/player-bullet.png")
                     :lives           (q/load-image "resources/life.png")
                     :invader         [(q/load-image "resources/invader1.png")
                                       (q/load-image "resources/invader2.png")]
                     :invader-bullet  (q/load-image "resources/invader-bullet.png")
                     :invader-explosion [(q/load-image "resources/invader-explosion1.png")
                                         (q/load-image "resources/invader-explosion2.png")
                                         ]
                     :mystery-ship    (load-mystery-ship-sprites)
                     :boss-ship       [(q/load-image "resources/boss1.png")
                                       (q/load-image "resources/boss2.png")]}
   :sounds          {:new-player-bullet   (.loadFile m "resources/new-player-bullet.wav")
                     :new-invader-bullet  (.loadFile m "resources/new-invader-bullet.wav")
                     :new-mystery-ship    (.loadFile m "resources/new-mystery-ship.mp3")
                     :new-mystery-bullet  (.loadFile m "resources/new-mystery-bullet.wav")
                     :new-boss-bullet     (.loadFile m "resources/new-boss-bullet.wav")
                     :boss-shot           (.loadFile m "resources/boss-shot.aiff")
                     :mystery-ship-dead   (.loadFile m "resources/mystery-ship-dead.wav")
                     :invader-dead        (.loadFile m "resources/invader-dead.aiff")
                     :invader-landed      (.loadFile m "resources/invader-landed.wav")
                     :player-dead         (.loadFile m "resources/player-dead.wav")}})

(defn reset-board [{{w :w h :h} :board :as state}]
  "Returns a new version of the board with all 'mutable' values
   in their orignal state."
  (-> state
    (assoc-in [:status] :in-progress)
    (assoc-in [:events] [])
    (assoc-in [:player :x] (* 0.5 w))
    (assoc-in [:player :y] (* 0.9 h))
    (assoc-in [:patrol :invaders] (make-invaders))
    (assoc-in [:patrol :direction] 1)
    (assoc-in [:patrol :dx] 1)
    (assoc-in [:invader-bullets] [])
    (assoc-in [:player-bullets] [])
    (assoc-in [:score] 0)
    (assoc-in [:level] 1)
    (assoc-in [:lives] 3)))

(defn game-over? [{lives :lives :as state}]
  (zero? lives))

(defn regular-level? [{level :level :as state}]
  (and (not (zero? (mod level 3)))
       (not (game-over? state))))

(defn boss-level? [{level :level :as state}]
  (and (zero? (mod level 3))
       (not (game-over? state))))

(defn invaders-reached-bottom? [{{h        :h}        :board
                                 {invaders :invaders} :patrol :as state}]
  (->> invaders (map :y) (apply max) (< (- h 100))))

(defn no-player-bullets? [{player-bullets :player-bullets}]
  (zero? (count player-bullets)))

(defn clear-previous-events [state]
  (assoc-in state [:events] []))

(defn update-explosions [state]
  (update-in state [:patrol :invader-explosions]
    (fn [explosions]
      (->> explosions
        (filter (fn [{stage :stage}] (< stage 1)))
        (map (fn [explosion] (update-in explosion [:stage] inc)))))))

(defn grazed-bullet? [{player-x :x player-y :y}
                      {bullet-x :x bullet-y :y}]
  "Returns true if the bullet is sufficiently close to the player
   and should count as a graze."
  (let [range-x 5
        range-y 5
        dy      (+ range-y bullet-y (- player-y))]
    (and (< (Math/abs (- bullet-y player-y)) range-y)
         (< (Math/abs (- bullet-x player-x)) (/ (* dy range-x) 2 range-y)))))

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

(defn within-mystery-hitbox? [{mystery-x :x mystery-y :y}
                              {bullet-x :x  bullet-y :y}]
  "Returns true if the bullet is within the hitbox of the mystery ship"
  (let [range-x 60
        range-y 25]
    (and (< (Math/abs (- bullet-x mystery-x)) range-x)
         (< (Math/abs (- bullet-y mystery-y)) range-y))))

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

(defn check-invaders-shot [{{invaders           :invaders
                             invader-explosions :invader-explosions}  :patrol
                             player-bullets :player-bullets :as state}]
  "Returns a new version of game state removing all bullets
   and invaders involved in collisions"
  (let [bullets-left-over     (remove (fn [bullet] (any-invader-shot? bullet invaders)) player-bullets)
        invaders-shot         (->> invaders
                                (filter (fn [invader] (entity-shot? invader player-bullets within-invader-hitbox?))))
        invaders-left-over    (remove (set invaders-shot) invaders)
        new-explosions        (map (fn [invader] (assoc-in invader [:stage] 0)) invaders-shot)
        points-scored         (* (count invaders-shot) 100)]
    (-> state
      (assoc-in [:player-bullets] bullets-left-over)
      (assoc-in [:patrol :invaders] invaders-left-over)
      (update-in [:patrol :invader-explosions] concat new-explosions)
      (update-in [:events] concat (repeat (count invaders-shot) :invader-dead))
      (update-in [:score] + points-scored))))

(defn check-mystery-ship-shot [{player-bullets   :player-bullets
                                mystery-location :mystery-ship
                                score            :score :as state}]
  (if (nil? mystery-location)
    state
    (let [mystery-shot (entity-shot? mystery-location player-bullets within-mystery-hitbox?)]
      (if mystery-shot
        (-> state
          (assoc-in [:player-bullets] [])
          (update-in [:score] + 500)
          (assoc-in [:mystery-ship] nil)
          (update-in [:events] conj :mystery-ship-dead))
         state))))

(defn check-boss-shot [{{boss-location :location
                         hits-left     :hits-left} :boss-ship
                         player-bullets            :player-bullets :as state}]
  "Returns a new version of game state removing all hitting player bullets"
  (let [bullets-missed      (remove (fn [bullet] (entity-shot? boss-location player-bullets within-boss-hitbox?)) player-bullets)
        bullets-hit         (filter (fn [bullet] (entity-shot? boss-location player-bullets within-boss-hitbox?)) player-bullets)
        points-scored         (* (count bullets-hit) 100)]
    (-> state
      (assoc-in [:player-bullets] bullets-missed)
      (assoc-in [:boss-ship :hits-left] (- hits-left (count bullets-hit)))
      (update-in [:score] + points-scored)
      (update-in [:events] concat (repeat (count bullets-hit) :boss-shot)))))

; TODO: Figure out how to give extra points for grazing bullets.
(defn check-player-shot [{{w :w h :h}      :board
                           invader-bullets :invader-bullets
                           lives           :lives
                           player          :player :as state}]
    (if (entity-shot? player invader-bullets within-player-hitbox?)
      (-> state
        (assoc-in  [:player :x] (* 0.5 w))
        (assoc-in  [:player :y] (* 0.9 h))
        (assoc-in  [:invader-bullets] [])
        (assoc-in  [:player-bullets] [])
        (assoc-in  [:mystery-ship] nil)
        (update-in [:events] conj :player-dead)
        (update-in [:lives] dec)
        (assoc-in [:status] (if (= 1 lives) :game-over :in-progress)))
      state))

(defn check-invaders-reached-bottom [state]
  (if (invaders-reached-bottom? state)
      (-> state
        (update-in [:events] conj :invader-landed)
        (assoc-in [:lives] 0))
    state))

(defn check-invaders-cleared [{{invaders :invaders} :patrol :as state}]
  "Returns a new version of game state with a brand new patrol
   if no invaders remain or the state unchanged."
  (if (zero? (count invaders))
    (-> state
      (update-in [:level] inc)
      (assoc-in [:patrol :invaders] (make-invaders))
      (assoc-in [:patrol :direction] 1)
      (assoc-in [:patrol :dx] 1))
    state))

(defn check-boss-dead [{{hits-left :hits-left} :boss-ship :as state}]
  (if (zero? hits-left)
    (-> state
      (update-in [:level] inc)
      (assoc-in [:boss-ship :location] {:x 400 :y 400})
      (assoc-in [:boss-ship :hits-left] 25)
      (assoc-in [:patrol :invaders] (make-invaders))
      (assoc-in [:patrol :direction] 1)
      (assoc-in [:patrol :dx] 1))
    state))

(defn move-player [{{x         :x
                     direction :direction} :player
                    {w         :w}         :board  :as state}]
  "Returns a new version of the board state representing
   a change in position of the player"
  (let [margin 125
        dx (cond
              (and (= direction -1) (> x margin)) -5
              (and (= direction 1) (< x (- w margin))) 5
              :else 0)]
    (update-in state [:player :x] (fn [x] (+ x dx)))))

(defn move-stars [{stars :stars :as state}]
  (assoc-in state [:stars]
    (into []
      (map (fn [star] (update-in star [:y] (fn [y] (- y 2)))) stars))))

(defn update-stars [{{w :w h :h} :board
                      stars      :stars :as state}]
  (let [new-star (if (< (q/random 1) 0.125) [{:x (q/random w) :y h}])]
    (-> state
      (assoc-in [:stars] (into [] (remove (fn [{y :y}] (< y 0)) stars)))
      (assoc-in [:stars] (concat stars new-star)))))

; TODO: Maybe make bullet-dy a property that can decrease to increase dificulty.
(defn move-player-bullets [state]
  "Returns a new version of game state by:

   * getting rid of player bullets that pass off screen, and
   * moving remaining player bullets upward"
  (let [bullet-dy 10]
    (update-in state [:player-bullets]
      (fn [bullets]
        (->> bullets
          (filter (fn [bullet] (> (bullet :y) 0)))
          (map (fn [bullet] (update-in bullet [:y] (fn [y] (- y bullet-dy))))))))))

(defn move-invader-bullets [{{h :h} :board :as state}]
  "Returns a new version of game state by:

   * getting rid of invader bullets that pass off screen, and
   * moving remaining invader bullets downward"
  (update-in state [:invader-bullets]
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

(defn move-mystery-ship [{location :mystery-ship :as state}]
  (if (nil? location)
    state
    (update-in state [:mystery-ship :x] (fn [x] (+ x 5)))))

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
      (update-in [:invader-bullets] concat new-bullets))))

(defn generate-mystery-ship-bullets [{location :mystery-ship :as state}]
  (cond
    (nil? location)
      state
    (not (zero? (mod (q/frame-count) 30)))
      state
    :else
      (let [new-bullets (into [] (map (fn [n] (update-in location [:x] (fn [x] (+ x n)))) [-30 0 30]))]
        (-> state
          (update-in [:invader-bullets] concat new-bullets)
          (update-in [:events] conj :new-mystery-bullet)))))

(defn generate-boss-bullets [{{{x :x y :y} :location} :boss-ship :as state}]
  (if (> (q/random 1) 0.1)
    state
    (let [bullet-x   (+ x -200 (q/random 255))
          bullet-y   (+ y (q/random 50))
          new-bullet {:x bullet-x :y bullet-y}]
      (-> state
        (update-in [:invader-bullets] conj new-bullet)
        (update-in [:events] conj :new-boss-bullet)))))

; TODO: Change strategy for determining when to bring out the mystery ship.
(defn update-mystery-ship [{{w :w}    :board
                             location :mystery-ship :as state}]
  "Returns a new version of the state with either a new mystery ship
   or it's being removed from the board."
  (cond
    (and (nil? location) (zero? (mod (q/frame-count) 1000)))
      (-> state
        (assoc-in [:mystery-ship] {:x -128 :y 75})
        (update-in [:events] conj :new-mystery-ship))
    (> (:x location 0) (+ w 128))
      (-> state
        (assoc-in [:mystery-ship] nil)
        (update-in [:events] conj :mystery-ship-gone))
    :else
      state))

(defn update-board [{status :status :as state}]
  "Primary hook which updates the entire game state before the next frame is drawn"
  (cond
    (= status :waiting)
      state
    (regular-level? state)
      (-> state
        clear-previous-events
        update-explosions
        check-player-shot
        check-invaders-reached-bottom
        check-invaders-shot
        check-invaders-cleared
        check-mystery-ship-shot
        move-player
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
        move-player
        move-boss-ship
        move-invader-bullets
        move-player-bullets
        generate-boss-bullets)
    :else
      (clear-previous-events state)))

; TODO: Figure out how to move sound clip playing out into main draw routine.
(defn add-player-bullet [{{x :x y :y} :player
                          sounds      :sounds :as state}]
  (let [pixels-above-player 48
        new-bullet {:x x :y (- y pixels-above-player)}]
    (doto (:new-player-bullet sounds) .rewind .play)
    (-> state
      (update-in [:player-bullets] conj new-bullet))))

(defn key-pressed [{status   :status :as state}
                   {key      :key
                    key-code :key-code :as event}]
  "Primary hook to return new version of game state taking into account:

    * moving the player left or right
    * generating a new bullet"
  (cond
    (and (= :s key) (contains? #{:waiting :game-over} status))
      (reset-board state)
    (and (= 32 key-code) (not= status :waiting) (no-player-bullets? state))
      (add-player-bullet state)
    (= :left key)
      (assoc-in state [:player :direction] -1)
    (= :right key)
      (assoc-in state [:player :direction] 1)
    :else
      state))

; TODO: Need to contact quil folks about this;
;         shouldn't key-relased also receive the event?
(defn key-released [state]
  (let [code (q/key-code)]
    (if (or (= code 37) (= code 39))
      (assoc-in state [:player :direction] 0)
      state)))

(defn play-background-music [{music :loops :as state}]
  (if (not (.isLooping music))
    (doto music .unmute .rewind .loop))
  state)

(defn draw-player [{{x :x y :y}      :player
                    {sprite :player} :sprites}]
  "Renders the player to the screen"
  (q/push-matrix)
  (q/translate x y)
  (q/image sprite 0 0)
  (q/pop-matrix))

(defn draw-player-bullets [{player-bullets         :player-bullets
                           {sprite :player-bullet} :sprites}]
  "Renders all player bullets to the screen"
  (doseq [{x :x y :y} player-bullets]
    (q/image sprite x y)))

(defn draw-invader-bullets [{invader-bullets         :invader-bullets
                            {sprite :invader-bullet} :sprites}]
  "Renders all player bullets to the screen"
  (doseq [{x :x y :y} invader-bullets]
    (q/image sprite x y)))

; TODO: Think about how to draw exploded invaders,
;         possibly introduce :status property for each invader
(defn draw-patrol [{{invaders :invaders
                     invader-explosions :invader-explosions} :patrol
                    {sprites    :invader
                     explosions :invader-explosion}  :sprites}]
  "Renders the entire invader patrol to the screen"
  ; This tick insures that sprites alternate over time...
  (let [tick              (quot (q/frame-count) 30)]
    (q/stroke-weight 1)
    (q/stroke 120 255 255)
    (doseq [{idx    :idx
             x      :x
             y      :y} invaders]
      ; ... and this logic insures that sprites alterate across the patrol.
      (let [sprite-idx (mod (+ tick idx (quot idx 8)) 2)]
        (q/image (sprites sprite-idx) x y)))
    (doseq [{x      :x
             y      :y
             stage  :stage} invader-explosions]
      (q/image (explosions stage) x y))
    ))
;          (q/image explosion x y)))

(defn draw-mystery-ship [{location                :mystery-ship
                          {sprites :mystery-ship} :sprites}]
  (if (not (nil? location))
    (let [tick (quot (q/frame-count) 10)
          idx  (mod tick (count sprites))]
      (q/image (sprites idx) (:x location) (:y location)))))

(defn draw-boss-ship [{{{x :x y :y} :location}  :boss-ship
                        {sprites    :boss-ship} :sprites}]
  (let [tick (quot (q/frame-count) 30)
        idx  (mod tick (count sprites))]
    (q/image (sprites idx) x y)))

(defn draw-score [{score           :score
                  {digits :digits} :sprites
                  {font   :game}   :fonts}]
  "Renders the current score to the screen"

  (q/text-font font)
  (q/fill 100 255 255)
  (q/text "{" 10 32)
  (q/fill 200 255 255)
  (q/text ":score" 30 32)
  (q/fill 0 0 127)
  (q/text (str score) 170 32)
  (q/fill 100 255 255)
  (q/text "}" (+ 170 (* 20 (count (str score)))) 32))

(defn draw-lives [{lives           :lives
                   {sprite :lives} :sprites
                   {font   :game}  :fonts}]
  "Renders the number of lives left for the player"
  (q/text-font font)
  (q/fill 100 255 255)
  (q/text "{" 590 32)
  (q/fill 200 255 255)
  (q/text ":lives" 610 32)
  (q/fill 0 0 127)
  (q/text (str lives) 750 32)
  (q/fill 100 255 255)
  (q/text "}" 770 32))

(defn draw-level [{level           :level
                  {digits :digits} :sprites
                  {font   :game}   :fonts}]
  (q/text-font font)
  (q/fill 100 255 255)
  (q/text "{" 590 780)
  (q/fill 200 255 255)
  (q/text ":level" 610 780)
  (q/fill 0 0 127)
  (q/text (str level) 750 780)
  (q/fill 100 255 255)
  (q/text "}" 770 780))

(defn draw-start-screen [{font :font :as state}]
  (let [all-text [[[100 255 255] "(" 20 200]
                  [[200 255 255] ":space-invaders" 50 200]
                  [[200 255 255] "  :in :quil" 20 250]
                  [[100 255 255] ")" 350 250]
                  [[100 255 255] "" 20 300]
                  [[100 255 255] "(" 20 350]
                  [[120 200 200] "case " 50 350]
                  [[0 0 127] "key-code" 200 350]
                  [[200 255 255] "  :s" 20 400]
                  [[127 255 255] "    (" 20 450]
                  [[0 0 127] "start-game" 170 450]
                  [[127 255 255] ")" 470 450]
                  [[200 255 255] ":else" 20 500]
                  [[127 255 255] "    (" 20 550]
                  [[0 0 127] "stare-at-this-screen" 170 550]
                  [[127 255 255] ")" 750 550]]]
    (q/background 0)
    (q/text-font (q/create-font "Courier" 48))

    (doseq [[text-color text x y] all-text]
      (apply q/fill text-color)
      (q/text text x y))
    ))

; TODO: Possibly play some thing amusing like a sad trombone clip. (BUT ONLY ONCE!)
(defn draw-game-over [{{font :game-over} :fonts}]
  (q/background 0)
  (q/text-font font)

  (q/fill 100 255 255)
  (q/text "{" 50 400)
  (q/fill 200 255 255)
  (q/text ":game-status :over" 85 400)
  (q/fill 100 255 255)
  (q/text "}" 740 400)
  )

(defn draw-stars [{{w :w h :h} :board
                   stars       :stars}]
  (q/background 0)
  (doseq [{x :x y :y} stars]
    (if (< 0.8 (q/random 1))
      (q/stroke (q/random 255) 255 255)
      (q/stroke 0 0 255))
    (q/rect x y 4 4)
    ))

(defn handle-sounds [{sounds :sounds
                      music  :loops
                      events :events :as state}]
  (doseq [event events]
    (case event
      :new-invader-bullet
        (doto (event sounds) .rewind .play)
      :new-mystery-bullet
        (doto (event sounds) .rewind .play)
      :new-boss-bullet
        (doto (event sounds) .rewind .play)
      :boss-shot
        (doto (event sounds) .rewind .play)
      :invader-dead
        (doto (event sounds) .rewind .play)
      :new-mystery-ship
        (doto (event sounds) .rewind .unmute .loop)
      :mystery-ship-dead
        (do
          (doto (:new-mystery-ship sounds) .mute .play)
          (doto (event sounds) .rewind .play))
      :mystery-ship-gone
        (doto (:new-mystery-ship sounds) .mute .play)
      :invader-landed
        (do
          (doto (event sounds) .rewind .play)
          (Thread/sleep 5000))
      :player-dead
        (do
;          (doto music .mute .play)
          (doto (:new-mystery-ship sounds) .mute .play)
          (doto (event sounds) .rewind .play)
;          (doto music .unmute .rewind .play)
          (Thread/sleep 5000))
      nil)))

(defn draw-regular-level [state]
;  (play-background-music state)
  (draw-stars state)
  (draw-player state)
  (draw-player-bullets state)
  (draw-invader-bullets state)
  (draw-patrol state)
  (draw-mystery-ship state)
  (draw-score state)
  (draw-lives state)
  (draw-level state)
  )

(defn draw-boss-level [state]
  (draw-stars state)
  (draw-player state)
  (draw-boss-ship state)
  (draw-player-bullets state)
  (draw-invader-bullets state)
  (draw-score state)
  (draw-lives state)
  (draw-level state)
  )

; TODO: Need start screen with directions.
;       Need to implement levels
;       Need to implement boss level
(defn draw-board [{status :status :as state}]
  "Primary hook to render all entities to the screen"
  (handle-sounds state)
  (cond
    (= status :waiting)
      (draw-start-screen state)
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
    (q/no-stroke)
    (q/color-mode :hsb)
    (q/image-mode :center)
    (q/rect-mode :center)
    (create-board w h m)))

(defn stop-all-sounds [{music  :loops
                        sounds :sounds :as state}]
  "Primary hook when sketch is closed to insure that all looping
   sounds are stopped without shutting down the entire JVM."
;  (doto music .mute .play)
  (doto (:new-mystery-ship sounds) .mute .play))

; TODO: Figure out how to configure this project such that
;         lein uberjar produces an executable jar.
(q/defsketch space-invaders
  :title        "space invaders"
  :size         [800 800]
  :setup        setup
  :draw         draw-board
  :key-pressed  key-pressed
  :key-released key-released
  :update       update-board
  :on-close     stop-all-sounds
  :middleware   [m/fun-mode])
