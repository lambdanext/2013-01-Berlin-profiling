(ns berlin-profiling.life
  (:require [quil.core :as q]
            [clojure.core.reducers :as r]))

; a cell
{:x 1 :y 2} ; [1 2]


  ; a generation
; a collection: a set
(def init #{{:x 1 :y 2} {:x 1 :y 1} 
  {:x 1 :y 0}})
  
(def world (atom init))
  
(comment
  ; check for liveness
(contains? gen cell)
; or
(gen cell))

(defrecord Cell [x y])

; computing neighbours
(defn neighbours [{:keys [x y]}]
  (for [nx (range (dec x) (+ x 2))
        ny (range (dec y) (+ y 2))
        :when (or (not= nx x)
                   (not= ny y))]
    (Cell. nx ny)))

; Any live cell with fewer than two live neighbours dies, as if caused by under-population.
; Any live cell with two or three live neighbours lives on to the next generation.
; Any live cell with more than three live neighbours dies, as if by overcrowding.
; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.



(defn stepper [neighbours survive? spawn?]
  (fn [cells]
    {:pre [(set? cells)]
     :post [(set? %)]}
    (let [fs (frequencies 
               (mapcat neighbours cells))]
      (into #{}
        (->> fs 
          (r/filter (fn [[cell n]]
                (if (cells cell)
                  (survive? n)
                  (spawn? n))))
          (r/map key))))))

(def step (stepper neighbours #{2 3} #{3}))

(def scale 2)
(def start-range 150)

(defn setup []
  (q/no-stroke)
  (q/no-smooth)
  (q/frame-rate 15)                    ;;Set framerate to 1 FPS
  (q/background 0 0 255))                 ;;Set the background colour to
                                    ;;  a nice shade of grey.
(defn draw []
  (q/background 0 0 255)
  (q/fill 255 255 0)               ;;Set the fill colour to a random grey
  (doseq [{:keys [x y]} @world]
    (q/rect (* scale x) (* scale y) scale scale)))       ;;Draw a circle at x y with the correct diameter

(q/defsketch life                  ;;Define a new sketch named example
  :title "life is good"  ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :draw draw                        ;;Specify the draw fn
  :size [600 600])                  ;;You struggle to beat the golden ratio

(reset! world
  (set (repeatedly (/ (* start-range start-range) 4)
                   (fn []
                     (map->Cell {:x (rand-int start-range)
                                 :y (rand-int start-range)})))))

(def stop (atom true))
(def stop (atom false))
#_(def f (future (while (not @stop) (swap! world step))))
