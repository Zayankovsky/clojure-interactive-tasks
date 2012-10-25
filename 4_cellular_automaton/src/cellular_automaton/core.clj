(ns cellular-automaton.core
  (:use quil.core))

(defn setup [in]
  (fn []
    (smooth)
    (frame-rate 10)
    (def grid (atom in))))
    (def running (atom true))

(defn draw-cell [[x y] col]
  (let [[r g b] (col (@grid [x y]))]
    (fill r g b)
    (rect (* 10 x) (* 10 y) 10 10)))

(defn draw [rul col]
  (fn []
    (when @running (swap! grid rul)
                   (doseq [cell (keys @grid)] (draw-cell cell col)))))

(defn change-state [col d-1 d-2]
  (fn []
    (let [mx (mouse-x) my (mouse-y)
          nx (- mx (mod mx 10)) ny (- my (mod my 10))
          x (/ nx 10) y (/ ny 10)
          b (mouse-button)]
      (cond (= b :left) (swap! grid #(assoc % [x y] d-1))
            (= b :right) (swap! grid #(assoc % [x y] d-2)))
      (draw-cell [x y] col))))

(defn pause-resume []
  (if (= (raw-key) \space)
    (swap! running not)))

(defn run [init rule color def-1 def-2]
  (sketch
    :title "Cellular automaton"
    :setup (setup init)
    :draw (draw rule color)
    :size [800 600]
    :mouse-clicked (change-state color def-1 def-2)
    :key-typed pause-resume))

(defn play [initial rules color default-1 default-2]
  (run initial rules color default-1 default-2))