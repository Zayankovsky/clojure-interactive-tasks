(ns cellular-automaton.work
  (:use [cellular-automaton.core :only (play)]))



;;; Your task is to implement cellular automaton.
;;; The most famous example of cellular automaton is Conway's Game of Life.
;;; Unlike previous tasks now you have to implement visualization and bots. So you need to implement everything :)
;;; I suggest to use quil library for animation (it was used in all previous tasks): https://github.com/quil/quil
;;; But of course you can use whatever you want.
;;; Keep in mind that it should be simple to run your simulator with different automata (Game of Life is only 1 example).

;returns number of neighbors of the cell [x y] in map o that have state s
(defn neighbors [[x y] o s]
  (reduce #(if (= (o %2) s) (inc %1) %1) 0 [[(- x 1) (- y 1)] [x (- y 1)] [(+ x 1) (- y 1)] [(- x 1) y] [(+ x 1) y] [(- x 1) (+ y 1)] [x (+ y 1)] [(+ x 1) (+ y 1)]]))

;Gosper glider gun
(def conway-init (atom {}))
(doseq [x (range 80) y (range 60)] (swap! conway-init #(assoc % [x y] :dead)))
(doseq [p [[1 5] [1 6] [2 5] [2 6] [11 5] [11 6] [11 7] [12 4] [12 8] [13 3] [13 9] [14 3] [14 9] [15 6] [16 4] [16 8] [17 5] [17 6] [17 7] [18 6] [21 3]
           [21 4] [21 5] [22 3] [22 4] [22 5] [23 2] [23 6] [25 1] [25 2] [25 6] [25 7] [35 3] [35 4] [36 3] [36 4]]] (swap! conway-init #(assoc % p :alive)))

(defn conway-rules [old]
  (reduce #(assoc %1 %2 (let [n (neighbors %2 old :alive)] (cond (or (< n 2) (> n 3)) :dead
                                                          (= n 2) (old %2)
                                                          (= n 3) :alive))) {} (keys old)))

(defn conway-color [state]
  (if (= state :alive) [0 255 0] [0 0 0]))

(play @conway-init conway-rules conway-color :alive :dead)


;;; Implement and run Brian's Brain automaton in your simulator: http://en.wikipedia.org/wiki/Brian%27s_Brain

;Oscillator
(def brians-brain-init (atom {}))
(doseq [x (range 80) y (range 60)] (swap! brians-brain-init #(assoc % [x y] :off)))
(doseq [p [[1 3] [2 1] [3 4] [4 2]]] (swap! brians-brain-init #(assoc % p :on)))
(doseq [p [[1 2] [2 4] [3 1] [4 3]]] (swap! brians-brain-init #(assoc % p :dying)))

(defn brians-brain-rules [old]
  (reduce #(assoc %1 %2 (let [s (old %2)] (cond (= s :off) (if (= (neighbors %2 old :on) 2) :on :off)
                                                (= s :on) :dying
                                                (= s :dying) :off))) {} (keys old)))

(defn brians-brain-color [state]
  (cond (= state :on) [255 255 255]
        (= state :dying) [0 0 255]
        (= state :off) [0 0 0]))

;(play @brians-brain-init brians-brain-rules brians-brain-color :on :off)


;;; Implement Wireworld automaton: http://en.wikipedia.org/wiki/Wireworld

;2 Clock generators sending electrons into an XOR gate
(def wireworld-init (atom {}))
(doseq [x (range 80) y (range 60)] (swap! wireworld-init #(assoc % [x y] :empty)))
(doseq [p [[0 6] [3 6] [4 5] [4 6] [4 7] [5 5] [5 7] [6 4] [6 5] [6 7] [6 8] [7 3] [7 5] [7 6] [7 7] [8 3] [9 3] [9 9] [10 3] [10 9]
[11 3] [11 9] [12 3] [12 9] [13 2] [13 8] [14 2] [14 8] [15 2] [15 4] [15 8] [15 10] [16 2] [16 4] [16 10] [17 2] [17 4] [17 10] [18 2]
[18 4] [18 8] [18 10] [19 2] [19 8] [19 10] [20 2] [20 8] [20 10] [21 3] [21 9]]] (swap! wireworld-init #(assoc % p :conductor)))
(doseq [p [[1 6] [7 9] [13 4] [13 10] [17 8] [19 4]]] (swap! wireworld-init #(assoc % p :head)))
(doseq [p [[2 6] [8 9] [14 4] [14 10] [16 8] [20 4]]] (swap! wireworld-init #(assoc % p :tail)))

(defn wireworld-rules [old]
  (reduce #(assoc %1 %2 (let [s (old %2)] (cond (= s :empty) :empty
                                                (= s :head) :tail
                                                (= s :tail) :conductor
                                                (= s :conductor) (let [n (neighbors %2 old :head)] (if (or (= n 1) (= n 2)) :head :conductor))))) {} (keys old)))

(defn wireworld-color [state]
  (cond (= state :empty) [0 0 0]
        (= state :head) [0 0 255]
        (= state :tail) [255 0 0]
        (= state :conductor) [255 255 0]))

;(play @wireworld-init wireworld-rules wireworld-color :conductor :empty)



;;; Add Wireworld implementation to Rosetta Code (it's not present here yet): http://rosettacode.org/wiki/Wireworld

;;; Implement Von Neumann cellular automaton: http://en.wikipedia.org/wiki/Von_Neumann_cellular_automata
;So many states and rules in this automaton :( I'll implement it when I have more free time

;;; Implement Langton's ant: http://en.wikipedia.org/wiki/Langton%27s_ant

(def ant-black #{:ant-up-black, :ant-right-black, :ant-down-black, :ant-left-black})
(def ant-white #{:ant-up-white, :ant-right-white, :ant-down-white, :ant-left-white})
(def ant #{:ant-up-black, :ant-right-black, :ant-down-black, :ant-left-black :ant-up-white, :ant-right-white, :ant-down-white, :ant-left-white})

(def langtons-ant-init (atom {}))
(doseq [x (range 80) y (range 60)] (swap! langtons-ant-init #(assoc % [x y] :white)))
(swap! langtons-ant-init #(assoc % [40 30] :ant-left-white))

(defn langtons-ant-rules [old]
  (let [cell (some #(when (contains? ant (second %)) (first %)) old)]
    (if cell
      (let [[x y] cell
            s (old cell)]
        (if (and (> x -1) (< x 80) (> y -1) (< y 60))
          (cond (= s :ant-up-black) (let [lp [(- x 1) y] ls (old lp)] (assoc (assoc old [x y] :white) lp (if (= ls :black) :ant-left-black :ant-left-white)))
                (= s :ant-right-black) (let [up [x (- y 1)] us (old up)] (assoc (assoc old [x y] :white) up (if (= us :black) :ant-up-black :ant-up-white)))
                (= s :ant-down-black) (let [rp [(+ x 1) y] rs (old rp)] (assoc (assoc old [x y] :white) rp (if (= rs :black) :ant-right-black :ant-right-white)))
                (= s :ant-left-black) (let [dp [x (+ y 1)] ds (old dp)] (assoc (assoc old [x y] :white) dp (if (= ds :black) :ant-down-black :ant-down-white)))
                (= s :ant-up-white) (let [rp [(+ x 1) y] rs (old rp)] (assoc (assoc old [x y] :black) rp (if (= rs :black) :ant-right-black :ant-right-white)))
                (= s :ant-right-white) (let [dp [x (+ y 1)] ds (old dp)] (assoc (assoc old [x y] :black) dp (if (= ds :black) :ant-down-black :ant-down-white)))
                (= s :ant-down-white) (let [lp [(- x 1) y] ls (old lp)] (assoc (assoc old [x y] :black) lp (if (= ls :black) :ant-left-black :ant-left-white)))
                (= s :ant-left-white) (let [up [x (- y 1)] us (old up)] (assoc (assoc old [x y] :black) up (if (= us :black) :ant-up-black :ant-up-white)))) old)) old)))

(defn langtons-ant-color [state]
  (cond (= state :white) [255 255 255]
        (= state :black) [0 0 0]
        (contains? ant-black state) [127 0 0]
        (contains? ant-white state) [255 127 127]))

;(play @langtons-ant-init langtons-ant-rules langtons-ant-color :black :white)


;;; Add ability to change cells' states by mouse click, to restart and pause simulation.
;See core.clj file. Left and right clicks for two different default states. They're sent to the play function
;Note: program won't react on hitting space before you click on its window. But that will cause changing the state of a clicked cell. So be careful :)