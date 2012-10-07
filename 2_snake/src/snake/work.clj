(ns snake.work
  (:use [snake.core :only (run-not-grow run-grow run-many-apples run-with-walls)]))

;;; You're writing a bot for playing snake.
;;; So, you are a snake and your goal is to collect apples.
;;; Field sizes: 40 x 30
;;; Every turn you move to one of the neighbours cell.
;;; Your function must take 2 arguments: snake's position and apple's position and decide which direction to move.
;;; Directions are: :up, :down, :left, :right (they are keywords). Your function must return one of these directions.
;;; Position (snake's or apple's) is a vector of 2 elements: x and y.
;;; In this task snake is not growing after it ate an apple so there is no danger of snake hitting itself.
;;; Note: upper left corner cell is (0, 0).

;;; Uncomment and substitute your solution

(defn run-not-grow-solution [spos apos]
  (let [dx (- (first apos) (first spos))
       dy (- (last apos) (last spos))]
  (if (= dx 0)
    (if (or (and (>= dy -15) (< dy 0)) (> dy 15)) :up :down)
    (if (or (and (>= dx -20) (< dx 0)) (> dx 20)) :left :right))))

;(run-not-grow run-not-grow-solution)



;;; Snake grows now (each time snake eats an apple, it's length increases).
;;; You need to write similar function as in previous task.
;;; It takes 2 arguments.
;;; First argument is snake's body - collection of cells, each cell is a vector of x and y. First cell is snake's head.
;;; Second argument is apple's position - vector of x and y.
;;; It should return direction: :up, :down, :left or :right.
;;; Note that you cannot change direction to the opposite in 1 move: snake will hit it's tail if length is 2 or more.
;;; Wait, you can change direction but snake will die :\

;;; Uncomment and substitute your solution

(defn run-stupid-solution [scol any]
  (let [y (last (first scol))
        x (first (first scol))]
  (if (and (= (mod x 2) 0) (< y 29)) :down
    (if (and (= (mod x 2) 1) (> y 0)) :up :right))))


;calculates the minimum amount of steps required to get to an apple from given position, snake's body is considered
;returns -1 if an apple can't be reached
;returns -2 if given position is occupied by snake

(defn run-grow-distance [[x y] scol apos]
  (if (empty? (filter #(= % [x y]) scol))
    ((fn d [reachable used scol apos]
      (if (empty? reachable) -1
        (if (contains? reachable apos) 0
          (let [s (reduce #(let [[x y] %2] (conj %1 [x (mod (- y 1) 30)] [(mod (+ x 1) 40) y] [x (mod (+ y 1) 30)] [(mod (- x 1) 40) y])) #{} reachable)
                newreachable (reduce #(disj %1 %2) (reduce #(disj %1 %2) s scol) used)
                newused (reduce #(conj %1 %2) used newreachable)
                t (d newreachable newused scol apos)]
            (if (= t -1) -1 (+ 1 t)))))) #{[x y]} #{[x y]} scol apos) -2))

(defn run-grow-solution [scol apos]
  (let [[x y] (first scol)
        ud (run-grow-distance [x (mod (- y 1) 30)] scol apos)
        rd (run-grow-distance [(mod (+ x 1) 40) y] scol apos)
        dd (run-grow-distance [x (mod (+ y 1) 30)] scol apos)
        ld (run-grow-distance [(mod (- x 1) 40) y] scol apos)
        ds (filter #(> % -1) [ud rd dd ld])
        d (if (empty? ds) -1 (first (sort ds)))]
    (if (= ud d) :up
      (if (= rd d) :right
        (if (= dd d) :down :left)))))

;(run-grow run-grow-solution)


;;; Now you have many apples (5) instead of one.
;;; Function the same as previous but it takes set of apples instead of the single apple.
;;; Each apple in the set is a vector of x and y.
;;; E.g. you can try to reach nearest apple to the snake.

;;; Uncomment and substitute your solution


;calculates the minimum amount of steps needed to get to the nearest apple from current position, snake's body is considered
;returns -1 if no apple can be reached
;returns -2 if given position is occupied by snake

(defn run-many-apples-distance [[x y] scol acol]
  (if (empty? (filter #(= % [x y]) scol))
    ((fn d [reachable used scol acol]
      (if (empty? reachable) -1
        (if (reduce #(or %1 (contains? reachable %2)) false acol) 0
          (let [s (reduce #(let [[x y] %2] (conj %1 [x (mod (- y 1) 30)] [(mod (+ x 1) 40) y] [x (mod (+ y 1) 30)] [(mod (- x 1) 40) y])) #{} reachable)
                newreachable (reduce #(disj %1 %2) (reduce #(disj %1 %2) s scol) used)
                newused (reduce #(conj %1 %2) used newreachable)
                t (d newreachable newused scol acol)]
            (if (= t -1) -1 (+ 1 t)))))) #{[x y]} #{[x y]} scol acol) -2))

(defn run-many-apples-solution [scol acol]
  (let [[x y] (first scol)
        ud (run-many-apples-distance [x (mod (- y 1) 30)] scol acol)
        rd (run-many-apples-distance [(mod (+ x 1) 40) y] scol acol)
        dd (run-many-apples-distance [x (mod (+ y 1) 30)] scol acol)
        ld (run-many-apples-distance [(mod (- x 1) 40) y] scol acol)
        ds (filter #(> % -1) [ud rd dd ld])
        d (if (empty? ds) -1 (first (sort ds)))]
    (if (= ud d) :up
      (if (= rd d) :right
        (if (= dd d) :down :left)))))

;(run-many-apples run-many-apples-solution)



;;; Walls are added. So snake can hit wall and die.
;;; Your function now takes third argument - set of walls.
;;; Each wall is a cell that snake is not allowed to  move to.
;;; Wall is a vector of x and y.

;;; Uncomment and substitute your solution


;calculates the minimum amount of steps needed to get to the nearest apple from current position, snake's body and walls are considered
;returns -1 if no apple can be reached
;returns -2 if given position is occupied by snake or wall

(defn run-with-walls-distance [[x y] scol acol wcol]
  (if (and (empty? (filter #(= % [x y]) scol)) (empty? (filter #(= % [x y]) wcol)))
    ((fn d [reachable used scol acol wcol]
      (if (empty? reachable) -1
        (if (reduce #(or %1 (contains? reachable %2)) false acol) 0
          (let [s (reduce #(let [[x y] %2] (conj %1 [x (mod (- y 1) 30)] [(mod (+ x 1) 40) y] [x (mod (+ y 1) 30)] [(mod (- x 1) 40) y])) #{} reachable)
                newreachable (reduce #(disj %1 %2) (reduce #(disj %1 %2) (reduce #(disj %1 %2) s wcol) scol) used)
                newused (reduce #(conj %1 %2) used newreachable)
                t (d newreachable newused scol acol wcol)]
            (if (= t -1) -1 (+ 1 t)))))) #{[x y]} #{[x y]} scol acol wcol) -2))

(defn run-with-walls-solution [scol acol wcol]
  (let [[x y] (first scol)
        ud (run-with-walls-distance [x (mod (- y 1) 30)] scol acol wcol)
        rd (run-with-walls-distance [(mod (+ x 1) 40) y] scol acol wcol)
        dd (run-with-walls-distance [x (mod (+ y 1) 30)] scol acol wcol)
        ld (run-with-walls-distance [(mod (- x 1) 40) y] scol acol wcol)
        ds (filter #(> % -1) [ud rd dd ld])
        d (if (empty? ds) -1 (first (sort ds)))]
    (if (= ud d) :up
      (if (= rd d) :right
        (if (= dd d) :down :left)))))

(run-with-walls run-with-walls-solution)