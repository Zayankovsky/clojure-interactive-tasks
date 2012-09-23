(ns artillery.work
  (:use [artillery.core :only (plane-static plane-dynamic ufo-static ufo-dynamic)]))


;;; You goal is to hit plane by missile.
;;; Plane always starts at position x = 0, y = 500.
;;; Plane's speed equal to 5.
;;; Plane flies from the left to the right. So it's positions will be (0, 500), (5, 500), (10, 500), etc...
;;; You position is x = 400, y = 0.
;;; Missile speed is 10.
;;; You goal is to calculate what angle you need to launch missile at in order to hit the plane.
;;; You solution is a function that takes no paremeters (constant function) and returns this angle.

;;; Here is an example of such function.
;;; It always returns PI / 2 (missile is launched straight up).
;;; You can either calculate answer or find it by trying and adjusting different angles.
(defn plane-static-solution []
  (let [t (/ 5 (Math/sqrt 164))]
  (+ (Math/acos t) (Math/acos (* 2 t)))))
;(defn plane-static-solution [] 1.844433867959335); - exact value

;needed to solve equation
;8 * sin(x) + 10 * cos(x) = 5

;;; Here's a function that will show you animation with plane you launching missiles.
;;; You need to pass your solution (function name) to this function and run this file.
;(plane-static plane-static-solution)



;;; Your goal is the same but now plane start at random position.
;;; And your position also changes every second.
;;; So only plane's speed and missiles' speed are known for sure.
;;; You need to write a function that takes 4 numbers - your coordinates (player) and plane's coordinates (target).
;;; Function should calculate angle to launch missile at.

;;; Example
;;; pl-x, pl-y - player's (your) coordinates.
;;; trg-x trg-y - target's coordinates.
;;; Run and see how it launches missile now and then fix it to hit the plane.
(defn plane-dynamic-solution [pl-x pl-y trg-x trg-y]
  (let [dx (- pl-x trg-x) dy (- trg-y pl-y)
  t (/ dy (Math/sqrt (+ (* dx dx) (* dy dy))))]
  (+ (Math/acos t) (Math/acos (/ t 2)))))

;needed to solve equation
;2 * (pl-x - trg-x) * sin(x) + 2 * (trg-y - pl-y) * cos(x) = trg-y - pl-y

;;; To run program uncomment - remove ';' symbol before '(plane-dynamic ...)'
;;; And also comment previous task - add ';' symbol before '(plane-static ...)'
; (plane-dynamic plane-dynamic-solution)



;;; Now you need to hit UFO.
;;; You're lucky it's not changing, just hanging in the air.
;;; But now gravity force is enabled so your missile won't fly in a straight but rather in a curve. Remember Worms? :)
;;; Gravity force is that missile's y speed will decrease by 0.1 every moment.
;;; UFO position x = 500, y = 300.
;;; UFO speed is equal to 0 (it's not moving).
;;; Your position x = 0, y = 0.
;;; Missile speed stays the same as before.
;;; You need to write function that takes no arguments and returns angle to launch missile at.

;;; Now you don't have template function, so write one yourself.
;;; Hint: try to pass random angle at first e.g. 0.5 and see how it works.
;;; To run program uncomment it (and comment others) and pass your function to it.
(defn ufo-static-solution []
  (let [t (Math/sqrt 136)]
  (/ (- (Math/acos (/ -11 t)) (Math/acos (/ 6 t))) 2)))
;(defn ufo-static-solution [] 0.886340422598924); - exact value

;needed to solve equation
;20 * sin(x) * cos(x) - 12 * cos^2(x) = 5

;(ufo-static ufo-static-solution)



;;; Same UFO, but now it appears at random position (same as plane-dynamic).
;;; Your position is also changing.
;;; You need to write function that takes 4 arguments: your position (x, y)  and UFO's position (x, y).
(defn ufo-dynamic-solution [pl-x pl-y trg-x trg-y]
  (let [dx (- trg-x pl-x) dy (- trg-y pl-y)
  t (Math/sqrt (+ (* dx dx) (* dy dy)))]
  (/ (- (Math/acos (Math/max (- (/ (+ dy (/ (* dx dx) 1000)) t)) -1.0)) (Math/asin (/ dx t))) 2)))

;needed to solve equation
;2000 * (trg-x - pl-x) * sin(x) * cos(x) - 2000 * (trg-y - pl-y) * cos^2(x) = (trg-x - pl-x)^2
;used max because in some cases acos is calculated from values less than -1

(ufo-dynamic ufo-dynamic-solution)




;;; If you're still full of energy I propose you to add wind to simulation.
;;; Open core.clj file and try to figure out (it's not very easy) where missile speed is changed and try to add wind.
;;; And solve tasks with new obstacles.
