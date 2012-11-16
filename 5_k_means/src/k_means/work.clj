(ns k-means.work
  (:use [k-means.core :only [run-empty run-2-circles run-3-circles run-random-circles]]))


;;; Your task is to implement clustering algorithm.
;;; You're a given a set of points on plane. And your goal is to divide them to k clusters.
;;; Implement k-means algorithm to solve this task: http://en.wikipedia.org/wiki/K-means_clustering
;;; Your function must take collection of points. Each point is a vector of x and y.
;;; It must return collection of clusters. Each cluster - collection of points.
;;; E.g. you have 4 points: [0 0] [1 1] [9 9] [10 10] and you need to partition them to 2 clusters.
;;; Input will be [[0 0] [9 9] [1 1] [10 10]] and output should be something like [[[0 0] [1 1]] [[9 9] [10 10]]].
;;; Note that you don't get k - number of clusters. You need to specify it somewhere in function.
;;; To test you solution use following tests:

; (run-empty SOLUTION)

(defn distance [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn mean [points]
  (map #(/ % (count points)) (apply map + points)))

(defn run-2-circles-solution [points]
  ((fn lloyd [center1 center2]
    (let [parts (reduce #(if (<= (distance %2 center1) (distance %2 center2)) [(conj (first %1) %2) (last %1)] [(first %1) (conj (last %1) %2)]) [[] []] points)
          new-center1 (mean (first parts))
          new-center2 (mean (last parts))]
      (if (and (= center1 new-center1) (= center2 new-center2)) parts (lloyd new-center1 new-center2)))) (first points) (last points)))

;(run-2-circles run-2-circles-solution)

(defn run-3-circles-solution [points]
  ((fn lloyd [center1 center2 center3]
    (let [parts (reduce #(let [d1 (distance %2 center1)
                               d2 (distance %2 center2)
                               d3 (distance %2 center3)]
                           (if (<= d1 d2)
                             (if (<= d1 d3) [(conj (first %1) %2) (second %1) (last %1)]
                                            [(first %1) (second %1) (conj (last %1) %2)])
                             (if (<= d2 d3) [(first %1) (conj (second %1) %2) (last %1)]
                                            [(first %1) (second %1) (conj (last %1) %2)]))) [[] [] []] points)
          new-center1 (mean (first parts))
          new-center2 (mean (second parts))
          new-center3 (mean (last parts))]
      (if (and (= center1 new-center1) (= center2 new-center2) (= center3 new-center3)) parts (lloyd new-center1 new-center2 new-center3)))) (first points) (nth points (/ (count points) 2)) (last points)))

(run-3-circles run-3-circles-solution)

;;; Manipulation: mouse click - add new point
;;;               space - reset simulation (remove all points or regenerate them, depends on test)
;;; Note that may need use different solutions (with k = 2 for run-2-circles and  k = 3 for run-3-circles).



;;; Now try to improve your solution so it can determine k based on given points. So if there are visually 3 clusters it should partition points to 3 clusters, if 4 than to 4 clusters.
;;; I have no idea how to do it, excluding some unrealizable ones from here: http://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set
;;; Test your solution on this test:

; (run-random-circles SOLUTION)



;;; Implement some other clustering algorithm.
