(ns church-encoding.work
  (:use [church-encoding.core]))

;;; Task is to implement arithmetic on Church numerals.
;;; Check this page: http://en.wikipedia.org/wiki/Church_encoding
;;; You can use utility function to-church-num and to-normal-num to convert normal to church and church to normal:
;;; Note that to-church-num returns function that takes 1 argument (f)
;;; and returns function that takes 1 argument (x) that calculates (f (f ... (f x)...))
;;; All functions in this task must be 1 argument functions that return other functions.

;;; Example:

(def church-5 (to-church-num 5))    ; 5 in church numerals

(defn print-star [x] (print "*") x) ; Takes 1 argument, prints a star and retuns argument without modification.

((church-5 print-star) nil)         ; Prints ***** to console
(println)

(to-normal-num church-5)            ; returns 5

(def church-2 (to-church-num 2))    ; we'll use it in examples later



;;; Implement + (plus) for church numerals.

(defn plus [m]
  (fn [n]
    (fn [f]
      (fn [x]
        ((m f) ((n f) x))))))

(to-normal-num ((plus church-2) church-2)) ; must return 4

(test-plus plus) ; test your solution



;;; Implement * (multiplication) for church numerals

(defn mult [m]
  (fn [n]
    (fn [f] (m (n f)))))

(to-normal-num ((mult church-2) church-5)) ; must return 10

(test-mult mult) ; test your solution



;;; Implement ^ (pow function) for church numerals.

(defn pow [m]
  (fn [n] (n m)))

(to-normal-num ((pow church-2) church-5)) ; must return 32

(test-pow pow) ; test your solution



;;; Implement dec function for church numerals.

(defn church-dec [n]
  (fn [f]
    (fn [x]
      (((n (fn [g]
             (fn [h]
               (h (g f))))) (fn [u] x)) (fn [u] u)))))

(to-normal-num (church-dec church-5)) ; must return 4

(test-dec church-dec) ; test your solution



;;; Implement sum function. sum takes number n and returns sum of all numbers less or equals to n.
;;; You'll need to use recursion here. For recursion you'll need lazy values.
;;; You can use delay for that: http://clojuredocs.org/clojure_core/1.2.0/clojure.core/delay

(defn church-true [a]
  (fn [b] a))

(defn church-false [a]
  (fn [b] b))

(defn church-zero? [n]
  ((n (fn [x] church-false)) church-true))

(defn church-if [m]
  (fn [a]
    (fn [b] ((m a) b))))

(defn recur-sum [r]
  (fn [n]
    @(((church-if (church-zero? n)) (delay (to-church-num 0)))
                                    (delay ((plus n) ((r r) (church-dec n)))))))

(defn sum [n]
  ((recur-sum recur-sum) n))

(to-normal-num (sum church-2)) ; must return 3

(test-sum sum)


;;; Implement set of function to create/manipulate lists.
;;; Your need to implement following functions:
;;; empty? - checks if list is empty, returns true or false. see church booleans http://en.wikipedia.org/wiki/Church_encoding#Church_booleans
;;; empty-list - used as "end" of the list.
;;; head - returns head of a list
;;; tail - returns tail of a list
;;; cons - takes 2 arguments h and t, and creates a list such that (head ((cons a) b)) = a, (tail ((cons a) b)) = b
;;;
;;; Help: http://en.wikipedia.org/wiki/Church_encoding#List_encodings

(defn pair [x]
  (fn [y]
    (fn [z] ((z x) y))))

(defn church-first [p]
  (p church-true))

(defn church-second [p]
  (p church-false))

(def church-empty? church-first)

(def empty-list ((pair church-true) church-true))

(defn head [l]
  (church-first (church-second l)))

(defn tail [l]
  (church-second (church-second l)))

(defn church-cons [x]
  (fn [l]
    ((pair church-false) ((pair x) l))))

(((church-empty? empty-list) true) false) ; must return true

(head ((church-cons "Hello") empty-list)) ; must return "Hello"

(let [list ((church-cons "Hello") empty-list)
      t (tail list)]
  ((church-empty? t) true) false) ; must return true

(test-list {:empty? church-empty?
            :empty-list empty-list
            :head head
            :tail tail
            :cons church-cons}) ; test your solution



;;; Additional task.
;;; Implement map and reduce functions for lambda lists.
;;; map takes 2 arguments: function and list
;;; reduce takes 3 arguments: function, init value and list

(defn recur-map [r]
  (fn [f]
    (fn [l]
      @(((church-if (church-empty? l)) (delay empty-list))
                                       (delay ((church-cons (f (head l))) (((r r) f) (tail l))))))))

(defn church-map [f]
  (fn [l]
    (((recur-map recur-map) f) l)))

(defn recur-reduce [r]
  (fn [f]
    (fn [i]
      (fn [l]
        @(((church-if (church-empty? l)) (delay i))
                                         (delay ((((r r) f) ((f i) (head l))) (tail l))))))))

(defn church-reduce [f]
  (fn [i]
    (fn [l]
      ((((recur-reduce recur-reduce) f) i) l))))

(test-map-reduce {:empty? church-empty?
                  :empty-list empty-list
                  :head head
                  :tail tail
                  :cons church-cons
                  :map church-map
                  :reduce church-reduce})
