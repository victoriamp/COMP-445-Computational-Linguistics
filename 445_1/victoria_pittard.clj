(ns victoria-pittard)

;;1
(defn abs [x] (Math/sqrt (* x x)))

;;2
(defn take-square [x] (* x x))
(defn sum-of-squares [x y] (+ (take-square x) (take-square y)))

;;3
(def exp-13-1 '(+ 12 1))
(def exp-13-2 '(+ 10 3))
(def exp-13-3 '(+ 9 4))
(def exp-13-4 '(+ 8 5))

;;4
(defn third [x] (first (rest (rest x))))

;;5
(defn compose [f g] (fn [& n] (f (apply g n))))

;;6
(defn first-two [x] (list (first x) (first (rest x))))

;;7
(defn remove-second [x] (cons (first x) (rest (rest x))))

;;8
(def add-to-end (fn [l x]
                    (if (empty? l)
                      (list x)
                      (cons (first l) (add-to-end (rest l) x)))))

;;9
(def reverse (fn [l]
                 (if (empty? l)
                   '()
                   (add-to-end (first l) (reverse (rest l))))))
;;reverse doesn't quite work yet - check later

;;10
(def count-to-1 (fn [n]
                    (if (= 1 n)
                      (list n)
                      (cons n count-to-1 (- n 1)))))

;;11
(defn count-to-n [n] (reverse (count-to-1 n)))

;;12
(def get-max (fn [l]
                 (if (empty? l)
                   0
                   (max (first l) (get-max (rest l))))))

;;13
(def greater-than-five? (fn [l]
                            (map (fn [x] (> x 5)) l)))

;;14
(def concat-three (fn [x y z]
                      (flatten (cons x (cons y z)))))

;;15
(def sequence-to-power (fn [x n]
                           (if (zero? n)
                             '()
                           (flatten (cons x (sequence-to-power x (- n 1))))))
;; stp doesn't quite work, try again later

;;16
(def in-L*? (fn [x]
                (if (empty? x)
                  true
                  (if (prefix? '(a) x)
                    (in-L*? (rest (rest x)))
                    false))))