(defpackage #:math-test
  (:use :cl :fiveam :alexandria-2 :memeozi :memeozi/types)
  (:import-from :trivia :match))
(in-package :math-test)

(def-suite :math
  :description "Math tests as examples and benchmarks")
(in-suite :math)

;; ----------------------------------------------------------------------------
(test :collatz-chain
  ;; find the number that creates the longest collatz sequence under one million
  (defun next (n)
    (if (evenp n)
        (/ n 2)
        (+ 1 (* 3 n))))

  (defmemo () distance (n)
    (if (= 1 n)
        1
        (+ 1 (distance (next n)))))

  (defun solve ()
    (loop :with max := 1
          :for i :from 1 :to 10000
          :when (> (distance i) (distance max))
            :do (setf max i)
          :finally (return max)))

  (is (= 6171 (solve)))
  )

;; (test :collatz-chain-2
;;   ;; find the number that creates the longest collatz sequence under one million
;;   (defun next (n)
;;     (if (evenp n)
;;         (/ n 2)
;;         (+ 1 (* 3 n))))

;;   (defmemo () distance (n)
;;     (if (= 1 n)
;;         1
;;         (+ 1 (distance (next n)))))

;;   (defun solve ()
;;     (loop :with max := 1
;;           :for i :from 1 :to 1200000
;;           :when (> (distance i) (distance max))
;;             :do (setf max i)
;;           :finally (return max)))

;;   (format t "time: ~a" (time (solve)))

;;   (is (= 837799 (solve)))
;;   )

;; ----------------------------------------------------------------------------
(test :square-sum-chain
  (defun square (n) (* n n))

  (defun digits (n) (map 'list #'digit-char-p (prin1-to-string n)))

  (defun next (n)
    (line-up-last
     (digits n)
     (mapcar #'square)
     (apply #'+)))

  (defmemo () chain (n)
    (match (next n)
      (1 1)
      (89 89)
      (next (chain next))))

  (defun solve ()
    (loop :for i :from 1 :to 100000
          :for chain := (chain i)
          :count (= chain 89)))

  (is (= 85623 (solve))))
