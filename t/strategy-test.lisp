(defpackage #:strategy-test
  (:use :cl :fiveam :alexandria-2 :memeozi :memeozi/types)
  (:import-from :trivia :match))
(in-package :strategy-test)

(def-suite :strategy
  :description "Tests for Memeozi strategies")
(in-suite :strategy)

;; ----------------------------------------------------------------------------
(defun memo-count (m)
  (hash-table-count (memo-fn-table m)))

;; ----------------------------------------------------------------------------
(test :mean-freq
  (defmemo (:strategy :mean-freq :size-limit 10) square (x)
    (* x x))

  (loop :for i :from 1 :to 100
        :do (square 100))
  (loop :for i :from 1 :to 9
        :do (square i))
  (is (= 10 (memo-count square-memo)))
  ;; purges when we add more
  (square 10)
  (is (= 2 (memo-count square-memo)))
  ;; retains 2
  (is (gethash '(100) (memo-fn-table square-memo)))
  ;; forgets 1
  (is (not (gethash '(1) (memo-fn-table square-memo))))
  ;; resets counts
  (loop :for v :being :the :hash-values :of (memo-fn-table square-memo)
        :do (is (= 1 (memo-entry-count v)))))

;; ----------------------------------------------------------------------------
(test :mean-age
  (defmemo (:strategy :mean-age :size-limit 200) square (x)
    (* x x))

  (loop :for i :from 1 :to 100
        :do (square i))

  (sleep 1)
  (loop :for i :from 1 :to 100
        :do (square (+ 100 i)))

  (is (= 200 (memo-count square-memo)))
  (square 9999)
  (is (= 101 (memo-count square-memo)))
  (is (gethash '(140) (memo-fn-table square-memo)))
  (is (not (gethash '(40) (memo-fn-table square-memo))))
  )


;; ----------------------------------------------------------------------------
(test :thanos
  (defmemo (:strategy :thanos :size-limit 10)
      square (x)
    (* x x))

  (loop :for i :from 1 :to 100
        :do (square 100))
  (loop :for i :from 1 :to 9
        :do (square i))
  (is (= 10 (memo-count square-memo)))
  ;; purges when we add more, so we get 5, then it adds square 10, making count 6
  (square 10)
  (is (= 6 (memo-count square-memo)))

  (defmemo (:strategy :thanos :size-limit 20)
      square2 (x)
    (* x x))
  (loop :for i :from 1 :to 100
        :do (square2 100))
  (loop :for i :from 1 :to 19
        :do (square2 i))
  (is (= 20 (memo-count square2-memo)))
  ;; purges when we add more, so we get 5, then it adds square 10, making count 6
  (square2 20)
  (is (= 11 (memo-count square2-memo)))
  )
