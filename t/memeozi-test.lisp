(defpackage #:memeozi-test
  (:use :cl :fiveam :alexandria-2 :memeozi)
  (:import-from :trivia :match))
(in-package :memeozi-test)

(def-suite :memeozi
  :description "Tests for Memeozi")
(in-suite :memeozi)

;; ----------------------------------------------------------------------------
(test :basic
  (defmemo () square (x)
    (* x x))
  (is (= 64 (square 8)))
  (is (= 1 (hash-table-count (memo-fn-table square-memo))))

  (is (= 64 (square 8)))
  (is (= 1 (hash-table-count (memo-fn-table square-memo))))

  (is (= 16 (square 4)))
  (is (= 2 (hash-table-count (memo-fn-table square-memo)))))

;; ----------------------------------------------------------------------------
(test :list-args
  (defmemo () concat-str (str-list)
    (apply (curry #'concatenate 'string) str-list))

  (is (equal "hi world" (concat-str '("hi" " " "world"))))
  (is (= 1 (hash-table-count (memo-fn-table concat-str-memo))))

  (is (equal "hi world!" (concat-str '("hi" " " "world!"))))
  (is (= 2 (hash-table-count (memo-fn-table concat-str-memo))))

  (is (equal "hi world!" (concat-str '("hi" " " "world!"))))
  (is (= 2 (hash-table-count (memo-fn-table concat-str-memo)))))

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

  (is (= 6171 (solve))))

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

;; ----------------------------------------------------------------------------
(test :timeout
  (let ((counter 0))
    (defmemo (:timeout 1) square (x)
      (incf counter)
      (* x x))

    ;; calculates
    (square 2)
    ;; doesnt calculate
    (square 2)
    (square 2)
    (sleep 2)
    ;; calculates
    (square 2)
    (is (= counter 2))))
