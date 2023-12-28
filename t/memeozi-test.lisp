(defpackage #:memeozi-test
  (:use :cl :fiveam :alexandria-2 :memeozi :memeozi/types)
  (:import-from :trivia :match))
(in-package :memeozi-test)

(def-suite :memeozi
  :description "Tests for Memeozi")
(in-suite :memeozi)

;; ----------------------------------------------------------------------------
(defun memo-count (m)
  (hash-table-count (memo-fn-table m)))

;; ----------------------------------------------------------------------------
(test :macros
  (defmemo (:size-limit 100 :age-limit 30) add (a b)
    (+ a b))

  ;; make sure limits initialized correctly
  (is (= 100 (memo-fn-size-limit add-memo)))
  (is (= 30 (memo-fn-age-limit add-memo)))
  ;; mean-freq is the default strategy
  (is (eql :mean-freq (memo-fn-strategy add-memo)))

  ;; make sure test inits correctly
  (defmemo (:test #'eql) my-eql (a b)
    (eql a b))
  (is (eql 'eql (hash-table-test (memo-fn-table my-eql-memo)))))

;; ----------------------------------------------------------------------------
(test :basic
  (defmemo/simple square (x)
    (* x x))

  (is (= 64 (square 8)))
  (is (= 64 (square 8)))
  (is (= 1 (hash-table-count square-memo)))
  (is (= 16 (square 4)))
  (is (= 2 (hash-table-count square-memo))))

;; ----------------------------------------------------------------------------
(test :list-args
  (defmemo () concat-str (str-list)
    (apply (curry #'concatenate 'string) str-list))

  (is (equal "hi world" (concat-str '("hi" " " "world"))))
  (is (= 1 (memo-count concat-str-memo)))
  (is (equal "hi world!" (concat-str '("hi" " " "world!"))))
  (is (= 2 (memo-count concat-str-memo)))
  (is (equal "hi world!" (concat-str '("hi" " " "world!"))))
  (is (= 2 (memo-count concat-str-memo))))

;; ----------------------------------------------------------------------------
(test :age-limit
  (let ((counter 0))
    (defmemo (:age-limit 1) square (x)
      (incf counter)
      (* x x))

    ;; calculates
    (square 2)
    ;; doesnt calculate
    (square 2)
    (square 2)
    (square 1)
    (is (= 3 (memo-entry-count (gethash '(2) (memo-fn-table square-memo)))))
    (is (= 2 counter))
    (sleep 2)
    ;; calculates
    (square 2)
    (is (= 3 counter))
    ;; (is (= 1 (memo-entry-count (gethash '(2) (memo-fn-table square-memo)))))
    (is (= 2 (memo-count square-memo)))))

;; ----------------------------------------------------------------------------
(test :equality
  (defmemo () my-equal (a b)
    (equal a b))

  (is (not (my-equal (float 1) 1)))
  (is (my-equal 1 1))
  (is (= 2 (memo-count my-equal-memo)))

  (defmemo (:test #'equalp) my-equalp (a b)
    (equalp a b))

  (is (my-equalp (float 1) 1))
  (is (my-equalp 1 1))
  (is (= 1 (memo-count my-equalp-memo)))

  (defmemo (:test #'eql) my-eql (a b)
    (eql a b))

  (is (not (my-eql 1 1.0)))
  (is (my-eql 1 1))
  (is (my-eql :hi :hi)))

;; ----------------------------------------------------------------------------
(test :multi-value
  (defmemo () vals (a b)
    (values a b))

  (is (equal '(1 2) (multiple-value-list (vals 1 2))))
  (is (equal '(1 "hi") (multiple-value-list (vals 1 "hi"))))
  (is (= 1 (vals 1 2))))

;; ----------------------------------------------------------------------------
(test :race
  (defmemo () square (x)
    (* x x))

  (let ((t1 (bt2:make-thread
             (lambda () (loop :for i :from 1 :to 100
                         :sum (square i) :into total
                         :finally (return total)))))
        (t2 (bt2:make-thread
             (lambda () (loop :for i :from 100 :to 200
                         :sum (square i) :into total
                         :finally (return total))))))
    (let ((r1 (bt2:join-thread t1))
          (r2 (bt2:join-thread t2)))
      (is (= r1 338350))
      (is (= r2 2358350)))))
