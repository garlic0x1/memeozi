(defpackage #:memeozi
  (:use :cl :alexandria)
  (:import-from :bt2 #:with-lock-held #:make-lock)
  (:export #:defmemo
           #:defmemo/atomic
           #:defmemo/timeout
           #:defmemo/int))
(in-package :memeozi)

;; ----------------------------------------------------------------------------
(defun suffix-name (name suffix)
  (read-from-string (format nil "~a~a" name suffix)))

;; ----------------------------------------------------------------------------
(defmacro defmemo (name args &body body)
  "General memoization macro"
  (let ((memo-name (suffix-name name "-memo")))
    `(progn (defparameter ,memo-name (make-hash-table :test #'equal))
            (defun ,name ,args
              (if-let ((memo (gethash (list ,@args) ,memo-name)))
                memo
                (setf (gethash (list ,@args) ,memo-name)
                      (progn ,@body)))))))

;; ----------------------------------------------------------------------------
(defmacro defmemo/atomic (name args &body body)
  "General thread-safe memoization macro"
  (let ((memo-name (suffix-name name "-memo"))
        (lock-name (suffix-name name "-memo-lock")))
    `(progn (defparameter ,memo-name (make-hash-table :test #'equal))
            (defparameter ,lock-name (make-lock))
            (defun ,name ,args
              (if-let ((memo (with-lock-held (,lock-name)
                               (gethash (list ,@args) ,memo-name))))
                memo
                (let ((result (progn ,@body)))
                  (with-lock-held (,lock-name)
                    (setf (gethash (list ,@args) ,memo-name) result))))))))

;; ----------------------------------------------------------------------------
(defmacro defmemo/timeout (timeout name args &body body)
  "General thread-safe memoization macro, result is recalculated if the memoized value is older than timeout seconds"
  (let ((memo-name (suffix-name name "-memo"))
        (lock-name (suffix-name name "-memo-lock")))
    `(progn (defparameter ,memo-name (make-hash-table :test #'equal))
            (defparameter ,lock-name (make-lock))
            (defun ,name ,args
              (let ((memo (with-lock-held (,lock-name)
                            (gethash (list ,@args) ,memo-name))))
                (if (and memo (< (get-universal-time) (+ ,timeout (car memo))))
                    (cdr memo)
                    (let ((result (progn ,@body)))
                      (with-lock-held (,lock-name)
                        (setf (gethash (list ,@args) ,memo-name)
                              (cons (get-universal-time) result)))
                      result)))))))

;; ----------------------------------------------------------------------------
(defmacro defmemo/int (size name (in) &body body)
  "Memoization optimized for quick maths, input must be an integer within the bounds of the vector size"
  (let ((memo-name (suffix-name name "-memo")))
    `(progn (defparameter ,memo-name (make-array (+ 1 ,size) :initial-element nil))
            (defun ,name (,in)
              (if-let ((memo (aref ,memo-name ,in)))
                memo
                (setf (aref ,memo-name ,in) (progn ,@body)))))))
