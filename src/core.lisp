(defpackage #:memeozi
  (:use :cl :alexandria)
  (:export #:defmemo/int->int
           #:defmemo
           #:defmemo/atomic))
(in-package :memeozi)

;; ----------------------------------------------------------------------------
(defmacro defmemo (name args &body body)
  "General memoization macro"
  (let ((memo-name (read-from-string (format nil "~a-memo" name))))
    `(progn (defparameter ,memo-name (make-hash-table :test #'equal))
            (defun ,name ,args
              (if-let ((memo (gethash (list ,@args) ,memo-name)))
                memo
                (setf (gethash (list ,@args) ,memo-name)
                      (progn ,@body)))))))

;; ----------------------------------------------------------------------------
(defmacro defmemo/atomic (name args &body body)
  "General thread-safe memoization macro"
  (let ((memo-name (read-from-string (format nil "~a-memo" name)))
        (lock-name (read-from-string (format nil "~a-memo-lock" name))))
    `(progn (defparameter ,memo-name (make-hash-table :test #'equal))
            (defparameter ,lock-name (bt:make-lock))
            (defun ,name ,args
              (if-let ((memo (bt:with-lock-held (,lock-name)
                               (gethash (list ,@args) ,memo-name))))
                memo
                (let ((result (progn ,@body)))
                  (bt:with-lock-held (,lock-name)
                    (setf (gethash (list ,@args) ,memo-name) result))))))))

;; ----------------------------------------------------------------------------
(defmacro defmemo/int->int (size name (in) &body body)
  "Memoization optimized for quick maths"
  (let ((memo-name (read-from-string (format nil "~a-memo" name))))
    `(progn (defparameter ,memo-name (make-array (+ 1 ,size) :initial-element nil))
            (defun ,name (,in)
              (if-let ((memo (aref ,memo-name ,in)))
                memo
                (setf (aref ,memo-name ,in) (progn ,@body)))))))
