(defpackage #:memeozi/core
  (:nicknames #:memeozi)
  (:use :cl :alexandria :memeozi/types :memeozi/methods)
  (:import-from :bt2 #:with-lock-held #:make-lock)
  (:export #:defmemo #:defmemo/simple))
(in-package :memeozi/core)

;; ----------------------------------------------------------------------------
(defun suffix-name (name suffix)
  (read-from-string (format nil "~a~a" name suffix)))

;; ----------------------------------------------------------------------------
(defmacro defmemo/simple (name args &body body)
  "Simple thread-safe memoization macro"
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
(defmacro defmemo (opts name args &body body)
  "Memoization with size and time-based limits"
  (let ((memo-name (suffix-name name "-memo")))
    `(progn
       (defparameter ,memo-name
         (make-instance
          'memo-fn
          :fn (lambda ,args ,@body)
          :size-limit ,(getf opts :size-limit)
          :age-limit ,(getf opts :age-limit)
          :strategy ,(getf opts :strategy :frequency)))
       (defun ,name ,args
         (if-let ((memo (lookup ,memo-name (list ,@args))))
           memo
           (let ((result (calculate ,memo-name (list ,@args))))
             (record ,memo-name (list ,@args) result)
             result))))))
