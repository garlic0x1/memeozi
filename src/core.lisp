(uiop:define-package #:memeozi
  (:use :cl :alexandria :cl-annot.class)
  (:import-from :bt2 #:with-lock-held #:make-lock)
  (:export #:defmemo))
(in-package :memeozi)
(cl-annot:enable-annot-syntax)

;; ----------------------------------------------------------------------------
(defun suffix-name (name suffix)
  (read-from-string (format nil "~a~a" name suffix)))

;; ----------------------------------------------------------------------------
(defmacro defmemo (opts name args &body body)
  (let ((memo-name (suffix-name name "-memo")))
    `(progn
       (defparameter ,memo-name
         (make-instance
          'memo-fn
          :fn (lambda ,args ,@body)
          :limit ,(getf opts :limit)
          :timeout ,(getf opts :timeout)))
       (defun ,name ,args
         (if-let ((memo (lookup ,memo-name (list ,@args))))
           memo
           (let ((result (calculate ,memo-name (list ,@args))))
             (record ,memo-name (list ,@args) result)
             result))))))

;; ----------------------------------------------------------------------------
@export-class
(defclass memo-entry ()
  ((value
    :initarg :value
    :accessor memo-entry-value
    :documentation "The stored memo value")
   (count
    :initform 1
    :accessor memo-entry-count
    :documentation "Frequency count of usage")
   (age
    :initform (get-universal-time)
    :accessor memo-entry-age
    :documentation "Time the result was computed")))

;; ----------------------------------------------------------------------------
@export-class
(defclass memo-fn ()
  ((fn
    :initarg :fn
    :accessor memo-fn-fn
    :documentation "Function to memoize")
   (table
    :initform (make-hash-table :test #'equal)
    :accessor memo-fn-table
    :documentation "Hash-table holding memos")
   (lock
    :initform (bt2:make-lock)
    :accessor memo-fn-lock
    :documentation "Lock to be used by internal methods")
   (size-limit
    :initarg :limit
    :accessor memo-fn-size-limit
    :documentation "Size limit of the memo-table, nil means no limit")
   (age-limit
    :initarg :timeout
    :accessor memo-fn-age-limit
    :documentation "Maximum age of results")))

;; ----------------------------------------------------------------------------
(defmethod lookup ((obj memo-fn) key)
  (with-lock-held ((memo-fn-lock obj))
    (when-let ((memo (gethash key (memo-fn-table obj))))
      (let ((timeout (memo-fn-age-limit obj)))
        (unless (and timeout (> (get-universal-time) (+ timeout (memo-entry-age memo))))
          (incf (memo-entry-count memo))
          (memo-entry-value memo))))))

;; ----------------------------------------------------------------------------
(defmethod mean-count ((obj memo-fn))
  (let* ((table (memo-fn-table obj))
         (numer (reduce #'+ (mapcar #'memo-entry-count (hash-table-values table))))
         (denom (hash-table-count table)))
    (ceiling (/ numer denom))))

;; ----------------------------------------------------------------------------
(defmethod purge ((obj memo-fn))
  (let* ((mean (mean-count obj))
         (timeout (memo-fn-age-limit obj))
         (too-old (lambda (v) (and timeout (< (- (get-universal-time) timeout) (memo-entry-age v)))))
         (too-rare (lambda (v) (< (memo-entry-count v) mean))))
    (loop :for k :being :the :hash-keys :of (memo-fn-table obj)
          :for v := (gethash k (memo-fn-table obj))
          :do (when (or (funcall too-old v) (funcall too-rare v))
                (remhash k (memo-fn-table obj))))))

;; ----------------------------------------------------------------------------
(defmethod calculate ((obj memo-fn) args)
  (apply (memo-fn-fn obj) args))

;; ----------------------------------------------------------------------------
(defmethod record ((obj memo-fn) args value)
  (with-lock-held ((memo-fn-lock obj))
    (let ((limit (memo-fn-size-limit obj)))
      (when (and limit (> (hash-table-count (memo-fn-table obj)) limit))
        (purge obj))
      (setf (gethash args (memo-fn-table obj))
            (make-instance 'memo-entry :value value)))))
