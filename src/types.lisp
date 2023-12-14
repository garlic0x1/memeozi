(uiop:define-package #:memeozi/types
  (:use :cl :cl-annot.class :alexandria)
  (:import-from #:bt2 #:with-lock-held #:make-lock)
  (:import-from #:trivia #:match))
(in-package :memeozi/types)
(cl-annot:enable-annot-syntax)

;; ----------------------------------------------------------------------------
@export-class
(defclass memo-entry ()
  ((value
    :initarg :value
    :accessor memo-entry-value
    :documentation "The stored memo value")
   (count
    :initform 0
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
   (limit
    :initarg :limit
    :accessor memo-fn-limit
    :documentation "Size limit of the memo-table, nil means no limit")
   (timeout
    :initarg :timeout
    :accessor memo-fn-timeout
    :documentation "Maximum age of results")))

;; ----------------------------------------------------------------------------
@export
(defmethod lookup ((obj memo-fn) key)
  (with-lock-held ((memo-fn-lock obj))
    (when-let ((memo (gethash key (memo-fn-table obj))))
      (let ((timeout (memo-fn-timeout obj)))
        (unless (and timeout (> (get-universal-time) (+ timeout (memo-entry-age memo))))
          (incf (memo-entry-count memo))
          (memo-entry-value memo))))))

;; ----------------------------------------------------------------------------
@export
(defmethod purge ((obj memo-fn))
  (loop :with total := 0
        :with index := 0
        :for k :being :the :hash-keys :of (memo-fn-table obj)
        :for v := (gethash k (memo-fn-table obj))
        :do (when (< (memo-entry-count v) (/ total index))
              (remhash k (memo-fn-table obj)))
        :do (incf total (memo-entry-count v))
        :do (incf index)))

;; ----------------------------------------------------------------------------
@export
(defmethod calculate ((obj memo-fn) args)
  (apply (memo-fn-fn obj) args))

;; ----------------------------------------------------------------------------
@export
(defmethod record ((obj memo-fn) args value)
  (with-lock-held ((memo-fn-lock obj))
    (let ((limit (memo-fn-limit obj)))
      (when (and limit (> (hash-table-count (memo-fn-table obj)) limit))
        (purge obj))
      (setf (gethash args (memo-fn-table obj))
            (make-instance 'memo-entry :value value)))))
