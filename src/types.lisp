(uiop:define-package #:memeozi/types
  (:use :cl :cl-annot.class))
(in-package :memeozi/types)
(cl-annot:enable-annot-syntax)

;; ----------------------------------------------------------------------------
@export-class
(defclass memo-entry ()
  ((value
    :initarg :value
    :initform (error ":value must be specified")
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
    :initform (error ":fn must be specified")
    :accessor memo-fn-fn
    :documentation "Function to memoize")
   (table
    :initarg :table
    :initform (error ":table must be specified")
    :accessor memo-fn-table
    :documentation "Hash-table holding memos")
   (lock
    :initarg :lock
    :initform (bt2:make-lock)
    :accessor memo-fn-lock
    :documentation "Lock to be used by internal methods")
   (strategy
    :initarg :strategy
    :initform (error ":strategy must be specified")
    :accessor memo-fn-strategy
    :documentation "Memoization strategy to use, effects purge method")
   (size-limit
    :initarg :size-limit
    :initform (error ":size-limit must be specified")
    :accessor memo-fn-size-limit
    :documentation "Size limit of the memo-table, nil means no limit")
   (age-limit
    :initarg :age-limit
    :initform (error ":age-limit must be specified")
    :accessor memo-fn-age-limit
    :documentation "Maximum age of results")))
