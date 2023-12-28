(defpackage #:memeozi/methods
  (:use :cl :alexandria #:memeozi/types #:memeozi/strategy)
  (:import-from :bt2 #:with-lock-held)
  (:export #:lookup #:record #:calculate))
(in-package :memeozi/methods)

;; ----------------------------------------------------------------------------
(defmethod lookup-internal ((obj memo-fn) key)
  (when-let ((memo (gethash key (memo-fn-table obj))))
    (let ((timeout (memo-fn-age-limit obj)))
      (unless (and timeout (> (get-universal-time) (+ timeout (memo-entry-age memo))))
        (incf (memo-entry-count memo))
        (memo-entry-value memo)))))

;; ----------------------------------------------------------------------------
(defmethod lookup ((obj memo-fn) key)
  (if-let (lock (memo-fn-lock obj))
    (with-lock-held (lock)
      (lookup-internal obj key))
    (lookup-internal obj key)))

;; ----------------------------------------------------------------------------
(defmethod calculate ((obj memo-fn) args)
  (apply (memo-fn-fn obj) args))

;; ----------------------------------------------------------------------------
(defmethod record-internal ((obj memo-fn) args value)
  (let ((limit (memo-fn-size-limit obj)))
    (when (and limit (>= (hash-table-count (memo-fn-table obj)) limit))
      (purge obj (memo-fn-strategy obj)))
    (setf (gethash args (memo-fn-table obj))
          (make-instance 'memo-entry :value value))))

;; ----------------------------------------------------------------------------
(defmethod record ((obj memo-fn) args value)
  (if-let (lock (memo-fn-lock obj))
    (with-lock-held (lock)
      (record-internal obj args value))
    (record-internal obj args value)))
