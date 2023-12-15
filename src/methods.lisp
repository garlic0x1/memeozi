(defpackage #:memeozi/methods
  (:use :cl :alexandria #:memeozi/types #:memeozi/strategy)
  (:import-from :bt2 #:with-lock-held)
  (:export #:lookup #:record #:calculate))
(in-package :memeozi/methods)

;; ----------------------------------------------------------------------------
(defmethod lookup ((obj memo-fn) key)
  (with-lock-held ((memo-fn-lock obj))
    (when-let ((memo (gethash key (memo-fn-table obj))))
      (let ((timeout (memo-fn-age-limit obj)))
        (unless (and timeout (> (get-universal-time) (+ timeout (memo-entry-age memo))))
          (incf (memo-entry-count memo))
          (memo-entry-value memo))))))

;; ----------------------------------------------------------------------------
(defmethod calculate ((obj memo-fn) args)
  (apply (memo-fn-fn obj) args))

;; ----------------------------------------------------------------------------
(defmethod record ((obj memo-fn) args value)
  (with-lock-held ((memo-fn-lock obj))
    (let ((limit (memo-fn-size-limit obj)))
      (when (and limit (>= (hash-table-count (memo-fn-table obj)) limit))
        (purge obj (memo-fn-strategy obj)))
      (setf (gethash args (memo-fn-table obj))
            (make-instance 'memo-entry :value value)))))
