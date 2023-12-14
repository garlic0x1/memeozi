(defpackage #:memeozi/methods
  (:use :cl :alexandria #:memeozi/types)
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
