(defpackage #:memeozi/strategy
  (:use :cl :alexandria :memeozi/types)
  (:export #:purge))
(in-package :memeozi/strategy)

;; ----------------------------------------------------------------------------
(defmethod mean-field ((obj memo-fn) accessor)
  "Get the mean of an entry slot, used to approximate what entries to forget."
  (let* ((table (memo-fn-table obj))
         (numer (reduce #'+ (mapcar accessor (hash-table-values table))))
         (denom (hash-table-count table)))
    (/ numer denom)))

;; ----------------------------------------------------------------------------
(defmethod purge-old ((obj memo-fn))
  "Get rid of entries older than the age-limit, if set.
   Run this with all strategies first."
  (when-let ((timeout (memo-fn-age-limit obj)))
    (loop :for k :being :the :hash-keys :of (memo-fn-table obj)
          :for v := (gethash k (memo-fn-table obj))
          :when (> (get-universal-time) (+ timeout (memo-entry-age v)))
            :do (remhash k (memo-fn-table obj)))))

;; ----------------------------------------------------------------------------
(defmethod purge ((obj memo-fn) (_strategy (eql :mean-freq)))
  "Get rid of entries with below-average lookup counts."
  (purge-old obj)
  (let ((mean-count (mean-field obj #'memo-entry-count)))
    (loop :for k :being :the :hash-keys :of (memo-fn-table obj)
          :for v := (gethash k (memo-fn-table obj))
          :do (if (< (memo-entry-count v) mean-count)
                  (remhash k (memo-fn-table obj))
                  (setf (memo-entry-count v) 1)))))

;; ----------------------------------------------------------------------------
(defmethod purge ((obj memo-fn) (_strategy (eql :mean-age)))
  "Get rid of entries with older than average."
  (purge-old obj)
  (let ((mean-age (mean-field obj #'memo-entry-age)))
    (loop :for k :being :the :hash-keys :of (memo-fn-table obj)
          :for v := (gethash k (memo-fn-table obj))
          :when (< (memo-entry-age v) mean-age)
            :do (remhash k (memo-fn-table obj)))))

;; ----------------------------------------------------------------------------
(defmethod purge ((obj memo-fn) (_strategy (eql :thanos)))
  "Perfectly balanced."
  (purge-old obj)
  (loop :with i := 0
        :for k :being :the :hash-keys :of (memo-fn-table obj)
        :do (incf i)
        :when (evenp i)
          :do (remhash k (memo-fn-table obj))))
