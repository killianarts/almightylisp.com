(defpackage #:almighty-toki/diff-human
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:resolve-tz)
  (:import-from #:local-time
                #:now
                #:timestamp-difference
                #:timestamp<
                #:timestamp-year
                #:timestamp-month
                #:*default-timezone*)
  (:export #:diff-for-humans))

(in-package #:almighty-toki/diff-human)

(defun pluralize (n unit)
  (format nil "~D ~A~:[s~;~]" n unit (= n 1)))

(defun diff-for-humans (ts &key other absolute timezone)
  "Return a human-readable string describing the time difference.
Without :OTHER, compares to now. With :OTHER, compares to that timestamp.
With :ABSOLUTE T, omits direction words."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (reference (or other (now)))
         (diff-seconds (timestamp-difference reference ts))
         (abs-seconds (abs diff-seconds))
         (past-p (> diff-seconds 0)))  ; ts is before reference
    ;; Determine unit and count
    (multiple-value-bind (count unit-name)
        (cond
          ((< abs-seconds 60)
           (values 0 nil))  ; "just now"
          ((< abs-seconds 3600)
           (values (truncate abs-seconds 60) "minute"))
          ((< abs-seconds 86400)
           (values (truncate abs-seconds 3600) "hour"))
          ((< abs-seconds 604800)  ; 7 days
           (values (truncate abs-seconds 86400) "day"))
          ((< abs-seconds 2419200)  ; ~28 days
           (values (truncate abs-seconds 604800) "week"))
          (t
           ;; For larger durations, use calendar-aware calculation
           (let* ((y1 (timestamp-year reference :timezone tz))
                  (m1 (timestamp-month reference :timezone tz))
                  (y2 (timestamp-year ts :timezone tz))
                  (m2 (timestamp-month ts :timezone tz))
                  (month-diff (abs (+ (* (- y1 y2) 12) (- m1 m2)))))
             (if (>= month-diff 12)
                 (values (truncate month-diff 12) "year")
                 (values month-diff "month")))))
      (cond
        ;; "just now" case
        ((null unit-name) "just now")
        ;; Absolute mode
        (absolute (pluralize count unit-name))
        ;; Relative to now (no :other)
        ((null other)
         (if past-p
             (format nil "~A ago" (pluralize count unit-name))
             (format nil "in ~A" (pluralize count unit-name))))
        ;; Relative to :other
        (t
         (if past-p
             (format nil "~A before" (pluralize count unit-name))
             (format nil "~A after" (pluralize count unit-name))))))))
