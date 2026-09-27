(defpackage #:almighty-toki/duration
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:ensure-toki)
  (:import-from #:local-time
                #:timestamp-difference
                #:timestamp<)
  (:export #:duration
           #:toki-
           #:toki+
           #:duration-years
           #:duration-months
           #:duration-weeks
           #:duration-days
           #:duration-hours
           #:duration-minutes
           #:duration-seconds
           #:duration-nanoseconds
           #:in-years
           #:in-months
           #:in-weeks
           #:in-days
           #:in-hours
           #:in-minutes
           #:in-seconds
           #:total-seconds
           #:in-words
           #:duration+
           #:duration-
           #:duration-zerop
           #:duration-negate))

(in-package #:almighty-toki/duration)

;;; Approximation constants
(defconstant +seconds-per-minute+ 60)
(defconstant +seconds-per-half-hour+ 1800)
(defconstant +seconds-per-hour+ 3600)
(defconstant +seconds-per-day+ 86400)
(defconstant +seconds-per-week+ 604800)
(defconstant +days-per-year+ 365.25d0)
(defconstant +days-per-month+ (/ 365.25d0 12))

(defstruct (duration (:constructor %make-duration)
                     (:conc-name duration-))
  (years 0 :type integer)
  (months 0 :type integer)
  (weeks 0 :type integer)
  (days 0 :type integer)
  (hours 0 :type integer)
  (minutes 0 :type integer)
  (seconds 0 :type integer)
  (nanoseconds 0 :type integer))

(defun duration (&key (years 0) (months 0) (weeks 0) (days 0)
                   (hours 0) (minutes 0) (seconds 0) (nanoseconds 0))
  "Create a duration with the specified components."
  (%make-duration :years years :months months :weeks weeks :days days
                  :hours hours :minutes minutes :seconds seconds
                  :nanoseconds nanoseconds))

(defun total-seconds (dur)
  "Return total seconds represented by DUR, using approximations for months/years.
Returns an integer when possible (no fractional components), otherwise a double-float."
  (let ((has-approx (or (not (zerop (duration-years dur)))
                        (not (zerop (duration-months dur))))))
    (if has-approx
        ;; Use floating point for approximate conversions
        (+ (* (duration-years dur) +days-per-year+ +seconds-per-day+)
           (* (duration-months dur) +days-per-month+ +seconds-per-day+)
           (* (duration-weeks dur) +seconds-per-week+)
           (* (duration-days dur) +seconds-per-day+)
           (* (duration-hours dur) +seconds-per-hour+)
           (* (duration-minutes dur) +seconds-per-minute+)
           (duration-seconds dur)
           (/ (duration-nanoseconds dur) 1000000000.0d0))
        ;; Exact integer arithmetic
        (+ (* (duration-weeks dur) +seconds-per-week+)
           (* (duration-days dur) +seconds-per-day+)
           (* (duration-hours dur) +seconds-per-hour+)
           (* (duration-minutes dur) +seconds-per-minute+)
           (duration-seconds dur)))))

(defun total-seconds-integer (dur)
  "Return total seconds as an integer (truncated)."
  (truncate (total-seconds dur)))

(defun in-years (dur)
  "Return approximate total years (truncated)."
  (truncate (/ (total-seconds dur) +seconds-per-day+ +days-per-year+)))

(defun in-months (dur)
  "Return approximate total months (truncated)."
  (truncate (/ (total-seconds dur) +seconds-per-day+ +days-per-month+)))

(defun in-weeks (dur)
  "Return total weeks (truncated)."
  (truncate (/ (total-seconds dur) +seconds-per-week+)))

(defun in-days (dur)
  "Return total days (truncated)."
  (truncate (/ (total-seconds dur) +seconds-per-day+)))

(defun in-hours (dur)
  "Return total hours (truncated)."
  (truncate (/ (total-seconds dur) +seconds-per-hour+)))

(defun in-minutes (dur)
  "Return total minutes (truncated)."
  (truncate (/ (total-seconds dur) +seconds-per-minute+)))

(defun in-half-hours (dur)
  "Return total minutes (truncated)."
  (truncate (/ (total-seconds dur) +seconds-per-half-hour+)))
(in-half-hours (duration :hours 1))
                                        ; => 2, 0

(defun in-seconds (dur)
  "Return total seconds (truncated)."
  (total-seconds-integer dur))

(defun toki- (t1 t2 &key (absolute t))
  "Compute the duration between two timestamps, or subtract a duration from a timestamp.
When ABSOLUTE is T (default), always returns a positive duration."
  (etypecase t2
    (local-time:timestamp
     (let* ((diff (timestamp-difference t1 t2))
            (sign (if (and (not absolute) (timestamp< t1 t2)) -1 1))
            (abs-diff (abs diff))
            (total-days (truncate abs-diff +seconds-per-day+))
            (remaining (- abs-diff (* total-days +seconds-per-day+)))
            (total-hours (truncate remaining +seconds-per-hour+))
            (remaining (- remaining (* total-hours +seconds-per-hour+)))
            (total-minutes (truncate remaining +seconds-per-minute+))
            (total-secs (truncate (- remaining (* total-minutes +seconds-per-minute+)))))
       (duration :days (* sign total-days)
                 :hours (* sign total-hours)
                 :minutes (* sign total-minutes)
                 :seconds (* sign total-secs))))
    (duration
     ;; Subtract a duration from a timestamp
     (toki+ t1 (duration-negate t2)))))

(defun toki+ (timestamp dur)
  "Add a duration to a timestamp, returning a new toki."
  (let ((result (local-time:clone-timestamp timestamp)))
    (macrolet ((offset-if (slot unit)
                 `(let ((v (,slot dur)))
                    (unless (zerop v)
                      (setf result (local-time:timestamp+ result v ,unit))))))
      (offset-if duration-years :year)
      (offset-if duration-months :month)
      (when (not (zerop (duration-weeks dur)))
        (setf result (local-time:timestamp+ result (* 7 (duration-weeks dur)) :day)))
      (offset-if duration-days :day)
      (offset-if duration-hours :hour)
      (offset-if duration-minutes :minute)
      (offset-if duration-seconds :sec))
    (ensure-toki result)))

(defun duration+ (d1 d2)
  "Add two durations component-wise."
  (duration :years (+ (duration-years d1) (duration-years d2))
            :months (+ (duration-months d1) (duration-months d2))
            :weeks (+ (duration-weeks d1) (duration-weeks d2))
            :days (+ (duration-days d1) (duration-days d2))
            :hours (+ (duration-hours d1) (duration-hours d2))
            :minutes (+ (duration-minutes d1) (duration-minutes d2))
            :seconds (+ (duration-seconds d1) (duration-seconds d2))
            :nanoseconds (+ (duration-nanoseconds d1) (duration-nanoseconds d2))))

(defun duration- (d1 d2)
  "Subtract D2 from D1 component-wise."
  (duration+ d1 (duration-negate d2)))

(defun duration-zerop (dur)
  "Return T if all components of DUR are zero."
  (and (zerop (duration-years dur))
       (zerop (duration-months dur))
       (zerop (duration-weeks dur))
       (zerop (duration-days dur))
       (zerop (duration-hours dur))
       (zerop (duration-minutes dur))
       (zerop (duration-seconds dur))
       (zerop (duration-nanoseconds dur))))

(defun duration-negate (dur)
  "Return a new duration with all components negated."
  (duration :years (- (duration-years dur))
            :months (- (duration-months dur))
            :weeks (- (duration-weeks dur))
            :days (- (duration-days dur))
            :hours (- (duration-hours dur))
            :minutes (- (duration-minutes dur))
            :seconds (- (duration-seconds dur))
            :nanoseconds (- (duration-nanoseconds dur))))

(defun pluralize (n unit)
  "Return \"N unit\" or \"N units\" as appropriate."
  (format nil "~D ~A~:[s~;~]" n unit (= n 1)))

(defun in-words (dur)
  "Return a human-readable string representation of DUR."
  (let ((parts nil))
    (macrolet ((check (accessor name)
                 `(let ((v (,accessor dur)))
                    (unless (zerop v)
                      (push (pluralize (abs v) ,name) parts)))))
      (check duration-years "year")
      (check duration-months "month")
      (check duration-weeks "week")
      (check duration-days "day")
      (check duration-hours "hour")
      (check duration-minutes "minute")
      (check duration-seconds "second")
      (check duration-nanoseconds "nanosecond"))
    (if parts
        (format nil "~{~A~^ ~}" (nreverse parts))
        "0 seconds")))

(defmethod print-object ((dur duration) stream)
  (print-unreadable-object (dur stream :type t)
    (write-string (in-words dur) stream)))
