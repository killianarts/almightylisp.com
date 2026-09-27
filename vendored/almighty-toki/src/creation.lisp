(defpackage #:almighty-toki/creation
  (:use #:cl)
  (:import-from #:local-time
                #:timestamp
                #:encode-timestamp
                #:now
                #:timestamp+
                #:timestamp-
                #:timestamp-year
                #:timestamp-month
                #:timestamp-day
                #:*default-timezone*
                #:+utc-zone+
                #:day-of
                #:sec-of
                #:nsec-of)
  (:export #:toki
           #:today
           #:tomorrow
           #:yesterday
           #:from-timestamp
           #:ensure-toki))

(in-package #:almighty-toki/creation)

(setf *default-timezone* +utc-zone+)

(defclass toki (timestamp) ()
  (:documentation "A timestamp subclass for almighty-toki. Inherits all local-time
functionality with no extra slots."))

(defun ensure-toki (timestamp)
  "Convert a local-time:timestamp to a toki instance. If already a toki, return as-is."
  (if (typep timestamp 'toki)
      timestamp
      (change-class (local-time:clone-timestamp timestamp) 'toki)))

(defvar *timezone-repository-loaded-p* nil)

(defun ensure-timezone-repository ()
  (unless *timezone-repository-loaded-p*
    (local-time:reread-timezone-repository)
    (setf *timezone-repository-loaded-p* t)))

(defun resolve-tz (tz)
  "Resolve TZ to a timezone object. Accepts string names or timezone objects."
  (etypecase tz
    (local-time::timezone tz)
    (string
     (cond
       ((string-equal tz "UTC") local-time:+utc-zone+)
       ((string-equal tz "GMT") local-time:+gmt-zone+)
       (t
        (ensure-timezone-repository)
        (or (local-time:find-timezone-by-location-name tz)
            (error "Unknown timezone: ~A" tz)))))))

(defun toki (year &optional (month 1) (day 1) &rest keys)
  "Create a toki timestamp. Year is required; month and day default to 1,
time components default to 0.
Keyword arguments: :hour :minute :second :nsec :timezone."
  ;; &rest + destructuring-bind avoids mixing &optional and &key (SBCL style-warning).
  (destructuring-bind (&key (hour 0) (minute 0) (second 0) (nsec 0) timezone)
      keys
    (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
      (ensure-toki
       (encode-timestamp nsec second minute hour day month year :timezone tz)))))

(defun today (&key timezone)
  "Return midnight of today as a toki, timezone-aware."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (current (now))
         (year (local-time:timestamp-year current :timezone tz))
         (month (local-time:timestamp-month current :timezone tz))
         (day (local-time:timestamp-day current :timezone tz)))
    (ensure-toki (encode-timestamp 0 0 0 0 day month year :timezone tz))))

(defun tomorrow (&key timezone)
  "Return midnight of tomorrow as a toki."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (current (now))
         ;; Get today's date components in the target timezone
         (year (local-time:timestamp-year current :timezone tz))
         (month (local-time:timestamp-month current :timezone tz))
         (day (local-time:timestamp-day current :timezone tz))
         ;; Create midnight today in target timezone, then add 1 day
         (midnight-today (encode-timestamp 0 0 0 0 day month year :timezone tz))
         (ts (timestamp+ midnight-today 1 :day)))
    (ensure-toki ts)))

(defun yesterday (&key timezone)
  "Return midnight of yesterday as a toki."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (current (now))
         (year (local-time:timestamp-year current :timezone tz))
         (month (local-time:timestamp-month current :timezone tz))
         (day (local-time:timestamp-day current :timezone tz))
         (midnight-today (encode-timestamp 0 0 0 0 day month year :timezone tz))
         (ts (timestamp- midnight-today 1 :day)))
    (ensure-toki ts)))

(defun from-timestamp (unix-seconds &key timezone)
  "Convert a Unix timestamp (seconds since epoch) to a toki."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (ts (local-time:unix-to-timestamp unix-seconds)))
    (declare (ignore tz))
    (ensure-toki ts)))
