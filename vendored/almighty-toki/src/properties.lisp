(defpackage #:almighty-toki/properties
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:ensure-toki
                #:resolve-tz)
  (:import-from #:local-time
                #:now
                #:timestamp-year
                #:timestamp-month
                #:timestamp-day
                #:timestamp-hour
                #:timestamp-minute
                #:timestamp-second
                #:timestamp-millisecond
                #:timestamp-microsecond
                #:timestamp-century
                #:timestamp-decade
                #:timestamp-millennium
                #:timestamp-day-of-week
                #:timestamp-subtimezone
                #:timestamp-to-unix
                #:timestamp-to-universal
                #:timestamp-minimum
                #:timestamp-maximum
                #:unix-to-timestamp
                #:timestamp-whole-year-difference
                #:days-in-month
                #:*default-timezone*
                #:+day-names+)
  (:export #:year
           #:month
           #:day
           #:hour
           #:minute
           #:sec
           #:day-of-week
           #:day-name
           #:millisecond
           #:microsecond
           #:century
           #:decade
           #:millennium
           #:to-unix
           #:to-universal
           #:earliest
           #:latest
           #:quarter
           #:day-of-year
           #:week-of-month
           #:week-of-year
           #:days-in-month-of
           #:age
           #:offset-seconds
           #:dst?
           #:average))

(in-package #:almighty-toki/properties)

;;; --- Short field accessors ---

(defun year (ts &key timezone)
  "Return the calendar year of TS."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-year ts :timezone tz)))

(defun month (ts &key timezone)
  "Return the calendar month of TS (1-12)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-month ts :timezone tz)))

(defun day (ts &key timezone)
  "Return the day of the month of TS (1-31)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-day ts :timezone tz)))

(defun hour (ts &key timezone)
  "Return the hour of TS (0-23)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-hour ts :timezone tz)))

(defun minute (ts &key timezone)
  "Return the minute of TS (0-59)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-minute ts :timezone tz)))

(defun sec (ts &key timezone)
  "Return the second of TS (0-59)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-second ts :timezone tz)))

(defun day-of-week (ts &key timezone)
  "Return the ISO day of week for TS (0=Monday, 6=Sunday).
Unlike local-time:timestamp-day-of-week, which uses 0=Sunday."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (lt-dow (timestamp-day-of-week ts :timezone tz)))
    (mod (1- lt-dow) 7)))

(defun day-name (ts &key timezone)
  "Return the English weekday name for TS (e.g. \"Monday\")."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (lt-dow (timestamp-day-of-week ts :timezone tz)))
    (aref +day-names+ lt-dow)))

(defun millisecond (ts)
  "Return the millisecond component of TS (0-999)."
  (timestamp-millisecond ts))

(defun microsecond (ts)
  "Return the microsecond component of TS (0-999999)."
  (timestamp-microsecond ts))

(defun century (ts &key timezone)
  "Return the ordinal century of TS (e.g. 21 for 2026)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-century ts :timezone tz)))

(defun decade (ts &key timezone)
  "Return the cardinal decade of TS (e.g. 202 for 2026)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-decade ts :timezone tz)))

(defun millennium (ts &key timezone)
  "Return the ordinal millennium of TS (e.g. 3 for 2026)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (timestamp-millennium ts :timezone tz)))

;;; --- Conversions ---

(defun to-unix (ts)
  "Return seconds since the Unix epoch (1970-01-01 00:00:00 UTC)."
  (timestamp-to-unix ts))

(defun to-universal (ts)
  "Return seconds since the universal time epoch (1900-01-01 00:00:00 UTC)."
  (timestamp-to-universal ts))

;;; --- Min/Max ---

(defun earliest (&rest timestamps)
  "Return the earliest (minimum) of TIMESTAMPS as a toki."
  (ensure-toki (apply #'timestamp-minimum timestamps)))

(defun latest (&rest timestamps)
  "Return the latest (maximum) of TIMESTAMPS as a toki."
  (ensure-toki (apply #'timestamp-maximum timestamps)))

;;; --- Derived properties ---

(defun quarter (ts &key timezone)
  "Return which quarter of the year TS falls in (1-4)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (ceiling (timestamp-month ts :timezone tz) 3)))

(defun day-of-year (ts &key timezone)
  "Return the ordinal day of the year for TS (1-366)."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (month (timestamp-month ts :timezone tz))
         (day (timestamp-day ts :timezone tz))
         (year (timestamp-year ts :timezone tz)))
    (+ day
       (loop for m from 1 below month
             sum (days-in-month m year)))))

(defun week-of-month (ts &key timezone)
  "Return which week of the month TS falls in (1-6).
Week 1 contains day 1. A new week starts each Monday."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (day (timestamp-day ts :timezone tz))
         ;; Find what day of week the 1st is
         (first-of-month (local-time:encode-timestamp
                          0 0 0 0 1 (timestamp-month ts :timezone tz)
                          (timestamp-year ts :timezone tz)
                          :timezone tz))
         (first-dow-lt (local-time:timestamp-day-of-week first-of-month :timezone tz))
         (first-dow-iso (mod (1- first-dow-lt) 7)))
    (1+ (floor (+ day first-dow-iso -1) 7))))

(defun week-of-year (ts &key timezone)
  "Return the ISO week number for TS (1-53).
Uses ISO 8601 week numbering: week 1 contains the first Thursday of the year."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (year (timestamp-year ts :timezone tz))
         (doy (day-of-year ts :timezone tz))
         ;; Day of week: local-time 0=Sun, convert to ISO 1=Mon..7=Sun
         (dow-lt (timestamp-day-of-week ts :timezone tz))
         (dow-iso (if (zerop dow-lt) 7 dow-lt))
         ;; ISO week: the Thursday of the current week determines the week number
         ;; Thursday is day 4 in ISO numbering
         (thursday-doy (+ doy (- 4 dow-iso)))
         (week (ceiling thursday-doy 7)))
    (cond
      ((<= week 0) 52)  ; belongs to last week of previous year
      ((> week 52)
       ;; Check if it belongs to week 1 of next year
       (let* ((jan1-next (local-time:encode-timestamp 0 0 0 0 1 1 (1+ year)
                                                      :timezone tz))
              (jan1-dow-lt (timestamp-day-of-week jan1-next :timezone tz))
              (jan1-dow-iso (if (zerop jan1-dow-lt) 7 jan1-dow-lt)))
         (if (<= jan1-dow-iso 4) 1 week)))
      (t week))))

(defun days-in-month-of (ts &key timezone)
  "Return the number of days in the month of TS."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (days-in-month (timestamp-month ts :timezone tz)
                   (timestamp-year ts :timezone tz))))

(defun age (birthdate &key timezone)
  "Return whole years from BIRTHDATE to now."
  (if timezone
      (let ((*default-timezone* (resolve-tz timezone)))
        (timestamp-whole-year-difference (now) birthdate))
      (timestamp-whole-year-difference (now) birthdate)))

(defun offset-seconds (ts &key timezone)
  "Return the UTC offset in seconds for TS in TIMEZONE."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (multiple-value-bind (offset daylight-p abbrev)
        (timestamp-subtimezone ts tz)
      (declare (ignore daylight-p abbrev))
      offset)))

(defun dst? (ts &key timezone)
  "Return T if Daylight Saving Time is active for TS in TIMEZONE."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (multiple-value-bind (offset daylight-p abbrev)
        (timestamp-subtimezone ts tz)
      (declare (ignore offset abbrev))
      daylight-p)))

(defun average (ts1 ts2)
  "Return the midpoint timestamp between TS1 and TS2."
  (let* ((unix1 (timestamp-to-unix ts1))
         (unix2 (timestamp-to-unix ts2))
         (mid (/ (+ unix1 unix2) 2)))
    (ensure-toki (unix-to-timestamp (truncate mid)))))
