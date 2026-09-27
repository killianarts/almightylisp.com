(defpackage #:almighty-toki/predicates
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:resolve-tz)
  (:import-from #:local-time
                #:now
                #:today
                #:timestamp<
                #:timestamp>
                #:timestamp<=
                #:timestamp>=
                #:timestamp=
                #:timestamp/=
                #:timestamp-year
                #:timestamp-month
                #:timestamp-day
                #:timestamp-day-of-week
                #:*default-timezone*)
  (:export #:today?
           #:yesterday?
           #:tomorrow?
           #:past?
           #:future?
           #:leap-year?
           #:weekend?
           #:same-day?
           #:same-month?
           #:same-year?
           #:before?
           #:after?
           #:between?
           #:toki=
           #:not-same?
           #:toki/=
           #:same?
           #:toki<
           #:toki>
           #:toki<=
           #:same-or-before?
           #:toki>=
           #:same-or-after?))

(in-package #:almighty-toki/predicates)

(defun same-date-p (ts1 ts2 &optional (tz *default-timezone*))
  "Return T if TS1 and TS2 fall on the same calendar date in timezone TZ."
  (and (= (timestamp-year ts1 :timezone tz) (timestamp-year ts2 :timezone tz))
       (= (timestamp-month ts1 :timezone tz) (timestamp-month ts2 :timezone tz))
       (= (timestamp-day ts1 :timezone tz) (timestamp-day ts2 :timezone tz))))

(defun today? (ts)
  "Return T if TS falls on today's date."
  (same-date-p ts (now)))

(defun yesterday? (ts)
  "Return T if TS falls on yesterday's date."
  (same-date-p ts (local-time:timestamp- (today) 1 :day)))

(defun tomorrow? (ts)
  "Return T if TS falls on tomorrow's date."
  (same-date-p ts (local-time:timestamp+ (today) 1 :day)))

(defun past? (ts)
  "Return T if TS is before now."
  (timestamp< ts (now)))

(defun future? (ts)
  "Return T if TS is after now."
  (timestamp> ts (now)))

(defun leap-year-integer-p (year)
  "Return T if integer YEAR is a leap year."
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun leap-year? (thing &key timezone)
  "Return T if THING (integer year or timestamp) represents a leap year."
  (etypecase thing
    (integer (leap-year-integer-p thing))
    (local-time:timestamp
     (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
       (leap-year-integer-p (timestamp-year thing :timezone tz))))))

(defun weekend? (ts &key timezone)
  "Return T if TS falls on a Saturday or Sunday."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (dow (timestamp-day-of-week ts :timezone tz)))
    (or (= dow 0) (= dow 6))))  ; local-time: 0=Sunday, 6=Saturday

(defun same-day? (ts1 ts2)
  "Return T if TS1 and TS2 fall on the same calendar date."
  (same-date-p ts1 ts2))

(defun same-month? (ts1 ts2 &key timezone)
  "Return T if TS1 and TS2 are in the same month and year."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (and (= (timestamp-year ts1 :timezone tz) (timestamp-year ts2 :timezone tz))
         (= (timestamp-month ts1 :timezone tz) (timestamp-month ts2 :timezone tz)))))

(defun same-year? (ts1 ts2 &key timezone)
  "Return T if TS1 and TS2 are in the same year."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (= (timestamp-year ts1 :timezone tz) (timestamp-year ts2 :timezone tz))))

(defun before? (ts1 ts2)
  "Return T if TS1 is strictly before TS2."
  (timestamp< ts1 ts2))

(defun after? (ts1 ts2)
  "Return T if TS1 is strictly after TS2."
  (timestamp> ts1 ts2))

(defun between? (ts start end)
  "Return T if TS is between START and END, inclusive on both ends."
  (and (timestamp<= start ts)
       (timestamp<= ts end)))

;;; --- toki comparison operators ---

(defun same? (ts1 ts2)
  "Return T if TS1 and TS2 represent the same instant. Equivalent to toki=."
  (timestamp= ts1 ts2))

(setf (fdefinition 'toki=) #'same?)

(defun not-same? (ts1 ts2)
  "Return T if TS1 and TS2 do not represent the same instant. Equivalent to toki/=."
  (timestamp/= ts1 ts2))

(setf (fdefinition 'toki/=) #'not-same?)

(setf (fdefinition 'toki<) #'before?)

(setf (fdefinition 'toki>) #'after?)

(defun same-or-before? (ts1 ts2)
  "Return T if TS1 is before or at the same instant as TS2. Equivalent to toki<=."
  (timestamp<= ts1 ts2))

(setf (fdefinition 'toki<=) #'same-or-before?)

(defun same-or-after? (ts1 ts2)
  "Return T if TS1 is after or at the same instant as TS2. Equivalent to toki>=."
  (timestamp>= ts1 ts2))

(setf (fdefinition 'toki>=) #'same-or-after?)
