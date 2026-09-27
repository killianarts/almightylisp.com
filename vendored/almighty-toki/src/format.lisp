(defpackage #:almighty-toki/format
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:resolve-tz)
  (:import-from #:local-time
                #:format-timestring
                #:with-decoded-timestamp
                #:+iso-8601-format+
                #:+iso-8601-date-format+
                #:+rfc3339-format+
                #:+rfc-1123-format+
                #:+short-day-names+
                #:+short-month-names+
                #:*default-timezone*)
  (:export #:to-date
           #:to-time
           #:to-datetime
           #:to-human
           #:to-iso
           #:to-rfc3339
           #:to-rfc1123))

(in-package #:almighty-toki/format)

(defvar +time-format+ '((:hour 2) #\: (:min 2) #\: (:sec 2)))

(defvar +date-format+ '((:year 4) #\- (:month 2) #\- (:day 2)))

(defvar +datetime-format+
  (append +date-format+ (list #\Space) +time-format+))

(defvar +iso-local-format+
  (append +date-format+ (list #\T) +time-format+))

(defun to-date (ts &key timezone)
  "Return TS as an ISO date string: \"YYYY-MM-DD\"."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (format-timestring nil ts :format +date-format+ :timezone tz)))

(defun to-time (ts &key timezone)
  "Return TS as a 24-hour time string: \"HH:MM:SS\"."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (format-timestring nil ts :format +time-format+ :timezone tz)))

(defun to-datetime (ts &key timezone)
  "Return TS as \"YYYY-MM-DD HH:MM:SS\"."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (format-timestring nil ts :format +datetime-format+ :timezone tz)))

(defun to-human (ts &key timezone)
  "Return TS as human-readable string: \"Thu, Mar 13, 2025 2:30 PM\"."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (with-decoded-timestamp (:hour h :minute m :day d :month mo :year y
                             :day-of-week dow :timezone tz)
        ts
      (let* ((h12 (cond ((= h 0) 12)
                        ((> h 12) (- h 12))
                        (t h)))
             (ampm (if (>= h 12) "PM" "AM"))
             (day-name (aref +short-day-names+ dow))
             (month-name (aref +short-month-names+ mo)))
        (format nil "~A, ~A ~D, ~D ~D:~2,'0D ~A"
                day-name month-name d y h12 m ampm)))))

(defun to-iso (ts &key timezone)
  "Return TS as ISO 8601 local datetime: \"YYYY-MM-DDTHH:MM:SS\".
Suitable for HTML datetime-local inputs."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (format-timestring nil ts :format +iso-local-format+ :timezone tz)))

(defun to-rfc3339 (ts &key timezone)
  "Return TS in RFC 3339 format with fractional seconds and timezone offset."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (format-timestring nil ts :format +rfc3339-format+ :timezone tz)))

(defun to-rfc1123 (ts &key timezone)
  "Return TS in RFC 1123 format (HTTP date)."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (format-timestring nil ts :format +rfc-1123-format+ :timezone tz)))
