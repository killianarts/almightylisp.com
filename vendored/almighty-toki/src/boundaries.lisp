(defpackage #:almighty-toki/boundaries
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:ensure-toki
                #:resolve-tz)
  (:import-from #:local-time
                #:timestamp+
                #:timestamp-
                #:timestamp-day-of-week
                #:timestamp-day
                #:timestamp-month
                #:timestamp-year
                #:days-in-month
                #:*default-timezone*
                #:adjust-timestamp
                #:clone-timestamp
                #:encode-timestamp)
  (:export #:start-of
           #:end-of
           #:next-weekday
           #:previous-weekday
           #:first-of
           #:last-of
           #:nth-of
           #:nth-weekday))

(in-package #:almighty-toki/boundaries)

;;; Day-of-week mapping
;;; local-time: 0=Sunday, 1=Monday, ..., 6=Saturday
;;; ISO (our convention): 0=Monday, 1=Tuesday, ..., 6=Sunday

(defun lt-dow-to-iso (lt-dow)
  "Convert local-time day-of-week (0=Sun) to ISO (0=Mon)."
  (mod (1- lt-dow) 7))

(defvar +weekday-keywords+
  '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

(defun weekday-keyword-to-iso (keyword)
  "Convert a weekday keyword to ISO index (0=Monday)."
  (or (position keyword +weekday-keywords+)
      (error "Unknown weekday keyword: ~A" keyword)))

(defun weekday-keyword-to-lt (keyword)
  "Convert a weekday keyword to local-time index (0=Sunday)."
  (let ((iso (weekday-keyword-to-iso keyword)))
    (mod (1+ iso) 7)))

;;; --- start-of ---

(defun start-of (ts unit &key timezone)
  "Return the start of the time period UNIT containing TS."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (result
           (ecase unit
             (:minute
              (adjust-timestamp ts
                (:timezone tz)
                (:set :sec 0)
                (:set :nsec 0)))
             (:hour
              (adjust-timestamp ts
                (:timezone tz)
                (:set :minute 0)
                (:set :sec 0)
                (:set :nsec 0)))
             (:day
              (adjust-timestamp ts
                (:timezone tz)
                (:set :hour 0)
                (:set :minute 0)
                (:set :sec 0)
                (:set :nsec 0)))
             (:week
              (let* ((lt-dow (timestamp-day-of-week ts :timezone tz))
                     (iso-dow (lt-dow-to-iso lt-dow))
                     (days-back iso-dow))
                (adjust-timestamp (timestamp- ts days-back :day)
                  (:timezone tz)
                  (:set :hour 0)
                  (:set :minute 0)
                  (:set :sec 0)
                  (:set :nsec 0))))
             (:month
              (adjust-timestamp ts
                (:timezone tz)
                (:set :day-of-month 1)
                (:set :hour 0)
                (:set :minute 0)
                (:set :sec 0)
                (:set :nsec 0)))
             (:year
              (adjust-timestamp ts
                (:timezone tz)
                (:set :month 1)
                (:set :day-of-month 1)
                (:set :hour 0)
                (:set :minute 0)
                (:set :sec 0)
                (:set :nsec 0))))))
    (ensure-toki result)))

;;; --- end-of ---

(defun end-of (ts unit &key timezone)
  "Return the end of the time period UNIT containing TS."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (result
           (ecase unit
             (:minute
              (adjust-timestamp ts
                (:timezone tz)
                (:set :sec 59)
                (:set :nsec 999999999)))
             (:hour
              (adjust-timestamp ts
                (:timezone tz)
                (:set :minute 59)
                (:set :sec 59)
                (:set :nsec 999999999)))
             (:day
              (adjust-timestamp ts
                (:timezone tz)
                (:set :hour 23)
                (:set :minute 59)
                (:set :sec 59)
                (:set :nsec 999999999)))
             (:week
              (let* ((lt-dow (timestamp-day-of-week ts :timezone tz))
                     (iso-dow (lt-dow-to-iso lt-dow))
                     (days-forward (- 6 iso-dow)))
                (adjust-timestamp (timestamp+ ts days-forward :day)
                  (:timezone tz)
                  (:set :hour 23)
                  (:set :minute 59)
                  (:set :sec 59)
                  (:set :nsec 999999999))))
             (:month
              (let ((last-day (days-in-month (timestamp-month ts :timezone tz)
                                             (timestamp-year ts :timezone tz))))
                (adjust-timestamp ts
                  (:timezone tz)
                  (:set :day-of-month last-day)
                  (:set :hour 23)
                  (:set :minute 59)
                  (:set :sec 59)
                  (:set :nsec 999999999))))
             (:year
              (adjust-timestamp ts
                (:timezone tz)
                (:set :month 12)
                (:set :day-of-month 31)
                (:set :hour 23)
                (:set :minute 59)
                (:set :sec 59)
                (:set :nsec 999999999))))))
    (ensure-toki result)))

;;; --- next-weekday / previous-weekday ---

(defun next-weekday (ts weekday &key keep-time timezone)
  "Return the next occurrence of WEEKDAY after TS.
If WEEKDAY is the same as TS's weekday, returns next week.
WEEKDAY is a keyword like :monday, :friday, etc.
When KEEP-TIME is T, preserves the original time; otherwise resets to midnight."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (current-iso (lt-dow-to-iso (timestamp-day-of-week ts :timezone tz)))
         (target-iso (weekday-keyword-to-iso weekday))
         (diff (mod (- target-iso current-iso) 7))
         (days-forward (if (zerop diff) 7 diff))
         (result (timestamp+ ts days-forward :day)))
    (unless keep-time
      (setf result (adjust-timestamp result
                     (:timezone tz)
                     (:set :hour 0) (:set :minute 0)
                     (:set :sec 0) (:set :nsec 0))))
    (ensure-toki result)))

(defun previous-weekday (ts weekday &key keep-time timezone)
  "Return the previous occurrence of WEEKDAY before TS.
If WEEKDAY is the same as TS's weekday, returns previous week."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (current-iso (lt-dow-to-iso (timestamp-day-of-week ts :timezone tz)))
         (target-iso (weekday-keyword-to-iso weekday))
         (diff (mod (- current-iso target-iso) 7))
         (days-back (if (zerop diff) 7 diff))
         (result (timestamp- ts days-back :day)))
    (unless keep-time
      (setf result (adjust-timestamp result
                     (:timezone tz)
                     (:set :hour 0) (:set :minute 0)
                     (:set :sec 0) (:set :nsec 0))))
    (ensure-toki result)))

;;; --- first-of / last-of / nth-of ---

(defun first-of (ts unit &key timezone)
  "Return the first day of the UNIT period containing TS, at midnight."
  (ecase unit
    (:week (start-of ts :week :timezone timezone))
    (:month (start-of ts :month :timezone timezone))
    (:year (start-of ts :year :timezone timezone))))

(defun last-of (ts unit &key timezone)
  "Return the last day of the UNIT period containing TS, at midnight."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (ecase unit
      (:week
       (let* ((lt-dow (timestamp-day-of-week ts :timezone tz))
              (iso-dow (lt-dow-to-iso lt-dow))
              (days-forward (- 6 iso-dow)))
         (ensure-toki
          (adjust-timestamp (timestamp+ ts days-forward :day)
            (:timezone tz)
            (:set :hour 0) (:set :minute 0)
            (:set :sec 0) (:set :nsec 0)))))
      (:month
       (let ((last-day (days-in-month (timestamp-month ts :timezone tz)
                                      (timestamp-year ts :timezone tz))))
         (ensure-toki
          (adjust-timestamp ts
            (:timezone tz)
            (:set :day-of-month last-day)
            (:set :hour 0) (:set :minute 0)
            (:set :sec 0) (:set :nsec 0)))))
      (:year
       (ensure-toki
        (adjust-timestamp ts
          (:timezone tz)
          (:set :month 12) (:set :day-of-month 31)
          (:set :hour 0) (:set :minute 0)
          (:set :sec 0) (:set :nsec 0)))))))

(defun nth-of (ts unit n &key timezone)
  "Return the Nth day of the UNIT period containing TS."
  (let ((tz (if timezone (resolve-tz timezone) *default-timezone*)))
    (ecase unit
      (:month
       (ensure-toki
        (adjust-timestamp ts
          (:timezone tz)
          (:set :day-of-month n)
          (:set :hour 0) (:set :minute 0)
          (:set :sec 0) (:set :nsec 0))))
      (:year
       (let ((jan1 (start-of ts :year :timezone timezone)))
         (ensure-toki (timestamp+ jan1 (1- n) :day)))))))

;;; --- nth-weekday ---

(defun nth-weekday (n weekday ts &key (unit :month) timezone)
  "Find the Nth occurrence of WEEKDAY in the month (or year) of TS.
N can be negative: -1 means last, -2 means second-to-last, etc."
  (let* ((tz (if timezone (resolve-tz timezone) *default-timezone*))
         (period-start (start-of ts unit :timezone timezone))
         (period-end (end-of ts unit :timezone timezone))
         (target-iso (weekday-keyword-to-iso weekday)))
    (if (plusp n)
        ;; Count forward
        (let ((cursor period-start)
              (count 0))
          (loop while (local-time:timestamp<= cursor period-end)
                do (when (= (lt-dow-to-iso (timestamp-day-of-week cursor :timezone tz))
                            target-iso)
                     (incf count)
                     (when (= count n)
                       (return-from nth-weekday (ensure-toki cursor))))
                   (setf cursor (timestamp+ cursor 1 :day)))
          (error "Only ~D ~As found in the ~A" count weekday unit))
        ;; Count backward (negative N)
        (let ((cursor period-end)
              (count 0)
              (target-count (abs n)))
          ;; Move cursor to start of last day
          (setf cursor (adjust-timestamp cursor
                         (:timezone tz)
                         (:set :hour 0) (:set :minute 0)
                         (:set :sec 0) (:set :nsec 0)))
          (loop while (local-time:timestamp>= cursor period-start)
                do (when (= (lt-dow-to-iso (timestamp-day-of-week cursor :timezone tz))
                            target-iso)
                     (incf count)
                     (when (= count target-count)
                       (return-from nth-weekday (ensure-toki cursor))))
                   (setf cursor (timestamp- cursor 1 :day)))
          (error "Only ~D ~As found from end in the ~A" count weekday unit)))))
