(defpackage #:almighty-toki/arithmetic
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:ensure-toki
                #:resolve-tz)
  (:import-from #:local-time
                #:timestamp+
                #:timestamp-
                #:clone-timestamp
                #:*default-timezone*)
  (:export #:add
           #:subtract
           #:set-timestamp))

(in-package #:almighty-toki/arithmetic)

(defun apply-offsets (ts unit-pairs)
  "Apply a list of (amount . unit) offsets to timestamp TS sequentially."
  (let ((result (clone-timestamp ts)))
    (loop for (amount . unit) in unit-pairs
          when (not (zerop amount))
            do (setf result (timestamp+ result amount unit)))
    (ensure-toki result)))

(defun add (ts &key (years 0) (months 0) (weeks 0) (days 0)
                    (hours 0) (minutes 0) (seconds 0))
  "Add calendar-aware offsets to a timestamp. Returns a new toki."
  (apply-offsets ts
                 (list (cons years :year)
                       (cons months :month)
                       (cons (* weeks 7) :day)
                       (cons days :day)
                       (cons hours :hour)
                       (cons minutes :minute)
                       (cons seconds :sec))))

(defun subtract (ts &key (years 0) (months 0) (weeks 0) (days 0)
                         (hours 0) (minutes 0) (seconds 0))
  "Subtract calendar-aware offsets from a timestamp. Returns a new toki."
  (add ts :years (- years) :months (- months) :weeks (- weeks)
       :days (- days) :hours (- hours) :minutes (- minutes) :seconds (- seconds)))

(defmacro set-timestamp (ts &key year month day hour minute second timezone)
  "Return a new toki with specified fields replaced. Unspecified fields are preserved."
  (let ((clauses nil))
    (when timezone
      (push `(:timezone (resolve-tz ,timezone)) clauses))
    (when year   (push `(:set :year ,year) clauses))
    (when month  (push `(:set :month ,month) clauses))
    (when day    (push `(:set :day-of-month ,day) clauses))
    (when hour   (push `(:set :hour ,hour) clauses))
    (when minute (push `(:set :minute ,minute) clauses))
    (when second (push `(:set :sec ,second) clauses))
    `(ensure-toki (local-time:adjust-timestamp ,ts ,@(nreverse clauses)))))
