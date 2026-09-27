(defpackage #:almighty-toki/iteration
  (:use #:cl)
  (:import-from #:almighty-toki/boundaries
                #:start-of
                #:end-of)
  (:import-from #:almighty-toki/creation
                #:ensure-toki)
  (:export #:span
           #:year-span
           #:month-span
           #:week-span
           #:day-span
           #:time-loop))

(in-package #:almighty-toki/iteration)

(defun span (ts unit)
  "Return (values start end) for the time period UNIT containing TS."
  (values (start-of ts unit) (end-of ts unit)))

(defun year-span (ts)
  "Return (values start end) for the year containing TS."
  (span ts :year))

(defun month-span (ts)
  "Return (values start end) for the month containing TS."
  (span ts :month))

(defun week-span (ts)
  "Return (values start end) for the week containing TS."
  (span ts :week))

(defun day-span (ts)
  "Return (values start end) for the day containing TS."
  (span ts :day))

(defun parse-by-clause (by-args)
  "Parse the by clause to extract unit keyword and optional step amount.
Returns (values unit step)."
  (let ((unit (first by-args))
        (step (if (and (rest by-args) (numberp (second by-args)))
                  (second by-args)
                  1)))
    ;; Convert plural keyword to local-time unit
    (let ((lt-unit (case unit
                     (:years :year)
                     (:months :month)
                     (:weeks :week)
                     (:days :day)
                     (:hours :hour)
                     (:minutes :minute)
                     (:seconds :sec)
                     (otherwise unit))))
      (values lt-unit step))))

(defmacro time-loop (&whole whole-form
                     for var &rest rest)
  "Datetime-aware loop macro. Supports from/to/by, from/by (unbounded), and in/by.
End is exclusive by default. Use :inclusive after the end form to include it.
Unbounded loops must terminate via loop clauses (repeat, until, while, return, etc.).
All standard LOOP clauses work after the by clause."
  (declare (ignore whole-form))
  (unless (and (symbolp for) (string-equal for "FOR"))
    (error "time-loop: expected FOR after TIME-LOOP, got ~S" for))
  (multiple-value-bind (range-type start-form end-form inclusive-p by-and-body)
      (parse-range-spec rest)
    (multiple-value-bind (lt-unit step body-clauses)
        (parse-by-and-body by-and-body)
      (let* ((cursor (gensym "CURSOR"))
             (end-var (gensym "END"))
             (step-form (if (eq lt-unit :week)
                           `(local-time:timestamp+ ,cursor ,(* step 7) :day)
                           `(local-time:timestamp+ ,cursor ,step ,lt-unit))))
        (ecase range-type
          (:from-to
           (let ((comparator (if inclusive-p
                                 'local-time:timestamp<=
                                 'local-time:timestamp<)))
             `(loop with ,cursor = ,start-form
                    with ,end-var = ,end-form
                    while (,comparator ,cursor ,end-var)
                    for ,var = (ensure-toki ,cursor)
                    ,@body-clauses
                    do (setf ,cursor ,step-form))))
          (:from-unbounded
           `(loop with ,cursor = ,start-form
                  for ,var = (ensure-toki ,cursor)
                  ,@body-clauses
                  do (setf ,cursor ,step-form)))
          (:in
           (let ((comparator (if inclusive-p
                                 'local-time:timestamp<=
                                 'local-time:timestamp<)))
             `(multiple-value-bind (,cursor ,end-var) ,start-form
                (loop while (,comparator ,cursor ,end-var)
                      for ,var = (ensure-toki ,cursor)
                      ,@body-clauses
                      do (setf ,cursor ,step-form))))))))))

(defun sym= (sym name)
  "Return T if SYM is a symbol whose name matches NAME (case-insensitive)."
  (and (symbolp sym) (string-equal (symbol-name sym) name)))

(defun sym-position (name list)
  "Find position of first symbol in LIST whose name matches NAME."
  (position-if (lambda (s) (sym= s name)) list))

(defun parse-range-spec (rest)
  "Parse from/to, from (unbounded), or in range specification.
Returns (values type start end inclusive-p remaining)."
  (cond
    ((sym= (first rest) "FROM")
     (let* ((start (second rest))
            (to-pos (sym-position "TO" rest))
            (by-pos (sym-position "BY" rest)))
       (if to-pos
           ;; from START to END [:inclusive] by ...
           (let* ((end (nth (1+ to-pos) rest))
                  (after-end (nthcdr (+ to-pos 2) rest))
                  (inclusive-p (and after-end (sym= (first after-end) "INCLUSIVE")))
                  (remaining (nthcdr by-pos rest)))
             (values :from-to start end inclusive-p remaining))
           ;; from START by ... (unbounded)
           (let ((remaining (nthcdr by-pos rest)))
             (values :from-unbounded start nil nil remaining)))))
    ((sym= (first rest) "IN")
     ;; in SPAN-FORM [:inclusive] by ...
     (let* ((span-form (second rest))
            (after-span (nthcdr 2 rest))
            (inclusive-p (and after-span (sym= (first after-span) "INCLUSIVE")))
            (by-pos (sym-position "BY" rest))
            (remaining (nthcdr by-pos rest)))
       (values :in span-form nil inclusive-p remaining)))
    (t (error "time-loop: expected FROM or IN after variable"))))

(defun parse-by-and-body (by-args)
  "Parse BY unit [step] body-clauses. Returns (values lt-unit step body-clauses)."
  ;; by-args starts with BY keyword
  (assert (sym= (first by-args) "BY"))
  (let* ((unit-keyword (second by-args))
         (after-unit (cddr by-args)))
    (multiple-value-bind (lt-unit step)
        (if (and after-unit (numberp (first after-unit)))
            (values (normalize-unit unit-keyword) (first after-unit))
            (values (normalize-unit unit-keyword) 1))
      (let ((body (if (and after-unit (numberp (first after-unit)))
                      (rest after-unit)
                      after-unit)))
        (values lt-unit step body)))))

(defun normalize-unit (unit)
  "Convert plural keyword to local-time unit keyword."
  (case unit
    (:years :year)
    (:months :month)
    (:weeks :week)
    (:days :day)
    (:hours :hour)
    (:minutes :minute)
    (:seconds :sec)
    (otherwise unit)))
