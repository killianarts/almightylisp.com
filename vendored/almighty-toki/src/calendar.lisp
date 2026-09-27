(defpackage #:almighty-toki/calendar
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:ensure-toki
                #:toki)
  (:import-from #:local-time
                #:timestamp
                #:timestamp-day-of-week
                #:timestamp-year
                #:timestamp-month
                #:timestamp-day
                #:days-in-month
                #:encode-timestamp
                #:+utc-zone+)
  (:export #:*first-weekday*
           #:monthrange
           #:month-calendar
           #:year-calendar
           #:weekday-names
           #:format-month
           #:format-year
           #:print-month
           #:print-year
           #:format-week-header
           #:format-month-html
           #:format-year-html
           #:iter-month-days
           #:iter-month-dates))

(in-package #:almighty-toki/calendar)

(defvar *first-weekday* :monday
  "First day of the week. Default is :monday (ISO convention).")

(defvar +weekday-keywords+
  '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

(defvar +short-day-names-2+
  '("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"))

(defvar +short-day-names-3+
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar +long-day-names+
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defvar +long-month-names+
  '("" "January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defvar +css-day-classes+
  '("mon" "tue" "wed" "thu" "fri" "sat" "sun"))

(defun weekday-offset ()
  "Return the ISO index of *first-weekday* (0 for Monday, 6 for Sunday)."
  (or (position *first-weekday* +weekday-keywords+)
      0))

(defun lt-dow-to-iso (lt-dow)
  "Convert local-time DOW (0=Sunday) to ISO (0=Monday)."
  (mod (1- lt-dow) 7))

(defun iso-to-relative (iso-dow)
  "Convert ISO DOW to position relative to *first-weekday*."
  (mod (- iso-dow (weekday-offset)) 7))

(defun weekday (year-or-ts &optional month day)
  "Return day of week for the given date. 0=*first-weekday*, 6=last.
Accepts either (weekday TIMESTAMP) or (weekday YEAR MONTH DAY)."
  (let* ((ts (if (typep year-or-ts 'timestamp)
                 year-or-ts
                 (encode-timestamp 0 0 0 0 day month year-or-ts
                                   :timezone +utc-zone+)))
         (lt-dow (timestamp-day-of-week ts))
         (iso-dow (lt-dow-to-iso lt-dow)))
    (iso-to-relative iso-dow)))

(defun monthrange (year month &key (firstweekday *first-weekday*))
  "Return (values first-weekday-number days-in-month) for the given month.
first-weekday-number is relative to the configured first weekday."
  (let* ((*first-weekday* firstweekday)
         (first-dow (weekday year month 1))
         (ndays (days-in-month month year)))
    (values first-dow ndays)))

(defun month-calendar (year month &key (firstweekday *first-weekday*))
  "Return a list of week-lists for the given month.
Each week is a list of 7 day numbers (0 for padding cells)."
  (multiple-value-bind (first-dow ndays)
      (monthrange year month :firstweekday firstweekday)
    (let* ((grid (append (make-list first-dow :initial-element 0)
                         (loop for d from 1 to ndays collect d)))
           ;; Pad to multiple of 7
           (pad-needed (mod (- 7 (mod (length grid) 7)) 7))
           (grid (append grid (make-list pad-needed :initial-element 0))))
      ;; Partition into weeks of 7
      (loop for i from 0 below (length grid) by 7
            collect (subseq grid i (+ i 7))))))

(defun year-calendar (year &key (width 3) (firstweekday *first-weekday*))
  "Return a list of rows, each row a list of month-calendars.
WIDTH controls months per row (default 3)."
  (let ((months (loop for m from 1 to 12
                      collect (month-calendar year m :firstweekday firstweekday))))
    (loop for i from 0 below 12 by width
          collect (subseq months i (min (+ i width) 12)))))

;;; --- Text formatting ---

(defun weekday-names (&key (firstweekday *first-weekday*) (style :short))
  "Return a list of 7 weekday name strings starting at FIRSTWEEKDAY.

STYLE is one of:
  :short  — \"Mo\" \"Tu\" … \"Su\"
  :medium — \"Mon\" \"Tue\" … \"Sun\"
  :long   — \"Monday\" \"Tuesday\" … \"Sunday\"

FIRSTWEEKDAY defaults to *first-weekday* (:monday by default)."
  (let* ((names (ecase style
                  (:short +short-day-names-2+)
                  (:medium +short-day-names-3+)
                  (:long +long-day-names+)))
         (offset (or (position firstweekday +weekday-keywords+)
                     (error "Unknown weekday keyword: ~A" firstweekday))))
    (append (nthcdr offset names) (subseq names 0 offset))))

(defun rotated-day-names (column-width &optional (firstweekday *first-weekday*))
  "Return day name abbreviations rotated to start at firstweekday."
  (weekday-names :firstweekday firstweekday
                 :style (if (>= column-width 3) :medium :short)))

(defun format-week-header (&key (firstweekday *first-weekday*) (column-width 2))
  "Return the weekday header row string."
  (format nil "~{~A~^ ~}" (rotated-day-names column-width firstweekday)))

(defun format-month (year month &key (firstweekday *first-weekday*)
                                     (column-width 2) stream)
  "Format a text calendar for one month. Returns string or writes to STREAM."
  (let* ((*first-weekday* firstweekday)
         (cal (month-calendar year month :firstweekday firstweekday))
         (header (format-week-header :firstweekday firstweekday
                                     :column-width column-width))
         (total-width (length header))
         (month-name (nth month +long-month-names+))
         (title (format nil "~A ~D" month-name year))
         (lines nil))
    ;; Center title
    (let ((pad (max 0 (floor (- total-width (length title)) 2))))
      (push (format nil "~A~A" (make-string pad :initial-element #\Space) title) lines))
    ;; Day header
    (push header lines)
    ;; Day rows
    (dolist (week cal)
      (let ((row-parts nil))
        (dolist (day week)
          (if (zerop day)
              (push (make-string column-width :initial-element #\Space) row-parts)
              (push (format nil "~VD" column-width day) row-parts)))
        (push (format nil "~{~A~^ ~}" (nreverse row-parts)) lines)))
    (let ((result (format nil "~{~A~%~}" (nreverse lines))))
      (if stream
          (progn (write-string result stream) nil)
          result))))

(defun format-year (year &key (months-per-row 3) (spacing 2) stream
                              (firstweekday *first-weekday*))
  "Format a full year text calendar."
  (declare (ignore spacing))
  (let ((lines nil))
    ;; Year title
    (push (format nil "~40<~A~>" year) lines)
    (push "" lines)
    ;; Process rows of months
    (loop for start from 1 to 12 by months-per-row
          do (let ((month-strings
                     (loop for m from start
                           below (min (+ start months-per-row) 13)
                           collect (format-month year m
                                                 :firstweekday firstweekday))))
               ;; Side-by-side: split each month string into lines and zip
               (let* ((month-lines (mapcar (lambda (s)
                                             (split-string-by-newline s))
                                           month-strings))
                      (max-lines (reduce #'max (mapcar #'length month-lines))))
                 ;; Pad each month to max-lines
                 (loop for i from 0 below max-lines
                       do (push (format nil "~{~A~^  ~}"
                                        (mapcar (lambda (ml)
                                                  (if (< i (length ml))
                                                      (nth i ml)
                                                      ""))
                                                month-lines))
                                lines))
                 (push "" lines))))
    (let ((result (format nil "~{~A~%~}" (nreverse lines))))
      (if stream
          (progn (write-string result stream) nil)
          result))))

(defun split-string-by-newline (s)
  "Split string S by newlines, removing trailing empty string."
  (let ((lines nil)
        (start 0))
    (loop for pos = (position #\Newline s :start start)
          while pos
          do (push (subseq s start pos) lines)
             (setf start (1+ pos)))
    (when (< start (length s))
      (push (subseq s start) lines))
    (nreverse lines)))

(defun print-month (year month &key (firstweekday *first-weekday*))
  "Print a text calendar for one month to *standard-output*."
  (format-month year month :firstweekday firstweekday
                           :stream *standard-output*))

(defun print-year (year &key (firstweekday *first-weekday*))
  "Print a full year calendar to *standard-output*."
  (format-year year :firstweekday firstweekday :stream *standard-output*))

;;; --- HTML output ---

(defun format-month-html (year month &key (firstweekday *first-weekday*)
                                          css-classes)
  "Return an HTML table string for one month's calendar."
  (let* ((*first-weekday* firstweekday)
         (cal (month-calendar year month :firstweekday firstweekday))
         (day-classes (or css-classes +css-day-classes+))
         (rotated-classes (let ((offset (position firstweekday +weekday-keywords+)))
                            (append (nthcdr offset day-classes)
                                    (subseq day-classes 0 offset))))
         (day-names (rotated-day-names 2 firstweekday))
         (month-name (nth month +long-month-names+))
         (out (make-string-output-stream)))
    (format out "<table class=\"month\">~%")
    (format out "<thead>~%")
    (format out "<tr class=\"month-name\"><th colspan=\"7\">~A ~D</th></tr>~%"
            month-name year)
    (format out "<tr>")
    (loop for name in day-names
          for cls in rotated-classes
          do (format out "<th class=\"~A\">~A</th>" cls name))
    (format out "</tr>~%</thead>~%<tbody>~%")
    (dolist (week cal)
      (format out "<tr>")
      (loop for day in week
            for cls in rotated-classes
            do (if (zerop day)
                   (format out "<td class=\"noday\">&nbsp;</td>")
                   (format out "<td class=\"~A\">~D</td>" cls day)))
      (format out "</tr>~%"))
    (format out "</tbody>~%</table>")
    (get-output-stream-string out)))

(defun format-year-html (year &key (width 3) (firstweekday *first-weekday*) css)
  "Return HTML for a full year calendar."
  (let ((out (make-string-output-stream)))
    (when css
      (format out "<link rel=\"stylesheet\" href=\"~A\">~%" css))
    (format out "<div class=\"year-calendar\">~%")
    (format out "<h1>~D</h1>~%" year)
    (loop for start from 1 to 12 by width
          do (format out "<div class=\"month-row\">~%")
             (loop for m from start below (min (+ start width) 13)
                   do (write-string
                       (format-month-html year m :firstweekday firstweekday)
                       out)
                      (terpri out))
             (format out "</div>~%"))
    (format out "</div>")
    (get-output-stream-string out)))

;;; --- Iteration helpers ---

(defun iter-month-days (year month &key (firstweekday *first-weekday*))
  "Return a flat list of (day . weekday) pairs for the full calendar grid.
Day = 0 for padding, 1-31 for real days. Weekday = 0-6 relative to *first-weekday*."
  (let* ((*first-weekday* firstweekday)
         (cal (month-calendar year month :firstweekday firstweekday)))
    (loop for week in cal
          append (loop for day in week
                       for col from 0
                       collect (cons day col)))))

(defun iter-month-dates (year month &key (firstweekday *first-weekday*))
  "Return a flat list of timestamps (or NIL for padding) for the full calendar grid."
  (let* ((*first-weekday* firstweekday)
         (cal (month-calendar year month :firstweekday firstweekday)))
    (loop for week in cal
          append (loop for day in week
                       collect (if (zerop day)
                                   nil
                                   (ensure-toki
                                    (encode-timestamp
                                     0 0 0 0 day month year
                                     :timezone +utc-zone+)))))))
