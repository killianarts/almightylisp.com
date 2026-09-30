(defpackage #:almighty-press/dates
  (:use #:cl)
  (:local-nicknames (#:at #:almighty-toki))
  (:export #:parse-date #:format-date #:today #:date> #:same-month-p))

(in-package #:almighty-press/dates)

;;;; Calendar dates.
;;;
;;; A date is an almighty-toki timestamp at midnight UTC. Dates are only ever
;;; formatted in UTC, so a date written as 2026-09-24 prints as the 24th
;;; wherever the server runs.

(defparameter *formats*
  '((:iso        (:year "-" (:month 2) "-" (:day 2)))       ; 2026-09-24
    (:weekday    (:long-weekday " " :day " " :long-month " " :year)) ; Thursday 24 September 2026
    (:short-weekday (:short-weekday " " :day " " :short-month " " :year)) ; Thu 24 Sep 2026
    (:long       (:day " " :long-month " " :year))          ; 24 September 2026
    (:compact    ((:day 2) " " :short-month " " :year))     ; 24 Sep 2026
    (:day-month  ((:day 2) " " :short-month))               ; 24 Sep
    (:month      (:long-month " " :year))                   ; September 2026
    (:month-id   (:year "-" (:month 2))))                   ; 2026-09
  "FORMAT-DATE's styles, as local-time format specs.")

(defun parse-date (text)
  "The date written YYYY-MM-DD in TEXT. Signals an error for anything else."
  (let* ((text (string-trim '(#\Space #\Tab) (or text "")))
         (parts (uiop:split-string text :separator "-"))
         (numbers (mapcar (lambda (part) (parse-integer part :junk-allowed t)) parts)))
    (unless (and (= (length parts) 3)
                 (every #'integerp numbers)
                 (<= 1 (second numbers) 12)
                 (<= 1 (third numbers) 31))
      (error "Dates are written YYYY-MM-DD; got ~s." text))
    (apply #'at:toki (append numbers (list :timezone "UTC")))))

(defun today ()
  "Today's date where the server runs."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (at:toki year month day :timezone "UTC")))

(defun format-date (date style)
  "DATE as text in STYLE, one of the keys of *FORMATS*."
  (let ((spec (second (assoc style *formats*))))
    (unless spec
      (error "Unknown date style ~s; use one of ~{~s~^, ~}." style (mapcar #'first *formats*)))
    (local-time:format-timestring nil date :format spec :timezone local-time:+utc-zone+)))

(defun date> (a b)
  "True when date A is later than date B."
  (local-time:timestamp> a b))

(defun same-month-p (a b)
  (at:same-month? a b :timezone "UTC"))
