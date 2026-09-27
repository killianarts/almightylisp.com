;;; Symbols that were once :export'ed without :import-from (home package symbols).
;;; Unintern them before redefining so import-from from subpackages does not
;;; name-conflict on reload.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:almighty-toki)))
    (when pkg
      (dolist (name '("YEAR" "MONTH" "DAY" "HOUR" "MINUTE" "SEC"
                      "DAY-OF-WEEK" "DAY-NAME" "MILLISECOND" "MICROSECOND"
                      "CENTURY" "DECADE" "MILLENNIUM"
                      "TO-UNIX" "TO-UNIVERSAL" "EARLIEST" "LATEST"))
        (let ((sym (find-symbol name pkg)))
          (when (and sym (eq (symbol-package sym) pkg))
            (unintern sym pkg)))))))

(defpackage #:almighty-toki
  (:nicknames #:at)
  (:use #:cl)
  ;; Re-export all local-time symbols
  (:import-from #:local-time
                #:*clock*
                #:*default-timezone*
                #:+asctime-format+
                #:+day-names+
                #:+days-per-week+
                #:+gmt-zone+
                #:+hours-per-day+
                #:+iso-8601-date-format+
                #:+iso-8601-format+
                #:+iso-8601-time-format+
                #:+iso-week-date-format+
                #:+minutes-per-day+
                #:+minutes-per-hour+
                #:+month-names+
                #:+months-per-year+
                #:+rfc-1123-format+
                #:+rfc3339-format+
                #:+rfc3339-format/date-only+
                #:+seconds-per-day+
                #:+seconds-per-hour+
                #:+seconds-per-minute+
                #:+short-day-names+
                #:+short-month-names+
                #:+utc-zone+
                #:adjust-timestamp
                #:adjust-timestamp!
                #:all-timezones-matching-subzone
                #:astronomical-julian-date
                #:astronomical-modified-julian-date
                #:clock-now
                #:clock-today
                #:clone-timestamp
                #:date
                #:day-of
                #:days-in-month
                #:decode-timestamp
                #:decode-universal-time-with-tz
                #:define-timezone
                #:enable-read-macros
                #:encode-timestamp
                #:encode-universal-time-with-tz
                #:find-timezone-by-location-name
                #:format-rfc1123-timestring
                #:format-rfc3339-timestring
                #:format-timestring
                #:friday?
                #:invalid-timestring
                #:leap-second-adjusted
                #:make-timestamp
                #:modified-julian-date
                #:monday?
                #:now
                #:nsec-of
                #:parse-rfc3339-timestring
                #:parse-timestring
                #:reread-timezone-repository
                #:saturday?
                #:sec-of
                #:sunday?
                #:thursday?
                #:time-of-day
                #:timestamp
                #:timestamp+
                #:timestamp-
                #:timestamp-century
                #:timestamp-day
                #:timestamp-day-of-week
                #:timestamp-decade
                #:timestamp-difference
                #:timestamp-hour
                #:timestamp-maximize-part
                #:timestamp-maximum
                #:timestamp-microsecond
                #:timestamp-millennium
                #:timestamp-millisecond
                #:timestamp-minimize-part
                #:timestamp-minimum
                #:timestamp-minute
                #:timestamp-month
                #:timestamp-second
                #:timestamp-subtimezone
                #:timestamp-to-universal
                #:timestamp-to-unix
                #:timestamp-week
                #:timestamp-whole-year-difference
                #:timestamp-year
                #:timestamp/=
                #:timestamp<
                #:timestamp<=
                #:timestamp=
                #:timestamp>
                #:timestamp>=
                #:timezones-matching-subzone
                #:to-rfc1123-timestring
                #:to-rfc3339-timestring
                #:tuesday?
                #:universal-to-timestamp
                #:unix-to-timestamp
                #:wednesday?
                #:with-decoded-timestamp
                #:zone-name)
  ;; Re-export creation symbols
  (:import-from #:almighty-toki/creation
                #:toki
                #:today
                #:tomorrow
                #:yesterday
                #:from-timestamp
                #:ensure-toki)
  ;; Re-export timezone symbols
  (:import-from #:almighty-toki/timezone
                #:in-timezone
                #:with-timezone)
  ;; Re-export arithmetic symbols
  (:import-from #:almighty-toki/arithmetic
                #:add
                #:subtract
                #:set-timestamp)
  ;; Re-export duration symbols
  (:import-from #:almighty-toki/duration
                #:duration
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
                #:duration-negate)
  ;; Re-export iteration symbols
  (:import-from #:almighty-toki/iteration
                #:span
                #:year-span
                #:month-span
                #:week-span
                #:day-span
                #:time-loop)
  ;; Re-export predicate symbols
  (:import-from #:almighty-toki/predicates
                #:today?
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
                #:same?
                #:toki/=
                #:not-same?
                #:toki<
                #:toki>
                #:toki<=
                #:same-or-before?
                #:toki>=
                #:same-or-after?)
  ;; Re-export boundary symbols
  (:import-from #:almighty-toki/boundaries
                #:start-of
                #:end-of
                #:next-weekday
                #:previous-weekday
                #:first-of
                #:last-of
                #:nth-of
                #:nth-weekday)
  ;; Re-export format symbols
  (:import-from #:almighty-toki/format
                #:to-date
                #:to-time
                #:to-datetime
                #:to-human
                #:to-iso
                #:to-rfc3339
                #:to-rfc1123)
  ;; Re-export property symbols
  (:import-from #:almighty-toki/properties
                #:year
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
                #:average)
  ;; Re-export diff-human symbols
  (:import-from #:almighty-toki/diff-human
                #:diff-for-humans)
  ;; Re-export testing symbols
  (:import-from #:almighty-toki/testing
                #:with-frozen-time
                #:with-time-travel)
  ;; Re-export calendar symbols
  (:import-from #:almighty-toki/calendar
                #:*first-weekday*
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
                #:iter-month-dates)
  ;; Export everything
  (:export
   ;; local-time re-exports
   #:*clock* #:*default-timezone*
   #:+asctime-format+ #:+day-names+ #:+days-per-week+ #:+gmt-zone+
   #:+hours-per-day+ #:+iso-8601-date-format+ #:+iso-8601-format+
   #:+iso-8601-time-format+ #:+iso-week-date-format+ #:+minutes-per-day+
   #:+minutes-per-hour+ #:+month-names+ #:+months-per-year+ #:+rfc-1123-format+
   #:+rfc3339-format+ #:+rfc3339-format/date-only+ #:+seconds-per-day+
   #:+seconds-per-hour+ #:+seconds-per-minute+ #:+short-day-names+
   #:+short-month-names+ #:+utc-zone+
   #:adjust-timestamp #:adjust-timestamp!
   #:all-timezones-matching-subzone
   #:astronomical-julian-date #:astronomical-modified-julian-date
   #:clock-now #:clock-today #:clone-timestamp
   #:date #:day-of #:days-in-month #:decode-timestamp
   #:decode-universal-time-with-tz #:define-timezone
   #:enable-read-macros #:encode-timestamp #:encode-universal-time-with-tz
   #:find-timezone-by-location-name
   #:format-rfc1123-timestring #:format-rfc3339-timestring #:format-timestring
   #:friday? #:invalid-timestring #:leap-second-adjusted
   #:make-timestamp #:modified-julian-date #:monday?
   #:now #:nsec-of
   #:parse-rfc3339-timestring #:parse-timestring
   #:reread-timezone-repository #:saturday? #:sec-of #:sunday?
   #:thursday? #:time-of-day #:timestamp #:timestamp+ #:timestamp-
   #:timestamp-century #:timestamp-day #:timestamp-day-of-week
   #:timestamp-decade #:timestamp-difference #:timestamp-hour
   #:timestamp-maximize-part #:timestamp-maximum #:timestamp-microsecond
   #:timestamp-millennium #:timestamp-millisecond #:timestamp-minimize-part
   #:timestamp-minimum #:timestamp-minute #:timestamp-month
   #:timestamp-second #:timestamp-subtimezone #:timestamp-to-universal
   #:timestamp-to-unix #:timestamp-week #:timestamp-whole-year-difference
   #:timestamp-year #:timestamp/= #:timestamp< #:timestamp<=
   #:timestamp= #:timestamp> #:timestamp>=
   #:timezones-matching-subzone
   #:to-rfc1123-timestring #:to-rfc3339-timestring
   #:tuesday? #:universal-to-timestamp #:unix-to-timestamp
   #:wednesday? #:with-decoded-timestamp #:zone-name
   ;; almighty-toki exports
   #:toki #:today #:tomorrow #:yesterday #:from-timestamp #:ensure-toki
   #:in-timezone #:with-timezone
   #:add #:subtract #:set-timestamp
   #:duration #:toki- #:toki+
   #:duration-years #:duration-months #:duration-weeks #:duration-days
   #:duration-hours #:duration-minutes #:duration-seconds #:duration-nanoseconds
   #:in-years #:in-months #:in-weeks #:in-days
   #:in-hours #:in-minutes #:in-seconds
   #:total-seconds #:in-words
   #:duration+ #:duration- #:duration-zerop #:duration-negate
   #:span #:year-span #:month-span #:week-span #:day-span #:time-loop
   #:today? #:yesterday? #:tomorrow? #:past? #:future?
   #:leap-year? #:weekend? #:same-day? #:same-month? #:same-year?
   #:before? #:after? #:between?
   #:toki= #:same? #:toki/= #:not-same?
   #:toki< #:toki> #:toki<= #:same-or-before? #:toki>= #:same-or-after?
   #:start-of #:end-of #:next-weekday #:previous-weekday
   #:first-of #:last-of #:nth-of #:nth-weekday
   #:to-date #:to-time #:to-datetime #:to-human
   #:to-iso #:to-rfc3339 #:to-rfc1123
   #:year #:month #:day #:hour
   #:minute #:sec #:day-of-week #:day-name
   #:millisecond #:microsecond
   #:century #:decade #:millennium
   #:to-unix #:to-universal
   #:earliest #:latest
   #:quarter #:day-of-year #:week-of-month #:week-of-year
   #:days-in-month-of #:age #:offset-seconds #:dst? #:average
   #:diff-for-humans
   #:with-frozen-time #:with-time-travel
   #:*first-weekday* #:monthrange #:month-calendar #:year-calendar
   #:weekday-names
   #:format-month #:format-year #:print-month #:print-year
   #:format-week-header #:format-month-html #:format-year-html
   #:iter-month-days #:iter-month-dates))

(in-package #:almighty-toki)
