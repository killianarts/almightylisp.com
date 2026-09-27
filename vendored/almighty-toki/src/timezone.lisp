(defpackage #:almighty-toki/timezone
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:resolve-tz
                #:ensure-toki)
  (:import-from #:local-time
                #:*default-timezone*)
  (:export #:in-timezone
           #:with-timezone))

(in-package #:almighty-toki/timezone)

(defun in-timezone (timestamp tz)
  "Return TIMESTAMP converted to display in timezone TZ.
Returns (values toki timezone-object). The instant is the same;
the second value is the resolved timezone for decoding/formatting."
  (let ((timezone (resolve-tz tz)))
    (values (ensure-toki (local-time:clone-timestamp timestamp))
            timezone)))

(defmacro with-timezone ((tz) &body body)
  "Bind local-time:*default-timezone* to TZ for the duration of BODY.
TZ can be a string name or a timezone object."
  `(let ((local-time:*default-timezone* (resolve-tz ,tz)))
     ,@body))
