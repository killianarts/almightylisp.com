(defpackage #:almighty-toki/testing
  (:use #:cl)
  (:import-from #:almighty-toki/creation
                #:ensure-toki
                #:toki)
  (:import-from #:local-time
                #:*clock*
                #:clock-now
                #:clock-today
                #:now
                #:timestamp+
                #:timestamp-difference
                #:clone-timestamp
                #:sec-of
                #:nsec-of)
  (:export #:with-frozen-time
           #:with-time-travel))

(in-package #:almighty-toki/testing)

;;; --- Frozen clock ---

(defstruct frozen-clock
  (timestamp (error "timestamp required")))

(defmethod clock-now ((clock frozen-clock))
  (frozen-clock-timestamp clock))

(defmethod clock-today ((clock frozen-clock))
  (let ((ts (clone-timestamp (frozen-clock-timestamp clock))))
    (setf (sec-of ts) 0
          (nsec-of ts) 0)
    ts))

(defmacro with-frozen-time ((ts) &body body)
  "Execute BODY with the clock frozen at timestamp TS.
All calls to (now) and (today) will return the frozen time."
  `(let ((*clock* (make-frozen-clock :timestamp ,ts)))
     ,@body))

;;; --- Offset clock ---

(defstruct offset-clock
  (offset-seconds 0 :type number)
  (base-clock nil)
  (frozen-timestamp nil))

(defmethod clock-now ((clock offset-clock))
  (if (offset-clock-frozen-timestamp clock)
      (offset-clock-frozen-timestamp clock)
      (timestamp+ (clock-now (or (offset-clock-base-clock clock)
                                 (make-instance 'local-time::clock)))
                  (offset-clock-offset-seconds clock) :sec)))

(defmethod clock-today ((clock offset-clock))
  (let ((ts (clone-timestamp (clock-now clock))))
    (setf (sec-of ts) 0
          (nsec-of ts) 0)
    ts))

(defmacro with-time-travel ((&key (days 0) (hours 0) (minutes 0) (seconds 0)
                                  (years 0) (months 0) freeze)
                            &body body)
  "Execute BODY with the clock offset by the specified duration.
Without :FREEZE, the clock continues ticking from the offset point.
With :FREEZE T, the clock is frozen at the offset point."
  (let ((offset-var (gensym "OFFSET"))
        (target-var (gensym "TARGET"))
        (current-clock (gensym "CLOCK")))
    `(let* ((,current-clock *clock*)
            ;; Calculate the target time by applying offsets to current now
            (,target-var (let ((ts (now)))
                           ,@(let ((offsets nil))
                               (when (not (zerop years))
                                 (push `(setf ts (timestamp+ ts ,years :year)) offsets))
                               (when (not (zerop months))
                                 (push `(setf ts (timestamp+ ts ,months :month)) offsets))
                               (when (not (zerop days))
                                 (push `(setf ts (timestamp+ ts ,days :day)) offsets))
                               (when (not (zerop hours))
                                 (push `(setf ts (timestamp+ ts ,hours :hour)) offsets))
                               (when (not (zerop minutes))
                                 (push `(setf ts (timestamp+ ts ,minutes :minute)) offsets))
                               (when (not (zerop seconds))
                                 (push `(setf ts (timestamp+ ts ,seconds :sec)) offsets))
                               (nreverse offsets))
                           ts))
            (,offset-var (timestamp-difference ,target-var (now)))
            (*clock* (if ,freeze
                         (make-frozen-clock :timestamp ,target-var)
                         (make-offset-clock
                          :offset-seconds (truncate ,offset-var)
                          :base-clock ,current-clock))))
       ,@body)))
