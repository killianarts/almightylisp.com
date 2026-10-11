(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "OBJC-SUPPORT"))

(defpackage #:opening-a-window
  (:use #:cl))

(in-package #:opening-a-window)

(defun show-window ()
  (let* ((window (make-instance 'ns:ns-window)))
    (#/setTitle: window #@"Baby's first window")
    (#/center window)
    window))

