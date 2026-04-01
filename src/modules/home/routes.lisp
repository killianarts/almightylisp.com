(defpackage #:home/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:home/controllers)))

(in-package #:home/routes)

(s:define-module home
  (:urls (:GET "/" #'controllers:index "index")))
