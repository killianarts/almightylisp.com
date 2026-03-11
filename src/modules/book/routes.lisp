(defpackage #:book/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:book/controllers)))

(in-package #:book/routes)

(s:define-module book
  (:urls (:GET "/" 'controllers:index "index")))
