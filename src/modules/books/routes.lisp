(defpackage #:books/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:books/controllers)))

(in-package #:books/routes)

(s:define-module books
  (:urls (:GET "/" 'controllers:index "index")))
