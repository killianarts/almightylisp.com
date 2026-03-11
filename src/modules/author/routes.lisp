(defpackage #:author/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:author/controllers)))

(in-package #:author/routes)

(s:define-module author
  (:urls (:GET "/" #'controllers:index "index")
         ((:GET :POST) "/create" 'controllers:author-create "create")))
