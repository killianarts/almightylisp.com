(defpackage #:articles/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:articles/controllers)))

(in-package #:articles/routes)

(s:define-module articles
  (:urls (:GET "/" #'controllers:index "index")
         ((:GET :POST) "/create" 'controllers:articles-create "create")))
