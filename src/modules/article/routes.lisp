(defpackage #:article/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:article/controllers)))

(in-package #:article/routes)

(s:define-module article
  (:urls (:GET "/" #'controllers:index "index")
         ((:GET :POST) "/create" 'controllers:article-create "create")))
