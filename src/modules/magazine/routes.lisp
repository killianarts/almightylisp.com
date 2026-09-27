(defpackage #:magazine/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:magazine/controllers)))

(in-package #:magazine/routes)

(s:define-module magazine
  (:urls (:GET "/" #'controllers:index "index")
         (:GET "/archive" #'controllers:archive "archive")
         (:GET "/:slug" #'controllers:article "article")))
