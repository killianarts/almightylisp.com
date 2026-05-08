(defpackage #:home/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:home/controllers)))

(in-package #:home/routes)

(s:define-module home
  (:urls (:GET "/" #'controllers:index "index")
         (:GET "/css-variable-test" #'controllers:test-css-variable-changing "test-css-variable-changing")
         (:POST "/change-color" #'controllers:change-color "change-color")))
