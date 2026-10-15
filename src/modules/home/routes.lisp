(defpackage #:home/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:home/controllers)))

(in-package #:home/routes)

;; "/" is the magazine's (see src/main.lisp).
(s:define-module home
  (:urls (:GET "/css-variable-test" #'controllers:test-css-variable-changing "test-css-variable-changing")
         (:POST "/change-color" #'controllers:change-color "change-color")))
