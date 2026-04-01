(defpackage #:almightylisp/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    ;; (#:teaser #:almightylisp/controllers/teaser)
                    ;; (#:essentials #:almightylisp/controllers/essentials)
                    ))

(in-package #:almightylisp/routes)

;; (s:define-routes teaser :root ""
;;   (:GET "/" (lambda (params) (shiso:http-response "hello")) "index"))
;; (s:define-routes essentials :root "/book"
;;   (:GET "/essentials" 'essentials:index "index"))
