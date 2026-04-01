(defpackage #:home/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:ah #:almighty-html)
                    (#:hhc #:home/hypermedia/components)
                    (#:hy #:almightylisp/hypermedia))
  (:export #:index))

(in-package #:home/controllers)

(defun index ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (hhc:ac-home-layout :title "Almighty Lisp: Lisp & Emacs Essentials")))))
