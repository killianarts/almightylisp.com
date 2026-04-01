(defpackage #:book
  (:use #:cl
    #:book/controllers
    #:book/forms
    #:book/models
    #:book/routes)
  (:local-nicknames (#:s #:shiso)))

(in-package #:book)
