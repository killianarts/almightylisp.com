(defpackage #:almightylisp/routes
  (:use #:cl)
  (:export #:*routes*
           #:routes
           #:routes-mapper))
(in-package #:almightylisp/routes)

(defclass routes ()
  ((mapper :initarg :mapper :reader routes-mapper :initform nil)))

(defparameter *routes* (make-instance 'routes :mapper (myway:make-mapper)))


