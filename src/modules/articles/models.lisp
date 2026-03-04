(defpackage #:articles/models
  (:use #:cl))

(in-package #:articles/models)

(shiso:define-model article
    ((title :initarg :title :col-type (:varchar 200))))

(shiso/admin:define-admin "article")
