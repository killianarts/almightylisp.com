(defpackage #:author/models
  (:use #:cl))

(in-package #:author/models)

(shiso:define-model author
    ((title :field-type :char
       :max-length 50)))

(shiso/admin:define-admin author)
