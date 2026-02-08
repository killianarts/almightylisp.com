(defpackage #:almightylisp/app
  (:use #:cl))

(in-package #:almightylisp/app)

(defparameter *app* (make-instance 'shiso:application :routes shiso:*routes*))

(lack:builder (:static
               :path (lambda (path)
                       (if (ppcre:scan "^(?:/css/|/js/|/assets/|/images/|/img/|/robot\\.txt$|/favicon\\.ico$)" path)
                           path
                           nil))
               :root (asdf:system-relative-pathname :almightylisp #P"public/")) *app*)
