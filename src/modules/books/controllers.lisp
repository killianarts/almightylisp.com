(defpackage #:books/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso))
  (:export #:index))

(in-package #:books/controllers)

(defun index ()
  (s:http-response "Welcome to books"))
