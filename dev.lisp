(defpackage #:almightylisp-dev
  (:use #:cl)
  (:export #:main))
(in-package #:almightylisp-dev)

;; Load your system
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :ignore-inherited-configuration))

(asdf:load-system "almightylisp")

(defparameter *appfile* "app.lisp")

;; Environment variable utilities.
(defun get-env-int (env default)
  (let ((env (uiop:getenv env)))
    (if env
        (parse-integer env :junk-allowed t)
        default)))

(defun get-env (env default)
  (or (uiop:getenv env) default))

(defun envp (env)
  (if (string-equal (uiop:getenvp env) "true")
      t
      nil))

(defun main ()
  (let ((host (get-env "HOST" "127.0.0.1"))
        (port (get-env-int "PORT" 5000))
        (debugp (envp "DEBUGP"))) 
    (shiso:start *appfile* port)))

(main)
