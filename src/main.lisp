(defpackage #:almightylisp
  (:use #:cl))
(in-package #:almightylisp)

;; * Environment variable utilities.
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

;; * Application Configuration/Middleware
(defparameter *app* (make-instance 'shiso:application :routes shiso:*routes*))

(setf *app* (lack:builder (:static
                           :path (lambda (path)
                                   (if (ppcre:scan "^(?:/css/|/js/|/assets/|/images/|/img/|/robot\\.txt$|/favicon\\.ico$)" path)
                                       path
                                       nil))
                           :root (asdf:system-relative-pathname :almightylisp #P"public/")) *app*))

(defun start-server (&key (host (get-env "HOST" "127.0.0.1")) (port (get-env-int "PORT" 5000)) (debugp (envp "DEBUGP")))
  (shiso:start *app* :host host :port port :debugp debugp))

