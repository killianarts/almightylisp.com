(defpackage #:almightylisp
  (:use #:cl)
  (:export
   #:start-server
   #:stop-server
   #:*almightylisp-application*))
(in-package #:almightylisp)

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

(shiso:define-application *almightylisp-application* ()
  (:modules
   ("/admin" shiso-admin)
   ("/book" book)
   ("/article" article)
   ("/author" author)
   ("" home)))

(defun setup-database ()
  (ensure-directories-exist #P"db/")
  (mito:connect-toplevel :sqlite3 :database-name "db/almightylisp.db")
  (dolist (model-name (shiso/models:all-models))
    (mito:ensure-table-exists (shiso/models:model-class model-name))))

(defun start-server (&key (host (get-env "HOST" "127.0.0.1")) (port (get-env-int "PORT" 5000)) (debugp (envp "DEBUGP")))
  (setup-database)
  (shiso:start *almightylisp-application* :host host :port port :debugp debugp))
#+nil
(start-server)

(defun stop-server ()
  (shiso:stop))
#+nil
(stop-server)
