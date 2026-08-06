(defpackage #:almightylisp
  (:use #:cl)
  (:export
   #:main
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
  (mito:connect-toplevel :sqlite3 :database-name "db/almightylisp2.db")
  (shiso:ensure-session-table :table-name "shiso_session")
  (dolist (model-name (shiso/models:all-models))
    (mito:ensure-table-exists (shiso/models:model-class model-name)))

  ;; If you are using Lack's DBI-backed session store (e.g. via shiso/auth/session
  ;; or directly with (:session :store (lack/session/store/dbi:make-dbi-store ...))
  ;; with table-name "shiso_session"), make sure the table exists:
  ;; (shiso:ensure-session-table :table-name "shiso_session")
  ;;
  ;; Call it here after the mito connection so the session table is created
  ;; alongside your models. The lack dbi store itself does not create the table.
  )

(defun start-server (&key (host (get-env "HOST" "127.0.0.1")) (port (get-env-int "PORT" 5000)) (debugp (envp "DEBUGP")))
  (setup-database)
  (shiso:start *almightylisp-application* :host host :port port :debugp debugp))
#+nil
(start-server)

(defun stop-server ()
  (shiso:stop))
#+nil
(stop-server)

(defun main ()
  "Executable entry point. Connects the DB, then starts the server.
Must call setup-database: the Lack DBI session store reuses
mito.connection:*connection*, which is NIL until connect-toplevel runs."
  (let ((host (get-env "HOST" "127.0.0.1"))
        (port (get-env-int "PORT" 5000))
        (debugp (envp "DEBUGP")))
    (handler-case
        (progn
          (start-server :host host :port port :debugp debugp)
          (loop (sleep 60)))
      (error (c)
        (format *error-output* "Aborting. ~a ~&" c)
        (force-output *error-output*)
        (uiop:quit 1)))))
