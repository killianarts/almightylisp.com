(defpackage #:almightylisp
  (:use #:cl)
  (:export
   #:main
   #:start-server
   #:stop-server
   #:*almightylisp-application*))
(in-package #:almightylisp)

;; The magazine is the site's root. Its articles are at /article/:slug, which
;; the article module's mount hands on: that module answers only /article and
;; /article/create. static/js/magazine-fx.js lists the other modules' prefixes
;; (OTHER_SECTIONS); a module mounted here belongs there too.
(shiso:define-application *almightylisp-application* ()
  (:modules
   ("/admin" shiso-admin)
   ("/book" book)
   ("/article" article)
   ("/author" author)
   ("/magazine" magazine-moved)
   ("" home)
   ("" magazine)))

;;; The settings (src/settings.lisp, the environment, and .env in
;;; development) are loaded, the database connected and its tables made, by
;;; SHISO:START. `(shiso:print-settings)` lists them.

(defun start-server (&key (host nil host-p) (port nil port-p) (debugp nil debugp-p))
  "Start the site. HOST, PORT and DEBUGP, when given, set :server/host,
:server/port and :server/debugp for this run."
  (when host-p (setf (shiso:setting :server/host) host))
  (when port-p (setf (shiso:setting :server/port) port))
  (when debugp-p (setf (shiso:setting :server/debugp) debugp))
  (shiso:start *almightylisp-application*))
#+nil
(start-server)

(defun stop-server ()
  (shiso:stop))
#+nil
(stop-server)

(defun run-server ()
  "Start the server, and keep the process alive."
  (handler-case
      (progn
        (start-server)
        (loop (sleep 60)))
    (error (c)
      (format *error-output* "Aborting. ~a ~&" c)
      (force-output *error-output*)
      (uiop:quit 1))))

(defun main ()
  "Executable entry point. With no arguments, the server starts; with a
management command's name, it runs: `almightylisp settings` lists the
settings, `almightylisp createsuperuser` makes an admin."
  (shiso:run-cli (uiop:command-line-arguments)
                 :program "almightylisp"
                 :serve #'run-server))
