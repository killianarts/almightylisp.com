(defpackage #:almightylisp/settings
  (:use #:cl))

(in-package #:almightylisp/settings)

;;;; The site's settings: Shiso's and every module's, in one place.
;;;;
;;;; What differs between machines goes in the environment: on the server,
;;;; almightylisp.env (HOST, PORT, SHISO_ENV=production); here, .env
;;;; (SHISO_ENV=development, and anything else for this machine). Secrets,
;;;; like GITHUB_TOKEN, only ever go there.
;;;;
;;;; This file loads before the modules that define the :magazine/ settings;
;;;; their names are checked when the server starts. `(shiso:print-settings)`,
;;;; or `almightylisp settings`, lists them all, with where each value came
;;;; from.

(shiso:configure
  ;; In development, absolute URLs point at this machine.
  (:shiso :site-origin (shiso:per-environment :production "https://almightylisp.com"))
  ;; Relative to the directory the server runs in.
  (:database :name "db/almightylisp2.db"))
