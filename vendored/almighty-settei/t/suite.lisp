(defpackage #:almighty-settei/t/suite
  (:use #:cl)
  (:local-nicknames (#:settei #:almighty-settei))
  (:export #:with-registry #:fake-environment #:load-with #:value #:value-source))

(in-package #:almighty-settei/t/suite)

(defmacro with-registry (&body body)
  "Run BODY with a registry, configuration and environment of its own: what
it defines, configures and loads is gone after."
  `(let ((settei:*registry* (settei:make-registry))
         (settei:*configuration* nil)
         (settei:*default-environment* :development)
         (settei:*environment-variable-prefix* ""))
     ,@body))

(defun fake-environment (&rest names-and-values)
  "A GETENV for LOAD-SETTINGS: these variables, and no others."
  (lambda (name)
    (loop for (variable value) on names-and-values by #'cddr
          when (string= variable name)
            return value)))

(defun load-with (&rest arguments &key environment-variables &allow-other-keys)
  "LOAD-SETTINGS with ENVIRONMENT-VARIABLES, a plist of names and values, as
the environment, and no .env file unless :DOTENV is given."
  (let ((arguments (loop for (key value) on arguments by #'cddr
                         unless (eq key :environment-variables)
                           append (list key value))))
    (apply #'settei:load-settings
           (append arguments
                   (list :getenv (apply #'fake-environment environment-variables)
                         :dotenv nil)))))

;; Through a variable: the tests' settings are defined as they run, so SETTING's
;; compiler macro, which warns about names it doesn't know, isn't involved.
(defun value (name &optional (configuration (settei:current-configuration)))
  (settei:setting name configuration))

(defun value-source (name)
  (settei:setting-source name))

(defun (setf value) (new name)
  (setf (settei:setting name) new))
