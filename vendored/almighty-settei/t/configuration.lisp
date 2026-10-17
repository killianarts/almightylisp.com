(defpackage #:almighty-settei/t/configuration
  (:use #:cl #:lisp-unit2 #:almighty-settei/t/suite)
  (:local-nicknames (#:settei #:almighty-settei)))

(in-package #:almighty-settei/t/configuration)

;; Names WITH-SETTINGS is given literally below, known so SETTING's compiler
;; macro doesn't warn about them.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #'settei:note-setting-name '(:t/port :t/desk)))

(defun define-port ()
  (settei:define-setting :t/port settei:integer-setting
    "The port."
    :environment "T_PORT"
    :minimum 1 :maximum 65535
    :default 1000))

;;; Sources

(define-test the-default-alone ()
  (with-registry
    (define-port)
    (load-with)
    (assert-eql 1000 (value :t/port))
    (assert-eq :default (settei:setting-source :t/port))))

(define-test each-source-beats-the-next ()
  (with-registry
    (define-port)
    (settei:configure :t/port 2000)
    (load-with)
    (assert-eql 2000 (value :t/port))
    (assert-eq :project (settei:setting-source :t/port))
    (load-with :dotenv '(("T_PORT" . "3000")))
    (assert-eql 3000 (value :t/port))
    (assert-eq :dotenv (settei:setting-source :t/port))
    (load-with :dotenv '(("T_PORT" . "3000")) :environment-variables '("T_PORT" "4000"))
    (assert-eql 4000 (value :t/port))
    (assert-eq :environment (settei:setting-source :t/port))
    (setf (value :t/port) 5000)
    (assert-eql 5000 (value :t/port))
    (assert-eq :session (settei:setting-source :t/port))
    (load-with :environment-variables '("T_PORT" "4000") :overrides '(:t/port 6000))
    (assert-eql 6000 (value :t/port))))

(define-test dotenv-only-in-development ()
  (with-registry
    (define-port)
    (load-with :environment :production :dotenv '(("T_PORT" . "3000")))
    (assert-eql 1000 (value :t/port))
    ;; The file may say the machine is for development, and then it counts.
    (load-with :environment :production
               :dotenv '(("SETTEI_ENV" . "development") ("T_PORT" . "3000")))
    (assert-true (settei:development-p))
    (assert-eq :dotenv (value-source :settei/environment))
    (assert-eql 3000 (value :t/port))
    ;; A real environment variable wins over the file.
    (load-with :environment :production
               :environment-variables '("SETTEI_ENV" "production")
               :dotenv '(("SETTEI_ENV" . "development") ("T_PORT" . "3000")))
    (assert-true (settei:production-p))
    (assert-eql 1000 (value :t/port))))

(define-test an-empty-variable-is-not-set ()
  (with-registry
    (define-port)
    (load-with :environment-variables '("T_PORT" ""))
    (assert-eql 1000 (value :t/port))))

(define-test the-first-variable-set-wins ()
  (with-registry
    (settei:define-setting :t/name settei:string-setting
      :environment '("NEW_NAME" "OLD_NAME"))
    (load-with :environment-variables '("OLD_NAME" "old"))
    (assert-equal "old" (value :t/name))
    (load-with :environment-variables '("OLD_NAME" "old" "NEW_NAME" "new"))
    (assert-equal "new" (value :t/name))))

(define-test variables-derived-from-names ()
  (with-registry
    (settei:define-setting :magazine/desk-enabled settei:boolean-setting)
    (let ((setting (settei:find-setting :magazine/desk-enabled)))
      (assert-equal '("MAGAZINE_DESK_ENABLED") (settei:setting-environment-variables setting))
      (let ((settei:*environment-variable-prefix* "SHISO_"))
        (assert-equal '("SHISO_MAGAZINE_DESK_ENABLED")
                      (settei:setting-environment-variables setting))
        ;; A name starting with the prefix doesn't repeat it.
        (settei:define-setting :shiso/site-origin settei:string-setting)
        (assert-equal '("SHISO_SITE_ORIGIN")
                      (settei:setting-environment-variables
                       (settei:find-setting :shiso/site-origin)))))
    (load-with :environment-variables '("MAGAZINE_DESK_ENABLED" "yes"))
    (assert-eq t (value :magazine/desk-enabled))))

(define-test a-computed-default-may-read-other-settings ()
  (with-registry
    (define-port)
    (settei:define-setting :t/origin settei:string-setting
      :default (lambda (configuration)
                 (declare (ignore configuration))
                 (format nil "http://localhost:~d" (value :t/port))))
    (load-with :environment-variables '("T_PORT" "5002"))
    (assert-equal "http://localhost:5002" (value :t/origin))
    (assert-eq :computed (settei:setting-source :t/origin))
    (setf (value :t/port) 5003)
    (assert-equal "http://localhost:5003" (value :t/origin))))

;;; The environment

(define-test the-environment ()
  (with-registry
    (load-with)
    (assert-eq :development (settei:current-environment))
    (assert-true (settei:development-p))
    (load-with :environment :production)
    (assert-true (settei:production-p))
    (load-with :environment :production :environment-variables '("SETTEI_ENV" "test"))
    (assert-true (settei:test-p))))

(define-test defaults-per-environment ()
  (with-registry
    (settei:define-setting :t/desk settei:boolean-setting
      :default (settei:per-environment :development t :otherwise nil))
    (settei:define-setting :t/level settei:integer-setting
      :default (settei:per-environment :production 3 :test 2))
    (load-with)
    (assert-eq t (value :t/desk))
    (assert-eq :environment-default (settei:setting-source :t/desk))
    (assert-eq nil (value :t/level))
    (load-with :environment :production)
    (assert-eq nil (value :t/desk))
    (assert-eql 3 (value :t/level))
    (setf (value :settei/environment) :test)
    (assert-eql 2 (value :t/level))))

(define-test a-framework-names-the-environment-setting ()
  (let ((almighty-settei/registry::*builtin-settings* almighty-settei/registry::*builtin-settings*)
        (settei:*environment-setting* settei:*environment-setting*))
    (with-registry
      (settei:define-environment-setting :t/environment
        "What kind of machine this is."
        :environment "T_ENV")
      (assert-eq :t/environment settei:*environment-setting*)
      (assert-false (settei:find-setting :settei/environment :errorp nil))
      (settei:define-setting :t/desk settei:boolean-setting
        :default (settei:per-environment :development t :otherwise nil))
      (load-with :environment-variables '("T_ENV" "production" "SETTEI_ENV" "test"))
      (assert-true (settei:production-p))
      (assert-eq nil (value :t/desk))
      (assert-equal '(:t/environment :t/desk)
                    (mapcar #'settei:setting-name (settei:list-settings)))
      ;; Registries made from now on have it, and not the library's.
      (assert-true (settei:find-setting :t/environment :registry (settei:make-registry)))
      (assert-false (settei:find-setting :settei/environment :errorp nil
                                                             :registry (settei:make-registry))))))

(define-condition framework-settings-invalid (settei:settings-invalid) ())
(define-condition framework-setting-unknown (settei:setting-unknown) ())

(define-test a-framework-names-the-conditions ()
  (let ((almighty-settei/conditions::*condition-classes* (make-hash-table :test 'eq)))
    (settei:use-condition-class 'settei:settings-invalid 'framework-settings-invalid)
    (settei:use-condition-class 'settei:setting-unknown 'framework-setting-unknown)
    (assert-error 'error (settei:use-condition-class 'settei:setting-unknown 'framework-settings-invalid))
    (with-registry
      (define-port)
      (assert-error 'framework-settings-invalid
                    (load-with :environment-variables '("T_PORT" "port")))
      ;; A handler for the library's class catches the framework's.
      (assert-error 'settei:settings-invalid
                    (load-with :environment-variables '("T_PORT" "port")))
      (load-with)
      (assert-error 'framework-setting-unknown (value :t/nothing))
      ;; Not every class need be the framework's.
      (assert-typep 'settei:setting-invalid
                    (handler-case (setf (value :t/port) "port")
                      (settei:setting-invalid (condition) condition))))))

;;; Checking

(defun problems-of (thunk)
  (handler-case (progn (funcall thunk) nil)
    (settei:settings-invalid (condition)
      (mapcar (lambda (problem)
                (list (settei:problem-name problem) (settei:problem-message problem)))
              (settei:settings-invalid-problems condition)))))

(define-test every-problem-at-once ()
  (with-registry
    (define-port)
    (settei:define-setting :t/workers settei:integer-setting :environment "T_WORKERS")
    (settei:define-setting :t/origin settei:string-setting
      :environment "T_ORIGIN"
      :required-in '(:production))
    (assert-equal '((:t/port "\"50OO\" from T_PORT is not an integer")
                    (:t/workers "\"many\" from T_WORKERS is not an integer")
                    (:t/origin "is required in production, and not set (T_ORIGIN)"))
                  (problems-of (lambda ()
                                 (load-with :environment :production
                                            :environment-variables '("T_PORT" "50OO"
                                                                     "T_WORKERS" "many")))))
    (assert-eq nil (problems-of (lambda () (load-with))))))

(define-test the-report-of-problems ()
  (with-registry
    (define-port)
    (let ((text (handler-case (load-with :environment-variables '("T_PORT" "0"))
                  (settei:settings-invalid (condition) (princ-to-string condition)))))
      (assert-true (search "1 setting is wrong for development" text) text)
      (assert-true (search ":t/port  \"0\" from T_PORT is less than 1" text) text))))

(define-test continuing-uses-the-defaults ()
  (with-registry
    (define-port)
    (handler-bind ((settei:settings-invalid #'continue))
      (load-with :environment-variables '("T_PORT" "port")))
    (assert-eql 1000 (value :t/port))
    (assert-eq :default (settei:setting-source :t/port))))

(define-test a-wrong-environment ()
  (with-registry
    (assert-equal '((:settei/environment
                     "\"staging\" from SETTEI_ENV is not one of development, test, production"))
                  (problems-of (lambda ()
                                 (load-with :environment-variables '("SETTEI_ENV" "staging")))))))

(define-test unknown-names ()
  (with-registry
    (load-with)
    (assert-error 'settei:setting-unknown (value :t/nothing))
    (assert-error 'settei:setting-unknown (load-with :overrides '(:t/nothing 1)))))

;;; The project's values

(define-test configuring-before-defining ()
  (with-registry
    (settei:configure :t/port 2000)
    (define-port)
    (load-with)
    (assert-eql 2000 (value :t/port))
    (assert-eq :project (settei:setting-source :t/port))))

(define-test configuring-in-groups ()
  (with-registry
    (settei:configure (:t :port 2000 :host "0.0.0.0")
                      :t/name "almighty")
    (define-port)
    (settei:define-setting :t/host settei:string-setting)
    (settei:define-setting :t/name settei:string-setting)
    (load-with)
    (assert-eql 2000 (value :t/port))
    (assert-equal "0.0.0.0" (value :t/host))
    (assert-equal "almighty" (value :t/name))))

(define-test configuring-per-environment ()
  (with-registry
    (settei:configure (:t :port (settei:per-environment :production 80 :otherwise 5000)))
    (define-port)
    (load-with)
    (assert-eql 5000 (value :t/port))
    (assert-eq :project (settei:setting-source :t/port))
    (setf (value :settei/environment) :production)
    (assert-eql 80 (value :t/port))
    (load-with :environment :production)
    (assert-eql 80 (value :t/port))))

(define-test configuring-for-some-environments-only ()
  (with-registry
    (settei:configure (:t :port (settei:per-environment :production 80)))
    (define-port)
    ;; Not given for development: the setting's default.
    (load-with)
    (assert-eql 1000 (value :t/port))
    (assert-eq :default (settei:setting-source :t/port))
    (load-with :environment :production)
    (assert-eql 80 (value :t/port))
    (assert-eq :project (settei:setting-source :t/port))))

(define-test configuring-a-function ()
  (with-registry
    (let ((calls 0))
      (settei:configure (:t :port (lambda (configuration)
                                    (declare (ignore configuration))
                                    (incf calls)
                                    (+ 2000 calls))))
      (define-port)
      (assert-eql 0 calls)
      (load-with)
      (assert-eql 2001 (value :t/port))
      (assert-eq :project (settei:setting-source :t/port))
      ;; Read again, as a computed value is, when a setting changes.
      (setf (value :settei/environment) :test)
      (assert-eql 2002 (value :t/port)))))

(define-test configuring-wrongly-written ()
  ;; CONFIGURE is a macro: a mistake in its form is found when it expands.
  (assert-error 'error (macroexpand '(settei:configure :t/port)))
  (assert-error 'error (macroexpand '(settei:configure (:t :port))))
  (assert-error 'error (macroexpand '(settei:configure "port" 1)))
  (with-registry
    (assert-error 'error (settei:add-project-values '(:t/port)))
    (settei:add-project-values (list :t/port 2000))
    (define-port)
    (load-with)
    (assert-eql 2000 (value :t/port))))

(define-test a-configured-name-that-is-no-setting ()
  (with-registry
    (define-port)
    (settei:configure :t/prot 2000)
    (assert-equal '((:t/prot "is configured, but there is no such setting: is it misspelt, or is its module not loaded?"))
                  (problems-of (lambda () (load-with))))
    (handler-bind ((settei:settings-invalid #'continue))
      (load-with))
    (assert-eql 1000 (value :t/port))))

(define-test a-configured-secret ()
  (with-registry
    (settei:configure :t/token "in a file")
    (settei:define-setting :t/token settei:string-setting
      :environment "T_TOKEN"
      :secret t)
    (assert-equal '((:t/token "is a secret, so it can't be configured: set T_TOKEN in the environment"))
                  (problems-of (lambda () (load-with :environment-variables '("T_TOKEN" "from-env")))))
    (handler-bind ((settei:settings-invalid #'continue))
      (load-with :environment-variables '("T_TOKEN" "from-env")))
    (assert-equal "from-env" (value :t/token))))

(define-test a-misspelt-name-warns-when-compiled ()
  (assert-warning 'settei:unknown-setting-warning
                  (compile nil '(lambda () (settei:setting :t/never-defined))))
  (assert-no-warning 'settei:unknown-setting-warning
                     (compile nil '(lambda () (settei:setting :settei/environment)))))

;;; Writing

(define-test setting-a-wrong-value ()
  (with-registry
    (define-port)
    (load-with)
    (let ((condition (handler-case (setf (value :t/port) "port")
                       (settei:setting-invalid (condition) condition))))
      (assert-eq :session (settei:setting-invalid-source condition)))
    (assert-eql 1000 (value :t/port))))

(define-test setting-from-a-string ()
  (with-registry
    (define-port)
    (load-with)
    (assert-eql 8080 (setf (value :t/port) "8080"))
    (assert-eql 8080 (value :t/port))))

(define-test unsetting ()
  (with-registry
    (define-port)
    (load-with :environment-variables '("T_PORT" "4000"))
    (setf (value :t/port) 5000)
    (assert-eql 4000 (settei:setting-unset :t/port))
    (assert-eq :environment (settei:setting-source :t/port))))

(define-test configuring-after-loading ()
  (with-registry
    (define-port)
    (load-with)
    (settei:configure :t/port 2000)
    (assert-eql 2000 (value :t/port))))

(define-test redefining-keeps-a-session-value ()
  (with-registry
    (define-port)
    (settei:define-setting :t/host settei:string-setting :default "127.0.0.1")
    (load-with)
    (setf (value :t/port) 5000)
    (value :t/host)
    (define-port)
    (settei:define-setting :t/host settei:string-setting :default "0.0.0.0")
    (assert-eql 5000 (value :t/port))
    (assert-equal "0.0.0.0" (value :t/host))))

;;; Secrets

(define-test secrets-come-from-the-environment ()
  (with-registry
    (settei:define-setting :t/token settei:string-setting
      :environment "T_TOKEN"
      :secret t)
    (assert-error 'settei:secret-configured (settei:configure :t/token "abc"))
    (load-with :environment-variables '("T_TOKEN" "s3cret-token"))
    (assert-equal "s3cret-token" (value :t/token))))

;;; Listeners

(define-test listeners ()
  (with-registry
    (define-port)
    (let ((calls nil))
      (load-with)
      (settei:on-setting-change :t/port (lambda (before after) (push (list before after) calls)))
      (setf (value :t/port) 5000)
      (setf (value :t/port) 5000)
      (settei:setting-unset :t/port)
      (assert-equal '((5000 1000) (1000 5000)) calls)
      (setf calls nil)
      (settei:reload-settings :getenv (fake-environment "T_PORT" "7000") :dotenv nil :overrides nil)
      (assert-equal '((1000 7000)) calls))))

(define-test reloading-keeps-session-values ()
  (with-registry
    (define-port)
    (settei:define-setting :t/host settei:string-setting :environment "T_HOST")
    (load-with :environment :production)
    (setf (value :t/port) 5000)
    (settei:reload-settings :getenv (fake-environment "T_HOST" "0.0.0.0") :dotenv nil)
    (assert-eql 5000 (value :t/port))
    (assert-equal "0.0.0.0" (value :t/host))
    (assert-true (settei:production-p))))

;;; For a while

(define-test with-settings ()
  (with-registry
    (define-port)
    (settei:define-setting :t/desk settei:boolean-setting)
    (load-with)
    (settei:with-settings (:t/port 2000 :t/desk "yes")
      (assert-eql 2000 (value :t/port))
      (assert-eq t (value :t/desk)))
    (assert-eql 1000 (value :t/port))
    (assert-eq nil (value :t/desk))))

;;; Loading, and not

(define-test loaded-on-first-use-in-development ()
  (with-registry
    (define-port)
    (assert-false (settei:settings-loaded-p))
    (assert-eql 1000 (value :t/port))
    (assert-true (settei:settings-loaded-p))))

(define-test a-saved-image-forgets-its-settings ()
  (with-registry
    (define-port)
    (load-with)
    (uiop:call-functions '(almighty-settei/configuration::forget-on-restore))
    (assert-false (settei:settings-loaded-p))
    (assert-eq :production settei:*default-environment*)
    (assert-error 'settei:settings-not-loaded (value :t/port))
    (load-with)
    (assert-true (settei:production-p))))
