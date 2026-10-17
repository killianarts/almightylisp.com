(defpackage #:almighty-settei/configuration
  (:use #:cl #:almighty-settei/conditions #:almighty-settei/kinds
        #:almighty-settei/registry #:almighty-settei/dotenv)
  (:export #:configuration #:*configuration* #:*default-environment*
           #:configuration-default-environment
           #:load-settings #:reload-settings #:forget-settings #:settings-loaded-p
           #:current-configuration #:copy-configuration
           #:setting #:setting-source #:setting-unset
           #:configure #:add-project-values #:per-environment #:per-environment-p #:per-environment-values
           #:*environment-setting* #:define-environment-setting #:*dotenv-environments*
           #:current-environment #:development-p #:test-p #:production-p
           #:with-settings #:on-setting-change #:remove-setting-listener
           #:describe-source))

(in-package #:almighty-settei/configuration)

;;;; A configuration: every setting's value for one run of the program, and
;;;; where each came from.
;;;
;;; A setting takes the first value found among, in order:
;;;
;;;   :session              set in this run: (setf setting), or LOAD-SETTINGS's
;;;                         :overrides
;;;   :environment          its environment variable
;;;   :dotenv               the same variable in the .env file, in development
;;;                         (*DOTENV-ENVIRONMENTS*); the environment setting
;;;                         anywhere
;;;   :project              CONFIGURE, the project's settings file
;;;   :environment-default  its default, when that is a PER-ENVIRONMENT
;;;   :computed             its default, when that is a function
;;;   :default              its default
;;;
;;; Values are read when first asked for and kept. LOAD-SETTINGS reads them
;;; all at once, to check them before the program goes on.
;;;
;;; Nothing is read when code is loaded. LOAD-SETTINGS is called when the
;;; program starts; in a saved image, what was loaded when it was saved is
;;; forgotten as it starts (see FORGET-ON-RESTORE).

(defvar *default-environment* :development
  "The environment when the environment setting isn't set: :development in an
image loaded from source, :production in one started from a saved image,
such as a built program.")

(defvar *configuration* nil
  "The settings' values in this run, or NIL until they are loaded. The global
value is the program's; WITH-SETTINGS binds another, in one thread.")

(defun make-table ()
  (make-hash-table :test 'eq #+sbcl :synchronized #+sbcl t))

(defun copy-table (table)
  (let ((copy (make-table)))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defclass configuration ()
  ((default-environment :initarg :default-environment
                        :reader configuration-default-environment)
   (getenv :initarg :getenv :reader configuration-getenv
           :documentation "A function of an environment variable's name.")
   (dotenv :initarg :dotenv :reader configuration-dotenv
           :documentation "The .env file's variables, an alist.")
   (sessions :initarg :sessions :reader configuration-sessions
             :documentation "Name to value, set in this run.")
   (resolved :initarg :resolved :reader configuration-resolved
             :documentation "Name to (value . source), for the settings read so far.")
   (notify-p :initarg :notify-p :reader configuration-notify-p
             :documentation "Whether a change calls the setting's listeners."))
  (:default-initargs :default-environment *default-environment* :getenv #'uiop:getenv
                     :dotenv nil :sessions (make-table) :resolved (make-table)
                     :notify-p t))

(defun copy-configuration (configuration &key (notify-p (configuration-notify-p configuration)))
  (make-instance 'configuration
                 :default-environment (configuration-default-environment configuration)
                 :getenv (configuration-getenv configuration)
                 :dotenv (configuration-dotenv configuration)
                 :sessions (copy-table (configuration-sessions configuration))
                 :resolved (copy-table (configuration-resolved configuration))
                 :notify-p notify-p))

;;; A misspelt name, caught when the code is compiled

(define-compiler-macro setting (&whole form name &optional configuration)
  (declare (ignore configuration))
  (when (and (keywordp name) (not (known-setting-name-p name)))
    (warn (condition-class 'unknown-setting-warning) :name name))
  form)

(define-compiler-macro (setf setting) (&whole form value name &optional configuration)
  (declare (ignore value configuration))
  (when (and (keywordp name) (not (known-setting-name-p name)))
    (warn (condition-class 'unknown-setting-warning) :name name))
  form)

;;; The environment

(defvar *environment-setting* nil
  "The name of the setting that says what kind of machine this is. A
framework names its own with DEFINE-ENVIRONMENT-SETTING.")

(defmacro define-environment-setting (name &body options)
  "Make NAME the setting that says what kind of machine this is, in place of
the one before, in every registry: a choice of :development, :test and
:production, which other settings' defaults may depend on (see
PER-ENVIRONMENT). OPTIONS are DEFINE-SETTING's; :environment names its
variable. A framework built on this library names its own:

  (define-environment-setting :shiso/environment
    \"What kind of machine this is.\"
    :environment \"SHISO_ENV\")"
  (let* ((documentation (when (stringp (first options)) (list (first options))))
         (options (if documentation (rest options) options)))
    `(progn
       (replace-environment-setting ,name)
       (define-builtin-setting ,name choice-setting
         ,@documentation
         ,@options
         :options '(:development :test :production)
         :default #'configuration-default-environment))))

(defun replace-environment-setting (name)
  "Forget the environment setting before NAME, and make NAME the one."
  (let ((before *environment-setting*))
    (when (and before (not (eq before name)))
      (remove-builtin-setting before)
      (remove-setting before))
    (setf *environment-setting* name)))

(define-environment-setting :settei/environment
  "What kind of machine this is. Other settings' defaults may depend on it
(see PER-ENVIRONMENT)."
  :environment "SETTEI_ENV")

(defstruct (per-environment (:constructor %per-environment (values)))
  values)

(defun per-environment (&rest values &key &allow-other-keys)
  "A default that depends on the environment:

  (per-environment :development t :otherwise nil)

A value for each environment named, and :OTHERWISE for the rest (NIL when it
isn't given)."
  (%per-environment values))

(defun per-environment-value (default environment)
  "DEFAULT's value for ENVIRONMENT, or its :OTHERWISE. A second value is
true when either was given."
  (let ((values (per-environment-values default)))
    (dolist (key (list environment :otherwise) (values nil nil))
      (loop for (given value) on values by #'cddr
            when (eq given key)
              do (return-from per-environment-value (values value t))))))

(defun current-environment (&optional (configuration (current-configuration)))
  "The environment: :development, :test or :production."
  (setting *environment-setting* configuration))

(defun development-p (&optional (configuration (current-configuration)))
  (eq (current-environment configuration) :development))

(defun test-p (&optional (configuration (current-configuration)))
  (eq (current-environment configuration) :test))

(defun production-p (&optional (configuration (current-configuration)))
  (eq (current-environment configuration) :production))

;;; Reading a value from its sources

(defun default-value (setting configuration)
  "SETTING's default in CONFIGURATION, and which kind of default it is."
  (let ((default (setting-default setting)))
    (cond ((per-environment-p default)
           (values (per-environment-value default (current-environment configuration))
                   :environment-default))
          ((functionp default)
           (values (funcall default configuration) :computed))
          (t (values default :default)))))

(defvar *dotenv-environments* '(:development)
  "The environments where the .env file's variables are read. The environment
setting itself is read from it anywhere, so the file can say what kind of
machine this is.")

(defun dotenv-applies-p (name configuration)
  (or (eq name *environment-setting*)
      (member (current-environment configuration) *dotenv-environments*)))

(defun given-p (value)
  (and value (plusp (length value))))

(defun raw-value (setting configuration)
  "SETTING's value before it is checked, its source, and the environment
variable it came from, if one."
  (let ((name (setting-name setting)))
    (multiple-value-bind (value found) (gethash name (configuration-sessions configuration))
      (when found
        (return-from raw-value (values value :session nil))))
    (let ((variables (setting-environment-variables setting)))
      (dolist (variable variables)
        (let ((value (funcall (configuration-getenv configuration) variable)))
          (when (given-p value)
            (return-from raw-value (values value :environment variable)))))
      (when (dotenv-applies-p name configuration)
        (dolist (variable variables)
          (let ((value (cdr (assoc variable (configuration-dotenv configuration) :test #'string=))))
            (when (given-p value)
              (return-from raw-value (values value :dotenv variable)))))))
    (unless (setting-secret-p setting)
      (multiple-value-bind (value found) (gethash name (registry-project-values *registry*))
        (when found
          (cond ((eq name *environment-setting*)
                 ;; The environment can't depend on itself.
                 (return-from raw-value (values value :project nil)))
                ((per-environment-p value)
                 ;; Given for some environments only: in the others, the
                 ;; setting's default.
                 (multiple-value-bind (value given)
                     (per-environment-value value (current-environment configuration))
                   (when given
                     (return-from raw-value (values value :project nil)))))
                ((functionp value)
                 (return-from raw-value (values (funcall value configuration) :project nil)))
                (t
                 (return-from raw-value (values value :project nil)))))))
    (default-value setting configuration)))

(defun resolve (setting configuration)
  "SETTING's value in CONFIGURATION and its source. Signals SETTING-INVALID,
with the source, when the value is refused."
  (multiple-value-bind (raw source variable) (raw-value setting configuration)
    (handler-case (values (accept-value setting raw configuration) source)
      (setting-invalid (condition)
        (if (eq (setting-invalid-name condition) (setting-name setting))
            (error (condition-class 'setting-invalid) :name (setting-name setting) :value raw
                                    :source source :variable variable
                                    :message (settei-error-message condition))
            (error condition))))))

(defun lookup (name configuration)
  "NAME's (value . source) in CONFIGURATION, read the first time it is asked for."
  (let ((setting (find-setting name))
        (table (configuration-resolved configuration)))
    (or (gethash name table)
        (setf (gethash name table)
              (multiple-value-call #'cons (resolve setting configuration))))))

(defun derived-p (name source)
  "Whether a value from SOURCE depends on other settings: a computed default,
or a default or project value given per environment or as a function."
  (or (member source '(:environment-default :computed))
      (and (eq source :project)
           (let ((value (gethash name (registry-project-values *registry*))))
             (or (per-environment-p value) (functionp value))))))

(defun forget-derived (configuration)
  "Forget the values computed from others, so they are computed again."
  (let ((table (configuration-resolved configuration))
        (stale nil))
    (maphash (lambda (name entry)
               (when (derived-p name (cdr entry))
                 (push name stale)))
             table)
    (dolist (name stale)
      (remhash name table))))

;;; Reading and writing

(defun current-configuration ()
  "*CONFIGURATION*. When nothing is loaded yet, in development, the settings
are loaded now; anywhere else, that is SETTINGS-NOT-LOADED."
  (or *configuration*
      (if (eq *default-environment* :development)
          (load-settings)
          (error (condition-class 'settings-not-loaded)))))

(defun setting (name &optional (configuration (current-configuration)))
  "The value of the setting NAME. Signals SETTING-UNKNOWN when there is no
such setting, and SETTING-INVALID when its value is refused."
  (car (lookup name configuration)))

(defun setting-source (name &optional (configuration (current-configuration)))
  "Where the value of the setting NAME came from: :session, :environment,
:dotenv, :project, :environment-default, :computed or :default."
  (cdr (lookup name configuration)))

(defun value-before (name configuration)
  (let ((entry (gethash name (configuration-resolved configuration))))
    (if entry
        (car entry)
        (handler-case (setting name configuration)
          (settei-error () nil)))))

(defun (setf setting) (value name &optional (configuration (current-configuration)))
  "Set the setting NAME for this run: the value is checked, and the
setting's listeners called when it changes. Returns the value as the setting
took it."
  (let* ((setting (find-setting name))
         (accepted (handler-case (accept-value setting value configuration)
                     (setting-invalid (condition)
                       (error (condition-class 'setting-invalid) :name name :value value :source :session
                                               :message (settei-error-message condition)))))
         (before (value-before name configuration)))
    (setf (gethash name (configuration-sessions configuration)) accepted
          (gethash name (configuration-resolved configuration)) (cons accepted :session))
    (forget-derived configuration)
    (notify configuration name before accepted)
    accepted))

(defun setting-unset (name &optional (configuration (current-configuration)))
  "Forget the value set in this run for NAME: it takes the value its other
sources give. Returns that value."
  (find-setting name)
  (let ((before (value-before name configuration)))
    (remhash name (configuration-sessions configuration))
    (remhash name (configuration-resolved configuration))
    (forget-derived configuration)
    (let ((after (setting name configuration)))
      (notify configuration name before after)
      after)))

;;; The project's values

(defun forget-value (name configuration)
  "Forget NAME's value in CONFIGURATION unless it was set in this run."
  (let ((entry (gethash name (configuration-resolved configuration))))
    (when (and entry (not (eq (cdr entry) :session)))
      (remhash name (configuration-resolved configuration)))))

(defun group-name (group key)
  (intern (concatenate 'string (symbol-name group) "/" (symbol-name key)) :keyword))

(defun configured-pairs (specification)
  "SPECIFICATION, CONFIGURE's arguments, as a list of (name value): names and
values, and groups, a module and its settings' short names and values."
  (flet ((plist-pairs (plist context)
           (unless (and (evenp (length plist)) (every #'keywordp (loop for (key) on plist by #'cddr collect key)))
             (error "CONFIGURE takes names and values: ~s" context))
           (loop for (name value) on plist by #'cddr collect (list name value))))
    (loop with rest = specification
          while rest
          append (let ((item (pop rest)))
                   (cond ((and (consp item) (keywordp (first item)))
                          (mapcar (lambda (pair) (list (group-name (first item) (first pair)) (second pair)))
                                  (plist-pairs (rest item) item)))
                         ((and (keywordp item) rest)
                          (list (list item (pop rest))))
                         (t (error "CONFIGURE takes names and values, or groups: ~s" item)))))))

(defmacro configure (&body specification)
  "The project's values for settings, as a settings file gives them. Names
and values, or groups of a module and its settings' short names:

  (configure
    (:server :host \"0.0.0.0\" :port 5000)
    (:magazine :desk-enabled (per-environment :development t :otherwise nil))
    :shiso/site-origin \"https://almightylisp.com\")

Names are written as they are; values are evaluated, and may be a
PER-ENVIRONMENT, or a function of the configuration, called when the
settings are loaded: for a value that depends on the machine, such as a
path under the user's home, which a value written here would fix when the
file is compiled. The settings needn't be defined yet, so the file may load
before the modules that define them: LOAD-SETTINGS reports a name that is
still not a setting then, and a secret, which is given in an environment
variable, never in a file that may be committed. See ADD-PROJECT-VALUES."
  `(add-project-values
    (list ,@(loop for (name form) in (configured-pairs specification)
                  append (list name form)))))

(defun add-project-values (names-and-values)
  "Add NAMES-AND-VALUES, a plist of settings' full names and values, to the
project's values: CONFIGURE, for values made as the program runs.
Configuring a secret already defined is an error at once. When the settings
are loaded already, the values are read again. Returns the names."
  (let* ((pairs (configured-pairs names-and-values))
         (names (mapcar #'first pairs)))
    (dolist (name names)
      (let ((setting (find-setting name :errorp nil)))
        (when (and setting (setting-secret-p setting))
          (error (condition-class 'secret-configured) :name name))))
    (loop for (name value) in pairs
          do (setf (gethash name (registry-project-values *registry*)) value))
    (when *configuration*
      (dolist (name names)
        (forget-value name *configuration*))
      (forget-derived *configuration*))
    names))

(defun configuration-problems ()
  "What is wrong with the project's values themselves: names that are not
settings, and secrets."
  (let ((problems nil))
    (maphash (lambda (name value)
               (declare (ignore value))
               (let ((setting (find-setting name :errorp nil)))
                 (cond ((null setting)
                        (push (make-problem name "is configured, but there is no such setting: is it misspelt, or is its module not loaded?")
                              problems))
                       ((setting-secret-p setting)
                        (push (make-problem name (format nil "is a secret, so it can't be configured: set ~{~a~^ or ~} in the environment"
                                                         (setting-environment-variables setting)))
                              problems)))))
             (registry-project-values *registry*))
    (sort problems #'string< :key (lambda (problem) (symbol-name (problem-name problem))))))

(defun forget-definition (name)
  "When NAME is defined again, its value is read again under the new
definition, unless it was set in this run."
  (when *configuration*
    (forget-value name *configuration*)
    (forget-derived *configuration*)))

(pushnew 'forget-definition *setting-defined-hooks*)

;;; Listeners

(defun on-setting-change (name function)
  "Call FUNCTION with the old and new values whenever the setting NAME's value
changes: set in the REPL, unset, or different when the settings are loaded
again. Returns FUNCTION, for REMOVE-SETTING-LISTENER."
  (find-setting name)
  (pushnew function (gethash name (registry-listeners *registry*)))
  function)

(defun remove-setting-listener (name function)
  (setf (gethash name (registry-listeners *registry*))
        (remove function (gethash name (registry-listeners *registry*))))
  function)

(defun notify (configuration name before after)
  (when (and (configuration-notify-p configuration) (not (equal before after)))
    (dolist (listener (gethash name (registry-listeners *registry*)))
      (funcall listener before after))))

;;; Loading

(defun describe-source (source &optional variable)
  "Where a value came from, said for a person."
  (ecase source
    (:session "this session")
    (:environment variable)
    (:dotenv (format nil "~a in .env" variable))
    (:project "the project's settings")
    (:environment-default "the default for this environment")
    ((:computed :default) "the default")))

(defun problem-of (condition)
  (make-problem (setting-invalid-name condition)
                (format nil "~s from ~a ~a"
                        (setting-invalid-value condition)
                        (describe-source (setting-invalid-source condition)
                                         (setting-invalid-variable condition))
                        (settei-error-message condition))))

(defun use-default (name configuration)
  "Give NAME its default in CONFIGURATION, or NIL when that is refused too."
  (let ((setting (find-setting name)))
    (remhash name (configuration-sessions configuration))
    (setf (gethash name (configuration-resolved configuration))
          (handler-case (multiple-value-bind (value source) (default-value setting configuration)
                          (cons (accept-value setting value configuration) source))
            (setting-invalid () (cons nil :default))))))

(defun signal-problems (configuration problems environment)
  (restart-case (error (condition-class 'settings-invalid) :problems problems :environment environment)
    (continue ()
      :report "Load the settings anyway, with the defaults of those that are wrong."
      ;; Only a value that is refused is replaced. A name that is no setting,
      ;; or a configured secret, is left: its configured value isn't used.
      (dolist (problem problems)
        (let ((name (problem-name problem)))
          (when (find-setting name :errorp nil)
            (handler-case (setting name configuration)
              (setting-invalid ()
                (use-default name configuration)))))))))

(defun check-configuration (configuration)
  "Read every setting in CONFIGURATION, and signal SETTINGS-INVALID listing
every one that is refused, or required in this environment and not set."
  (handler-case (current-environment configuration)
    (setting-invalid (condition)
      (signal-problems configuration (list (problem-of condition)) nil)))
  (let* ((environment (current-environment configuration))
         (problems
           (loop for setting in (list-settings)
                 for name = (setting-name setting)
                 for problem = (handler-case
                                   (when (and (null (setting name configuration))
                                              (member environment (setting-required-in setting)))
                                     (make-problem
                                      name (format nil "is required in ~(~a~), and not set (~{~a~^ or ~})"
                                                   environment
                                                   (setting-environment-variables setting))))
                                 (setting-invalid (condition)
                                   (problem-of condition)))
                 when problem
                   collect problem)))
    (setf problems (append problems (configuration-problems)))
    (when problems
      (signal-problems configuration problems environment))))

(defun install (configuration)
  "Make CONFIGURATION the program's, and call the listeners of settings whose
values changed from the one before."
  (let ((before *configuration*))
    (setf *configuration* configuration)
    (when before
      (maphash (lambda (name entry)
                 (when (find-setting name :errorp nil)
                   (notify configuration name (car entry)
                           (handler-case (setting name configuration)
                             (settei-error () nil)))))
               (configuration-resolved before)))
    configuration))

(defun default-dotenv ()
  (merge-pathnames ".env" (uiop:getcwd)))

(defun load-settings (&key (environment *default-environment*) (dotenv (default-dotenv))
                           (getenv #'uiop:getenv) overrides)
  "Read every setting's value, check them all, and make them the program's.
Call it when the program starts.

ENVIRONMENT is the environment when the environment setting isn't set
(see DEFINE-ENVIRONMENT-SETTING). DOTENV is a .env
file, read when it exists; its variables, an alist; or NIL. GETENV is a function of an environment
variable's name. OVERRIDES is a plist of values for this run, as if set with
(SETF SETTING).

Signals SETTINGS-INVALID listing every setting that is wrong, with a
CONTINUE restart that uses their defaults. Returns the configuration."
  (let ((configuration (make-instance 'configuration :default-environment environment
                                                     :getenv getenv
                                                     :dotenv (if (listp dotenv)
                                                                 dotenv
                                                                 (read-dotenv dotenv)))))
    (loop for (name value) on overrides by #'cddr
          do (find-setting name)
             (setf (gethash name (configuration-sessions configuration)) value))
    ;; A default computed from other settings may read them without passing
    ;; the configuration.
    (let ((*configuration* configuration))
      (check-configuration configuration))
    (install configuration)))

(defun session-plist (configuration)
  (let ((plist nil))
    (maphash (lambda (name value) (push value plist) (push name plist))
             (configuration-sessions configuration))
    plist))

(defun reload-settings (&rest arguments &key &allow-other-keys)
  "LOAD-SETTINGS again, with the same environment and the values set in this
session; ARGUMENTS, LOAD-SETTINGS's, take precedence. The environment
variables and the .env file are read again."
  (let ((before *configuration*))
    (apply #'load-settings
           (append arguments
                   (when before
                     (list :environment (configuration-default-environment before)
                           :getenv (configuration-getenv before)
                           :overrides (session-plist before)))))))

(defun forget-settings ()
  "Forget the loaded settings."
  (setf *configuration* nil))

(defun settings-loaded-p ()
  (and *configuration* t))

(defun forget-on-restore ()
  "A saved image starting: the settings loaded when it was saved are
forgotten, and the environment is :production unless its setting says
otherwise. A built program reads the machine it runs on."
  (setf *configuration* nil
        *default-environment* :production))

(uiop:register-image-restore-hook 'forget-on-restore nil)

;;; Settings for a while

(defmacro with-settings ((&rest names-and-values) &body body)
  "Run BODY with these settings' values, in this thread only:

  (with-settings (:magazine/desk-enabled t)
    (request \"/og-preview\"))

For tests that call the program in their own thread: a server's threads see
the program's settings. Listeners aren't called."
  `(let ((*configuration* (copy-configuration (current-configuration) :notify-p nil)))
     ,@(loop for (name value) on names-and-values by #'cddr
             collect `(setf (setting ,name) ,value))
     ,@body))
