(defpackage #:almighty-settei/registry
  (:use #:cl #:almighty-settei/conditions #:almighty-settei/kinds)
  (:export #:registry #:make-registry #:*registry*
           #:registry-project-values #:registry-listeners
           #:define-setting #:register-setting #:remove-setting #:find-setting #:list-settings
           #:define-builtin-setting #:remove-builtin-setting
           #:known-setting-name-p #:note-setting-name
           #:*environment-variable-prefix* #:setting-environment-variables
           #:*setting-defined-hooks*))

(in-package #:almighty-settei/registry)

;;;; The registry: every setting defined, by name and in the order they were.
;;;
;;; It also holds what is kept about settings apart from any one run: the
;;; project's values (CONFIGURE) and the listeners. A test binds *REGISTRY*
;;; to a fresh one, and nothing it defines or configures outlives it.

(defun make-table ()
  ;; Requests read settings on their own threads while the REPL may write them.
  (make-hash-table :test 'eq #+sbcl :synchronized #+sbcl t))

(defstruct (registry (:constructor %make-registry))
  (settings (make-table))
  ;; Names, newest first.
  (order nil)
  ;; Name to value, from CONFIGURE.
  (project-values (make-table))
  ;; Name to a list of functions, from ON-SETTING-CHANGE.
  (listeners (make-table)))

;; Special from here on; given its value below, once MAKE-REGISTRY can make it.
(defvar *registry*)

(defvar *builtin-settings* nil
  "Settings every registry has, as functions making them, oldest first.")

(defvar *setting-defined-hooks* nil
  "Functions of a setting's name, called when it is defined or redefined.")

(defun register-setting (setting &optional (registry *registry*))
  "Add SETTING to REGISTRY, in place of one of the same name. Returns SETTING."
  (let ((name (setting-name setting)))
    (unless (gethash name (registry-settings registry))
      (push name (registry-order registry)))
    (setf (gethash name (registry-settings registry)) setting)
    (dolist (hook *setting-defined-hooks*)
      (funcall hook name))
    setting))

(defun make-registry (&key (builtins t))
  "A new registry, holding the built-in settings unless BUILTINS is false."
  (let ((registry (%make-registry)))
    (when builtins
      (dolist (make (reverse *builtin-settings*))
        (register-setting (funcall make) registry)))
    registry))

(defvar *registry* (make-registry)
  "Where settings are defined and found.")

(defun remove-setting (name &optional (registry *registry*))
  (remhash name (registry-settings registry))
  (setf (registry-order registry) (remove name (registry-order registry)))
  name)

(defun find-setting (name &key (errorp t) (registry *registry*))
  "The setting NAME. Signals SETTING-UNKNOWN when there is none, unless ERRORP
is false."
  (or (gethash name (registry-settings registry))
      (and errorp (error (condition-class 'setting-unknown) :name name))))

(defun list-settings (&optional (registry *registry*))
  "Every setting, in the order they were first defined."
  (loop for name in (reverse (registry-order registry))
        collect (gethash name (registry-settings registry))))

;;; Names known while compiling, so SETTING's compiler macro knows a setting
;;; defined earlier in the same file.

(defvar *compiled-names* (make-hash-table :test 'eq))

(defun note-setting-name (name)
  (setf (gethash name *compiled-names*) t))

(defun known-setting-name-p (name)
  (or (gethash name *compiled-names*)
      (and (find-setting name :errorp nil) t)))

(defmacro define-setting (name kind &body options)
  "Define the setting NAME, a keyword written module/name, of KIND, a setting
class: BOOLEAN-SETTING, INTEGER-SETTING, STRING-SETTING, PATHNAME-SETTING,
CHOICE-SETTING, LIST-SETTING, or SETTING for any :TYPE. OPTIONS may start
with a documentation string; then:

  :DEFAULT       a value, a function of the configuration, or a PER-ENVIRONMENT
  :ENVIRONMENT   the environment variable, or a list of them, that set it;
                 without, one is derived from NAME
  :SECRET        read only from the environment; masked when shown
  :REQUIRED-IN   environments, or one, where it must have a value
  :VALIDATOR     a function of the value and configuration: NIL, or why not
  :LABEL :GROUP  how a settings page shows it
  :TYPE          for SETTING
  :MINIMUM :MAXIMUM  for INTEGER-SETTING
  :OPTIONS       for CHOICE-SETTING: a list, or a function of the configuration

Defining it again replaces the definition. A value set in this session is
kept; any other is read again, under the new definition."
  (check-type name keyword)
  (let ((kind (if (consp kind) (first kind) kind))
        (documentation (when (stringp (first options)) (first options)))
        (options (if (stringp (first options)) (rest options) options)))
    `(progn
       (eval-when (:compile-toplevel)
         (note-setting-name ,name))
       (register-setting (make-instance ',kind :name ,name
                                               ,@(when documentation
                                                   `(:documentation ,documentation))
                                               ,@options)))))

(defun remove-builtin-setting (name)
  "NAME is no longer in every registry MAKE-REGISTRY makes."
  (setf *builtin-settings*
        (remove name *builtin-settings* :key (lambda (make) (setting-name (funcall make)))))
  name)

(defmacro define-builtin-setting (name kind &body options)
  "DEFINE-SETTING, and the setting is in every registry MAKE-REGISTRY makes."
  `(progn
     (setf *builtin-settings*
           (append (remove ,name *builtin-settings*
                           :key (lambda (make) (setting-name (funcall make))))
                   (list (lambda () (make-instance ',(if (consp kind) (first kind) kind)
                                                   :name ,name
                                                   ,@(if (stringp (first options))
                                                         `(:documentation ,@options)
                                                         options))))))
     (define-setting ,name ,kind ,@options)))

;;; Environment variables

(defvar *environment-variable-prefix* ""
  "Put before the names of environment variables derived from settings' names:
\"SHISO_\" makes :server/port's SHISO_SERVER_PORT.")

(defun derived-variable (setting)
  "The name's variable: :server/port's SERVER_PORT, after the prefix, which a
name starting with it doesn't repeat (:shiso/site-origin's SHISO_SITE_ORIGIN,
not SHISO_SHISO_SITE_ORIGIN)."
  (let ((name (substitute-if #\_ (lambda (char) (member char '(#\/ #\- #\.)))
                             (string-upcase (symbol-name (setting-name setting))))))
    (if (and (plusp (length *environment-variable-prefix*))
             (eql 0 (search *environment-variable-prefix* name)))
        name
        (concatenate 'string *environment-variable-prefix* name))))

(defun setting-environment-variables (setting)
  "The environment variables that set SETTING, the first set winning: its
:ENVIRONMENT, or one derived from its name."
  (or (setting-variables setting)
      (list (derived-variable setting))))
