(defpackage #:almighty-settei/conditions
  (:use #:cl)
  (:export #:settei-error #:settei-error-message
           #:setting-unknown #:setting-unknown-name
           #:setting-invalid #:setting-invalid-name #:setting-invalid-value
           #:setting-invalid-source #:setting-invalid-variable
           #:problem #:make-problem #:problem-name #:problem-message
           #:settings-invalid #:settings-invalid-problems #:settings-invalid-environment
           #:secret-configured #:secret-configured-name
           #:settings-not-loaded
           #:unknown-setting-warning #:unknown-setting-warning-name
           #:condition-class #:use-condition-class))

(in-package #:almighty-settei/conditions)

;;;; What goes wrong, and how it is said.
;;;
;;; A framework may have its own subclass of each condition signalled in its
;;; place (USE-CONDITION-CLASS), so that its users, and the debugger, see the
;;; framework's names. A handler for the class here catches it all the same.

(defvar *condition-classes* (make-hash-table :test 'eq)
  "A condition class's name to the class signalled in its place.")

(defun condition-class (name)
  "The class to signal for the condition NAME: NAME, or the framework's
subclass of it."
  (gethash name *condition-classes* name))

(defun use-condition-class (name class)
  "Signal CLASS wherever the condition NAME is signalled. CLASS must be a
subclass of NAME:

  (define-condition settings-invalid (settei:settings-invalid) ())
  (settei:use-condition-class 'settei:settings-invalid 'settings-invalid)"
  (unless (subtypep class name)
    (error "~s is not a subclass of ~s." class name))
  (setf (gethash name *condition-classes*) class))

(define-condition settei-error (error)
  ((message :initarg :message :initform nil :reader settei-error-message))
  (:report (lambda (condition stream)
             (write-string (or (settei-error-message condition) "Settings error.") stream))))

(define-condition setting-unknown (settei-error)
  ((name :initarg :name :reader setting-unknown-name))
  (:report (lambda (condition stream)
             (format stream "There is no setting named ~s." (setting-unknown-name condition)))))

(define-condition setting-invalid (settei-error)
  ((name :initarg :name :reader setting-invalid-name)
   (value :initarg :value :reader setting-invalid-value)
   ;; Where the value came from (see SETTING-SOURCE), and the environment
   ;; variable when it came from one. NIL until the value's source is known.
   (source :initarg :source :initform nil :reader setting-invalid-source)
   (variable :initarg :variable :initform nil :reader setting-invalid-variable))
  (:report (lambda (condition stream)
             (format stream "~s: ~s ~a"
                     (setting-invalid-name condition)
                     (setting-invalid-value condition)
                     (settei-error-message condition)))))

(defstruct (problem (:constructor make-problem (name message)))
  "One setting that is wrong: its NAME, and MESSAGE, what is wrong with it."
  name message)

(define-condition settings-invalid (settei-error)
  ((problems :initarg :problems :reader settings-invalid-problems)
   (environment :initarg :environment :initform nil :reader settings-invalid-environment))
  (:report (lambda (condition stream)
             (let* ((problems (settings-invalid-problems condition))
                    (names (mapcar (lambda (problem)
                                     (string-downcase (prin1-to-string (problem-name problem))))
                                   problems))
                    (width (reduce #'max names :key #'length :initial-value 0)))
               (format stream "~d setting~:p ~:*~[are~;is~:;are~] wrong~@[ for ~(~a~)~]:~%"
                       (length problems) (settings-invalid-environment condition))
               (loop for problem in problems
                     for name in names
                     do (format stream "~%  ~va  ~a" width name (problem-message problem)))))))

(define-condition secret-configured (settei-error)
  ((name :initarg :name :reader secret-configured-name))
  (:report (lambda (condition stream)
             (format stream "~s is a secret: give it in an environment variable, ~
                             not in a file that may be committed."
                     (secret-configured-name condition)))))

(define-condition settings-not-loaded (settei-error)
  ()
  (:report "The settings aren't loaded: call LOAD-SETTINGS when the program starts."))

(define-condition unknown-setting-warning (style-warning)
  ((name :initarg :name :reader unknown-setting-warning-name))
  (:report (lambda (condition stream)
             (format stream "~s is not a defined setting (yet)."
                     (unknown-setting-warning-name condition)))))
