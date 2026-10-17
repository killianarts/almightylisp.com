(defpackage #:almighty-settei/kinds
  (:use #:cl #:almighty-settei/conditions)
  (:export #:setting #:setting-name #:setting-documentation #:setting-label #:setting-group
           #:setting-default #:setting-variables #:setting-secret-p #:setting-required-in
           #:setting-validator #:setting-type
           #:boolean-setting #:integer-setting #:setting-minimum #:setting-maximum
           #:string-setting #:pathname-setting #:choice-setting #:setting-options
           #:list-setting
           #:setting-choices #:coerce-value #:check-value #:accept-value #:reject
           #:render-value))

(in-package #:almighty-settei/kinds)

;;;; Settings and their kinds.
;;;
;;; A setting is a named value with a kind: the kind's class says what values
;;; it takes and reads them from strings, as environment variables give them.
;;; ACCEPT-VALUE is the one door a value goes through: COERCE-VALUE turns it
;;; into the kind's type, CHECK-VALUE checks it (bounds, options, the
;;; setting's own validator). Either REJECTs it with a message that completes
;;; "<value> ...", as in "is not an integer".
;;;
;;; NIL is "not set" for every kind but booleans, and is always accepted:
;;; whether a setting must have a value is :REQUIRED-IN's business.

(defclass setting ()
  ((name :initarg :name :reader setting-name
         :documentation "A keyword, module/name: :magazine/desk-enabled.")
   (documentation :initarg :documentation :initform nil :reader setting-documentation)
   (label :initarg :label :initform nil)
   (group :initarg :group :initform nil)
   (default :initarg :default :initform nil :reader setting-default
            :documentation "A value; a function of the configuration; or a PER-ENVIRONMENT.")
   (variables :initarg :environment :initform nil :reader setting-variables
              :documentation "Environment variables that set it, the first set winning.
NIL to derive one from the name.")
   (secret :initarg :secret :initform nil :reader setting-secret-p)
   (required-in :initarg :required-in :initform nil :reader setting-required-in
                :documentation "Environments where it must have a value.")
   (validator :initarg :validator :initform nil :reader setting-validator
              :documentation "A function of a value and the configuration: NIL to
accept it, or a message completing \"<value> ...\".")
   (type :initarg :type :initform t :reader setting-type)))

(defmethod initialize-instance :after ((setting setting) &key)
  (with-slots (name variables required-in) setting
    (check-type name keyword)
    (when (stringp variables)
      (setf variables (list variables)))
    (when (keywordp required-in)
      (setf required-in (list required-in)))))

(defun name-parts (setting)
  "The setting's name, split at its slash: \"magazine\" and \"desk-enabled\"."
  (let* ((name (string-downcase (symbol-name (setting-name setting))))
         (slash (position #\/ name)))
    (if slash
        (values (subseq name 0 slash) (subseq name (1+ slash)))
        (values nil name))))

(defun setting-group (setting)
  "What the setting is shown under: its :GROUP, or the module its name names."
  (or (slot-value setting 'group)
      (nth-value 0 (name-parts setting))
      "general"))

(defun setting-label (setting)
  "A name for people: its :LABEL, or its name's words, the first capitalised."
  (or (slot-value setting 'label)
      (let ((words (substitute #\Space #\- (nth-value 1 (name-parts setting)))))
        (if (plusp (length words))
            (concatenate 'string (string-upcase (subseq words 0 1)) (subseq words 1))
            words))))

(defclass boolean-setting (setting) ())

(defclass integer-setting (setting)
  ((minimum :initarg :minimum :initform nil :reader setting-minimum)
   (maximum :initarg :maximum :initform nil :reader setting-maximum)))

(defclass string-setting (setting) ())

(defclass pathname-setting (setting) ())

(defclass choice-setting (setting)
  ((options :initarg :options :initform nil :reader setting-options
            :documentation "The values it may take: a list, or a function of the
configuration returning one.")))

(defclass list-setting (setting) ()
  (:documentation "A list of strings. From a string, the parts between its commas."))

;;; Checking

(defun reject (setting value control &rest arguments)
  "Refuse VALUE for SETTING: signal SETTING-INVALID with the message."
  (error (condition-class 'setting-invalid) :name (setting-name setting) :value value
                          :message (apply #'format nil control arguments)))

(defun setting-choices (setting configuration)
  (let ((options (setting-options setting)))
    (if (functionp options) (funcall options configuration) options)))

(defun blank-string-p (value)
  (and (stringp value) (zerop (length (string-trim " " value)))))

(defgeneric coerce-value (setting value configuration)
  (:documentation "VALUE as SETTING's kind takes it, read from a string when it is
one. REJECTs a value it can't take."))

(defgeneric check-value (setting value configuration)
  (:documentation "REJECT VALUE, already coerced, when SETTING refuses it.")
  (:method ((setting setting) value configuration)
    (let ((validator (setting-validator setting)))
      (when (and validator value)
        (let ((message (funcall validator value configuration)))
          (when message
            (reject setting value "~a" message)))))))

(defun accept-value (setting value configuration)
  "VALUE as SETTING takes it, coerced and checked."
  (let ((coerced (coerce-value setting value configuration)))
    (check-value setting coerced configuration)
    coerced))

(defmethod coerce-value ((setting setting) value configuration)
  (declare (ignore configuration))
  (if (or (null value) (typep value (setting-type setting)))
      value
      (reject setting value "is not of type ~s" (setting-type setting))))

(defparameter *true-words* '("true" "yes" "on" "1"))
(defparameter *false-words* '("false" "no" "off" "0"))

(defmethod coerce-value ((setting boolean-setting) value configuration)
  (declare (ignore configuration))
  (cond ((eq value t) t)
        ((null value) nil)
        ((and (stringp value)
              (member (string-trim " " value) *true-words* :test #'string-equal))
         t)
        ((and (stringp value)
              (member (string-trim " " value) *false-words* :test #'string-equal))
         nil)
        (t (reject setting value "is not true or false (~{~a~^, ~})"
                   (append *true-words* *false-words*)))))

(defmethod coerce-value ((setting integer-setting) value configuration)
  (declare (ignore configuration))
  (cond ((or (null value) (integerp value)) value)
        ((blank-string-p value) nil)
        ((stringp value)
         (multiple-value-bind (integer end)
             (parse-integer value :junk-allowed t)
           (if (and integer (= end (length (string-right-trim " " value))))
               integer
               (reject setting value "is not an integer"))))
        (t (reject setting value "is not an integer"))))

(defmethod check-value ((setting integer-setting) value configuration)
  (when value
    (let ((minimum (setting-minimum setting))
          (maximum (setting-maximum setting)))
      (when (and minimum (< value minimum))
        (reject setting value "is less than ~d" minimum))
      (when (and maximum (> value maximum))
        (reject setting value "is more than ~d" maximum))))
  (call-next-method))

(defmethod coerce-value ((setting string-setting) value configuration)
  (declare (ignore configuration))
  (if (or (null value) (stringp value))
      value
      (reject setting value "is not a string")))

(defmethod coerce-value ((setting pathname-setting) value configuration)
  (declare (ignore configuration))
  (cond ((or (null value) (pathnamep value)) value)
        ((blank-string-p value) nil)
        ((stringp value) (uiop:parse-native-namestring value))
        (t (reject setting value "is not a pathname"))))

(defun option-name (option)
  (if (symbolp option) (symbol-name option) (princ-to-string option)))

(defmethod coerce-value ((setting choice-setting) value configuration)
  (let ((options (setting-choices setting configuration)))
    (cond ((null value) nil)
          ((member value options :test #'equal) value)
          ((stringp value)
           (or (find (string-trim " " value) options :key #'option-name :test #'string-equal)
               (reject setting value "is not one of ~{~(~a~)~^, ~}"
                       (mapcar #'option-name options))))
          (t (reject setting value "is not one of ~{~(~a~)~^, ~}"
                     (mapcar #'option-name options))))))

(defmethod coerce-value ((setting list-setting) value configuration)
  (declare (ignore configuration))
  (cond ((null value) nil)
        ((stringp value)
         (loop for start = 0 then (1+ comma)
               for comma = (position #\, value :start start)
               for part = (string-trim " " (subseq value start comma))
               when (plusp (length part))
                 collect part
               while comma))
        ((and (listp value) (every #'stringp value)) value)
        (t (reject setting value "is not a list of strings"))))

;;; Showing

(defgeneric render-value (setting value)
  (:documentation "VALUE as a person reads it, in a list of settings.")
  (:method ((setting setting) value)
    (cond ((null value) "(not set)")
          ((stringp value) value)
          ((pathnamep value) (uiop:native-namestring value))
          ((keywordp value) (string-downcase (prin1-to-string value)))
          (t (prin1-to-string value))))
  (:method ((setting boolean-setting) value)
    (if value "true" "false"))
  (:method ((setting list-setting) value)
    (if value (format nil "~{~a~^, ~}" value) "(not set)")))
