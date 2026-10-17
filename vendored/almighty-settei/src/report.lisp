(defpackage #:almighty-settei/report
  (:use #:cl #:almighty-settei/conditions #:almighty-settei/kinds
        #:almighty-settei/registry #:almighty-settei/configuration)
  (:export #:settings-report #:print-settings #:secret-values #:redact #:*mask*))

(in-package #:almighty-settei/report)

;;;; Showing the settings: every one's value, where it came from, and what it
;;;; does, for a command or a settings page. Secrets are masked, here and in
;;;; any text REDACT is given.

(defparameter *mask* "••••"
  "What a secret's value is shown as.")

(defun defaulted-p (source)
  (member source '(:default :computed :environment-default)))

(defun settings-report (&key (configuration (current-configuration)) changed)
  "Every setting, as a plist: :NAME :LABEL :GROUP :VALUE (as shown, secrets
masked) :SOURCE :VARIABLES :DOCUMENTATION :SECRET, and :PROBLEM when its
value is refused. With CHANGED, only those not at their defaults."
  (loop for setting in (list-settings)
        for name = (setting-name setting)
        for entry = (handler-case (multiple-value-list
                                   (values (setting name configuration)
                                           (setting-source name configuration)))
                      (setting-invalid (condition) condition))
        for problem = (and (typep entry 'condition) (settei-error-message entry))
        for (value source) = (if problem
                                 (list nil (setting-invalid-source entry))
                                 entry)
        unless (and changed (not problem) (defaulted-p source))
          collect (list :name name
                        :label (setting-label setting)
                        :group (setting-group setting)
                        :value (cond (problem (format nil "~s" (setting-invalid-value entry)))
                                     ((and (setting-secret-p setting) value) *mask*)
                                     (t (render-value setting value)))
                        :source source
                        :variables (setting-environment-variables setting)
                        :documentation (setting-documentation setting)
                        :secret (setting-secret-p setting)
                        :problem problem)))

(defun first-paragraph (text)
  "TEXT up to its first blank line, its lines joined."
  (when text
    (let ((end (search (format nil "~%~%") text)))
      (substitute #\Space #\Newline (string-trim '(#\Space #\Newline) (subseq text 0 end))))))

(defun source-word (source)
  "SOURCE in a word, for a column: a default is a default, however it was made."
  (case source
    ((:default :computed :environment-default) "default")
    (:dotenv ".env")
    ((nil) "")
    (t (string-downcase (symbol-name source)))))

(defun name-text (name)
  (string-downcase (prin1-to-string name)))

(defun column-width (report function &optional (limit 40))
  (min limit (reduce #'max report :key (lambda (row) (length (funcall function row)))
                                  :initial-value 0)))

(defun print-settings (&key (stream *standard-output*) (configuration (current-configuration))
                            changed)
  "Print the settings, grouped, each with its value, its source, its
environment variables and the first paragraph of its documentation:

  server
    :server/port  5000  default  SHISO_SERVER_PORT
                  The port the server listens on."
  (let* ((report (settings-report :configuration configuration :changed changed))
         (name-width (column-width report (lambda (row) (name-text (getf row :name)))))
         (value-width (column-width report (lambda (row) (getf row :value))))
         (source-width (column-width report (lambda (row) (source-word (getf row :source)))))
         (groups (remove-duplicates (mapcar (lambda (row) (getf row :group)) report)
                                    :test #'string= :from-end t)))
    (format stream "Environment: ~(~a~)~%" (current-environment configuration))
    (dolist (group groups)
      (format stream "~%~a~%" group)
      (dolist (row report)
        (when (string= group (getf row :group))
          (format stream "  ~va  ~va  ~va  ~{~a~^ or ~}~%"
                  name-width (name-text (getf row :name))
                  value-width (getf row :value)
                  source-width (source-word (getf row :source))
                  (getf row :variables))
          (let ((indent (make-string (+ 4 name-width) :initial-element #\Space)))
            (when (getf row :problem)
              (format stream "~a! ~a~%" indent (getf row :problem)))
            (let ((line (first-paragraph (getf row :documentation))))
              (when line
                (format stream "~a~a~%" indent line)))))))
    (values)))

(defun secret-values (&optional (configuration *configuration*))
  "The values of the secrets that are set, as strings."
  (when configuration
    (loop for setting in (list-settings)
          when (setting-secret-p setting)
            append (let ((value (handler-case (setting (setting-name setting) configuration)
                                  (settei-error () nil))))
                     (when value
                       (list (if (stringp value) value (princ-to-string value))))))))

(defun redact (text &optional (configuration *configuration*))
  "TEXT with every secret's value in it replaced by *MASK*, for an error page
or a log. Values shorter than four characters are left: they would mask too
much else."
  (let ((secrets (sort (remove-if (lambda (secret) (< (length secret) 4))
                                  (secret-values configuration))
                       #'> :key #'length)))
    (dolist (secret secrets text)
      (setf text (with-output-to-string (out)
                   (loop with start = 0
                         for found = (search secret text :start2 start)
                         do (write-string text out :start start :end found)
                            (when found
                              (write-string *mask* out))
                         while found
                         do (setf start (+ found (length secret)))))))))
