(defpackage #:almighty-settei/dotenv
  (:use #:cl)
  (:export #:parse-dotenv #:read-dotenv))

(in-package #:almighty-settei/dotenv)

;;;; .env files: environment variables written in a file, for development.
;;;
;;;   # A comment
;;;   INTEL_DESK=true
;;;   export SHISO_SITE_ORIGIN=http://localhost:5000
;;;   GREETING="Hello, \"world\"\n"   # escapes inside double quotes
;;;   LITERAL='kept $as is'
;;;
;;; An unquoted value ends at " #". A line that isn't NAME=VALUE is skipped.

(defun unquote (value)
  "VALUE without its quotes: inside double quotes, \\n, \\t, \\\" and \\\\ are
read; inside single quotes, nothing is."
  (let ((mark (char value 0))
        (end (position (char value 0) value :start 1 :from-end t)))
    (cond ((null end) value)
          ((char= mark #\')
           (subseq value 1 end))
          (t
           (with-output-to-string (out)
             (loop with i = 1
                   while (< i end)
                   do (let ((char (char value i)))
                        (if (and (char= char #\\) (< (1+ i) end))
                            (progn
                              (write-char (case (char value (1+ i))
                                            (#\n #\Newline)
                                            (#\t #\Tab)
                                            (t (char value (1+ i))))
                                          out)
                              (incf i 2))
                            (progn
                              (write-char char out)
                              (incf i))))))))))

(defun parse-line (line)
  "LINE's name and value, or NIL."
  (let* ((line (string-trim '(#\Space #\Tab #\Return) line))
         (line (if (and (> (length line) 7) (string= "export " line :end2 7))
                   (string-left-trim " " (subseq line 7))
                   line))
         (equals (position #\= line)))
    (when (and equals (plusp equals) (char/= (char line 0) #\#))
      (let ((name (string-trim " " (subseq line 0 equals)))
            (value (string-trim " " (subseq line (1+ equals)))))
        (cons name
              (cond ((zerop (length value)) value)
                    ((member (char value 0) '(#\" #\'))
                     (unquote value))
                    (t (let ((comment (search " #" value)))
                         (string-right-trim " " (if comment (subseq value 0 comment) value))))))))))

(defun parse-dotenv (text)
  "The variables TEXT, a .env file's contents, sets: an alist of name and value."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil)
          while line
          for pair = (parse-line line)
          when pair
            collect pair)))

(defun read-dotenv (path)
  "The variables the .env file at PATH sets, or NIL when there is none."
  (when (and path (probe-file path))
    (parse-dotenv (uiop:read-file-string path))))
