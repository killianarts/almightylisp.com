(defpackage #:book/convert
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html))
  (:export #:convert-html-directory))

(in-package #:book/convert)

(defun convert-html-directory (directory)
  "Convert all .html files in DIRECTORY to .lisp files using almighty-html.
Each file foo.html produces a corresponding foo.lisp in the same directory."
  (let ((html-files (directory (merge-pathnames "*.html" (uiop:ensure-directory-pathname directory)))))
    (dolist (html-file html-files)
      (let ((lisp-file (make-pathname :type "lisp" :defaults html-file)))
        (format t "Converting ~A -> ~A~%" (file-namestring html-file) (file-namestring lisp-file))
        (ah:convert-html-to-almighty html-file lisp-file)))
    (format t "Converted ~D file~:P.~%" (length html-files))))
