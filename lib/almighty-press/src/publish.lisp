(defpackage #:almighty-press/publish
  (:use #:cl)
  (:export #:write-page #:publish-pages))

(in-package #:almighty-press/publish)

(defun write-page (directory name html)
  "Write HTML to DIRECTORY/NAME.html and return its path."
  (let ((path (merge-pathnames (make-pathname :name name :type "html")
                               (uiop:ensure-directory-pathname directory))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string html out))
    path))

(defun publish-pages (directory pages)
  "Write PAGES, a list of (name html), to DIRECTORY, and delete any other
.html file there, such as the page of an article that has been removed.
Returns DIRECTORY."
  (dolist (page pages)
    (write-page directory (first page) (second page)))
  (dolist (old (directory (merge-pathnames "*.html" (uiop:ensure-directory-pathname directory))))
    (unless (member (pathname-name old) pages :key #'first :test #'string=)
      (delete-file old)))
  directory)
