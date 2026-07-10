(defpackage #:book/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso))
  (:export #:essentials-chapter #:chapter))

(in-package #:book/controllers)
(defparameter *books-directory*
  (asdf:system-relative-pathname "book" "books/"))

(defun read-chapter-file (book-name slug)
  "Read html/<slug>.html from books/<book-name>/html/."
  (let ((path (merge-pathnames
               (make-pathname :directory `(:relative ,book-name "html")
                              :name slug :type "html")
               *books-directory*)))
    (when (probe-file path)
      (uiop:read-file-string path))))

(defun chapter (book-name &optional (slug "index"))
  "Serve a book chapter. Returns the HTML file for book-name/slug."
  (when (or (find #\/ book-name) (search ".." book-name)
            (find #\/ slug) (search ".." slug))
    (return-from chapter (s:http-response "Not found" :code 404)))
  (let ((html (read-chapter-file book-name slug)))
    (if html
        (s:http-response html)
        (s:http-response "Not found" :code 404))))
