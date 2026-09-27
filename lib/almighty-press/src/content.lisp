(defpackage #:almighty-press/content
  (:use #:cl)
  (:export #:org-files #:load-files
           #:collection #:make-collection #:collection-items #:take-changes))

(in-package #:almighty-press/content)

;;;; Content collections.
;;;
;;; A collection is a set of directories and a function that loads them. Its
;;; items are loaded on first use and again whenever a file in one of the
;;; directories is added, removed or saved, so a running server always shows
;;; what is on disk. TAKE-CHANGES says, once, that a reload happened: the cue
;;; to write the static pages again.

(defun org-files (directory &key (include (constantly t)))
  "The .org files directly in DIRECTORY, sorted by name, for which INCLUDE is true."
  (sort (remove-if-not include
                       (directory (merge-pathnames "*.org" (uiop:ensure-directory-pathname directory))))
        #'string<
        :key #'namestring))

(defun load-files (files function)
  "FUNCTION called on each of FILES, collected. An error while loading one
names the file."
  (mapcar (lambda (file)
            (handler-case (funcall function file)
              (error (condition)
                (error "Cannot read ~a: ~a" (uiop:native-namestring file) condition))))
          files))

(defstruct (collection (:constructor make-collection (&key directories load)))
  "DIRECTORIES are watched; LOAD is called with no arguments to read them."
  directories load %items stamp changed)

(defun stamp (directories)
  (loop for directory in directories
        for files = (org-files directory)
        collect (mapcar #'namestring files)
        collect (mapcar #'file-write-date files)))

(defun collection-items (collection)
  "What LOAD returned, loaded again first when the files have changed."
  (let ((stamp (stamp (collection-directories collection))))
    (unless (equal stamp (collection-stamp collection))
      (setf (collection-%items collection) (funcall (collection-load collection))
            (collection-stamp collection) stamp
            (collection-changed collection) t))
    (collection-%items collection)))

(defun take-changes (collection)
  "True once after each reload, then false until the next one."
  (collection-items collection)
  (shiftf (collection-changed collection) nil))
