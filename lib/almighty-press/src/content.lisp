(defpackage #:almighty-press/content
  (:use #:cl)
  (:export #:org-files #:load-files
           #:collection #:make-collection #:collection-items #:take-changes))

(in-package #:almighty-press/content)

;;;; Content collections.
;;;
;;; A collection is a set of directories and a function that loads them. Its
;;; items are loaded on first use and again whenever a file in one of the
;;; directories (or, with :RECURSIVE, their subdirectories) is added, removed
;;; or saved, so a running server always shows what is on disk. TAKE-CHANGES says, once, that a reload happened: the cue
;;; to write the static pages again.

(defun org-files (directory &key (include (constantly t)) recursive)
  "The .org files in DIRECTORY, sorted by name, for which INCLUDE is true.
With RECURSIVE, those in its subdirectories too."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (sort (remove-if-not include
                         (append (directory (merge-pathnames "*.org" directory))
                                 (when recursive
                                   (mapcan (lambda (sub) (org-files sub :recursive t))
                                           (uiop:subdirectories directory)))))
          #'string<
          :key #'namestring)))

(defun load-files (files function)
  "FUNCTION called on each of FILES, collected. An error while loading one
names the file."
  (mapcar (lambda (file)
            (handler-case (funcall function file)
              (error (condition)
                (error "Cannot read ~a: ~a" (uiop:native-namestring file) condition))))
          files))

(defstruct (collection (:constructor make-collection (&key directories recursive load)))
  "DIRECTORIES are watched, and with RECURSIVE their subdirectories; LOAD is
called with no arguments to read them."
  directories recursive load %items stamp changed)

(defun stamp (directories recursive)
  (loop for directory in directories
        for files = (org-files directory :recursive recursive)
        collect (mapcar #'namestring files)
        collect (mapcar #'file-write-date files)))

(defun collection-items (collection)
  "What LOAD returned, loaded again first when the files have changed."
  (let ((stamp (stamp (collection-directories collection) (collection-recursive collection))))
    (unless (equal stamp (collection-stamp collection))
      (setf (collection-%items collection) (funcall (collection-load collection))
            (collection-stamp collection) stamp
            (collection-changed collection) t))
    (collection-%items collection)))

(defun take-changes (collection)
  "True once after each reload, then false until the next one."
  (collection-items collection)
  (shiftf (collection-changed collection) nil))
