(uiop:define-package #:book
  (:use #:cl)
  (:local-nicknames (#:s #:shiso))
  (:use-reexport #:book/controllers
                 #:book/forms
                 #:book/models
                 #:book/routes))

(in-package #:book)

#+nil
(mito:select-dao (shiso/models:model-class 'book))
