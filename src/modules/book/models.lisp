(defpackage #:book/models
  (:use #:cl)
  (:local-nicknames (#:validators #:shiso/validators))
  (:export #:book
           #:book-title
           #:book-description
           #:book-status
           #:book-published-at))

(in-package #:book/models)

;; (shiso:define-model book
;;     ((title :initarg :title
;;        :col-type (:varchar 200)
;;        :verbose-name "Book Title"
;;        :help-text "The full title of the book"
;;        :validators ((validators:max-length 200)))
;;      (description :initarg :description
;;                   :col-type :text
;;                   :verbose-name "Description"
;;                   :blankp t
;;                   :widget :textarea)
;;      (status :initarg :status
;;              :col-type (:varchar 20)
;;              :choices ((:draft . "Draft")
;;                        (:published . "Published")
;;                        (:archived . "Archived"))
;;              :initform "draft")
;;      ;; If I add this, the column
;;      (published-at :initarg :published-at
;;                    :col-type :timestamptz
;;                    :initform (local-time:now))))

(shiso:define-model book
    ((title :field-type :char :max-length 200
            :verbose-name "Book Title"
            :help-text "The full title of the book")
     (description :field-type :text
                  :verbose-name "Description"
                  :blankp t)
     (status :field-type :choice
             :choices ((:draft . "Draft")
                       (:published . "Published")
                       (:archived . "Archived"))
             :default :draft)
     (published-at :field-type :datetime
                   :initform (local-time:now))))

#+nil
(let ((books (mito:select-by-sql 'book (sxql:select (:book.*) (sxql:from :book)))))
  (book-published-on (first books)))
                                        ; => @2026-03-04T06:44:35.398779+09:00
#+nil
(let ((books (mito:select-dao 'book)))
  (book-published-on (first books)))
;; The slot BOOKS/MODELS::DATE is unbound in the object
;; #<BOOK {7005848143}>.
;; [Condition of type UNBOUND-SLOT]


#+nil
(setf mito:*auto-migration-mode* t)

#+nil
(mito:migrate-table 'book)

(shiso/admin:define-admin book)
