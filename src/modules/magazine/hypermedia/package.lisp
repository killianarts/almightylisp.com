(defpackage #:magazine/hypermedia/components
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html)
                    (#:s #:shiso)
                    (#:model #:magazine/model))
  (:export #:page-home #:page-archive #:page-article #:page-not-found))

(in-package #:magazine/hypermedia/components)

;;;; Links, assets and text shared by every page.
;;;
;;; The magazine is one package split over these files:
;;;
;;;   package.lisp    links, assets, small text helpers
;;;   sheet.lisp      the document and the sheet every page is set on: bands,
;;;                   gutters and side labels, running header and footer
;;;   logos.lisp      series logos and stamps
;;;   cards.lisp      briefs, the book panel, series cards and progress
;;;   blocks.lisp     an article's body: prose, code, tables, margin notes
;;;   home.lisp       the homepage
;;;   article.lisp    the article page
;;;   archive.lisp    the archive and the not-found page

(defun magazine-href ()
  (s:url "magazine:index"))

(defun archive-href (&optional month)
  "The archive, or the archive at MONTH, a date in it."
  (let ((href (s:url "magazine:archive")))
    (if month
        (format nil "~a#~a" href (press:format-date month :month-id))
        href)))

(defun slug-href (slug)
  (s:url "magazine:article" :slug slug))

(defun article-href (article)
  (slug-href (model:article-slug article)))

(defparameter *book-href* "/book/essentials"
  "The book's first page. Written out: Shiso's URL for another module is only
right once that module has served a request.")

(defparameter *hardcover-href* "https://www.amazon.com/dp/B0HL586JWT"
  "Where Purchase hardcover goes: the print edition's store page.")

(defun asset (path)
  "The URL of static/PATH, versioned so browsers fetch it again when it changes."
  (s:cached-static path))

(defun image-src (name)
  (asset (concatenate 'string "assets/images/magazine/" name)))

(defun inline-href (target)
  "A link in article text: a bare slug is another article, anything else as written."
  (cond ((zerop (length target)) (magazine-href))
        ((or (char= (char target 0) #\/) (search "://" target)) target)
        (t (slug-href target))))

(defun render-inlines (text)
  (press:render-inlines text :href #'inline-href))

(defun joined (&rest parts)
  "PARTS set apart with the black square that separates facts in a label."
  (format nil "~{~a~^  ■  ~}" (remove nil parts)))

(defun two-digits (n)
  (format nil "~2,'0d" n))

(defun part-of (article series)
  (format nil "Part ~d of ~d" (model:article-part article) (model:series-total series)))

(defun part-fraction (article series)
  (format nil "Part ~d/~d" (model:article-part article) (model:series-total series)))

(defun date-text (date)
  "A date as labels show it: 24 Sep 2026."
  (press:format-date date :compact))
