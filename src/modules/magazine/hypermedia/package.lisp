(defpackage #:magazine/hypermedia/components
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html)
                    (#:s #:shiso)
                    (#:model #:magazine/model)
                    (#:intel #:magazine/intel)
                    (#:irc #:magazine/irc))
  (:export #:page-home #:page-archive #:page-intel #:page-intel-desk #:irc-lines-html #:page-archive-lab #:page-docs-lab #:page-article #:page-not-found
           #:render-search-results))

(in-package #:magazine/hypermedia/components)

;;;; Links, assets and text shared by every page.
;;;
;;; The magazine is one package split over these files:
;;;
;;;   package.lisp    links, assets, small text helpers
;;;   sheet.lisp      the document and the sheet every page is set on: bands,
;;;                   gutters and side labels, running header, the footer
;;;   logos.lisp      series logos and stamps
;;;   cards.lisp      briefs, the book panel, series cards and progress
;;;   blocks.lisp     an article's body: prose, code, tables, margin notes
;;;   evolving.lisp   an evolving article's body
;;;   home.lisp       the homepage
;;;   article.lisp    the article page
;;;   archive.lisp    the archive and the not-found page
;;;   search.lisp     search: the palette every page carries, and its results

(defun magazine-href ()
  (s:url "magazine:index"))

(defun archive-href (&key month type series open)
  "The archive. TYPE and SERIES, a series' code, keep only those articles.
OPEN is a month to show open, by its id, or \"all\". With MONTH, a date, the
link goes to that month, open."
  (let* ((id (when month (press:format-date month :month-id)))
         (query (loop for (name value) in (list (list "type" type)
                                                (list "series" series)
                                                (list "open" (or open id)))
                      when value
                        collect (format nil "~a=~a" name (quri:url-encode value)))))
    (format nil "~a~@[?~{~a~^&~}~]~@[#~a~]" (s:url "magazine:archive") query id)))

(defun search-href (&key query field selected focus wide)
  "Where the search palette asks for its results: for QUERY in FIELD, a
field's name, with article SELECTED chosen and match FOCUS of it in focus,
WIDE to show more of the text around that match. What is left out is as a
new search has it."
  (let ((query (loop for (name value) in (list (list "q" query)
                                               (list "field" field)
                                               (list "sel" (and selected (plusp selected) selected))
                                               (list "focus" (and focus (plusp focus) focus))
                                               (list "wide" (and wide 1)))
                     when value
                       collect (format nil "~a=~a" name (quri:url-encode (princ-to-string value))))))
    (format nil "~a~@[?~{~a~^&~}~]" (s:url "magazine:search") query)))

(defun slug-href (slug)
  (s:url "magazine:article" :slug slug))

(defun article-href (article)
  (slug-href (model:article-slug article)))

(defparameter *book-href* "/book/essentials"
  "The book's first page. Written out: Shiso's URL for another module is only
right once that module has served a request.")

(defparameter *hardcover-href* "https://www.amazon.com/dp/B0HL586JWT"
  "Where Purchase hardcover goes: the print edition's store page.")

(defparameter *ebook-href*
  "https://almightylisp.gumroad.com/l/almighty-lisp-lisp-and-emacs-essentials-ebook"
  "Where Purchase ebook goes: the PDF's store page.")

(defparameter *theme-packs*
  '(("Theme Pack 1" "Lisp ■ Titan ■ Gear ■ Macro"
     "https://almightylisp.gumroad.com/l/almighty-themes-pack-1")
    ("Theme Pack 2" "Ena ■ Mikasa ■ Lisp-X ■ Saga"
     "https://almightylisp.gumroad.com/l/almighty-themes-pack-2"))
  "Name, themes and store page of each Doom Emacs theme pack.")

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

(defun slashed (&rest parts)
  "PARTS set apart with a slash, as an eyebrow sets its facts."
  (format nil "~{~a~^ / ~}" (remove nil parts)))

(defun two-digits (n)
  (format nil "~2,'0d" n))

(defun part-of (article series)
  (format nil "Part ~d of ~d" (model:article-part article) (model:series-total series)))

(defun part-fraction (article series)
  (format nil "Part ~d/~d" (model:article-part article) (model:series-total series)))

(defun date-text (date)
  "A date as labels show it: 24 Sep 2026."
  (press:format-date date :compact))
