(defpackage #:magazine/model
  (:use #:cl)
  (:export
   ;; Articles
   #:article #:article-title-lines #:article-title #:article-subtitle #:article-author
   #:article-date #:article-type #:article-topic #:article-code #:article-slug
   #:article-lead #:article-related #:article-book-chapter #:article-book-title
   #:article-book-summary #:article-book-href #:article-sections #:article-draft
   #:article-series #:article-part #:article-pathname
   #:section-title-lines #:section-title #:section-id #:section-blocks
   ;; Series
   #:series #:series-code #:series-title #:series-summary #:series-featured
   #:series-parts #:series-total #:series-published
   #:part-number #:part-title #:part-article
   ;; The loaded magazine
   #:magazine-articles #:magazine-series #:take-changes
   #:find-article #:find-series #:article-series-of
   #:homepage-layout #:group-by-month #:keep-reading-articles
   ;; Parsing, for tests and tools
   #:parse-article #:parse-series #:*article-types* #:*article-topics*
   ;; Places
   #:content-directory #:series-directory #:publish-directory))

(in-package #:magazine/model)

;;;; The magazine's content: articles and series, read from org files.
;;;
;;; content/magazine/WRITING.org is the writers' guide: every keyword, type
;;; and topic, with examples. In short:
;;;
;;; One file in content/magazine/ is one article, and its filename is the slug.
;;; Keywords before the first heading:
;;;
;;;   #+TITLE: The image\\is still\\running     required; \\ breaks the headline
;;;   #+SUBTITLE: the dek under the headline
;;;   #+AUTHOR: Micah Killian
;;;   #+DATE: 2026-09-24                        required, YYYY-MM-DD
;;;   #+TYPE: Opinion                           required, one of *article-types*
;;;   #+TOPIC: Lisp                             required, one of *article-topics*
;;;   #+CODE: CL-OP-26924                       optional; normally generated
;;;   #+SLUG: other-slug                        optional; normally the filename
;;;   #+HOME: lead                              make this the homepage lead
;;;   #+RELATED: slug-one slug-two              first picks for Keep reading
;;;   #+BOOK_CHAPTER: 8                         a book chapter for Keep reading
;;;   #+BOOK_TITLE: The LOOP Macro
;;;   #+BOOK_SUMMARY: dek for the book card
;;;   #+BOOK_HREF: /book/essentials/the-loop-macro
;;;   #+SERIES: CND                             the CODE of a series file
;;;   #+PART: 1                                 required with SERIES
;;;   #+DRAFT: t                                not published
;;;
;;; The body is sections (* headings) of paragraphs, ** subheads, source
;;; blocks with #+RESULTS:, tables, and #+BEGIN_NOTE label ... #+END_NOTE
;;; margin notes; see almighty-press/org for the syntax. Files named with a
;;; leading capital, like WRITING.org, are not articles.
;;;
;;; One file in content/magazine/series/ is one series:
;;;
;;;   #+CODE: CND                               three letters; picks the logo
;;;   #+TITLE: The condition system, bottom up
;;;   #+SUMMARY: one or two sentences for the series card
;;;   #+FEATURED: t                             at most one series; shown first
;;;
;;;   1. Signals are not exceptions             planned parts, in order
;;;   2. Handlers that don't unwind
;;;
;;; An article joins a series with #+SERIES and #+PART. Its own title replaces
;;; the planned one; planned parts with no article yet are shown as queued.

(defstruct article
  title-lines title subtitle author date type topic code slug lead related
  book-chapter book-title book-summary book-href
  sections draft pathname series part)

(defstruct series
  code title summary featured planned
  ;; Filled in by LINK-SERIES once the articles are loaded: a list of PART
  ;; structs, how many parts there are, and how many have an article.
  parts total published)

(defstruct part
  number title article)

;; Sections are almighty-press's. Their blocks are press's too, except that
;; notes become (:note label paragraphs) and source blocks get :highlight.
(defun section-title-lines (section) (press:section-title-lines section))
(defun section-title (section) (press:section-title section))
(defun section-id (section) (press:section-id section))
(defun section-blocks (section) (press:section-blocks section))

;;; Places

(defun project-root ()
  (or (ignore-errors (asdf:system-source-directory "almightylisp"))
      (uiop:getcwd)))

(defun content-directory ()
  (merge-pathnames "content/magazine/" (project-root)))

(defun series-directory ()
  (merge-pathnames "content/magazine/series/" (project-root)))

(defun publish-directory ()
  (merge-pathnames "static/magazine/published/" (project-root)))

;;; Types, topics and serials. An article's serial is TOPIC-TYPE-DATE, e.g.
;;; CL-OP-26924: topic Lisp, type Opinion, 2026-09-24 (two-digit year, month,
;;; two-digit day).

(defparameter *article-topics*
  '(("Lisp" "CL")
    ("Emacs" "EM")
    ("Tooling" "TL"))
  "Allowed values of #+TOPIC, with their serial codes.")

(defparameter *article-types*
  '(("Opinion" "OP")
    ("Tutorial" "TU")
    ("How-to" "HT")
    ("Explanation" "EX")
    ("Reference" "RF")
    ("News" "NW")
    ("Review" "RV")
    ("Case study" "CS")
    ("Post-mortem" "PM")
    ("Code reading" "CR")
    ("Interview" "IV")
    ("Q&A" "QA"))
  "Allowed values of #+TYPE, with their serial codes.")

(defparameter *article-keywords*
  '("TITLE" "SUBTITLE" "AUTHOR" "DATE" "TYPE" "TOPIC" "CODE" "SLUG" "HOME"
    "RELATED" "BOOK_CHAPTER" "BOOK_TITLE" "BOOK_SUMMARY" "BOOK_HREF"
    "SERIES" "PART" "DRAFT")
  "Keywords an article may use. Others get a warning, to catch typos.")

(defun choice (keyword text table)
  "The spelling of TEXT among TABLE's names."
  (or (first (assoc (string-trim '(#\Space #\Tab) (or text "")) table :test #'string-equal))
      (error "~a must be one of ~{~a~^, ~}; got ~s." keyword (mapcar #'first table) text)))

(defun code-of (name table)
  (second (assoc name table :test #'string=)))

(defun make-serial (topic type date)
  (format nil "~a-~a-~2,'0d~d~2,'0d"
          (code-of topic *article-topics*) (code-of type *article-types*)
          (mod (at:year date) 100) (at:month date) (at:day date)))

(defun safe-slug-p (slug)
  (and (stringp slug)
       (plusp (length slug))
       (every (lambda (ch) (or (alphanumericp ch) (char= ch #\-))) slug)
       (not (member slug '("archive" "index") :test #'string=))))

;;; Articles

(defun highlighted-lines (block)
  "The line numbers in a source block's :highlight, such as \"3 4\"."
  (let ((text (cdr (assoc "highlight" (getf (rest block) :args) :test #'string=)))
        (count (length (getf (rest block) :lines))))
    (loop for word in (and text (press:split-words text))
          for n = (ignore-errors (parse-integer word))
          unless n
            do (error ":highlight takes line numbers, like :highlight 3 4; got ~s." text)
          unless (<= 1 n count)
            do (error ":highlight ~d isn't a line of this ~d-line source block; lines count from 1." n count)
          collect n)))

(defun article-block (block)
  "BLOCK as the article page renders it. Notes become (:note label
paragraphs), and a source block gets :highlight, a list of line numbers."
  (case (first block)
    (:block
     (destructuring-bind (name label paragraphs) (rest block)
       (unless (string= name "NOTE")
         (error "#+BEGIN_~a isn't used in articles; use #+BEGIN_NOTE or #+BEGIN_SRC." name))
       (list :note (if (plusp (length label)) label "Note") paragraphs)))
    (:src
     (list* :src :highlight (highlighted-lines block) (rest block)))
    (t block)))

(defun parse-article (text &key slug pathname)
  "The article in TEXT, an org document. SLUG is used unless #+SLUG is given."
  (let* ((document (press:parse-org text))
         (keyword (lambda (name) (press:document-keyword document name))))
    (flet ((value (name) (funcall keyword name))
           (required (name)
             (or (funcall keyword name) (error "#+~a is required." name))))
      (when (press:document-intro document)
        (error "Text before the first heading: ~s" (first (press:document-intro document))))
      (loop for (old . new) in '(("KIND" . "TYPE") ("CATEGORY" . "TOPIC"))
            when (value old) do (error "#+~a is now #+~a." old new))
      (loop for name being the hash-keys of (press:document-keywords document)
            unless (member name *article-keywords* :test #'string=)
              do (warn "~a: #+~a isn't an article keyword; it is ignored."
                       (or pathname slug) name))
      (let* ((title-lines (press:split-breaks (required "TITLE")))
             (slug (or (value "SLUG") slug))
             (date (press:parse-date (required "DATE")))
             (type (choice "TYPE" (required "TYPE") *article-types*))
             (topic (choice "TOPIC" (required "TOPIC") *article-topics*))
             (home (value "HOME")))
        (unless (safe-slug-p slug)
          (error "Slug ~s must be letters, digits and hyphens, and not archive or index." slug))
        (when (and home (string-not-equal home "lead"))
          (error "#+HOME can only be lead; got ~s." home))
        (when (and (value "SERIES") (not (value "PART")))
          (error "#+PART is required with #+SERIES."))
        (dolist (section (press:document-sections document))
          (setf (press:section-blocks section)
                (mapcar #'article-block (press:section-blocks section))))
        (make-article
         :title-lines title-lines
         :title (press:join-lines title-lines)
         :subtitle (or (value "SUBTITLE") "")
         :author (or (value "AUTHOR") "")
         :date date
         :type type
         :topic topic
         ;; #+CODE overrides the generated serial, for the rare exception.
         :code (if (value "CODE") (string-upcase (value "CODE")) (make-serial topic type date))
         :slug slug
         :lead (and home t)
         :related (press:split-words (or (value "RELATED") ""))
         :book-chapter (value "BOOK_CHAPTER")
         :book-title (value "BOOK_TITLE")
         :book-summary (or (value "BOOK_SUMMARY") "")
         :book-href (or (value "BOOK_HREF") "/book/essentials")
         :sections (press:document-sections document)
         :draft (press:truthy (value "DRAFT"))
         :series (when (value "SERIES") (string-upcase (value "SERIES")))
         :part (when (value "PART") (parse-integer (value "PART")))
         :pathname pathname)))))

(defun article-file-p (file)
  "Article files are named in lowercase. A name starting with a capital,
like WRITING.org, is a note for writers."
  (let ((name (pathname-name file)))
    (and (plusp (length name)) (not (upper-case-p (char name 0))))))

(defun disambiguate-serials (articles)
  "Two articles with the same topic, type and date would share a serial. The
first by slug keeps it; the others get B, C, ... and a warning."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (article (sort (copy-list articles) #'string< :key #'article-slug) articles)
      (let* ((code (article-code article))
             (n (gethash code seen 0)))
        (setf (gethash code seen) (1+ n))
        (when (plusp n)
          (let ((new (format nil "~a~c" code (code-char (+ (char-code #\A) n)))))
            (warn "~a and another article share the serial ~a; using ~a."
                  (article-slug article) code new)
            (setf (article-code article) new)))))))

(defun load-articles ()
  "Published articles, newest first."
  (let ((articles (press:load-files
                   (press:org-files (content-directory) :include #'article-file-p)
                   (lambda (file)
                     (parse-article (uiop:read-file-string file :external-format :utf-8)
                                    :slug (pathname-name file) :pathname file)))))
    (sort (disambiguate-serials (remove-if #'article-draft articles))
          #'press:date> :key #'article-date)))

;;; Series

(defun planned-part-title (line)
  "The title from a planned-part line: \"1. Title\", \"1) Title\", or \"- Title\"."
  (let* ((text (string-left-trim '(#\Space #\Tab) line))
         (digits (position-if-not #'digit-char-p text)))
    (cond
      ((and (plusp (length text)) (char= (char text 0) #\-))
       (string-trim '(#\Space #\Tab) (subseq text 1)))
      ((and digits (plusp digits) (member (char text digits) '(#\. #\))))
       (string-trim '(#\Space #\Tab) (subseq text (1+ digits))))
      (t nil))))

(defun parse-series (text)
  "The series in TEXT: keywords, then its planned parts, one a line."
  (let* ((document (press:parse-org text))
         (code (press:document-keyword document "CODE"))
         (title (press:document-keyword document "TITLE")))
    (when (press:document-sections document)
      (error "A series file has no headings, only keywords and planned parts."))
    (unless code (error "#+CODE is required."))
    (unless title (error "#+TITLE is required."))
    (make-series
     :code (string-upcase code)
     :title title
     :summary (press:document-keyword document "SUMMARY" "")
     :featured (press:truthy (press:document-keyword document "FEATURED"))
     :planned (loop for line in (press:document-intro document)
                    for title = (planned-part-title line)
                    unless title
                      do (error "Expected a numbered part, got ~s." line)
                    collect title))))

(defun load-series ()
  (let ((series (press:load-files (press:org-files (series-directory))
                                  (lambda (file)
                                    (parse-series (uiop:read-file-string
                                                   file :external-format :utf-8))))))
    (loop for (one . more) on series
          when (find (series-code one) more :key #'series-code :test #'string=)
            do (error "Two series files use the code ~a." (series-code one)))
    series))

(defun link-series (series articles)
  "Check each article's series exists, and fill in every series' parts."
  (dolist (article articles)
    (when (and (article-series article)
               (not (find-series (article-series article) series)))
      (error "~a names #+SERIES ~a, but no file in ~a has that code."
             (article-pathname article) (article-series article) (series-directory))))
  (dolist (one series series)
    (let* ((members (remove (series-code one) articles :key #'article-series :test-not #'equal))
           (total (max (length (series-planned one))
                       (reduce #'max members :key #'article-part :initial-value 0))))
      (setf (series-parts one)
            (loop for n from 1 to total
                  for article = (find n members :key #'article-part)
                  collect (make-part :number n
                                     :article article
                                     :title (if article
                                                (article-title article)
                                                (or (nth (1- n) (series-planned one)) "Untitled"))))
            (series-total one) total
            (series-published one) (count-if #'part-article (series-parts one))))))

;;; The loaded magazine

(defvar *magazine*
  (press:make-collection
   :directories (list (content-directory) (series-directory))
   :load (lambda ()
           (let ((articles (load-articles)))
             (list articles (link-series (load-series) articles))))))

(defun magazine-articles ()
  "Every published article, newest first."
  (first (press:collection-items *magazine*)))

(defun magazine-series ()
  "Every series, its parts linked to their articles."
  (second (press:collection-items *magazine*)))

(defun take-changes ()
  "True once after the org files change: time to publish again."
  (press:take-changes *magazine*))

(defun find-article (slug articles)
  (find slug articles :key #'article-slug :test #'string=))

(defun find-series (code series)
  (find code series :key #'series-code :test #'equal))

(defun article-series-of (article series)
  (when (article-series article)
    (find-series (article-series article) series)))

;;; Choosing what goes where

(defparameter *latest-count* 4
  "Briefs in the Latest section.")

(defparameter *archive-rows* 8
  "Articles listed one by one in the homepage's archive column. Older months
are folded into the Earlier months index.")

(defparameter *series-cards* 3
  "Series shown on the homepage.")

(defun take-n (list n)
  (subseq list 0 (min n (length list))))

(defun month-key (article)
  (press:format-date (article-date article) :month-id))

(defun group-by-month (articles)
  "ARTICLES, in order, as a list of (month-date articles...) per month."
  (let ((groups nil))
    (dolist (article articles (nreverse (mapcar (lambda (group)
                                                  (cons (car group) (reverse (cdr group))))
                                                groups)))
      (if (and groups (press:same-month-p (car (first groups)) (article-date article)))
          (push article (cdr (first groups)))
          (push (list (article-date article) article) groups)))))

(defun latest-date (series)
  "The newest part's date, or NIL when none are out."
  (let ((dates (loop for part in (series-parts series)
                     when (part-article part) collect (article-date (part-article part)))))
    (first (sort dates #'press:date>))))

(defun homepage-layout (articles series)
  "Everything the homepage shows, as a plist:
  :lead       the lead: the newest #+HOME: lead article, or the newest article
  :featured   the featured series, if any
  :series     series with a part out, featured first, then most recently updated
  :latest     the next *latest-count* articles
  :months     (month-date shown-above-count articles) for months listed in full
  :earlier    (month-date count) for older months
  :since      the oldest article's date"
  (let* ((lead (or (find-if #'article-lead articles) (first articles)))
         (rest (remove lead articles))
         (latest (take-n rest *latest-count*))
         (above (cons lead latest))
         (featured (find-if #'series-featured series))
         (running (sort (remove-if #'zerop (copy-list series) :key #'series-published)
                        (lambda (a b)
                          (cond ((eq a featured) t)
                                ((eq b featured) nil)
                                (t (press:date> (latest-date a) (latest-date b)))))))
         (months nil)
         (earlier nil)
         (listed 0))
    (loop for (date . members) in (group-by-month (nthcdr (length latest) rest))
          do (if (or (null months) (<= (+ listed (length members)) *archive-rows*))
                 (progn
                   (push (list date
                               (count-if (lambda (article) (press:same-month-p (article-date article) date))
                                         above)
                               members)
                         months)
                   (incf listed (length members)))
                 (push (list date (length members)) earlier)))
    (list :lead lead
          :featured featured
          :series (take-n running *series-cards*)
          :latest latest
          :months (nreverse months)
          :earlier (nreverse earlier)
          :since (when articles (article-date (car (last articles)))))))

(defun keep-reading-articles (article articles)
  "Other articles for the end of ARTICLE: its #+RELATED ones first, then the
newest. A book card takes one of the three places."
  (let* ((slots (if (article-book-title article) 2 3))
         (related (loop for slug in (article-related article)
                        for found = (find-article slug articles)
                        when (and found (not (eq found article)))
                          collect found))
         (chosen (take-n related slots)))
    (append chosen
            (take-n (remove-if (lambda (other) (or (eq other article) (member other chosen)))
                               articles)
                    (- slots (length chosen))))))
