(defpackage #:magazine/model
  (:use #:cl)
  (:export
   ;; Articles
   #:article #:article-title-lines #:article-title #:article-subtitle #:article-author
   #:article-date #:article-type #:article-topic #:article-code #:article-slug
   #:article-lead #:article-related #:article-book-chapter #:article-book-title
   #:article-book-summary #:article-book-href #:article-sections #:article-draft
   #:article-series #:article-part #:article-recap #:article-pathname #:article-layout
   #:section-title-lines #:section-title #:section-id #:section-blocks
   #:section-evolution
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
   #:content-directory #:series-directories #:publish-directory))

(in-package #:magazine/model)

;;;; The magazine's content: articles and series, read from org files.
;;;
;;; content/magazine/WRITING.org is the writers' guide: every keyword, type
;;; and topic, with examples. In short:
;;;
;;; One file in content/magazine/, or in a series' folder there, is one
;;; article, and its filename is the slug. Keywords before the first heading:
;;;
;;;   #+TITLE: The image\\is still\\running     required; \\ breaks the headline
;;;   #+SUBTITLE: the dek under the headline
;;;   #+AUTHOR: Micah Killian
;;;   #+DATE: 2026-09-24                        required unless a draft; YYYY-MM-DD
;;;   #+TYPE: Opinion                           required unless a draft; *article-types*
;;;   #+TOPIC: Lisp                             required unless a draft; *article-topics*
;;;   #+CODE: CL-OP-26924                       optional; normally generated
;;;   #+SLUG: other-slug                        optional; normally the filename
;;;   #+HOME: lead                              make this the homepage lead
;;;   #+RELATED: slug-one slug-two              first picks for Keep reading
;;;   #+BOOK_CHAPTER: 8                         a book chapter for Keep reading
;;;   #+BOOK_TITLE: The LOOP Macro
;;;   #+BOOK_SUMMARY: dek for the book card
;;;   #+BOOK_HREF: /book/essentials/the-loop-macro
;;;   #+PART: 1                                 required in a series' folder, and only there
;;;   #+RECAP: what the part before did         from part 2; normally that part's dek
;;;   #+LAYOUT: evolving                        an evolving article; see below
;;;   #+DRAFT: t                                not published
;;;
;;; The body is sections (* headings) of paragraphs, ** subheads, source
;;; blocks with #+RESULTS:, tables, and #+BEGIN_NOTE label ... #+END_NOTE
;;; margin notes; see almighty-press/org for the syntax. Files named with a
;;; leading capital, like WRITING.org, are not articles.
;;;
;;; An evolving article (#+LAYOUT: evolving) is read one paragraph at a time
;;; beside its section's heading, or beside code that changes as the reader
;;; goes: source blocks that share an :evolve name are the steps of one
;;; block. It is written in the same org, with rules about what may follow
;;; what; SECTION-EVOLUTION has them.
;;;
;;; A folder in content/magazine/ is a series (except templates/). Its
;;; series.org has the series' keywords and nothing else:
;;;
;;;   #+CODE: CND                               three letters; picks the logo
;;;   #+TITLE: The condition system, bottom up
;;;   #+SUMMARY: one or two sentences for the series card
;;;   #+FEATURED: t                             at most one series; shown first
;;;
;;; Every other file in the folder is a part, numbered by its #+PART. A part
;;; that isn't written yet is a draft with only #+TITLE, #+PART and
;;; #+DRAFT: t; the series shows it as queued. So a part's title is only
;;; ever in its own file.

(defstruct article
  title-lines title subtitle author date type topic code slug lead related
  book-chapter book-title book-summary book-href
  sections draft pathname series part
  ;; The recap of the part before, which this part's lede closes with; NIL
  ;; to use that part's dek.
  recap
  ;; NIL for an ordinary article, :EVOLVING for an evolving one.
  layout)

(defstruct series
  code title summary featured
  ;; Filled in by READ-SERIES from the folder's articles: a list of PART
  ;; structs, how many parts there are, and how many are published.
  parts total published)

;; A part's ARTICLE is NIL while it is a draft.
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

(defparameter *not-series* '("templates")
  "Folders in content/magazine/ that aren't series.")

(defun series-directories ()
  "The series' folders in content/magazine/, by name."
  (sort (remove-if (lambda (directory)
                     (let ((name (car (last (pathname-directory directory)))))
                       (or (char= (char name 0) #\.)
                           (member name *not-series* :test #'string=))))
                   (uiop:subdirectories (content-directory)))
        #'string< :key #'namestring))

(defun publish-directory ()
  (merge-pathnames "static/magazine/published/" (project-root)))

;;; Types, topics and serials. An article's serial is TOPIC-TYPE-DATE, e.g.
;;; CL-OP-26924: topic Lisp, type Opinion, 2026-09-24 (two-digit year, month,
;;; two-digit day).

(defparameter *article-topics*
  '(("Lisp" "CL")
    ("Emacs" "EM")
    ("Tooling" "TL")
    ("Industry" "IN")
    ("Native Development" "ND")
    ("Web Development" "WD"))
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
    "PART" "RECAP" "DRAFT" "LAYOUT")
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
paragraphs), and a source block gets :highlight, a list of line numbers
(MARK-STEPS adds to it)."
  (case (first block)
    (:block
        (destructuring-bind (name label paragraphs) (rest block)
          (unless (string= name "NOTE")
            (error "#+BEGIN_~a isn't used in articles; use #+BEGIN_NOTE or #+BEGIN_SRC." name))
          (list :note (if (plusp (length label)) label "Note") paragraphs)))
    (:src
     (list* :src :highlight (highlighted-lines block) (rest block)))
    (t block)))

;;; In an ordinary article, source blocks that share an :evolve name are the
;;; steps of one piece of code, each the whole code as it stands at that
;;; step. A step after the first is marked with what it changed, as an
;;; evolving article marks its steps: LINE-ORIGINS is match in
;;; static/js/magazine-evolving.js, and the two should agree.

(defun line-text (line)
  (string-left-trim '(#\Space #\Tab) line))

(defun lines-alike-p (a b)
  "Are the texts A and B one line, edited? They are when they start alike."
  (let* ((n (min (length a) (length b)))
         (same (or (mismatch a b) n)))
    (and (>= n 4) (>= same (* n 0.4)))))

(defun line-origins (old new)
  "For each of the texts NEW, the index in OLD of the line it came from, or
NIL: by the longest common subsequence of the two, and then, between one
match and the next, by pairing the lines left over that are alike."
  (let* ((n (length old))
         (m (length new))
         (common (make-array (list (1+ n) (1+ m)) :initial-element 0))
         (origins (make-array m :initial-element nil))
         (i 0) (j 0) (gone nil) (come nil))
    (loop for i from (1- n) downto 0
          do (loop for j from (1- m) downto 0
                   do (setf (aref common i j)
                            (if (string= (aref old i) (aref new j))
                                (1+ (aref common (1+ i) (1+ j)))
                                (max (aref common (1+ i) j) (aref common i (1+ j)))))))
    (flet ((pair-leftovers ()
             (let ((left (reverse gone)))
               (dolist (to (reverse come))
                 (let ((found (member-if (lambda (from)
                                           (lines-alike-p (aref old from) (aref new to)))
                                         left)))
                   (when found
                     (setf (aref origins to) (first found)
                           left (rest found))))))
             (setf gone nil come nil)))
      (loop while (or (< i n) (< j m))
            do (cond ((and (< i n) (< j m) (string= (aref old i) (aref new j)))
                      (setf (aref origins j) i)
                      (incf i)
                      (incf j)
                      (pair-leftovers))
                     ((and (< j m)
                           (or (>= i n) (>= (aref common i (1+ j)) (aref common (1+ i) j))))
                      (push j come)
                      (incf j))
                     (t
                      (push i gone)
                      (incf i))))
      (pair-leftovers))
    origins))

(defun step-changes (old new)
  "What the step with the lines NEW changed from the one with the lines OLD,
as (values added moved), each a list of line numbers from 1. A line is moved
when it was there but its text or its indentation is different."
  (flet ((texts (lines) (map 'vector #'line-text lines))
         (indent (line) (- (length line) (length (line-text line)))))
    (let* ((was (texts old))
           (now (texts new))
           (origins (line-origins was now)))
      (loop for line in new
            for n from 1
            for from = (aref origins (1- n))
            if (null from)
              collect n into added
            else if (or (string/= (aref was from) (aref now (1- n)))
                        (/= (indent (nth from old)) (indent line)))
                   collect n into moved
            finally (return (values added moved))))))

(defun mark-steps (blocks steps)
  "BLOCKS, with each source block that has :evolve marked with what it
changed from the step before it: the lines it added join its :highlight, and
it gets :moved, the lines it changed. STEPS, a table, holds the lines each
:evolve name was last seen with, so steps can be sections apart."
  (mapcar (lambda (block)
            (let ((name (and (eq (first block) :src)
                             (cdr (assoc "evolve" (getf (rest block) :args) :test #'string=)))))
              (cond ((null name) block)
                    ((zerop (length name))
                     (error ":evolve needs a name, like :evolve run."))
                    (t
                     (let ((lines (getf (rest block) :lines)))
                       (multiple-value-bind (before seen) (gethash name steps)
                         (setf (gethash name steps) lines)
                         (if (not seen)
                             block
                             (multiple-value-bind (added moved) (step-changes before lines)
                               (let ((highlight (sort (union added (copy-list (getf (rest block) :highlight)))
                                                      #'<)))
                                 (list* :src :highlight highlight
                                        :moved (set-difference moved highlight)
                                        (rest block)))))))))))
          blocks))

;;; Evolving articles
;;;
;;; An evolving article shows one paragraph at a time. Beside it is the
;;; section's heading, until a paragraph brings something else to that side:
;;; a note, a source block, or a step of an evolving block. What comes after
;;; a paragraph in the org file is what that paragraph brings.

(defun step-link-count (text)
  "How many [[step][expression]] links are in TEXT, a paragraph."
  (labels ((count-in (inlines)
             (loop for inline in inlines
                   sum (cond ((stringp inline) 0)
                             ((eq (first inline) :code) 0)
                             ((eq (first inline) :link)
                              (+ (if (string= (second inline) "step") 1 0)
                                 (count-in (third inline))))
                             (t (count-in (second inline)))))))
    (count-in (press:parse-inlines text))))

(defun section-evolution (section)
  "SECTION of an evolving article, as (values items stops).

ITEMS are what can take the heading's place, each with an id unique in the
article:
  (:code id name frames)        a source block; FRAMES is a list of
                                (lines results), one for each step
  (:note id label paragraphs)   a #+BEGIN_NOTE

STOPS are the paragraphs, lists and tables, in order, as plists:
  :block   the block itself
  :more    the list or table read with it: one that comes straight after
           a paragraph ending in a colon
  :head    the ** subheading before it, if any
  :sub     the ** subheading it is under, if any, and :sub-number, which
           one of the section's that is, from 1
  :item    the id of the item beside it, or NIL for the heading
  :frame   the step of that item this stop moves it to, if it does
  :steps   for each [[step][...]] link in the paragraph, (id . frame)

The rules, each an error when broken:
- A note or a source block belongs to the paragraph before it, so a section
  can't start with one.
- A paragraph has a note, or one source block, or as many source blocks as
  it has [[step][...]] links, in the links' order.
- Source blocks with the same :evolve name are the steps of one block, in
  order, and each holds the whole code as it stands at that step. That
  block then stays beside the paragraphs after it. A source block without
  :evolve, and a note, are there only for their own paragraph."
  (let ((items nil) (stops nil) (stop nil) (head nil) (staying nil) (count 0)
        (sub nil) (subs 0))
    (labels ((fail (control &rest arguments)
               (error "In ~s: ~?" (section-title section) control arguments))
             (new-id ()
               (format nil "~a-item-~d" (section-id section) (incf count)))
             (close-stop ()
               (when stop
                 (let ((links (getf stop :links)) (given (getf stop :given)))
                   (when (and (plusp links) (/= links given))
                     (fail "a paragraph marks ~d expression~:p with [[step][...]] but ~d source block~:p follow~:[~;s~] it; each marked expression needs its own block."
                           links given (= given 1))))
                 (push stop stops)
                 (setf stop nil)))
             (open-stop (block)
               (close-stop)
               (setf stop (list :block block :head head :item staying :frame nil :steps nil
                                :links (if (eq (first block) :p) (step-link-count (second block)) 0)
                                :given 0 :note nil :more nil
                                :sub sub :sub-number (when sub subs))
                     head nil))
             (introduced-p ()
               "Is the open stop a paragraph that ends in a colon, with nothing after it yet?"
               (and stop
                    (eq (first (getf stop :block)) :p)
                    (null (getf stop :more))
                    (not (getf stop :note))
                    (zerop (getf stop :given))
                    (let ((text (string-right-trim '(#\Space #\Tab #\Newline) (second (getf stop :block)))))
                      (and (plusp (length text))
                           (char= (char text (1- (length text))) #\:)))))
             (add-note (block)
               (destructuring-bind (label paragraphs) (rest block)
                 (cond ((null stop)
                        (fail "a note comes before any paragraph. In an evolving article a note belongs to the paragraph before it."))
                       ((or (getf stop :note) (plusp (getf stop :given)) (plusp (getf stop :links)))
                        (fail "the note ~s follows a paragraph that already has a note or code beside it; put a paragraph before the note." label)))
                 (let ((id (new-id)))
                   (push (list :note id label paragraphs) items)
                   (setf (getf stop :item) id
                         (getf stop :note) t))))
             (add-src (block)
               (let* ((plist (rest block))
                      (args (getf plist :args))
                      (name (cdr (assoc "evolve" args :test #'string=)))
                      (frame (list (getf plist :lines) (getf plist :results)))
                      (item (and name (find-if (lambda (item)
                                                 (and (eq (first item) :code)
                                                      (equal (fifth item) name)))
                                               items))))
                 (cond ((null stop)
                        (fail "a source block comes before any paragraph. In an evolving article code belongs to the paragraph before it."))
                       ((getf stop :note)
                        (fail "a source block follows a paragraph that already has a note beside it; put a paragraph before the code."))
                       ((and (zerop (getf stop :links)) (plusp (getf stop :given)))
                        (fail "two source blocks follow one paragraph. Put a paragraph between them, or mark an expression in the paragraph for each with [[step][...]].")))
                 (when (and name (zerop (length name)))
                   (fail ":evolve needs a name, like :evolve run."))
                 (if item
                     (setf (fourth item) (append (fourth item) (list frame)))
                     (push (setf item (list :code (new-id)
                                            (or (cdr (assoc "buffer" args :test #'string=))
                                                (getf plist :lang))
                                            (list frame)
                                            name))
                           items))
                 (let ((step (cons (second item) (length (fourth item)))))
                   (incf (getf stop :given))
                   (if (plusp (getf stop :links))
                       (setf (getf stop :steps) (append (getf stop :steps) (list step)))
                       (setf (getf stop :item) (car step)
                             (getf stop :frame) (cdr step))))
                 (when name
                   (setf staying (second item))))))
      (dolist (block (section-blocks section))
        (ecase (first block)
          (:h (setf head (second block)
                    sub head)
              (incf subs))
          (:p (open-stop block))
          ;; "Strengths:" and its list are read together.
          ((:list :table)
           (if (introduced-p)
               (setf (getf stop :more) (list block))
               (open-stop block)))
          (:note (add-note block))
          (:src (add-src block))))
      (close-stop)
      (values (mapcar (lambda (item) (subseq item 0 4)) (reverse items))
              (reverse stops)))))

(defun parse-layout (text)
  (cond ((null text) nil)
        ((string-equal text "evolving") :evolving)
        (t (error "#+LAYOUT can only be evolving; got ~s. Leave it out for an ordinary article." text))))

(defun parse-article (text &key slug pathname series)
  "The article in TEXT, an org document. SLUG is used unless #+SLUG is given.
SERIES is the code of the series whose folder it is in, if any. A draft
needs only #+TITLE (and #+PART in a series), so a queued part can be one."
  (let* ((document (press:parse-org text))
         (keyword (lambda (name) (press:document-keyword document name)))
         (draft (press:truthy (funcall keyword "DRAFT"))))
    (flet ((value (name) (funcall keyword name))
           (required (name)
             (or (funcall keyword name) (error "#+~a is required." name)))
           (unless-draft (name)
             (or (funcall keyword name)
                 (unless draft (error "#+~a is required, unless #+DRAFT: t." name)))))
      (when (press:document-intro document)
        (error "Text before the first heading: ~s" (first (press:document-intro document))))
      (loop for (old . new) in '(("KIND" . "TYPE") ("CATEGORY" . "TOPIC"))
            when (value old) do (error "#+~a is now #+~a." old new))
      (when (value "SERIES")
        (error "#+SERIES is gone: move the file into the series' folder, which says which series it's in."))
      (loop for name being the hash-keys of (press:document-keywords document)
            unless (member name *article-keywords* :test #'string=)
              do (warn "~a: #+~a isn't an article keyword; it is ignored."
                       (or pathname slug) name))
      (let* ((title-lines (press:split-breaks (required "TITLE")))
             (slug (or (value "SLUG") slug))
             (date (let ((text (unless-draft "DATE"))) (and text (press:parse-date text))))
             (type (let ((text (unless-draft "TYPE"))) (and text (choice "TYPE" text *article-types*))))
             (topic (let ((text (unless-draft "TOPIC"))) (and text (choice "TOPIC" text *article-topics*))))
             (part (value "PART"))
             (home (value "HOME"))
             (layout (parse-layout (value "LAYOUT"))))
        (unless (safe-slug-p slug)
          (error "Slug ~s must be letters, digits and hyphens, and not archive or index." slug))
        (when (and home (string-not-equal home "lead"))
          (error "#+HOME can only be lead; got ~s." home))
        (cond ((and series (not part))
               (error "#+PART is required in a series' folder."))
              ((and part (not series))
               (error "#+PART is only for articles in a series' folder."))
              ((and part (not (ignore-errors (plusp (parse-integer part)))))
               (error "#+PART must be a number from 1; got ~s." part))
              ((and (value "RECAP") (not series))
               (error "#+RECAP is only for parts of a series, to recap the part before."))
              ((and (value "RECAP") (= (parse-integer part) 1))
               (error "#+RECAP is for part 2 on: part 1 has no part before it to recap.")))
        (let ((steps (make-hash-table :test #'equal)))
          (dolist (section (press:document-sections document))
            (setf (press:section-blocks section)
                  (mapcar #'article-block (press:section-blocks section)))
            ;; Read an evolving section now, so its mistakes stop the file
            ;; loading and not a page rendering. An ordinary article's steps
            ;; are marked instead.
            (if (eq layout :evolving)
                (section-evolution section)
                (setf (press:section-blocks section)
                      (mark-steps (press:section-blocks section) steps)))))
        (make-article
         :title-lines title-lines
         :title (press:join-lines title-lines)
         :subtitle (or (value "SUBTITLE") "")
         :author (or (value "AUTHOR") "")
         :date date
         :type type
         :topic topic
         ;; #+CODE overrides the generated serial, for the rare exception.
         :code (cond ((value "CODE") (string-upcase (value "CODE")))
                     ((and topic type date) (make-serial topic type date)))
         :slug slug
         :lead (and home t)
         :related (press:split-words (or (value "RELATED") ""))
         :book-chapter (value "BOOK_CHAPTER")
         :book-title (value "BOOK_TITLE")
         :book-summary (or (value "BOOK_SUMMARY") "")
         :book-href (or (value "BOOK_HREF") "/book/essentials")
         :sections (press:document-sections document)
         :draft draft
         :series series
         :part (and part (parse-integer part))
         :recap (value "RECAP")
         :layout layout
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

(defun read-article (file &optional series)
  (parse-article (uiop:read-file-string file :external-format :utf-8)
                 :slug (pathname-name file) :pathname file :series series))

(defun read-articles (directory &optional series)
  "Every article in DIRECTORY, drafts included."
  (press:load-files (press:org-files directory :include #'article-file-p)
                    (lambda (file) (read-article file series))))

;;; Series

(defun parse-series (text)
  "The series in TEXT, a series.org: keywords only."
  (let* ((document (press:parse-org text))
         (code (press:document-keyword document "CODE"))
         (title (press:document-keyword document "TITLE")))
    (when (press:document-sections document)
      (error "series.org has no headings, only the series' keywords."))
    (when (press:document-intro document)
      (error "series.org lists no parts; each part is a file in the folder, a draft if it isn't written yet. Found ~s."
             (first (press:document-intro document))))
    (unless code (error "#+CODE is required."))
    (unless title (error "#+TITLE is required."))
    (make-series
     :code (string-upcase code)
     :title title
     :summary (press:document-keyword document "SUMMARY" "")
     :featured (press:truthy (press:document-keyword document "FEATURED")))))

(defun series-article-file-p (file)
  (and (article-file-p file) (string/= (pathname-name file) "series")))

(defun read-series (directory)
  "The series in DIRECTORY, and its parts' articles, drafts included."
  (let ((file (merge-pathnames "series.org" directory)))
    (unless (probe-file file)
      (error "~a has no series.org. Every folder in ~a is a series, except ~{~a/~^, ~}."
             (uiop:native-namestring directory) (uiop:native-namestring (content-directory))
             *not-series*))
    (let* ((series (car (press:load-files
                         (list file)
                         (lambda (file) (parse-series (uiop:read-file-string file :external-format :utf-8))))))
           (members (press:load-files
                     (press:org-files directory :include #'series-article-file-p)
                     (lambda (file) (read-article file (series-code series))))))
      (setf (series-parts series) (series-parts-of series members directory)
            (series-total series) (length (series-parts series))
            (series-published series) (count-if #'part-article (series-parts series)))
      (values series members))))

(defun series-parts-of (series members directory)
  "SERIES' parts, 1 to the highest #+PART, each linked to its article unless
it is a draft. Every number needs exactly one file."
  (let ((total (reduce #'max members :key #'article-part :initial-value 0)))
    (loop for n from 1 to total
          for files = (remove n members :key #'article-part :test-not #'eql)
          do (cond ((null files)
                    (error "~a in ~a has no part ~d. Add a draft for it: #+TITLE, #+PART: ~d and #+DRAFT: t."
                           (series-code series) (uiop:native-namestring directory) n n))
                   ((rest files)
                    (error "~{~a~^ and ~} are both part ~d of ~a."
                           (mapcar #'article-slug files) n (series-code series))))
          collect (let ((article (first files)))
                    (make-part :number n
                               :title (article-title article)
                               :article (unless (article-draft article) article))))))

;;; Loading everything

(defun check-unique (items key message)
  "Signal MESSAGE, a format control given the clashing value, if two ITEMS share a KEY."
  (loop for (one . more) on items
        when (find (funcall key one) more :key key :test #'equal)
          do (error message (funcall key one))))

(defun load-magazine ()
  "(articles series): published articles newest first, and every series."
  (let ((articles (read-articles (content-directory)))
        (series nil))
    (dolist (directory (series-directories))
      (multiple-value-bind (one members) (read-series directory)
        (push one series)
        (setf articles (append articles members))))
    (check-unique series #'series-code "Two series use the code ~a.")
    (check-unique articles #'article-slug
                  "Two articles have the slug ~a, so they'd share a URL. Rename one file, or give it #+SLUG.")
    (list (sort (disambiguate-serials (remove-if #'article-draft articles))
                #'press:date> :key #'article-date)
          (nreverse series))))

;;; The loaded magazine

(defvar *magazine*
  (press:make-collection :directories (list (content-directory))
                         :recursive t
                         :load #'load-magazine))

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
