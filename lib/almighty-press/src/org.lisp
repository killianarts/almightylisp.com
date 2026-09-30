(defpackage #:almighty-press/org
  (:use #:cl)
  (:export
   ;; Parsing
   #:parse-org #:read-org-file #:org-error
   ;; Documents
   #:document #:document-keywords #:document-keyword #:document-intro
   #:document-sections
   ;; Sections
   #:section #:section-title #:section-title-lines #:section-id #:section-blocks
   ;; Inline markup and text helpers
   #:parse-inlines #:split-breaks #:join-lines #:slugify #:split-words #:truthy))

(in-package #:almighty-press/org)

;;;; A small org-mode reader for publishing.
;;;
;;; It reads the parts of org that articles are written in, and nothing else:
;;;
;;;   #+KEYWORD: value          before the first heading: the document's keywords
;;;   other lines before it     kept as the document's intro, one string a line
;;;   * Heading                 a section; \\ in a heading forces a line break
;;;   ** Heading                a subheading inside the section: (:h "text")
;;;   plain lines               paragraphs, split on blank lines: (:p "text")
;;;   #+BEGIN_SRC lang :k v     code: (:src :lang "lisp" :args (("k" . "v")) :lines (...) :results "..")
;;;   #+RESULTS:                right after a source block: the lines up to a blank line
;;;   #+BEGIN_NAME label        any other block: (:block "NAME" "label" ("paragraph" ...))
;;;   | a | b |                 a table: (:table header-or-nil rows); a |---| line
;;;                             after the first row makes it the header
;;;   - item, + item            a list: (:list :ul items); 1. item or 1) item make
;;;                             it (:list :ol items). An item is (paragraphs
;;;                             sublists): lines indented under the bullet continue
;;;                             it, and a deeper bullet starts a list inside it
;;;   # comment                 skipped
;;;
;;; Inline markup is left in the text; PARSE-INLINES reads emphasis and
;;; [[target][label]] links out of it.

(define-condition org-error (simple-error)
  ((line :initarg :line :initform nil :reader org-error-line))
  (:report (lambda (condition stream)
             (when (org-error-line condition)
               (format stream "line ~d: " (org-error-line condition)))
             (apply #'format stream
                    (simple-condition-format-control condition)
                    (simple-condition-format-arguments condition)))))

(defun fail (line control &rest arguments)
  (error 'org-error :line line :format-control control :format-arguments arguments))

(defstruct (document (:constructor %make-document))
  "A parsed org file. KEYWORDS is a hash table of upcased names to values."
  keywords intro sections)

(defstruct (section (:constructor %make-section))
  "A top-level heading and what follows it, up to the next one. ID is unique
within its document and made from the title, for links to #id."
  title title-lines id blocks)

(defun document-keyword (document name &optional default)
  "The value of #+NAME:, trimmed, or DEFAULT when it is missing or empty."
  (let ((value (gethash (string-upcase name) (document-keywords document))))
    (if (and value (plusp (length value))) value default)))

;;; Text helpers

(defun split-lines (text)
  (let ((lines nil)
        (start 0))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (push (string-right-trim '(#\Return) (subseq text start i)) lines)
               (setf start (1+ i)))
    (push (string-right-trim '(#\Return) (subseq text start)) lines)
    (nreverse lines)))

(defun blank-p (line)
  (every (lambda (ch) (member ch '(#\Space #\Tab))) line))

(defun join-lines (lines)
  "LINES joined with single spaces."
  (format nil "~{~a~^ ~}" lines))

(defun split-words (text)
  "TEXT split on whitespace."
  (let ((out nil)
        (start nil))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (if (member ch '(#\Space #\Tab #\Newline #\Return))
                 (when start
                   (push (subseq text start i) out)
                   (setf start nil))
                 (unless start
                   (setf start i)))
          finally (when start
                    (push (subseq text start) out)))
    (nreverse out)))

(defun split-breaks (text)
  "TEXT split at each \\\\, the forced line break in titles."
  (let ((parts nil)
        (start 0)
        (i 0)
        (n (length text)))
    (loop while (< i n)
          do (if (and (char= (char text i) #\\)
                      (< (1+ i) n)
                      (char= (char text (1+ i)) #\\))
                 (progn
                   (push (subseq text start i) parts)
                   (incf i 2)
                   (setf start i))
                 (incf i)))
    (push (subseq text start) parts)
    (remove "" (mapcar (lambda (part) (string-trim '(#\Space #\Tab) part)) (nreverse parts))
            :test #'string=)))

(defun slugify (text)
  "TEXT in lowercase, with runs of anything but letters and digits made one hyphen."
  (let ((out (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (dash nil))
    (loop for ch across (string-downcase text)
          do (cond
               ((alphanumericp ch)
                (vector-push-extend ch out)
                (setf dash nil))
               ((not dash)
                (when (plusp (length out))
                  (vector-push-extend #\- out)
                  (setf dash t)))))
    (string-right-trim "-" out)))

(defun truthy (text)
  "True for t, yes, true (any case). NIL for anything else, including NIL."
  (and text
       (member (string-downcase (string-trim '(#\Space #\Tab) text))
               '("t" "yes" "true")
               :test #'string=)
       t))

;;; Line kinds

(defun directive-p (line)
  (and (>= (length line) 2) (string= "#+" line :end2 2)))

(defun comment-p (line)
  (or (string= line "#")
      (and (>= (length line) 2) (string= "# " line :end2 2))))

(defun parse-directive (line)
  "The directive's name, upcased, and the rest of the line.
#+TITLE: value and #+BEGIN_SRC lisp :buffer x both work."
  (let* ((body (string-left-trim '(#\Space #\Tab) (subseq line 2)))
         (colon (position #\: body))
         (space (position #\Space body)))
    (cond
      ((and colon (or (null space) (< colon space)))
       (values (string-upcase (string-trim '(#\Space #\Tab) (subseq body 0 colon)))
               (string-trim '(#\Space #\Tab) (subseq body (1+ colon)))))
      (space
       (values (string-upcase (subseq body 0 space))
               (string-trim '(#\Space #\Tab) (subseq body (1+ space)))))
      (t
       (values (string-upcase (string-trim '(#\Space #\Tab) body)) "")))))

(defun heading-line (line)
  "The heading's level and title, or NIL when LINE isn't a heading."
  (let ((i (or (position #\* line :test-not #'char=) (length line))))
    (when (and (plusp i) (< i (length line)) (char= (char line i) #\Space))
      (values i (string-trim '(#\Space #\Tab) (subseq line i))))))

(defun table-line-p (line)
  (and (plusp (length line)) (char= (char line 0) #\|)))

(defun begin-name (line)
  "For #+BEGIN_NAME, the upcased NAME; otherwise NIL."
  (when (directive-p line)
    (let ((name (parse-directive line)))
      (when (and (> (length name) 6) (string= "BEGIN_" name :end2 6))
        (subseq name 6)))))

(defun end-p (line name)
  (and (directive-p line)
       (string= (parse-directive line) (concatenate 'string "END_" name))))

;;; Blocks

(defun split-row (line)
  (let ((cells (loop with start = 0
                     for i from 0 to (length line)
                     when (or (= i (length line)) (char= (char line i) #\|))
                       collect (string-trim '(#\Space #\Tab) (subseq line start i))
                       and do (setf start (1+ i)))))
    ;; The leading and trailing pipes leave empty cells at each end.
    (butlast (rest cells) (if (string= "" (car (last cells))) 1 0))))

(defun rule-row-p (cells)
  (and cells
       (every (lambda (cell)
                (and (plusp (length cell))
                     (every (lambda (ch) (member ch '(#\- #\+ #\Space #\:))) cell)))
              cells)))

(defun read-table (lines i)
  (let ((rows (loop while (and (< i (length lines)) (table-line-p (aref lines i)))
                    collect (split-row (aref lines i))
                    do (incf i))))
    (let ((header (when (and (>= (length rows) 2) (rule-row-p (second rows)))
                    (first rows))))
      (values (list :table header (remove-if #'rule-row-p (if header (cddr rows) rows)))
              i))))

(defun skip-space (text i)
  (or (position-if-not (lambda (ch) (member ch '(#\Space #\Tab))) text :start i)
      (length text)))

(defun read-arg-value (text i)
  (cond
    ((>= i (length text))
     (values "" i))
    ((char= (char text i) #\")
     (let ((end (position #\" text :start (1+ i))))
       (if end
           (values (subseq text (1+ i) end) (1+ end))
           (values (subseq text (1+ i)) (length text)))))
    (t
     (let ((next (or (search " :" text :start2 i) (length text))))
       (values (string-trim '(#\Space #\Tab) (subseq text i next)) next)))))

(defun parse-header-args (text)
  "An alist of the :key value pairs after a source block's language."
  (let ((i 0)
        (args nil))
    (loop
      (setf i (skip-space text i))
      (unless (and (< i (length text)) (char= (char text i) #\:))
        (return (nreverse args)))
      (let* ((start (1+ i))
             (end (or (position-if (lambda (ch) (member ch '(#\Space #\Tab))) text :start start)
                      (length text)))
             (key (string-downcase (subseq text start end))))
        (multiple-value-bind (value next) (read-arg-value text (skip-space text end))
          (push (cons key value) args)
          (setf i next))))))

(defun read-src (lines i)
  (multiple-value-bind (name rest) (parse-directive (aref lines i))
    (declare (ignore name))
    (let* ((space (position #\Space rest))
           (lang (string-downcase (if space (subseq rest 0 space) rest)))
           (args (when space (parse-header-args (subseq rest space))))
           (start i)
           (n (length lines)))
      (incf i)
      (let ((body (loop while (and (< i n) (not (end-p (aref lines i) "SRC")))
                        collect (aref lines i)
                        do (incf i))))
        (unless (< i n)
          (fail (1+ start) "#+BEGIN_SRC has no #+END_SRC."))
        (incf i)
        ;; #+RESULTS: may follow, after blank lines.
        (let ((after (or (position-if-not #'blank-p lines :start i) n))
              (results nil))
          (when (and (< after n)
                     (directive-p (aref lines after))
                     (string= (parse-directive (aref lines after)) "RESULTS"))
            (let ((same-line (nth-value 1 (parse-directive (aref lines after)))))
              (setf i (1+ after))
              (when (plusp (length same-line))
                (push same-line results))
              (loop while (and (< i n)
                               (let ((line (aref lines i)))
                                 (not (or (blank-p line) (directive-p line)
                                          (heading-line line) (table-line-p line)))))
                    do (push (aref lines i) results)
                       (incf i))))
          (values (list :src
                        :lang (if (string= lang "") "lisp" lang)
                        :args args
                        :lines body
                        :results (when results (format nil "~{~a~^~%~}" (nreverse results))))
                  i))))))

(defun read-special-block (lines i name)
  "A #+BEGIN_NAME block: the rest of the begin line as its label, and its
lines as paragraphs split on blank lines."
  (let ((label (nth-value 1 (parse-directive (aref lines i))))
        (start i)
        (n (length lines))
        (paragraphs nil)
        (buffer nil))
    (incf i)
    (flet ((flush ()
             (when buffer
               (push (join-lines (nreverse buffer)) paragraphs)
               (setf buffer nil))))
      (loop while (and (< i n) (not (end-p (aref lines i) name)))
            for line = (aref lines i)
            do (cond
                 ((blank-p line) (flush))
                 ((comment-p line))
                 ((or (directive-p line) (heading-line line) (table-line-p line))
                  (fail (1+ i) "#+BEGIN_~a on line ~d has no #+END_~a." name (1+ start) name))
                 (t (push line buffer)))
               (incf i))
      (unless (< i n)
        (fail (1+ start) "#+BEGIN_~a has no #+END_~a." name name))
      (flush)
      (values (list :block name label (nreverse paragraphs)) (1+ i)))))

;;; Lists

(defun indent-of (line)
  (or (position-if-not (lambda (ch) (member ch '(#\Space #\Tab))) line) (length line)))

(defun list-item-line (line)
  "For a list item's first line, its indent, :UL or :OL, and the text after
the bullet; otherwise NIL. Bullets are -, + and a number with . or ) after
it, each followed by a space."
  (let* ((indent (indent-of line))
         (n (length line))
         (digits-end (or (position-if-not #'digit-char-p line :start indent) n)))
    (flet ((item (kind after)
             (when (or (= after n) (char= (char line after) #\Space))
               (values indent kind (string-trim '(#\Space #\Tab) (subseq line after))))))
      (cond
        ((>= indent n) nil)
        ((member (char line indent) '(#\- #\+))
         (item :ul (1+ indent)))
        ((and (> digits-end indent) (< digits-end n)
              (member (char line digits-end) '(#\. #\))))
         (item :ol (1+ digits-end)))))))

(defun read-list (lines i)
  "The list starting at line I, and the index after it. Items are bullets at
the first one's indent; one blank line may separate them, two end the list.
A heading, directive or table ends it too, as does a line that is neither a
bullet nor indented under one."
  (let ((n (length lines))
        (base (list-item-line (aref lines i)))
        (kind (nth-value 1 (list-item-line (aref lines i))))
        (items nil))
    (labels ((line (j) (aref lines j))
             (ends-list-p (line)
               (or (heading-line line) (directive-p line) (table-line-p line)))
             (next-inside-p (j)
               "After a blank line at J-1: does the list go on at J?"
               (and (< j n)
                    (not (blank-p (line j)))
                    (not (ends-list-p (line j)))
                    (> (indent-of (line j)) base)))
             (item-here-p (j)
               (and (< j n) (eql (list-item-line (line j)) base))))
      (loop while (item-here-p i)
            do (let ((paragraphs nil)
                     (buffer (list (nth-value 2 (list-item-line (line i)))))
                     (sublists nil))
                 (flet ((flush ()
                          (when buffer
                            (push (join-lines (nreverse buffer)) paragraphs)
                            (setf buffer nil))))
                   (incf i)
                   (loop while (< i n)
                         for line = (line i)
                         do (cond
                              ((blank-p line)
                               (flush)
                               (if (next-inside-p (1+ i))
                                   (incf i)
                                   (return)))
                              ((ends-list-p line) (return))
                              ((<= (indent-of line) base) (return))
                              ((let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
                                 (or (directive-p trimmed) (table-line-p trimmed)))
                               (fail (1+ i) "Blocks and tables can't go inside a list item."))
                              ((list-item-line line)
                               (flush)
                               (multiple-value-bind (sublist next) (read-list lines i)
                                 (push sublist sublists)
                                 (setf i next)))
                              (t
                               (push (string-trim '(#\Space #\Tab) line) buffer)
                               (incf i))))
                   (flush)
                   (push (list (nreverse paragraphs) (nreverse sublists)) items)))
               ;; A single blank line may separate items.
               (when (and (< (1+ i) n) (blank-p (line i)) (item-here-p (1+ i)))
                 (incf i)))
      (values (list :list kind (nreverse items)) i))))

;;; Documents

(defun assign-ids (sections)
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (section sections sections)
      (let* ((base (let ((slug (slugify (section-title section))))
                     (if (plusp (length slug)) slug "section")))
             (id base))
        (loop for n from 2
              while (gethash id seen)
              do (setf id (format nil "~a-~d" base n)))
        (setf (gethash id seen) t
              (section-id section) id)))))

(defun parse-org (text)
  "Parse TEXT as an org document. Signals ORG-ERROR, with the line number,
when the text doesn't fit the shape described at the top of this file."
  (let* ((lines (coerce (split-lines text) 'vector))
         (n (length lines))
         (i 0)
         (keywords (make-hash-table :test #'equal))
         (intro nil)
         (section nil)
         (blocks nil)
         (paragraph nil)
         (sections nil))
    (labels ((flush-paragraph ()
               (when paragraph
                 (push (list :p (join-lines (nreverse paragraph))) blocks)
                 (setf paragraph nil)))
             (flush-section ()
               (flush-paragraph)
               (when section
                 (setf (section-blocks section) (nreverse blocks))
                 (push section sections)
                 (setf section nil
                       blocks nil)))
             (need-section (what)
               (unless section
                 (fail (1+ i) "~a before the first heading." what)))
             (add (block next)
               (push block blocks)
               (setf i next)))
      (loop while (< i n)
            for line = (aref lines i)
            do (cond
                 ((or (blank-p line) (comment-p line))
                  (flush-paragraph)
                  (incf i))
                 ((begin-name line)
                  (flush-paragraph)
                  (let ((name (begin-name line)))
                    (need-section (format nil "#+BEGIN_~a" name))
                    (multiple-value-call #'add
                      (if (string= name "SRC")
                          (read-src lines i)
                          (read-special-block lines i name)))))
                 ((directive-p line)
                  (flush-paragraph)
                  (when section
                    (fail (1+ i) "#+~a after the first heading." (parse-directive line)))
                  (multiple-value-bind (name value) (parse-directive line)
                    (setf (gethash name keywords) value))
                  (incf i))
                 ((and section (list-item-line line))
                  (flush-paragraph)
                  (multiple-value-call #'add (read-list lines i)))
                 ((table-line-p line)
                  (flush-paragraph)
                  (need-section "A table")
                  (multiple-value-call #'add (read-table lines i)))
                 ((heading-line line)
                  (multiple-value-bind (level title) (heading-line line)
                    (cond
                      ((= level 1)
                       (flush-section)
                       (let ((pieces (split-breaks title)))
                         (setf section (%make-section :title-lines pieces
                                                      :title (join-lines pieces)))))
                      (t
                       (need-section "A subheading")
                       (flush-paragraph)
                       (push (list :h (join-lines (split-breaks title))) blocks))))
                  (incf i))
                 (section
                   (push line paragraph)
                   (incf i))
                 (t
                  (push line intro)
                  (incf i))))
      (flush-section))
    (%make-document :keywords keywords
                    :intro (nreverse intro)
                    :sections (assign-ids (nreverse sections)))))

(defun read-org-file (pathname)
  "Parse the org file at PATHNAME."
  (parse-org (uiop:read-file-string pathname :external-format :utf-8)))

;;; Inline markup
;;;
;;; Org's emphasis: *bold*, /italic/, _underline_, +strike+, =verbatim= and
;;; ~code~. As in Org, a marker only counts at a word's edge: the opening one
;;; after a space, the start, or an opening bracket or quote, with no space
;;; just inside it; the closing one before a space, punctuation or the end.
;;; So 2*3*4 and a/b/c stay as written.

(defparameter *emphasis*
  '((#\* . :bold) (#\/ . :italic) (#\_ . :underline) (#\+ . :strike)
    (#\= . :code) (#\~ . :code))
  "Emphasis markers and what they make. :code's contents are literal.")

(defun emphasis-pre-p (text i)
  "May an emphasis marker at I open? Only at the start or after these."
  (or (zerop i)
      (member (char text (1- i)) '(#\Space #\Tab #\Newline #\- #\( #\{ #\' #\" #\[))))

(defun emphasis-post-p (text i)
  "May an emphasis marker end just before I? Only at the end or before these."
  (or (>= i (length text))
      (member (char text i) '(#\Space #\Tab #\Newline #\- #\. #\, #\; #\: #\! #\?
                              #\' #\" #\) #\} #\[ #\] #\\))))

(defun blank-char-p (char)
  )

(defun emphasis-end (text i)
  "If an emphasis span opens at I, the index of its closing marker."
  (let ((marker (char text i))
        (n (length text)))
    (when (and (assoc marker *emphasis*)
               (emphasis-pre-p text i)
               (< (1+ i) n)
               (not (blank-char-p (char text (1+ i)))))
      (loop for j = (position marker text :start (+ i 2)) then (position marker text :start (1+ j))
            while j
            when (and (not (blank-char-p (char text (1- j))))
                      (emphasis-post-p text (1+ j)))
              return j))))

(defun parse-inlines (text)
  "TEXT as a list of strings and elements: (:code \"text\") for =text= or
~text~; (:bold inlines), (:italic inlines), (:underline inlines) and
(:strike inlines) for *, /, _ and +; and (:link \"target\" inlines) for
[[target][label]] or [[target]]."
  (when (and text (plusp (length text)))
    (let ((out nil)
          (i 0)
          (start 0)
          (n (length text)))
      (flet ((flush (end)
               (when (< start end)
                 (push (subseq text start end) out))))
        (loop while (< i n)
              do (let ((emphasis-end (emphasis-end text i))
                       (link-end (and (< (1+ i) n)
                                      (string= "[[" text :start2 i :end2 (+ i 2))
                                      (search "]]" text :start2 (+ i 2)))))
                   (cond
                     (emphasis-end
                      (let ((kind (cdr (assoc (char text i) *emphasis*)))
                            (body (subseq text (1+ i) emphasis-end)))
                        (flush i)
                        (push (if (eq kind :code)
                                  (list :code body)
                                  (list kind (parse-inlines body)))
                              out)
                        (setf i (1+ emphasis-end)
                              start i)))
                     (link-end
                      (let* ((body (subseq text (+ i 2) link-end))
                             (sep (search "][" body))
                             (target (if sep (subseq body 0 sep) body))
                             (label (if sep (subseq body (+ sep 2)) target)))
                        (flush i)
                        (push (list :link target (parse-inlines label)) out)
                        (setf i (+ link-end 2)
                              start i)))
                     (t (incf i)))))
        (flush n))
      (nreverse out))))
