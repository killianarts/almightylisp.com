(in-package #:magazine/hypermedia/components)

;;;; Search, on every magazine page (Figma: Search B · Command palette · 4 ·
;;;; Alternative: split preview).
;;;
;;; "/" (or the strip's Search cell) opens a palette over the page: the query
;;; across the top, the articles that match down the left, and on the right
;;; the selected article with every match in it and where each falls.
;;;
;;;   Up / Down        the article before or after
;;;   Left / Right     the match before or after, while the caret is at the
;;;                    end of the query (Tab and Shift+Tab do the same)
;;;   Enter            open the article at the match in focus, and mark it
;;;   Shift+Space      more of the text around that match, or less
;;;   Esc              close
;;;
;;; The search runs here, and so does the drawing. The palette is part of
;;; every page (render-search-palette) and htmx drives it. What is being
;;; searched for is in the address a control asks: /magazine/search with q,
;;; field, sel, focus and wide (search-href). Typing asks for the query in
;;; the field chosen; every other control (a field, an article, a match) asks
;;; for the search as it stands with its own change made, so the server
;;; works out what each one does next. The answer is the palette's chips,
;;; list, preview and status again (render-search-results), each to be
;;; changed to match where the page has it.
;;;
;;; A control that a key stands for names the key in data-keys, and
;;; static/js/magazine-search.js presses it. That script also opens and
;;; closes the palette, which no request is needed for, and finds and marks
;;; the match in an article opened from here.
;;;
;;; It looks through titles, subtitles and body text. An article has no
;;; summary apart from its subtitle, so there are those three places to look
;;; and no fourth. A query is one phrase, matched whatever its case.

(defparameter *search-fields*
  '((:all "all" "All") (:title "title" "Title") (:subtitle "subtitle" "Subtitle")
    (:body "body" "Body text"))
  "Where a search can look: a name, the name an address uses, and a label.")

(defparameter *search-shortest* 2
  "A query shorter than this isn't searched.")

(defparameter *search-near* 90
  "Characters of text kept either side of a match.")

(defparameter *search-slots* 40
  "Ticks in a hit map.")

;;; The text searched: every article's sections as paragraphs of plain text.

(defun inline-text (inlines)
  "The text of INLINES, as PRESS:PARSE-INLINES returns them, without markup."
  (with-output-to-string (out)
    (labels ((walk (inline)
               (cond ((stringp inline) (write-string inline out))
                     ((eq (first inline) :code) (write-string (second inline) out))
                     ((eq (first inline) :link) (mapc #'walk (third inline)))
                     (t (mapc #'walk (second inline))))))
      (mapc #'walk inlines))))

(defun plain-text (text)
  "TEXT, a paragraph as it is written in org, as plain text on one line."
  (substitute #\Space #\Newline (inline-text (press:parse-inlines text))))

(defun block-paragraphs (block)
  "The paragraphs of text in BLOCK, one of an article's blocks: a paragraph
or subhead is one; a list or note, one for each of its paragraphs; a table,
one a row; a source block, its code on one line and its results on another."
  (case (first block)
    ((:p :h) (list (plain-text (second block))))
    (:note (mapcar #'plain-text (third block)))
    (:list (loop for (paragraphs sublists) in (third block)
                 append (mapcar #'plain-text paragraphs)
                 append (mapcan #'block-paragraphs sublists)))
    (:table (loop for row in (if (second block)
                                 (cons (second block) (third block))
                                 (third block))
                  collect (format nil "~{~a~^  ~}" (mapcar #'plain-text row))))
    (:src (let ((code (format nil "~{~a~^ ~}"
                              (remove "" (mapcar (lambda (line) (string-trim " 	" line))
                                                 (getf (rest block) :lines))
                                      :test #'string=)))
                (results (getf (rest block) :results)))
            (append (when (plusp (length code)) (list code))
                    (when (and (stringp results) (plusp (length results)))
                      (list (substitute #\Space #\Newline results))))))))

(defun section-paragraphs (section)
  "SECTION's heading and paragraphs, as text."
  (remove "" (cons (or (model:section-title section) "")
                   (mapcan #'block-paragraphs (copy-list (model:section-blocks section))))
          :test #'string=))

(defstruct indexed
  "An article as search reads it. SECTIONS is a list, a section to an entry:
its number, the section, and its paragraphs. LENGTH is how long the text is."
  article sections length)

(defun index-article (article)
  (let ((length 0))
    (make-indexed
     :article article
     :sections (loop for section in (model:article-sections article)
                     for number from 1
                     for paragraphs = (section-paragraphs section)
                     collect (list number section paragraphs)
                     do (incf length (reduce #'+ paragraphs :key #'length)))
     :length (max length 1))))

(defvar *search-index* nil
  "The articles last searched and their index, as (articles . index). The
magazine hands out the same list of articles until an org file changes.")

(defun search-index (articles)
  (let ((made *search-index*))
    (if (and made (eq (car made) articles))
        (cdr made)
        (cdr (setf *search-index* (cons articles (mapcar #'index-article articles)))))))

;;; Searching

(defstruct match
  "A passage with the query in it: the title, the subtitle, or a paragraph of
the text, however many times the query is in it. AT is where it first is,
WHERE how far through the article that is, from 0 to 1. A match in the text
has its SECTION and the section's NUMBER, and is paragraph INDEX of PARAGRAPHS."
  field text at (where 0) section number paragraphs index)

(defstruct result
  article matches rank order)

(defun article-matches (entry needle)
  "Every match for NEEDLE in ENTRY, an indexed article, in reading order."
  (let ((article (indexed-article entry))
        (matches nil)
        (offset 0))
    (flet ((look (field text &rest more)
             (let ((at (and text (search needle text :test #'char-equal))))
               (when at
                 (push (apply #'make-match :field field :text text :at at more) matches))
               at)))
      (look :title (model:article-title article))
      (look :subtitle (model:article-subtitle article))
      (loop for (number section paragraphs) in (indexed-sections entry)
            do (loop for text in paragraphs
                     for index from 0
                     for at = (look :body text :section section :number number
                                               :paragraphs paragraphs :index index)
                     do (when at
                          (setf (match-where (first matches))
                                (/ (+ offset at) (indexed-length entry))))
                        (incf offset (length text)))))
    (nreverse matches)))

(defun search-articles (articles needle field)
  "The articles with NEEDLE in FIELD, best first: one with it in its title
comes before one with it in its subtitle, and that before one with it only in
the text; then the more matches the better. Ties stay newest first, as
ARTICLES is. Also returns how many matches each field has, as a plist."
  (let ((counts (list :all 0 :title 0 :subtitle 0 :body 0))
        (results nil))
    (loop for entry in (search-index articles)
          for order from 0
          for every = (article-matches entry needle)
          for matches = (if (eq field :all)
                            every
                            (remove field every :key #'match-field :test-not #'eq))
          do (dolist (match every)
               (incf (getf counts :all))
               (incf (getf counts (match-field match))))
             (when matches
               (push (make-result :article (indexed-article entry)
                                  :matches matches
                                  :rank (case (match-field (first matches))
                                          (:title 2)
                                          (:subtitle 1)
                                          (t 0))
                                  :order order)
                     results)))
    (flet ((better-p (a b)
             (cond ((/= (result-rank a) (result-rank b)) (> (result-rank a) (result-rank b)))
                   ((/= (length (result-matches a)) (length (result-matches b)))
                    (> (length (result-matches a)) (length (result-matches b))))
                   (t (< (result-order a) (result-order b))))))
      (values (sort results #'better-p) counts))))

;;; Drawing

(defun render-marked (text needle)
  "TEXT, with every place NEEDLE is in it set in a mark."
  (let ((pieces nil)
        (from 0))
    (loop for at = (search needle text :start2 from :test #'char-equal)
          while at
          do (push (subseq text from at) pieces)
             (push (ah:</> (mark :class "search-mark" (subseq text at (+ at (length needle)))))
                   pieces)
             (setf from (+ at (length needle))))
    (push (subseq text from) pieces)
    (ah:</> (<> (nreverse pieces)))))

(defun near-text (text at needle)
  "The text around the match at AT: a way either side, cut at a space."
  (let* ((end (+ at (length needle)))
         (from (max 0 (- at *search-near*)))
         (to (min (length text) (+ end *search-near*))))
    (when (plusp from)
      (let ((space (position #\Space text :start from :end at)))
        (when space (setf from (1+ space)))))
    (when (< to (length text))
      (setf to (max end (or (position #\Space text :start end :end to :from-end t) end))))
    (format nil "~:[~;…~]~a~:[~;…~]"
            (plusp from) (subseq text from to) (< to (length text)))))

(defun tail-text (text)
  (if (> (length text) *search-near*)
      (format nil "…~a" (subseq text (- (length text) *search-near*)))
      text))

(defun head-text (text)
  (if (> (length text) *search-near*)
      (format nil "~a…" (subseq text 0 *search-near*))
      text))

(defun match-href (article match)
  "Where Enter goes: the match's section, or the article's top for a match
in its title or subtitle."
  (format nil "~a~@[#~a~]"
          (article-href article)
          (and (match-section match) (model:section-id (match-section match)))))

(defun count-in (needle text)
  "How many times NEEDLE is in TEXT, whatever its case."
  (loop for at = (search needle text :test #'char-equal)
          then (search needle text :start2 (1+ at) :test #'char-equal)
        while at
        count t))

(defun match-nth (match needle)
  "Which of the places NEEDLE is in the match's section the match is, from 0:
how many there are in the paragraphs before its own. In a title or subtitle,
the first."
  (if (match-paragraphs match)
      (reduce #'+ (subseq (match-paragraphs match) 0 (match-index match))
              :key (lambda (text) (count-in needle text)))
      0))

(defun render-search-stamp (article series)
  (let ((in (model:article-series-of article series)))
    (ah:</>
     (span :class "search-stamp"
       (when in (render-series-stamp (model:series-code in)))))))

(defvar *search-answer* nil
  "True while an answer to a search is drawn. Its parts then say that they go
where the page has them, changing those to match.")

(defun search-part ()
  "What a part of the palette carries for htmx to put it in its place."
  (when *search-answer* "outerMorph"))

(defun search-step (state &rest changes)
  "Where a control asks for the search STATE describes with CHANGES made.
STATE is SEARCH-HREF's arguments."
  (apply #'search-href (append changes state)))

(defun render-search-chips (field state &optional counts)
  "The fields to look in, FIELD pressed. With COUNTS, each shows its matches."
  (ah:</>
   (div :class "search-chips" :id "search-chips" :hx-swap-oob (search-part)
     ;; Typing searches the field that is pressed.
     (input :type "hidden" :name "field" :value (getf state :field))
     (loop for (name value label) in *search-fields*
           collect (ah:</>
                    (button :class "search-chip" :type "button"
                      :aria-pressed (if (eq name field) "true" "false")
                      :hx-get (search-step state :field value :selected 0 :focus 0 :wide nil)
                      (span label)
                      (when counts
                        (ah:</> (span :class "n" (format nil "~d" (getf counts name)))))))))))

(defun render-search-list (results selected needle series state)
  (ah:</>
   (div :class "search-list" :id "search-list" :role "listbox" :hx-swap-oob (search-part)
     (div :class "search-list-head"
       (span (joined (count-text (length results)) "Best match first"))
       (span "Matches"))
     (loop for result in results
           for n from 0
           for article = (result-article result)
           collect (ah:</>
                    (button :class "search-item" :type "button" :role "option"
                      :id (format nil "search-item-~a" (model:article-slug article))
                      :aria-selected (if (= n selected) "true" "false")
                      ;; Up and Down press the article before and after.
                      :data-keys (cond ((= n (1- selected)) "ArrowUp")
                                       ((= n (1+ selected)) "ArrowDown"))
                      :hx-get (search-step state :selected n :focus 0 :wide nil)
                      (render-search-stamp article series)
                      (span :class "search-item-text"
                        (span :class "search-item-title"
                          (render-marked (model:article-title article) needle))
                        (span :class "search-item-facts"
                          (let ((in (model:article-series-of article series)))
                            (joined (date-text (model:article-date article))
                                    (model:article-type article)
                                    (when in (part-fraction article in))))))
                      (span :class "search-item-n" (two-digits (length (result-matches result))))))))))

(defun render-hit-map (matches focus)
  "Where the matches fall in the article, start to end; the one in focus marked."
  (let ((slots (make-array *search-slots* :initial-element nil)))
    (loop for match in matches
          for n from 0
          for slot = (min (1- *search-slots*) (floor (* (match-where match) *search-slots*)))
          do (setf (aref slots slot) (if (or (= n focus) (eq (aref slots slot) :now)) :now :on)))
    (ah:</>
     (div :class "search-map"
       (span "Hit map")
       (span :class "search-map-bars"
         (loop for slot across slots
               collect (ah:</> (i :class (case slot (:now "on now") (:on "on"))))))
       (span "Start to end")))))

(defun render-search-match (result match n focus wide needle state)
  (let* ((article (result-article result))
         (total (length (result-matches result)))
         (focused (= n focus))
         (body (eq (match-field match) :body))
         (index (match-index match))
         (before (and body (plusp index) (nth (1- index) (match-paragraphs match))))
         (after (and body (nth (1+ index) (match-paragraphs match))))
         (wide (and focused wide)))
    (ah:</>
     (div :class (ah:clsx "search-match" (when focused "on"))
       :id (format nil "search-match-~d" n)
       ;; Left and Right (and Shift+Tab and Tab) press the match before and
       ;; after. The match in focus takes the keys that have nothing to
       ;; press, so they don't do what they would in the page: Tab at
       ;; either end, and Shift+Space where there is no more text to show.
       :data-keys (cond ((= n (1- focus)) "ArrowLeft ShiftTab")
                        ((= n (1+ focus)) "ArrowRight Tab")
                        (focused (format nil "~{~a~^ ~}"
                                         (remove nil (list (when (zerop n) "ShiftTab")
                                                           (when (= n (1- total)) "Tab")
                                                           (unless body "ShiftSpace"))))))
       :hx-get (unless focused (search-step state :focus n :wide nil))
       (div :class "search-match-where"
         (span :class "search-match-field"
           (case (match-field match)
             (:title "Title")
             (:subtitle "Subtitle")
             (t (format nil "Body  §~a" (two-digits (match-number match))))))
         (when (and body (model:section-title (match-section match)))
           (ah:</>
            (span :class "search-match-section" (model:section-title (match-section match)))))
         (span :class "search-match-n" (format nil "~a / ~a" (two-digits (1+ n)) (two-digits total))))
       ;; The paragraphs either side come along with the match in focus.
       (when (and focused before)
         (ah:</> (p :class "search-match-near" (if wide before (tail-text before)))))
       (p :class (ah:clsx "search-match-text" (when (eq (match-field match) :title) "title"))
         (render-marked (if (or wide (not body))
                            (match-text match)
                            (near-text (match-text match) (match-at match) needle))
                        needle))
       (when (and focused after)
         (ah:</> (p :class "search-match-near" (if wide after (head-text after)))))
       (when focused
         (ah:</>
          (div :class "search-match-actions"
            ;; A link like any other, to the match's section. What it
            ;; carries is for magazine-search.js, which finds the match in
            ;; the page once it has arrived, and marks it.
            (a :class "search-key on" :href (match-href article match)
              ;; Its page takes the body's place, as any link's does, where
              ;; the palette's own answers go nowhere.
              :hx-swap "innerHTML" :data-keys "Enter"
              :data-find needle :data-nth (format nil "~d" (match-nth match needle))
              (joined "Enter" "Open at this match"))
            (when body
              (ah:</>
               (button :class "search-key" :type "button" :data-keys "ShiftSpace"
                 :hx-get (search-step state :wide (not wide))
                 (joined "Shift Space" (if wide "Less context" "More context"))))))))))))

(defun render-search-preview (result focus wide needle series state &key note)
  "The selected article and every match in it, or NOTE when there is none."
  (ah:</>
   (div :class "search-preview" :id "search-preview" :aria-live "polite"
     :hx-swap-oob (search-part)
     ;; magazine-search.js starts a new article from its top.
     :data-article (when result (model:article-slug (result-article result)))
     (if (null result)
         (ah:</> (p :class "search-note" note))
         (let* ((article (result-article result))
                (matches (result-matches result))
                (in (model:article-series-of article series)))
           (ah:</>
            (<>
              (div :class "search-facts"
                (render-search-stamp article series)
                (span :class "search-facts-text"
                  (joined (when in (model:series-code in))
                          (model:article-code article)
                          (date-text (model:article-date article))
                          (model:article-type article)
                          (when in (part-fraction article in))))
                (span :class "search-facts-n"
                  (format nil "Match ~a of ~a" (two-digits (1+ focus)) (two-digits (length matches)))))
              (h2 :class "search-title" (render-marked (model:article-title article) needle))
              (render-hit-map matches focus)
              (loop for match in matches
                    for n from 0
                    collect (render-search-match result match n focus wide needle state)))))))))

(defun render-search-status (&optional results)
  (ah:</>
   (span :class "search-status" :id "search-status" :hx-swap-oob (search-part)
     (when results
       (joined (count-text (length results))
               (let ((matches (reduce #'+ results
                                      :key (lambda (result) (length (result-matches result))))))
                 (format nil "~d match~:[es~;~]" matches (= matches 1))))))))

(defun search-parts (articles series &key query field selected focus wide)
  "The palette's chips, list, preview and status for a search: QUERY in FIELD,
with article SELECTED chosen and match FOCUS of it in focus, WIDE to show
more of the text around that match."
  (let* ((needle (string-trim " " (or query "")))
         (found (or (find field *search-fields* :key #'second :test #'equal)
                    (first *search-fields*)))
         (field (first found)))
    (flet ((state (&optional (selected 0) (focus 0) wide)
             (list :query needle :field (second found)
                   :selected selected :focus focus :wide wide)))
      (if (< (length needle) *search-shortest*)
          (list (render-search-chips field (state))
                (render-search-list nil 0 "" series (state))
                (render-search-preview
                 nil 0 nil "" series (state)
                 :note "Type to search every article: titles, subtitles and body text.")
                (render-search-status))
          (multiple-value-bind (results counts) (search-articles articles needle field)
            (let* ((selected (max 0 (min (or selected 0) (1- (length results)))))
                   (result (nth selected results))
                   (focus (if result
                              (max 0 (min (or focus 0) (1- (length (result-matches result)))))
                              0))
                   (state (state selected focus wide)))
              (list (render-search-chips field state counts)
                    (render-search-list results selected needle series state)
                    (render-search-preview result focus wide needle series state
                                           :note (format nil "Nothing matches “~a”." needle))
                    (render-search-status results))))))))

(defun render-search-results (articles series &rest state)
  "What /magazine/search answers with: the parts of the palette that a search
changes, each under the id it has in the page, for htmx to change the page's
to match."
  (let ((*search-answer* t))
    (ah:render-to-string
     (ah:</> (<> (apply #'search-parts articles series state))))))

(defun render-search-palette ()
  "The palette, closed and empty, as every page carries it."
  (destructuring-bind (chips list preview status) (search-parts nil nil)
    (ah:</>
     ;; Closed until magazine-search.js opens it. Nothing a search fetches
     ;; goes where its control is: each part of the answer says where it
     ;; goes. A search asked for while another is on its way takes its place.
     (div :class "search" :id "search" :role "dialog" :aria-modal "true" :aria-label "Search"
       :|hx-swap:inherited| "none" :|hx-sync:inherited| "#search:replace"
       (div :class "search-scrim" :data-search "close")
       (div :class "search-frame"
         (div :class "search-palette"
           ;; Typing searches, once it has paused. Enter is the match in
           ;; focus's to take; with none, it searches at once.
           (form :class "search-field" :id "search-form"
             :hx-get (search-href)
             :hx-trigger "input changed delay:150ms from:#search-input, submit"
             (label :class "search-label" :for "search-input" "Search")
             (input :class "search-input" :id "search-input" :type "text" :name "q"
               :autocomplete "off" :spellcheck "false" :aria-controls "search-list")
             chips
             (button :class "search-key" :type "button" :data-search "close" "Esc"))
           (div :class "search-panes"
             list
             preview)
           (div :class "search-keys"
             (span (joined "Up / Down  Article" "Left / Right  Match" "Enter  Open at match"
                           "Esc  Close"))
             status)))))))
