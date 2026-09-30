(in-package #:magazine/hypermedia/components)

;;;; Cards: briefs, the book panel, and series.

;;; Briefs: the card an article, or a book chapter, is listed with.

(defun render-brief (&key href class strip code title dek by)
  (ah:</>
   (a :class (ah:clsx "neo-brief" class) :href href
     strip
     (span :class "neo-brief-head"
       (img :class "neo-brief-mark" :src (image-src "Neo/Neo/tiny-almighty.svg") :alt ""
         :width "18" :height "20")
       (span :class "neo-brief-dots" :aria-hidden t)
       (span :class "neo-brief-code" code))
     (span :class "neo-brief-title" title)
     (span :class "neo-brief-dek" dek)
     (span :class "neo-brief-by" by))))

(defun render-article-brief (article &optional series)
  "ARTICLE's brief. With SERIES, a black strip across the top names its part."
  (render-brief :href (article-href article)
                :class (when series "in-series")
                :strip (when series
                         (ah:</>
                          (span :class "series-strip"
                            (render-series-stamp (model:series-code series) :tone "paper")
                            (span (joined (model:series-code series)
                                          (part-fraction article series))))))
                :code (model:article-code article)
                :title (press:render-lines (model:article-title-lines article))
                :dek (model:article-subtitle article)
                :by (model:article-author article)))

(defun render-book-brief (article)
  "The book chapter ARTICLE points to, as a brief."
  (render-brief :href (model:article-book-href article)
                :code (format nil "Chapter ~a" (model:article-book-chapter article))
                :title (model:article-book-title article)
                :dek (model:article-book-summary article)
                :by "Lisp & Emacs Essentials"))

;;; The book panel

(defparameter *book-specs*
  '(("Lisp" "Macros, CLOS, conditions")
    ("Emacs" "Doom Emacs, Sly, structural editing")
    ("Audience" "Lisp-curious, veteran programmers")
    ("Setup" "macOS, Linux, Windows")
    ("Format" "Free online, PDF $5, print $50")
    ("# of chapters" "16"))
  "Rows of the book panel's spec list.")

(defun render-book-ad (&key compact)
  "The book panel beside the lead. COMPACT, beside an article, drops the
pitch and the spec list. htmx leaves its links alone: they leave the magazine."
  (ah:</>
   (aside :class (ah:clsx "book-ad" (when compact "compact"))
     :aria-label "Lisp & Emacs Essentials" :hx-boost "false"
     (p :class "book-ad-label" "The Book (your eyes only)")
     (img :class "book-ad-logo" :src (image-src "essentials-logo.svg") :alt "Lisp & Emacs Essentials"
       :width "156" :height "69")
     (unless compact
       (ah:</>
        (<>
          (div :class "book-ad-pitch"
            (p "Common Lisp is a language optimized for ultimate adaptability. It’s a generalist’s secret weapon; a programming language that isn’t a master at anything, but is quite capable at doing everything.")
            (p "With the rise of LLMs and a rapidly changing software industry, it’s easy to feel anxious about your own future as a software developer.")
            (p "But you don’t need to worry. You need to adapt. Common Lisp is an almighty programming language that enables and even summons its users to become almighty. The macros are waiting. The REPL is loaded. The buffers and windows are at your command.")
            (p "Be not defeated by the rapidly shifting winds of code and craft. Embrace the piercing light of destiny, beaming from the flaming horizon over an effervescent ocean of functions, classes, and parentheses. Become Almighty."))
          (render-spec *book-specs* :class "spec book-spec"))))
     (div :class "book-ad-actions"
       (a :class "neo-button primary" :href *book-href* "Start reading")
       (a :class "neo-button secondary" :href *hardcover-href* "Purchase hardcover")))))

;;; The book band closes a narrow page. Where the panel has no column of its
;;; own, this replaces it: the logo and a line, then both buttons.

(defun render-book-band ()
  (ah:</>
   (ac-band :class "book-band"
     (aside :class "book-strip" :aria-label "Lisp & Emacs Essentials" :hx-boost "false"
       (div :class "book-strip-copy"
         (img :class "book-ad-logo" :src (image-src "essentials-logo.svg") :alt "Lisp & Emacs Essentials"
           :width "156" :height "69")
         (p :class "book-strip-line"
           (span :class "book-ad-label" "The Book (your eyes only)")
           (span :class "book-strip-text" "Be not defeated by the rapidly shifting winds of code and craft. Become Almighty.")))
       (div :class "book-ad-actions"
         (a :class "neo-button primary" :href *book-href* "Start reading")
         (a :class "neo-button secondary" :href *hardcover-href* "Purchase hardcover"))))))

;;; Series progress

(defun render-meter (series)
  "One square per part, filled when the part is out."
  (ah:</>
   (span :class "meter" :aria-hidden t
     (mapcar (lambda (part)
               (ah:</> (span :class (ah:clsx "cell" (when (model:part-article part) "on")))))
             (model:series-parts series)))))

(defun progress-text (series)
  (let ((out (model:series-published series))
        (total (model:series-total series)))
    (if (= out total)
        (format nil "Complete, ~d parts" total)
        (format nil "~d of ~d published" out total))))

(defun render-series-progress (series)
  (ah:</>
   (p :class "series-progress"
     (render-meter series)
     (span (progress-text series)))))

(defun render-featured-banner (article series)
  "Across the top of the lead when the lead is part of the featured series."
  (ah:</>
   (div :class "featured-banner"
     (render-series-logo (model:series-code series) :class "series-logo banner-logo")
     (div :class "featured-name"
       (p :class "featured-label" (joined "Featured series" (model:series-code series)))
       (p :class "featured-title" (model:series-title series)))
     (div :class "featured-part"
       (render-meter series)
       (span :class "featured-count" (part-fraction article series))))))

;;; Series cards

(defun part-window (series)
  "Items to list on a card: (:part part), (:before count) or (:after shown total).
A long series shows its newest part with two before it and one after, and
folds the rest into summary rows."
  (let* ((parts (model:series-parts series))
         (total (length parts))
         (latest (or (position-if #'model:part-article parts :from-end t) 0)))
    (if (<= total 7)
        (mapcar (lambda (part) (list :part part)) parts)
        (let ((start (max 0 (- latest 2)))
              (end (min total (+ latest 2))))
          (append
           (when (plusp start)
             (list (list :before start)))
           (mapcar (lambda (part) (list :part part)) (subseq parts start end))
           (when (< end total)
             (list (list :after end total))))))))

(defun render-part-row (number title &key class href state new)
  (ah:</>
   (li :class (ah:clsx "part-row" class)
     (span :class "part-n" number)
     (if href
         (ah:</> (a :class "part-t" :href href title))
         (ah:</> (span :class "part-t" title)))
     (when state (ah:</> (span :class "part-state" state)))
     (when new (ah:</> (span :class "part-new" "New"))))))

(defun render-window-item (item latest)
  "One item from PART-WINDOW. LATEST, the newest part out, is marked New."
  (destructuring-bind (kind &rest args) item
    (ecase kind
      (:before
       (let ((count (first args)))
         (render-part-row (format nil "01–~a" (two-digits count))
                          (format nil "~d earlier parts" count)
                          :class "summary")))
      (:after
       (destructuring-bind (shown total) args
         (render-part-row (format nil "~a–~a" (two-digits (1+ shown)) (two-digits total))
                          (format nil "~d more parts queued" (- total shown))
                          :class "summary queued" :state "Queued")))
      (:part
       (let* ((part (first args))
              (article (model:part-article part))
              (number (two-digits (model:part-number part)))
              (title (model:part-title part)))
         (cond ((null article)
                (render-part-row number title :class "queued" :state "Queued"))
               ((eq part latest)
                (render-part-row number title :class "newest" :href (article-href article) :new t))
               (t
                (render-part-row number title :href (article-href article)))))))))

(defun render-series-card (series featured)
  (let ((code (model:series-code series))
        (latest (find-if #'model:part-article (model:series-parts series) :from-end t)))
    (ah:</>
     (article :class (ah:clsx "series-card" (when featured "featured"))
       (div :class "series-bar"
         (render-series-stamp code :tone "paper")
         (span (if featured (joined code "Featured") code)))
       (div :class "series-body"
         (div :class "series-title-row"
           (h3 :class "series-title" (model:series-title series))
           (render-series-logo code :class "series-logo card-logo"))
         (p :class "series-summary" (model:series-summary series))
         (render-series-progress series)
         (ol :class "series-parts"
           (mapcar (lambda (item) (render-window-item item latest))
                   (part-window series)))
         (render-hatch))))))

;;; Indexes: a head cell on the left, numbered cells on the right, filled
;;; column by column. The end of a series lists its parts in one; the
;;; archive lists each month in one.

(defun render-index-cell (number title meta &key class href)
  (let ((class (ah:clsx "index-cell" class))
        (inner (ah:</>
                (<>
                  (span :class "index-cell-n" number)
                  (span :class "index-cell-text"
                    (span :class "index-cell-title" title)
                    (span :class "index-cell-meta" meta))))))
    (if href
        (ah:</> (a :class class :href href inner))
        (ah:</> (div :class class inner)))))

(defun render-index (&key id class top name body cells)
  "TOP is the head's label row, NAME its title, BODY what follows; CELLS
are INDEX-CELLs. An odd cell out is paired with hatching."
  (ah:</>
   (section :class (ah:clsx "index" class) :id id
     (div :class "index-head"
       (h2 :class "index-name" name)
       body)
     (div :class "index-cells" :style (format nil "--rows: ~d" (ceiling (length cells) 2))
       cells
       (when (oddp (length cells))
         (ah:</> (span :class "index-fill" :aria-hidden t)))))))
