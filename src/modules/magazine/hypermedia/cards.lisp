(in-package #:magazine/hypermedia/components)

;;;; Cards: briefs, the book panel, and series.
;;;
;;; (Figma: Homepage (v30) for briefs and series; Book ad / C1 Landing for
;;; the book.)

;;; Briefs: the card an article, or a book chapter, is listed with.

(defun render-brief (&key href strip code title dek by)
  (ah:</>
   (a :class "neo-brief" :href href
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
  "ARTICLE's brief. With SERIES, a black chip at the top names its part, beside
a bar filled as far as that part."
  (render-brief :href (article-href article)
                :strip (when series
                         (ah:</>
                          (span :class "series-strip"
                            (render-series-stamp (model:series-code series) :tone "paper")
                            (span (slashed "Series" (model:series-code series)
                                           (part-fraction article series)))
                            (render-progress-bar (/ (model:article-part article)
                                                    (model:series-total series))))))
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

;;; The book panel: on the homepage in a band of its own, the pitch as a
;;; headline over three ways to read; beside an article, a compact one with
;;; two buttons. htmx leaves its links alone: they leave the magazine.

(defparameter *book-cards*
  `((,*book-href* "Free" "Read free online" "All 16 chapters, in your browser.")
    (,*ebook-href* "PDF / $5" "Buy the ebook" "All 16 chapters, to keep and read offline.")
    (,*hardcover-href* "Print / $50" "Buy the hardcover" "The whole book, on your desk."))
  "Address, price, title and line of each of the book band's cards.")

(defun render-dot-grid (index)
  "A card's corner icon, a 3 by 3 grid of dots with every other one dimmed:
all nine on the first card, a different five or four on each of the others."
  (ah:</>
   (span :class "book-card-icon" :aria-hidden t
     (loop for row below 3
           append (loop for column below 3
                        when (or (zerop index) (evenp (+ row column index)))
                          collect (ah:</>
                                   (i :class (when (oddp (+ row column)) "dim")
                                     :style (format nil "left: ~dpx; top: ~dpx"
                                                    (* column 6) (* row 6)))))))))

(defun render-book-card (card index)
  "The first card is the lit one; the last, the hardcover's, is fainter and
pulses (see the card pulse in magazine-fx.js)."
  (destructuring-bind (href meta title sub) card
    (ah:</>
     (a :class (ah:clsx "book-card"
                        (cond ((zerop index) "on")
                              ((= index (1- (length *book-cards*))) "faint")))
       :href href
       (render-dot-grid index)
       (span :class "book-card-meta" meta)
       (span :class "book-card-title" title)
       (span :class "book-card-sub" sub)
       (span :class "book-card-arrow" :aria-hidden t "→")))))

(defun render-book-ring ()
  "Six dots in a ring."
  (ah:</>
   (span :class "book-ring"
     (loop for k below 6
           for angle = (* k (/ pi 3))
           collect (ah:</>
                    (i :style (format nil "left: ~,1fpx; top: ~,1fpx"
                                      (+ 14.5 (* 13 (cos angle)))
                                      (+ 14.5 (* 13 (sin angle))))))))))

(defun render-book-landing ()
  "The homepage's book panel."
  (ah:</>
   (aside :class "book-ad landing" :aria-label "Lisp & Emacs Essentials" :hx-boost "false"
     (p :class "book-ad-label" "The Book (your eyes only)")
     (img :class "book-ad-logo" :src (image-src "essentials-logo.svg") :alt "Lisp & Emacs Essentials"
       :width "156" :height "69")
     (div :class "book-ad-pitch"
       (p "Common Lisp is your secret weapon in the age of LLMs. Be not defeated by the
rapidly shifting winds of code and craft. Adapt, Build, Overcome. Embrace the parentheses. Become Almighty."))
     (div :class "book-rule" :aria-hidden t
       (render-gauge)
       (render-dot-pair)
       (render-dot-pair :alt t))
     (div :class "book-cards"
       (loop for card in *book-cards*
             for index from 0
             collect (render-book-card card index)))
     (render-dot-pairs :class "panel-corner")
     ;; Under the logo: a ring of dots and a slider.
     (div :class "book-side" :aria-hidden t
       (render-book-ring)
       (span :class "book-slider"
         (i :class "a")
         (i :class "b")
         (i :class "c")
         (i :class "d"))))))

(defun render-book-band ()
  "The book's band on the homepage, under the plate. Narrow pages don't show
it: there the book is in the footer."
  (ah:</>
   (<>
     (div :class "band gap-band book-gap" :aria-hidden t)
     (ac-band :class "book-band" :side (list "The Book" "Flight Manual") :marks t
       (render-book-landing)))))

(defun render-book-ad ()
  "The book panel beside an article: the logo and two buttons."
  (ah:</>
   (aside :class "book-ad compact" :aria-label "Lisp & Emacs Essentials" :hx-boost "false"
     (p :class "book-ad-label" "The Book (your eyes only)")
     (img :class "book-ad-logo" :src (image-src "essentials-logo.svg") :alt "Lisp & Emacs Essentials"
       :width "156" :height "69")
     (div :class "book-ad-actions"
       (a :class "neo-button primary" :href *book-href* "Start reading")
       (a :class "neo-button secondary" :href *hardcover-href* "Purchase hardcover")))))

;;; Series progress

(defun render-meter (series)
  "One cell per part, filled when the part is out. Drawn as one bar: filled to
the newest part, a tick, then the rest in outline."
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
     (render-rule-dots)
     (span (progress-text series)))))

(defun render-featured-banner (article series)
  "Across the top of the lead when the lead is part of the featured series: the
series' name and the lead's part, over the series' progress. The part's
number is drawn large behind the banner (data-part)."
  (ah:</>
   (div :class "featured-banner" :data-part (two-digits (model:article-part article))
     (render-series-logo (model:series-code series) :class "series-logo banner-logo")
     (div :class "featured-name"
       (p :class "featured-label" (slashed "Featured series" (model:series-code series)))
       (p :class "featured-title" (model:series-title series)))
     (span :class "featured-count" (part-fraction article series))
     (div :class "progress-rule"
       (render-meter series)
       (render-rule-dots)
       (span :class "arrow-label" "All parts")))))

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
         (span (if featured (slashed code "Featured") code)))
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
