(in-package #:magazine/hypermedia/components)

;;;; The article page (Figma: Article restyled · C1 · Landing).
;;;
;;; The homepage's sheet. The head band holds the lede, and beside it the
;;; book and the contents. Each section is rows of two cells, the text and
;;; the notes that belong to it: one row under the section's heading, then
;;; one for each subheading. A contents strip pins to the top of the window once
;;; the lede has scrolled away (static/js/magazine-toc.js). The end band
;;; lists the rest of the series, or more articles to read.
;;;
;;; An evolving article (#+LAYOUT: evolving) is the same page with other
;;; sections: see evolving.lisp.

(defun previous-part (article series)
  "The part before ARTICLE, when it is out."
  (let ((part (find (1- (model:article-part article)) (model:series-parts series)
                    :key #'model:part-number)))
    (when (and part (model:part-article part))
      part)))

(defun render-previously (article series)
  "The recap of the part before ARTICLE (Figma: Lede D · Banner + track
recap): its number small and pale beside its title, then ARTICLE's #+RECAP,
or the part's dek when ARTICLE has none. The banner above the lede is this
part, in black; the recap is kept quiet so the two don't read alike."
  (let ((part (previous-part article series)))
    (when part
      (let* ((previous (model:part-article part))
             (number (two-digits (model:part-number part)))
             (href (article-href previous)))
        (ah:</>
         (div :class "previously"
           (p :class "previously-label" (format nil "Previously, in part ~a" number))
           (div :class "previously-body"
             (span :class "previously-n" :aria-hidden t number)
             (p :class "previously-title" (a :href href (model:article-title previous)))
             (p :class "previously-recap"
               (render-inlines (or (model:article-recap article)
                                   (model:article-subtitle previous))))
             (a :class "previously-link" :href href (format nil "Return to part ~a" number)))))))))

(defun render-article-lede (article series)
  "The lede. A series part has the series' banner across its top, as the
homepage's lead does, with this part's number and the meter filled to it."
  (let* ((title (render-headline (model:article-title-lines article) :class "art-headline"))
         (row (render-lede-row article :hazard))
         (recap (when series (render-previously article series)))
         (lede (ah:</>
                (header :class "neo-lead art-lede"
                  (render-crop-marks)
                  ;; A series part with a part before it closes with a recap
                  ;; of that part, under the dek.
                  (if recap
                      (ah:</> (<> title row (render-hatch) recap))
                      (ah:</> (<> title (render-hatch) row)))))))
    (if series
        (ah:</>
         (div :class "lead-cell"
           (render-featured-banner article series
                                   :label "Series" :through (model:article-part article))
           lede))
        lede)))

;;; Contents: the sections, or in a series every part with this one's
;;; sections under it.

(defun render-contents-row (number title &key href (class "contents-row"))
  (let ((inner (ah:</> (<> (span :class "contents-n" number) (span :class "contents-t" title)))))
    (ah:</>
     (li (if href
             (ah:</> (a :class class :href href inner))
             (ah:</> (span :class class inner)))))))

(defun render-section-rows (article &key (class "contents-row"))
  (loop for section in (model:article-sections article)
        for n from 1
        collect (render-contents-row (princ-to-string n) (model:section-title section)
                                     :href (format nil "#~a" (model:section-id section))
                                     :class class)))

(defun render-series-contents (article series)
  (mapcar (lambda (part)
            (let ((other (model:part-article part))
                  (number (two-digits (model:part-number part)))
                  (title (model:part-title part)))
              (cond ((eq other article)
                     (ah:</>
                      (<>
                        (render-contents-row number title :class "contents-row current")
                        (render-section-rows article :class "contents-row sub"))))
                    (other
                     (render-contents-row number title :href (article-href other)))
                    (t
                     (render-contents-row number title :class "contents-row queued")))))
          (model:series-parts series)))

(defun render-article-contents (article series)
  (ah:</>
   (nav :class (ah:clsx "contents-panel" (when series "in-series")) :aria-label "Contents"
     (render-crop-marks '("tr" "bl"))
     (p :class "contents-title"
       (span (if series "In this series" "Contents"))
       (span :class "contents-folio"
         (if series
             (part-of article series)
             (format nil "~d sections" (length (model:article-sections article))))))
     (ol :class "contents-list"
       (if series
           (render-series-contents article series)
           (render-section-rows article)))
     (render-hatch))))

;;; Sections

(defun render-piece-heading (section number total)
  "A section's heading: its place among the sections as an eyebrow and as a
bar, and its number drawn large behind."
  (ah:</>
   (header :class "piece-heading"
     (span :class "ghost-numeral" :aria-hidden t (two-digits number))
     (render-eyebrow (format nil "Section ~a / ~a" (two-digits number) (two-digits total))
                     :marks t)
     (h2 :class "piece-name" (press:render-lines (model:section-title-lines section)))
     (div :class "progress-rule"
       (render-progress-bar (/ number total))
       (render-rule-dots)))))

(defun render-subhead (text number sub)
  (ah:</>
   (div :class "subhead-group"
     (span :class "dash")
     (render-eyebrow (format nil "Section ~d.~d" number sub))
     (h3 :class "subhead" text))))

(defun render-piece (section number total)
  "SECTION, the NUMBERth of TOTAL: its heading, then its blocks in one column,
each note where it is written."
  (let ((sub 0))
    (ah:</>
     (section :class "neo-piece" :id (model:section-id section)
       (div :class "piece-row"
         (div :class "piece-cell piece-text"
           (render-piece-heading section number total)
           (div :class "piece-body"
             (mapcar (lambda (block)
                       (if (eq (first block) :h)
                           (render-subhead (second block) number (incf sub))
                           (render-block block)))
                     (model:section-blocks section)))))
       (render-marks (evenp number))))))

(defun render-toc-strip (article series)
  "The contents strip that pins to the top of the window while reading. Each
cell's number carries the count of sections, for layouts that show both."
  (ah:</>
   (div :class "toc-dock" :aria-hidden "true"
     (nav :class "toc-strip"
       (a :class "toc-article" :href "#top" :tabindex "-1"
         (when series (render-series-stamp (model:series-code series)))
         (span :class "toc-article-text"
           (span :class "toc-article-label"
             (if series
                 (joined (model:series-code series) (part-fraction article series))
                 (model:article-code article)))
           (span :class "toc-article-title" (model:article-title article))))
       (loop for section in (model:article-sections article)
             for n from 1
             collect (ah:</>
                      (a :class "toc-cell" :tabindex "-1"
                        :href (format nil "#~a" (model:section-id section))
                        :data-section (model:section-id section)
                        ;; Shown on hover where a cell has no room for it.
                        :title (model:section-title section)
                        (span :class "strip-n"
                          :data-total (two-digits (length (model:article-sections article)))
                          (two-digits n))
                        (span :class "strip-t" (model:section-title section))
                        (span :class "strip-bar"))))))))

;;; The end of an article: End transmission, a black head, then the end band.
;;; The head says what comes next: Continue when there is something to read
;;; now, Standby when the next part isn't out, Complete after the last part.

(defun render-article-end (title meta content &key jp action action-href)
  (ah:</>
   (<>
     (render-end-transmission)
     (render-section-head "end" title meta :jp jp :action action :action-href action-href
                                           :gap nil)
     (render-end-band nil content))))

(defun render-keep-reading (article articles series)
  (let ((book (model:article-book-title article))
        (others (model:keep-reading-articles article articles)))
    (render-article-end
     "Continue..."
     (format nil "~d brief~:p" (+ (if book 1 0) (length others)))
     (ah:</>
      (div :class "keep-briefs"
        (when book (render-book-brief article))
        (mapcar (lambda (other) (render-article-brief other (model:article-series-of other series)))
                others)))
     :jp "briefing" :action "Full archive  >>>" :action-href (archive-href))))

(defun render-series-part-cell (part article series)
  (let ((other (model:part-article part))
        (here (eq (model:part-article part) article)))
    (render-index-cell (two-digits (model:part-number part))
                       (model:part-title part)
                       (cond (here (joined (model:series-code series) "You are here"))
                             (other (joined (model:article-code other)
                                            (press:format-date (model:article-date other) :day-month)))
                             (t "Queued"))
                       :class (cond (here "current") ((null other) "queued"))
                       :href (when (and other (not here)) (article-href other)))))

(defun render-series-end (article series)
  (let* ((next (find (1+ (model:article-part article)) (model:series-parts series)
                     :key #'model:part-number))
         (next-article (and next (model:part-article next))))
    (render-article-end
     (cond ((null next) "Complete")
           (next-article "Continue...")
           (t "Standby..."))
     (part-of article series)
     (render-index
      :top (ah:</>
            (<>
              (render-series-stamp (model:series-code series))
              (span (format nil "[Series of ~d]" (model:series-total series)))))
      :name (model:series-title series)
      :body (ah:</>
             (<>
               (p :class "series-summary" (model:series-summary series))
               (render-series-progress series)))
      :cells (mapcar (lambda (part) (render-series-part-cell part article series))
                     (model:series-parts series)))
     :jp "series"
     :action (cond ((null next) "Last part")
                   (next-article (format nil "Next: part ~d  >>>" (model:part-number next)))
                   (t (format nil "Next: part ~d, queued" (model:part-number next))))
     :action-href (when next-article (article-href next-article)))))

(defun page-article (article articles series)
  (let ((in-series (model:article-series-of article series))
        (sections (model:article-sections article))
        (evolving (eq (model:article-layout article) :evolving))
        (*marked-bands* 0))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title (format nil "~a — Almighty Lisp" (model:article-title article))
        :description (model:article-subtitle article)
        :page-class (ah:clsx "page-article" (when evolving "page-evolving")) :sheet-id "top"
        :scripts (ah:</>
                  (<>
                    (script :src (asset "js/magazine-toc.js") :defer t)
                    (when evolving
                      (ah:</> (script :src (asset "js/magazine-evolving.js") :defer t)))))
        (ac-band :class "art-head-band" :marks :foot
          :side (list (joined (model:article-code article)
                              (date-text (model:article-date article)))
                      (if in-series
                          (joined "Series" (model:series-code in-series)
                                  (part-of article in-series))
                          (joined "Contents" (format nil "~d sections" (length sections)))))
          (div :class "art-head"
            (render-article-lede article in-series)
            (div :class "art-side"
              (render-book-ad)
              (render-article-contents article in-series))))
        (ac-band :class "pieces-band"
          :side (list (joined "Almighty Lisp" "Continuous operation" "Live image system")
                      (joined "Common Lisp" "Emacs" "Sly" "REPL: connected"))
          (div :class "piece-list"
            (loop for section in sections
                  for number from 1
                  collect (if evolving
                              (render-evolving-piece section)
                              (render-piece section number (length sections))))))
        (when sections (render-toc-strip article in-series))
        (if in-series
            (render-series-end article in-series)
            (render-keep-reading article articles series)))))))
