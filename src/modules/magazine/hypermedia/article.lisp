(in-package #:magazine/hypermedia/components)

;;;; The article page (Figma: Neoindustrial / Article (v18), Article (v18 · Series)).
;;;
;;; The homepage's sheet. The head band holds the lede, and beside it the
;;; book and the contents. Each section is a row of three cells: heading,
;;; text, margin notes. A contents strip pins to the top of the window once
;;; the lede has scrolled away (static/js/magazine-toc.js). The end band
;;; lists the rest of the series, or more articles to read.

(defun render-series-tags (article series)
  (ah:</>
   (div :class "series-tags"
     (span :class "series-tag outline" (format nil "[ Series: ~a ]" (model:series-title series)))
     (span :class "series-tag filled" (part-of article series))
     (span :class "series-tag slashes" :aria-hidden t "//////////////"))))

(defun previous-part (article series)
  "The part before ARTICLE, when it is out."
  (let ((part (find (1- (model:article-part article)) (model:series-parts series)
                    :key #'model:part-number)))
    (when (and part (model:part-article part))
      part)))

(defun render-previously (article series)
  (let ((part (previous-part article series)))
    (when part
      (let ((previous (model:part-article part)))
        (ah:</>
         (div :class "previously"
           (p :class "previously-label" (format nil "Previously, in part ~d" (model:part-number part)))
           (p :class "previously-recap"
             (a :class "inline-link" :href (article-href previous) (model:article-title previous))
             (format nil ". ~a" (model:article-subtitle previous)))))))))

(defun render-article-lede (article series)
  (let ((title (render-headline (model:article-title-lines article) :class "art-headline"))
        (tags (when series (render-series-tags article series)))
        (row (render-lede-row article :hazard))
        (recap (when series (render-previously article series))))
    (ah:</>
     (header :class "neo-lead art-lede"
       (render-crop-marks)
       ;; A series part with a part before it closes with a recap of that
       ;; part, under the dek.
       (if recap
           (ah:</> (<> title tags row (render-hatch) recap))
           (ah:</> (<> title tags (render-hatch) row)))))))

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

(defun render-piece (section number)
  (let ((blocks (model:section-blocks section)))
    (ah:</>
     (section :class "neo-piece" :id (model:section-id section)
       (div :class "piece-row"
         (div :class "piece-cell piece-head"
           (p :class "piece-number" (princ-to-string number))
           (h2 :class "piece-name" (press:render-lines (model:section-title-lines section)))
           (render-hatch))
         (div :class "piece-cell piece-text"
           (mapcar #'render-block blocks))
         (div :class "piece-cell piece-margin"
           (mapcar #'render-margin-note
                   (remove :note blocks :key #'first :test-not #'eq))
           (render-hatch)))))))

(defun render-toc-strip (article series)
  "The contents strip that pins to the top of the window while reading."
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
                        (span :class "strip-n" (two-digits n))
                        (span :class "strip-t" (model:section-title section))
                        (span :class "strip-bar"))))))))

;;; The end band

(defun render-keep-reading (article articles series)
  (render-end-band
   "Keep reading"
   (ah:</>
    (div :class "keep-briefs"
      (when (model:article-book-title article) (render-book-brief article))
      (mapcar (lambda (other) (render-article-brief other (model:article-series-of other series)))
              (model:keep-reading-articles article articles))))))

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
  (let ((next (find (1+ (model:article-part article)) (model:series-parts series)
                    :key #'model:part-number)))
    (render-end-band
     (joined "Series" (model:series-title series))
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
     :dark t
     :folio (cond ((null next) "Last part")
                  ((model:part-article next) (format nil "Next: part ~d" (model:part-number next)))
                  (t (format nil "Next: part ~d, queued" (model:part-number next)))))))

(defun page-article (article articles series)
  (let ((in-series (model:article-series-of article series))
        (sections (model:article-sections article)))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title (format nil "~a — Almighty Lisp" (model:article-title article))
        :description (model:article-subtitle article)
        :page-class "page-article" :sheet-id "top" 
        :scripts (ah:</> (script :src (asset "js/magazine-toc.js") :defer t))
        (ac-band :class "art-head-band"
          :side (list (joined (model:article-code article)
                              (date-text (model:article-date article)))
                      (if in-series
                          (joined "Series" (model:series-code in-series)
                                  (part-of article in-series))
                          (joined "Contents" (format nil "~d sections" (length sections)))))
          (div :class "art-head"
            (render-article-lede article in-series)
            (div :class "art-side"
              (render-book-ad :compact t)
              (render-article-contents article in-series))))
        (ac-band :class "pieces-band"
          :side (list (joined "Almighty Lisp" "Continuous operation" "Live image system")
                      (joined "Common Lisp" "Emacs" "Sly" "REPL: connected"))
          (div :class "piece-list"
            (loop for section in sections
                  for number from 1
                  collect (render-piece section number))))
        (when sections (render-toc-strip article in-series))
        (if in-series
            (render-series-end article in-series)
            (render-keep-reading article articles series))
        (render-book-band))))))
