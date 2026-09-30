(in-package #:magazine/hypermedia/components)

;;;; The homepage (Figma: Neoindustrial / Homepage (v20)).
;;;
;;; The plate: the lead article beside the book panel. Then the Series
;;; section, the running series as cards, and the Latest section: the next
;;; articles as briefs beside the archive column.

(defun render-lede-row (article extra)
  "The dek beside the byline, under a headline. EXTRA closes the byline column."
  (ah:</>
   (div :class "neo-lead-row"
     (p :class "neo-dek" (render-inlines (model:article-subtitle article)))
     (div :class "neo-lead-meta"
       (render-spec (list (list "By" (model:article-author article))
                          (list "Serial" (model:article-code article))
                          (if (eq extra :cta)
                              (list "Status" "[Activated]")
                              (list "Activated" (date-text (model:article-date article)))))
                    :class "spec byline-spec")
       (if (eq extra :cta)
           (ah:</> (a :class "neo-cta" :href (article-href article) "Read the article"))
           (ah:</> (span :class "hazard-plate" :aria-hidden t)))))))

(defun render-lead (article)
  (ah:</>
   (article :class "neo-lead"
     (render-crop-marks)
     (render-headline (model:article-title-lines article) :href (article-href article))
     (render-hatch)
     (render-lede-row article :cta))))

(defun render-lead-cell (lead series featured)
  "The lead, under the featured-series banner when it is part of that series."
  (let ((lead-series (when lead (model:article-series-of lead series))))
    (ah:</>
     (div :class "lead-cell"
       (when (and lead-series (eq lead-series featured))
         (render-featured-banner lead lead-series))
       (if lead
           (render-lead lead)
           (ah:</> (p :class "neo-lead" "No articles have been published yet.")))))))

(defun plate-labels (lead series featured)
  (let ((lead-series (when lead (model:article-series-of lead series))))
    (list (cond ((null lead) "Almighty Lisp")
                ((and lead-series (eq lead-series featured))
                 (list (format nil "  ■  ~a" (joined (model:series-code lead-series)
                                                     (part-of lead lead-series)))
                       "omona-series"))
                (t (joined "Latest Brief" (model:article-code lead)
                           (date-text (model:article-date lead)))))
          (joined "Flight Manual" "Lisp & Emacs Essentials"))))

(defun render-series-section (running featured)
  (ah:</>
   (<>
     (render-section-head "series" "Series"
                          (joined (format nil "~d running" (length running))
                                  (when featured
                                    (format nil "~a featured" (model:series-code featured))))
                          :jp "series")
     (ac-band :class "series-band"
       :side (list (joined "Series" (format nil "~d running" (length running)))
                   (apply #'joined (mapcar #'model:series-code running)))
       (div :class "series-cards"
         (mapcar (lambda (one) (render-series-card one (eq one featured))) running))))))

;;; The archive column: recent months in full, older months as links.

(defun render-archive-line (article series)
  (ah:</>
   (li
     (a :class "arch-line" :href (article-href article)
       (span :class "arch-when" (press:format-date (model:article-date article) :day-month))
       (span :class "arch-mark"
         (when series
           (ah:</>
            (<>
              (render-series-stamp (model:series-code series))
              (span :class "arch-code" (model:series-code series))))))
       (span :class "arch-name" (model:article-title article))))))

(defun render-column-month (heading count lines)
  (ah:</>
   (section :class "arch-month"
     (h3 :class "arch-month-head"
       (span heading)
       (span :class "arch-count" count))
     (ul :class "arch-lines" lines))))

(defun render-archive-column (layout series)
  (ah:</>
   (nav :class "archive-col" :aria-label "Archive"
     (mapcar (lambda (month)
               (destructuring-bind (date shown members) month
                 (render-column-month
                  (press:format-date date :month)
                  (format nil (if (plusp shown) "~d more" "~d articles") (length members))
                  (mapcar (lambda (article)
                            (render-archive-line article (model:article-series-of article series)))
                          members))))
             (getf layout :months))
     (when (getf layout :earlier)
       (render-column-month
        "Earlier months"
        (format nil "Since ~a" (press:format-date (getf layout :since) :month))
        (mapcar (lambda (month)
                  (destructuring-bind (date count) month
                    (ah:</>
                     (li
                       (a :class "arch-line month" :href (archive-href date)
                         (span :class "arch-name" (press:format-date date :month))
                         (span :class "arch-count" (format nil "~d  >>>" count)))))))
                (getf layout :earlier))))
     (render-hatch))))

(defun render-latest-section (layout series)
  (let ((column (when (or (getf layout :months) (getf layout :earlier))
                  (render-archive-column layout series)))
        (since (getf layout :since)))
    (ah:</>
     (<>
       (render-section-head "latest" "Briefs" "Newest first" :jp "briefing"
                                                             :action "Full archive  >>>" :action-href (archive-href))
       (ac-band :class "latest-band"
         :side (list (joined "Latest" (format nil "~d briefs" (length (getf layout :latest))))
                     (joined "Archive" (when since
                                         (format nil "Since ~a" (press:format-date since :month)))))
         (div :class (ah:clsx "latest-grid" (unless column "no-archive"))
           (div :class "latest-briefs"
             (mapcar (lambda (article)
                       (render-article-brief article (model:article-series-of article series)))
                     (getf layout :latest)))
           column))))))

(defun page-home (articles series)
  (let* ((layout (model:homepage-layout articles series))
         (lead (getf layout :lead))
         (featured (getf layout :featured)))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title "Almighty Lisp"
        (ac-band :class "plate-band" :side (plate-labels lead series featured)
          (div :class "neo-plate"
            (render-lead-cell lead series featured)
            (render-book-ad)))
        (when (getf layout :latest)
          (render-latest-section layout series))
        (when (getf layout :series)
          (render-series-section (getf layout :series) featured))
        (render-book-band))))))
