(in-package #:magazine/hypermedia/components)

;;;; The archive, and the page for a slug that isn't an article.
;;;
;;; The archive is the homepage's sheet with one section: every month, newest
;;; first, each a block like the end of a series: the month on the left, its
;;; articles as cells on the right.

(defun render-archive-month (date articles series)
  (render-index
   :id (press:format-date date :month-id)
   :top (ah:</> (<> (span "Month") (span (format nil "~d article~:p" (length articles)))))
   :name (press:format-date date :month)
   :body (render-hatch)
   :cells (mapcar (lambda (article)
                    (let ((in (model:article-series-of article series)))
                      (render-index-cell
                       (press:format-date (model:article-date article) :day-month)
                       (model:article-title article)
                       (joined (model:article-code article)
                               (model:article-type article)
                               (when in (part-fraction article in)))
                       :href (article-href article))))
                  articles)))

(defun page-archive (articles series)
  (let ((since (car (last articles))))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title "Archive — Almighty Lisp"
                     :description "Every article published in Almighty Lisp."
                     :page-class "page-archive"
        (render-section-head "archive" "Archive"
                             (format nil "~d article~:p" (length articles)))
        (ac-band :class "archive-band"
                 :side (list (joined "Archive" (format nil "~d article~:p" (length articles)))
                             (joined "Newest first"
                                     (when since
                                       (format nil "Since ~a"
                                               (press:format-date (model:article-date since) :month)))))
          (div :class "end-block"
            (if articles
                (mapcar (lambda (month)
                          (render-archive-month (first month) (rest month) series))
                        (model:group-by-month articles))
                (ah:</> (p :class "end-label" "Nothing has been published yet."))))))))))

(defun page-not-found (articles series)
  (ah:render-to-string
   (ah:</>
    (ac-sheet-page :title "Not found — Almighty Lisp"
                   :description "There is no article at this address."
                   :page-class "page-missing"
      (render-section-head "missing" "Not found" "No article at this address"
                           :action "Full archive  >>>" :action-href (archive-href))
      (render-end-band
       "Try one of these instead"
       (ah:</>
        (div :class "keep-briefs"
          (mapcar (lambda (article)
                    (render-article-brief article (model:article-series-of article series)))
                  (subseq articles 0 (min 3 (length articles))))))
       :folio (ah:</> (a :href (magazine-href) "Back to the magazine  >>>")))))))
