(in-package #:magazine/hypermedia/components)

;;;; The archive, and the page for a slug that isn't an article.
;;;
;;; The archive is a card catalogue (Figma: Archive C · Card catalogue): a
;;; black index of types, series and months beside the catalogue itself,
;;; every month newest first. The newest articles are briefs, as on the
;;; homepage; the rest are lines. The months after the first few start
;;; closed.
;;;
;;; The index filters the catalogue, and the filtering is done here: a type's
;;; or a series' row is a link to the archive with ?type= or ?series=, and
;;; the page it leads to lists those articles alone. With htmx in the page
;;; the link fetches that page, takes the catalogue out of it and sets it in
;;; place of this one; without, it is a link like any other. A month is a
;;; <details>, so it opens and closes with no script at all.

(defparameter *archive-open-months* 2
  "Months the archive shows in full when it loads. The rest start closed.")

(defparameter *archive-briefs* 4
  "Articles the archive shows as briefs. The rest are lines.")

(defun count-text (n)
  (format nil "~d article~:p" n))

(defun tally (items key)
  "How many ITEMS have each value of KEY, as (value count), most first."
  (let ((counts nil))
    (dolist (item items)
      (let* ((value (funcall key item))
             (found (assoc value counts :test #'equal)))
        (if found
            (incf (second found))
            (push (list value 1) counts))))
    (stable-sort (nreverse counts) #'> :key #'second)))

;;; The index: a black panel of rows, each a name, a dot leader and a
;;; number. A type's or a series' row filters the catalogue; a month's
;;; jumps to it.

(defun render-catalogue-row (name value &key href filter pressed on-click stamp)
  "A row of the index, a link to HREF. A FILTER row changes the catalogue
(see render-catalogue-index), and is marked while it is PRESSED. ON-CLICK is
something else to do."
  (ah:</>
   (a :class "cat-row" :href href
     :aria-current (when pressed "true")
     :hx-get (when filter href)
     :|hx-on:click| on-click
     (when stamp (render-series-stamp stamp :tone "paper"))
     (span :class "cat-row-name" name)
     (span :class "spec-lead" :aria-hidden t)
     (span value))))

(defun render-catalogue-index (articles shown months series &key type in)
  "The index of ARTICLES, SHOWN of them in the catalogue: those of TYPE, in
the series whose code is IN. A filter's row takes it off when it is on."
  (let ((since (model:article-date (car (last articles))))
        (running (remove-if #'zerop series :key #'model:series-published)))
    (ah:</>
     ;; A filter's row fetches the page it links to, and the catalogue in
     ;; that page takes this one's place. The address becomes the row's.
     (aside :class "cat-index" :aria-label "Index"
       :|hx-target:inherited| "#catalogue" :|hx-select:inherited| "#catalogue"
       :|hx-swap:inherited| "outerHTML" :|hx-push-url:inherited| "true"
       (div :class "cat-index-inner"
         (div
           (p :class "cat-label" (joined "Index" "On file"))
           (p :class "cat-count" (format nil "~d" (length shown)))
           (p :class "cat-label on" (format nil "Articles since ~a" (press:format-date since :month))))
         (div :class "cat-group"
           (p :class "cat-head" "By type")
           (render-catalogue-row "All" (format nil "~d" (length articles))
                                 :href (archive-href :series in) :filter t :pressed (null type))
           (mapcar (lambda (one)
                     (let ((pressed (equal (first one) type)))
                       (render-catalogue-row
                        (first one) (two-digits (second one))
                        :href (archive-href :type (unless pressed (first one)) :series in)
                        :filter t :pressed pressed)))
                   (tally articles #'model:article-type)))
         (when running
           (ah:</>
            (div :class "cat-group"
              (p :class "cat-head" "By series")
              (mapcar (lambda (one)
                        (let ((pressed (equal (model:series-code one) in)))
                          (render-catalogue-row
                           (model:series-title one)
                           (format nil "~d/~d" (model:series-published one) (model:series-total one))
                           :href (archive-href :type type
                                               :series (unless pressed (model:series-code one)))
                           :filter t :pressed pressed
                           :stamp (model:series-code one))))
                      running))))
         (div :class "cat-group"
           (p :class "cat-head" "By month")
           (mapcar (lambda (month)
                     (let ((id (press:format-date (first month) :month-id)))
                       (render-catalogue-row
                        (press:format-date (first month) :month)
                        (two-digits (length (rest month)))
                        :href (format nil "#~a" id)
                        ;; A jump to a month opens it.
                        :on-click (format nil "document.getElementById('~a')?.setAttribute('open', '')" id))))
                   months))
         (p :class "cat-label" "Newest first"))))))

;;; The catalogue

(defun render-catalogue-card (article series)
  "ARTICLE as a brief, as the homepage sets one."
  (ah:</>
   (div :class "cat-card"
     (render-article-brief article (model:article-series-of article series)))))

(defun render-catalogue-line (article series)
  "ARTICLE as a line of the homepage's archive column, its type and serial at
the end."
  (let ((in (model:article-series-of article series)))
    (ah:</>
     (li
       (a :class "arch-line" :href (article-href article)
         (span :class "arch-when" (press:format-date (model:article-date article) :day-month))
         (span :class "arch-mark"
           (when in
             (ah:</>
              (<>
                (render-series-stamp (model:series-code in))
                (span :class "arch-code" (model:series-code in))))))
         (span :class "arch-name" (model:article-title article))
         (span :class "cat-line-meta"
           (joined (model:article-type article) (model:article-code article))))))))

(defun render-catalogue-month (date members briefs series open)
  "A month of the catalogue under its heading, which opens and closes it.
Those of MEMBERS that are among BRIEFS are set as briefs."
  (let ((cards (remove-if-not (lambda (article) (member article briefs)) members))
        (lines (remove-if (lambda (article) (member article briefs)) members)))
    (ah:</>
     (details :class "cat-month" :id (press:format-date date :month-id) :open open
       (summary :class "cat-month-head"
         (span :class "arch-month-head"
           (span (press:format-date date :month))
           (span :class "arch-count" (count-text (length members)))))
       (div :class "cat-month-body"
         (when cards
           (ah:</>
            (div :class "latest-briefs"
              (mapcar (lambda (article) (render-catalogue-card article series)) cards))))
         (when lines
           (ah:</>
            (ul :class "cat-lines"
              (mapcar (lambda (article) (render-catalogue-line article series)) lines)))))))))

(defun render-catalogue (articles series &key type in open)
  "The index and the catalogue of ARTICLES: those of TYPE, in the series
whose code is IN. OPEN is a month to show open, by its id, or \"all\"; a
filter's articles are spread over every month, so it opens them all."
  (let* ((shown (remove-if-not (lambda (article)
                                 (and (or (null type) (equal type (model:article-type article)))
                                      (or (null in) (equal in (model:article-series article)))))
                               articles))
         (months (model:group-by-month shown))
         (briefs (subseq articles 0 (min *archive-briefs* (length articles))))
         (every (or type in (equal open "all")))
         (closed 0))
    (ah:</>
     (div :class "catalogue" :id "catalogue"
       (render-catalogue-index articles shown (model:group-by-month articles) series
                               :type type :in in)
       (div :class "cat-list"
         (loop for (date . members) in months
               for index from 0
               for month-open = (or every
                                    (< index *archive-open-months*)
                                    (equal open (press:format-date date :month-id)))
               unless month-open
                 do (incf closed)
               collect (render-catalogue-month date members briefs series month-open))
         (unless shown
           (ah:</> (p :class "cat-none" "Nothing on file matches both.")))
         (div :class "cat-foot"
           (span (format nil "Showing  ~d of ~d" (length shown) (length articles)))
           (when (plusp closed)
             (let ((href (archive-href :type type :series in :open "all")))
               (ah:</>
                (a :class "cat-foot-button" :href href
                  :hx-get href :hx-target "#catalogue" :hx-select "#catalogue"
                  :hx-swap "outerHTML" :hx-replace-url "true"
                  "Open every month  >>>"))))))
       ;; A page that arrives at a month by its #fragment alone opens it.
       (script (raw! "{ let month = document.getElementById(location.hash.slice(1)); if (month?.matches('.cat-month')) { month.open = true; month.scrollIntoView(); } }"))))))

(defun page-archive (articles series &rest filters)
  "The archive. FILTERS are RENDER-CATALOGUE's."
  (let ((since (car (last articles)))
        (*marked-bands* 0))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title "Archive — Almighty Lisp"
                     :description "Every article published in Almighty Lisp."
                     :page-class "page-archive"
        (render-section-head "archive" "Archive" (count-text (length articles))
                             :jp "archive"
                             :action (joined (count-text (length articles)) "Newest first"))
        (ac-band :class "archive-band"
                 :side (list (joined "Index" "Type" "Series" "Month")
                             (joined "Archive"
                                     (when since
                                       (format nil "Since ~a"
                                               (press:format-date (model:article-date since) :month)))))
          (if articles
              (apply #'render-catalogue articles series filters)
              (ah:</>
               (div :class "end-block"
                 (p :class "end-label" "Nothing has been published yet."))))))))))

(defun page-not-found (articles series &aux (*marked-bands* 0))
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
                  (subseq articles 0 (min 3 (length articles)))))))))))
