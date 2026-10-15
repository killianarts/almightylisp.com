(in-package #:magazine/hypermedia/components)

;;;; The archive lab: three candidate archives on one page, to compare.
;;;
;;; (Figma: Archive A · Manifest, Archive B · Month plates, Archive C · Card
;;; catalogue.) /magazine/archive/lab?design=a|b|c&data=sample|live shows one
;;; design at a time, over the magazine's own articles or over a sample of 59,
;;; enough to see how a design holds a year of them.
;;;
;;; A test page. It is not published with the static site, and nothing links
;;; to it. Its styles are static/css/magazine-archive-lab.css and its script
;;; static/js/magazine-archive-lab.js: filters, months that open and close,
;;; and jumps to a month, the same for all three designs. Once a design is
;;; chosen it moves into archive.lisp and magazine.css, and the lab goes.

(defstruct lab-entry
  "An article as the lab lists it: one of the magazine's, or one of the sample's."
  date title kind code series series-title part total dek author href)

(defparameter *lab-designs*
  '(("a" "A" "Manifest")
    ("b" "B" "Month plates")
    ("c" "C" "Card catalogue"))
  "Key, letter and name of each design.")

(defparameter *lab-open-months* 2
  "Months shown in full when the page loads. The rest start closed.")

(defparameter *lab-briefs* 4
  "Articles design C shows as briefs. The rest are lines.")

;;; The sample: the articles in the Figma designs, and enough before them to
;;; fill the months those designs only count.

(defparameter *lab-sample-series*
  '(("MAC" "Native app development in macOS" 6)
    ("CND" "The condition system, bottom up" 5)
    ("EMX" "An Emacs of your own" 4)
    ("CLS" "CLOS from the ground up" 4))
  "Code, title and number of parts of each of the sample's series.")

(defparameter *lab-sample*
  '(("2026-09-27" "MAC" 3 "Tutorial" "CL" "Positioning GUI elements"
     "Cocoa places views with constraints, not coordinates. Describe a layout as plain Lisp data and let Auto Layout solve it.")
    ("2026-09-24" nil nil "Opinion" "CL" "The image is still running"
     "Most languages make you stop a program before you can change it. In Common Lisp the program keeps running while you fix it.")
    ("2026-09-22" "CND" 5 "Tutorial" "CL" "An error that waits for you to decide"
     "When a Common Lisp program signals an error, the stack is still there. Follow one bug from the signal to the restart you choose.")
    ("2026-09-19" "EMX" 1 "How-to" "EM" "Learn enough Emacs to write Lisp, then stop"
     "The commands you need on day one, the ones worth learning once they'd save you time, and the Sly tools to pick up after that.")
    ("2026-09-16" nil nil "Opinion" "TL" "Restarting isn't a build step"
     "For fifty years we've killed the process to fix a bug and called the wait part of the job. It never had to be.")
    ("2026-09-13" "MAC" 2 "Tutorial" "CL" "A window that survives a recompile")
    ("2026-09-11" nil nil "How-to" "CL" "Handling money without floating point")
    ("2026-08-30" "MAC" 1 "Tutorial" "CL" "Talking to Cocoa from the REPL")
    ("2026-08-27" "CND" 4 "Tutorial" "CL" "Your own debugger hook")
    ("2026-08-22" nil nil "Opinion" "CL" "To LOOP or not to LOOP")
    ("2026-08-18" "CND" 3 "Tutorial" "CL" "Restarts are just functions")
    ("2026-08-12" nil nil "How-to" "CL" "Reading the HyperSpec without crying")
    ("2026-08-08" "CND" 2 "Tutorial" "CL" "Handlers that don't unwind")
    ("2026-07-29" "CND" 1 "Tutorial" "CL" "Signals are not exceptions")
    ("2026-07-27" nil nil "Opinion" "IN" "Nobody got fired for choosing boring")
    ("2026-07-24" nil nil "How-to" "CL" "Packages are not modules")
    ("2026-07-22" nil nil "Tutorial" "CL" "Format directives you will actually use")
    ("2026-07-20" nil nil "Case study" "WD" "Serving this site from one Lisp image")
    ("2026-07-17" nil nil "How-to" "EM" "Paredit in ten keystrokes")
    ("2026-07-14" nil nil "Opinion" "CL" "The REPL is not a shell")
    ("2026-07-11" nil nil "Tutorial" "CL" "Macros that write macros")
    ("2026-07-08" nil nil "How-to" "TL" "Pinning dependencies with vend")
    ("2026-07-05" nil nil "Opinion" "IN" "Thirty years of the same standard")
    ("2026-07-02" nil nil "Tutorial" "CL" "Multiple values, one return")
    ("2026-06-30" "CLS" 4 "Tutorial" "CL" "Method combinations, in order")
    ("2026-06-27" nil nil "Opinion" "CL" "Dynamic scope is a feature")
    ("2026-06-24" nil nil "How-to" "EM" "Jumping to definitions with Sly")
    ("2026-06-20" "CLS" 3 "Tutorial" "CL" "Generic functions before classes")
    ("2026-06-17" nil nil "Case study" "CL" "Parsing org files without regex")
    ("2026-06-13" nil nil "How-to" "TL" "Building a binary with Roswell")
    ("2026-06-10" "CLS" 2 "Tutorial" "CL" "Slots, accessors and initargs")
    ("2026-06-06" nil nil "Opinion" "IN" "Your framework is a library now")
    ("2026-06-03" nil nil "How-to" "CL" "Reading a stack trace in SBCL")
    ("2026-05-31" "CLS" 1 "Tutorial" "CL" "Defclass without the ceremony")
    ("2026-05-28" nil nil "Opinion" "CL" "Parentheses were never the problem")
    ("2026-05-26" nil nil "How-to" "CL" "Hash tables, plainly")
    ("2026-05-23" nil nil "Tutorial" "CL" "Streams all the way down")
    ("2026-05-20" nil nil "Case study" "WD" "A router in sixty lines")
    ("2026-05-18" nil nil "How-to" "EM" "A modeline that tells the truth")
    ("2026-05-15" nil nil "Opinion" "TL" "Quicklisp was enough")
    ("2026-05-12" nil nil "Tutorial" "CL" "Closures as objects")
    ("2026-05-09" nil nil "How-to" "CL" "Timing code with TIME")
    ("2026-05-07" nil nil "Tutorial" "CL" "Sequences versus lists")
    ("2026-05-04" nil nil "Opinion" "IN" "Learning Lisp in public")
    ("2026-05-01" nil nil "How-to" "TL" "Setting up SBCL on a new Mac")
    ("2026-04-28" nil nil "Tutorial" "CL" "Symbols, keywords and strings")
    ("2026-04-24" nil nil "Opinion" "CL" "Why LET over SETF")
    ("2026-04-21" nil nil "How-to" "EM" "Evaluating forms in place")
    ("2026-04-17" nil nil "Case study" "CL" "Porting a script from Python")
    ("2026-04-14" nil nil "Tutorial" "CL" "Conses, drawn by hand")
    ("2026-04-10" nil nil "How-to" "CL" "Printing things readably")
    ("2026-04-07" nil nil "Opinion" "IN" "The language that waited")
    ("2026-04-03" nil nil "Tutorial" "CL" "Functions are values")
    ("2026-03-30" nil nil "How-to" "TL" "Installing Emacs and Sly")
    ("2026-03-26" nil nil "Tutorial" "CL" "Your first defun")
    ("2026-03-22" nil nil "Opinion" "CL" "Start at the REPL")
    ("2026-03-18" nil nil "How-to" "EM" "Surviving your first hour in Emacs")
    ("2026-03-14" nil nil "Tutorial" "CL" "Atoms and lists")
    ("2026-03-10" nil nil "Opinion" "IN" "Why this magazine exists"))
  "The sample's articles, newest first: date, series code and part (or NIL),
type, topic code, title, and a summary for the ones new enough to be briefs.")

(defun lab-sample-entries ()
  (mapcar (lambda (row)
            (destructuring-bind (date series part kind topic title &optional dek) row
              (make-lab-entry
               :date (press:parse-date date)
               :title title
               :kind kind
               ;; A serial as MODEL::MAKE-SERIAL writes one: CL-TU-26927.
               :code (format nil "~a-~a-~a~d~a" topic
                             (second (assoc kind model:*article-types* :test #'string=))
                             (subseq date 2 4)
                             (parse-integer date :start 5 :end 7)
                             (subseq date 8))
               :series series
               :series-title (second (assoc series *lab-sample-series* :test #'equal))
               :part part
               :total (third (assoc series *lab-sample-series* :test #'equal))
               :dek dek
               :author "Micah Killian"
               ;; The sample's articles don't exist; the script stops the click.
               :href "#")))
          *lab-sample*))

(defun lab-live-entries (articles series)
  (mapcar (lambda (article)
            (let ((in (model:article-series-of article series)))
              (make-lab-entry
               :date (model:article-date article)
               :title (model:article-title article)
               :kind (model:article-type article)
               :code (model:article-code article)
               :series (when in (model:series-code in))
               :series-title (when in (model:series-title in))
               :part (when in (model:article-part article))
               :total (when in (model:series-total in))
               :dek (model:article-subtitle article)
               :author (model:article-author article)
               :href (article-href article))))
          articles))

;;; Counting and grouping

(defun lab-months (entries)
  "ENTRIES, newest first, as a list of (month-date entries...) per month."
  (let ((groups nil))
    (dolist (entry entries)
      (if (and groups (press:same-month-p (car (first groups)) (lab-entry-date entry)))
          (push entry (cdr (first groups)))
          (push (list (lab-entry-date entry) entry) groups)))
    (nreverse (mapcar (lambda (group) (cons (car group) (reverse (cdr group))))
                      groups))))

(defun lab-tally (entries key)
  "How many ENTRIES have each value of KEY, as (value count), most first.
Entries without one aren't counted."
  (let ((tally nil))
    (dolist (entry entries)
      (let ((value (funcall key entry)))
        (when value
          (let ((found (assoc value tally :test #'equal)))
            (if found
                (incf (second found))
                (push (list value 1) tally))))))
    (stable-sort (nreverse tally) #'> :key #'second)))

(defun lab-series-rows (entries titles)
  "Each series among ENTRIES as (code title parts-out total), in the order
its newest part appears. TITLES is an alist of code and title."
  (let ((rows nil))
    (dolist (entry entries (nreverse rows))
      (let ((code (lab-entry-series entry)))
        (when code
          (let ((found (assoc code rows :test #'equal)))
            (if found
                (incf (third found))
                (push (list code
                            (or (second (assoc code titles :test #'equal)) code)
                            1
                            (lab-entry-total entry))
                      rows))))))))

(defun lab-count-text (n)
  (format nil "~d article~:p" n))

(defun lab-day (date)
  (parse-integer (press:format-date date :day-month) :junk-allowed t))

(defun lab-month-name (date)
  "DATE's month without its year: September."
  (let ((text (press:format-date date :month)))
    (subseq text 0 (position #\Space text))))

(defun lab-part-text (entry)
  (when (lab-entry-series entry)
    (format nil "Part ~d/~d" (lab-entry-part entry) (lab-entry-total entry))))

(defun lab-href (design data)
  (format nil "~a?design=~a&data=~a" (s:url "magazine:archive-lab") design data))

;;; Parts the designs share

(defun render-lab-barcode (entries)
  "A tick for each day of a month, tall on the days one of ENTRIES came out."
  (let ((days (mapcar (lambda (entry) (lab-day (lab-entry-date entry))) entries)))
    (ah:</>
     (span :class "lab-barcode" :aria-hidden t
       (loop for day from 1 to 31
             collect (ah:</> (i :class (when (member day days) "on"))))))))

(defun render-lab-foot (total &key view)
  "VIEW, when the design can be sorted, says how it is sorted now."
  (ah:</>
   (div :class "lab-foot"
     (span (span "Showing  ") (span :data-shown t (format nil "~d of ~d" total total))
       (when view (ah:</> (span :data-view t (format nil "  ■  ~a" view)))))
     (button :class "lab-foot-button" :type "button" :data-expand t "Open every month  >>>"))))

(defun render-lab-switch (design data sample-count live-count)
  "The lab's own controls: which design, and which articles it lists."
  (ah:</>
   (div :class "lab-switch" :|hx-boost:inherited| "false"
     (div :class "lab-switch-group"
       (span :class "lab-switch-label" (joined "Test page" "Design"))
       (mapcar (lambda (one)
                 (destructuring-bind (key letter name) one
                   (ah:</>
                    (a :class (ah:clsx "lab-tab" (when (string= key design) "on"))
                      :href (lab-href key data)
                      (joined letter name)))))
               *lab-designs*))
     (div :class "lab-switch-group"
       (span :class "lab-switch-label" "Articles")
       (a :class (ah:clsx "lab-tab" (when (string= data "sample") "on"))
         :href (lab-href design "sample")
         (joined "Sample" sample-count))
       (a :class (ah:clsx "lab-tab" (when (string= data "live") "on"))
         :href (lab-href design "live")
         (joined "Live" live-count)))
     (div :class "lab-switch-group end"
       (a :class "lab-tab" :href (archive-href) "Current archive  >>>")))))

;;; A · Manifest

(defun render-lab-row (entry)
  (ah:</>
   (a :class "lab-row" :href (lab-entry-href entry)
     :data-article t :data-kind (lab-entry-kind entry) :data-series (or (lab-entry-series entry) "")
     ;; What the script sorts and groups by (see the column heads).
     :data-date (press:format-date (lab-entry-date entry) :iso)
     :data-month-name (press:format-date (lab-entry-date entry) :month)
     :data-title (lab-entry-title entry)
     :data-series-title (or (lab-entry-series-title entry) "")
     :data-part (if (lab-entry-series entry) (format nil "~d" (lab-entry-part entry)) "")
     :data-code (lab-entry-code entry)
     (span :class "lab-row-date" (press:format-date (lab-entry-date entry) :day-month))
     (span :class "lab-row-series"
       (when (lab-entry-series entry)
         (ah:</>
          (<>
            (render-series-stamp (lab-entry-series entry))
            (span (lab-entry-series entry))))))
     (span :class "lab-row-title" (lab-entry-title entry))
     (span :class "lab-row-kind" (lab-entry-kind entry))
     (span :class "lab-row-part" (or (lab-part-text entry) "—"))
     (span :class "lab-row-code" (lab-entry-code entry))
     (span :class "lab-row-go" :aria-hidden t ">>>"))))

(defparameter *lab-columns*
  '(("date" "Date") ("series" "Series") ("title" "Title")
    ("kind" "Kind") ("part" "Part") ("serial" "Serial"))
  "Key and name of each of the manifest's columns. Each name is a button: it
sorts the manifest by that column, or groups it (see magazine-archive-lab.js).")

(defun render-lab-col (column)
  "A column's head. The manifest starts sorted by date, newest first."
  (destructuring-bind (key name) column
    (let ((on (string= key "date")))
      (ah:</>
       (button :class "lab-col" :type "button" :data-sort key
         :aria-pressed (if on "true" "false") :data-dir (when on "desc")
         (span name)
         (i :class "lab-col-dir" :aria-hidden t))))))

(defun render-lab-chip (filter value label count &key pressed stamp)
  (ah:</>
   (button :class "lab-chip" :type "button" :data-filter filter :data-value value
     :aria-pressed (if pressed "true" "false")
     (when stamp (render-series-stamp stamp))
     (span label)
     (when count (ah:</> (span :class "n" count))))))

(defun render-lab-manifest (entries)
  (let ((total (length entries)))
    (ah:</>
     (div :class "lab lab-a" :data-lab t :data-total total
       (div :class "lab-filter"
         (div :class "lab-chips"
           (render-lab-chip "kind" "" "All" total :pressed t)
           (mapcar (lambda (kind)
                     (render-lab-chip "kind" (first kind) (first kind) (second kind)))
                   (lab-tally entries #'lab-entry-kind)))
         (div :class "lab-chips"
           (span :class "lab-chips-label" "Series")
           (mapcar (lambda (series)
                     (render-lab-chip "series" (first series) (first series) (second series)
                                      :stamp (first series)))
                   (lab-tally entries #'lab-entry-series))))
       (div :class "lab-cols"
         (mapcar #'render-lab-col *lab-columns*)
         (span))
       (div :class "lab-groups" :data-groups t
         (loop for (date . members) in (lab-months entries)
               for index from 0
               for collapsed = (>= index *lab-open-months*)
               collect (ah:</>
                        (section :class "lab-month" :id (press:format-date date :month-id)
                          :data-month t :data-collapsed (when collapsed t)
                          (button :class "lab-month-bar" :type "button" :data-toggle t
                            :aria-expanded (if collapsed "false" "true")
                            (span :class "lab-month-name" (press:format-date date :month))
                            (span :class "lab-hatch" :aria-hidden t)
                            (render-lab-barcode members)
                            (span :class "lab-month-count" :data-count t
                              (lab-count-text (length members)))
                            (span :class "lab-month-go" :aria-hidden t ">>>"))
                          (div :class "lab-rows lab-month-body"
                            (mapcar #'render-lab-row members))))))
       (render-lab-foot total :view "By date, newest first")))))

;;; B · Month plates

(defparameter *lab-short-months*
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun render-lab-rail (entries)
  "The newest article's year, month by month, each a jump to its plate."
  (let* ((newest (press:format-date (lab-entry-date (first entries)) :month-id))
         (year (subseq newest 0 4))
         (counts (lab-tally entries (lambda (entry)
                                      (press:format-date (lab-entry-date entry) :month-id)))))
    (ah:</>
     (nav :class "lab-rail" :aria-label "Months"
       (div :class "lab-rail-year"
         (span (joined "Year" (format nil "~d on file" (length entries))))
         (span :class "lab-rail-n" year))
       (loop for name in *lab-short-months*
             for month from 1
             for id = (format nil "~a-~2,'0d" year month)
             for count = (second (assoc id counts :test #'string=))
             collect (ah:</>
                      (a :class (ah:clsx "lab-rail-cell"
                                         (cond ((null count) "none")
                                               ((string= id newest) "on")))
                        :href (format nil "#~a" id)
                        :aria-hidden (unless count "true")
                        :tabindex (unless count "-1")
                        (span name)
                        (span :class "lab-rail-n" (if count (two-digits count) "—")))))))))

(defun render-lab-cell (entry)
  (ah:</>
   (a :class "index-cell" :href (lab-entry-href entry)
     :data-article t :data-kind (lab-entry-kind entry) :data-series (or (lab-entry-series entry) "")
     (span :class "index-cell-n" (press:format-date (lab-entry-date entry) :day-month))
     (span :class "index-cell-text"
       (span :class "index-cell-title" (lab-entry-title entry))
       (span :class "index-cell-meta"
         (when (lab-entry-series entry) (render-series-stamp (lab-entry-series entry)))
         ;; The facts may wrap, but not inside "Part 3/6".
         (joined (lab-entry-series entry) (lab-entry-code entry) (lab-entry-kind entry)
                 (when (lab-entry-series entry)
                   (format nil "Part~a~d/~d" (code-char 160)
                           (lab-entry-part entry) (lab-entry-total entry)))))))))

(defun render-lab-plates (entries)
  (let ((total (length entries)))
    (ah:</>
     (div :class "lab lab-b" :data-lab t :data-total total
       (render-lab-rail entries)
       (loop for (date . members) in (lab-months entries)
             for index from 0
             for collapsed = (>= index *lab-open-months*)
             collect (ah:</>
                      (section :class "lab-month" :id (press:format-date date :month-id)
                        :data-month t :data-collapsed (when collapsed t)
                        (button :class "lab-plate-head" :type "button" :data-toggle t
                          :aria-expanded (if collapsed "false" "true")
                          (span :class "index-name lab-month-name" (lab-month-name date))
                          (span :class "lab-plate-sub"
                            (span (format nil "~a  ■  " (subseq (press:format-date date :month-id) 0 4)))
                            (span :data-count t (lab-count-text (length members))))
                          (render-lab-barcode members)
                          (span :class "lab-scale" :aria-hidden t
                            (span "01") (span "08") (span "15") (span "22") (span "31"))
                          (span :class "lab-hatch" :aria-hidden t))
                        (div :class "lab-cells lab-month-body"
                          (mapcar #'render-lab-cell members)
                          (span :class "index-fill" :aria-hidden t :data-fill t
                            :hidden (when (evenp (length members)) t))))))
       (render-lab-foot total)))))

;;; C · Card catalogue

(defun render-lab-lead-row (name value &key filter filter-value pressed href stamp)
  "A row of the index: NAME, a rule, VALUE. With FILTER it's a filter's
button; with HREF, a link."
  (let ((inner (ah:</>
                (<>
                  (when stamp (render-series-stamp stamp :tone "paper"))
                  (span :class "lab-lead-name" name)
                  (span :class "lab-lead" :aria-hidden t)
                  (span value)))))
    (if filter
        (ah:</>
         (button :class "lab-lead-row" :type "button" :data-filter filter :data-value filter-value
           :aria-pressed (if pressed "true" "false")
           inner))
        (ah:</> (a :class "lab-lead-row" :href href inner)))))

(defun render-lab-index (entries months series-titles)
  (let ((total (length entries))
        (since (lab-entry-date (car (last entries)))))
    (ah:</>
     (aside :class "lab-index" :aria-label "Index"
       (div :class "lab-index-inner"
         (div
           (p :class "lab-index-label" (joined "Index" "On file"))
           (p :class "lab-index-count" :data-shown t :data-bare t total)
           (p :class "lab-index-label" (format nil "Articles since ~a" (press:format-date since :month))))
         (div :class "lab-index-group"
           (p :class "lab-index-head" "By kind")
           (render-lab-lead-row "All" total :filter "kind" :filter-value "" :pressed t)
           (mapcar (lambda (kind)
                     (render-lab-lead-row (first kind) (second kind)
                                          :filter "kind" :filter-value (first kind)))
                   (lab-tally entries #'lab-entry-kind)))
         (when (some #'lab-entry-series entries)
           (ah:</>
            (div :class "lab-index-group"
              (p :class "lab-index-head" "By series")
              (mapcar (lambda (row)
                        (destructuring-bind (code title out parts) row
                          (render-lab-lead-row title (format nil "~d/~d" out parts)
                                               :filter "series" :filter-value code :stamp code)))
                      (lab-series-rows entries series-titles)))))
         (div :class "lab-index-group"
           (p :class "lab-index-head" "By month")
           (mapcar (lambda (month)
                     (render-lab-lead-row (press:format-date (first month) :month)
                                          (two-digits (length (rest month)))
                                          :href (format nil "#~a" (press:format-date (first month) :month-id))))
                   months))
         (p :class "lab-index-label" "Newest first"))))))

(defun render-lab-card (entry)
  "ENTRY as a brief, as the homepage sets one."
  (ah:</>
   (div :class "lab-card"
     :data-article t :data-kind (lab-entry-kind entry) :data-series (or (lab-entry-series entry) "")
     (render-brief
      :href (lab-entry-href entry)
      :strip (when (lab-entry-series entry)
               (ah:</>
                (span :class "series-strip"
                  (render-series-stamp (lab-entry-series entry) :tone "paper")
                  (span (slashed "Series" (lab-entry-series entry) (lab-part-text entry)))
                  (render-progress-bar (/ (lab-entry-part entry) (lab-entry-total entry))))))
      :code (lab-entry-code entry)
      :title (lab-entry-title entry)
      :dek (lab-entry-dek entry)
      :by (lab-entry-author entry)))))

(defun render-lab-line (entry)
  "ENTRY as a line of the homepage's archive column, with its facts at the end."
  (ah:</>
   (li :data-article t :data-kind (lab-entry-kind entry) :data-series (or (lab-entry-series entry) "")
     (a :class "arch-line" :href (lab-entry-href entry)
       (span :class "arch-when" (press:format-date (lab-entry-date entry) :day-month))
       (span :class "arch-mark"
         (when (lab-entry-series entry)
           (ah:</>
            (<>
              (render-series-stamp (lab-entry-series entry))
              (span :class "arch-code" (lab-entry-series entry))))))
       (span :class "arch-name" (lab-entry-title entry))
       (span :class "lab-line-meta" (joined (lab-entry-kind entry) (lab-entry-code entry)))))))

(defun render-lab-catalogue (entries series-titles)
  (let* ((total (length entries))
         (months (lab-months entries))
         (briefs (subseq entries 0 (min *lab-briefs* total))))
    (ah:</>
     (div :class "lab lab-c" :data-lab t :data-total total
       (render-lab-index entries months series-titles)
       (div :class "lab-catalogue"
         (loop for (date . members) in months
               for index from 0
               for collapsed = (>= index *lab-open-months*)
               for cards = (remove-if-not (lambda (entry) (member entry briefs)) members)
               for lines = (remove-if (lambda (entry) (member entry briefs)) members)
               collect (ah:</>
                        (section :class "lab-month" :id (press:format-date date :month-id)
                          :data-month t :data-collapsed (when collapsed t)
                          (button :class "lab-c-head" :type "button" :data-toggle t
                            :aria-expanded (if collapsed "false" "true")
                            (span :class "arch-month-head"
                              (span (press:format-date date :month))
                              (span :class "arch-count" :data-count t
                                (lab-count-text (length members)))))
                          (div :class "lab-catalogue lab-month-body"
                            (when cards
                              (ah:</>
                               (div :class "latest-briefs"
                                 (mapcar #'render-lab-card cards))))
                            (when lines
                              (ah:</>
                               (ul :class "lab-lines"
                                 (mapcar #'render-lab-line lines))))))))
         (render-lab-foot total))))))

;;; The page

(defun page-archive-lab (articles series &key design data)
  "The lab, showing DESIGN (a key of *LAB-DESIGNS*; A when it isn't one) over
the sample, or over ARTICLES when DATA is \"live\"."
  (let* ((design (if (assoc design *lab-designs* :test #'equal) design "a"))
         (data (if (equal data "live") "live" "sample"))
         (sample (lab-sample-entries))
         (live (lab-live-entries articles series))
         (entries (if (string= data "live") live sample))
         (titles (if (string= data "live")
                     (mapcar (lambda (one) (list (model:series-code one) (model:series-title one)))
                             series)
                     *lab-sample-series*))
         (since (car (last entries)))
         (*marked-bands* 0))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title "Archive lab — Almighty Lisp"
                     :description "Three candidate designs for the archive, side by side."
                     :page-class "page-archive page-archive-lab"
                     :scripts (ah:</> (script :src (asset "js/magazine-archive-lab.js") :defer t))
        (link :rel "stylesheet" :href (asset "css/magazine-archive-lab.css"))
        (render-section-head "archive" "Archive" (lab-count-text (length entries))
                             :action "Newest first")
        (ac-band :class "lab-switch-band"
          (render-lab-switch design data (length sample) (length live)))
        (ac-band :class "archive-band"
                 :side (list (joined "Archive" (lab-count-text (length entries)))
                             (joined (third (assoc design *lab-designs* :test #'string=))
                                     (when since
                                       (format nil "Since ~a"
                                               (press:format-date (lab-entry-date since) :month)))))
          (cond ((null entries)
                 (ah:</> (div :class "end-block"
                           (p :class "end-label" "Nothing has been published yet."))))
                ((string= design "b") (render-lab-plates entries))
                ((string= design "c") (render-lab-catalogue entries titles))
                (t (render-lab-manifest entries)))))))))
