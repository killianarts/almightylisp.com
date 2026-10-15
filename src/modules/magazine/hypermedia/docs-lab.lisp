(in-package #:magazine/hypermedia/components)

;;;; The docs lab: one page of the Almighty Docs set on the magazine's sheet.
;;;
;;; (Figma: Almighty Documentation, Docs C · Field manual.) /magazine/docs/lab
;;; shows a how-to guide for local-time as the docs would set it: the article
;;; page, with the library's four kinds of documentation where a series' parts
;;; go, and the same topic in the other kinds where the rest of a series goes.
;;;
;;; A test page. It is not published with the static site, and nothing links
;;; to it. The library, its pages and the guide are written out below; none of
;;; it is read from content/. The guide's code was run against local-time
;;; 1.0.6 on SBCL 2.6.3, and its results are what that printed. Once the docs
;;; have a model of their own this file goes.

(defparameter *docs-lab-library*
  '(:name "local-time"
    :summary "Timestamps, timezones and clocks for Common Lisp. Everything the manual says, and the parts it leaves out.")
  "The library the lab's page belongs to.")

(defparameter *docs-lab-modes*
  '(("Tutorials" "TU"
     ("Your first timestamp"
      "Build a countdown to a release date"))
    ("How-to guides" "HT"
     ("Parse a timestring from user input"
      "Format an HTTP date header"
      "Convert between timezones"
      "Test time-dependent code with a fixed clock"
      "Add months safely"
      "Store timestamps in PostgreSQL"
      "Read timestamps as literals with @"))
    ("Reference" "RF"
     ("Types"
      "Timestamps"
      "Timezones"
      "Formatting and parsing"
      "Clocks"
      "Constants"))
    ("Explanation" "EX"
     ("The day, sec, nsec model"
      "Why the epoch is 2000-03-01"
      "How local-time decides what now is"
      "Offsets are not timezones")))
  "Name, serial code and pages of each kind of documentation, in the order the
contents lists them.")

(defparameter *docs-lab-page*
  '(:mode "How-to guides"
    :number 4
    :title "Test time-dependent code with a fixed clock"
    :title-lines ("Test Time-" "Dependent Code" "With a Fixed" "Clock")
    :dek "local-time asks a clock object for the current time. Bind your own clock and ~now~ returns whatever you decide, so a test can run at midnight on New Year's Day every time."
    :verified "SBCL 2.6.3")
  "The page the lab shows: which of *DOCS-LAB-MODES*' pages it is, and its lede.")

(defparameter *docs-lab-sections*
  '(("define-a-clock" "Define a clock"
     ((:p "A clock is any object with a ~clock-now~ method. This one returns the same timestamp every time it is asked.")
      (:src :lang "lisp"
            :lines ("(defclass fixed-clock ()"
                    "  ((time :initarg :time :reader fixed-clock-time)))"
                    ""
                    "(defmethod local-time:clock-now ((clock fixed-clock))"
                    "  (fixed-clock-time clock))"))))
    ("bind-the-clock" "Bind *clock* around the code under test"
     ((:p "~*clock*~ is a special variable, so the binding lasts only for the body of the ~let~. Code outside it still reads the system clock.")
      (:src :lang "lisp"
            :lines ("(let ((local-time:*clock*"
                    "        (make-instance 'fixed-clock"
                    "                       :time (local-time:parse-timestring"
                    "                              \"2026-01-01T00:00:00Z\"))))"
                    "  (local-time:format-timestring nil (local-time:now)"
                    "                                :timezone local-time:+utc-zone+))")
            :results "\"2026-01-01T00:00:00.000000Z\"")
      (:note "Timezones"
             ("A timestamp prints in ~*default-timezone*~, so the same moment reads ~@2026-01-01T09:00:00.000000+09:00~ in Tokyo. Format it in UTC when a test compares text."))))
    ("use-it-in-a-test" "Use it in a test"
     ((:p "Wrap the binding in a macro and each test names the moment it runs at.")
      (:src :lang "lisp"
            :lines ("(defmacro with-fixed-clock ((timestring) &body body)"
                    "  `(let ((local-time:*clock*"
                    "           (make-instance 'fixed-clock"
                    "                          :time (local-time:parse-timestring ,timestring))))"
                    "     ,@body))"
                    ""
                    "(with-fixed-clock (\"2026-01-31T00:00:00Z\")"
                    "  (local-time:timestamp> (local-time:now)"
                    "                         (local-time:parse-timestring \"2026-01-30T00:00:00Z\")))")
            :results "T")))
    ("why-this-works" "Why this works"
     ((:p "~now~ calls ~clock-now~ on the value of ~*clock*~, and ~today~ calls ~clock-today~. Any object with a ~clock-now~ method can stand in for the system clock.")
      (:p "Reference: Clocks lists every method a clock can define. Explanation: How local-time decides what now is follows the call from top to bottom."))))
  "The guide's sections: id, title and blocks, as RENDER-BLOCK reads them.")

(defparameter *docs-lab-related*
  '(("Reference" "Clocks")
    ("Explanation" "How local-time decides what now is")
    ("Tutorials" "Build a countdown to a release date"))
  "The same topic in the other kinds of documentation: kind and page title.")

;;; Finding a page among the modes

(defun docs-lab-mode (name)
  (assoc name *docs-lab-modes* :test #'string=))

(defun docs-lab-code (mode-name number)
  "A page's serial: its library, its kind and its place among that kind's pages."
  (format nil "LT-~a-~a" (second (docs-lab-mode mode-name)) (two-digits number)))

(defun docs-lab-kind (mode-name)
  "A kind of documentation as a label names one page of it: How-to, Tutorial."
  (cond ((string= mode-name "How-to guides") "How-to")
        ((string= mode-name "Tutorials") "Tutorial")
        (t mode-name)))

(defun docs-lab-place ()
  "Where the page is among its kind's pages: How-to 4 of 7."
  (let ((mode (getf *docs-lab-page* :mode)))
    (format nil "~a ~d of ~d" (docs-lab-kind mode) (getf *docs-lab-page* :number)
            (length (third (docs-lab-mode mode))))))

(defun docs-lab-serial ()
  (docs-lab-code (getf *docs-lab-page* :mode) (getf *docs-lab-page* :number)))

;;; The head band: the lede, and beside it the book and the library's contents.

(defun render-docs-lab-lede ()
  (ah:</>
   (header :class "neo-lead art-lede"
     (render-crop-marks)
     (render-headline (getf *docs-lab-page* :title-lines) :class "art-headline")
     (div :class "series-tags"
       (span :class "series-tag outline"
         (format nil "[ Library: ~a ]" (getf *docs-lab-library* :name)))
       (span :class "series-tag filled" (docs-lab-place))
       (span :class "series-tag slashes" :aria-hidden t "//////////////"))
     (render-hatch)
     (div :class "neo-lead-row"
       (p :class "neo-dek" (render-inlines (getf *docs-lab-page* :dek)))
       (div :class "neo-lead-meta"
         (render-spec (list (list "Mode" (docs-lab-kind (getf *docs-lab-page* :mode)))
                            (list "Serial" (docs-lab-serial))
                            (list "Verified" (getf *docs-lab-page* :verified)))
                      :class "spec byline-spec")
         (span :class "hazard-plate" :aria-hidden t))))))

(defun render-docs-lab-contents ()
  "The four kinds of documentation, with the pages of the page's own kind
under it and the page itself marked."
  (ah:</>
   (nav :class "contents-panel in-series" :aria-label "Contents"
     (render-crop-marks '("tr" "bl"))
     (p :class "contents-title"
       (span "In this library")
       (span :class "contents-folio" (docs-lab-place)))
     (ol :class "contents-list"
       (loop for (name nil pages) in *docs-lab-modes*
             for n from 1
             collect (render-contents-row (two-digits n) name)
             when (string= name (getf *docs-lab-page* :mode))
               append (loop for page in pages
                            for number from 1
                            collect (render-contents-row
                                     (princ-to-string number) page
                                     :class (if (= number (getf *docs-lab-page* :number))
                                                "contents-row sub current"
                                                "contents-row sub")))))
     (render-hatch))))

;;; Sections, as the article page sets them.

(defun render-docs-lab-piece (section number total)
  (destructuring-bind (id title blocks) section
    (ah:</>
     (section :class "neo-piece" :id id
       (div :class "piece-row"
         (div :class "piece-cell piece-text"
           (header :class "piece-heading"
             (span :class "ghost-numeral" :aria-hidden t (two-digits number))
             (render-eyebrow (format nil "Section ~a / ~a" (two-digits number) (two-digits total))
                             :marks t)
             (h2 :class "piece-name" title)
             (div :class "progress-rule"
               (render-progress-bar (/ number total))
               (render-rule-dots)))
           (div :class "piece-body"
             (mapcar #'render-block blocks))))
       (render-marks (evenp number))))))

;;; The end band: this page, the same topic in the other kinds, and the pages
;;; either side of it.

(defun docs-lab-end-cells ()
  (let* ((mode (getf *docs-lab-page* :mode))
         (number (getf *docs-lab-page* :number))
         (pages (third (docs-lab-mode mode)))
         (kind (docs-lab-kind mode))
         (cells (list (list (getf *docs-lab-page* :title) (joined kind "You are here") "current"))))
    (loop for (other-mode title) in *docs-lab-related*
          for place = (position title (third (docs-lab-mode other-mode)) :test #'string=)
          do (push (list title (joined (docs-lab-kind other-mode)
                                       (docs-lab-code other-mode (1+ place))))
                   cells))
    (when (> number 1)
      (push (list (nth (- number 2) pages) (joined kind "Previous")) cells))
    (when (< number (length pages))
      (push (list (nth number pages) (joined kind "Next")) cells))
    (loop for (title meta class) in (reverse cells)
          for n from 1
          collect (render-index-cell (two-digits n) title meta :class class))))

(defun render-docs-lab-end ()
  (let* ((mode (getf *docs-lab-page* :mode))
         (number (getf *docs-lab-page* :number))
         (more (< number (length (third (docs-lab-mode mode))))))
    (render-article-end
     "Keep going..."
     (docs-lab-place)
     (render-index
      :name (getf *docs-lab-library* :name)
      :body (ah:</> (p :class "series-summary" (getf *docs-lab-library* :summary)))
      :cells (docs-lab-end-cells))
     :jp "series"
     :action (if more
                 (format nil "Next: ~a  >>>" (docs-lab-code mode (1+ number)))
                 "Last page"))))

(defun page-docs-lab ()
  "The lab: *DOCS-LAB-PAGE* set as an article."
  (let ((total (length *docs-lab-sections*))
        (*marked-bands* 0))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title (format nil "~a — Almighty Docs" (getf *docs-lab-page* :title))
                     :description "A test page for the Almighty Docs: one how-to guide for local-time."
                     :page-class "page-article page-docs-lab" :sheet-id "top"
        (ac-band :class "art-head-band" :marks :foot
                 :side (list (joined (docs-lab-serial) (date-text (press:today)))
                             (joined (getf *docs-lab-library* :name) (docs-lab-place)))
          (div :class "art-head"
            (render-docs-lab-lede)
            (div :class "art-side"
              (render-book-ad)
              (render-docs-lab-contents))))
        (ac-band :class "pieces-band"
                 :side (list (joined "Almighty Docs" "Four kinds of documentation")
                             (joined "Common Lisp" "Emacs" "Sly" "REPL: connected"))
          (div :class "piece-list"
            (loop for section in *docs-lab-sections*
                  for number from 1
                  collect (render-docs-lab-piece section number total))))
        (render-docs-lab-end))))))
