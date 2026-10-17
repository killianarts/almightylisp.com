(defpackage #:magazine/og-image
  (:use #:cl)
  (:local-nicknames (#:model #:magazine/model)
                    (#:fm #:magazine/font-metrics))
  (:export #:image #:image-article #:image-file #:image-svg #:image-title-size
           #:image-title-lines #:image-title-cut #:image-dek-lines #:image-dek-cut
           #:article-image #:ensure-images #:image-drawn-p #:image-static-path
           #:renderer-problem #:plain-text #:+width+ #:+height+))

(in-package #:magazine/og-image)

;;;; Social preview images: the picture a link to an article shows on X,
;;;; Facebook, Discord and the like (Figma: Social Media · Article OG image).
;;;
;;; Each article's image is an SVG, drawn as a PNG by rsvg-convert into
;;; static/magazine/published/og/. The file is named after the slug and a hash
;;; of the SVG, so an image is drawn again only when what it shows changes,
;;; and a changed one gets a new address: the social sites keep the old one.
;;;
;;; The title is set as large as it fits: the largest size whose lines fill the
;;; space between the band and the subtitle, leaving a strip of hatching. SVG
;;; doesn't wrap text, so the lines are broken here, measured with the font
;;; files' own widths (see font-metrics.lisp).
;;;
;;; rsvg-convert finds the fonts through og-fonts/fonts.conf, which lists only
;;; that folder: the images come out the same on any machine. On macOS, pango
;;; uses Core Text unless PANGOCAIRO_BACKEND says fontconfig.

(shiso:define-setting :magazine/og-preview-enabled shiso:boolean-setting
  "Whether /og-preview answers: each article's preview image as the social
sites show it, for checking one before it is shared."
  :default (shiso:per-environment :development t :otherwise nil))

(defconstant +width+ 1200)
(defconstant +height+ 630)

;;; Places

(defun project-root ()
  (or (ignore-errors (asdf:system-source-directory "almightylisp"))
      (uiop:getcwd)))

(defun fonts-directory ()
  (asdf:system-relative-pathname "magazine" "og-fonts/"))

(defun image-directory ()
  (merge-pathnames "og/" (model:publish-directory)))

;;; Fonts: each one's file, and how the SVG names it.

(defparameter *fonts*
  '((:title "ScienceGothic-Medium.ttf" "Science Gothic" 500)
    (:dek "Mohave-Regular.ttf" "Mohave" 400)
    (:mono "ConstructMono.otf" "Construct Mono" 400)
    (:jp "JF-Dot-MPlusQ10-og.ttf" "JF Dot M+Q 10" 400)))

(defvar *metrics* (make-hash-table))

(defun metrics (key)
  (or (gethash key *metrics*)
      (setf (gethash key *metrics*)
            (fm:load-font (merge-pathnames (second (assoc key *fonts*)) (fonts-directory))))))

(defun width (key text size &optional (letter-spacing 0))
  (fm:text-width (metrics key) text size :letter-spacing letter-spacing))

(defun baseline (key top size line-height)
  "Where the baseline of a line LINE-HEIGHT tall at TOP falls, as Figma sets
it: the font's ascent and descent centred in the line."
  (let* ((font (metrics key))
         (scale (/ size (fm:font-units-per-em font)))
         (ascent (* scale (fm:font-ascender font)))
         (descent (* scale (- (fm:font-descender font)))))
    (+ top (/ (- line-height ascent descent) 2) ascent)))

(defun auto-baseline (key top size)
  "The baseline of a line at TOP whose height is the font's own."
  (let ((font (metrics key)))
    (baseline key top size (* (/ size (fm:font-units-per-em font))
                              (- (fm:font-ascender font) (fm:font-descender font))))))

;;; Breaking lines

(defun words (text)
  (cl-ppcre:split "\\s+" (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun join-words (words)
  (format nil "~{~a~^ ~}" words))

(defun wrap (text limit measure)
  "TEXT broken into lines no wider than LIMIT, by MEASURE, a function of a
string. NIL when one word is wider than LIMIT by itself."
  (let ((lines nil)
        (line nil))
    (dolist (word (words text))
      (when (> (funcall measure word) limit)
        (return-from wrap nil))
      (let ((longer (if line (concatenate 'string line " " word) word)))
        (cond ((or (null line) (<= (funcall measure longer) limit))
               (setf line longer))
              (t (push line lines)
                 (setf line word)))))
    (when line
      (push line lines))
    (nreverse lines)))

(defun ellipsize (line limit measure)
  "LINE cut short and ended with an ellipsis, no wider than LIMIT."
  (loop for words = (words line) then (butlast words)
        while words
        for cut = (concatenate 'string (join-words words) "…")
        when (<= (funcall measure cut) limit)
          return cut
        finally (return "…")))

(defun fit-line (line limit measure)
  "LINE, or LINE cut short with an ellipsis when it is wider than LIMIT."
  (if (<= (funcall measure line) limit)
      line
      (ellipsize line limit measure)))

(defun keep-lines (lines count limit measure)
  "The first COUNT of LINES, the last of them ended with an ellipsis when
some are left out. A second value is true when some were."
  (if (<= (length lines) count)
      (values lines nil)
      (let ((kept (subseq lines 0 count)))
        (values (append (butlast kept)
                        (list (ellipsize (car (last kept)) limit measure)))
                t))))

;;; The layout (Figma: OG / almighty-bakers). Every number is a pixel on the
;;; 1200 x 630 sheet.

(defparameter *column-left* 128)
(defparameter *column-width* 944)
(defparameter *column-top* 140)
(defparameter *column-bottom* 572)
(defparameter *gap* 18)
(defparameter *min-hatch* 20)

;; Measured widths leave this much to spare: kerning, which they leave out,
;; can widen a line a little.
(defparameter *slack* 0.99)

(defparameter *title-sizes* (loop for size from 180 downto 54 by 2 collect size)
  "The sizes a title may be set at, largest first.")

(defun title-line-height (size) (round (* size 0.85)))
(defun title-spacing (size) (* size -0.015))

(defparameter *dek-size* 26)
(defparameter *dek-line-height* 35)
(defparameter *dek-width* 640)
(defparameter *dek-lines* 4)
(defparameter *meta-top* 486)
(defparameter *meta-height* 86)

(defun title-measure (size)
  (lambda (text) (width :title text size (title-spacing size))))

(defun set-title (title-lines size)
  "TITLE-LINES, the title's own lines, broken to the column at SIZE."
  (let ((measure (title-measure size))
        (limit (* *slack* *column-width*)))
    (loop for line in title-lines
          for wrapped = (wrap (string-upcase line) limit measure)
          unless wrapped
            return nil
          append wrapped)))

(defun fit-title (title-lines height)
  "The largest size whose lines of TITLE-LINES fit HEIGHT, and those lines.
At the smallest size, as many lines as fit, the last cut short. A third value
is true when it was."
  (dolist (size *title-sizes*)
    (let ((lines (set-title title-lines size)))
      (when (and lines (<= (* (length lines) (title-line-height size)) height))
        (return-from fit-title (values size lines nil)))))
  (let* ((size (car (last *title-sizes*)))
         (measure (title-measure size))
         (limit (* *slack* *column-width*))
         ;; A word wider than the column at the smallest size: set it anyway.
         (lines (or (set-title title-lines size)
                    (list (string-upcase (join-words title-lines))))))
    (multiple-value-bind (kept cut)
        (keep-lines lines (max 1 (floor height (title-line-height size))) limit measure)
      (values size kept cut))))

(defun plain-text (text)
  "TEXT without its org markup: a link's label, the words of a code span."
  (labels ((flat (inline)
             (cond ((stringp inline) inline)
                   ((eq (first inline) :code) (second inline))
                   ((eq (first inline) :link) (flat-all (third inline)))
                   (t (flat-all (second inline)))))
           (flat-all (inlines)
             (format nil "~{~a~}" (mapcar #'flat inlines))))
    (join-words (words (flat-all (press:parse-inlines (or text "")))))))

(defun set-dek (subtitle)
  "SUBTITLE broken to its column, at most *DEK-LINES* of it. A second value is
true when some was cut."
  (let ((measure (lambda (text) (width :dek text *dek-size*)))
        (limit (* *slack* *dek-width*)))
    (keep-lines (or (wrap subtitle limit measure) (list subtitle))
                *dek-lines* limit measure)))

;;; Writing SVG

(defun num (x)
  "X as SVG writes a length: two decimals at most."
  (let ((text (string-right-trim "0" (format nil "~,2f" x))))
    (string-right-trim "." text)))

(defun escape (text)
  (with-output-to-string (out)
    (loop for char across text
          do (case char
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char char out))))))

(defun text (out key size x y content &key (fill "#000") opacity (anchor "start")
                                           (letter-spacing 0) transform)
  "A line of CONTENT in font KEY. CONTENT is a string, written escaped, or a
list of strings and (:square), a black square set in the Japanese font."
  (destructuring-bind (file family weight) (rest (assoc key *fonts*))
    (declare (ignore file))
    (format out "<text x=\"~a\" y=\"~a\" font-family=\"~a\" font-weight=\"~a\" font-size=\"~a\" fill=\"~a\"~@[ fill-opacity=\"~a\"~]~:[~*~; letter-spacing=\"~a\"~]~:[~*~; text-anchor=\"~a\"~]~@[ transform=\"~a\"~] xml:space=\"preserve\">"
            (num x) (num y) family weight (num size) fill (and opacity (num opacity))
            (/= letter-spacing 0) (num letter-spacing)
            (string/= anchor "start") anchor transform)
    (dolist (part (if (listp content) content (list content)))
      (if (stringp part)
          (write-string (escape part) out)
          (format out "<tspan font-family=\"~a\">■</tspan>" (third (assoc :jp *fonts*)))))
    (format out "</text>~%")))

(defun rect (out x y w h fill &key opacity)
  (format out "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" fill=\"~a\"~@[ fill-opacity=\"~a\"~]/>~%"
          (num x) (num y) (num w) (num h) fill (and opacity (num opacity))))

(defun patterned (out pattern x y w h)
  "A W by H box of PATTERN, the pattern starting at its corner."
  (format out "<g transform=\"translate(~a ~a)\"><rect width=\"~a\" height=\"~a\" fill=\"url(#~a)\"/></g>~%"
          (num x) (num y) (num w) (num h) pattern))

(defparameter *defs*
  "<defs>
<pattern id=\"dots\" width=\"14\" height=\"14\" patternUnits=\"userSpaceOnUse\"><rect width=\"2\" height=\"2\" fill=\"#c9c9c9\"/></pattern>
<pattern id=\"hatch\" width=\"8\" height=\"8\" patternUnits=\"userSpaceOnUse\"><path d=\"M-1 1L1 -1M0 8L8 0M7 9L9 7\" stroke=\"#c9c9c9\" stroke-width=\"1\"/></pattern>
<pattern id=\"hazard\" width=\"28\" height=\"28\" patternUnits=\"userSpaceOnUse\"><path d=\"M0 0H14L0 14ZM28 0V14L14 28H0Z\" fill=\"#000\"/></pattern>
</defs>
"
  "The sheet's patterns (Figma: Neo / Pattern, Paper): the dot grid, the
hatch and the hazard stripes.")

(defvar *logos* (make-hash-table :test #'equal))

(defun logo (name)
  "The drawing inside static/assets/images/magazine/NAME, in white."
  (or (gethash name *logos*)
      (setf (gethash name *logos*)
            (let* ((svg (uiop:read-file-string
                         (merge-pathnames (concatenate 'string "static/assets/images/magazine/" name)
                                          (project-root))))
                   (start (1+ (position #\> svg :start (search "<svg" svg))))
                   (end (search "</svg>" svg :from-end t)))
              (cl-ppcre:regex-replace-all "\"black\"" (subseq svg start end) "\"#fff\"")))))

(defun draw-sheet (out)
  "Everything on the image that isn't the article's: the dotted edges, the
rules, the running header and footer, and the black band's logo."
  (patterned out "dots" 0 0 40 +height+)
  (patterned out "dots" 1160 0 40 +height+)
  (dolist (x '(40 88 1112 1160))
    (rect out x 0 1 +height+ "#000" :opacity 0.18))
  (dolist (y '(28 602))
    (rect out 0 y +width+ 1 "#000" :opacity 0.18))
  (loop for (top strip) in '((0 ((104 "System loaded successfully") (440.3) (594.5 "オールマイティ" :jp)
                                 (824.8) (1096 "almightylisp.com" :mono "end")))
                             (602 ((104 "© Almighty Lisp") (319) (433 "Absolutely insane / absolutely (( no )) rust")
                                   (862) (1096 "戦わなければ勝てない" :jp "end"))))
        for line = (auto-baseline :mono (+ top 8) 12)
        do (dolist (item strip)
             (destructuring-bind (x &optional content (key :mono) (anchor "start")) item
               (if content
                   (text out key 12 x line (if (eq key :mono) (string-upcase content) content)
                         :opacity 0.7 :anchor anchor :letter-spacing (if (eq key :mono) 1.6 0))
                   (rect out (1+ x) (+ top 11.5) 6 6 "#000" :opacity 0.7)))))
  (rect out 88 29 1024 76 "#000")
  (format out "<g transform=\"translate(112 44) scale(~a)\">~a</g>~%" (num (/ 46 78)) (logo "almighty-logo.svg"))
  (format out "<g transform=\"translate(176 52) scale(~,5f)\">~a</g>~%" (/ 380 1322) (logo "almighty-lisp-typemark.svg"))
  (rect out 176 79 96 3 "#fff")
  (rect out 276 80 280 1 "#fff" :opacity 0.35)
  (loop for x in '(567.5 578.5 589.5 600.5)
        for filled in '(t nil nil t)
        do (format out "<circle cx=\"~a\" cy=\"80.5\" r=\"3\" ~:[fill=\"none\" stroke=\"#fff\" stroke-width=\"1\"~;fill=\"#fff\" stroke=\"#fff\" stroke-width=\"1\"~]/>~%"
                   (num x) filled))
  (loop for (x y dx dy) in '((104 121 1 1) (1096 121 -1 1) (104 586 1 -1) (1096 586 -1 -1))
        do (rect out (if (plusp dx) x (- x 12)) (if (plusp dy) y (- y 2)) 12 2 "#000")
           (rect out (if (plusp dx) x (- x 2)) (if (plusp dy) y (- y 12)) 2 12 "#000"))
  (loop for (x angle words) in '((64 -90 ("Powerful" "Propaganda")) (1136 90 ("Become" "Almighty")))
        ;; Centred on the gutter: a 13px line around 0, turned.
        do (text out :mono 13 0 (baseline :mono -6.5 13 13)
                 (list (string-upcase (first words)) "  " '(:square) "  " (string-upcase (second words)))
                 :letter-spacing 3 :anchor "middle"
                 :transform (format nil "translate(~a 356) rotate(~a)" x angle))))

;;; An article's image

(defstruct image
  article
  ;; The PNG's name in the image directory, and the SVG it is drawn from.
  file svg
  ;; The title's size, its lines, and whether it was cut short; the
  ;; subtitle's lines, and whether it was.
  title-size title-lines title-cut dek-lines dek-cut)

(defun band-texts (article in-series)
  "The black band's two labels, small and large: a series part's type and
topic, and its part; any other article's topic, and its type."
  (if in-series
      (values (format nil "~a / ~a" (model:article-type article) (model:article-topic article))
              (format nil "Part ~d/~d" (model:article-part article) (model:series-total in-series)))
      (values (format nil "Topic / ~a" (model:article-topic article))
              (model:article-type article))))

(defun article-svg (article in-series)
  "ARTICLE's image as SVG, and an IMAGE describing it, without its file."
  (let* ((kicker (when in-series
                   (format nil "Series / ~a" (model:series-title in-series))))
         (title-top (if kicker (+ *column-top* 14 *gap*) *column-top*)))
    (multiple-value-bind (dek-lines dek-cut) (set-dek (plain-text (model:article-subtitle article)))
      (let* ((row-height (max (* (length dek-lines) *dek-line-height*) *meta-height*))
             (row-top (- *column-bottom* row-height))
             (room (- row-top *gap* *min-hatch* *gap* title-top)))
        (multiple-value-bind (size title-lines title-cut)
            (fit-title (model:article-title-lines article) room)
          (let* ((line-height (title-line-height size))
                 (hatch-top (+ title-top (* (length title-lines) line-height) *gap*))
                 (svg (with-output-to-string (out)
                        (format out "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~d\" height=\"~d\" viewBox=\"0 0 ~d ~d\">~%"
                                +width+ +height+ +width+ +height+)
                        (write-string *defs* out)
                        (rect out 0 0 +width+ +height+ "#fff")
                        (draw-sheet out)
                        (multiple-value-bind (small large) (band-texts article in-series)
                          (text out :mono 12 1088 (auto-baseline :mono 40 12) (string-upcase small)
                                :fill "#fff" :opacity 0.6 :letter-spacing 2.6 :anchor "end")
                          (text out :title 30 1088 (baseline :title 58 30 27) (string-upcase large)
                                :fill "#fff" :letter-spacing -0.3 :anchor "end"))
                        (when kicker
                          (text out :mono 14 *column-left* (auto-baseline :mono *column-top* 14)
                                (list '(:square) " "
                                      (fit-line (string-upcase kicker) (- *column-width* 40)
                                                (lambda (text) (width :mono text 14 3))))
                                :fill "#8f8f8f" :letter-spacing 3))
                        (loop for line in title-lines
                              for top from title-top by line-height
                              do (text out :title size *column-left* (baseline :title top size line-height)
                                       line :letter-spacing (title-spacing size)))
                        (patterned out "hatch" *column-left* hatch-top *column-width*
                                   (- row-top *gap* hatch-top))
                        (loop with top = (- *column-bottom* (* (length dek-lines) *dek-line-height*))
                              for line in dek-lines
                              for line-top from top by *dek-line-height*
                              do (text out :dek *dek-size* *column-left*
                                       (baseline :dek line-top *dek-size* *dek-line-height*) line))
                        (loop for (label value) in (list (list "By........." (model:article-author article))
                                                         (list "Serial....." (model:article-code article))
                                                         (list "Activated.." (press:format-date (model:article-date article) :compact)))
                              for top from *meta-top* by 20
                              for line = (auto-baseline :mono top 14)
                              do (text out :mono 14 812 line (string-upcase label) :fill "#8f8f8f" :letter-spacing 1.4)
                                 (text out :mono 14 908 line (string-upcase (or value "")) :letter-spacing 1.4))
                        (patterned out "hazard" 812 546 260 26)
                        (format out "</svg>~%"))))
            (make-image :article article :svg svg
                        :title-size size :title-lines title-lines :title-cut title-cut
                        :dek-lines dek-lines :dek-cut dek-cut)))))))

(defun digest (text)
  (let ((bytes (md5:md5sum-string text :external-format :utf-8)))
    (format nil "~(~{~2,'0x~}~)" (coerce (subseq bytes 0 5) 'list))))

(defun article-image (article series)
  "ARTICLE's preview image: its SVG, and the name its PNG has once drawn.
SERIES is every series, to find ARTICLE's."
  (let ((image (article-svg article (model:article-series-of article series))))
    (setf (image-file image)
          (format nil "~a-~a.png" (model:article-slug article) (digest (image-svg image))))
    image))

;;; Drawing

(defvar *renderer-problem* nil
  "Why the last image couldn't be drawn, or NIL.")

(defun renderer-problem () *renderer-problem*)

(defun image-path (image)
  (merge-pathnames (image-file image) (image-directory)))

(defun image-drawn-p (image)
  (and (probe-file (image-path image)) t))

(defun image-static-path (image)
  "IMAGE's PNG as a path under static/, for SHISO:STATIC."
  (concatenate 'string "magazine/published/og/" (image-file image)))

(defun draw (image)
  "Draw IMAGE's PNG with rsvg-convert. True when it was drawn; when it wasn't,
*RENDERER-PROBLEM* says why."
  (let* ((path (image-path image))
         (stem (format nil "~a.~d" (pathname-name path) (random 1000000)))
         (svg (make-pathname :name stem :type "svg" :defaults path))
         (part (make-pathname :name stem :type "png" :defaults path)))
    (ensure-directories-exist path)
    (with-open-file (out svg :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string (image-svg image) out))
    (unwind-protect
         (multiple-value-bind (output error status)
             (uiop:run-program (list "env" "PANGOCAIRO_BACKEND=fc"
                                     (format nil "FONTCONFIG_FILE=~a"
                                             (uiop:native-namestring
                                              (merge-pathnames "fonts.conf" (fonts-directory))))
                                     "rsvg-convert" "--format" "png"
                                     "--output" (uiop:native-namestring part)
                                     (uiop:native-namestring svg))
                               :ignore-error-status t :output :string :error-output :string)
           (declare (ignore output))
           (cond ((zerop status)
                  (rename-file part path)
                  (setf *renderer-problem* nil)
                  t)
                 (t (setf *renderer-problem*
                          (if (= status 127)
                              "rsvg-convert isn't installed (apt install librsvg2-bin, or brew install librsvg)."
                              (format nil "rsvg-convert failed: ~a" (string-trim '(#\Newline) error))))
                    nil)))
      (when (probe-file svg) (delete-file svg))
      (when (probe-file part) (delete-file part)))))

(defun ensure-images (articles series)
  "Each of ARTICLES' images, drawn when it isn't yet. Images of articles that
are gone or have changed are deleted."
  (let ((images (mapcar (lambda (article) (article-image article series)) articles)))
    (ensure-directories-exist (image-directory))
    (dolist (image images)
      (unless (image-drawn-p image)
        (unless (draw image)
          (warn "Preview image ~a not drawn: ~a" (image-file image) *renderer-problem*))))
    (dolist (old (directory (merge-pathnames "*.png" (image-directory))))
      (unless (member (file-namestring old) images :key #'image-file :test #'string=)
        (delete-file old)))
    images))
