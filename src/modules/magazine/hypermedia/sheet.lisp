(in-package #:magazine/hypermedia/components)

;;;; The document, and the sheet each page is set on.
;;;
;;; A page is a grey sheet on a five-column grid: a dotted edge, a gutter, the
;;; main column, a gutter, a dotted edge. Its content is a stack of bands.
;;; Each band spans the sheet, puts its content in the main column, and can
;;; carry a vertical label in each gutter. Bands are placed on fixed grid rows
;;; (see magazine.css), so the full-height rules and dots line up with them.
;;;
;;; The page's ornaments are two small parts (Figma: Homepage restyled · C1 ·
;;; Landing): a gauge, a short bar part filled and part hatched, and a pair
;;; of dots. They stand in the gutters beside a band, in section heads and in
;;; the black panels.

(ah:define-component ac-magazine-document (&key title description body-class children)
  (ah:</>
   (html :lang "en"
     (head
       (meta :charset "utf-8")
       (meta :name "viewport" :content "width=device-width, initial-scale=1")
       (title title)
       (meta :name "description" :content description)
       (link :rel "icon" :type "image/png" :sizes "32x32"
         :href (asset "assets/images/favicon/favicon-32x32.png"))
       ;; Loads the fonts once the page is up, and switches them in once
       ;; they're all here. Not deferred: it hides the sheet for a moment
       ;; before the first paint (see magazine-fonts.js).
       (script :src (asset "js/magazine-fonts.js"))
       (noscript (link :rel "stylesheet" :href (asset "css/magazine-fonts.css")))
       ;; magazine.css imports resets.css; fetch it alongside, not after.
       ;; The same URL as the import, so it's one download.
       (link :rel "preload" :as "style" :href "/static/css/resets.css")
       (link :rel "stylesheet" :href (asset "css/magazine.css"))
       ;; htmx boosts magazine links into in-page swaps; magazine-fx.js
       ;; configures it, so it has to load first.
       (script :src (asset "js/htmx-2.0.11.min.js") :defer t)
       (script :src (asset "js/htmx-ext-preload-2.1.2.js") :defer t)
       (script :src (asset "js/magazine-fx.js") :defer t)
       ;; Justified text: Pretext measures it, magazine-justify.js sets it.
       (script :src (asset "js/pretext-0.0.9.min.js") :defer t)
       (script :src (asset "js/magazine-justify.js") :defer t))
     (body :class body-class :hx-boost "true" :hx-ext "preload"
       children
       (script :src (asset "js/highlight-lisp.js"))
       (script "HighlightLisp.highlight_auto(); HighlightLisp.paren_match();")))))

(ah:define-component ac-sheet-page (&key title
                                         (description "A magazine about Common Lisp and Emacs.")
                                         page-class sheet-id scripts children)
  "A whole magazine page: the sheet with its running header, nameplate and
strip, then CHILDREN, the page's own bands, then the footer. JP sets
the header's centre in Japanese. SCRIPTS go after the sheet."
  (let ((body-class (ah:clsx "mag" page-class)))
    (ah:</>
     (ac-magazine-document :title title :description description :body-class body-class
       ;; htmx swaps only the body's contents; magazine-fx.js sets the body's
       ;; class from data-body-class after a swap.
       (div :class "sheet" :id sheet-id :data-body-class body-class
         (render-sheet-furniture)
         (render-running-header)
         (render-nameplate)
         (render-strip)
         children
         (render-site-footer)
         (render-running-footer))
       scripts))))

(ah:define-component ac-band (&key class side marks children)
  "One band of the sheet. SIDE, a list (left right), labels the gutters; either
can be NIL. MARKS draws the gutter marks (see render-gutter-marks): T for
all of them, :FOOT for the gauge alone."
  (ah:</>
   (div :class (ah:clsx "band" class)
     children
     (when side (render-side-labels (first side) (second side)))
     (when marks (render-gutter-marks :dots (not (eq marks :foot)))))))

;;; Gauges and dot pairs

(defun render-gauge (&key class)
  (ah:</>
   (span :class (ah:clsx "gauge" class) :aria-hidden t
     (i :class "done")
     (i :class "tick")
     (i :class "rest"))))

(defun render-dot-pair (&key alt class)
  "Two dots, the first filled. ALT is the other pair: an open dot, then a dim one."
  (ah:</>
   (span :class (ah:clsx "dot-pair" (when alt "alt") class) :aria-hidden t
     (i)
     (i))))

(defun render-dot-pairs (&key class)
  "Both pairs side by side, for a panel's corner."
  (ah:</>
   (span :class class :aria-hidden t
     (render-dot-pair)
     (render-dot-pair :alt t))))

(defvar *marked-bands* 0
  "How many bands of the page being rendered have gutter marks so far, so the
two kinds of dot pair change sides from one band to the next down the page.
Each page function binds it around its render. Only ac-band counts: a
component's body runs as the page is rendered, in page order, while a plain
function called in a template runs earlier, as the template is built.")

(defun render-marks (swap &key (dots t))
  "The marks in the gutters beside a band, or a section of an article: a dot
pair at the top of each, and a gauge at the foot of the right one. SWAP
trades the two kinds of pair. Without DOTS, the gauge alone."
  (ah:</>
   (<>
     (when dots
       (ah:</>
        (<>
          (render-dot-pair :alt swap :class "gutter-mark left")
          (render-dot-pair :alt (not swap) :class "gutter-mark right"))))
     (render-gauge :class "gutter-mark foot"))))

(defun render-gutter-marks (&key (dots t))
  "The next band's marks, the pairs on the other sides from the band before."
  (let ((swap (oddp *marked-bands*)))
    (incf *marked-bands*)
    (render-marks swap :dots dots)))

(defun render-sheet-furniture ()
  "Gutter dots and column rules, running the sheet's full height."
  (ah:</>
   (div :class "sheet-furniture" :aria-hidden t
     (span :class "edge left")
     (span :class "edge right")
     (span :class "vrule r1")
     (span :class "vrule r2")
     (span :class "vrule r3")
     (span :class "vrule r4"))))

(defun render-side-labels (left right)
  "Vertical labels in the two gutters, centred on the band they sit in. LEFT
can be a list (text jp), with JP the name of a Japanese label (see
render-jp) set upright below the text. A gutter whose label is NIL is bare."
  (destructuring-bind (&optional text jp) (if (listp left) left (list left))
    (ah:</>
     (<>
       (cond (jp
              (ah:</>
               (span :class "side-label left has-jp" :aria-hidden t
                 (span (string-upcase text))
                 (render-jp jp))))
             (text
               (ah:</> (span :class "side-label left" :aria-hidden t (string-upcase text)))))
       (when right
         (ah:</> (span :class "side-label right" :aria-hidden t (string-upcase right))))))))

;;; Japanese labels are set in pixel faces (see .jp in magazine.css): JF Dot
;;; M+Q 10 in the running header, the running footer and the gutters, KKM
;;; Analog TV beside a section's name. The site's copy of JF Dot has only the
;;; kanji listed in dev/jfdot-vert.py: add a new label's there. The footer's
;;; motto is in JF Dot Kappa 20 0213, and the site's copy of that has only the
;;; motto's characters.

(defparameter *jp-labels*
  '(("almighty" "オールマイティ")
    ("tatakawanakereba-katenai" "戦わなければ勝てない")
    ("series" "シリーズ")
    ("briefing" "ブリーフィング")
    ("omona-series" "主なシリーズ")
    ("shinzou-wo-sasageyou" "心臓を捧げよう"))
  "Name and text of each Japanese label.")

(defun render-jp (name &key class)
  (let ((text (second (assoc name *jp-labels* :test #'string=))))
    (ah:</> (span :class (ah:clsx "jp" class) :lang "ja" text))))

(defun render-sep ()
  (ah:</> (span :class "sep" :aria-hidden t "■")))

(defun render-running-header ()
  (ah:</>
   (div :class "band running top"
     (div :class "running-row"
       (span "System Loaded Successfully")
       (render-sep)
       (render-jp "almighty" :class "running-center")
       (render-sep)
       (span :class "running-status" "Image Status: Reading Input")))))

(defun render-running-footer ()
  (ah:</>
   (footer :class "band running bottom"
     (div :class "running-row"
       (span "© 2026 Almighty Lisp")
       (render-sep)
       (span "Absolutely Insane / Absolutely (( No )) Rust")
       (render-sep)
       (render-jp "tatakawanakereba-katenai")))))

(defun render-site-footer ()
  "The black band that closes every page: the colophon, the book and the theme
packs. On a narrow page, which has no book band, this is where the book is.
htmx leaves the shop links alone: they leave the magazine."
  (ah:</>
   (<>
     (div :class "band gap-band footer-gap" :aria-hidden t)
     (ac-band :class "footer-band" :side (list (joined "Book" "Themes") nil) :marks t
       (div :class "site-footer"
         (a :class "footer-colophon" :href (magazine-href)
           (img :class "footer-mark" :src (image-src "almighty-logo-outline.png") :alt ""
             :width "40" :height "42")
           (img :class "footer-typemark" :src (image-src "almighty-lisp-typemark.svg")
             :alt "Almighty Lisp" :width "377" :height "17")
           (render-dot-pairs :class "panel-corner")
           (render-jp "shinzou-wo-sasageyou" :class "footer-motto"))
         (section :class "footer-block footer-book" :aria-label "Lisp & Emacs Essentials"
           :hx-boost "false"
           (p :class "book-ad-label" "The Book (your eyes only)")
           (img :class "book-ad-logo" :src (image-src "essentials-logo.svg")
             :alt "Lisp & Emacs Essentials" :width "156" :height "69")
           (render-gauge :class "footer-gauge")
           (div :class "footer-actions"
             (a :class "neo-button primary" :href *book-href* "Read free online")
             (a :class "neo-button secondary" :href *ebook-href* "Buy ebook")
             (a :class "neo-button secondary" :href *hardcover-href* "Buy hardcover"))
           (render-dot-pairs :class "panel-corner"))
         (section :class "footer-block footer-themes" :aria-label "Theme packs"
           :hx-boost "false"
           (p :class "book-ad-label" (joined "Doom Emacs" "16 themes"))
           (h2 :class "footer-title" "Theme Packs")
           (render-spec *theme-packs* :class "spec book-spec")
           (div :class "footer-actions"
             (mapcar (lambda (pack)
                       (ah:</> (a :class "neo-button secondary" :href (third pack) (first pack))))
                     *theme-packs*))
           (render-dot-pairs :class "panel-corner")))))))

(defun render-nameplate ()
  (ah:</>
   (ac-band :class "nameplate-band"
     (a :class "neo-nameplate" :href (magazine-href)
       (img :class "neo-logo" :src (image-src "almighty-logo.png") :alt ""
         :width "75" :height "78")
       (img :class "neo-typemark" :src (image-src "almighty-lisp-typemark.svg") :alt "Almighty Lisp"
         :width "1322" :height "59")))))

(defun render-strip ()
  (ah:</>
   (ac-band :class "strip-band"
     (div :class "strip"
       (div :class "strip-cell start" "Welcome Back")
       (div :class "strip-cell mid"
         (span :class "date-long" (press:format-date (press:today) :weekday))
         (span :class "date-short" (press:format-date (press:today) :short-weekday)))
       (div :class "strip-cell end" (a :href *book-href* "Become Almighty"))))))

(defun render-section-head (key title meta &key jp action action-href (gap t))
  "A grey gap band, then the section name set large on a black band. KEY names
the section; its bands' classes place them on the sheet's rows. After the
name comes META and a rule (a gauge and two dot pairs); with JP, a Japanese
label (see render-jp), the label stands over the rule and META is not
shown. ACTION is a link when ACTION-HREF is given. Without GAP, the head sits
against the band before it."
  (ah:</>
   (<>
     (when gap
       (ah:</> (div :class (format nil "band gap-band ~a-gap" key) :aria-hidden t)))
     (ac-band :class (format nil "head-band ~a-head" key) :marks t
       (h2 :class "section-head"
         (span :class "section-title" title)
         (if jp
             (ah:</>
              (span :class "section-tag"
                (render-jp jp)
                (render-head-rule)))
             (ah:</>
              (<>
                (span :class "section-meta" meta)
                (render-head-rule))))
         (cond ((and action action-href)
                (ah:</> (a :class "section-action" :href action-href action)))
               (action (ah:</> (span :class "section-action" action)))))))))

(defun render-head-rule ()
  (ah:</>
   (span :class "head-rule" :aria-hidden t
     (render-gauge)
     (render-dot-pair)
     (render-dot-pair :alt t))))

(defun render-end-transmission ()
  "The line that closes an article's text, above its end head."
  (ah:</>
   (ac-band :class "transmission-band"
     (p :class "end-transmission"
       (span :class "spec-lead" :aria-hidden t)
       (span :class "end-transmission-text" "End transmission")
       (span :class "spec-lead" :aria-hidden t)))))

(defun render-end-band (label content)
  "The band that closes a page: CONTENT, under a row for LABEL when there is one."
  (ah:</>
   (ac-band :class "end-band" :marks t
     (div :class "end-block"
       (when label (ah:</> (p :class "end-label" label)))
       content))))

;;; Eyebrows and progress rules (Figma: Article (v30), Homepage (v30)): a
;;; small tracked label over a heading, and under it a bar filled as far as
;;; the reader, or the series, has got.

(defun render-eyebrow (text &key marks)
  "TEXT as the label over a heading. MARKS sets squares either side of it."
  (ah:</>
   (p :class "eyebrow"
     (when marks (ah:</> (i :class "sq")))
     (span text)
     (when marks
       (ah:</>
        (<>
          (i :class "sq open")
          (i :class "sq")))))))

(defun render-progress-bar (fraction)
  "A bar filled to FRACTION, a tick, then the rest hatched."
  (let ((full (>= fraction 1)))
    (ah:</>
     (span :class (ah:clsx "progress-bar" (when full "full")) :aria-hidden t
       :style (format nil "--p: ~,5f" fraction)
       (i :class "done")
       (i :class "tick")
       (unless full (ah:</> (i :class "rest")))))))

(defun render-rule-dots ()
  (ah:</>
   (span :class "rule-dots" :aria-hidden t
     (i)
     (i))))

(defun render-hatch ()
  "Spacer hatch for the space left over in a column. It grows into whatever
room is left and shows nothing when there is none."
  (ah:</> (span :class "hatch-fill" :aria-hidden t)))

(defun render-crop-marks (&optional (corners '("tl" "tr" "bl" "br")))
  (ah:</>
   (<>
     (mapcar (lambda (corner)
               (ah:</> (span :class (ah:clsx "crop" corner) :aria-hidden t)))
             corners))))

(defun render-spec (rows &key (class "spec"))
  "Term, dot leader, value: one row per (term value)."
  (ah:</>
   (dl :class class
     (mapcar (lambda (row)
               (ah:</>
                (div :class "spec-row"
                  (dt (first row))
                  (span :class "spec-lead" :aria-hidden t)
                  (dd (second row)))))
             rows))))
