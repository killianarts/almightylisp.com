(in-package #:magazine/hypermedia/components)

;;;; The document, and the sheet each page is set on.
;;;
;;; A page is a grey sheet on a five-column grid: a dotted edge, a gutter, the
;;; main column, a gutter, a dotted edge. Its content is a stack of bands.
;;; Each band spans the sheet, puts its content in the main column, and can
;;; carry a vertical label in each gutter. Bands are placed on fixed grid rows
;;; (see magazine.css), so the full-height rules and dots line up with them.

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
       (script :src (asset "js/magazine-fx.js") :defer t))
     (body :class body-class :hx-boost "true" :hx-ext "preload"
       children
       (script :src (asset "js/highlight-lisp.js"))
       (script "HighlightLisp.highlight_auto(); HighlightLisp.paren_match();")))))

(ah:define-component ac-sheet-page (&key title
                                         (description "A magazine about Common Lisp and Emacs.")
                                         page-class sheet-id scripts children)
  "A whole magazine page: the sheet with its running header, nameplate and
strip, then CHILDREN, the page's own bands, then the running footer. JP sets
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
         (render-running-footer))
       scripts))))

(ah:define-component ac-band (&key class side children)
  "One band of the sheet. SIDE, a list (left right), labels the gutters."
  (ah:</>
   (div :class (ah:clsx "band" class)
     children
     (when side (render-side-labels (first side) (second side))))))

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
render-jp) set upright below the text."
  (destructuring-bind (text &optional jp) (if (listp left) left (list left))
    (ah:</>
     (<>
       (if jp
           (ah:</>
            (span :class "side-label left has-jp" :aria-hidden t
              (span (string-upcase text))
              (render-jp jp)))
           (ah:</> (span :class "side-label left" :aria-hidden t (string-upcase text))))
       (span :class "side-label right" :aria-hidden t (string-upcase right))))))

;;; Japanese labels are set in a pixel face the site doesn't load, so each is
;;; an SVG of its outlines, exported from Figma (Homepage (v22)).

(defparameter *jp-labels*
  '(("almighty" "オールマイティ" 102 13)
    ("tatakawanakereba-katenai" "タタカワナケレバカテナイ" 149 11)
    ("series" "シリーズ" 56 16)
    ("briefing" "ブリーフィング" 99 16)
    ("omona-series" "おもなシリーズ" 15 116))
  "Name, text, width and height of each SVG in images/magazine/jp/.")

(defun render-jp (name &key class)
  (destructuring-bind (text width height) (rest (assoc name *jp-labels* :test #'string=))
    (ah:</>
     (img :class (ah:clsx "jp" class) :src (image-src (format nil "jp/~a.svg" name))
       :alt text :lang "ja" :width (princ-to-string width) :height (princ-to-string height)))))

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
       (span "No build step  /  Absolutely (( No )) Rust")
       (render-sep)
       (render-jp "tatakawanakereba-katenai")))))

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

(defun render-section-head (key title meta &key jp action action-href)
  "A grey gap band, then the section name set large on a black band with a
hazard strip in the left gutter. KEY names the section; its bands' classes
place them on the sheet's rows. JP, a Japanese label (see render-jp), sits
above META."
  (ah:</>
   (<>
     (div :class (format nil "band gap-band ~a-gap" key) :aria-hidden t)
     (div :class (format nil "band head-band ~a-head" key)
       (span :class "hazard" :aria-hidden t)
       (h2 :class "section-head"
         (span :class "section-title" title)
         (if jp
             (ah:</>
              (span :class "section-tag"
                (render-jp jp)
                (span :class "section-meta" meta)))
             (ah:</> (span :class "section-meta" meta)))
         (when action
           (ah:</> (a :class "section-action" :href action-href action))))))))

(defun render-end-band (label content &key dark folio)
  "The band that closes an article: a label row over CONTENT."
  (ah:</>
   (ac-band :class "end-band"
     (div :class "end-block"
       (p :class (ah:clsx "end-label" (when dark "dark"))
         (if folio (ah:</> (span label)) label)
         (when folio (ah:</> (span :class "end-folio" folio))))
       content))))

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
