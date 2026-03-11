(defpackage #:book/hypermedia
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html)
                    (#:css #:lass))
  (:export #:ac-skeleton
           #:ac-code-block
           #:ac-book-layout
           #:ac-sidebar-layout))

(in-package #:book/hypermedia)

(css:define-special-block layer (&rest args)
  "Full native support for CSS Cascade Layers (@layer) in LASS.

   Declaration forms:
     (:layer reset base utilities)
     (:layer \"reset, base, utilities\")

   Named block form:
     (:layer base
       (html :font-size \"16px\")
       (.card :background white))"
  (cond
    ;; 1. Single-string declaration (most common in real CSS)
    ;;    (:layer "reset, base, utilities")
    ((and (= (length args) 1)
          (stringp (first args)))
     (list (css:make-property (format nil "@layer ~a" (first args)))))

    ;; 2. Multiple-name declaration (symbols/keywords/strings)
    ;;    (:layer reset base utilities)
    ((every (lambda (x) (or (stringp x) (symbolp x) (keywordp x))) args)
     (list (css:make-property
            (format nil "@layer ~{~a~^, ~}"
                    (mapcar #'css:resolve args)))))

    ;; 3. Named layer block (the important one)
    ;;    (:layer base ...rules...)
    (t
     (destructuring-bind (name &rest body) args
       (list (css::make-superblock
              "layer"
              (list (list :constraint :literal (css:resolve name)))
              (apply #'css:compile-sheet body)))))))



(ah:define-component ac-skeleton (&key title children)
  (let ((html-class "dark")
        (html-lang "en"))
    (ah:</>
     (html :class html-class :lang html-lang
       (head
         (title (str:concat (string-upcase title) " - " (string-upcase "almightylisp.com")))
         (meta :charset "utf-8")
         (meta :name "viewport" :content "width=device-width, initial-scale=1")
         (link :rel "stylesheet" :href (shiso/utils:static "css/almighty-lisp-essentials.css"))
         (link :rel "preconnect" :href "https://fonts.googleapis.com")
         (link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin t)
         (link :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Instrument+Serif:ital@0;1&family=Oswald:wght@200..700&display=swa")
         (link :rel "apple-touch-icon" :sizes "180x180" :href (shiso:static "assets/images/favicon/apple-touch-icon.png"))
         (link :rel "icon" :type "image/png" :sizes "32x32" :href (shiso:static "assets/images/favicon/favicon-32x32.png"))
         (link :rel "icon" :type "image/png" :sizes "16x16" :href (shiso:static "assets/images/favicon/favicon-16x16.png"))
         (link :rel "manifest" :href (shiso:static "assets/images/favicon/site.webmanifest"))
         (script :src (shiso:static "js/highlight-lisp.js"))
         ;; (ac-meta-information :title title :description "almightylisp.com")
         )
       (body
         children
         ;; syntax highlighting
         (script "HighlightLisp.highlight_auto();"))))))

(ah:define-component ac-sidebar-layout (&key chapter headings children)
  (ah:</>
   (ac-skeleton :title chapter
     (div :class "sidebar-container"
       (div :class "book-navigation"
         (p :class "book-navigation__chapter-name-heading" (span "Chapter Name"))
         (nav
           (ul
             (li :class "book-navigation__section-link-container" (a :class "book-navigation__section-link" :href "/" (span :class "book-navigation__section-name" "Section Name") (span :class "book-navigation__section-number" "1.0.0")))
             (li :class "book-navigation__section-link-container" (a :class "book-navigation__section-link" :href "/" (span :class "book-navigation__section-name" "Section Name") (span :class "book-navigation__section-number" "1.1.0")))
             (li :class "book-navigation__section-link-container" (a :class "book-navigation__section-link" :href "/" (span :class "book-navigation__section-name" "Section Name") (span :class "book-navigation__section-number" "1.2.0")))
             (li :class "book-navigation__section-link-container" (a :class "book-navigation__section-link" :href "/" (span :class "book-navigation__section-name" "Section Name") (span :class "book-navigation__section-number" "1.3.0"))))
           )
         (div :class "book-navigation__previous-and-next" (a :href "previous/" :class "book-navigation__previous-section" "Previous") (a :href "next/" :class "book-navigation__next-section" "Next"))))
     (main :class "sidebar-main" children)
     (button :class "toc-viewer" "TOC"))))

(ah:define-component ac-code-block (&key children)
  (ah:</>
   (pre (code :class "lisp" children))))
