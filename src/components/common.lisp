(defpackage #:almightylisp/components/common
  (:use #:cl #:almighty-html)
  (:export
   #:ac-skeleton
   #:ac-nav-link
   #:ac-admin-layout
   #:ac-site-layout
   #:ac-meta-information
   #:ac-code-block
   #:ac-teaser-slide
   #:ac-hero-text
   #:ac-code))
(in-package #:almightylisp/components/common)

(define-component ac-meta-information (&key title description url children)
  (</>
   (<>
     ;; X/Twitter
     (meta :name "twitter:card" :content "summary_large_image")
     (meta :name "twitter:site" :content "@killian_arts")
     (meta :name "twitter:creator" :content "@killian_arts")
     (meta :name "twitter:title" :content title)
     (meta :name "twitter:description" :content description)
     (meta :name "twitter:image" :content (shiso:static "assets/images/x-posts-preview-image.png"))
     ;; Opengraph (Facebook)
     (meta :property "og:title" :content title)
     (meta :property "og:description" :content description)
     (meta :property "og:image" :content (shiso:static "assets/images/x-posts-preview-image.png"))
     ;; TODO add current-page to shiso
     (meta :property "og:url" :content (shiso:current-path))
     (meta :property "og:type" :content title))))

(define-component ac-skeleton (&key title children)
  (let ((html-class (string-downcase "dark"))
        (html-lang "en"))
    (</>
     (html :class html-class :lang html-lang
       (head
         (title (str:concat (string-upcase title) " /// " (string-upcase "almightylisp.com")))
         (meta :charset "utf-8")
         (meta :name "viewport" :content "width=device-width, initial-scale=1")
         
         (link :href "css/almightylisp.css" :rel "stylesheet" :type "text/css")
         (link :rel "preconnect" :href "https://fonts.googleapis.com")
         (link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin t)
         (link :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Instrument+Serif:ital@0;1&family=Oswald:wght@200..700&display=swa")
         (link :rel "apple-touch-icon" :sizes "180x180" :href "assets/images/favicon/apple-touch-icon.png")
         (link :rel "icon" :type "image/png" :sizes "32x32" :href "assets/images/favicon/favicon-32x32.png")
         (link :rel "icon" :type "image/png" :sizes "16x16" :href "assets/images/favicon/favicon-16x16.png")
         (link :rel "manifest" :href "assets/images/favicon/site.webmanifest")
         (script :src "js/highlight-lisp.js")
         (ac-meta-information :title title :description "almightylisp.com"))
       ;; (script :src "js/almighty-animations.js" :type "module")
       (body
         children
         ;; syntax highlighting
         (script "HighlightLisp.highlight_auto();"))))))

(define-component ac-nav-link (&key url text)
  (</>
   (span :class "relative"
     (span :class (clsx "absolute inset-y-0 -left-4 w-0.5"
                        (if (string= url (lack/request:request-path-info shiso:*request*)) "bg-secondary" "bg-primary-50")))
     (a :href url
       :class (clsx "text-left sm:text-sm hover:text-accent hover:bg-primary-700 flex w-full"
                    (if (string= url (lack/request:request-path-info shiso:*request*)) "text-secondary" "text-primary-50"))
       (span :class "truncate" text)))))

(define-component ac-admin-layout (&key title (header "Default Header") (subheader "default subheader") children)
  (</>
   (ac-skeleton
     :title title
     (div :class "flex selection:bg-primary-darker"
       (div :class "relative isolate flex min-h-svh w-full bg-primary max-lg:flex-col
                            dark:bg-primary dark:lg:bg-primary-800"
         (div :class "fixed border-r-1 border-primary-900 inset-y-0 left-0 w-(--sidebar-width) max-lg:hidden"
           (nav :class "font-berkeley uppercase text-accent flex h-full min-h-0 flex-col"
             (div :class "flex flex-col border-b border-primary-900 p-4 dark:border-primary-900
                                           [&amp;>[data-slot=section]+[data-slot=section]]:mt-2.5"
               (span :class "relative"
                 (button :type "button" :aria-haspopup "menu" :aria-expanded "false" :class "flex w-full"
                   (span
                     :class "absolute top-1/2 left-1/2 size-[max(100%,2.75rem)] -translate-x-1/2 -translate-y-1/2
                                                          [@media(pointer:fine)]:hidden"
                     :aria-hidden "true")
                   (span :data-slot "avatar" :class "flex items-center gap-5"
                     (span
                       :class "uppercase font-berkeley-thin text-3xl"
                       "Shiso")))))
             (div :class "flex flex-1 flex-col overflow-y-auto p-4 [&amp;>[data-slot=section]+[data-slot=section]]:mt-8"
               (div :data-slot "section"
                 :class "flex flex-col"
                 (ac-nav-link :url "/" :text "Home"))
               (div :data-slot "section"
                 :class "flex flex-col"
                 (h2 :class "text-sm" "Keyboard")
                 (ac-nav-link :url "/keyboard/alphabet" :text "Alphabet")))))
         (main :id "main" :class "ml-72"
           (header :class "w-fit"
             (h1 :class "font-berkeley text-3xl uppercase text-secondary" (string-upcase header))
             (p :class "font-berkeley text-accent bg-primary-900 uppercase" (string-upcase subheader)))
           children))))))

(define-component ac-site-layout (&key title children)
  (</>
   (ac-skeleton
     :title title
     (main :class "overflow-x-hidden"
       children))))



(define-component ac-code-block (&key children)
  (</>
   (pre :class "slide__code-block bg-primary p-5 overflow-x-auto text-sm/4" (code :class "lisp font-berkeley-uc" children))))

(define-component ac-teaser-slide (&key id class heading code children)
  (</>
   (section :id id :class (almighty-html:clsx "slide z-20 px-5 py-[2ch]" class)
     (div :class "slide-contents max-w-[120ch] space-y-[1ch]"
       (when heading
         (</>
          (hgroup :class "heading-container"
            (h2 :class "font-heading text-5xl pt-5" heading))))
       (div :class "slide__slide-contents w-full space-y-[1ch]"
         (when code
           (ac-code-block code))
         (div :class "slide__slide-text pb-5 [-webkit-text-stroke:1px_var(--color-primary-950]" children))))))

(define-component ac-hero-text (&key class author children)
  (</>
   (div :class "grid place-items-center min-h-[100dvh] px-5"
     (hgroup :class (almighty-html:clsx "font-hero text-7xl sm:text-[20rem]" class)
       (h1 :id "page-title" :class "text-center text-balance" children)
       (when author
         (</> (p :class "mt-10 text-3xl font-bodoni" "by " author)))))))

(define-component ac-code (&key children)
  (</>
   (span :class "inline-code font-berkeley-uc-thin text-accent" children)))

