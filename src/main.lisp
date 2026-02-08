(defpackage #:almightylisp
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html))
  (:import-from #:almighty-html
                #:</>
                #:define-component)
  (:import-from #:almightylisp/app
                #:*app*)
  (:export #:*app*))
(in-package #:almightylisp)

(define-component ac-nav-link (&key url text)
  (</>
   (span :class "relative"
     (span :class (ah:clsx "absolute inset-y-0 -left-4 w-0.5"
                           (if (string= url (lack/request:request-path-info shiso:*request*)) "bg-secondary" "bg-primary-50")))
     (a :href url
       :class (ah:clsx "text-left sm:text-sm hover:text-accent hover:bg-primary-700 flex w-full"
                       (if (string= url (lack/request:request-path-info shiso:*request*)) "text-secondary" "text-primary-50"))
       (span :class "truncate" text)))))

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
         (script :src "https://cdn.jsdelivr.net/npm/animejs/dist/bundles/anime.umd.min.js")
         (script :src "js/highlight-lisp.js")
         (script
           :src "https://unpkg.com/htmx.org@2.0.4/dist/htmx.js"
           :integrity "sha384-oeUn82QNXPuVkGCkcrInrS1twIxKhkZiFfr2TdiuObZ3n3yIeMiqcRzkIcguaof1"
           :crossorigin "anonymous")
         (script :src "https://unpkg.com/hyperscript.org@0.9.13"))
       (script :src "js/almighty-animations.js" :type "module")
       (body
         children
         ;; syntax highlighting
         (script "HighlightLisp.highlight_auto();"))))))

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
   (section :id id :class (almighty-html:clsx "slide min-h-[100dvh] z-20 px-5 py-[2ch]" class)
     (div :class "slide-contents max-w-[120ch] space-y-[1ch]"
       (when heading
         (</>
          (hgroup :class "heading-container"
            (h2 :class "font-heading text-5xl pt-5" heading))))
       (div :class "slide__slide-contents w-full space-y-[1ch]"
         (when code
           (ac-code-block code))
         (div :class "slide__slide-text pb-5 [-webkit-text-stroke:1px_var(--color-primary-950]" children))))))

;; (define-component ac-hero-text (&key class author children)
;;   (let ((words (loop :for word :in (str:split " " (first children))
;;                      :collect (</> (p word)))))
;;     (</>
;;      (hgroup :class (str:concat "hero-text-right hfixed lg:inline order-last -z-10 font-hero text-left text-[20rem] leading-[3.15ch] " class)
;;        words 
;;        (when author
;;          (</> (p :class "mt-10 text-3xl font-bodoni" "by " author)))))))

(define-component ac-hero-text (&key class author children)
  (</>
   (div :class "grid place-items-center min-h-[100dvh] px-5"
     (hgroup :class (almighty-html:clsx "font-hero text-7xl sm:text-9xl" class)
       (h1 :id "page-title" :class "text-center text-balance" children)
       (when author
         (</> (p :class "mt-10 text-3xl font-bodoni" "by " author)))
       ;; (svg :width "0" :height "0"
       ;;   (filter :id "kill"
       ;;     (fecolormatrix :type "matrix"
       ;;       :result "red_"
       ;;       :values "4 0 0 0 0
       ;;                0 0 0 0 0
       ;;                0 0 0 0 0
       ;;                0 0 0 1 0")
       ;;     (feoffset :in "red_" :dx "2" :dy "0" :result "red")
       ;;     (fecolormatrix :type "matrix"
       ;;       :result "blue_"
       ;;       :values "0 0 0 0 0
       ;;                0 3 0 0 0
       ;;                0 0 10 0 0
       ;;                0 0 0 1 0")
       ;;     (feoffset :in "blue_" :dx "-3" :dy "0" :result "blue")
       ;;     (feblend :mode "lighten" :in "red" :in2 "blue")))
       ))))

(define-component ac-code (&key children)
  (</>
   (span :class "inline-code font-berkeley-uc-thin text-accent" children)))



(defun make-concat-count (&optional (count 0))
  (let ((count count))
    (lambda (string)
      (let ((string-count (write-to-string count)))
        (incf count)
        (concatenate 'string string string-count)))))

(defun index (params)
  (declare (ignore params))
  (let ((concat-count (make-concat-count)))
    (shiso:http-response
     (ah:render-to-string
      (</>
       (ac-site-layout :title "THIS IS A LIST"
         (ac-hero-text
           "THIS IS A LIST")
         (div :class "slides max-w-[120ch] mx-auto relative [anchor-name:--hero-text-anchor]" 
           (ac-teaser-slide
             :heading "A LIST"
             :id (apply concat-count '("slide")) :code "()"
             (p "This is a " (ac-code "list") "."))
           (ac-teaser-slide
             :heading "ANOTHER LIST"
             :id (apply concat-count '("slide")) :code "(1 2 3)"
             (p "So is this. It has three numbers."))
           (ac-teaser-slide
             :heading "NESTED LISTS"
             :id (apply concat-count '("slide")) :code "((10 11 12) (1 2 3))"
             (p "This is three lists. One list contains two nested lists."))
           (ac-teaser-slide
             :heading "ASSOCIATIVE LISTS"
             :id (apply concat-count '("slide")) :code "((:micah . \"lisper\")
 (:sussman . \"lisper\")
 (:joe-blow . \"jai guy\")
 (:dhh . \"ruby boob\"))"
             (p "This is an " (ac-code "associative list") ", also called an " (ac-code "alist") ". Lists can be used as tables storing key-value pairs.")
             (p "Symbols that begin with " (ac-code ":") " are called " (ac-code "keywords") ". Keywords are evaluated to themselves."))
           (ac-teaser-slide
             :heading "SYMBOL"
             :id (apply concat-count '("slide")) :code "micah"
             (p "This is a " (ac-code "symbol") ". A symbol holds a reference to some data."))
           (ac-teaser-slide
             :heading "FUNCTION CALLS"
             :id (apply concat-count '("slide")) :code "(say-hello) ; => \"Hello!\""
             (p "This is a list with a symbol. The first item in a list is treated as the name of a function to be called."))
           (ac-teaser-slide
             :heading "ARGUMENTS"
             :id (apply concat-count '("slide")) :code "(person-birthday :micah '((:micah . \"1985-12-14\")
                          (:takae . \"1987-11-19\")
                          (:mom . \"1955-02-12\")
                          (:papa . \"1952-07-31\"))) ; => \"1985-12-14\""
             (p "If there are other elements in the list besides the function name, they are passed as arguments to the function. Arguments are evaluated first--from left to right--before being passed to the function."))
           (ac-teaser-slide
             :heading "FUNCTION DEFINITIONS"
             :id (apply concat-count '("slide")) :code "(defun person-birthday (name db) (cdr (assoc name db)))"
             (p "This is how you create a " (ac-code "function") ". There is one list that contains two symbols and two nested lists. The first element, " (ac-code "defun") ", is a macro. " "The second element, " (ac-code "person-birthday") ", is a name to give to the function. " (ac-code "(name db)") " is a " (span :class "italic" "lambda-list") " that defines the arguments to the function." (ac-code "(cdr (assoc name db))") " is another list--the body of the function."))
           (ac-teaser-slide
             :heading "QUOTING"
             :id (apply concat-count '("slide")) :code "(quote (person-birthday :micah '((:micah . \"1985-12-14\")
                                 (:takae . \"1987-11-19\")
                                 (:mom . \"1955-02-12\")
                                 (:papa . \"1952-07-31\"))))
                                        ; => (PERSON-BIRTHDAY :MICAH
                                        ; '((:MICAH . \"1985-12-14\")
                                        ;   (:TAKAE . \"1987-11-19\")
                                        ;   (:MOM . \"1955-02-12\")
                                        ;   (:PAPA . \"1952-07-31\")))"
             (p "Quoting a list returns the literal list object " (span :class "italic" "without evaluating its contents") ". Symbols are upcased."))
           (ac-teaser-slide
             :heading "READER MACROS"
             :id (apply concat-count '("slide")) :code  "'(person-birthday :micah '((:micah . \"1985-12-14\")
                           (:takae . \"1987-11-19\")
                           (:mom . \"1955-02-12\")
                           (:papa . \"1952-07-31\")))
                                        ; => (PERSON-BIRTHDAY :MICAH
                                        ; '((:MICAH . \"1985-12-14\")
                                        ;   (:TAKAE . \"1987-11-19\")
                                        ;   (:MOM . \"1955-02-12\")
                                        ;   (:PAPA . \"1952-07-31\")))"
             (p (ac-code "'") " is a reader macro. It performs the same function as the above call to " (ac-code "quote") "."))
           (ac-teaser-slide
             :heading "CODE IS DATA"
             :id (apply concat-count '("slide")) :code "(quote (person-birthday :micah '((:micah . \"1985-12-14\")
                                 (:takae . \"1987-11-19\")
                                 (:mom . \"1955-02-12\")
                                 (:papa . \"1952-07-31\"))))
                                        ; => (PERSON-BIRTHDAY :MICAH
                                        ; '((:MICAH . \"1985-12-14\")
                                        ;   (:TAKAE . \"1987-11-19\")
                                        ;   (:MOM . \"1955-02-12\")
                                        ;   (:PAPA . \"1952-07-31\")))

(eval '(person-birthday :micah '((:micah . \"1985-12-14\")
                                 (:takae . \"1987-11-19\")
                                 (:mom . \"1955-02-12\")
                                 (:papa . \"1952-07-31\"))))
                                        ; => \"1985-12-14\""
             (p "The first list is a function call to " (ac-code "quote") ". It returns the literal representation of its argument.")
             (p "The the second list is a function call to " (ac-code "eval") ". It will take the data it receives and evaluate it as code.")
             (p "I repeat: it takes the literal representation of data and evaluates it as code. The code is data."))
           (ac-teaser-slide
             :heading "DATA IS CODE"
             :id (apply concat-count '("slide")) :code "(defun tree-type-of (x)
  (cond ((null x) nil)
        ((symbolp x)
         (cond ((macro-function x) (list 'macro))
               ((and (boundp x) (symbol-function x)) (list 'fun))
               ((and (boundp x) (symbol-value x)) (list 'var))
               (t (list 'symbol))))
        ((numberp x) (list 'number))
        ((atom x) (list (type-of x)))
        (t (and (append (tree-type-of (car x))
                        (tree-type-of (cdr x)))))))

(tree-type-of '(defun cube-and-double (x) (* 2 (* x x x))))
                                        ; => (MACRO SYMBOL SYMBOL FUN NUMBER FUN SYMBOL SYMBOL SYMBOL)
"
             (p "Code can be traversed and transformed as data."))
           (ac-teaser-slide
             :heading "MACRO DEFINITIONS"
             :id (apply concat-count '("slide")) :code "(defmacro to-keyword (name)
  (typecase name
    (keyword name)
    (string (intern (string-upcase name) :keyword))
    (symbol (intern (symbol-name name) :keyword))
    (t \"The argument must be either a keyword, string, or symbol.\")))

(to-keyword :micah)
                                        ; => :MICAH
(to-keyword \"micah\")
                                        ; => :MICAH
(to-keyword micah)
                                        ; => :MICAH
"
             (p "This is a macro. It's like a function, but it writes code. This macro can turn a symbol or string passed to it into a keyword."))
           (ac-teaser-slide
             :heading "REWRITING CODE"
             :id (apply concat-count '("slide")) :code "(defmacro person-birthday-macro (name db)
  `(cdr (assoc (to-keyword ,name) ,db)))

;; This code...
(person-birthday-macro micah '((:micah . \"1985-12-14\")
                               (:takae . \"1987-11-19\")
                               (:mom . \"1955-02-12\")
                               (:papa . \"1952-07-31\")))

;; ...transforms into this code...
(cdr
 (assoc (to-keyword micah)
        '((:micah . \"1985-12-14\") (:takae . \"1987-11-19\") (:mom . \"1955-02-12\")
          (:papa . \"1952-07-31\"))))

;; ...which finally completes its transformation into this code:
(cdr
 (assoc :micah
        '((:micah . \"1985-12-14\") (:takae . \"1987-11-19\") (:mom . \"1955-02-12\")
          (:papa . \"1952-07-31\"))))
"
             (p "Macros take code as data and rewrite it into other code."))
           (ac-teaser-slide
             :heading "SQL QUERY MACRO"
             :id (apply concat-count '("slide")) :code "(sxql:select ((:as (:coalesce (:sum :leg.entry_amount) 0) :amount)
                          (:as :leg.entry_currency :currency))
                 (sxql:from (:as :almighty_account :account))
                 (sxql:inner-join (:as :almighty_leg :leg) :on (:= :leg.account_id :account.id))
                 (sxql:inner-join (:as :almighty_transaction :tran) :on (:= :leg.transaction_id :tran.id))
                 (sxql:where (:and (:= :account.id (m:object-id account))
                                (:<= :tran.date as-of)))
                 (sxql:group-by :leg.entry_currency))"
             (p "This is " (a :href "https://github.com/fukamachi/sxql" "SXQL") " code. It's a library for generating SQL queries using macros like " (ac-code "select") ", " (ac-code "from") ", etc."))
           (ac-teaser-slide
             :heading "HTML MACRO"
             :id (apply concat-count '("slide")) :code "(define-component ac-skeleton (&key title children)
  (let ((html-class (string-downcase \"dark\"))
      (html-lang \"en\"))
    (</>
     (html :class html-class :lang html-lang
       (head
         (title (str:concat (string-upcase title) \" /// \" (string-upcase \"almightylisp.com\")))
         (meta :charset \"utf-8\")
         (meta :name \"viewport\" :content \"width=device-width, initial-scale=1\")
         (link :href \"css/almightylisp.css\" :rel \"stylesheet\" :type \"text/css\")
         (script
           :src \"https://unpkg.com/htmx.org@2.0.4/dist/htmx.js\"
           :integrity \"sha384-oeUn82QNXPuVkGCkcrInrS1twIxKhkZiFfr2TdiuObZ3n3yIeMiqcRzkIcguaof1\"
           :crossorigin \"anonymous\")
         (script :src \"https://unpkg.com/hyperscript.org@0.9.13\"))
       (body :class \"font-berkeley bg-primary text-primary-50\"
         children)))))"
             (p "This is code from the almighty-html library. The " (ac-code "</>") " macro generates HTML and can create components."))
           (ac-teaser-slide
             :heading "A NEW LISP LANGUAGE"
             :id (apply concat-count '("slide")) :code "(declare withdraw (AccountName -> Amount -> BankM (BankResult Account)))
(define (withdraw account-name amount)
  \"Withdraw AMOUNT from account with ACCOUNT-NAME, returning the Account for convenience.\"
  (do
   (protection? <- (asks overdraft-protection_))
   (minimum <- (asks minimum-balance_))
    (do-resultT
        (err-ifM (< amount 0) (InvalidWithdrawal amount))
      (acc <- (get-accountM account-name))
      (map-errM
       (fn (er)
           (Unknown
            (s:concat \"Cannot withdraw from an invalid account: \"
                      (into er))))
       (check-account-is-valid acc))
      (let new-account = (subtract-balance amount acc))
      (if (and protection?
               (< (.balance new-account) minimum))
          (pure (Err (InvalidWithdrawal amount)))
          (set-account new-account)))))
" 
             (p "This is " (a :href "https://coalton-lang.github.io/" "Coalton") ", a new programming language written in Common Lisp with a Haskell-inspired type system. It's \"just a macro\".")))
         (div :class "min-h-[150dvh] max-w-[120ch] px-5 mx-auto flex items-center"
           (h2 "It all starts with a " (ac-code "list") "."))
         (div :class "px-5 max-w-5xl mx-auto min-h-[100dvh] flex items-center justify-center flex-wrap"
           (div
             (div :class "w-full flex justify-center"
               (img :class "max-h-[75dvh]" :src "assets/images/almighty-lisp-essentials.png"))
             (div :class "w-fit mx-auto mt-10"
               (p :class "loading text-center" "Coming Soon..."))))
         ))))))

(shiso:route :GET "/" 'index)

#+nil
(defparameter *server* (clack:clackup (lack:builder shiso::*app*) :server :woo :address "127.0.0.1" :port 5000))

#+nil
(clack:stop *server*)
