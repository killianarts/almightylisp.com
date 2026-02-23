(defpackage #:almightylisp/controllers/essentials
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html)
                    (#:x #:almightylisp/components)
                    (#:s #:shiso)))

(in-package #:almightylisp/controllers/essentials)

(defparameter *book-path* "/book")

(defun index ()
  (ah:render-to-string
   (ah:</>
    (x:ac-site-layout
      (header :id "toc" :class "book-toc"
        (div :id "table-of-contents" :role "doc-toc"
          (h2
            "Table of Contents")
          (div :id "text-table-of-contents" :role "doc-toc"
            (ul
              (li
                (a :href "#meta"
                  "1."
                  (span :class "todo TODO"
                    "TODO")
                  "META")
                (ul
                  (li
                    (a :href "#why-i-wrote-this-book"
                      "1.1. WHY I WROTE THIS BOOK"))
                  (li
                    (a :href "#almighty"
                      "1.2."
                      (span :class "todo TODO"
                        "TODO")
                      "ALMIGHTY"))))
              (li
                (a :href "#emacs-setup-use"
                  "2. EMACS SETUP & USE")
                (ul
                  (li
                    (a :href "#purpose"
                      "2.1. PURPOSE"))
                  (li
                    (a :href "#doom-emacs-install-setup"
                      "2.2. DOOM EMACS INSTALL & SETUP"))
                  (li
                    (a :href "#emacs-dictionary"
                      "2.3. EMACS DICTIONARY"))
                  (li
                    (a :href "#survival-emacs"
                      "2.4. SURVIVAL EMACS"))))
              (li
                (a :href "#lisp-setup-use"
                  "3."
                  (span :class "todo TODO"
                    "TODO")
                  "LISP SETUP & USE")
                (ul
                  (li
                    (a :href "#installing-lisp"
                      "3.1. INSTALLING LISP"))
                  (li
                    (a :href "#survival-lisp-commands"
                      "3.2. SURVIVAL LISP COMMANDS"))))
              (li
                (a :href "#lisp-fundamentals"
                  "4. LISP FUNDAMENTALS")
                (ul
                  (li
                    (a :href "#syntax-grammar"
                      "4.1. SYNTAX & GRAMMAR"))
                  (li
                    (a :href "#symbols"
                      "4.2. SYMBOLS"))
                  (li
                    (a :href "#functions"
                      "4.3. FUNCTIONS"))
                  (li
                    (a :href "#lists"
                      "4.4. LISTS"))
                  (li
                    (a :href "#control-flow"
                      "4.5. CONTROL FLOW"))
                  (li
                    (a :href "#iteration"
                      "4.6. ITERATION"))
                  (li
                    (a :href "#strings-i-o"
                      "4.7. STRINGS & I/O"))
                  (li
                    (a :href "#craps"
                      "4.8."
                      (span :class "todo TODO"
                        "TODO")
                      (code
                        "CRAPS")))
                  (li
                    (a :href "#tic-tac-toe"
                      "4.9."
                      (span :class "todo TODO"
                        "TODO")
                      (code
                        "TIC-TAC-TOE")))))
              (li
                (a :href "#emacs-fundamentals"
                  "5. EMACS FUNDAMENTALS")
                (ul
                  (li
                    (a :href "#beyond-survival-mode"
                      "5.1. BEYOND SURVIVAL MODE"))
                  (li
                    (a :href "#support-level-disclaimer"
                      "5.2. SUPPORT LEVEL DISCLAIMER"))
                  (li
                    (a :href "#setting-expectations"
                      "5.3. SETTING EXPECTATIONS"))
                  (li
                    (a :href "#text-editing"
                      "5.4. TEXT EDITING"))
                  (li
                    (a :href "#buffer-navigation-management"
                      "5.5. BUFFER NAVIGATION & MANAGEMENT"))
                  (li
                    (a :href "#window-navigation-management"
                      "5.6. WINDOW NAVIGATION & MANAGEMENT"))
                  (li
                    (a :href "#project-navigation-management"
                      "5.7. PROJECT NAVIGATION & MANAGEMENT"))
                  (li
                    (a :href "#learning-more"
                      "5.8. LEARNING MORE"))))
              (li
                (a :href "#the-lisp-ide"
                  "6. THE LISP IDE")
                (ul
                  (li
                    (a :href "#the-lisp-coding-environment"
                      "6.1. THE LISP CODING ENVIRONMENT"))
                  (li
                    (a :href "#sly-backtrace-navigation"
                      "6.2. SLY BACKTRACE NAVIGATION"))
                  (li
                    (a :href "#tracing"
                      "6.3. TRACING"))
                  (li
                    (a :href "#stickers"
                      "6.4. STICKERS"))))
              (li
                (a :href "#structured-editing"
                  "7. STRUCTURED EDITING")
                (ul
                  (li
                    (a :href "#restructuring-code-with-ease"
                      "7.1. RESTRUCTURING CODE WITH EASE"))
                  (li
                    (a :href "#navigating-the-sea-of-parentheses"
                      "7.2. NAVIGATING THE SEA OF PARENTHESES"))
                  (li
                    (a :href "#more-restructuring-navigating-tools"
                      "7.3. MORE RESTRUCTURING & NAVIGATING TOOLS"))))
              (li
                (a :href "#beyond-lists"
                  "8. BEYOND LISTS")
                (ul
                  (li
                    (a :href "#oop"
                      "8.1. OOP"))
                  (li
                    (a :href "#hash-tables"
                      "8.2. HASH-TABLES"))
                  (li
                    (a :href "#arrays"
                      "8.3. ARRAYS"))
                  (li
                    (a :href "#structures"
                      "8.4. STRUCTURES"))))
              (li
                (a :href "#errors-conditions"
                  "9. ERRORS & CONDITIONS")
                (ul
                  (li
                    (a :href "#signaling-conditions"
                      "9.1. Signaling Conditions"))
                  (li
                    (a :href "#assertions"
                      "9.2. Assertions"))
                  (li
                    (a :href "#conditions"
                      "9.3. Conditions"))
                  (li
                    (a :href "#restarts"
                      "9.4. Restarts"))
                  (li
                    (a :href "#handlers"
                      "9.5. Handlers"))
                  (li
                    (a :href "#break-on-signals"
                      "9.6."
                      (code
                        "*break-on-signals*")))))
              (li
                (a :href "#macros"
                  "10. MACROS")
                (ul
                  (li
                    (a :href "#the-forbidden-fruit"
                      "10.1. The Forbidden Fruit"))
                  (li
                    (a :href "#some-demonstrations"
                      "10.2. Some Demonstrations"))
                  (li
                    (a :href "#understanding-macros"
                      "10.3. Understanding Macros"))
                  (li
                    (a :href "#defining-macros"
                      "10.4. Defining Macros"))
                  (li
                    (a :href "#redefining-macros"
                      "10.5. Redefining Macros"))
                  (li
                    (a :href "#determining-when-to-use-macros"
                      "10.6. Determining When To Use Macros"))
                  (li
                    (a :href "#determining-when-not-to-use-macros"
                      "10.7. Determining When Not To Use Macros"))
                  (li
                    (a :href "#variable-capture-hygiene"
                      "10.8. Variable Capture & Hygiene"))))
              (li
                (a :href "#organizing-code"
                  "11. ORGANIZING CODE")
                (ul
                  (li
                    (a :href "#packages"
                      "11.1. Packages"))
                  (li
                    (a :href "#systems"
                      "11.2. Systems"))
                  (li
                    (a :href "#styles-of-factoring-packages-systems"
                      "11.3. Styles of Factoring Packages & Systems"))
                  (li
                    (a :href "#the-great-debate-package-system-best-practices"
                      "11.4. The Great Debate: Package & System Best Practices"))))
              (li
                (a :href "#the-ecosystem"
                  "12. THE ECOSYSTEM")
                (ul
                  (li
                    (a :href "#quicklisp"
                      "12.1. QUICKLISP"))
                  (li
                    (a :href "#ultralisp"
                      "12.2. ULTRALISP"))
                  (li
                    (a :href "#qlot"
                      "12.3. QLOT"))
                  (li
                    (a :href "#ocicl"
                      "12.4. OCICL"))
                  (li
                    (a :href "#vend"
                      "12.5. VEND"))))
              (li
                (a :href "#projects"
                  "13."
                  (span :class "todo TODO"
                    "TODO")
                  "PROJECTS")
                (ul
                  (li
                    (a :href "#almighty-money"
                      "13.1."
                      (code
                        "ALMIGHTY-MONEY")))
                  (li
                    (a :href "#almighty-kaikei"
                      "13.2."
                      (code
                        "ALMIGHTY-KAIKEI")))))
              (li
                (a :href "#deploying"
                  "14. DEPLOYING")
                (ul
                  (li
                    (a :href "#executable-cli-app"
                      "14.1. EXECUTABLE CLI APP"))
                  (li
                    (a :href "#deploying-fuka-stack-web-app"
                      "14.2. DEPLOYING FUKA STACK WEB APP"))
                  (li
                    (a :href "#deploying-an-electron-app"
                      "14.3."
                      (span :class "todo HOLD"
                        "HOLD")
                      "DEPLOYING AN ELECTRON APP"))))
              (li
                (a :href "#resources"
                  "15. RESOURCES"))))))))))
(shiso:define-route :GET (str:concat *book-path* "/essentials") 'index)
