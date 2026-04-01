(defpackage #:home/hypermedia/components
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html)
                    (#:h #:almightylisp/hypermedia))
  (:export #:ac-home-layout))

(in-package #:home/hypermedia/components)

(ah:define-component ac-home-layout (&key title children)
  (h:ac-skeleton :title title
    (ah:</>
     (main
       (div :class "simple-landing"
         (div :class "logo"
           (img :class "logo-image" :src (shiso:static "assets/images/book-cover-essentials.png")))
         (div :class "hero"
           (h1 :class "hero-header" "Almighty Tools For " (span "Almighty") " Programmers")
           (p :class "hero-body"
             "Common Lisp is the most powerful programming language in history.
 Emacs is the most powerful text editor in history. Now, it is time for you to
 make history by mastering the essentials of these almighty tools with " (span :class "book-name" "Almighty
 Lisp: Lisp & Emacs Essentials") ".")
           (a :href "book/essentials" "Learn Lisp & Emacs Immediately"))
         (section
           (h2 "Who This Book Is For")
           (p "If you’re a Lisp-curious veteran programming looking for the perfect book for
getting started with Common Lisp and Emacs, you need to read this book.")
           (p "If you’ve tried to learn Common Lisp in the past, but couldn’t overcome the
Emacs-wall, this book will pull you up and over that wall.")
           (p "If you seek to learn the tools that will take you to another celestial plane of
existence and software engineering ecstasy, first *settle down you insane
person* (but I like that enthusiasm), and then begin reading " (span :class "book-name" "Almighty Lisp: Lisp
& Emacs Essentials") " IMMEDIATELY."))
         (section
           (h2 "Learn The Language & The Editor")
           (p "Learning Common Lisp is easy, but it’s complicated by the fact that Emacs is the
defacto standard editor for it. Other editors support Common Lisp, but none of
them have the rich set of features that you find in Emacs--either for text
editing or for Common Lisp development." (p "Other books on Common Lisp teach the language, but they don’t teach the editor
for the language. Finally, with " (span :class "book-name" "Almighty Lisp: Lisp & Emacs Essentials") ", you can
learn the language and the editor with the same book, drastically speeding up
your proficiency with both. You’ll be barfing and slurping while you "
                                           (span :class "code" "C-c") " " (span :class "code" "C-c") " macros in no time.")))
         (section
           (h2 "What You'll Learn")
           (p "In " (span :class "book-name" "Almighty Lisp: Lisp & Emacs Essentials") ", the Common Lisp content covers:")
           (ul
             (li "The Fundamentals (symbols, functions, control flow, etc.)")
             (li "List processing")
             (li "Functional Programming features")
             (li "The most powerful Object-Oriented Programming language features in any language.")
             (li "Macros")
             (li "Errors & Conditions")
             (li "Creating Executables & Deploying")
             (li "... and more."))
           (p "There are several small projects included that you can follow along with to
practice editing with Emacs and become familiar with the essential features
included with Common Lisp. I’ll also introduce you to the ecosystem teach you
proper Common Lisp style.")
           (p "The Emacs content covers:")
           (ul
             (li "Downloading & Installing Doom Emacs")
             (li "Basic Survival Emacs Without Keybindings")
             (li "Text Editing")
             (li "Buffer Navigation & Management")
             (li "Window Navigation & Management")
             (li "Advanced Features Including Project-Level Search & Replace")
             (li "Using the Sly Common Lisp IDE")
             (li "Common Lisp Structured Editing")
             (li "... and more."))
           (p "You won’t need to learn all of Emacs’ features immediately. In fact, you will
begin by using Emacs’ GUI menus for basic operations before advancing to using
keybindings and using more advanced features."))
         (section
           (h2 "The Almighty Philosophy")
           (p "Common Lisp is a language optimized for ultimate " (i "adaptability") ".
 It’s a generalist’s secret weapon; a programming language that isn’t a master
 at anything, but is quite capable at doing everything.")
           (p "With the rise of LLMs and a rapidly changing software industry, it’s easy to
feel anxious about your own future as a software developer.")
           (p "But you don’t need to worry. You need to " (i "adapt") ". Common Lisp is an " (i "almighty") "
programming language that enables and even " (i "summons") " its users to become almighty.
The macros are waiting. The REPL is loaded. The buffers and windows are at your
command.")
           (p "Be not defeated by the rapidly shifting winds of code and craft. Embrace the
piercing light of destiny, beaming from the flaming horizon over an effervescent
ocean of functions, classes, and parentheses. Become Almighty.")
           (a :href "book/essentials" "Read The Book Immediately"))
         (section :class "about-author"
           (h2 "About Me, The Author")
           (p "I’m Micah Killian. I’m a former English teacher (ALT) and current freelance
software developer living and working in Saga, Japan. I have a wife and two
daughters. When I’m rewriting everything in Common Lisp (which you should do
IMMEDIATELY), I’m out with my family climbing mountains, playing at the park,
repairing bicycles, and dreaming of lambdas (not lambos).")))))))
