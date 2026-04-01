
(body
 (div :class "sidebar-container"
 (div :class "book-navigation"
 (h1 :class "book-navigation__book-title" "Almighty Lisp: The Essentials")
 (p :class "book-navigation__chapter-name-heading"
 (span "Almighty Lisp: The Essentials"))
 (nav
 (ul
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#packages"
 (span :class "book-navigation__section-name" "Packages")
 (span :class "book-navigation__section-number" "1.1.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#defining-packages"
 (span :class "book-navigation__section-name" "Defining Packages")
 (span :class "book-navigation__section-number" "1.2.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#interning-symbols"
 (span :class "book-navigation__section-name" "Interning Symbols")
 (span :class "book-navigation__section-number" "1.3.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#using-importing-exporting"
 (span :class "book-navigation__section-name" "Using, Importing & Exporting")
 (span :class "book-navigation__section-number" "1.4.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#shadowing-and-conflicts"
 (span :class "book-navigation__section-name" "Shadowing And Conflicts")
 (span :class "book-navigation__section-number" "1.5.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#nicknames"
 (span :class "book-navigation__section-name" "Nicknames")
 (span :class "book-navigation__section-number" "1.6.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#another-way-to-define-packages"
 (span :class "book-navigation__section-name" "Another Way to Define Packages")
 (span :class "book-navigation__section-number" "1.7.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#systems"
 (span :class "book-navigation__section-name" "Systems")
 (span :class "book-navigation__section-number" "1.8.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#what-are-systems-"
 (span :class "book-navigation__section-name" "What Are Systems?")
 (span :class "book-navigation__section-number" "1.9.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#what-is-asdf-"
 (span :class "book-navigation__section-name" "What Is ASDF?")
 (span :class "book-navigation__section-number" "1.10.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#defining-systems"
 (span :class "book-navigation__section-name" "Defining Systems")
 (span :class "book-navigation__section-number" "1.11.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#loading-systems"
 (span :class "book-navigation__section-name" "Loading Systems")
 (span :class "book-navigation__section-number" "1.12.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#styles-of-factoring-packages-systems"
 (span :class "book-navigation__section-name" "Styles of Factoring Packages & Systems")
 (span :class "book-navigation__section-number" "1.13.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#mother-of-all-package-strategy"
 (span :class "book-navigation__section-name" "Mother Of All Package Strategy")
 (span :class "book-navigation__section-number" "1.14.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#multiple-systems-strategy"
 (span :class "book-navigation__section-name" "Multiple Systems Strategy")
 (span :class "book-navigation__section-number" "1.15.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#one-package-per-file-strategy"
 (span :class "book-navigation__section-name" "One Package Per File Strategy")
 (span :class "book-navigation__section-number" "1.16.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#one-system-per-package"
 (span :class "book-navigation__section-name" "One System Per Package")
 (span :class "book-navigation__section-number" "1.17.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#the-great-debate-package-system-best-practices"
 (span :class "book-navigation__section-name" "The Great Debate: Package & System Best Practices")
 (span :class "book-navigation__section-number" "1.18.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#argument-in-favor-of-pis"
 (span :class "book-navigation__section-name" "Argument in Favor of PIS")
 (span :class "book-navigation__section-number" "1.19.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#arguments-against-pis"
 (span :class "book-navigation__section-name" "Arguments Against PIS")
 (span :class "book-navigation__section-number" "1.20.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#conclusion"
 (span :class "book-navigation__section-name" "Conclusion")
 (span :class "book-navigation__section-number" "1.21.0")))))
 (div :class "book-navigation__previous-and-next")))
 (main :class "sidebar-main"
 (article :class "book"
 (section :id "organizing-code"
 (hgroup
 (span)
 (h1 "ORGANIZING CODE"))
 (div :class "outline-text-2" :id "text-1")
 (section :id "packages"
 (hgroup
 (span)
 (h2 "Packages"))
 (div :class "outline-text-3" :id "text-1-1"
 (p "Packages are namespaces for symbols. Symbols are \"interned\" inside packages.
They can be interned in number of ways which we'll see later.")
 (p "The fate of a symbol is tied to the package within which it is interned")
 (p "In old-style code, entire libraries or projects used one packages for all
symbols. One central file defines all of the symbols imported and exported into
and out of the package, then all files within the project simply use the package.")
 (p "Modern style uses one package per file, with each file exporting symbols it
wants to share with other packages.")
 (p "We'll have a discussion about these different factoring styles later."))
 (section :id "defining-packages"
 (hgroup
 (span)
 (h3 "Defining Packages"))
 (div :class "outline-text-4" :id "text-1-1-1"
 (p "Packages are defined with "
 (code "defpackage") ".")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:my-package
  (:use #:cl))"))
 (p
 (code "my-package") " inherits or \"uses\" all of the external symbols of "
 (code "cl") ", a nickname
for the "
 (code "common-lisp") " package, which contains all of the standard Common Lisp
symbols.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(describe 'cl:defun)
                                        ; COMMON-LISP:DEFUN [symbol]

                                        ; DEFUN names a macro: Lambda-list:
                                        ;   (NAME LAMBDA-LIST &BODY BODY)
                                        ;   Documentation: Define a function at
                                        ;   top level. Source file:
                                        ;   SYS:SRC;CODE;MACROS.LISP => ; No
                                        ;   value"))
 (p "If you don't include this line, you will need to add a package-qualifier to all
of the symbols built into Common Lisp.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:messy-package)
(in-package #:messy-package)

(defun hello-world ()
  (print \"Hello world\"))"))
 (p "If you evaluate this code, you'll get a strange error:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "The variable HELLO-WORLD is unbound.
   [Condition of type COMMON-LISP:UNBOUND-VARIABLE]"))
 (p "Why? Because the "
 (code "defun") " special operator is actually a "
 (i "macro") " whose first
argument is a name for the function you are defining. Because it's a macro, the
order of operations is to first "
 (i "expand") " the macro and then execute the code.
During the macro expansion, the "
 (code "hello-world") " symbol will be interned and its"
 (i "function name") " value will be bound to "
 (code "hello-world") ".")
 (p "However, because we didn't "
 (code ":use #:cl") ", "
 (code "defun") " is interpreted as the name of a"
 (i "function") ", which requires first that all inner forms be evaluated and their
values returned. And since "
 (code "hello-world") " isn't bound to any value yet, we get
the above error.")
 (p "All that is to say, unless you're a Lisp wizard concocting a sexp brew, you
should be adding "
 (code "(:use #:cl)") " to all of your package definitions.")))
 (section :id "interning-symbols"
 (hgroup
 (span)
 (h3 "Interning Symbols"))
 (div :class "outline-text-4" :id "text-1-1-2"
 (p "Notice also the call to "
 (code "in-package") ". Forms following a call to "
 (code "in-package") "will have access to, and be accessible in, that package. That's because symbols
are \"interned\" in packages If you have a"
 (code "defpackage") " but no "
 (code "in-package") ", then none of your symbols will be interned in
that package, and none of the forms will have access to the symbols in that
package (without a package-qualifier prefix).")
 (p "The most common and intuitive way they are interned is when they are defined as
the name of a function, a variable, etc.")
 (p "Which package a symbol is interned to depends on a couple of factors. If you are
using the REPL and you execute "
 (code "(defparameter *name* 'micah)") " in the REPL, the
symbol "
 (code "*name*") " will be interned into "
 (b "whichever package the REPL is synced to") ".
For example:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source repl" "CL-USER> (defparameter *name* 'micah)"))
 (p "Here, "
 (code "*name*") " is interned into the default "
 (code "CL-USER") " package.")
 (p "If you are in a file buffer and you "
 (code "sly-eval") " or "
 (code "sly-compile") " the"
 (code "defparameter") " form, "
 (code "*name*") " will be interned into the package named in"
 (code "in-package") ".")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:my-package
  (:use #:cl))
(in-package #:my-package) ; <- "))))
 (section :id "using-importing-exporting"
 (hgroup
 (span)
 (h3 "Using, Importing & Exporting"))
 (div :class "outline-text-4" :id "text-1-1-3"
 (p "Let's expand the example:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:my-package (:use #:cl) (:export #:*global*))

(in-package #:my-package)

(defparameter *global* \"This is a global variable.\")

(defpackage #:your-package (:use #:cl) (:import-from #:my-package)) (in-package
#:your-package)

(print my-package:*global*)
(symbol-package 'my-package:*global*)

(let ((my-package:*global* \"Symbol from MY-PACKAGE lexically rebound in
  YOUR-PACKAGE\")) my-package:*global*)

(defparameter *global* \"Global variable interned in YOUR-PACKAGE.\")

(symbol-package '*global*)"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "(print my-package:*global*)            => \"This is a global variable.\"
(symbol-package 'my-package:*global*) => #<PACKAGE \"MY-PACKAGE\">
(let (...) my-package:*global*)        => \"Symbol from MY-PACKAGE lexically
                                          rebound in YOUR-PACKAGE\"
(symbol-package 'global)             => #<PACKAGE \"YOUR-PACKAGE\">")))
 (p "By importing packages, you can reuse symbol names without naming conflicts.")
 (p "You may also want to import the symbol, allowing you to use it without a
package-name qualifier.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:your-package (:use #:cl) (:import-from #:my-package #:*global*))"))
 (p "If you start this way, then you won't need a package name qualifier when you use
it.")))
 (section :id "shadowing-and-conflicts"
 (hgroup
 (span)
 (h3 "Shadowing And Conflicts"))
 (div :class "outline-text-4" :id "text-1-1-4"
 (p "This is not the case if you "
 (code "use")
 (code "my-package") ". Change the definition of"
 (code "your-package") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:your-package
  (:use #:cl #:my-package))"))
 (p "If you compile that form you will get this error:")
 (pre :class "code-block-source-pre"
 (code "USE-PACKAGE (MY-PACKAGE:*GLOBAL*) causes name-conflicts in
#<PACKAGE \"YOUR-PACKAGE\"> between the following symbols:
  YOUR-PACKAGE::*GLOBAL*, MY-PACKAGE:*GLOBAL*
   [Condition of type SB-EXT:NAME-CONFLICT]

Restarts:
 0: [KEEP-OLD] Keep YOUR-PACKAGE::*GLOBAL* accessible in YOUR-PACKAGE (shadowing MY-PACKAGE:*GLOBAL*).
 1: [TAKE-NEW] Make MY-PACKAGE:*GLOBAL* accessible in YOUR-PACKAGE (uninterning YOUR-PACKAGE::*GLOBAL*).
 2: [RESOLVE-CONFLICT] Resolve conflict.
 3: [ABORT] Abort compilation.
 4: [*ABORT] Return to SLY's top level.
 5: [ABORT] abort thread (#<THREAD tid=20391 \"slynk-worker\" RUNNING {7005097833}>)"))
 (p "Now you'll have several different "
 (code "restarts") " to choose from, including"
 (code "TAKE-NEW") ", "
 (code "KEEP-OLD") ", etc. and a bit of a mess to clean up. My recommendation
is to only use "
 (code ":use") " sparingly in situations you know you aren't going to have
conflicts later (like in your testing packages).")
 (p "If you choose "
 (code "KEEP-OLD") ", you will "
 (i "shadow") " the "
 (code "my-package:*global*") " with"
 (code "your-package:*global*") ". Shadowing means that the imported symbol is hidden,
allowing the same symbol in the current package.")
 (p "My experience has been that doing anything short of aborting, "
 (code "unintern") " ing the
symbol or "
 (code "unuse-package") " ing the package I'm using leads to frustration. You're
better off not using "
 (code ":use") " in the first place.")
 (p "Even if you do as I recommend you might still have this name conflict problem.
Consider this situation:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:my-package
  (:use #:cl)
  (:export #:my-function))
(in-package #:my-package)

(defun my-function () (print \"my-function\"))

(defpackage #:my-other-package
  (:use #:cl)
  (:import-from #:my-package))

(in-package #:my-other-package)

(defun my-other-function ()
  (my-package:my-function))
(my-other-function)"))
 (p "Right now, we have a couple of problems.")
 (ol :class "org-ol"
 (li "I forgot to "
 (b "export the symbol from my-package") ".")
 (li "I don't have a package-qualifier attached to "
 (code "my-function") " when I try to use
it."))
 (p "When I try to compile "
 (code "my-other-function") ", I'll get a style-warning about an
undefined function. At this point, there are two options:")
 (ol :class "org-ol"
 (li "Add a package name qualifier, as in "
 (code "(my-package:my-function)") "; or")
 (li "Add the function symbol in the imports as below"))
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:my-other-package
  (:use #:cl)
  (:import-from #:my-package
                #:my-function))
(in-package #:my-other-package)"))
 (p "If you add the package name qualifier, you'll get this error:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "*Org Src essentials.org[ lisp ]*:14:25:
  read-error: 
    READ error during COMPILE-FILE:

      The symbol \"MY-FUNCTION\" is not external in the MY-PACKAGE package.

        Line: 2, Column: 25, File-Position: 52

        Stream: #<SB-INT:FORM-TRACKING-STREAM for \"file /var/tmp/slime1YpOBd\"
 {7008A37DA3}>

Compilation failed."))
 (p "Oh bother, forgot to export it.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:my-package
  (:use #:cl)
  (:export #:my-function))"))
 (p "Recompile and continue.")
 (p "If you fix the problem while it is just a style-warning, you'll be okay. But if
you happen to execute code that uses "
 (code "my-function") ", then you'll get an error
message:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "The function MY-OTHER-PACKAGE::MY-FUNCTION is undefined.
   [Condition of type UNDEFINED-FUNCTION]

Restarts:
 0: [CONTINUE] Retry calling MY-FUNCTION.
 1: [USE-VALUE] Call specified function.
 2: [RETURN-VALUE] Return specified values.
 3: [RETURN-NOTHING] Return zero values.
 4: [*ABORT] Return to SLY's top level.
 5: [ABORT] abort thread (#<THREAD tid=15507 \"slynk-worker\" RUNNING
 {7005A4AA33}>)"))
 (p "Whoops, "
 (i "now") " let's add it to imports and recompile.")
 (pre :class "code-block-source-pre"
 (code "IMPORT (MY-PACKAGE::MY-FUNCTION) causes name-conflicts in
#<PACKAGE \"MY-OTHER-PACKAGE\"> between the following symbols:
  MY-OTHER-PACKAGE::MY-FUNCTION, MY-PACKAGE::MY-FUNCTION
   [Condition of type SB-EXT:NAME-CONFLICT]

Restarts:
 0: [SHADOWING-IMPORT-IT] Shadowing-import MY-PACKAGE::MY-FUNCTION, uninterning MY-OTHER-PACKAGE::MY-FUNCTION.
 1: [DONT-IMPORT-IT] Don't import MY-PACKAGE::MY-FUNCTION, keeping MY-OTHER-PACKAGE::MY-FUNCTION.
 2: [RESOLVE-CONFLICT] Resolve conflict.
 3: [ABORT] Abort compilation.
 4: [*ABORT] Return to SLY's top level.
 5: [ABORT] abort thread (#<THREAD tid=20827 \"slynk-worker\" RUNNING {700670E963}>)"))
 (p "If you choose "
 (code "SHADOWING-IMPORT-IT") " and try to recompile your code again, you'll
get a similar error:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "*Org Src essentials.org[ lisp ]*:7:1:
  warning: 
    MY-OTHER-PACKAGE also shadows the following symbols:
      (MY-PACKAGE::MY-FUNCTION)
    ==>
      (SB-IMPL::%DEFPACKAGE \"MY-OTHER-PACKAGE\" 'NIL 'NIL 'NIL 'NIL '(\"CL\")
                            '((\"MY-PACKAGE\" \"MY-FUNCTION\")) 'NIL 'NIL
                            '(\"MY-OTHER-PACKAGE\") 'NIL ...)


Compilation failed."))
 (p "If all this has your head spinning now, you're not alone.")
 (p "To reiterate, you have two options:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" ";; * Option 1: Add a package name qualifier
(defpackage #:my-package
  (:use #:cl)
  (:export #:my-function))              ; <- Don't forget to export.
(in-package #:my-package)

(defun my-function () (print \"my-function\"))

(defpackage #:my-other-package
  (:use #:cl)
  (:import-from #:my-package))

(in-package #:my-other-package)

(defun my-other-function ()
  (my-package:my-function))             ; <- Added package name qualifier

(my-other-function)

;; * Option 2: Add MY-FUNCTION to your imports
(defpackage #:my-package
  (:use #:cl)
  (:export #:my-function))
(in-package #:my-package)

(defun my-function () (print \"my-function\"))

(defpackage #:my-other-package
  (:use #:cl)
  (:import-from #:my-package
                #:my-function)) ; <- Added import

(in-package #:my-other-package)

(defun my-other-function ()
  (my-function))

;; Then while in MY-OTHER-PACKAGE, unintern MY-FUNCTION.
(unintern 'my-function)

;; Recompile MY-OTHER-PACKAGE's defpackage form above. It should now succeed.

;; Now you can call your code.
(my-other-function)"))
 (p "It'll take some getting used to, but eventually you'll know how to clean up any
messes you make.")))
 (section :id "nicknames"
 (hgroup
 (span)
 (h3 "Nicknames"))
 (div :class "outline-text-4" :id "text-1-1-5"
 (p "It's possible for packages to define nicknames. Nicknames are useful for calling
individual symbols from packages that have long names.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:a-package-with-a-long/and-annoying/name
  (:use #:cl)
  (:nicknames #:smol-name)
  (:export #:smol-hello))

(in-package #:a-package-with-a-long/and-annoying/name)

(defun smol-hello ()
  :hi)

(defpackage #:my-package
  (:use #:cl)
  (:import-from #:some-package))

(in-package #:my-package)"))
 (p "Now we can call the function using the nickname:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(smol-name:smol-hello)"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") ":HI")))
 (p "If a package doesn't define its own nickname, you can define a nickname local to
your package:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:a-package-with-a-long/and-annoying/name
  (:use #:cl)
  (:export #:smol-hello))

(in-package #:a-package-with-a-long/and-annoying/name)

(defun smol-hello ()
  :hi)

(defpackage #:my-package
  (:use #:cl)
  (:local-nicknames (#:annoying #:a-package-with-a-long/and-annoying/name)))

(in-package #:my-package)"))
 (p "The local nickname works the same way:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(annoying:smol-hello)"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") ":HI")))
 (p "Unfortunately, you can't define nicknames for individual symbols.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:a-package-with-a-long/and-annoying/name (:use #:cl) (:export
  #:a-very-long-and-annoying-function-or-macro-name #:smol-hello))

(in-package #:a-package-with-a-long/and-annoying/name)

(defun smol-hello () :hi)

(defun a-very-long-and-annoying-function-or-macro-name () :oof)

(defpackage #:my-package (:use #:cl) (:local-nicknames (#:annoying
  #:a-package-with-a-long/and-annoying/name)))

(in-package #:my-package)"))
 (p "No shortcut for long symbol names:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" ";; Deal with it.
(annoying:a-very-long-and-annoying-function-or-macro-name)"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") ":OOF")))))
 (section :id "another-way-to-define-packages"
 (hgroup
 (span)
 (h3 "Another Way to Define Packages"))
 (div :class "outline-text-4" :id "text-1-1-6"
 (p "UIOP is a cross-implementation compatibility library that comes with ASDF (more
on that in a second). Its "
 (code "define-package") " macro provides some extra
capabilities that the standard "
 (code "defpackage") " doesn't. We'll see them in use later
when discussing different factoring styles."))))
 (section :id "systems"
 (hgroup
 (span)
 (h2 "Systems"))
 (div :class "outline-text-3" :id "text-1-2")
 (section :id "what-are-systems-"
 (hgroup
 (span)
 (h3 "What Are Systems?"))
 (div :class "outline-text-4" :id "text-1-2-1"
 (p "Common Lisp system are what other languages might call \"packages\".")
 (p "A project may have code organized across many different files. In order to call
code from one file in another file, you will need to have that file loaded into
your Lisp image. If "
 (code "core.lisp") " calls and relies on "
 (code "utils.lisp") " code, then"
 (code "utils.lisp") " code needs to be loaded before "
 (code "core.lisp") " is loaded and run. If
you have a lot of files that need to be loaded in a particular order, you
probably want a way to automate this process. Systems, defined with "
 (code "defsystem") "and the "
 (code "ASDF") " library, are declarations of groups of code loaded in a
particular order.")
 (p "When you need to coordinate the loading of multiple files, including third-party
systems, then it's time to think about your "
 (code "defsystem") ".")))
 (section :id "what-is-asdf-"
 (hgroup
 (span)
 (h3 "What Is ASDF?"))
 (div :class "outline-text-4" :id "text-1-2-2"
 (p "ASDF is the defacto-standard library (available out of the box with SBCL and
other implementations) that adds convenient code loading orchestration
capabilities to Lisp. You do so by defining a "
 (code "defsystem") " in a "
 (code ".asd") " file in
your project's root directory and then loading the system. When you load the
system, ASDF will load your other files depending on your system definition.")
 (p "Because "
 (code "defsystem") " is a third-party extension to Lisp and was not previously
available, there are different styles of organizing packages and systems in
older projects that are less common in more modern projects. We'll be taking a
look at different styles of factoring later, but for now let's focus on"
 (code "defsystem") " alone.")))
 (section :id "defining-systems"
 (hgroup
 (span)
 (h3 "Defining Systems"))
 (div :class "outline-text-4" :id "text-1-2-3"
 (p "Let's say we have a project that looks like this:")
 (pre :class "code-block-result-pre result"
 (code :class "code-block-result" "<p>
% tree
.
├── almighty-system.asd
└── src
    └── core.lisp
</p>"))
 (p "This is our "
 (code "defsystem") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defsystem \"almighty-system\"
  :author \"Micah Killian\"
  :version \"0.0.1\"
  :description \"A system for demonstrating how to define systems.\"
  :depends-on (\"local-time\")
  :components (:module \"src\"
               :pathname \"src\"
               (:file \"core\")))"))
 (p "If we load this system (more on that later), ASDF will look for a local copy of
the "
 (code "local-time") " library, downloading it with "
 (code "Quicklisp") " (more on that later)
if a local copy doesn't exist. After loading the "
 (code "defsystem") " defined by"
 (code "local-time") ", ASDF will read the "
 (code "core.lisp") " file in the "
 (code "src") " directory and
load it into the Lisp image.")))
 (section :id "loading-systems"
 (hgroup
 (span)
 (h3 "Loading Systems"))
 (div :class "outline-text-4" :id "text-1-2-4"
 (p "Loading a system requires first that the "
 (code ".asd") " file is loaded.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(asdf:load-asd (merge-pathnames *default-pathname-defaults* \"almighty-system.asd\"))"))
 (p "This requires some explanation. "
 (code "asdf:load-system") " takes a "
 (code "pathname") " to the"
 (code ".asd") " file. If you have the absolute path available, you can use that:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(asdf:load-asd \"

/Users/micah/lisp/almighty/content/essentials/code/projects/almighty-system/almighty-system.asd\")"))
 (p "Or, in the REPL, you can type "
 (code ",") " ("
 (code "sly-mrepl-shortcut") ") and select"
 (code "set-directory") " and ensure that the current working directory is set to the
project root (where the "
 (code ".asd") " file lives), and then run the other line above."
 (code "set-directory") " will set the value of "
 (code "*default-pathname-defaults*") ", and"
 (code "merge-pathnames") " does what it says. Thus, it's equivalent to the following:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(asdf:load-asd (merge-pathnames \"
/Users/micah/lisp/almighty/content/essentials/code/projects/almighty-system/\" \"
almighty-system.asd\"))"))
 (p "After you load the asd file, you can load the system with "
 (code ",") " and "
 (code "load system") "or "
 (code "(asdf:load-system \"almighty-system\")") " in the REPL."))))
 (section :id "styles-of-factoring-packages-systems"
 (hgroup
 (span)
 (h2 "Styles of Factoring Packages & Systems"))
 (div :class "outline-text-3" :id "text-1-3"
 (p "Instead of dealing with systems separately from packages, it's useful to see how
systems can be configured depending on different code organization strategies.
As you might expect, the way you organize a project is heavily influenced by how
large the project is."))
 (section :id "mother-of-all-package-strategy"
 (hgroup
 (span)
 (h3 "Mother Of All Package Strategy"))
 (div :class "outline-text-4" :id "text-1-3-1"
 (p "A good representation of the Mother Of All package strategy of Common Lisp code
organization is "
 (code "hunchentoot") "–a popular Lisp web server and web dev framework.
If we take a look at the file tree it looks like this:")
 (pre :class "code-block-result-pre result"
 (code :class "code-block-result" "<p>
% tree
.
├── acceptor.lisp
├── CHANGELOG
├── CHANGELOG<sub>TBNL</sub>
├── compat.lisp
├── conditions.lisp
├── cookie.lisp
├── docs
│   ├── hunchentoot.gif
│   ├── index.html
│   └── LICENSE.txt
├── easy-handlers.lisp
├── headers.lisp
├── hunchentoot.asd
├── lispworks.lisp
├── log.lisp
├── make-docstrings.lisp
├── mime-types.lisp
├── misc.lisp
├── packages.lisp
├── README.md
├── release-checklist.txt
├── reply.lisp
├── request.lisp
├── run-test.lisp
├── session.lisp
├── set-timeouts.lisp
├── specials.lisp
├── ssl.lisp
├── taskmaster.lisp
├── test
│   ├── ca-built-via-xca.xdb
│   ├── ca.crt
│   ├── client.crt
│   ├── favicon.ico
│   ├── fz.jpg
│   ├── packages.lisp
│   ├── script-engine.lisp
│   ├── script.lisp
│   ├── server.crt
│   ├── server+ca.crt
│   ├── test-handlers.lisp
│   ├── test-key-no-password.key
│   └── UTF-8-demo.html
├── url-rewrite
│   ├── packages.lisp
│   ├── primitives.lisp
│   ├── specials.lisp
│   ├── url-rewrite.lisp
│   └── util.lisp
└── util.lisp
</p>"))
 (p "The organization is flat, lots of separate files, mostly in the project root
directory.")
 (p "Hunchentoot has one package definition (well, two), found in the "
 (code "packages.lisp") "file.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(in-package :cl-user)

(defpackage #:hunchentoot
  (:nicknames #:tbnl)
  (:use :cl :cl-ppcre :chunga :flexi-streams :url-rewrite :alexandria)
  (:shadow #:defconstant
           #:url-encode)
  (:export #:*acceptor*
           #:*catch-errors-p*
           #+:lispworks
           #:*cleanup-function*
           #+:lispworks
           #:*cleanup-interval*
           #:*content-types-for-url-rewrite*
           #:*default-connection-timeout*
           #:*default-content-type*
           #:*dispatch-table*
           #:*file-upload-hook*
                                        ; ... and many more
           ))                           "))
 (p
 (code ":nicknames") " defines what other package name qualifiers you can use besides"
 (code "hunchentoot") ". That means that Hunchentoot itself provides you the ability to
use "
 (code "(tbnl:*acceptor*)") " if you want. Useful for allowing users to use a smaller
package name qualifier on their symbols.")
 (p "Hunchentoot uses "
 (code ":use") " for several utility libraries. Again, I don't recommend
it, but it is not uncommon.")
 (p
 (code ":shadow") " here creates an symbol or symbols that are initially unbound in the
package. Clearly, there was a naming conflict somewhere between Common Lisp's
built-in "
 (code "defconstant") " and the "
 (code "url-encode") " symbol intended to have two
different values ("
 (code "utils.lisp") " and "
 (code "url-rewrite/url-rewrite.lisp") " both define"
 (code "url-encode") " functions). What this means is that in the "
 (code "hunchentoot") " package,
if you write "
 (code "url-encode") " while you are in the "
 (code "hunchentoot") " package, it has a
different value than if you are in the "
 (code "url-rewrite") " package.")
 (p "It's a wonky way of preventing a naming conflict error because the "
 (code "hunchentoot") "package "
 (i "uses")
 (code ":url-rewrite") " instead of importing it. However, you might
sometimes need to use "
 (code ":shadow") " to get around naming conflicts caused by other
systems/libraries that use Common Lisp symbol names, so it's something to keep
in mind.")
 (p "The important point is this: All of the files in the project root begin with"
 (code "(in-package :hunchentoot)") ". Each file contributes their defined symbols to the"
 (code ":hunchentoot") " package.")
 (p "If all this package talk has the ol' nogging working overtime, you might be
thinking, \"Hey Micah, doesn't that assume that the "
 (code "packages.lisp") " file is
loaded before all the others?\"")
 (p "Yes, yes it does. Let's take a look at the "
 (code "hunchentoot.asd") " file:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defsystem :hunchentoot
  :serial t
  :version \"1.3.1\"
  :description \"Hunchentoot is a HTTP server based on USOCKET and
  BORDEAUX-THREADS. It supports HTTP 1.1, serves static files, has a simple
  framework for user-defined handlers and can be extended through subclassing.\"
  :license \"BSD-2-Clause\"
  :depends-on (:chunga
               :cl-base64
               :cl-fad
               :cl-ppcre
               :flexi-streams
               (:feature (:not (:or :lispworks :hunchentoot-no-ssl))
                         :cl+ssl)
               :md5
               :alexandria
               :rfc2388
               :trivial-backtrace
               (:feature (:not :lispworks) :usocket)
               (:feature (:not :lispworks) :bordeaux-threads))
  :components ((:module \"url-rewrite\"
                :serial t
                :components ((:file \"packages\")
                             (:file \"specials\")
                             (:file \"primitives\")
                             (:file \"util\")
                             (:file \"url-rewrite\")))
               (:file \"packages\")
               (:file \"lispworks\" :if-feature :lispworks)
               (:file \"compat\" :if-feature (:not :lispworks))
               (:file \"specials\")
               (:file \"conditions\")
               (:file \"mime-types\")
               (:file \"util\")
               (:file \"log\")
               (:file \"cookie\")
               (:file \"reply\")
               (:file \"request\")
               (:file \"session\")
               (:file \"misc\")
               (:file \"headers\")
               (:file \"set-timeouts\")
               (:file \"taskmaster\")
               (:file \"acceptor\")
               (:file \"ssl\" :if-feature (:not :hunchentoot-no-ssl))
               (:file \"easy-handlers\"))
  :perform (test-op (o c) (load (merge-pathnames \"run-test.lisp\" (system-source-directory
 c)))))"))
 (p "Notice "
 (code ":serial t") " up at the top. That means that all of the "
 (code ":components") " will
be loaded "
 (i "in the order they are listed") ". So, after all of the "
 (code ":depends-on") "dependency systems are loaded, the "
 (code "url-rewrite") " module will be loaded. Its
contents will also be loaded in order, and so first the"
 (code "url-rewrite/packages.lisp") " file will be loaded, then "
 (code "specials.lisp") ","
 (code "primitives.lisp") ", etc. After that module is loaded, the first file in the
project root that will be loaded is its "
 (code "packages.lisp") " file that we look at
before. The Mother Of All "
 (code "packages.lisp") " is where all the symbols for the
majority of the project live and are namespaced.")
 (p "There are only two other systems in the Hunchentoot project: one for development
and the other for testing. They don't provide any further insight into this
strategy, so we will skip them.")
 (p "In summary, the Mother Of All Package Strategy is this: you have one package
("
 (code "defpackage :hunchentoot") ") and one system ("
 (code "defsystem :hunchentoot") "), and that
one package "
 (code ":uses") " or imports from several other packages. Other code files
begin with "
 (code "(in-package :hunchentoot)") ", giving them access to the symbols
defined in other files and contributing the symbols they define to that package.
The "
 (code "packages.lisp") " file's exports define the public interface for users. This
style is \"retro\", but still perfectly usable especially on smaller projects, and
even larger projects like Hunchentoot. The "
 (code "vend") " library is a modern Lisp
library that use this Mother of All Package Strategy.")))
 (section :id "multiple-systems-strategy"
 (hgroup
 (span)
 (h3 "Multiple Systems Strategy"))
 (div :class "outline-text-4" :id "text-1-3-2"
 (p "It's also possible that a single project might have several systems, primarily
used as optional extensions to the library's core functionality. "
 (code "transducers") "is one such library, as is "
 (code "mito") ". However, "
 (code "transducers") " still uses just one
package for the core functionality. It's "
 (code "defsystem") " definition also uses the"
 (code "serial t") " approach of loading source files in order. This Multi-System Strategy
is modular, yet does not exclude the possibility of using the Mother Of All
Package Strategy.")))
 (section :id "one-package-per-file-strategy"
 (hgroup
 (span)
 (h3 "One Package Per File Strategy"))
 (div :class "outline-text-4" :id "text-1-3-3"
 (p
 (code "mito") ", on the other hand, in addition to having multiple systems–one for core
functionality and others for auxiliary functionality–also uses the One Package
Per File Strategy. This is a more \"modern\" approach and considered by some to be
the preferred strategy generally.")
 (p "Let's take a look at the "
 (code "mito") " system:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defsystem \"mito\"
  :version \"0.2.0\"
  :author \"Eitaro Fukamachi\"
  :license \"BSD 3-Clause\"
  :depends-on (\"mito-core\"
               \"mito-migration\"
               \"lack-middleware-mito\"
               (:feature :sb-package-locks \"cl-package-locks\"))
  :components ((:file \"src/mito\"))
  :description \"Abstraction layer for DB schema\"
  :in-order-to ((test-op (test-op \"mito-test\"))))"))
 (p "Here, we notice that the "
 (code ":depends-on") " list is three other subsystems in the"
 (code "mito") " project. Let's look at "
 (code "mito-core.asd") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defsystem \"mito-core\"
  :version \"0.2.0\"
  :author \"Eitaro Fukamachi\"
  :license \"BSD 3-Clause\"
  :depends-on ((:version \"dbi\" \"0.11.1\")
               \"sxql\"
               \"cl-ppcre\"
               \"closer-mop\"
               \"dissect\"
               \"trivia\"
               \"local-time\"
               \"uuid\"
               \"alexandria\")
  :components ((:file \"src/core\" :depends-on (\"core-components\"))
               (:module \"core-components\"
                :pathname \"src/core\"
                :components
                ((:file \"dao\" :depends-on (\"dao-components\"))
                 (:module \"dao-components\"
                  :pathname \"dao\"
                  :depends-on (\"connection\" \"class\" \"db\" \"conversion\" \"logger\" \"
util\")
                  :components
                  ((:file \"table\" :depends-on (\"column\" \"mixin\" \"view\"))
                   (:file \"view\" :depends-on (\"column\"))
                   (:file \"mixin\" :depends-on (\"column\"))
                   (:file \"column\")))
                 (:file \"class\" :depends-on (\"class-components\"))
                 (:module \"class-components\"
                  :pathname \"class\"
                  :depends-on (\"error\" \"util\")
                  :components
                  ((:file \"table\" :depends-on (\"column\"))
                   (:file \"column\")))
                 (:file \"connection\" :depends-on (\"error\"))
                 (:file \"type\" :depends-on (\"db\"))
                 (:file \"db\" :depends-on (\"db-drivers\" \"connection\" \"class\" \"
util\"))
                 (:module \"db-drivers\"
                  :pathname \"db\"
                  :depends-on (\"logger\" \"util\")
                  :components
                  ((:file \"mysql\")
                   (:file \"postgres\")
                   (:file \"sqlite3\")))
                 (:file \"conversion\")
                 (:file \"logger\")
                 (:file \"error\")
                 (:file \"util\")))))"))
 (p "Now we see some third-party systems in the "
 (code ":depends-on") " list. What's more
interesting here is the definition in "
 (code ":components") ": Many of the files use"
 (code ":depends-on") " as well, pointing to other files within the system. We don't see"
 (code ":serial t") " here, either. Instead of a linear dependency tree, we have a graph.
In the "
 (code "hunchentoot") " system, we saw that the more minor, prerequisite files like"
 (code "packages.lisp") " came "
 (i "first") ". Here in the "
 (code "mito-core") " system, the major files
are listed first, with their prerequisites coming later in the list. The order
is inverted.")
 (p "Let's take a look inside "
 (code "src/mito.lisp") " (the only component in the "
 (code "mito.asd") "file above):")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(uiop:define-package #:mito
  (:use #:cl)
  (:use-reexport #:mito.core
                 #:mito.migration))
(in-package #:mito)"))
 (p "Instead of "
 (code "defpackage") ", Mito uses UIOP's "
 (code "define-package") " macro. It uses it
because it provides a new option, "
 (code ":use-reexport") ". It's like "
 (code ":use") ", but with
two important differences:")
 (ol :class "org-ol"
 (li "It will import all of the "
 (i "exported") " symbols of the packages that are passed
as arguments, and then "
 (i "reexport them") ".")
 (li "If there is a naming conflict between symbols in the packages passed in the
form, "
 (i "the first one takes precedence") ", shadowing versions that come later.
That means that if there is a "
 (code "mito.core:cool-symbol") " and a"
 (code "mito.migration:cool-symbol") ", because "
 (code "mito.core") " comes first in the above
definition, the "
 (code "mito") " package with inherit "
 (code "mito.core:cool-symbol") " and then
that version will be reexported."))
 (p "In systems like Mito, the "
 (code "reexport") " options ("
 (code ":reexport") ", "
 (code ":use-reexport") ", and"
 (code ":mix-reexport") ") are used to help easily \"bubble up\" exports from peripheral
packages into one final public API.")
 (p "However, although Mito bubbles up dependencies with "
 (code ":depends-on") ", using the One
Package Per File Strategy doesn't preclude using "
 (code ":serial t") ", either.")))
 (section :id "one-system-per-package"
 (hgroup
 (span)
 (h3 "One System Per Package"))
 (div :class "outline-text-4" :id "text-1-3-4"
 (p "There is one other strategy worth noting. It's perhaps best represented by the"
 (code "utopian") " web framework. Let's take a look at its system definition:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defsystem \"utopian\"
  :class :package-inferred-system
  :version \"0.9.1\"
  :author \"Eitaro Fukamachi\"
  :license \"LLGPL\"
  :description \"Web application framework\"
  :pathname \"src\"
  :depends-on (\"utopian/main\")
  :in-order-to ((test-op (test-op \"utopian-tests\"))))

(register-system-packages \"lack-component\" '(#:lack.component))
(register-system-packages \"lack-request\" '(#:lack.request))
(register-system-packages \"lack-response\" '(#:lack.response))
(register-system-packages \"mystic\" '(#:mystic.util))
(register-system-packages \"mystic-file-mixin\" '(#:mystic.template.file))"))
 (p "The first thing you'll notice is that the definition is short. The dependencies
only include…another file in the system? And there are no "
 (code ":components") "! To
investigate, let's look in the \"utopian/main\"… file? Package? System?")
 (p "It's found in "
 (code "src/main.lisp") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(uiop:define-package #:utopian
  (:nicknames #:utopian/main)
  (:use #:cl)
  (:mix-reexport #:utopian/routes
                 #:utopian/views
                 #:utopian/context
                 #:utopian/app
                 #:utopian/config
                 #:utopian/exceptions))
(in-package #:utopian)"))
 (p "We already saw "
 (code ":nicknames") " in the Hunchentoot "
 (code "packages.lisp") " file."
 (code ":mix-reexport") " is like "
 (code ":use-reexport") " that we saw in Mito, except that it will
inherit "
 (i "all") " symbols, exported and not, from the packages that follow into the
current package. Every single symbol from those six packages above are going to
be inherited into the "
 (code "#:utopian") " package.")
 (p "But now you may be wondering, \"How do they get loaded if the system file doesn't
list them in "
 (code ":components") "?\"")
 (p "That's not all, friends. The mystery deepens when we look in the"
 (code "utopian/routes") " package file:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defpackage #:utopian/routes
  (:use #:cl)
  (:import-from #:utopian/context
                #:*request*
                #:*response*)
  (:import-from #:utopian/file-loader
                #:intern-rule)
  (:import-from #:myway
                #:make-mapper
                #:connect
                #:next-route)
  (:import-from #:lack.request
                #:request-parameters)
  (:export #:defroutes
           #:routes
           #:routes-mapper
           #:routes-controllers-directory
           #:route

           ;; from MyWay
           #:next-route))
;; ..."))
 (p "Wait, there is a third-party library--"
 (code "myway") "–imported in this package! That
wasn't in the system definition!")
 (p "Actually, it is. Look again. See this line?")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" ":class :package-inferred-system"))
 (p "That one line drastically changes how packages and systems work.")
 (p
 (b "All packages in a "
 (code ":package-inferred-system") " are treated as entire systems
themselves") ".")
 (p "Package inferred systems will all have an entry-point into the system, like"
 (code ":depends-on (\"utopian/main\")") ". Remember: up until now, dependencies were
systems. We were confused because "
 (code ":utopian") " is a "
 (i "package") ", not a "
 (i "system") ". But
because of "
 (code ":package-inferred-system") ", "
 (code ":utopian") " and all of its dependent
packages are treated like their own systems by ASDF! That's why the files for
the rest of the project aren't listed in "
 (code ":depends-on") ": "
 (code "utopian/main") " is
treated like a system, where its prerequisite packages are as well. ASDF will do
the work of resolving the dependencies, hence why you don't need to list them
manually. It will "
 (code "load") " prerequisite packages/systems both in the "
 (code "utopian") "project, "
 (b "including third-party libraries") ", downloading them if necessary.")
 (p "Package inferred systems are the Package Per File taken to its limit: System Per
File.")
 (p "Using package inferred systems requires following a few rules which I'll
demonstrate.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defsystem \"almighty\"
  :class :package-inferred-system
  :author \"Micah Killian\"
  :version \"0.0.1\"
  :description \"A system for demonstrating how to define systems.\"
  :pathname \"src\"
  :depends-on (\"almighty/main\"))"))
 (p "Notice here that the system is named "
 (code "almighty") ", and that the package/system in"
 (code ":depends-on") " begins with "
 (code "almighty") ". The entry-point and all subsequent
packages in your package-inferred-system must begin with "
 (code "almighty") ".")
 (p "Notice also that "
 (code ":pathname") " is set. The tree view of this project looks like
this:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" ".
├── almighty.asd
└── src
    ├── config.lisp
    ├── db.lisp
    ├── main.lisp
    └── models
        └── person.lisp"))
 (p "And the files look like this:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" ";; src/main.lisp
(uiop:define-package #:almighty
  (:use #:cl)
  (:nicknames #:almighty/main)
  (:use-reexport #:almighty/config
                 #:almighty/db
                 #:almighty/models/person))
(in-package #:almighty)

;; src/db.lisp
(defpackage #:almighty/db
  (:use #:cl)
  (:export #:hello-from-db))
(in-package #:almighty/db)

(defun hello-from-db ()
  :hello-from-deebee)

;; src/config.lisp
(defpackage #:almighty/config
  (:use #:cl)
  (:export #:hello-from-config))
(in-package #:almighty/config)

(defun hello-from-config ()
  :hello-from-config)

;; src/models/person.lisp
(defpackage #:almighty/models/person
  (:use #:cl)
  (:export
   #:hello-person))
(in-package #:almighty/models/person)

(defun hello-person ()
  :i-am-a-person)"))
 (p "In a PIS, The package name of a system begins with the system name, "
 (code "almighty") ".
The part that follows is the file name, such as "
 (code "almighty/db") " for "
 (code "src/db.lisp") ".
If a package's file is in a subdirectory, the name of the directory comes before
the file name, as in "
 (code "almighty/models/person") " for "
 (code "src/models/person.lisp") ".")
 (p "If you don't follow these rules, ASDF won't be able to find the files. For
example, if you have a file "
 (code "src/wrong-name.lisp") " that defines a package"
 (code "almighty/correct-name") " and try to "
 (code ":import-from") ", "
 (code ":use-reexport") ", etc. the
package "
 (code "almighty/correct-name") " in another package like "
 (code "almighty/db") ", you will
receive an error stating:")
 (pre :class "code-block-result-pre result"
 (code :class "code-block-result" "<p>
Unknown location:
  error: 
    Component \"almighty/correct-name\" not found, required by
    #&lt;PACKAGE-INFERRED-SYSTEM \"almighty/db\"&gt;
</p>

<p>
Compilation failed.
</p>"))
 (p "With the exceptions of the different system definition and file and package
naming conventions imposed by them, PIS are the same as the One Package Per File
Strategy above."))))
 (section :id "the-great-debate-package-system-best-practices"
 (hgroup
 (span)
 (h2 "The Great Debate: Package & System Best Practices"))
 (div :class "outline-text-3" :id "text-1-4"
 (p "As a newcomer to Lisp, you might want to be able to fit in with the broader
ecosystem. You want to learn the idioms and patterns common to Lisp users,
private and professional. So even if you're a sophisticated senior who knows
that \"it depends\" is probably the answer, you still ask the question, \"Which
strategy is should I choose? Which one's the best?\""))
 (section :id "argument-in-favor-of-pis"
 (hgroup
 (span)
 (h3 "Argument in Favor of PIS"))
 (div :class "outline-text-4" :id "text-1-4-1"
 (p "The architect of ASDF wrote "
 (a :href "https://github.com/fare/asdf/blob/master/doc/best_practices.md" "a document") " precisely to answer this question.
Unfortunately, it leaves more questions than answers. First, it explains
problems with the other strategies we've covered:")
 (blockquote
 (p "When you start writing large enough systems, putting everything in one big
package leads to a big mess: it's hard to find what function is defined where,
or should be defined where; you invent your own symbol prefixing system to avoid
name clashes; totally unrelated things end up in the same mother-of-all package;
you divide your mother-of-all package into a few subpackages, but as the
software keeps growing each of these packages in turn becomes too big.")
 (p "Meanwhile, as you grow large enough libraries, you find that you loading a big
library just to use a small part of it becomes a big hassle, leading to code
bloat, too much recompilation, too much re-testing, and not enough understanding
of what's going on."))
 (p "So the problems with the Mother Of All Package Strategy are:")
 (ol :class "org-ol"
 (li "Naming conflicts.")
 (li "Hard to understand.")
 (li "A hassle to load and recompile."))
 (p "We saw with Hunchentoot that \"shadowing\" of symbols was a little hard to
understand, yet appears to be the solution to naming conflicts in a project
factored as it is. If you have files using symbol names found in other files in
project, it can be hard to understand which symbol's value is the "
 (i "actual") " value
used in the overall package. Thus, you namespace using some sort of naming
convention.")
 (p "Additionally, you might try dividing into some sub-packages, but it's somewhat
arbitrary, meaning developers have to come up with their own coding conventions
regarding package management. Whether you use one package or multiple, if your
system is large enough, those strategies will ultimately cause trouble.")
 (p "After explaining how Package Inferred Systems work, he explains how they solve
the above problems:")
 (blockquote
 (p "This allows for large modular libraries, wherein you may use one file, and only
that file and its transitive dependencies will be loaded, rather than the entire
humongous library.")
 (p "This also helps you enforce a discipline wherein it is always clear in which
file each symbol is defined, which files have symbols used by any other file,
etc."))
 (p "In "
 (a :href "https://x.com/almighty_lisp/status/1938416300826759368?s=20" "a discussion I had with the author on X") ", our friend François-René Rideau (or"
 (a :href "https://github.com/fare" "fare on Github") "), I asked about the rationale behind PIS, why it is the best
practice, etc. He said that the motivation to create PIS was to help large
teams, but he also said that PIS can be useful for individuals with large or
ambitious projects. He said that large .asd files (remember Mito's system?) tend
to be error-prone. Further, he says,")
 (blockquote
 (p "[In large .asd files] you often leave a lot of obsolete dependencies that slow
the build and the understanding of the code by newcomers."))
 (p "So large system definitions are scary to mess with, making it safer and
therefore more likely for teams to leave unused dependencies in the system. That
can make understanding the code more difficult, and make builds unnecessarily
slower.")
 (p "While ultimately he didn't make a conclusive recommendation to use PIS, he
appears to prefer using PIS from the beginning to improve modularity and
understandability of code, and to make coordination with other developers
easier.")))
 (section :id "arguments-against-pis"
 (hgroup
 (span)
 (h3 "Arguments Against PIS"))
 (div :class "outline-text-4" :id "text-1-4-2"
 (p "So the argument in favor of Package Inferred Systems boils down to this:")
 (ol :class "org-ol"
 (li "It helps reduce problems of coordination with other developers.")
 (li "It helps make the code more understandable.")
 (li "It helps reduce the amount of unnecessary code that remains in the project."))
 (p "The question is: Does it really do all those things? And are PIS more effective
than the alternatives?")
 (p "Understanding packages and systems is one of the more challenging things about
learning Common Lisp for newcomers. That's why I've gone into more detail here
on the subject than I have with other language features.")
 (p "Before I make my counter-argument to PIS, I must emphasize that I have no
professional experience with Common Lisp, nor have I written any large projects
with it. Fare, on the other hand, is an accomplished Lisper with experience
writing Lisp in large teams. The odds that I am not understanding the benefits
of PIS because of a lack of experience are astronomically high. I make my
counter-arguments with fear and trepidation in my heart. All I have to guide me
are my small brain and an intuition–as limited as it may be–about how simple
programs and programming should be.")
 (p "My experience of package inferred systems is that they actually led to "
 (i "more") "confusion, not less. Perhaps you felt it too as we took our journey through
these different strategies.")
 (p "Hunchentoot listed all third-party dependencies in the system definition–thus
you had a sense of what code you needed to download, and what your potential
exposure to bugs or security problems was. The "
 (code "defsystem") " used the "
 (code ":serial t") "option to let you, the newcomer, know that all of the "
 (code ":component") " files were
loaded in the order they were listed. The Mother Of All "
 (code "packages.lisp") " gave us
an easy to read overview of the public API. Without any sophisticated knowledge
or tools, the code was understandable (with perhaps the exception of the use of"
 (code ":shadow") " for a couple of symbols).")
 (p "Was it always that way? How many challenges did the project have with
coordination or understandability before it reached the point where we saw it?
How much unseen discipline was practiced in the naming and organization of
symbols in the project? For example, perhaps it was difficult to organize and
line up the files in the "
 (code "defsystem") " definition so that all of the right code
was loaded in the correct order? Is the project bloated? We see only a snapshot
of the project as it is now. We can't know the challenges of coordination,
dependency management, and code factoring that had to be overcome just from
looking at the source.")
 (p "However, when we look at the source, the design of the system is undeniably"
 (i "simple") " and "
 (i "easy to understand") ", even for a dummy newcomer like me.")
 (p "The same is true of the "
 (code "vend") " library, which uses a similar strategy.")
 (p
 (code "mito") ", on the other hand, has a somewhat more difficult to follow "
 (code "defsystem") ":
the liberal use of "
 (code ":depends-on") " and the inversion of the dependency tree makes
the system definition much more difficult to follow. The system has several
subsystems, and some of the packages use options like "
 (code ":use-reexport") " and so-on.
You need to have a deeper understanding of how systems work, how shadowing
works, and how those package options will effect the final shape of the system.
While Hunchentoot was fairly transparent, Mito's system architecture introduced
a fair bit of noisy abstractions that make reasoning about the system difficult
at a glance.")
 (p "But perhaps with experience I would see the value in the abstractions? Perhaps
the alternative would have been noisier, more difficult to extend with other
developers, and more bloated?")
 (p "And then there's "
 (code "utopian") " and "
 (code ":package-inferred-system") ". The .asd file is
small, but that just left us more questions than answers. Chief among them is:
how many dependencies does it have, and where do they come from? What does the
final public API actually look like after all subsystem symbols are bubbled up
to the top system? What code is loaded (and potentially executed), and when?")
 (p "About API discoverability, Fare said the following in our discussion on X:")
 (blockquote
 (p "The SLIME REPL, not the unaided source code, is how you discover the code. Plus
the generated documentation."))
 (p "This is an important point: Great tools can make a developer's job easier.
That's why one of the goals of this book is to help you understand all of the
tools available to you in Emacs. If you code Common Lisp in Emacs without
knowing about window management, REPL shortcuts, SLY debugger
restarts/keybindings, "
 (code "evil-mode") " VIM keybindings, etc. you are missing out on a
great number of very powerful tools for efficient and pleasant coding.")
 (p "However, I disagree with Fare about how code is discovered. Yes, we can type"
 (code "SPC m g d") " or "
 (code "g d") " to use "
 (code "sly-edit-definition") " to go to the definition of
some symbol. Yes, we can look at the documentation. But none of those provide a
clear overview of third-party dependencies of the project nor a clear view of
the shape of the system as a whole. Tools like PIS blur our vision of the whole
system. When I look at projects that use those tools, every bone in my body
screams, \"Complexity spirit demon.\"")
 (p "Unfortunately, I am not the only one that feels this way. Do a search for
\"package-inferred-system\" on X and you'll find a lot of confusion or even
distaste for it.")
 (p "Still, there are some notable people that really enjoy it. The most well-known
users are probably the "
 (a :href "https://github.com/fukamachi/" "Eitaro Fukamachi (https://github.com/fukamachi)") " (author
of "
 (code "mito") " and many other libraries) and "
 (a :href "https://github.com/svetlyak40wt" "Alexander Artemenko
(https://github.com/svetlyak40wt)") "–the man behind "
 (a :href "https://github.com/40ants" "40ants
(https://github.com/40ants)") " and Ultralisp–both of whom are prolific giants in
the Common Lisp community (I think I'm starting to sweat here). Alexander has
explained why he likes PIS "
 (a :href "https://x.com/svetlyak40wt/status/1179260459255549952?s=20" "on X") ":")
 (blockquote
 (p "For me this more important consequence is that I have not to figure out
dependencies between files anymore and to hardcode them in the *.asd files.
System definition can be just: (defsystem \"foo\" :class :package-inferred-system
:depends-on (\"foo/main\"))"))
 (p "This echos what Fare says about system understandability and ease of code
factoring. So maybe there is something to the arguments about the challenges of
factoring that standard "
 (code "defsystem") " definitions impose.")
 (p "My brain may not be big enough to understand the ease-of-use and
ease-of-understanding arguments. There's a good chance it's a skill issue.")
 (p "However, there is one feature of PIS that unfortunately I can't forgive, no
matter how small my brain.")
 (p "The fact that third-party dependencies are not defined in the .asd file, but
instead are spread across the codebase, is simply not acceptable. As an Almighty
Lisp programmer, dependency management needs to be crystal clear both for
understanding and for containment. Package inferred systems simply make it too
easy to introduce dependencies in unknown places. My goal is the keep my use of
dependencies limited, decreasing my dependencies over time. If it's easy to add
and forget about them, then the temptation to increase my dependencies will be
too great.")
 (p "The community doesn't appear to view PIS as the way forward. Community adoption
of PIS is very low. Package inferred systems have been around since 2014, more
than 10 years ago. At present, while there are nearly 5000 systems available on
the Quicklisp repository, only around 60 unique systems use"
 (code ":package-inferred-system") ".")
 (p "LEM–an Emacs clone written from scratch in Common Lisp–and Coalton–a new Lisp
language implementing a Haskell-like type system–are among the largest and most
ambitious publicly available projects. Neither use PIS. Both use the "
 (code ":serial t") "option for the "
 (code ":components") ", manually listing dependencies and files to load
from the project. Both make heavy use of the One Package Per File Strategy, but
LEM also uses a hybrid Mother Of All Package. Coalton provides a public API in a"
 (code "package.lisp") " file! In fact, Coalton uses a strategy similar to Fare's "
 (code "LIL") "library–providing a "
 (code "package.lisp") " file for smaller \"modules\" in the system
(modules by convention) and reexporting the symbols from the module files,
allowing the system to bubble up symbols to the top level packages.")))
 (section :id "conclusion"
 (hgroup
 (span)
 (h3 "Conclusion"))
 (div :class "outline-text-4" :id "text-1-4-3"
 (p "So to answer the question, \"Is it essential to learn and use package inferred
systems?\", the answer is "
 (b "No") ". For small projects, a Mother Of All package can
work fine, but the most common approach on modern projects by far is One Package
Per File with some strategy for bubbling up and potentially listing exported
symbols into a final API.")))))))
 (button :class "toc-viewer" "TOC")
 (script "HighlightLisp.highlight_auto();"))
"                                                                                                                                                                                                                                                                                                                                                                                                                                                              "