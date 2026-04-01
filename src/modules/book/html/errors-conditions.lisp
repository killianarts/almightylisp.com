
(body
 (div :class "sidebar-container"
 (div :class "book-navigation"
 (h1 :class "book-navigation__book-title" "Almighty Lisp: The Essentials")
 (p :class "book-navigation__chapter-name-heading"
 (span "Almighty Lisp: The Essentials"))
 (nav
 (ul
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#signaling-conditions"
 (span :class "book-navigation__section-name" "Signaling Conditions")
 (span :class "book-navigation__section-number" "1.1.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#assertions"
 (span :class "book-navigation__section-name" "Assertions")
 (span :class "book-navigation__section-number" "1.2.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#conditions"
 (span :class "book-navigation__section-name" "Conditions")
 (span :class "book-navigation__section-number" "1.3.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#restarts"
 (span :class "book-navigation__section-name" "Restarts")
 (span :class "book-navigation__section-number" "1.4.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#handlers"
 (span :class "book-navigation__section-name" "Handlers")
 (span :class "book-navigation__section-number" "1.5.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-break-on-signals-"
 (span :class "book-navigation__section-name" "=*break-on-signals*=")
 (span :class "book-navigation__section-number" "1.6.0")))))
 (div :class "book-navigation__previous-and-next")))
 (main :class "sidebar-main"
 (article :class "book"
 (section :id "errors-conditions"
 (hgroup
 (span)
 (h1 "ERRORS & CONDITIONS"))
 (div :class "outline-text-2" :id "text-1")
 (section :id "signaling-conditions"
 (hgroup
 (span)
 (h2 "Signaling Conditions"))
 (div :class "outline-text-3" :id "text-1-1"
 (p "In Lisp, instead of \"throwing\" or \"returning\" an error, you \"signal a
condition\". The most common way to signal some "
 (code "condition") " is to use "
 (code "error") " or"
 (code "cerror") ".")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun oops (x)
  (if (zerop x)
      (error \"What the h?\")))"))
 (p "That opens the debugger:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(oops 0)"))
 (pre :class "code-block-result-pre result error"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "What the h?
   [Condition of type SIMPLE-ERROR]
Restarts:
 0: [*ABORT] Return to SLY's top level.
 1: [ABORT] abort thread (#<THREAD tid=12411 \"slynk-worker\" RUNNING {7007185183}>)")))
 (p "Notice that the "
 (code "condition") " is a "
 (code "SIMPLE-ERROR") ".")
 (p
 (code "cerror") " will create a "
 (i "continuable") " error.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun oops (x)
  (if (zerop x)
      (cerror \"Want to continue?\" \"You passed =d\" x))
  (print \"You continued.\"))"))
 (p "Calling it:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(oops 0)"))
 (pre :class "code-block-result-pre result error"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "You passed 0
   [Condition of type SIMPLE-ERROR]
Restarts:
 0: [CONTINUE] Want to continue?
 1: [*ABORT] Return to SLY's top level.
 2: [ABORT] abort thread (#<THREAD tid=12483 \"slynk-worker\" RUNNING {700776ADA3}>)")))
 (p "Continuing will continue after the point of the error.")
 (p "If you only want to make a warning, you can use "
 (code "warn") ".")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(progn
  (warn \"You are about to be Rick Rolled.\")
  (format t \"Never Gonna Give You Up\")
  (error \"You've been deserted.\")
  (format t \"I hate meta.\"))"))
 (p "If you execute this, it will print the warning and the first "
 (code "format") " in the
REPL and then open a debugger with the error message. You won't see the second"
 (code "format") " message.")))
 (section :id "assertions"
 (hgroup
 (span)
 (h2 "Assertions"))
 (div :class "outline-text-3" :id "text-1-2"
 (p "With "
 (code "assert") ", you can check a condition. If it returns "
 (code "t") ", the program
continues. If the condition returns "
 (code "nil") ", it will signal a continuable error
and then you can assign a value to the selected symbols and retry the assert.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun using-assertion (x)
  (print \"before assert\")
  (assert (> x 0)
          (x)
          \"=d Needs to be a positive number.\" x)
  (print \"after assert\")
  (print x))"))
 (p "Calling it:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(using-assertion -4)"))
 (pre :class "code-block-result-pre result error"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "Needs to be a positive number.
   [Condition of type SIMPLE-ERROR]
Restarts:
 0: [CONTINUE] Retry assertion with new value for X.
 1: [*ABORT] Return to SLY's top level.
 2: [ABORT] abort thread (#<THREAD tid=12435 \"slynk-worker\" RUNNING {7007A4FF33}>)")))
 (p "If you continue, in the REPL, this is what you'll see:")
 (pre :class "code-block-result-pre result"
 (code :class "code-block-result" "<p>
\"before assert\"
The old value of X is -4.
Do you want to supply a new value?  (y or n)
</p>"))
 (p "If you type "
 (code "y") " and "
 (code "Enter") ", then you will be prompted to enter a form to be
evaluated.")
 (pre :class "code-block-result-pre result"
 (code :class "code-block-result" "<p>
Type a form to be evaluated:
[I type in 1]
</p>

<p>
\"after assert\"
1
</p>"))))
 (section :id "conditions"
 (hgroup
 (span)
 (h2 "Conditions"))
 (div :class "outline-text-3" :id "text-1-3"
 (p "If you need or want to provide more control over error handling, you can define
your own conditions.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(define-condition naughty-word (error)
  ()
  (:documentation \"A condition for when naughty words are detected.\"))"))
 (p "Conditions are "
 (code "objects") " using the CLOS.")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(make-condition 'naughty-word)"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "#<NAUGHTY-WORD {7009850503}>")))
 (p "Let's make a list of naughty words to detect and a function to detect them.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defparameter *naughty-words* '(rust java haskell ruby python crypto bitcoin)
  \"Words that must never be uttered.\")

(defun naughty-word-p (word)
  \"Check if a word is in *naughty-words*.\"
  (member word *naughty-words*))

(defun process-word (word)
  \"Signal a naughty-word condition if a word is naughty.\"
  (when (naughty-word-p word)
    (error 'naughty-word))
  word)

(defun process-sentence (sentence)
  \"Look through all the words in a sentence and do something if any of them are
naughty.\"
  (loop :for word :in sentence
        :collect (process-word word)))

(process-sentence '(java has the best oop system among all programming languages))"))
 (p "When you run the above code, the debugger will pop up with an error:")
 (pre :class "code-block-result-pre code-block-result-pre result error"
 (code :class "code-block-result" "<p>
Condition ALMIGHTY/KAIKEI/TESTS::NAUGHTY-WORD was signalled.
   [Condition of type NAUGHTY-WORD]
</p>

<p>
Restarts:
 0: [*ABORT] Return to SLY's top level.
 1: [ABORT] abort thread (#&lt;THREAD tid=12475 \"slynk-worker\" RUNNING {70069EF533}&gt;)
</p>"))
 (p "We can improve our error messages by modifying the condition definition.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(define-condition naughty-word (error)
  ((bad-word :initarg :bad-word :initform nil :reader bad-word))
  (:documentation \"A condition for when naughty words are detected.\"
   :report (lambda (c stream)
             (format stream \"Naughty bad-word detected: ~a\" (bad-word c)))))"))
 (p "Then we update the call to "
 (code "error") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun process-word (word)
  \"Signal a naughty-word condition if a word is naughty.\"
  (when (naughty-word-p word)
    (error 'naughty-word :bad-word word))
  word)"))
 (p "Run "
 (code "process-sentence") " again and your debugger will display the following
message:")
 (pre :class "code-block-result-pre code-block-result-pre result error"
 (code :class "code-block-result" "<p>
Naughty word detected: JAVA
   [Condition of type NAUGHTY-WORD]
</p>

<p>
Restarts:
 0: [*ABORT] Return to SLY's top level.
 1: [ABORT] abort thread (#&lt;THREAD tid=12487 \"slynk-worker\" RUNNING {7007CA75C3}&gt;)
</p>

<p>
Backtrace:
 0: (PROCESS-WORD JAVA)
 1: (PROCESS-SENTENCE (JAVA HAS THE BEST OOP SYSTEM &#x2026;))
</p>"))))
 (section :id "restarts"
 (hgroup
 (span)
 (h2 "Restarts"))
 (div :class "outline-text-3" :id "text-1-4"
 (p "If you define a condition, it's because you want to provide the ability to
recover from some error. One recovery mechanism you can provide are "
 (code "restarts") ".
To provide more restarts, use "
 (code "restart-case") ".")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun replace-word (word)
  \"Replace a word in a list.\"
  (intern (string-upcase (read-line))))

(defun process-word (word)
  \"Check a word. If it's naughty, give the user the choice to replace the word.\"
  (if (naughty-word-p word)
      (restart-case
          (error 'naughty-word :word word)
        (continue ()
          :report \"Sticks and stones. Just leave the word as it is.\"
          word)
        (censor ()
          :report \"Replace the naughty word with one you like more.\"
          (replace-word word)))
      word))"))
 (p "Now when you run "
 (code "process-sentence") ", you can replace any of the words found in
the "
 (code "*naughty-words*") " list using a restart.")))
 (section :id "handlers"
 (hgroup
 (span)
 (h2 "Handlers"))
 (div :class "outline-text-3" :id "text-1-5"
 (p "Another recovery mechanism is via \"handling\". When the "
 (code "naughty-word") " condition
is signaled, you can prevent the program from stopping and the debugger from
opening by using either "
 (code "handler-case") ".")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun process-sentence (sentence)
  \"Look through all the words in a sentence and do something if any of them are
naughty.\"
  (loop :for word :in sentence
        :collect (handler-case (process-word word)
                   (naughty-word () '***))))

(process-sentence '(java has the best oop system among all programming languages))"))
 (p "Now, instead of bringing up the debugger, if a naughty word is encountered
during the iteration, it will simply be replaced by \""
 (b "*") "\".")))
 (section :id "-break-on-signals-"
 (hgroup
 (span)
 (h2
 (code "*break-on-signals*")))
 (div :class "outline-text-3" :id "text-1-6"
 (p "The "
 (code "*break-on-signals*") " is set to "
 (code "nil") " by default, but when set to some
condition or conditions, will break just before the condition is signaled. It's
useful for getting a look at the state of the program just before a condition is
signaled, especially when you don't know exactly where the error is coming from
(in which case, you could use Sly stickers + breaking as I wrote about earlier
in "
 (a :href "essentials.html#ID-f6a4fdc2-7295-4bfa-8551-f5cf4e242ee7" "THE LISP IDE/STICKERS") ")."))))))
 (button :class "toc-viewer" "TOC")
 (script "HighlightLisp.highlight_auto();"))