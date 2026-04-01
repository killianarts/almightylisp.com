
(body
 (div :class "sidebar-container"
 (div :class "book-navigation"
 (h1 :class "book-navigation__book-title" "Almighty Lisp: The Essentials")
 (p :class "book-navigation__chapter-name-heading"
 (span "Almighty Lisp: The Essentials"))
 (nav
 (ul
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#sly-backtrace-navigation"
 (span :class "book-navigation__section-name" "SLY BACKTRACE NAVIGATION")
 (span :class "book-navigation__section-number" "1.1.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-db-up-sly-db-down-"
 (span :class "book-navigation__section-name" "=sly-db-up= & =sly-db-down=")
 (span :class "book-navigation__section-number" "1.2.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-db-toggle-details-"
 (span :class "book-navigation__section-name" "=sly-db-toggle-details=")
 (span :class "book-navigation__section-number" "1.3.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-db-show-frame-source-"
 (span :class "book-navigation__section-name" "=sly-db-show-frame-source=")
 (span :class "book-navigation__section-number" "1.4.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-db-details-up-sly-db-details-down-"
 (span :class "book-navigation__section-name" "=sly-db-details-up= & =sly-db-details-down=")
 (span :class "book-navigation__section-number" "1.5.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#tracing"
 (span :class "book-navigation__section-name" "TRACING")
 (span :class "book-navigation__section-number" "1.6.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-fancy-trace-"
 (span :class "book-navigation__section-name" "=sly-fancy-trace=")
 (span :class "book-navigation__section-number" "1.7.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-trace-dialog-toggle-trace-"
 (span :class "book-navigation__section-name" "=sly-trace-dialog-toggle-trace=")
 (span :class "book-navigation__section-number" "1.8.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#stickers"
 (span :class "book-navigation__section-name" "STICKERS")
 (span :class "book-navigation__section-number" "1.9.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-stickers-dwim-"
 (span :class "book-navigation__section-name" "=sly-stickers-dwim=")
 (span :class "book-navigation__section-number" "1.10.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-stickers-replay-"
 (span :class "book-navigation__section-name" "=sly-stickers-replay=")
 (span :class "book-navigation__section-number" "1.11.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-stickers-toggle-break-on-stickers-sly-db-step-"
 (span :class "book-navigation__section-name" "=sly-stickers-toggle-break-on-stickers= & =sly-db-step=")
 (span :class "book-navigation__section-number" "1.12.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#-sly-stepper-"
 (span :class "book-navigation__section-name" "=sly-stepper=")
 (span :class "book-navigation__section-number" "1.13.0")))))
 (div :class "book-navigation__previous-and-next")))
 (main :class "sidebar-main"
 (article :class "book"
 (section :id "the-lisp-ide"
 (hgroup
 (span)
 (h1 "THE LISP IDE"))
 (div :class "outline-text-2" :id "text-1"
 (p "I've already introduced some essential functions and keybindings for coding Lisp
in Emacs. Now that you have a basic knowledge of both Lisp and Emacs commands
for coding in Lisp, it will be useful to gain a deeper knowledge of the Lisp IDE
in Emacs.")
 (p "Common Lisp includes debugging functions like "
 (code "trace") ", "
 (code "break") ", etc. that allow
you to do debugging directly in the REPL.")
 (p "However, there are Common Lisp IDEs that help improve the ergonomics of using
the debugger. One of those IDEs is Sly, which was installed when we configured
the "
 (code "init.el") " file to include "
 (code "common-lisp") " language support."))
 (section :id "sly-backtrace-navigation"
 (hgroup
 (span)
 (h2 "SLY BACKTRACE NAVIGATION"))
 (div :class "outline-text-3" :id "text-1-1"
 (p "There are some functions within the Sly debugger that we can use for simple
navigation. They all are prefixed with "
 (code "sly-db-") " and are discoverable if you run"
 (code "embark-bindings") " with "
 (code "SPC h b b") " or "
 (code "C-h b b") ". We'll look at a few of them
here."))
 (section :id "-sly-db-up-sly-db-down-"
 (hgroup
 (span)
 (h3
 (code "sly-db-up") " & "
 (code "sly-db-down")))
 (div :class "outline-text-4" :id "text-1-1-1"
 (p "Move the cursor up or down the stack in the backtrace with "
 (code "C-k") " and "
 (code "C-j") ". If
the cursor is not highlighting any frames in the stack, the cursor will first
move to the top frame if you run "
 (code "sly-db-down") ".")))
 (section :id "-sly-db-toggle-details-"
 (hgroup
 (span)
 (h3
 (code "sly-db-toggle-details")))
 (div :class "outline-text-4" :id "text-1-1-2"
 (p "With the cursor over a stack frame in the backtrace, type "
 (code "t") " to show any local
variables and bindings, arguments passed, etc.")))
 (section :id "-sly-db-show-frame-source-"
 (hgroup
 (span)
 (h3
 (code "sly-db-show-frame-source")))
 (div :class "outline-text-4" :id "text-1-1-3"
 (p "If you bring the cursor over a stack frame in the backtrace and press the "
 (code "v") "key, Sly will open the source code file for that line in the backtrace and
briefly highlight the exact form that executed in that frame. Useful for
navigating straight to possible sources of error.")))
 (section :id "-sly-db-details-up-sly-db-details-down-"
 (hgroup
 (span)
 (h3
 (code "sly-db-details-up") " & "
 (code "sly-db-details-down")))
 (div :class "outline-text-4" :id "text-1-1-4"
 (p "These will do all three functions above: move the cursor to a stack from in the
backtrace, toggle open/close the details for that frame, open the source for the
frame in a different window, and highlight the form in that source. The bindings
are "
 (code "M-p") " and "
 (code "M-n") " (or "
 (code "M-k") " and "
 (code "M-j") " if you prefer more Evilly bindings
consistent with the ones above for the normal up and down commands)."))))
 (section :id "tracing"
 (hgroup
 (span)
 (h2 "TRACING"))
 (div :class "outline-text-3" :id "text-1-2"
 (p "Beyond simple navigation within the debugger is actual debugging methods and
tools.")
 (p "Tracing is a debugging method that shows a form as it is called and then what it
returns. Tracing is typically used for recursive functions."))
 (section :id "-sly-fancy-trace-"
 (hgroup
 (span)
 (h3
 (code "sly-fancy-trace")))
 (div :class "outline-text-4" :id "text-1-2-1"
 (p "Let's say you have this function:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun factorial (n)
  \"Compute factorial using linear recursion.\"
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))"))
 (p "You can run "
 (code "trace") " on a function in Emacs like this:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(trace factorial)"))
 (p "…or by highlighting a function name symbol and typing "
 (code "C-c M-t") " for"
 (code "sly-fancy-trace") ".")
 (p "If you use fancy tracing on "
 (code "factorial") " above, you will get output like this in
the REPL:")
 (pre :class "example" :id "orgc9700e3" "0: (FACTORIAL 5)
  1: (FACTORIAL 4)
    2: (FACTORIAL 3)
      3: (FACTORIAL 2)
        4: (FACTORIAL 1)
        4: FACTORIAL returned 1
      3: FACTORIAL returned 2
    2: FACTORIAL returned 6
  1: FACTORIAL returned 24
0: FACTORIAL returned 120")))
 (section :id "-sly-trace-dialog-toggle-trace-"
 (hgroup
 (span)
 (h3
 (code "sly-trace-dialog-toggle-trace")))
 (div :class "outline-text-4" :id "text-1-2-2"
 (p "Alternatively, you can trace using "
 (code "sly-trace-dialog-toggle-trace") " via "
 (code "C-c C-t") "or "
 (code "SPC m T T") ". The difference is that fancy trace will show the trace in the
REPL, whereas the trace dialog toggle requires you to open the trace dialog with"
 (code "C-c T") ".")
 (p "If you use the trace dialog, you get output like this:")
 (pre :class "example" :id "orgb3b36e9" "0 - factorial
  | > 5 (3 bits, #x5, #o5, #b101)
  | < 120 (7 bits, #x78, #o170, #b1111000)
1 `-- factorial
     | > 4 (3 bits, #x4, #o4, #b100)
     | < 24 (5 bits, #x18, #o30, #b11000)
2    `-- factorial
        | > 3 (2 bits, #x3, #o3, #b11)
        | < 6 (3 bits, #x6, #o6, #b110)
3       `-- factorial
           | > 2 (2 bits, #x2, #o2, #b10)
           | < 2 (2 bits, #x2, #o2, #b10)
4          `-- factorial
                > 1 (1 bit, #x1, #o1, #b1)
                < 1 (1 bit, #x1, #o1, #b1)")
 (p "You can "
 (code "untrace") " the function when you're done either with the Lisp function or
by using the same commands above in Emacs."))))
 (section :id "stickers"
 (hgroup
 (span)
 (h2 "STICKERS"))
 (div :class "outline-text-3" :id "text-1-3"
 (p "Where Sly's debugging capabilities really shine is with "
 (code "stickers") ". Stickers are
basically a replacement for "
 (code "print") " and "
 (code "break") " functions"))
 (section :id "-sly-stickers-dwim-"
 (hgroup
 (span)
 (h3
 (code "sly-stickers-dwim")))
 (div :class "outline-text-4" :id "text-1-3-1"
 (p "Let's say we have the following code:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun do-some-math (num)
  (/ (+ num (* num num num) (- num 10 num)) 7))
(do-some-math 9)"))
 (p "And you're thinking, \"Hmm, I wonder what the result of that addition was?\"")
 (p "You could wrap it in "
 (code "print") ". Or you could use a sticker. Highlight the opening
parenthesis of the addition form then use "
 (code "sly-stickers-dwim") " via "
 (code "C-c C-s C-s") "or "
 (code "SPC m s s") " to place a sticker on it. After placing the sticker, you need to
recompile the function ("
 (code "C-c C-c") ") to \"arm\" the sticker.")))
 (section :id "-sly-stickers-replay-"
 (hgroup
 (span)
 (h3
 (code "sly-stickers-replay")))
 (div :class "outline-text-4" :id "text-1-3-2"
 (p "With the sticker armed, when you run the function, the return value of the form
you placed the sticker on will be recorded. You can view the recording with"
 (code "sly-stickers-replay") " using "
 (code "C-c C-s C-r") " or "
 (code "SPC m s r") ".")))
 (section :id "-sly-stickers-toggle-break-on-stickers-sly-db-step-"
 (hgroup
 (span)
 (h3
 (code "sly-stickers-toggle-break-on-stickers") " & "
 (code "sly-db-step")))
 (div :class "outline-text-4" :id "text-1-3-3"
 (p "You can also configure Sly to break when computation reaches the sticker using"
 (code "sly-stickers-toggle-break-on-stickers") " via "
 (code "SPC m s b") ". During computation,
when a sticker is reached, the debugger will open with a message like this:")
 (pre :class "example" :id "orgbdd33a4" "#<JUST BEFORE #<STICKER id=145 hit-count=1>>
   [Condition of type SLYNK-STICKERS::JUST-BEFORE-STICKER]")
 (p "The sticker will also flash in the source code file window. Useful for when you
have multiple stickers and need to know which one you're looking at.")
 (p "If you choose the "
 (code "CONTINUE") " restart (either via "
 (code "c") " or "
 (code "0") " or navigating to"
 (code "[CONTINUE]") " and typing "
 (code "Enter") ") or you use "
 (code "sly-db-step") " via "
 (code "s") " in the
debugger window, you will see a message like this:")
 (pre :class "example" :id "org36a2082" "#<RIGHT-AFTER #<STICKER id=255 hit-count=2> (recorded #<RECORDING 1 values>)>
   [Condition of type SLYNK-STICKERS::RIGHT-AFTER-STICKER]")
 (p "…and the return value of the form where the sticker is placed.")))
 (section :id "-sly-stepper-"
 (hgroup
 (span)
 (h3
 (code "sly-stepper")))
 (div :class "outline-text-4" :id "text-1-3-4"
 (p "If you are having trouble tracking down the exact location of your error, you
can perform a comprehensive sweep using "
 (code "sly-stepper") " with "
 (code "C-c C-s P") ". That
will apply stickers to every form inside a function. After running"
 (code "sly-stepper") ", compile the function to arm the stickers.")
 (p
 (code "sly-stickers-replay") " is especially useful because, like with the breaking
option, it will open the source code file and flash the exact sticker whose
value it's displaying. You can also navigate to the next sticker with "
 (code "n") ", "
 (i "or
the previous one") " with "
 (code "p") ". Every time you move forward or backward in the
replay, the stickers will flash. This makes a bit easier to follow the control
flow of the code.")))))))
 (button :class "toc-viewer" "TOC")
 (script "HighlightLisp.highlight_auto();"))