
(body
 (div :class "sidebar-container"
 (div :class "book-navigation"
 (h1 :class "book-navigation__book-title" "Almighty Lisp: The Essentials")
 (p :class "book-navigation__chapter-name-heading"
 (span "Almighty Lisp: The Essentials"))
 (nav
 (ul
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#simple-loops"
 (span :class "book-navigation__section-name" "Simple Loops")
 (span :class "book-navigation__section-number" "1.1.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#iterating-over-lists"
 (span :class "book-navigation__section-name" "Iterating Over Lists")
 (span :class "book-navigation__section-number" "1.2.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#iterating-over-vectors-and-strings"
 (span :class "book-navigation__section-name" "Iterating Over Vectors and Strings")
 (span :class "book-navigation__section-number" "1.3.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#numeric-iteration"
 (span :class "book-navigation__section-name" "Numeric Iteration")
 (span :class "book-navigation__section-number" "1.4.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#accumulation-clauses"
 (span :class "book-navigation__section-name" "Accumulation Clauses")
 (span :class "book-navigation__section-number" "1.5.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#conditional-clauses"
 (span :class "book-navigation__section-name" "Conditional Clauses")
 (span :class "book-navigation__section-number" "1.6.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#termination"
 (span :class "book-navigation__section-name" "Termination")
 (span :class "book-navigation__section-number" "1.7.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#manually-updated-variable-bindings"
 (span :class "book-navigation__section-name" "Manually Updated Variable Bindings")
 (span :class "book-navigation__section-number" "1.8.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#parallel-iterating"
 (span :class "book-navigation__section-name" "Parallel Iterating")
 (span :class "book-navigation__section-number" "1.9.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#destructuring"
 (span :class "book-navigation__section-name" "Destructuring")
 (span :class "book-navigation__section-number" "1.10.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#the-step-binding"
 (span :class "book-navigation__section-name" "The Step Binding")
 (span :class "book-navigation__section-number" "1.11.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#running-code-before-the-first-iteration-or-after-the-last-iteration"
 (span :class "book-navigation__section-name" "Running Code Before The First Iteration Or After The Last Iteration")
 (span :class "book-navigation__section-number" "1.12.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#returning-from-named-loops"
 (span :class "book-navigation__section-name" "Returning From Named Loops")
 (span :class "book-navigation__section-number" "1.13.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#combining-everything"
 (span :class "book-navigation__section-name" "Combining Everything")
 (span :class "book-navigation__section-number" "1.14.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#when-to-use-something-else"
 (span :class "book-navigation__section-name" "When to Use Something Else")
 (span :class "book-navigation__section-number" "1.15.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#a-note-on-style-keywords-vs-symbols"
 (span :class "book-navigation__section-name" "A Note on Style: Keywords vs Symbols")
 (span :class "book-navigation__section-number" "1.16.0")))))
 (div :class "book-navigation__previous-and-next")))
 (main :class "sidebar-main"
 (article :class "book"
 (section :id "the-loop-macro"
 (hgroup
 (span)
 (h1 "THE LOOP MACRO"))
 (div :class "outline-text-2" :id "text-1"
 (p "In the chapter on Lisp Fundamentals, we briefly touched on the "
 (code "loop") " macro.
However, with a deeper knowledge of the "
 (code "loop") " macro, you can replace the use of
several built-in functions with just "
 (code "loop") ". It also has the advantage of being
easier to understand for those that are familiar with other languages (although
that led it to being somewhat controversial among lispers when it was
introduced).")
 (p "Given the amount of utility you can get out of a good understanding of "
 (code "loop") ",
we'll take a closer look at it, comparing to other built-in functions along the way."))
 (section :id "simple-loops"
 (hgroup
 (span)
 (h2 "Simple Loops"))
 (div :class "outline-text-3" :id "text-1-1"
 (p "The simplest "
 (code "loop") " runs forever:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop (print \"forever\")
      (return))"))
 (p "This is the \"simple\" form of "
 (code "loop") ". It takes a body with no clauses and
repeats it. You'll rarely use this form–it's only useful when combined with"
 (code "return") " or "
 (code "return-from") " to break out manually. The \"extended\" form, which uses
clauses like "
 (code ":for") " and "
 (code ":collect") ", is what the rest of this chapter covers.")))
 (section :id "iterating-over-lists"
 (hgroup
 (span)
 (h2 "Iterating Over Lists"))
 (div :class "outline-text-3" :id "text-1-2"
 (p "The most basic pattern iterates over the elements of a list:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for item :in '(one two three)
      :do (print item))"))
 (p "Compare this with "
 (code "dolist") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(dolist (item '(one two three))
  (print item))"))
 (p "They do the same thing. "
 (code "dolist") " is shorter for this case. When you need to do
many operations, however, then you'll find "
 (code "loop") " is more efficient and easier
to understand.")
 (p "Where "
 (code ":in") " binds the variable to each "
 (i "element") " of the list, "
 (code ":on") " binds it to
each successive "
 (i "sublist") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for sublist :on '(a b c d)
      :do (print sublist))"))))
 (section :id "iterating-over-vectors-and-strings"
 (hgroup
 (span)
 (h2 "Iterating Over Vectors and Strings"))
 (div :class "outline-text-3" :id "text-1-3"
 (p
 (code ":in") " works on lists. For vectors (including strings), use "
 (code ":across") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for char :across \"hello\"
      :collect (char-upcase char))"))
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for elem :across #(10 20 30 40)
      :sum elem)"))
 (p "Compare with "
 (code "map") ":")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(map 'list #'char-upcase \"hello\")"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "(#####)")))
 (p
 (code "map") " is more concise here, but "
 (code "loop") " lets you mix in conditions, multiple
accumulations, and other clauses that "
 (code "map") " can't express.")))
 (section :id "numeric-iteration"
 (hgroup
 (span)
 (h2 "Numeric Iteration"))
 (div :class "outline-text-3" :id "text-1-4"
 (p "You can, of course, iterated over number ranges. You can run the loop body a
fixed number of times with "
 (code ":repeat") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :repeat 3
      :do (print \"hello\"))"))
 (p "If you need an \"index\" variable, then use "
 (code ":from ... :to ..."))
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for i :from 0 :to 4
      :collect i)"))
 (p
 (code ":to") " is inclusive. If you want exclusive, use "
 (code ":below") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for i :from 0 :below 4
      :collect i)"))
 (p "Compare with "
 (code "dotimes") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(let ((result nil))
  (dotimes (i 4 (nreverse result))
    (push i result)))"))
 (p
 (code "dotimes") " gets the job done, but "
 (code "loop") " with "
 (code ":collect") " saves you from
manually pushing and reversing.")
 (p "You can count down:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for i :from 10 :downto 1
      :do (format t \"~a... \" i))"))
 (p
 (code ":above") " is the exclusive version of "
 (code ":downto") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for i :from 10 :above 0
      :collect i)"))
 (p "And you can step by any amount:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for i :from 0 :to 20 :by 5
      :collect i)"))))
 (section :id "accumulation-clauses"
 (hgroup
 (span)
 (h2 "Accumulation Clauses"))
 (div :class "outline-text-3" :id "text-1-5"
 (p
 (code ":collect") " is the most common accumulation clause, but there are several others.
It builds a list from the values:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(1 2 3 4 5)
      :collect (* 2 (sqrt n)))"))
 (p "If you need to flattens lists of lists, use "
 (code ":append") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for sublist :in '((1 2) (3 4) (5 6))
      :append sublist)"))
 (p "Compare with "
 (code "reduce") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(reduce #'append '((1 2) (3 4) (5 6)))"))
 (p
 (code ":nconc") " does the same thing destructively (faster, but mutates the sublists).
Use "
 (code ":append") " unless you're sure the sublists are freshly created and won't be
needed again.")
 (p "You can accumulate via summing with "
 (code ":sum") " or counting with "
 (code ":count") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(3 1 4 1 5 9 2 6)
      :sum n)

(loop :for n :in '(3 1 4 1 5 9 2 6)
      :count (oddp n))"))
 (p "Compare "
 (code ":sum") " with "
 (code "reduce") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(reduce #'+ '(3 1 4 1 5 9 2 6))"))
 (p "You can get the maximum or minimum values in some collection with "
 (code ":maximize") "and "
 (code ":minimize") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(3 1 4 1 5 9 2 6)
      :maximize n)

(loop :for n :in '(3 1 4 1 5 9 2 6)
      :minimize n)"))
 (p "Compare "
 (code ":count") " with "
 (code "count-if") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(count-if #'oddp '(3 1 4 1 5 9 2 6))"))))
 (section :id "conditional-clauses"
 (hgroup
 (span)
 (h2 "Conditional Clauses"))
 (div :class "outline-text-3" :id "text-1-6"
 (p "You can filter data with "
 (code "when") " and "
 (code "unless") " clauses:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(1 2 3 4 5 6 7 8 9 10)
      :when (evenp n)
        :collect n)"))
 (p "Compare with "
 (code "remove-if-not") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))"))
 (p
 (code ":unless") " is the opposite of "
 (code ":when") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(1 2 3 4 5 6 7 8 9 10)
      :unless (evenp n)
        :collect n)"))
 (p "For more complex branching, use "
 (code ":if") ", "
 (code ":else") ", "
 (code ":end") ", and "
 (code ":finally") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(1 2 3 4 5 6 7 8 9 10)
      :if (evenp n)
        :collect n :into evens
      :else
        :collect n :into odds
      :end
      :finally (return (list :evens evens :odds odds)))"))
 (p "The "
 (code ":end") " marks where the "
 (code ":if") " / "
 (code ":else") " block ends. "
 (code ":into") " collects into
a named variable instead of the loop's default return value, and "
 (code ":finally") "runs code after the loop finishes.")))
 (section :id "termination"
 (hgroup
 (span)
 (h2 "Termination"))
 (div :class "outline-text-3" :id "text-1-7"
 (p
 (code ":while") " continues looping as long as the condition is true. "
 (code ":until") " stops as
soon as the condition becomes true:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(2 4 6 7 8 10)
      :while (evenp n)
      :collect n)"))
 (p "The loop stopped at 7 because it's odd. Note that 8 and 10 are not visited at
all--"
 (code ":while") " terminates the loop immediately.")
 (p "This is useful for reading data from a file:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(with-open-file (stream #P\"some-file.txt\" :direction :input)
  (loop :for line = (read-line stream nil nil)
        :while line
        :collect line))"))
 (p "Here "
 (code ":for line \\=") " assigns the result of "
 (code "(read-line ...)") " to "
 (code "line") " each
iteration, and "
 (code ":while line") " stops when "
 (code "read-line") " returns "
 (code "nil") " (end of
file).")
 (p "You can exit a "
 (code "loop") " early with "
 (code "return") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(2 4 6 7 8 10)
      :when (oddp n)
        :do (return n))"))
 (p
 (code ":thereis") ", "
 (code ":always") ", and "
 (code ":never") " are shorthand for common patterns:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(2 4 6 8 10)
      :always (evenp n))

(loop :for n :in '(2 4 6 7 8 10)
      :always (evenp n))

(loop :for n :in '(2 4 6 7 8 10)
      :thereis (oddp n))

(loop :for n :in '(2 4 6 8 10)
      :never (oddp n))"))
 (p "Compare with the standard functions:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(every #'evenp '(2 4 6 8 10))
(some #'oddp '(2 4 6 7 8 10))
(notany #'oddp '(2 4 6 8 10))"))))
 (section :id "manually-updated-variable-bindings"
 (hgroup
 (span)
 (h2 "Manually Updated Variable Bindings"))
 (div :class "outline-text-3" :id "text-1-8"
 (p
 (code ":with") " creates a local variable that persists across iterations but is "
 (i "not") " a
loop variable. It won't step or update automatically. Here, the total is
manually incremented with "
 (code "incf") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :with total = 0
      :for n :in '(1 2 3 4 5)
      :do (incf total n)
      :finally (return total))"))
 (p "Of course, "
 (code ":sum") " would be simpler here. "
 (code ":with") " is more useful when you need
to maintain state that doesn't fit neatly into an accumulation clause:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :with prev = nil
      :for n :in '(1 1 2 3 3 3 4 5 5)
      :unless (eql n prev)
        :collect n
      :do (setf prev n))"))
 (p "This removes consecutive duplicates–something that would require a "
 (code "let") " and
manual bookkeeping with "
 (code "dolist") ".")))
 (section :id "parallel-iterating"
 (hgroup
 (span)
 (h2 "Parallel Iterating"))
 (div :class "outline-text-3" :id "text-1-9"
 (p "You can iterate over multiple things in parallel. The loop ends when the
shortest sequence runs out:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for name :in '(\"Alice\" \"Bob\" \"Charlie\")
      :for i :from 1
      :collect (format nil \"~a. ~a\" i name))"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "(\"1. Alice\" \"2. Bob\" \"3. Charlie\")")))
 (p "Compare with "
 (code "mapcar") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(let ((i 0))
  (mapcar (lambda (name)
            (incf i)
            (format nil \"~a. ~a\" i name))
          '(\"Alice\" \"Bob\" \"Charlie\")))"))
 (p "The "
 (code "loop") " version is cleaner because it has a built-in counter. No need for an
external variable.")
 (p "You can also iterate over different types of sequences in parallel:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for letter :across \"abcde\"
      :for number :in '(1 2 3 4 5)
      :collect (list letter number))"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "((#1) (#2) (#3) (#4) (#5))")))))
 (section :id "destructuring"
 (hgroup
 (span)
 (h2 "Destructuring"))
 (div :class "outline-text-3" :id "text-1-10"
 (p
 (code "loop") " can destructure list elements, pulling them apart as it iterates:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for (name age) :in '((\"Alice\" 30) (\"Bob\" 25) (\"Charlie\" 35))
      :collect (format nil \"~a is ~a years old\" name age))"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "(\"Alice is 30 years old\" \"Bob is 25 years old\" \"Charlie is 35 years old\")")))
 (p "This is particularly useful for association lists:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defparameter *people*
  '((:micah male 40 japan)
    (:takae female 35 japan)
    (:mom female 71 america)))

(loop :for (name gender age country) :in *people*
      :when (eql country 'japan)
        :collect name)"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "(:MICAH :TAKAE)")))
 (p "Without "
 (code "loop") " destructuring, you'd need "
 (code "first") ", "
 (code "second") ", "
 (code "third") ", etc.:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(mapcar #'first
        (remove-if-not (lambda (row) (eql (fourth row) 'japan))
                        *people*))"))
 (p "The "
 (code "loop") " version reads more naturally, especially as the structure gets more
complex.")
 (p "If you want to step through a plist you can combine "
 (code ":on") " with "
 (code ":by") ":")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for (key value) :on '(:name \"Micah\" :age 40 :lang \"Lisp\")
        :by #'cddr
      :do (format t \"~&~a: ~a\" key value))"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "NAME: Micah
AGE: 40
LANG: Lisp
=> NIL")))
 (p
 (code ":by") " specifies the stepping function. The default is "
 (code "#'cdr") " (move one element
forward). Using "
 (code "#'cddr") " moves two elements forward, which is exactly what you
need for key-value pairs.")))
 (section :id "the-step-binding"
 (hgroup
 (span)
 (h2 "The Step Binding"))
 (div :class "outline-text-3" :id "text-1-11"
 (p "Sometimes you need to compute a value each iteration rather than pull it from a
sequence:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for line = (read-line *standard-input* nil nil)
      :while line
      :collect line)"))
 (p "This assigns the result of the expression to "
 (code "line") " each time through. Without"
 (code ":then") ", the same expression is evaluated every iteration.")
 (p "With "
 (code ":then") ", you can specify different expressions for the first iteration and
subsequent ones:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for x = 1 :then (* x 2)
      :repeat 8
      :collect x)"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "(1 2 4 8 16 32 64 128)")))))
 (section :id "running-code-before-the-first-iteration-or-after-the-last-iteration"
 (hgroup
 (span)
 (h2 "Running Code Before The First Iteration Or After The Last Iteration"))
 (div :class "outline-text-3" :id "text-1-12"
 (p
 (code ":initially") " runs code before the first iteration. "
 (code ":finally") " runs code after
the last iteration:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(1 2 3 4 5)
      :sum n :into total
      :finally (return (/ total 5)))"))
 (p "This computes an average. "
 (code ":finally") " has access to variables created by "
 (code ":into") ",
which makes it useful for post-processing accumulated results.")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :initially (format t \"~&Starting...~%\")
      :for item :in '(a b c)
      :do (format t \"~&Processing ~a~%\" item)
      :finally (format t \"~&Done.~%\"))"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "Starting...
Processing A
Processing B
Processing C
Done.
=> NIL")))))
 (section :id "returning-from-named-loops"
 (hgroup
 (span)
 (h2 "Returning From Named Loops"))
 (div :class "outline-text-3" :id "text-1-13"
 (p "You can name a "
 (code "loop") " with "
 (code ":named") " and exit it with "
 (code "return-from") ". This is
useful for nested loops:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :named outer
      :for x :in '(1 2 3)
      :do (loop :for y :in '(10 20 30)
                :when (and (= x 2) (= y 20))
                  :do (return-from outer (list x y))))"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "(2 20)")))
 (p "Without "
 (code ":named") ", a "
 (code "return") " from the inner loop would only exit the inner
loop. "
 (code "return-from") " lets you break out of any enclosing named block.")))
 (section :id "combining-everything"
 (hgroup
 (span)
 (h2 "Combining Everything"))
 (div :class "outline-text-3" :id "text-1-14"
 (p "The real power of "
 (code "loop") " shows when you combine clauses. Here's a single pass
over a list that does several things at once:")
 (div :class "code-block"
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(loop :for n :in '(3 1 4 1 5 9 2 6 5 3 5)
      :count (oddp n) :into odd-count
      :sum n :into total
      :maximize n :into biggest
      :when (evenp n)
        :collect n :into evens
      :finally (return (list :odd-count odd-count
                             :total total
                             :biggest biggest
                             :evens evens)))"))
 (pre :class "result"
 (code :class "code-block-result"
 (span :class "code-block-result-label" "Result") "(:ODD-COUNT 8 :TOTAL 44 :BIGGEST 9 :EVENS (4 2 6))")))
 (p "Doing this without "
 (code "loop") " would require a "
 (code "let") " with four variables and a"
 (code "dolist") " with manual bookkeeping for each accumulation. The "
 (code "loop") " version
declares what you want and lets the macro handle the mechanics.")))
 (section :id "when-to-use-something-else"
 (hgroup
 (span)
 (h2 "When to Use Something Else"))
 (div :class "outline-text-3" :id "text-1-15"
 (p
 (code "loop") " is not always the right tool.")
 (p "For a simple transformation of each element, "
 (code "mapcar") " is more concise:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(mapcar #'1+ '(1 2 3))
(loop :for n :in '(1 2 3) :collect (1+ n))"))
 (p "For a simple predicate test, "
 (code "every") ", "
 (code "some") ", "
 (code "notany") " are clearer:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(every #'evenp numbers)
(loop :for n :in numbers :always (evenp n))"))
 (p "For reducing to a single value with a known function, "
 (code "reduce") " says it directly:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(reduce #'+ numbers)
(loop :for n :in numbers :sum n)"))
 (p "For a simple side effect on each element, "
 (code "dolist") " is fine:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(dolist (item items) (print item))
(loop :for item :in items :do (print item))"))
 (p "Use "
 (code "loop") " when the iteration is complex enough that you'd otherwise need to
combine multiple constructs, maintain manual state, or make multiple passes over
the data. If a simpler construct expresses your intent clearly, prefer it.")))
 (section :id "a-note-on-style-keywords-vs-symbols"
 (hgroup
 (span)
 (h2 "A Note on Style: Keywords vs Symbols"))
 (div :class "outline-text-3" :id "text-1-16"
 (p "You'll see "
 (code "loop") " written two ways:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" ";; With keywords (this book's style):
(loop :for i :from 0 :below 5 :collect i)

;; With plain symbols:
(loop for i from 0 below 5 collect i)"))
 (p "Both are valid. The keyword style (with colons) makes the loop clauses visually
distinct from variables and expressions. This helps make inner variables (like"
 (code "i") " above) distinct and easy to find. Prefer the keyword style. "))))))
 (button :class "toc-viewer" "TOC")
 (script "HighlightLisp.highlight_auto();"))