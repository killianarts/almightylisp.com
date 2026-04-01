
(body
  (div :class "sidebar-container"
    (div :class "book-navigation"
      (h1 :class "book-navigation__book-title" "Almighty Lisp: The Essentials")
      (p :class "book-navigation__chapter-name-heading"
        (span "Almighty Lisp: The Essentials"))
      (nav
        (ul
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#syntax-grammar"
              (span :class "book-navigation__section-name" "SYNTAX & GRAMMAR")
              (span :class "book-navigation__section-number" "1.1.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#s-expressions"
              (span :class "book-navigation__section-name" "S-expressions")
              (span :class "book-navigation__section-number" "1.2.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#prefix-notation"
              (span :class "book-navigation__section-name" "Prefix Notation")
              (span :class "book-navigation__section-number" "1.3.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#more-complicated-s-expression"
              (span :class "book-navigation__section-name" "More complicated s-expression")
              (span :class "book-navigation__section-number" "1.4.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#symbols"
              (span :class "book-navigation__section-name" "SYMBOLS")
              (span :class "book-navigation__section-number" "1.5.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#introducing-global-variables"
              (span :class "book-navigation__section-name" "Introducing Global Variables")
              (span :class "book-navigation__section-number" "1.6.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#introducing-local-variables"
              (span :class "book-navigation__section-name" "Introducing Local Variables")
              (span :class "book-navigation__section-number" "1.7.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#more-than-just-variables"
              (span :class "book-navigation__section-name" "More than just variables")
              (span :class "book-navigation__section-number" "1.8.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#functions"
              (span :class "book-navigation__section-name" "FUNCTIONS")
              (span :class "book-navigation__section-number" "1.9.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#parameters"
              (span :class "book-navigation__section-name" "Parameters")
              (span :class "book-navigation__section-number" "1.10.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#return-values"
              (span :class "book-navigation__section-name" "Return Values")
              (span :class "book-navigation__section-number" "1.11.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#returning-multiple-values"
              (span :class "book-navigation__section-name" "Returning Multiple Values")
              (span :class "book-navigation__section-number" "1.12.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#binding-multiple-values"
              (span :class "book-navigation__section-name" "Binding Multiple Values")
              (span :class "book-navigation__section-number" "1.13.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#breaking-lists-into-multiple-values"
              (span :class "book-navigation__section-name" "Breaking Lists Into Multiple Values")
              (span :class "book-navigation__section-number" "1.14.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#pass-by-value"
              (span :class "book-navigation__section-name" "Pass by Value")
              (span :class "book-navigation__section-number" "1.15.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#first-class-functions"
              (span :class "book-navigation__section-name" "First-Class Functions")
              (span :class "book-navigation__section-number" "1.16.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#anonymous-functions"
              (span :class "book-navigation__section-name" "Anonymous Functions")
              (span :class "book-navigation__section-number" "1.17.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#lists"
              (span :class "book-navigation__section-name" "LISTS")
              (span :class "book-navigation__section-number" "1.18.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#basic-list-functions"
              (span :class "book-navigation__section-name" "Basic List Functions")
              (span :class "book-navigation__section-number" "1.19.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#lists-as-trees"
              (span :class "book-navigation__section-name" "Lists as Trees")
              (span :class "book-navigation__section-number" "1.20.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#lists-as-tables"
              (span :class "book-navigation__section-name" "Lists as Tables")
              (span :class "book-navigation__section-number" "1.21.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#lists-as-sets"
              (span :class "book-navigation__section-name" "Lists as Sets")
              (span :class "book-navigation__section-number" "1.22.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#control-flow"
              (span :class "book-navigation__section-name" "CONTROL FLOW")
              (span :class "book-navigation__section-number" "1.23.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#general-equality"
              (span :class "book-navigation__section-name" "General Equality")
              (span :class "book-navigation__section-number" "1.24.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#logical-operators"
              (span :class "book-navigation__section-name" "Logical Operators")
              (span :class "book-navigation__section-number" "1.25.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#conditional-forms"
              (span :class "book-navigation__section-name" "Conditional Forms")
              (span :class "book-navigation__section-number" "1.26.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#iteration"
              (span :class "book-navigation__section-name" "ITERATION")
              (span :class "book-navigation__section-number" "1.27.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#iterating-by-mapping"
              (span :class "book-navigation__section-name" "Iterating by Mapping")
              (span :class "book-navigation__section-number" "1.28.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#iterating-by-reducing"
              (span :class "book-navigation__section-name" "Iterating by Reducing")
              (span :class "book-navigation__section-number" "1.29.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#iterating-by-filtering"
              (span :class "book-navigation__section-name" "Iterating by Filtering")
              (span :class "book-navigation__section-number" "1.30.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#iterating-by-doing"
              (span :class "book-navigation__section-name" "Iterating by Doing")
              (span :class "book-navigation__section-number" "1.31.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-dotimes-"
              (span :class "book-navigation__section-name" "=dotimes=")
              (span :class "book-navigation__section-number" "1.32.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-dolist-"
              (span :class "book-navigation__section-name" "=dolist=")
              (span :class "book-navigation__section-number" "1.33.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-do-"
              (span :class "book-navigation__section-name" "=do=")
              (span :class "book-navigation__section-number" "1.34.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#iterating-by-looping"
              (span :class "book-navigation__section-name" "Iterating by Looping")
              (span :class "book-navigation__section-number" "1.35.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#predicate-iterators"
              (span :class "book-navigation__section-name" "Predicate Iterators")
              (span :class "book-navigation__section-number" "1.36.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#early-returns"
              (span :class "book-navigation__section-name" "Early Returns")
              (span :class "book-navigation__section-number" "1.37.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#strings-i-o"
              (span :class "book-navigation__section-name" "STRINGS & I/O")
              (span :class "book-navigation__section-number" "1.38.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#printing-information-to-repl"
              (span :class "book-navigation__section-name" "Printing Information to REPL")
              (span :class "book-navigation__section-number" "1.39.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#using-sequence-operations-on-strings"
              (span :class "book-navigation__section-name" "Using Sequence Operations on Strings")
              (span :class "book-navigation__section-number" "1.40.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-concatenate-"
              (span :class "book-navigation__section-name" "=concatenate=")
              (span :class "book-navigation__section-number" "1.41.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-length-"
              (span :class "book-navigation__section-name" "=length=")
              (span :class "book-navigation__section-number" "1.42.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-reverse-"
              (span :class "book-navigation__section-name" "=reverse=")
              (span :class "book-navigation__section-number" "1.43.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-map-"
              (span :class "book-navigation__section-name" "=map=")
              (span :class "book-navigation__section-number" "1.44.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#string-specific-operations"
              (span :class "book-navigation__section-name" "String Specific Operations")
              (span :class "book-navigation__section-number" "1.45.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#string-modification"
              (span :class "book-navigation__section-name" "String Modification")
              (span :class "book-navigation__section-number" "1.46.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#string-comparison-operators"
              (span :class "book-navigation__section-name" "String Comparison Operators")
              (span :class "book-navigation__section-number" "1.47.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#streams"
              (span :class "book-navigation__section-name" "Streams")
              (span :class "book-navigation__section-number" "1.48.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#writing-files"
              (span :class "book-navigation__section-name" "Writing Files")
              (span :class "book-navigation__section-number" "1.49.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#reading-files"
              (span :class "book-navigation__section-name" "Reading Files")
              (span :class "book-navigation__section-number" "1.50.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#beyond-the-basics-w-files"
              (span :class "book-navigation__section-name" "Beyond the Basics w/Files")
              (span :class "book-navigation__section-number" "1.51.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-format-"
              (span :class "book-navigation__section-name" "=format=")
              (span :class "book-navigation__section-number" "1.52.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#stream-output"
              (span :class "book-navigation__section-name" "Stream Output")
              (span :class "book-navigation__section-number" "1.53.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#the-almighty-tilde"
              (span :class "book-navigation__section-name" "The Almighty Tilde")
              (span :class "book-navigation__section-number" "1.54.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-tilde-a-"
              (span :class "book-navigation__section-name" "=Tilde a=")
              (span :class "book-navigation__section-number" "1.55.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-tilde-and-tilde-"
              (span :class "book-navigation__section-name" "=Tilde &= and =Tilde %=")
              (span :class "book-navigation__section-number" "1.56.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#format-directive-cheat-sheet"
              (span :class "book-navigation__section-name" "Format Directive Cheat Sheet")
              (span :class "book-navigation__section-number" "1.57.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#-tilde-d-"
              (span :class "book-navigation__section-name" "=TILDE D=")
              (span :class "book-navigation__section-number" "1.58.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#tic-tac-toe"
              (span :class "book-navigation__section-name" "Tic-tac-toe")
              (span :class "book-navigation__section-number" "1.59.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#choose-data-representation"
              (span :class "book-navigation__section-name" "Choose data representation")
              (span :class "book-navigation__section-number" "1.60.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#write-functions-for-manipulating-data"
              (span :class "book-navigation__section-name" "Write functions for manipulating data")
              (span :class "book-navigation__section-number" "1.61.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#write-game-logic"
              (span :class "book-navigation__section-name" "Write game logic")
              (span :class "book-navigation__section-number" "1.62.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#representing-data-to-the-player"
              (span :class "book-navigation__section-name" "Representing data to the player")
              (span :class "book-navigation__section-number" "1.63.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#getting-user-input"
              (span :class "book-navigation__section-name" "Getting user input")
              (span :class "book-navigation__section-number" "1.64.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#write-computer-moving-logic"
              (span :class "book-navigation__section-name" "Write computer moving logic")
              (span :class "book-navigation__section-number" "1.65.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#choose-a-strategy"
              (span :class "book-navigation__section-name" "Choose a strategy")
              (span :class "book-navigation__section-number" "1.66.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#finding-a-winning-move"
              (span :class "book-navigation__section-name" "Finding a winning move")
              (span :class "book-navigation__section-number" "1.67.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#adding-strategies"
              (span :class "book-navigation__section-name" "Adding strategies")
              (span :class "book-navigation__section-number" "1.68.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#fixing-a-bug"
              (span :class "book-navigation__section-name" "Fixing a bug")
              (span :class "book-navigation__section-number" "1.69.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#add-computer-strategies"
              (span :class "book-navigation__section-name" "Add computer strategies")
              (span :class "book-navigation__section-number" "1.70.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#beyond-triplets"
              (span :class "book-navigation__section-name" "Beyond triplets")
              (span :class "book-navigation__section-number" "1.71.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#detecting-a-squeeze"
              (span :class "book-navigation__section-name" "Detecting a squeeze")
              (span :class "book-navigation__section-number" "1.72.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#detecting-a-two-on-one"
              (span :class "book-navigation__section-name" "Detecting a two-on-one")
              (span :class "book-navigation__section-number" "1.73.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#updating-choose-move-"
              (span :class "book-navigation__section-name" "Updating =choose-move=")
              (span :class "book-navigation__section-number" "1.74.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#summary"
              (span :class "book-navigation__section-name" "Summary")
              (span :class "book-navigation__section-number" "1.75.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#matters-of-style"
              (span :class "book-navigation__section-name" "MATTERS OF STYLE")
              (span :class "book-navigation__section-number" "1.76.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#reading-lisp-code"
              (span :class "book-navigation__section-name" "Reading Lisp Code")
              (span :class "book-navigation__section-number" "1.77.0")))
          (li :class "book-navigation__section-link-container"
            (a :class "book-navigation__section-link" :href "#good-code-communication"
              (span :class "book-navigation__section-name" "Good Code Communication")
              (span :class "book-navigation__section-number" "1.78.0")))))
      (div :class "book-navigation__previous-and-next")))
  (main :class "sidebar-main"
    (article :class "book"
      (section :id "lisp-fundamentals"
        (hgroup
          (span)
          (h1 "LISP FUNDAMENTALS"))
        (div :class "outline-text-2" :id "text-1"
          (p "Now that you've gotten your feet wet with installing and using Doom Emacs, we
can get to work on learning Common Lisp.")
          (p "Lisp comes from a lineage of functional/applicative programming. That means that
among the fundamentals we won't be seeing object-oriented programming
stuff–that comes later. Perhaps the most important section is the one on Lists.
They are the core data and "
            (i "code") " structure primitive.")
          (p "To get some practice writing Lisp in Emacs you'll make a game of tic-tac-toe.
Nothing fancy, something just interesting enough to let you play with some of
the core concepts."))
        (section :id "syntax-grammar"
          (hgroup
            (span)
            (h2 "SYNTAX & GRAMMAR"))
          (div :class "outline-text-3" :id "text-1-1"
            (p "Common Lisp's syntax and grammar are very simple. It has two distinctive
features: all those parentheses, and prefix notation."))
          (section :id "s-expressions"
            (hgroup
              (span)
              (h3 "S-expressions"))
            (div :class "outline-text-4" :id "text-1-1-1"
              (p "This is a Common Lisp "
                (code "s-expression") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(+ 3 4)"))
              (p "S-expressions are made of indivisible units called "
                (code "atoms") "–such as the symbol"
                (code "+") " and the number "
                (code "3") "–and divisible units called "
                (code "lists") ". They are also called"
                (code "forms") ".")
              (p "Atoms evaluate to themselves.")
              (p "The first atom in a list is evaluated as a function name. The rest of the
arguments are evaluated before being passed to the function.")
              (p "Symbols, other than the first one in a list, are evaluated as variables.")
              (p "Some forms have special evaluation rules and are called special forms.")
              (p "All "
                (i "inner") " forms are first evaluated left-to-right before being passed to a
function.")))
          (section :id "prefix-notation"
            (hgroup
              (span)
              (h3 "Prefix Notation"))
            (div :class "outline-text-4" :id "text-1-1-2"
              (p "Math operations in Common Lisp are probably different than what you're used to.")
              (p "Common Lisp math operations use "
                (code "prefix notation") ": the math operator is a
function that comes at the beginning of the parenthesis, and all numbers
afterward evaluated using that operator, from left to right. In school, we learn
math using "
                (code "infix notation") ", where the operators are placed between each number.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(+ (* 5 5 5) (/ 18 2) (- 20 3)) ; using infix notation: 5 * 5 * 5 + 18 / 2 + 20
                                ; - 3"))))
          (section :id "more-complicated-s-expression"
            (hgroup
              (span)
              (h3 "More complicated s-expression"))
            (div :class "outline-text-4" :id "text-1-1-3"
              (p "Let's look at a slightly more complicated example of Common Lisp syntax:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun cube (x)
  (* x x x))"))
              (p "After the opening parenthesis comes "
                (code "defun") ", a built-in "
                (code "special form") ". Special
forms have different evaluation rules compared to regular function calls."
                (code "defun") " is used to define a function. The function above is given the name"
                (code "cube") ". After giving the function a name, we define it's "
                (code "parameters") ", the data
it takes as "
                (code "arguments") " to run the operations inside the function. The function
takes one argument: "
                (code "x") ".")
              (p "Notice that "
                (code "cube") " isn't evaluated as a variable. The "
                (code "x") " in "
                (code "(x)") " isn't
initially interpreted as a function to be called.")
              (p
                (code "defun") " is a special operator where the first argument it receives is a symbol
designating the name to give the function, and the second argument is a list of
arguments that the function is expected to receive when it's called. The forms
that follow afterward are called the "
                (code "body") " of the function.")
              (p "There are several other special forms with their own syntax. We'll cover them
later."))))
        (section :id "symbols"
          (hgroup
            (span)
            (h2 "SYMBOLS"))
          (div :class "outline-text-3" :id "text-1-2"
            (p "Symbols are atom units that represent and point to some other data. ")
            (p "Under normal use, symbols in Common Lisp are case-insensitive. "
              (code "my-symbol") " and"
              (code "MY-SYMBOL") " and "
              (code "mY-sYmBoL") " are all the same.")
            (p "By calling "
              (code "quote") " or using the reader-macro "
              (code "'") ", you can return the names of
the symbols, rather than evaluating and returning the values of the symbols.")
            (div :class "code-block"
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "'my-num"))
              (pre :class "result"
                (code :class "code-block-result"
                  (span :class "code-block-result-label" "Result") "MY-NUM")))
            (p "Quoting things in general is likely to be both a little confusing but also very
important later, especially when you learn macros."))
          (section :id "introducing-global-variables"
            (hgroup
              (span)
              (h3 "Introducing Global Variables"))
            (div :class "outline-text-4" :id "text-1-2-1"
              (p "Common Lisp supports both global variables, but also lexically scope variables.
There are several ways to define variables.")
              (p "You can use "
                (code "setf") " to reassign values to variables you've already introduced.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(setf *debug* nil)"))
              (p "However, if this variable hasn't been introduced yet, you'll get an error.")
              (p "Use "
                (code "defvar") " to introduce a global variable without a value.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defvar *debug*)"))
              (p "To change the value of a "
                (code "defvar") " variable, you need to call "
                (code "setf") " on the
variable. If you do decide that you want to add a value, or change the initial
value, you can't simply redefine the "
                (code "defvar") " and recompile the form.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defvar *debug* t)
*debug*"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "NIL")))
              (p "Still "
                (code "nil") ".")
              (p "You have to use "
                (code "setf") " to modify the value of a "
                (code "defvar") " variable.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defvar *no-changey* 'no)
*no-changey*"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "YES")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(setf *no-changey* 'yes)
*no-changey*"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "YES")))
              (p "Use "
                (code "defparameter") " to introduce a global variable with a starting value that can
be modified by recompiling the "
                (code "defparameter") " form.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defparameter *debug* nil)
*debug*"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "NIL")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defparameter *debug* t)
*debug*"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "T")))
              (p "Global variables defined with "
                (code "defvar") " and "
                (code "defparameter") " are surrounded with"
                (code "*") " by convention.")
              (p "Use "
                (code "defconstant") " to introduce a global variable that has a value that can't be
modified.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defconstant +pi+ 3.14)
+pi+"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "3.14")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(setf +pi+ 42)"))
                (pre :class "code-block-result-pre result error"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "\\+PI\\+ is a constant and thus can't be set.")))
              (p "If you try to recompile the "
                (code "defconstant") " with a different value, you'll be
given the option to redefine the constant via a restart.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defconstant +pi+ 42)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "The constant \\+PI\\+ is being redefined (from 3.14 to 42)")))
              (p "Constant variable names are surrounded with "
                (code "+") " by convention.")))
          (section :id "introducing-local-variables"
            (hgroup
              (span)
              (h3 "Introducing Local Variables"))
            (div :class "outline-text-4" :id "text-1-2-2"
              (p "Use "
                (code "let") " to introduce or reassign a local variable.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(let ((name 'micah))
  name)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "MICAH")))
              (aside :class "book-aside"
                (p
                  (code "let") " was initially confusing to me because of how it creates a block. It feels
heavier than a typical "
                  (code "let var = \"some value\"") " as in other languages. One
upside to this increased weight of variable binding is that it give an
indication of how complex the operation really is via the indentation and width
of the code."))
              (p "You can also reassign a global variable temporarily.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defparameter *var* 'outside)
*var*                   "))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "OUTSIDE")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(let ((*var* 'inside))  
  *var*)              "))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "INSIDE")))
              (p "And the value outside the let will remain.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "*var*"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "OUTSIDE")))
              (p "If you need to use a variable defined within a "
                (code "let")
                (b "within the same "
                  (code "let") "form") " then you need "
                (code "let*") ".")
              (p "This "
                (code "let") " form will break:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(let ((data 'data)
      (clean-data data))
  clean-data)"))
                (pre :class "code-block-result-pre result error"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "The variable DATA is unbound.")))
              (p "Change the "
                (code "let") " to "
                (code "let*") " to make it work properly:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(let* ((data 'data)
       (clean-data data))
  clean-data)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "DATA")))))
          (section :id "more-than-just-variables"
            (hgroup
              (span)
              (h3 "More than just variables"))
            (div :class "outline-text-4" :id "text-1-2-3"
              (p "While variables are an important kind of symbol, they aren't the only kind of
symbol. Function names, package names, class names, etc. are all symbols.
Symbols themselves are a data structure and can hold more than one piece of
information.")
              (p "There are several functions for getting at the data saved in a symbol. ")
              (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                (colgroup
                  (col :class "org-left")
                  (col :class "org-left"))
                (thead
                  (tr
                    (th :scope "col" :class "org-left" "Function")
                    (th :scope "col" :class "org-left" "Return Value")))
                (tbody
                  (tr
                    (td :class "org-left" "symbol-function")
                    (td :class "org-left" "The function object saved in the symbol."))
                  (tr
                    (td :class "org-left" "symbol-name")
                    (td :class "org-left" "An uppercase string with the name of the symbol."))
                  (tr
                    (td :class "org-left" "symbol-package")
                    (td :class "org-left" "An instance of the package object where the symbol is interned."))
                  (tr
                    (td :class "org-left" "symbol-value")
                    (td :class "org-left" "The value saved to a symbol as a variable."))
                  (tr
                    (td :class "org-left" "symbol-plist")
                    (td :class "org-left" "The property-list saved to the symbol."))))
              (p "While most of the time you'll use "
                (code "defparameter") ", "
                (code "let") ", "
                (code "defun") ", etc. to define
symbols, sometimes you'll need to do symbol surgery, constructing and interning
symbols in manually. Some of the above functions will come in handy for those
kinds of operations.")
              (div :class "matters_of_style" :id "orge0838ac"
                (p "Now that you know that "
                  (code "let") " and "
                  (code "let*") " are nearly the same, with "
                  (code "let*") "providing more functionality that "
                  (code "let") ", you are probably tempted to never use"
                  (code "let") ". However, it is good style to "
                  (b "be specific") ". If you don't need to refer to
variables bound earlier in the "
                  (code "let") " form, don't use "
                  (code "let*") ". This prevents
readers of your code from being confused.")))))
        (section :id "functions"
          (hgroup
            (span)
            (h2 "FUNCTIONS"))
          (div :class "outline-text-3" :id "text-1-3"
            (p "On the subject of functions: Functions describe the world. Or at least that's
what some insane Haskell-loving university teacher once told me.")
            (p "You can define functions with "
              (code "defun") ":")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(defun sum (x y) 
  (+ x y))"))
            (p "All functions defined with "
              (code "defun") " are global in scope. For example,"
              (code "compile-food-ingredients") " will not be lexically scoped to "
              (code "categorize-food") ", it
will be available at the top level.")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(defun categorize-food (food)
  (defun compile-food-ingredients (food) ...)
  ...)"))
            (p "To define locally-scoped named functions, use "
              (code "flet") " or "
              (code "labels") ":")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(defun squirt-then-double (x)
  (flet ((squirt (y)
           (* y y)))
    (* 2 (squirt x))))

(defun squirt-then-double-with-labels (x)
  (labels ((squirt (y)        
             (* y y))
           (double-squirted ()
             (* 2 (squirt x))))
    (double-squirted x)))"))
            (p
              (code "flet") " is like "
              (code "let") ", and "
              (code "labels") " is like "
              (code "let*") ". Importantly, this means if
you want to do anything "
              (i "recursive") " you need to use "
              (code "labels"))
            (p "The second parameter to "
              (code "defun") " is called a "
              (code "lambda list") ". Parameters for the
function being defined are specified inside the lambda list."))
          (section :id "parameters"
            (hgroup
              (span)
              (h3 "Parameters"))
            (div :class "outline-text-4" :id "text-1-3-1"
              (p "There are several different kinds of parameters you can define. The most common
will be "
                (b "required") " parameters. Parameters defined without any other options are required.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun my-fun (this-is-required))"))
              (p "If you don't want a parameter to be required, you can use the "
                (code "&optional") "lambda list keyword to make it optional.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun fun-with-optional (a &optional b)
  (format nil \"The values passed: ~a and ~a.~&\" a b))

(fun-with-optional 5)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "The values passed: 5 and NIL.")))
              (p "The default value for optional arguments not passed is "
                (code "nil") ". You can set the
default value.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun fun-with-optional (a &optional (b 10))
  (format nil \"The values passed: ~a and ~a.~&\" a b))

(fun-with-optional 5)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "The values passed: 5 and 10.")))
              (p "You can create a predicate that will return whether the value was supplied or
the default is being used.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun fun-with-optional (a &optional (b 10 b-supplied-p))
  (if b-supplied-p
      (format nil \"The values passed: ~a and ~a (passed by user).~&\" a b)
      (format nil \"The values passed: ~a and ~a (default).~&\" a b)))"))
              (p "By convention the predicate is named "
                (code "*-supplied-p") ", with "
                (code "*") " being the name of
the parameter. It's value is "
                (code "t") " if the user supplied an argument in that
position, otherwise it defaults to "
                (code "nil") ".")
              (p "Using the default value of the optional argument:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(fun-with-optional 5) "))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "The values passed: 5 and 10 (default).")))
              (p "With a value passed to the optional argument position:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(fun-with-optional 5 10)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "The values passed: 5 and 10 (passed by user).")))
              (p "All parameters following the "
                (code "&optional") " lambda list keyword will be optional.
This is true of all lambda list keywords.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun fun-with-lots-of-options (&optional (a 1) (b 2) (c 3))
  (+ a b c))"))
              (p "If you want an argument to be both "
                (b "optional") ", but also want the user to be able
to set the argument in any position by name, then you can use the "
                (code "&key") " keyword.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun fun-with-keys (&key a b c)
  (+ a b c))

(fun-with-keys :a 1 :b 2 :c 3)         "))
              (p "As with "
                (code "&optional") ", you can specify default values.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun fun-with-keys (&key (a 1) (b 2) (c 3))
  (+ a b c))"))
              (p
                (code "*-supplied-p") " predicates can also be specified.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun fun-with-keys (&key (a 1 a-supplied-p) (b 2 b-supplied-p) (c 3 c-supplied-p))
  (format nil \"~a (~a) + ~a (~a) + ~a (~a) = ~a~&\"
          a a-supplied-p
          b b-supplied-p
          c c-supplied-p
          (+ a b c)))

(fun-with-keys :a 5 :c 7)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "5 (T) + 2 (NIL) + 7 (T) = 14")))
              (p "You can collect arbitrary arguments into a list with "
                (code "&rest") ".")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun add (&rest args)
  (apply #'+ args))

(add 1 2 3 4 5)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "15")))
              (p "It is possible to mix the different kinds of parameters.")
              (p "As a general rule, you don't want to mix "
                (code "&optional") " and "
                (code "&key") " parameters.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun do-not-mix (&optional (a 1) (b 2) &key (c 3))
  (+ a b c))

(do-not-mix :c 5)                       "))
                (pre :class "code-block-result-pre result error"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "The value
  :C
is not of type
  NUMBER")))
              (p "Use one or the other, but not both.")
              (p "You can mix "
                (code "&optional") " and "
                (code "&rest") ", but mixing "
                (code "&key") " and "
                (code "&rest") " will also
result in an error. You don't need the "
                (code "&key") " to have named arguments in your"
                (code "&rest") " args anyway.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun rest-args-as-plist (&rest args)
  args)

(rest-args-as-plist :a 1 :b 2 :name 'micah)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(:A 1 :B 2 :NAME MICAH)")))
              (p "We'll see a bit later what's going on in "
                (code "loop") " below, but this is just to say
that you can have an arbitrary number of key-value pairs in your "
                (code "&rest") " argument.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(let ((args (rest-args-as-plist :a 1 :b 2 :name 'micah)))
  (loop :for (k v) :on args :by #'cddr
        :when (eql k :name)
          :append v))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "MICAH")))))
          (section :id "return-values"
            (hgroup
              (span)
              (h3 "Return Values"))
            (div :class "outline-text-4" :id "text-1-3-2"
              (p "In Common Lisp, the value returned by the last expression called in a form will
be the return value.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defvar *some-var*)
(defun do-a-bunch-of-stuff ()
  (setf *some-var* \"I assigned a value to *some-var*.\")
  (let ((num 5))
    (squirt-then-double num)
    (random 10)
    \"I am the value that this function will return\"))
(do-a-bunch-of-stuff)"))
              (p "There are cases, however, when you might want to do an early return, especially
when looping. You can do that with "
                (code "return") ".")))
          (section :id "returning-multiple-values"
            (hgroup
              (span)
              (h3 "Returning Multiple Values"))
            (div :class "outline-text-4" :id "text-1-3-3"
              (p "It's possible to return multiple values using the "
                (code "values") " function.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun return-a-bunch-of-stuff ()
  (values
   (+ 2 7)
   (* 7 7)
   (/ 100 25)))
(return-a-bunch-of-stuff)
                                        ; => 9, 49, 4"))))
          (section :id "binding-multiple-values"
            (hgroup
              (span)
              (h3 "Binding Multiple Values"))
            (div :class "outline-text-4" :id "text-1-3-4"
              (p "A simple "
                (code "let") " won't bind all of the values returned by a function that returns
multiple values. Instead, only the first value will be bound.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(let ((val (return-a-bunch-of-stuff)))
  val)
                                        ; => 9 (4 bits, #x9, #o11, #b1001)"))
              (p "If you need to bind multiple values, use "
                (code "multiple-value-bind") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(multiple-value-bind (a b c)
    (return-a-bunch-of-stuff)
  (format t \"~a * ~a * ~a = ~a\" a b c (* a b c)))
                                        ; 9 * 49 * 4 = 1764 => NIL"))))
          (section :id "breaking-lists-into-multiple-values"
            (hgroup
              (span)
              (h3 "Breaking Lists Into Multiple Values"))
            (div :class "outline-text-4" :id "text-1-3-5"
              (p "Sometimes you might want to take a list and break it into pieces–called
destructuring. For that, there's "
                (code "destructuring-bind") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(destructuring-bind (a b c)
    (list 1 2 3)
  (format t \"~a * ~a * ~a = ~a\" a b c (* a b c)))
                                        ; 1 * 2 * 3 = 6 => NIL"))
              (p "This destructuring can be done on arbitrarily deep trees of cons cells.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *deep-tree* `(defun function-name (a lambda-list)
                            (let ((b :some-value))
                              b)))

(destructuring-bind (the-defun the-function-name (the-a the-lambda-list)
                     (the-let ((the-b the-value-bound-to-b))
                      the-b-at-the-end))
    *deep-tree*
  the-value-bound-to-b)
                                        ; => :SOME-VALUE"))))
          (section :id "pass-by-value"
            (hgroup
              (span)
              (h3 "Pass by Value"))
            (div :class "outline-text-4" :id "text-1-3-6"
              (p "When a variable is passed to a function, a new, local variable is introduce with
the value of the variable passed to the function.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defvar *some-num* 5)
(defun add-5 (num) ; local variable num is introduced
  (setf num (+ num 5)))

;; num is assigned the value of *some-num*
(add-5 *some-num*) ; => 10
;; add-5 does not modify *some-num*
*some-num* ; => 5, not 10"))
              (p "If you want to modify the value of the global variable, you need to use "
                (code "setf") "on the global variable directly.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun add-5 ()
  (setf *some-num* (+ *some-num* 5)))
(add-5)
some-num ; => 10"))
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *names* '(Micah Greg Takae Marcia Ena Mikasa))

(defun add-name (name sequence)
  (setf sequence (push name sequence)))
(add-name 'Guy *names*)"))))
          (section :id "first-class-functions"
            (hgroup
              (span)
              (h3 "First-Class Functions"))
            (div :class "outline-text-4" :id "text-1-3-7"
              (p "Common Lisp functions are first-class. Many of Lisp's functions can take
functions as arguments.")
              (p "To pass a function as an argument, you have a few options.")
              (p "You can use "
                (code "function") ":")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(mapcar (function squirt-then-double) '(1 2 3 4 5))"))
              (p "Or, more commonly, use the reader macro "
                (code "#'") ":")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(mapcar #'squirt-then-double '(1 2 3 4 5))"))
              (p "Or you can use anonymous functions.")))
          (section :id "anonymous-functions"
            (hgroup
              (span)
              (h3 "Anonymous Functions"))
            (div :class "outline-text-4" :id "text-1-3-8"
              (p "You can use anonymous functions with "
                (code "lambda") ":")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "((lambda (x) (* x x)) 5) ; => 25"))
              (p "Lambdas are useful when using functional programming functions like "
                (code "mapcar") " or"
                (code "remove-if-not") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(mapcar #'(lambda (x) (+ (* x x) (* x x))) '(1 2 3 4 5)) ; => (2 8 18 32 50)")))))
        (section :id "lists"
          (hgroup
            (span)
            (h2 "LISTS"))
          (div :class "outline-text-3" :id "text-1-4"
            (p "Lists are the most flexible and fundamental data structure in Common Lisp.")
            (p "The easiest way to make a list is like this:")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(list 'this 'is 'a 'list)"))
            (p "That list contains four symbols. The more common way to create the above list is
to use "
              (code "quote") ":")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(quote (this is a list))"))
            (p "Or, using a reader macro:")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "'(this is a list)"))
            (p "If you are experienced in other languages like Python or JavaScript, you might
think Lisp's lists are the same as in those languages. However, that isn't the
case.")
            (p "The more fundamental data structure that lists are built on top of are cons
cells. Lisp's lists are "
              (b "linked lists") " of cons cells.")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(cons 'this (cons 'is (cons 'a (cons 'linked (cons 'list nil)))))"))
            (p "A cons cell has two parts or slots: a "
              (code "car") " and a "
              (code "cdr") ". The car contains some
data, and the cdr contains either more cons cells or "
              (code "nil") ". "
              (code "nil") " terminates the
linked list branch.")
            (p "More importantly, and maybe confusingly, Common Lisp code is written using these
very same cons cells.")
            (div :class "code-block"
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" ";; Parenthesis, then function name \"cons\", then data, all establishing a nested
;; linked list.
(cons 'defun (cons 'sum (cons (cons 'x (cons 'y nil))
                              (cons
                               (cons '+
                                (cons 'x (cons 'y nil)))
                               nil))))"))
              (pre :class "result"
                (code :class "code-block-result"
                  (span :class "code-block-result-label" "Result") "(DEFUN SUM (X Y) (+ X Y))")))
            (p "The result is the literal representation of this code:")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(defun sum (x y)
  (+ x y))"))
            (p "Quoting the "
              (code "sum") " function definition will produce the same result")
            (div :class "code-block"
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(quote (defun sum (x y)
         (+ x y)))"))
              (pre :class "result"
                (code :class "code-block-result"
                  (span :class "code-block-result-label" "Result") "(DEFUN SUM (X Y) (+ X Y))")))
            (p "Importantly, that's different from merely returning a string with the code:")
            (div :class "code-block"
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "\"(defun sum (x y)
   (+ x y))\""))
              (pre :class "result"
                (code :class "code-block-result"
                  (span :class "code-block-result-label" "Result") "\"(defun sum (x y)
   (+ x y))\"")))
            (p "How is it different? Because I can evaluate the quoted code, and I can evaluate
the version manually created with "
              (code "cons") ":")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(eval (quote (defun sum (x y)
               (+ x y))))

(eval
 (cons 'defun (cons 'sum (cons (cons 'x (cons 'y nil))
                               (cons
                                (cons '+
                                      (cons 'x (cons 'y nil)))
                                nil)))))"))
            (p "The line between \"code\" and \"data\" that is clearly drawn in other languages like
Python or JavaScript "
              (i "does not exist") " in Lisp, owing to the fact that code and
data both share the same syntax and data structure."))
          (section :id "basic-list-functions"
            (hgroup
              (span)
              (h3 "Basic List Functions"))
            (div :class "outline-text-4" :id "text-1-4-1"
              (p "Because lists are so fundamental in Lisp, there are many functions for
manipulating lists.")
              (p
                (code "length") " returns how many items are in the list:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(length '(Gerald Sussman stole my wife and kicked my dog that skallywag))"))
              (p
                (code "reverse") " returns a new list that has the order of the elements of the initial list reversed:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(reverse '(1 2 3 4 5))"))
              (p
                (code "first") ", "
                (code "second") " … "
                (code "tenth") " get items in a list based on their position in the
list:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(let ((my-list '(common lisp is a general purpose multi-paradigm programming
                 language)))
  (list (first my-list)
        (second my-list)
        (ninth my-list)))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(COMMON LISP LANGUAGE)")))
              (p
                (code "member") " searches for a single item in a list:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(member 'general '(common lisp is a general purpose multi-paradigm programming
 language))"))
              (p
                (code "nth") " selects individual items by their index:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(nth 4 '(common lisp is a general purpose multi-paradigm programming language))"))
              (p
                (code "position") " tells you the index of an item in a list:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(position 'general '(common lisp is a general purpose multi-paradigm programming language))"))
              (p
                (code "append") " combines two lists:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(append '(this list is a list) '(a list))"))))
          (section :id "lists-as-trees"
            (hgroup
              (span)
              (h3 "Lists as Trees"))
            (div :class "outline-text-4" :id "text-1-4-2"
              (p "Lists are a very simple data structure capable of making other data structures.
Consider the linked list of cons cells above:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(cons 'defun (cons 'sum (cons (cons 'x
                                     (cons 'y nil))
                               (cons
                                (cons '+
                                      (cons 'x (cons 'y nil)))
                                nil))))"))
              (p "While it is simply a linked list of cons cells, it's useful to think of it
another way: a tree. Each cons cell has two parts: a car and a cdr. The car
holds a leaf in the tree, whereas the cdr holds either another branch (in the
common case) or another leaf. When working with lists as a tree, you can use"
                (code "car") " and "
                (code "cdr") " to access those two positions.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *my-tree* (cons 'defun (cons 'sum (cons (cons 'x
                                     (cons 'y nil))
                               (cons
                                (cons '+
                                      (cons 'x (cons 'y nil)))
                                nil)))))"))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(car *my-tree*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "DEFUN")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(cdr *my-tree*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(SUM (X Y) (+ X Y))")))
              (p
                (code "copy-tree") " returns a copy of a tree of cons cells:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(copy-tree *my-tree*)"))
              (p
                (code "subst") " returns a new list that substitutes leaves in the tree:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(subst 'z 'y *my-tree*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(DEFUN SUM (X Z) (+ X Z))")))
              (p
                (code "sublis") " does the same with multiple leaves in the tree:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(sublis '((sum  . subtract)
          (+    . -)
          (x    . a)
          (y    . b))
        *my-tree*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(DEFUN SUBTRACT (A B) (- A B))")))))
          (section :id "lists-as-tables"
            (hgroup
              (span)
              (h3 "Lists as Tables"))
            (div :class "outline-text-4" :id "text-1-4-3"
              (p
                (code "sublis") " takes a special type of list for its first argument: an association
list, otherwise known as an alist or table. A table is a list with nested dotted
lists–lists that have a non-nil leaf in the cdr position.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *en-to-ja-table* '((one   . ichi)
                                 (two   . ni)
                                 (three . san)
                                 (four  . yon)
                                 (five  . go)))"))
              (p "With tables, the car is a key and the cdr is a value. You can search a table by
either key or value using "
                (code "assoc") " and "
                (code "rassoc") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(assoc 'two en-to-ja-table)
(rassoc 'ni en-to-ja-table)"))))
          (section :id "lists-as-sets"
            (hgroup
              (span)
              (h3 "Lists as Sets"))
            (div :class "outline-text-4" :id "text-1-4-4"
              (p "Lists can also be treated like sets–an unordered sequence of unique elements.")
              (p "With "
                (code "adjoin") ", you can a single unique element into one list.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(adjoin 'three '(one two three))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(ONE TWO THREE)")))
              (p "Notice that "
                (code "'three") " only occurs once. Since it was already in the \"set\", it
wasn't added.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(adjoin 'four '(one two three))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(ONE TWO THREE FOUR)")))
              (p "Contents of two sets can be compared to form new sets:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *pizza* '(salty sweet cheese sauce round carbs))
(defparameter *cake* '(sweet chocolate brown carbs round))"))
              (p
                (code "intersection") " returns a set of unique elements that are present in both"
                (code "*pizza*") " and "
                (code "*cake*") ":")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(intersection *pizza* *cake*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(CARBS ROUND SWEET)")))
              (p
                (code "union") " returns a set that combines all unique elements of both "
                (code "*pizza*") " and "
                (code "*cake*") ":")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(union *cake* *pizza*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(SAUCE CHEESE SALTY SWEET CHOCOLATE BROWN CARBS ROUND)")))
              (p
                (code "set-difference") " returns a set that includes all of the elements in "
                (code "*cake*") "that are not present in "
                (code "*pizza*") ":")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(set-difference *cake* *pizza*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(BROWN CHOCOLATE)")))
              (p "Or vise versa:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(set-difference *pizza* *cake*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(SAUCE CHEESE SALTY)")))
              (p
                (code "subsetp") " returns "
                (code "t") " if a set is a subset of some other set or "
                (code "nil") " otherwise:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defparameter *cheese-pizza* '(salty cheese sauce round carbs))
(subsetp *cheese-pizza* *pizza*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "T"))))))
        (section :id "control-flow"
          (hgroup
            (span)
            (h2 "CONTROL FLOW"))
          (div :class "outline-text-3" :id "text-1-5"
            (p "Common Lisp contains a number of built-in functions for comparisons, judging
equality, logical operations, and conditions. Unlike other languages, Common
Lisp doesn't really have some kind of universal comparison operator like "
              (code "==") ".
It can be confusing at first, but after a while, you'll get the hang of it.")
            (p "To start with, we need to know how truth and falsehood is represented.")
            (p
              (code "t") " is true and "
              (code "nil") " is false. In the type hierarchy, all data types except for"
              (code "nil") "–which is the empty list–extend "
              (code "t") ". That means that every value except
for "
              (code "nil") " or the empty list are "
              (code "t") ".")
            (p "Common Lisp has the typical comparison operators for math (with "
              (code "\\=") " being
non-typical in other languages):")
            (pre :class "code-block-source-pre"
              (code :class "code-block-source lisp" "(= 1 1)
(/= 1 1)
(> 199 180)
(>= 155 155)
(< 7 19)
(<= 77 77)")))
          (section :id "general-equality"
            (hgroup
              (span)
              (h3 "General Equality"))
            (div :class "outline-text-4" :id "text-1-5-1"
              (p "For symbols, variables, lists, and other objects, you need to use one of "
                (code "eq") ","
                (code "eql") ", "
                (code "equal") ", or "
                (code "equalp") ". Each of them tests equality of different degrees.
If you come from Python or JavaScript you'll usually expect functionality
similar to "
                (code "equal") " or "
                (code "equalp") ".")
              (p
                (code "eq") " will test equality of identity. Do these two objects share the same place
in memory?")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *deez-nums* '(1 2 3 4 5))
(defparameter *your-nums* '(1 2 3 4 5))
(defparameter *gods-nums* '(one two three four five))

(eq *deez-nums* *your-nums*)                ; NIL, two different lists.
(eq *deez-nums* *deez-nums*)                ; T, same list.
(eq *gods-nums* '(one two three four five)) ; NIL
(eq 'one 'one)                              ; T, symbols are reused when they
                                            ; are from the same package.
(eq '(1 2 3 4 5) '(1 2 3 4 5))              ; NIL, two lists are constructed
                                            ; separately.
(eq #\\a #\\a)                                ; T
(eq #\\a #\\A)                                ; NIL
(eq \"hello\" \"hello\")                        ; NIL, strings are arrays of
                                            ; characters and are constructed
                                            ; separately.
(defparameter *greeting* \"Hello, world!\")
(eq *greeting* *greeting*)                  ; T, same array of characters."))
              (p
                (code "eql") " is the same as "
                (code "eq") ", except that if the arguments are characters or
numbers of the same type then their values (not their places in memory) are
compared.")
              (p
                (code "equal") " tests structural similarity.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(equal '(1 2 3) '(1 2 3))               ; T
(equal '(1 3 2) '(1 2 3))               ; NIL
(equal \"hello\" \"hello\")                 ; T
(equal \"HELLO\" \"hello\")                 ; NIL
(equal 1 1.0)                           ; NIL, different types"))
              (p
                (code "equalp") " is further lenient:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(equalp #\\a #\\A)                        ; T
(equalp \"hello\" \"HELLO\")                ; T, good for case-insensitive testing
                                        ; of characters or strings
(equalp 1 1.0)                          ; T, good for testing numbers across
                                        ; number types
(equalp '(1 2 3) '(1 3 2))              ; NIL"))
              (p "Characters and strings have their own equality operators: "
                (code "char-equal") " and"
                (code "string-equal") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(char-equal #\\a #\\A)                    ; T, same as (equalp #\\a #\\A)
(string-equal \"hello\" \"HELLO\")          ; T, same as (equalp \"hello\" \"HELLO\")"))
              (p "Additionally, there are tests like "
                (code "char-greaterp") ", "
                (code "string=") ", "
                (code "char<") ", etc.")
              (p "For more detail you should look at "
                (a :href "https://novaspec.org/cl/f_equal" "the Hyperspec") ".")))
          (section :id "logical-operators"
            (hgroup
              (span)
              (h3 "Logical Operators"))
            (div :class "outline-text-4" :id "text-1-5-2"
              (p "Common Lisp has the typical logical operators.")
              (p
                (code "and") " tests if more than two forms are true. All forms are evaluated
left-to-right, and evaluation stops if any inner forms return "
                (code "nil") ". The "
                (code "and") "form returns the value returned by the last form inside it if all forms in it
evaluate to "
                (code "t") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(and 't 5)          ; => 5
(and 't 5 'hello)   ; => HELLO
(and 'nil 5 'hello) ; => NIL"))
              (p
                (code "or") " returns the value of the first form that evaluates to true. If no form
passed to it returns a true value, it returns "
                (code "nil") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(or 5 't 'nil 'hello)                  ; => 5
(or 'nil (> 1 5) (eq \"hello\" \"hello\")) ; => NIL"))
              (p
                (code "or") " will stop evaluating forms on the first form that evaluates to true.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(or 'nil (> 5 10) (eq \"hello\" \"hello\")  ; all evaluate to nil
    (print \"Evaluated, returns true.\")
    (print \"Not evaluated.\"))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "Evaluated, returns true.")))
              (p
                (code "not") " will return "
                (code "t") " if the inner form returns "
                (code "nil") ", and "
                (code "nil") " if that form
returns "
                (code "t") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(not (= 1 1))  ; => NIL
(not (oddp 2)) ; => T"))))
          (section :id "conditional-forms"
            (hgroup
              (span)
              (h3 "Conditional Forms"))
            (div :class "outline-text-4" :id "text-1-5-3"
              (p "There are a number of different conditional forms. Of course, there is trusty
ol' "
                (code "if") ". "
                (code "if") " is a special operator. It takes a test argument. If the test
returns true, the next form is evaluated. If the test returns false, then the
form after that is evaluated.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(if (> 10 1)                   ; if
    '10-is-greater-than-1      ; then branch
    '10-is-not-greater-than-1) ; (optional) else branch"))
              (p "If you don't need a second (else) branch, you can use "
                (code "when") " or "
                (code "unless") ":")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(let ((num 0))
  (when (<= 1 num 10)
    (format t \"~&FROM WHEN EXPRESSION: ~a is between 1 and 10\" num)
    (print num)
    (print (+ num num)))

  (unless (<= 1 num 10)
    (format t \"~&FROM UNLESS EXPRESSION: ~a is not between 1 and 10\" num)
    (print num)
    (print (+ num num))))"))
              (p
                (code "(unless test ...)") " is equivalent to "
                (code "(when (not test) ...)") ".")
              (p "One of the upsides of not having an else branch is that you can do multiple
operations after the test with "
                (code "when") " and "
                (code "unless") ".")
              (p
                (code "cond") " takes lists of tests and forms to evaluate if the test returns "
                (code "t") ". The
parentheses can be tricky here.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(cond (test form-to-evaluate-if-test-returns-t)
      (test form-to-evaluate-if-test-returns-t))"))
              (p "Here is a practical example:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defparameter *monster-happiness-meter* 49)
(let ((mhm *monster-happiness-meter*))
  (cond
    ((>= 0 mhm)     (format t \"~&Get this monster lifting weights at the gym, now!\")    'take-monster-to-gym)
    ((<= 70 mhm 89)  (format t \"~&This monster is pretty happy.\")                       'hang-out-with-monster)
    ((<= 50 mhm 69)  (format t \"~&This monster is feeling a little down.\")              'invite-monster-to-lunch)
    ((<= 30 mhm 49)  (format t \"~&Get this monster's mommy on the phone.\")              'call-monsters-mommy)
    ((<= 1 mhm 29)   (format t \"~&Did this monster's grandma die or something?\")        'console-monster)
    (t              (format t \"~&This is the catchall fallback expression.\")            'fallback)))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "CALL-MONSTERS-MOMMY")))
              (p
                (code "cond") " will stop evaluating on the first form that returns "
                (code "t") ".")
              (p
                (code "case") " takes a key expression and then some clauses. It evaluates the clauses in
order. If the value of the first item of a clause is "
                (code "eql") " to the value returned
by the key expression, the rest of the items in the clause are evaluated.")
              (p
                (code "case") " is part of the family of case forms: "
                (code "case") ", "
                (code "ccase") ", "
                (code "ecase") ","
                (code "typecase") ", "
                (code "ctypecase") ", and "
                (code "etypecase") ". If you know case statements from other
languages, you understand the basic idea.")
              (p
                (code "case") " and "
                (code "typecase") " do nothing if no match is found. If you want to trigger
errors when no match is found (rather than providing an fallback clause), use"
                (code "ecase") " and "
                (code "etypecase") ". If you want the option to provide a value for the key
expression and continue the program, use "
                (code "ccase") " and "
                (code "ctypecase") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *env* :DEVELOPMENT)

(defun check-environment ()
  (case *env*
    (:DEVELOPMENT \"logging EVERYTHING\")
    (:PRODUCTION \"locking in and locking down\")))

(check-environment)         ; => \"logging EVERYTHING\"

(let ((*env* :PRODUCTION))
  (check-environment))      ; => \"locking in and locking down\"

(let ((*env* :AWS))
  (check-environment))      ; => NIL"))
              (p
                (code "case") " will return "
                (code "nil") " when none of the other cases match. You can set the
fallback case using "
                (code "t") " or "
                (code "otherwise") ".")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun check-environment ()
  (case *env*
    (:DEVELOPMENT \"logging EVERYTHING\")
    (:PRODUCTION \"locking in and locking down\")
    (otherwise \"you gotta set your *env* to :DEVELOPMENT or :PRODUCTION\")))

(let ((*env* :AWS))
  (check-environment))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "you gotta set your envto :DEVELOPMENT or :PRODUCTION")))
              (p "If you want to return an error if the argument falls through all the checks, use"
                (code "ecase") ".")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun check-environment ()
  (ecase *env*
    (:DEVELOPMENT \"logging EVERYTHING\")
    (:PRODUCTION \"locking in and locking down\")))

(let ((*env* :AWS))
  (check-environment))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") ":AWS fell through ECASE expression.
Wanted one of (:DEVELOPMENT :PRODUCTION).
[Condition of type SB-KERNEL:CASE-FAILURE]
Restarts:
0: [RETRY] Retry SLY evaluation request.
1: [*ABORT] Return to SLY's top level.
2: [ABORT] abort thread (#<THREAD tid=11923 \"slynk-worker\" RUNNING {70071BE463}>)")))
              (p "If you want a "
                (i "continuable") " error, use "
                (code "ccase") ".")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun check-environment ()
  (ccase *env*
    (:DEVELOPMENT \"logging all errors\")
    (:PRODUCTION \"locking in and locking down\")))

(let ((*env* :AWS))
  (check-environment))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") ":AWS fell through CCASE expression.
Wanted one of (:DEVELOPMENT :PRODUCTION).
[Condition of type SB-KERNEL:CASE-FAILURE]
Restarts:
0: [STORE-VALUE] Supply a new value for ENV.
1: [RETRY] Retry SLY evaluation request.
2: [*ABORT] Return to SLY's top level.
3: [ABORT] abort thread (#<THREAD tid=11955 \"slynk-worker\" RUNNING {7007B83833}>)")))
              (p "Notice that now you can store a value in "
                (code "*env*") ". If you do, it will retry the
case check with that value.")
              (p "The "
                (code "typecase") " family works the same, but will check the type of value returned
by the key expression.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(let ((x 5))
  (typecase x
    (list 'this-is-a-list)
    (number 'this-is-a-number)
    (function 'this-is-a-function)
    (otherwise 'i-dont-know-what-this-is)))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "THIS-IS-A-NUMBER")))
              (p
                (code "handler-case") " is another member of the "
                (code "case") " family that works on conditions
and errors. We'll look at it more in the chapter on Errors & Conditions."))))
        (section :id "iteration"
          (hgroup
            (span)
            (h2 "ITERATION"))
          (div :class "outline-text-3" :id "text-1-6"
            (p "There are many ways to iterate in Common Lisp. There are the map/filter/reduce
operations as in other functional programming languages, but Lisp additionally
has "
              (i "do") " iterators, a special iterator macro, and predicate iterators."))
          (section :id "iterating-by-mapping"
            (hgroup
              (span)
              (h3 "Iterating by Mapping"))
            (div :class "outline-text-4" :id "text-1-6-1"
              (p "You can iterate by using a functional programming style mapping. You've seen it
in action already:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(mapcar #'squirt-then-double '(1 2 3 4 5))"))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(mapcar #'first '((1 2 3 4 5) (one two three four five) (what is going on here) (once
 upon a time)))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "((1 ONE WHAT ONCE))")))
              (p
                (a :href "https://novaspec.org/cl/f_mapc" "There are several varieties of mapping functions") ". "
                (code "mapcar") " is the most common.
The most general mapping function is "
                (code "map") ".")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" ";; Go through each character in the string and upper case it. Return a string.
(map 'string #'char-upcase \"hello\")"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "\"HELLO\"")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" ";; Take an item from each of the lists and multiply them, returning a list.
;; Finishes at the end of the shortest list.
(map 'list #'* '(2 4 6 8) '(3 5 7 9 11 13 15) '(10 10 10))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(60 200 420)")))))
          (section :id "iterating-by-reducing"
            (hgroup
              (span)
              (h3 "Iterating by Reducing"))
            (div :class "outline-text-4" :id "text-1-6-2"
              (p "You can reduce multiple values down to one value by applying some function to
successive items in the sequence with "
                (code "reduce") ".")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(reduce #'+ '(2 2 2))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "6")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(reduce #'* '(2 2 2))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "8")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(reduce #'append '((2 2 2) (3 3 3) (4 4)))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(2 2 2 3 3 3 4 4)")))
              (p "If you define a function to use for reducing, it needs to take two arguments:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun sum (x y) (+ x y))"))
              (p "Call it with "
                (code "reduce") ":")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(reduce #'sum '(1 1 1 3 4 5 6 7))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "28")))))
          (section :id "iterating-by-filtering"
            (hgroup
              (span)
              (h3 "Iterating by Filtering"))
            (div :class "outline-text-4" :id "text-1-6-3"
              (p "You can filter the contents of a sequence with "
                (code "remove-if-not") ":")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(remove-if-not #'oddp '(1 2 3 4 5 6 7 8 9 10))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(1 3 5 7 9)")))
              (p "You can define your own predicate and pass it to "
                (code "remove-if-not") ":")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *dangerous-animals* '(lion tiger bear snake shark))

(defun safe-animal-p (animal)
  (not (member animal *dangerous-animals*)))"))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(remove-if-not #'safe-animal-p '(dog cat monkey lion hamster shark snake bear
 koala frog))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(DOG CAT MONKEY HAMSTER KOALA FROG)")))
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun greater-than-50-p (num)
  (> num 50))"))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(remove-if-not #'greater-than-50-p '(40 50 30 90 80 10 70 100 25 60 55 2))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(90 80 70 100 60 55)")))
              (p "You can also use "
                (code "remove-if") ":")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun dangerous-animal-p (animal)
  (member animal *dangerous-animals*))"))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(remove-if #'dangerous-animal-p '(dog cat monkey lion hamster shark snake bear
 koala frog))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(DOG CAT MONKEY HAMSTER KOALA FROG)")))))
          (section :id "iterating-by-doing"
            (hgroup
              (span)
              (h3 "Iterating by Doing"))
            (div :class "outline-text-4" :id "text-1-6-4")
            (section :id "-dotimes-"
              (hgroup
                (span)
                (h4
                  (code "dotimes")))
              (div :class "outline-text-5" :id "text-1-6-4-1"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(dotimes (index-var n [result-form])
  body)"))
                (p
                  (code "dotimes") " is for doing something a set number of times. Simple enough. It should
feel familiar if you've ever done this in JavaScript:")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source c" "for (let i = 0; i < 10; i++) {
 ...}"))
                (p "or this in Python:")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source python" "for i in range(5):
    ..."))
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(dotimes (i 5)
  (if (= i 4)
      (print \"I'M NOT CRAZY!!!\")
      (print \"I'm not crazy!\")))"))
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(let* ((num-list '(99 98 97 96 95))
       (times (length num-list)))
  (dotimes (i times)
    (let ((bottles-of-milk (nth i num-list)))
      (format t \"~&~a of bottles of milk on the wall, ~a bottles of milk.\"
 bottles-of-milk bottles-of-milk))))"))))
            (section :id "-dolist-"
              (hgroup
                (span)
                (h4
                  (code "dolist")))
              (div :class "outline-text-5" :id "text-1-6-4-2"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(dolist (index-var list [result-form])
  body)"))
                (p
                  (code "dolist") " is similar. If you're doing simple stuff with lists, this is probably
what you want.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(dolist (i '(yet another beautiful short list))
  (format t \"~&~a\" i))"))
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun my-reverse (list)
  (let ((reversed nil))
    (dolist (i cat reversed)
      (push (pop cat) reversed))))"))
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun check-all-even (nums)
  (dolist (i nums t)
    (format t \"~&Looking at ~a...\" i)
    (when (oddp i)
      (format t \"~&Ooops, this is odd!\")
      (return nil))))"))
                (p "The [result-form] is in square brackets because it's optional. If you don't set
it, then you need to return the result-form manually.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun my-reverse-manual-return (list)
  (let ((reversed nil))
    (dolist (i cat)                     ; reversed not set as result-form
      (push (pop cat) reversed))
    reversed))                          ; reversed manually called at the end of
                                        ; the let block"))
                (p "Notice the use of "
                  (code "push") " and "
                  (code "pop") " here. They are both destructive functions
that modify lists, either by adding or removing items. "
                  (code "push") " puts items at the
front of the list, returning the modified list. "
                  (code "pop") " removes the first item and
returns that item.")))
            (section :id "-do-"
              (hgroup
                (span)
                (h4
                  (code "do")))
              (div :class "outline-text-5" :id "text-1-6-4-3"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(do ((var1 init1 [update1])
     (var2 init2 [update2])
     ...)
    (test action-1... action-n) ; base case
 body)"))
                (p
                  (code "do") " is the most general and powerful of the "
                  (code "do") " family. It is also a bit
complicated and difficult to read/understand.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun check-all-even-do (nums)
  (do ((n nums (cdr n)))
      ((null n) (return t))
    (format t \"~&Looking at ~a...\" (first n))
    (when (oddp (first n))
      (format t \"~&~a is odd.\" (first n))
      (return nil))))"))
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(check-all-even-do '(2 4 6 7 8 10))"))
                (p "Unlike "
                  (code "dotimes") " and "
                  (code "dolist") ", which take care of incrementing the counter or
stepping through the list, "
                  (code "do") " requires you to specify the step/update at the
end of each iteration. It also requires you to take care of specifying the base
case–the conditions for ending the loop.")
                (p
                  (code "do") " is useful especially for people who are comfortable with iterating
recursively because recursion also requires the programmer to specify the
stepping function and base case.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun check-all-even-recursive (nums)
  (cond ((null nums) t)
        ((oddp (car nums)) (format t \"~&~a is odd!\" (first nums)) nil)
        (t (format t \"~&~a is even.\" (car nums))
           (check-all-even-recursive (cdr nums)))))"))
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(check-all-even-recursive '(2 4 6 7 8 10))"))
                (p "The step function is "
                  (code "(cdr nums)") ". Using "
                  (code "cdr") " to step through a list is called
\"cdring down\" the list. The similarities between the recursive method and the"
                  (code "do") " method make it both powerful and often unergonomic. As a reader of code,
when you see either a recursive or "
                  (code "do") " iteration, you have to check the step
function and base case–something you don't have to do with "
                  (code "dotimes") ", "
                  (code "dolist") ","
                  (code "mapcar") ", "
                  (code "remove-if-not") ", etc.")
                (p "To reiterate, the "
                  (code "loop") " iterator is much more widely used than the "
                  (code "do") "iterators, so if you wish for most other Lispers to understand your code and
collaborate with others, you should generally prefer "
                  (code "loop") ".")
                (p "If you prefer a more applicative/functional style, then map/filter/reduce is the
way to go."))))
          (section :id "iterating-by-looping"
            (hgroup
              (span)
              (h3 "Iterating by Looping"))
            (div :class "outline-text-4" :id "text-1-6-5"
              (p "The most popular iterating construct in Common Lisp is the "
                (code "loop") " macro:")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(loop :for item :in '(one two three)
      :do (print item))"))
              (p
                (code "loop") " is a macro that uses its own syntax. If you come from Python or
JavaScript, it doesn't look so strange, but it looks different from typical Lisp
code.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(loop :for n :in '(1 2 3 4 5)
      :collect (* 2 (sqrt n)))"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(2.0 2.828427 3.4641016 4.0 4.472136)")))
              (p "The "
                (code "loop") " macro ")))
          (section :id "predicate-iterators"
            (hgroup
              (span)
              (h3 "Predicate Iterators"))
            (div :class "outline-text-4" :id "text-1-6-6"
              (p "If you only need to test the sequence, "
                (code "check-all-even") " could be rewritten using"
                (code "every") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(every #'evenp '(2 4 6 8 10))
                                        ; => T
(every #'evenp '(1 4 6 8 10))
                                        ; => NIL"))
              (p
                (code "every") " runs a test on all items of a sequence and returns "
                (code "t") " if the test
returns "
                (code "t") " for every item.")
              (p "Similarly, "
                (code "some") " tests all items of a sequence, but will return "
                (code "t") " as soon as
the test returns "
                (code "t") " for an item.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(some #'oddp '(2 4 6 8))
                                        ; => NIL
(some #'oddp '(1 4 6 8))
                                        ; => T"))
              (p
                (code "notevery") " runs a test on all items of a sequence and returns "
                (code "t") " if the test
returns "
                (code "nil") " for every item.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(notevery #'oddp '(1 3 5))
                                        ; => NIL
(notevery #'oddp '(1 2 3 5))
                                        ; => T"))
              (p
                (code "notany") " runs a test on all items of a sequence and returns "
                (code "t") " if the test
returns "
                (code "nil") " for one item.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(notany #'oddp '(2 4 6))
                                        ; => T
(notany #'oddp '(1 2 4 6))
                                        ; => NIL"))))
          (section :id "early-returns"
            (hgroup
              (span)
              (h3 "Early Returns"))
            (div :class "outline-text-4" :id "text-1-6-7"
              (p "There are times when it's necessary to do an early return. We saw an example
with "
                (code "check-all-even") ":")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun check-all-even (nums)
  (dolist (i nums t)
    (format t \"~&Looking at ~a...\" i)
    (when (oddp i)
      (format t \"~&Ooops, this is odd!\")
      (return nil))))"))
              (p "The result-form set for "
                (code "dolist") " is "
                (code "t") ", meaning that when we get to the end of
the list we should return "
                (code "t") ".")
              (p "However, when we spot an odd number, we need to return from the "
                (code "dolist") " loop
early using "
                (code "(return nil)") "."))))
        (section :id "strings-i-o"
          (hgroup
            (span)
            (h2 "STRINGS & I/O"))
          (div :class "outline-text-3" :id "text-1-7"
            (p "Strings in Common Lisp are "
              (code "vectors") " of characters. As a result, all operations
that can be used on "
              (code "sequences") " can be used on arrays."))
          (section :id "printing-information-to-repl"
            (hgroup
              (span)
              (h3 "Printing Information to REPL"))
            (div :class "outline-text-4" :id "text-1-7-1"
              (p "You can print information in the REPL using "
                (code "print") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(print \"hello world\")"))
              (p
                (code "print") " both sends data to the REPL, but also returns the data as a value. That
means you can place "
                (code "print") " over many different kinds of code, making if useful
for simple debugging.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun factorial (x)
  (labels ((_factorial (n)
             (cond ((= n 0) 1)
                   (t (print (* n (_factorial (- n 1))))))))
    (_factorial x)))"))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(factorial 5)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "1 2 6 24 120 => 120")))))
          (section :id "using-sequence-operations-on-strings"
            (hgroup
              (span)
              (h3 "Using Sequence Operations on Strings"))
            (div :class "outline-text-4" :id "text-1-7-2")
            (section :id "-concatenate-"
              (hgroup
                (span)
                (h4
                  (code "concatenate")))
              (div :class "outline-text-5" :id "text-1-7-2-1"
                (p
                  (code "concatenate") " combines two or more sequences–meaning it can combine lists,
vectors, or strings. The first argument specifies the output type.")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(concatenate 'list '(#\\h #\\e #\\l #\\l #\\o #\\space) '(#\\w #\\o #\\r #\\l #\\d))"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "(######\\  #####)")))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(concatenate 'vector '(#\\h #\\e #\\l #\\l #\\o #\\space) '(#\\w #\\o #\\r #\\l #\\d))"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "#(######\\  #####)")))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(concatenate 'string '(#\\h #\\e #\\l #\\l #\\o #\\space) '(#\\w #\\o #\\r #\\l #\\d))"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "\"hello world\"")))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(concatenate 'string \"hello \" \"world\")"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "\"hello world\"")))))
            (section :id "-length-"
              (hgroup
                (span)
                (h4
                  (code "length")))
              (div :class "outline-text-5" :id "text-1-7-2-2"
                (p
                  (code "length") " can tell you the number of characters in a string.")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(length \"hello world\")"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "11")))))
            (section :id "-reverse-"
              (hgroup
                (span)
                (h4
                  (code "reverse")))
              (div :class "outline-text-5" :id "text-1-7-2-3"
                (p "Just as with lists, "
                  (code "reverse") " can place characters in a string in reverse order.")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(reverse \"YOU WILL REWRITE THAT THING IN COMMON LISP IMMEDIATELY\")"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "\"YLETAIDEMMI PSIL NOMMOC NI GNIHT TAHT ETIRWER LLIW UOY\"")))))
            (section :id "-map-"
              (hgroup
                (span)
                (h4
                  (code "map")))
              (div :class "outline-text-5" :id "text-1-7-2-4"
                (p
                  (code "map") " is the most general of the mapping functions. If you pass "
                  (code "string") " as the
result type, you can run character operations on the string and get back a new
string.")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(map 'string #'char-upcase \"hello\")"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "\"HELLO\""))))))
          (section :id "string-specific-operations"
            (hgroup
              (span)
              (h3 "String Specific Operations"))
            (div :class "outline-text-4" :id "text-1-7-3")
            (section :id "string-modification"
              (hgroup
                (span)
                (h4 "String Modification"))
              (div :class "outline-text-5" :id "text-1-7-3-1"
                (p "There are also string-specific functions.")
                (p "The previous "
                  (code "map") " call can be simplified using "
                  (code "string-upcase") ".")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(string-upcase \"almighty\")"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "\"ALMIGHTY\"")))
                (p "You can probably guess what "
                  (code "string-downcase") " does.")))
            (section :id "string-comparison-operators"
              (hgroup
                (span)
                (h4 "String Comparison Operators"))
              (div :class "outline-text-5" :id "text-1-7-3-2"
                (p
                  (code "string=") " and "
                  (code "string-equal") " can be used to test if two strings are the same."
                  (code "string=") " is case-sensitive, "
                  (code "string-equal") " is case-insensitive.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(string= \"almighty\" \"almighty\")
                                        ; => T
(string= \"almighty\" \"Almighty\")         ; case-sensitive => NIL
(string-equal \"almighty\" \"ALMIGHTY\")    ; case-insensitive => T"))
                (p "There are other string comparison operators: "
                  (code "string>") ", "
                  (code "string/=") ","
                  (code "string-not-greaterp") ", etc."))))
          (section :id "streams"
            (hgroup
              (span)
              (h3 "Streams"))
            (div :class "outline-text-4" :id "text-1-7-4"
              (p "Streams are either a source or destination for some data. Common Lisp uses
streams for reading and writing files, etc.")))
          (section :id "writing-files"
            (hgroup
              (span)
              (h3 "Writing Files"))
            (div :class "outline-text-4" :id "text-1-7-5"
              (p "Use "
                (code "with-open-file") " to create a block where a streaming connection with some
file is active.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(with-open-file (stream #P\"io-test.txt\" :direction :output :if-exists :supersede
 :if-does-not-exist :create)
  (format stream \"~&Put this in the test file.~&This will be on a new line.~&\"))"))
              (p
                (code "with-open-file") " is a macro providing a shortcut to using the lower-level
functions "
                (code "open") " and "
                (code "close") " combined with "
                (code "unwind-protect") ". It ensures that the
connection to the file is closed before leaving the block.")
              (p "An exhaustive explanation of all the options "
                (code "open") " and "
                (code "with-open-file") " can
take are in "
                (a :href "https://cl-community-spec.github.io/pages/open.html" "the Hyperspec") ". We'll cover the basics here.")))
          (section :id "reading-files"
            (hgroup
              (span)
              (h3 "Reading Files"))
            (div :class "outline-text-4" :id "text-1-7-6"
              (p "If you want to read a file, set "
                (code ":direction") " to "
                (code ":input") " and use one of "
                (code "read") ","
                (code "read-line") ", or "
                (code "read-char") ".")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(with-open-file (stream #P\"io-test.txt\" :direction :input :if-exists :supersede
 :if-does-not-exist :create)
  (loop for line = (read-line stream nil nil)
        while line
        do (format t \"~a~&\" line)))"))))
          (section :id "beyond-the-basics-w-files"
            (hgroup
              (span)
              (h3 "Beyond the Basics w/Files"))
            (div :class "outline-text-4" :id "text-1-7-7"
              (p "Common Lisp functions for working with file systems, paths, etc. are generally
not portable between Lisp implementations. The package "
                (code "UIOP") " is the
defacto-standard source of portable path and filesystem utilities.")))
          (section :id "-format-"
            (hgroup
              (span)
              (h3
                (code "format")))
            (div :class "outline-text-4" :id "text-1-7-8")
            (section :id "stream-output"
              (hgroup
                (span)
                (h4 "Stream Output"))
              (div :class "outline-text-5" :id "text-1-7-8-1"
                (p
                  (code "format") " is used to write strings to output streams. The first argument is the
stream. If set to "
                  (code "t") ", then it will send the input to "
                  (code "*standard-output*") ", which
is output to the REPL.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(format t \"~&Almighty Lisp~%\")"))
                (p "However, as in the "
                  (code "with-open-file") " examples, you can also use "
                  (code "format") " to write
to files, etc.")))
            (section :id "the-almighty-tilde"
              (hgroup
                (span)
                (h4 "The Almighty Tilde"))
              (div :class "outline-text-5" :id "text-1-7-8-2"
                (p
                  (code "format") " has an extensive set of "
                  (code "control-string directives") " used for
customizing how text is formatted. All of the directives begin with "
                  (code "tilde") ",
such as "
                  (code "~a") ", "
                  (code "~&") ", etc. They also have an extensive set of modifiers. Complex
format strings are vaguely similar to regular expressions and tend to get just
as hairy.")
                (p "I will cover the bare minimum here. Refer to the "
                  (a :href "https://cl-community-spec.github.io/pages/Formatted-Output.html" "Hyperspec") " to learn all of the
directives. I will explain any other directives as necessary."))
              (section :id "-tilde-a-"
                (hgroup
                  (span)
                  (h5
                    (code "Tilde a")))
                (div :class "outline-text-6" :id "text-1-7-8-2-1"
                  (p
                    (code "Tilde a") " will output the data in human readable, \"Aesthetic\" format.")
                  (div :class "code-block"
                    (pre :class "code-block-source-pre"
                      (code :class "code-block-source lisp" "(format t \"~a\" (aref \"hello\" 0))"))
                    (pre :class "result"
                      (code :class "code-block-result"
                        (span :class "code-block-result-label" "Result") "h => NIL NOTE: not #")))
                  (p
                    (code "format") " takes an arbitrary number of arguments after the format string. For
each argument, you need to supply another "
                    (code "Tilde a") ".")
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(defparameter *alist* '((:micah 'male '39 'married 'japan)
                        (:takae 'female '34 'married 'japan)
                        (:mom 'female '70 'married 'america)
                        (:papa 'male '74 'married 'america)))"))
                  (div :class "code-block"
                    (pre :class "code-block-source-pre"
                      (code :class "code-block-source lisp" "(loop for row in *alist*
      do (format t \"~&~a: ~a~%\" (first row) (rest row)))"))
                    (pre :class "result"
                      (code :class "code-block-result"
                        (span :class "code-block-result-label" "Result") "MICAH: ('MALE '39 'MARRIED 'JAPAN)
TAKAE: ('FEMALE '34 'MARRIED 'JAPAN)
MOM: ('FEMALE '70 'MARRIED 'AMERICA)
PAPA: ('MALE '74 'MARRIED 'AMERICA)
=> NIL")))
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(loop for row in *alist*
      do (format t \"=?~%\" \"~a: ~a ~a ~a ~a\" row))"))))
              (section :id "-tilde-and-tilde-"
                (hgroup
                  (span)
                  (h5
                    (code "Tilde &") " and "
                    (code "Tilde %")))
                (div :class "outline-text-6" :id "text-1-7-8-2-2"
                  (p "These directives output newlines. "
                    (code "Tilde %") " will always output a newline. "
                    (code "Tilde
&") " will only output a newline if the output stream is not on a newline already."))))
            (section :id "format-directive-cheat-sheet"
              (hgroup
                (span)
                (h4 "Format Directive Cheat Sheet"))
              (div :class "outline-text-5" :id "text-1-7-8-3"
                (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                  (colgroup
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-left"))
                  (thead
                    (tr
                      (th :scope "col" :class "org-left" "Directive")
                      (th :scope "col" :class "org-left" "Description")
                      (th :scope "col" :class "org-left" "Parameters")
                      (th :scope "col" :class "org-left" "Colon")
                      (th :scope "col" :class "org-left" "At-sign")
                      (th :scope "col" :class "org-left" "Examples")
                      (th :scope "col" :class "org-left" " ")
                      (th :scope "col" :class "org-left" " ")
                      (th :scope "col" :class "org-left" " ")
                      (th :scope "col" :class "org-left" " ")))
                  (tbody
                    (tr
                      (td :class "org-left" "~A")
                      (td :class "org-left" "Prints an argument without escape characters (like princ). Supports mincol for minimum width, full form for padding control.")
                      (td :class "org-left" "mincol=0, colinc=1, minpad=0, padchar=space")
                      (td :class "org-left" "Prints nil as ()")
                      (td :class "org-left" "Left-justify (pad left)")
                      (td :class "org-left" "(format nil \"~5A\" \"hi\") => \"   hi\"; (format nil \"~5:@A\" \"hi\") => \"hi   \"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~S")
                      (td :class "org-left" "Prints an argument with escape characters (like prin1). Same params/modifiers as ~A.")
                      (td :class "org-left" "Same as ~A")
                      (td :class "org-left" "Same as ~A")
                      (td :class "org-left" "Same as ~A, upcases")
                      (td :class "org-left" "(format nil \""
                        (code "S\" '(1 2)) => \"(1 2)\"; (format nil \"") ":@S\" '(1 2)) => \"(1 2)\" (upcased if applicable)")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~D")
                      (td :class "org-left" "Prints integer in decimal (base 10). Supports width, padding, commas.")
                      (td :class "org-left" "mincol=0, padchar=0/space, comma=,, interval=3")
                      (td :class "org-left" "No commas")
                      (td :class "org-left" "Always print sign (+/-)")
                      (td :class "org-left" "(format nil \"~:D\" 1234) => \"1,234\"; (format nil \"~@D\" -1234) => \"-1234\"; (format nil \"~12,'0D\" 42) => \"0000000042\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~B")
                      (td :class "org-left" "Prints integer in binary (base 2). Same as ~D.")
                      (td :class "org-left" "Same as ~D")
                      (td :class "org-left" "No commas")
                      (td :class "org-left" "Always print sign")
                      (td :class "org-left" "(format nil \""
                        (code "B\" 5) => \"101\"; (format nil \"") ":B\" 5) => \"101\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~O")
                      (td :class "org-left" "Prints integer in octal (base 8). Same as ~D.")
                      (td :class "org-left" "Same as ~D")
                      (td :class "org-left" "No commas")
                      (td :class "org-left" "Always print sign")
                      (td :class "org-left" "(format nil \"~O\" 8) => \"10\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~X")
                      (td :class "org-left" "Prints integer in hex (base 16). Same as ~D; letters A-F.")
                      (td :class "org-left" "Same as ~D")
                      (td :class "org-left" "No commas")
                      (td :class "org-left" "Always print sign, upcase letters")
                      (td :class "org-left" "(format nil \"~X\" 255) => \"FF\"; (format nil \"~@X\" 255) => \"+FF\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~R")
                      (td :class "org-left" "Prints number in words (English cardinal) or radix. No params: words; with radix: like ~D.")
                      (td :class "org-left" "radix=10, mincol=0, padchar=0/space, comma=,, interval=3")
                      (td :class "org-left" "Ordinal (e.g., \"fourth\")")
                      (td :class "org-left" "Roman numerals (e.g., IV)")
                      (td :class "org-left" "(format nil \""
                        (code "R\" 14) => \"fourteen\"; (format nil \"") ":R\" 4) => \"fourth\"; (format nil \""
                        (code "@R\" 4) => \"IV\"; (format nil \"") ":@R\" 1234) => \"MCCXXXIIII\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~P")
                      (td :class "org-left" "Pluralization: no output, but affects following ~S/~A by backing up arg and adding 's' if arg !=1 (or 0).")
                      (td :class "org-left" "None")
                      (td :class "org-left" "Back up one arg (for ~R etc.)")
                      (td :class "org-left" "Use 'ies' instead of 's'")
                      (td :class "org-left" "(format nil \""
                        (code "D ~P item~P\" 1) => \"1 item\"; (format nil \"~D ~:@P\" 2) => \"2 tries\"; (format nil \"~R file") ":P\" 10) => \"ten files\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~C")
                      (td :class "org-left" "Prints a character.")
                      (td :class "org-left" "None")
                      (td :class "org-left" "Spell out name (e.g., \"Space\")")
                      (td :class "org-left" "Reader syntax (#\\X)")
                      (td :class "org-left" "(format nil \""
                        (code "C\" #\\a) => \"a\"; (format nil \"") ":C\" #\\Space) => \"Space\"; (format nil \""
                        (code "@C\" #\\a) => \"#\\\\a\"; (format nil \"") ":@C\" (code-char 0)) => \"Control-@\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~F")
                      (td :class "org-left" "Fixed-point float.")
                      (td :class "org-left" "w=0 (width), d=0 (dec places), k=0 (scale), overflow=#, pad=space")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "Always sign (+/-)")
                      (td :class "org-left" "(format nil \""
                        (code "6,2F\" 3.14) => \"  3.14\"; (format nil \"") ",4F\" 3.14159) => \"3.1416\"; (format nil \"~@F\" 3.14) => \"+3.14\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~E")
                      (td :class "org-left" "Exponential float (scientific).")
                      (td :class "org-left" "w=0, d=0, e=0 (exp digits), k=1, overflow=#, pad=space, expchar=E")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "Always sign (+/-)")
                      (td :class "org-left" "(format nil \""
                        (code "9,2E\" 3141.59) => \"  3.14E+03\"; (format nil \"") ",4E\" 3.14159) => \"3.1416E+00\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~G")
                      (td :class "org-left" "General float: chooses ~F or ~E.")
                      (td :class "org-left" "w=0, d=0, e=3, k=1, overflow=#, pad=space, expchar=G")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "Always sign")
                      (td :class "org-left" "(format nil \"~G\" 0.031) => \"0.031\"; (format nil \"~4G\" 314159) => \"3.142e5\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~$")
                      (td :class "org-left" "Monetary format (like ~F but with $ and 2 decimals).")
                      (td :class "org-left" "d=2 (dec), n=1 (min whole digits), w=0 (width), pad=space")
                      (td :class "org-left" "Sign before padding")
                      (td :class "org-left" "Always sign")
                      (td :class "org-left" "(format nil \"~$\" 1234.56) => \"\\(1234.56\"; (format nil \"~2,4\\)\" 12.34) => \"0012.34\"; (format nil \"~@$\" 1234.56) => \"+$1234.56\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~%")
                      (td :class "org-left" "Unconditional newline(s).")
                      (td :class "org-left" "reps=1 (newlines)")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \"~%\") => \"\\n\"; (format nil \"~2%\") => \"\\n\\n\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~&")
                      (td :class "org-left" "Fresh line: newline if not at start of line.")
                      (td :class "org-left" "reps=1")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format t \"~&Hello\") ensures newline before \"Hello\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~")
                      (td :class "org-left" " ")
                      (td :class "org-left" "Page separator (formfeed).")
                      (td :class "org-left" "reps=1")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \"~")
                      (td :class "org-left" "\") => formfeed char")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~~")
                      (td :class "org-left" "Literal tilde.")
                      (td :class "org-left" "reps=1")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \""
                        (code "~\") => \"") "\"; (format nil \""
                        (code "2") "\") => \"~~\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~<newline>")
                      (td :class "org-left" "Ignore newline and following whitespace in format string.")
                      (td :class "org-left" "None")
                      (td :class "org-left" "Preserve following whitespace")
                      (td :class "org-left" "Ignore following whitespace")
                      (td :class "org-left" "Used for multi-line: ~\\n  text => \"text\" (ignores indent)")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~T")
                      (td :class "org-left" "Tabulate to column or relative.")
                      (td :class "org-left" "colnum=0 (absolute col), colinc=1")
                      (td :class "org-left" "Relative to current position? Wait, CLtL: colon for absolute from left margin in pretty print")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \"~10Tfoo\") => spaces to col 10 + \"foo\"; (format nil \"~5,2T\") => tab 5, inc 2")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~*")
                      (td :class "org-left" "Argument navigation: skip args.")
                      (td :class "org-left" "n=1 (args to skip)")
                      (td :class "org-left" "Back up n args")
                      (td :class "org-left" "Goto nth arg (1-based)")
                      (td :class "org-left" "(format nil \"~*~A\" \"x\" \"y\") => \"y\" (skips x); (format nil \"~3:*~A\" a b c d) => \"d\" (goto 3rd back? Wait, : backs up)")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~?")
                      (td :class "org-left" "Indirection: insert format string and args.")
                      (td :class "org-left" "Takes string arg, then list arg")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "Replace with the sub-format")
                      (td :class "org-left" "(format nil \"~? ~D\" \"~A is ~D\" '(\"hi\" 5) 10) => \"hi is 5 10\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~_")
                      (td :class "org-left" "Pretty-print newline (pprint-newline).")
                      (td :class "org-left" "None")
                      (td :class "org-left" ":fill or :mandatory style")
                      (td :class "org-left" ":miser style")
                      (td :class "org-left" "Used in ~<…~> for logical blocks")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~W")
                      (td :class "org-left" "Write object, respecting print vars ("
                        (b "print-escape") " etc.).")
                      (td :class "org-left" "None")
                      (td :class "org-left" "Ignore "
                        (b "print-level") ", "
                        (b "print-length"))
                      (td :class "org-left" "Pretty-print (with indentation)")
                      (td :class "org-left" "(format nil \"~W\" '(1 2 3)) => \"(1 2 3)\" (escaped if *print-escape*=t)")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~I")
                      (td :class "org-left" "Indent in pretty printing.")
                      (td :class "org-left" "n=0 (spaces)")
                      (td :class "org-left" "Relative to block start")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \"~2I~A~I\" \"text\") indents \"text\" by 2")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left"
                        (code "(~str") ")")
                      (td :class "org-left" "Case conversion on enclosed output.")
                      (td :class "org-left" "None")
                      (td :class "org-left" "Capitalize each word")
                      (td :class "org-left" "Capitalize first word")
                      (td :class "org-left" "(format nil \""
                        (code "(~A") ")\" \"hello WORLD\") => \"hello world\"; (format nil \""
                        (code ":(~A") ")\" \"hello\") => \"Hello\"; (format nil \""
                        (code "@(~A") ")\" \"hello\") => \"HELLO\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left"
                        (code "[str0") ";str1~;…~:;default~]")
                      (td :class "org-left" "Conditional: select clause by arg (0=true first, etc.).")
                      (td :class "org-left" "None (clauses separated by ;)")
                      (td :class "org-left" "Use ~:; for default if arg >= #clauses")
                      (td :class "org-left" "If arg non-nil, do body; else skip")
                      (td :class "org-left" "(format nil \""
                        (code "[zero") ";one~;~:;many~]\" 0) => \"zero\"; (format nil \""
                        (code "[FAIL") ";PASS~]\" nil) => \"FAIL\"; (format nil \"~@[~A~]\" t \"ok\") => \"ok\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left"
                        (code "{str") "}")
                      (td :class "org-left" "Iteration over list arg.")
                      (td :class "org-left" "n=max reps (default all)")
                      (td :class "org-left" "List is one arg (elements iterated)")
                      (td :class "org-left" "Iterate over remaining args as list")
                      (td :class "org-left" "(format nil \""
                        (code "{~A") "}\" '(1 2 3)) => \"123\"; (format nil \""
                        (code "@{~A~^, ~}\" 1 2 3) => \"1, 2, 3\"; (format nil \"") ":{ ~A ~}~%\" '((1) (2))) => \" 1 \\n 2 \\n\"")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~<str~>")
                      (td :class "org-left" "Justification/block.")
                      (td :class "org-left" "~/fun/")
                      (td :class "org-left" "User-defined dispatch to function.")
                      (td :class "org-left" "params passed to fun")
                      (td :class "org-left" "Passed as :colon")
                      (td :class "org-left" "Passed as :at-sign")
                      (td :class "org-left" "Define (defun my-fun (stream colon at &rest args) …); (format nil \"~/my-fun/ ~D\" 42) calls it")
                      (td :class "org-left" " ")
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~^")
                      (td :class "org-left" "Termination condition: exit loop if no args or test fails.")
                      (td :class "org-left" "n=1 (args left), n1=0,n2=1,n3=2 (for ~[)")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \"~{~A~^, ~}\" '(1 2 3)) => \"1, 2, 3\" (stops on no args); Used in ~{ to avoid trailing comma")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")
                      (td :class "org-left" " ")))))
              (section :id "-tilde-d-"
                (hgroup
                  (span)
                  (h5
                    (code "TILDE D")))
                (div :class "outline-text-6" :id "text-1-7-8-3-1"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(defmethod format-money (stream (this us-dollar))
  (let ((total-cents (amount this)))
    (cond ((< total-cents 0)
           (let* ((total-cents (- total-cents))
                  (dollars (floor total-cents 100))
                  (cents (mod total-cents 100)))
             (format stream \"-~a~:D.~2,'0D\" (currency-sign this) dollars cents)))
          (t
           (let ((dollars (floor total-cents 100))
                 (cents (mod total-cents 100)))
             (format stream \"~a~:D.~2,'0D\" (currency-sign this) dollars cents))))))"))
                  (p "For small, negative values of "
                    (code "total-cents") ", "
                    (code "floor") " will be inaccurate ("
                    (code "(floor
-5 100)") " will return "
                    (code "-1") " rather than "
                    (code "0") "), so we make all negative integers
positive before formatting them, adding the minus-sign back at the beginning of
the format string.")
                  (p
                    (code "~2,'0D") " looks crazy, I know. Format directives can be modified by optional
parameters–separated by commas–and by modifiers "
                    (code "COLON") " or "
                    (code "AT-SIGN") ". For"
                    (code "TILDE D") ", we have four possible parameters:")
                  (ol :class "org-ol"
                    (li "mincol")
                    (li "padchar")
                    (li "comma")
                    (li "interval"))
                  (p "Let's take a look at them in action:")
                  (div :class "code-block"
                    (pre :class "code-block-source-pre"
                      (code :class "code-block-source lisp" "(let ((small-num 7)
      (big-num 987654321))
  (format t \"~%~40a | ~d | ~d\"                          \"no modifications\"
                  small-num big-num)
  (format t \"~%~40a | ~4d | ~4d\"                        \"mincol of 4\"
                       small-num big-num)
  (format t \"~%~40a | ~4,'xd | ~4,'xd\"                  \"mincol padding using
 character x\"  small-num big-num)
  (format t \"~%~40a | ~4,'x:d | ~4,'x:d\"                \"colon added\"
                       small-num big-num)
  (format t \"~%~40a | ~4,'x,'x:d | ~4,'x,'x:d\"          \"commas replaced with x\"
            small-num big-num)
  (format t \"~%~40a | ~4,'x,'x,1:d | ~4,'x,'x,1:d\"      \"comma interval set to 1\"
           small-num big-num)
  (format t \"~%~40a | ~4,'x@d | ~4,'x,'x,1@d\"           \"colon replaced with
 at-sign\"       small-num big-num)
  (format t \"~%~40a | ~4,'x@:d | ~4,'x,'x,1@:d\"         \"at-sign and colon
 combined\"        small-num big-num))"))
                    (pre :class "result"
                      (code :class "code-block-result"
                        (span :class "code-block-result-label" "Result") "no modifications                         | 7 | 987654321
mincol of 4                              |    7 | 987654321
mincol padding using character x         | xxx7 | 987654321
colon added                              | xxx7 | 987,654,321
commas replaced with x                   | xxx7 | 987x654x321
comma interval set to 1                  | xxx7 | 9x8x7x6x5x4x3x2x1
colon replaced with at-sign              | xx+7 | +987654321
at-sign and colon combined               | xx+7 | +9x8x7x6x5x4x3x2x1 => NIL")))
                  (p "It should be a bit easier now to understand the format string for"
                    (code "format-money") ": "
                    (code "~a") ", just display the "
                    (code "currency-sign") ". "
                    (code "~:D") ", add commas to
dollars side where appropriate. "
                    (code ".") ", we add the dot. "
                    (code "~2,'0D") ", set the "
                    (code "mincol") "to 2 and pad empty space with 0s. This ensures that "
                    (code "(usd 5)") " returns "
                    (code "$0.05") "and not "
                    (code "$0.5") "."))))))
        (section :id "tic-tac-toe"
          (hgroup
            (span)
            (h2 "Tic-tac-toe"))
          (div :class "outline-text-3" :id "text-1-8"
            (p "For this project we'll be making a game of tic-tac-toe complete with a computer
opponent. Tic-tac-toe is a relatively common coding project. The version we'll
be making is by David Touretzky.")
            (p "The purpose of this project is to help you get a feel for what editing code is
like in Emacs. You'll have a chance to try out structural editing (if you want),
and you'll get some practice with tricky forms like "
              (code "let") " and "
              (code "cond") " that use an
abundance of parentheses.")
            (p "Common Lisp code tends to be wider than other languages. This is partially
because of the culture of using unabbreviated names, but also because of Lisp's
functional programming roots. As a result, Lisp programmers often make smaller
abstractions to make code more readable and fit into the line-width limits of
the editor. This project will give you some exposure to both the \"problem\" of
wider code and the solutions for it.")
            (p "In order to get the most out of this project, you should follow along and
actually type out the code in Emacs, rather than copying and pasting."))
          (section :id "choose-data-representation"
            (hgroup
              (span)
              (h3 "Choose data representation"))
            (div :class "outline-text-4" :id "text-1-8-1"
              (p "Tic-tac-toe works like this:")
              (ul :class "org-ul"
                (li "There are two players.")
                (li "Each take turns putting their \"piece\" on the board–either an X or an O.")
                (li "If either player gets three of their pieces in a row vertically, horizontally,
or diagonally, they win.")
                (li "If all spaces are filled without any player winning, it's a draw."))
              (p "The game is simple, so the data representation can be simple.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defvar *board*)

(defun reset-board ()
  (setf *board* (list 'board
                      0 0 0
                      0 0 0
                      0 0 0)))

(defparameter *player-one* 1)
(defparameter *player-two* 10)"))
              (p "The board is a flat list of 0's representing empty spaces. "
                (code "board") " is a filler
symbol to make later code more intuitive to understand. We use "
                (code "setf") " to assign
the globally scoped "
                (code "*board*") " variable to the value "
                (code "'(board 0 0 0 0 0 0 0 0)") ".")
              (p "We represent player \"pieces\" as 1 and 10.")
              (ul :class "org-ul"
                (li "Type the above code into a buffer named "
                  (code "tic-tac-toe.lisp") ".")
                (li "Compile each form individually with "
                  (code "Sly->Compilation->Compile Defun"))
                (li "Without typing anything more, practice evaluating the symbols "
                  (code "*player-one*") "and "
                  (code "*player-two*") " with "
                  (code "Sly->Evaluation->Eval Defun") ".")
                (li "Type "
                  (code "(reset-board)") " in the buffer and then evaluate that form. What is the
value of "
                  (code "*board*") "?"))))
          (section :id "write-functions-for-manipulating-data"
            (hgroup
              (span)
              (h3 "Write functions for manipulating data"))
            (div :class "outline-text-4" :id "text-1-8-2"
              (p "Now that we have our data representations settled, we need a way to manipulate
it.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun place-piece (board piece position)
  (setf (nth position board) piece)
  board)"))
              (p "Using "
                (code "setf") " with "
                (code "nth") " here is similar to doing something like "
                (code "var[n] =
my-value") " in other languages. You "
                (code "setf") " to a "
                (code "place") ", such as a variable, a
hashtable key, list position, array index, etc.")
              (p "Here we assign the "
                (code "place") " in our "
                (code "*board*") " to the value of "
                (code "piece") ". The last
value returned by the last form evaluated in a function becomes that function's
return value. We return the "
                (code "board") " to be able to show the updated board to the
players.")
              (p "We also need a way to map positions on the board to all possible winning
positions–Touretzky calls them triplets.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defparameter *triplets* '((1 2 3) (4 5 6) (7 8 9) 
                           (1 4 7) (2 5 8) (3 6 9) 
                           (1 5 9) (3 5 7)))       "))
              (p "The first three triplets are horizontal winning positions, the second three are
vertical, and the last two are diagonal.")))
          (section :id "write-game-logic"
            (hgroup
              (span)
              (h3 "Write game logic"))
            (div :class "outline-text-4" :id "text-1-8-3"
              (p "Next, we need a way to calculate the state of those triplets.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet) (sum-triplet board triplet)) *triplets*))"))
              (p
                (code "mapcar") " is one of the many built-in functions that takes a function as an argument."
                (code "mapcar") " will apply the function to each item in the sequence and collect them
into a list, returning the list.")
              (p "Let's test it out by placing some pieces manually.")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(place-piece *board* *player-one* 1)
(place-piece *board* *player-two* 2)
(place-piece *board* *player-one* 5)
(place-piece *board* *player-two* 9)
(place-piece *board* *player-one* 7)
(place-piece *board* *player-two* 3)
(place-piece *board* *player-one* 4)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(BOARD 1 10 10 1 1 0 1 0 10)")))
              (p "Now let's test "
                (code "compute-triplet") ".")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(compute-sums *board*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(21 2 11 3 11 20 12 12)")))
              (p "We can see now that player one, represented as 1s on the board, occupies all
three spaces in a triplet. We have a winner, but our program doesn't know that
yet. Let's add win and tie detection.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *player-one*) sums)
        (member (* 3 *player-two*) sums))))

(defun tie-p (board)
  (not (member 0 board)))"))
              (p "Test them on the current board:")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(winner-p *board*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "(3 11 20 12 12)")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(tie-p *board*)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "NIL")))
              (p
                (code "member") " is called a "
                (code "semi-predicate") ": it searches a sequence like "
                (code "sums") " for an
item like "
                (code "(* 3 *player-one*)") ". If none is found, it returns "
                (code "nil") ". If one is
found, however, it doesn't return "
                (code "t") "; it returns a list of the item plus the"
                (code "rest") " of the list after the item.")
              (p "We can add pieces to the board, calculate the state of the board, and detect a
winner. If we make an interface for two human players, we can have a game.")))
          (section :id "representing-data-to-the-player"
            (hgroup
              (span)
              (h3 "Representing data to the player"))
            (div :class "outline-text-4" :id "text-1-8-4"
              (p "We need a way to show the board to players using "
                (code "format") ". Instead of trying to
do everything at once, we'll break it down into pieces, starting with converting
player pieces from numbers to letters")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun convert-to-letter (piece)
  (ecase piece
    (0  \" \")
    (1  \"X\")
    (10 \"O\")))

(defun opponent (piece)
  (if (= piece 1)
      10
      1))"))
              (p "Test "
                (code "convert-to-letter") ":")
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(convert-to-letter 1)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "X")))
              (div :class "code-block"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(convert-to-letter 10)"))
                (pre :class "result"
                  (code :class "code-block-result"
                    (span :class "code-block-result-label" "Result") "O")))
              (p "Now let's print a row from the board.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun print-row (x y z)
  (format t \"~& ~a | ~a | ~a ~%\" (convert-to-letter x) (convert-to-letter y) (convert-to-letter
 z)))"))
              (p "Now let's print the board.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun print-board (board)
  (format t \"~&\")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t \"--------\")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t \"--------\")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t \"~&=&\"))"))
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(print-board *board*)"))
              (p "Here's where having "
                (code "board") " as a filler item in the "
                (code "*board*") " list is useful:
the calls to "
                (code "nth") " here are intuitive to understand.")))
          (section :id "getting-user-input"
            (hgroup
              (span)
              (h3 "Getting user input"))
            (div :class "outline-text-4" :id "text-1-8-5"
              (p "Now we need to get player input. We need to ensure that our input is well-formed
and that the move from the player is legal.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun read-legal-move (piece board)
  (let ((move (read)))
    (cond ((not (integerp move))                                                    ; Condition
           (format t \"~&Moves must be a number between 1-9. Your move: ~a~%\" move)  ; Code
                                                                                    ; to
                                                                                    ; run
                                                                                    ; if
                                                                                    ; condition
                                                                                    ; is
                                                                                    ; true.
           (print-board board)                                                      ; Multiple
                                                                                    ; forms
                                                                                    ; can
                                                                                    ; be
                                                                                    ; evaluated
           (read-legal-move piece board))                                           ; after
                                                                                    ; the
                                                                                    ; condition.
          ((not (and (>= move 1) (>= 9 move)))
           (format t \"~&Choose a space between 1 and 9.\")
           (print-board board)
           (read-legal-move piece board))
          ((/= (nth move board) 0)
           (format t \"~&You must place a piece on an empty space.\")
           (print-board board)
           (read-legal-move piece board))
          (t move))))"))
              (p "The function "
                (code "read") " is how we request user input from the REPL. In the "
                (code "cond") "form–which can look pretty hairy to our new Lisp brothers–we run a few checks."
                (code "integerp") " is a "
                (code "predicate") " function (with names typically ending with "
                (code "p")
                (code "-p") ")
that checks if the user input is an integer. If the player input isn't a number,
we tell them we need a number, print the board, and let the player try again.")
              (p "The next check makes sure that the number the user inputted was a number between
1 and 9.")
              (p "Finally, we need to check that the space chosen by the player is empty.")
              (p "If the "
                (code "move") " passes all the checks, then the "
                (code "cond") " will evaluate the final
form "
                (code "(t move)") ". This is the conventional way of providing a default branch in
the "
                (code "cond") " if all other conditions return "
                (code "nil") ". In this instance, we just
return "
                (code "move") ".")
              (p "Now we can make a move.")
              (pre :class "code-block-source-pre"
                (code :class "code-block-source lisp" "(defun move (piece board)
  (format t \"~&It's ~a's turn.~%\" (convert-to-letter piece))
  (print-board board)
  (let* ((move (read-legal-move piece board))
         (updated-board (place-piece board piece move))
         (winner (winner-p updated-board)))
    (cond (winner
            (format t \"~a wins!\" (convert-to-letter (/ (first winner) 3))))
          ((tie-p updated-board)
            (format t \"It's a tie!\"))
          (t (move (opponent piece) board)))))

(defun play-game ()
  (reset-board)
  (print \"X goes first\")
  (move *player-one* *board*))"))
              (p
                (code "let*") " is how we bound locally-scoped variables. "
                (code "let*") ", unlike the regular"
                (code "let") ", can bind variables to values that were computed earlier in the form. We
pass "
                (code "move") " to "
                (code "place-piece") " and bind "
                (code "updated-board") " to the value returned. If
we used "
                (code "let") " instead, we would get an error.")))
          (section :id "write-computer-moving-logic"
            (hgroup
              (span)
              (h3 "Write computer moving logic"))
            (div :class "outline-text-4" :id "text-1-8-6"
              (p "At this point, the human-vs-human version of the game is feature-complete. What
we want now is to add a computer opponent.")
              (p "At minimum, the computer needs to do the following:")
              (ul :class "org-ul"
                (li "Choose a strategy"
                  (ul :class "org-ul"
                    (li "If there is a winning move, choose it.")
                    (li "If there is a blocking move, choose it.")
                    (li "Otherwise, take a random position.")))))
            (section :id "choose-a-strategy"
              (hgroup
                (span)
                (h4 "Choose a strategy"))
              (div :class "outline-text-5" :id "text-1-8-6-1"
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun choose-move (board)
  (or (make-three-in-a-row board)       ; Take the winning move.
      (block-opponent-win board)        ; Block the opponent.
      (take-random-position board)))    ; Take a random position."))
                (p
                  (code "or") " will evaluate its arguments in order. It will stop evaluation on the first
argument that returns a non-nil value.")))
            (section :id "finding-a-winning-move"
              (hgroup
                (span)
                (h4 "Finding a winning move"))
              (div :class "outline-text-5" :id "text-1-8-6-2"
                (p "The computer needs to know if there it has a potential win. There is a potential
win if any of the triplets sum to 20.")
                (p "First, let's make a test board.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defparameter *test-board* '(board
                             10 1 0
                             10 1 0
                             0 0 0))"))
                (p "What we want to is to find a triplet that sums to 20.")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet)))
 *triplets*)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "(1 4 7)")))
                (p
                  (code "find-if") " takes a predicate function and returns the first value in a sequence
that evaluates to "
                  (code "t") " when the predicate is applied to it. It iterates over"
                  (code "*triplets*") ", and tests if any of the triplets on the board sum up to 20.")
                (p "If we find a triplet with a winning move, we want to return the position on the
board to take. That means we need to find the element in the winning triplet
that is 0.")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= 0 (nth element *test-board*)))
         (find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet)))
 *triplets*))"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "7")))
                (p "Since we need to do the same thing to check if we need to block…")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= 0 (nth element *test-board*)))
         ;; Notice (= 2 ...), not (= 20 ...)
         (find-if #'(lambda (triplet) (= 2 (sum-triplet *test-board* triplet)))
 *triplets*))"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "8")))
                (p "…we can make this one function.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun win-or-block (board target-sum)
  (let ((target-triplet (find-if #'(lambda (triplet)
                                    ;; NOTE: bug left purposefully for teaching
                                    ;; purposes
                                    (= target-sum (sum-triplet *test-board*
 triplet)))
                                *triplets*)))
    (if target-triplet
        (find-if #'(lambda (element) (= (nth element board) 0)) target-triplet))))"))
                (p "Test it. Passing "
                  (code "2") " checks if we need to block; passing "
                  (code "20") " checks if we can win:")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(win-or-block *test-board* 2)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "8")))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(win-or-block *test-board* 20)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "7")))
                (aside :class "book-aside"
                  (p "Lisp code often needs to be read \"inside-out\". The nested "
                    (code "find-if") " forms above
demonstrate that. The inner "
                    (code "find-if") " returned a triplet from the list of"
                    (code "*triplets*") ", then the outer "
                    (code "find-if") " returned an item from the triplet. This
can be confusing if you're not used reading code this way.")
                  (p "By first assigning "
                    (code "target-triplet") " to the triplet that sums to "
                    (code "target-sum") ", we
reverse the order of the code we read first, making it clearer. It also allows
us to check if a "
                    (code "target-triplet") " even exists."))))
            (section :id "adding-strategies"
              (hgroup
                (span)
                (h4 "Adding strategies"))
              (div :class "outline-text-5" :id "text-1-8-6-3"
                (p "Now that we have a function that can find spaces to either win or block a win,
we can call it with the appropriate target-sum in our win and block strategies.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun make-three-in-a-row (board)
  (let ((move (win-or-block board (* 2 *player-two*))))
    (when move
        (list move (format nil \"~&I see a winning move at ~a.~%\" move)))))

(defun block-opponent-win (board)
  (let ((move (win-or-block board (* 2 *player-one*))))
    (when move
        (list move (format nil \"~&Danger! Loss imminent! Moving to block at
 ~a.~%\" move)))))"))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(make-three-in-a-row *test-board*)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "(7 \"I see a winning move at 7. \")")))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(block-opponent-win *test-board*)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "(8 \"Danger! Loss imminent! Moving to block at 8. \")")))
                (p "We will return a list with the move and also the strategy employed.")
                (aside :class "book-aside"
                  (p "Notice that we use "
                    (code "when") ". We could use "
                    (code "if") " instead, but "
                    (code "when") " communicates
more precisely what we mean. If we don't need the optional \"else\" branch of the"
                    (code "if") " form, then it's better style to use "
                    (code "when") " or "
                    (code "unless") "."))
                (p "If the computer is going first, it should just take a random position.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun take-random-position (board)
  (let ((move (+ 1 (random 9))))
    (cond ((= 0 (nth move board))
           (place-piece board *player-two* move)
           (list move \"Picking random position.\"))
          (t
           (take-random-position board)))))"))
                (p
                  (code "random") " will choose a semi-random value "
                  (b "between") " 0 and the argument passed.
Unfortunately, there is no way to specify the beginning of the \"range\" nor is
the number random enough to be used for security purposes. Since we need to pick
a number between 1 (remember the filler "
                  (code "BOARD") " symbol) and 9 inclusive, we add
1 to the result.")
                (p "Since the random position chosen may be occupied, the catchall branch simply
makes a recursive call to "
                  (code "take-random-position") " to try again.")
                (p "Right now, "
                  (code "move") " calls itself with the opponent player. We'll need a"
                  (code "human-move") " and "
                  (code "computer-move") " to give us the ability to let the computer
choose.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun human-move (board)
  (format t \"~&It's ~a's turn.~%\" (convert-to-letter *player-one*))
  (print-board board)
  (let* ((move (read-legal-move *player-one* board))
         (updated-board (place-piece board *player-one* move))
         (winner (winner-p updated-board)))
    (cond (winner
           (print-board updated-board)
           (format t \"~a wins!\" (convert-to-letter (/ (first winner) 3))))
          ((tie-p updated-board)
           (print-board updated-board)
           (format t \"It's a tie!\"))
          (t (computer-move board)))))

(defun computer-move (board)
  (format t \"~&It's ~a's turn.~%\" (convert-to-letter *player-two*))
  (print-board board)
  (let* ((move-and-strategy (choose-move board))
         (move (first move-and-strategy))
         (strategy (second move-and-strategy))
         (updated-board (place-piece board *player-two* move))
         (winner (winner-p updated-board)))
    (format t \"~&My move: ~a~%\" move)
    (format t \"~&My strategy: ~a~%\" strategy)
    (cond (winner
           (print-board updated-board)
           (format t \"~a wins!\" (convert-to-letter (/ (first winner) 3))))
          ((tie-p updated-board)
           (print-board updated-board)
           (format t \"It's a tie!\"))
          (t (human-move board)))))

(defun play-game-with-computer ()
  (reset-board)
  (if (y-or-n-p \"Do you want to go first, human?\")
      (human-move *board*)
      (computer-move *board*)))"))
                (p "The "
                  (code "y-or-n-p") " function takes user input like "
                  (code "read") " does, but it only accepts
two possible inputs: "
                  (code "y") " or "
                  (code "n") ", meaning yes or no. It evaluates to "
                  (code "t") " for yes
and "
                  (code "nil") " for no.")))
            (section :id "fixing-a-bug"
              (hgroup
                (span)
                (h4 "Fixing a bug"))
              (div :class "outline-text-5" :id "text-1-8-6-4"
                (p "Try playing with the computer. You'll notice that the computer is always going
with the "
                  (code "make-three-in-a-row") " strategy, even if you let it go first.")
                (p "In the "
                  (code "win-or-block") " function, there is a bug: we forgot to remove"
                  (code "*test-board*") " from the first "
                  (code "find-if") ".")
                (p "Try this: Start a game, let the computer go first. It should tell you "
                  (code "\"My
strategy: I see a winning move at\"") ". We expect it to simply pick a random space.")
                (p
                  (b "While the game is still running") ", update and compile the "
                  (code "win-or-block") " function.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun win-or-block (board target-sum)
  (let ((target-triplet (find-if #'(lambda (triplet)
                                    ;; NOTE: *test-board* -> board
                                    (= target-sum (sum-triplet board triplet)))
                                *triplets*)))
    (if target-triplet
        (find-if #'(lambda (element) (= (nth element board) 0)) target-triplet))))"))
                (p "After compiling, "
                  (i "continue the game") ". Make your move. You should now see the
computer choose a random space.")
                (p "This small interaction demonstrates a big feature of Lisp: "
                  (b "We can update code
as it is running, without restarting it.") " Whether we are updating a small
tic-tac-toe game, "
                  (a :href "https://www.youtube.com/watch?v=gdjkSkRFcr4" "a program for music and visualization generation") ", or a running
web app, we can update it while it's running."))))
          (section :id "add-computer-strategies"
            (hgroup
              (span)
              (h3 "Add computer strategies"))
            (div :class "outline-text-4" :id "text-1-8-7"
              (p "The simple data representation we chose at the beginning has made it fairly easy
to get a simple human-vs-computer tic-tac-toe game made. However, the computer
is very dumb. It doesn't think ahead and doesn't recognize different human
strategies. Let's change that.")
              (p "Tic-tac-toe has a rather unfun characteristic: if played well by both players,
every game will end in a draw.")
              (p "For our computer strategies, then, we are going to be mostly reacting to the
human player, recognizing different strategies and countering them perfectly. By
the end, we'll totally drain what little fun can be had from the game. But the
coding will be fun, so let's go."))
            (section :id "beyond-triplets"
              (hgroup
                (span)
                (h4 "Beyond triplets"))
              (div :class "outline-text-5" :id "text-1-8-7-1"
                (p "There are two strategies you can employ that can guarantee a victory if the
opponent doesn't react correctly: the "
                  (i "squeeze play") " and a "
                  (i "two-on-one play") ".")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defparameter *squeeze* (list 'board 1 0 0 0 10 0 0 0 1))
(print-board *squeeze*)"))
                (p "The squeeze happens when one player takes two corners and one player takes the
middle. The two-on-one happens when one player takes the corner and the middle,
and the other player takes a corner as well. In both scenarios, O is guaranteed
to lose if X plays properly.")
                (p "To avoid these two scenarios, the computer must recognize possible strategies
being deployed against it and react correctly.")
                (ul :class "org-ul"
                  (li "If the computer is in the middle between two human pieces, it's a possible
squeeze."
                    (ul :class "org-ul"
                      (li "To counter, take a side: don't take a corner.")))
                  (li "If the computer is in a corner and the human has the middle and the corner
lining up his pieces against the computer, it's a possible two-on-one."
                    (ul :class "org-ul"
                      (li "To counter, take a corner: don't take a side."))))
                (p "Right now, the computer reads the board as a list of triplets and identifies
possible wins and danger. But to ensure both players end the game disappointed,
the computer needs to recognize some other characteristics of the board: corners
and sides.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defparameter *corners* '(1 3 7 9))
(defparameter *sides* '(1 2 3 4 6 7 8 9))"))))
            (section :id "detecting-a-squeeze"
              (hgroup
                (span)
                (h4 "Detecting a squeeze"))
              (div :class "outline-text-5" :id "text-1-8-7-2"
                (p "Let's start by detecting a squeeze.")
                (p "First, we need to search the board and cross-reference the triplets, looking for
any triplet with values that reduce to 12")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun detect-squeeze (board)
  (find-if #'(lambda (triplet)
               (= 12 (sum-triplet board triplet)))
           *triplets*))"))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(detect-squeeze *squeeze*)
(detect-squeeze *two-on-one*)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "Both return (1 5 9).")))
                (p "We need to know for sure this is a diagonal triplet, though.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun diagonal-p (triplet)
  (every #'(lambda (item)
             ;; Every item is either a corner or the middle.
             (or (member item *corners*)
                 (= 5 item)))
         triplet))
(defun detect-squeeze (board)
  (find-if #'(lambda (triplet)
               ;; Add AND and DIAGONAL-P
               (and (= 12 (sum-triplet board triplet))
                    (diagonal-p triplet)))
           *triplets*))"))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(detect-squeeze *squeeze*)
(detect-squeeze *two-on-one*)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "Both still return (1 5 9).")))
                (p
                  (code "every") " runs a predicate function on a sequence and returns "
                  (code "t") " if the predicate
evaluated to "
                  (code "t") " for every element of the sequence. In "
                  (code "diagonal-p") ", it checks
if every element of the triplet is either a corner or the middle space.")
                (p "We also need to know who is in the middle.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun human-in-middle-p (board)
  (= (nth 5 board) *player-one*))

(defun detect-squeeze (board target-sum)
  (find-if #'(lambda (triplet)
               (and (= (sum-triplet board triplet) target-sum)
                    (diagonal-p triplet)
                    (not (human-in-middle-p board))))
           *triplets*))"))
                (p "Now the two boards produce different results:")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(detect-squeeze *squeeze*)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "(1 5 9)")))
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(detect-squeeze *two-on-one*)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "NIL")))
                (p "Finally, we just need to be sure that we're at the beginning of the game. The
player may have already blocked the squeeze or two-on-one, or the game may have
otherwise progressed beyond the first diagonals.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun side-empty-p (board)
  (find-empty-position board *sides*))

(defun find-empty-position (board search-area)
  (find-if #'(lambda (x) (= 0 (nth x board))) search-area))

(defun detect-squeeze (board target-sum)
  (let ((squeeze-p
          (find-if #'(lambda (triplet)
                       (and (= (sum-triplet board triplet) target-sum)     ; Is
                                                                           ; the
                                                                           ; triplet
                                                                           ; the
                                                                           ; target-sum?
                            (diagonal-p triplet)                           ; Is
                                                                           ; the
                                                                           ; triplet
                                                                           ; a
                                                                           ; diagonal?
                            (not (human-in-middle-p board))                ; Is
                                                                           ; the
                                                                           ; human
                                                                           ; not
                                                                           ; in
                                                                           ; the
                                                                           ; middle?
                            (side-empty-p board)))                         ; Are
                                                                           ; all
                                                                           ; the
                                                                           ; sides
                                                                           ; empty?
                   *triplets*)))
    (if squeeze-p
        (find-empty-position board *sides*))))"))
                (p "If we see a squeeze, we need to counter. To counter a squeeze, we need to take a
side (not a corner). So we look for an empty position in one of the "
                  (code "*sides*") ".")
                (p "If we test "
                  (code "detect-squeeze") ", we should get an empty space on one of the sides:")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(detect-squeeze *squeeze* 12)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "2")))
                (p "2 is the space between corner 1 and 3, so it's given an expected return value.")))
            (section :id "detecting-a-two-on-one"
              (hgroup
                (span)
                (h4 "Detecting a two-on-one"))
              (div :class "outline-text-5" :id "text-1-8-7-3"
                (p
                  (code "detect-two-on-one") " is nearly identical to "
                  (code "detect-squeeze") ":")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun detect-two-on-one (board target-sum)
  (let ((two-on-one-p
          (find-if #'(lambda (triplet)
                       (and (= (sum-triplet board triplet) target-sum)
                            (diagonal-p triplet)
                            (human-in-middle-p board)  ; Human in the middle?
                            (side-empty-p board)))
                   *triplets*)))
    (when two-on-one-p
      (find-empty-position board *corners*)))) ; Look for an empty space in the
                                               ; corners."))
                (p "Test both detection functions against both boards:")
                (div :class "code-block"
                  (pre :class "code-block-source-pre"
                    (code :class "code-block-source lisp" "(detect-squeeze *two-on-one* 12)
(detect-squeeze *squeeze* 12)
(detect-two-on-one *two-on-one* 12)
(detect-two-on-one *squeeze* 12)"))
                  (pre :class "result"
                    (code :class "code-block-result"
                      (span :class "code-block-result-label" "Result") "detect-squeeze on two-on-one: NIL
detect-squeeze on squeeze: 2
detect-two-on-one on two-on-one: 3
detect-two-on-one on squeeze: NIL")))
                (p "With that, we just need a couple of small wrappers to encapsulate our
strategies.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun block-squeeze-play (board)
  (let ((move (detect-squeeze board (+ (* *player-one* 2) *player-two*))))
    (when move
      (list move \"I'm being squeezed! Taking side space.\"))))

(defun block-two-on-one-play (board)
  (let ((move (detect-two-on-one board (+ (* *player-one* 2) *player-two*))))
    (when move
      (list move \"It's two-on-one! Taking corner space.\"))))"))))
            (section :id "updating-choose-move-"
              (hgroup
                (span)
                (h4 "Updating "
                  (code "choose-move")))
              (div :class "outline-text-5" :id "text-1-8-7-4"
                (p "Finally, we update "
                  (code "choose-move") " by adding our two new strategies.")
                (pre :class "code-block-source-pre"
                  (code :class "code-block-source lisp" "(defun choose-move (board)
  (or (make-three-in-a-row board)
      (block-squeeze-play board)         ; Added
      (block-two-on-one-play board)      ; Added
      (block-opponent-win board)
      (take-random-position board)))")))))
          (section :id "summary"
            (hgroup
              (span)
              (h3 "Summary"))
            (div :class "outline-text-4" :id "text-1-8-8"
              (p "Now you have a finished AI that will force a draw every game. Try playing it and
enjoy infinite draws.")
              (p "With this, you have gotten your first experience writing a program in Almighty
Common Lisp. It may have been painful: if you aren't using structural editing
modes like "
                (code "lispy-mode") " or "
                (code "paredit-mode") ", you had to make sure to keep your
parentheses balanced and moving code around may have been harder than you
expected. However, with practice, you'll be stacking parens like an expert."))))
        (section :id "matters-of-style"
          (hgroup
            (span)
            (h2 "MATTERS OF STYLE"))
          (div :class "outline-text-3" :id "text-1-9")
          (section :id "reading-lisp-code"
            (hgroup
              (span)
              (h3 "Reading Lisp Code"))
            (div :class "outline-text-4" :id "text-1-9-1"
              (p "\"inside-out\", using "
                (code "let") " to change order of code.")
              (aside :class "book-aside"
                (p "Lisp code often needs to be read \"inside-out\". The nested "
                  (code "find-if") " forms above
demonstrate that. The inner "
                  (code "find-if") " returned a triplet from the list of"
                  (code "*triplets*") ", then the outer "
                  (code "find-if") " returned an item from the triplet. This
can be confusing if you're not used reading code this way.")
                (p "By first assigning "
                  (code "target-triplet") " to the triplet that sums to "
                  (code "target-sum") ", we
reverse the order of the code we read first, making it clearer. It also allows
us to check if a "
                  (code "target-triplet") " even exists."))))
          (section :id "good-code-communication"
            (hgroup
              (span)
              (h3 "Good Code Communication"))
            (div :class "outline-text-4" :id "text-1-9-2"
              (p "be specific with "
                (code "if/when/unless/cond") ". "
                (code "*global-variables*"))))))))
  (button :class "toc-viewer" "TOC")
  (script "HighlightLisp.highlight_auto();"))
