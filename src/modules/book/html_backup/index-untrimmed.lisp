
(body
  (div :class "sidebar-container"
    (div :class "book-navigation"
      (h1 :class "book-navigation__book-title" "Almighty Lisp: The Essentials")
      (p :class "book-navigation__chapter-name-heading"
        (span "Almighty Lisp: The Essentials"))

      (nav
        (ul))
      (div :class "book-navigation__previous-and-next")))
  (main :class "sidebar-main"
    (article :class "book"
      (div :id "table-of-contents" :role "doc-toc"
        (h2 "Table of Contents")
        (div :id "text-table-of-contents" :role "doc-toc"
          (ul
            (li
              (a :href "#org90151c1"
                (span :class "todo TODO" "TODO") " META")
              (ul
                (li
                  (a :href "#org4280390" "WHY I WROTE THIS BOOK"))
                (li
                  (a :href "#org7f05309"
                    (span :class "todo TODO" "TODO") " ALMIGHTY"))))
            (li
              (a :href "#org9aaa190" "EMACS SETUP & USE")
              (ul
                (li
                  (a :href "#org6d9f85c" "PURPOSE"))
                (li
                  (a :href "#orgca51cd1" "DOOM EMACS INSTALL & SETUP")
                  (ul
                    (li
                      (a :href "#orgfff619f" "MacOS"))
                    (li
                      (a :href "#orgda46584" "Remap Capslock to Control"))))
                (li
                  (a :href "#org694f452" "EMACS DICTIONARY")
                  (ul
                    (li
                      (a :href "#org1e2e2df" "Concepts"))
                    (li
                      (a :href "#org005d8f4" "Keybinding Basics"))))
                (li
                  (a :href "#orgb32f55b" "SURVIVAL EMACS")
                  (ul
                    (li
                      (a :href "#org3b1f077" "Creating & Opening File Buffers"))
                    (li
                      (a :href "#orgb346030" "Reading The Modeline"))
                    (li
                      (a :href "#orgc68effb" "Inserting Text"))
                    (li
                      (a :href "#org1051bf2" "Saving Files"))
                    (li
                      (a :href "#org48bc1c7" "View A List Of Open Buffers"))
                    (li
                      (a :href "#org765f78e" "Kill/Close A Buffer"))
                    (li
                      (a :href "#org28643fa" "Strange Behavior"))
                    (li
                      (a :href "#org8e3b633" "Speeding Up w/Keybindings"))))))
            (li
              (a :href "#org6798ebc"
                (span :class "todo TODO" "TODO") " LISP SETUP & USE")
              (ul
                (li
                  (a :href "#org85b8a9d" "INSTALLING LISP"))
                (li
                  (a :href "#orgb0b83ef" "SURVIVAL LISP COMMANDS")
                  (ul
                    (li
                      (a :href "#orge37984d" "Compiling/Evaluating Lisp Code From the Buffer"))
                    (li
                      (a :href "#orgf7f11d1" "Lisp Images"))
                    (li
                      (a :href "#orgfaaa93e" "Using the REPL"))
                    (li
                      (a :href "#orgd30dcb1" "Structural Editing"))
                    (li
                      (a :href "#org643950c" "The Lisp Debugger"))))))
            (li
              (a :href "#orgfff93b6" "LISP FUNDAMENTALS")
              (ul
                (li
                  (a :href "#org284d65f" "SYNTAX & GRAMMAR")
                  (ul
                    (li
                      (a :href "#org8281000" "S-expressions"))
                    (li
                      (a :href "#orgd647c46" "Prefix Notation"))
                    (li
                      (a :href "#org3579b50" "More complicated s-expression"))
                    (li
                      (a :href "#orge48b2d4" "Evaluation Rules"))))
                (li
                  (a :href "#orgf010bda" "SYMBOLS")
                  (ul
                    (li
                      (a :href "#orgc091151" "Symbol Names"))
                    (li
                      (a :href "#org4b67934" "Introducing Global Variables"))
                    (li
                      (a :href "#org1170203" "Introducing Local Variables"))
                    (li
                      (a :href "#orgf6ee416" "More than just variables"))))
                (li
                  (a :href "#org533e73e" "FUNCTIONS")
                  (ul
                    (li
                      (a :href "#org83e10ee" "Defining Named Functions"))
                    (li
                      (a :href "#orgc316852" "Lambda List"))
                    (li
                      (a :href "#org282882a" "Parameters"))
                    (li
                      (a :href "#orgf32489b" "Return Values"))
                    (li
                      (a :href "#orge6ed646" "Returning Multiple Values"))
                    (li
                      (a :href "#org230a675" "Binding Multiple Values"))
                    (li
                      (a :href "#org994a697" "Breaking Lists Into Multiple Values"))
                    (li
                      (a :href "#org66b1d30" "Pass by Value"))
                    (li
                      (a :href "#orgabae553" "First-Class Functions"))
                    (li
                      (a :href "#org37ed084" "Anonymous Functions"))))
                (li
                  (a :href "#orgb89a47d" "LISTS")
                  (ul
                    (li
                      (a :href "#orgc9643da" "In The Beginning Was The Cons"))
                    (li
                      (a :href "#orge904a48" "Basic List Functions"))
                    (li
                      (a :href "#org67c832a" "Lists as Trees"))
                    (li
                      (a :href "#org797cf59" "Lists as Tables"))
                    (li
                      (a :href "#orgd831553" "Lists as Sets"))))
                (li
                  (a :href "#org2af26e6" "CONTROL FLOW")
                  (ul
                    (li
                      (a :href "#org32c8be4" "True and False"))
                    (li
                      (a :href "#org3613743" "Equality & Comparison"))
                    (li
                      (a :href "#orged27651" "Logical Operators"))
                    (li
                      (a :href "#org0359b4d" "Conditional Forms"))))
                (li
                  (a :href "#org7cd232a" "ITERATION")
                  (ul
                    (li
                      (a :href "#org81995cb" "Iterating by Looping"))
                    (li
                      (a :href "#orgcf80847" "Iterating by Mapping"))
                    (li
                      (a :href "#org98e68d9" "Iterating by Reducing"))
                    (li
                      (a :href "#org9690f06" "Iterating by Filtering"))
                    (li
                      (a :href "#orge627a8b" "Iterating by Doing"))
                    (li
                      (a :href "#orgfa292b1" "Early Returns"))
                    (li
                      (a :href "#org0b57685"
                        (code "every") ", "
                        (code "some") ", "
                        (code "notevery") ", "
                        (code "notany")))))
                (li
                  (a :href "#orge95946d" "STRINGS & I/O")
                  (ul
                    (li
                      (a :href "#org7545ed6" "What are strings?"))
                    (li
                      (a :href "#org3a11876" "Printing Information to REPL"))
                    (li
                      (a :href "#orgbd5f557" "Using Sequence Operations on Strings"))
                    (li
                      (a :href "#org01b5880" "String Specific Operations"))
                    (li
                      (a :href "#org9b0b753" "Streams"))
                    (li
                      (a :href "#orgf65b6d9" "Writing Files"))
                    (li
                      (a :href "#orgf314f33" "Reading Files"))
                    (li
                      (a :href "#orgc030fbb" "Beyond the Basics w/Files"))
                    (li
                      (a :href "#org0af233a"
                        (code "format")))))
                (li
                  (a :href "#org3a6368d"
                    (span :class "todo TODO" "TODO")
                    (code "CRAPS"))
                  (ul
                    (li
                      (a :href "#org6764e30" "Step 1: Make Dice"))
                    (li
                      (a :href "#orgbddfee3" "Step 2: Defining Win and Lose Conditions"))
                    (li
                      (a :href "#orgd6bd06e" "Step 3: Play"))
                    (li
                      (a :href "#org610e6c2" "Step 4: Continue Rolling for Point"))
                    (li
                      (a :href "#org7d20f7e" "FULL CODE"))))
                (li
                  (a :href "#orge428698"
                    (span :class "todo TODO" "TODO")
                    (code "TIC-TAC-TOE"))
                  (ul
                    (li
                      (a :href "#org31d1f47" "Introduction"))
                    (li
                      (a :href "#org984301e" "Tutorial"))
                    (li
                      (a :href "#org1db6bf0" "Discussion"))))))
            (li
              (a :href "#orge1187eb" "EMACS FUNDAMENTALS")
              (ul
                (li
                  (a :href "#orgd85bae2" "BEYOND SURVIVAL MODE"))
                (li
                  (a :href "#org67e9e16" "SUPPORT LEVEL DISCLAIMER"))
                (li
                  (a :href "#orgc68f28f" "SETTING EXPECTATIONS"))
                (li
                  (a :href "#org0e0cc8b" "TEXT EDITING")
                  (ul
                    (li
                      (a :href "#orgb55d9fc" "Evil Bindings"))
                    (li
                      (a :href "#org824e9a9" "Inserting"))
                    (li
                      (a :href "#org47f1f9e" "Cutting/Copying/Pasting"))
                    (li
                      (a :href "#org61b2893" "Marking Text"))
                    (li
                      (a :href "#orge9f38b6" "Simple Searching"))
                    (li
                      (a :href "#org635017f" "Aborting A command"))
                    (li
                      (a :href "#org7d4eda6" "Undoing An Action"))
                    (li
                      (a :href "#orgfca5385" "Doing An Action Multiple Times"))
                    (li
                      (a :href "#org24669f9" "Multiple Cursors"))
                    (li
                      (a :href "#org1a51890" "Multiediting"))
                    (li
                      (a :href "#orgb081cdf" "Searching & Replacing"))
                    (li
                      (a :href "#org212ec2b" "Running Commands/Functions With No Keybindings"))
                    (li
                      (a :href "#org943ec7c" "Quick Reference Table"))))
                (li
                  (a :href "#org233a8f6" "BUFFER NAVIGATION & MANAGEMENT")
                  (ul
                    (li
                      (a :href "#org3b12a0d" "It All Begins With A Buffer"))
                    (li
                      (a :href "#orgdf4b755" "Opening Buffers"))
                    (li
                      (a :href "#orgd40d004" "Scratch Buffer"))
                    (li
                      (a :href "#orgd4836a9" "Switching Buffers"))
                    (li
                      (a :href "#org465a595" "Closing Buffers"))
                    (li
                      (a :href "#orgbd0533e" "Saving File Buffers"))
                    (li
                      (a :href "#orgc6f7d50" "Getting More Information About A Buffer"))
                    (li
                      (a :href "#org9bc0159" "Quick Reference Table"))))
                (li
                  (a :href "#orgc201bef" "WINDOW NAVIGATION & MANAGEMENT")
                  (ul
                    (li
                      (a :href "#org7acb387" "Getting Familiar With Windows"))
                    (li
                      (a :href "#org46f25b0" "Splitting & Closing Windows"))
                    (li
                      (a :href "#org6686a99" "Moving Between Windows"))
                    (li
                      (a :href "#org7469d03" "Moving Window Positions"))
                    (li
                      (a :href "#orgf7bc50b" "Resizing Windows"))
                    (li
                      (a :href "#org269a561" "Undoing Changes"))
                    (li
                      (a :href "#org068d845" "More About Windows"))
                    (li
                      (a :href "#orga97d930" "Quick Reference Table"))))
                (li
                  (a :href "#org3783f1c" "PROJECT NAVIGATION & MANAGEMENT")
                  (ul
                    (li
                      (a :href "#org221e07e" "Buffer & Window Management w/Projects"))
                    (li
                      (a :href "#orga59b6e8" "Project-Related Packages"))
                    (li
                      (a :href "#org9e434b7" "Creating & Removing Emacs Projects"))
                    (li
                      (a :href "#orge434174" "Switching Between Projects"))
                    (li
                      (a :href "#org0dae8b9" "Saving & Loading Project Workspaces"))
                    (li
                      (a :href "#orgf5eea4e" "Saving Project Files"))
                    (li
                      (a :href "#org8358401" "Switching Project Buffers"))
                    (li
                      (a :href "#orga36788b" "Searching In Projects"))
                    (li
                      (a :href "#org8415182" "Project Tree View"))
                    (li
                      (a :href "#org3389c03" "Quick Reference Table"))))
                (li
                  (a :href "#orgf7e7c14" "LEARNING MORE")
                  (ul
                    (li
                      (a :href "#org90e0399"
                        (code "describe-key")))
                    (li
                      (a :href "#org61285b7"
                        (code "describe-mode")))
                    (li
                      (a :href "#org2b9889b"
                        (code "embark-bindings")))
                    (li
                      (a :href "#org4c491ab"
                        (code "info")))))))
            (li
              (a :href "#org153fd8d" "THE LISP IDE")
              (ul
                (li
                  (a :href "#org34e1c0e" "THE LISP CODING ENVIRONMENT"))
                (li
                  (a :href "#org3a6309e" "SLY BACKTRACE NAVIGATION")
                  (ul
                    (li
                      (a :href "#org35e6da9"
                        (code "sly-db-up") " & "
                        (code "sly-db-down")))
                    (li
                      (a :href "#org327ca3d"
                        (code "sly-db-toggle-details")))
                    (li
                      (a :href "#orgaab90da"
                        (code "sly-db-show-frame-source")))
                    (li
                      (a :href "#org72734f5"
                        (code "sly-db-details-up") " & "
                        (code "sly-db-details-down")))))
                (li
                  (a :href "#org4767d04" "TRACING")
                  (ul
                    (li
                      (a :href "#org19cce56"
                        (code "sly-fancy-trace")))
                    (li
                      (a :href "#org6cbbd9e"
                        (code "sly-trace-dialog-toggle-trace")))))
                (li
                  (a :href "#org602ef47" "STICKERS")
                  (ul
                    (li
                      (a :href "#org050a1a6"
                        (code "sly-stickers-dwim")))
                    (li
                      (a :href "#org2f31631"
                        (code "sly-stickers-replay")))
                    (li
                      (a :href "#orgb17b7b3"
                        (code "sly-stickers-toggle-break-on-stickers") " & "
                        (code "sly-db-step")))
                    (li
                      (a :href "#org54e688e"
                        (code "sly-stepper")))))))
            (li
              (a :href "#org004be2d" "STRUCTURED EDITING")
              (ul
                (li
                  (a :href "#org0a9909d" "RESTRUCTURING CODE WITH EASE")
                  (ul
                    (li
                      (a :href "#org46559e2" "Slurping and barfing"))
                    (li
                      (a :href "#org4b6b0b6" "Dragging forms forward/backward"))))
                (li
                  (a :href "#org6ac65ce" "NAVIGATING THE SEA OF PARENTHESES")
                  (ul
                    (li
                      (a :href "#org0f2bec9" "Generic searching"))
                    (li
                      (a :href "#orge1c03cc" "Tab navigation"))))
                (li
                  (a :href "#orga913c69" "MORE RESTRUCTURING & NAVIGATING TOOLS"))))
            (li
              (a :href "#org12828d5" "BEYOND LISTS")
              (ul
                (li
                  (a :href "#org593ade7" "OOP")
                  (ul
                    (li
                      (a :href "#org42c261d" "What are Common Lisp classes?"))
                    (li
                      (a :href "#org5fc2865" "Class Basics"))
                    (li
                      (a :href "#org3b865fc" "Custom Constructors"))
                    (li
                      (a :href "#org2a72971" "Generic Functions & Methods"))
                    (li
                      (a :href "#org692737a" "Extending OOP Systems"))
                    (li
                      (a :href "#org67fbb3b" "Before, After, & Around Methods"))
                    (li
                      (a :href "#org7a74bf2" "Pretty Printing Objects"))
                    (li
                      (a :href "#org3884624" "Modifying Object Initialization"))
                    (li
                      (a :href "#org2760ca8" "Specializing Methods On Other Values"))))
                (li
                  (a :href "#org606ea16" "HASH-TABLES")
                  (ul
                    (li
                      (a :href "#org9960bda" "What is a hash-table?"))
                    (li
                      (a :href "#org314c1e1" "Making hash-tables"))
                    (li
                      (a :href "#org98467aa" "Adding/Accessing/Modifying items to a hash-table"))
                    (li
                      (a :href "#org2be89e9" "Printing a hash-table"))
                    (li
                      (a :href "#orgd439716" "Using strings as keys"))))
                (li
                  (a :href "#org202a148" "ARRAYS")
                  (ul
                    (li
                      (a :href "#orgdb4fec1" "Differences between arrays and lists"))
                    (li
                      (a :href "#orgeac02df" "When to use arrays"))
                    (li
                      (a :href "#orgd3a2505" "Kinds of arrays"))
                    (li
                      (a :href "#org6cd6c4d" "Vectors"))))
                (li
                  (a :href "#org5fd942f" "STRUCTURES")
                  (ul
                    (li
                      (a :href "#orgaaf5cd6" "What is a structure?"))
                    (li
                      (a :href "#orgaabab85" "Defining structures and making instances"))
                    (li
                      (a :href "#orgd1d1cfd" "Accessing/Modifying slots in a structure instance"))
                    (li
                      (a :href "#org5d3f335" "Printing structures"))
                    (li
                      (a :href "#org2d9cdac" "Structures Vs. Classes"))))))
            (li
              (a :href "#orgeb3270e" "ERRORS & CONDITIONS")
              (ul
                (li
                  (a :href "#org2c9016a" "Signaling Conditions"))
                (li
                  (a :href "#org1266cf8" "Assertions"))
                (li
                  (a :href "#orgca365cf" "Conditions"))
                (li
                  (a :href "#orga89b573" "Restarts"))
                (li
                  (a :href "#org9d2772d" "Handlers"))
                (li
                  (a :href "#orgffd260d"
                    (code "*break-on-signals*")))))
            (li
              (a :href "#orge2102c4" "MACROS")
              (ul
                (li
                  (a :href "#org1f6d7c5" "The Forbidden Fruit"))
                (li
                  (a :href "#orgd32ac6c" "Some Demonstrations")
                  (ul
                    (li
                      (a :href "#orgf448e43" "The "
                        (code "hsx") " macro"))
                    (li
                      (a :href "#org5846e9f" "The "
                        (code "sxql") " macro"))
                    (li
                      (a :href "#org34d7860" "The Coalton language"))
                    (li
                      (a :href "#org94ac892" "Code That Writes Code"))))
                (li
                  (a :href "#org9c16e48" "Understanding Macros")
                  (ul
                    (li
                      (a :href "#org5a0d978" "Compilation Before Evaluation"))))
                (li
                  (a :href "#org6a72e7e" "Defining Macros")
                  (ul
                    (li
                      (a :href "#org0fdbdbf" "Backquotes"))
                    (li
                      (a :href "#orgf5cbac6" "Back to "
                        (code "when-let")))
                    (li
                      (a :href "#orgeca5b0e" "Destructuring Arbitrary List Structures In Parameters"))))
                (li
                  (a :href "#orgf819b25" "Redefining Macros"))
                (li
                  (a :href "#orgeee55c2" "Determining When To Use Macros")
                  (ul
                    (li
                      (a :href "#org0733c8c" "Transformation"))
                    (li
                      (a :href "#org5ca02f1" "Binding"))
                    (li
                      (a :href "#orgea6db82" "Conditional Evaluation"))
                    (li
                      (a :href "#orgc3331d0" "Wrapping An Environment"))))
                (li
                  (a :href "#orgeed7d93" "Determining When Not To Use Macros"))
                (li
                  (a :href "#org1be8a10" "Variable Capture & Hygiene")
                  (ul
                    (li
                      (a :href "#org910a4cc" "Examples Of Macros Vulnerable To Variable Capture"))
                    (li
                      (a :href "#orgdb125eb" "Avoiding Variable Capture"))))))
            (li
              (a :href "#orgae6599d" "ORGANIZING CODE")
              (ul
                (li
                  (a :href "#org9f7ec81" "Packages")
                  (ul
                    (li
                      (a :href "#org7d1311c" "What Are Packages?"))
                    (li
                      (a :href "#orgabc44db" "Defining Packages"))
                    (li
                      (a :href "#org65541b7"
                        (span :class "todo TODO" "TODO") " Using, Importing & Exporting"))
                    (li
                      (a :href "#orgddf5b34" "Shadowing And Conflicts"))
                    (li
                      (a :href "#org622dad1" "Nicknames"))
                    (li
                      (a :href "#org33d3f4e"
                        (code "uiop:define-package")))))
                (li
                  (a :href "#org971ce72" "Systems")
                  (ul
                    (li
                      (a :href "#org8fc2a28" "What Are Systems?"))
                    (li
                      (a :href "#orgc6478d5" "What Is ASDF?"))
                    (li
                      (a :href "#org9b802a5" "Defining Systems"))
                    (li
                      (a :href "#orga9f74bb"
                        (span :class "todo TODO" "TODO") " Loading Systems"))))
                (li
                  (a :href "#org19dedc3" "Styles of Factoring Packages & Systems")
                  (ul
                    (li
                      (a :href "#orge1c7c81" "Mother Of All Package Strategy"))
                    (li
                      (a :href "#org0fba80b" "Multiple Systems Strategy"))
                    (li
                      (a :href "#orga752bd1" "One Package Per File Strategy"))
                    (li
                      (a :href "#org78bbc53" "One System Per Package"))))
                (li
                  (a :href "#org26eeab8" "The Great Debate: Package & System Best Practices")
                  (ul
                    (li
                      (a :href "#orgd7a0961" "Argument in Favor of PIS"))
                    (li
                      (a :href "#orgb37f9e0" "Arguments Against PIS"))
                    (li
                      (a :href "#org2d20d39" "Conclusion"))))))
            (li
              (a :href "#org5cb413a" "THE ECOSYSTEM")
              (ul
                (li
                  (a :href "#org40ec7c8" "QUICKLISP")
                  (ul
                    (li
                      (a :href "#orgb61efea" "Using Quicklisp"))
                    (li
                      (a :href "#org93ccce3" "Local Code"))))
                (li
                  (a :href "#orgaafb773" "ULTRALISP"))
                (li
                  (a :href "#orga6b4ee9" "QLOT"))
                (li
                  (a :href "#org0e8b2e6" "OCICL"))
                (li
                  (a :href "#org8e3c0c6" "VEND"))))
            (li
              (a :href "#org91f60c3"
                (span :class "todo TODO" "TODO") " PROJECTS")
              (ul
                (li
                  (a :href "#orgdb7a798"
                    (code "ALMIGHTY-MONEY"))
                  (ul
                    (li
                      (a :href "#orgfad53b3" "Introduction"))
                    (li
                      (a :href "#orgb6d093c" "Project Setup"))
                    (li
                      (a :href "#org90f5334" "A "
                        (code "money") " type"))
                    (li
                      (a :href "#orga2eefc3" "Making money"))
                    (li
                      (a :href "#orgc8dd144" "Adding money"))
                    (li
                      (a :href "#orgf57f471" "The rest of the math functions"))
                    (li
                      (a :href "#org9a79e5b" "Improving Ergonomics"))
                    (li
                      (a :href "#org29c8e53" "Formatting for Human Consumption"))
                    (li
                      (a :href "#orgf1a9239" "Adding Currencies"))))
                (li
                  (a :href "#org287bbe3"
                    (code "ALMIGHTY-KAIKEI"))
                  (ul
                    (li
                      (a :href "#org105e6b4" "A Double-Entry Accounting System"))
                    (li
                      (a :href "#orgc7a3253" "Requirements"))
                    (li
                      (a :href "#org13de01c" "Testing & Examples"))
                    (li
                      (a :href "#org8a2f262" "Dependencies"))
                    (li
                      (a :href "#org5806017" "Vendor Your Dependencies"))
                    (li
                      (a :href "#orgddeffc9" "Project Setup"))
                    (li
                      (a :href "#org25ce0f2" "Introducing Our Dependencies"))
                    (li
                      (a :href "#org1696148" "Why SQL?"))
                    (li
                      (a :href "#org618650f" "Accounts"))
                    (li
                      (a :href "#org57abeac" "Transactions"))
                    (li
                      (a :href "#org919da1b" "Legs"))
                    (li
                      (a :href "#org120f715" "Playing around"))
                    (li
                      (a :href "#org95c7031" "Account Balance"))
                    (li
                      (a :href "#org25c9860" "Long-term problems"))
                    (li
                      (a :href "#orgc251b89" "Optimization"))
                    (li
                      (a :href "#orgb130547" "Example Usage"))
                    (li
                      (a :href "#org53c62fe" "Tests"))))))
            (li
              (a :href "#org9c81f6c" "DEPLOYING")
              (ul
                (li
                  (a :href "#org648444c" "EXECUTABLE CLI APP")
                  (ul
                    (li
                      (a :href "#org95674cf" "Tic-tac-toe"))
                    (li
                      (a :href "#org581447e" "almighty-kaikei"))))
                (li
                  (a :href "#org9bd3bc7" "DEPLOYING FUKA STACK WEB APP")
                  (ul
                    (li
                      (a :href "#org4aa1bff" "Requirements"))
                    (li
                      (a :href "#org968fa2d" "The Server"))
                    (li
                      (a :href "#org325bec0" "The Code"))
                    (li
                      (a :href "#orgef1d44e" "The Deploy"))
                    (li
                      (a :href "#org9de5f56" "Summary"))))
                (li
                  (a :href "#org8598b22" "HOLD DEPLOYING AN ELECTRON APP")
                  (ul
                    (li
                      (a :href "#org0ff6c01" "The Code"))))))
            (li
              (a :href "#org7b8f75a" "RESOURCES")))))
      (section :id "meta"
        (hgroup
          (span)
          (h1 "META"))
        (div :class "outline-text-2" :id "text-org90151c1")
        (section :id "why-i-wrote-this-book"
          (hgroup
            (span)
            (h2 "WHY I WROTE THIS BOOK"))
          (div :class "outline-text-3" :id "text-org4280390"
            (p "Common Lisp is an old programming language that hasn't changed since it was standardized. That means that any learning material produced for the language is still useful even today. You can read Touretzky, Norvig, etc. and learn from people far smarter than me.")
            (p "However, the methods of teaching material, the culture of the people reading the material, and the problems or projects presented in the material may not be as relevant to modern audiences. Touretzky writes for new programmers and doesn't even teach Lisp's OOP facilities; Norvig teaches first principles of programming geared toward old fashioned AI; SICP is heavy on math and wasn't written in an age of ubiquitous ecosystems like JavaScript or Python; Steele wrote a language reference, not a tutorial; various other authors focus on macros, CLOS, etc. These older teaching materials are still irreplaceable masterpieces (Norvig perhaps the best of them), but a modern audience has modern needs and sensibilities. ")
            (p "Practical Common Lisp at one time was the book for modern audiences, but even when it was released it fell short of what it should have been. It provides lots of practical projects, but doesn't provide a detailed guide for how to present those projects to the world. It's a book written for experienced programmers who are Lisp-curious and is partially meant to advocate for Lisp, but does little to fix the problem of Lisp's steep learning curve.")
            (p "And it does have a steep learning curve. Why? Not really because of any feature of Common Lisp. The parentheses can be a bit challenging at first, but it's more of an emotional challenge than a technical one. Macros are tough to master, but not so hard to get started with. Common Lisp is a big language, but beginning is not particularly hard and searching the HyperSpec can go a long way to deepen your knowledge of the language.")
            (p "No, it's not the language that's the source of the learning curve–it's Emacs. Emacs is the defacto-standard text editor for Common Lisp development. It's also 50 years old and doesn't have the mainstream popularity of other editors, creating a considerable mental block to even considering learning it or Common Lisp. On top of that, developers interact with Emacs very differently from how they interact with VSCode or the JetBrains IDEs (let alone OpenCode or Claude Code-like agentic LLM CLIs), hindering new users from transferring knowledge from the development environments they are familiar with.")
            (p "Emacs is the elephant in the room that no books on Common Lisp address. PCL made a half-hearted attempt with Lisp in a Box (a distribution of Emacs somewhat analogous to the later Portacle, now deprecated), but it was insufficient to the task.")
            (p "With this book I hope to provide you–the experienced Lisp-curious developer–with a source for learning both Common Lisp and Emacs at the same time, help you get oriented to the Lisp ecosystem and current practices, and get you deploying your applications in your preferred medium by the time you're finished reading.")))
        (section :id "almighty"
          (hgroup
            (span)
            (h2 "ALMIGHTY"))
          (div :class "outline-text-3" :id "text-org7f05309"
            (p "When you are given the job of choosing the tech stack for a job, how do you choose? If the task is data science, what language would you choose? Would your choice be different if your task was web development? Would it be different if it was embedded development? Or accounting? Or mobile apps?")
            (p "Programming is fraught with choices. Probably the most important choice is which language to use for any particular task. If you're doing data analytics, you need to use Python. If you're developing a web application, you need to use JavaScript or Ruby. If you're writing accounting software, you need to use Java. If you're writing real-time software with performance and size limitations, you use C or Rust.")
            (p "We take it for granted that such choices are necessary, that we need to choose a language based on a task. No language is adapted to every task; when you need to drill, you don't use a hammer.")
            (p "But what if we had a special, universal tool? What if the tool could be "
              (i "adapted") " to the task, rather than "
              (i "chosen") " for it?")
            (p "Common Lisp is just such a language. It's the highest level language out there thanks to macros and the parentheses, but a surprisingly low level language thanks to the ability to inspect generated assembly output and the ability to specify types (although in a somewhat sloppy way) to optimize for speed and memory efficiency. As a result, Lisp is both highly expressive while sacrificing little performance. The result is a highly adaptable language–perhaps the "
              (i "most") " adaptable.")
            (p "Lisp does sacrifice "
              (i "something") ", though. Well-optimized C is going to yield smaller executables, lower memory use, and faster performance. Well written Ruby or Python is probably aesthetically more pleasing than well written Lisp–and I believe that aesthetics are important. The Lisp ecosystem can usually get you started, but it's not going to do most of the heavy lifting like in Java or JavaScript's ecosystems.")
            (p "With Lisp, you sacrifice choosing the absolute optimal tool for the job in exchange for absolute "
              (i "flexibility") ". Lisp may not initially be the best tool for any single job, but you can "
              (i "mould") " it into the best tool for many jobs. It's not the fastest, but it's competitive with the fastest. It's not the prettiest, but you can make it prettier if that's important to you. There may not be a free library available to do exactly what you want, but you usually don't have to start from scratch, either.")
            (p "Lisp is not a language specialized to a domain of work; it's a language specialized to being "
              (i "generally adaptable") ". It's not a master of any domain, but it can be adapted to a wide variety of domains.")
            (p "In English, a man who isn't a specialist in any field, but who commands a holistic knowledge and capabilities in many domains, is called a "
              (i "generalist") ".")
            (p "In Japanese, such a man is called オールマイティ–almighty.")
            (p "That's what Lisp is. Lisp is almighty, and it summons programmers to become almighty.")
            (p "And in the current environment, you have no choice. AI is here. Whether we like it or not, many people will gladly trade their birthright for a bowl of stew; their ability to do things themselves now and in the future for their ability to do a lot of things (badly) "
              (i "now") "; craftmanship for crude replacements; their fellow humans and society as a whole for the chance at \"generational wealth\".")
            (p "Lines are being drawn. Either you join them on their quest or they will seek to crush you.")
            (p "You and I, friend, must become almighty. "))))
      (section :id "emacs-setup-use"
        (hgroup
          (span)
          (h1 "EMACS SETUP & USE"))
        (div :class "outline-text-2" :id "text-org9aaa190")
        (section :id "purpose"
          (hgroup
            (span)
            (h2 "PURPOSE"))
          (div :class "outline-text-3" :id "text-org6d9f85c"
            (p "Learning Common Lisp is complicated by the fact that Emacs is the defacto standard Common Lisp code editor. Emacs is good, but making use of its capabilities requires adopting a different mental model for code editing, making it difficult and confusing to learn. Finding out that you need to learn an "
              (i "old") ", "
              (i "weird") " editor before you can even begin learning Common Lisp in ernest is enough to turn newcomers off.")
            (p "This chapter is a quick references for getting started with Emacs and Common Lisp coding. It's intended to help bootstrap an understanding of Emacs concepts, Common Lisp concepts, and an understanding of common actions in both. You should experience a smooth transition to learning Common Lisp after completing this chapter.")
            (p "My intention is to make you less confused. If you are confused after reading this chapter, ask me for help on X ("
              (a :href "https://x.com/almighty_lisp" "@almighty"
                (sub "lisp")) "). Consider me your Emacs and Common Lisp tech support.")))
        (section :id "doom-emacs-install-setup"
          (hgroup
            (span)
            (h2 "DOOM EMACS INSTALL & SETUP"))
          (div :class "outline-text-3" :id "text-orgca51cd1")
          (section :id "macos"
            (hgroup
              (span)
              (h3 "MacOS"))
            (div :class "outline-text-4" :id "text-orgfff619f")
            (section :id "install-homebrew"
              (hgroup
                (span)
                (h4 "Install Homebrew"))
              (div :class "outline-text-5" :id "text-orgfdea3aa"
                (p
                  (a :href "https://brew.sh/" "Homebrew") " is a package manager for MacOS.")
                (pre
                  (code "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""))
                (p "Run this command in the terminal and follow instructions.")))
            (section :id "install-sbcl"
              (hgroup
                (span)
                (h4 "Install SBCL"))
              (div :class "outline-text-5" :id "text-orgd94dc0a"
                (p
                  (a :href "https://sbcl.org" "SBCL") " is the most popular Common Lisp implementation.")
                (pre
                  (code "brew install sbcl"))
                (p "Test that it's installed and working.")
                (pre
                  (code "$ sbcl --version"))))
            (section :id "install-emacs"
              (hgroup
                (span)
                (h4 "Install Emacs"))
              (div :class "outline-text-5" :id "text-org786c9ab"
                (p
                  (a :href "https://www.gnu.org/software/emacs/" "Emacs") " is the defacto-standard editor for Common Lisp.")
                (p "Install dependencies:")
                (pre
                  (code "brew install git ripgrep coreutils fd"))
                (p "Install Emacs Plus")
                (pre
                  (code "brew tap d12frosted/emacs-plus
brew install emacs-plus"))
                (p "Test that Emacs is installed and working.")
                (pre
                  (code "$ emacs"))))
            (section :id "install-doom"
              (hgroup
                (span)
                (h4 "Install Doom"))
              (div :class "outline-text-5" :id "text-orga626d21"
                (p
                  (a :href "https://github.com/doomemacs/doomemacs" "Doom") " is an Emacs distribution. It provides everything Emacs-related for integration with Common Lisp, and generally makes Emacs more ergonomic.")
                (pre
                  (code "git clone --depth 1 https://github.com/doomemacs/doomemacs =/.config/emacs
=/.config/emacs/bin/doom install"))
                (p "After installing, add "
                  (code "=/.config/emacs/bin/") " to "
                  (code "PATH") ".")
                (p "Open either "
                  (code ".bash_profile") " or "
                  (code ".zshrc") ", add this line and save.")
                (pre
                  (code "export PATH=\"$HOME/.config/emacs/bin:$PATH\"
export PATH=\"$PATH:/User/yourhomedirectoryhere/.config/emacs/bin\""))
                (p "Restart your terminal for the change to take effect.")
                (p "Test that Doom is installed and that the changes to your "
                  (code "PATH") " are working.")
                (pre
                  (code "$ doom sync
$ doom emacs"))))
            (section :id "configure-doom-for-common-lisp"
              (hgroup
                (span)
                (h4 "Configure Doom for Common Lisp"))
              (div :class "outline-text-5" :id "text-org012a16e"
                (p "Edit "
                  (code "init.el") " file.")
                (pre
                  (code :class "lisp" ";;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

;; NOTE I have abbreviated the full init.el file.
;;      The below settings are the only ones I've changed.
(doom! :input
       :completion
       company           ; the ultimate code completion backend
       :ui
       treemacs          ; a project drawer, like neotree but cooler
       :editor
       lispy             ; vim for lisp, for people who don't like vim
       :emacs
       :term
       vterm             ; the best terminal emulation in Emacs
       :checkers
       :tools
       :os
       :lang
       common-lisp       ; if you've seen one lisp, you've seen them all

       ;; The rest is unchanged."))
                (p "Save the "
                  (code "init.el") " file.")
                (p "Reload Doom with "
                  (code "SPC h r r") " (space key then h then r then r). Alternative: Close Emacs. In the terminal, run "
                  (code "doom sync") ". Reopen "
                  (code "doom emacs") "."))))
          (section :id "remap-capslock-to-control"
            (hgroup
              (span)
              (h3 "Remap Capslock to Control"))
            (div :class "outline-text-4" :id "text-orgda46584"
              (p "Because the Control key is used so heavily in Emacs keybindings, most Emacs users will remap the Caps Lock key to Control. That improves comfort and usability of Emacs keybindings. I can give you instructions for MacOS."))
            (section :id "macos"
              (hgroup
                (span)
                (h4 "MacOS"))
              (div :class "outline-text-5" :id "text-org8ca9b16"
                (ul :class "org-ul"
                  (li "Open Settings.")
                  (li "Go to Keyboard.")
                  (li "Click on Keyboard Shortcuts.")
                  (li "Go to Modifier Keys (bottom left).")
                  (li "Click on dropdown menu right of where it says Caps Lock Key.")
                  (li "Change it to Control.")
                  (li "Click Done."))))))
        (section :id "emacs-dictionary"
          (hgroup
            (span)
            (h2 "EMACS DICTIONARY"))
          (section :id "concepts"
            (hgroup
              (span)
              (h3 "Concepts"))
            (div :class "outline-text-4" :id "text-org1e2e2df")
            (section :id "buffer"
              (hgroup
                (span)
                (h4 "Buffer"))
              (div :class "outline-text-5" :id "text-org0d5ba0e"
                (p "An Emacs "
                  (code "buffer") " displays the contents of a file, terminal, repl, message/log, etc.")))
            (section :id "modeline"
              (hgroup
                (span)
                (h4 "Modeline"))
              (div :class "outline-text-5" :id "text-org49676cc"
                (p "The modeline is the area at the bottom of nearly every buffer that shows information about that buffer–including the file path for file buffers, the current major mode, and the current Evil state.")
                (p "You can interact with the modeline. If you hover over the modeline with the mouse, it will tell you what clicking on that area will do.")
                (p "The modeline, and the toolbar, are both invaluable sources of information for beginners.")))
            (section :id "window"
              (hgroup
                (span)
                (h4 "Window"))
              (div :class "outline-text-5" :id "text-org09846bb"
                (p "An Emacs "
                  (code "window") " is closer to the modern concept of a "
                  (code "pane") ". A window can only display one buffer at a time.")))
            (section :id "frame"
              (hgroup
                (span)
                (h4 "Frame"))
              (div :class "outline-text-5" :id "text-orgb16c039"
                (p "An Emacs "
                  (code "frame") " is the equivalent of the modern concept of a "
                  (code "window") ". It's the box that displays the Emacs application. A frame can hold multiple Emacs windows and minibuffers simultaneously.")))
            (section :id "minibuffer"
              (hgroup
                (span)
                (h4 "Minibuffer"))
              (div :class "outline-text-5" :id "text-org01017bb"
                (p "A minibuffer is a "
                  (code "buffer") " that is intended to be temporary. Usually they display some information while you are in the middle of a command or other action, and close automatically when you finish or abort the action/command.")))
            (section :id "mode"
              (hgroup
                (span)
                (h4 "Mode"))
              (div :class "outline-text-5" :id "text-orge314329"
                (p "Buffers have both "
                  (code "major") " and "
                  (code "minor") " modes. If you open a "
                  (code ".lisp") " file, the file will automatically open in "
                  (code "lisp-mode") ". "
                  (code "lisp-mode") " is a "
                  (code "major-mode") ". ~auto-save-mode=, "
                  (code "evil-mode") ", "
                  (code "company-mode") ", "
                  (code "hl-mode") ", etc. are "
                  (code "minor") " modes. Only one "
                  (code "major") " mode can be active in one buffer. Multiple "
                  (code "minor") " modes can be active in a buffer.")
                (p "Modes provide syntax highlighting, specialized commands, and various features intended to improve the editing experience.")))
            (section :id "state"
              (hgroup
                (span)
                (h4 "State"))
              (div :class "outline-text-5" :id "text-org4b9331a"
                (p "This is more specific to the "
                  (code "evil-mode") " package. States describe the keyboard cursor. In normal state")))
            (section :id "workspaces-projects"
              (hgroup
                (span)
                (h4 "Workspaces & Projects"))
              (div :class "outline-text-5" :id "text-org458e510"
                (p "Doom includes packages that provide workspace and project functionality, allowing you to more effectively separate groups of buffers and give you the ability to quickly search multiple projects. The functionality idiomatic of workspaces and projects aren't native to Emacs, but they are conveniently provided by Doom Emacs through several packages.")))
            (section :id "packages"
              (hgroup
                (span)
                (h4 "Packages"))
              (div :class "outline-text-5" :id "text-org5384b1e"
                (p "Emacs packages are equivalent of \"extensions\" or \"plugins\". Not to be confused with "
                  (i "Common Lisp packages") " which are an entirely separate idiom and functionality."))))
          (section :id "keybinding-basics"
            (hgroup
              (span)
              (h3 "Keybinding Basics"))
            (div :class "outline-text-4" :id "text-org005d8f4"
              (p "Doom Emacs have vanilla Emacs keybindings and vim-style keybindings (see: "
                (code "evil") " in the "
                (code "init.el") " file) enabled by default.")
              (p
                (code "C") " is the control key."
                (code "M") " is the meta/option/alt key."
                (code "s") " is the super/command/windows key."
                (code "SPC") " is the space key"
                (code "ESC") " is the escape key.")
              (p
                (code "C-x") " means \"Hold control then press x\"."
                (code "C-c C-c") " means \"Hold control then press c, then hold control then press c\"."
                (code "SPC f f") " means \"Press the space key then f then f\"."))))
        (section :id "survival-emacs"
          (hgroup
            (span)
            (h2 "SURVIVAL EMACS"))
          (div :class "outline-text-3" :id "text-orgb32f55b"
            (p "When you first begin with this book, I assume you at least don't know Emacs keybindings. If you know Vim/Neovim style bindings, then you'll know how to do basic text navigation and editing, search and replace, etc. I'll teach you the basics of how to get around and get started following along with the next Lisp chapter. Importantly, you won't need to learn any keybindings at all; you can mostly pretend Emacs is a plain text editor. Thus, in this chapter I'll teach you how to:")
            (ul :class "org-ul"
              (li "Create & Open a file buffer using the menu.")
              (li "Read the modeline.")
              (li "Insert text.")
              (li "Save a file buffer using the menu.")
              (li "View a list of open buffers in the menu."))
            (p "Additionally, there are some behaviors in Emacs that may be confusing while doing basic text editing. I'll tell you about these behaviors ahead of time so it's less frustrating when you first begin."))
          (section :id "creating-opening-file-buffers"
            (hgroup
              (span)
              (h3 "Creating & Opening File Buffers"))
            (div :class "outline-text-4" :id "text-org3b1f077"
              (p "In Emacs, creating a new file buffer and opening an existing file into a buffer are nearly the same action, making it somewhat confusing.")
              (p "Let's say you want to create a new file. In the menu, navigate to "
                (code "File->Visit New File") ". A new minibuffer will open. In the top left of the minibuffer it will say \"Find File\" and show the path to the current directory. To make a new file there, type in the name of the file you want to create, like \"abc.txt\" or \"main.lisp\", then press "
                (code "Enter") ". A new, empty buffer with that name will open. "
                (i "The file doesn't exist until you save it."))
              (p "To navigate to other directories from that minibuffer, you can type the name of a subdirectory, or you can press "
                (code "Backspace") " to navigate to a parent directory instead.")
              (p "Now let's say you want to open an existing file. In the menu, navigate to "
                (code "File->Open File") ". The identical minibuffer will open. Navigate to your file, type in its name, and press "
                (code "Enter") " to open the file.")))
          (section :id "reading-the-modeline"
            (hgroup
              (span)
              (h3 "Reading The Modeline"))
            (div :class "outline-text-4" :id "text-orgb346030"
              (p "With a file buffer open, take a look at the bar at the bottom of the buffer. It contains important information. It's contents, from left to right:")
              (ul :class "org-ul"
                (li "An icon showing the current "
                  (code "evil-mode") " state.")
                (li "An integer showing the buffer size.")
                (li "The path to the current file/buffer.")
                (li "Two numbers separated by a colon. These show the current line and column of the cursor. Example: "
                  (code "50:20") " means "
                  (code "line 50, column 20")))))
          (section :id "inserting-text"
            (hgroup
              (span)
              (h3 "Inserting Text"))
            (div :class "outline-text-4" :id "text-orgc68effb"
              (p "By default, you are in "
                (code "evil-mode") "'s "
                (code "Normal State") ". We want to initially avoid learning any complicated systems of text editing. To do that, we can switch to "
                (code "Emacs State") ". Type "
                (code "C-z") ". In the modeline in the bottom left, you should see the icon change, probably to "
                (code "E") ", indicating that you are in "
                (code "Emacs State") ". In this state:")
              (ul :class "org-ul"
                (li "You have access to all of Emacs' keybindings.")
                (li "You can insert, highlight, cut, copy, paste, and delete text as you usually would."))
              (p "You will need to switch to "
                (code "Emacs State") " every time you open a new buffer.")
              (p "To switch out of "
                (code "Emacs State") " and back into "
                (code "Normal State") ", type "
                (code "C-z") " again.")))
          (section :id "saving-files"
            (hgroup
              (span)
              (h3 "Saving Files"))
            (div :class "outline-text-4" :id "text-org1051bf2"
              (p "You can save a file using your standard keybinding "
                (code "s-s") ", or you can save with the menu. Navigate to "
                (code "File->Save") ".")))
          (section :id "view-a-list-of-open-buffers"
            (hgroup
              (span)
              (h3 "View A List Of Open Buffers"))
            (div :class "outline-text-4" :id "text-org48bc1c7"
              (p "You can view a list of open buffers and switch to one in the menu. Navigate to "
                (code "Buffers") ". Buffers that have names like "
                (code "*Messages*") " are special Emacs buffers. All others should be file buffers.")
              (p "If you click on one of the buffers, the window with the keyboard cursor active will switch to that buffer.")))
          (section :id "kill-close-a-buffer"
            (hgroup
              (span)
              (h3 "Kill/Close A Buffer"))
            (div :class "outline-text-4" :id "text-org765f78e"
              (p "To close a buffer, navigate to "
                (code "File->Close") ".")))
          (section :id "strange-behavior"
            (hgroup
              (span)
              (h3 "Strange Behavior"))
            (div :class "outline-text-4" :id "text-org28643fa"
              (p "That'll get you started with the basics. There are few strange behaviors to look out for, though."))
            (section :id "-i-typed-something-once-but-it-got-repeated-several-times-"
              (hgroup
                (span)
                (h4 "\"I typed something once but it got repeated several times.\""))
              (div :class "outline-text-5" :id "text-org0a77f6a"
                (p "In "
                  (code "Emacs State") ", if you type "
                  (code "C-u") " and then a number "
                  (code "n") ", the next thing you type will be repeated "
                  (code "n") " times. If you type "
                  (code "C-u 8 d") " you will type 8 d's.")
                (p "This is useful behavior when you know what's happening, confusing and frustrating when you don't. It's usually more of a problem in Evil "
                  (code "Normal State") " because you don't even have to type "
                  (code "C-u") ", and if you type "
                  (code "8") ", go into "
                  (code "Insert State") " and type in a bunch of code and then type "
                  (code "ESC") " to go back to "
                  (code "Normal State") ", all of the code will be repeated 8 times.")))
            (section :id "-i-can-t-type-anything-the-window-seems-locked-"
              (hgroup
                (span)
                (h4 "\"I can't type anything; the window seems locked.\""))
              (div :class "outline-text-5" :id "text-org77fdca1"
                (p "Look at the beginning of the buffer name. Does it have a Lock icon? You may have accidentally typed "
                  (code "C-x C-q") " or "
                  (code "SPC t r") ", putting the buffer into read-only mode. Type "
                  (code "C-x C-q") " or "
                  (code "SPC t r") " again to make the buffer editable again.")))
            (section :id "-i-typed-something-and-the-buffer-changed-"
              (hgroup
                (span)
                (h4 "\"I typed something and the buffer changed.\""))
              (div :class "outline-text-5" :id "text-orgfce8915"
                (p "You may have accidentally closed or switched the buffer. You can try switching back using the method above, or you can type "
                  (code "C-w C-u") " to undo whatever the heck you did. We'll see later that the "
                  (code "C-w C-u") " ("
                  (code "winner-undo") ") command has some other uses.")
                (p "And if you accidentally undid something that you want to "
                  (i "redo") ", you can type "
                  (code "C-w C-r") ".")))
            (section :id "-i-typed-something-and-some-minibuffer-or-prompt-came-up-and-i-want-to-just-get-rid-of-it-"
              (hgroup
                (span)
                (h4 "\"I typed something and some minibuffer or prompt came up and I want to just get rid of it.\""))
              (div :class "outline-text-5" :id "text-org728100e"
                (p "Typically this happens to me when I accidently type "
                  (code "C-x k") " to kill/close a buffer. Whatever you did, you can cancel whatever action you are in the middle of by typing "
                  (code "C-g") " a few times. "
                  (code "C-g") " is a universal command for canceling some action.")))
            (section :id "-my-buffer-has-a-file-open-but-i-can-only-see-part-of-it-or-perhaps-none-of-it-at-all-"
              (hgroup
                (span)
                (h4 "\"My buffer has a file open but I can only see part of it (or perhaps none of it at all).\""))
              (div :class "outline-text-5" :id "text-org398b2d7"
                (p "If you're sure you have a file buffer open to a file that exists and has data in it, then you may be a victim of "
                  (i "narrowing") ". You can confirm by looking at the modeline: At the beginning of the buffer/file path in the modeline, if you see two arrows, one above the other, pointing to each other, then the buffer has been narrowed.")
                (p "To unnarrow or "
                  (i "widen") ", switch to "
                  (code "Normal State") " and type "
                  (code "SPC b -") " ("
                  (code "doom/toggle-narrow-buffer") ").")))
            (section :id "-i-cut-some-text-but-when-i-try-to-paste-some-other-text-appears-"
              (hgroup
                (span)
                (h4 "\"I cut some text, but when I try to paste, some other text appears.\""))
              (div :class "outline-text-5" :id "text-org1624ca1"
                (p "Did you cut some text, delete a word with "
                  (code "C-backspace") "? Is that word the one being pasted?")
                (p "You are experiencing expected behavior of the kill ring. You can undo everything the normal way you undo any text operation using "
                  (code "s-z") ".")
                (p "To avoid this behavior, you need to use "
                  (code "backspace") " or "
                  (code "delete") " alone (erasing a character at a time). I'll cover killing later."))))
          (section :id "speeding-up-w-keybindings"
            (hgroup
              (span)
              (h3 "Speeding Up w/Keybindings"))
            (div :class "outline-text-4" :id "text-org8e3b633"
              (p "If during the next chapter you find yourself limited by using the menu for doing things, notice that in the menu there are keybindings printed next to nearly every command available in there. Try using some of the keybindings for your most frequently used commands (like "
                (code "Eval Defun") " or "
                (code "Compile Defun") ").")))))
      (section :id "lisp-setup-use"
        (hgroup
          (span)
          (h1 "LISP SETUP & USE"))
        (div :class "outline-text-2" :id "text-org6798ebc")
        (section :id "installing-lisp"
          (hgroup
            (span)
            (h2 "INSTALLING LISP")))
        (section :id "survival-lisp-commands"
          (hgroup
            (span)
            (h2 "SURVIVAL LISP COMMANDS"))
          (div :class "outline-text-3" :id "text-orgb0b83ef"
            (p "Beyond simple file/buffer operations and text editing are Common Lisp-specific concepts and commands. This chapter will quickly run through the essentials for getting started."))
          (section :id "compiling-evaluating-lisp-code-from-the-buffer"
            (hgroup
              (span)
              (h3 "Compiling/Evaluating Lisp Code From the Buffer"))
            (div :class "outline-text-4" :id "text-orge37984d")
            (section :id "form"
              (hgroup
                (span)
                (h4 "Form"))
              (div :class "outline-text-5" :id "text-orgc1f9385"
                (p "To "
                  (i "compile") " a form, place the cursor anywhere in a form then in the menu navigate to "
                  (code "Sly->Compilation->Compile Defun") ". This works for other forms, not just "
                  (code "defun") " (which we'll learn about later).")
                (p "To "
                  (i "evaluate") " the form place the cursor anywhere in the form then navigate to "
                  (code "Sly->Evaluation->Eval Defun") ".")))
            (section :id "variable"
              (hgroup
                (span)
                (h4 "Variable"))
              (div :class "outline-text-5" :id "text-org72b5c78"
                (p "If a global variable is set, you can get the value of the variable anywhere it is in a Lisp file.")
                (p "Set the cursor at the end of the variable then navigate to "
                  (code "Sly->Evaluation->Eval last expression") ". This works for individual symbols and other forms, too.")))
            (section :id "file"
              (hgroup
                (span)
                (h4 "File"))
              (div :class "outline-text-5" :id "text-orgb01951a"
                (p "To compile a whole file, navigate to "
                  (code "Sly->Compilation->Compile and Load File") "."))))
          (section :id "lisp-images"
            (hgroup
              (span)
              (h3 "Lisp Images"))
            (div :class "outline-text-4" :id "text-orgf7f11d1"
              (p "An important concept that has deep implications for Lisp development is the Lisp Image.")
              (p "When you first open a Common Lisp file in Emacs, a new REPL session and Lisp image is created. Code is then loaded, compiled, and executed in the image. It is sometimes likened to an operating system.")
              (p "Some operations that you perform will modify image-level settings. We will see some examples later.")
              (p "An image can be saved and restored. We'll learn how to do that at the appropriate time.")))
          (section :id "using-the-repl"
            (hgroup
              (span)
              (h3 "Using the REPL"))
            (div :class "outline-text-4" :id "text-orgfaaa93e"
              (p "Since a REPL was opened when you opened a Common Lisp file, you "
                (i "can") " switch to it using the standard menu navigation for switching buffers. However, the entire window will be taken up with the REPL, which may not be what you want. ")
              (p "If you would like a REPL opened in a "
                (i "minibuffer") " under your Lisp buffer, then do the following: With a Lisp file open and the cursor in the buffer, type "
                (code "SPC m '") ". A Lisp REPL will be opened. The REPL/image is \"attached\" to the Emacs instance. If you open a different Lisp file and compile the code, it will be loaded into the same image.")))
          (section :id "structural-editing"
            (hgroup
              (span)
              (h3 "Structural Editing"))
            (div :class "outline-text-4" :id "text-orgd30dcb1"
              (p "Editing Lisp code can be tricky at first.")
              (p "If you delete a closing-parenthesis, all of the code between and including the opening and closing parentheses will be deleted.")
              (p "Deleting an end-quote will do the same.")
              (p "This is call \"structural editing\". It prevents unbalanced parentheses or quotes. However, there are some circumstances where it actually makes it difficult to fix unbalanced parentheses (usually because of copy/pasting code).")
              (p "There can be a lot of confusing behavior with structural editing. To turn it off, type "
                (code "M-x lispy-mode") " to toggle "
                (code "lispy-mode") " off. With lispy-mode off, you will have more freedom, but less help from the editor. As the spirit moves, you can toggle it on and play around with it. We'll cover some useful features of structural editing later.")))
          (section :id "the-lisp-debugger"
            (hgroup
              (span)
              (h3 "The Lisp Debugger"))
            (div :class "outline-text-4" :id "text-org643950c"
              (p "We will take a closer look at the debugger and errors in the chapter on errors and conditions, but before you get to that section you'll probably cause the debugger to open and display a message like this:")
              (pre :class "example" :id "org7aa7f68" ":AWS fell through ECASE expression.
Wanted one of (:DEVELOPMENT :PRODUCTION).
   [Condition of type SB-KERNEL:CASE-FAILURE]

Restarts:
 0: [RETRY] Retry SLY evaluation request.
 1: [*ABORT] Return to SLY's top level.
 2: [ABORT] abort thread (#<THREAD tid=11923 \"slynk-worker\" RUNNING {70071BE463}>)")
              (p "In this situation, you can do two things:")
              (ol :class "org-ol"
                (li "Navigate the cursor to one of the "
                  (code "restarts") " and press "
                  (code "Enter") ".")
                (li "Type the key corresponding to the "
                  (code "restart") " (0 for [RETRY], 1 for [*ABORT], etc.) to select it."))
              (p "In the beginning, it's safest to choose whichever restart says "
                (code "[*ABORT]") ". If your list of restarts is too long, it will be truncated and you might need to highlight and type "
                (code "Enter") " on a "
                (code "Show More") " option.")
              (p "However, if you want to, you can give Common Lisp's debugger a test drive. If you cause an error, try leaving the debugger open, fixing the bug in your code (make sure to compile it), and then selecting a "
                (code "RETRY") " restart if one is available. This is one of Common Lisp's most powerful "
                (b "built-in") " features.")))))
      (section :id "lisp-fundamentals"
        (hgroup
          (span)
          (h1 "LISP FUNDAMENTALS"))
        (div :class "outline-text-2" :id "text-orgfff93b6")
        (section :id "syntax-grammar"
          (hgroup
            (span)
            (h2 "SYNTAX & GRAMMAR"))
          (div :class "outline-text-3" :id "text-org284d65f")
          (section :id "s-expressions"
            (hgroup
              (span)
              (h3 "S-expressions"))
            (div :class "outline-text-4" :id "text-org8281000"
              (pre
                (code :class "lisp" "(+ 3 4)"))
              (p "This is a Common Lisp "
                (code "s-expression") ". S-expressions are made of indivisible units called "
                (code "atoms") "–such as the "
                (code "symbol")
                (code "+") " and the number "
                (code "3") "–and divisible units called "
                (code "lists") ". They are also called "
                (code "forms") ".")
              (p "Atoms evaluate to themselves.")
              (p "The first atom in a list is evaluated as a function name. The rest of the arguments are evaluated before being passed to the function.")
              (p "Symbols, other than the first one in a list, are evaluated as variables.")
              (p "Some forms have special evaluation rules and are called special forms.")))
          (section :id "prefix-notation"
            (hgroup
              (span)
              (h3 "Prefix Notation"))
            (div :class "outline-text-4" :id "text-orgd647c46"
              (p "Math operations in Common Lisp are probably different than what you're used to.")
              (p "Common Lisp math operations use "
                (code "prefix notation") ": the math operator is a function that comes at the beginning of the parenthesis, and all numbers afterward evaluated using that operator, from left to right. In school, we learn math using "
                (code "infix notation") ", where the operators are placed between each number.")
              (pre
                (code :class "lisp" "(+ (* 5 5 5) (/ 18 2) (- 20 3)) ; using infix notation: 5 * 5 * 5 + 18 / 2 + 20 - 3"))))
          (section :id "more-complicated-s-expression"
            (hgroup
              (span)
              (h3 "More complicated s-expression"))
            (div :class "outline-text-4" :id "text-org3579b50"
              (p "Let's look at a slightly more complicated example of Common Lisp syntax:")
              (pre
                (code :class "lisp" "(defun cube (x)
  (* x x x))"))
              (p "After the opening parenthesis comes "
                (code "defun") ", a built-in "
                (code "special form") ". Special forms have different evaluation rules compared to regular function calls. "
                (code "defun") " is used to define a function. The function above is given the name "
                (code "cube") ". After giving the function a name, we define it's "
                (code "parameters") ", the data it takes as ~arguments= to run the operations inside the function. The function takes one argument: "
                (code "x") ".")
              (p "Notice that "
                (code "cube") " isn't evaluated as a variable. The "
                (code "x") " in "
                (code "(x)") " isn't initially interpreted as a function to be called.")
              (p
                (code "defun") " is a special operator where the first argument it receives is a symbol designating the name to give the function, and the second argument is a list of arguments that the function is expected to receive when it's called. The forms that follow afterward are called the "
                (code "body") " of the function.")
              (p "There are several other special forms with their own syntax. We'll cover them later.")))
          (section :id "evaluation-rules"
            (hgroup
              (span)
              (h3 "Evaluation Rules"))))
        (section :id "symbols"
          (hgroup
            (span)
            (h2 "SYMBOLS"))
          (div :class "outline-text-3" :id "text-orgf010bda")
          (section :id "symbol-names"
            (hgroup
              (span)
              (h3 "Symbol Names"))
            (div :class "outline-text-4" :id "text-orgc091151"
              (p "Under normal use, symbols in Common Lisp are case-insensitive.")
              (pre
                (code :class "lisp" "mY-nUm
(MY-num)"))
              (p "By calling "
                (code "quote") " or using the reader-macro "
                (code "'") ", you can return the names of the symbols, rather than evaluating and returning the values of the symbols.")
              (pre
                (code :class "lisp" "'my-num ; => MY-NUM"))))
          (section :id "introducing-global-variables"
            (hgroup
              (span)
              (h3 "Introducing Global Variables"))
            (div :class "outline-text-4" :id "text-org4b67934")
            (section :id "variables-with-no-starting-value"
              (hgroup
                (span)
                (h4 "Variables with no starting value"))
              (div :class "outline-text-5" :id "text-org350b4bb"
                (p "If you want to introduce a variable you have a few options.")
                (p "Use "
                  (code "defvar") " to introduce a global variable without a value.")
                (pre
                  (code :class "lisp" "(defvar *a-variable-with-no-value*)"))
                (p "To change the value of a "
                  (code "defvar") " variable, you need to call "
                  (code "setf") " on the variable.")))
            (section :id "variables-whose-value-can-change"
              (hgroup
                (span)
                (h4 "Variables whose value can change"))
              (div :class "outline-text-5" :id "text-org77162f9"
                (p "Use "
                  (code "defparameter") " to introduce a global variable with a starting value that can be modified.")
                (pre
                  (code :class "lisp" "(defparameter *a-global-var* 'hello-world)"))
                (p "Global variables defined with "
                  (code "defvar") " and "
                  (code "defparameter") " are surrounded with "
                  (code "*") " by convention.")))
            (section :id "variables-with-values-that-don-t-change"
              (hgroup
                (span)
                (h4 "Variables with values that don't change"))
              (div :class "outline-text-5" :id "text-orge10d101"
                (p "Use "
                  (code "defconstant") " to introduce a global variable that has a value that can't be modified.")
                (pre
                  (code :class "lisp" "(defconstant +pi+ 3.14)"))
                (p "Constant variable names are surrounded with "
                  (code "+") " by convention.")))
            (section :id "modifying-the-value-of-a-variable"
              (hgroup
                (span)
                (h4 "Modifying the value of a variable"))
              (div :class "outline-text-5" :id "text-org9cdd725"
                (p "You can also use "
                  (code "setf") " to assign values to variables you've already introduced.")
                (pre
                  (code :class "lisp" "(setf *a-global-var* 'goodnight-world)"))
                (p "You have to use "
                  (code "setf") " to modify the value of a "
                  (code "defvar") " variable, but "
                  (code "defparameter") " variable values can be modified with another call to "
                  (code "defparameter") " with a new value for the variable.")
                (p "Using "
                  (code "setf") " on a variable that you haven't introduced yet will result in a warning."))))
          (section :id "introducing-local-variables"
            (hgroup
              (span)
              (h3 "Introducing Local Variables"))
            (div :class "outline-text-4" :id "text-org1170203"
              (p "Use "
                (code "let") " to introduce a local variable. The variable will only exist or contain the set value inside the "
                (code "let") " form.")
              (pre
                (code :class "lisp" "(let ((lucky-num 7))
  lucky-num)
 ; => 7 (3 bits, #x7, #o7, #b111)"))
              (p "You can also reassign a global variable temporarily.")
              (pre
                (code :class "lisp" "(defparameter *var* 'outside)
*var*                   ; => OUTSIDE
(let ((*var* 'inside))   ; *var* is reassigned a new value.
  *var*)                ; => INSIDE
*var*                   ; => OUTSIDE"))
              (p "When variables have different values depending on their context, it's called "
                (code "lexical scoping") ". Scoping variables is critical to avoiding bugs.")
              (p "There are several other ways to introduce locally-scoped variables. One of them is inside "
                (code "defun") " definitions.")))
          (section :id "more-than-just-variables"
            (hgroup
              (span)
              (h3 "More than just variables"))
            (div :class "outline-text-4" :id "text-orgf6ee416"
              (p "While variables are an important kind of symbol, they aren't the only kind of symbol. Function names, package names, class names, etc. are all symbols. Symbols themselves are a data structure and can hold more than one piece of information."))))
        (section :id "functions"
          (hgroup
            (span)
            (h2 "FUNCTIONS"))
          (div :class "outline-text-3" :id "text-org533e73e")
          (section :id "defining-named-functions"
            (hgroup
              (span)
              (h3 "Defining Named Functions"))
            (div :class "outline-text-4" :id "text-org83e10ee")
            (section :id "globally"
              (hgroup
                (span)
                (h4 "Globally"))
              (div :class "outline-text-5" :id "text-org98c5e17"
                (p "You can define functions with "
                  (code "defun") ":")
                (pre
                  (code :class "lisp" "(defun sum (x y) ; x and y are now locally-scoped variables
  (+ x y))"))
                (p "All functions defined with "
                  (code "defun") " are global in scope.")
                (pre
                  (code :class "lisp" "(defun categorize-food (food)
  (defun compile-food-ingredients (food) ...) ; This is global, don't do it!
  ...)"))))
            (section :id "locally"
              (hgroup
                (span)
                (h4 "Locally"))
              (div :class "outline-text-5" :id "text-org92ff3cb"
                (p "To define locally-scoped named functions, use "
                  (code "flet") " or "
                  (code "labels") ":")
                (pre
                  (code :class "lisp" "(defun squirt-then-double (x)
  (flet ((squirt (y)                        ; Locally-scoped function named \"squirt\".
           (* y y)))
    (* 2 (squirt x))))

(defun squirt-then-double-with-let (x)
  (flet ((squirt (y)
           (* y y)))
    (let ((squirted (squirt x)))            ; Nesting let, flet, etc. is perfectly crumulent.
      (* 2 squirted))))

(defun squirt-then-double-with-labels (x)
  (labels ((squirt (y)                      ; labels is the equivalent of let* for functions
             (* y y))
           (double ()
             (* 2 (squirt x))))
    (double x)))")))))
          (section :id "lambda-list"
            (hgroup
              (span)
              (h3 "Lambda List"))
            (div :class "outline-text-4" :id "text-orgc316852"
              (pre
                (code :class "lisp" "(defun a-fun (a b c))"))
              (p "The second parameter to "
                (code "defun") " is called a "
                (code "lambda list") ". Parameters for the function being defined are specified inside the lambda list.")))
          (section :id "parameters"
            (hgroup
              (span)
              (h3 "Parameters"))
            (div :class "outline-text-4" :id "text-org282882a")
            (section :id "required"
              (hgroup
                (span)
                (h4 "required"))
              (div :class "outline-text-5" :id "text-orgf3c11e3"
                (p "Parameters defined without any other options are required.")
                (pre
                  (code :class "lisp" "(defun my-fun (this-is-required))"))))
            (section :id "-optional-"
              (hgroup
                (span)
                (h4
                  (code "&optional")))
              (div :class "outline-text-5" :id "text-org8b5f51c"
                (p "You can define different types of parameters with "
                  (code "lambda list keywords") ".")
                (p "Define optional parameters with the "
                  (code "&optional") " lambda list keyword.")
                (pre
                  (code :class "lisp" "(defun fun-with-optional (a &optional b)
  (format t \"The values passed: ~a and ~a.~&\" a b))

(fun-with-optional 5) ; => The values passed: 5 and NIL."))
                (p "The default value for optional arguments not passed is "
                  (code "nil") ". You can set the default value.")
                (pre
                  (code :class "lisp" "(defun fun-with-optional (a &optional (b 10))
  (format t \"The values passed: ~a and ~a.~&\" a b))

(fun-with-optional 5) ; => The values passed: 5 and 10."))
                (p "You can create a predicate that will return whether the value was supplied or the default is being used.")
                (pre
                  (code :class "lisp" "(defun fun-with-optional (a &optional (b 10 b-supplied-p))
  (if b-supplied-p
      (format t \"The values passed: ~a and ~a (passed by user).~&\" a b)
      (format t \"The values passed: ~a and ~a (default).~&\" a b)))

(fun-with-optional 5) ; => The values passed: 5 and 10 (default).
(fun-with-optional 5 10) ; => The values passed: 5 and 10 (passed by user)."))
                (p "All parameters following the "
                  (code "&optional") " lambda list keyword will be optional. This is true of all lambda list keywords.")
                (pre
                  (code :class "lisp" "(defun fun-with-lots-of-options (&optional (a 1) (b 2) (c 3))
  (+ a b c))
(fun-with-lots-of-options)              ; => 6
(fun-with-lots-of-options 2)            ; => 7
(fun-with-lots-of-options 5 5 5)        ; => 15"))))
            (section :id "-key-"
              (hgroup
                (span)
                (h4
                  (code "&key")))
              (div :class "outline-text-5" :id "text-org3d367fb"
                (p "Define named parameters using "
                  (code "&key") ".")
                (pre
                  (code :class "lisp" "(defun fun-with-keys (&key a b c)
  (+ a b c))
(fun-with-keys :a 1 :b 2 :c 3)          ; => 6"))
                (p "As with "
                  (code "&optional") ", you can specify default values.")
                (pre
                  (code :class "lisp" "(defun fun-with-keys (&key (a 1) (b 2) (c 3))
  (+ a b c))
(fun-with-keys)                         ; => 6
(fun-with-keys :c 5 :b 7 :a 3)          ; => 15"))
                (p
                  (code "-supplied-p") " predicates can also be specified.")
                (pre
                  (code :class "lisp" "(defun fun-with-keys (&key (a 1 a-supplied-p) (b 2 b-supplied-p) (c 3 c-supplied-p))
  (format t \"~a (~a) + ~a (~a) + ~a (~a) = ~a~&\" a a-supplied-p b b-supplied-p c c-supplied-p (+ a b c)))
(fun-with-keys)
                                        ; 1 (NIL) + 2 (NIL) + 3 (NIL) = 6
                                        ;  => NIL
(fun-with-keys :c 5 :b 7 :a 3)
                                        ; 3 (T) + 7 (T) + 5 (T) = 15
                                        ;  => NIL"))))
            (section :id "-rest-"
              (hgroup
                (span)
                (h4
                  (code "&rest")))
              (div :class "outline-text-5" :id "text-orgf09a04d"
                (p "Collect arbitrary arguments into a list with "
                  (code "&rest") ".")
                (pre
                  (code :class "lisp" "(defun fun-with-rest (&rest args)
  (loop for arg in args
        do (print arg)))
(fun-with-rest 'a)                      ; A  => NIL
(fun-with-rest 'a 'b 5 \"rest a while\")
                                        ; A
                                        ; B
                                        ; 5
                                        ; \"rest a while\"  => NIL

(defun add (&rest args)
  (apply #'+ args))
(add 1 2 3 4 5)                         ; => 15"))))
            (section :id "mixing-lambda-list-keywords"
              (hgroup
                (span)
                (h4 "Mixing lambda list keywords"))
              (div :class "outline-text-5" :id "text-orgd69b547"
                (p "As a general rule, you don't want to mix "
                  (code "&optional") " and "
                  (code "&key") " parameters.")
                (pre
                  (code :class "lisp" "(defun do-not-mix (&optional (a 1) (b 2) &key (c 3))
  (+ a b c))
(do-not-mix :c 5)                       ; Error: :c is not a number.
                                        ; It's interpreted as argument a."))
                (p "Use one or the other, but not both."))))
          (section :id "return-values"
            (hgroup
              (span)
              (h3 "Return Values"))
            (div :class "outline-text-4" :id "text-orgf32489b"
              (p "In Common Lisp, the value returned by the last expression called in a form will be the return value.")
              (pre
                (code :class "lisp" "(defvar *some-var*)
(defun do-a-bunch-of-stuff ()
  (setf *some-var* \"I assigned a value to *some-var*.\")
  (let ((num 5))
    (squirt-then-double num)
    (random 10)
    \"I am the value that this function will return\"))
(do-a-bunch-of-stuff)"))
              (p "There are cases, however, when you might want to do an early return, especially when looping. You can do that with "
                (code "return") ".")))
          (section :id "returning-multiple-values"
            (hgroup
              (span)
              (h3 "Returning Multiple Values"))
            (div :class "outline-text-4" :id "text-orge6ed646"
              (p "It's possible to return multiple values using the "
                (code "values") " function.")
              (pre
                (code :class "lisp" "(defun return-a-bunch-of-stuff ()
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
            (div :class "outline-text-4" :id "text-org230a675"
              (p "A simple "
                (code "let") " won't bind all of the values returned by a function that returns multiple values. Instead, only the first value will be bound.")
              (pre
                (code :class "lisp" "(let ((val (return-a-bunch-of-stuff)))
  val)
                                        ; => 9 (4 bits, #x9, #o11, #b1001)"))
              (p "If you need to bind multiple values, use "
                (code "multiple-value-bind") ".")
              (pre
                (code :class "lisp" "(multiple-value-bind (a b c)
    (return-a-bunch-of-stuff)
  (format t \"~a * ~a * ~a = ~a\" a b c (* a b c)))
                                        ; 9 * 49 * 4 = 1764 => NIL"))))
          (section :id "breaking-lists-into-multiple-values"
            (hgroup
              (span)
              (h3 "Breaking Lists Into Multiple Values"))
            (div :class "outline-text-4" :id "text-org994a697"
              (p "Sometimes you might want to take a list and break it into pieces–called destructuring. For that, there's "
                (code "destructuring-bind") ".")
              (pre
                (code :class "lisp" "(destructuring-bind (a b c)
    (list 1 2 3)
  (format t \"~a * ~a * ~a = ~a\" a b c (* a b c)))
                                        ; 1 * 2 * 3 = 6 => NIL"))
              (p "This destructuring can be done on arbitrarily deep trees of cons cells.")
              (pre
                (code :class "lisp" "(defparameter *deep-tree* `(defun function-name (a lambda-list)
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
            (div :class "outline-text-4" :id "text-org66b1d30"
              (p "When a variable is passed to a function, a new, local variable is introduce with the value of the variable passed to the function.")
              (pre
                (code :class "lisp" "(defvar *some-num* 5)
(defun add-5 (num) ; local variable num is introduced
  (setf num (+ num 5)))

;; num is assigned the value of *some-num*
(add-5 *some-num*) ; => 10
;; add-5 does not modify *some-num*
*some-num* ; => 5, not 10"))
              (p "If you want to modify the value of the global variable, you need to use "
                (code "setf") " on the global variable directly.")
              (pre
                (code :class "lisp" "(defun add-5 ()
  (setf *some-num* (+ *some-num* 5)))
(add-5)
some-num ; => 10"))
              (pre
                (code :class "lisp" "(defparameter *names* '(Micah Greg Takae Marcia Ena Mikasa))

(defun add-name (name sequence)
  (setf sequence (push name sequence)))
(add-name 'Guy *names*)"))))
          (section :id "first-class-functions"
            (hgroup
              (span)
              (h3 "First-Class Functions"))
            (div :class "outline-text-4" :id "text-orgabae553"
              (p "Common Lisp functions are first-class. Many of Lisp's functions can take functions as arguments.")
              (p "To pass a function as an argument, you have a few options.")
              (p "You can use "
                (code "function") ":")
              (pre
                (code :class "lisp" "(mapcar (function squirt-then-double) '(1 2 3 4 5))"))
              (p "Or, more commonly, use the reader macro "
                (code "#'") ":")
              (pre
                (code :class "lisp" "(mapcar #'squirt-then-double '(1 2 3 4 5))"))
              (p "Or you can use anonymous functions.")))
          (section :id "anonymous-functions"
            (hgroup
              (span)
              (h3 "Anonymous Functions"))
            (div :class "outline-text-4" :id "text-org37ed084"
              (p "You can use anonymous functions with "
                (code "lambda") ":")
              (pre
                (code :class "lisp" "((lambda (x) (* x x)) 5) ; => 25"))
              (p "Lambdas are useful when using functional programming functions like "
                (code "mapcar") " or "
                (code "remove-if-not") ".")
              (pre
                (code :class "lisp" "(mapcar #'(lambda (x) (+ (* x x) (* x x))) '(1 2 3 4 5)) ; => (2 8 18 32 50)")))))
        (section :id "lists"
          (hgroup
            (span)
            (h2 "LISTS"))
          (div :class "outline-text-3" :id "text-orgb89a47d")
          (section :id "in-the-beginning-was-the-cons"
            (hgroup
              (span)
              (h3 "In The Beginning Was The Cons"))
            (div :class "outline-text-4" :id "text-orgc9643da"
              (p "Lists are the most flexible and fundamental data structure in Common Lisp.")
              (p "The easiest way to make a list is like this:")
              (pre
                (code :class "lisp" "(list 'this 'is 'a 'list)
;; => (THIS IS A LIST)"))
              (p "That list contains four symbols. The more common way to create the above list is to use "
                (code "quote") ":")
              (pre
                (code :class "lisp" "(quote (this is a list))
;; => (THIS IS A LIST)"))
              (p "Or, using a reader macro:")
              (pre
                (code :class "lisp" "'(this is a list)
;; => (THIS IS A LIST)"))
              (p "If you are experienced in other languages like Python or JavaScript, you might think Lisp's lists are the same as in those languages. However, that isn't the case.")
              (p "The more fundamental data structure that lists are built on top of are cons cells. Lisp's lists are "
                (b "linked lists") " of cons cells.")
              (pre
                (code :class "lisp" "(cons 'this nil)

(cons 'this (cons 'is nil))

(cons 'this (cons 'is (cons 'a (cons 'linked (cons 'list nil)))))"))
              (p "A cons cell has two parts or slots: a "
                (code "car") " and a "
                (code "cdr") ". The car contains some data, and the cdr contains either more cons cells or "
                (code "nil") ". "
                (code "nil") " terminates the linked list branch.")
              (p "More importantly, and maybe confusingly, Common Lisp code is written using these very same cons cells.")
              (pre
                (code :class "lisp" ";; Parenthesis, then function name \"cons\", then data, all establishing a nested linked list.
(cons 'defun (cons 'sum (cons (cons 'x (cons 'y nil))
                              (cons
                               (cons '+
                                (cons 'x (cons 'y nil)))
                               nil))))

;; Parenthesis, then function name \"defun\", then data, all establishing a nested linked list.
(defun sum (x y)
  (+ x y))

;; quoting the \"sum\" definition returns the same output as the nested cons cells above.
(quote
 (defun sum (x y)
   (+ x y)))

;;... which is different from returning a string containing that code.
\"(defun sum (x y)
   (+ x y))\"

;; How is it different? Because I can evaluate it as code.
(eval
 (quote
  (defun sum (x y)
    (+ x y))))

;; And I can do the same to the cons cells above
(eval
 (cons 'defun (cons 'sum (cons (cons 'x (cons 'y nil))
                               (cons
                                (cons '+
                                      (cons 'x (cons 'y nil)))
                                nil)))))"))
              (p "The line between \"code\" and \"data\" that is clearly drawn in other languages like Python or JavaScript "
                (i "does not exist") " in Lisp, owing to the fact that code and data both share the same syntax and data structure.")))
          (section :id "basic-list-functions"
            (hgroup
              (span)
              (h3 "Basic List Functions"))
            (div :class "outline-text-4" :id "text-orge904a48"
              (p "Because lists are so fundamental in Lisp, there are many functions for manipulating lists.")
              (p "You can determine how many items are in the list:")
              (pre
                (code :class "lisp" "(length '(Gerald Sussman stole my wife and kicked my dog that skallywag))"))
              (p "You can reverse the order of the elements of the list:")
              (pre
                (code :class "lisp" "(reverse '(1 2 3 4 5))"))
              (p "You can get items in a list based on their position in the list:")
              (pre
                (code :class "lisp" "(setf my-num '(common lisp is a general purpose multi-paradigm programming language))
(first my-list)
(second my-list)
(ninth my-list)
(nth 3 my-list)"))
              (p "You can search for a single item in a list:")
              (pre
                (code :class "lisp" "(member 'general '(common lisp is a general purpose multi-paradigm programming language))"))
              (p "And you can select individual items by their index:")
              (pre
                (code :class "lisp" "(nth 4 genesis)"))
              (p "You can combine two lists:")
              (pre
                (code :class "lisp" "(append '(this list is a list) '(a list))"))))
          (section :id "lists-as-trees"
            (hgroup
              (span)
              (h3 "Lists as Trees"))
            (div :class "outline-text-4" :id "text-org67c832a"
              (p "Lists are a very simple data structure capable of making other data structures. Consider the linked list of cons cells above:")
              (pre
                (code :class "lisp" "(cons 'defun (cons 'sum (cons (cons 'x
                                     (cons 'y nil))
                               (cons
                                (cons '+
                                      (cons 'x (cons 'y nil)))
                                nil))))"))
              (p "While it is simply a linked list of cons cells, it's useful to think of it another way: a tree. Each cons cell has two parts: a car and a cdr. The car holds a leaf in the tree, whereas the cdr holds either another branch (in the common case) or another leaf. When working with lists as a tree, you can use "
                (code "car") " and "
                (code "cdr") " to access those two positions.")
              (pre
                (code :class "lisp" "(setq my-tree (cons 'defun (cons 'sum (cons (cons 'x
                                     (cons 'y nil))
                               (cons
                                (cons '+
                                      (cons 'x (cons 'y nil)))
                                nil)))))

(car my-tree) ; => DEFUN
(cdr my-tree) ; => (SUM (X Y) (+ X Y))"))
              (p "You can copy a tree:")
              (pre
                (code :class "lisp" "(copy-tree my-tree)"))
              (p "You can substitute leaves in the tree:")
              (pre
                (code :class "lisp" ";; Take all instances of y in the tree and replace them with z
(subst 'z 'y my-tree)

;; Substitute multiple leaves in a tree
(sublis '((sum . subtract) (+ . -)) my-tree)"))))
          (section :id "lists-as-tables"
            (hgroup
              (span)
              (h3 "Lists as Tables"))
            (div :class "outline-text-4" :id "text-org797cf59"
              (p
                (code "sublis") " takes a special type of list for its first argument: an association list, otherwise known as an alist or table. A table is a list with nested dotted lists–lists that have a non-nil leaf in the cdr position.")
              (pre
                (code :class "lisp" "(setf en-to-ja-table '((one . ichi)
                       (two . ni)
                       (three . san)
                       (four . yon)
                       (five . go)))"))
              (p "With tables, the car is a key and the cdr is a value. You can search a table by either key or value.")
              (pre
                (code :class "lisp" "(assoc 'two en-to-ja-table)
(rassoc 'yon en-to-ja-table)"))))
          (section :id "lists-as-sets"
            (hgroup
              (span)
              (h3 "Lists as Sets"))
            (div :class "outline-text-4" :id "text-orgd831553"
              (p "Lists can also be treated like sets–an unordered sequence of unique elements.")
              (pre
                (code :class "lisp" ";; Append will combine two lists regardless of the contents.
(append '(three two) '(one two three))

;; Adjoin will add a single element to a set, but only if that element doesn't exist in the set.
(adjoin 'three '(one two three))
(adjoin 'four '(one two three))"))
              (p "Contents of two sets can be compared to form new sets:")
              (pre
                (code :class "lisp" "(defparameter *pizza* '(salty sweet cheese sauce round carbs))
(defparameter *cake* '(sweet chocolate brown carbs round))

;; Return a set of unique elements that are present in both pizza and cake
(intersection *pizza* *cake*)
                                        ; => (CARBS ROUND SWEET)

;; Return a set that combines all unique elements of both *pizza* and *cake*
(union *cake* *pizza*)
                                        ; => (SAUCE CHEESE SALTY SWEET CHOCOLATE BROWN CARBS ROUND)

;; Return a set that includes all of the elements in *cake* that are not present in *pizza*
(set-difference *cake* *pizza*)
                                        ; => (BROWN CHOCOLATE)
;; ... or vise versa
(set-difference *pizza* *cake*)
                                        ; => (SAUCE CHEESE SALTY)"))
              (p "We can check if a set is a subset of some other set:")
              (pre
                (code :class "lisp" "(defparameter *cheese-pizza* '(salty cheese sauce round carbs))
(subsetp *cheese-pizza* *pizza*)

                                        ; => T")))))
        (section :id "control-flow"
          (hgroup
            (span)
            (h2 "CONTROL FLOW"))
          (div :class "outline-text-3" :id "text-org2af26e6")
          (section :id "true-and-false"
            (hgroup
              (span)
              (h3 "True and False"))
            (div :class "outline-text-4" :id "text-org32c8be4"
              (p
                (code "t") " is true and "
                (code "nil") " is false. In the type hierarchy, all data types except for "
                (code "nil") "–which is the empty list–extend "
                (code "t") ". That means that every value except for "
                (code "nil") " or the empty list are true.")))
          (section :id "equality-comparison"
            (hgroup
              (span)
              (h3 "Equality & Comparison"))
            (div :class "outline-text-4" :id "text-org3613743"
              (p "Common Lisp has a wide array of equality and comparison operators."))
            (section :id "math"
              (hgroup
                (span)
                (h4 "Math"))
              (div :class "outline-text-5" :id "text-org7db0aa6"
                (pre
                  (code :class "lisp" "(= 1 1)
(/= 1 1)
(> 199 180)
(>= 155 155)
(< 7 19)
(<= 77 77)"))))
            (section :id "general-use"
              (hgroup
                (span)
                (h4 "General Use"))
              (div :class "outline-text-5" :id "text-org485c575"
                (p "For symbols, variables, lists, and other objects, you need to use one of "
                  (code "eq") ", "
                  (code "eql") ", "
                  (code "equal") ", or "
                  (code "equalp") ". Each of them tests equality of different degrees. If you come from Python or JavaScript you'll usually expect functionality similar to "
                  (code "equal") " or "
                  (code "equalp") ".")
                (p
                  (code "eq") " will test equality of identity. Do these two objects share the same place in memory?")
                (pre
                  (code :class "lisp" "(defparameter *deez-nums* '(1 2 3 4 5))
(defparameter *your-nums* '(1 2 3 4 5))
(defparameter *gods-nums* '(one two three four five))

(eq *deez-nums* *your-nums*)                ; NIL, two different lists.
(eq *deez-nums* *deez-nums*)                ; T, same list.
(eq *gods-nums* '(one two three four five)) ; NIL
(eq 'one 'one)                              ; T, symbols are reused when they are from the same package.
(eq '(1 2 3 4 5) '(1 2 3 4 5))              ; NIL, two lists are constructed separately.
(eq #\\a #\\a)                                ; T
(eq #\\a #\\A)                                ; NIL
(eq \"hello\" \"hello\")                        ; NIL, strings are arrays of characters and are constructed separately.
(defparameter *greeting* \"Hello, world!\")
(eq *greeting* *greeting*)                  ; T, same array of characters."))
                (p
                  (code "eql") " is the same as "
                  (code "eq") ", except that if the arguments are characters or numbers of the same type then their values are compared.")
                (p
                  (code "equal") " tests structural similarity.")
                (pre
                  (code :class "lisp" "(equal '(1 2 3) '(1 2 3))               ; T
(equal '(1 3 2) '(1 2 3))               ; NIL
(equal \"hello\" \"hello\")                 ; T
(equal \"HELLO\" \"hello\")                 ; NIL
(equal 1 1.0)                           ; NIL, different types"))
                (p
                  (code "equalp") " is further lenient:")
                (pre
                  (code :class "lisp" "(equalp #\\a #\\A)                        ; T
(equalp \"hello\" \"HELLO\")                ; T, good for case-insensitive testing of characters or strings
(equalp 1 1.0)                          ; T, good for testing numbers across number types
(equalp '(1 2 3) '(1 3 2))              ; NIL"))))
            (section :id "characters-strings"
              (hgroup
                (span)
                (h4 "Characters & Strings"))
              (div :class "outline-text-5" :id "text-orgbd571d4"
                (p "Characters and strings have their own equality operators: "
                  (code "char-equal") " and "
                  (code "string-equal") ".")
                (pre
                  (code :class "lisp" "(char-equal #\\a #\\A)                    ; T, same as (equalp #\\a #\\A)
(string-equal \"hello\" \"HELLO\")          ; T, same as (equalp \"hello\" \"HELLO\")"))
                (p "Additionally, there are tests like "
                  (code "char-greaterp") ", "
                  (code "string=") ", "
                  (code "char<") ", etc.")
                (p "For more detail you should look at "
                  (a :href "https://novaspec.org/cl/f_equal" "the Hyperspec") "."))))
          (section :id "logical-operators"
            (hgroup
              (span)
              (h3 "Logical Operators"))
            (div :class "outline-text-4" :id "text-orged27651")
            (section :id "-and"
              (hgroup
                (span)
                (h4 "~and"))
              (div :class "outline-text-5" :id "text-orgf473913"
                (p
                  (code "and") " tests if more than two forms are true. The "
                  (code "and") " form returns the value returned by the last form inside it if all forms in it evaluate to "
                  (code "t") ". Otherwise, it returns "
                  (code "nil") ".")
                (pre
                  (code :class "lisp" "(and 't 5)
                                        ; => 5
(and 't 5 'hello)
                                        ; => HELLO
(and 'nil 5 'hello)
                                        ; => NIL"))
                (p
                  (code "and") " will evaluate all forms inside it.")))
            (section :id "-or-"
              (hgroup
                (span)
                (h4
                  (code "or")))
              (div :class "outline-text-5" :id "text-orgcebb6bb"
                (p
                  (code "or") " returns the value of the first form that evaluates to true. If no form passed to it returns a true value, it returns "
                  (code "nil") ".")
                (pre
                  (code :class "lisp" "(or 5 't 'nil 'hello)
                                        ; => 5
(or 'nil (> 1 5) (eq \"hello\" \"hello\"))
                                        ; => NIL"))
                (p
                  (code "or") " will stop evaluating forms on the first form that evaluates to true.")
                (pre
                  (code :class "lisp" "(or 'nil (> 5 10) (eq \"hello\" \"hello\")  ; all evaluate to nil
    (print \"Evaluated, returns true.\")
    (print \"Not evaluated.\"))
; \"Evaluated, returns true.\"  => \"Evaluated, returns true.\""))))
            (section :id "-not-"
              (hgroup
                (span)
                (h4
                  (code "not")))
              (div :class "outline-text-5" :id "text-orga3fe243"
                (p
                  (code "not") " will return "
                  (code "t") " if the inner form returns "
                  (code "nil") ", and "
                  (code "nil") " if that form returns "
                  (code "t") ".")
                (pre
                  (code :class "lisp" "(not (= 1 1))
                                        ; => NIL
(not (oddp 2))
                                        ; => T")))))
          (section :id "conditional-forms"
            (hgroup
              (span)
              (h3 "Conditional Forms"))
            (div :class "outline-text-4" :id "text-org0359b4d")
            (section :id "-if-"
              (hgroup
                (span)
                (h4
                  (code "if")))
              (div :class "outline-text-5" :id "text-orgb7f0e88"
                (p
                  (code "if") " is a special operator. It takes a test argument. If the test returns true, the next form is evaluated. If the test returns false, then the form after that is evaluated.")
                (pre
                  (code :class "lisp" "(if (> 10 1)                            ; if
    '10-is-greater-than-1               ; then branch
    '10-is-not-greater-than-1)          ; (optional) else branch"))))
            (section :id "-when-unless-"
              (hgroup
                (span)
                (h4
                  (code "when") " & "
                  (code "unless")))
              (div :class "outline-text-5" :id "text-orgcb1dd12"
                (p "If you don't need a second (else) branch, you can use "
                  (code "when") " or "
                  (code "unless") ":")
                (pre
                  (code :class "lisp" "(let ((num 0))
  (when (<= 1 num 10)
    (format t \"~&FROM WHEN EXPRESSION: ~a is between 1 and 10\" num)
    (print num)                         ; when and unless don't have an else branch.
    (print (+ num num)))                ; You can run multiple expressions after the test.

  (unless (<= 1 num 10)
    (format t \"~&FROM UNLESS EXPRESSION: ~a is not between 1 and 10\" num)
    (print num)
    (print (+ num num))))"))
                (p
                  (code "(unless test ...)") " is equivalent to "
                  (code "(when (not test) ...)") ".")))
            (section :id "-cond-"
              (hgroup
                (span)
                (h4
                  (code "cond")))
              (div :class "outline-text-5" :id "text-org8031e68"
                (p
                  (code "cond") " takes lists of tests and forms to evaluate if the test returns "
                  (code "t") ".")
                (pre
                  (code :class "lisp" "(defparameter *monster-happiness-meter* 39)
(let ((mhm *monster-happiness-meter*))
  (cond
    (    ; a list to hold a test and forms to evaluate if the test returns true.
     (= 0 mhm)                                                        ; test
     (format t \"~&Get this monster lifting weights at the gym, now!\") ; More than one form
     'take-monster-to-gym                                            ; can be evaluated.
     )
    ((< 70 mhm 89) (format t \"~&This monster is pretty happy.\") 'hang-out-with-monster)
    ((< 50 mhm 69) (format t \"~&This monster is feeling a little down.\") 'invite-monster-to-lunch)
    ((< 30 mhm 49) (format t \"~&Get this monster's mommy on the phone.\") 'call-monsters-mommy)
    ((< 1 mhm 29) (format t \"~&Did this monster's grandma die or something?\") 'console-monster)
    (t (format t \"~&This is the catchall fallback expression.\"))))
; Get this monster's mommy on the phone. => CALL-MONSTERS-MOMMY"))
                (p
                  (code "cond") " is a little tricky because of the parentheses.")
                (pre
                  (code :class "lisp" "(cond
  ((test) (code to run if test returns true) (more code if you want))
  ((test) (code to run if test returns true) (more code if you want))
  ((test) (code to run if test returns true) (more code if you want))
  ((test) (code to run if test returns true) (more code if you want)))"))
                (p
                  (code "cond") " will stop evaluating on the first form that returns "
                  (code "t") ".")))
            (section :id "-case-"
              (hgroup
                (span)
                (h4
                  (code "case")))
              (div :class "outline-text-5" :id "text-org52f63d8"
                (p
                  (code "case") " takes a key expression and then some clauses. It evaluates the clauses in order. If the value of the first item of a clause is "
                  (code "eql") " to the value returned by the key expression, the rest of the items in the clause are evaluated.")
                (p
                  (code "case") " is part of the family of case forms: "
                  (code "case") ", "
                  (code "ccase") ", "
                  (code "ecase") ", "
                  (code "typecase") ", "
                  (code "ctypecase") ", and "
                  (code "etypecase") ". If you know case statements from other languages, you understand the basic idea.")
                (p
                  (code "case") " and "
                  (code "typecase") " do nothing if no match is found. If you want to trigger errors when no match is found (rather than providing an fallback clause), use "
                  (code "ecase") " and "
                  (code "etypecase") ". If you want the option to provide a value for the key expression and continue the program, use "
                  (code "ccase") " and "
                  (code "ctypecase") ".")
                (pre
                  (code :class "lisp" "(defparameter *env* :DEVELOPMENT)

(defun check-environment ()
  (case *env*
    (:DEVELOPMENT \"logging all errors\")
    (:PRODUCTION \"locking in and locking down\")))

(check-environment)
                                        ; => \"logging all errors\"
(let ((*env* :PRODUCTION))
  (check-environment))
                                        ; => \"locking in and locking down\"
(let ((*env* :AWS))
  (check-environment))
                                        ; => NIL"))
                (p
                  (code "case") " will return "
                  (code "nil") " when none of the other cases match. You can set the fallback case using "
                  (code "t") " or "
                  (code "otherwise") ".")
                (pre
                  (code :class "lisp" "(defun check-environment ()
  (case *env*
    (:DEVELOPMENT \"logging all errors\")
    (:PRODUCTION \"locking in and locking down\")
    (otherwise \"you gotta set your *env* to :DEVELOPMENT or :PRODUCTION\")))

(let ((*env* :AWS))
  (check-environment))
                                        ; => \"you gotta set your *env* to :DEVELOPMENT or :PRODUCTION\""))
                (p "If you want to return an error if the argument falls through all the checks, use "
                  (code "ecase") ".")
                (pre
                  (code :class "lisp" "(defun check-environment ()
  (ecase *env*
    (:DEVELOPMENT \"logging all errors\")
    (:PRODUCTION \"locking in and locking down\")))

(let ((*env* :AWS))
  (check-environment))
;; :AWS fell through ECASE expression.
;; Wanted one of (:DEVELOPMENT :PRODUCTION).
;;    [Condition of type SB-KERNEL:CASE-FAILURE]

;; Restarts:
;;  0: [RETRY] Retry SLY evaluation request.
;;  1: [*ABORT] Return to SLY's top level.
;;  2: [ABORT] abort thread (#<THREAD tid=11923 \"slynk-worker\" RUNNING {70071BE463}>)"))
                (p "If you want a "
                  (i "continuable") " error, use "
                  (code "ccase") ".")
                (pre
                  (code :class "lisp" "(defun check-environment ()
  (ccase *env*
    (:DEVELOPMENT \"logging all errors\")
    (:PRODUCTION \"locking in and locking down\")))

(let ((*env* :AWS))
  (check-environment))
;; :AWS fell through CCASE expression.
;; Wanted one of (:DEVELOPMENT :PRODUCTION).
;;    [Condition of type SB-KERNEL:CASE-FAILURE]

;; Restarts:
;;  0: [STORE-VALUE] Supply a new value for *ENV*.
;;  1: [RETRY] Retry SLY evaluation request.
;;  2: [*ABORT] Return to SLY's top level.
;;  3: [ABORT] abort thread (#<THREAD tid=11955 \"slynk-worker\" RUNNING {7007B83833}>)"))
                (p "Notice that now you can store a value in "
                  (code "*env*") ". If you do, it will retry the case check with that value.")
                (p "The "
                  (code "typecase") " family works the same, but will check the type of value returned by the key expression.")
                (pre
                  (code :class "lisp" "(let ((x 5))
  (typecase x
    (list 'this-is-a-list)
    (number 'this-is-a-number)
    (function 'this-is-a-function)
    (otherwise 'i-dont-know-what-this-is)))
                                        ; => THIS-IS-A-NUMBER"))
                (p
                  (code "handler-case") " is another member of the "
                  (code "case") " family that works on conditions and errors.")))))
        (section :id "iteration"
          (hgroup
            (span)
            (h2 "ITERATION"))
          (div :class "outline-text-3" :id "text-org7cd232a")
          (section :id "iterating-by-looping"
            (hgroup
              (span)
              (h3 "Iterating by Looping"))
            (div :class "outline-text-4" :id "text-org81995cb"
              (p "Common Lisp has several different ways of iterating. The most popular is the "
                (code "loop") " macro:")
              (pre
                (code :class "lisp" "(loop :for item :in '(one two three)
      :do (print item))
                                        ; ONE
                                        ; TWO
                                        ; THREE  => NIL"))
              (p
                (code "loop") " is a macro that uses its own syntax. If you come from Python or JavaScript, it doesn't look so strange, but it looks different from typical Lisp code.")
              (pre
                (code :class "lisp" "(loop :for n :in '(1 2 3 4 5)
      :collect (* 2 (sqrt n)))
                                        ; => (2.0 2.828427 3.4641016 4.0 4.472136)"))))
          (section :id "iterating-by-mapping"
            (hgroup
              (span)
              (h3 "Iterating by Mapping"))
            (div :class "outline-text-4" :id "text-orgcf80847"
              (p "You can iterate by using a functional programming style mapping. You've seen it in action already:")
              (pre
                (code :class "lisp" "(mapcar #'squirt-then-double '(1 2 3 4 5))
(mapcar #'first '((1 2 3 4 5) (one two three four five) (what is going on here) (once upon a time)))"))
              (p
                (a :href "https://novaspec.org/cl/f_mapc" "There are several varieties of mapping functions") ". "
                (code "mapcar") " is the most common. The most general mapping function is "
                (code "map") ".")
              (pre
                (code :class "lisp" ";; Go through each character in the string and upper case it. Return a string
(map 'string #'char-upcase \"hello\")
;; Take an item from each of the lists and multiply them, returning a list. Finishes at the end of the shortest list.
(map 'list #'* '(2 4 6 8) '(3 5 7 9 11 13 15) '(10 10 10))"))))
          (section :id "iterating-by-reducing"
            (hgroup
              (span)
              (h3 "Iterating by Reducing"))
            (div :class "outline-text-4" :id "text-org98e68d9"
              (p "You can reduce multiple values down to one value by applying some function to successive items in the sequence with "
                (code "reduce") ".")
              (pre
                (code :class "lisp" "(reduce #'+ '(2 2 2))
(reduce #'* '(2 2 2))
(reduce #'append '((2 2 2) (3 3 3) (4 4)))"))
              (p "If you define a function to use for reducing, it needs to take two arguments:")
              (pre
                (code :class "lisp" "(defun sum (x y) (+ x y))
(reduce #'sum '(1 1 1 3 4 5 6 7))"))))
          (section :id "iterating-by-filtering"
            (hgroup
              (span)
              (h3 "Iterating by Filtering"))
            (div :class "outline-text-4" :id "text-org9690f06"
              (p "You can filter the contents of a sequence with "
                (code "remove-if-not") ":")
              (pre
                (code :class "lisp" "(remove-if-not #'oddp '(1 2 3 4 5 6 7 8 9 10))
                                        ; => (1 3 5 7 9)

(defparameter *dangerous-animals* '(lion tiger bear snake shark))

(defun safe-animal-p (animal)
  (not (member animal *dangerous-animals*)))

(remove-if-not #'safe-animal-p '(dog cat monkey lion hamster shark snake bear koala frog))
                                        ; => (DOG CAT MONKEY HAMSTER KOALA FROG)

(defun greater-than-50-p (num)
  (> num 50))

(remove-if-not #'greater-than-50-p '(40 50 30 90 80 10 70 100 25 60 55 2))
                                        ; => (90 80 70 100 60 55)"))
              (p "You can also use "
                (code "remove-if") ":")
              (pre
                (code :class "lisp" "(defun dangerous-animal-p (animal)
  (member animal *dangerous-animals*))
(remove-if #'dangerous-animal-p '(dog cat monkey lion hamster shark snake bear koala frog))
                                        ; => (DOG CAT MONKEY HAMSTER KOALA FROG)"))))
          (section :id "iterating-by-doing"
            (hgroup
              (span)
              (h3 "Iterating by Doing"))
            (div :class "outline-text-4" :id "text-orge627a8b")
            (section :id "-dotimes-"
              (hgroup
                (span)
                (h4
                  (code "dotimes")))
              (div :class "outline-text-5" :id "text-org8023153"
                (pre
                  (code :class "lisp" "(dotimes (index-var n [result-form])
  body)"))
                (p
                  (code "dotimes") " is for doing something a set number of times. Simple enough. It should feel familiar if you've ever done this in JavaScript:")
                (pre
                  (code :class "c" "for (let i = 0; i < 10; i++) {
 ...}"))
                (p "or this in Python:")
                (pre
                  (code :class "python" "for i in range(5):
    ..."))
                (pre
                  (code :class "lisp" "(dotimes (i 5)
  (if (= i 4)
      (print \"I'M NOT CRAZY!!!\")
      (print \"I'm not crazy!\")))

(let* ((num-list '(99 98 97 96 95))
       (times (length num-list)))
  (dotimes (i times)
    (let ((bottles-of-milk (nth i num-list)))
      (format t \"~&~a of bottles of milk on the wall, ~a bottles of milk.\" bottles-of-milk bottles-of-milk))))"))))
            (section :id "-dolist-"
              (hgroup
                (span)
                (h4
                  (code "dolist")))
              (div :class "outline-text-5" :id "text-org07fa188"
                (pre
                  (code :class "lisp" "(dolist (index-var list [result-form])
  body)"))
                (p
                  (code "dolist") " is similar. If you're doing simple stuff with lists, this is probably what you want.")
                (pre
                  (code :class "lisp" "(dolist (i '(yet another beautiful short list))
  (format t \"~&~a\" i))

(defun my-reverse (list)
  (let ((reversed nil))
    (dolist (i cat reversed)
      (push (pop cat) reversed))))

(defun check-all-even (nums)
  (dolist (i nums t)
    (format t \"~&Looking at ~a...\" i)
    (when (oddp i)
      (format t \"~&Ooops, this is odd!\")
      (return nil))))"))
                (p "The [result-form] is in square brackets because it's optional. If you don't set it, then you need to return the result-form manually.")
                (pre
                  (code :class "lisp" "(defun my-reverse-manual-return (list)
  (let ((reversed nil))
    (dolist (i cat)                     ; reversed not set as result-form
      (push (pop cat) reversed))
    reversed))                          ; reversed manually called at the end of the let block"))
                (p "Notice the use of "
                  (code "push") " and "
                  (code "pop") " here. They are both destructive functions that modify lists, either by adding or removing items. "
                  (code "push") " puts items at the front of the list, returning the modified list. "
                  (code "pop") " removes the first item and returns that item.")))
            (section :id "-do-"
              (hgroup
                (span)
                (h4
                  (code "do")))
              (div :class "outline-text-5" :id "text-org673706b"
                (pre
                  (code :class "lisp" "(do ((var1 init1 [update1])
     (var2 init2 [update2])
     ...)
    (test action-1... action-n) ; base case
 body)"))
                (p
                  (code "do") " is the most general and powerful of the "
                  (code "do") " family. It is also a bit complicated and difficult to read/understand.")
                (pre
                  (code :class "lisp" "(defun check-all-even-do (nums)
  (do ((n nums (cdr n)))
      ((null n) (return t))
    (format t \"~&Looking at ~a...\" (first n))
    (when (oddp (first n))
      (format t \"~&~a is odd.\" (first n))
      (return nil))))

(check-all-even-do '(2 4 6 7 8 10))"))
                (p "Unlike "
                  (code "dotimes") " and "
                  (code "dolist") ", which take care of incrementing the counter or stepping through the list, "
                  (code "do") " requires you to specify the step/update at the end of each iteration. It also requires you to take care of specifying the base case–the conditions for ending the loop.")
                (p
                  (code "do") " is useful especially for people who are comfortable with iterating recursively because recursion also requires the programmer to specify the stepping function and base case.")
                (pre
                  (code :class "lisp" "(defun check-all-even-recursive (nums)
  (cond ((null nums) t)
        ((oddp (car nums)) (format t \"~&~a is odd!\" (first nums)) nil)
        (t (format t \"~&~a is even.\" (car nums))
           (check-all-even-recursive (cdr nums)))))

(check-all-even-recursive '(2 4 6 7 8 10))"))
                (p "The step function is "
                  (code "(cdr nums)") ". Using "
                  (code "cdr") " to step through a list is called \"cdring down\" the list. The similarities between the recursive method and the "
                  (code "do") " method make it both powerful and often unergonomic. As a reader of code, when you see either a recursive or "
                  (code "do") " iteration, you have to check the step function and base case–something you don't have to do with "
                  (code "dotimes") ", "
                  (code "dolist") ", "
                  (code "mapcar") ", "
                  (code "remove-if-not") ", etc.")
                (p "To reiterate, the "
                  (code "loop") " iterator is much more widely used than the "
                  (code "do") " iterators, so if you wish for most other Lispers to understand your code and collaborate with others, you should generally prefer "
                  (code "loop") ".")
                (p "If you prefer a more applicative/functional style, then map/filter/reduce is the way to go."))))
          (section :id "early-returns"
            (hgroup
              (span)
              (h3 "Early Returns"))
            (div :class "outline-text-4" :id "text-orgfa292b1"
              (p "There are times when it's necessary to do an early return. We saw an example with "
                (code "check-all-even") ":")
              (pre
                (code :class "lisp" "(defun check-all-even (nums)
  (dolist (i nums t)
    (format t \"~&Looking at ~a...\" i)
    (when (oddp i)
      (format t \"~&Ooops, this is odd!\")
      (return nil))))"))
              (p "The result-form set for "
                (code "dolist") " is "
                (code "t") ", meaning that when we get to the end of the list we should return "
                (code "t") ".")
              (p "However, when we spot an odd number, we need to return from the "
                (code "dolist") " loop early using "
                (code "(return nil)") ".")))
          (section :id "-every-some-notevery-notany-"
            (hgroup
              (span)
              (h3
                (code "every") ", "
                (code "some") ", "
                (code "notevery") ", "
                (code "notany")))
            (div :class "outline-text-4" :id "text-org0b57685"
              (p "If you only need to test the sequence, "
                (code "check-all-even") " could be rewritten using "
                (code "every") ".")
              (pre
                (code :class "lisp" "(every #'evenp '(2 4 6 8 10))
                                        ; => T
(every #'evenp '(1 4 6 8 10))
                                        ; => NIL"))
              (p
                (code "every") " runs a test on all items of a sequence and returns "
                (code "t") " if the test returns "
                (code "t") " for every item.")
              (p "Similarly, "
                (code "some") " tests all items of a sequence, but will return "
                (code "t") " as soon as the test returns "
                (code "t") " for an item.")
              (pre
                (code :class "lisp" "(some #'oddp '(2 4 6 8))
                                        ; => NIL
(some #'oddp '(1 4 6 8))
                                        ; => T"))
              (p
                (code "notevery") " runs a test on all items of a sequence and returns "
                (code "t") " if the test returns "
                (code "nil") " for every item.")
              (pre
                (code :class "lisp" "(notevery #'oddp '(1 3 5))
                                        ; => NIL
(notevery #'oddp '(1 2 3 5))
                                        ; => T"))
              (p
                (code "notany") " runs a test on all items of a sequence and returns "
                (code "t") " if the test returns "
                (code "nil") " for one item.")
              (pre
                (code :class "lisp" "(notany #'oddp '(2 4 6))
                                        ; => T
(notany #'oddp '(1 2 4 6))
                                        ; => NIL")))))
        (section :id "strings-i-o"
          (hgroup
            (span)
            (h2 "STRINGS & I/O"))
          (div :class "outline-text-3" :id "text-orge95946d")
          (section :id "what-are-strings-"
            (hgroup
              (span)
              (h3 "What are strings?"))
            (div :class "outline-text-4" :id "text-org7545ed6"
              (p "Strings in Common Lisp are ~arrays= of characters. As a result, all operations that can be used on "
                (code "sequences") " can be used on arrays.")))
          (section :id "printing-information-to-repl"
            (hgroup
              (span)
              (h3 "Printing Information to REPL"))
            (div :class "outline-text-4" :id "text-org3a11876")
            (section :id "-print-"
              (hgroup
                (span)
                (h4
                  (code "print")))
              (div :class "outline-text-5" :id "text-org0061ff9"
                (p "You can print information in the REPL using "
                  (code "print") ".")
                (pre
                  (code :class "lisp" "(print \"hello world\")"))
                (p
                  (code "print") " both sends data to the REPL, but also returns the data as a value. That means you can place "
                  (code "print") " over many different kinds of code, making if useful for simple debugging.")
                (pre
                  (code :class "lisp" "(defun factorial (x)
  (labels ((_factorial (n)
             (cond ((= n 0) 1)
                   (t (print (* n (_factorial (- n 1))))))))
    (_factorial x)))

(factorial 5)

; 1
; 2
; 6
; 24
; 120  => 120 (7 bits, #x78, #o170, #b1111000)")))))
          (section :id "using-sequence-operations-on-strings"
            (hgroup
              (span)
              (h3 "Using Sequence Operations on Strings"))
            (div :class "outline-text-4" :id "text-orgbd5f557")
            (section :id "-concatenate-"
              (hgroup
                (span)
                (h4
                  (code "concatenate")))
              (div :class "outline-text-5" :id "text-orgc0687c4"
                (p
                  (code "concatenate") " combines two or more sequences–meaning it can combine lists, vectors, or strings. The first argument specifies the output type.")
                (pre
                  (code :class "lisp" "(concatenate 'list '(#\\h #\\e #\\l #\\l #\\o #\\space) '(#\\w #\\o #\\r #\\l #\\d))
                                        ; => (#\\h #\\e #\\l #\\l #\\o #\\  #\\w #\\o #\\r #\\l #\\d)
(concatenate 'vector '(#\\h #\\e #\\l #\\l #\\o #\\space) '(#\\w #\\o #\\r #\\l #\\d))
                                        ; => #(#\\h #\\e #\\l #\\l #\\o #\\  #\\w #\\o #\\r #\\l #\\d)
(concatenate 'string '(#\\h #\\e #\\l #\\l #\\o #\\space) '(#\\w #\\o #\\r #\\l #\\d))
                                        ; => \"hello world\"
(concatenate 'string \"hello \" \"world\")
                                        ; => \"hello world\""))))
            (section :id "-length-"
              (hgroup
                (span)
                (h4
                  (code "length")))
              (div :class "outline-text-5" :id "text-org4bffbc7"
                (p
                  (code "length") " can tell you the number of characters in a string.")
                (pre
                  (code :class "lisp" "(length \"hello world\")
                                        ; => 11"))))
            (section :id "-reverse-"
              (hgroup
                (span)
                (h4
                  (code "reverse")))
              (div :class "outline-text-5" :id "text-org5036fa5"
                (p "Just as with lists, "
                  (code "reverse") " can place characters in a string in reverse order.")
                (pre
                  (code :class "lisp" "(reverse \"YOU WILL REWRITE THAT THING IN COMMON LISP IMMEDIATELY\")
                                        ; => \"YLETAIDEMMI PSIL NOMMOC NI GNIHT TAHT ETIRWER LLIW UOY\""))))
            (section :id "-map-"
              (hgroup
                (span)
                (h4
                  (code "map")))
              (div :class "outline-text-5" :id "text-org36efdbc"
                (p
                  (code "map") " is the most general of the mapping functions. If you pass "
                  (code "string") " as the result type, you can run character operations on the string and get back a new string.")
                (pre
                  (code :class "lisp" "(map 'string #'char-upcase \"hello\")
                                        ; => \"HELLO\"")))))
          (section :id "string-specific-operations"
            (hgroup
              (span)
              (h3 "String Specific Operations"))
            (div :class "outline-text-4" :id "text-org01b5880")
            (section :id "string-modification"
              (hgroup
                (span)
                (h4 "String Modification"))
              (div :class "outline-text-5" :id "text-org3a800b5"
                (p "There are also string-specific functions.")
                (p "The previous "
                  (code "map") " call can be simplified using "
                  (code "string-upcase") ".")
                (pre
                  (code :class "lisp" "(string-upcase \"almighty\")
                                        ; => \"ALMIGHTY\""))
                (p "You can probably guess what "
                  (code "string-downcase") " does.")))
            (section :id "string-comparison-operators"
              (hgroup
                (span)
                (h4 "String Comparison Operators"))
              (div :class "outline-text-5" :id "text-orgb787a46"
                (p
                  (code "string=") " and "
                  (code "string-equal") " can be used to test if two strings are the same. "
                  (code "string=") " is case-sensitive, "
                  (code "string-equal") " is case-insensitive.")
                (pre
                  (code :class "lisp" "(string= \"almighty\" \"almighty\")
                                        ; => T
(string= \"almighty\" \"Almighty\")         ; case-sensitive
                                        ; => NIL
(string-equal \"almighty\" \"ALMIGHTY\")    ; case-insensitive
                                        ; => T"))
                (p "There are other string comparison operators: "
                  (code "string>") ", "
                  (code "string/=") ", "
                  (code "string-not-greaterp") ", etc."))))
          (section :id "streams"
            (hgroup
              (span)
              (h3 "Streams"))
            (div :class "outline-text-4" :id "text-org9b0b753"
              (p "Streams are either a source or destination for some data. Common Lisp uses streams for reading and writing files, etc.")))
          (section :id "writing-files"
            (hgroup
              (span)
              (h3 "Writing Files"))
            (div :class "outline-text-4" :id "text-orgf65b6d9"
              (p "Use "
                (code "with-open-file") " to create a block where a streaming connection with some file is active.")
              (pre
                (code :class "lisp" "(with-open-file (stream #P\"io-test.txt\" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (format stream \"~&Put this in the test file.~&This will be on a new line.~&\"))"))
              (p
                (code "with-open-file") " is a macro providing a shortcut to using the lower-level functions "
                (code "open") " and "
                (code "close") " combined with "
                (code "unwind-protect") ". It ensures that the connection to the file is closed before leaving the block.")
              (p "An exhaustive explanation of all the options "
                (code "open") " and "
                (code "with-open-file") " can take are in "
                (a :href "https://cl-community-spec.github.io/pages/open.html" "the Hyperspec") ". We'll cover the basics here.")))
          (section :id "reading-files"
            (hgroup
              (span)
              (h3 "Reading Files"))
            (div :class "outline-text-4" :id "text-orgf314f33"
              (p "If you want to read a file, set "
                (code ":direction") " to "
                (code ":input") " and use one of "
                (code "read") ", "
                (code "read-line") ", or "
                (code "read-char") ".")
              (pre
                (code :class "lisp" "(with-open-file (stream #P\"io-test.txt\" :direction :input :if-exists :supersede :if-does-not-exist :create)
  (loop for line = (read-line stream nil nil)
        while line
        do (format t \"~a~&\" line)))"))))
          (section :id "beyond-the-basics-w-files"
            (hgroup
              (span)
              (h3 "Beyond the Basics w/Files"))
            (div :class "outline-text-4" :id "text-orgc030fbb"
              (p "Common Lisp functions for working with file systems, paths, etc. are generally not portable between Lisp implementations. The package "
                (code "UIOP") " is the defacto-standard source of portable path and filesystem utilities.")))
          (section :id "-format-"
            (hgroup
              (span)
              (h3
                (code "format")))
            (div :class "outline-text-4" :id "text-org0af233a")
            (section :id "stream-output"
              (hgroup
                (span)
                (h4 "Stream Output"))
              (div :class "outline-text-5" :id "text-org5d8faab"
                (p
                  (code "format") " is used to write strings to output streams. The first argument is the stream. If set to "
                  (code "t") ", then it will send the input to "
                  (code "*standard-output*") ", which is output to the REPL.")
                (pre
                  (code :class "lisp" "(format t \"~&Almighty Lisp~%\")"))
                (p "However, as in the "
                  (code "with-open-file") " examples, you can also use "
                  (code "format") " to write to files, etc.")))
            (section :id "the-almighty-tilde"
              (hgroup
                (span)
                (h4 "The Almighty Tilde"))
              (div :class "outline-text-5" :id "text-org2abeef4"
                (p
                  (code "format") " has an extensive set of "
                  (code "control-string directives") " used for customizing how text is formatted. All of the directives begin with "
                  (code "tilde") ", such as "
                  (code "~a") ", "
                  (code "~&") ", etc. They also have an extensive set of modifiers. Complex format strings are vaguely similar to regular expressions and tend to get just as hairy.")
                (p "I will cover the bare minimum here. Refer to the "
                  (a :href "https://cl-community-spec.github.io/pages/Formatted-Output.html" "Hyperspec") " to learn all of the directives. I will explain any other directives as necessary."))
              (section :id "-tilda-a-"
                (hgroup
                  (span)
                  (h5
                    (code "Tilda a")))
                (div :class "outline-text-6" :id "text-org188ff76"
                  (p
                    (code "Tilda a") " will output the data in human readable, \"aesthetic\" format.")
                  (pre
                    (code :class "lisp" "(format t \"~a\" (aref \"hello\" 0))
                                        ; h => NIL
                                        ; NOTE: not #\\h"))
                  (p
                    (code "format") " takes an arbitrary number of arguments after the format string. For each argument, you need to supply another "
                    (code "Tilda a") ".")
                  (pre
                    (code :class "lisp" "(defparameter *alist* '((:micah 'male '39 'married 'japan)
                        (:takae 'female '34 'married 'japan)
                        (:mom 'female '70 'married 'america)
                        (:papa 'male '74 'married 'america)))
(loop for row in *alist*
      do (format t \"~&~a: ~a~%\" (first row) (rest row)))
                                        ; MICAH: ('MALE '39 'MARRIED 'JAPAN)
                                        ; TAKAE: ('FEMALE '34 'MARRIED 'JAPAN)
                                        ; MOM:   ('FEMALE '70 'MARRIED 'AMERICA)
                                        ; PAPA:  ('MALE '74 'MARRIED 'AMERICA)
                                        ;  => NIL

(loop for row in *alist*
      do (format t \"=?~%\" \"~a: ~a ~a ~a ~a\" row))"))))
              (section :id "-tilda-and-tilda-"
                (hgroup
                  (span)
                  (h5
                    (code "Tilda &") " and "
                    (code "Tilda %")))
                (div :class "outline-text-6" :id "text-orge291162"
                  (p "These directives output newlines. "
                    (code "Tilda %") " will always output a newline. "
                    (code "Tilda &") " will only output a newline if the output stream is not on a newline already."))))
            (section :id "format-directive-cheat-sheet"
              (hgroup
                (span)
                (h4 "Format Directive Cheat Sheet"))
              (div :class "outline-text-5" :id "text-org645c570"
                (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                  (colgroup
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~D")
                      (td :class "org-left" "Prints integer in decimal (base 10). Supports width, padding, commas.")
                      (td :class "org-left" "mincol=0, padchar=0/space, comma=,, interval=3")
                      (td :class "org-left" "No commas")
                      (td :class "org-left" "Always print sign (+/-)")
                      (td :class "org-left" "(format nil \"~:D\" 1234) => \"1,234\"; (format nil \"~@D\" -1234) => \"-1234\"; (format nil \"~12,'0D\" 42) => \"0000000042\"")
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~O")
                      (td :class "org-left" "Prints integer in octal (base 8). Same as ~D.")
                      (td :class "org-left" "Same as ~D")
                      (td :class "org-left" "No commas")
                      (td :class "org-left" "Always print sign")
                      (td :class "org-left" "(format nil \"~O\" 8) => \"10\"")
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~G")
                      (td :class "org-left" "General float: chooses ~F or ~E.")
                      (td :class "org-left" "w=0, d=0, e=3, k=1, overflow=#, pad=space, expchar=G")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "Always sign")
                      (td :class "org-left" "(format nil \"~G\" 0.031) => \"0.031\"; (format nil \"~4G\" 314159) => \"3.142e5\"")
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~%")
                      (td :class "org-left" "Unconditional newline(s).")
                      (td :class "org-left" "reps=1 (newlines)")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \"~%\") => \"\\n\"; (format nil \"~2%\") => \"\\n\\n\"")
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~")
                      (td :class "org-left" " ")
                      (td :class "org-left" "Page separator (formfeed).")
                      (td :class "org-left" "reps=1")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \"~")
                      (td :class "org-left" "\") => formfeed char"))
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~<newline>")
                      (td :class "org-left" "Ignore newline and following whitespace in format string.")
                      (td :class "org-left" "None")
                      (td :class "org-left" "Preserve following whitespace")
                      (td :class "org-left" "Ignore following whitespace")
                      (td :class "org-left" "Used for multi-line: ~\\n  text => \"text\" (ignores indent)")
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~*")
                      (td :class "org-left" "Argument navigation: skip args.")
                      (td :class "org-left" "n=1 (args to skip)")
                      (td :class "org-left" "Back up n args")
                      (td :class "org-left" "Goto nth arg (1-based)")
                      (td :class "org-left" "(format nil \"~*~A\" \"x\" \"y\") => \"y\" (skips x); (format nil \"~3:*~A\" a b c d) => \"d\" (goto 3rd back? Wait, : backs up)")
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~_")
                      (td :class "org-left" "Pretty-print newline (pprint-newline).")
                      (td :class "org-left" "None")
                      (td :class "org-left" ":fill or :mandatory style")
                      (td :class "org-left" ":miser style")
                      (td :class "org-left" "Used in ~<…~> for logical blocks")
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
                      (td :class "org-left" " "))
                    (tr
                      (td :class "org-left" "~I")
                      (td :class "org-left" "Indent in pretty printing.")
                      (td :class "org-left" "n=0 (spaces)")
                      (td :class "org-left" "Relative to block start")
                      (td :class "org-left" "N/A")
                      (td :class "org-left" "(format nil \"~2I~A~I\" \"text\") indents \"text\" by 2")
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
                      (td :class "org-left" " ")))))
              (section :id "-tilde-d-"
                (hgroup
                  (span)
                  (h5
                    (code "TILDE D")))
                (div :class "outline-text-6" :id "text-org683043f"
                  (pre
                    (code :class "lisp" "(defmethod format-money (stream (this us-dollar))
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
                    (code "(floor -5 100)") " will return "
                    (code "-1") " rather than "
                    (code "0") "), so we make all negative integers positive before formatting them, adding the minus-sign back at the beginning of the format string.")
                  (p
                    (code "~2,'0D") " looks crazy, I know. Format directives can be modified by optional parameters–separated by commas–and by modifiers "
                    (code "COLON") " or "
                    (code "AT-SIGN") ". For "
                    (code "TILDE D") ", we have four possible parameters:")
                  (ol :class "org-ol"
                    (li "mincol")
                    (li "padchar")
                    (li "comma")
                    (li "interval"))
                  (p "Let's take a look at them in action:")
                  (pre
                    (code :class "lisp" "(let ((small-num 7)
      (big-num 987654321))
  (format t \"~%~40a | ~d | ~d\"                          \"no modifications\"                  small-num big-num)
  (format t \"~%~40a | ~4d | ~4d\"                        \"mincol of 4\"                       small-num big-num)
  (format t \"~%~40a | ~4,'xd | ~4,'xd\"                  \"mincol padding using character x\"  small-num big-num)
  (format t \"~%~40a | ~4,'x:d | ~4,'x:d\"                \"colon added\"                       small-num big-num)
  (format t \"~%~40a | ~4,'x,'x:d | ~4,'x,'x:d\"          \"commas replaced with x\"            small-num big-num)
  (format t \"~%~40a | ~4,'x,'x,1:d | ~4,'x,'x,1:d\"      \"comma interval set to 1\"           small-num big-num)
  (format t \"~%~40a | ~4,'x@d | ~4,'x,'x,1@d\"           \"colon replaced with at-sign\"       small-num big-num)
  (format t \"~%~40a | ~4,'x@:d | ~4,'x,'x,1@:d\"         \"at-sign and colon combined\"        small-num big-num))"))
                  (p "Returns:")
                  (pre :class "example" :id "org21b27d1" "no modifications                         | 7 | 987654321
mincol of 4                              |    7 | 987654321
mincol padding using character x         | xxx7 | 987654321
colon added                              | xxx7 | 987,654,321
commas replaced with x                   | xxx7 | 987x654x321
comma interval set to 1                  | xxx7 | 9x8x7x6x5x4x3x2x1
colon replaced with at-sign              | xx+7 | +987654321
at-sign and colon combined               | xx+7 | +9x8x7x6x5x4x3x2x1 => NIL")
                  (p "It should be a bit easier now to understand the format string for "
                    (code "format-money") ":"
                    (code "~a") ", just display the "
                    (code "currency-sign") "."
                    (code "~:D") ", add commas to dollars side where appropriate."
                    (code ".") ", we add the dot."
                    (code "~2,'0D") ", set the "
                    (code "mincol") " to 2 and pad empty space with 0s. This ensures that "
                    (code "(usd 5)") " returns "
                    (code "$0.05") " and not "
                    (code "$0.5") "."))))))
        (section :id "-craps-"
          (hgroup
            (span)
            (h2
              (code "CRAPS")))
          (div :class "outline-text-3" :id "text-org3a6368d"
            (p "This how-to guide comes from "
              (a :href "https://www.cs.cmu.edu/afs/cs.cmu.edu/user/dst/www/LispBook/index.html" "Common Lisp: A Gentle Introduction to Symbolic Computation") ".")
            (p "In this how-to guide I will show you how to build a casino game called craps. Here are the rules:")
            (ul :class "org-ul"
              (li "The game is played by throwing two dice.")
              (li "When the player throws the dice, the player wins instantly if he rolls a 7 or 11.")
              (li "He loses instantly if he rolls 2, 3, or 12.")
              (li "If the player rolls any other number, it becomes his \"point\".")
              (li "He wins the game if he rolls the \"point\" again.")
              (li "He loses if he rolls a 7.")))
          (section :id "step-1-make-dice"
            (hgroup
              (span)
              (h3 "Step 1: Make Dice"))
            (div :class "outline-text-4" :id "text-org6764e30"
              (p "Let's start by defining a dice roll.")
              (pre
                (code :class "lisp" "(defun roll-die ()
  (+ 1 (random 6)))"))
              (p "Random numbers are generated with the function "
                (code "random") ". The argument "
                (code "5") " here is the upper limit. The lower limit is 0 and it can't be set with "
                (code "random") ", so "
                (code "(random 6)") " generates a number between 0 and 5. We add 1 to the result of "
                (code "(random 6)") " to get a random number between 1 and 6.")
              (p "To play craps, we need to roll two dice at a time. That's easy:")
              (pre
                (code :class "lisp" "(defun roll-dice ()
  (list (roll-die) (roll-die)))"))
              (p "This list with two numbers is a "
                (code "dice") ", our data structure that we can pass around and work with.")))
          (section :id "step-2-defining-win-and-lose-conditions"
            (hgroup
              (span)
              (h3 "Step 2: Defining Win and Lose Conditions"))
            (div :class "outline-text-4" :id "text-orgbddfee3"
              (p "Now we need to determine of the value of a dice roll is one of the instant win or instant loss values. To reiterate, an instant win occurs when the player rolls a 7 or 11. In other words, the combined value of the two dice must be either 7 or 11. For an instant loss, the combined value must be either 2, 3, or 12.")
              (pre
                (code :class "lisp" "(defun instant-win-p (dice)
  (or (= 7 (reduce #'+ dice))
      (= 11 (reduce #'+ dice))))

(defun instant-loss-p (dice)
  (or (= 2 (reduce #'+ dice))
      (= 3 (reduce #'+ dice))
      (= 12 (reduce #'+ dice))))"))
              (p "Functions that return "
                (code "t") " or "
                (code "nil") " (or some data that matches the check) are ended with \"p\" by convention.")
              (p "These two functions take a "
                (code "dice") ", add up their values, and then check if they equal the instant win or instant lose numbers.")
              (p "Since we know that a "
                (code "dice") " only has two values, we could have used "
                (code "first") " and "
                (code "last") ", or "
                (code "nth") ", but "
                (code "reduce") " is pretty convenient here.")
              (p "There's a lot of repetition, though. Let's fix that with "
                (code "let") ".")
              (pre
                (code :class "lisp" "(defun instant-win-p (dice)
  (let ((result (reduce #'+ dice)))
    (or (= 7 result)
        (= 11 result))))

(defun instant-loss-p (dice)
  (let ((result (reduce #'+ dice)))
    (or (= 2 result)
        (= 3 result)
        (= 12 result))))"))))
          (section :id "step-3-play"
            (hgroup
              (span)
              (h3 "Step 3: Play"))
            (div :class "outline-text-4" :id "text-orgd6bd06e"
              (p "We can \"roll\" the dice, and we can determine an instant win or loss. Now, let's test them out together.")
              (pre
                (code :class "lisp" "(defun play-craps ()
  (let ((dice (roll-dice)))
    (format t \"~&You rolled ~a and ~a\" (first dice) (second dice))
    (cond ((instant-win-p dice) (format t \"~&You win!\"))
          ((instant-loss-p dice) (format t \"~&You lose...\")))))"))
              (p "This is simple enough. In "
                (code "play-craps") ", we "
                (code "roll-dice") ", saving the value to a variable called "
                (code "dice") ".")
              (p
                (code "format") " is for printing text to the REPL or other streams. The string between quotes is called a format string. It has a complex grammar for printing and interpolating values. We will cover "
                (code "format") " in more detail later, but we'll start with the basics here.")
              (p
                (code "&") " will add a new line if one doesn't already exist. "
                (code "~a") " will interpolate a value passed to format after the format string. The first call to "
                (code "format") " has two values after the format string. The first one will be interpolated into the place where the first "
                (code "~a") " exists. The second value will be interpolated into the next "
                (code "~a") ".")
              (p
                (code "cond") " is special operator for executing code based on different conditions. "
                (code "cond") " is typically used when there are more than two condition checks. The "
                (code "cond") " block will do each check from top to bottom. It returns the value of the first condition that returns "
                (code "t") ".")
              (p "If the player doesn't instantly win or lose, they need to know what their \"point\" is. Obviously, they can do the math by looking at the values of the first and second die, but let's just make it convenient for the player.")
              (pre
                (code :class "lisp" "(defun play-craps ()
  (let ((dice (roll-dice)))
    (format t \"~&You rolled ~a and ~a\" (first dice) (second dice))
    (cond ((instant-win-p dice) (format t \"~&You win!\"))
          ((instant-loss-p dice) (format t \"~&You lose...\"))
          (t (format t \"~&Your point is: ~a\" (reduce #'+ dice))))))"))))
          (section :id "step-4-continue-rolling-for-point"
            (hgroup
              (span)
              (h3 "Step 4: Continue Rolling for Point"))
            (div :class "outline-text-4" :id "text-org610e6c2"
              (p "We can throw dice and determine an instant win or loss. Now we need to give the player the ability to continue playing and roll for a point.")
              (p "To keep things extra simple, we're going to ask the player to do more than just run "
                (code "play-craps") ". They will need to run one other function, "
                (code "try-for-point") ", manually inputing their \"point\" as an argument.")
              (pre
                (code :class "lisp" "(defun try-for-point (point)
  (let* ((dice (roll-dice))
         (roll-point (reduce #'+ dice)))
    (format t \"~&You rolled: ~a\" roll-point)
    (cond ((= roll-point point) (format t \"~&You win!\"))
          ((= roll-point 7) (format t \"~&You lose!\"))
          (t (format t \"~&Roll again.\")))))"))
              (p "Again, we roll, but since we are going to be using the value of the dice a few times, we can save the points in "
                (code "roll-point") ". Notice that we use "
                (code "let*") " because otherwise "
                (code "(reduce #'+ dice)") " will return an error saying that a variable of "
                (code "dice") " hasn't been set. In order to use variables defined earlier in the let block, we need to use "
                (code "let*") ".")
              (p "In this version of the game, the player will need to repeatedly run "
                (code "try-for-point") " manually. We'll take a look at making a bit more ergonomic later.")))
          (section :id "full-code"
            (hgroup
              (span)
              (h3 "FULL CODE"))
            (div :class "outline-text-4" :id "text-org7d20f7e"
              (pre
                (code :class "lisp" "(defun roll-die ()
  (+ 1 (random 6)))

(defun roll-dice ()
  (list (roll-die) (roll-die)))

(defun instant-win-p (dice)
  (let ((result (reduce #'+ dice)))
    (or (= 7 result)
        (= 11 result))))

(defun instant-loss-p (dice)
  (let ((result (reduce #'+ dice)))
    (or (= 2 result)
        (= 3 result)
        (= 12 result))))

(defun play-craps ()
  (let ((dice (roll-dice)))
    (format t \"~&You rolled ~a and ~a\" (first dice) (second dice))
    (cond ((instant-win-p dice) (format t \"~&You win!\"))
          ((instant-loss-p dice) (format t \"~&You lose...\"))
          (t (format t \"~&Your point is: ~a\" (reduce #'+ dice))))))

(defun try-for-point (point)
  (let* ((dice (roll-dice))
         (roll-point (reduce #'+ dice)))
    (format t \"~&You rolled: ~a\" roll-point)
    (cond ((= roll-point point) (format t \"~&You win!\"))
          ((= roll-point 7) (format t \"~&You lose!\"))
          (t (format t \"~&Roll again.\")))))")))))
        (section :id "-tic-tac-toe-"
          (hgroup
            (span)
            (h2
              (code "TIC-TAC-TOE")))
          (div :class "outline-text-3" :id "text-orge428698"
            (p "#+COMMENT Reorganize structure as in first step of tutorial. Reconsider Discussion section."))
          (section :id "introduction"
            (hgroup
              (span)
              (h3 "Introduction"))
            (div :class "outline-text-4" :id "text-org31d1f47"
              (p "For this project we'll be making a game of tic-tac-toe complete with a computer opponent. Tic-tac-toe is a relatively common coding project. The version we'll be making is by David Touretzky.")
              (p "The purpose of this project is to help you get a feel for what editing code is like in Emacs. You'll have a chance to try out structural editing (if you want), and you'll get some practice with tricky forms like "
                (code "let") " and "
                (code "cond") " that use an abundance of parentheses.")
              (p "Common Lisp code tends to be wider than other languages. This is partially because of the culture of using unabbreviated names, but also because of Lisp's functional programming roots. As a result, Lisp programmers often make smaller abstractions to make code more readable and fit into the line-width limits of the editor. This project will give you some exposure to both the \"problem\" of wider code and the solutions for it.")
              (p "In order to get the most out of this project, you should follow along and actually type out the code in Emacs, rather than copying and pasting.")))
          (section :id "tutorial"
            (hgroup
              (span)
              (h3 "Tutorial"))
            (div :class "outline-text-4" :id "text-org984301e")
            (section :id "choose-data-representation"
              (hgroup
                (span)
                (h4 "Choose data representation"))
              (div :class "outline-text-5" :id "text-orgcbba043")
              (section :id "code"
                (hgroup
                  (span)
                  (h5 "code"))
                (div :class "outline-text-6" :id "text-org4ad75f6"
                  (pre
                    (code :class "lisp" "(defvar *board*)

(defun reset-board ()
  (setf *board* (list 'board
                      0 0 0
                      0 0 0
                      0 0 0)))

(defparameter *player-one* 1)
(defparameter *player-two* 10)"))))
              (section :id "explanation"
                (hgroup
                  (span)
                  (h5 "explanation"))
                (div :class "outline-text-6" :id "text-orge0218b4"
                  (p "The first step in solving a problem–like how to make a tic-tac-toe game–is to decide how we will represent data. The simpler the data representation, the simpler the rest of the code will be; thus, we want to choose the simplest data representation that can solve the problem.")
                  (p "Tic-tac-toe works like this:")
                  (ul :class "org-ul"
                    (li "There are two players.")
                    (li "Each take turns putting their \"piece\" on the board–either an X or an O.")
                    (li "If either player gets three of their pieces in a row vertically, horizontally, or diagonally, they win.")
                    (li "If all spaces are filled without any player winning, it's a draw."))
                  (p "The game is simple, so the data representation can be simple.")
                  (p "The board is a flat list of 0's representing empty spaces. "
                    (code "board") " is a filler symbol to make later code more intuitive to understand. We use "
                    (code "setf") " to assign the globally scoped "
                    (code "*board*") " variable to the value "
                    (code "'(board 0 0 0 0 0 0 0 0)") ".")
                  (p "We represent player \"pieces\" as 1 and 10.")
                  (p "There are numerous advantages to representing the board as a \"flat\" list and the players as abstract numbers, rather than directly as the strings \"X\" and \"O\".")
                  (ul :class "org-ul"
                    (li "Data access is trivial.")
                    (li "Detecting player piece positions is easy.")
                    (li "Adding an AI opponent is simple."))))
              (section :id "instructions"
                (hgroup
                  (span)
                  (h5 "instructions"))
                (div :class "outline-text-6" :id "text-org57bc16d"
                  (ul :class "org-ul"
                    (li "Type the above code into a buffer named "
                      (code "tic-tac-toe.lisp") ".")
                    (li "Compile each form individually with "
                      (code "Sly->Compilation->Compile Defun"))
                    (li "Without typing anything more, practice evaluating the symbols "
                      (code "*player-one*") " and "
                      (code "*player-two*") " with "
                      (code "Sly->Evaluation->Eval Defun") ".")
                    (li "Type "
                      (code "(reset-board)") " in the buffer and then evaluate that form. What is the value of "
                      (code "*board*") "?")))))
            (section :id "write-functions-for-manipulating-data"
              (hgroup
                (span)
                (h4 "Write functions for manipulating data"))
              (div :class "outline-text-5" :id "text-org5c0aed6"
                (p "Now that we have our data representations settled, we need a way to manipulate it.")
                (pre
                  (code :class "lisp" "(defun place-piece (board piece position)
  (setf (nth position board) piece)
  board)"))
                (p "Using "
                  (code "setf") " with "
                  (code "nth") " here is similar to doing something like "
                  (code "var[n] = my-value") " in other languages. You "
                  (code "setf") " to a "
                  (code "place") ", such as a variable, a hashtable key, list position, array index, etc.")
                (p "Here we assign the "
                  (code "place") " in our "
                  (code "*board*") " to the value of "
                  (code "piece") ". The last value returned by the last form evaluated in a function becomes that function's return value. We return the "
                  (code "board") " to be able to show the updated board to the players.")
                (p "We also need a way to map positions on the board to all possible winning positions–Touretzky calls them triplets.")
                (pre
                  (code :class "lisp" "(defparameter *triplets* '((1 2 3) (4 5 6) (7 8 9) ; horizontal winning positions
                           (1 4 7) (2 5 8) (3 6 9) ; vertical winning positions
                           (1 5 9) (3 5 7)))       ; diagonal winning positions"))))
            (section :id "write-game-logic"
              (hgroup
                (span)
                (h4 "Write game logic"))
              (div :class "outline-text-5" :id "text-org1bc3d30"
                (p "Next, we need a way to calculate the state of those triplets.")
                (pre
                  (code :class "lisp" "(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet) (sum-triplet board triplet)) *triplets*))"))
                (p
                  (code "mapcar") " is one of the many functions that takes a function as an argument. "
                  (code "mapcar") " will apply the function to each item in the sequence and collect them into a list, returning the list.")
                (p "Let's test it out by placing some pieces manually.")
                (pre
                  (code :class "lisp" "(place-piece *board* *player-one* 1)
                                        ; => (BOARD 1 0 0 0 0 0 0 0 0)
(place-piece *board* *player-two* 2)
                                        ; => (BOARD 1 10 0 0 0 0 0 0 0)
(place-piece *board* *player-one* 5)
                                        ; => (BOARD 1 10 0 0 1 0 0 0 0)
(place-piece *board* *player-two* 9)
                                        ; => (BOARD 1 10 0 0 1 0 0 0 10)
(place-piece *board* *player-one* 7)
                                        ; => (BOARD 1 10 0 0 1 0 1 0 10)
(place-piece *board* *player-two* 3)
                                        ; => (BOARD 1 10 10 0 1 0 1 0 10)
(place-piece *board* *player-one* 4)
                                        ; => (BOARD 1 10 10 1 1 0 1 0 10)"))
                (p "Now let's test "
                  (code "compute-triplet") ".")
                (pre
                  (code :class "lisp" "(compute-sums *board*)
                                        ; => (21 2 11 3 11 20 12 12)"))
                (p "We can see now that player one, represented as 1s on the board, occupies all three spaces in a triplet. We have a winner, but our program doesn't know that yet. Let's add win and tie detection.")
                (pre
                  (code :class "lisp" "(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *player-one*) sums)
        (member (* 3 *player-two*) sums))))

(defun tie-p (board)
  (not (member 0 board)))

(winner-p *board*)
                                        ; => (3 11 20 12 12)
(tie-p *board*)
                                        ; => NIL"))
                (p
                  (code "member") " is called a "
                  (code "semi-predicate") ": it searches a sequence like "
                  (code "sums") " for an item like "
                  (code "(* 3 *player-one*)") ". If none is found, it returns "
                  (code "nil") ". If one is found, however, it doesn't return "
                  (code "t") "; it returns a list of the item plus the "
                  (code "rest") " of the list after the item.")
                (p "We can add pieces to the board, calculate the state of the board, and detect a winner. If we make an interface for two human players, we can have a game.")))
            (section :id "representing-data-to-the-player"
              (hgroup
                (span)
                (h4 "Representing data to the player"))
              (div :class "outline-text-5" :id "text-org2b22fff"
                (p "We need a way to show the board to players using "
                  (code "format") ". Instead of trying to do everything at once, we'll break it down into pieces, starting with converting player pieces from numbers to letters")
                (pre
                  (code :class "lisp" "(defun convert-to-letter (piece)
  (ecase piece
    (0  \" \")
    (1  \"X\")
    (10 \"O\")))

(convert-to-letter 1)
                                        ; => X
(convert-to-letter 10)
                                        ; => O
(defun opponent (piece)
  (if (= piece 1)
      10
      1))"))
                (p "Now let's print a row from the board.")
                (pre
                  (code :class "lisp" "(defun print-row (x y z)
  (format t \"~& ~a | ~a | ~a ~%\" (convert-to-letter x) (convert-to-letter y) (convert-to-letter z)))"))
                (p "Now let's print the board.")
                (pre
                  (code :class "lisp" "(defun print-board (board)
  (format t \"~&\")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t \"-----------\")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t \"-----------\")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t \"~&=&\"))

(print-board *board*)
;  X | O | O
; -----------
;  X | X |
; -----------
;  X |   | O
;  => NIL"))
                (p "Here's where having "
                  (code "board") " as a filler item in the "
                  (code "*board*") " list is useful: the calls to "
                  (code "nth") " here are intuitive to understand.")))
            (section :id "getting-user-input"
              (hgroup
                (span)
                (h4 "Getting user input"))
              (div :class "outline-text-5" :id "text-org36b5d24"
                (p "Now we need to get player input. We need to ensure that our input is well-formed and that the move from the player is legal.")
                (pre
                  (code :class "lisp" "(defun read-legal-move (piece board)
  (let ((move (read)))
    (cond ((not (integerp move))                                                    ; Condition
           (format t \"~&Moves must be a number between 1-9. Your move: ~a~%\" move)  ; Code to run if condition is true.
           (print-board board)                                                      ; Multiple forms can be evaluated
           (read-legal-move piece board))                                           ; after the condition.
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
                  (code "cond") " form–which can look pretty hairy to our new Lisp brothers–we run a few checks. "
                  (code "integerp") " is a "
                  (code "predicate") " function (with names typically ending with "
                  (code "p")
                  (code "-p") ") that checks if the user input is an integer. If the player input isn't a number, we tell them we need a number, print the board, and let the player try again.")
                (p "The next check makes sure that the number the user inputted was a number between 1 and 9.")
                (p "Finally, we need to check that the space chosen by the player is empty.")
                (p "If the "
                  (code "move") " passes all the checks, then the "
                  (code "cond") " will evaluate the final form "
                  (code "(t move)") ". This is the conventional way of providing a default branch in the "
                  (code "cond") " if all other conditions return "
                  (code "nil") ". In this instance, we just return "
                  (code "move") ".")
                (p "Now we can make a move.")
                (pre
                  (code :class "lisp" "(defun move (piece board)
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
                  (code "let*") ", unlike the regular "
                  (code "let") ", can bind variables to values that were computed earlier in the form. We pass "
                  (code "move") " to "
                  (code "place-piece") " and bind "
                  (code "updated-board") " to the value returned. If we used "
                  (code "let") " instead, we would get an error.")))
            (section :id "write-computer-moving-logic"
              (hgroup
                (span)
                (h4 "Write computer moving logic"))
              (div :class "outline-text-5" :id "text-org15850c9"
                (p "At this point, the human-vs-human version of the game is feature-complete. What we want now is to add a computer opponent.")
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
                  (h5 "Choose a strategy"))
                (div :class "outline-text-6" :id "text-org31ae1cd"
                  (pre
                    (code :class "lisp" "(defun choose-move (board)
  (or (make-three-in-a-row board)       ; Take the winning move.
      (block-opponent-win board)        ; Block the opponent.
      (take-random-position board)))    ; Take a random position."))
                  (p
                    (code "or") " will evaluate its arguments in order. It will stop evaluation on the first argument that returns a non-nil value.")))
              (section :id "finding-a-winning-move"
                (hgroup
                  (span)
                  (h5 "Finding a winning move"))
                (div :class "outline-text-6" :id "text-org5869832"
                  (p "The computer needs to know if there it has a potential win. There is a potential win if any of the triplets sum to 20.")
                  (p "First, let's make a test board.")
                  (pre
                    (code :class "lisp" "(defparameter *test-board* '(board
                             10 1 0
                             10 1 0
                             0 0 0))"))
                  (p "What we want to is to find a triplet that sums to 20.")
                  (pre
                    (code :class "lisp" "(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*)
                                        ; => (1 4 7)"))
                  (p
                    (code "find-if") " takes a predicate function and returns the first value in a sequence that evaluates to "
                    (code "t") " when the predicate is applied to it. It iterates over "
                    (code "*triplets*") ", and tests if any of the triplets on the board sum up to 20.")
                  (p "If we find a triplet with a winning move, we want to return the position on the board to take. That means we need to find the element in the winning triplet that is 0.")
                  (pre
                    (code :class "lisp" "(find-if #'(lambda (element) (= 0 (nth element *test-board*)))
         (find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*))
                                        ; => 7 (3 bits, #x7, #o7, #b111)"))
                  (p "Since we need to do the same thing to check if we need to block…")
                  (pre
                    (code :class "lisp" "(find-if #'(lambda (element) (= 0 (nth element *test-board*)))
         ;; Notice (= 2 ...), not (= 20 ...)
         (find-if #'(lambda (triplet) (= 2 (sum-triplet *test-board* triplet))) *triplets*))
                                        ; => 8 (4 bits, #x8, #o10, #b1000)"))
                  (p "…we can make this one function.")
                  (pre
                    (code :class "lisp" "(defun win-or-block (board target-sum)
  (let ((target-triplet (find-if #'(lambda (triplet)
                                    ;; NOTE: bug left purposefully for teaching purposes
                                    (= target-sum (sum-triplet *test-board* triplet)))
                                *triplets*)))
    (if target-triplet
        (find-if #'(lambda (element) (= (nth element board) 0)) target-triplet))))

(win-or-block *test-board* 2)
                                        ; => 8 (4 bits, #x8, #o10, #b1000)
                                        ; This is the space to take if we want to block.
(win-or-block *test-board* 20)
                                        ; => 7 (3 bits, #x7, #o7, #b111)
                                        ; This is the space to take if we want to win."))
                  (aside :class "book-aside"
                    (p "Lisp code often needs to be read \"inside-out\". The nested "
                      (code "find-if") " forms above demonstrate that. The inner "
                      (code "find-if") " returned a triplet from the list of "
                      (code "*triplets*") ", then the outer "
                      (code "find-if") " returned an item from the triplet. This can be confusing if you're not used reading code this way.")
                    (p "By first assigning "
                      (code "target-triplet") " to the triplet that sums to "
                      (code "target-sum") ", we reverse the order of the code we read first, making it clearer. It also allows us to check if a "
                      (code "target-triplet") " even exists."))))
              (section :id "adding-strategies"
                (hgroup
                  (span)
                  (h5 "Adding strategies"))
                (div :class "outline-text-6" :id "text-org3de6ec6"
                  (p "Now that we have a function that can find spaces to either win or block a win, we can call it with the appropriate target-sum in our win and block strategies.")
                  (pre
                    (code :class "lisp" "(defun make-three-in-a-row (board)
  (let ((move (win-or-block board (* 2 *player-two*))))
    (when move
        (list move (format nil \"~&I see a winning move at ~a.~%\" move)))))

(defun block-opponent-win (board)
  (let ((move (win-or-block board (* 2 *player-one*))))
    (when move
        (list move (format nil \"~&Danger! Loss imminent! Moving to block at ~a.~%\" move)))))

(print-board *test-board*)
                                        ;  O | X |
                                        ; -----------
                                        ;  O | X |
                                        ; -----------
                                        ;    |   |
                                        ;  => NIL
(make-three-in-a-row *test-board*)
                                        ;  => (7 \"I see a winning move at 7.
                                        ; \")
(block-opponent-win *test-board*)
                                        ;  => (8 \"Danger! Loss imminent! Moving to block at 8.
                                        ; \")"))
                  (p "We will return a list with the move and also the strategy employed.")
                  (aside :class "book-aside"
                    (p "Notice that we use "
                      (code "when") ". We could use "
                      (code "if") " instead, but "
                      (code "when") " communicates more precisely what we mean. If we don't need the optional \"else\" branch of the "
                      (code "if") " form, then it's better style to use "
                      (code "when") " or "
                      (code "unless") "."))
                  (p "If the computer is going first, it should just take a random position.")
                  (pre
                    (code :class "lisp" "(defun take-random-position (board)
  (let ((move (+ 1 (random 9))))
    (cond ((= 0 (nth move board))
           (place-piece board *player-two* move)
           (list move \"Picking random position.\"))
          (t
           (take-random-position board)))))"))
                  (p
                    (code "random") " will choose a semi-random value "
                    (b "between") " 0 and the argument passed. Unfortunately, there is no way to specify the beginning of the \"range\" nor is the number random enough to be used for security purposes. Since we need to pick a number between 1 (remember the filler "
                    (code "BOARD") " symbol) and 9 inclusive, we add 1 to the result.")
                  (p "Since the random position chosen may be occupied, the catchall branch simply makes a recursive call to "
                    (code "take-random-position") " to try again.")
                  (p "Right now, "
                    (code "move") " calls itself with the opponent player. We'll need a "
                    (code "human-move") " and "
                    (code "computer-move") " to give us the ability to let the computer choose.")
                  (pre
                    (code :class "lisp" "(defun human-move (board)
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
                    (code "read") " does, but it only accepts two possible inputs: "
                    (code "y") " or "
                    (code "n") ", meaning yes or no. It evaluates to "
                    (code "t") " for yes and "
                    (code "nil") " for no.")))
              (section :id "fixing-a-bug"
                (hgroup
                  (span)
                  (h5 "Fixing a bug"))
                (div :class "outline-text-6" :id "text-org7e967e0"
                  (p "Try playing with the computer. You'll notice that the computer is always going with the "
                    (code "make-three-in-a-row") " strategy, even if you let it go first.")
                  (p "In the "
                    (code "win-or-block") " function, there is a bug: we forgot to remove "
                    (code "*test-board*") " from the first "
                    (code "find-if") ".")
                  (p "Try this: Start a game, let the computer go first. It should tell you "
                    (code "\"My strategy: I see a winning move at\"") ". We expect it to simply pick a random space.")
                  (p
                    (b "While the game is still running") ", update and compile the "
                    (code "win-or-block") " function.")
                  (pre
                    (code :class "lisp" "(defun win-or-block (board target-sum)
  (let ((target-triplet (find-if #'(lambda (triplet)
                                    ;; NOTE:  *test-board* -> board
                                    (= target-sum (sum-triplet board triplet)))
                                *triplets*)))
    (if target-triplet
        (find-if #'(lambda (element) (= (nth element board) 0)) target-triplet))))"))
                  (p "After compiling, "
                    (i "continue the game") ". Make your move. You should now see the computer choose a random space.")
                  (p "This small interaction demonstrates a big feature of Lisp: "
                    (b "We can update code as it is running, without restarting it.") " Whether we are updating a small tic-tac-toe game, "
                    (a :href "https://www.youtube.com/watch?v=gdjkSkRFcr4" "a program for music and visualization generation") ", or a running web app, we can update it while it's running."))))
            (section :id "add-computer-strategies"
              (hgroup
                (span)
                (h4 "Add computer strategies"))
              (div :class "outline-text-5" :id "text-org321b893"
                (p "The simple data representation we chose at the beginning has made it fairly easy to get a simple human-vs-computer tic-tac-toe game made. However, the computer is very dumb. It doesn't think ahead and doesn't recognize different human strategies. Let's change that.")
                (p "Tic-tac-toe has a rather unfun characteristic: if played well by both players, every game will end in a draw.")
                (p "For our computer strategies, then, we are going to be mostly reacting to the human player, recognizing different strategies and countering them perfectly. By the end, we'll totally drain what little fun can be had from the game. But the coding will be fun, so let's go."))
              (section :id "beyond-triplets"
                (hgroup
                  (span)
                  (h5 "Beyond triplets"))
                (div :class "outline-text-6" :id "text-orgbe016a2"
                  (p "There are two strategies you can employ that can guarantee a victory if the opponent doesn't react correctly: the "
                    (i "squeeze play") " and a "
                    (i "two-on-one play") ".")
                  (pre
                    (code :class "lisp" "(defparameter *squeeze* (list 'board 1 0 0 0 10 0 0 0 1))
(defparameter *two-on-one* (list 'board 10 0 0 0 1 0 0 0 1))
(print-board *squeeze*)
;  X |   |
; -----------
;    | O |
; -----------
;    |   | X
;  => NIL

(print-board *two-on-one*)
;  O |   |
; -----------
;    | X |
; -----------
;    |   | X
;  => NIL"))
                  (p "The squeeze happens when one player takes two corners and one player takes the middle. The two-on-one happens when one player takes the corner and the middle, and the other player takes a corner as well. In both scenarios, O is guaranteed to lose if X plays properly.")
                  (p "To avoid these two scenarios, the computer must recognize possible strategies being deployed against it and react correctly.")
                  (ul :class "org-ul"
                    (li "If the computer is in the middle between two human pieces, it's a possible squeeze."
                      (ul :class "org-ul"
                        (li "To counter, take a side: don't take a corner.")))
                    (li "If the computer is in a corner and the human has the middle and the corner lining up his pieces against the computer, it's a possible two-on-one."
                      (ul :class "org-ul"
                        (li "To counter, take a corner: don't take a side."))))
                  (p "Right now, the computer reads the board as a list of triplets and identifies possible wins and danger. But to ensure both players end the game disappointed, the computer needs to recognize some other characteristics of the board: corners and sides.")
                  (pre
                    (code :class "lisp" "(defparameter *corners* '(1 3 7 9))
(defparameter *sides* '(1 2 3 4 6 7 8 9))"))))
              (section :id "detecting-a-squeeze"
                (hgroup
                  (span)
                  (h5 "Detecting a squeeze"))
                (div :class "outline-text-6" :id "text-org10acbc6"
                  (p "Let's start by detecting a squeeze.")
                  (p "First, we need to search the board and cross-reference the triplets, looking for any triplet with values that reduce to 12")
                  (pre
                    (code :class "lisp" "(defun detect-squeeze (board)
  (find-if #'(lambda (triplet)
               (= 12 (sum-triplet board triplet)))
           *triplets*))
(detect-squeeze *squeeze*)
                                        ; => (1 5 9)
(detect-squeeze *two-on-one*)
                                        ; => (1 5 9)"))
                  (p "We need to know for sure this is a diagonal triplet, though.")
                  (pre
                    (code :class "lisp" "(defun diagonal-p (triplet)
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
           *triplets*))
(detect-squeeze *squeeze*)
                                        ; => (1 5 9)
(detect-squeeze *two-on-one*)
                                        ; => (1 5 9)"))
                  (p
                    (code "every") " runs a predicate function on a sequence and returns "
                    (code "t") " if the predicate evaluated to "
                    (code "t") " for every element of the sequence. In "
                    (code "diagonal-p") ", it checks if every element of the triplet is either a corner or the middle space.")
                  (p "We also need to know who is in the middle.")
                  (pre
                    (code :class "lisp" "(defun human-in-middle-p (board)
  (= (nth 5 board) *player-one*))

(defun detect-squeeze (board target-sum)
  (find-if #'(lambda (triplet)
               (and (= (sum-triplet board triplet) target-sum)
                    (diagonal-p triplet)
                    (not (human-in-middle-p board))))
           *triplets*))
(detect-squeeze *squeeze*)
                                        ; => (1 5 9)
(detect-squeeze *two-on-one*)
                                        ; => NIL"))
                  (p "Finally, we just need to be sure that we're at the beginning of the game. The player may have already blocked the squeeze or two-on-one, or the game may have otherwise progressed beyond the first diagonals.")
                  (pre
                    (code :class "lisp" "(defun side-empty-p (board)
  (find-empty-position board *sides*))

(defun find-empty-position (board search-area)
  (find-if #'(lambda (x) (= 0 (nth x board))) search-area))

(defun detect-squeeze (board target-sum)
  (let ((squeeze-p
          (find-if #'(lambda (triplet)
                       (and (= (sum-triplet board triplet) target-sum)     ; Is the triplet the target-sum?
                            (diagonal-p triplet)                           ; Is the triplet a diagonal?
                            (not (human-in-middle-p board))                ; Is the human not in the middle?
                            (side-empty-p board)))                         ; Are all the sides empty?
                   *triplets*)))
    (if squeeze-p
        (find-empty-position board *sides*))))"))
                  (p "If we see a squeeze, we need to counter. To counter a squeeze, we need to take a side (not a corner). So we look for an empty position in one of the "
                    (code "*sides*") ".")
                  (p "If we test "
                    (code "detect-squeeze") ", we should get an empty space on one of the sides:")
                  (pre
                    (code :class "lisp" "(detect-squeeze *squeeze* 12)
                                        ; => 2 (2 bits, #x2, #o2, #b10)"))
                  (p "2 is the space between corner 1 and 3, so it's given an expected return value.")))
              (section :id "detecting-a-two-on-one"
                (hgroup
                  (span)
                  (h5 "Detecting a two-on-one"))
                (div :class "outline-text-6" :id "text-orgbbe57d9"
                  (p
                    (code "detect-two-on-one") " is nearly identical to "
                    (code "detect-squeeze") ":")
                  (pre
                    (code :class "lisp" "(defun detect-two-on-one (board target-sum)
  (let ((two-on-one-p
          (find-if #'(lambda (triplet)
                       (and (= (sum-triplet board triplet) target-sum)
                            (diagonal-p triplet)
                            (human-in-middle-p board)  ; Human in the middle?
                            (side-empty-p board)))
                   *triplets*)))
    (when two-on-one-p
      (find-empty-position board *corners*)))) ; Look for an empty space in the corners.

(detect-squeeze *two-on-one* 12)
                                        ; => NIL
(detect-squeeze *squeeze* 12)
                                        ; => 2 (2 bits, #x2, #o2, #b10)
(detect-two-on-one *two-on-one* 12)
                                        ; => 3 (2 bits, #x3, #o3, #b11)
(detect-two-on-one *squeeze* 12)
                                        ; => NIL"))
                  (p "With that, we just need a couple of small wrappers to encapsulate our strategies.")
                  (pre
                    (code :class "lisp" "(defun block-squeeze-play (board)
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
                  (h5 "Updating "
                    (code "choose-move")))
                (div :class "outline-text-6" :id "text-orgc9f0532"
                  (p "Finally, we update "
                    (code "choose-move") " by adding our two new strategies.")
                  (pre
                    (code :class "lisp" "(defun choose-move (board)
  (or (make-three-in-a-row board)
      (block-squeeze-play board)         ; Added
      (block-two-on-one-play board)      ; Added
      (block-opponent-win board)
      (take-random-position board)))")))))
            (section :id "summary"
              (hgroup
                (span)
                (h4 "Summary"))
              (div :class "outline-text-5" :id "text-org6796e5e"
                (p "Now you have a finished AI that will force a draw every game. Try playing it and enjoy infinite draws.")
                (p "With this, you have gotten your first experience writing a program in Almighty Common Lisp. It may have been painful: if you aren't using structural editing modes like "
                  (code "lispy-mode") " or "
                  (code "paredit-mode") ", you had to make sure to keep your parentheses balanced and moving code around may have been harder than you expected. However, with practice, you'll be stacking parens like an expert."))))
          (section :id "discussion"
            (hgroup
              (span)
              (h3 "Discussion"))
            (div :class "outline-text-4" :id "text-org1db6bf0")
            (section :id "reading-lisp-code"
              (hgroup
                (span)
                (h4 "Reading Lisp Code"))
              (div :class "outline-text-5" :id "text-org9f3a046"
                (p "\"inside-out\", using "
                  (code "let") " to change order of code.")
                (aside :class "book-aside"
                  (p "Lisp code often needs to be read \"inside-out\". The nested "
                    (code "find-if") " forms above demonstrate that. The inner "
                    (code "find-if") " returned a triplet from the list of "
                    (code "*triplets*") ", then the outer "
                    (code "find-if") " returned an item from the triplet. This can be confusing if you're not used reading code this way.")
                  (p "By first assigning "
                    (code "target-triplet") " to the triplet that sums to "
                    (code "target-sum") ", we reverse the order of the code we read first, making it clearer. It also allows us to check if a "
                    (code "target-triplet") " even exists."))))
            (section :id "good-code-communication"
              (hgroup
                (span)
                (h4 "Good Code Communication"))
              (div :class "outline-text-5" :id "text-orgb49daa8"
                (p "be specific with "
                  (code "if/when/unless/cond") "."
                  (code "*global-variables*")))))))
      (section :id "emacs-fundamentals"
        (hgroup
          (span)
          (h1 "EMACS FUNDAMENTALS"))
        (div :class "outline-text-2" :id "text-orge1187eb")
        (section :id "beyond-survival-mode"
          (hgroup
            (span)
            (h2 "BEYOND SURVIVAL MODE"))
          (div :class "outline-text-3" :id "text-orgd85bae2"
            (p "In order to do any Lisp coding at all, you need to be able to edit text (duh). With Doom Emacs, you have three options: Use Emacs keybindings, use Evil keybindings, or use Emacs as much as possible like VSCode or other editors you're likely familiar with. We'll call the last one Survival Mode. So far, you've been in Survival Mode.")
            (ol :class "org-ol"
              (li "You've typed "
                (code "C-z") " to enable Evil "
                (code "Emacs State") ", which enables editing using standard Emacs keybindings.")
              (li "You probably turned off "
                (code "lispy-mode") " to avoid confusing behavior.")
              (li "You edited text as usual: type, mouse click around, scroll wheel up and down, click and drag to highlight text, copy/paste with the keybindings you already know, etc.")
              (li "You've used the top menu bar or mode line to browse commands you can use while editing Lisp files."))
            (p "If you're still on the fence about this whole Emacs/Lisp thing, there is no shame in staying in Survival Mode. However, you probably ended up using some keybindings already for frequently used commands that you learned in the menu. This is the perfect start to learning Emacs–gradually incorporating more and more keybindings/functionality into your workflow as your power grows.")
            (p "This chapter will help you increase your mastery of Emacs by introducing you to keybindings for many of the most common and useful functions in Doom Emacs, starting with text and Lisp editing, then moving on to buffers, windows, and projects.")))
        (section :id "support-level-disclaimer"
          (hgroup
            (span)
            (h2 "SUPPORT LEVEL DISCLAIMER"))
          (div :class "outline-text-3" :id "text-org67e9e16"
            (p "None of the functionality here is necessary to continue this book. Instead, you should consider this chapter a reference to return to whenever you are ready to add a little more Emacs functionality into your workflow.")
            (p "However, I won't be including any more references to the menu or using the mode line to navigate to and execute commands. For the rest of this book, I will refer to keybindings and give the names of the commands as well so that you can become familiar with the different packages and commands that are associated with different parts of Doom Emacs' functionality.")
            (p "I will also assume that you are using Evil "
              (code "Normal State") " at least partially. If you are in "
              (code "Emacs State") " and the keybindings aren't working, type "
              (code "M-x") " and type in the name of the command I am referencing. Some commands may not have keybindings in "
              (code "Emacs State") ", or they may be confusing.")
            (p "Example: "
              (code "evil-delete-line") " will not have a keybinding in "
              (code "Emacs State") " and "
              (code "org-kill-line") " in "
              (code "Normal State") " is bound to "
              (code "<deleteline>") ".")
            (p "I won't be testing any of the keybindings in "
              (code "Emacs State") ". If you want to use "
              (code "Emacs State") " exclusively you "
              (i "might") " be able to run some keybindings using "
              (code "M-SPC") " instead of "
              (code "SPC") ", but not all commands have such alternative bindings. ")
            (p "I will only test keybindings in "
              (code "lisp-mode") "; other modes may have different bindings, or commands that do similar but subtly different things may share bindings in different major modes.")))
        (section :id "setting-expectations"
          (hgroup
            (span)
            (h2 "SETTING EXPECTATIONS"))
          (div :class "outline-text-3" :id "text-orgc68f28f"
            (p "Learning Evil bindings + Doom Emacs exclusive bindings will take time. Expect to stumble around for two weeks, feel somewhat productive in two months, and proficient in about 6-12 months. As always, focus on the keybindings and features that seem most useful to the task at hand–learning Common Lisp–and learn more Emacs as you learn more Common Lisp.")))
        (section :id "text-editing"
          (hgroup
            (span)
            (h2 "TEXT EDITING"))
          (div :class "outline-text-3" :id "text-org0e0cc8b")
          (section :id "evil-bindings"
            (hgroup
              (span)
              (h3 "Evil Bindings"))
            (div :class "outline-text-4" :id "text-orgb55d9fc"
              (p "By default, "
                (code "evil-mode") " is available and active in Doom Emacs. This means that you have access to both Emac's default bindings as well as bindings typical of Vim/Neovim.")
              (p "If you don't already know how to use the Neovim keybindings, you can check out "
                (a :href "https://openvim.com" "https://openvim.com") " or "
                (a :href "https://vim-adventures.com/" "https://vim-adventures.com/") ". Both provide an interactive experience for learning the VIM text navigation keybindings.")
              (p
                (code "evil") " uses several different editing states. The three most common are Normal, Insert, and Visual. In "
                (code "Normal State") ", you navigate and run commands, often starting with "
                (code "SPC") ". In "
                (code "Insert State") ", you can edit text. In "
                (code "Visual State") ", you can select and act upon regions of text.")
              (p "These states are set on a per-buffer basis. The state of the current buffer is displayed on the left side of the mode line.")
              (p "Whenever a keybinding in this book mentions a keybinding starting with "
                (code "SPC") ", I assume you are in Evil "
                (code "Normal State") ".")))
          (section :id "inserting"
            (hgroup
              (span)
              (h3 "Inserting"))
            (div :class "outline-text-4" :id "text-org824e9a9"
              (p "Type "
                (code "i") " to enter Evil "
                (code "Insert State") " and type as usual.")))
          (section :id "cutting-copying-pasting"
            (hgroup
              (span)
              (h3 "Cutting/Copying/Pasting"))
            (div :class "outline-text-4" :id "text-org47f1f9e")
            (section :id "-kill-"
              (hgroup
                (span)
                (h4
                  (code "kill")))
              (div :class "outline-text-5" :id "text-orgb09fff3"
                (p "To cut some text, you must "
                  (code "kill") " it.")
                (p "The simplest way to kill some text is to kill the entire contents right of the cursor with "
                  (code "D") " ("
                  (code "lispyville-delete-line") ") while in "
                  (code "Normal State") ".")
                (p "To kill a whole line, type "
                  (code "d d") " in "
                  (code "Normal State") ".")
                (p "You can kill single characters with "
                  (code "x") ", single words with "
                  (code "k i w") " or "
                  (code "M-backspace") ". You can kill the contents within some textual boundaries like () or \"\" with "
                  (code "k i (") " or "
                  (code "k i quotemark") ". You can include the boundaries with "
                  (code "k o") " instead of "
                  (code "k i") ".")))
            (section :id "-yank-"
              (hgroup
                (span)
                (h4
                  (code "yank")))
              (div :class "outline-text-5" :id "text-orgf5dfe1e"
                (p "To copy something, you must "
                  (code "yank") " it.")
                (p "Use "
                  (code "y y") " in Evil "
                  (code "Normal State") " to yank a line. You can yank a word with "
                  (code "y i w") " or a symbol with "
                  (code "y i o") ".")))
            (section :id "-paste-"
              (hgroup
                (span)
                (h4
                  (code "paste")))
              (div :class "outline-text-5" :id "text-org1c13de3"
                (p "To paste some text, type "
                  (code "p") " in Evil "
                  (code "Normal State") ", or "
                  (code "s-v") " (or whatever keyboard shortcut you're accustomed to using to paste text)."))))
          (section :id "marking-text"
            (hgroup
              (span)
              (h3 "Marking Text"))
            (div :class "outline-text-4" :id "text-org61b2893"
              (p "You can mark regions of text to work on using "
                (code "Visual State") ". Type "
                (code "v") " and then navigate text to select it from where you first entered.")
              (p "Marked text can be killed or yanked. There are plenty of other operations that you can do on marked text, but we'll save that for another time.")))
          (section :id "simple-searching"
            (hgroup
              (span)
              (h3 "Simple Searching"))
            (div :class "outline-text-4" :id "text-orge9f38b6"
              (p "You can search for some text in a buffer with "
                (code "SPC s s") " ("
                (code "+default/search-buffer") ").")))
          (section :id "aborting-a-command"
            (hgroup
              (span)
              (h3 "Aborting A command"))
            (div :class "outline-text-4" :id "text-org635017f"
              (p "Let's say you pressed "
                (code "SPC f") ", but you decided you don't want to continue. To abort the command, press "
                (code "C-g") " ("
                (code "doom/escape") ").")
              (p "It's common to accidently hit a key or combination of keys you didn't mean to. If you are \"stuck\" inside "
                (i "some") " command, or in the middle of a keybinding, the quickest solution is to press "
                (code "C-g") " a few times.")))
          (section :id "undoing-an-action"
            (hgroup
              (span)
              (h3 "Undoing An Action"))
            (div :class "outline-text-4" :id "text-org7d4eda6"
              (p
                (code "C-/") " ("
                (code "undo-fu-only-undo") ") will undo the previous action. Doom also includes the "
                (code "s-z") " keybinding, which is the same as undo in the browser, MS Word, etc.")))
          (section :id "doing-an-action-multiple-times"
            (hgroup
              (span)
              (h3 "Doing An Action Multiple Times"))
            (div :class "outline-text-4" :id "text-orgfca5385"
              (p "In Doom Emacs, if you press a number while in "
                (code "Normal State") " before running a command, entering Evil "
                (code "Insert State") ", etc. Emacs will run the action the same number of times as the number you typed.")
              (p "If you don't know VIM/evil keybindings, you probably won't "
                (i "want") " to do this, but you may accidently do so. For example, you may be in Evil "
                (code "Normal State") ", press 9, enter "
                (code "Insert State") " and type something, then return to "
                (code "Normal State") " with "
                (code "ESC") ", at which point you now have inserted the same text 9 times. It might look something like this:")
              (pre
                (code :class "lisp" "(+ 1 1(+ 1 1(+ 1 1(+ 1 1(+ 1 1(+ 1 1(+ 1 1(+ 1 1(+ 1 1)))))))))"))
              (p "The best thing to do in this situation is just undo and retype.")
              (p "This functionality can be quite useful. For example, if you want to navigate down 10 lines, you can type "
                (code "10 j") " in "
                (code "Normal State") " to navigate down ten lines. It also makes it easy to type decorative dividers in comments like this:")
              (pre
                (code :class "lisp" ";; =============================="))))
          (section :id "multiple-cursors"
            (hgroup
              (span)
              (h3 "Multiple Cursors"))
            (div :class "outline-text-4" :id "text-org24669f9"
              (p "Type "
                (code "g z z") " ("
                (code "+multiple-cursors/evil-mc-toggle-cursor-here") ") to create a cursor. Move your cursor and the one you created stays in place. You can create arbitrary numbers of cursors. Once you've made all the cursors you need, you can begin editing them by entering Evil "
                (code "Insert State") ".")
              (p "If you need to pause the cursors, type "
                (code "g z t") " ("
                (code "+multiple-cursors/evil-mc-toggle-cursors") "). If you need to undo the creation of the last cursor, type "
                (code "g z u") " ("
                (code "+multiple-cursors/evil-mc-undo-cursor") ").")))
          (section :id "multiediting"
            (hgroup
              (span)
              (h3 "Multiediting"))
            (div :class "outline-text-4" :id "text-org1a51890"
              (p "If you like multicursor editing in other editors, you're going to love multiediting in Doom Emacs. With multiple cursors, you can place cursors in arbitrary locations. The downside is that you need to move the cursor to each location you want to place the cursor. With multiediting, you can search and highlight text and edit all highlighted areas simultaneously. You can leave the highlighted regions and edit other text, then return to the highlighted region an"))
            (section :id "next-match"
              (hgroup
                (span)
                (h4 "Next Match"))
              (div :class "outline-text-5" :id "text-orgcc6e730"
                (p "Place the cursor or mark a region. Type "
                  (code "M-d") " ("
                  (code "evil-multiedit-match-symbol-and-next") ") to begin multiediting. If you type "
                  (code "M-d") " again, you will mark the next symbol or region that matches the first one. You can continue that as many times as you want.")))
            (section :id "previous-match"
              (hgroup
                (span)
                (h4 "Previous Match"))
              (div :class "outline-text-5" :id "text-org612adda"
                (p "You can also match backwards with "
                  (code "M-D") " ("
                  (code "evil-multiedit-match-symbol-and-prev") ").")))
            (section :id "toggling-matches"
              (hgroup
                (span)
                (h4 "Toggling Matches"))
              (div :class "outline-text-5" :id "text-orgeabd643"
                (p "While multiediting, if you press "
                  (code "Enter") ", you will remove a match from being edited. "
                  (code "C-g") " to quit multiediting. Typing "
                  (code "Enter") " again will toggle the match back on.")))
            (section :id "navigating-matches"
              (hgroup
                (span)
                (h4 "Navigating Matches"))
              (div :class "outline-text-5" :id "text-org4d94ea7"
                (p "After marking several matches, you can navigate directly between them with "
                  (code "C-n") " ("
                  (code "evil-multiedit-next") ") and "
                  (code "C-p") " ("
                  (code "evil-multiedit-prev") ").")))
            (section :id "quitting-multiediting"
              (hgroup
                (span)
                (h4 "Quitting Multiediting"))
              (div :class "outline-text-5" :id "text-org3f4a6b5"
                (p "To unmark all matches and quit multiediting, type "
                  (code "C-g") " (maybe a few times)."))))
          (section :id "searching-replacing"
            (hgroup
              (span)
              (h3 "Searching & Replacing"))
            (div :class "outline-text-4" :id "text-orgb081cdf"
              (p "When you want to rename a few instances of a symbol in a "
                (code "defun") ", multiediting is the right tool for the job. If you want to rename a function throughout a file, there is a better tool for the job: VIM search and replace.")
              (p "To begin using it, just type "
                (code ":") " ("
                (code "evil-ex") "). How you proceed from there depends on how sophisticated your search and replace operation needs to be. The simplest case is replacing all instances of some text throughout the buffer:")
              (pre :class "example" :id "org0c70cf3" ":%s/TEXT TO REPLACE/TEXT TO REPLACE IT WITH/g")
              (p "This search and replace tool is very powerful, but a full treatment is out of the scope of this book. A good place to learn more is "
                (a :href "https://vim.fandom.com/wiki/Search_and_replace" "the Vim tips wiki") " ("
                (a :href "https://vim.fandom.com/wiki/Search_and_replace" "https://vim.fandom.com/wiki/Search_and_replace") ").")))
          (section :id "running-commands-functions-with-no-keybindings"
            (hgroup
              (span)
              (h3 "Running Commands/Functions With No Keybindings"))
            (div :class "outline-text-4" :id "text-org212ec2b"
              (p "There are many commands that don't have keybindings. There are also many commands you don't know the keybindings for.")
              (p "To find and run those commands, type "
                (code "M-x") ". A minibuffer will open with a list of commands and a short description. If they have a keybinding, it will be displayed in parentheses next to the command itself.")
              (p
                (code "C-n") " will move the cursor down a line inside that minibuffer."
                (code "C-p") " will move the cursor up a line.")))
          (section :id "quick-reference-table"
            (hgroup
              (span)
              (h3 "Quick Reference Table"))
            (div :class "outline-text-4" :id "text-org943ec7c"
              (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                (colgroup
                  (col :class "org-left")
                  (col :class "org-left")
                  (col :class "org-left"))
                (thead
                  (tr
                    (th :scope "col" :class "org-left" "Action")
                    (th :scope "col" :class "org-left" "Emacs Binding")
                    (th :scope "col" :class "org-left" "Evil Binding (Normal State)")))
                (tbody
                  (tr
                    (td :class "org-left" "enter Insert State")
                    (td :class "org-left" " ")
                    (td :class "org-left" "i"))
                  (tr
                    (td :class "org-left" "delete character")
                    (td :class "org-left" " ")
                    (td :class "org-left" "x"))
                  (tr
                    (td :class "org-left" "delete to end of line")
                    (td :class "org-left" " ")
                    (td :class "org-left" "D"))
                  (tr
                    (td :class "org-left" "delete whole line")
                    (td :class "org-left" " ")
                    (td :class "org-left" "dd"))
                  (tr
                    (td :class "org-left" "delete inner word")
                    (td :class "org-left" " ")
                    (td :class "org-left" "diw"))
                  (tr
                    (td :class "org-left" "delete a word")
                    (td :class "org-left" " ")
                    (td :class "org-left" "daw"))
                  (tr
                    (td :class "org-left" "delete inner parentheses")
                    (td :class "org-left" " ")
                    (td :class "org-left" "di("))
                  (tr
                    (td :class "org-left" "delete inner quotes")
                    (td :class "org-left" " ")
                    (td :class "org-left" "di\""))
                  (tr
                    (td :class "org-left" "delete around parentheses")
                    (td :class "org-left" " ")
                    (td :class "org-left" "da("))
                  (tr
                    (td :class "org-left" "delete around quotes")
                    (td :class "org-left" " ")
                    (td :class "org-left" "da\""))
                  (tr
                    (td :class "org-left" "delete with alt-backspace")
                    (td :class "org-left" "M-Backspace")
                    (td :class "org-left" "M-Backspace"))
                  (tr
                    (td :class "org-left" "yank (copy) line")
                    (td :class "org-left" " ")
                    (td :class "org-left" "yy"))
                  (tr
                    (td :class "org-left" "yank (copy) inner word")
                    (td :class "org-left" " ")
                    (td :class "org-left" "yiw"))
                  (tr
                    (td :class "org-left" "yank (copy) inner symbol")
                    (td :class "org-left" " ")
                    (td :class "org-left" "yis"))
                  (tr
                    (td :class "org-left" "paste after cursor")
                    (td :class "org-left" " ")
                    (td :class "org-left" "p"))
                  (tr
                    (td :class "org-left" "paste from system clipboard")
                    (td :class "org-left" "C-v")
                    (td :class "org-left" "s-v"))
                  (tr
                    (td :class "org-left" "enter Visual State")
                    (td :class "org-left" " ")
                    (td :class "org-left" "v"))
                  (tr
                    (td :class "org-left" "search in buffer")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC s s"))
                  (tr
                    (td :class "org-left" "abort command")
                    (td :class "org-left" "C-g")
                    (td :class "org-left" "C-g"))
                  (tr
                    (td :class "org-left" "undo")
                    (td :class "org-left" "C-/")
                    (td :class "org-left" "s-z or C-/"))
                  (tr
                    (td :class "org-left" "repeat action N times")
                    (td :class "org-left" " ")
                    (td :class "org-left" "<number> then command"))
                  (tr
                    (td :class "org-left" "multiedit: match symbol and next")
                    (td :class "org-left" "M-d")
                    (td :class "org-left" "M-d"))
                  (tr
                    (td :class "org-left" "multiedit: match symbol and prev")
                    (td :class "org-left" "M-D")
                    (td :class "org-left" "M-D"))
                  (tr
                    (td :class "org-left" "multiedit: toggle match")
                    (td :class "org-left" "Enter")
                    (td :class "org-left" "Enter"))
                  (tr
                    (td :class "org-left" "multiedit: next match")
                    (td :class "org-left" "C-n")
                    (td :class "org-left" "C-n"))
                  (tr
                    (td :class "org-left" "multiedit: previous match")
                    (td :class "org-left" "C-p")
                    (td :class "org-left" "C-p"))
                  (tr
                    (td :class "org-left" "quit multiediting")
                    (td :class "org-left" "C-g")
                    (td :class "org-left" "C-g"))
                  (tr
                    (td :class "org-left" "start ex command (search & replace)")
                    (td :class "org-left" " ")
                    (td :class "org-left" ":"))
                  (tr
                    (td :class "org-left" "execute command (M-x)")
                    (td :class "org-left" "M-x")
                    (td :class "org-left" "M-x"))
                  (tr
                    (td :class "org-left" "navigate down in minibuffer")
                    (td :class "org-left" "C-n")
                    (td :class "org-left" "C-n"))
                  (tr
                    (td :class "org-left" "navigate up in minibuffer")
                    (td :class "org-left" "C-p")
                    (td :class "org-left" "C-p")))))))
        (section :id "buffer-navigation-management"
          (hgroup
            (span)
            (h2 "BUFFER NAVIGATION & MANAGEMENT"))
          (div :class "outline-text-3" :id "text-org233a8f6")
          (section :id "it-all-begins-with-a-buffer"
            (hgroup
              (span)
              (h3 "It All Begins With A Buffer"))
            (div :class "outline-text-4" :id "text-org3b12a0d"
              (p "Buffers are the most fundamental abstraction in Emacs. A buffer is a named place that displays some data. Usually buffers display the contents of text files, but they can also point to Emacs-specific functionality like the "
                (code "*scratch*") ", "
                (code "*Messages*") ", or "
                (code "*Org-Agenda*") " buffers. Some buffers may be called "
                (code "minibuffers") ". Minibuffers are typically designed to be ephemeral, like the buffer that appears when you type "
                (code "M-x") " to run some command.")
              (p "Once you're done learning the essentials of buffer navigation and management, you'll be able to:")
              (ul :class "org-ul"
                (li "Open & Close buffers.")
                (li "Open a \"scratch\" buffer.")
                (li "Switch between buffers.")
                (li "View lists of buffers.")
                (li "Save Files.")
                (li "Get more information about buffers."))))
          (section :id "opening-buffers"
            (hgroup
              (span)
              (h3 "Opening Buffers"))
            (div :class "outline-text-4" :id "text-orgdf4b755"
              (p "To open a file buffer, type "
                (code "C-x C-f") " or "
                (code "SPC f f") " to run "
                (code "find-file") ". You can open a file that already exists, or you can navigate to a directory and/or file that doesn't exist, type "
                (code "Enter") ", and create a buffer for that location. The file/directory doesn't exist until you save the buffer.")
              (p "Besides file buffers, there are many other kinds of buffers. The most important buffer to know as a beginner is the "
                (code "M-x") " command minibuffer. There you can get a list of function you can (possibly) run. There are many commands that aren't bound to keybindings that you may want to use on occasion (or perhaps you would like bind them to a custom key command).")))
          (section :id "scratch-buffer"
            (hgroup
              (span)
              (h3 "Scratch Buffer"))
            (div :class "outline-text-4" :id "text-orgd40d004"
              (p "The scratch buffer is a buffer that can be opened to use the same way \"scratch\" paper is use–to do a little work that you'll quickly save somewhere else or discard after a short time. You can open it with "
                (code "SPC x") ". Once in the buffer, you can change the mode using "
                (code "M-x") " and typing in the mode.")))
          (section :id "switching-buffers"
            (hgroup
              (span)
              (h3 "Switching Buffers"))
            (div :class "outline-text-4" :id "text-orgd4836a9"
              (p "You can switch buffers by typing "
                (code "C-x b") " or "
                (code "SPC b B") " ("
                (code "consult-buffer") "). You will be presented with a minibuffer listing all buffers that are open. Unlike "
                (code "list-buffers") ", you can navigate the list of buffers using "
                (code "C-n/C-p") " and peak at the buffer before switching to it. The minibuffer is smaller and disappears after you select a buffer to switch to.")
              (p "You can't mouse click buffers in the list to switch to them. You have to either type the name (often the ideal way to switch to a buffer anyway) or use Emacs keybindings to move the cursor up and down the list with "
                (code "C-n") " an "
                (code "C-p") " and select with "
                (code "Enter") ".")))
          (section :id "closing-buffers"
            (hgroup
              (span)
              (h3 "Closing Buffers"))
            (div :class "outline-text-4" :id "text-org465a595"
              (p "To close the active buffer, type "
                (code "s-k") " or "
                (code "SPC b k") ". If you want to close multiple buffers, you can type "
                (code "C-x C-b") " to run "
                (code "list-buffers") ". Inside the "
                (code "*Buffer List*") " buffer, you can mark a buffer to close with "
                (code "d") ". After marking all of the buffers you want to close, you can \"execute\" the closing with "
                (code "x") ". ")
              (p "Different minibuffers often can be closed with simpler keybindings like "
                (code "ESC") " or "
                (code "q") ".")))
          (section :id "saving-file-buffers"
            (hgroup
              (span)
              (h3 "Saving File Buffers"))
            (div :class "outline-text-4" :id "text-orgbd0533e"
              (p "You can save the current file buffer with "
                (code "C-x C-s") " or "
                (code "s-s") ".")))
          (section :id "getting-more-information-about-a-buffer"
            (hgroup
              (span)
              (h3 "Getting More Information About A Buffer"))
            (div :class "outline-text-4" :id "text-orgc6f7d50"
              (p "As you get more experience with Emacs, you may want to know how something works. Many functions–especially in a distribution like Doom–are not native functions. They are often available because of a major or minor mode created by a certain package that's installed. Thus, learning more about the environment can help you come to grips with what package does what and what other functionality is available but \"hidden\"."))
            (section :id "getting-information-about-major-minor-modes"
              (hgroup
                (span)
                (h4 "Getting Information About Major & Minor Modes"))
              (div :class "outline-text-5" :id "text-org48b910c"
                (p "You can get more information about the major and minor modes of the active buffer using "
                  (code "C-h m") " or "
                  (code "SPC h m") " ("
                  (code "describe-mode") "). A minibuffer listing all the modes–including all of the functions/keybindings those modes provide–will appear.")))
            (section :id "getting-information-about-a-function"
              (hgroup
                (span)
                (h4 "Getting Information About A Function"))
              (div :class "outline-text-5" :id "text-org600d477"
                (p "You can go deeper and find information about a function with "
                  (code "C-h o") " or "
                  (code "SPC h o") " ("
                  (code "describe-symbol") ") and then typing in an Emacs Lisp symbol or function name. Documentation and the actual code of the function will be made available in a minibuffer.")
                (p "The documentation that "
                  (code "describe-symbol") " shows includes keybindings for the function. Unlike the "
                  (code "M-x") " minibuffer, you can see all of the bindings for a single function (many functions have multiple bindings, as you've probably noticed already). The "
                  (code "M-x") " minibuffer will only show one binding.")))
            (section :id "getting-information-about-a-keybinding"
              (hgroup
                (span)
                (h4 "Getting Information About A Keybinding"))
              (div :class "outline-text-5" :id "text-org9d2db72"
                (p "Maybe you know a keybinding but don't know or remember the function that the keybinding calls? Maybe you hit a key that did something surprising and want to know what the heck it was?")
                (p "In that case, you can use "
                  (code "C-h k") " or "
                  (code "SPC h k") " ("
                  (code "describe-key") "). You will be prompted to type the keybinding you want to know more about.")
                (p "To browse keybindings and functions, you can either type "
                  (code "M-x") " to open a list of all commands, or you can open "
                  (code "embark-bindings") " with "
                  (code "C-h b b") " or "
                  (code "SPC h b b") ". embark-bindings is especially useful because it only shows the keybindings available in the current buffer. "
                  (code "describe-mode") " also shows all of the functions a mode provides, but only shows a maximum of one binding per function. Embark bindings shows you all of them. Additionally, "
                  (code "embark-bindings") " lets you search "
                  (i "keybindings") ". For example, you can search for \"C-c\" and find all keybindings that include "
                  (code "C-c") " in any of them. This will be especially useful for cases where you might not know the "
                  (i "exact") " keybinding you are looking for, or if you're looking for an open keybinding in a buffer you want to make a custom shortcut for."))))
          (section :id "quick-reference-table"
            (hgroup
              (span)
              (h3 "Quick Reference Table"))
            (div :class "outline-text-4" :id "text-org9bc0159"
              (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                (colgroup
                  (col :class "org-left")
                  (col :class "org-left")
                  (col :class "org-left"))
                (thead
                  (tr
                    (th :scope "col" :class "org-left" "Action")
                    (th :scope "col" :class "org-left" "Emacs Binding")
                    (th :scope "col" :class "org-left" "Evil Binding")))
                (tbody
                  (tr
                    (td :class "org-left" "find file / open buffer")
                    (td :class "org-left" "C-x C-f")
                    (td :class "org-left" "SPC f f"))
                  (tr
                    (td :class "org-left" "open scratch buffer")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC x"))
                  (tr
                    (td :class "org-left" "switch buffers")
                    (td :class "org-left" "C-x b")
                    (td :class "org-left" "SPC b B"))
                  (tr
                    (td :class "org-left" "close active buffer")
                    (td :class "org-left" " ")
                    (td :class "org-left" "s-k or SPC b k"))
                  (tr
                    (td :class "org-left" "list buffers")
                    (td :class "org-left" "C-x C-b")
                    (td :class "org-left" " "))
                  (tr
                    (td :class "org-left" "mark buffer to close (in list)")
                    (td :class "org-left" "d")
                    (td :class "org-left" "d"))
                  (tr
                    (td :class "org-left" "execute closing (in list)")
                    (td :class "org-left" "x")
                    (td :class "org-left" "x"))
                  (tr
                    (td :class "org-left" "save current file buffer")
                    (td :class "org-left" "C-x C-s")
                    (td :class "org-left" "s-s"))
                  (tr
                    (td :class "org-left" "describe mode")
                    (td :class "org-left" "C-h m")
                    (td :class "org-left" "SPC h m"))
                  (tr
                    (td :class "org-left" "describe symbol")
                    (td :class "org-left" "C-h o")
                    (td :class "org-left" "SPC h o"))
                  (tr
                    (td :class "org-left" "describe key")
                    (td :class "org-left" "C-h k")
                    (td :class "org-left" "SPC h k"))
                  (tr
                    (td :class "org-left" "embark bindings")
                    (td :class "org-left" "C-h b b")
                    (td :class "org-left" "SPC h b b"))
                  (tr
                    (td :class "org-left" "close minibuffer")
                    (td :class "org-left" "ESC or q")
                    (td :class "org-left" "ESC or q")))))))
        (section :id "window-navigation-management"
          (hgroup
            (span)
            (h2 "WINDOW NAVIGATION & MANAGEMENT"))
          (div :class "outline-text-3" :id "text-orgc201bef")
          (section :id "getting-familiar-with-windows"
            (hgroup
              (span)
              (h3 "Getting Familiar With Windows"))
            (div :class "outline-text-4" :id "text-org7acb387"
              (p "In Emacs, a \"frame\" is what most people would call a window: an operating system-level defined space to display the contents of an application.")
              (p "Emacs uses the word \"window\" differently from how operating systems do. Emacs windows are separate places where buffers can be displayed. Buffers, of course, show either file contents or things like the Lisp REPL. A frame can be subdivided into many windows, each displaying a single buffer at a time.")
              (p "Opening, closing, and navigating between windows are essential skills for using Emacs. Now that you know the basics of Lisp, it's time to become more familiar with windows.")))
          (section :id "splitting-closing-windows"
            (hgroup
              (span)
              (h3 "Splitting & Closing Windows"))
            (div :class "outline-text-4" :id "text-org46f25b0"
              (p "You can split a window vertically or horizontally (creating another window).")
              (p "To split vertically, type "
                (code "C-x 2") " or "
                (code "SPC w s") ".
To split horizontally, type "
                (code "C-x 3") " or "
                (code "SPC w v") ".")
              (p "You can delete a window with "
                (code "C-x 0") " or "
                (code "SPC w d") ", or you can close all "
                (i "except") " the currently selected window with "
                (code "C-x 1") " or "
                (code "C-w C-o") ".")))
          (section :id "moving-between-windows"
            (hgroup
              (span)
              (h3 "Moving Between Windows"))
            (div :class "outline-text-4" :id "text-org6686a99"
              (p "While you can use the mouse to move the cursor around, eventually you'll want to learn to use keybindings instead. Trust me, it's nice."))
            (section :id "ace-window"
              (hgroup
                (span)
                (h4 "Ace window"))
              (div :class "outline-text-5" :id "text-orgb8a2027"
                (p "You can use the "
                  (code "ace-window") " packages' functionality to switch windows. Type "
                  (code "C-x o") " or "
                  (code "C-w C-w") " to run "
                  (code "ace-window") ". If there are only two windows open, then it will move the cursor to the one that isn't currently active. If however, you have three or more windows open, "
                  (code "ace-window") " will display letters at the top left of each buffer. Typing the letter displayed in a window will move the cursor to that window.")
                (p "This method is my preferred method of switching windows because it works regardless of what Evil state or Emacs major mode I'm in. It's also more direct. However, it does require some getting used to. As a beginner, you can't predict what letter you can use to switch to a window. Thus, in the beginning this method will often be slower then the next method I'll introduce to you.")))
            (section :id "evil-window"
              (hgroup
                (span)
                (h4 "Evil window"))
              (div :class "outline-text-5" :id "text-orga9b1d81"
                (p "The next method uses "
                  (code "evil-window") " keybindings. With "
                  (code "evil") " navigation, in Normal State, you navigate the cursor one line up and down with "
                  (code "j") " and "
                  (code "k") ", and one character left and right with "
                  (code "h") " and "
                  (code "l") ".")
                (p "Using this familiar navigation method, you switch windows like this:")
                (ul :class "org-ul"
                  (li
                    (code "SPC w l") " or "
                    (code "C-w l") " ("
                    (code "evil-window-right") "), move the cursor one window to the right.")
                  (li
                    (code "SPC w h") " or "
                    (code "C-w h") " ("
                    (code "evil-window-left") "), move the cursor one window to the left.")
                  (li
                    (code "SPC w j") " or "
                    (code "C-w j") " ("
                    (code "evil-window-down") "), move the cursor one window down.")
                  (li
                    (code "SPC w k") " or "
                    (code "C-w k") " ("
                    (code "evil-window-down") "), move the cursor one window up."))
                (p "It's simple and effective. Even with experience, you may prefer it over using "
                  (code "ace-window") ". However, as I said above, all of these shortcuts require you to be in Evil Normal State ("
                  (code "C-w") " in Insert State is mapped to "
                  (code "evil-delete-backward-word") ")."))))
          (section :id "moving-window-positions"
            (hgroup
              (span)
              (h3 "Moving Window Positions"))
            (div :class "outline-text-4" :id "text-org7469d03"
              (p "The above commands move the "
                (i "cursor") " from one window to another. It's also possible to move a window from one position to another.")
              (ul :class "org-ul"
                (li
                  (code "SPC w L") " or "
                  (code "C-w L") " ("
                  (code "+evil/window-move-right") ")")
                (li
                  (code "SPC w H") " or "
                  (code "C-w H") " ("
                  (code "+evil/window-move-left") ")")
                (li
                  (code "SPC w J") " or "
                  (code "C-w J") " ("
                  (code "+evil/window-move-down") ")")
                (li
                  (code "SPC w K") " or "
                  (code "C-w K") " ("
                  (code "+evil/window-move-up") ")"))
              (p "Technically, it's more like you are switching "
                (i "buffers") " between windows. What this means is that all windows maintain their current size; if you have one large window and one small window, if you run one of the above commands while the cursor is in the large window, the buffer in that window will appear in the smaller window")))
          (section :id "resizing-windows"
            (hgroup
              (span)
              (h3 "Resizing Windows"))
            (div :class "outline-text-4" :id "text-orgf7bc50b"
              (p "You probably won't find yourself resizing windows very often. Sometimes Emacs will do some weird resizing of windows for certain actions (keep an eye out when using "
                (code "sly-sticker-replay") ", introduced in a later chapter) that you need to fix, you might want to dedicate a window to a certain buffer (such as the "
                (code "org-agenda") " buffer, introduced in a later chapter) and keep it open, and thus want to make the window smaller. Or maybe you really need to micro-adjust everything.")
              (p "You can use the mouse to resize windows (click and drag the border between windows). It's honestly not such a bad idea since it will likely be an infrequent activity. However, it's also possible to resize windows with keybindings.")
              (ul :class "org-ul"
                (li
                  (code "C-x {") " ("
                  (code "shrink-window-horizontally") ") or "
                  (code "SPC w >") " or "
                  (code "C-w >") " ("
                  (code "evil-window-increase-width") ").")
                (li
                  (code "C-x }") " ("
                  (code "enlarge-window-horizontally") ") or "
                  (code "SPC w <") " or "
                  (code "C-w <") " ("
                  (code "evil-window-decrease-width") ").")
                (li
                  (code "SPC w +") " or "
                  (code "C-w +") " ("
                  (code "evil-window-increase-height") ")")
                (li
                  (code "SPC w -") " or "
                  (code "C-w -") " ("
                  (code "evil-window-decrease-height") ")"))
              (p "Each of the above commands will resize by one line or character. If you are in "
                (code "evil") " Normal State, you can type a number "
                (code "n") " and then run the command "
                (code "n") " times. In Normal State, typing "
                (code "50 SPC w >") " will increase the window width by 50 characters.")
              (p "You can also make one window larger than all your other windows using "
                (code "C-w o") " or "
                (code "SPC w o") " ("
                (code "doom/window-enlargen") ").")
              (p "You can make all windows equal in size with "
                (code "SPC w =") " or "
                (code "C-w =") " ("
                (code "balance-windows") ").")))
          (section :id "undoing-changes"
            (hgroup
              (span)
              (h3 "Undoing Changes"))
            (div :class "outline-text-4" :id "text-org269a561"
              (p "Undoing any change is simple: "
                (code "C-w u") " or "
                (code "SPC w u") " ("
                (code "winner-undo") ").")
              (p "This is useful not only for typos (for example, maybe you accidentally typed "
                (code "C-x C-x a") " and accidently closed all but one window) or for strange changes made by Emacs (sometimes closing a minibuffer drastically resizes other windows for no known reason), but also for intentional changes. For example, the above "
                (code "doom/window-enlargen") " command drastically resizes all windows. You can undo that change with "
                (code "winner-undo") ".")
              (p
                (code "winner-undo") " no only undoes window resizes, but any changes to windows. If you accidentally closed a window, you can undo with "
                (code "winner-undo") ". If you move a window, you can undo the move. It's perhaps one of the most essential commands for beginners who are most likely to make mistakes or be faced with strange changes in window arrangements they weren't expecting.")
              (p "Of course, you can also run "
                (code "winner-redo") " with "
                (code "C-w C-r") " or "
                (code "SPC w C-r"))))
          (section :id "more-about-windows"
            (hgroup
              (span)
              (h3 "More About Windows"))
            (div :class "outline-text-4" :id "text-org068d845"
              (p "If you want to learn more about what's possible with windows, just type "
                (code "C-w") " and look at the list of commands available in the minibuffer.")))
          (section :id "quick-reference-table"
            (hgroup
              (span)
              (h3 "Quick Reference Table"))
            (div :class "outline-text-4" :id "text-orga97d930"
              (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                (colgroup
                  (col :class "org-left")
                  (col :class "org-left")
                  (col :class "org-left"))
                (thead
                  (tr
                    (th :scope "col" :class "org-left" "Action")
                    (th :scope "col" :class "org-left" "Emacs Binding")
                    (th :scope "col" :class "org-left" "Evil Binding")))
                (tbody
                  (tr
                    (td :class "org-left" "split window vertically")
                    (td :class "org-left" "C-x 3")
                    (td :class "org-left" "C-w C-v"))
                  (tr
                    (td :class "org-left" "split window horizontally")
                    (td :class "org-left" "C-x 2")
                    (td :class "org-left" "C-w C-s"))
                  (tr
                    (td :class "org-left" "kill window")
                    (td :class "org-left" "C-x 0")
                    (td :class "org-left" "C-w d"))
                  (tr
                    (td :class "org-left" "split window vertically")
                    (td :class "org-left" "C-x 2")
                    (td :class "org-left" "SPC w s"))
                  (tr
                    (td :class "org-left" "split window horizontally")
                    (td :class "org-left" "C-x 3")
                    (td :class "org-left" "SPC w v"))
                  (tr
                    (td :class "org-left" "delete window")
                    (td :class "org-left" "C-x 0")
                    (td :class "org-left" "SPC w d"))
                  (tr
                    (td :class "org-left" "close all except the currently selected window")
                    (td :class "org-left" "C-x 1")
                    (td :class "org-left" "C-w C-o"))
                  (tr
                    (td :class "org-left" "run ace-window")
                    (td :class "org-left" "C-x o")
                    (td :class "org-left" "C-w C-w"))
                  (tr
                    (td :class "org-left" "move the cursor one window to the right")
                    (td :class "org-left" "C-w l")
                    (td :class "org-left" "SPC w l"))
                  (tr
                    (td :class "org-left" "move the cursor one window to the left")
                    (td :class "org-left" "C-w h")
                    (td :class "org-left" "SPC w h"))
                  (tr
                    (td :class "org-left" "move the cursor one window down")
                    (td :class "org-left" "C-w j")
                    (td :class "org-left" "SPC w j"))
                  (tr
                    (td :class "org-left" "move the cursor one window up")
                    (td :class "org-left" "C-w k")
                    (td :class "org-left" "SPC w k"))
                  (tr
                    (td :class "org-left" "move window right")
                    (td :class "org-left" "C-w L")
                    (td :class "org-left" "SPC w L"))
                  (tr
                    (td :class "org-left" "move window left")
                    (td :class "org-left" "C-w H")
                    (td :class "org-left" "SPC w H"))
                  (tr
                    (td :class "org-left" "move window down")
                    (td :class "org-left" "C-w J")
                    (td :class "org-left" "SPC w J"))
                  (tr
                    (td :class "org-left" "move window up")
                    (td :class "org-left" "C-w K")
                    (td :class "org-left" "SPC w K"))
                  (tr
                    (td :class "org-left" "shrink window horizontally")
                    (td :class "org-left" "C-x {")
                    (td :class "org-left" "SPC w > or C-w >"))
                  (tr
                    (td :class "org-left" "enlarge window horizontally")
                    (td :class "org-left" "C-x }")
                    (td :class "org-left" "SPC w < or C-w <"))
                  (tr
                    (td :class "org-left" "increase window height")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC w + or C-w +"))
                  (tr
                    (td :class "org-left" "decrease window height")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC w - or C-w -"))
                  (tr
                    (td :class "org-left" "make one window larger than all your other windows")
                    (td :class "org-left" "C-w o")
                    (td :class "org-left" "SPC w o"))
                  (tr
                    (td :class "org-left" "make all windows equal in size")
                    (td :class "org-left" "C-w =")
                    (td :class "org-left" "SPC w ="))
                  (tr
                    (td :class "org-left" "undoing any change")
                    (td :class "org-left" "C-w u")
                    (td :class "org-left" "SPC w u"))
                  (tr
                    (td :class "org-left" "redoing any change")
                    (td :class "org-left" "C-w C-r")
                    (td :class "org-left" "SPC w C-r")))))))
        (section :id "project-navigation-management"
          (hgroup
            (span)
            (h2 "PROJECT NAVIGATION & MANAGEMENT"))
          (div :class "outline-text-3" :id "text-org3783f1c")
          (section :id "buffer-window-management-w-projects"
            (hgroup
              (span)
              (h3 "Buffer & Window Management w/Projects"))
            (div :class "outline-text-4" :id "text-org221e07e"
              (p "Beyond buffers and windows are projects–groups of buffers and windows related to a project. Using projects will provide the following benefits:")
              (ul :class "org-ul"
                (li "Project-focused "
                  (code "find-file") " searches.")
                (li "Project-focused buffer switching/listing.")
                (li "Project-focused text searches.")
                (li "Project buffer saving.")
                (li "Project state saving/loading.")
                (li "Project switching.")
                (li "Project file tree view."))
              (p "And probably more I'm not aware of.")))
          (section :id "project-related-packages"
            (hgroup
              (span)
              (h3 "Project-Related Packages"))
            (div :class "outline-text-4" :id "text-orga59b6e8"
              (p "Project-level functionality is not the result of just a single package. Instead, it is the result of the coordination between several packages.")
              (ul :class "org-ul"
                (li
                  (code "projectile") " provides many of the functions like project-level file searching, project-wide file saving, project switching, etc.")
                (li
                  (code "helm/ido/ivy/vertico") " provide the project-level text search functionality.")
                (li
                  (code "persp-mode") " provides \"workspaces\" that can be saved and loaded.")
                (li
                  (code "embark") " enables project-wide text search-replace operations."))
              (p "Doom Emacs provides some abstractions over some of these modes, so some names could become confusing later.")))
          (section :id "creating-removing-emacs-projects"
            (hgroup
              (span)
              (h3 "Creating & Removing Emacs Projects"))
            (div :class "outline-text-4" :id "text-org9e434b7"
              (p "To create a project:")
              (ul :class "org-ul"
                (li "Create an empty "
                  (code ".project") " file in the project root directory.")
                (li "Type "
                  (code "SPC p a") " ("
                  (code "project-add-known-project") "), navigate to the project root directory and then type "
                  (code "Enter") "."))
              (p "Now if you type "
                (code "SPC p p") " ("
                (code "projectile-switch-project") ") you will find your project in a list of projects you can open or switch to.")))
          (section :id "switching-between-projects"
            (hgroup
              (span)
              (h3 "Switching Between Projects"))
            (div :class "outline-text-4" :id "text-orge434174"
              (p "After creating and switching to a project, a small label at the bottom will appear with the name of the project (the name of the project root directory). That label is a "
                (code "workspace") ".")
              (p "If you have multiple projects open, you can switch between them via several means.")
              (ul :class "org-ul"
                (li
                  (code "SPC TAB [") " ("
                  (code "+workspace/switch-left") ") to switch to the workspace to the right in the list of workspaces.")
                (li
                  (code "SPC TAB ]") " ("
                  (code "+workspace/switch-right") ")")
                (li
                  (code "SPC TAB .") " ("
                  (code "+workspace/switch-to") ") to choose from a list of open workspaces.")
                (li
                  (code "SPC TAB <number>") " Switch to the "
                  (code "nth") " workspace."))))
          (section :id "saving-loading-project-workspaces"
            (hgroup
              (span)
              (h3 "Saving & Loading Project Workspaces"))
            (div :class "outline-text-4" :id "text-org0dae8b9"
              (p "Workspaces are not strictly speaking exclusive to projects. A workspace can be created independent of projects using "
                (code "SPC TAB n") " ("
                (code "+workspace/new") "). If open some buffers, windows, etc. and then save the workspace with "
                (code "SPC TAB s") ", you can close the workspace and reopen it with "
                (code "SPC TAB l") ".")
              (p "If you have a complex setup that you want to save, the ability to save the state of your workspace is a useful feature.")))
          (section :id "saving-project-files"
            (hgroup
              (span)
              (h3 "Saving Project Files"))
            (div :class "outline-text-4" :id "text-orgf5eea4e"
              (p "Once you have a project/workspace open and start editing, you may find several files in your project haven't been saved. This is especially true in Common Lisp projects where you update them by compiling files or individual functions–with no need to save the application files in order to update them in memory.")
              (p "To save all the files in your project, type "
                (code "SPC p s") " ("
                (code "projectile-save-project-buffers") ").")))
          (section :id "switching-project-buffers"
            (hgroup
              (span)
              (h3 "Switching Project Buffers"))
            (div :class "outline-text-4" :id "text-org8358401"
              (p "Using "
                (code "C-x b") " or "
                (code "SPC b B") " you can switch between "
                (i "all") " buffers. However, if you want switch between "
                (i "project") " buffers, you can use "
                (code "C-c p b") " or "
                (code "SPC b b") ". The exact function is called depends on the "
                (code ":completion") " library you chose in the "
                (code "init.el") " file when we first installed Doom Emacs.")
              (pre
                (code :class "lisp" ":completion
company           ; the ultimate code completion backend
;;helm              ; the *other* search engine for love and life
;;ido               ; the other *other* search engine...
;; ivy               ; a search engine for love and life
vertico           ; the search engine of the future"))
              (p "I recommended vertico, but "
                (code "helm") ", "
                (code "ido") ", and "
                (code "ivy") " will all have similar functionality.")
              (p "The vertico command called is "
                (code "+vertico/switch-workspace-buffer") ".")))
          (section :id "searching-in-projects"
            (hgroup
              (span)
              (h3 "Searching In Projects"))
            (div :class "outline-text-4" :id "text-orga36788b"
              (p "There are two kinds of searches: filename searches and text searches."))
            (section :id "filename-searches"
              (hgroup
                (span)
                (h4 "Filename searches"))
              (div :class "outline-text-5" :id "text-orgdf215af"
                (p "You can search for a file within your project using "
                  (code "SPC SPC") " ("
                  (code "projectile-find-file") "). Searching with the normal "
                  (code "find-file") " begins with a view of files within a directory, then you need to navigate to the directory and file you want. With "
                  (code "projectile-find-file") ", all files in your project root directory–including subdirectories–will be available to search.")
                (p "This is a very powerful feature allowing quick searching. If you know you have a file in "
                  (code "src/system/components/ui/button.lisp") ", you can type "
                  (code "SPC SPC ui but Enter") " and open the file without doing any scrolling or navigation of your filesystem.")))
            (section :id "project-text-search-replace"
              (hgroup
                (span)
                (h4 "Project text search & replace"))
              (div :class "outline-text-5" :id "text-org2bcaa96"
                (p "You can search for some text within the files of your project using "
                  (code "SPC /") " ("
                  (code "+default/search-project") "). ")
                (p "While searching, you can type "
                  (code "C-c C-e") " ("
                  (code "embark-export") ") to export all the search matches to an "
                  (i "editable buffer") ". Inside the buffer, you can type "
                  (code "Enter") " on individual matches to go to the file to see context. In the Embark buffer, after editing the buffer, you can save changes to all of the files with "
                  (code "C-c C-c") ", or discard changes with "
                  (code "C-c C-k") "."))))
          (section :id "project-tree-view"
            (hgroup
              (span)
              (h3 "Project Tree View"))
            (div :class "outline-text-4" :id "text-org8415182"
              (p
                (code "treemacs") " provides a tree view of your project. Open it with "
                (code "SPC o p") " ("
                (code "+treemacs/toggle") ")."))
            (section :id "switching-to-treemacs-buffer"
              (hgroup
                (span)
                (h4 "Switching to treemacs buffer"))
              (div :class "outline-text-5" :id "text-orgb511c50"
                (p "If you have a treemacs buffer open and move away from it into another buffer, if you use "
                  (code "ace-window") ", ace-window won't give you the option of switching to the treemacs buffer. You either need to use one of the "
                  (code "evil-window-") " commands from the previous chapter, or toggle the treemacs buffer closed and reopen it.")))
            (section :id "opening-files"
              (hgroup
                (span)
                (h4 "Opening Files"))
              (div :class "outline-text-5" :id "text-org830b912"
                (p "In addition to using treemacs to browse your project, you can open files by typing "
                  (code "Enter") " while highlighting a file.")))
            (section :id "renaming-files"
              (hgroup
                (span)
                (h4 "Renaming Files"))
              (div :class "outline-text-5" :id "text-org5626522"
                (p "While highlighting a file, type "
                  (code "R") " to rename it.")
                (p "The advantage of using treemacs to rename files is this: if the file is open in a buffer, treemacs will automatically (or in some cases, provide you the option to) rename "
                  (i "the open buffer for the file") ", too. The other usual means of renaming files ("
                  (code "rename-file") ") doesn't automatically rename the buffer.")
                (p "If you rename a directory, and you have files in the directory open in buffers, "
                  (i "those buffers will also be renamed") ".")))
            (section :id "moving-files"
              (hgroup
                (span)
                (h4 "Moving Files"))
              (div :class "outline-text-5" :id "text-orgb4b072a"
                (p "While highlighting a file, type "
                  (code "m") " to move it. You will be asked where to move it.")
                (p "When you move a file that is currently open in a buffer, treemacs will ask if you want to delete the open buffer. If you don't delete it and save the buffer, you will recreate the file in the old location.")
                (p "Unfortunately, if you move a directory with files from that directory open in buffers, those buffers won't be renamed and you won't be prompted to kill them, so you will need to do that yourself.")))
            (section :id "deleting-files"
              (hgroup
                (span)
                (h4 "Deleting Files"))
              (div :class "outline-text-5" :id "text-org0cd5ee3"
                (p "While highlighting a file or directory, type "
                  (code "d") " to delete it. You'll be prompted to confirm you want to delete the file.")
                (p "Similarly for moving directories, if you have files open from a directory you delete, treemacs will not delete the buffers for you.")))
            (section :id "bulk-actions"
              (hgroup
                (span)
                (h4 "Bulk Actions"))
              (div :class "outline-text-5" :id "text-org7562122"
                (p "If you need to delete, copy, or move multiple files, you can type "
                  (code "M-m") " to be given options for the currently highlighted file."))))
          (section :id "quick-reference-table"
            (hgroup
              (span)
              (h3 "Quick Reference Table"))
            (div :class "outline-text-4" :id "text-org3389c03"
              (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                (colgroup
                  (col :class "org-left")
                  (col :class "org-left")
                  (col :class "org-left"))
                (thead
                  (tr
                    (th :scope "col" :class "org-left" "Action")
                    (th :scope "col" :class "org-left" "Emacs Binding")
                    (th :scope "col" :class "org-left" "Evil Binding")))
                (tbody
                  (tr
                    (td :class "org-left" "add known project")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC p a"))
                  (tr
                    (td :class "org-left" "switch project")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC p p"))
                  (tr
                    (td :class "org-left" "switch workspace left")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC TAB ["))
                  (tr
                    (td :class "org-left" "switch workspace right")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC TAB ]"))
                  (tr
                    (td :class "org-left" "switch to workspace")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC TAB ."))
                  (tr
                    (td :class "org-left" "switch to nth workspace")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC TAB <number>"))
                  (tr
                    (td :class "org-left" "create new workspace")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC TAB n"))
                  (tr
                    (td :class "org-left" "save workspace")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC TAB s"))
                  (tr
                    (td :class "org-left" "load workspace")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC TAB l"))
                  (tr
                    (td :class "org-left" "save all project buffers")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC p s"))
                  (tr
                    (td :class "org-left" "switch between all buffers")
                    (td :class "org-left" "C-x b")
                    (td :class "org-left" "SPC b B"))
                  (tr
                    (td :class "org-left" "switch between project buffers")
                    (td :class "org-left" "C-c p b")
                    (td :class "org-left" "SPC b b"))
                  (tr
                    (td :class "org-left" "find file in project")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC SPC"))
                  (tr
                    (td :class "org-left" "search text in project")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC /"))
                  (tr
                    (td :class "org-left" "embark export")
                    (td :class "org-left" "C-c C-e")
                    (td :class "org-left" " "))
                  (tr
                    (td :class "org-left" "save changes in embark buffer")
                    (td :class "org-left" "C-c C-c")
                    (td :class "org-left" " "))
                  (tr
                    (td :class "org-left" "discard changes in embark buffer")
                    (td :class "org-left" "C-c C-k")
                    (td :class "org-left" " "))
                  (tr
                    (td :class "org-left" "toggle treemacs")
                    (td :class "org-left" " ")
                    (td :class "org-left" "SPC o p"))
                  (tr
                    (td :class "org-left" "open file in treemacs")
                    (td :class "org-left" "Enter")
                    (td :class "org-left" "Enter"))
                  (tr
                    (td :class "org-left" "rename file in treemacs")
                    (td :class "org-left" "R")
                    (td :class "org-left" "R"))
                  (tr
                    (td :class "org-left" "move file in treemacs")
                    (td :class "org-left" "m")
                    (td :class "org-left" "m"))
                  (tr
                    (td :class "org-left" "delete file in treemacs")
                    (td :class "org-left" "d")
                    (td :class "org-left" "d"))
                  (tr
                    (td :class "org-left" "bulk actions in treemacs")
                    (td :class "org-left" "M-m")
                    (td :class "org-left" "M-m")))))))
        (section :id "learning-more"
          (hgroup
            (span)
            (h2 "LEARNING MORE"))
          (div :class "outline-text-3" :id "text-orgf7e7c14"
            (p "Type "
              (code "M-x") " to open that minibuffer. Type "
              (code "describe") ". \"Describing\" is how you get more information about something in Emacs. This feature makes it \"self-documenting\"."))
          (section :id "-describe-key-"
            (hgroup
              (span)
              (h3
                (code "describe-key")))
            (div :class "outline-text-4" :id "text-org90e0399"
              (p
                (code "C-h k") " will run the command "
                (code "describe-key") ". You will be prompted to enter a keybinding to describe.")
              (p "Type "
                (code "SPC f f") " in that prompt. It will tell you that the keybinding is for the "
                (code "find-file") " function and lots of other information. It will tell you if there are other keybindings, for example. You may prefer an "
                (code "evil") " binding or a Emacs binding.")))
          (section :id "-describe-mode-"
            (hgroup
              (span)
              (h3
                (code "describe-mode")))
            (div :class "outline-text-4" :id "text-org61285b7"
              (p
                (code "C-h m") " will run the command "
                (code "describe-mode") ". This will provide detailed information about all of the modes that are active in a given buffer.")))
          (section :id "-embark-bindings-"
            (hgroup
              (span)
              (h3
                (code "embark-bindings")))
            (div :class "outline-text-4" :id "text-org2b9889b"
              (p
                (code "C-h b b") " will run the command "
                (code "embark-bindings") ". This will show all of the bindings available in the current buffer. Useful especially for discovering Evil keybindings that may only be one character long.")))
          (section :id "-info-"
            (hgroup
              (span)
              (h3
                (code "info")))
            (div :class "outline-text-4" :id "text-org4c491ab"
              (p "Type "
                (code "C-h i") " or "
                (code "SPC h i") " ("
                (code "info") ") to open the Emacs manual.")))))
      (section :id "the-lisp-ide"
        (hgroup
          (span)
          (h1 "THE LISP IDE"))
        (div :class "outline-text-2" :id "text-org153fd8d")
        (section :id "the-lisp-coding-environment"
          (hgroup
            (span)
            (h2 "THE LISP CODING ENVIRONMENT"))
          (div :class "outline-text-3" :id "text-org34e1c0e"
            (p "I've already introduced some essential functions and keybindings for coding Lisp in Emacs. Now that you have a basic knowledge of both Lisp and Emacs commands for coding in Lisp, it will be useful to gain a deeper knowledge of the Lisp IDE in Emacs.")
            (p "Common Lisp includes debugging functions like "
              (code "trace") ", "
              (code "break") ", etc. that allow you to do debugging directly in the REPL.")
            (p "However, there are Common Lisp IDEs that help improve the ergonomics of using the debugger. One of those IDEs is Sly, which was installed when we configured the "
              (code "init.el") " file to include "
              (code "common-lisp") " language support.")))
        (section :id "sly-backtrace-navigation"
          (hgroup
            (span)
            (h2 "SLY BACKTRACE NAVIGATION"))
          (div :class "outline-text-3" :id "text-org3a6309e"
            (p "There are some functions within the Sly debugger that we can use for simple navigation. They all are prefixed with "
              (code "sly-db-") " and are discoverable if you run "
              (code "embark-bindings") " with "
              (code "SPC h b b") " or "
              (code "C-h b b") ". We'll look at a few of them here."))
          (section :id "-sly-db-up-sly-db-down-"
            (hgroup
              (span)
              (h3
                (code "sly-db-up") " & "
                (code "sly-db-down")))
            (div :class "outline-text-4" :id "text-org35e6da9"
              (p "Move the cursor up or down the stack in the backtrace with "
                (code "C-k") " and "
                (code "C-j") ". If the cursor is not highlighting any frames in the stack, the cursor will first move to the top frame if you run "
                (code "sly-db-down") ".")))
          (section :id "-sly-db-toggle-details-"
            (hgroup
              (span)
              (h3
                (code "sly-db-toggle-details")))
            (div :class "outline-text-4" :id "text-org327ca3d"
              (p "With the cursor over a stack frame in the backtrace, type "
                (code "t") " to show any local variables and bindings, arguments passed, etc.")))
          (section :id "-sly-db-show-frame-source-"
            (hgroup
              (span)
              (h3
                (code "sly-db-show-frame-source")))
            (div :class "outline-text-4" :id "text-orgaab90da"
              (p "If you bring the cursor over a stack frame in the backtrace and press the "
                (code "v") " key, Sly will open the source code file for that line in the backtrace and briefly highlight the exact form that executed in that frame. Useful for navigating straight to possible sources of error.")))
          (section :id "-sly-db-details-up-sly-db-details-down-"
            (hgroup
              (span)
              (h3
                (code "sly-db-details-up") " & "
                (code "sly-db-details-down")))
            (div :class "outline-text-4" :id "text-org72734f5"
              (p "These will do all three functions above: move the cursor to a stack from in the backtrace, toggle open/close the details for that frame, open the source for the frame in a different window, and highlight the form in that source. The bindings are "
                (code "M-p") " and "
                (code "M-n") " (or "
                (code "M-k") " and "
                (code "M-j") " if you prefer more Evilly bindings consistent with the ones above for the normal up and down commands)."))))
        (section :id "tracing"
          (hgroup
            (span)
            (h2 "TRACING"))
          (div :class "outline-text-3" :id "text-org4767d04"
            (p "Beyond simple navigation within the debugger is actual debugging methods and tools.")
            (p "Tracing is a debugging method that shows a form as it is called and then what it returns. Tracing is typically used for recursive functions."))
          (section :id "-sly-fancy-trace-"
            (hgroup
              (span)
              (h3
                (code "sly-fancy-trace")))
            (div :class "outline-text-4" :id "text-org19cce56"
              (p "Let's say you have this function:")
              (pre
                (code :class "lisp" "(defun factorial (n)
  \"Compute factorial using linear recursion.\"
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))"))
              (p "You can run "
                (code "trace") " on a function in Emacs like this:")
              (pre
                (code :class "lisp" "(trace factorial)"))
              (p "…or by highlighting a function name symbol and typing "
                (code "C-c M-t") " for "
                (code "sly-fancy-trace") ".")
              (p "If you use fancy tracing on "
                (code "factorial") " above, you will get output like this in the REPL:")
              (pre :class "example" :id "orgeed3ef8" "0: (FACTORIAL 5)
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
            (div :class "outline-text-4" :id "text-org6cbbd9e"
              (p "Alternatively, you can trace using "
                (code "sly-trace-dialog-toggle-trace") " via "
                (code "C-c C-t") " or "
                (code "SPC m T T") ". The difference is that fancy trace will show the trace in the REPL, whereas the trace dialog toggle requires you to open the trace dialog with "
                (code "C-c T") ".")
              (p "If you use the trace dialog, you get output like this:")
              (pre :class "example" :id "orgd124b5a" "0 - factorial
  | > 5 (3 bits, #x5, #o5, #b101)
  | < 120 (7 bits, #x78, #o170, #b1111000)
1 `--- factorial
     | > 4 (3 bits, #x4, #o4, #b100)
     | < 24 (5 bits, #x18, #o30, #b11000)
2    `--- factorial
        | > 3 (2 bits, #x3, #o3, #b11)
        | < 6 (3 bits, #x6, #o6, #b110)
3       `--- factorial
           | > 2 (2 bits, #x2, #o2, #b10)
           | < 2 (2 bits, #x2, #o2, #b10)
4          `--- factorial
                > 1 (1 bit, #x1, #o1, #b1)
                < 1 (1 bit, #x1, #o1, #b1)")
              (p "You can "
                (code "untrace") " the function when you're done either with the Lisp function or by using the same commands above in Emacs."))))
        (section :id "stickers"
          (hgroup
            (span)
            (h2 "STICKERS"))
          (div :class "outline-text-3" :id "text-org602ef47"
            (p "Where Sly's debugging capabilities really shine is with "
              (code "stickers") ". Stickers are basically a replacement for "
              (code "print") " and "
              (code "break") " functions"))
          (section :id "-sly-stickers-dwim-"
            (hgroup
              (span)
              (h3
                (code "sly-stickers-dwim")))
            (div :class "outline-text-4" :id "text-org050a1a6"
              (p "Let's say we have the following code:")
              (pre
                (code :class "lisp" "(defun do-some-math (num)
  (/ (+ num (* num num num) (- num 10 num)) 7))
(do-some-math 9)"))
              (p "And you're thinking, \"Hmm, I wonder what the result of that addition was?\"")
              (p "You could wrap it in "
                (code "print") ". Or you could use a sticker. Highlight the opening parenthesis of the addition form then use "
                (code "sly-stickers-dwim") " via "
                (code "C-c C-s C-s") " or "
                (code "SPC m s s") " to place a sticker on it. After placing the sticker, you need to recompile the function ("
                (code "C-c C-c") ") to \"arm\" the sticker.")))
          (section :id "-sly-stickers-replay-"
            (hgroup
              (span)
              (h3
                (code "sly-stickers-replay")))
            (div :class "outline-text-4" :id "text-org2f31631"
              (p "With the sticker armed, when you run the function, the return value of the form you placed the sticker on will be recorded. You can view the recording with "
                (code "sly-stickers-replay") " using "
                (code "C-c C-s C-r") " or "
                (code "SPC m s r") ".")))
          (section :id "-sly-stickers-toggle-break-on-stickers-sly-db-step-"
            (hgroup
              (span)
              (h3
                (code "sly-stickers-toggle-break-on-stickers") " & "
                (code "sly-db-step")))
            (div :class "outline-text-4" :id "text-orgb17b7b3"
              (p "You can also configure Sly to break when computation reaches the sticker using "
                (code "sly-stickers-toggle-break-on-stickers") " via "
                (code "SPC m s b") ". During computation, when a sticker is reached, the debugger will open with a message like this:")
              (pre :class "example" :id "org92e8164" "#<JUST BEFORE #<STICKER id=145 hit-count=1>>
   [Condition of type SLYNK-STICKERS::JUST-BEFORE-STICKER]")
              (p "The sticker will also flash in the source code file window. Useful for when you have multiple stickers and need to know which one you're looking at.")
              (p "If you choose the "
                (code "CONTINUE") " restart (either via "
                (code "c") " or "
                (code "0") " or navigating to "
                (code "[CONTINUE]") " and typing "
                (code "Enter") ") or you use "
                (code "sly-db-step") " via "
                (code "s") " in the debugger window, you will see a message like this:")
              (pre :class "example" :id "org47ad75f" "#<RIGHT-AFTER #<STICKER id=255 hit-count=2> (recorded #<RECORDING 1 values>)>
   [Condition of type SLYNK-STICKERS::RIGHT-AFTER-STICKER]")
              (p "…and the return value of the form where the sticker is placed.")))
          (section :id "-sly-stepper-"
            (hgroup
              (span)
              (h3
                (code "sly-stepper")))
            (div :class "outline-text-4" :id "text-org54e688e"
              (p "If you are having trouble tracking down the exact location of your error, you can perform a comprehensive sweep using "
                (code "sly-stepper") " with "
                (code "C-c C-s P") ". That will apply stickers to every form inside a function. After running "
                (code "sly-stepper") ", compile the function to arm the stickers.")
              (p
                (code "sly-stickers-replay") " is especially useful because, like with the breaking option, it will open the source code file and flash the exact sticker whose value it's displaying. You can also navigate to the next sticker with "
                (code "n") ", "
                (i "or the previous one") " with "
                (code "p") ". Every time you move forward or backward in the replay, the stickers will flash. This makes a bit easier to follow the control flow of the code.")))))
      (section :id "structured-editing"
        (hgroup
          (span)
          (h1 "STRUCTURED EDITING"))
        (div :class "outline-text-2" :id "text-org004be2d")
        (section :id "restructuring-code-with-ease"
          (hgroup
            (span)
            (h2 "RESTRUCTURING CODE WITH EASE"))
          (div :class "outline-text-3" :id "text-org0a9909d"
            (p "Writing Lisp code often involves moving forms around or wrapping forms in new forms. If you're using "
              (code "lispy-mode") " or "
              (code "lispyville-mode") "–both turned on with Doom's "
              (code "lispy") " config in the "
              (code "init.el") " file–you can more easily build out and modify Lisp code."))
          (section :id "slurping-and-barfing"
            (hgroup
              (span)
              (h3 "Slurping and barfing"))
            (div :class "outline-text-4" :id "text-org46559e2"
              (p "Slurping and barfing are terms for describing moving parentheses to either include or exclude forms from other forms. Let me show you what I mean.")
              (p "In the tic-tac-toe project, we wrote code to find positions on the board to win or block the opponent's win. To begin, we wrote this:")
              (pre
                (code :class "lisp" "(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*)"))
              (p "This finds any triplet that sums to 20 on the board. But we then wanted to search the result of that "
                (code "find-if") " form to search for the empty space in the triplet.")
              (pre
                (code :class "lisp" "(find-if #'(lambda (element) (= (nth element board) 0))
         (find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*))"))
              (p "If you use structural editing, this can actually be difficult at first. You might naturally try to write the code like this:")
              (pre
                (code :class "lisp" "(find-if #'(lambda (element) (= (nth element board) 0)))
(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*)"))
              (p "And then, in order to wrap the bottom "
                (code "find-if") " form with the top "
                (code "find-if") " form, you might try to delete the final parenthesis of the top "
                (code "find-if") " and then move it to the end of the bottom "
                (code "find-if") " form:")
              (pre
                (code :class "lisp" "(find-if #'(lambda (element) (= (nth element board) 0)) ; deleted paren
(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*)) ; added paren"))
              (p "What actually happens, however, is that the entire top "
                (code "find-if") " form is deleted when you try to delete just the last parenthesis! This very confusing behavior is from "
                (code "lispy-mode") ". It is designed to keep parentheses balanced at all times.")
              (p "If you want to move only one parenthesis, then, what do you do?")
              (p "One option would be to "
                (code "kill") " the "
                (code "find-if") " form intended to become the inner form…")
              (pre
                (code :class "lisp" "(find-if #'(lambda (element) (= (nth element board) 0)) ; bottom form killed"))
              (p "and then paste it at the end of the outer "
                (code "find-if") "…")
              (pre
                (code :class "lisp" "(find-if #'(lambda (element) (= (nth element board) 0)) (find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*))"))
              (p "and then move the cursor to the beginning parenthesis of the inner "
                (code "find-if") " and press "
                (code "ENTER") " to place it on a new line.")
              (pre
                (code :class "lisp" "(find-if #'(lambda (element) (= (nth element board) 0))
         (find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*))"))
              (p "This is okay, but there is an even easier method called "
                (code "slurping") ".")
              (p "Start with this code:")
              (pre
                (code :class "lisp" "(find-if #'(lambda (element) (= (nth element board) 0)))
(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*)"))
              (p "Place the cursor on the last parenthesis of the top form. While still in Evil Normal mode (not Insert mode), press "
                (code ">") " (the "
                (code "lispyville-mode") " keybinding for "
                (code "lispyville-slurp") "). The parenthesis will move to the "
                (b "end") " of the bottom "
                (code "find-if") " form, making it an inner nested form.")
              (p "Slurping is especially useful for any time you need to \"build out\" from some code. Consider this:")
              (pre
                (code :class "lisp" "(find-if #'(lambda (triple)
             (= (sum-triple *test-board* triple) target-sum))
         *triples*)"))
              (p "This is how we started building out the code for detecting squeezes. But then we realized that we needed to add more conditions. For a strategy to be a squeeze, the triplet need to sum to the target, "
                (b "and") " it needs to be diagonal as well as some other conditions. That means that we needed to wrap several predicate functions in an "
                (code "and") " form.")
              (pre
                (code :class "lisp" ";; These need to be wrapped in an (and ...) form
(= (sum-triple board triple) target-sum)
(diagonal-p triple)"))
              (p "We can do that by first writing the "
                (code "and") " form before the other two forms:")
              (pre
                (code :class "lisp" "(and) (= (sum-triple board triple) target-sum)
(diagonal-p triple)"))
              (p "Put the Evil Normal-mode cursor on the end paren of the "
                (code "and") " form, and then "
                (code "slurp") " up the other two forms.")
              (pre
                (code :class "lisp" "(and (= (sum-triple board triple) target-sum)
     (diagonal-p triple))"))
              (p "Once we added the rest of the conditions, we wanted to then bind the return value of the "
                (code "find-if") " form to a local variable using "
                (code "let") ". The process looks like this:")
              (pre
                (code :class "lisp" ";; Write the LET and variable we want in the LET.
;; let               Finished find-if
(let ((squeeze-p))) (find-if #'(lambda (triple)
             (and (= (sum-triple board triple) target-sum)
                  (diagonal-p triple)
                  (not (human-in-middle-p board))
                  (side-empty-p board)))
         *triples*)"))
              (p "Then use "
                (code "lispyville-slurp") " on the three ending parentheses of the "
                (code "let") " form to complete the new form.")
              (p "After adding an "
                (code "if") " form, we need to make this a function, so we can write the function definition above the entire "
                (code "let") " form:")
              (pre
                (code :class "lisp" "(defun detect-squeeze (board target-sum))
(let ((squeeze-p
        (find-if #'(lambda (triple)
                     (and (= (sum-triple board triple) target-sum)
                          (diagonal-p triple)
                          (not (human-in-middle-p board))
                          (side-empty-p board)))
                 *triples*)))
  (if squeeze-p
      (find-empty-position board *sides*)))"))
              (p "And then just slurp up the entire "
                (code "let") " into the "
                (code "defun") " form.")
              (p "Again, you "
                (i "could") " just "
                (code "kill") " and "
                (code "paste") " the forms around, but killing can sometimes be dangerous (if you kill a word using "
                (code "M-backspace") ", you've just replaced your function in the kill-ring). It's nicer to simply keep the code on the screen and using slurp to do what you want.")
              (p "The reverse of "
                (code "slurp") " is "
                (code "barf") ", bound to "
                (code "<") " in Doom Emacs "
                (code "lispyville-mode") ". I don't find it nearly as useful as slurping, but maybe you can find a use for it.")))
          (section :id "dragging-forms-forward-backward"
            (hgroup
              (span)
              (h3 "Dragging forms forward/backward"))
            (div :class "outline-text-4" :id "text-org4b6b0b6"
              (p "Another operation you'll probably want to do a lot is move forms around. For example, maybe you wrote this:")
              (pre
                (code :class "lisp" "(defun choose-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (take-random-position board)
      (block-two-on-one-play board)
      (block-squeeze-play board)))"))
              (p "Then you realized that the "
                (code "block-two-on-one") " and "
                (code "block-squeeze-play") " strategies need to take higher priority over all but the "
                (code "make-three-in-a-row") " strategy. You could, of course, kill/paste them into the proper position. But if you want, you can also use "
                (code "lispyville-drag-backward") "–bound to "
                (code "M-k") "–while the cursor is on the opening parentheses of the "
                (code "block-") " forms to move them. "
                (code "lispyville-drag-forward") "–bound to "
                (code "M-j") "–goes in the opposite direction.")
              (p "They also work on individual symbols. For example, let's say you have a call to "
                (code "nth") ":")
              (pre
                (code :class "lisp" "(nth 4 my-list)"))
              (p "But then you decide you want to use "
                (code "member") " instead.")
              (pre
                (code :class "lisp" "(member 4 my-list)"))
              (p "Unfortunately, you now need to switch the location of 4 and "
                (code "my-list") " in the arguments to "
                (code "member") ". Fortunately, you can use "
                (code "lispyville-drag-") " functions for that, too. Place the cursor over the 4 and use "
                (code "lispyville-drag-forward") " ("
                (code "M-j") ") to switch the 4 and "
                (code "my-list") "."))))
        (section :id "navigating-the-sea-of-parentheses"
          (hgroup
            (span)
            (h2 "NAVIGATING THE SEA OF PARENTHESES"))
          (div :class "outline-text-3" :id "text-org6ac65ce"
            (p "Navigation is an important topic in Emacs. When programming Lisp in Doom Emacs, you have additional navigation options that can dramatically speed up your development flow."))
          (section :id "generic-searching"
            (hgroup
              (span)
              (h3 "Generic searching"))
            (div :class "outline-text-4" :id "text-org0f2bec9"
              (p "Doom Emacs comes with a function called "
                (code "+default/search-buffer") ", bound to "
                (code "SPC s s") ", that you can use in any mode to search for some text. This is an essential tool for navigation.")))
          (section :id "tab-navigation"
            (hgroup
              (span)
              (h3 "Tab navigation"))
            (div :class "outline-text-4" :id "text-orge1c03cc"
              (p "When your Normal mode cursor is on a parenthesis, if you press "
                (code "TAB") " the cursor will move to the matching parenthesis on the other side. This is actually a more general function called "
                (code "evil-jump-item") ".")
              (p
                (code "+default/search-buffer") " will only move the cursor to the "
                (i "first") " instance of some text on a line. It's often quicker to tab to the \"cattail\" at the end of a Lisp form, move the cursor to another parenthesis, and then tab to the matching parenthesis on the other side to get closer to the text you're navigating to."))))
        (section :id "more-restructuring-navigating-tools"
          (hgroup
            (span)
            (h2 "MORE RESTRUCTURING & NAVIGATING TOOLS"))
          (div :class "outline-text-3" :id "text-orga913c69"
            (p "Remember that you can get more information about the keybindings available in the buffer using either "
              (code "M-x describe-key") " ("
              (code "SPC h k") "), "
              (code "M-x describe-mode") " ("
              (code "SPC h m") "), and "
              (code "M-x embark-bindings") " ("
              (code "SPC h b b") "). Look into the "
              (code "lispy-") ", "
              (code "lispyville-") ", and "
              (code "special-lispy-") " commands and their keybindings."))))
      (section :id "beyond-lists"
        (hgroup
          (span)
          (h1 "BEYOND LISTS"))
        (div :class "outline-text-2" :id "text-org12828d5")
        (section :id "oop"
          (hgroup
            (span)
            (h2 "OOP"))
          (div :class "outline-text-3" :id "text-org593ade7")
          (section :id "what-are-common-lisp-classes-"
            (hgroup
              (span)
              (h3 "What are Common Lisp classes?"))
            (div :class "outline-text-4" :id "text-org42c261d"
              (p
                (code "Classes") " are user-defined data types that group related data. Unlike the OOP systems found elsewhere, Common Lisp classes don't package data and an API for acting on that data together in the same module. Instead, classes define types of data that can be targeted or specialized for by "
                (code "typecase") " forms and generic functions/methods.")))
          (section :id "class-basics"
            (hgroup
              (span)
              (h3 "Class Basics"))
            (div :class "outline-text-4" :id "text-org5fc2865"
              (pre
                (code :class "lisp" "(defclass creature ()
  ((name :initarg :name
         :initform nil
         :accessor creature-name)
   (territory :initarg :territory
              :initform \"Earth\"
              :accessor creature-territory)))"))
              (p "This is the basic structure of a class definition. "
                (code "defclass") " takes at least three arguments: a name, a list of classes to inherit from, and "
                (code "slot") " definitions.")
              (p "Use "
                (code "make-instance") " to make an instance of the class.")
              (pre
                (code :class "lisp" "(make-instance 'creature :name \"Cat\")"))
              (p "Slot definitions can take a number of options. The essentials are:"))
            (section :id "-initargs-"
              (hgroup
                (span)
                (h4
                  (code ":initargs")))
              (div :class "outline-text-5" :id "text-org819170b"
                (p "Defines the keyword argument used for setting the slot's value when creating an instance of the class.")
                (pre
                  (code :class "lisp" "(defparameter *cat* (make-instance 'creature :name \"Cat\"))"))))
            (section :id "-initform-"
              (hgroup
                (span)
                (h4
                  (code ":initform")))
              (div :class "outline-text-5" :id "text-orgbdd74dc"
                (p "Defines the default value of the slot if none is provided at the time of instantiation.")
                (pre
                  (code :class "lisp" "(creature-territory *cat*)
                                        ; => \"Earth\""))
                (p "When a class "
                  (code "inherits") " from another class, its definition includes everything in the superclass plus the additional slots it's own definition includes. You can also overwrite the definition of a superclass's slot.")
                (pre
                  (code :class "lisp" "(defclass person (creature)
  ((name :initarg :creature-name
         :initform \"John Doe\"
         :accessor person-name)
   (marrital-status :initarg :marrital-status
                    :initform :single
                    :accessor marrital-status)))

(defparameter *john-doe* (make-instance 'person :marrital-status :married))

(person-name *john-doe*)
                                        ; => \"John Doe\""))))
            (section :id "-accessor-"
              (hgroup
                (span)
                (h4
                  (code ":accessor")))
              (div :class "outline-text-5" :id "text-orgcc14706"
                (p "Defines the "
                  (i "generic method") " that can be used to both access and modify the slot's value.")
                (pre
                  (code :class "lisp" "(creature-name *cat*)
                                        ; => \"Cat\"

(setf (creature-name *cat*) \"Tiger\")

(creature-name *cat*)
                                        ; => \"Tiger\""))
                (p "Because accessors are generic methods specialized on instances of the class that defines them, two classes can define accessors with the same name without any conflicts.")
                (p "In practice, you'll often see accessors that follow a "
                  (code "<class-name>-<slot-name>") " convention for clarity.")))
            (section :id "-reader-writer-"
              (hgroup
                (span)
                (h4
                  (code ":reader") " & "
                  (code ":writer")))
              (div :class "outline-text-5" :id "text-org54cc782"
                (p "Usually you want an accessor–a method that allows you to both read and write to a slot. In some cases, as in condition definitions, you may want to only allow the program to read from a slot. In that case, you can replace "
                  (code ":accessor") " with "
                  (code ":reader") "."))))
          (section :id "custom-constructors"
            (hgroup
              (span)
              (h3 "Custom Constructors"))
            (div :class "outline-text-4" :id "text-org3b865fc"
              (p "While you can manually called "
                (code "make-instance") " all the time, it's common to define a custom constructor for objects.")
              (pre
                (code :class "lisp" "(defun make-creature (name territory)
  (make-instance 'creature :name name :territory territory))"))
              (p "In addition to reducing typing in the simple case, custom constructors are also where you can filter input, add type-checking with "
                (code "etypecase") ", modify data before making an instance, etc.")))
          (section :id "generic-functions-methods"
            (hgroup
              (span)
              (h3 "Generic Functions & Methods"))
            (div :class "outline-text-4" :id "text-org2a72971"
              (p "Classes describe what an object "
                (i "has") ", but they don't say anything about what an object "
                (i "does") ". That's because in Common Lisp, "
                (i "classes") " don't have "
                (code "methods") "; "
                (i "generic functions") " have methods.")
              (pre
                (code :class "lisp" "(defclass person ()
  ((name :initarg :name :initform nil :accessor person-name)))
(defgeneric greet (instance))
(defmethod greet ((this person))
  (format t \"~&Hello, ~a~%\" (person-name this)))
(defparameter *john-doe* (make-instance 'person :name \"John Doe\"))
(greet *john-doe*)
                                        ; Hello, John Doe
                                        ;  => NIL"))
              (p
                (code "defgeneric") " defines a generic function. Generic functions define an interface for, and then dispatch to, "
                (code "generic methods") ". "
                (code "greet") " defines an interface: it only requires an instance of a class. When a call to "
                (code "greet") " is made, it looks at the object instance passed and dispatches to the method specialized on that generic function.")))
          (section :id "extending-oop-systems"
            (hgroup
              (span)
              (h3 "Extending OOP Systems"))
            (div :class "outline-text-4" :id "text-org692737a"
              (p "Every generic method can be wrapped in code that will modify its behavior before it's called, after it's called, or both (\"around\"). Methods can be specialized on more than one type. A method call is sent to the method that is most \"specific\"–the method that specializes on types that are closest to the ones passed to the method as argument. A more specific method can call the next less specific method in a chain of methods.")
              (p "Method combination is a technique that involves chaining together some mixture of the above techniques. Extending an OOP system in Common Lisp will involve some degree of method combination.")))
          (section :id "before-after-around-methods"
            (hgroup
              (span)
              (h3 "Before, After, & Around Methods"))
            (div :class "outline-text-4" :id "text-org67fbb3b"
              (p "Before, after, and around methods are methods that modify the behavior of a method–called the primary method–at certain points in the method call.")
              (pre
                (code :class "lisp" "(defmethod greet :before ((this person))
  (format t \"~&Oh, what a nice looking young man. Let me go talk to him...~%\"))
(greet *john-doe*)
                                        ; Oh, what a nice looking young man. Let me go talk to him...
                                        ; Hello, John Doe
                                        ;  => NIL

(defmethod greet :after ((this person))
  (format t \"~&(Strong handshake, good man.)~%\"))
(greet *john-doe*)
                                        ; Oh, what a nice looking young man. Let me go talk to him...
                                        ; Hello, John Doe
                                        ; (Strong handshake, good man.)
                                        ;  => NIL"))
              (p "Before and after methods are fairly self explanatory. Primary methods are the core of the onion; before and after methods are two layers around the core.")
              (p "After methods are a layer outside of before and after methods.")
              (pre
                (code :class "lisp" "(defmethod greet :around ((this person))
  (format t \"I should get off the couch and go talk to people.\"))
(greet *john-doe*)
; I should get off the couch and go talk to people. => NIL"))
              (p "However, they require extra work to use.")
              (pre
                (code :class "lisp" "(defmethod greet :around ((this person))
  (format t \"I should get off the couch and go talk to people.\")
  (call-next-method)
  (format t \"Here's my card, ~a. Let me know if you need help learning Lisp.\" (person-name this)))
(greet *john-doe*)
; I should get off the couch and go talk to people.
; Oh, what a nice looking young man. Let me go talk to him...
; Hello, John Doe
; (Strong handshake, good man.)
; Here's my card, John Doe. Let me know if you need help learning Lisp. => NIL"))
              (p
                (code "call-next-method") " calls the next most specific version of "
                (code "greet") " for the "
                (code "*john-doe*") " object, which is the before method. The before method automatically calls the next method–the primary method–so we don't have to call "
                (code "call-next-method") " in it. Same goes for the around method.")
              (p "Since "
                (code "call-next-method") " works by calling the next most specific method, it can be used to chain together methods in other ways.")
              (pre
                (code :class "lisp" "(defclass smart-person (person) ())
(defclass rich-person (person) ())
(defclass dumb-person (person) ())
(defclass poor-person (person) ())

(defparameter *musk* (make-instance 'smart-person :name \"Mr. Musk\"))

(defmethod greet ((this smart-person))
  (format t \"~&(Wow, it's ~a! I can't believe he's here!)~%\" (person-name this)))
(greet *musk*)
                                        ; I should get off the couch and go talk to people.
                                        ; Oh, what a nice looking young man. Let me go talk to him...
                                        ; (Wow, it's Mr. Musk! I can't believe he's here!) <- Primary method response.
                                        ; (Strong handshake, good man.)
                                        ; Here's my card, Mr. Musk. Let me know if you need help learning Lisp. => NIL
(defmethod greet ((this smart-person))
  (format t \"~&(Wow, it's ~a! I can't believe he's here!)~%\" (person-name this))
  (call-next-method))
(greet *musk*)
                                        ; I should get off the couch and go talk to people.
                                        ; Oh, what a nice looking young man. Let me go talk to him...
                                        ; (Wow, it's Mr. Musk! I can't believe he's here!) <- Primary method response.
                                        ; Hello, Mr. Musk                                  <- Next most specific method response.
                                        ; (Strong handshake, good man.)
                                        ; Here's my card, Mr. Musk. Let me know if you need help learning Lisp. => NIL"))
              (p "All this indirection can be confusing. If you are extending some OOP system and your method isn't being called for some reason, you can use "
                (code "compute-applicable-methods") " to find out which methods a particular combination of arguments to a certain function name apply to those arguments.")
              (pre
                (code :class "lisp" "(compute-applicable-methods #'greet (list (make-instance 'person :name \"Micah\")))
                                        ; => (#<STANDARD-METHOD COMMON-LISP-USER::GREET :AROUND (PERSON) {7009D091C3}>
                                        ; #<STANDARD-METHOD COMMON-LISP-USER::GREET :AFTER (PERSON) {7009D091E3}>
                                        ; #<STANDARD-METHOD COMMON-LISP-USER::GREET :BEFORE (PERSON) {7009D09203}>
                                        ; #<STANDARD-METHOD COMMON-LISP-USER::GREET (PERSON) {7009D09223}>)

(compute-applicable-methods #'greet (list (make-instance 'smart-person :name \"Mr. Musk\")))
                                        ; => (#<STANDARD-METHOD COMMON-LISP-USER::GREET (SMART-PERSON) {700792E213}>
                                        ; #<STANDARD-METHOD COMMON-LISP-USER::GREET :AROUND (PERSON) {7009D091C3}>
                                        ; #<STANDARD-METHOD COMMON-LISP-USER::GREET :AFTER (PERSON) {7009D091E3}>
                                        ; #<STANDARD-METHOD COMMON-LISP-USER::GREET :BEFORE (PERSON) {7009D09203}>
                                        ; #<STANDARD-METHOD COMMON-LISP-USER::GREET (PERSON) {7009D09223}>)"))))
          (section :id "pretty-printing-objects"
            (hgroup
              (span)
              (h3 "Pretty Printing Objects"))
            (div :class "outline-text-4" :id "text-org7a74bf2"
              (p "There are several generic functions worth knowing about when you first start using Lisp. One of them is "
                (code "print-object") "–used for pretty printing objects.")
              (pre
                (code :class "lisp" "(defmethod print-object ((this creature) stream)
  (with-slots (name territory) this
    (format stream \"~a ~a\" name territory)))

(make-instance 'creature :name \"Cat\" :territory \"House\")
                                        ; => Cat House"))
              (p
                (code "with-slots") " is a useful macro for dealing with object data. You could use "
                (code "let") " instead…")
              (pre
                (code :class "lisp" "(let ((name (slot-value this 'name))
      (territory (slot-value this 'territory)))
  ...)"))
              (p "But "
                (code "with-slots") " is nicer for this purpose. There is also "
                (code "with-accessors") ", which is equivalent to using "
                (code "creature-name") " and "
                (code "creature-territory") " instead of "
                (code "slot-value") ".")
              (p "In the case of "
                (code "print-object") ", it's safest to stick with "
                (code "with-slots") " or "
                (code "slot-value") ". If you change the accessor, you might end up with difficult to debug errors when printing objects.")
              (p "Another common error would be to try to print slot values that aren't bound. That's why you should probably have "
                (code ":initform") " set to "
                (code "nil") " if you have no other desired default value; you won't have trouble with "
                (code "UNBOUND SLOT") " errors.")
              (pre
                (code :class "lisp" "(defclass beverage ()
  ((name :initarg :name :accessor name)
   (amount :initarg :amount :accessor amount)))

(make-instance 'beverage)
                                        ; => #<BEVERAGE {700B0D0343}>

(defmethod print-object ((this beverage) stream)
  (with-slots (name amount) this
    (format stream \"~a ~a\" name amount)))

(make-instance 'beverage)
                                        ; => #<BEVERAGE <<error printing object>> {7008470343}>"))
              (p "Since we didn't bind any of the slots–neither in the call to "
                (code "make-instance") " nor in the class definition with "
                (code ":initform") "–we get the "
                (code "<<error printing object>>") " message. We have two choices to fix the problem: check for unbound slots in the "
                (code "print-object") " method using "
                (code "slot-boundp") ", or prevent slots from being unbound when the object is initialized.")
              (pre
                (code :class "lisp" ";; Check for unbound slots.
(defmethod print-object ((this beverage) stream)
  (let ((name (when (slot-boundp this 'name) (slot-value this 'name)))
        (amount (when (slot-boundp this 'amount) (slot-value this 'amount))))
    (format stream \"~a ~a\" name amount)))

(make-instance 'beverage)
                                        ; => NIL NIL

;; Prevent slots from being unbound by specifying a default value of nil.
(defclass beverage ()
  ((name :initarg :name :initform nil :accessor name)
   (amount :initarg :amount :initform nil :accessor amount)))

(defmethod print-object ((this beverage) stream)
  (with-slots (name amount) this
    (format stream \"~a ~a\" name amount)))

(make-instance 'beverage)
                                        ; => NIL NIL")))
            (section :id "-print-unreadable-object-"
              (hgroup
                (span)
                (h4
                  (code "print-unreadable-object")))
              (div :class "outline-text-5" :id "text-org0a238c3"
                (p "By default, Lisp will provide a printed representation of an object that looks like this:")
                (pre :class "example" :id "orgf567f89" "#<BEVERAGE {700B0D0343}>")
                (p "This representation contains two important piece of information:")
                (ol :class "org-ol"
                  (li
                    (code "#<BEVERAGE ...>") ": This is a "
                    (code "BEVERAGE") " object.")
                  (li
                    (code "{700B0D0343}") ": This is where the object is in memory."))
                (p "Consider this:")
                (pre
                  (code :class "lisp" "(defclass warm-beverage (beverage) ())
(defclass cold-beverage (beverage) ())
(values (make-instance 'warm-beverage :name \"Coffee\" :amount 1000)
        (make-instance 'cold-beverage :name \"Coffee\" :amount 1000))
                                        ; => Coffee 1000, Coffee 1000"))
                (p "There's no indication that there is a difference between these two objects. If they weren't shown side-by-side, we wouldn't even know they were two different objects.")
                (p "To add this information back into your custom pretty printers, you can use the "
                  (code "print-unreadable-object") " macro.")
                (pre
                  (code :class "lisp" "(defmethod print-object ((this beverage) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (with-slots (name amount) this
      (format stream \"~a ~a\" name amount))))
(make-instance 'cold-beverage :name \"Coffee\" :amount 1000)
                                        ; => #<COLD-BEVERAGE Coffee 1000 {7008A403A3}>")))))
          (section :id "modifying-object-initialization"
            (hgroup
              (span)
              (h3 "Modifying Object Initialization"))
            (div :class "outline-text-4" :id "text-org3884624"
              (p "Let's say we have the following "
                (code "defclass") ":")
              (pre
                (code :class "lisp" "(defclass user ()
  ((name :initarg :name
         :initform nil
         :accessor user-name)
   (email :initarg :email
          :initform nil
          :accessor user-email)))"))
              (p "We've specified the type of data the slots will hold: strings.")
              (p "Let's run "
                (code "make-instance") ":")
              (pre
                (code :class "lisp" "(make-instance 'user)
                                        ; => #<USER {7007C40343}>"))
              (p "It works, but what we really want is:")
              (ol :class "org-ol"
                (li "Both slots to be required.")
                (li "To validate that the value assigned in "
                  (code "email") " is actually an email."))
              (p "We can do that using two important features of Common Lisp's object system: custom initializer methods and method modifiers.")
              (p "To validate that the email is a valid email, we would like to be able to use regular expressions. Lisp doesn't have regular expression support built in, so we'll use the "
                (code "ppcre") " package to give us regular expressions:")
              (pre
                (code :class "lisp" "(ql:quickload \"cl-ppcre\")"))
              (p "Then we'll make a function for validating that the string passed to "
                (code "make-instance") " is a valid email.")
              (pre
                (code :class "lisp" "(defun valid-email-address-p (string)
  (not (null
        (ppcre:scan \"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}$\" string))))

(valid-email-address-p \"micah@almightylisp.com\")
                                        ; => T
(valid-email-address-p \"dude*@*man.com\")
                                        ; => NIL"))
              (p "Now we just need some way to run this function when we run "
                (code "make-instance") ". To do that, we can use custom initialization with the "
                (code "initialize-instance") " method.")
              (pre
                (code :class "lisp" "(defmethod initialize-instance :before ((this user) &key name email)
  (unless name
    (error \"Name is required.\"))
  (unless email
    (error \"Email is required.\"))
  (unless (valid-email-address-p email)
    (error \"~a is not a valid email.\" email)))"))
              (p
                (code "initialize-instance") " is a generic method run whenever an object is created. Using the "
                (code ":before") " method modifier, we can run some code before the main "
                (code "initialize-instance") " method. Here, we ensure that the name and email slots are bound and that the email is valid. Importantly, if the data doesn't pass the checks, the developer is thrown into the debugger, and the object is never created.")))
          (section :id "specializing-methods-on-other-values"
            (hgroup
              (span)
              (h3 "Specializing Methods On Other Values"))
            (div :class "outline-text-4" :id "text-org2760ca8"
              (p "It's possible to specialize methods on other values besides just classes/types.")
              (pre
                (code :class "lisp" "(defmethod greet ((this (eql :robot)))
  (format t \"Woah, this is a realistic robot.\"))
(greet :robot)
                                        ; Woah, this is a realistic robot. => NIL
(defmethod greet ((this (eql (+ 2 2))))
  (format t \"Hey, Micah.\"))
(greet 4)
                                        ; Hey, Micah. => NIL
(defmethod greet ((this (eql #'+)))
  (funcall this 1 2 3 4 5))
(greet #'+)
                                        ; => 15 (4 bits, #xF, #o17, #b1111)"))
              (p "In the section later about structures, I'll show you that methods can even be specialized on structures and don't require you to use classes/types for your data structure.")
              (p "Unfortunately, you can't specialize a method on things like the type "
                (code "string") " or "
                (i "the shape of") " some data structure. For that, there's Coalton.")
              (pre
                (code :class "lisp" "(defmethod greet ((this (eql 'string)))
  (format t \"What's up, ~a?~%\" this))

(greet \"Dude\")
;; => Error
;; There is no applicable method for the generic function
;;   #<STANDARD-GENERIC-FUNCTION COMMON-LISP-USER::GREET (12)>
;; when called with arguments
;;   (\"Dude\").
;;    [Condition of type SB-PCL::NO-APPLICABLE-METHOD-ERROR]")))))
        (section :id "hash-tables"
          (hgroup
            (span)
            (h2 "HASH-TABLES"))
          (div :class "outline-text-3" :id "text-org606ea16"
            (p ":ID:       6872b282-fed5-44f1-95b1-3554cce21b80"))
          (section :id "what-is-a-hash-table-"
            (hgroup
              (span)
              (h3 "What is a hash-table?"))
            (div :class "outline-text-4" :id "text-org9960bda"
              (p "A hash-table is a group of key/value pairs. ~associated-lists= are the list-based equivalent.")))
          (section :id "making-hash-tables"
            (hgroup
              (span)
              (h3 "Making hash-tables"))
            (div :class "outline-text-4" :id "text-org314c1e1"
              (p "To make a hash table, use "
                (code "make-hash-table") ".")
              (pre
                (code :class "lisp" "(make-hash-table)
                                        ; => #<HASH-TABLE :TEST EQL :COUNT 0 {7008090473}>"))))
          (section :id "adding-accessing-modifying-items-to-a-hash-table"
            (hgroup
              (span)
              (h3 "Adding/Accessing/Modifying items to a hash-table"))
            (div :class "outline-text-4" :id "text-org98467aa"
              (pre
                (code :class "lisp" "(defparameter *h* (make-hash-table))
                                        ; => *H*
;; Add an item
(setf (gethash :name *h*) \"Micah\")
                                        ; => \"Micah\"

;; Access it
(gethash :name *h*)
                                        ; => \"Micah\", T
(gethash :age *h*)
                                        ; => NIL, NIL
;; Modify it
(setf (gethash :name *h*) \"Bob\")
                                        ; => \"Bob\""))
              (p "Compare this to ~alists=:")
              (pre
                (code :class "lisp" "(defparameter *a* '())
(setf *a* (append *a* '((:name \"Micah\"))))
                                        ; => ((:NAME \"Micah\"))
*a*
                                        ; => ((:NAME \"Micah\"))
(cadr (assoc :name *a*))
                                        ; => \"Micah\"
(cadr (assoc :age *a*))
                                        ; => NIL
(setf (cadr (assoc :name *a*)) \"Bob\")
                                        ; => \"Bob\"
*a*
                                        ; => ((:NAME \"Bob\"))"))))
          (section :id "printing-a-hash-table"
            (hgroup
              (span)
              (h3 "Printing a hash-table"))
            (div :class "outline-text-4" :id "text-org2be89e9"
              (p "Hash-tables don't get printed nicely like lists do.")
              (pre
                (code :class "lisp" "*h*
                                        ; => #<HASH-TABLE :TEST EQL :COUNT 1 {70075F0503}>
*a*
                                        ; => ((:NAME \"Bob\"))"))
              (p "Use "
                (code "maphash") " to print key/value pairs or other mapping operations.")
              (pre
                (code :class "lisp" "(defun print-hash (hash-table)
  (maphash #'(lambda (key value) (format t \"~&~a -> ~a~%\" key value)) hash-table))

(print-hash *h*)
                                        ; NAME -> Bob
                                        ;  => NIL
(setf (gethash :age *h*) 39)
                                        ; => 39 (6 bits, #x27, #o47, #b100111)
(print-hash *h*)
                                        ; NAME -> Bob
                                        ; AGE -> 39
                                        ;  => NIL"))))
          (section :id "using-strings-as-keys"
            (hgroup
              (span)
              (h3 "Using strings as keys"))
            (div :class "outline-text-4" :id "text-orgd439716"
              (p "The "
                (code "keys") " of a hash-table can be keywords, strings, or symbols. But, there's a catch:")
              (pre
                (code :class "lisp" "(setf (gethash 'favorite-language *h*) \"Common Lisp\")
                                        ; => \"Common Lisp\"
(print-hash *h*)
                                        ; NAME -> Bob
                                        ; AGE -> 39
                                        ; FAVORITE-LANGUAGE -> Common Lisp
                                        ;  => NIL
(gethash 'favorite-language *h*)
                                        ; => \"Common Lisp\", T
(setf (gethash \"location\" *h*) \"Japan\")
                                        ; => \"Japan\"
(print-hash *h*)
                                        ; NAME -> Bob
                                        ; AGE -> 39
                                        ; FAVORITE-LANGUAGE -> Common Lisp
                                        ; location -> Japan
                                        ;  => NIL
(gethash \"location\" *h*)
                                        ; => NIL, NIL
                                        ; Huh?"))
              (p
                (code "make-hash-table") ", when searching for "
                (code "\"location\"") " or any other key, defaults to testing keys using "
                (code "eql") ".")
              (pre
                (code :class "lisp" "(eql 'favorite-language 'favorite-language)
                                        ; => T
(eql \"location\" \"location\")
                                        ; => NIL"))
              (p "You can change the test to "
                (code "equal") ", "
                (code "equalp") ", or "
                (code "eq") ".")
              (pre
                (code :class "lisp" "(defparameter *h* (make-hash-table :test #'equal))
                                        ; => *H*
(setf (gethash \"location\" *h*) \"Japan\")
                                        ; => \"Japan\"
(gethash \"location\" *h*)
                                        ; => \"Japan\", T
(gethash \"LOCATION\" *h*)
                                        ; => NIL, NIL
(setf *h* (make-hash-table :test #'equalp))
                                        ; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {7008AF0503}>
(setf (gethash \"location\" *h*) \"Japan\")
                                        ; => \"Japan\"
(gethash \"location\" *h*)
                                        ; => \"Japan\", T
(gethash \"LOCatiON\" *h*)
                                        ; => \"Japan\", T")))))
        (section :id "arrays"
          (hgroup
            (span)
            (h2 "ARRAYS"))
          (div :class "outline-text-3" :id "text-org202a148"
            (p ":ID:       4ee903b2-651e-4d4b-9565-fd6b6abefa1a"))
          (section :id "differences-between-arrays-and-lists"
            (hgroup
              (span)
              (h3 "Differences between arrays and lists"))
            (div :class "outline-text-4" :id "text-orgdb4fec1"
              (p "One of the most fun things about Common Lisp is that it is the highest-level language to exist (thanks to macros), but it is also surprisingly low level as well.")
              (p "In Lisp, a list is a linked-list. Arrays, on the other hand, are surprisingly similar to the arrays in C. You need to manually set their size at creation time. Resizing them involves either assigning them a special property or simply recreating the array with the new size.")))
          (section :id "when-to-use-arrays"
            (hgroup
              (span)
              (h3 "When to use arrays"))
            (div :class "outline-text-4" :id "text-orgeac02df"
              (p "Arrays are useful when you have a prototype using lists and need to optimize for speed and memory efficiency.")))
          (section :id "kinds-of-arrays"
            (hgroup
              (span)
              (h3 "Kinds of arrays"))
            (div :class "outline-text-4" :id "text-orgd3a2505"
              (p "There are several different kinds of arrays. They include:")
              (ul :class "org-ul"
                (li "Vectors (one-dimensional arrays)")
                (li "Multi-dimensional arrays")
                (li "Specialized arrays")
                (li "Adjustable arrays")
                (li "Strings"))
              (p "Because my goal is to cover the essentials for getting started with Common Lisp and arrays are largely for optimization, we won't go into great detail about arrays.")))
          (section :id "vectors"
            (hgroup
              (span)
              (h3 "Vectors"))
            (div :class "outline-text-4" :id "text-org6cd6c4d")
            (section :id "making-a-vector"
              (hgroup
                (span)
                (h4 "Making a vector"))
              (div :class "outline-text-5" :id "text-org6a72335"
                (p
                  (code "make-array") " is the primary function for making arrays of all types.")
                (pre
                  (code :class "lisp" "(make-array 5)
                                        ; => #(0 0 0 0 0)"))
                (p "The first argument to "
                  (code "make-array") " is the "
                  (code "dimensions") " of the array: how many columns in how many rows. A single number makes a one-dimension array with the same number of elements as the number.")
                (p "You can set the initial element(s) in the array:")
                (pre
                  (code :class "lisp" "(make-array 5 :initial-element \"ALMIGHTY\")
 ; => #(\"ALMIGHTY\" \"ALMIGHTY\" \"ALMIGHTY\" \"ALMIGHTY\" \"ALMIGHTY\")
(make-array 5 :initial-contents '(this is almighty common lisp))
 ; => #(THIS IS ALMIGHTY COMMON LISP)"))
                (p "You can also use the "
                  (code "#") " reader macro.")
                (pre
                  (code :class "lisp" "(defparameter *almighty* #(this is almighty common lisp))
                                        ; => #(THIS IS ALMIGHTY COMMON LISP)"))))
            (section :id "accessing-data-in-a-vector"
              (hgroup
                (span)
                (h4 "Accessing data in a vector"))
              (div :class "outline-text-5" :id "text-orgc61d162"
                (p "Use ~aref= to access the data by index.")
                (pre
                  (code :class "lisp" "(aref *almighty* 0)
                                        ; => THIS
(aref *almighty* 4)
                                        ; => LISP
(aref *almighty* 5)
                                        ; => Error: Invalid index"))
                (p "This shows one of the first important differences between a list and an array in practical use. Using "
                  (code "nth") " to get an element in a list by its place in the list, if you go beyond the last element, "
                  (code "nth") " only returns "
                  (code "NIL") " without an error.")
                (pre
                  (code :class "lisp" "(defparameter *almighty-list* '(this is almighty common lisp))
(nth 4 *almighty-list*)
                                        ; => LISP
(nth 5 *almighty-list*)
                                        ; => NIL
(nth 99 *almighty-list*)
                                        ; => NIL"))))
            (section :id "modifying-data-in-a-vector"
              (hgroup
                (span)
                (h4 "Modifying data in a vector"))
              (div :class "outline-text-5" :id "text-org368af05"
                (p "Use "
                  (code "setf") " to modify the data at a place in a vector")
                (pre
                  (code :class "lisp" "(aref *almighty* 3)
                                        ; => COMMON
(setf (aref *almighty* 3) 'powerful)
                                        ; => POWERFUL
*almighty*
                                        ; => #(THIS IS ALMIGHTY POWERFUL LISP)"))))
            (section :id "adding-deleting-an-element-of-a-vector"
              (hgroup
                (span)
                (h4 "Adding/Deleting an element of a vector"))
              (div :class "outline-text-5" :id "text-org5d50dd6")
              (section :id "starting-with-an-adjustable-vector"
                (hgroup
                  (span)
                  (h5 "Starting with an adjustable vector"))
                (div :class "outline-text-6" :id "text-org94eb0cf"
                  (p "Deleting items from an array requires more manual work than with lists.")
                  (p "One method involves changing the size of the vector with ~adjust-array=, leaving no \"empty\" cells. This method relies on the array being created with the "
                    (code ":adjustable") " option set to "
                    (code "t") ", making it an ~adjustable array=.")
                  (pre
                    (code :class "lisp" "(defparameter *adjustably-almighty* (make-array 5 :initial-contents '(THIS IS ALMIGHTY COMMON LISP) :adjustable t))
(defun inefficiently-delete-vector-element (vector index)
  (when (< index (length vector))
    (loop for i from index below (1- (length vector))
          ;; Move all elements above the target index down one cell,
          do (setf (aref vector i) (aref vector (1+ i))))
    (adjust-array vector (1- (length vector))))  ; Resize the array.
  vector)

(inefficiently-delete-vector-element *adjustably-almighty* 3)
                                        ; => #(THIS IS ALMIGHTY LISP)"))))
              (section :id "starting-with-an-adjustable-vector-with-a-fill-pointer"
                (hgroup
                  (span)
                  (h5 "Starting with an adjustable vector with a fill pointer"))
                (div :class "outline-text-6" :id "text-orgf5babc9"
                  (p "Use "
                    (code "vector-pop") " to remove the last element of a vector that has a "
                    (code ":fill-pointer") " set. Use "
                    (code "vector-push") " to add an element to the end of the vector. It will not change the size/dimensions of the array.")
                  (pre
                    (code :class "lisp" "(defparameter *fill-pointer-almighty* (make-array 5 :initial-contents '(this is almighty common lisp) :fill-pointer 5))
(vector-pop *fill-pointer-almighty*)
                                        ; => LISP
(aref *fill-pointer-almighty* 4)
                                        ; => LISP
                                        ; The data is still there if we look.
(array-dimensions *fill-pointer-almighty*)
                                        ; => (5)
                                        ; The array hasn't changed physical size; memory is still used.
(print *fill-pointer-almighty*)
                                        ; #(THIS IS ALMIGHTY COMMON)  => #(THIS IS ALMIGHTY COMMON)
                                        ; But the \"active\" portion--the logical size--of the array has changed.
                                        ;
(vector-push 'lisp *fill-pointer-almighty*)
                                        ; => 4 (3 bits, #x4, #o4, #b100)
(print *fill-pointer-almighty*)

                                        ; #(THIS IS ALMIGHTY COMMON LISP)  => #(THIS IS ALMIGHTY COMMON LISP)"))
                  (p
                    (code "vector-push") " won't resize the array beyond the dimensions defined with "
                    (code "make-array") ". Use "
                    (code "vector-push-extend") " to resize the array when it's necessary.")
                  (pre
                    (code :class "lisp" "(vector-push 'dude *fill-pointer-almighty*)
                                        ; => NIL
(print *fill-pointer-almighty*)
                                        ; #(THIS IS ALMIGHTY COMMON LISP)  => #(THIS IS ALMIGHTY COMMON LISP)
(array-dimensions *fill-pointer-almighty*)
                                        ; => (5)
(vector-push-extend 'dude *fill-pointer-almighty*)
                                        ; => 5
(print *fill-pointer-almighty*)
                                        ; #(THIS IS ALMIGHTY COMMON LISP DUDE)  => #(THIS IS ALMIGHTY COMMON LISP DUDE)
(array-dimensions *fill-pointer-almighty*)
                                        ; => (10)"))))
              (section :id "starting-with-a-non-adjustable-vector"
                (hgroup
                  (span)
                  (h5 "Starting with a non-adjustable vector"))
                (div :class "outline-text-6" :id "text-orgc236ac8"
                  (p "Deleting an element in the middle of a non-adjustable vector requires shifting elements that come after it down one cell and then setting the last element in the array to "
                    (code "NIL") ".")
                  (pre
                    (code :class "lisp" "(defparameter *almighty* (make-array 5 :initial-contents '(THIS IS ALMIGHTY COMMON LISP)))
(defun delete-vector-element (vector index)
  (when (< index (length vector))
    (loop for i from index below (1- (length vector))
          do (setf (aref vector i) (aref vector (1+ i))))
    (setf (aref vector (1- (length vector))) nil))  ; Clear last element
  vector)

(delete-vector-element *almighty* 3)
                                        ; => #(THIS IS ALMIGHTY LISP NIL)
(delete-vector-element *almighty* 3)
                                        ; => #(THIS IS ALMIGHTY NIL NIL)"))
                  (p "This method leaves the array dimensions untouched, shifts all elements above the given index down one cell in the array, and leaves "
                    (code "NIL") " to fill the empty space."))))))
        (section :id "structures"
          (hgroup
            (span)
            (h2 "STRUCTURES"))
          (div :class "outline-text-3" :id "text-org5fd942f"
            (p ":ID:       859ccd29-fc5e-4178-9e55-6edba480744d"))
          (section :id "what-is-a-structure-"
            (hgroup
              (span)
              (h3 "What is a structure?"))
            (div :class "outline-text-4" :id "text-orgaaf5cd6"
              (p "Structures are user-defined data types that group related data. They are similar to classes, but are faster and more memory efficient than classes under some scenarios.")))
          (section :id "defining-structures-and-making-instances"
            (hgroup
              (span)
              (h3 "Defining structures and making instances"))
            (div :class "outline-text-4" :id "text-orgaabab85")
            (section :id "slots-w-default-values"
              (hgroup
                (span)
                (h4 "Slots w/default values"))
              (div :class "outline-text-5" :id "text-orgccfcf37"
                (p "Use "
                  (code "defstruct") " to define a structure. After giving the structure a name, define the slots. Use "
                  (code "make-<structure name>") " to make an instance of the structure. Assign slot values at instantiation using keywords.")
                (pre
                  (code :class "lisp" "(defstruct s-point
  x
  y)

(make-s-point)
                                        ; => #S(S-POINT :X NIL :Y NIL)

(defstruct http-request
  (status 200)
  (headers '(()))
  (body nil))
(make-http-request)
                                        ; => #S(HTTP-REQUEST :STATUS 200 :HEADERS (NIL) :BODY NIL)
(make-http-request :status 404 :body \"This page doesn't exist.\")
                                        ; => #S(HTTP-REQUEST :STATUS 404 :HEADERS (NIL) :BODY \"This page doesn't exist.\")"))
                (p "Slots default to "
                  (code "NIL") ", but you can define defaults as in the "
                  (code "http-request") " example. "
                  (code "status") " defaults to "
                  (code "200") ", "
                  (code "headers") " to "
                  (code "'(())") ", etc.")))
            (section :id "defining-required-slots"
              (hgroup
                (span)
                (h4 "Defining required slots"))
              (div :class "outline-text-5" :id "text-org1baa8e5"
                (p "If you define the "
                  (code ":constructor") " for the structure you can make slots required.")
                (pre
                  (code :class "lisp" "(defstruct (animal (:constructor raise-animal (name &optional territory)))
  (name \"\")
  (territory \"World\"))
(raise-animal)                          ; Error: invalid number of arguments: 0
(raise-animal \"Tiger\")
                                        ; => #S(ANIMAL :NAME \"Tiger\" :TERRITORY \"World\")"))))
            (section :id "defining-required-types"
              (hgroup
                (span)
                (h4 "Defining required types"))
              (div :class "outline-text-5" :id "text-orgc0e42e4"
                (p "You can also define types–checked at compile time–for slots.")
                (pre
                  (code :class "lisp" "(defstruct s-point
  (x 0.0 :type float)
  (y 0.0 :type float))

(make-s-point)
                                        ; => #S(S-POINT :X 0.0 :Y 0.0)
(make-s-point :x 5)
;; The value
;;   5
;; is not of type
;;   FLOAT
;; when setting slot X of structure S-POINT
;;    [Condition of type TYPE-ERROR]"))))
            (section :id "inheritance"
              (hgroup
                (span)
                (h4 "Inheritance"))
              (div :class "outline-text-5" :id "text-orgb9c4a83"
                (p "Structures support single inheritance.")
                (pre
                  (code :class "lisp" "(defstruct vehicle
  make
  model)

(defstruct (van (:include vehicle))
  (doors 4))

(make-van)
                                        ; => #S(VAN :MAKE NIL :MODEL NIL :DOORS 4)")))))
          (section :id "accessing-modifying-slots-in-a-structure-instance"
            (hgroup
              (span)
              (h3 "Accessing/Modifying slots in a structure instance"))
            (div :class "outline-text-4" :id "text-orgd1d1cfd"
              (p "To access slot values, pass the structure to the function called "
                (code "<structure-name>-<slot-name>") ", created when compiling the structure definition. Use "
                (code "setf") " to modify the value.")
              (pre
                (code :class "lisp" "(defstruct s-point
  (x 0.0 :type float)
  (y 0.0 :type float))
(defparameter *s-point* (make-s-point :x 5.0 :y 10.0))
(s-point-x *s-point*)
                                        ; => 5.0
(setf (s-point-x *s-point*) 10.0)
                                        ; => 10.0
(s-point-x *s-point*)
                                        ; => 10.0"))))
          (section :id "printing-structures"
            (hgroup
              (span)
              (h3 "Printing structures"))
            (div :class "outline-text-4" :id "text-org5d3f335"
              (p "Structures print in a somewhat human-readable form.")
              (pre
                (code :class "lisp" "(print *s-point*)
                                        ; => #S(S-POINT :X 10.0 :Y 10.0)"))
              (p "Use the "
                (code ":print-structure") " option to customize how it is printed.")
              (pre
                (code :class "lisp" "(defstruct (s-point (:print-function
                     (lambda (this stream depth)
                       (format stream \"~&X: ~a Y: ~a~%\" (s-point-x this) (s-point-y this)))))
  (x 0.0 :type float)
  (y 0.0 :type float))

(print (make-s-point :x 5.0 :y 10.0))
                                        ;   => X: 5.0 Y: 10.0"))
              (p "The "
                (code "depth") " argument in the lambda list is required. When necessary, it is used to determine how to print nested structures.")))
          (section :id "structures-vs-classes"
            (hgroup
              (span)
              (h3 "Structures Vs. Classes"))
            (div :class "outline-text-4" :id "text-org2d9cdac")
            (section :id "similarities"
              (hgroup
                (span)
                (h4 "Similarities"))
              (div :class "outline-text-5" :id "text-orgacb62a4"
                (p "Structures and classes are both user-defined data types, they both let you define groups of related data into slots, and those slots can include default values and other options.")
                (p
                  (code "Generic methods") " can be specialized on both structures and classes.")
                (pre
                  (code :class "lisp" "(defgeneric add (point-a point-b))

(defmethod add ((a s-point) (b s-point))
  (make-s-point :x (+ (s-point-x a) (s-point-x b))
                :y (+ (s-point-y a) (s-point-y b))))

(add *s-point* *s-point*)
                                        ; => X: 20.0 Y: 20.0
(defmethod add ((a c-point) (b c-point))
  (make-instance 'c-point
                 :x (+ (c-point-x a) (c-point-x b))
                 :y (+ (c-point-y a) (c-point-y b))))
(setf *c-point* (make-instance 'c-point :x 4.0 :y 7.5))

(add *c-point* *c-point*)
                                        ; => This is your object data: X is 8.0, Y is 15.0"))))
            (section :id "differences"
              (hgroup
                (span)
                (h4 "Differences"))
              (div :class "outline-text-5" :id "text-org779f689")
              (section :id "dynamic-redefinition"
                (hgroup
                  (span)
                  (h5 "Dynamic redefinition"))
                (div :class "outline-text-6" :id "text-org5b538cd"
                  (p "Similarities between structures and classes are superficial–the differences are much more significant.")
                  (p "Probably the most important difference between structures and classes in Lisp is that when you modify their definition, currently instantiated structures aren't updated with the new definition, but object instances are. This is called "
                    (code "dynamic redefinition") ".")
                  (p "Let's setup a structure and a class:")
                  (pre
                    (code :class "lisp" "(defstruct s-point
  x
  y)
(defparameter *s-point* (make-s-point :x 5 :y 10))
                                        ; => *S-POINT*
(s-point-x *s-point*)
                                        ; => 5 (3 bits, #x5, #o5, #b101)

(defclass c-point ()
  ((x :initarg :x
      :initform nil
      :accessor c-point-x)
   (y :initarg :y
      :initform nil
      :accessor c-point-y)))
(defparameter *c-point* (make-instance 'c-point :x 5 :y 10))

(c-point-x *c-point*)
                                        ; => 5 (3 bits, #x5, #o5, #b101)"))
                  (p "Now, what happens to "
                    (i "instances") " if we change their "
                    (i "definitions") "?")
                  (p "With the structure:")
                  (pre
                    (code :class "lisp" "(defstruct s-point
  id
  x
  y)

;; attempt to redefine the STRUCTURE-OBJECT class S-POINT
;; incompatibly with the current definition
;;    [Condition of type SIMPLE-ERROR]

;; Restarts:
;;  0: [CONTINUE] Use the new definition of S-POINT, invalidating already-loaded code and instances.
;;  1: [RECKLESSLY-CONTINUE] Use the new definition of S-POINT as if it were compatible, allowing old accessors to use new instances and allowing new accessors to use old instances.
;;  2: [ABORT] Abort compilation.
;;  3: [*ABORT] Return to SLY's top level.
;;  4: [ABORT] abort thread (#<THREAD tid=6447 \"slynk-worker\" RUNNING {70072FDD93}>)"))
                  (p "We need to overwrite the current definition and invalidate loaded code and "
                    (i "instances") ", or "
                    (code "RECKLESSLY-CONTINUE") ".")
                  (p "With the class:")
                  (pre
                    (code :class "lisp" "(defclass c-point ()
  ((id :initarg :id
       :initform 7
       :accessor id)
   (x :initarg :x
      :initform nil
      :accessor x)
   (y :initarg :y
      :initform nil
      :accessor y)))
                                        ; => #<STANDARD-CLASS COMMON-LISP-USER::C-POINT>
                                        ; Compiles without trouble.

(id *c-point*)
                                        ; => 7 (3 bits, #x7, #o7, #b111)
                                        ; The instance is given the default initform.

(defclass c-point ()
  ((x :initarg :x
      :initform nil
      :accessor x)
   (y :initarg :y
      :initform nil
      :accessor y)))

(id *c-point*)                          ; => Error, slot no longer exists.

(defclass c-point ()
  ((id :initarg :id
       :initform (random most-positive-fixnum)
       :accessor id)
   (x :initarg :x
      :initform nil
      :accessor x)
   (y :initarg :y
      :initform nil
      :accessor y)))

(id (make-instance 'c-point))
                                        ; => 4069218540621267255 (62 bits, #x3878C4B3FB2A0137)
(id *c-point*)
                                        ; => 3821555703091291730 (62 bits, #x3508E4AA1C1FE652)"))))
              (section :id "inheritance"
                (hgroup
                  (span)
                  (h5 "Inheritance"))
                (div :class "outline-text-6" :id "text-org0db5243"
                  (p "Both structures and classes support inheritance, but classes support "
                    (code "multiple inheritance") ".")))
              (section :id "printing"
                (hgroup
                  (span)
                  (h5 "Printing"))
                (div :class "outline-text-6" :id "text-org006b0b8"
                  (p "Structures can modify how "
                    (code "print") " will display data using "
                    (code ":print-function") " as above, but classes need to use the "
                    (code "print-object") " generic method.")
                  (pre
                    (code :class "lisp" "(defmethod print-object ((this c-point) stream)
  (format stream \"~&This is your object data: X is ~a, Y is ~a~%\" (c-point-x this) (c-point-y this)))
(print *c-point*)
                                        ;   => This is your object data: X is 5, Y is 10"))))
              (section :id "typing"
                (hgroup
                  (span)
                  (h5 "Typing"))
                (div :class "outline-text-6" :id "text-org81124b7"
                  (p "When you define slot types with structures, Lisp will check the type of value passed to it at compile time. Class slot type definitions aren't enforced–you need to enforce them yourself with "
                    (code "initialize-instance :before") ".")
                  (pre
                    (code :class "lisp" "(defclass c-point ()
  ((x :initarg :x
      :initform 0.0
      :accessor c-point-x
      :type float)                      ; added type
   (y :initarg :y
      :initform 0.0
      :accessor c-point-y
      :type float)))                    ; added type

(make-instance 'c-point)
                                        ; => This is your object data: X is 0.0, Y is 0.0
(make-instance 'c-point :x 5.0 :y 12.3)
                                        ; => This is your object data: X is 5.0, Y is 12.3
(make-instance 'c-point :x \"five dot zero\")
                                        ; => This is your object data: X is five dot zero, Y is 0.0

;; Check the types before initializing the instance.
(defmethod initialize-instance :before ((p c-point) &key x y)
  (unless (typep x 'float)
    (error 'type-error :datum x :expected-type 'float))
  (unless (typep y 'float)
    (error 'type-error :datum y :expected-type 'float)))

(handler-case
    (let ((p2 (make-instance 'c-point :x \"invalid\" :y 2.0)))
      (format t \"This won't print due to type error~%\"))
  (type-error (e)
    (format t \"Type error caught: ~a~%\" e)))
                                        ; Type error caught: The value
                                        ;                      \"invalid\"
                                        ;                    is not of type
                                        ;                      FLOAT
                                        ;  => NIL"))
                  (p "Type definitions in class slots should be considered \"documentation\"–you need to enforce the types yourself. With structure slots, types are enforced by the compiler."))))
            (section :id "which-one-should-you-use-"
              (hgroup
                (span)
                (h4 "Which one should you use?"))
              (div :class "outline-text-5" :id "text-org1759aff"
                (p "Generally, structures are going to be most useful for optimizing speed and memory efficiency. Classes are otherwise superior due to their more dynamic, flexible design. Unless you know for certain that you need an optimized data structure, stick with classes."))))))
      (section :id "errors-conditions"
        (hgroup
          (span)
          (h1 "ERRORS & CONDITIONS"))
        (div :class "outline-text-2" :id "text-orgeb3270e")
        (section :id "signaling-conditions"
          (hgroup
            (span)
            (h2 "Signaling Conditions"))
          (div :class "outline-text-3" :id "text-org2c9016a"
            (p "In Lisp, instead of \"throwing\" or \"returning\" an error, you \"signal a condition\". The most common way to signal some "
              (code "condition") " is to use "
              (code "error") " or "
              (code "cerror") ".")
            (pre
              (code :class "lisp" "(defun oops (x)
  (if (zerop x)
      (error \"What the h?\")))

(oops 0)"))
            (p "That opens the debugger:")
            (pre :class "example" :id "orga9c0fff" "What the h?
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [*ABORT] Return to SLY's top level.
 1: [ABORT] abort thread (#<THREAD tid=12411 \"slynk-worker\" RUNNING {7007185183}>)")
            (p "Notice that the "
              (code "condition") " is a "
              (code "SIMPLE-ERROR") ".")
            (p
              (code "cerror") " will create a "
              (i "continuable") " error.")
            (pre
              (code :class "lisp" "(defun oops (x)
  (if (zerop x)
      (cerror \"Want to continue?\" \"You passed =d\" x))
  (print \"You continued.\"))

(oops 0)"))
            (p "Returns:")
            (pre :class "example" :id "orgae00b78" "You passed 0
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Want to continue?
 1: [*ABORT] Return to SLY's top level.
 2: [ABORT] abort thread (#<THREAD tid=12483 \"slynk-worker\" RUNNING {700776ADA3}>)")
            (p "Continuing will continue after the point of the error.")
            (p "If you only want to make a warning, you can use "
              (code "warn") ".")
            (pre
              (code :class "lisp" "(progn
  (warn \"You are about to be Rick Rolled.\")
  (format t \"Never Gonna Give You Up\")
  (error \"You've been deserted.\")
  (format t \"I hate meta.\"))"))
            (p "If you execute this, it will print the warning and the first "
              (code "format") " in the REPL and then open a debugger with the error message. You won't see the second "
              (code "format") " message.")))
        (section :id "assertions"
          (hgroup
            (span)
            (h2 "Assertions"))
          (div :class "outline-text-3" :id "text-org1266cf8"
            (p "With ~assert=, you can check a condition. If it returns "
              (code "t") ", the program continues. If the condition returns "
              (code "nil") ", it will signal a continuable error and then you can assign a value to the selected symbols and retry the assert.")
            (pre
              (code :class "lisp" "(defun using-assertion (x)
  (print \"before assert\")
  (assert (> x 0)
          (x)
          \"=d Needs to be a positive number.\" x)
  (print \"after assert\")
  (print x))
(using-assertion -4)"))
            (p "Returns:")
            (pre :class "example" :id "org5355cd3" "Needs to be a positive number.
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Retry assertion with new value for X.
 1: [*ABORT] Return to SLY's top level.
 2: [ABORT] abort thread (#<THREAD tid=12435 \"slynk-worker\" RUNNING {7007A4FF33}>)")
            (p "If you continue, in the REPL, this is what you'll see:")
            (pre :class "example" :id "org9faa0f5" "\"before assert\"
The old value of X is -4.
Do you want to supply a new value?  (y or n)")
            (p "If you type "
              (code "y") " and "
              (code "Enter") ", then you will be prompted to enter a form to be evaluated.")
            (pre :class "example" :id "org40ff820" "Type a form to be evaluated:
[I type in 1]

\"after assert\"
1")))
        (section :id "conditions"
          (hgroup
            (span)
            (h2 "Conditions"))
          (div :class "outline-text-3" :id "text-orgca365cf"
            (p "If you need or want to provide more control over error handling, you can define your own conditions.")
            (pre
              (code :class "lisp" "(define-condition naughty-word (error)
  ()
  (:documentation \"A condition for when naughty words are detected.\"))"))
            (p "Conditions are "
              (code "objects") " using the CLOS.")
            (pre
              (code :class "lisp" "(make-condition 'naughty-word)
                                        ; => #<NAUGHTY-WORD {7009850503}>"))
            (p "Let's make a list of naughty words to detect and a function to detect them.")
            (pre
              (code :class "lisp" "(defparameter *naughty-words* '(rust java haskell cobol crypto bitcoin)
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
            (pre :class "example" :id "org4bb12fe" "Condition ALMIGHTY/KAIKEI/TESTS::NAUGHTY-WORD was signalled.
   [Condition of type NAUGHTY-WORD]

Restarts:
 0: [*ABORT] Return to SLY's top level.
 1: [ABORT] abort thread (#<THREAD tid=12475 \"slynk-worker\" RUNNING {70069EF533}>)")
            (p "We can improve our error messages by modifying the condition definition.")
            (pre
              (code :class "lisp" "(define-condition naughty-word (error)
  ((bad-word :initarg :bad-word :initform nil :reader bad-word))
  (:documentation \"A condition for when naughty words are detected.\"
   :report (lambda (c stream)
             (format stream \"Naughty bad-word detected: ~a\" (word c)))))"))
            (p "Then we update the call to "
              (code "error") ":")
            (pre
              (code :class "lisp" "(defun process-word (word)
  \"Signal a naughty-word condition if a word is naughty.\"
  (when (naughty-word-p word)
    (error 'naughty-word :word word))
  word)"))
            (p "Run "
              (code "process-sentence") " again and your debugger will display the following message:")
            (pre :class "example" :id "orgac3aabc" "Naughty word detected: JAVA
   [Condition of type NAUGHTY-WORD]

Restarts:
 0: [*ABORT] Return to SLY's top level.
 1: [ABORT] abort thread (#<THREAD tid=12487 \"slynk-worker\" RUNNING {7007CA75C3}>)

Backtrace:
 0: (PROCESS-WORD JAVA)
 1: (PROCESS-SENTENCE (JAVA HAS THE BEST OOP SYSTEM ...))")))
        (section :id "restarts"
          (hgroup
            (span)
            (h2 "Restarts"))
          (div :class "outline-text-3" :id "text-orga89b573"
            (p "If you define a condition, it's because you want to provide the ability to recover from some error. One recovery mechanism you can provide are "
              (code "restarts") ". To provide more restarts, use "
              (code "restart-case") ".")
            (pre
              (code :class "lisp" "(defun replace-word (word)
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
              (code "process-sentence") ", you can replace any of the words found in the "
              (code "*naughty-words*") " list using a restart.")))
        (section :id "handlers"
          (hgroup
            (span)
            (h2 "Handlers"))
          (div :class "outline-text-3" :id "text-org9d2772d"
            (p "Another recovery mechanism is via \"handling\". When the "
              (code "naughty-word") " condition is signaled, you can prevent the program from stopping and the debugger from opening by using either "
              (code "handler-case") ".")
            (pre
              (code :class "lisp" "(defun process-sentence (sentence)
  \"Look through all the words in a sentence and do something if any of them are
naughty.\"
  (loop :for word :in sentence
        :collect (handler-case (process-word word)
                   (naughty-word () '***))))

(process-sentence '(java has the best oop system among all programming languages))"))
            (p "Now, instead of bringing up the debugger, if a naughty word is encountered during the iteration, it will simply be replaced by \""
              (b "*") "\".")))
        (section :id "-break-on-signals-"
          (hgroup
            (span)
            (h2
              (code "*break-on-signals*")))
          (div :class "outline-text-3" :id "text-orgffd260d"
            (p "The "
              (code "*break-on-signals*") " is set to "
              (code "nil") " by default, but when set to some condition or conditions, will break just before the condition is signaled. It's useful for getting a look at the state of the program just before a condition is signaled, especially when you don't know exactly where the error is coming from (in which case, you could use Sly stickers + breaking as I wrote about earlier in "
              (a :href "#org602ef47" "THE LISP IDE/STICKERS") ")."))))
      (section :id "macros"
        (hgroup
          (span)
          (h1 "MACROS"))
        (div :class "outline-text-2" :id "text-orge2102c4")
        (section :id "the-forbidden-fruit"
          (hgroup
            (span)
            (h2 "The Forbidden Fruit"))
          (div :class "outline-text-3" :id "text-org1f6d7c5"
            (p "Macros are a controversial feature of Lisp. On the one hand, they are one of the main reasons why Lisp is so powerful and flexible. They are a defining feature, enabling a masterful macro writer to bend the language to his will. There are no limits; you can rewrite the language in your own image.")
            (p "It's such a compelling feature that I use it to advertise this book.")
            (p "On the other hand, many experienced users of Lisp warn against the use of macros. While some Lisp advocates strongly encourage the use of macros, Peter Norvig in Paradigms of Artificial Intelligence Programming writes:")
            (blockquote
              (p "The first step in writing a macro is to recognize that every time you write one, you are defining a new language …. Introducing a macro puts much more memory strain on the reader of your program than does introducing a function, variable or data type, so it should not be taken lightly. Introduce macros only when there is a clear need, and when the macro fits in well with your existing system."))
            (p "In other words, macros are powerful and require experience to use properly. In fact, they require experience just to understand their value and power. Many Lisp-skeptics question whether their benefits are worth the cost of using a niche language like Lisp over languages with more energetic ecosystems.")
            (p "Lisp advocates find it challenging to convince skeptics to give Lisp a serious look because demonstrating the value of macros for common programming tasks is honestly not possible, and it's easy to imagine the nightmare of domain specific languages all over the place written by people unqualified to write more than a CRUD app (like me). Macros are something that need to be experienced to be understood–similar to Lisp's image-based programming model. Unfortunately, \"You have to try it to understand\" sounds a lot like cope from delusional nerds advocating for solutions without recognizing the weaknesses of their solutions (something nerds are wont to do).")
            (p "However, Lisp-curious programmers are often attracted to the "
              (i "platonic ideal") " of macros. Its the One Ring, the fruit when eaten that grants forbidden knowledge that they hope will give them an edge over the programmers using lesser languages. When faced with a problem, macros are the first thing that these people (maybe even you, my smart and handsome customer) want to try to use to solve the problem. The warnings of the graybeards make macros ever more tantalizing, and the macro-skeptics create an opponent in the mind of the macro-maximalist who sees them as the losers they will crush when they finally unlock the true potential of meta programming. ")
            (p "If you're reading this, you are likely among those curious to learn more about macros in particular. Because macros are such a deep subject, even if I were a macro wizard (which I am not), I couldn't provide a tutorial expansive and worthy enough to be called complete. Here in this chapter I can only teach the essential things to know about macros. See the RESOURCES chapter to get a list of resources for learning more about macros and more.")))
        (section :id "some-demonstrations"
          (hgroup
            (span)
            (h2 "Some Demonstrations"))
          (div :class "outline-text-3" :id "text-orgd32ac6c"
            (p "First, I think it's useful to see what macros can do."))
          (section :id "the-hsx-macro"
            (hgroup
              (span)
              (h3 "The "
                (code "hsx") " macro"))
            (div :class "outline-text-4" :id "text-orgf448e43"
              (pre
                (code :class "lisp" "(ql:quickload \"hsx\")
(in-package :hsx)
(hsx
 (div :id \"card-container\"
   (a :href \"/path/to/book-1\"
     (div :class \"card\"
       (h2 :class \"card-title\" \"Book 1\")
       (p :class \"card-body\" \"This is book 1 of the series of books.\")))))
; =>
; <div id=\"card-container\">
;   <a href=\"/path/to/book-1\">
;     <div class=\"card\">
;       <h2 class=\"card-title\">Book 1</h2>
;       <p class=\"card-body\">This is book 1 of the series of books.</p>
;     </div>
;   </a>
; </div>"))
              (p "This is the "
                (code "hsx") " macro from the "
                (code "hsx") " library. It produces HTML.")))
          (section :id "the-sxql-macro"
            (hgroup
              (span)
              (h3 "The "
                (code "sxql") " macro"))
            (div :class "outline-text-4" :id "text-org5846e9f"
              (pre
                (code :class "lisp" "(ql:quickload \"sxql\")
(in-package :sxql)
(yield (select :*
         (from :users)
         (where (:= :age 30))))
 ; => \"SELECT * FROM users WHERE (age = ?)\", (30)"))
              (p "This is a SQL query produced with the "
                (code "sxql") " library using several macros.")))
          (section :id "the-coalton-language"
            (hgroup
              (span)
              (h3 "The Coalton language"))
            (div :class "outline-text-4" :id "text-org34d7860"
              (pre
                (code :class "lisp" "(declare create-account (AccountName -> Balance -> BankM (BankResult Account)))
(define (create-account name initial-balance)
  \"Adds an account to the BankState and return the created account.\"
  (run-resultT
   (do
    (accounts <- (lift (lift get)))
    (ResultT
     (match (get-account name accounts)
       ((Err _) (pure (Ok Unit)))
       ((Ok _) (pure (Err (AccountAlreadyExists name))))))
     (let unvalidated-account = (Account name initial-balance))
     (account <- (ResultT (check-account-is-valid unvalidated-account)))
     (ResultT (set-account account)))))"))
              (p "This is "
                (a :href "https://github.com/coalton-lang/coalton/blob/main/examples/small-coalton-programs/src/monads-bank.lisp" "example code") " for "
                (a :href "https://www.youtube.com/watch?v=of92m4XNgrM" "Coalton") ", a new programming language built on top of Common Lisp. It's \"just a macro\".")))
          (section :id "code-that-writes-code"
            (hgroup
              (span)
              (h3 "Code That Writes Code"))
            (div :class "outline-text-4" :id "text-org94ac892"
              (p "What we see above is that macros are code that is capable of either generating, or fully implementing, entirely other languages. Of course, they can produce Lisp code, as the "
                (code "defun") " and "
                (code "loop") " macros do."))))
        (section :id "understanding-macros"
          (hgroup
            (span)
            (h2 "Understanding Macros"))
          (div :class "outline-text-3" :id "text-org9c16e48"
            (p "Before I show how to write macros, it will be useful to know a little about how macros work."))
          (section :id "compilation-before-evaluation"
            (hgroup
              (span)
              (h3 "Compilation Before Evaluation"))
            (div :class "outline-text-4" :id "text-org5a0d978"
              (p "The most important thing to know about macros is that they are compiled or expanded before they are evaluated. Because of this expansion step, macros are the only tool programmers can use to modify the evaluation of the code it expands into.")
              (p "In Lisp, the first symbol in a list is evaluated as a function name, and the rest of the values are evaluated from left to right. Before the function name is looked up, the parameters are evaluated.")
              (pre
                (code :class "lisp" "(hello my name is \"Micah\")"))
              (p "The error you get here is about a non-existent "
                (code "my") " variable--"
                (code "hello") " will be evaluated after the arguments, and "
                (code "my") " is the first argument.")
              (p "But what about "
                (code "defun") "? The whole point is that the first symbol "
                (i "is suppose to be bound to the definition that follows") ".")
              (pre
                (code :class "lisp" "(defun hello (name)
  (print name))"))
              (p
                (code "hello") " isn't evaluated as a symbol containing a value–it's used as the name for the code that follows.")
              (p "And that's how we know "
                (code "defun") " is a macro.")
              (p "We can even view what the "
                (code "defun") " macro expands to right in Emacs. With the cursor over the part that says "
                (code "defun hello") ", type "
                (code "SPC m m") " ("
                (code "macrostep-expand") "). If you are using SBCL as I suggested at the beginning of the book, you should see something like this:")
              (pre
                (code :class "lisp" "(progn
 (eval-when (:compile-toplevel) (sb-c:%compiler-defun 'hello t nil nil))
 (sb-impl::%defun 'hello
                  (sb-int:named-lambda hello
                      (name)
                    (block hello (print name)))))"))
              (p "Because macros can expand into more macros, you may be able to expand more macros after the first expansion.")
              (pre
                (code :class "lisp" "(defun hello (names)
  (loop :for name :in names
        :do (format t \"Hello, ~a.~&\" name)))
;; expands into
(progn
 (eval-when (:compile-toplevel) (sb-c:%compiler-defun 'hello t nil nil))
 (sb-impl::%defun 'hello
                  (sb-int:named-lambda hello
                      (names)
                    (block hello
                      (loop :for name :in names
                            :do (format t \"Hello, ~a.~&\" name))))))
;; which further expands into... 
(progn
 (eval-when (:compile-toplevel) (sb-c:%compiler-defun 'hello t nil nil))
 (sb-impl::%defun 'hello
                  (sb-int:named-lambda hello
                      (names)
                    (block hello
                      (block nil
                        (let ((name nil)
                              (#:l548
                               (sb-kernel:the* (list :use-annotations t :source-form names) names)))
                          (declare (ignorable #:l548)
                                   (ignorable name))
                          (tagbody
                           sb-loop::next-loop
                            (when (endp #:l548) (go sb-loop::end-loop))
                            (sb-loop::loop-desetq name (car #:l548))
                            (sb-loop::loop-desetq #:l548 (cdr #:l548))
                            (format t \"Hello, ~a.~&\" name)
                            (go sb-loop::next-loop)
                           sb-loop::end-loop)))))))
;; and further...
(progn
 (eval-when (:compile-toplevel) (sb-c:%compiler-defun 'hello t nil nil))
 (sb-impl::%defun 'hello
                  (sb-int:named-lambda hello
                      (names)
                    (block hello
                      (block nil
                        (let ((name nil)
                              (#:l551
                               (sb-kernel:the* (list :use-annotations t :source-form names) names)))
                          (declare (ignorable #:l551)
                                   (ignorable name))
                          (tagbody
                           sb-loop::next-loop
                            (if (endp #:l551)
                                (go sb-loop::end-loop))
                            (setq name (car #:l551))
                            (setq #:l551 (cdr #:l551))
                            (format t \"Hello, ~a.~&\" name)
                            (go sb-loop::next-loop)
                           sb-loop::end-loop)))))))"))
              (p "After calling "
                (code "macroexpand-step") " the first time, you can expand one more step with "
                (code "e") ".")
              (p "You can undo one expansion at a time with "
                (code "u") " or completely revert all expansions everywhere with "
                (code "q") "."))))
        (section :id "defining-macros"
          (hgroup
            (span)
            (h2 "Defining Macros"))
          (div :class "outline-text-3" :id "text-org6a72e7e"
            (p "As you can see, macros significantly reduce the amount of visual noise in the code and make it easier to understand. But after looking at the code it expands into, you might wonder how the heck you even get started writing a macro at all?")
            (p "To begin defining a macro, first you need to do some wishful thinking.")
            (ul :class "org-ul"
              (li "Write the desired call.")
              (li "Write the desired expansion.")
              (li "Construct macro parameter list from call."))
            (p "For example, let's say you want to define a "
              (code "when-let") " macro that combines "
              (code "let") " and "
              (code "when") " into one form. First, you begin with the macro call you want to have:")
            (pre
              (code :class "lisp" "(when-let (user (get-user id))
  (welcome-message user)
  (redirect-to-dashboard user))"))
            (p "Then, you write the code you want it to expand into:")
            (pre
              (code :class "lisp" "(let ((user (get-user id)))
  (when user
    (welcome-message user)
    (redirect-to-dashboard user)))"))
            (p "Finally, you write the macro definition.")
            (pre
              (code :class "lisp" "(defmacro when-let ((var expression) &body body)
  `(let ((,var ,expression))
     (when ,var
       ,@body)))"))
            (p "Before we go further, first you need to understand backquotes."))
          (section :id "backquotes"
            (hgroup
              (span)
              (h3 "Backquotes"))
            (div :class "outline-text-4" :id "text-org0fdbdbf"
              (p "In the previous code, you saw a backquote in the form "
                (code "`(let ...)") ". Backquotes are like normal single-quotes–they are an abbreviation of "
                (code "quote") ", which returns the literal representation of some code.")
              (pre
                (code :class "lisp" "(quote (+ 2 2))
                                        ; => (+ 2 2)
'(+ 2 2)
                                        ; => (+ 2 2)
`(+ 2 2)
                                        ; => (+ 2 2)"))
              (p "Notice that those symbols aren't interpreted. They and the list they are included in are returned as-is.")
              (p "Backquotes have an important feature: you can tell the interpreter to actually evaluate some of parts of the code."))
            (section :id "comma-in-backquotes"
              (hgroup
                (span)
                (h4 "Comma In Backquotes"))
              (div :class "outline-text-5" :id "text-org956a344"
                (p "To tell the interpreter to evaluate parts inside a backquoted form, type "
                  (code ",") " before the part you want evaluated.")
                (pre
                  (code :class "lisp" "(let ((a 2)
      (b 3))
  `((+ ,a ,b) is ,(+ a b)))
                                        ; => ((+ 2 3) IS 5)"))
                (p "There are some rules you have to follow when using backquotes:")
                (ul :class "org-ul"
                  (li "A comma must follow a backquote.")
                  (li "A comma can't follow another comma."))
                (p "For example:")
                (pre
                  (code :class "lisp" "(let ((a 2)
      (b 3))
  ;; Added commas    v  v
  `((+ ,a ,b) is ,(+ ,a ,b)))
;; Error:
;; Comma not inside a backquote.

;;   Stream: #<dynamic-extent STRING-INPUT-STREAM (unavailable) from \"(let ((a...\">
;;    [Condition of type SB-INT:SIMPLE-READER-ERROR]"))
                (p "If we add the commas to the second "
                  (code "a") " and "
                  (code "b") ", since the outer "
                  (code "(+ ...)") " form is already preceded with a comma, we get that error.")))
            (section :id "comma-at-sign-in-backquotes"
              (hgroup
                (span)
                (h4 "Comma At-Sign In Backquotes"))
              (div :class "outline-text-5" :id "text-org4513bc1"
                (p "In the "
                  (code "when-let") " macro, we also saw "
                  (code ",@body") ". The "
                  (code ",@") " \"splices\" the form into the backquoted code–removing the outermost parentheses.")
                (pre
                  (code :class "lisp" "(let ((a 5)
      (b '(1 2 3))
      (c 9))
  `(+ ,a ,@b ,c))
                                        ; => (+ 5 1 2 3 9)"))
                (p
                  (code ",@") " is commonly used with macros that take an arbitrary number of arguments and passing those arguments along to another form that also takes an arbitrary number of arguments. That's what the "
                  (code "body") " parameter for "
                  (code "when-let") " is."))))
          (section :id "back-to-when-let-"
            (hgroup
              (span)
              (h3 "Back to "
                (code "when-let")))
            (div :class "outline-text-4" :id "text-orgf5cbac6"
              (p "Getting back to constructing a macro, after having the call and expansion prepared, you need to look at the parts that are common between them, and then make those parameters in the "
                (code "defmacro") " lambda-list.")
              (p "For example, look at "
                (code "user") ". It's in the expansion code three times:")
              (pre
                (code :class "lisp" "(let ((user ...))
 (... user
   (... user))"))
              (p "and in the macro call twice:")
              (pre
                (code :class "lisp" "(when-let (user ...)
  (... user))"))
              (p "Which means a parameter in the "
                (code "when-let") " definition needs to correspond to this variable.")
              (pre
                (code :class "lisp" "(defmacro when-let ((var ...)...)
  ...)"))
              (p "…and any instance of "
                (code "var") " in the body of the macro definition needs to be preceded with a comma.")
              (pre
                (code :class "lisp" "(defmacro when-let ((var ...)...)
  `(let ((,var ...))
     (when ,var
       ...)))"))
              (p "The same goes for "
                (code "(get-user id)") ". It appears once in the expansion and once in the macro call.")
              (pre
                (code :class "lisp" "(let ((... (get-user id)))
 (... ...
   (... ...)))

(when-let (... (get-user id))
  ...)"))
              (p "So it gets a spot in the parameter list of the macro:")
              (pre
                (code :class "lisp" "(defmacro when-let ((... expression) ...)
  ...)"))
              (p "And where "
                (code "expression") " appears in the macro body, it's preceded with a comma.")
              (pre
                (code :class "lisp" "(defmacro when-let ((... expression) ...)
  `(let ((... ,expression))
     ...))"))
              (p "The calls to "
                (code "(welcome-message user)") " and "
                (code "(redirect-to-dashboard user)") " are actually in a space where we expect any arbitrary number of arbitrary forms. For that, we use a "
                (code "&body") " parameter and splice it in with "
                (code ",@") ".")
              (pre
                (code :class "lisp" "(defmacro when-let ((... ...) &body body)
  `(let ((... ...))
     (when ...
       ,@body)))"))
              (p "Altogether:")
              (pre
                (code :class "lisp" "(defmacro when-let ((var expression) &body body)
  `(let ((,var ,expression))
     (when ,var
       ,@body)))"))))
          (section :id "destructuring-arbitrary-list-structures-in-parameters"
            (hgroup
              (span)
              (h3 "Destructuring Arbitrary List Structures In Parameters"))
            (div :class "outline-text-4" :id "text-orgeca5b0e"
              (p "In the example of "
                (code "when-let") ", we saw something strange in the lambda-list:")
              (pre
                (code :class "lisp" ";;                  v              v
(defmacro when-let ((var expression) &body body)
  ...)"))
              (p "What's going on there?")
              (p "Macros can take any list structure as a parameter and destructure them–binding parameters to elements within the list passed to the macro. To understand this characteristic of macro parameters, first we need to understand destructuring, represented best by "
                (code "destructuring-bind") ".")
              (p
                (code "destructuring-bind") " takes an arbitrary list structure and binds the elements of the list to local variables.")
              (pre
                (code :class "lisp" "(let ((people '((\"Micah\" 40 (\"Lisp\" \"Hiking\" \"Photography\"))
                (\"Guy\" 71 (\"Engineering\" \"Frogs\" \"Education\"))
                (\"Joe\" 50 (\"Games\" \"Civilization\" \"Finding Good Women\")))))
  (dolist (person people)
    (destructuring-bind (name age (interest1 interest2 interest3)) person
      (print interest1))))
                                        ; \"Lisp\" 
                                        ; \"Engineering\" 
                                        ; \"Games\"  => NIL"))
              (p "The first argument to "
                (code "destructuring-bind") " is a lambda-list of the variables to bind. The key is that it can bind any arbitrary element in any arbitrary list structure directly. All you need to do is mimic the structure of the list in the parameter list to "
                (code "destructuring-bind") ".")
              (pre
                (code :class "lisp" "(let ((tree '(1 (2 (3 4) 5 ((6 ((7))))))))
  (destructuring-bind (a (b (c d) e ((f ((g)))))) tree
    (format t \"~a + ~a = ~a\" d f (+ d f))))
                                        ; 4 + 6 = 10 => NIL"))
              (p "Macros can similarly destructure lists in their parameters list:")
              (pre
                (code :class "lisp" "(defmacro add-elements ((a (b (c))))
  (+ a b c))

(add-elements (1 (2 (3))))
                                        ; => 6 (3 bits, #x6, #o6, #b110)"))
              (p "It's important to remember that we didn't call "
                (code "(add-elements '(1 (2 (3)))") " or "
                (code "(add-elements (list 1 (2 (3)))") ": those are both single elements. With a function, this would be an invalid call because the 3 (the innermost element) would be interpreted as a function-name, the Lisp evaluator would look for it, and not find it.")
              (p "However, macros have the previous macro expansion/compilation step, so the form "
                (code "(1 (2 (3)))") " is valid: the macro would be expanded "
                (i "before") " the elements are evaluated."))))
        (section :id "redefining-macros"
          (hgroup
            (span)
            (h2 "Redefining Macros"))
          (div :class "outline-text-3" :id "text-orgf819b25"
            (p "One thing to keep in mind with macros is that if you make a macro, use the macro to write some code and compile that code, if you redefine the macro, you also need to "
              (i "recompile the other code") ". Otherwise, that code is using the old macro expansion and not the new one.")))
        (section :id "determining-when-to-use-macros"
          (hgroup
            (span)
            (h2 "Determining When To Use Macros"))
          (div :class "outline-text-3" :id "text-orgeee55c2"
            (p "As I said before, the general rule is to be conservative with your creation of macros. But there "
              (i "are") " times when you just gotta use the facilities that a macro provides."))
          (section :id "transformation"
            (hgroup
              (span)
              (h3 "Transformation"))
            (div :class "outline-text-4" :id "text-org0733c8c"
              (p "Macros allow you to expand into different forms based on the arguments you send to it--"
                (i "before the arguments are evaluated") ". Consider the macro "
                (code "setf") ".")
              (pre
                (code :class "lisp" "(defvar *lang*)
(defstruct vehicle
  make
  model
  color)
(defclass person ()
  ((name :initarg :name :initform nil :accessor person-name)
   (age :initarg :age :initform nil :accessor person-age)))
(defparameter *cruiser* (make-vehicle :make \"Toyota\" :model \"Land Cruiser 70\" :color \"Beige\"))
(defparameter *micah* (make-instance 'person :name \"Micah\" :age 40))
(defparameter *list* '(1 2 3))

(setf *lang* \"Japanese\")
(setf (car *list*) 11)
(setf (vehicle-color *cruiser*) \"White\")
(setf (person-age *micah*) 25)"))
              (p "The calls to "
                (code "setf") " expand to:")
              (pre
                (code :class "lisp" "(setq *lang* \"Japanese\")
(sb-kernel:%rplaca *list* 11)
(let* ((#:*cruiser*575 *cruiser*) (#:new574 \"White\"))
  (funcall #'(setf vehicle-color) #:new574 #:*cruiser*575))
(let* ((#:*micah*579 *micah*) (#:new578 25))
  (funcall #'(setf person-age) #:new578 #:*micah*579))"))
              (p "The "
                (code "setf") " macro looks at the arguments it receives and decides how to expand based on what it gets. If it sees "
                (code "(car ...)") " then it uses "
                (code "rplaca") ", if it sees another function like "
                (code "(person-age ...)") " then it calls a generic function "
                (code "setf") " with the "
                (code "PERSON-AGE") " setter sent to it.")
              (p "It does this without evaluating the expressions passed to it–it only knows the literal representations of the forms. The code is the data it works with. If you need to transform code based on the shape of its inputs, you might need a macro.")))
          (section :id "binding"
            (hgroup
              (span)
              (h3 "Binding"))
            (div :class "outline-text-4" :id "text-org5ca02f1"
              (p "As I said before, symbols aren't evaluated at expansion/compilation time, so if you want to bind symbols to values, macros are probably what you need. That's why "
                (code "defun") ", etc. are macros.")))
          (section :id "conditional-evaluation"
            (hgroup
              (span)
              (h3 "Conditional Evaluation"))
            (div :class "outline-text-4" :id "text-orgea6db82"
              (p "Because macros can transform code and control the evaluation of code, when you need to control the order of evaluation or whether a form gets evaluated at all, you might need a macro. That's why "
                (code "if") ", "
                (code "when") ", "
                (code "unless") ", etc. conditional forms are macros.")))
          (section :id "wrapping-an-environment"
            (hgroup
              (span)
              (h3 "Wrapping An Environment"))
            (div :class "outline-text-4" :id "text-orgc3331d0"
              (p "When you want to create a lexical environment like the "
                (code "with-") " family of macros do, then you probably want a macro."))))
        (section :id "determining-when-not-to-use-macros"
          (hgroup
            (span)
            (h2 "Determining When Not To Use Macros"))
          (div :class "outline-text-3" :id "text-orgeed7d93"
            (p "There are certain situations under which you absolutely can't use macros. Macros can't be used as data, in the same way functions can.")
            (pre
              (code :class "lisp" "(mapcar #'my-macro data) ; <- Error"))
            (p "Only functions can be passed to higher-order functions like "
              (code "mapcar") ", "
              (code "reduce") ", "
              (code "remove-if-not") ", etc.")
            (p "And of course, always keep in mind that if you are trying to develop a DSL, you might be overcomplicating things. If you want to collaborate with others, and you want to write DSLs, you better make them "
              (i "good") ".")))
        (section :id "variable-capture-hygiene"
          (hgroup
            (span)
            (h2 "Variable Capture & Hygiene"))
          (div :class "outline-text-3" :id "text-org1be8a10"
            (p "Macros can be tough to debug. That's partially because macros are generally harder to read because you have to keep the "
              (i "expansion") " code and "
              (i "expander") " code in your head at the same time.")
            (p "But macros are also more difficult to debug because they are subject to a unique class of bugs called variable capture. The subject of macro hygiene is primarily concerned with avoiding this class of bugs."))
          (section :id "examples-of-macros-vulnerable-to-variable-capture"
            (hgroup
              (span)
              (h3 "Examples Of Macros Vulnerable To Variable Capture"))
            (section :id "free-variables-same-binding-context"
              (hgroup
                (span)
                (h4 "Free Variables & Same Binding Context"))
              (div :class "outline-text-5" :id "text-org78908dc"
                (p "The simplest example of a macro that's vulnerable to variable capture is a macro that returns a free variable.")
                (pre
                  (code :class "lisp" "(defmacro might-get-captured1 ()
  '(+ x 1))"))
                (p "Why is this macro vulnerable? Because it doesn't return "
                  (code "'(+ x 1)") ". It "
                  (i "expands into")
                  (code "(+ x 1)") ", which is then evaluated at runtime.")
                (pre
                  (code :class "lisp" "(might-get-captured1) ; => Error: The variable X is unbound.
(let ((x 1))
  (might-get-captured1))
                                        ; => 2 (2 bits, #x2, #o2, #b10)"))
                (p "Try expanding the code with "
                  (code "SPC m m") " ("
                  (code "macrostep-expand") ") to verify.")
                (p "Compare to a function:")
                (pre
                  (code :class "lisp" "(defun cant-capture-me ()
  '(+ x 1))
(cant-capture-me)
                                        ; => (+ X 1)"))
                (p "So it's important to remember that quotes/backquotes in functions return literal representations, whereas macros use quotes/backquotes as templates for expanding into code-to-be-evaluated.")
                (p "In addition to macros with free variables, macros that bind multiple variables within one "
                  (code "let") " are also vulnerable to capture. Consider the following macro:")
                (pre
                  (code :class "lisp" "(defmacro might-get-captured2 (variable)
  `(let ((x 1)
         (,variable 0))
     (values x ,variable)))"))
                (p "If we call it like this:")
                (pre
                  (code :class "lisp" "(let ((y 5))
  (might-get-captured2 y))"))
                (p "What do you expect the return values to be?")
                (pre
                  (code :class "lisp" "(let ((y 5))
  (might-get-captured2 y))
                                        ; => 1, 0
;; Expanded
(let ((y 5))
  (let ((x 1) (y 0))
    (values x y)))"))
                (p "Or what about this wombo combo:")
                (pre
                  (code :class "lisp" "(let ((x 5))
  (might-get-captured2 x))"))
                (p "What is the return value? It's an error because "
                  (code "x") " occurs twice in the expanded "
                  (code "let") " form.")
                (pre
                  (code :class "lisp" ";; Expanded
(let ((x 5))
  (let ((x 1) (x 0))
    (values x x)))"))))
            (section :id "body-forms-containing-variables-in-macro"
              (hgroup
                (span)
                (h4 "Body Forms Containing Variables In Macro"))
              (div :class "outline-text-5" :id "text-orga51d24a"
                (p "You don't know what symbols could be in a "
                  (code "&body") " parameter of a macro. A symbol used in the macro may get used in the "
                  (code "&body") " as well.")
                (pre
                  (code :class "lisp" "(defmacro might-get-captured3 (&body body)
  `(let ((x 5))
     (+ x ,@body)))"))
                (p "Because the "
                  (code "body") " expands into an environment where "
                  (code "x") " is bound to the value 5, if the symbol "
                  (code "x") " appears in the "
                  (code "body") ", it will likely be overwritten in rebound by the macro's "
                  (code "leg") " context.")
                (pre
                  (code :class "lisp" "(let ((x 70)
      (y 50))
  (might-get-captured3
    (+ x y)))
                                        ; => 60 (6 bits, #x3C, #o74, #b111100)")))))
          (section :id "avoiding-variable-capture"
            (hgroup
              (span)
              (h3 "Avoiding Variable Capture"))
            (div :class "outline-text-4" :id "text-orgdb125eb"
              (p "These are general coding patterns that help you avoid variable capture."))
            (section :id "better-names-for-global-variables"
              (hgroup
                (span)
                (h4 "Better Names For Global Variables"))
              (div :class "outline-text-5" :id "text-orgc8e56d7"
                (p "Make sure you name global variables using the "
                  (code "*...*") " convention.")))
            (section :id "assign-parameters-to-local-variables"
              (hgroup
                (span)
                (h4 "Assign Parameters To Local Variables"))
              (div :class "outline-text-5" :id "text-org2484116"
                (p "Instead of using parameters directly…")
                (pre
                  (code :class "lisp" ";; Vulnerable
(defmacro comes-before-p (x y sequence)
  `(let ((sequence ,sequence))
     (< (position ,x sequence)
        (position ,y sequence))))

(comes-before-p 1 5 '(1 2 3 4 5))
 ; => T"))
                (p "…assign parameters to local variables before using them.")
                (pre
                  (code :class "lisp" "(defmacro comes-before-p (x y sequence)
  `(let ((sequence ,sequence)
         (xvar ,x)                         ; different variable names
         (yvar ,y))                        ; different variable names
     (< (position xvar sequence)
        (position yvar sequence))))"))))
            (section :id "define-macros-in-separate-packages"
              (hgroup
                (span)
                (h4 "Define Macros In Separate Packages"))
              (div :class "outline-text-5" :id "text-org48cd5d0"
                (p "To some extent, simply following the more modern One-Package-Per-File convention for factoring projects can also prevent some instances of variable capture. If you have "
                  (code "my-macro") " with symbol "
                  (code "x") " inside it, if someone uses "
                  (code "my-macro") " in their own package with an "
                  (code "x") " symbol, they will actually be two different symbols: "
                  (code "my-macro-package::x") " and "
                  (code "their-package::x") ".")))
            (section :id "use-gensym-"
              (hgroup
                (span)
                (h4 "Use "
                  (code "gensym")))
              (div :class "outline-text-5" :id "text-org49e5678"
                (p "Even if you give variables different names, and even if you give them names that others are unlikely to use, you have no guarantees that they will "
                  (i "never") " be used by users of your macro.")
                (p "Instead, you can create "
                  (i "anonymous symbols") " with "
                  (code "gensym") ".")
                (pre
                  (code :class "lisp" "(gensym)
                                        ; => #:G586"))
                (p "Every time you call "
                  (code "gensym") ", it will generate a symbol with a unique name. ")
                (p "Above, we expanded several different calls to "
                  (code "setf") " to show how it transforms based on the data it's passed. When we passed it "
                  (code "(vehicle-color *cruiser*)") ", it expanded into this:")
                (pre
                  (code :class "lisp" "(let* ((#:*cruiser*575 *cruiser*) (#:new574 \"White\"))
  (funcall #'(setf vehicle-color) #:new574 #:*cruiser*575))"))
                (p "You can see the variables generated by "
                  (code "gensym") ": "
                  (code "#:*cruiser*575") " and "
                  (code "#:new574") ". By default, symbols generated with "
                  (code "gensym") " are prefixed with "
                  (code "G") ", but we can change that:")
                (pre
                  (code :class "lisp" "(gensym \"*CRUISER*\")
                                        ; => #:*CRUISER*592"))
                (p "We can even change the suffix number (usually derived from "
                  (code "*gensym-counter*") ") if we pass an integer.")
                (pre
                  (code :class "lisp" "(gensym 500)
                                        ; => #:G500"))
                (p "Importantly, these anonymous symbols aren't interned, so you can't even find them if you go searching.")
                (pre
                  (code :class "lisp" "(find-symbol \"*CRUISER*592\" )
                                        ; => NIL, NIL"))
                (p "And in fact, even if they appear to have the same name, two symbols generated with "
                  (code "gensyms") " are still entirely different objects in memory.")
                (pre
                  (code :class "lisp" "(let ((a (gensym 100))
      (b (gensym 100)))
  (values a b))
                                        ; => #:G100, #:G100

(let ((a (gensym 100))
      (b (gensym 100)))
  (eq a b))
                                        ; => NIL"))
                (p "Once you find a variable that can possibly be captured, saving it to an anonymous symbol is a bulletproof way to avoid capture.")
                (pre
                  (code :class "lisp" "(defmacro comes-before-p (x y sequence)
  (let ((xvar (gensym))
        (yvar (gensym)))
    `(let ((sequence ,sequence)
           (,xvar ,x) 
           (,yvar ,y))
       (< (position ,xvar sequence)
          (position ,yvar sequence)))))")))))))
      (section :id "organizing-code"
        (hgroup
          (span)
          (h1 "ORGANIZING CODE"))
        (div :class "outline-text-2" :id "text-orgae6599d")
        (section :id "packages"
          (hgroup
            (span)
            (h2 "Packages"))
          (div :class "outline-text-3" :id "text-org9f7ec81")
          (section :id "what-are-packages-"
            (hgroup
              (span)
              (h3 "What Are Packages?"))
            (div :class "outline-text-4" :id "text-org7d1311c"
              (p "Packages are namespaces for symbols. ")))
          (section :id "defining-packages"
            (hgroup
              (span)
              (h3 "Defining Packages"))
            (div :class "outline-text-4" :id "text-orgabc44db"
              (p "Packages are defined with "
                (code "defpackage") ".")
              (pre
                (code :class "lisp" "(defpackage #:my-package
  (:use #:cl))"))
              (p
                (code "my-package") " inherits or \"uses\" all of the external symbols of "
                (code "cl") ", a nickname for the "
                (code "common-lisp") " package, which contains all of the standard Common Lisp symbols.")
              (pre
                (code :class "lisp" "(describe 'cl:defun)
                                        ; COMMON-LISP:DEFUN
                                        ;   [symbol]

                                        ; DEFUN names a macro:
                                        ;   Lambda-list: (NAME LAMBDA-LIST &BODY BODY)
                                        ;   Documentation:
                                        ;     Define a function at top level.
                                        ;   Source file: SYS:SRC;CODE;MACROS.LISP
                                        ;  => ; No value"))
              (p "If you don't include this line, you will need to add a package-qualifier to all of the symbols built into Common Lisp.")
              (pre
                (code :class "lisp" "(defpackage #:messy-package)
(in-package #:messy-package)

(defun hello-world ()
  (print \"Hello world\"))"))
              (p "If you evaluate this code, you'll get a strange error:")
              (pre
                (code :class "lisp" "The variable HELLO-WORLD is unbound.
   [Condition of type COMMON-LISP:UNBOUND-VARIABLE]"))
              (p "Why? Because the "
                (code "defun") " special operator is actually a "
                (i "macro") " whose first argument is a name for the function you are defining. Because it's a macro, the order of operations is to first "
                (i "expand") " the macro and then execute the code. During the macro expansion, the "
                (code "hello-world") " symbol will be interned and its "
                (i "function name") " value will be bound to "
                (code "hello-world") ".")
              (p "However, because we didn't "
                (code ":use #:cl") ", "
                (code "defun") " is interpreted as the name of a "
                (i "function") ", which requires first that all inner forms be evaluated and their values returned. And since "
                (code "hello-world") " isn't bound to any value yet, we get the above error.")
              (p "All that is to say, unless you're a Lisp wizard concocting a sexp brew, you should be adding "
                (code "(:use #:cl)") " to all of your package definitions.")))
          (section :id "using-importing-exporting"
            (hgroup
              (span)
              (h3 "Using, Importing & Exporting"))
            (div :class "outline-text-4" :id "text-org65541b7"
              (p "Let's expand the example:")
              (pre
                (code :class "lisp" "(defpackage #:my-package
  (:use #:cl)
  (:export #:*global*))

(in-package #:my-package)

(defparameter *global* \"This is a global variable.\")

(defpackage #:your-package
  (:use #:cl)
  (:import-from #:my-package))
(in-package #:your-package)

(print my-package:*global*)
                                        ; \"This is a global variable.\"  => \"This is a global variable.\"
(symbol-package 'my-package:*global*)
                                        ; => #<PACKAGE \"MY-PACKAGE\">

(let ((my-package:*global* \"Symbol from MY-PACKAGE lexically rebound in YOUR-PACKAGE\"))
  my-package:*global*)
                                        ; => \"Symbol from MY-PACKAGE lexically rebound in YOUR-PACKAGE\"

(defparameter *global* \"Global variable interned in YOUR-PACKAGE.\")

(symbol-package '*global*)
                                        ; => #<PACKAGE \"YOUR-PACKAGE\">"))
              (p "By importing packages, you can reuse symbol names without naming conflicts.")
              (p "You may also want to import the symbol, allowing you to use it without a package-name qualifier.")
              (pre
                (code :class "lisp" "(defpackage #:your-package
  (:use #:cl)
  (:import-from #:my-package
                #:*global*))"))
              (p "If you start this way, then you won't need a package name qualifier when you use it.")))
          (section :id "shadowing-and-conflicts"
            (hgroup
              (span)
              (h3 "Shadowing And Conflicts"))
            (div :class "outline-text-4" :id "text-orgddf5b34"
              (p "This is not the case if you "
                (code "use")
                (code "my-package") ". Change the definition of "
                (code "your-package") ":")
              (pre
                (code :class "lisp" "(defpackage #:your-package
  (:use #:cl #:my-package))"))
              (p "If you compile that form you will get this error:")
              (pre
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
                (code "restarts") " to choose from, including "
                (code "TAKE-NEW") ", "
                (code "KEEP-OLD") ", etc. and a bit of a mess to clean up. My recommendation is to only use "
                (code ":use") " sparingly in situations you know you aren't going to have conflicts later (like in your testing packages).")
              (p "If you choose "
                (code "KEEP-OLD") ", you will "
                (i "shadow") " the "
                (code "my-package:*global*") " with "
                (code "your-package:*global*") ". Shadowing means that the imported symbol is hidden, allowing the same symbol in the current package.")
              (p "My experience has been that doing anything short of aborting, "
                (code "unintern") " ing the symbol or "
                (code "unuse-package") " ing the package I'm using leads to frustration. You're better off not using "
                (code ":use") " in the first place.")
              (p "Even if you do as I recommend you might still have this name conflict problem. Consider this situation:")
              (pre
                (code :class "lisp" "(defpackage #:my-package
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
                  (code "my-function") " when I try to use it."))
              (p "When I try to compile "
                (code "my-other-function") ", I'll get a style-warning about an undefined function. At this point, there are two options:")
              (ol :class "org-ol"
                (li "Add a package name qualifier, as in "
                  (code "(my-package:my-function)") "; or")
                (li "Add the function symbol in the imports as below"))
              (pre
                (code :class "lisp" "(defpackage #:my-other-package
  (:use #:cl)
  (:import-from #:my-package
                #:my-function))
(in-package #:my-other-package)"))
              (p "If you add the package name qualifier, you'll get this error:")
              (pre
                (code :class "lisp" "*Org Src essentials.org[ lisp ]*:14:25:
  read-error: 
    READ error during COMPILE-FILE:

      The symbol \"MY-FUNCTION\" is not external in the MY-PACKAGE package.

        Line: 2, Column: 25, File-Position: 52

        Stream: #<SB-INT:FORM-TRACKING-STREAM for \"file /var/tmp/slime1YpOBd\" {7008A37DA3}>

Compilation failed."))
              (p "Oh bother, forgot to export it.")
              (pre
                (code :class "lisp" "(defpackage #:my-function
  (:use #:cl)
  (:export #:my-function))"))
              (p "Recompile and continue.")
              (p "If you fix the problem while it is just a style-warning, you'll be okay. But if you happen to execute code that uses "
                (code "my-function") ", then you'll get an error message:")
              (pre
                (code :class "lisp" "The function MY-OTHER-PACKAGE::MY-FUNCTION is undefined.
   [Condition of type UNDEFINED-FUNCTION]

Restarts:
 0: [CONTINUE] Retry calling MY-FUNCTION.
 1: [USE-VALUE] Call specified function.
 2: [RETURN-VALUE] Return specified values.
 3: [RETURN-NOTHING] Return zero values.
 4: [*ABORT] Return to SLY's top level.
 5: [ABORT] abort thread (#<THREAD tid=15507 \"slynk-worker\" RUNNING {7005A4AA33}>)"))
              (p "Whoops, "
                (i "now") " let's add it to imports and recompile.")
              (pre
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
                (code "SHADOWING-IMPORT-IT") " and try to recompile your code again, you'll get a similar error:")
              (pre
                (code :class "lisp" "*Org Src essentials.org[ lisp ]*:7:1:
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
              (pre
                (code :class "lisp" ";; * Option 1: Add a package name qualifier
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

;; Recompile MY-OTHER-PACKAGE's defpackage form above.
;; It should now succeed.

;; Now you can call your code.
(my-other-function)"))
              (p "It'll take some getting used to, but eventually you'll know how to clean up any messes you make.")))
          (section :id "nicknames"
            (hgroup
              (span)
              (h3 "Nicknames"))
            (div :class "outline-text-4" :id "text-org622dad1"
              (p "It's possible for packages to define nicknames. Nicknames are useful for calling individual symbols from packages that have long names.")
              (pre
                (code :class "lisp" "(defpackage #:a-package-with-a-long/and-annoying/name
  (:use #:cl)
  (:nicknames #:smol-name)
  (:export #:smol-hello))

(in-package #:a-package-with-a-long/and-annoying/name)

(defun smol-hello ()
  :hi)

(defpackage #:my-package
  (:use #:cl)
  (:import-from #:some-package))

(in-package #:my-package)

(smol-name:smol-hello) ; => :HI"))
              (p "If a package doesn't define its own nickname, you can define a nickname local to your package:")
              (pre
                (code :class "lisp" "(defpackage #:a-package-with-a-long/and-annoying/name
  (:use #:cl)
  (:export #:smol-hello))

(in-package #:a-package-with-a-long/and-annoying/name)

(defun smol-hello ()
  :hi)

(defpackage #:my-package
  (:use #:cl)
  (:local-nicknames (#:annoying #:a-package-with-a-long/and-annoying/name)))

(in-package #:my-package)

(annoying:smol-hello) ; => :HI"))
              (p "Unfortunately, you can't define nicknames for individual symbols.")
              (pre
                (code :class "lisp" "(defpackage #:a-package-with-a-long/and-annoying/name
  (:use #:cl)
  (:export #:a-very-long-and-annoying-function-or-macro-name
           #:smol-hello))

(in-package #:a-package-with-a-long/and-annoying/name)

(defun smol-hello ()
  :hi)

(defun a-very-long-and-annoying-function-or-macro-name ()
  :oof)

(defpackage #:my-package
  (:use #:cl)
  (:local-nicknames (#:annoying #:a-package-with-a-long/and-annoying/name)))

(in-package #:my-package)

;; Deal with it.
(annoying:a-very-long-and-annoying-function-or-macro-name) ; => :OOF"))))
          (section :id "-uiop-define-package-"
            (hgroup
              (span)
              (h3
                (code "uiop:define-package")))
            (div :class "outline-text-4" :id "text-org33d3f4e"
              (p "UIOP is a cross-implementation compatibility library that comes with ASDF (more on that in a second). Its "
                (code "define-package") " macro provides some extra capabilities that the standard "
                (code "defpackage") " doesn't. We'll see them in use later when discussing different factoring styles."))))
        (section :id "systems"
          (hgroup
            (span)
            (h2 "Systems"))
          (div :class "outline-text-3" :id "text-org971ce72")
          (section :id "what-are-systems-"
            (hgroup
              (span)
              (h3 "What Are Systems?"))
            (div :class "outline-text-4" :id "text-org8fc2a28"
              (p "Common Lisp system are what other languages might call \"packages\".")
              (p "A project may have code organized across many different files. In order to call code from one file in another file, you will need to have that file loaded into your Lisp image. If "
                (code "core.lisp") " calls and relies on "
                (code "utils.lisp") " code, then "
                (code "utils.lisp") " code needs to be loaded before "
                (code "core.lisp") " is loaded and run. If you have a lot of files that need to be loaded in a particular order, you probably want a way to automate this process. Systems, defined with "
                (code "defsystem") " and the "
                (code "ASDF") " library, are declarations of groups of code loaded in a particular order.")
              (p "When you need to coordinate the loading of multiple files, including third-party systems, then it's time to think about your "
                (code "defsystem") ".")))
          (section :id "what-is-asdf-"
            (hgroup
              (span)
              (h3 "What Is ASDF?"))
            (div :class "outline-text-4" :id "text-orgc6478d5"
              (p "ASDF is the defacto-standard library (available out of the box with SBCL and other implementations) that adds convenient code loading orchestration capabilities to Lisp. You do so by defining a "
                (code "defsystem") " in a "
                (code ".asd") " file in your project's root directory and then loading the system. When you load the system, ASDF will load your other files depending on your system definition.")
              (p "Because "
                (code "defsystem") " is a third-party extension to Lisp and was not previously available, there are different styles of organizing packages and systems in older projects that are less common in more modern projects. We'll be taking a look at different styles of factoring later, but for now let's focus on "
                (code "defsystem") " alone.")))
          (section :id "defining-systems"
            (hgroup
              (span)
              (h3 "Defining Systems"))
            (div :class "outline-text-4" :id "text-org9b802a5"
              (p "Let's say we have a project that looks like this:")
              (pre :class "example" :id "orgddf2536" "% tree
.
├── almighty-system.asd
└── src
    └── core.lisp")
              (p "This is our "
                (code "defsystem") ":")
              (pre
                (code :class "lisp" "(defsystem \"almighty-system\"
  :author \"Micah Killian\"
  :version \"0.0.1\"
  :description \"A system for demonstrating how to define systems.\"
  :depends-on (\"local-time\")
  :components (:module \"src\"
               :pathname \"src\"
               (:file \"core\")))"))
              (p "If we load this system (more on that later), ASDF will look for a local copy of the "
                (code "local-time") " library, downloading it with "
                (code "Quicklisp") " (more on that later) if a local copy doesn't exist. After loading the "
                (code "defsystem") " defined by "
                (code "local-time") ", ASDF will read the "
                (code "core.lisp") " file in the "
                (code "src") " directory and load it into the Lisp image. ")))
          (section :id "loading-systems"
            (hgroup
              (span)
              (h3 "Loading Systems"))
            (div :class "outline-text-4" :id "text-orga9f74bb"
              (p "Loading a system requires first that the "
                (code ".asd") " file is loaded.")
              (pre
                (code :class "lisp" "(asdf:load-asd (merge-pathnames *default-pathname-defaults* \"almighty-system.asd\"))"))
              (p "This requires some explanation. "
                (code "asdf:load-system") " takes a "
                (code "pathname") " to the "
                (code ".asd") " file. If you have the absolute path available, you can use that:")
              (pre
                (code :class "lisp" "(asdf:load-asd \"/Users/micah/lisp/almighty/content/essentials/code/projects/almighty-system/almighty-system.asd\")"))
              (p "Or, in the REPL, you can type "
                (code ",") " ("
                (code "sly-mrepl-shortcut") ") and select "
                (code "set-directory") " and ensure that the current working directory is set to the project root (where the "
                (code ".asd") " file lives), and then run the other line above. "
                (code "set-directory") " will set the value of "
                (code "*default-pathname-defaults*") ", and "
                (code "merge-pathnames") " does what it says. Thus, it's equivalent to the following:")
              (pre
                (code :class "lisp" "(asdf:load-asd (merge-pathnames \"/Users/micah/lisp/almighty/content/essentials/code/projects/almighty-system/\" \"almighty-system.asd\"))"))
              (p "After you load the asd file, you can load the system with "
                (code ",") " and "
                (code "load system") " or "
                (code "(asdf:load-system \"almighty-system\")") " in the REPL."))))
        (section :id "styles-of-factoring-packages-systems"
          (hgroup
            (span)
            (h2 "Styles of Factoring Packages & Systems"))
          (div :class "outline-text-3" :id "text-org19dedc3"
            (p "Instead of dealing with systems separately from packages, it's useful to see how systems can be configured depending on different code organization strategies. As you might expect, the way you organize a project is heavily influenced by how large the project is."))
          (section :id "mother-of-all-package-strategy"
            (hgroup
              (span)
              (h3 "Mother Of All Package Strategy"))
            (div :class "outline-text-4" :id "text-orge1c7c81"
              (p "A good representation of the Mother Of All package strategy of Common Lisp code organization is "
                (code "hunchentoot") "–a popular Lisp web server and web dev framework. If we take a look at the file tree it looks like this:")
              (pre :class "example" :id "org4ddcb5e" "% tree
.
├── acceptor.lisp
├── CHANGELOG
├── CHANGELOG_TBNL
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
└── util.lisp")
              (p "The organization is flat, lots of separate files, mostly in the project root directory.")
              (p "Hunchentoot has one package definition (well, two), found in the "
                (code "packages.lisp") " file.")
              (pre
                (code :class "lisp" "(in-package :cl-user)

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
                (code ":nicknames") " defines what other package name qualifiers you can use besides "
                (code "hunchentoot") ". That means that Hunchentoot itself provides you the ability to use "
                (code "(tbnl:*acceptor*)") " if you want. Useful for allowing users to use a smaller package name qualifier on their symbols.")
              (p "Hunchentoot uses "
                (code ":use") " for several utility libraries. Again, I don't recommend it, but it is not uncommon.")
              (p
                (code ":shadow") " here creates an symbol or symbols that are initially unbound in the package. Clearly, there was a naming conflict somewhere between Common Lisp's built-in "
                (code "defconstant") " and the "
                (code "url-encode") " symbol intended to have two different values ("
                (code "utils.lisp") " and "
                (code "url-rewrite/url-rewrite.lisp") " both define "
                (code "url-encode") " functions). What this means is that in the "
                (code "hunchentoot") " package, if you write "
                (code "url-encode") " while you are in the "
                (code "hunchentoot") " package, it has a different value than if you are in the "
                (code "url-rewrite") " package.")
              (p "It's a wonky way of preventing a naming conflict error because the "
                (code "hunchentoot") " package "
                (i "uses")
                (code ":url-rewrite") " instead of importing it. However, you might sometimes need to use "
                (code ":shadow") " to get around naming conflicts caused by other systems/libraries that use Common Lisp symbol names, so it's something to keep in mind.")
              (p "The important point is this: All of the files in the project root begin with "
                (code "(in-package :hunchentoot)") ". Each file contributes their defined symbols to the "
                (code ":hunchentoot") " package.")
              (p "If all this package talk has the ol' nogging working overtime, you might be thinking, \"Hey Micah, doesn't that assume that the "
                (code "packages.lisp") " file is loaded before all the others?\"")
              (p "Yes, yes it does. Let's take a look at the "
                (code "hunchentoot.asd") " file:")
              (pre
                (code :class "lisp" "(defsystem :hunchentoot
  :serial t
  :version \"1.3.1\"
  :description \"Hunchentoot is a HTTP server based on USOCKET and
  BORDEAUX-THREADS.  It supports HTTP 1.1, serves static files, has a
  simple framework for user-defined handlers and can be extended
  through subclassing.\"
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
  :perform (test-op (o c) (load (merge-pathnames \"run-test.lisp\" (system-source-directory c)))))"))
              (p "Notice "
                (code ":serial t") " up at the top. That means that all of the "
                (code ":components") " will be loaded "
                (i "in the order they are listed") ". So, after all of the "
                (code ":depends-on") " dependency systems are loaded, the "
                (code "url-rewrite") " module will be loaded. Its contents will also be loaded in order, and so first the "
                (code "url-rewrite/packages.lisp") " file will be loaded, then "
                (code "specials.lisp") ", "
                (code "primitives.lisp") ", etc. After that module is loaded, the first file in the project root that will be loaded is its "
                (code "packages.lisp") " file that we look at before. The Mother Of All "
                (code "packages.lisp") " is where all the symbols for the majority of the project live and are namespaced.")
              (p "There are only two other systems in the Hunchentoot project: one for development and the other for testing. They don't provide any further insight into this strategy, so we will skip them.")
              (p "In summary, the Mother Of All Package Strategy is this: you have one package ("
                (code "defpackage :hunchentoot") ") and one system ("
                (code "defsystem :hunchentoot") "), and that one package "
                (code ":uses") " or imports from several other packages. Other code files begin with "
                (code "(in-package :hunchentoot)") ", giving them access to the symbols defined in other files and contributing the symbols they define to that package. The "
                (code "packages.lisp") " file's exports define the public interface for users. This style is \"retro\", but still perfectly usable especially on smaller projects, and even larger projects like Hunchentoot. The "
                (code "vend") " library is a modern Lisp library that use this Mother of All Package Strategy.")))
          (section :id "multiple-systems-strategy"
            (hgroup
              (span)
              (h3 "Multiple Systems Strategy"))
            (div :class "outline-text-4" :id "text-org0fba80b"
              (p "It's also possible that a single project might have several systems, primarily used as optional extensions to the library's core functionality. "
                (code "transducers") " is one such library, as is "
                (code "mito") ". However, "
                (code "transducers") " still uses just one package for the core functionality. It's "
                (code "defsystem") " definition also uses the "
                (code "serial t") " approach of loading source files in order. This Multi-System Strategy is modular, yet does not exclude the possibility of using the Mother Of All Package Strategy.")))
          (section :id "one-package-per-file-strategy"
            (hgroup
              (span)
              (h3 "One Package Per File Strategy"))
            (div :class "outline-text-4" :id "text-orga752bd1"
              (p
                (code "mito") ", on the other hand, in addition to having multiple systems–one for core functionality and others for auxiliary functionality–also uses the One Package Per File Strategy. This is a more \"modern\" approach and considered by some to be the preferred strategy generally.")
              (p "Let's take a look at the "
                (code "mito") " system:")
              (pre
                (code :class "lisp" "(defsystem \"mito\"
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
                (code ":depends-on") " list is three other subsystems in the "
                (code "mito") " project. Let's look at "
                (code "mito-core.asd") ":")
              (pre
                (code :class "lisp" "(defsystem \"mito-core\"
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
                  :depends-on (\"connection\" \"class\" \"db\" \"conversion\" \"logger\" \"util\")
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
                 (:file \"db\" :depends-on (\"db-drivers\" \"connection\" \"class\" \"util\"))
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
                (code ":depends-on") " list. What's more interesting here is the definition in "
                (code ":components") ": Many of the files use "
                (code ":depends-on") " as well, pointing to other files within the system. We don't see "
                (code ":serial t") " here, either. Instead of a linear dependency tree, we have a graph. In the "
                (code "hunchentoot") " system, we saw that the more minor, prerequisite files like "
                (code "packages.lisp") " came "
                (i "first") ". Here in the "
                (code "mito-core") " system, the major files are listed first, with their prerequisites coming later in the list. The order is inverted.")
              (p "Let's take a look inside "
                (code "src/mito.lisp") " (the only component in the "
                (code "mito.asd") " file above):")
              (pre
                (code :class "lisp" "(uiop:define-package #:mito
  (:use #:cl)
  (:use-reexport #:mito.core
                 #:mito.migration))
(in-package #:mito)"))
              (p "Instead of "
                (code "defpackage") ", Mito uses UIOP's "
                (code "define-package") " macro. It uses it because it provides a new option, "
                (code ":use-reexport") ". It's like "
                (code ":use") ", but with two important differences:")
              (ol :class "org-ol"
                (li "It will import all of the "
                  (i "exported") " symbols of the packages that are passed as arguments, and then "
                  (i "reexport them") ".")
                (li "If there is a naming conflict between symbols in the packages passed in the form, "
                  (i "the first one takes precedence") ", shadowing versions that come later. That means that if there is a "
                  (code "mito.core:cool-symbol") " and a "
                  (code "mito.migration:cool-symbol") ", because "
                  (code "mito.core") " comes first in the above definition, the "
                  (code "mito") " package with inherit "
                  (code "mito.core:cool-symbol") " and then that version will be reexported."))
              (p "In systems like Mito, the "
                (code "reexport") " options ("
                (code ":reexport") ", "
                (code ":use-reexport") ", and "
                (code ":mix-reexport") ") are used to help easily \"bubble up\" exports from peripheral packages into one final public API.")
              (p "However, although Mito bubbles up dependencies with "
                (code ":depends-on") ", using the One Package Per File Strategy doesn't preclude using "
                (code ":serial t") ", either. ")))
          (section :id "one-system-per-package"
            (hgroup
              (span)
              (h3 "One System Per Package"))
            (div :class "outline-text-4" :id "text-org78bbc53"
              (p "There is one other strategy worth noting. It's perhaps best represented by the "
                (code "utopian") " web framework. Let's take a look at its system definition:")
              (pre
                (code :class "lisp" "(defsystem \"utopian\"
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
              (p "The first thing you'll notice is that the definition is short. The dependencies only include…another file in the system? And there are no "
                (code ":components") "! To investigate, let's look in the \"utopian/main\"… file? Package? System?")
              (p "It's found in "
                (code "src/main.lisp") ":")
              (pre
                (code :class "lisp" "(uiop:define-package #:utopian
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
                (code "packages.lisp") " file. "
                (code ":mix-reexport") " is like "
                (code ":use-reexport") " that we saw in Mito, except that it will inherit "
                (i "all") " symbols, exported and not, from the packages that follow into the current package. Every single symbol from those six packages above are going to be inherited into the "
                (code "#:utopian") " package.")
              (p "But now you may be wondering, \"How do they get loaded if the system file doesn't list them in "
                (code ":components") "?\"")
              (p "That's not all, friends. The mystery deepens when we look in the "
                (code "utopian/routes") " package file:")
              (pre
                (code :class "lisp" "(defpackage #:utopian/routes
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
                (code "myway") "–imported in this package! That wasn't in the system definition!")
              (p "Actually, it is. Look again. See this line?")
              (pre
                (code :class "lisp" ":class :package-inferred-system"))
              (p "That one line drastically changes how packages and systems work.")
              (p
                (b "All packages in a "
                  (code ":package-inferred-system") " are treated as entire systems themselves") ".")
              (p "Package inferred systems will all have an entry-point into the system, like "
                (code ":depends-on (\"utopian/main\")") ". Remember: up until now, dependencies were systems. We were confused because "
                (code ":utopian") " is a "
                (i "package") ", not a "
                (i "system") ". But because of "
                (code ":package-inferred-system") ", "
                (code ":utopian") " and all of its dependent packages are treated like their own systems by ASDF! That's why the files for the rest of the project aren't listed in "
                (code ":depends-on") ": "
                (code "utopian/main") " is treated like a system, where its prerequisite packages are as well. ASDF will do the work of resolving the dependencies, hence why you don't need to list them manually. It will "
                (code "load") " prerequisite packages/systems both in the "
                (code "utopian") " project, "
                (b "including third-party libraries") ", downloading them if necessary.")
              (p "Package inferred systems are the Package Per File taken to its limit: System Per File.")
              (p "Using package inferred systems requires following a few rules which I'll demonstrate.")
              (pre
                (code :class "lisp" "(defsystem \"almighty\"
  :class :package-inferred-system
  :author \"Micah Killian\"
  :version \"0.0.1\"
  :description \"A system for demonstrating how to define systems.\"
  :pathname \"src\"
  :depends-on (\"almighty/main\"))"))
              (p "Notice here that the system is named "
                (code "almighty") ", and that the package/system in "
                (code ":depends-on") " begins with "
                (code "almighty") ". The entry-point and all subsequent packages in your package-inferred-system must begin with "
                (code "almighty") ".")
              (p "Notice also that "
                (code ":pathname") " is set. The tree view of this project looks like this:")
              (pre
                (code :class "lisp" ".
├── almighty.asd
└── src
    ├── config.lisp
    ├── db.lisp
    ├── main.lisp
    └── models
        └── person.lisp"))
              (p "And the files look like this:")
              (pre
                (code :class "lisp" ";; src/main.lisp
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
                (code "almighty") ". The part that follows is the file name, such as "
                (code "almighty/db") " for "
                (code "src/db.lisp") ". If a package's file is in a subdirectory, the name of the directory comes before the file name, as in "
                (code "almighty/models/person") " for "
                (code "src/models/person.lisp") ".")
              (p "If you don't follow these rules, ASDF won't be able to find the files. For example, if you have a file "
                (code "src/wrong-name.lisp") " that defines a package "
                (code "almighty/correct-name") " and try to "
                (code ":import-from") ", "
                (code ":use-reexport") ", etc. the package "
                (code "almighty/correct-name") " in another package like "
                (code "almighty/db") ", you will receive an error stating:")
              (pre :class "example" :id "org4b59039" "Unknown location:
  error: 
    Component \"almighty/correct-name\" not found, required by
    #<PACKAGE-INFERRED-SYSTEM \"almighty/db\">

Compilation failed.")
              (p "With the exceptions of the different system definition and file and package naming conventions imposed by them, PIS are the same as the One Package Per File Strategy above."))))
        (section :id "the-great-debate-package-system-best-practices"
          (hgroup
            (span)
            (h2 "The Great Debate: Package & System Best Practices"))
          (div :class "outline-text-3" :id "text-org26eeab8"
            (p "As a newcomer to Lisp, you might want to be able to fit in with the broader ecosystem. You want to learn the idioms and patterns common to Lisp users, private and professional. So even if you're a sophisticated senior who knows that \"it depends\" is probably the answer, you still ask the question, \"Which strategy is should I choose? Which one's the best?\""))
          (section :id "argument-in-favor-of-pis"
            (hgroup
              (span)
              (h3 "Argument in Favor of PIS"))
            (div :class "outline-text-4" :id "text-orgd7a0961"
              (p "The architect of ASDF wrote "
                (a :href "https://github.com/fare/asdf/blob/master/doc/best_practices.md" "a document") " precisely to answer this question. Unfortunately, it leaves more questions than answers. First, it explains problems with the other strategies we've covered:")
              (blockquote
                (p "When you start writing large enough systems, putting everything in one big package leads to a big mess: it's hard to find what function is defined where, or should be defined where; you invent your own symbol prefixing system to avoid name clashes; totally unrelated things end up in the same mother-of-all package; you divide your mother-of-all package into a few subpackages, but as the software keeps growing each of these packages in turn becomes too big.")
                (p "Meanwhile, as you grow large enough libraries, you find that you loading a big library just to use a small part of it becomes a big hassle, leading to code bloat, too much recompilation, too much re-testing, and not enough understanding of what's going on."))
              (p "So the problems with the Mother Of All Package Strategy are:")
              (ol :class "org-ol"
                (li "Naming conflicts.")
                (li "Hard to understand.")
                (li "A hassle to load and recompile."))
              (p "We saw with Hunchentoot that \"shadowing\" of symbols was a little hard to understand, yet appears to be the solution to naming conflicts in a project factored as it is. If you have files using symbol names found in other files in project, it can be hard to understand which symbol's value is the "
                (i "actual") " value used in the overall package. Thus, you namespace using some sort of naming convention.")
              (p "Additionally, you might try dividing into some sub-packages, but it's somewhat arbitrary, meaning developers have to come up with their own coding conventions regarding package management. Whether you use one package or multiple, if your system is large enough, those strategies will ultimately cause trouble.")
              (p "After explaining how Package Inferred Systems work, he explains how they solve the above problems:")
              (blockquote
                (p "This allows for large modular libraries, wherein you may use one file, and only that file and its transitive dependencies will be loaded, rather than the entire humongous library.")
                (p "This also helps you enforce a discipline wherein it is always clear in which file each symbol is defined, which files have symbols used by any other file, etc."))
              (p "In "
                (a :href "https://x.com/almighty_lisp/status/1938416300826759368?s=20" "a discussion I had with the author on X") ", our friend François-René Rideau (or "
                (a :href "https://github.com/fare" "fare on Github") "), I asked about the rationale behind PIS, why it is the best practice, etc. He said that the motivation to create PIS was to help large teams, but he also said that PIS can be useful for individuals with large or ambitious projects. He said that large .asd files (remember Mito's system?) tend to be error-prone. Further, he says,")
              (blockquote
                (p "[In large .asd files] you often leave a lot of obsolete dependencies that slow the build and the understanding of the code by newcomers."))
              (p "So large system definitions are scary to mess with, making it safer and therefore more likely for teams to leave unused dependencies in the system. That can make understanding the code more difficult, and make builds unnecessarily slower.")
              (p "While ultimately he didn't make a conclusive recommendation to use PIS, he appears to prefer using PIS from the beginning to improve modularity and understandability of code, and to make coordination with other developers easier.")))
          (section :id "arguments-against-pis"
            (hgroup
              (span)
              (h3 "Arguments Against PIS"))
            (div :class "outline-text-4" :id "text-orgb37f9e0"
              (p "So the argument in favor of Package Inferred Systems boils down to this:")
              (ol :class "org-ol"
                (li "It helps reduce problems of coordination with other developers.")
                (li "It helps make the code more understandable.")
                (li "It helps reduce the amount of unnecessary code that remains in the project."))
              (p "The question is: Does it really do all those things? And are PIS more effective than the alternatives?")
              (p "Understanding packages and systems is one of the more challenging things about learning Common Lisp for newcomers. That's why I've gone into more detail here on the subject than I have with other language features.")
              (p "Before I make my counter-argument to PIS, I must emphasize that I have no professional experience with Common Lisp, nor have I written any large projects with it. Fare, on the other hand, is an accomplished Lisper with experience writing Lisp in large teams. The odds that I am not understanding the benefits of PIS because of a lack of experience are astronomically high. I make my counter-arguments with fear and trepidation in my heart. All I have to guide me are my small brain and an intuition–as limited as it may be–about how simple programs and programming should be.")
              (p "My experience of package inferred systems is that they actually led to "
                (i "more") " confusion, not less. Perhaps you felt it too as we took our journey through these different strategies.")
              (p "Hunchentoot listed all third-party dependencies in the system definition–thus you had a sense of what code you needed to download, and what your potential exposure to bugs or security problems was. The "
                (code "defsystem") " used the "
                (code ":serial t") " option to let you, the newcomer, know that all of the "
                (code ":component") " files were loaded in the order they were listed. The Mother Of All "
                (code "packages.lisp") " gave us an easy to read overview of the public API. Without any sophisticated knowledge or tools, the code was understandable (with perhaps the exception of the use of "
                (code ":shadow") " for a couple of symbols).")
              (p "Was it always that way? How many challenges did the project have with coordination or understandability before it reached the point where we saw it? How much unseen discipline was practiced in the naming and organization of symbols in the project? For example, perhaps it was difficult to organize and line up the files in the "
                (code "defsystem") " definition so that all of the right code was loaded in the correct order? Is the project bloated? We see only a snapshot of the project as it is now. We can't know the challenges of coordination, dependency management, and code factoring that had to be overcome just from looking at the source.")
              (p "However, when we look at the source, the design of the system is undeniably "
                (i "simple") " and "
                (i "easy to understand") ", even for a dummy newcomer like me.")
              (p "The same is true of the "
                (code "vend") " library, which uses a similar strategy.")
              (p
                (code "mito") ", on the other hand, has a somewhat more difficult to follow "
                (code "defsystem") ": the liberal use of "
                (code ":depends-on") " and the inversion of the dependency tree makes the system definition much more difficult to follow. The system has several subsystems, and some of the packages use options like "
                (code ":use-reexport") " and so-on. You need to have a deeper understanding of how systems work, how shadowing works, and how those package options will effect the final shape of the system. While Hunchentoot was fairly transparent, Mito's system architecture introduced a fair bit of noisy abstractions that make reasoning about the system difficult at a glance.")
              (p "But perhaps with experience I would see the value in the abstractions? Perhaps the alternative would have been noisier, more difficult to extend with other developers, and more bloated?")
              (p "And then there's "
                (code "utopian") " and "
                (code ":package-inferred-system") ". The .asd file is small, but that just left us more questions than answers. Chief among them is: how many dependencies does it have, and where do they come from? What does the final public API actually look like after all subsystem symbols are bubbled up to the top system? What code is loaded (and potentially executed), and when?")
              (p "About API discoverability, Fare said the following in our discussion on X:")
              (blockquote
                (p "The SLIME REPL, not the unaided source code, is how you discover the code. Plus the generated documentation."))
              (p "This is an important point: Great tools can make a developer's job easier. That's why one of the goals of this book is to help you understand all of the tools available to you in Emacs. If you code Common Lisp in Emacs without knowing about window management, REPL shortcuts, SLY debugger restarts/keybindings, "
                (code "evil-mode") " VIM keybindings, etc. you are missing out on a great number of very powerful tools for efficient and pleasant coding.")
              (p "However, I disagree with Fare about how code is discovered. Yes, we can type "
                (code "SPC m g d") " or "
                (code "g d") " to use "
                (code "sly-edit-definition") " to go to the definition of some symbol. Yes, we can look at the documentation. But none of those provide a clear overview of third-party dependencies of the project nor a clear view of the shape of the system as a whole. Tools like PIS blur our vision of the whole system. When I look at projects that use those tools, every bone in my body screams, \"Complexity spirit demon.\"")
              (p "Unfortunately, I am not the only one that feels this way. Do a search for \"package-inferred-system\" on X and you'll find a lot of confusion or even distaste for it. ")
              (p "Still, there are some notable people that really enjoy it. The most well-known users are probably the "
                (a :href "https://github.com/fukamachi/" "Eitaro Fukamachi (https://github.com/fukamachi)") " (author of "
                (code "mito") " and many other libraries) and "
                (a :href "https://github.com/svetlyak40wt" "Alexander Artemenko (https://github.com/svetlyak40wt)") "–the man behind "
                (a :href "https://github.com/40ants" "40ants (https://github.com/40ants)") " and Ultralisp–both of whom are prolific giants in the Common Lisp community (I think I'm starting to sweat here). Alexander has explained why he likes PIS "
                (a :href "https://x.com/svetlyak40wt/status/1179260459255549952?s=20" "on X") ":")
              (blockquote
                (p "For me this more important consequence is that I have not to figure out dependencies between files anymore and to hardcode them in the *.asd files. System definition can be just: (defsystem \"foo\" :class :package-inferred-system :depends-on (\"foo/main\"))"))
              (p "This echos what Fare says about system understandability and ease of code factoring. So maybe there is something to the arguments about the challenges of factoring that standard "
                (code "defsystem") " definitions impose.")
              (p "My brain may not be big enough to understand the ease-of-use and ease-of-understanding arguments. There's a good chance it's a skill issue.")
              (p "However, there is one feature of PIS that unfortunately I can't forgive, no matter how small my brain.")
              (p "The fact that third-party dependencies are not defined in the .asd file, but instead are spread across the codebase, is simply not acceptable. As an Almighty Lisp programmer, dependency management needs to be crystal clear both for understanding and for containment. Package inferred systems simply make it too easy to introduce dependencies in unknown places. My goal is the keep my use of dependencies limited, decreasing my dependencies over time. If it's easy to add and forget about them, then the temptation to increase my dependencies will be too great.")
              (p "The community doesn't appear to view PIS as the way forward. Community adoption of PIS is very low. Package inferred systems have been around since 2014, more than 10 years ago. At present, while there are nearly 5000 systems available on the Quicklisp repository, only around 60 unique systems use "
                (code ":package-inferred-system") ". ")
              (p "LEM–an Emacs clone written from scratch in Common Lisp–and Coalton–a new Lisp language implementing a Haskell-like type system–are among the largest and most ambitious publicly available projects. Neither use PIS. Both use the "
                (code ":serial t") " option for the "
                (code ":components") ", manually listing dependencies and files to load from the project. Both make heavy use of the One Package Per File Strategy, but LEM also uses a hybrid Mother Of All Package. Coalton provides a public API in a "
                (code "package.lisp") " file! In fact, Coalton uses a strategy similar to Fare's "
                (code "LIL") " library–providing a "
                (code "package.lisp") " file for smaller \"modules\" in the system (modules by convention) and reexporting the symbols from the module files, allowing the system to bubble up symbols to the top level packages.")))
          (section :id "conclusion"
            (hgroup
              (span)
              (h3 "Conclusion"))
            (div :class "outline-text-4" :id "text-org2d20d39"
              (p "So to answer the question, \"Is it essential to learn and use package inferred systems?\", the answer is "
                (b "No") ". For small projects, a Mother Of All package can work fine, but the most common approach on modern projects by far is One Package Per File with some strategy for bubbling up and potentially listing exported symbols into a final API.")))))
      (section :id "the-ecosystem"
        (hgroup
          (span)
          (h1 "THE ECOSYSTEM"))
        (div :class "outline-text-2" :id "text-org5cb413a")
        (section :id "quicklisp"
          (hgroup
            (span)
            (h2 "QUICKLISP"))
          (div :class "outline-text-3" :id "text-org40ec7c8"
            (p "There is a surprising amount of open source code available for Lisp. The most common way to obtain it is through "
              (code "quicklisp") ", the defacto-standard library manager for Lisp. As I said before, ASDF by default uses quicklisp to download systems defined in the "
              (code ":depends-on") " portion of the "
              (code "defsystem") " if no local copy exists (in the "
              (code "~/quicklisp") " folder). When you want third-party libraries, you will likely begin by using quicklisp."))
          (section :id "using-quicklisp"
            (hgroup
              (span)
              (h3 "Using Quicklisp"))
            (div :class "outline-text-4" :id "text-orgb61efea"
              (p "Using quicklisp is simple. Let's say you want to load the "
                (code "local-time") " system. To do that:")
              (pre
                (code :class "lisp" "(ql:quickload \"local-time\")"))
              (p "If you don't already have "
                (code "local-time") " in your "
                (code "~/quicklisp") " folder, quicklisp will download it from the quicklisp repository. After downloading it, it will then load the system as if you had run "
                (code "asdf:load-system") ". To text it, try running "
                (code "(local-time:now)") " in the REPL.")))
          (section :id "local-code"
            (hgroup
              (span)
              (h3 "Local Code"))
            (div :class "outline-text-4" :id "text-org93ccce3"
              (p "Quicklisp will download and load code from the "
                (code "quicklisp") " \"dist\". What if you want to load your own local code? How do you create your own library that you can add to a "
                (code ".asd") " file and load seamlessly like any other library?")
              (p "To do that with Quicklisp, you can add code to either the "
                (code "~/common-lisp") " or "
                (code "~/quicklisp/local-projects") " folders.")
              (p "I've personally had trouble with Quicklisp loading code from these folders, rather than the code I've vendored (using "
                (code "vend") ", introduced below). As a result, I don't like using either of these folders, but you might have better results than I did."))))
        (section :id "ultralisp"
          (hgroup
            (span)
            (h2 "ULTRALISP"))
          (div :class "outline-text-3" :id "text-orgaafb773"
            (p "Quicklisp is a repository that is only infrequently updated (by one guy). As a result, you might not be able to get the latest version of a project, or it may never be available at all on Quicklisp.")
            (p "An alternative to Quicklisp is "
              (code "ultralisp") ". Ultralisp is a \"dist\" that is installed on Quicklisp (which has its own default \"dist\" named "
              (code "quicklisp") ") and that is updated every five minutes. If you go to "
              (a :href "https://ultralisp.org" "https://ultralisp.org") " you can get instructions on how to install and use it. I haven't ever used it, so I can't give my opinion one way or the other.")
            (p "In passing, I've seen people have trouble using Ultralisp because they may inadvertently install different versions of the same library and getting Quicklisp (the library manager, not the dist) to load one or the other can be a bit troublesome. This might be similar to the problems I've had with using local code with Quicklisp as I explained above.")))
        (section :id "qlot"
          (hgroup
            (span)
            (h2 "QLOT"))
          (div :class "outline-text-3" :id "text-orga6b4ee9"
            (p
              (code "Qlot") " is an alternative library dependency library, found at "
              (a :href "https://qlot.tech/" "https://qlot.tech/") ". It is designed to essentially solve \"versioning\" problems above. It allows you to pin your dependencies to certain versions of libraries–as found on the Quicklisp \"dist\".")
            (p "Qlot is worthy of taking a closer look at. It includes useful command-line commands and library dependencies are defined in separate "
              (code "qlfile") " and "
              (code "qlfile.lock") " files, making it feel a bit more familiar to library management systems found in other ecosystems. After getting set up, you might find it more comfortable to use than Quicklisp or Ultralisp.")))
        (section :id "ocicl"
          (hgroup
            (span)
            (h2 "OCICL"))
          (div :class "outline-text-3" :id "text-org0e8b2e6"
            (p
              (code "ocicl") " is another alternative to Quicklisp that does more than just package management. It also does linting and has project scaffolding capabilities. It's developed by a programmer at Red Hat. It's Github page states:")
            (blockquote
              (p "The main innovation behind ocicl is the idea of applying the ecosystem of tooling and services from the world of application container images to ordinary tarballs of Lisp code. In essence, OCI + CL = ocicl."))
            (p "If you know what it means for software to be packaged as \"OCI-compliant artifacts\", you might find ocicl of interest. I'm not sophisticated enough to understand the value proposition, so I've never used it.")))
        (section :id "vend"
          (hgroup
            (span)
            (h2 "VEND"))
          (div :class "outline-text-3" :id "text-org8e3c0c6"
            (p
              (code "vend") " ("
              (a :href "https://github.com/fosskers/vend" "https://github.com/fosskers/vend") ") is my preferred alternative to Quicklisp. The vend philosophy is this: vendor dependencies, and make that simple.")
            (p "Unlike the other alternatives above, it makes no use of either Quicklisp the tool or the \"dist\". Instead, it downloads dependencies directly from git repositories. It has its own repository–just a list of libraries and their git repo link.")
            (p "The marketing copy from the "
              (code "vend") " Github readme states:")
            (blockquote
              (p "Why vend?")
              (p "Dependency management in Common Lisp has traditionally centred around Quicklisp. A desire for access to more rapid package updates spawned Ultralisp. The need for version pinning and isolation birthed Qlot. The want for a better distribution system brought us ocicl.")
              (p "But, could there be a simpler paradigm than just downloading the code and putting it right there?")
              (p "With vend:")
              (p "We need not perform bespoke installation scripts to get started.
We need not wait for Quicklisp to update.
We need not relegate all our systems to ~/common-lisp/.
We need not worry about where ASDF is looking for systems.
We need not fret over tools performing strange internal overrides.
We need not manage extra config files or lockfiles.
Plus, vend is actually an external tool with extra commands to help you inspect and manage your dependencies."))
            (p "If we, as almighty developers, want to defeat the machines, we need to consciously seek ways of doing more work in fewer steps. Finding and using the simplest way to do things is a matter of survival. Vend is an important for simplifying our tech stack and empowering us to do more. That's why for projects in the next chapter, we will be using "
              (code "vend") ".")
            (p "I will explain how to install and use "
              (code "vend") " when it becomes necessary."))))
      (section :id "projects"
        (hgroup
          (span)
          (h1 "PROJECTS"))
        (div :class "outline-text-2" :id "text-org91f60c3")
        (section :id "-almighty-money-"
          (hgroup
            (span)
            (h2
              (code "ALMIGHTY-MONEY")))
          (div :class "outline-text-3" :id "text-orgdb7a798")
          (section :id "introduction"
            (hgroup
              (span)
              (h3 "Introduction"))
            (div :class "outline-text-4" :id "text-orgfad53b3"
              (p "For this project, we are going to build a simple library for dealing with money.")
              (p "In our library, money will be an integer with some extras. Money has a currency–USD, JPY, etc.–that both limits how mathematical operations can be conducted on it and how it is displayed. You can add 5 to 5, but you can't add 5 USD into 5 JPY directly. USD is displayed with dollars on the left of a decimal, cents on the right: $10.52. 1000 USD is displayed with a comma after the 1: $1,000. This is true of every fourth digit–counting from right to left on the dollars side. USD uses the $ sign, and it's placed at the beginning of the number.")
              (p "The functions that will be implemented for this library are simple: "
                (code "money+") ", "
                (code "money-") ", "
                (code "money*") ", "
                (code "money/") ", "
                (code "money=") ", "
                (code "money>") ", and "
                (code "money<") ". We won't be doing any currency conversion here; we just want a way to do simple math on numbers that are a little special.")
              (p "We will use Common Lisp's hash-tables and OOP facilities to make our library extensible: I don't know how to display whatever the currency is in India or Transylvania, but we'll make it easy for someone from there to add support for those currencies without modifying our code.")
              (p "The necessary functionality is basic, so our use of hash-tables and OOP will also only be the basics.")))
          (section :id "project-setup"
            (hgroup
              (span)
              (h3 "Project Setup"))
            (div :class "outline-text-4" :id "text-orgb6d093c"
              (p "This is going to be a simple project, requiring only one lisp file and a system file."))
            (section :id "make-directory-files"
              (hgroup
                (span)
                (h4 "Make Directory & Files"))
              (div :class "outline-text-5" :id "text-org1c1c1af"
                (p "Make a file named "
                  (code "almighty-money.asd") " in a directory called "
                  (code "almighty-money") ". Save that file and make another file in that same directory named "
                  (code "almighty-money.lisp") ". Save that one, too. Those are the only files we'll need. Very small library.")))
            (section :id "define-system"
              (hgroup
                (span)
                (h4 "Define System"))
              (div :class "outline-text-5" :id "text-org54def40"
                (p "In "
                  (code "almighty-money.asd") ", add this code:")
                (pre
                  (code :class "lisp" "(defsystem \"almighty-money\"
  :author \"Micah Killian\"
  :version \"0.0.1\"
  :description \"A Library for Safely Handling Money\"
  :components ((:file \"almighty-money\")))"))
                (p "The rest of the code for this project will go inside the "
                  (code "almighty-money.lisp") " file."))))
          (section :id "a-money-type"
            (hgroup
              (span)
              (h3 "A "
                (code "money") " type"))
            (div :class "outline-text-4" :id "text-org90f5334"
              (p "Let's say that we have two numbers, 5000 and 3000. We want to add those numbers, but there's a catch: we need to know that they are the same "
                (i "type") " of number. If we are putting $5,000 and ¥3,000 into our wallet, we want to keep them separate. 7/11 in Japan doesn't want our dollars, and 7/11 in the US doesn't want our yen, so we can't mix them all willy nilly. We need to keep them separate and treat them as two entirely different "
                (i "types") " of money.")
              (p "In Common Lisp, to create a new type, we use "
                (code "defclass") ". So let's create a "
                (code "money") " type:")
              (pre
                (code :class "lisp" "(defclass money ()
  ((amount :initarg :amount :initform 0 :accessor amount)
   (currency-code :initarg :currency-code :initform nil :accessor currency-code)
   (currency-sign :initarg :currency-sign :initform nil :accessor currency-sign))
  (:documentation \"A class that holds all the information about some money. Any
 class that inherits from the MONEY class must provide a default CURRENCY-CODE
 and CURRENCY-SIGN.

AMOUNT is an integer. Ex. 1000000. This value reflects the smallest monetary
unit for each currency. A MONEY object with AMOUNT 5000 and CURRENCY-CODE USD
means $50.00, or 5,000 cents.

CURRENCY-CODE is a string using the ISO 4217 format. Ex. \\\"USD\\\"

CURRENCY-SIGN is a string. It is some symbol to represent the type of money
being displayed. Ex. \\\"$\\\"\"))"))
              (p "We could add "
                (code ":type") " specifications to each of the slots, but how the compiler enforces the types is "
                (i "implementation dependent") ". That means we can't be sure if, how, or under what conditions our implementation will enforce those types. "
                (code ":type") " might be useful under certain circumstances (and perhaps the mere value of defining "
                (code ":type") " as documentation is useful to you), but for now we'll keep things simple.")
              (p "The ~amount= slot is an integer, and it represents the smallest monetary unit for its currency. That means that 5000 USD is $50.00 and 5000 JPY is ¥5,000.")))
          (section :id "making-money"
            (hgroup
              (span)
              (h3 "Making money"))
            (div :class "outline-text-4" :id "text-orga2eefc3"
              (p "Now, let's try making some money:")
              (pre
                (code :class "lisp" "(make-instance 'money :amount 4000 :currency-code \"USD\" :currency-sign \"$\")
                                        ; => #<MONEY {7006D80343}>"))
              (p "We now see one of the downsides of using OOP: the representation of the "
                (code "money") " object in the REPL is unhelpful. We will fix DX problems like that later. For now, let's define a "
                (code "make-money") " function to make the code a bit less verbose:")
              (pre
                (code :class "lisp" "(defun make-money (&optional (amount 0) (currency *default-currency*))
  (assert (and (integerp amount)) (amount)
          \"AMOUNT must be an integer, got ~a.\" amount)
  (etypecase currency
    (money (make-instance (type-of currency) :amount amount))
    (string (make-instance (get-registered-currency currency) :amount amount))
    (symbol (make-instance currency :amount amount))))

(let ((50-bucks (make-money 50)))
  (list (amount 50-bucks) (currency-code 50-bucks) (currency-sign 50-bucks)))
                                        ; => (50 \"USD\" \"$\")"))
              (p "For the "
                (code "currency") " argument, "
                (code "make-money") " can take either another "
                (code "money") " object, a string like \"USD\", or a symbol like "
                (code "'us-dollar") ". Internally, we'll be sending "
                (code "money") " objects or using symbols, but users of the library might prefer to just use currency codes instead without importing the "
                (code "defclass") " of their desired currency everywhere they want to use "
                (code "make-money") ".")
              (p "Our system for registering and looking up currencies is simple: We use a hash-table to store and look up currency classes by currency code.")
              (pre
                (code :class "lisp" "(defparameter *default-currency* \"USD\")

(defparameter *registered-currencies* (make-hash-table :test #'equalp))

(defun register-currency (currency-code currency-class)
  (setf (gethash currency-code *registered-currencies*) currency-class))

(defun get-registered-currency (currency-code)

  (gethash currency-code *registered-currencies*))"))
              (p "Let's add USD as a type:")
              (pre
                (code :class "lisp" ";; * USD
(defclass us-dollar (money)
  ((currency-code :initform \"USD\")
   (currency-sign :initform \"$\")))

(register-currency \"USD\" 'us-dollar)

(defun usd (amount)
  (assert (and (integerp amount)) (amount)
          \"AMOUNT must be an integer, got ~a.\" amount)
  (make-money amount 'us-dollar))

(usd 50)
                                        ; => #<USD {7006D80343}>"))))
          (section :id "adding-money"
            (hgroup
              (span)
              (h3 "Adding money"))
            (div :class "outline-text-4" :id "text-orgc8dd144"
              (p "The core purpose of this library is this: ensure that only money of the same currency is combined or compared mathematically. That means that the implementation of the "
                (code "money+") " function is going to be simple:")
              (pre
                (code :class "lisp" "(defun moneyp (x)
  (typep x 'money))

(defun currencies-match-p (expected received)
  (equal (type-of expected) (type-of received)))

(defun money+ (&rest moneys)
  (let ((expected-currency (first moneys)))
    (unless (moneyp expected-currency)
      (error 'type-error :expected-type 'money :datum expected-currency))
    (let ((total (amount expected-currency)))
      (dolist (current-currency (rest moneys))
        (etypecase current-currency
          (money
           (unless (currencies-match-p expected-currency current-currency)
             (error 'mismatched-currencies :expected expected-currency
                                           :received current-currency))
           (setf total (+ total (amount current-currency))))))
      (make-money total expected-currency))))"))
              (p
                (code "money+") " takes an arbitrary number of arguments. We check each argument with "
                (code "dolist") ": in the case where "
                (code "current-currency") " is of type "
                (code "money") " and the currencies of "
                (code "expected-currency") " and "
                (code "current-currency") " match, we add its ~amount= to "
                (code "total") ".")
              (p "This function only takes "
                (code "money") " objects. We try to detect non-money objects early by checking if the first argument is "
                (code "moneyp") ", but "
                (code "etypecase") " takes care of the rest.")
              (p "If the currencies of the expected and current currency don't match, then "
                (code "money+") " signals a "
                (code "mismatched-currencies") " condition.")
              (pre
                (code :class "lisp" "(define-condition mismatched-currencies (error)
  ((expected :initarg :expected :initform nil :reader expected)
   (received :initarg :received :initform nil :reader received))
  (:report (lambda (c stream)
             (format stream \"Mismatched currencies. Expected ~a, received ~a.\" (expected c) (received c)))))"))
              (p "It's not strictly necessary to make a condition. We could just use a simple "
                (code "error") ":")
              (pre
                (code :class "lisp" "(error \"Mismatched currencies. Expected ~a, received ~a.\" expected-currency current-currency)"))
              (p "Having a custom condition saves us the trouble of copying the same error in several later functions; convenient but no big deal.")
              (p "The real utility is allowing us (sometime later in a different library) or others (in their own application code) to act specifically on this condition. We aren't going to implement currency conversion functionality in this library, but we or others might want to be able to make a currency conversion when currencies are mismatched. Or maybe that would be a silly idea. I don't know, but I'm leaving that up to the next guy to decide. With a tiny bit of effort, I can provide myself and others that flexibility.")))
          (section :id "the-rest-of-the-math-functions"
            (hgroup
              (span)
              (h3 "The rest of the math functions"))
            (div :class "outline-text-4" :id "text-orgf57f471"
              (p
                (code "money-") " is only going to look a little different:")
              (pre
                (code :class "lisp" "(defun money- (&rest moneys)
  \"A function for subtracting the AMOUNTs of MONEY objects of the same currency. If
passed one MONEY object, will negate the AMOUNT\"
  (let ((expected-currency (first moneys)))
    (unless (moneyp expected-currency)
      (error 'type-error :expected-type 'money :datum expected-currency))
    (let ((total (amount expected-currency)))
      (cond ((= (length moneys) 1)
             (make-money (- (amount expected-currency)) expected-currency))
            (t
             (dolist (current-currency (rest moneys))
               (etypecase current-currency
                 (money
                  (unless (currencies-match-p expected-currency current-currency)
                    (error 'mismatched-currencies :expected expected-currency
                                                  :received current-currency))
                  (setf total (- total (amount current-currency))))))
             (make-money total expected-currency))))))"))
              (p "The only difference here is that we check if there is more than one argument passed to "
                (code "money-") ". If you pass only one argument, then it will negate the total.")
              (p
                (code "money*") " and "
                (code "money/") " are nearly the identical to each others:")
              (pre
                (code :class "lisp" "(defun money* (&rest moneys)
  \"A function for multiplying the amounts of MONEY objects of the same currency. If
passed an integer after the first MONEY object, will multiply the AMOUNT of the
MONEY by the integer.\"
  (let ((expected-currency (first moneys)))
    (unless (moneyp expected-currency)
      (error 'type-error :expected-type 'money :datum expected-currency))
    (let ((total (amount expected-currency)))
      (dolist (current-currency (rest moneys))
        (etypecase current-currency
          (money
           (unless (currencies-match-p expected-currency current-currency)
             (error 'mismatched-currencies :expected expected-currency
                                           :received current-currency))
           (setf total (* (amount current-currency) total)))
          (integer (setf total (floor (* total current-currency))))))
      (make-money total expected-currency))))

(defun money/ (&rest moneys)
  \"A function for dividing the amounts of MONEY objects of the same currency. If
passed an integer after the first MONEY object, will divide the AMOUNT of the
MONEY by the integer.\"
  (let ((expected-currency (first moneys)))
    (unless (moneyp expected-currency)
      (error 'type-error :expected-type 'money :datum expected-currency))
    (let ((total (amount expected-currency)))
      (dolist (current-currency (rest moneys))
        (etypecase current-currency
          (money
           (unless (currencies-match-p expected-currency current-currency)
             (error 'mismatched-currencies :expected expected-currency
                                           :received current-currency))
           (setf total (/ total (amount current-currency))))
          (integer (setf total (floor (/ total current-currency))))))
      (make-money total expected-currency))))"))
              (p "Here, we actually allow an integer type to be passed as an argument. It feels a bit odd to multiply currencies into each other, but it does make sense to say \"double the amount of money\" or \"apply a 25% discount\".")
              (p "Finally, we'll add three more simple math functions:")
              (pre
                (code :class "lisp" "(defun money= (money1 money2)
  \"A function to check if the AMOUNTs of two MONEY objects are =.\"
  (unless (moneyp money1)
    (error 'type-error :expected-type 'money :datum money1))
  (unless (moneyp money2)
    (error 'type-error :expected-type 'money :datum money2))
  (unless (currencies-match-p money1 money2)
    (error 'mismatched-currencies :expected money1
                                  :received money2))
  (and (= (amount money1) (amount money2))
       (equal (type-of money1) (type-of money2))))

(defun money> (money1 money2)
  \"A function that checks if the AMOUNT of one MONEY object is greater than that of
the other.\"
  (unless (moneyp money1)
    (error 'type-error :expected-type 'money :datum money1))
  (unless (moneyp money2)
    (error 'type-error :expected-type 'money :datum money2))
  (unless (currencies-match-p money1 money2)
    (error 'mismatched-currencies :expected money1
                                  :received money2))
  (> (amount money1) (amount money2)))

(defun money< (money1 money2)
  \"A function that checks if the AMOUNT of one MONEY object is greater than that of
the other.\"
  (unless (moneyp money1)
    (error 'type-error :expected-type 'money :datum money1))
  (unless (moneyp money2)
    (error 'type-error :expected-type 'money :datum money2))
  (unless (currencies-match-p money1 money2)
    (error 'mismatched-currencies :expected money1
                                  :received money2))
  (< (amount money1) (amount money2)))"))
              (p "I'm purposefully limiting these functions to only two arguments because I can't imagine myself trying to check more than two using any of these functions. If you can think of a scenario where checking the equality of 3 or more "
                (code "money") " objects would be useful, let me know.")))
          (section :id "improving-ergonomics"
            (hgroup
              (span)
              (h3 "Improving Ergonomics"))
            (div :class "outline-text-4" :id "text-org9a79e5b"
              (p "And that's it: the critical functionality of our library is complete. However, we really should improve the ergonomics a bit. When we print a "
                (code "money") " object, it looks like this:")
              (pre :class "example" :id "orgad6bcd9" "#<USD {7006D80343}>")
              (p "What we really want is for it to look like this:")
              (pre :class "example" :id "org4d64201" "#<USD 5000>")
              (p "We do that with the "
                (code "print-object") " generic function. We can change how an object is printed by creating a method specialized on our "
                (code "money") " or "
                (code "us-dollar") " types.")
              (pre
                (code :class "lisp" "(defmethod print-object ((this money) stream)
  (print-unreadable-object (this stream)
    (format stream \"MONEY ~a ~a\" (slot-value this 'amount) (slot-value this 'currency-code))))

(defmethod print-object ((this us-dollar) stream)
  (print-unreadable-object (this stream)
    (format stream \"~a ~a\" (slot-value this 'currency-code) (slot-value this 'amount))))"))
              (p "The "
                (code "print-unreadable-object") " function needs some explanation. When you return an object in the REPL, you can highlight it with the cursor and type "
                (code "Enter") " or left-click it with your mouse to "
                (code "inspect") " the object. This default behavior is preserved for "
                (code "print-object") " methods that you specialize on classes you define if you wrap the call to "
                (code "format") " in "
                (code "print-unreadable-object") ".")))
          (section :id "formatting-for-human-consumption"
            (hgroup
              (span)
              (h3 "Formatting for Human Consumption"))
            (div :class "outline-text-4" :id "text-org29c8e53"
              (p "One last important job for our library will be displaying the money amount as normal people expect money amounts to be displayed. That means that "
                (code "#<USD 5000>") " needs to be formatted as $50.00 and "
                (code "#<JPY 5000>") " needs to be formatted as "
                (code "¥5,000") ". We want one function to be able to format any currency properly. We could have one function with a big "
                (code "cond") " do that:")
              (pre
                (code :class "lisp" "(defun format-money (money-obj)
  (cond ((usdp money-obj) (format-usd money-obj))
        ((jpyp money-obj) (format-jpy money-obj))
        ((gbpp money-obj) (format-gbp money-obj))
        ...))"))
              (p "But that has the one downside that it requires updating this one function any time we want to add support for a currency to the library. It's not a huge burden, it's true, and some people may even prefer it. However, Common Lisp has an OOP solution to make our code more extensible: generic functions.")
              (pre
                (code :class "lisp" "(defgeneric format-money (stream currency)
  (:documentation \"A generic function that takes a currency object and formats it in for human
consumption.\"))"))
              (p "Then we need to make a generic "
                (i "method") " to format USD:")
              (pre
                (code :class "lisp" "(defmethod format-money (stream (this us-dollar))
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
                (code "(floor -5 100)") " will return "
                (code "-1") " rather than "
                (code "0") "), so we make all negative integers positive before formatting them, adding the minus-sign back at the beginning of the format string.")
              (p
                (code "~2,'0D") " looks crazy, I know. Format directives can be modified by optional parameters–separated by commas–and by modifiers "
                (code "COLON") " or "
                (code "AT-SIGN") ". For "
                (code "Tilde D") ", we have four possible parameters:")
              (ol :class "org-ol"
                (li "mincol")
                (li "padchar")
                (li "comma")
                (li "interval"))
              (p "Let's take a look at them in action:")
              (pre
                (code :class "lisp" "(let ((small-num 7)
      (big-num 987654321))
  (format t \"~%~40a | ~d | ~d\"                          \"no modifications\"                  small-num big-num)
  (format t \"~%~40a | ~4d | ~4d\"                        \"mincol of 4\"                       small-num big-num)
  (format t \"~%~40a | ~4,'xd | ~4,'xd\"                  \"mincol padding using character x\"  small-num big-num)
  (format t \"~%~40a | ~4,'x:d | ~4,'x:d\"                \"colon added\"                       small-num big-num)
  (format t \"~%~40a | ~4,'x,'x:d | ~4,'x,'x:d\"          \"commas replaced with x\"            small-num big-num)
  (format t \"~%~40a | ~4,'x,'x,1:d | ~4,'x,'x,1:d\"      \"comma interval set to 1\"           small-num big-num)
  (format t \"~%~40a | ~4,'x@d | ~4,'x,'x,1@d\"           \"colon replaced with at-sign\"       small-num big-num)
  (format t \"~%~40a | ~4,'x@:d | ~4,'x,'x,1@:d\"         \"at-sign and colon combined\"        small-num big-num))"))
              (p "Returns:")
              (pre :class "example" :id "orgeabc550" "no modifications                         | 7 | 987654321
mincol of 4                              |    7 | 987654321
mincol padding using character x         | xxx7 | 987654321
colon added                              | xxx7 | 987,654,321
commas replaced with x                   | xxx7 | 987x654x321
comma interval set to 1                  | xxx7 | 9x8x7x6x5x4x3x2x1
colon replaced with at-sign              | xx+7 | +987654321
at-sign and colon combined               | xx+7 | +9x8x7x6x5x4x3x2x1 => NIL")
              (p "It should be a bit easier now to understand the format string for "
                (code "format-money") ":"
                (code "~a") ", just display the "
                (code "currency-sign") "."
                (code "~:D") ", add commas to dollars side where appropriate."
                (code ".") ", we add the dot."
                (code "~2,'0D") ", set the "
                (code "mincol") " to 2 and pad empty space with 0s. This ensures that "
                (code "(usd 5)") " returns "
                (code "$0.05") " and not "
                (code "$0.5") ".")
              (p "Also, for the big-brains screaming, \"Micah! What about ~$ you dummy?\", "
                (code "Tilde $") " won't add commas to the number, and there's no way to combine both the effects of "
                (code "Tilde $") " and "
                (code "Tilde D") ".")))
          (section :id "adding-currencies"
            (hgroup
              (span)
              (h3 "Adding Currencies"))
            (div :class "outline-text-4" :id "text-orgf1a9239"
              (p "The library is \"complete\", in the sense that it does everything we want it to do. One catch: it only works with USD.")
              (p "How do we add support for more currencies?")
              (p "It's fairly simple. We'll do it with Japanese Yen:")
              (pre
                (code :class "lisp" ";; * JPY
(defclass japanese-yen (money)
  ((currency-code :initform \"JPY\")
   (currency-sign :initform \"¥\")))

(register-currency \"JPY\" 'japanese-yen)

(defmethod format-money (stream (this japanese-yen))
  (format stream \"~a~:d\" (currency-sign this) (amount this)))

(defmethod print-object ((this japanese-yen) stream)
  (print-unreadable-object (this stream)
    (format stream \"~a ~a\" (slot-value this 'currency-code) (slot-value this 'amount))))

(defun jpy (amount)
  (assert (integerp amount) (amount)
          \"AMOUNT must be an integer, got ~a.\" amount)
  (make-money amount 'japanese-yen))"))
              (p "We inherit and extend the "
                (code "money") " class with the "
                (code "japanese-yen") " class, assigning default values to "
                (code "currency-code") " and "
                (code "currency-sign") ".")
              (p "We register the new currency so that "
                (code "make-money") " will work properly if called like this: "
                (code "(make-money 5000 \"JPY\")") ". It's likely that people will only use "
                (code "jpy") ", but who knows?")
              (p "We specialize the "
                (code "format-money") " and "
                (code "print-object") " generic functions with methods for the new "
                (code "japanese-yen") " type. Formatting Japanese yen is even easier than USD: The lowest and "
                (i "only") " monetary unit is the yen, so no need for a decimal or the yen equivalent of \"cents\".")
              (p "Then we make a helper-function to make Japanese Yen.")
              (p "Neither we nor future developers will need to modify any existing code; we use OOP modularity and extensibility that would make every senior Java developer proud.")
              (p "And thus, our library is complete and ready to be upgraded with support for other currencies in the future."))))
        (section :id "-almighty-kaikei-"
          (hgroup
            (span)
            (h2
              (code "ALMIGHTY-KAIKEI")))
          (section :id "a-double-entry-accounting-system"
            (hgroup
              (span)
              (h3 "A Double-Entry Accounting System"))
            (div :class "outline-text-4" :id "text-org105e6b4"
              (p "For this project, we will be building the foundation for a double-entry accounting system called "
                (code "almighty-kaikei") ". double-entry accounting is a system that is designed to prevent or detect accounting errors by tracking two sides of a ledger: the debit and credit sides."))
            (section :id "types-of-accounts"
              (hgroup
                (span)
                (h4 "Types of Accounts"))
              (div :class "outline-text-5" :id "text-org847663f"
                (p "There are five basic types of accounts in an double-entry accounting system: Asset, Expense, Income, Liability, and Equity. All of the accounts are recorded in what's called a Chart of Accounts.")
                (p "The types are divided into two groups: "
                  (b "left hand side") " (LHS) and "
                  (b "right hand side") " (RHS) accounts. Assets and Expenses are LHS account, the rest are RHS accounts.")))
            (section :id "balancing-one-account"
              (hgroup
                (span)
                (h4 "Balancing One Account"))
              (div :class "outline-text-5" :id "text-org3c76baa"
                (p "The balance of an account's ledger is calculated based on all of its debits and credits. For example, for a Sales Revenue account:")
                (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                  (colgroup
                    (col :class "org-right")
                    (col :class "org-left")
                    (col :class "org-left")
                    (col :class "org-right")
                    (col :class "org-right"))
                  (thead
                    (tr
                      (th :scope "col" :class "org-right" "Date")
                      (th :scope "col" :class "org-left" "Description")
                      (th :scope "col" :class "org-left" "Debit")
                      (th :scope "col" :class "org-right" "Credit")
                      (th :scope "col" :class "org-right" "Balance")))
                  (tbody
                    (tr
                      (td :class "org-right" "2025-11-4")
                      (td :class "org-left" "Opening Balance")
                      (td :class "org-left" " ")
                      (td :class "org-right" " ")
                      (td :class "org-right" "0"))
                    (tr
                      (td :class "org-right" "2025-11-5")
                      (td :class "org-left" "Cash Sale")
                      (td :class "org-left" " ")
                      (td :class "org-right" "100")
                      (td :class "org-right" "100"))
                    (tr
                      (td :class "org-right" "2025-11-5")
                      (td :class "org-left" "Cash Sale")
                      (td :class "org-left" " ")
                      (td :class "org-right" "50")
                      (td :class "org-right" "150"))
                    (tr
                      (td :class "org-right" "2025-11-6")
                      (td :class "org-left" "Return")
                      (td :class "org-left" "20")
                      (td :class "org-right" " ")
                      (td :class "org-right" "130"))
                    (tr
                      (td :class "org-right" "2025-11-7")
                      (td :class "org-left" "Cash Sale")
                      (td :class "org-left" " ")
                      (td :class "org-right" "200")
                      (td :class "org-right" "330"))
                    (tr
                      (td :class "org-right" "---------")
                      (td :class "org-left" "---------------")
                      (td :class "org-left" "-----")
                      (td :class "org-right" "------")
                      (td :class "org-right" "-------"))
                    (tr
                      (td :class "org-right" "Total")
                      (td :class "org-left" " ")
                      (td :class "org-left" "20")
                      (td :class "org-right" "350")
                      (td :class "org-right" "330"))))
                (p "On the other hand, the Cost of Goods Sold (COGS) account for these transactions will look like this:")
                (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                  (colgroup
                    (col :class "org-right")
                    (col :class "org-left")
                    (col :class "org-right")
                    (col :class "org-left")
                    (col :class "org-right"))
                  (thead
                    (tr
                      (th :scope "col" :class "org-right" "Date")
                      (th :scope "col" :class "org-left" "Description")
                      (th :scope "col" :class "org-right" "Debit")
                      (th :scope "col" :class "org-left" "Credit")
                      (th :scope "col" :class "org-right" "Balance")))
                  (tbody
                    (tr
                      (td :class "org-right" "2025-11-4")
                      (td :class "org-left" "Opening Balance")
                      (td :class "org-right" " ")
                      (td :class "org-left" " ")
                      (td :class "org-right" "0"))
                    (tr
                      (td :class "org-right" "2025-11-5")
                      (td :class "org-left" "Cash Sale")
                      (td :class "org-right" "60")
                      (td :class "org-left" " ")
                      (td :class "org-right" "60"))
                    (tr
                      (td :class "org-right" "2025-11-5")
                      (td :class "org-left" "Cash Sale")
                      (td :class "org-right" "20")
                      (td :class "org-left" " ")
                      (td :class "org-right" "80"))
                    (tr
                      (td :class "org-right" "2025-11-6")
                      (td :class "org-left" "Return")
                      (td :class "org-right" " ")
                      (td :class "org-left" "10")
                      (td :class "org-right" "70"))
                    (tr
                      (td :class "org-right" "2025-11-7")
                      (td :class "org-left" "Cash Sale")
                      (td :class "org-right" "150")
                      (td :class "org-left" " ")
                      (td :class "org-right" "220"))
                    (tr
                      (td :class "org-right" "---------")
                      (td :class "org-left" "---------------")
                      (td :class "org-right" "-----")
                      (td :class "org-left" "------")
                      (td :class "org-right" "-------"))
                    (tr
                      (td :class "org-right" "Total")
                      (td :class "org-left" " ")
                      (td :class "org-right" "230")
                      (td :class "org-left" "10")
                      (td :class "org-right" "220"))))
                (p "Notice that the value of the balance for the Sales (income/revenue) account increases with credits, but for the COGS (an asset account) the balance increases with debits.")
                (p "For LHS accounts, "
                  (b "credits increase") " the balance, "
                  (b "debits decrease") " the balance.
For RHS accounts, "
                  (b "debits increase") " the balance, "
                  (b "credits decrease") " the balance.")
                (p "Thus,")
                (ul :class "org-ul"
                  (li "For LHS accounts, "
                    (code "(= balance (- debits credits))") ".")
                  (li "For RHS accounts, "
                    (code "(= balance (- credits debits))") "."))))
            (section :id "balancing-a-transaction"
              (hgroup
                (span)
                (h4 "Balancing a Transaction"))
              (div :class "outline-text-5" :id "text-orgc0c639b"
                (p "The balance of a transaction is calculated from the debits and credits on two or more accounts in the transaction. A transaction consists of a least one debit and one credit to two separate accounts.")
                (p "Let's say we are a business that wants to record a cash sale of physical merchandise. A sale will affect that following accounts:")
                (ol :class "org-ol"
                  (li "Inventory (asset)")
                  (li "Cash (asset)")
                  (li "Sales Revenue (income)")
                  (li "Cost of Goods Sold (COGS, expense)"))
                (p "Typically, transactions are visualized as a table with a debit and credit column:")
                (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                  (colgroup
                    (col :class "org-left")
                    (col :class "org-right")
                    (col :class "org-right"))
                  (thead
                    (tr
                      (th :scope "col" :class "org-left" "Account")
                      (th :scope "col" :class "org-right" "Debit")
                      (th :scope "col" :class "org-right" "Credit")))
                  (tbody
                    (tr
                      (td :class "org-left" "Cash")
                      (td :class "org-right" "100")
                      (td :class "org-right" " "))
                    (tr
                      (td :class "org-left" "Sales Revenue")
                      (td :class "org-right" " ")
                      (td :class "org-right" "100"))
                    (tr
                      (td :class "org-left" "COGS")
                      (td :class "org-right" "60")
                      (td :class "org-right" " "))
                    (tr
                      (td :class "org-left" "Inventory")
                      (td :class "org-right" " ")
                      (td :class "org-right" "60"))
                    (tr
                      (td :class "org-left" "-------------")
                      (td :class "org-right" "-----")
                      (td :class "org-right" "------"))
                    (tr
                      (td :class "org-left" "Totals")
                      (td :class "org-right" "160")
                      (td :class "org-right" "160"))))
                (p "So in the table above, cash is debited (increasing in value), sales revenue (an income account) is credited (increasing in value). You now have recorded an increase in your cash-on-hand and your sales.")
                (p "The Cash of Goods Sold account is debited (increasing in value), and your inventory is credited (decreasing in value). You have paid down the expense you had when purchasing an inventory good and the size or value of your inventory has decreased. By subtracting COGS from Sales Revenue, you can also know how much money you made from the transaction.")
                (p "And the debit and credit totals match on both sides.")))
            (section :id "ledgers"
              (hgroup
                (span)
                (h4 "Ledgers"))
              (div :class "outline-text-5" :id "text-orgef5950f"
                (p "Ledgers are ways of viewing the data your system records. For example, a general ledger will show raw data: what transactions occurred. A sales ledger will show sales. An account ledger will show the transactions for an account and its current balance. An accounts payable aging report will show how much money you owe, to whom, and how overdue you might be on a certain account. All of these different ledgers provide insight into your accounting: cash flow, sales/returns, etc."))))
          (section :id "requirements"
            (hgroup
              (span)
              (h3 "Requirements"))
            (div :class "outline-text-4" :id "text-orgc7a3253"
              (p "Given the above, we need to provide the following functionality in our library:")
              (ol :class "org-ol"
                (li "Account Creation")
                (li "Balanced Transaction Creation")
                (li "Account Balance Retrieval")
                (li "Chart of Accounts Balance Retrieval"))
              (p "The library should also provide the ability to easily create ledgers from the data it provides.")))
          (section :id "testing-examples"
            (hgroup
              (span)
              (h3 "Testing & Examples"))
            (div :class "outline-text-4" :id "text-org13de01c"
              (p "This library's functionality is going to be trickier than the "
                (code "almighty-money") " library's was, so we're going to need to create tests. For that, we'll be using "
                (code "lisp-unit2") ". We will do some basic unit tests, and we'll make some example ledgers to demonstrate the library's use in an accounting system.")))
          (section :id "dependencies"
            (hgroup
              (span)
              (h3 "Dependencies"))
            (div :class "outline-text-4" :id "text-org8a2f262"
              (p "We will be using the "
                (code "almighty-money") " library we created along with some other dependencies.")
              (p "Since we are now adding dependencies to our project, I will introduce you to "
                (code "vend") ", a library that replaces the defacto-standard "
                (code "quicklisp") " library for dependency management. Let me explain why.")))
          (section :id "vendor-your-dependencies"
            (hgroup
              (span)
              (h3 "Vendor Your Dependencies"))
            (div :class "outline-text-4" :id "text-org5806017"
              (p "The guiding principle of the Almighty Lisp developer is to, above all else, prioritize simplicity. Simplicity is the key to writing code that you and others can maintain without fear or frustration.")
              (p "When it comes to dependencies, simplicity looks like this:")
              (ol :class "org-ol"
                (li "Generally avoid adding dependencies to a project unless necessary. If you can write a simple thing yourself that gets the job done, you probably should.")
                (li "When adding dependencies to your project, adopt them as "
                  (b "your code") ". You are now responsible for their bugs, features, efficiency, etc.")
                (li "When possible, remove dependencies. Usually this means slowly extracting functionality from a dependency and rewriting it customized for your needs."))
              (p "Some people feel that the trend to make dependencies "
                (i "easier") " to add to a project via tools like "
                (code "npm") " or "
                (code "uv") " are detrimental to codebase quality and are a security liability. If adding and maintaining dependencies with such tools is easy, you will be tempted to add and keep large amounts of third-party code in your codebase. Adding a dependency "
                (i "is") " and "
                (i "should feel") " like a serious, risky decision.")
              (p "While I can see the benefits of that mindset, the Almighty Lisp philosophy is one that is designed to maximize the effectiveness of the "
                (i "individual programmer") ". If you have a team of developers, you can expect to be able to work together to write the exact code that you need, both your library code and application code. Individual programmers need to prioritize writing application code over library code, which means individuals need to rely more heavily on dependencies.")
              (p "If your brain is so big you can crank out the whole web stack by yourself while producing high-quality code, you can ignore the above. For the rest of us, we need a simple way of introducing dependencies into our projects that provide us the means to easily transition dependencies into code fit exactly to our purpose, free from the potential of introduced security vulnerabilities.")
              (p
                (code "vend") " is a library that does precisely that by making it simple to "
                (i "vendor") " our dependencies. "
                (code "Vend") " works by looking at our "
                (code "defsystem") " definitions, looks at what other systems our system "
                (code ":depends-on") ", and cloning the requisite systems from github into a directory in our project folder–defaulting to "
                (code "vendored") ".")
              (p "If for some reason you don't want to use "
                (code "vend") ", you can feel free to use "
                (code "quicklisp") " instead."))
            (section :id "installing-vend-"
              (hgroup
                (span)
                (h4 "Installing "
                  (code "vend")))
              (div :class "outline-text-5" :id "text-org10fe28c"
                (p "Installing "
                  (code "vend") " is simple, but it does have one unusual requirement: it requires ECL, a different implementation of Common Lisp (we installed SBCL at the very beginning, before even installing Emacs). So first, we need to install ECL.")
                (p "If you're on Linux, you can try looking for a package for your distribution.")
                (p "On MacOS, ECL is available on Homebrew.")
                (p "Otherwise, you can follow the instructions below to compile ECL from the source. These come directly from "
                  (a :href "https://ecl.common-lisp.dev/static/files/manual/current-manual/Building-ECL.html" "the manual") "."))
              (section :id "download-ecl"
                (hgroup
                  (span)
                  (h5 "Download ECL"))
                (div :class "outline-text-6" :id "text-org30de562"
                  (p "Download the latest release of ECL from "
                    (a :href "https://ecl.common-lisp.dev/static/files/release/" "their server") ".")))
              (section :id "extract-the-source-code-and-enter-its-directory"
                (hgroup
                  (span)
                  (h5 "Extract the source code and enter its directory"))
                (div :class "outline-text-6" :id "text-org58d3838"
                  (pre
                    (code "$ tar -xf ecl-xx.x.x.tgz
$ cd ecl-xx.x.x"))))
              (section :id "run-the-configuration-file-build-the-program-and-install-it"
                (hgroup
                  (span)
                  (h5 "Run the configuration file, build the program and install it"))
                (div :class "outline-text-6" :id "text-org547f073"
                  (pre
                    (code "$ ./configure --prefix=/usr/local
$ make                          # -jX if you have X cores
$ make install"))))
              (section :id "make-sure-the-program-is-installed-and-ready-to-run"
                (hgroup
                  (span)
                  (h5 "Make sure the program is installed and ready to run"))
                (div :class "outline-text-6" :id "text-org22f0732"
                  (pre
                    (code "$ /usr/local/bin/ecl

ECL (Embeddable Common-Lisp) 16.0.0
Copyright (C) 1984 Taiichi Yuasa and Masami Hagiya
Copyright (C) 1993 Giuseppe Attardi
Copyright (C) 2000 Juan J. Garcia-Ripoll
Copyright (C) 2015 Daniel Kochmanski
ECL is free software, and you are welcome to redistribute it
under certain conditions; see file 'Copyright' for details.
Type :h for Help.
Top level in: #<process TOP-LEVEL>.
>"))))
              (section :id "clone-vend-git-repo"
                (hgroup
                  (span)
                  (h5 "Clone "
                    (code "vend") " git repo"))
                (div :class "outline-text-6" :id "text-orgfbef3d3"
                  (p "Now that you have ECL installed, you can install vend.")
                  (p "Clone "
                    (a :href "https://github.com/fosskers/vend" "the "
                      (code "vend") " git repository") " from Github.")))
              (section :id "run-make-"
                (hgroup
                  (span)
                  (h5 "Run "
                    (code "make")))
                (div :class "outline-text-6" :id "text-org175a4b7"
                  (p "After cloning the repo, navigate into the vend directory and run the following commands:")
                  (pre
                    (code "make
make install"))
                  (p "After that, enter "
                    (code "vend") " in the command line (you may need to restart the terminal) and you should see this:")
                  (pre
                    (code "vend - Vendor your Common Lisp dependencies

Commands:
  check  [focus] - Check your dependencies for issues
  get            - Download all project dependencies into 'vendored/'
  graph  [focus] - Visualise a graph of transitive project dependencies
  init   [name]  - Create a minimal project skeleton
  repl   [args]  - Start a Lisp session with only your vendored ASDF systems
  search [term]  - Search known systems
  test   [args]  - Run all detected test systems

Flags:
  --help    - Display this help message
  --version - Display the current version of vend"))))
              (section :id "emacs-configuration"
                (hgroup
                  (span)
                  (h5 "Emacs configuration"))
                (div :class "outline-text-6" :id "text-orgfad8359"
                  (p "You need to configure Emacs to open the REPL via "
                    (code "vend") ". To open your "
                    (code "config.el") " file, type "
                    (code "C-h d c") " or "
                    (code "SPC f P") " ("
                    (code "doom/open-private-config") ") and open "
                    (code "config.el") " from the minibuffer. In your "
                    (code "config.el") " file, add:")
                  (pre
                    (code :class "lisp" "(setq sly-default-lisp 'sbcl
      sly-lisp-implementations '((sbcl  (\"vend\" \"repl\" \"sbcl\")  :coding-system utf-8-unix)
                                 (ecl   (\"vend\" \"repl\" \"ecl\")   :coding-system utf-8-unix)
                                 (abcl  (\"vend\" \"repl\" \"abcl\")  :coding-system utf-8-unix)
                                 (clasp (\"vend\" \"repl\" \"clasp\") :coding-system utf-8-unix)))")))
                (section :id "can-t-find-vend-"
                  (hgroup
                    (span)
                    (h6 "Can't find "
                      (code "vend") "?"))
                  (div :class "outline-text-7" :id "text-org207d23c"
                    (p "I had this error while testing on a fresh install of Ubuntu:")
                    (p
                      (code "File local-variables error: (doom-hook-error lisp-mode-local-vars-hook sly-editing-mode (file-missing Searching for program No such file or directory vend))"))
                    (p "That means that Emacs can't find the "
                      (code "vend") " executable. To resolve the error, I installed the package "
                      (code "exec-path-from-shell") ". ")
                    (p "To install it, type "
                      (code "SPC f P") " and open "
                      (code "config.el") ". Add this to your "
                      (code "config.el") " file:")
                    (pre
                      (code :class "elisp" "(use-package! exec-path-from-shell
  :when (or (memq window-system '(mac ns x))
            (daemonp))
  :config
  (exec-path-from-shell-initialize))"))
                    (p "And add this to your "
                      (code "packages.el") " file (in the same folder as the "
                      (code "config.el") "):")
                    (pre
                      (code :class "elisp" "(package! exec-path-from-shell)"))
                    (p "Run "
                      (code "doom sync") " in the terminal and restart doom with "
                      (code "SPC q r") " ("
                      (code "doom/restart-and-restore") ").")
                    (p "Strangely, after I did this "
                      (i "and then undid the changes") ", "
                      (code "vend") " continued to work. We all love software that just works, don't we folks?"))))))
          (section :id "project-setup"
            (hgroup
              (span)
              (h3 "Project Setup"))
            (section :id "project-files"
              (hgroup
                (span)
                (h4 "Project Files"))
              (div :class "outline-text-5" :id "text-orgbd1e14a"
                (p "Our project is called "
                  (code "almighty-kaikei") ", so "
                  (code "find-file") " ("
                  (code "C-x C-f") " or "
                  (code "SPC f f") ") "
                  (code "almighty-kaikei/almighty-kaikei.asd") " and save it. We're going to need tests later, so just make a "
                  (code "t") " directory, too.")))
            (section :id "defining-a-system"
              (hgroup
                (span)
                (h4 "Defining a System"))
              (div :class "outline-text-5" :id "text-org2fa323a"
                (p "Now that we have "
                  (code "vend") " installed, we can get the rest of our dependencies. First, we create a directory called "
                  (code "almighty-kaikei") " and define our system in "
                  (code "almighty-kaikei.asd") ".")
                (pre
                  (code :class "lisp" "(defsystem \"almighty-kaikei\"
  :author \"Micah Killian\"
  :version \"0.0.1\"
  :description \"Almighty Double-Entry Accounting Program\"
  :depends-on (#:almighty-money #:local-time #:mito #:sxql #:dbi)
  :components ((:file \"src/main\")))"))
                (p "Make sure to save the file.")))
            (section :id "download-dependencies"
              (hgroup
                (span)
                (h4 "Download Dependencies"))
              (div :class "outline-text-5" :id "text-org5d2d5b2"
                (p "Now it's time to run "
                  (code "vend") ". In the terminal, run this command:")
                (pre
                  (code "% vend get"))
                (p "You will get an error saying that "
                  (code "almighty-money") " isn't a known system. Not surprising–it's on our computer!")
                (p "To get around this, first we make the "
                  (code "vendored") " directory ourselves and make another directory inside it called "
                  (code "almighty-money") ".")
                (pre
                  (code "% mkdir vendored
% cd vendored
% mkdir almighty-money"))
                (p "Now we can make a symbolic link from where we saved "
                  (code "almighty-money") " and the directory we just made.")
                (pre
                  (code "% ln -s ~/path/to/the/original/almighty-money almighty-money"))
                (p "If you did everything right, you can run "
                  (code "vend get") " in the "
                  (code "almighty-kaikei") " directory. It will find the "
                  (code "almighty-money.asd") " file and won't attempt to search for or download it. Then it will download the rest of the dependencies as expected.")))
            (section :id "defining-the-almighty-kaikei-package"
              (hgroup
                (span)
                (h4 "Defining the "
                  (code "almighty-kaikei") " package"))
              (div :class "outline-text-5" :id "text-org5027463"
                (p "Now that we have our system setup, let's create the "
                  (code "main.lisp") " file in the project root and define the package.")
                (pre
                  (code :class "lisp" "(defpackage #:almighty-kaikei
  (:use #:cl)
  (:nicknames #:almighty-kaikei/main #:ak)
  (:local-nicknames (#:lt #:local-time)
                    (#:m #:mito)
                    (#:s #:sxql)
                    (#:d #:dbi)
                    (#:am #:almighty-money)))"))
                (p "Now's a good time to learn a bit about our dependencies."))))
          (section :id "introducing-our-dependencies"
            (hgroup
              (span)
              (h3 "Introducing Our Dependencies"))
            (section :id "-local-time-"
              (hgroup
                (span)
                (h4
                  (code "local-time")))
              (div :class "outline-text-5" :id "text-org0bcb4d4"
                (p "This is the defacto standard library for working with time, similar to Python's "
                  (code "datetime") " library. ")))
            (section :id "-mito-"
              (hgroup
                (span)
                (h4
                  (code "mito")))
              (div :class "outline-text-5" :id "text-org698bc65"
                (p "This is a SQL object-relational mapper. It's not such an opinionated ORM–we can rely on simple SQL queries for the most part when using it. It does the work of creating database tables and serializing between Lisp and SQL data.")))
            (section :id "-sxql-"
              (hgroup
                (span)
                (h4
                  (code "sxql")))
              (div :class "outline-text-5" :id "text-org6f16f2d"
                (p "This is a domain specific language for writing SQL in Lisp. I happen to really enjoy doing everything in Lisp (in a later project we will write HTML with the "
                  (code "hsx") " library, another DSL). Putting everything I'm doing right in one spot, rather than spreading it across several files, just feels right to me.")
                (p "If you're not such an enthusiast of this style, you can check out the "
                  (code "cl-yesql") " library. It is a library similar to Clojure's Yesql library. It allows you to "
                  (i "use") " SQL from Lisp, rather than "
                  (i "write") " it as with "
                  (code "sxql") ".")))
            (section :id "-dbi-"
              (hgroup
                (span)
                (h4
                  (code "dbi")))
              (div :class "outline-text-5" :id "text-org03ec96e"
                (p "This is a library for interfacing with databases. It provides functionality like SQL transactions, preparing and executing queries, etc. We need this to connect to our database and one query."))))
          (section :id "why-sql-"
            (hgroup
              (span)
              (h3 "Why SQL?"))
            (div :class "outline-text-4" :id "text-org1696148"
              (p "You might be wondering why we're using SQL instead of straight Lisp. The answer is that we want to be able to use this library as is in a production environment where we expect to be using a SQL database.")))
          (section :id "accounts"
            (hgroup
              (span)
              (h3 "Accounts"))
            (div :class "outline-text-4" :id "text-org618650f"
              (p "The first thing we need to do is provide a way of designating and knowing whether an account is a left-hand-side account or a right-hand-side account.")
              (p "Recall that there are five basic categories of account: asset, expense, income, liability, and equity. Recall also that debits and credits influence the value of LHS and RHS differently and use a different formula for calculating their balance. So when we make accounts, we are going to assign it a certain category and we'll need to know which side it is in order to calculate its balance.")
              (pre
                (code :class "lisp" ";; * ░ ACCOUNT ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
(defparameter *left-hand-side* '((\"asset\" -1) (\"expense\" -1)))
(defparameter *right-hand-side* '((\"income\" 1) (\"liability\" 1) (\"equity\" 1)))
(defparameter *account-types* (append *left-hand-side* *right-hand-side*))

(defun valid-account-category-p (account-category)
    \"A predicate that takes an account category string. Returns a list of the
category and its side, or nil.\"
  (assoc account-category *account-types* :test #'equal))"))
              (p "Just a reminder: the "
                (code ":test #'equal") " parameter in the call to "
                (code "assoc") " is necessary because strings are arrays of characters that are instantiated separately. Since the default for "
                (code ":test") " is "
                (code "#'eql") ", "
                (code "member") " usually tests if two things "
                (i "are in identical places in memory") ". "
                (code "equal") ", on the other hand, will compare the structure of its inputs.")
              (pre
                (code :class "lisp" "(eql \"hello\" \"hello\") ;; two separately instatiated arrays
                                        ; => NIL
(let ((str \"hello\"))
  (eql str str)) ;; same variable, same place in memory
                                        ; => T
(equal \"hello\" \"hello\")
                                        ; => T"))
              (p "Next, we define the "
                (code "account") " table with Mito:")
              (pre
                (code :class "lisp" "(m:deftable account ()
  ((name :col-type :text)
   (code :col-type :text)
   (category :col-type :text)
   (currency :col-type :text))
  (:table-name \"almighty_account\")
  (:documentation \"A mito macro that defines a class and SQL table for double-entry accounting
accounts.\"))

(defmethod print-object ((this account) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (with-slots (name code) this
      (format stream \"~a ~a\" name code))))

(defun side (account)
  \"A function for getting the multiplier used for determining whether a
debit/credit increases or decreases the value of an account.\"
  (second (assoc (account-category account) *account-types* :test #'equal)))"))
              (p "Mito's "
                (code "deftable") " macro expand into a "
                (code "defclass") " with a special "
                (code ":meta-class") ". The slots of the class will all have accessors prefixed with the name of the table. In this case, the accessor for the "
                (code "name") " slot is "
                (code "account-name") ". This is similar to how structures work.")
              (p "The "
                (code ":col-type") " slot option is for setting the SQL column type. In our case, we are going to use SQLite, which only really has text and integer types. If we were using PostgreSQL, we'd need to be more specific with our "
                (code ":col-types") ".")
              (p "By default Mito will set the SQL table name to the name of the Lisp class ("
                (code "account") " here), but we can manually set it with the "
                (code ":table-name") " class option. Here we are namespacing it with the "
                (code "almighty_") " prefix.")
              (p "We also define a "
                (code "print-object") " method for the "
                (code "account") " class to make it easier to understand when we print account objects to the REPL.")
              (p
                (code "side") " will be important for when we enter and retrieve data from the database: it will determine whether the amount entered or returned is negative or positive.")
              (p "We can manually construct "
                (code "account") " objects with "
                (code "make-instance") ", but we'll make a custom constructor for convenience.")
              (pre
                (code :class "lisp" "(defun make-account (name code category &optional (currency am:*default-currency*))
  \"A constructor for ACCOUNT objects. CURRENCY defaults to
ALMIGHTY-MONEY:*DEFAULT-CURRENCY*.\"
  (unless (valid-account-category-p category)
    (error \"~a is not a valid account category.\" category))
  (make-instance 'account :name name :code code :category category :currency currency))"))
              (p "That also allows us to validate the category.")
              (p "Because we need to store and retrieve accounts to and from SQL tables, we need some functions for doing that for accounts.")
              (pre
                (code :class "lisp" "(defun save-account (account)
  \"A function for saving ACCOUNT rows to a database. Returns an error if the
account already exists.\"
  (let* ((code (account-code account))
         (account-exists (m:select-dao 'account (s:where (:= :code (account-code account))))))
    (when account-exists
      (error \"An account with code ~a already exists.\" code))
    (m:insert-dao account)))

(defun get-or-create-account (account)
  \"A function that takes an ACCOUNT object and searches your database for an
ACCOUNT with the same CODE. If one exists, it's returned. Eitherwise, it's
created and returned.\"
  (let ((account-exists (m:select-dao 'account (s:where (:= :code (account-code account))))))
    (if account-exists
        (first account-exists)
        (m:insert-dao account))))

(defun get-account (account-code)
  \"A function that takes an ACCOUNT CODE and returns an ACCOUNT object. Returns an
error if an ACCOUNT with that CODE doesn't exist.\"
  (let ((account (first (m:select-dao 'account
                          (s:where (:= :code account-code))))))
    (unless account
      (error \"An account with code ~a doesn't exist.\" account-code))
    account))

(m:define-accessor account-transactions (this account)
  (m:select-dao 'transaction
    (s:where (:= :account_id (object-id this)))))"))
              (p
                (code "save-account") " is self-explanatory: We don't want duplicate accounts, so we prevent that with "
                (code "save-account") " by first doing a lookup for the "
                (code "account-code") " of the account we're trying to create.")
              (p
                (code "get-or-create-account") " is useful for times where you want to ensure an account is saved in the database without causing an error if it already exists.")
              (p
                (code "get-account") " makes it easy to get an account from a simple string containing an account code.")
              (p
                (code "define-accessor") " is a mito macro useful for reducing database hits for things like one-to-many and many-to-many relationship queries. In this case, we have a one-to-many query: getting all the transactions for an account object we pass to it.")
              (p "For all of these, we used mito and sxql to query our database. Mito provides "
                (code "select-dao") ", a macro creating a list of Lisp objects containing the queried data. The first argument is a symbol–the name of a table defined with "
                (code "mito:deftable") ". It acts as the \"SELECT * FROM [table-name]\" part of a SQL query. You then provide the rest of the query using sxql.")
              (p "You'll notice that we refer to the "
                (code "transaction") " table in "
                (code "account-transactions") ". We'll define that next.")))
          (section :id "transactions"
            (hgroup
              (span)
              (h3 "Transactions"))
            (div :class "outline-text-4" :id "text-org57abeac"
              (p "When a transaction is made, such as a sale, a return, a payment, etc. we need to create different parts (what we'll call Legs)–debits and credits–that we need a way to know which parts go together. Transactions timestamps and ids are doing to be how we refer to groups of Legs.")
              (p "Transactions are simple to implement.")
              (pre
                (code :class "lisp" ";; * ░ TRANSACTION ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
(m:deftable transaction ()
  ((date :col-type :timestamp
         :inflate #'lt:universal-to-timestamp
         :deflate #'lt:timestamp-to-universal))
  (:table-name \"almighty_transaction\"))

(defmethod print-object ((this transaction) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (with-slots (date) this
      (format stream \"~a\" date))))

(defun make-transaction (&optional (date (lt:now)))
  (make-instance 'transaction :date date))"))
              (p "Mito provides the ability to set how a slot \"inflates\" and \"deflates\"–serializes into Lisp or SQL values. We are using "
                (code "local-time") " to convert between universal times and local-time timestamps. Universal times are a simple integer–the number of seconds after 2000-3-1. While it's not strictly necessary to serialize to universal times before saving to the database, it will make future queries easier.")
              (p
                (code "make-transaction") " sets the default value of the "
                (code "date") " slot to "
                (code "local-time:now") ".")))
          (section :id "legs"
            (hgroup
              (span)
              (h3 "Legs"))
            (div :class "outline-text-4" :id "text-org919da1b"
              (p "The real fun begins with legs. Legs are like entries in the accounting books: This account was debited this amount at this time. A Transaction has to have at least two Legs–a debit and a credit–to be balanced. The whole purposes of a double-entry accounting system is to record at least two sides of a transaction to reduce errors in money tracking. If a transaction doesn't balance, that's a sign that there is an error in money tracking.")
              (p "So first, let's setup our leg table.")
              (pre
                (code :class "lisp" "(m:deftable leg ()
  ((account :col-type account)
   (side :col-type :integer)
   ;; Use LEG-ENTRY to access ENTRY-AMOUNT and ENTRY-CURRENCY as an
   ;; ALMIGHTY-MONEY:MONEY object.
   (entry-amount :col-type :integer)    ; ALMIGHTY-MONEY:MONEY object AMOUNT.
   (entry-currency :col-type :text) ; ALMIGHTY-MONEY:MONEY object CURRENCY-CODE.
   (transaction :col-type (or transaction :null)))
  (:table-name \"almighty_leg\"))

(defmethod print-object ((this leg) stream)
  (print-unreadable-object (this stream :type t :identity t)
    (let ((account (when (slot-boundp this 'account) (slot-value this 'account)))
          (transaction (when (slot-boundp this 'transaction) (slot-value this 'transaction)))) 
      (format stream \"~a ~[DEBIT~;CREDIT~] ~a\" account side (leg-entry this)))))"))
              (p "Mito allows us to assign "
                (code ":col-type") " to other tables, creating a relationship between different tables. The SQL table will actually have "
                (code "account_id") " and "
                (code "transaction_id") " columns corresponding to the ids of the other table rows.")
              (p
                (code "side") " records whether a leg is a debit or a credit. Debits will be represented with 0, and credits with 1. This makes the format string in "
                (code "print-object") " easier: we use "
                (code "tilde brackets")
                (code "~[...;...~]") " to do conditional printing. If the value of "
                (code "slot") " is 0, then the first string (zero-indexed) will be printed. If it's 1, then the string after the "
                (code "~;") " will be printed. You can do this with an arbitrary number of integer values.")
              (pre
                (code :class "lisp" "(format nil \"~[HELLO~;GOODBYE~;KONNICHIWA~;SAYONARA~]\" 0)
                                        ; => \"HELLO\"
(format nil \"~[HELLO~;GOODBYE~;KONNICHIWA~;SAYONARA~]\" 3)
                                        ; => \"SAYONARA\""))
              (p
                (code "entry-amount") " and "
                (code "entry-currency") " will record the amount of a certain currency of the leg. Ideally, we would be able to define some kind of custom "
                (code ":col-type") " for an "
                (code "almighty-money:money") " object and name it "
                (code "entry") ", but mito doesn't provide the ability to create custom column types on the Lisp side, so we will simulate it by creating an \"accessor\" for entries:")
              (pre
                (code :class "lisp" "(defgeneric leg-entry (leg)
  (:documentation \"A function for getting the ENTRY-AMOUNT and ENTRY-CURRENCY of a LEG object and returning a MONEY object.\")
  (:method ((this leg))
    (am:make-money (leg-entry-amount this) (leg-entry-currency this))))"))
              (p "Since all class slot accessors are generic functions, I am implementing "
                (code "leg-entry") " as a generic, too.")
              (p "Now we need a constructor.")
              (pre
                (code :class "lisp" "(defun make-leg (account side entry transaction)
  \"A constructor for making LEG objects. ACCOUNT can either be an ACCOUNT object or
an ACCOUNT-CODE. ENTRY must receive an ALMIGHTY-MONEY:MONEY object.

Usage example:
    (make-leg \\\"CODE000\\\" (usd 5000))\"
  (let ((account (etypecase account
                   (account account)
                   (string (get-account account)))))
    (unless (am:moneyp entry)
      (error 'type-error :expected 'am:money :datum entry))
    (make-instance 'leg :account account
                        :side side
                        :entry-amount (am:amount entry)
                        :entry-currency (am:currency-code entry)
                        :transaction transaction)))"))
              (p "We use "
                (code "etypecase") " to convert from one a string "
                (code "account-code") " to an "
                (code "account") " object if necessary. We want "
                (code "almighty-money:money") " objects passed as the "
                (code "entry") ", so we signal a "
                (code "type-error") " condition if we don't get a "
                (code "money") " object in the "
                (code "entry") " position.")
              (p "Usually we don't want to use "
                (code "make-leg") " directly. Instead, we'll provide some helper functions for making debits and credits.")
              (pre
                (code :class "lisp" "(defparameter *debit* -1)
(defparameter *credit* 1)

(defun debit (account amount)
  \"A function for constructing a LEG object with a side of *DEBIT*. The TRANSACTION
slot will be modified later before saving the row to the database.

Usage examples:
    (debit \\\"CASH000\\\" (usd 5000))
    (debit \\\"COGS000\\\" 10000)
\"
  (let ((account (etypecase account
                   (account account)
                   (string (get-account account))))
        (amount (etypecase amount
                  (integer (am:make-money amount))
                  (am:money amount))))
    (make-leg account *debit* (am:money* amount (side account) *debit*) nil)))

(defun credit (account amount)
  \"A function for constructing a LEG object with a side of *CREDIT*. The TRANSACTION
slot will be modified later before saving the row to the database.

Usage example:
    (credit \\\"REV000\\\" (usd 5000))
    (credit \\\"SLS000\\\" 10000)
\"
  (let ((account (etypecase account
                   (account account)
                   (string (get-account account))))
        (amount (etypecase amount
                  (integer (am:make-money amount))
                  (am:money amount))))
    (make-leg account *credit* (am:money* amount (side account) *credit*) nil)))"))
              (p "The "
                (code "amount") " can be either a "
                (code "almighty-money:money") " object, or a simple integer. If it's an integer, then we convert it to a "
                (code "money") " object using the "
                (code "almighty-money:*default-currency*") ". Unless you're dealing with multiple currencies, you can simply set the "
                (code "*default-currency*") " for your application and save yourself some typing.")
              (p "Remember that "
                (code "almighty-money") " only supports "
                (code "USD") " and "
                (code "JPY") " at the moment. If you want to use some other currency in "
                (code "almighty-kaikei") ", you need to add support to "
                (code "almighty-money") ".")
              (p "The important bit is the fact that we take the "
                (code "amount") " and multiply it by the side of the account "
                (i "and") " by "
                (code "*debit*") " or "
                (code "*credit*") ". An asset account that is credited should save a "
                (i "negative") " amount into the database–crediting a left-hand-side account should "
                (i "decrease") " its value. On the other hand, crediting a right-hand-side account should "
                (i "increase") " its value. ")
              (p "It might be clarifying to look at the math this way:")
              (pre
                (code :class "lisp" "(let ((lhs -1)
      (rhs 1)
      (debit -1)
      (credit 1)
      (amount 7))
  (list (* amount lhs debit)
        (* amount lhs credit)
        (* amount rhs debit)
        (* amount rhs credit)))
                                        ; => (7 -7 -7 7)"))
              (p "We're getting close to being able to make a transaction. Before we get there, we need to be able to tell debit legs from credit legs so we can see if the two sides of the transaction are balanced.")
              (pre
                (code :class "lisp" "(defun debit-p (leg)
  (= (leg-side leg) *debit*))

(defun credit-p (leg)
  (= (leg-side leg) *credit*))

(defun sides-balanced-p (legs)
  (let ((debits (sum-all-if #'debit-p legs))
        (credits (sum-all-if #'credit-p legs)))
    (= (abs (am:amount debits)) (abs (am:amount credits)))))"))
              (p "Note that we are looking at the absolute value of the debit and credit amounts. As mentioned before, crediting a left-hand-side account will result in a "
                (i "negative") " value being passed to "
                (code "make-leg") ", the same is if you debited a right-hand-side account like an income account. The effect is that we always are going to have a positive and a negative value in our transactions. Both should have the same absolute value, though.")
              (p "Finally, we make the "
                (code "transact!") " function.")
              (pre
                (code :class "lisp" "(define-condition unbalanced-transaction (error)
  ((debit-amount :initarg :debit-amount :reader debit-amount)
   (credit-amount :initarg :credit-amount :reader credit-amount)
   (message :initarg :message :reader message
            :initform \"Left and right entries of the transaction do not balance.\"))
  (:report (lambda (condition stream)
             (with-slots (debit-amount credit-amount message) condition
               (format stream \"~a LEFT: ~a | RIGHT: ~a\" message debit-amount credit-amount)))))

(defun record-transaction (transaction legs)
    \"A function for saving the data rows of a transaction to a SQL database: Legs for
the transaction, and the Transaction itself.\"
  (let ((transaction (m:insert-dao transaction)))
    (loop :for leg :in legs
          :do (m:insert-dao leg))
    transaction))

(defun transact! (&rest legs)
  \"A function for making a double-entry accounting transaction having at least one
debit and one credit leg.

If there is a LOCAL-TIME:TIMESTAMP in LEGS, the first one is used for the
transaction. If no LOCAL-TIME:TIMESTAMP is in LEGS, uses LOCAL-TIME:NOW for the
transaction.

Returns an UNBALANCED-TRANSACTION error if debits /= credits.

Usage Examples:

    (transact! (debit \\\"CASH000\\\" 5000)
               (credit \\\"REV000\\\" 5000))
    (transact! (local-time:parse-timestring \\\"2025-12-25\\\")
               (debit \\\"CASH000\\\" 5000)
               (credit \\\"REV000\\\" 5000))
\"
  (let ((date (or (find-if #'(lambda (x) (typep x 'lt:timestamp)) legs) (lt:now)))
        (legs (remove-if-not #'(lambda (x) (typep x 'leg)) legs)))
    (unless (> (length legs) 1)
      (error \"transactions must have at least two legs.\"))
    (unless (and (find-if #'debit-p legs)
                 (find-if #'credit-p legs))
      (error \"transactions must have at least one debit and one credit leg.\"))
    (let ((transaction (make-transaction date))
          (debits (sum-all-if #'debit-p legs))
          (credits (sum-all-if #'credit-p legs)))
      (unless (sides-balanced-p legs)
        (error 'unbalanced-transaction
               :debit-amount (am:amount debits)
               :credit-amount (am:amount credits)))
      (loop :for leg :in legs
            :do (setf (leg-transaction leg) transaction))
      (record-transaction transaction legs))))"))
              (p "If we look at "
                (code "transact!") " first, it can take an arbitrary number of legs. Since "
                (code "legs") " is a "
                (code "&rest") " parameter, legs will be accessible in a list. First, we check to see if any of the arguments are a "
                (code "local-time:timestamp") ". If we find one, we will use that for the transaction.")
              (p "After that, we're going to ensure first that we have at least two legs. We also ensure that there is at least one debit and one credit in there. If the sides are all balanced, we add a "
                (code "transaction") " to the transaction slot of all the "
                (code "legs") " and save the transaction to the database.")
              (p "If the sides are unbalanced, we signal an "
                (code "unbalanced-transaction") " error. Creating and signaling a condition will make it easy to take specific actions for that kind of error (in a web application, we'd like to send the error to the user in the browser, for example).")))
          (section :id "playing-around"
            (hgroup
              (span)
              (h3 "Playing around"))
            (div :class "outline-text-4" :id "text-org120f715"
              (p "Before we continue, how about we try playing with the code. In order to actually create any data, first we need to connect to our database and create tables. For right now, let's just keep things simple by establishing a long-lasting connection at toplevel.")
              (pre
                (code :class "lisp" "(mito:connect-toplevel :sqlite3 :database-name \"t/test.db\")"))
              (p "If you forgot to make a "
                (code "t") " directory for tests during project setup, you should do that before running the above code.")
              (p "Later, you can disconnect using "
                (code "disconnect-toplevel"))
              (pre
                (code :class "lisp" "(mito:disconnect-toplevel)"))
              (p "After establishing a connection, we need to create the tables. Mito provides a useful function for this purpose:")
              (pre
                (code :class "lisp" "(m:ensure-table-exists 'account)
(m:ensure-table-exists 'transaction)
(m:ensure-table-exists 'leg)"))
              (p
                (code "ensure-table-exists") " will look to see if a table exists. If it doesn't, it creates it. Otherwise, it returns "
                (code "nil") ".")
              (p "Once you have your tables, you need some accounts to mess with.")
              (pre
                (code :class "lisp" "(save-account (make-account \"Cash\" \"CSH000\" \"asset\"))
(save-account (make-account \"Sales Revenue\" \"REV000\" \"income\"))"))
              (p "We'll just make these for now.")
              (p "Let's make a sale: customer pays cash for something.")
              (pre
                (code :class "lisp" "(transact! (debit \"CSH000\" (usd 5000))
           (credit \"REV000\" (usd 5000)))
                                        ; => #<TRANSACTION 2025-11-26T04:41:56.367255Z {700805E313}>"))
              (p "We can look at the contents of the database using Emacs' built-in "
                (code "sqlite-mode") " for looking at SQLite databases. Type "
                (code "M-x sqlite-mode-open-file") " and open "
                (code "t/test.db") ". You should see the names of the tables we made and the number of rows in each table.")
              (table :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "hsides"
                (colgroup
                  (col :class "org-left")
                  (col :class "org-right"))
                (thead
                  (tr
                    (th :scope "col" :class "org-left" "Table Name")
                    (th :scope "col" :class "org-right" "Number of Rows")))
                (tbody
                  (tr
                    (td :class "org-left" "almighty"
                      (sub "account"))
                    (td :class "org-right" "2"))
                  (tr
                    (td :class "org-left" "almighty"
                      (sub "leg"))
                    (td :class "org-right" "2"))
                  (tr
                    (td :class "org-left" "almighty"
                      (sub "transaction"))
                    (td :class "org-right" "1"))))
              (p "If you switch from Evil's "
                (code "Normal-mode") " to "
                (code "Emacs-mode") ", a mode that acts exactly like if you weren't using Evil at all, then you can even press "
                (code "Enter") " on a table name and see the rows and their data.")))
          (section :id "account-balance"
            (hgroup
              (span)
              (h3 "Account Balance"))
            (div :class "outline-text-4" :id "text-org95c7031"
              (p "So we've confirmed that we're saving data to the database, but now we need to know something about the accounts: What are their current balances? For that, we need write another function.")
              (pre
                (code :class "lisp" "(defun account-legs (account &optional (as-of (universal-now)))
  \"A function for getting all the legs of an account as of a certain time,
defaulting to now.

Usage Examples:
    (account-legs \\\"CSH000\\\")
    (account-legs sales-account-object transaction-object)
    (account-legs cogs-account-object (local-time:parse-timestring \\\"2025-11-27\\\"))
\"
  (let* ((account (etypecase account
                    (account account)
                    (string (get-account account))))
         (as-of (etypecase as-of
                  (lt:timestamp (lt:timestamp-to-universal as-of))
                  (transaction (lt:timestamp-to-universal (transaction-date as-of)))
                  (string (lt:timestamp-to-universal (lt:parse-timestring as-of)))
                  (integer as-of))))
    (m:select-by-sql 'leg
                     (s:select :*
                       (s:from (:as :almighty_leg :l))
                       (s:inner-join (:as :almighty_transaction :t) :on (:= :l.transaction_id :t.id))
                       (s:where (:and (:= :l.account_id (m:object-id account))
                                      (:<= :t.date as-of)))))))

(defun account-balance (account &optional (as-of (universal-now)))
  \"A function for getting the balance of an account at a certain point in time,
defaulting to now.

Usage Example:
    (account-balance \\\"CSH000\\\")
    (account-balance sales-account-object)
\"
  (let* ((account (if (stringp account)
                      (get-account account)
                      account))
         (legs (account-legs account as-of)))
    (if legs
        (am:money* (reduce #'am:money+ (mapcar #'leg-entry legs)) (side account))
        (am:make-money 0 (account-currency account)))))"))
              (p
                (code "account-legs") " gets all the legs for an account, filtered by the date of the transaction. "
                (code "account-balance") " sums up the debit and credit legs of an account then subtracts debits from credits or credits from debits depending on the type of account.")))
          (section :id "long-term-problems"
            (hgroup
              (span)
              (h3 "Long-term problems"))
            (section :id "many-transactions"
              (hgroup
                (span)
                (h4 "Many transactions"))
              (div :class "outline-text-5" :id "text-orgbff9fb1"
                (p "Everything is chugging along quite nicely, but there's a problem. If you're familiar with database query optimization, you probably already wrote a nasty post about me on X when we made the "
                  (code "account-balance") " function.")
                (p "\"You absolute clown, you're getting "
                  (i "all") " of the leg rows for a certain account, then passing them to Lisp to do the summation of their debits and credits? Delete this book.\"")
                (p "This is a reasonable position to have. With very few leg rows, this isn't really such a big deal. However, as the number of leg rows grows, the slower the operation will become. We can confirm that by making some mock data and then running "
                  (code "account-balance") " on it.")
                (pre
                  (code :class "lisp" ";; Create lots of mock data, saving to disk. If you already had some test data
;; of your own, you can delete the .db file before running this.
;; 
;; This will take some time. Emacs will freeze while this process runs.
(with-coa
  (let ((cash (ak:get-account \"CSH000\"))
        (sale (ak:get-account \"SLS000\"))
        (date (lt:now))
        (times 100000)
        (amount 500))
    (dotimes (i times)
      (ak:transact! (ak:debit cash (am:usd amount))
                    (ak:credit sale (am:usd amount))
                    date))))
;; Run ACCOUNT-BALANCE on mock data
(with-coa (ak:account-balance \"CSH000\"))"))
                (p "If you run "
                  (code "account-balance") " now, there will be a noticable lag between executing and receiving the return value.")
                (p "Let's do some basic profiling:")
                (pre
                  (code :class "lisp" "(time (with-coa (ak:account-balance \"CSH000\")))"))
                (p
                  (code "time") " is a function for basic, dirty profiling. It will show how long it takes to complete a task along with some other information. If you run it in the REPL, you
should get a result that looks something like this:")
                (pre :class "example" :id "org1702d4c" "Evaluation took:
  2.944 seconds of real time
  2.950970 seconds of total run time (2.826497 user, 0.124473 system)
  [ Real times consist of 0.256 seconds GC time, and 2.688 seconds non-GC time. ]
  [ Run times consist of 0.254 seconds GC time, and 2.697 seconds non-GC time. ]
  100.24% CPU
  7 forms interpreted
  1,680,876,464 bytes consed")
                (p "Well, what happens if we add a check for the account balance of the sales account?")
                (pre
                  (code :class "lisp" "(time (with-coa
        (ak:account-balance-slow \"CSH000\")
        (ak:account-balance-slow \"SLS000\")))"))
                (p "The result:")
                (pre :class "example" :id "orgc0b4007" "Evaluation took:
  5.879 seconds of real time
  5.890071 seconds of total run time (5.630371 user, 0.259700 system)
  [ Real times consist of 0.511 seconds GC time, and 5.368 seconds non-GC time. ]
  [ Run times consist of 0.509 seconds GC time, and 5.382 seconds non-GC time. ]
  100.19% CPU
  7 forms interpreted
  3,361,540,272 bytes consed")
                (p "As suspected, "
                  (code "account-balance") " is going to be a problem in the long-term. We need to optimize."))))
          (section :id "optimization"
            (hgroup
              (span)
              (h3 "Optimization"))
            (div :class "outline-text-4" :id "text-orgc251b89"
              (p "We could try getting back less data in our "
                (code "account-legs") " function just as an experiment.")
              (pre
                (code :class "diff" "(defun account-legs (account &optional (as-of (universal-now)))
  \"A function for getting all the legs of an account as of a certain time,
defaulting to now.

Usage Examples:
    (account-legs \\\"CSH000\\\")
    (account-legs sales-account-object transaction-object)
    (account-legs cogs-account-object (local-time:parse-timestring \\\"2025-11-27\\\"))
\"
  (let* ((account (etypecase account
                    (account account)
                    (string (get-account account))))
         (as-of (etypecase as-of
                  (lt:timestamp (lt:timestamp-to-universal as-of))
                  (transaction (lt:timestamp-to-universal (transaction-date as-of)))
                  (string (lt:timestamp-to-universal (lt:parse-timestring as-of)))
                  (integer as-of))))
    (m:select-by-sql 'leg
-                    (s:select :*
+                    (s:select (:entry_amount :entry_currency :side)
                       (s:from (:as :almighty_leg :l))
                       (s:inner-join (:as :almighty_transaction :t) :on (:= :l.transaction_id :t.id))
                       (s:where (:and (:= :l.account_id (m:object-id account))
                                      (:<= :t.date as-of)))))))"))
              (p "If we run our little test again (running "
                (code "account-balance") " twice) we get:")
              (pre :class "example" :id "orgd2661ed" "Evaluation took:
  1.296 seconds of real time
  1.300803 seconds of total run time (1.235469 user, 0.065334 system)
  [ Real times consist of 0.112 seconds GC time, and 1.184 seconds non-GC time. ]
  [ Run times consist of 0.112 seconds GC time, and 1.189 seconds non-GC time. ]
  100.39% CPU
  7 forms interpreted
  782,634,128 bytes consed")
              (p "This confirms that we are definitely being bogged down by all this object construction and deconstruction (notice the dramatic decrease in bytes consed), but focusing what data we get from the database isn't remotely enough to fix our problem.")
              (p "This is a common problem when using ORMs: if we try to do serious calculations by first serializing all of the data into our backend, we will inevitably see performance drops.")
              (p "What we really want is for our database to do all the work of marshaling the data and summing up the "
                (code "amount") " column of all of the "
                (code "leg") " rows, and just receive a single value–the balance! That way we aren't building up and breaking down a bunch of Lisp objects just to do a little bit of math."))
            (section :id "making-a-query-with-cl-dbi-"
              (hgroup
                (span)
                (h4 "Making a Query With "
                  (code "cl-dbi")))
              (div :class "outline-text-5" :id "text-orgedb2bc8"
                (p "This is where "
                  (code "cl-dbi") " will be necessary. It will allow us to execute a query manually.")
                (p
                  (code "dbi") " works by first preparing a query for a certain database connection.")
                (pre
                  (code :class "lisp" "(dbi:prepare database-connection query)"))
                (p "The query is a simple string. You can "
                  (code "yield") " one from a SQL statement prepared with "
                  (code "sxql") ".")
                (pre
                  (code :class "lisp" "(sxql:select :*
  (sxql:from :almighty_account)
  (sxql:where (:= :almighty_account.id 5)))
                                        ; => #<SXQL-STATEMENT:
                                        ; SELECT * FROM almighty_account
                                        ; WHERE (almighty_account.id = 5)>


(sxql:yield
 (sxql:select :*
   (sxql:from :almighty_account)
   (sxql:where (:= :almighty_account.id 5))))
                                        ; => \"SELECT * FROM almighty_account WHERE (almighty_account.id = ?)\", (5)"))
                (p "Notice that we receive two values from "
                  (code "yield") ". This is to make it easy to use "
                  (code "dbi") " to prepare the statement–sanitizing user input to prevent SQL inject attacks.")
                (pre
                  (code :class "lisp" "(with-chart-of-accounts
  (multiple-value-bind (query binds)
      (sxql:yield
       (sxql:select :*
         (sxql:from :almighty_account)
         (sxql:where (:= :almighty_account.id 5))))
    (dbi:prepare mito:*connection* query)))
                                        ; => #<DBD.SQLITE3:DBD-SQLITE3-QUERY {70078DE2C3}>"))
                (p "We pass the prepared query and binds to "
                  (code "execute") " to get a result.")
                (pre
                  (code :class "lisp" "(with-chart-of-accounts
  (multiple-value-bind (query binds)
      (sxql:yield
       (sxql:select :*
         (sxql:from :almighty_account)
         (sxql:where (:= :almighty_account.id 5))))
    (dbi:execute (dbi:prepare mito:*connection* query)
                 binds)))
                                        ; => #<DBD.SQLITE3:DBD-SQLITE3-QUERY {70081DDED3}>"))
                (p "We have two options: we can take one row of the result at a time and do something with it, or we can receive the whole thing all at once and then do something with all of the data.")
                (pre
                  (code :class "lisp" "(with-chart-of-accounts
  (multiple-value-bind (query binds)
      (sxql:yield
       (sxql:select :*
         (sxql:from :almighty_account)
         (sxql:where (:= :almighty_account.id 5))))
    (dbi:fetch (dbi:execute (dbi:prepare mito:*connection* query)
                            binds))))
                                        ; => (:|id| 5 :|name| \"General Expenses\" :|code| \"GEN000\" :|category| \"expense\"
                                        ; :|currency| \"USD\" :|created_at| \"2025-11-30 03:59:02.662347Z\" :|updated_at|
                                        ; \"2025-11-30 03:59:02.662347Z\")

(with-chart-of-accounts
  (multiple-value-bind (query binds)
      (sxql:yield
       (sxql:select :*
         (sxql:from :almighty_account)
         (sxql:where (:= :almighty_account.id 5))))
    (dbi:fetch-all (dbi:execute (dbi:prepare mito:*connection* query)
                                binds))))
                                        ; => ((:|id| 5 :|name| \"General Expenses\" :|code| \"GEN000\" :|category| \"expense\"
                                        ;  :|currency| \"USD\" :|created_at| \"2025-11-30 03:59:02.662347Z\" :|updated_at|
                                        ;  \"2025-11-30 03:59:02.662347Z\"))
                                        ; notice the extra parentheses"))
                (p "And that's the basics of using "
                  (code "dbi") ". "
                  (code "multiple-value-bind") " is the go-to macro for working with forms that return multiple values like "
                  (code "sxql:yield") ".")))
            (section :id "rename-to-account-balance-"
              (hgroup
                (span)
                (h4 "Rename to "
                  (code "account-balance")))
              (div :class "outline-text-5" :id "text-org54c7640"
                (p "Now that you know basically how we're going to use "
                  (code "dbi") " to get our data, we can get started on the task at hand.")
                (p "First, rename the "
                  (code "account-balance") " function to "
                  (code "account-balance-slow") ". We'll use it to test later.")
                (pre
                  (code :class "diff" "+ (defun account-balance-slow (account &optional (as-of (universal-now)))
+    ....)"))
                (p "Don't forget to export it with "
                  (code "sly-export-symbol-at-point") " ("
                  (code "C-c x") " with the cursor on function name).")))
            (section :id "writing-the-query"
              (hgroup
                (span)
                (h4 "Writing the query"))
              (div :class "outline-text-5" :id "text-org8f65379"
                (p "Now we need to get to the SQL query. What we want to do is get all of the legs for a certain account (as we do with "
                  (code "account-legs") ") and then sum up the "
                  (code "entry_amount") " columns of all those legs. In the end, we need an "
                  (code "amount") " and a "
                  (code "currency") " so we can properly make a "
                  (code "money") " object later, so we're not going to "
                  (code "(select :*)") ", we're going to return a sum.")
                (pre
                  (code :class "lisp" "(with-coa
  (let ((account (ak:get-account \"CSH000\")))
    (multiple-value-bind (query binds)
        (s:yield
         (s:select ((:as (:sum :leg.entry_amount) :amount))
           (s:from (:as :almighty_account :account))
           (s:inner-join (:as :almighty_leg :leg) :on (:= :leg.account_id :account.id))
           (s:where (:= :account.id (m:object-id account)))))
      (d:fetch-all (d:execute (d:prepare m:*connection* query)
                              binds)))))
                                        ; => ((:|amount| 100000000))"))
                (p "That looks good, but let's just be sure with a smaller test.")
                (pre
                  (code :class "lisp" "(with-chart-of-accounts                 ; Using in-memory database
  ;; Use cash and revenue accounts.
  (let ((cash (ak:get-account \"CSH000\"))
        (revenue (ak:get-account \"RMR000\")))
    ;; Make transactions.
    (ak:transact! (ak:debit cash 5000)
                  (ak:credit revenue 5000))
    (ak:transact! (ak:debit cash 5000)
                  (ak:credit revenue 5000))
    (ak:transact! (ak:debit cash 5000)
                  (ak:credit revenue 5000))
    ;; Reverse the transactions.
    (ak:transact! (ak:credit cash 5000)
                  (ak:debit revenue 5000))
    (ak:transact! (ak:credit cash 5000)
                  (ak:debit revenue 5000))
    (ak:transact! (ak:credit cash 5000)
                  (ak:debit revenue 5000))
    (multiple-value-bind (query binds)
        (s:yield
         (s:select ((:as (:sum :leg.entry_amount) :amount))
           (s:from (:as :almighty_account :account))
           (s:inner-join (:as :almighty_leg :leg) :on (:= :leg.account_id :account.id))
           ;; Check cash balance.
           (s:where (:= :account.id (m:object-id cash)))))
      (d:fetch-all (d:execute (d:prepare m:*connection* query)
                              binds)))))
                                        ;  => ((:|amount| 0))"))
                (p "At this point, we just need to add a way to filter for the "
                  (code "as-of") " transaction date, add the "
                  (code "entry_currency") " to the return values, and turn this into a proper function.")
                (pre
                  (code :class "lisp" "(defun sql-account-balance (account as-of)
  (multiple-value-bind (query binds)
      (s:yield (s:select ((:as (:coalesce (:sum :leg.entry_amount) 0) :amount)
                          (:as :leg.entry_currency :currency))
                 (s:from (:as :almighty_account :account))
                 (s:inner-join (:as :almighty_leg :leg) :on (:= :leg.account_id :account.id))
                 (s:inner-join (:as :almighty_transaction :tran) :on (:= :leg.transaction_id :tran.id))
                 (s:where (:and (:= :account.id (m:object-id account))
                                (:<= :tran.date as-of)))
                 (s:group-by :leg.entry_currency)))
    (let ((result (d:fetch (d:execute (d:prepare m:*connection* query)
                                      binds))))
      (am:make-money (second result) (fourth result)))))"))
                (p "Then let's make a new "
                  (code "account-balance") " function:")
                (pre
                  (code :class "lisp" "(defun account-balance (account &optional (as-of (universal-now)))
  \"A function for getting the balance of an account at a certain point in time,
defaulting to now.

Usage Example:
    (account-balance \\\"CSH000\\\")
    (account-balance sales-account-object)
\"
  (let ((account (if (stringp account)
                     (get-account account)
                     account))
        (as-of (etypecase as-of
                 (lt:timestamp (lt:timestamp-to-universal as-of))
                 (transaction (lt:timestamp-to-universal (transaction-date as-of)))
                 (string (lt:timestamp-to-universal (lt:parse-timestring as-of)))
                 (integer as-of))))
    (sql-account-balance account as-of)))"))
                (p "Now we can test it out against the slower version.")
                (pre
                  (code :class "lisp" "(time (with-coa (ak:account-balance-slow \"CSH000\")))"))
                (p "Results in:")
                (pre :class "example" :id "org2ced51e" "Evaluation took:
  0.675 seconds of real time
  0.677828 seconds of total run time (0.638939 user, 0.038889 system)
  [ Real times consist of 0.060 seconds GC time, and 0.615 seconds non-GC time. ]
  [ Run times consist of 0.060 seconds GC time, and 0.618 seconds non-GC time. ]
  100.44% CPU
  7 forms interpreted
  391,349,920 bytes consed")
                (p "Now for the optimized version:")
                (pre
                  (code :class "lisp" "(time (with-coa (ak:account-balance \"CSH000\")))"))
                (p "Results in:")
                (pre :class "example" :id "org942c18f" "Evaluation took:
  0.051 seconds of real time
  0.044906 seconds of total run time (0.033658 user, 0.011248 system)
  88.24% CPU
  7 forms interpreted
  262,080 bytes consed")
                (p "Much better."))))
          (section :id "example-usage"
            (hgroup
              (span)
              (h3 "Example Usage"))
            (div :class "outline-text-4" :id "text-orgb130547"
              (p "Now that we have the basic functionality of our library finished, it's time to kick the tires a bit and see what it can do. First, we'll make it a REPL-based application, but later we'll upgrade it into a CLI script in the chapter on "
                (code "DEPLOYING") "."))
            (section :id "setup"
              (hgroup
                (span)
                (h4 "Setup"))
              (div :class "outline-text-5" :id "text-org6b131da"
                (p "In the project root directory, make a new directory named "
                  (code "example") ".")
                (p "We need a system for this example, so let's set that up.")
                (pre
                  (code :class "lisp" ";; almighty-kaikei/almighty-kaikei-examples.asd
(defsystem \"almighty-kaikei-examples\"
  :author \"Micah Killian\"
  :version \"0.0.1\"
  :description \"Examples of how to use the Almighty-Kaikei library.\"
  :depends-on (#:almighty-money #:almighty-kaikei #:local-time #:mito #:sxql #:dbi)
  :components ((:module \"examples\"
                :serial t
                :components ((:file \"db\")
                             (:file \"views\")))))"))
                (p "Before we work on our queries, it'll help to know the motivation for getting the data by looking at our views.")))
            (section :id "views"
              (hgroup
                (span)
                (h4 "Views"))
              (div :class "outline-text-5" :id "text-org34f97d2"
                (p "First, let's get our package set up in "
                  (code "examples/views.lisp") ".")
                (pre
                  (code :class "lisp" "(defpackage #:examples/views
  (:use #:cl)
  (:local-nicknames (#:db #:examples/db)
                    (#:lt #:local-time)
                    (#:ak #:almighty-kaikei)
                    (#:am #:almighty-money)))
(in-package #:examples/views)"))
                (p "In here, we're going to create some simple reports. The first one is a general ledger. The most basic kind of accounting report is the general ledger. It simply contains all of the transactions made, detailing the two sides and accounts affected.")
                (pre
                  (code :class "lisp" ";; * ░ VIEWS ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

(defun format-general-ledger (from to)
  (let ((transaction-count (db:transaction-count-for-period from to))
        (legs (db:legs-for-period from to)))
    (format t \"~&Showing ~a transactions.~%\" transaction-count)
    (format t \"~a~30t~a~60t~a~%\" \"Transaction #\" \"Debits\" \"Credits\")
    (loop :for leg :in legs
          :for i = 0
          :if (and (ak:debit-p leg) (= 0 i))
            :do (format t \"~a~30t~30a~%\" (ak:leg-transaction-id leg) (list (am:format-money nil (ak:leg-entry leg)) (ak:account-name (ak:leg-account leg))))
          :else
            :do (format t \"~a~60t~30a~%\" (ak:leg-transaction-id leg) (list (am:format-money nil (ak:leg-entry leg)) (ak:account-name (ak:leg-account leg))))
          :finally (format t \"~%\"))))"))
                (p "This function will")))
            (section :id "queries"
              (hgroup
                (span)
                (h4 "Queries"))
              (div :class "outline-text-5" :id "text-org717e67e"
                (p "Let's make "
                  (code "examples/db.lisp") " and define the package:")
                (pre
                  (code :class "lisp" "(defpackage #:examples/db
  (:use #:cl)
  (:local-nicknames (#:lt #:local-time)
                    (#:m #:mito)
                    (#:s #:sxql)
                    (#:ak #:almighty-kaikei)
                    (#:am #:almighty-money)))

(in-package #:examples/db)"))
                (p "This package will be in charge of helping us work with the database: establishing a connection, select queries, and other \"business logic\" queries.")
                (pre
                  (code :class "lisp" ";; * ░ UTILS ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
(defmacro with-universals (bindings &body body)
  `(let ,(loop :for (var val) :in bindings
               :collect (list var
                              `(lt:timestamp-to-universal
                                (if (stringp ,val)
                                    (lt:parse-timestring ,val)
                                    ,val))))
     ,@body))

(defmacro with-db-connection (&optional (db-name \"sqlite.db\") &body body)
  `(let ((mito:*connection* (dbi:connect-cached :sqlite3 :database-name ,db-name)))
     (unwind-protect (progn ,@body)
       (dbi:disconnect mito:*connection*))))

(defun db-path ()
  (let ((db-path (asdf:system-relative-pathname :almighty-kaikei-examples #p\"data/sqlite.db\")))
    (ensure-directories-exist db-path)
    db-path))

(defun initialize-application-database ()
  (with-db-connection (db-path)
    (let (
          ;; Asset Accounts
          (cash-account                 (ak:make-account \"Cash\"                \"CSH000\" \"asset\"))
          (inventory-account            (ak:make-account \"Inventory\"           \"INV000\" \"asset\"))
          (accounts-receivable-account  (ak:make-account \"Accounts Receivable\" \"REC000\" \"asset\"))
          ;; Expense Accounts
          (cost-of-goods-sold-account   (ak:make-account \"Cost of Goods Sold\"  \"CGS000\" \"expense\"))
          (general-expenses-account     (ak:make-account \"General Expenses\"    \"GEN000\" \"expense\"))
          ;; Income Account
          (sales-revenue-account        (ak:make-account \"Sales Revenue\"       \"SLS000\" \"income\"))
          ;; Liability Account
          (tax-account                  (ak:make-account \"Tax Payable\"         \"TAX000\" \"liability\"))
          ;; Equity Accounts
          (room-revenue-account         (ak:make-account \"Room Revenue\"        \"RMR000\" \"equity\")))
      (m:ensure-table-exists 'ak:account)
      (m:ensure-table-exists 'ak:transaction)
      (m:ensure-table-exists 'ak:leg)

      ;; Asset
      (ak:get-or-save-account cash-account)
      (ak:get-or-save-account inventory-account)
      (ak:get-or-save-account accounts-receivable-account)
      ;; Expense
      (ak:get-or-save-account cost-of-goods-sold-account)
      (ak:get-or-save-account general-expenses-account)
      ;; Income
      (ak:get-or-save-account sales-revenue-account)
      ;; Liablity
      (ak:get-or-save-account tax-account)
      ;; Equity
      (ak:get-or-save-account room-revenue-account))))"))
                (p "To begin with, we have "
                  (code "with-universals") ". This converts "
                  (code "local-time:timestamp") " objects into Common Lisp "
                  (code "universal times") ". According to the HyperSpec ("
                  (a :href "https://cl-community-spec.github.io/pages/Universal-Time.html" "https://cl-community-spec.github.io/pages/Universal-Time.html") "),")
                (blockquote
                  (p "Universal time is an absolute time represented as a single non-negative integer—the number of seconds since midnight, January 1, 1900 GMT (ignoring leap seconds). "))
                (p "Earlier, we wrote the "
                  (code "transaction")
                  (code "deftable") " in order to automatically serialize timestamps to universal times as well as the reverse. For our queries, if we ever want to filter rows by timestamps, we might be tempted to use "
                  (code "local-time:timestamp") " objects directly, but they don't work. We could try converting timestamps into a format that SQLite understands that still looks like a timestamp, but for our purposes, a simple integer will do.")
                (p
                  (code "with-db-connection") " is a simple macro for running some code with a database connection. "
                  (code "mito:*connection*") " needs to be set to a "
                  (code "dbi") " connection, which we set to sqlite3. "
                  (code "unwind-protect") " is a macro that will always run the form of its second argument even if the first argument form has some error. It's useful for any necessary cleanup after a form does some work, perhaps saving some data to a file, but then fails in the middle. In our case, we use it to close the connection.")
                (p
                  (code "db-path") " ensures that we have a place to put our database file (if the "
                  (code "data") " folder isn't already created, for example).")
                (p
                  (code "initialize-application-database") " is for ensuring the "
                  (code "almighty-kaikei") " tables are available and our Chart of Accounts is created. When we first start our application, we need to run this function.")))
            (section :id "general-ledger"
              (hgroup
                (span)
                (h4 "General ledger"))
              (div :class "outline-text-5" :id "text-org4932191"
                (p "The most basic kind of accounting report is the General Ledger. It simply contains all of the transactions made, detailing the two sides and accounts affected. ")
                (pre
                  (code :class "lisp" "(defun sales (from to)
  (with-universals ((from from)
                    (to to))
    (let* ((transaction-count (second (first (m:retrieve-by-sql
                                              (s:select ((:count :*))
                                                (s:from :almighty_transaction)
                                                (s:inner-join :almighty_leg :on (:= :almighty_leg.transaction_id :almighty_transaction.id))
                                                (s:where (:and
                                                          (:= :almighty_leg.account_id (m:object-id (ak:get-account \"RMR000\")))
                                                          (:>= :almighty_transaction.date from)
                                                          (:<= :almighty_transaction.date to))))))))
           (sales-account (ak:get-account \"RMR000\"))
           (legs (m:select-dao 'ak:leg
                   (m:joins 'ak:transaction)
                   (s:where (:and
                             (:= :almighty_leg.account-id (m:object-id sales-account))
                             (:>= :almighty_transaction.date from)
                             (:<= :almighty_transaction.date to)))
                   (s:order-by :almighty_transaction.id))))
      (format t \"~&Showing ~a transactions.~%\" transaction-count)

      (format t \"~a~15t~a~45t~a~%\" \"Transaction #\" \"Debits\" \"Credits\")
      (loop :for leg :in legs
            :for i = 0
            :if (and (ak:debit-p leg) (= 0 i))
              :do (format t \"~a~15t~a~%\" (ak:leg-transaction-id leg) (list (am:format-money nil (ak:leg-entry leg)) (ak:account-name (ak:leg-account leg))))
            :else
              :do (format t \"~a~45t~a~%\" (ak:leg-transaction-id leg) (list (am:format-money nil (ak:leg-entry leg)) (ak:account-name (ak:leg-account leg))))
            :finally (format t \"~%\")))))"))
                (p
                  (code "general-ledger") " uses a "
                  (code "with-universals") " macro for taking two "
                  (code "local-time") " \"timestrings\" and converting them to "
                  (i "universal times") ". When making queries like this, universal times are easier to use than trying to work with local-time timestamps–that's why we set up transaction dates to inflate/deflate (serialize/deserialize) between timestamps and universal times earlier.")
                (p "Here's "
                  (code "with-universals") ":")
                (pre
                  (code :class "lisp" "(defmacro with-universals (bindings &body body)
  `(let ,(loop :for (var val) :in bindings
               :collect (list var
                             `(lt:timestamp-to-universal
                               (if (stringp ,val)
                                   (lt:parse-timestring ,val)
                                   ,val))))
     ,@body))"))
                (p "It takes "
                  (code "bindings") ", a list of lists (as in a "
                  (code "let") " form) with a variable and the value to bind it to. It takes a string and converts it to a universal time. A simple but useful for reports like "
                  (code "general-ledger") " that want to show data for a certain time range.")
                (p "The "
                  (code "transaction-count") " variable is bound to the result of a simple SQL query that gets the number of transactions for a time range.")
                (p
                  (code "legs") " is similar, with one small difference: it uses "
                  (code "mito:joins") "–the equivalent of typing in "
                  (code "inner-join :table_name :on ...") ". Since we're using "
                  (code "select-dao") ", we can use the "
                  (code "joins") " macro to save a little bit of typing.")
                (p "After that, we just go through the list of leg objects and "
                  (code "format") " them. The "
                  (code "TILDE t") " format directive here is useful for making tabular output.  "
                  (code "~nt") " is "
                  (i "padded") " to the "
                  (code "nth") " space. "
                  (code "~15t") " means \"If you aren't at column 15 yet, move there before continuing to format.\"")
                (p "Output of "
                  (code "general-ledger") " should look like this:")
                (pre :class "example" :id "org1ec3913" "Showing 300 transactions.
Transaction #                 Debits                        Credits
1                             ($50.00 Cash)                 
1                                                           ($50.00 Room Revenue)         
2                             ($50.00 Cash)                 
2                                                           ($50.00 Room Revenue)         
3                             ($50.00 Cash)                 
3                                                           ($50.00 Room Revenue)         
4                             ($50.00 Cash)                 
4                                                           ($50.00 Room Revenue)         ")
                (p "We're implementing these reports using "
                  (code "format") ", but we could just as easily use HTML to format them for the web. For simplicity we're keeping the query definition and the \"front end\" together in one function.")))
            (section :id "trial-balance"
              (hgroup
                (span)
                (h4 "Trial balance"))
              (div :class "outline-text-5" :id "text-org7d2981f"
                (p "A trial balance shows the net total balance of all accounts."))))
          (section :id "tests"
            (hgroup
              (span)
              (h3 "Tests"))
            (div :class "outline-text-4" :id "text-org53c62fe"
              (p "At this point, we need to start thinking about testing a little more seriously. We want to be able to test not only our ability to get account balances, but to do so at specific times. We need to be able to set up an environment for our tests that won't affect the other tests. What we want is essentially a lexical database environment.")
              (p "So now we need to get systematic with testing, and for that we'll use "
                (code "lisp-unit2")))
            (section :id "getting-started-with-lisp-unit2-"
              (hgroup
                (span)
                (h4 "Getting Started with "
                  (code "lisp-unit2")))
              (div :class "outline-text-5" :id "text-org6fdb54d"
                (p "To get started, open a buffer in "
                  (code "t/main.lisp") " and add this:")
                (pre
                  (code :class "lisp" "(defpackage #:almighty-kaikei/test/main
  (:use #:cl)
  (:local-nicknames (#:lu #:lisp-unit2)
                    (#:lt #:local-time)
                    (#:ak #:almighty-kaikei)
                    (#:am #:almighty-money)))
(in-package #:almighty-kaikei/test/main)"))))
            (section :id "making-a-test"
              (hgroup
                (span)
                (h4 "Making a test"))
              (div :class "outline-text-5" :id "text-org02ed76a"
                (p "Now let's make a simple test to learn how to use "
                  (code "lisp-unit2") ".")
                (pre
                  (code :class "lisp" "(lu:define-test testing-testing-123 ()
  (lu:assert-eql 123 123))"))
                (p "This test will pass if (eql 123 123) or fail otherwise. To run the test, we can use "
                  (code "lu:run-tests") ".")
                (pre
                  (code :class "lisp" "(lu:run-tests)"))
                (p "It should return something like this:")
                (pre :class "example" :id "org904be00" "#<LU:TEST-RESULTS-DB Tests:(1) Passed:(1) Failed:(0) Errors:(0) Warnings:(0) {700C0D01D3}>")))
            (section :id "tags"
              (hgroup
                (span)
                (h4 "Tags"))
              (div :class "outline-text-5" :id "text-org3e13a1d"
                (p "You can group sets of tests using \"tags\". You can specify one or more tags.")
                (pre
                  (code :class "lisp" "(lu:define-test test-my-test (:tags '(math)))
(lu:define-test test-my-test2 (:tags '(accounting reporting)))"))
                (p "To run tests with a certain tag or tags:")
                (pre
                  (code :class "lisp" "(lu:run-tests :tags '(accounting))"))))
            (section :id "contexts"
              (hgroup
                (span)
                (h4 "Contexts"))
              (div :class "outline-text-5" :id "text-orgb67cd36"
                (p "In addition to tags, you can specify contexts. Contexts are lexical environments for tests. You can specify them on a per-test basis or for all tests in the call to "
                  (code "lu:run-tests") ".")
                (pre
                  (code :class "lisp" "(lu:define-test test-my-context-text (:contexts (list #'with-my-context #'with-another-context)))
(lu:run-tests :run-contexts (list #'lu:with-failure-debugging-context))"))
                (p
                  (code "lu:with-failure-debugging-context") " is a built-in context that will open the debugger if an assertion fails, giving you an opportunity to look at the data.")
                (p "Contexts are functions that return a "
                  (code "funcall") " to a body function.")
                (pre
                  (code :class "lisp" "(defun with-my-context (body-fn)
  (let ((...))
    (do-thing)
    (do-another-thing)
    (funcall body-fn)))"))
                (p "Every test that uses "
                  (code "with-my-custom-context") " will include this lexical environment.")))
            (section :id "making-testing-contexts"
              (hgroup
                (span)
                (h4 "Making testing contexts"))
              (div :class "outline-text-5" :id "text-org11c4acc"
                (p "In order to run our tests, we need to be able to do several things for every test:")
                (ol :class "org-ol"
                  (li "Connect and disconnect to a database.")
                  (li "Create and destroy our database tables.")
                  (li "Create a standard set of accounts (called a Chart of Accounts)."))
                (p "First, let's create the function and macro for connecting to the database:")
                (pre
                  (code :class "lisp" "(defmacro with-db-connection (&optional (db-name \"sqlite.db\") &body body)
  `(let ((mito:*connection* (dbi:connect-cached :sqlite3 :database-name ,db-name)))
     (unwind-protect (progn ,@body)
       (dbi:disconnect mito:*connection*))))

(defun test-db-path ()
  (let ((db-path (asdf:system-relative-pathname :almighty-kaikei #p\"t/test.db\")))
    (ensure-directories-exist db-path)
    db-path))"))
                (p "Next, we need to make a \"chart of accounts\" lexical database environment–\"fixtures\". To reiterate, there are five different categories of accounts, and a chart of accounts may have several of each category. In our context, we will make the SQL tables and then add the accounts to the database.")
                (pre
                  (code :class "lisp" "(defun with-chart-of-accounts-context (body-fn)
  (with-db-connection #p\":memory:\"
    (m:ensure-table-exists 'ak:account)
    (m:ensure-table-exists 'ak:transaction)
    (m:ensure-table-exists 'ak:leg)
    (let (
          ;; Asset Accounts
          (cash-account                 (ak:make-account \"Cash\"                \"CSH000\" \"asset\"))
          (inventory-account            (ak:make-account \"Inventory\"           \"INV000\" \"asset\"))
          (accounts-receivable-account  (ak:make-account \"Accounts Receivable\" \"REC000\" \"asset\"))
          ;; Expense Accounts
          (cost-of-goods-sold-account   (ak:make-account \"Cost of Goods Sold\"  \"CGS000\" \"expense\"))
          (general-expenses-account     (ak:make-account \"General Expenses\"    \"GEN000\" \"expense\"))
          ;; Income Account
          (sales-revenue-account        (ak:make-account \"Sales Revenue\"       \"SLS000\" \"equity\"))
          ;; Liability Account
          (liability-account            (ak:make-account \"Liabilities\"         \"LIA000\" \"liability\"))
          ;; Equity Accounts
          (room-revenue-account         (ak:make-account \"Room Revenue\"        \"RMR000\" \"equity\")))
      ;; Asset
      (ak:get-or-save-account cash-account)
      (ak:get-or-save-account inventory-account)
      (ak:get-or-save-account accounts-receivable-account)
      ;; Expense
      (ak:get-or-save-account cost-of-goods-sold-account)
      (ak:get-or-save-account general-expenses-account)
      ;; Income
      (ak:get-or-save-account sales-revenue-account)
      ;; Liablity
      (ak:get-or-save-account liability-account)
      ;; Equity
      (ak:get-or-save-account room-revenue-account)
      (funcall body-fn))))"))
                (p "This is the context that we'll use for our "
                  (code "lisp-unit2") " tests. It uses SQLite's in-memory database, making it temporary for the duration of each connection.")
                (p "We also want a way to mess around outside of the tests, so let's make a macro that lets us do that:")
                (pre
                  (code :class "lisp" "(defmacro with-coa (&body body)
  `(with-db-connection (test-db-path)
     (let (
           ;; Asset Accounts
           (cash-account                 (ak:make-account \"Cash\"                \"CSH000\" \"asset\"))
           (inventory-account            (ak:make-account \"Inventory\"           \"INV000\" \"asset\"))
           (accounts-receivable-account  (ak:make-account \"Accounts Receivable\" \"REC000\" \"asset\"))
           ;; Expense Accounts
           (cost-of-goods-sold-account   (ak:make-account \"Cost of Goods Sold\"  \"CGS000\" \"expense\"))
           (general-expenses-account     (ak:make-account \"General Expenses\"    \"GEN000\" \"expense\"))
           ;; Income Account
           (sales-revenue-account        (ak:make-account \"Sales Revenue\"       \"SLS000\" \"income\"))
           ;; Liability Account
           (liability-account            (ak:make-account \"Liabilities\"         \"LIA000\" \"liability\"))
           ;; Equity Accounts
           (room-revenue-account         (ak:make-account \"Room Revenue\"        \"RMR000\" \"equity\")))
       (m:ensure-table-exists 'ak:account)
       (m:ensure-table-exists 'ak:transaction)
       (m:ensure-table-exists 'ak:leg)

       ;; Asset
       (ak:get-or-save-account cash-account)
       (ak:get-or-save-account inventory-account)
       (ak:get-or-save-account accounts-receivable-account)
       ;; Expense
       (ak:get-or-save-account cost-of-goods-sold-account)
       (ak:get-or-save-account general-expenses-account)
       ;; Income
       (ak:get-or-save-account sales-revenue-account)
       ;; Liablity
       (ak:get-or-save-account liability-account)
       ;; Equity
       (ak:get-or-save-account room-revenue-account)
       ,@body)))"))
                (p "Using "
                  (code "with-coa") " will cause a .db file to be created that we can inspect and run queries on.")))
            (section :id "making-tests"
              (hgroup
                (span)
                (h4 "Making tests"))
              (div :class "outline-text-5" :id "text-org1ea8d29"
                (p "So now it's time to make some tests. We'll start with some warm-up tests just to see if we can do basic stuff.")
                (pre
                  (code :class "lisp" ";; * ░ Accounting ░░░░░░░░░░░░░░░░░░░░
;; For our accounting scenarios, we will imagine the accounting for a hotel.
;; The hotel sells stays, has a restaurant, and rooms have snack fridges.
;; They take cash and credit.

(lu:define-test tests-get-account (:tags '(accounting) :contexts (list #'with-chart-of-accounts-context))
  (lu:assert-true (ak:get-account \"CSH000\")))

(lu:define-test test-account-code (:tags '(accounting) :contexts (list #'with-chart-of-accounts-context))
  (lu:assert-equal \"CSH000\" (ak:account-code (ak:get-account \"CSH000\"))))

(lu:define-test test-make-account (:tags '(accounting) :contexts (list #'with-chart-of-accounts-context))
  (lu:assert-true (typep (ak:make-account \"Expenses\" \"EXP\" \"expense\") 'ak:account)))

(lu:define-test test-get-or-create-account (:tags '(accounting) :contexts (list #'with-chart-of-accounts-context))
  (let ((new-account (ak:get-or-save-account (ak:make-account \"More Assets\" \"ASS001\" \"asset\"))))
    (lu:assert-no-error 'error (ak:account-code (ak:get-account \"ASS001\")))))"))
                (p "These tests check to see if everything related to getting or making accounts is working. We can run these with "
                  (code "(lu:run-tests)") ". If everything passes, great. If you have a failing test, run "
                  (code "(lu:run-tests :run-contexts (list #'lu:with-failure-debugging-context))") ".")
                (p "Assuming you have everything settled with the above tests, let's start getting into the meat of things. Remember that the purpose of these tests is to confirm both that the system is working and that we understand how the accounting works.")
                (pre
                  (code :class "lisp" ";; * ░ Transactions ░░░░░░░░░░░░░░░░░░░░
;; ** ░ Card Sale of Service ░░░░░░░░░░░░░░░░░░░░
;; This scenario involves tracking:
;; 1. A card payment going to our Receivable account.
;; 2. Card payment provider expense.
;; 3. The revenue after paying that expense.

(lu:define-test test-transact!-card-sale-of-service (:tags '(accounting transactions)
                                                     :contexts (list #'with-chart-of-accounts-context))

  (ak:transact! (ak:debit \"REC000\" 4500)
                (ak:debit \"GEN000\" 500)
                (ak:credit \"RMR000\" 5000))
  (lu:assert-equality #'am:money= (ak:account-balance (ak:get-account \"REC000\")) (am:usd 4500))
  (lu:assert-equality #'am:money= (ak:account-balance (ak:get-account \"GEN000\")) (am:usd 500))
  (lu:assert-equality #'am:money= (ak:account-balance (ak:get-account \"RMR000\")) (am:usd 5000)))

(lu:define-test test-transact!-unbalanced-sides (:tags '(accounting transactions)
                                                 :contexts (list #'with-chart-of-accounts-context))
  (lu:assert-error 'ak:unbalanced-transaction (ak:transact! (ak:debit \"CSH000\" 5000)
                                                            (ak:credit \"RMR000\" 4000))))

(lu:define-test test-transact!-not-enough-legs (:tags '(accounting transactions)
                                                :contexts (list #'with-chart-of-accounts-context))
  (lu:assert-error 'simple-error (ak:transact! (ak:debit \"CSH000\" 5000))))"))
                (p "Recall that we're considering the example of a hotel's finances. In the "
                  (code "card-cale-of-service") " scenario, we receive a credit card payment for a room. We credit our Room Revenue income account 5000 cents ($50)–increasing its value. The card payment service charges us 500 cents ($5) that is debited to our general expenses account–adding to its value. Finally we credit our Receivable account the remainder of the money, 4500 cents ($45), which is what we expect to actually have after paying our expenses.")
                (p "Since the test begins with no transactions in the database, all three accounts should have account balances equal the amounts they've been increased by.")
                (p "The second test just checks that our "
                  (code "unbalanced-transaction") " condition works properly. The third checks to make sure we receive an error if there aren't at least two legs in the transaction.")
                (p "We'll make a couple more tests just to confirm that our understanding of the principles of double-entry accounting are solid.")
                (pre
                  (code :class "lisp" ";; ** ░ Cash Sale of Goods ░░░░░░░░░░░░░░░░░░░░
;; This scenario involves tracking:
;; 1. The cash paid.
;; 2. The revenue earned.
;; 3. The cost of the good sold.
;; 4. The reduction in inventory of the good.
;; 5. The total profit (revenue - cogs)

(lu:define-test test-cash-sale-of-goods (:tags '(accounting transactions)
                                         :contexts (list #'with-chart-of-accounts-context))
  (let ((cash-account \"CSH000\")
        (sales-account \"SLS000\")
        (cogs-account \"CGS000\")
        (inventory-account \"INV000\"))
    (ak:transact! (ak:debit cash-account 5000) (ak:credit sales-account 5000)
                  (ak:debit inventory-account 3000) (ak:credit cogs-account 3000))
    (lu:assert-equality #'am:money= (ak:account-balance cash-account) (am:usd 5000))
    (lu:assert-equality #'am:money= (ak:account-balance sales-account) (am:usd 5000))
    (lu:assert-equality #'am:money= (ak:account-balance cogs-account) (am:usd -3000)) ; Crediting expense account decreases its value.
    (lu:assert-equality #'am:money= (ak:account-balance inventory-account) (am:usd 3000))
    (lu:assert-equality #'am:money=
                        (am:money+ (ak:account-balance sales-account) (ak:account-balance cogs-account))
                        (am:usd 2000))))

;; ** ░ Credit Card Sale of Hotel Stay ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
;; This scenario involves tracking:                         
;; 1. Accounts Receivable (Asset) for credit card payment.  
;; 2. The room revenue (Equity) earned.                     
;; 3. Credit card processing fees (Expense)                 

(lu:define-test test-debit-card-sale-of-hotel-stay (:tags '(accounting transactions)
                                                    :contexts (list #'with-chart-of-accounts-context))
  (let ((receivable-asset-account \"REC000\")
        (general-expense-account \"GEN000\")
        (room-revenue-account \"RMR000\")
        (cash-asset-account \"CSH000\"))
    ;; Sale is made--debit Receivable Account for card payment, debit general expenses for card fees.
    ;; Credit Room Revenue for sale.
    (ak:transact! (ak:credit room-revenue-account (am:usd 5000))
                  (ak:debit general-expense-account (am:usd 500))
                  (ak:debit receivable-asset-account (am:usd 4500)))
    (lu:assert-equality #'am:money=
                        (ak:account-balance receivable-asset-account)
                        (am:usd 4500))
    (lu:assert-equality #'am:money=
                        (ak:account-balance general-expense-account)
                        (am:usd 500))
    (lu:assert-equality #'am:money=
                        (ak:account-balance room-revenue-account)
                        (am:usd 5000))
    ;; Bank fulfills payment; clear receivable and add to cash
    (ak:transact! (ak:debit cash-asset-account (am:usd 4500))
                  (ak:credit receivable-asset-account (am:usd 4500)))
    (lu:assert-equality #'am:money=
                        (ak:account-balance receivable-asset-account)
                        (am:usd 0))
    (lu:assert-equality #'am:money=
                        (ak:account-balance cash-asset-account)
                        (am:usd 4500))))"))
                (p "Call "
                  (code "(lu:run-tests)") " again. Everything pass? Good. Let's test one more important feature: the "
                  (code "as-of") " parameter of "
                  (code "account-balance") ".")
                (pre
                  (code :class "lisp" "(lu:define-test test-account-legs-as-of
    (:tags '(query) :contexts (list #'with-chart-of-accounts-context))
  (let ((cash (ak:get-account \"CSH000\"))
        (sale (ak:get-account \"SLS000\"))
        (date (lt:now)))
    (let ((1st (ak:transact! (ak:debit cash (am:usd 500))
                             (ak:credit sale (am:usd 500))
                             date))
          (2nd (ak:transact! (ak:debit cash (am:usd 500))
                             (ak:credit sale (am:usd 500))
                             (lt:timestamp+ date 10 :sec))))
      (lu:assert-number-equal 1 (length (ak:account-legs cash (lt:timestamp+ (ak:transaction-date 2nd) -1 :sec)))))))"))
                (p "Here we make two transactions, one ten seconds after the other. We set the "
                  (code "as-of") " parameter to one second "
                  (i "before") " the second transaction. We expect for the cash account that we should only have one leg–the one for the first transaction. Call "
                  (code "(lu:run-tests)") " and check if this passes. It does? Good."))))))
      (section :id "deploying"
        (hgroup
          (span)
          (h1 "DEPLOYING"))
        (div :class "outline-text-2" :id "text-org9c81f6c")
        (section :id "executable-cli-app"
          (hgroup
            (span)
            (h2 "EXECUTABLE CLI APP"))
          (div :class "outline-text-3" :id "text-org648444c"
            (p "Common Lisp can make executables. There are several ways to make executables. Which method you use will depend on the nature of your project. "))
          (section :id "tic-tac-toe"
            (hgroup
              (span)
              (h3 "Tic-tac-toe"))
            (div :class "outline-text-4" :id "text-org95674cf"
              (p "Our tic-tac-toe game is the simplest example. We will use SBCL's "
                (code "sb-ext:save-lisp-and-die") " function.")
              (pre
                (code :class "lisp" "(sb-ext:save-lisp-and-die #P\"path/to/the/binary\"                    
                          :top-level \"ttt:play-game-with-computer\" 
                          :executable t)                          "))
              (p "The first argument is the path to the binary you want to create.")
              (p "The second argument is the package-qualified name of the \"main\" function you want to call when running the executable.")
              (p "The third argument tells SBCL to return an executable, not a Lisp image.")
              (p "In order to call "
                (code "sb-ext:save-lisp-and-die") ", you of course need to have some code compiled and loaded; however, you can't run it in a Sly REPL. Instead, you need to run it in the command line. That would look like this:")
              (pre
                (code "sbcl --load tic-tac-toe.lisp --eval \"(sb-ext:save-lisp-and-die #p\\\"ttt\\\" :toplevel #'ttt:play-game-with-computer :executable t)\""))
              (p "First, load the lisp file, then evaluate the call to "
                (code "sb-ext:save-lisp-and-die") ".")
              (p "You can make the process more accessible to users by creating a Makefile to run the above:")
              (pre
                (code :class "makefile" "build:
        sbcl --load tic-tac-toe.lisp \\
                 --eval \"(sb-ext:save-lisp-and-die #p\\\"ttt\\\" :toplevel #'ttt:play-game-with-computer :executable t)\""))
              (p "But, if you try to run this, you'll have a problem: the package "
                (code "ttt") " doesn't exist because we hadn't learned about packages before making the tic-tac-toe game. So let's go add a package definition to the tic-tac-toe.lisp file.")
              (pre
                (code :class "lisp" "(defpackage #:ttt
  (:use :cl)
  (:export #:play-game-with-computer))
(in-package #:ttt)"))
              (p "With that, you can run "
                (code "make build") ". Once the executable is created, you can type "
                (code "./ttt") " in your command line and enjoy a spirited game of tic-tac-toe against the computer.")))
          (section :id "almighty-kaikei"
            (hgroup
              (span)
              (h3 "almighty-kaikei"))
            (div :class "outline-text-4" :id "text-org581447e"
              (p "For very simple programs, the above is good enough. However, if your project has an "
                (code ".asd") " file, then you have the option of making an executable from a whole system.")
              (p "The simple version of this process looks like this:")
              (ul :class "org-ul"
                (li "Configure the system to build an executable.")
                (li "Load the system with "
                  (code "asdf:load-system") ".")
                (li "Run "
                  (code "asdf:make") " on the loaded system."))
              (p "This process uses ASDF, rather than calling "
                (code "sb-ext:save-lisp-and-die") " directly."))
            (section :id "configure-system"
              (hgroup
                (span)
                (h4 "Configure System"))
              (div :class "outline-text-5" :id "text-orgd307da2"
                (p "First, we need to configure the system. In our case, we want to load the "
                  (code "almighty-kaikei-examples") " system, since it actually "
                  (i "uses") " the library.")
                (pre
                  (code :class "lisp" "(defsystem \"almighty-kaikei-examples\"
  :author \"Micah Killian\"
  :version \"0.0.1\"
  :description \"Almighty Double-Booking Accounting Program\"
  :depends-on (#:almighty-money #:almighty-kaikei #:local-time #:mito #:sxql #:dbi #:dbd-sqlite3)
  :components ((:file \"reports\"))
  :build-operation \"program-op\"
  :build-pathname \"kaikei\"
  :entry-point \"almighty-kaikei-reports:main\")")))
              (section :id "add-driver"
                (hgroup
                  (span)
                  (h5 "Add driver"))
                (div :class "outline-text-6" :id "text-orge25cbb8"
                  (p "The first change is in the "
                    (code ":depends-on") " list: "
                    (code "cl-dbi") " will compile and load a driver for your database if you don't already have one loaded. From "
                    (a :href "https://github.com/fukamachi/cl-dbi?tab=readme-ov-file#installation" "the cl-dbi readme") ":")
                  (blockquote
                    (p "cl-dbi will load another system on the fly depending on your database's driver:")
                    (p ":dbd-sqlite3
:dbd-mysql
:dbd-postgres")
                    (p "You must reference the required one in your system definition if you plan to build an executable (and if you plan to run it on a machine where Quicklisp is not installed)."))
                  (p "It will cause a nasty error in your executable if you don't add either the "
                    (code ":dbd-sqlite3") ", "
                    (code ":dbd-mysql") ", or "
                    (code ":dbd-postgres") " systems to your system "
                    (code ":depends-on") ". And don't forget to "
                    (code "vend get") " when you add this system!")))
              (section :id "add-build-configuration"
                (hgroup
                  (span)
                  (h5 "Add build configuration"))
                (div :class "outline-text-6" :id "text-orgb9b8d93"
                  (p "Next, we add "
                    (code ":build-operation") " with the argument "
                    (code "program-op") ". ASDF has a number of "
                    (a :href "https://asdf.common-lisp.dev/asdf.html#Predefined-operations-of-ASDF-1" "predefined operations (https://asdf.common-lisp.dev/asdf.html#Predefined-operations-of-ASDF-1)") " for compiling and loading systems. The default op is "
                    (code "load-op") ". If you don't set it to "
                    (code "program-op") ", you will simply load the system and get dropped into a REPL. "
                    (code "program-op") " tells ASDF to make an executable out of the system.")
                  (p
                    (code ":build-pathname") " is the name of the executable file we want to create.")
                  (p
                    (code ":entry-point") " is the package-qualified name of the function we want to call when running the executable. More on this in just a second. For now, our system is all set up with a few changes."))))
            (section :id "load-make-system"
              (hgroup
                (span)
                (h4 "Load & Make System"))
              (div :class "outline-text-5" :id "text-org81bd2c5"
                (p "Now we just need to load and make the system. We're going to do things a little differently this time, though. What we'll do is make a "
                  (code "build.lisp") " file and simply load that file. This will give us more flexibility for orchestrating the next steps. First, the Makefile:")
                (pre
                  (code :class "makefile" "kaikei: build.lisp almighty-kaikei-examples.asd main.lisp reports.lisp
        sbcl --load build.lisp "))
                (p "The filenames after "
                  (code "kaikei:") " tell make to run the code that follows if any of the files listed are updated.")
                (p "Now for the "
                  (code "build.lisp") " file:")
                (pre
                  (code :class "lisp" "(require :asdf)

;; Force ASDF to only look here for systems.
(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))

(progn
  (format t \"--- LOADING SYSTEM ---~%\")
  (asdf:load-system :almighty-kaikei-examples)
  (format t \"--- SYSTEM LOADED ---~%\"))

(progn
  (asdf:make \"almighty-kaikei-examples\")
  (format t \"--- DONE ---~%\")
  (quit))"))
                (p "We "
                  (code "require") " ASDF in order to use it here. "
                  (code "require") " loads a package if it isn't already loaded. Usually ASDF is built into modern implementations of Common Lisp, but this ensures that ASDF is loaded in cases where your implementation doesn't load it (or have it built in).")
                (p "The call to "
                  (code "asdf:initialize-source-registry") " is to ensure that when ASDF loads the system that it resolves dependences using only the systems within our project directory.")
                (p
                  (code "asdf:load-system") " is the same operation performed with the REPL shortcut "
                  (code ", load-system") ".")
                (p
                  (code "asdf:make") " runs the "
                  (code ":build-operation") " we configured earlier.")
                (p "We run "
                  (code "quit") " because if for some reason you are running "
                  (code "make") " and there are no changes, then no executable will be made and you'll be thrown into the REPL. "
                  (code "quit") " is there to conveniently kick us out immediately.")))
            (section :id "add-almighty-kaikei-examples-main-function"
              (hgroup
                (span)
                (h4 "Add "
                  (code "almighty-kaikei-examples:main") " function"))
              (div :class "outline-text-5" :id "text-org47c3b8f"
                (p "The "
                  (code ":entry-point") " in the system is set to a function called "
                  (code "main") ", but we haven't written that function yet. For now, we're going to make this very simple:")
                (pre
                  (code :class "lisp" ";; add
(defun main ()
  (with-coa (general-ledger \"2025-12-1\" \"2025-12-5\")))"))))
            (section :id "run-make-"
              (hgroup
                (span)
                (h4 "Run "
                  (code "make")))
              (div :class "outline-text-5" :id "text-org6a2b188"
                (p "Now, all you need to do is run "
                  (code "make") " to create the binary.")))))
        (section :id "deploying-fuka-stack-web-app"
          (hgroup
            (span)
            (h2 "DEPLOYING FUKA STACK WEB APP"))
          (div :class "outline-text-3" :id "text-org9bd3bc7"
            (p "We are going to deploy a simple hello world web app to a Ubuntu 24.04 LTS box. We'll make it using the Fuka Stack: "
              (code "clack") ", "
              (code "lack") ", "
              (code "woo") ", and "
              (code "myway") ". We'll use "
              (code "vend") " for our dependencies. After we setup a small hello world example of using the Fuka Stack, we'll setup the server, upload the code, build an executable, and then run that executable as a service via "
              (code "systemd") ".")
            (p "If you use Ubuntu 24.04 LTS on an x86-64 processor–the same OS and hardware as the server–you "
              (i "might") " even be able to skip the step of uploading the code to the server, building the executable on your machine and then uploading the executable to the server.")
            (p "The purpose is not to get into the weeds of how to make a web framework. When we get to the part about the code, we'll focus on the parts relevant to deploying, not to setting up the framework."))
          (section :id "requirements"
            (hgroup
              (span)
              (h3 "Requirements"))
            (div :class "outline-text-4" :id "text-org4aa1bff"
              (ul :class "org-ul"
                (li "An SSH Key.")
                (li "A server with Ubuntu 24.04 LTS.")
                (li
                  (code "vend") " installed on your computer."))))
          (section :id "the-server"
            (hgroup
              (span)
              (h3 "The Server"))
            (div :class "outline-text-4" :id "text-org968fa2d")
            (section :id "add-non-root-user"
              (hgroup
                (span)
                (h4 "Add non-root user"))
              (div :class "outline-text-5" :id "text-org55e6d1a"
                (p "First, SSH into your server. Once you're in, run this command, replacing "
                  (code "yourusername") " with one of your choosing.")
                (pre
                  (code "adduser yourusername"))
                (p "Then, give that username sudo permissions.")
                (pre
                  (code "usermod -aG sudo yourusername"))
                (p "Then you need to set up SSH for the new username. Exit the SSH session. On your local machine, run this command in the terminal:")
                (pre
                  (code "ssh-copy-id yourusername@your-server-ip"))))
            (section :id "install-roswell-dependencies-libev4-caddy"
              (hgroup
                (span)
                (h4 "Install Roswell dependencies, libev4, Caddy"))
              (div :class "outline-text-5" :id "text-orgb1820e0"
                (p "In the terminal, run this command:")
                (pre
                  (code "sudo apt install git build-essential automake autoconf libcurl4-openssl-dev zlib1g-dev libev4 -y"))))
            (section :id "install-roswell"
              (hgroup
                (span)
                (h4 "Install Roswell"))
              (div :class "outline-text-5" :id "text-org6399059"
                (p "We use Roswell so that we can get the latest version of SBCL or use some other implementation later. While Common Lisp isn't getting update, implementations update their compilers, adding optimizations, etc.")
                (p "In the terminal, run these commands:")
                (pre
                  (code "git clone -b release https://github.com/roswell/roswell.git
cd roswell
sh bootstrap
./configure
make
sudo make install
cd ..
rm -rf roswell "))))
            (section :id "install-caddy"
              (hgroup
                (span)
                (h4 "Install Caddy"))
              (div :class "outline-text-5" :id "text-org912cc7a"
                (p "Caddy is a reverse proxy that will let use run our Woo server on port 5000 but be accessible in the browser from "
                  (a :href "https://the-ip-address-or-domain-name" "https://the-ip-address-or-domain-name") " rather than "
                  (a :href "https://the-ip-address-or-domain-name:5000" "https://the-ip-address-or-domain-name:5000") ". I like it because it takes care of SSL certificates automatically, so when you use it, "
                  (a :href "https://" "https://") " just works.")
                (p "Run the following commands:")
                (pre
                  (code "sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list
sudo apt update
sudo apt install caddy"))
                (p "Open up "
                  (code "/etc/caddy/Caddyfile") " with your favorite editor (such as "
                  (code "nano") "). There "
                  (i "should") " already be some default stuff there. Create a block like this:")
                (pre
                  (code "your-server-ip-address-or-domain-name {
    reverse_proxy localhost:5000
}"))
                (p "You need to enable and start the service after your changes.")
                (pre
                  (code "sudo systemctl enable caddy
sudo systemctl start caddy"))))
            (section :id "-optional-change-hostname"
              (hgroup
                (span)
                (h4 "(Optional) Change Hostname"))
              (div :class "outline-text-5" :id "text-org7094dc7"
                (p "The server terminal should show something like "
                  (code "yourusername@localhost") ". You can change "
                  (code "localhost") " to something of your choosing.")
                (p "Run this command:")
                (pre
                  (code "sudo hostnamectl set-hostname almightylisp"))
                (p "Then use our favorite editor to "
                  (code "sudo") " open "
                  (code "/etc/hosts") ". Add "
                  (code "127.0.0.1  yourpreferredhostname") ", save, and close. Then run "
                  (code "exec $SHELL") " or just restart the SSH session.")
                (p "With that, you are ready to move on to the code."))))
          (section :id "the-code"
            (hgroup
              (span)
              (h3 "The Code"))
            (div :class "outline-text-4" :id "text-org325bec0")
            (section :id "project-setup"
              (hgroup
                (span)
                (h4 "Project Setup"))
              (div :class "outline-text-5" :id "text-orgca3c2d0"
                (p "First, on your local machine, make a new project root directory called "
                  (code "fuka-stack") ". Inside it, you can also make another directory called "
                  (code "src") ".")))
            (section :id "build-script-service-makefile"
              (hgroup
                (span)
                (h4 "Build Script, Service, Makefile"))
              (div :class "outline-text-5" :id "text-orgd3e5d8a")
              (section :id "build-script"
                (hgroup
                  (span)
                  (h5 "Build Script"))
                (div :class "outline-text-6" :id "text-org6b42216"
                  (p "The script for building the executable is going to look similar to our previous "
                    (code "build.lisp") " file. However, this one is a "
                    (code ".ros") " file, used by Roswell to build the executable.")
                  (p "Open a buffer in "
                    (code "fuka-stack/fuka-stack-app.ros") ", add the following code, and then save:")
                  (pre
                    (code :class "lisp" "#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 \"$@\"
|#

;; Roswell script header (above) makes this executable

(defpackage #:fuka-stack-script
  (:use #:cl)
  (:export #:main))
(in-package #:fuka-stack-script)

;; Load your system
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :ignore-inherited-configuration))

(asdf:load-system \"fuka-stack\")

;; Environment variable utilities.
(defun get-env-int (env default)
  (let ((env (uiop:getenv env)))
    (if env
        (parse-integer env :junk-allowed t)
        default)))

(defun get-env (env default)
  (or (uiop:getenv env) default))

(defun envp (env)
  (if (string-equal (uiop:getenvp env) \"true\")
      t
      nil))

(defun main ()
  (let ((host (get-env \"HOST\" \"127.0.0.1\"))
        (port (get-env-int \"PORT\" 5000))
        (debugp (envp \"DEBUGP\"))) 
    (handler-case
        (progn
          (clack:clackup (lack:builder fuka-stack:*component*) :server :woo :address host :port port :debug debugp)
          (sleep most-positive-fixnum))
      (error (c)
        (format *error-output* \"Aborting. ~a ~&\" c)
        (force-output *error-output*)
        (uiop:quit 1)))))"))
                  (p "The "
                    (code ".ros") " file needs to have its own "
                    (code "main") " function. For the purposes of deploying a web app, all you need to understand is this "
                    (code "main") " function. That function uses "
                    (code "uiop:getenv") " via "
                    (code "get-env") " and "
                    (code "get-env-int") " helpers to–you guessed it–get the environment variables "
                    (code "HOST") " and "
                    (code "PORT") ", defaulting to "
                    (code "127.0.01") " and "
                    (code "5000") ". If the "
                    (code "DEBUGP") " environment variable is set to "
                    (code "true") ", then the "
                    (code "clack:clackup")
                    (code ":debug") " setting will be set to "
                    (code "t") ", otherwise "
                    (code "nil") ". Later we'll change the variables on the production environment, but this allows you to try this code out on your computer.")
                  (p
                    (code "sleep") " is a function we haven't seen before. Usually, after loading the system and calling "
                    (code "clack:clackup") " to start the server, it will remain on (usually you save it to a parameter so you can turn it off with "
                    (code "clack:stop") ").")
                  (p "When you run the executable built with this code, if you don't include the call to "
                    (code "(sleep most-positive-fixnum)") ", systemd will assume your executable is done working and stop the service, turning off your server.")))
              (section :id "service-env-files"
                (hgroup
                  (span)
                  (h5 "Service & Env files"))
                (div :class "outline-text-6" :id "text-org6cd2836"
                  (p "We will need to run our executable as a service. If we don't, the server will block our terminal session, and if there is an error, it will simply crash the server. Systemd can be configured to restart any crashed services, and the service won't block our terminal session, either.")
                  (p "Open a buffer in "
                    (code "fuka-stack/fuka-stack-app.service") ", add the following code, and then save.")
                  (pre
                    (code :class "makefile" "[Unit]
Description=Fuka Base App
After=network.target

[Service]
Type=simple
# Replace with the username you create when setting up the server
User=micah 
# ...same here
WorkingDirectory=/home/micah/.local/bin/
EnvironmentFile=/etc/systemd/system/fuka-stack-app.env

# ...and here
ExecStart=/home/micah/.local/bin/fuka-stack

TimeoutStartSec=120
TimeoutStopSec=10
KillMode=process
KillSignal=SIGTERM

Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target"))
                  (p "We also need environment variables set up, which we can do in a separate file (notice "
                    (code "EnvironmentFile") " above).")
                  (p "Open a buffer in "
                    (code "fuka-stack/fuka-stack-app.env") ", add the following code, and then save.")
                  (pre
                    (code :class "makefile" "HOST=0.0.0.0
PORT=5000
DEBUGP=false"))))
              (section :id "makefile"
                (hgroup
                  (span)
                  (h5 "Makefile"))
                (div :class "outline-text-6" :id "text-orgf3523fa"
                  (p "The makefile includes both "
                    (code "make") " and "
                    (code "make install") " commands. Open a buffer in "
                    (code "fuka-stack/makefile") " and save this code inside:")
                  (pre
                    (code :class "makefile" "fuka-stack: build.lisp *.asd *.lisp src/*.lisp
        ros build fuka-stack-app.ros
install:
        mv fuka-stack-app ~/.local/bin/fuka-stack-app
        sudo cp fuka-stack-app.service /etc/systemd/system/fuka-stack-app.service
        sudo cp fuka-stack-app.env /etc/systemd/system/fuka-stack-app.env
        sudo systemctl daemon-reload
        sudo systemctl enable fuka-stack-app.service
run:
        sudo systemctl start fuka-stack-app.service"))
                  (p
                    (code "make") " will run Roswell's version of SBCL to build the executable. If you want to use/test this script locally you need to install Roswell.")
                  (p
                    (code "make install") " will move the "
                    (code "fuka-stack-app") " executable, setup the systemd service and environment variables, and then start the service."))))
            (section :id "hello-world-code"
              (hgroup
                (span)
                (h4 "Hello World Code"))
              (div :class "outline-text-5" :id "text-org86813c0"
                (p "Now that we have our server set up, and we have all we need to build an executable and start it as a service, we can make our hello-world web app."))
              (section :id "system"
                (hgroup
                  (span)
                  (h5 "System"))
                (div :class "outline-text-6" :id "text-orgb639317"
                  (p "In the project root directory, make a file called "
                    (code "fuka-stack.asd") " and place the below code into it.")
                  (pre
                    (code :class "lisp" "(defsystem \"fuka-stack\"
  :author \"Micah Killian <micah@almightylisp.com>\"
  :maintainer \"Micah Killian <micah@almightylisp.com>\"
  :description \"Fuka Stack hello world deploy example\"
  :license \"MIT\"
  :version \"0.1\"
  :depends-on (:clack
               :woo 
               :myway
               :lack
               :lack-component
               :lack-request
               :lack-response)
  :components ((:module \"src\"
                :components
                ((:file \"main\")))))"))
                  (p "Notice that we don't have any of the build options like we had for the previous executable CLI app. That's because we'll be using Roswell to build the executable.")
                  (p "Save this file and then be sure to run "
                    (code "vend get") " to download the dependencies into your project.")))
              (section :id "main"
                (hgroup
                  (span)
                  (h5 "Main"))
                (div :class "outline-text-6" :id "text-org83c9661"
                  (p "Finally, we have the main body of the app. I won't be explaining the mechanics of the stack–that is the subject of a future book.")
                  (p "Open a buffer in "
                    (code "fuka-stack/src/main.lisp") ", add this code, and save it.")
                  (pre
                    (code :class "lisp" "(defpackage #:fuka-stack
  (:use #:cl)
  (:nicknames #:fuka-stack/main))
(in-package #:fuka-stack/main)

;; Lack setup
(defclass fuka-component (lack/component::lack-component)
  ((routes :initarg :routes :accessor component-routes :initform (myway:make-mapper))))

(defparameter *component*
  (make-instance 'fuka-component))

(defparameter *request* nil)
(defparameter *response* nil)

(defmethod lack/component:call ((component fuka-component) env)
  (let ((*request* (lack/request:make-request env)))
    (multiple-value-bind (response foundp)
        (myway:dispatch (slot-value component 'routes)
                        (lack/request:request-path-info *request*)
                        :method (lack/request:request-method *request*))
      (if foundp
          (if (functionp response)
              response
              (destructuring-bind (status headers body) response
                (lack/response:finalize-response (lack/response:make-response status headers body))))
          (lack/response:finalize-response (lack/response:make-response 404 '(:content-type \"text/html\") '(\"Not found\")))))))

;; Clack response utility
(defun http-response (body &key (code 200) (headers nil))
  (let ((headers (append `(:content-type \"text/html; charset=utf-8\"
                           :content-length ,(length body))
                         headers)))
    `(,code ,headers (,body))))


;; Myway routing utilities
(defun make-endpoint (fn)
  (lambda (params)
    (funcall (symbol-function fn) params)))

(defun route (method routing-rule endpoint &optional (mapper (component-routes *component*)))
  (myway:connect mapper
                 routing-rule
                 (make-endpoint endpoint)
                 :method method))


;; Index controller and route
(defun index (params)
  (declare (ignore params))
  (http-response (format nil
                         \"Hello, world! This is the path: ~a. This is the HTTP request method: ~a.\"
                         (lack/request:request-path-info *request*)
                         (lack/request:request-method *request*))))

(route :GET \"/\" 'index)"))))))
          (section :id "the-deploy"
            (hgroup
              (span)
              (h3 "The Deploy"))
            (div :class "outline-text-4" :id "text-orgef1d44e"
              (p "You are one the precipice of a new beginning, my friend. Your Common Lisp web development life arch begins with the next two steps."))
            (section :id "uploading-code"
              (hgroup
                (span)
                (h4 "Uploading Code"))
              (div :class "outline-text-5" :id "text-org952fd8f"
                (p "It's time to upload your code to the server. If you are comfortable using git, you could use it now. For the sake of simplicity, we'll just upload with the "
                  (code "rsync") " command.")
                (p "From your local machine, run this command:")
                (pre
                  (code "rsync -avz --progress ~/path/to/your/local/version/of/fuka-stack yourusername@domain-name-or-ip-address-to-your-server:/home/yourusername/fuka-stack"))))
            (section :id "run-make-commands"
              (hgroup
                (span)
                (h4 "Run "
                  (code "make") " Commands"))
              (div :class "outline-text-5" :id "text-orgeb8c18a"
                (p "Now all you need to do is ssh back into your server, navigate to the fuka-stack directory, and run the "
                  (code "make") ", "
                  (code "make install") ", and "
                  (code "make run") " commands in order. If all goes well (and we know things always go well with deploys), you should now be able to access your hello-world example in your browser with "
                  (a :href "https://your-domain-name-or-server-ip-address" "https://your-domain-name-or-server-ip-address") ".")
                (p "Congratulations, you have successfully deployed a Common Lisp web app!"))))
          (section :id "summary"
            (hgroup
              (span)
              (h3 "Summary"))
            (div :class "outline-text-4" :id "text-org9de5f56"
              (p "There are a number of things we could have done differently. We could have skipped Roswell and just used whatever version of SBCL was available on the Ubuntu 24.04 LTS package repository. We could have compiled the latest one from source. We could have built the executable in about three other ways. We could have used nginx instead of Caddy. We could have used "
                (code "hunchentoot") " instead of "
                (code "clack") " and friends. We could have used Docker to set up the entire environment on the server and simply run the code from that environment.")
              (p "The principles to keep in mind when deploying a Common Lisp web app are these:")
              (ol :class "org-ol"
                (li "You need to have some entry point–the call to start the server.")
                (li "You need to run that call "
                  (i "someway") " as a service to enable automatic restarts.")
                (li "You will need to call "
                  (code "sleep") " in the entry point to keep the server thread open in the background.")
                (li "You want to turn off the debugger in prod.")))))
        (section :id "hold-deploying-an-electron-app"
          (hgroup
            (span)
            (h2 "HOLD DEPLOYING AN ELECTRON APP"))
          (div :class "outline-text-3" :id "text-org8598b22")
          (section :id "the-code"
            (hgroup
              (span)
              (h3 "The Code"))
            (div :class "outline-text-4" :id "text-org0ff6c01")
            (section :id "project-setup"
              (hgroup
                (span)
                (h4 "Project Setup"))
              (div :class "outline-text-5" :id "text-org3ede36f"
                (p "First, on your local machine, make a new project root directory called "
                  (code "fuka-stack") ". Inside it, you can also make another directory called "
                  (code "src") ".")))
            (section :id "build-script-service-makefile"
              (hgroup
                (span)
                (h4 "Build Script, Service, Makefile"))
              (div :class "outline-text-5" :id "text-org7ac5e4b")
              (section :id "build-script"
                (hgroup
                  (span)
                  (h5 "Build Script"))
                (div :class "outline-text-6" :id "text-org1234b2f"
                  (p "The script for building the executable is going to look similar to our previous "
                    (code "build.lisp") " file. However, this one is a "
                    (code ".ros") " file, used by Roswell to build the executable.")
                  (p "Open a buffer in "
                    (code "fuka-stack/fuka-stack-app.ros") ", add the following code, and then save:")
                  (pre
                    (code :class "lisp" "#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 \"$@\"
|#

;; Roswell script header (above) makes this executable
(ql:quickload \"ceramic\")
(defpackage #:ceramic-app-script
  (:use #:cl)
  (:export #:main))
(in-package #:ceramic-app-script)

;; Load your system
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :ignore-inherited-configuration))

(asdf:load-system \"ceramic-app\")

;; Environment variable utilities.
(defun get-env-int (env default)
  (let ((env (uiop:getenv env)))
    (if env
        (parse-integer env :junk-allowed t)
        default)))

(defun get-env (env default)
  (or (uiop:getenv env) default))

(defun envp (env)
  (if (string-equal (uiop:getenvp env) \"true\")
      t
      nil))

(defun main ()
  (ceramic:setup)
  (let ((connection (clack:clackup (lack:builder ceramic-app:*component*) :server :woo :address \"127.0.0.1\" :port 5000 :debug nil))
        (window (ceramic:make-window :url \"http://127.0.0.1:5000\")))
    (ceramic:show-window window)))"))
                  (p "The "
                    (code ".ros") " file needs to have its own "
                    (code "main") " function. For the purposes of deploying a web app, all you need to understand is this "
                    (code "main") " function. That function uses "
                    (code "uiop:getenv") " via "
                    (code "get-env") " and "
                    (code "get-env-int") " helpers to–you guessed it–get the environment variables "
                    (code "HOST") " and "
                    (code "PORT") ", defaulting to "
                    (code "127.0.01") " and "
                    (code "5000") ". If the "
                    (code "DEBUGP") " environment variable is set to "
                    (code "true") ", then the "
                    (code "clack:clackup")
                    (code ":debug") " setting will be set to "
                    (code "t") ", otherwise "
                    (code "nil") ". Later we'll change the variables on the production environment, but this allows you to try this code out on your computer.")
                  (p
                    (code "sleep") " is a function we haven't seen before. Usually, after loading the system and calling "
                    (code "clack:clackup") " to start the server, it will remain on (usually you save it to a parameter so you can turn it off with "
                    (code "clack:stop") ").")
                  (p "When you run the executable built with this code, if you don't include the call to "
                    (code "(sleep most-positive-fixnum)") ", systemd will assume your executable is done working and stop the service, turning off your server.")))
              (section :id "service-env-files"
                (hgroup
                  (span)
                  (h5 "Service & Env files"))
                (div :class "outline-text-6" :id "text-orgb5fad64"
                  (p "We will need to run our executable as a service. If we don't, the server will block our terminal session, and if there is an error, it will simply crash the server. Systemd can be configured to restart any crashed services, and the service won't block our terminal session, either.")
                  (p "Open a buffer in "
                    (code "fuka-stack/fuka-stack-app.service") ", add the following code, and then save.")
                  (pre
                    (code :class "makefile" "[Unit]
Description=Fuka Base App
After=network.target

[Service]
Type=simple
# Replace with the username you create when setting up the server
User=micah 
# ...same here
WorkingDirectory=/home/micah/.local/bin/
EnvironmentFile=/etc/systemd/system/fuka-stack-app.env

# ...and here
ExecStart=/home/micah/.local/bin/fuka-stack

TimeoutStartSec=120
TimeoutStopSec=10
KillMode=process
KillSignal=SIGTERM

Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target"))
                  (p "We also need environment variables set up, which we can do in a separate file (notice "
                    (code "EnvironmentFile") " above).")
                  (p "Open a buffer in "
                    (code "fuka-stack/fuka-stack-app.env") ", add the following code, and then save.")
                  (pre
                    (code :class "makefile" "HOST=0.0.0.0
PORT=5000
DEBUGP=false"))))
              (section :id "makefile"
                (hgroup
                  (span)
                  (h5 "Makefile"))
                (div :class "outline-text-6" :id "text-org190d457"
                  (p "The makefile includes both "
                    (code "make") " and "
                    (code "make install") " commands. Open a buffer in "
                    (code "fuka-stack/makefile") " and save this code inside:")
                  (pre
                    (code :class "makefile" "fuka-stack: build.lisp *.asd *.lisp src/*.lisp
        ros build fuka-stack-app.ros
install:
        mv fuka-stack-app ~/.local/bin/fuka-stack-app
        sudo cp fuka-stack-app.service /etc/systemd/system/fuka-stack-app.service
        sudo cp fuka-stack-app.env /etc/systemd/system/fuka-stack-app.env
        sudo systemctl daemon-reload
        sudo systemctl enable fuka-stack-app.service
run:
        sudo systemctl start fuka-stack-app.service"))
                  (p
                    (code "make") " will run Roswell's version of SBCL to build the executable. If you want to use/test this script locally you need to install Roswell.")
                  (p
                    (code "make install") " will move the "
                    (code "fuka-stack-app") " executable, setup the systemd service and environment variables, and then start the service."))))
            (section :id "hello-world-code"
              (hgroup
                (span)
                (h4 "Hello World Code"))
              (div :class "outline-text-5" :id "text-orgc8e4b13"
                (p "Now that we have our server set up, and we have all we need to build an executable and start it as a service, we can make our hello-world web app."))
              (section :id "system"
                (hgroup
                  (span)
                  (h5 "System"))
                (div :class "outline-text-6" :id "text-org47cc69d"
                  (p "In the project root directory, make a file called "
                    (code "fuka-stack.asd") " and place the below code into it.")
                  (pre
                    (code :class "lisp" "(defsystem \"ceramic-app\"
  :author \"Micah Killian <micah@almightylisp.com>\"
  :maintainer \"Micah Killian <micah@almightylisp.com>\"
  :description \"Ceramic Electron Example App\"
  :license \"MIT\"
  :version \"0.1\"
  :depends-on (:clack
               :woo 
               :myway
               :lack
               :lack-component
               :lack-request
               :lack-response)
  :components ((:module \"src\"
                :components
                ((:file \"main\")))))"))
                  (p "Notice that we don't have any of the build options like we had for the previous executable CLI app. That's because we'll be using Roswell to build the executable.")
                  (p "Save this file and then be sure to run "
                    (code "vend get") " to download the dependencies into your project.")))
              (section :id "main"
                (hgroup
                  (span)
                  (h5 "Main"))
                (div :class "outline-text-6" :id "text-org308393e"
                  (p "Finally, we have the main body of the app. I won't be explaining the mechanics of the stack–that is the subject of a future book.")
                  (p "Open a buffer in "
                    (code "fuka-stack/src/main.lisp") ", add this code, and save it.")
                  (pre
                    (code :class "lisp" "(defpackage #:ceramic-app
  (:use #:cl)
  (:nicknames #:ceramic-app/main))
(in-package #:ceramic-app/main)

;; Lack setup
(defclass fuka-component (lack/component::lack-component)
  ((routes :initarg :routes :accessor component-routes :initform (myway:make-mapper))))

(defparameter *component*
  (make-instance 'fuka-component))

(defparameter *request* nil)
(defparameter *response* nil)

(defmethod lack/component:call ((component fuka-component) env)
  (let ((*request* (lack/request:make-request env)))
    (multiple-value-bind (response foundp)
        (myway:dispatch (slot-value component 'routes)
                        (lack/request:request-path-info *request*)
                        :method (lack/request:request-method *request*))
      (if foundp
          (if (functionp response)
              response
              (destructuring-bind (status headers body) response
                (lack/response:finalize-response (lack/response:make-response status headers body))))
          (lack/response:finalize-response (lack/response:make-response 404 '(:content-type \"text/html\") '(\"Not found\")))))))

;; Clack response utility
(defun http-response (body &key (code 200) (headers nil))
  (let ((headers (append `(:content-type \"text/html; charset=utf-8\"
                           :content-length ,(length body))
                         headers)))
    `(,code ,headers (,body))))


;; Myway routing utilities
(defun make-endpoint (fn)
  (lambda (params)
    (funcall (symbol-function fn) params)))

(defun route (method routing-rule endpoint &optional (mapper (component-routes *component*)))
  (myway:connect mapper
                 routing-rule
                 (make-endpoint endpoint)
                 :method method))


;; Index controller and route
(defun index (params)
  (declare (ignore params))
  (http-response (format nil
                         \"Hello, world! This is the path: ~a. This is the HTTP request method: ~a.\"
                         (lack/request:request-path-info *request*)
                         (lack/request:request-method *request*))))

(route :GET \"/\" 'index)"))))))))
      (section :id "resources"
        (hgroup
          (span)
          (h1 "RESOURCES"))
        (div :class "outline-text-2" :id "text-org7b8f75a"))))
  (button :class "toc-viewer" "TOC")
  (script "HighlightLisp.highlight_auto();"))
"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "
