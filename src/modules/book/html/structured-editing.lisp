
(body
 (div :class "sidebar-container"
 (div :class "book-navigation"
 (h1 :class "book-navigation__book-title" "Almighty Lisp: The Essentials")
 (p :class "book-navigation__chapter-name-heading"
 (span "Almighty Lisp: The Essentials"))
 (nav
 (ul
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#slurping-and-barfing"
 (span :class "book-navigation__section-name" "SLURPING AND BARFING")
 (span :class "book-navigation__section-number" "1.1.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#dragging-forms-forward-backward"
 (span :class "book-navigation__section-name" "DRAGGING FORMS FORWARD/BACKWARD")
 (span :class "book-navigation__section-number" "1.2.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#navigating-the-sea-of-parentheses"
 (span :class "book-navigation__section-name" "NAVIGATING THE SEA OF PARENTHESES")
 (span :class "book-navigation__section-number" "1.3.0")))
 (li :class "book-navigation__section-link-container"
 (a :class "book-navigation__section-link" :href "#more-restructuring-navigating-tools"
 (span :class "book-navigation__section-name" "MORE RESTRUCTURING & NAVIGATING TOOLS")
 (span :class "book-navigation__section-number" "1.4.0")))))
 (div :class "book-navigation__previous-and-next")))
 (main :class "sidebar-main"
 (article :class "book"
 (section :id "structured-editing"
 (hgroup
 (span)
 (h1 "STRUCTURED EDITING"))
 (div :class "outline-text-2" :id "text-1"
 (p "Writing Lisp code often involves moving forms around or wrapping forms in new
forms. If you're using "
 (code "lispy-mode") " or "
 (code "lispyville-mode") "–both turned on with
Doom's "
 (code "lispy") " config in the "
 (code "init.el") " file–you can more easily build out and
modify Lisp code."))
 (section :id "slurping-and-barfing"
 (hgroup
 (span)
 (h2 "SLURPING AND BARFING"))
 (div :class "outline-text-3" :id "text-1-1"
 (p "Slurping and barfing are terms for describing moving parentheses to either
include or exclude forms from other forms. Let me show you what I mean.")
 (p "In the tic-tac-toe project, we wrote code to find positions on the board to win
or block the opponent's win. To begin, we wrote this:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet)))
 *triplets*)"))
 (p "This finds any triplet that sums to 20 on the board. But we then wanted to
search the result of that "
 (code "find-if") " form to search for the empty space in the
triplet.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= (nth element board) 0))
         (find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet)))
 *triplets*))"))
 (p "If you use structural editing, this can actually be difficult at first. You
might naturally try to write the code like this:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= (nth element board) 0)))
(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet)))
 *triplets*)"))
 (p "And then, in order to wrap the bottom "
 (code "find-if") " form with the top "
 (code "find-if") "form, you might try to delete the final parenthesis of the top "
 (code "find-if") " and
then move it to the end of the bottom "
 (code "find-if") " form:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= (nth element board) 0)) ; deleted paren
(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet))) *triplets*)) ; added
                                                                                     ; paren"))
 (p "What actually happens, however, is that the entire top "
 (code "find-if") " form is deleted
when you try to delete just the last parenthesis! This very confusing behavior
is from "
 (code "lispy-mode") ". It is designed to keep parentheses balanced at all times.")
 (p "If you want to move only one parenthesis, then, what do you do?")
 (p "One option would be to "
 (code "kill") " the "
 (code "find-if") " form intended to become the inner
form…")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= (nth element board) 0)) ; bottom form killed"))
 (p "and then paste it at the end of the outer "
 (code "find-if") "…")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= (nth element board) 0)) (find-if #'(lambda (triplet) (=
 20 (sum-triplet *test-board* triplet))) *triplets*))"))
 (p "and then move the cursor to the beginning parenthesis of the inner "
 (code "find-if") " and
press "
 (code "ENTER") " to place it on a new line.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= (nth element board) 0))
         (find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet)))
 *triplets*))"))
 (p "This is okay, but there is an even easier method called "
 (code "slurping") ".")
 (p "Start with this code:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (element) (= (nth element board) 0)))
(find-if #'(lambda (triplet) (= 20 (sum-triplet *test-board* triplet)))
 *triplets*)"))
 (p "Place the cursor on the last parenthesis of the top form. While still in Evil
Normal mode (not Insert mode), press "
 (code ">") " (the "
 (code "lispyville-mode") " keybinding for"
 (code "lispyville-slurp") "). The parenthesis will move to the "
 (b "end") " of the bottom"
 (code "find-if") " form, making it an inner nested form.")
 (p "Slurping is especially useful for any time you need to \"build out\" from some
code. Consider this:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(find-if #'(lambda (triple)
             (= (sum-triple *test-board* triple) target-sum))
         *triples*)"))
 (p "This is how we started building out the code for detecting squeezes. But then we
realized that we needed to add more conditions. For a strategy to be a squeeze,
the triplet need to sum to the target, "
 (b "and") " it needs to be diagonal as well as
some other conditions. That means that we needed to wrap several predicate
functions in an "
 (code "and") " form.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" ";; These need to be wrapped in an (and ...) form
(= (sum-triple board triple) target-sum)
(diagonal-p triple)"))
 (p "We can do that by first writing the "
 (code "and") " form before the other two forms:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(and) (= (sum-triple board triple) target-sum)
(diagonal-p triple)"))
 (p "Put the Evil Normal-mode cursor on the end paren of the "
 (code "and") " form, and then"
 (code "slurp") " up the other two forms.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(and (= (sum-triple board triple) target-sum)
     (diagonal-p triple))"))
 (p "Once we added the rest of the conditions, we wanted to then bind the return
value of the "
 (code "find-if") " form to a local variable using "
 (code "let") ". The process looks
like this:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" ";; Write the LET and variable we want in the LET. let Finished find-if
(let ((squeeze-p))) (find-if #'(lambda (triple)
             (and (= (sum-triple board triple) target-sum)
                  (diagonal-p triple)
                  (not (human-in-middle-p board))
                  (side-empty-p board)))
         *triples*)"))
 (p "Then use "
 (code "lispyville-slurp") " on the three ending parentheses of the "
 (code "let") " form to
complete the new form.")
 (p "After adding an "
 (code "if") " form, we need to make this a function, so we can write the
function definition above the entire "
 (code "let") " form:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun detect-squeeze (board target-sum))
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
 (code "paste") " the forms around, but killing can
sometimes be dangerous (if you kill a word using "
 (code "M-backspace") ", you've just
replaced your function in the kill-ring). It's nicer to simply keep the code on
the screen and using slurp to do what you want.")
 (p "The reverse of "
 (code "slurp") " is "
 (code "barf") ", bound to "
 (code "<") " in Doom Emacs "
 (code "lispyville-mode") ".
I don't find it nearly as useful as slurping, but maybe you can find a use for
it.")))
 (section :id "dragging-forms-forward-backward"
 (hgroup
 (span)
 (h2 "DRAGGING FORMS FORWARD/BACKWARD"))
 (div :class "outline-text-3" :id "text-1-2"
 (p "Another operation you'll probably want to do a lot is move forms around. For
example, maybe you wrote this:")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(defun choose-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (take-random-position board)
      (block-two-on-one-play board)
      (block-squeeze-play board)))"))
 (p "Then you realized that the "
 (code "block-two-on-one") " and "
 (code "block-squeeze-play") "strategies need to take higher priority over all but the "
 (code "make-three-in-a-row") "strategy. You could, of course, kill/paste them into the proper position. But if
you want, you can also use "
 (code "lispyville-drag-backward") "–bound to "
 (code "M-k") "–while the
cursor is on the opening parentheses of the "
 (code "block-") " forms to move them."
 (code "lispyville-drag-forward") "–bound to "
 (code "M-j") "–goes in the opposite direction.")
 (p "They also work on individual symbols. For example, let's say you have a call to"
 (code "nth") ":")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(nth 4 my-list)"))
 (p "But then you decide you want to use "
 (code "member") " instead.")
 (pre :class "code-block-source-pre"
 (code :class "code-block-source lisp" "(member 4 my-list)"))
 (p "Unfortunately, you now need to switch the location of 4 and "
 (code "my-list") " in the
arguments to "
 (code "member") ". Fortunately, you can use "
 (code "lispyville-drag-") " functions for
that, too. Place the cursor over the 4 and use "
 (code "lispyville-drag-forward") " ("
 (code "M-j") ")
to switch the 4 and "
 (code "my-list") ".")))
 (section :id "navigating-the-sea-of-parentheses"
 (hgroup
 (span)
 (h2 "NAVIGATING THE SEA OF PARENTHESES"))
 (div :class "outline-text-3" :id "text-1-3"
 (p "Navigation is an important topic in Emacs. When programming Lisp in Doom Emacs,
you have additional navigation options that can dramatically speed up your
development flow.")
 (p "Doom Emacs comes with a function called "
 (code "+default/search-buffer") ", bound to "
 (code "SPC
s s") ", that you can use in any mode to search for some text. This is an essential
tool for navigation.")
 (p "When your Normal mode cursor is on a parenthesis, if you press "
 (code "TAB") " the cursor
will move to the matching parenthesis on the other side. This is actually a more
general function called "
 (code "evil-jump-item") ".")
 (p
 (code "+default/search-buffer") " will only move the cursor to the "
 (i "first") " instance of
some text on a line. It's often quicker to tab to the \"cattail\" at the end of a
Lisp form, move the cursor to another parenthesis, and then tab to the matching
parenthesis on the other side to get closer to the text you're navigating to.")))
 (section :id "more-restructuring-navigating-tools"
 (hgroup
 (span)
 (h2 "MORE RESTRUCTURING & NAVIGATING TOOLS"))
 (div :class "outline-text-3" :id "text-1-4"
 (p "Remember that you can get more information about the keybindings available in
the buffer using either "
 (code "M-x describe-key") " ("
 (code "SPC h k") "), "
 (code "M-x describe-mode") "("
 (code "SPC h m") "), and "
 (code "M-x embark-bindings") " ("
 (code "SPC h b b") "). Look into the "
 (code "lispy-") ","
 (code "lispyville-") ", and "
 (code "special-lispy-") " commands and their keybindings."))))))
 (button :class "toc-viewer" "TOC")
 (script "HighlightLisp.highlight_auto();"))