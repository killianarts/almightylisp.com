(defpackage #:almightylisp/controllers/teaser
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html)
                    (#:x #:almightylisp/components)
                    (#:s #:shiso)))
(in-package #:almightylisp/controllers/teaser)

(defparameter *teaser-path* "/")

(defun make-concat-count (&optional (count 0))
  (let ((count count))
    (lambda (string)
      (let ((string-count (write-to-string count)))
        (incf count)
        (concatenate 'string string string-count)))))

(defun index ()
  (let ((concat-count (make-concat-count)))
    (shiso:http-response
     (ah:render-to-string
      (</>
       (c:ac-site-layout :title "THIS IS A LIST"
         (c:ac-hero-text
           "THIS IS A LIST")
         (div :class "slides max-w-[120ch] mx-auto relative [anchor-name:--hero-text-anchor]" 
           (c:ac-teaser-slide
             :heading "A LIST"
             :id (apply concat-count '("slide")) :code "()"
             (p "This is a " (c:ac-code "list") "."))
           (c:ac-teaser-slide
             :heading "ANOTHER LIST"
             :id (apply concat-count '("slide")) :code "(1 2 3)"
             (p "So is this. It has three numbers."))
           (c:ac-teaser-slide
             :heading "NESTED LISTS"
             :id (apply concat-count '("slide")) :code "((10 11 12) (1 2 3))"
             (p "This is three lists. One list contains two nested lists."))
           (c:ac-teaser-slide
             :heading "ASSOCIATIVE LISTS"
             :id (apply concat-count '("slide")) :code "((:micah . \"lisper\")
 (:sussman . \"lisper\")
 (:joe-blow . \"jai guy\")
 (:dhh . \"ruby boob\"))"
             (p "This is an " (c:ac-code "associative list") ", also called an " (c:ac-code "alist") ". Lists can be used as tables storing key-value pairs.")
             (p "Symbols that begin with " (c:ac-code ":") " are called " (c:ac-code "keywords") ". Keywords are evaluated to themselves."))
           (c:ac-teaser-slide
             :heading "SYMBOL"
             :id (apply concat-count '("slide")) :code "micah"
             (p "This is a " (c:ac-code "symbol") ". A symbol holds a reference to some data."))
           (c:ac-teaser-slide
             :heading "FUNCTION CALLS"
             :id (apply concat-count '("slide")) :code "(say-hello) ; => \"Hello!\""
             (p "This is a list with a symbol. The first item in a list is treated as the name of a function to be called."))
           (c:ac-teaser-slide
             :heading "ARGUMENTS"
             :id (apply concat-count '("slide")) :code "(person-birthday :micah '((:micah . \"1985-12-14\")
                          (:takae . \"1987-11-19\")
                          (:mom . \"1955-02-12\")
                          (:papa . \"1952-07-31\"))) ; => \"1985-12-14\""
             (p "If there are other elements in the list besides the function name, they are passed as arguments to the function. Arguments are evaluated first--from left to right--before being passed to the function."))
           (c:ac-teaser-slide
             :heading "FUNCTION DEFINITIONS"
             :id (apply concat-count '("slide")) :code "(defun person-birthday (name db) (cdr (assoc name db)))"
             (p "This is how you create a " (c:ac-code "function") ". There is one list that contains two symbols and two nested lists. The first element, " (c:ac-code "defun") ", is a macro. " "The second element, " (c:ac-code "person-birthday") ", is a name to give to the function. " (c:ac-code "(name db)") " is a " (span :class "italic" "lambda-list") " that defines the arguments to the function." (c:ac-code "(cdr (assoc name db))") " is another list--the body of the function."))
           (c:ac-teaser-slide
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
           (c:ac-teaser-slide
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
             (p (c:ac-code "'") " is a reader macro. It performs the same function as the above call to " (c:ac-code "quote") "."))
           (c:ac-teaser-slide
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
             (p "The first list is a function call to " (c:ac-code "quote") ". It returns the literal representation of its argument.")
             (p "The the second list is a function call to " (c:ac-code "eval") ". It will take the data it receives and evaluate it as code.")
             (p "I repeat: it takes the literal representation of data and evaluates it as code. The code is data."))
           (c:ac-teaser-slide
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
           (c:ac-teaser-slide
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
           (c:ac-teaser-slide
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
           (c:ac-teaser-slide
             :heading "SQL QUERY MACRO"
             :id (apply concat-count '("slide")) :code "(sxql:select ((:as (:coalesce (:sum :leg.entry_amount) 0) :amount)
                          (:as :leg.entry_currency :currency))
                 (sxql:from (:as :almighty_account :account))
                 (sxql:inner-join (:as :almighty_leg :leg) :on (:= :leg.account_id :account.id))
                 (sxql:inner-join (:as :almighty_transaction :tran) :on (:= :leg.transaction_id :tran.id))
                 (sxql:where (:and (:= :account.id (m:object-id account))
                                (:<= :tran.date as-of)))
                 (sxql:group-by :leg.entry_currency))"
             (p "This is " (a :href "https://github.com/fukamachi/sxql" "SXQL") " code. It's a library for generating SQL queries using macros like " (c:ac-code "select") ", " (c:ac-code "from") ", etc."))
           (c:ac-teaser-slide
             :heading "HTML MACRO"
             :id (apply concat-count '("slide")) :code "(define-component c:ac-skeleton (&key title children)
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
             (p "This is code from the almighty-html library. The " (c:ac-code "</>") " macro generates HTML and can create components."))
           (c:ac-teaser-slide
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
           (h2 "It all starts with a " (c:ac-code "list") "."))
         (div :class "px-5 max-w-5xl mx-auto min-h-[100dvh] flex items-center justify-center flex-wrap"
           (div
             (div :class "w-full flex justify-center"
               (img :class "max-h-[75dvh]" :src "assets/images/almighty-lisp-essentials.png"))
             (div :class "w-fit mx-auto mt-10"
               (p :class "loading text-center" "Coming Soon..."))))
         ))))))

(shiso:define-route :GET *teaser-path* :controller 'index :name "teaser:index")
