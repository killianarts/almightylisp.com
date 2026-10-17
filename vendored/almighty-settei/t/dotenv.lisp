(defpackage #:almighty-settei/t/dotenv
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:settei #:almighty-settei)))

(in-package #:almighty-settei/t/dotenv)

(defun parse (&rest lines)
  (settei:parse-dotenv (format nil "~{~a~%~}" lines)))

(define-test names-and-values ()
  (assert-equal '(("INTEL_DESK" . "true") ("PORT" . "5000"))
                (parse "INTEL_DESK=true" "PORT = 5000")))

(define-test comments-and-blank-lines ()
  (assert-equal '(("A" . "1"))
                (parse "# a comment" "" "   " "A=1" "not a variable")))

(define-test export-before-a-name ()
  (assert-equal '(("SHISO_ENV" . "development"))
                (parse "export SHISO_ENV=development")))

(define-test quotes ()
  (assert-equal '(("GREETING" . "Hello, \"world\"
"))
                (parse "GREETING=\"Hello, \\\"world\\\"\\n\""))
  (assert-equal '(("LITERAL" . "kept $as \\n is"))
                (parse "LITERAL='kept $as \\n is'"))
  (assert-equal '(("HASH" . "a # b"))
                (parse "HASH=\"a # b\"  # not this")))

(define-test inline-comments ()
  (assert-equal '(("PORT" . "5000") ("COLOR" . "#fff"))
                (parse "PORT=5000 # the port" "COLOR=#fff")))

(define-test empty-values ()
  (assert-equal '(("EMPTY" . "")) (parse "EMPTY=")))

(define-test a-missing-file ()
  (assert-eq nil (settei:read-dotenv "/nonexistent/.env"))
  (assert-eq nil (settei:read-dotenv nil)))
