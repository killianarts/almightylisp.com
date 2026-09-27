(defsystem "almighty-toki"
  :version "0.0.1"
  :description "Convenient and Powerful Datetime Operations built on local-time"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("local-time")
  :components ((:file "creation")
               (:file "timezone")
               (:file "arithmetic")
               (:file "duration")
               (:file "predicates")
               (:file "boundaries")
               (:file "iteration")
               (:file "format")
               (:file "properties")
               (:file "diff-human")
               (:file "testing")
               (:file "calendar")
               (:file "package"))
  :in-order-to ((test-op (test-op "almighty-toki/tests"))))

(defsystem "almighty-toki/tests"
  :depends-on (:almighty-toki :lisp-unit2)
  :components ((:module "t"
                :serial t
                :components
                ((:file "suite")
                 (:file "creation")
                 (:file "timezone")
                 (:file "arithmetic")
                 (:file "duration")
                 (:file "iteration")
                 (:file "predicates")
                 (:file "boundaries")
                 (:file "format")
                 (:file "properties")
                 (:file "diff-human")
                 (:file "testing")
                 (:file "calendar")
                 (:file "timezone-consistency"))))
  :perform (test-op (o s)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/creation)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/timezone)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/arithmetic)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/duration)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/iteration)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/predicates)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/boundaries)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/format)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/properties)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/diff-human)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/testing)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/calendar)
                    (uiop:symbol-call :lisp-unit2 :run-tests
                                      :package :almighty-toki/t/timezone-consistency)))
