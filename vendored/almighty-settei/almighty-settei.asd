(defsystem "almighty-settei"
  :version "0.1.0"
  :description "Declared, typed, documented settings for web applications"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ()
  :components ((:file "conditions")
               (:file "kinds")
               (:file "registry")
               (:file "dotenv")
               (:file "configuration")
               (:file "report")
               (:file "package"))
  :in-order-to ((test-op (test-op "almighty-settei/tests"))))

(defsystem "almighty-settei/tests"
  :depends-on (:almighty-settei :lisp-unit2)
  :components ((:module "t"
                :serial t
                :components ((:file "suite")
                             (:file "kinds")
                             (:file "dotenv")
                             (:file "configuration")
                             (:file "report"))))
  :perform (test-op (o s)
                    (dolist (package '(:almighty-settei/t/kinds
                                       :almighty-settei/t/dotenv
                                       :almighty-settei/t/configuration
                                       :almighty-settei/t/report))
                      (uiop:symbol-call :lisp-unit2 :print-summary
                                        (uiop:symbol-call :lisp-unit2 :run-tests
                                                          :package package)))))
