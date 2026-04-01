(defsystem "almightylisp"
  :author "Micah Killian <micah@almightylisp.com>"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :description "The almightylisp.com website"
  :license "MIT"
  :version "0.1"
  :depends-on (:almightylisp/core
               :cl-org-mode
               ;; Project modules
               :book
               :article
               :author
               :home)
  :build-operation "program-op"
  :build-pathname "almightylisp"
  :entry-point "almightylisp:main")

(defsystem "almightylisp/core"
  :author "Micah Killian <micah@killianarts.online>"
  :maintainer "Micah Killian <micah@killianarts.online>"
  :description "The almightylisp.com website"
  :license "MIT"
  :version "0.1"
  :depends-on (:shiso :datastar-cl)
  :pathname "src"
  :serial t
  :components ((:module "hypermedia"
                :serial t
                :components ((:file "components")
                             (:file "package")))
               (:file "utils")
               (:file "routes")
               (:file "main")))
