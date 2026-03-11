(defsystem "author"
  :description "Module for Authors"
  :version "0.1"
  :depends-on (:shiso :almightylisp/core)
  :pathname "."
  :serial t
  :components ((:file "models")
               (:file "forms")
               (:module "hypermedia"
                :components ((:file "components")))
               (:file "controllers")
               (:file "routes")
               (:file "author")))
