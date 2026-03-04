(defsystem "articles"
  :description "articles module"
  :version "0.1"
  :depends-on (:shiso :almightylisp/core)
  :pathname "."
  :serial t
  :components ((:file "articles")
               (:file "models")
               (:file "forms")
               (:module "hypermedia"
                :components ((:file "components")))
               (:file "controllers")
               (:file "routes")))
