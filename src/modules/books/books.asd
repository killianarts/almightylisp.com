(defsystem "books"
  :description "books module"
  :version "0.1"
  :depends-on (:shiso :almightylisp/core)
  :pathname "."
  :serial t
  :components ((:file "books")
               (:file "models")
               (:file "forms")
               (:module "hypermedia"
                :components ((:file "components")))
               (:file "controllers")
               (:file "routes")))
