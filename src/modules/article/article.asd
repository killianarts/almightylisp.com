(defsystem "article"
  :description "Module for Articles"
  :version "0.1"
  :depends-on (:shiso :almightylisp/core ;; :cl-org-mode
                      )
  :pathname "."
  :serial t
  :components ((:file "org-mode-parser")
               (:file "models")
               (:file "forms")
               (:module "hypermedia"
                :components ((:file "components")))
               (:file "controllers")
               (:file "routes")
               (:file "article")))
