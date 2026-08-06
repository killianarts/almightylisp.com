(defsystem "home"
  :description "Module for Home"
  :version "0.1"
  ;; It's necessary to put `:almightylisp/core' here in order to ensure it's
  ;; loaded before the rest of the `:components'. This module is coupled to
  ;; `:almightylisp/core' because it depends on some shared components. Example:
  ;; `ac-skeleton'.
  :depends-on (:shiso :almightylisp/core)
  :pathname "."
  :serial t
  :components ((:file "models")
               (:file "forms")
               (:module "hypermedia"
                :components ((:file "components")))
               (:file "controllers")
               (:file "routes")
               (:file "home")))
