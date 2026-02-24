(defsystem "almightylisp"
  :author "Micah Killian <micah@killianarts.online>"
  :maintainer "Micah Killian <micah@killianarts.online>"
  :description "The almightylisp.com website"
  :license "MIT"
  :version "0.1"
  :depends-on (:shiso)
  :pathname "src"
  :serial t
  :components ((:module "components"
                :serial t
                :components ((:file "common")
                             (:file "package")))
               (:file "utils")
               (:file "routes")
               (:module "controllers"
                :serial t
                :components ((:file "teaser")
                             (:file "essentials")))
               (:file "main")))
