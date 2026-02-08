(defsystem "almightylisp"
  :author "Micah Killian <micah@killianarts.online>"
  :maintainer "Micah Killian <micah@killianarts.online>"
  :description "The almightylisp.com website"
  :license "MIT"
  :version "0.1"
  :depends-on (:shiso)
  :components ((:module "src"
                :serial t
                :components
                ((:file "routes")
                 (:file "app")
                 (:file "main")))))
