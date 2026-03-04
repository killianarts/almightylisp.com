(defsystem "almightylisp"
  :author "Micah Killian <micah@killianarts.online>"
  :maintainer "Micah Killian <micah@killianarts.online>"
  :description "The almightylisp.com website"
  :license "MIT"
  :version "0.1"
  :depends-on (:shiso :almightylisp/core :books :articles))

(defsystem "almightylisp/core"
  :author "Micah Killian <micah@killianarts.online>"
  :maintainer "Micah Killian <micah@killianarts.online>"
  :description "The almightylisp.com website"
  :license "MIT"
  :version "0.1"
  :depends-on (:shiso)
  :pathname "src"
  :serial t
  :components ((:module "hypermedia"
                :serial t
                :components ((:file "components")
                             (:file "package")))
               (:file "utils")
               (:file "routes")
               (:file "main")))
