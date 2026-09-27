(defsystem "magazine"
  :description "The magazine at /magazine: articles written in org files under
content/magazine/, served live and published as static pages."
  :author "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :version "0.2"
  :depends-on (:shiso :almighty-html :almighty-toki :almighty-press)
  :pathname "."
  :serial t
  :components ((:file "model")
               (:module "hypermedia"
                :serial t
                :components ((:file "package")
                             (:file "sheet")
                             (:file "logos")
                             (:file "cards")
                             (:file "blocks")
                             (:file "home")
                             (:file "article")
                             (:file "archive")))
               (:file "publish")
               (:file "controllers")
               (:file "routes")
               (:file "magazine")))
