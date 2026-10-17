(defsystem "magazine"
  :description "The magazine at the site's root: articles written in org files under
content/magazine/, served live and published as static pages."
  :author "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :version "0.2"
  :depends-on (:shiso :quri :almighty-html :almighty-toki :almighty-press
               :cl-ppcre :com.inuoe.jzon
               :usocket :cl+ssl :bordeaux-threads :babel :md5)
  :pathname "."
  :serial t
  :components ((:file "model")
               (:file "font-metrics")
               (:file "og-image")
               (:file "intel")
               (:file "irc")
               (:module "hypermedia"
                :serial t
                :components ((:file "package")
                             (:file "sheet")
                             (:file "logos")
                             (:file "cards")
                             (:file "blocks")
                             (:file "evolving")
                             (:file "home")
                             (:file "article")
                             (:file "search")
                             (:file "archive")
                             (:file "intel")
                             (:file "archive-lab")
                             (:file "docs-lab")
                             (:file "og-preview")))
               (:file "publish")
               (:file "controllers")
               (:file "routes")
               (:file "magazine")))
