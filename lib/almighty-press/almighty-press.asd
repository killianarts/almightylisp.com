(defsystem "almighty-press"
  :description "Publish org files as web pages with almighty-html: an org reader,
content collections that reload when their files change, dates, inline markup,
and static page output."
  :author "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:almighty-html :almighty-toki)
  :pathname "src"
  :serial t
  :components ((:file "org")
               (:file "dates")
               (:file "content")
               (:file "html")
               (:file "publish")
               (:file "press")))
