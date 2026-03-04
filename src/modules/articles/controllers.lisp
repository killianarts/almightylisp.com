(defpackage #:articles/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:ah #:almighty-html)
                    (#:hy #:almightylisp/hypermedia))
  (:export #:index
           #:articles-create))

(in-package #:articles/controllers)

(defun index ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (hy:ac-admin-layout
       :title "Articles Index"
       :header "Articles Index"
       :subheader "Where it all begins"
       "This is the index")))))

(defun articles-create ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (hy:ac-admin-layout
       :title "Create An Article"
       :header "Create An Article"
       :subheader "You know you want to."
       "Updated. Again.")))))
