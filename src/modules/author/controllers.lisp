(defpackage #:author/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:ah #:almighty-html)
                    (#:hy #:almightylisp/hypermedia))
  (:export #:index
           #:author-create))

(in-package #:author/controllers)

(defun index ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (hy:ac-admin-layout
       :title "author Index"
       :header "author Index"
       :subheader "Where it all begins"
       "This is the index")))))

(defun author-create ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (hy:ac-admin-layout
       :title "Create An author"
       :header "Create An author"
       :subheader "You know you want to."
       "Updated. Again.")))))
