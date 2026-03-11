(defpackage #:book/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:ah #:almighty-html))
  (:export #:index))

(in-package #:book/controllers)

(defun index ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (book/hypermedia:ac-sidebar-layout :chapter "Testing CSS"
       (article :class "book"
         (section 
           (hgroup
             (span "subtitle")
             (h1 "Hello this is a test"))
           (p "this is some text")
           (book/hypermedia:ac-code-block "(defun test ()
(mapcar #'(lambda (x) (print x)) '(1 2 3 4 5)")
           (p "Here is a link: " (a :href "/" "This goes to the index.")))))))))

