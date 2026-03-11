(defpackage #:article/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:ah #:almighty-html)
                    (#:hy #:almightylisp/hypermedia))
  (:export #:index
           #:article-create))

(in-package #:article/controllers)

(defun index ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (hy:ac-admin-layout
       :title "Article Index"
       :header "Article Index"
       :subheader "Where it all begins"
       "This is the index")))))

(defun article-create ()
  (let* ((q (lack/request:request-body-parameters shiso:*request*))
         (my-input-value (cdr (assoc "my-input" q :test #'equal)))
         (org-output (if my-input-value (article/org-mode-parser:org-string-to-html my-input-value))))
    (s:http-response
     (ah:render-to-string
      (ah:</>
       (hy:ac-admin-layout
         :title "Create An Article"
         :header "Create An Article"
         :subheader "You know you want to."

         (div
           (form :method "GET" :action (shiso:url "article:create")
             (textarea :rows "10" :cols "70" :class "bg-white text-black p-5"
               :|data-on:keydown__debounce.1ms| (format nil "@post('~a')" (shiso:url "article:create"))
               :name "my-input" :data-bind "my-input" my-input-value)
             (textarea :rows "10" :cols "70" :class "bg-white text-black p-5"
               :|data-on:keydown__debounce.1ms| (format nil "@post('~a')" (shiso:url "article:create"))
               :name "my-input" :data-bind "my-input" my-input-value))
           (main :class "" (raw! org-output)))))))))

