(defpackage #:home/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:ah #:almighty-html)
                    (#:hhc #:home/hypermedia/components)
                    (#:hy #:almightylisp/hypermedia))
  (:export #:index
           #:test-css-variable-changing
           #:change-color))

(in-package #:home/controllers)

(defun index ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (hhc:ac-home-layout :title "Almighty Lisp: Lisp & Emacs Essentials")))))

;; NOTE Datastar
;; (defun test-css-variable-changing ()
;;   (let ((behavior "on click set :colorCanvas to localStorage.colorCanvas then
;;                       set :colorCanvasDark to localStorage.colorCanvasDark then
;;                       if :colorCanvas log :colorCanvas end
;;                       send theme:colorChange to the body"))
;;     (s:http-response
;;      (ah:render-to-string
;;       (ah:</>
;;        (hy:ac-skeleton :title "Modifying CSS Variable Values"
;;          (section :class "card-container"
;;            (div :class "card" :data-script "on load set :colorCanvas to localStorage.colorCanvas then document.documentElement.style.setProperty('--color-canvas', :colorCanvas)"
;;              (h2 "Modifying CSS Variable Values")
;;              (p (format nil "query param: ~a" (lack/request:request-query-parameters shiso:*request*)))
;;              (p :id "patch-text" "This is a demonstration of how to change CSS variable values using Hyperscript.")
;;              (form :id "color-select-form" :class "color-select-form" :method "post" :action (format nil "@post('~a')" (shiso:url "home:change-color"))
;;                :|data-on:change| (format nil "@post('~a')" (shiso:url "home:change-color"))
;;                (select :id "color-select" :name "color-select" :data-bind "colorSelect"
;;                  (option :value "almighty-blue-800" "blue")
;;                  (option :value "almighty-red-800" "red")))))))))))

(defparameter *colors* '(("blue")))

(defun test-css-variable-changing ()
  (s:http-response
   (ah:render-to-string
    (ah:</>
     (hy:ac-skeleton :title "Modifying CSS Variable Values"
       (section :class "card-container"
         (div :class "card" 
           (h2 "Modifying CSS Variable Values")
           (p (format nil "query param: ~a" (lack/request:request-query-parameters shiso:*request*)))
           (p :id "patch-text" "This is a demonstration of how to change CSS variable values using Hyperscript.")
           (form :id "theme-select-form" :class "theme-select-form"
             (select :id "theme-select" :name "theme-select"
               :data-script "on change
                                set $theme to my.value
                                then set @data-theme of document.documentElement to $theme
                                then set localStorage.theme to $theme
                               end
                             on load set $theme to localStorage.theme
                              set @data-theme of document.documentElement to $theme
                              if $theme
                                set document.documentElement.dataset.theme to $theme
                                set my.value to localStorage.theme"
               (option :value "almighty-lisp"       "almighty-lisp")
               (option :value "almighty-lisp-moon"  "almighty-lisp-moon")
               (option :value "almighty-titan"      "almighty-titan")
               (option :value "almighty-titan-moon" "almighty-titan-moon")
               (option :value "almighty-gear"       "almighty-gear")
               (option :value "almighty-gear-moon"  "almighty-gear-moon")
               (option :value "almighty-macro"      "almighty-macro")
               (option :value "almighty-macro-moon" "almighty-macro-moon")
               (option :value "almighty-ena"             "almighty-ena")
               (option :value "almighty-ena-moon"        "almighty-ena-moon")
               (option :value "almighty-mikasa"          "almighty-mikasa")
               (option :value "almighty-mikasa-moon"     "almighty-mikasa-moon")
               (option :value "almighty-lisp-x"       "almighty-lisp-x")
               (option :value "almighty-lisp-x-moon"  "almighty-lisp-x-moon")
               (option :value "almighty-saga"            "almighty-saga")
               (option :value "almighty-saga-moon"       "almighty-saga-moon")
               (option :value "almighty-mono"            "almighty-mono")
               (option :value "almighty-mono-moon"       "almighty-mono-moon"))))))))))

;; TODO Move this API into Shiso
(defun query-param (key)
  (when s:*request*
    (cdr (assoc (string key)
                (lack/request:request-query-parameters s:*request*)
                :test #'string=))))

(defun body-param (key)
  (when s:*request*
    (cdr (assoc (string key)
                (lack/request:request-body-parameters s:*request*)
                :test #'string=))))
