(require :asdf)

;; Limit `asdf's search for systems to this project's root.
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:getcwd))
   :ignore-inherited-configuration))

(asdf:load-system "almightylisp")
