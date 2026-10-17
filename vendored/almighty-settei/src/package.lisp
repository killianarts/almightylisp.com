(uiop:define-package #:almighty-settei
  (:nicknames #:settei)
  (:use #:cl)
  (:use-reexport #:almighty-settei/conditions
                 #:almighty-settei/kinds
                 #:almighty-settei/registry
                 #:almighty-settei/dotenv
                 #:almighty-settei/configuration
                 #:almighty-settei/report))
