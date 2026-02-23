(in-package :cl-user)
(defpackage myway-test
  (:use :cl
        :myway
        :prove))
(in-package :myway-test)

(plan nil)

(defparameter *mapper* (make-mapper))

(is (find-route *mapper* "/") nil)
(is-values (dispatch *mapper* "/" :method :GET) '(nil nil))

(connect *mapper* "/"
         (lambda (params)
           (declare (ignore params))
           "Hello, World!"))

(ok (find-route *mapper* "/"))
(is (find-route *mapper* "/" :method :POST) nil)
(is (dispatch *mapper* "/" :method :GET) "Hello, World!")

(connect *mapper* "/"
         (lambda (params)
           (declare (ignore params))
           "Hello, World!")
         :name :test)
(ok (find-route-by-name *mapper* :test))
(is (find-route-by-name *mapper* :test) (find-route *mapper* "/" :name :test))

(connect *mapper* "/post"
         (lambda (params)
           (declare (ignore params))
           "posted")
         :method :POST)

(is (find-route *mapper* "/post") nil)
(ok (find-route *mapper* "/post" :method :POST))
(is-values (dispatch *mapper* "/post" :method :GET) '(nil nil))
(is (dispatch *mapper* "/post" :method :POST) "posted")

(connect *mapper* "/new"
         (lambda (params)
           (declare (ignore params))
           "new"))

(is (dispatch *mapper* "/new" :method :GET) "new")

(is (funcall (to-app *mapper*)
             '(:request-method :GET :path-info "/"))
    "Hello, World!")

(connect *mapper* "/id/:n"
         (lambda (params)
           (if (oddp (parse-integer (getf params :n)))
               "odd"
               (next-route)))
         :name 'odd-id)

(connect *mapper* "/id/*"
         (lambda (params)
           (declare (ignore params))
           "even")
         :name 'even-id)

(is (dispatch *mapper* "/id/1") "odd")
(is (dispatch *mapper* "/id/2") "even")

;; Route namespace tests

(defparameter *ns-mapper* (make-mapper))

(connect *ns-mapper* "/"
         (lambda (params)
           (declare (ignore params))
           "home")
         :name :index)

(connect *ns-mapper* "/admin"
         (lambda (params)
           (declare (ignore params))
           "admin home")
         :name :index
         :namespace :admin)

(connect *ns-mapper* "/admin/users"
         (lambda (params)
           (declare (ignore params))
           "admin users")
         :name :users
         :namespace :admin)

;; route-namespace accessor
(is (route-namespace (find-route-by-name *ns-mapper* :index)) nil
    "Route without namespace has nil namespace")
(is (route-namespace (find-route-by-name *ns-mapper* "admin:index")) :admin
    "Namespaced route has correct namespace")

;; find-route-by-name with keyword (backward compatible)
(ok (find-route-by-name *ns-mapper* :index)
    "Keyword lookup still works")

;; find-route-by-name with string namespace:name syntax
(ok (find-route-by-name *ns-mapper* "admin:index")
    "String namespace:name lookup finds route")
(ok (find-route-by-name *ns-mapper* "admin:users")
    "String namespace:name lookup finds second namespaced route")
(is (find-route-by-name *ns-mapper* "admin:nonexistent") nil
    "String lookup returns nil for nonexistent name")
(is (find-route-by-name *ns-mapper* "bogus:index") nil
    "String lookup returns nil for nonexistent namespace")

;; find-routes-by-namespace
(is (length (find-routes-by-namespace *ns-mapper* :admin)) 2
    "find-routes-by-namespace returns all routes in namespace")
(is (find-routes-by-namespace *ns-mapper* :nonexistent) nil
    "find-routes-by-namespace returns nil for unknown namespace")

(finalize)
