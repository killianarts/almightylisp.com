(in-package :cl-user)

(defpackage myway
  (:use :cl)
  (:import-from :myway.mapper
                :mapper
                :clear-routes
                :mapper-routes
                :make-mapper
                :member-route
                :member-route-by-name
                :member-routes-by-namespace
                :add-route
                :next-route
                :dispatch)
  (:import-from :myway.route
                :route
                :route-name
                :route-namespace
                :route-rule
                :route-handler
                :equal-route
                :match-route
                :url-for)
  (:import-from :alexandria
                :delete-from-plist)
  (:export :make-mapper
           :connect
           :next-route
           :dispatch

           :*env*
           :to-app

           :mapper
           :clear-routes
           :mapper-routes
           :add-route
           :find-route
           :find-route-by-name
           :find-routes-by-namespace

           :route
           :route-name
           :route-namespace
           :route-rule
           :route-handler
           :equal-route
           :match-route
           :url-for))

(in-package :myway)

(defun connect (mapper url fn &key (method '(:GET)) regexp name namespace)
  (add-route mapper
             (make-instance 'route
                            :url url
                            :method method
                            :regexp regexp
                            :name name
                            :namespace namespace
                            :handler fn)))

(defun find-route (mapper url &rest args &key method regexp name (route-class 'route) &allow-other-keys)
  (declare (ignore method regexp name))
  (car
   (member-route mapper
                 (apply #'make-instance route-class
                        :url url
                        (delete-from-plist args :route-class)))))

(defun find-route-by-name (mapper name)
  (if (keywordp name)
      (car (member-route-by-name mapper name))
      (when (and (stringp name) (position #\: name))
        (let* ((pos (position #\: name))
               (ns (intern (string-upcase (subseq name 0 pos)) :keyword))
               (n (intern (string-upcase (subseq name (1+ pos))) :keyword)))
          (find-if (lambda (route)
                     (and (eq (route-namespace route) ns)
                          (eq (route-name route) n)))
                   (mapper-routes mapper))))))

(defun find-routes-by-namespace (mapper namespace)
  (member-routes-by-namespace mapper namespace))

(defparameter *env* nil)

(defun to-app (mapper)
  (lambda (env)
    (let ((*env* env))
      (destructuring-bind (&key request-method path-info &allow-other-keys) env
        (dispatch mapper path-info :method request-method)))))
