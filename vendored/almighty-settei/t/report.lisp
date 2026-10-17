(defpackage #:almighty-settei/t/report
  (:use #:cl #:lisp-unit2 #:almighty-settei/t/suite)
  (:local-nicknames (#:settei #:almighty-settei)))

(in-package #:almighty-settei/t/report)

(defun define-settings ()
  (settei:define-setting :server/port settei:integer-setting
    "The port the server
listens on.

More about it."
    :environment "PORT"
    :default 5000)
  (settei:define-setting :magazine/github-token settei:string-setting
    "Sent with GitHub searches."
    :environment "GITHUB_TOKEN"
    :secret t))

(defun row (name report)
  (find name report :key (lambda (row) (getf row :name))))

(define-test the-report ()
  (with-registry
    (define-settings)
    (load-with :environment-variables '("GITHUB_TOKEN" "ghp_abcdef123456"))
    (let ((report (settei:settings-report)))
      (assert-equal '(:settei/environment :server/port :magazine/github-token)
                    (mapcar (lambda (row) (getf row :name)) report))
      (let ((port (row :server/port report)))
        (assert-equal "5000" (getf port :value))
        (assert-eq :default (getf port :source))
        (assert-equal "server" (getf port :group))
        (assert-equal '("PORT") (getf port :variables)))
      (let ((token (row :magazine/github-token report)))
        (assert-equal settei:*mask* (getf token :value))
        (assert-eq :environment (getf token :source))
        (assert-true (getf token :secret))))))

(define-test only-what-changed ()
  (with-registry
    (define-settings)
    (load-with :environment-variables '("PORT" "8080"))
    (assert-equal '(:server/port)
                  (mapcar (lambda (row) (getf row :name))
                          (settei:settings-report :changed t)))))

(define-test a-refused-value-in-the-report ()
  (with-registry
    (define-settings)
    (load-with)
    (settei:configure :server/port "port")
    (let ((port (row :server/port (settei:settings-report))))
      (assert-equal "is not an integer" (getf port :problem))
      (assert-equal "\"port\"" (getf port :value))
      (assert-eq :project (getf port :source)))))

(define-test printing ()
  (with-registry
    (define-settings)
    (load-with :environment-variables '("GITHUB_TOKEN" "ghp_abcdef123456"))
    (let ((text (with-output-to-string (out) (settei:print-settings :stream out))))
      (assert-true (search "Environment: development" text) text)
      (assert-true (search "server" text) text)
      (assert-true (search ":server/port" text) text)
      (assert-true (search "The port the server listens on." text) text)
      (assert-false (search "More about it." text) text)
      (assert-false (search "ghp_abcdef123456" text) text))))

(define-test redacting ()
  (with-registry
    (define-settings)
    (load-with :environment-variables '("GITHUB_TOKEN" "ghp_abcdef123456"))
    (assert-equal (format nil "Authorization: token ~a; again ~a" settei:*mask* settei:*mask*)
                  (settei:redact "Authorization: token ghp_abcdef123456; again ghp_abcdef123456"))
    (assert-equal "nothing secret" (settei:redact "nothing secret")))
  (with-registry
    (define-settings)
    (load-with :environment-variables '("GITHUB_TOKEN" "ab"))
    (assert-equal "a cab" (settei:redact "a cab"))))
