(defpackage #:magazine
  (:use #:cl)
  (:local-nicknames (#:model #:magazine/model)
                    (#:pub #:magazine/publish))
  (:export #:publish))

(in-package #:magazine)

(defun publish ()
  "Read content/magazine/ again and write static/magazine/published/.

The server publishes by itself whenever an org file changes, so this is only
needed to force it."
  (model:take-changes)
  (pub:publish-site))
