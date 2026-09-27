(defpackage #:magazine
  (:use #:cl)
  (:local-nicknames (#:model #:magazine/model)
                    (#:pub #:magazine/publish))
  (:export #:publish))

(in-package #:magazine)

(defun publish ()
  "Read content/magazine/ again and write static/magazine/published/.

Links are made with SHISO:URL, which only knows where the magazine is mounted
once it has served a request. Until then this signals an error rather than
write pages with broken links. The server publishes by itself whenever an org
file changes, so this is only needed to force it."
  (when (string= (shiso:url "magazine:index") "/")
    (error "Open any /magazine page on the running server first, so Shiso knows ~
where the magazine is mounted."))
  (model:take-changes)
  (pub:publish-site))
