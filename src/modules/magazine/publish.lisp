(defpackage #:magazine/publish
  (:use #:cl)
  (:local-nicknames (#:model #:magazine/model)
                    (#:hhc #:magazine/hypermedia/components))
  (:export #:publish-site))

(in-package #:magazine/publish)

(defun publish-site (&optional (articles (model:magazine-articles))
                       (series (model:magazine-series)))
  "Write every magazine page to static/magazine/published/, removing pages of
articles that are gone. The running site renders the same pages from the org
files; this is the static copy. Returns the directory."
  (press:publish-pages
   (model:publish-directory)
   (list* (list "index" (hhc:page-home articles series))
          (list "archive" (hhc:page-archive articles series))
          (mapcar (lambda (article)
                    (list (model:article-slug article) (hhc:page-article article articles series)))
                  articles))))
