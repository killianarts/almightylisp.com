(defpackage #:magazine/publish
  (:use #:cl)
  (:local-nicknames (#:model #:magazine/model)
                    (#:intel #:magazine/intel)
                    (#:hhc #:magazine/hypermedia/components))
  (:export #:publish-site))

(in-package #:magazine/publish)

(defun publish-site (&optional (articles (model:magazine-articles))
                       (series (model:magazine-series)))
  "Write every magazine page to static/magazine/published/, removing pages of
articles that are gone. The running site renders the same pages from the org
files; this is the static copy. Returns the directory."
  (let ((updates (intel:published-updates))
        (watching (length (intel:watch-list))))
    (press:publish-pages
     (model:publish-directory)
     (list* (list "index" (hhc:page-home articles series updates watching))
            (list "archive" (hhc:page-archive articles series))
            (list "intel" (hhc:page-intel updates watching))
            (mapcar (lambda (article)
                      (list (model:article-slug article) (hhc:page-article article articles series)))
                    articles)))))
