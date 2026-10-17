(defpackage #:magazine/publish
  (:use #:cl)
  (:local-nicknames (#:model #:magazine/model)
                    (#:intel #:magazine/intel)
                    (#:og #:magazine/og-image)
                    (#:hhc #:magazine/hypermedia/components))
  (:export #:publish-site))

(in-package #:magazine/publish)

(defun publish-site (&optional (articles (model:magazine-articles))
                       (series (model:magazine-series)))
  "Write every magazine page to static/magazine/published/, removing pages of
articles that are gone, and draw the articles' preview images that aren't
drawn yet into its og/ folder. The running site renders the same pages from
the org files; this is the static copy. Returns the directory."
  ;; Before the pages: an article's page points at its image once it is drawn.
  ;; An image that can't be drawn doesn't keep the pages from being written.
  (handler-case (og:ensure-images articles series)
    (error (condition)
      (warn "Preview images not drawn: ~a" condition)))
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
