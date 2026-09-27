(defpackage #:magazine/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:model #:magazine/model)
                    (#:pub #:magazine/publish)
                    (#:hhc #:magazine/hypermedia/components))
  (:export #:index #:archive #:article))

(in-package #:magazine/controllers)

(defparameter *page-cache* '(:cache-control "private, max-age=60")
  "Magazine pages may be reused from the browser's cache for a minute. htmx's
preload extension depends on it: it fetches a page when a link is hovered,
and the click is then served from the cache.")

(defun page-response (html &key (code 200))
  (s:http-response html :code code :headers *page-cache*))

(defun ensure-published ()
  "Publish the static pages again when an org file has changed."
  (when (model:take-changes)
    (handler-case (pub:publish-site)
      (error (condition)
        (warn "Magazine publish failed: ~a" condition)))))

(defun index ()
  (ensure-published)
  (page-response (hhc:page-home (model:magazine-articles) (model:magazine-series))))

(defun archive ()
  (ensure-published)
  (page-response (hhc:page-archive (model:magazine-articles) (model:magazine-series))))

(defun article (slug)
  (ensure-published)
  (let* ((articles (model:magazine-articles))
         (series (model:magazine-series))
         (found (model:find-article slug articles)))
    (if found
        (page-response (hhc:page-article found articles series))
        (s:http-response (hhc:page-not-found articles series) :code 404))))
