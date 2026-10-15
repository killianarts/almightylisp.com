(defpackage #:magazine/routes
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:controllers #:magazine/controllers)))

(in-package #:magazine/routes)

(s:define-module magazine
  (:urls (:GET "/" #'controllers:index "index")
         (:GET "/archive" #'controllers:archive "archive")
         (:GET "/archive/lab" #'controllers:archive-lab "archive-lab")
         (:GET "/docs/lab" #'controllers:docs-lab "docs-lab")
         (:GET "/search" #'controllers:search-results "search")
         (:GET "/intel" #'controllers:intel "intel")
         (:GET "/intel/desk" #'controllers:intel-desk "intel-desk")
         (:POST "/intel/desk/fetch" #'controllers:intel-fetch "intel-fetch")
         (:POST "/intel/desk/publish" #'controllers:intel-publish "intel-publish")
         (:POST "/intel/desk/unpublish" #'controllers:intel-unpublish "intel-unpublish")
         (:POST "/intel/desk/dismiss" #'controllers:intel-dismiss "intel-dismiss")
         (:POST "/intel/desk/restore" #'controllers:intel-restore "intel-restore")
         (:POST "/intel/desk/follow" #'controllers:intel-follow "intel-follow")
         (:POST "/intel/desk/unfollow" #'controllers:intel-unfollow "intel-unfollow")
         (:POST "/intel/desk/seen" #'controllers:intel-seen "intel-seen")
         (:POST "/intel/desk/hide" #'controllers:intel-hide "intel-hide")
         (:GET "/intel/stream" #'controllers:intel-stream "intel-stream")
         (:POST "/intel/desk/irc/start" #'controllers:intel-irc-start "intel-irc-start")
         (:POST "/intel/desk/irc/stop" #'controllers:intel-irc-stop "intel-irc-stop")
         (:GET "/article/:slug" #'controllers:article "article")))

;; The magazine used to be at /magazine. This is mounted there, and sends each
;; of its old addresses to the same page at the root.
(s:define-module magazine-moved
  (:urls (:GET "/" #'controllers:moved "index")
         (:GET "/*" #'controllers:moved "page")))
