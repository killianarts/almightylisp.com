(defpackage #:magazine/controllers
  (:use #:cl)
  (:local-nicknames (#:s #:shiso)
                    (#:model #:magazine/model)
                    (#:intel #:magazine/intel)
                    (#:irc #:magazine/irc)
                    (#:pub #:magazine/publish)
                    (#:hhc #:magazine/hypermedia/components))
  (:export #:index #:archive #:archive-lab #:docs-lab #:search-results #:article
           #:intel #:intel-desk #:intel-fetch #:intel-publish #:intel-unpublish
           #:intel-dismiss #:intel-restore
           #:intel-follow #:intel-unfollow #:intel-seen #:intel-hide
           #:intel-stream #:intel-irc-start #:intel-irc-stop
           #:moved))

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
  ;; Where the image is to listen to IRC, it starts with the first visit.
  (irc:ensure-started)
  (page-response (hhc:page-home (model:magazine-articles) (model:magazine-series)
                                (intel:published-updates) (length (intel:watch-list)))))

(defun query-param (name)
  "The request's query parameter NAME, or NIL when it is missing or empty."
  (when s:*request*
    (let ((value (cdr (assoc name (lack/request:request-query-parameters s:*request*)
                             :test #'string=))))
      (and (stringp value) (plusp (length value)) value))))

(defun archive ()
  "The archive: ?type and ?series keep only those articles, ?open opens a
month, or \"all\" of them."
  (ensure-published)
  (page-response (hhc:page-archive (model:magazine-articles) (model:magazine-series)
                                   :type (query-param "type")
                                   :in (query-param "series")
                                   :open (query-param "open"))))

(defun archive-lab ()
  "The archive's candidate designs (see hypermedia/archive-lab.lisp): ?design
picks one, ?data=live lists the magazine's own articles instead of the sample."
  (page-response (hhc:page-archive-lab (model:magazine-articles) (model:magazine-series)
                                       :design (query-param "design")
                                       :data (query-param "data"))))

(defun docs-lab ()
  "The Almighty Docs' test page (see hypermedia/docs-lab.lisp)."
  (page-response (hhc:page-docs-lab)))

(defun search-results ()
  "The search palette's results (see hypermedia/search.lisp), for the search
the address describes: ?q in ?field, with article ?sel chosen, match ?focus
of it in focus and, with ?wide, more of the text around it. Not for a cache
to keep."
  (flet ((number-param (name)
           (let ((value (query-param name)))
             (and value (parse-integer value :junk-allowed t)))))
    (s:http-response
     (hhc:render-search-results (model:magazine-articles) (model:magazine-series)
                                :query (query-param "q")
                                :field (query-param "field")
                                :selected (number-param "sel")
                                :focus (number-param "focus")
                                :wide (and (query-param "wide") t))
     :headers '(:cache-control "no-store"))))

;;; Intel (see intel.lisp): the page everyone gets, and the desk, which is
;;; found only where INTEL:DESK-ENABLED-P is true.

(defun intel ()
  "The Intel page: ?kind keeps one kind of update, ?show=all lists them all."
  (page-response (hhc:page-intel (intel:published-updates) (length (intel:watch-list))
                                 :kind (query-param "kind")
                                 :show (query-param "show"))))

(defun body-param (name)
  "The posted form's field NAME, or NIL when it is missing."
  (when s:*request*
    (let ((value (cdr (assoc name (lack/request:request-body-parameters s:*request*)
                             :test #'string=))))
      (and (stringp value) value))))

(defun not-found ()
  (s:http-response (hhc:page-not-found (model:magazine-articles) (model:magazine-series))
                   :code 404))

(defun desk-page (&optional (view (query-param "view")))
  (multiple-value-bind (inbox about) (intel:inbox)
    (s:http-response (hhc:page-intel-desk inbox about (intel:watch-list) :view view)
                     :headers '(:cache-control "no-store"))))

(defun intel-desk ()
  "The desk: ?view is the list to show, the queue when there is none."
  (if (intel:desk-enabled-p)
      (desk-page)
      (not-found)))

(defun desk-action (action &key publishes)
  "Do ACTION, a function of the posted update's id, and send the desk back to
the list it was posted from. With PUBLISHES, the static pages are written
again: what the site shows has changed."
  (cond ((not (intel:desk-enabled-p)) (not-found))
        (t (let ((id (body-param "update")))
             (when id
               (funcall action id)
               (when publishes
                 (handler-case (pub:publish-site)
                   (error (condition)
                     (warn "Magazine publish failed: ~a" condition))))))
           (s:redirect-response (format nil "~a~@[?view=~a~]" (s:url "magazine:intel-desk")
                                        (body-param "view"))
                                :code 303))))

(defun intel-fetch ()
  "Fetch the watch list's releases and the new repositories into the queue.
The static pages are written again: a fetch changes what the site shows of
the followed projects' commits."
  (cond ((not (intel:desk-enabled-p)) (not-found))
        (t (handler-case (progn (intel:fetch-updates)
                                (pub:publish-site))
             (error (condition)
               (warn "Intel fetch failed: ~a" condition)))
           (s:redirect-response (s:url "magazine:intel-desk") :code 303))))

(defun intel-publish ()
  (desk-action (lambda (id) (intel:publish-update id (body-param "line"))) :publishes t))

(defun intel-unpublish ()
  (desk-action #'intel:unpublish-update :publishes t))

(defun intel-dismiss ()
  (desk-action #'intel:dismiss-update))

(defun intel-restore ()
  (desk-action #'intel:restore-update))

(defun intel-follow ()
  (desk-action #'intel:follow))

(defun intel-unfollow ()
  (desk-action #'intel:unfollow))

(defun intel-seen ()
  (desk-action #'intel:mark-seen))

(defun intel-hide ()
  (desk-action #'intel:hide-active))

;;; Live (see irc.lisp): the channel's newest lines, for the lists that show
;;; them to ask for again. Found while the listener runs, and on the desk.

(defun intel-stream ()
  "The newest ?n lines the listener has heard, as list items."
  (if (or (irc:running-p) (intel:desk-enabled-p))
      (let ((count (or (ignore-errors (parse-integer (or (query-param "n") ""))) 8)))
        (s:http-response (hhc:irc-lines-html (max 1 (min 300 count)))
                         :headers '(:cache-control "no-store")))
      (not-found)))

(defun irc-action (action)
  (cond ((not (intel:desk-enabled-p)) (not-found))
        (t (funcall action)
           (s:redirect-response (format nil "~a?view=live" (s:url "magazine:intel-desk"))
                                :code 303))))

(defun intel-irc-start ()
  (irc-action #'irc:start))

(defun intel-irc-stop ()
  (irc-action #'irc:stop))

(defun article (slug)
  (ensure-published)
  (let* ((articles (model:magazine-articles))
         (series (model:magazine-series))
         (found (model:find-article slug articles)))
    (if found
        (page-response (hhc:page-article found articles series))
        (s:http-response (hhc:page-not-found articles series) :code 404))))

(defun moved (&rest splat)
  "Send an address under /magazine, where the magazine used to be, to where
its page is now: an article's to /article/:slug, any other to the same path at
the root. The path asked for is what follows the mount; its leading slashes
are cut to one, so that //host can't send a reader to another site."
  (declare (ignore splat))
  (let* ((path (string-left-trim "/" (lack/request:request-path-info s:*request*)))
         (query (lack/request:request-query-string s:*request*))
         (article (model:find-article path (model:magazine-articles))))
    (s:redirect-response (format nil "~a~@[?~a~]"
                                 (if article
                                     (s:url "magazine:article" :slug path)
                                     (concatenate 'string "/" path))
                                 (and query (plusp (length query)) query))
                         :code 301)))
