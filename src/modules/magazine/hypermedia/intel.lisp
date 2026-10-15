(in-package #:magazine/hypermedia/components)

;;;; Intel: updates about Common Lisp projects (see ../intel.lisp).
;;;
;;; Three things are set here (Figma: Intel · project activity feed):
;;;
;;;   the home page's band   a black rail with the count and the week, then
;;;                          four blocks: the newest releases, new projects
;;;                          and blog posts, and the followed projects'
;;;                          commits; under them, the channel live
;;;   the page at /intel     every published update, by day, beside an index
;;;   the desk               where fetched updates are given a line and
;;;                          published. Only on a machine that turns it on.
;;;
;;; An update's link leaves the magazine, so htmx leaves these alone.

(defparameter *intel-band-rows* 4
  "Updates each stream of the home page's band shows.")

(defparameter *intel-page-size* 60
  "Updates the Intel page shows before it is asked for them all.")

(defun intel-href (&key kind show)
  "The Intel page. KIND, \"release\" or \"new\", keeps only those updates;
SHOW is \"all\" for every update."
  (format nil "~a~@[?~{~a~^&~}~]" (s:url "magazine:intel")
          (loop for (name value) in (list (list "kind" kind) (list "show" show))
                when value
                  collect (format nil "~a=~a" name (quri:url-encode value)))))

(defun intel-desk-href (&optional view)
  (format nil "~a~@[?view=~a~]" (s:url "magazine:intel-desk") view))

(defun update-count-text (n)
  (format nil "~d update~:p" n))

(defun updates-of-kind (kind updates)
  (remove kind updates :key (lambda (update) (getf update :kind)) :test-not #'eq))

(defun updates-since (updates)
  "The month of the oldest of UPDATES, newest first."
  (press:format-date (intel:update-date (car (last updates))) :month))

(defun render-intel-kind (kind)
  (ah:</>
   (span :class (ah:clsx "intel-kind" (case kind (:new "new") (:post "post")))
     (case kind (:new "New") (:post "Post") (t "Release")))))

(defun render-intel-row (update &key chip)
  "UPDATE as two lines: the project, its version and the date, then the line
written for it. With CHIP, its kind comes first and its host is named."
  (let ((version (getf update :version))
        (day (press:format-date (intel:update-date update) :day-month)))
    (ah:</>
     (a :class "intel-row" :href (getf update :url) :rel "noopener"
       (span :class "intel-row-top"
         (when chip (render-intel-kind (getf update :kind)))
         (span :class "intel-project" (getf update :project))
         (cond (version (ah:</> (span :class "intel-version" version)))
               ((not chip) (ah:</> (span :class "intel-host" (getf update :host)))))
         (span :class "intel-when"
           (format nil "~a  >>>" (if chip (joined (getf update :host) day) day))))
       (span :class "intel-line" (getf update :line))))))

;;; The home page's band

(defun intel-week (updates)
  "The last seven days, oldest first, as (date count): how many of UPDATES
are dated each."
  (let ((today (press:today)))
    (loop for back from 6 downto 0
          for day = (local-time:timestamp- today back :day)
          collect (list day (count (press:format-date day :iso) updates
                                   :key (lambda (update) (getf update :date))
                                   :test #'string=)))))

(defun render-intel-rail (updates watching)
  "The band's black panel: the count, a bar for each day of the week, the
totals, and the way to the rest."
  (let* ((week (intel-week updates))
         (most (max 1 (reduce #'max week :key #'second))))
    (ah:</>
     (aside :class "intel-rail" :aria-label "Intel in numbers"
       (p :class "cat-label" (joined "Intel" "Intercepted this week"))
       (p :class "intel-count" (format nil "~d" (length updates)))
       (p :class "cat-label on" (format nil "Updates since ~a" (updates-since updates)))
       (div :class "intel-chart" :aria-hidden t
         (mapcar (lambda (day)
                   (destructuring-bind (date count) day
                     (ah:</>
                      (span :class "intel-day-bar"
                        (span (format nil "~d" count))
                        (i :style (format nil "--share: ~,3f" (/ count most)))
                        (span (subseq (press:format-date date :short-weekday) 0 1))))))
                 week))
       (render-spec (list (list "Releases" (format nil "~d" (length (updates-of-kind :release updates))))
                          (list "New projects" (format nil "~d" (length (updates-of-kind :new updates))))
                          (list "Posts" (format nil "~d" (length (updates-of-kind :post updates))))
                          (list "Commits this week" (commit-count-text (intel:published-commits)))
                          (list "Watched" (format nil "~d" watching)))
                    :class "spec intel-spec")
       (a :class "neo-button primary" :href (intel-href) "All updates")))))

(defun commit-count-text (summaries)
  "How many commits SUMMARIES, the followed projects' weeks, come to: with a
plus when any of them may have more than were counted."
  (format nil "~d~:[~;+~]" (reduce #'+ summaries :key (lambda (summary) (getf summary :count)))
          (some (lambda (summary) (getf summary :more)) summaries)))

(defun commit-rows (summaries)
  "SUMMARIES as updates, for RENDER-INTEL-ROW to set: the count where a
release has its version, the newest commit's first line as the line."
  (mapcar (lambda (summary)
            (list :project (getf summary :project)
                  :host (getf summary :host)
                  :url (getf summary :url)
                  :version (format nil "~d~:[~;+~] commit~p" (getf summary :count)
                                   (getf summary :more) (getf summary :count))
                  :date (getf summary :date)
                  :line (format nil "Latest: ~a" (getf summary :line))))
          summaries))

(defun render-intel-stream (title updates &key class count)
  "One kind of update under its heading, the newest of UPDATES as rows. COUNT
is what the heading says there are, when it isn't how many UPDATES are."
  (let ((shown (subseq updates 0 (min *intel-band-rows* (length updates)))))
    (ah:</>
     (section :class (ah:clsx "intel-stream" class) :|hx-boost:inherited| "false"
       (h3 :class "intel-stream-head"
         (span (joined title (or count (format nil "~d" (length updates)))))
         (when shown
           (ah:</> (span :class "intel-stream-note" (format nil "Latest ~d" (length shown))))))
       (if shown
           (mapcar #'render-intel-row shown)
           (ah:</> (p :class "intel-none" "Nothing on file yet.")))))))

(defun render-intel-section (updates watching)
  "The home page's Intel section: its head, then the band."
  (let* ((releases (updates-of-kind :release updates))
         (new (updates-of-kind :new updates))
         (posts (updates-of-kind :post updates))
         (commits (intel:published-commits))
         (shown (reduce #'+ (list releases new posts commits)
                        :key (lambda (stream) (min *intel-band-rows* (length stream))))))
    (ah:</>
     (<>
       (render-section-head "intel" "Intel" (update-count-text (length updates)) :jp "intel"
                                                                                 :action "All updates  >>>" :action-href (intel-href))
       (ac-band :class "intel-band"
         :side (list (joined "Intel" (format nil "~d latest" shown))
                     (joined (update-count-text (length updates))
                             (format nil "Since ~a" (updates-since updates))))
         ;; One child: a band's children all stand in the same cell.
         (div :class "intel-body"
           (div :class "intel-streams"
             (render-intel-rail updates watching)
             ;; Four blocks, two by two, beside the rail (Figma: Intel band
             ;; · E · Two by two).
             (render-intel-stream "Releases" releases)
             (render-intel-stream "New projects" new)
             (render-intel-stream "Blogs" posts)
             (render-intel-stream "Commits" (commit-rows commits)
                                  :count (commit-count-text commits)))
           (when (irc:running-p)
             (render-intel-live))))))))

;;; Live: what the IRC listener hears (see ../irc.lisp), newest last. The
;;; list asks the server for itself again every few seconds, and htmx
;;; changes it to match: the lines it has stay, the new ones are added.

(defparameter *live-home-lines* 8
  "Lines of the channel the homepage shows.")

(defun stream-href (count)
  (format nil "~a?n=~d" (s:url "magazine:intel-stream") count))

(defun render-irc-line (line)
  (multiple-value-bind (second minute hour) (decode-universal-time (getf line :time) 0)
    (declare (ignore second))
    (ah:</>
     (li :class (ah:clsx "irc-line" (when (getf line :action) "action"))
       :id (format nil "irc-~d" (getf line :id))
       (span :class "irc-time" (format nil "~2,'0d:~2,'0d" hour minute))
       (span :class "irc-nick" (getf line :nick))
       (span :class "irc-text" (getf line :text))))))

(defun render-irc-lines (count)
  (let ((lines (irc:recent-lines count)))
    (if lines
        (mapcar #'render-irc-line lines)
        (ah:</> (li :class "irc-none" "Nothing has been said since the listener joined.")))))

(defun irc-lines-html (count)
  "The newest COUNT lines alone, for a list of them to change itself to."
  (ah:render-to-string (ah:</> (<> (render-irc-lines count)))))

(defun render-irc-window (count seconds)
  "The newest COUNT lines, asked for again every SECONDS."
  (ah:</>
   (ul :class "irc-window" :id "irc-window"
     :hx-get (stream-href count) :hx-trigger (format nil "every ~ds" seconds)
     :hx-target "this" :hx-swap "innerMorph"
     (render-irc-lines count))))

(defun irc-where ()
  (joined (format nil "~{~a~^, ~}" irc:*channels*) irc:*server*))

(defun render-intel-live ()
  "The homepage's strip of the channel, under the band's streams."
  (ah:</>
   (div :class "intel-live"
     (p :class "intel-live-head"
       (span (joined "Live" (irc-where)))
       (span :class "intel-live-note" (joined "Monitor Chatter" "Times UTC")))
     (render-irc-window *live-home-lines* 10))))

(defun render-desk-live ()
  "The desk's Live list: the listener, to connect or disconnect, and what it
hears."
  (let* ((status (irc:status))
         (state (getf status :state))
         (on (irc:running-p)))
    (ah:</>
     (div :class "cat-list desk-list" :id "desk-list"
       (h3 :class "arch-month-head desk-list-head"
         (span "Live")
         (span :class "arch-count" (irc-where)))
       (form :class "desk-card" :method "post"
         :action (s:url (if on "magazine:intel-irc-stop" "magazine:intel-irc-start"))
         (render-form-token)
         (div :class "desk-card-head"
           (span :class (ah:clsx "intel-kind" (unless (eq state :listening) "new"))
             (case state
               (:listening "Listening")
               (:connecting "Connecting")
               (:waiting "Waiting to reconnect")
               (t "Off")))
           (span :class "intel-project" (or (getf status :nick) irc:*nick*))
           (span :class "desk-meta" (irc-where)))
         (when (getf status :note)
           (ah:</> (p :class "desk-quiet" (getf status :note))))
         (p :class "desk-quiet"
           "The listener reads and never speaks. People in the channel see it join and leave. While it listens, the homepage shows the newest lines too.")
         (div :class "desk-actions"
           (button :class "desk-button primary" :type "submit" (if on "Disconnect" "Connect"))))
       (div :class "desk-live"
         (render-irc-window 200 4))))))

;;; The Intel page: an index beside the updates, a day at a time.

(defparameter *intel-kinds* '(("release" :release "Releases") ("new" :new "New projects")
                              ("post" :post "Posts"))
  "Each kind of update: its name in an address, itself, and its heading.")

(defun group-by-day (updates)
  "UPDATES, newest first, as a list of (date-text . updates), a day each."
  (let ((days nil))
    (dolist (update updates (nreverse (mapcar (lambda (day) (cons (car day) (reverse (cdr day))))
                                              days)))
      (if (and days (string= (car (first days)) (getf update :date)))
          (push update (cdr (first days)))
          (push (list (getf update :date) update) days)))))

(defun render-intel-index (updates shown watching kind)
  "The index of UPDATES, SHOWN of them listed: those of KIND, a name of
*INTEL-KINDS*."
  (ah:</>
   (aside :class "cat-index" :aria-label "Index"
     (div :class "cat-index-inner"
       (div
         (p :class "cat-label" (joined "Intel" "On file"))
         (p :class "cat-count" (format nil "~d" (length shown)))
         (p :class "cat-label on" (format nil "Updates since ~a" (updates-since updates))))
       (div :class "cat-group"
         (p :class "cat-head" "By kind")
         (render-catalogue-row "All" (format nil "~d" (length updates))
                               :href (intel-href) :pressed (null kind))
         (mapcar (lambda (one)
                   (destructuring-bind (name key title) one
                     (render-catalogue-row
                      title (two-digits (length (updates-of-kind key updates)))
                      :href (intel-href :kind (unless (equal name kind) name))
                      :pressed (equal name kind))))
                 *intel-kinds*))
       (div :class "cat-group"
         (p :class "cat-head" "Most active")
         (mapcar (lambda (one)
                   (render-catalogue-row (first one) (two-digits (second one))))
                 (let ((projects (tally updates (lambda (update) (getf update :project)))))
                   (subseq projects 0 (min 6 (length projects))))))
       (div :class "cat-group"
         (p :class "cat-head" "By month")
         (mapcar (lambda (one)
                   (render-catalogue-row (first one) (two-digits (second one))))
                 (tally updates (lambda (update)
                                  (press:format-date (intel:update-date update) :month)))))
       (p :class "cat-label" (format nil "Watching ~d project~:p" watching))))))

(defun render-intel-day (day)
  (destructuring-bind (date . updates) day
    (ah:</>
     (section :class "intel-day"
       (h3 :class "arch-month-head"
         (span (press:format-date (press:parse-date date) :long))
         (span :class "arch-count" (update-count-text (length updates))))
       (mapcar (lambda (update) (render-intel-row update :chip t)) updates)))))

(defun page-intel (updates watching &key kind show)
  "The Intel page: UPDATES, newest first. KIND and SHOW are INTEL-HREF's."
  (let* ((key (second (assoc kind *intel-kinds* :test #'equal)))
         (kind (when key kind))
         (matching (if key (updates-of-kind key updates) updates))
         (shown (if (equal show "all")
                    matching
                    (subseq matching 0 (min *intel-page-size* (length matching)))))
         (*marked-bands* 0))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title "Intel — Almighty Lisp"
        :description "Releases and new projects from across Common Lisp, a line each."
        :page-class "page-archive page-intel"
        (render-section-head "archive" "Intel" (update-count-text (length updates))
                             :jp "intel"
                             :action (joined (update-count-text (length updates)) "Newest first"))
        (ac-band :class "archive-band"
          :side (list (joined "Index" "Kind" "Project" "Month")
                      (joined "Intel" (when updates
                                        (format nil "Since ~a" (updates-since updates)))))
          (if updates
              (ah:</>
               (div :class "catalogue"
                 (render-intel-index updates matching watching kind)
                 (div :class "cat-list intel-list" :|hx-boost:inherited| "false"
                   (mapcar #'render-intel-day (group-by-day shown))
                   (div :class "cat-foot"
                     (span (format nil "Showing  ~d of ~d" (length shown) (length matching)))
                     (when (< (length shown) (length matching))
                       (ah:</>
                        (a :class "cat-foot-button" :href (intel-href :kind kind :show "all")
                          "Older updates  >>>")))))))
              (ah:</>
               (div :class "end-block"
                 (p :class "end-label" "Nothing has been published yet."))))))))))

;;; The desk (Figma: Intel desk A · Inbox ledger). Its forms post to the
;;; server, which answers by sending the desk again.

(defparameter *desk-views* '(("queue" :new "To review")
                             ("following" nil "Following")
                             ("active" nil "Active elsewhere")
                             ("live" nil "Live")
                             ("published" :published "Published")
                             ("dismissed" :dismissed "Dismissed"))
  "Each list of the desk: its name in an address, the status of the updates
in it, and its heading. Three have no status. Two list projects: the
followed ones (see render-following-card), and those pushed to lately that
the watch list doesn't have (see render-active-card). Live is what the IRC
listener hears (see render-desk-live).")

(defun update-followed-p (update followed)
  "True when UPDATE is of a project among FOLLOWED, a list of keys."
  (member (intel:update-key (getf update :id)) followed :test #'string-equal))

(defun render-desk-index (inbox about watch view)
  (flet ((with-status (status)
           (count status inbox :key (lambda (update) (getf update :status)))))
    (let ((queue (updates-of-status :new inbox))
          (fetched (getf about :fetched)))
      (ah:</>
       (aside :class "cat-index" :id "desk-index" :aria-label "Desk"
         (div :class "cat-index-inner"
           (div
             (p :class "cat-label" (joined "Desk" "This machine only"))
             (p :class "cat-count" (format nil "~d" (length queue)))
             (p :class "cat-label on" "To review"))
           (div :class "cat-group"
             (p :class "cat-head" "Lists")
             (mapcar (lambda (one)
                       (destructuring-bind (name status title) one
                         (render-catalogue-row
                          title
                          (cond (status (two-digits (with-status status)))
                                ((equal name "active")
                                 (two-digits (length (intel:active-projects))))
                                ((equal name "live") (if (irc:running-p) "On" "Off"))
                                ;; Following: those with something new.
                                (t (two-digits (count-if (lambda (project) (getf project :fresh))
                                                         (intel:followed-projects)))))
                          :href (intel-desk-href name)
                          :pressed (equal name view))))
                     *desk-views*))
           (div :class "cat-group"
             (p :class "cat-head" "In the queue")
             (mapcar (lambda (one)
                       (destructuring-bind (name key title) one
                         (declare (ignore name))
                         (render-catalogue-row
                          title (two-digits (length (updates-of-kind key queue))))))
                     *intel-kinds*))
           (div :class "cat-group"
             (p :class "cat-head" "Watch list")
             (render-catalogue-row "Projects" (format nil "~d" (length watch)))
             (render-catalogue-row "Blogs" (format nil "~d" (length (intel:blog-list))))
             (render-catalogue-row "Last fetch"
                                   (if fetched
                                       (multiple-value-bind (second minute hour day month)
                                           (decode-universal-time fetched)
                                         (declare (ignore second))
                                         (format nil "~d/~d ~2,'0d:~2,'0d" day month hour minute))
                                       "Never"))
             (when fetched
               (render-catalogue-row "Brought in" (format nil "~d" (getf about :added 0))))
             (form :method "post" :action (s:url "magazine:intel-fetch")
               (render-form-token)
               (button :class "desk-button paper" :type "submit" "Fetch now"))
             (p :class "desk-note"
               "Projects are listed in content/intel/watch.txt and blogs in blogs.txt beside it. A fetch asks each one's host, and takes a moment."))
           (when (getf about :errors)
             (ah:</>
              (div :class "cat-group"
                (p :class "cat-head" "Not reached")
                (mapcar (lambda (text) (ah:</> (p :class "desk-note" text)))
                        (getf about :errors)))))))))))

;; Shiso turns away a posted form that doesn't carry its session's token.
(defun render-form-token ()
  (let ((token (and s:*request* (s:csrf-token-value))))
    (when token
      (ah:</> (input :type "hidden" :name "_csrf_token" :value token)))))

(defun updates-of-status (status updates)
  (remove status updates :key (lambda (update) (getf update :status)) :test-not #'eq))

(defun render-desk-line (update)
  "The field an update's line is written in, with how much of it is used."
  (let ((line (or (getf update :line) "")))
    (ah:</>
     (label :class "desk-line"
       (span :class "desk-line-label"
         (span "Line for Intel")
         (span :class "desk-count" (format nil "~d/~d" (length line) intel:*line-limit*)))
       (input :type "text" :name "line" :value line :required t
         :maxlength (format nil "~d" intel:*line-limit*) :autocomplete "off"
         :oninput (format nil "this.closest('label').querySelector('.desk-count').textContent = this.value.length + '/~d'"
                          intel:*line-limit*))))))

(defun desk-card-id (update)
  "An id for UPDATE's card, so a card keeps what was typed into it when the
list around it changes."
  (format nil "desk-~a" (ppcre:regex-replace-all "[^A-Za-z0-9]+" (getf update :id) "-")))

(defun render-desk-card (update view &optional followed)
  "UPDATE with what the desk can do about it in VIEW, the list it is in.
FOLLOWED is the followed projects' keys."
  (let ((status (getf update :status))
        (following (update-followed-p update followed)))
    (ah:</>
     (form :class "desk-card" :method "post" :action (s:url "magazine:intel-publish")
       :id (desk-card-id update)
       (render-form-token)
       ;; Not named "id": a field of that name takes the place of the form's
       ;; own id wherever a script reads it.
       (input :type "hidden" :name "update" :value (getf update :id))
       (input :type "hidden" :name "view" :value view)
       (div :class "desk-card-head"
         (render-intel-kind (getf update :kind))
         (span :class "intel-project" (getf update :project))
         (when (getf update :version)
           (ah:</> (span :class "intel-version" (getf update :version))))
         (when following
           (ah:</> (span :class "intel-kind follow" "Following")))
         (span :class "desk-meta"
           (joined (date-text (intel:update-date update))
                   (getf update :host)
                   (when (getf update :stars)
                     (format nil "~d star~:p" (getf update :stars))))))
       (when (plusp (length (getf update :source)))
         (ah:</>
          (div :class "desk-source"
            (p :class "desk-source-label" "From the source")
            (p :class "desk-source-text" (getf update :source)))))
       (unless (eq status :dismissed)
         (render-desk-line update))
       (div :class "desk-actions"
         (case status
           (:new
            (ah:</>
             (<>
               (button :class "desk-button primary" :type "submit" "Publish")
               (button :class "desk-button" :type "submit" :formnovalidate t
                 :formaction (s:url "magazine:intel-dismiss")
                 "Dismiss"))))
           (:published
            (ah:</>
             (<>
               (button :class "desk-button primary" :type "submit" "Save the line")
               (button :class "desk-button" :type "submit" :formnovalidate t
                 :formaction (s:url "magazine:intel-unpublish")
                 "Unpublish"))))
           (t
            (ah:</>
             (button :class "desk-button" :type "submit"
               :formaction (s:url "magazine:intel-restore")
               "Back to the queue"))))
         ;; A post has no project to follow.
         (unless (eq (getf update :kind) :post)
           (ah:</>
            (button :class "desk-button quiet" :type "submit" :formnovalidate t
              :formaction (s:url (if following "magazine:intel-unfollow" "magazine:intel-follow"))
              (if following "Unfollow" "Follow the project"))))
         (a :class "desk-open" :href (getf update :url) :target "_blank" :rel "noopener"
           "Open the source  >>>"))))))

(defun desk-view (view)
  "VIEW's entry in *DESK-VIEWS*, the queue's when there is no such list."
  (or (assoc view *desk-views* :test #'equal) (first *desk-views*)))

(defun render-commit (commit)
  (ah:</>
   (li
     (a :class "desk-commit" :href (getf commit :url) :target "_blank" :rel "noopener"
       (span :class "arch-when" (press:format-date (press:parse-date (getf commit :date)) :day-month))
       (span :class "desk-commit-title" (getf commit :title))
       (span :class "desk-commit-by" (getf commit :author))))))

(defparameter *commits-shown* 8
  "The most commits a followed project's card lists.")

(defun render-following-card (project inbox)
  "A followed PROJECT: the commits new since it was last looked at, and how
much of INBOX's queue is its."
  (let* ((fresh (getf project :fresh))
         (commits (getf project :commits))
         (shown (subseq fresh 0 (min *commits-shown* (length fresh))))
         (waiting (count-if (lambda (update)
                              (and (eq (getf update :status) :new)
                                   (string-equal (intel:update-key (getf update :id))
                                                 (getf project :key))))
                            inbox)))
    (ah:</>
     (form :class "desk-card" :method "post" :action (s:url "magazine:intel-seen")
       :id (format nil "follow-~a" (ppcre:regex-replace-all "[^A-Za-z0-9]+" (getf project :key) "-"))
       (render-form-token)
       (input :type "hidden" :name "update" :value (getf project :key))
       (input :type "hidden" :name "view" :value "following")
       (div :class "desk-card-head"
         (span :class (ah:clsx "intel-kind" (unless fresh "new"))
           (cond ((null fresh) "Quiet")
                 ;; Every commit kept is new: there may be more than these.
                 ((= (length fresh) (length commits))
                  (format nil "~d+ new commits" (length fresh)))
                 (t (format nil "~d new commit~:p" (length fresh)))))
         (span :class "intel-project" (getf project :name))
         (span :class "desk-meta"
           (joined (getf project :host)
                   (when (plusp waiting) (format nil "~d in the queue" waiting)))))
       (cond (shown
              (ah:</>
               (<>
                 (ul :class "desk-commits" (mapcar #'render-commit shown))
                 (when (< (length shown) (length fresh))
                   (ah:</>
                    (p :class "desk-quiet"
                      (format nil "And ~d more." (- (length fresh) (length shown)))))))))
             ((not (getf project :tracked))
              (ah:</>
               (p :class "desk-quiet"
                 "Commits are followed on GitHub only. This project's releases still arrive in the queue, first.")))
             ((not (getf project :checked))
              (ah:</> (p :class "desk-quiet" "Not asked yet. Fetch now looks for its commits.")))
             (commits
              (ah:</>
               (<>
                 (p :class "desk-quiet" "Nothing new since you last looked. The latest:")
                 (ul :class "desk-commits" (render-commit (first commits)))))))
       (div :class "desk-actions"
         (when fresh
           (ah:</> (button :class "desk-button primary" :type "submit" "Mark as seen")))
         (button :class "desk-button" :type "submit"
           :formaction (s:url "magazine:intel-unfollow")
           "Unfollow")
         (a :class "desk-open" :href (format nil "~a~:[~;/commits~]" (getf project :url)
                                             (getf project :tracked))
           :target "_blank" :rel "noopener"
           (if (getf project :tracked) "Open the commits  >>>" "Open the project  >>>")))))))

(defun render-active-card (repository)
  "REPOSITORY, pushed to lately and not on the watch list, to follow or hide."
  (ah:</>
   (form :class "desk-card" :method "post" :action (s:url "magazine:intel-follow")
     :id (format nil "active-~a" (ppcre:regex-replace-all "[^A-Za-z0-9]+" (getf repository :key) "-"))
     (render-form-token)
     (input :type "hidden" :name "update" :value (getf repository :key))
     (input :type "hidden" :name "view" :value "active")
     (div :class "desk-card-head"
       (span :class "intel-kind new" "Active")
       (span :class "intel-project" (getf repository :name))
       (span :class "desk-meta"
         (joined (format nil "Pushed ~a" (date-text (press:parse-date (getf repository :pushed))))
                 (getf repository :host)
                 (format nil "~d star~:p" (getf repository :stars)))))
     (when (plusp (length (getf repository :description)))
       (ah:</> (p :class "desk-quiet" (getf repository :description))))
     (div :class "desk-actions"
       (button :class "desk-button primary" :type "submit" "Follow the project")
       (button :class "desk-button" :type "submit"
         :formaction (s:url "magazine:intel-hide")
         "Hide")
       (a :class "desk-open" :href (format nil "~a/commits" (getf repository :url))
         :target "_blank" :rel "noopener"
         "Open the commits  >>>")))))

(defun render-desk-list (inbox view)
  "The updates of INBOX in VIEW, an entry of *DESK-VIEWS*, a card each: the
followed projects' first. A view with no status lists projects: Following
the followed ones, Active elsewhere those pushed to lately."
  (destructuring-bind (name status title) view
    (let* ((projects (intel:followed-projects))
           (followed (mapcar (lambda (project) (getf project :key)) projects))
           (active (equal name "active"))
           (listed (cond (status
                          (stable-sort (updates-of-status status inbox)
                                       (lambda (a b) (and a (not b)))
                                       :key (lambda (update)
                                              (and (update-followed-p update followed) t))))
                         (active (intel:active-projects))
                         ;; Those with something new first.
                         (t (stable-sort (copy-list projects)
                                         (lambda (a b) (and a (not b)))
                                         :key (lambda (project) (and (getf project :fresh) t)))))))
      (ah:</>
       ;; htmx posts a card's form, to the address of the button pressed,
       ;; and follows the answer back to the desk. Of that page it takes
       ;; this list, and changes the one here to match, so a line being
       ;; typed in another card stays; and the index, for its counts. The
       ;; rest of the page is left as it is, and where it is: a form htmx
       ;; has boosted is otherwise scrolled to the top of what it changed.
       (div :class "cat-list desk-list" :id "desk-list"
         :|hx-target:inherited| "#desk-list" :|hx-select:inherited| "#desk-list"
         :|hx-swap:inherited| "outerMorph show:none" :|hx-select-oob:inherited| "#desk-index"
         :|hx-push-url:inherited| "false"
         (h3 :class "arch-month-head desk-list-head"
           (span title)
           (span :class "arch-count"
             (if status
                 (update-count-text (length listed))
                 (format nil "~d project~:p" (length listed)))))
         (cond (status
                (mapcar (lambda (update) (render-desk-card update name followed)) listed))
               (active (mapcar #'render-active-card listed))
               (t (mapcar (lambda (project) (render-following-card project inbox)) listed)))
         (unless listed
           (ah:</>
            (p :class "cat-none"
              (cond ((equal name "queue") "Nothing to review. Fetch now looks for more.")
                    (active
                     "Nothing to offer. Fetch now asks which Common Lisp repositories were pushed to lately; those already on the watch list aren't listed.")
                    ((null status)
                     "No project is followed. Follow the project, on a card in the queue, follows one; so does a * before its line in content/intel/watch.txt.")
                    (t "Nothing here."))))))))))

(defun page-intel-desk (inbox about watch &key view)
  "The desk: INBOX's updates in VIEW, a name of *DESK-VIEWS*. ABOUT is the
last fetch, WATCH the watch list."
  (let ((view (desk-view view))
        (*marked-bands* 0))
    (ah:render-to-string
     (ah:</>
      (ac-sheet-page :title "Intel desk — Almighty Lisp"
        :description "Where Intel's updates are chosen."
        :page-class "page-archive page-intel-desk"
        (render-section-head "archive" "Intel desk" (third view)
                             :action "The Intel page  >>>" :action-href (intel-href))
        (ac-band :class "archive-band"
          :side (list (joined "Desk" (third view)) (joined "Intel" "Not published"))
          (div :class "catalogue"
            (render-desk-index inbox about watch (first view))
            (if (equal (first view) "live")
                (render-desk-live)
                (render-desk-list inbox view)))))))))
