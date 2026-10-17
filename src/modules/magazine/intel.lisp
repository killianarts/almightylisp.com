(defpackage #:magazine/intel
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export
   ;; What the site shows
   #:published-updates #:update-date #:watch-list #:blog-list #:published-commits
   ;; The desk
   #:desk-enabled-p #:*line-limit*
   #:inbox #:fetch-updates
   #:publish-update #:unpublish-update #:dismiss-update #:restore-update
   ;; Following
   #:followed-projects #:update-key #:follow #:unfollow #:mark-seen
   ;; Active elsewhere
   #:active-projects #:hide-active
   ;; Places
   #:intel-directory #:watch-file #:blogs-file #:updates-file #:inbox-file
   #:following-file #:active-file #:commits-file))

(in-package #:magazine/intel)

;;;; Intel: short updates about activity in Common Lisp projects.
;;;
;;; Four files in content/intel/:
;;;
;;;   watch.txt      the projects to watch, one repository URL a line,
;;;                  optionally followed by the name to show it under
;;;   blogs.txt      the blogs to watch, one feed's URL a line, optionally
;;;                  followed by the name to show its posts under
;;;   updates.sexp   the published updates, one plist a line. This is the
;;;                  file the site reads, and the one to commit.
;;;   inbox.sexp     everything fetched, with what was done about it: new,
;;;                  published or dismissed. Local to the machine the desk
;;;                  runs on; git ignores it.
;;;
;;; A project whose line in watch.txt starts with * is followed: the desk
;;; puts its updates first, and a fetch also asks for its newest commits,
;;; so the desk can say which are new since they were last looked at. Those
;;; are kept, with the last one seen, in following.sexp, local as the inbox
;;; is. Commits are not updates: nobody writes a line for them. What the
;;; site shows of them is a count: commits.sexp has, for each followed
;;; project, how many commits the week brought and the newest one's first
;;; line. A fetch writes it, and it is committed as updates.sexp is.
;;;
;;; A fetch also asks GitHub which Common Lisp repositories have been pushed
;;; to lately, so that an established project nobody listed is still
;;; noticed. Those the watch list doesn't have are kept in active.sexp, local
;;; too, for the desk to offer: each can be followed, or hidden for good.
;;;
;;; Three kinds of update are fetched: a release or tag of a watched project,
;;; from its host's feed; a new Common Lisp repository, from GitHub's search;
;;; and a post on a watched blog, from its feed. Nothing is published by
;;; fetching. The desk (hypermedia/intel.lisp)
;;; lists what came in, and a person writes a line for each update worth
;;; passing on.
;;;
;;; An update is a plist:
;;;
;;;   :id       "github.com/sbcl/sbcl@sbcl-2.6.9"; names it across fetches
;;;   :kind     :release, :new or :post
;;;   :project  the name it is shown under: a project's, or a post's author's
;;;   :host     "github.com"
;;;   :url      the release's, the repository's or the post's page
;;;   :version  "2.6.9"; NIL for a new repository or a post
;;;   :date     "2026-09-26", when it was released, created or posted
;;;   :line     the one line the site prints
;;;
;;; and in the inbox also :status (:new, :published or :dismissed), :source
;;; (the release notes, the repository's description or the start of the
;;; post, as text), and for a new repository :stars.

(defparameter *line-limit* 160
  "The longest line an update is published with.")

(defparameter *release-days* 45
  "A release older than this many days is not brought into the inbox.")

(defparameter *releases-per-project* 3
  "The most releases of one project a fetch brings in.")

(defparameter *new-repository-days* 14
  "How far back a fetch looks for new repositories.")

(defparameter *new-repositories* 40
  "The most new repositories a fetch asks for, most starred first.")

(defparameter *post-days* 30
  "A post older than this many days is not brought into the inbox.")

(defparameter *posts-per-blog* 10
  "The most posts of one blog a fetch brings in.")

(defparameter *active-days* 7
  "How far back a fetch looks for repositories that have been pushed to.")

(defparameter *active-stars* 20
  "The fewest stars a repository has to be noticed for being pushed to.")

(defparameter *active-repositories* 100
  "The most such repositories a fetch asks for, the last pushed to first: as
many as GitHub gives in one answer.")

(defparameter *commits-kept* 20
  "The newest commits kept for each followed project.")

(shiso:define-setting :magazine/desk-enabled shiso:boolean-setting
  "Whether the Intel desk answers on this machine: anywhere else its
addresses are not found, so the public site never serves it."
  :environment "INTEL_DESK"
  :default (shiso:per-environment :development t :otherwise nil))

(shiso:define-setting :magazine/github-token shiso:string-setting
  "Sent with the desk's GitHub searches, for a higher rate limit."
  :environment "GITHUB_TOKEN"
  :secret t)

(defun desk-enabled-p ()
  "True on a machine the desk is for (:magazine/desk-enabled)."
  (shiso:setting :magazine/desk-enabled))

;;; Places

(defun project-root ()
  (or (ignore-errors (asdf:system-source-directory "almightylisp"))
      (uiop:getcwd)))

(defun intel-directory ()
  (merge-pathnames "content/intel/" (project-root)))

(defun watch-file () (merge-pathnames "watch.txt" (intel-directory)))
(defun blogs-file () (merge-pathnames "blogs.txt" (intel-directory)))
(defun active-file () (merge-pathnames "active.sexp" (intel-directory)))
(defun commits-file () (merge-pathnames "commits.sexp" (intel-directory)))
(defun following-file () (merge-pathnames "following.sexp" (intel-directory)))
(defun updates-file () (merge-pathnames "updates.sexp" (intel-directory)))
(defun inbox-file () (merge-pathnames "inbox.sexp" (intel-directory)))

;;; Reading and writing plists, one a line

(defun read-forms (file)
  (when (probe-file file)
    (with-open-file (in file :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (loop for form = (read in nil in)
                until (eq form in)
                collect form))))))

(defun write-forms (file forms heading)
  (ensure-directories-exist file)
  (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
    (format out ";;;; ~a~%" heading)
    (with-standard-io-syntax
      ;; Not readably: that writes a string with its element type, as an array.
      (let ((*print-pretty* nil)
            (*print-readably* nil))
        (dolist (form forms)
          (prin1 form out)
          (terpri out))))))

;;; Dates are written YYYY-MM-DD, which sorts as text.

(defun iso-date (universal-time)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal-time 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun days-ago (days)
  (iso-date (- (get-universal-time) (* days 86400))))

(defun update-date (update)
  "UPDATE's date, as almighty-press formats one."
  (press:parse-date (getf update :date)))

(defun newest-first (updates)
  (stable-sort (copy-list updates) #'string> :key (lambda (update) (getf update :date))))

;;; The watch list

(defun feed-url (host url)
  "Where HOST lists the releases or tags of the repository at URL. A URL on a
host that isn't known is taken to be a feed already."
  (cond ((string= host "github.com") (format nil "~a/releases.atom" url))
        ((search "gitlab" host) (format nil "~a/-/tags?format=atom" url))
        ((or (string= host "codeberg.org") (search "gitea" host) (search "forgejo" host))
         (format nil "~a/tags.atom" url))
        (t url)))

(defun parse-watch-line (line)
  "The project LINE names, or NIL: a URL, then optionally a name. A * before
the URL marks the project as followed. A line that starts with ; or # is a
comment."
  (let* ((words (remove "" (uiop:split-string (string-trim '(#\Space #\Tab #\Return) line)
                                              :separator '(#\Space #\Tab))
                        :test #'string=))
         (followed (and words (char= (char (first words) 0) #\*)))
         (words (if followed
                    (remove "" (cons (string-left-trim "*" (first words)) (rest words))
                            :test #'string=)
                    words)))
    (when (and words (not (find (char (first words) 0) ";#")))
      (ppcre:register-groups-bind (host path) ("^https?://([^/]+)/(.*?)/?$" (first words))
        (let ((url (string-right-trim "/" (first words))))
          (list :url url
                :host host
                :key (format nil "~a/~a" host path)
                :name (if (rest words)
                          (format nil "~{~a~^ ~}" (rest words))
                          (subseq path (1+ (or (position #\/ path :from-end t) -1))))
                :feed (feed-url host url)
                :followed followed))))))

(defun watch-list ()
  "The projects in watch.txt, in the file's order."
  (let ((file (watch-file)))
    (when (probe-file file)
      (remove nil (mapcar #'parse-watch-line (uiop:read-file-lines file :external-format :utf-8))))))

(defun blog-list ()
  "The blogs in blogs.txt, in the file's order. A line is read as one of
watch.txt is, but its URL is the blog's feed as it stands, and a blog with no
name written has none: its posts are named by their own titles (see
ENTRY-POST)."
  (let ((file (blogs-file)))
    (when (probe-file file)
      (loop for line in (uiop:read-file-lines file :external-format :utf-8)
            for blog = (parse-watch-line line)
            when blog
              collect (list :feed (getf blog :url)
                            :host (getf blog :host)
                            :name (when (find-if (lambda (c) (find c '(#\Space #\Tab)))
                                                 (string-trim '(#\Space #\Tab #\Return) line))
                                    (getf blog :name)))))))

;;; What the site shows

(defvar *published* nil
  "The published updates as last read, after the file's write date.")

(defun published-updates ()
  "The published updates, newest first. Read again when the file changes."
  (let* ((file (updates-file))
         (stamp (and (probe-file file) (file-write-date file))))
    (unless (and *published* (eql (car *published*) stamp))
      (setf *published* (cons stamp (newest-first (read-forms file)))))
    (cdr *published*)))

(defun write-published (updates)
  (write-forms (updates-file) (newest-first updates)
               "Intel's published updates, one a line. Written by the desk at /intel/desk.")
  (setf *published* nil))

;;; The inbox: a line about the last fetch, then the updates.

(defun inbox ()
  "The inbox's updates, newest first, and a plist about the last fetch:
:fetched, a universal time, :added, and :errors, a list of text."
  (let ((forms (read-forms (inbox-file))))
    (values (newest-first (remove-if-not (lambda (form) (getf form :id)) forms))
            (find-if (lambda (form) (getf form :fetched)) forms))))

(defun write-inbox (updates about)
  (write-forms (inbox-file) (cons (or about (list :fetched nil)) (newest-first updates))
               "Intel's inbox: everything fetched, and what was done about it. Local; not committed."))

;;; Fetching. curl does it: one run for every feed, another for GitHub's search.

(defun curl (jobs &key headers)
  "Download each of JOBS, a list of (url . file), several at a time."
  (uiop:run-program
   (append (list "curl" "-sL" "--max-time" "25" "--parallel" "--parallel-max" "8"
                 "-A" "almightylisp-intel/1.0")
           (loop for header in headers append (list "-H" header))
           (loop for (url . file) in jobs
                 append (list "-o" (uiop:native-namestring file) url)))
   :ignore-error-status t))

(defun file-text (file)
  (if (probe-file file)
      (or (ignore-errors (uiop:read-file-string file :external-format :utf-8)) "")
      ""))

;;; Feeds are Atom or RSS, and are read with regular expressions: a title, a
;;; link, a date and some text from each entry is all that is wanted.

(defun unescape (text)
  "TEXT with its character references read."
  (flet ((named (text name character)
           (ppcre:regex-replace-all (format nil "&~a;" name) text (string character))))
    (let* ((text (ppcre:regex-replace-all
                  "&#(x?)([0-9a-fA-F]+);" text
                  (lambda (match hex digits)
                    (declare (ignore match))
                    (let ((code (ignore-errors
                                 (parse-integer digits :radix (if (string= hex "") 10 16)))))
                      (if (and code (< 0 code char-code-limit))
                          (string (code-char code))
                          "")))
                  :simple-calls t))
           (text (named text "lt" #\<))
           (text (named text "gt" #\>))
           (text (named text "quot" #\"))
           (text (named text "apos" #\'))
           (text (named text "nbsp" #\Space)))
      (named text "amp" #\&))))

(defun shortened (text limit)
  (if (> (length text) limit)
      (format nil "~a…" (string-right-trim " " (subseq text 0 (1- limit))))
      text))

(defun plain-text (html &optional (limit 700))
  "HTML as text, a line to a paragraph or list item, at most LIMIT characters."
  (let* ((text (ppcre:regex-replace-all "(?i)<li[^>]*>" html (format nil "~%- ")))
         (text (ppcre:regex-replace-all "(?i)<(br|/p|/h\\d|/li|/tr|/div|/ul|/ol)[^>]*>" text
                                        (string #\Newline)))
         (text (ppcre:regex-replace-all "<[^>]+>" text ""))
         (text (unescape text))
         (text (ppcre:regex-replace-all "[ \\t\\r]+" text " "))
         (text (ppcre:regex-replace-all " ?\\n[ \\n]*" text (string #\Newline))))
    (shortened (string-trim '(#\Space #\Newline) text) limit)))

(defun element-text (name xml)
  "What the first NAME element of XML holds, unescaped once."
  (ppcre:register-groups-bind (text)
      ((format nil "(?s)<~a(?:\\s[^>]*)?>(.*?)</~a>" name name) xml)
    (unescape (ppcre:regex-replace-all "<!\\[CDATA\\[|\\]\\]>" text ""))))

(defun feed-entries (xml)
  (or (ppcre:all-matches-as-strings "(?s)<entry[\\s>].*?</entry>" xml)
      (ppcre:all-matches-as-strings "(?s)<item[\\s>].*?</item>" xml)))

(defun entry-link (entry)
  (or (ppcre:register-groups-bind (href) ("<link[^>]*href=\"([^\"]+)\"" entry)
        (unescape href))
      (element-text "link" entry)))

(defparameter *months*
  '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

(defun entry-date (entry)
  "The entry's date as YYYY-MM-DD: an Atom date, or an RSS one."
  (let ((text (or (element-text "updated" entry) (element-text "published" entry)
                  (element-text "pubDate" entry) "")))
    (or (ppcre:scan-to-strings "^\\d{4}-\\d{2}-\\d{2}" (string-trim " " text))
        (ppcre:register-groups-bind (day month year) ("(\\d{1,2}) (\\w{3}) (\\d{4})" text)
          (let ((number (position month *months* :test #'string-equal)))
            (when number
              (format nil "~a-~2,'0d-~2,'0d" year (1+ number) (parse-integer day))))))))

(defun tag-version (tag)
  "The version in TAG: sbcl-2.6.9 and v2.6.9 are both 2.6.9. A tag with no
number in it is its own version."
  (or (ppcre:register-groups-bind (version) ("(?:^|[-_/])v?(\\d[\\w.+-]*)$" tag)
        version)
      tag))

(defun entry-update (entry project)
  "The release ENTRY describes, of PROJECT, a watch-list plist."
  (let* ((link (entry-link entry))
         (date (entry-date entry))
         (tag (when link
                (quri:url-decode (subseq link (1+ (or (position #\/ link :from-end t) -1)))))))
    (when (and link date (plusp (length tag)))
      (let ((notes (plain-text (or (element-text "content" entry)
                                   (element-text "summary" entry)
                                   (element-text "description" entry)
                                   "")))
            (title (plain-text (or (element-text "title" entry) "") 120)))
        (list :id (format nil "~a@~a" (getf project :key) tag)
              :kind :release
              :project (getf project :name)
              :host (getf project :host)
              :url link
              :version (tag-version tag)
              :date date
              :line ""
              :status :new
              :source (if (plusp (length notes)) notes title))))))

(defun entry-post (entry blog)
  "The post ENTRY describes, on BLOG, a blog-list plist. It is shown under
the blog's name. A blog without one gathers other people's posts, each
titled Author: Title, and the post is shown under its author."
  (let* ((link (entry-link entry))
         (link (when link (ppcre:regex-replace "[?&]utm_.*$" link "")))
         (date (entry-date entry))
         (title (plain-text (or (element-text "title" entry) "") 200))
         (colon (search ": " title))
         (host (when link
                 (ppcre:register-groups-bind (host) ("^https?://([^/]+)" link) host))))
    (when (and link date host)
      (multiple-value-bind (author title)
          (cond ((getf blog :name) (values (getf blog :name) title))
                (colon (values (subseq title 0 colon) (subseq title (+ colon 2))))
                (t (values (if (plusp (length title)) title host) "")))
        (list :id (format nil "post:~a" link)
              :kind :post
              :project author
              :host host
              :url link
              :version nil
              :date date
              :line (shortened title *line-limit*)
              :status :new
              :source (format nil "~@[~a~%~]~a" (when (plusp (length title)) title)
                              (plain-text (or (element-text "content:encoded" entry)
                                              (element-text "content" entry)
                                              (element-text "summary" entry)
                                              (element-text "description" entry)
                                              ""))))))))

(defun recent (updates days most)
  "Those of UPDATES, newest first, dated in the last DAYS, at most MOST."
  (let* ((since (days-ago days))
         (updates (remove-if (lambda (update)
                               (or (null update) (string< (getf update :date) since)))
                             updates)))
    (subseq updates 0 (min most (length updates)))))

(defun blog-posts (blog xml)
  "BLOG's recent posts in XML, its feed. The second value is true when XML is
a feed at all."
  (let ((feed (or (search "<feed" xml) (search "<rss" xml) (search "<rdf:RDF" xml))))
    (values (when feed
              (recent (mapcar (lambda (entry) (entry-post entry blog)) (feed-entries xml))
                      *post-days* *posts-per-blog*))
            (and feed t))))

(defun project-releases (project xml)
  "PROJECT's recent releases in XML, its feed. The second value is true when
XML is a feed at all."
  (let ((since (days-ago *release-days*))
        (feed (or (search "<feed" xml) (search "<rss" xml))))
    (values (when feed
              (let ((updates (remove-if (lambda (update)
                                          (or (null update) (string< (getf update :date) since)))
                                        (mapcar (lambda (entry) (entry-update entry project))
                                                (feed-entries xml)))))
                (subseq updates 0 (min *releases-per-project* (length updates)))))
            (and feed t))))

;;; New repositories, from GitHub's search. GITHUB_TOKEN, when it is set,
;;; raises the number of searches GitHub allows; none is needed.

(defun new-repositories-url ()
  (format nil "https://api.github.com/search/repositories?q=~a&sort=stars&order=desc&per_page=~d"
          (quri:url-encode (format nil "language:\"Common Lisp\" created:>=~a fork:false"
                                   (days-ago *new-repository-days*)))
          *new-repositories*))

(defun repository-update (repository)
  (flet ((field (name)
           (let ((value (gethash name repository)))
             (unless (eq value 'null) value))))
    (let ((description (plain-text (or (field "description") "") 400)))
      (list :id (format nil "github.com/~a" (field "full_name"))
            :kind :new
            :project (field "name")
            :host "github.com"
            :url (field "html_url")
            :version nil
            :date (subseq (field "created_at") 0 10)
            :line (shortened description *line-limit*)
            :status :new
            :source (format nil "~a~%~a" (field "full_name") description)
            :stars (field "stargazers_count")))))

(defun new-repositories (json)
  "The repositories in JSON, GitHub's answer. The second value is true when
JSON is such an answer."
  (let* ((answer (ignore-errors (jzon:parse json)))
         (items (and (hash-table-p answer) (gethash "items" answer))))
    (values (when (vectorp items)
              (remove nil (map 'list (lambda (repository)
                                       (ignore-errors (repository-update repository)))
                               items)))
            (vectorp items))))

(defun download-everything (sources directory)
  "Fetch the feed of each of SOURCES, plists with a :feed, and GitHub's two
searches into DIRECTORY. Returns the files: a list in SOURCES' order, the
search for new repositories', and the search for active ones'."
  (let ((feeds (loop for index from 0 below (length sources)
                     collect (merge-pathnames (format nil "feed-~d.xml" index) directory)))
        (search (merge-pathnames "new.json" directory))
        (active (merge-pathnames "active.json" directory))
        (token (shiso:setting :magazine/github-token)))
    (when sources
      (curl (loop for source in sources
                  for file in feeds
                  collect (cons (getf source :feed) file))))
    (curl (list (cons (new-repositories-url) search)
                (cons (active-repositories-url) active))
          :headers (list* "Accept: application/vnd.github+json"
                          (when (plusp (length token))
                            (list (format nil "Authorization: Bearer ~a" token)))))
    (values feeds search active)))

(defun fetch-updates ()
  "Fetch the watch list's releases, the blogs' posts and the new repositories,
and add those the inbox hasn't seen as new; and the followed projects'
newest commits (see RECORD-COMMITS). Returns how many updates were added and
a list of what went wrong, as text."
  (let* ((projects (watch-list))
         (blogs (blog-list))
         (followed (commit-sources projects))
         (commits nil)
         (directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "intel-~d/" (get-universal-time))
                                      (uiop:temporary-directory))))
         (found nil)
         (errors nil))
    (ensure-directories-exist directory)
    (unwind-protect
         (multiple-value-bind (feeds search active)
             (download-everything (append projects blogs followed) directory)
           (loop for source in (append projects blogs followed)
                 for file in feeds
                 do (multiple-value-bind (found-here feed)
                        (cond ((member source blogs) (blog-posts source (file-text file)))
                              ((member source followed) (project-commits (file-text file)))
                              (t (project-releases source (file-text file))))
                      (cond ((not feed)
                             (push (format nil "~a: no feed at ~a"
                                           (or (getf source :name) (getf source :host))
                                           (getf source :feed))
                                   errors))
                            ((member source followed)
                             (push (cons (getf source :key) found-here) commits))
                            (t (setf found (append found found-here))))))
           (multiple-value-bind (updates answered) (new-repositories (file-text search))
             (if answered
                 (setf found (append found updates))
                 (push "GitHub's search for new repositories gave no answer." errors)))
           (multiple-value-bind (repositories answered) (active-repositories (file-text active))
             (if answered
                 (record-active repositories)
                 (push "GitHub's search for active repositories gave no answer." errors))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))
    (let* ((known (inbox))
           (seen (append (mapcar (lambda (update) (getf update :id)) known)
                         (mapcar (lambda (update) (getf update :id)) (published-updates))))
           (added (remove-if (lambda (update) (member (getf update :id) seen :test #'string=))
                             (remove-duplicates found :key (lambda (update) (getf update :id))
                                                      :test #'string= :from-end t)))
           (errors (nreverse errors)))
      (record-commits commits)
      (write-inbox (append added known)
                   (list :fetched (get-universal-time) :added (length added) :errors errors))
      (values (length added) errors))))

;;; Following. A followed project's commits come from its host's feed of
;;; them; GitHub is the one host whose feed of commits is known here.

(defun commit-feed-url (project)
  (when (string= (getf project :host) "github.com")
    (format nil "~a/commits.atom" (getf project :url))))

(defun commit-sources (projects)
  "The followed of PROJECTS that have a feed of commits, each with that feed
as its :feed."
  (loop for project in projects
        for feed = (and (getf project :followed) (commit-feed-url project))
        when feed
          ;; The first :feed of a plist is the one that is read.
          collect (list* :feed feed project)))

(defun entry-commit (entry)
  "The commit ENTRY describes: its hash as :id, the first line of its
message, its date, its author and its page."
  (let ((link (entry-link entry))
        (date (entry-date entry)))
    (when (and link date)
      (list :id (subseq link (1+ (or (position #\/ link :from-end t) -1)))
            :title (plain-text (or (element-text "title" entry) "") 140)
            :date date
            :author (plain-text (or (element-text "name" entry) "") 60)
            :url link))))

(defun project-commits (xml)
  "The newest commits in XML, a project's feed of them, newest first. The
second value is true when XML is a feed at all."
  (let ((feed (search "<feed" xml)))
    (values (when feed
              (let ((commits (remove nil (mapcar #'entry-commit (feed-entries xml)))))
                (subseq commits 0 (min *commits-kept* (length commits)))))
            (and feed t))))

(defun following ()
  "What is kept about followed projects: a plist each, with the project's
:key, its newest :commits and :seen, the hash of the last one looked at."
  (read-forms (following-file)))

(defun write-following (entries)
  (write-forms (following-file) entries
               "Intel's followed projects: their newest commits, and the last one seen. Local; not committed."))

(defun record-commits (fetched)
  "Keep FETCHED, a list of (key . commits) from a fetch. A project fetched
for the first time has nothing new: its newest commit is taken as seen."
  (when fetched
    (let ((entries (following)))
      (loop for (key . commits) in fetched
            for entry = (find key entries :key (lambda (entry) (getf entry :key))
                                          :test #'string-equal)
            when commits
              do (setf entries
                       (cons (list :key key
                                   :seen (or (getf entry :seen) (getf (first commits) :id))
                                   :commits commits)
                             (remove entry entries))))
      (write-following entries)))
  (write-commit-summary))

(defun write-commit-summary ()
  "Write commits.sexp, what the site shows of the followed projects' commits:
a plist for each project the last week brought any, the newest first. Its
:count is how many of the commits kept are the week's, and :more is true
when every one kept is, so that there may be more; :date and :line are the
newest one's."
  (let ((since (days-ago 7)))
    (write-forms
     (commits-file)
     (stable-sort
      (loop for project in (followed-projects)
            for commits = (getf project :commits)
            for week = (remove-if (lambda (commit) (string< (getf commit :date) since)) commits)
            when week
              collect (list :key (getf project :key)
                            :project (getf project :name)
                            :host (getf project :host)
                            :url (format nil "~a/commits" (getf project :url))
                            :count (length week)
                            :more (= (length week) (length commits) *commits-kept*)
                            :date (getf (first week) :date)
                            :line (getf (first week) :title)))
      #'string> :key (lambda (summary) (getf summary :date)))
     "Intel's followed projects' commits this week, one project a line. Written by a fetch.")))

(defun published-commits ()
  "The followed projects' commits this week, as WRITE-COMMIT-SUMMARY wrote
them."
  (read-forms (commits-file)))

(defun followed-projects ()
  "The followed projects of the watch list, in its order, each with what is
known of its commits: :commits, the newest kept; :fresh, those since the
last one seen (all of them, when that one is no longer among those kept);
:checked, true once a fetch has asked; :tracked, true when the project's
host has a feed of commits at all."
  (let ((entries (following)))
    (loop for project in (watch-list)
          when (getf project :followed)
            collect (let* ((entry (find (getf project :key) entries
                                        :key (lambda (entry) (getf entry :key))
                                        :test #'string-equal))
                           (commits (getf entry :commits))
                           (seen (position (getf entry :seen) commits
                                           :key (lambda (commit) (getf commit :id))
                                           :test #'equal)))
                      (append project
                              (list :commits commits
                                    :fresh (if seen (subseq commits 0 seen) commits)
                                    :checked (and entry t)
                                    :tracked (and (commit-feed-url project) t)))))))

(defun update-key (id)
  "The project an update named ID is of: what stands before the @ of a
release's id, and all of a new repository's. A project's own key is itself."
  (subseq id 0 (position #\@ id)))

(defun set-followed (key followed)
  "Mark the project KEY names in watch.txt as FOLLOWED, or not: its line
gains or loses the * it starts with, and the rest of the file is left as it
is. A project to follow that the file doesn't have is added at its end."
  (let* ((file (watch-file))
         (lines (when (probe-file file)
                  (uiop:read-file-lines file :external-format :utf-8)))
         (found nil)
         (lines (loop for line in lines
                      for project = (parse-watch-line line)
                      collect (if (and project (string-equal (getf project :key) key))
                                  (let ((bare (string-left-trim '(#\Space #\Tab #\*) line)))
                                    (setf found t)
                                    (if followed (format nil "* ~a" bare) bare))
                                  line))))
    (when (or found followed)
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
        (format out "~{~a~%~}" lines)
        (when (and followed (not found))
          (format out "* https://~a~%" key))))
    t))

(defun follow (id)
  "Follow the project of the update named ID, or the project ID names."
  (let ((key (update-key id)))
    (unless (ppcre:scan "^post:" key)
      (set-followed key t))))

(defun unfollow (id)
  "Stop following the project of the update named ID, or the project ID
names. It stays on the watch list, and leaves what the site shows of
commits."
  (set-followed (update-key id) nil)
  (write-commit-summary))

(defun mark-seen (key)
  "Take the newest commit kept for the project KEY names as seen."
  (let* ((entries (following))
         (entry (find key entries :key (lambda (entry) (getf entry :key)) :test #'string-equal)))
    (when (getf entry :commits)
      (write-following
       (substitute (list :key (getf entry :key)
                         :seen (getf (first (getf entry :commits)) :id)
                         :commits (getf entry :commits))
                   entry entries))
      t)))

;;; Active elsewhere: repositories pushed to lately that the watch list
;;; doesn't have.

(defun active-repositories-url ()
  (format nil "https://api.github.com/search/repositories?q=~a&sort=updated&order=desc&per_page=~d"
          (quri:url-encode (format nil "language:\"Common Lisp\" pushed:>=~a stars:>=~d fork:false"
                                   (days-ago *active-days*) *active-stars*))
          *active-repositories*))

(defun active-repository (repository)
  (flet ((field (name)
           (let ((value (gethash name repository)))
             (unless (eq value 'null) value))))
    (list :key (format nil "github.com/~a" (field "full_name"))
          :name (field "full_name")
          :host "github.com"
          :url (field "html_url")
          :description (plain-text (or (field "description") "") 300)
          :pushed (subseq (field "pushed_at") 0 10)
          :stars (field "stargazers_count"))))

(defun active-repositories (json)
  "The repositories in JSON, GitHub's answer. The second value is true when
JSON is such an answer."
  (let* ((answer (ignore-errors (jzon:parse json)))
         (items (and (hash-table-p answer) (gethash "items" answer))))
    (values (when (vectorp items)
              (remove nil (map 'list (lambda (repository)
                                       (ignore-errors (active-repository repository)))
                               items)))
            (vectorp items))))

(defun read-active ()
  "The repositories last found active, and the keys of those hidden."
  (let ((forms (read-forms (active-file))))
    (values (remove-if-not (lambda (form) (getf form :key)) forms)
            (getf (find-if (lambda (form) (member :hidden form)) forms) :hidden))))

(defun write-active (repositories hidden)
  (write-forms (active-file) (cons (list :hidden hidden) repositories)
               "Intel's active repositories: those pushed to lately, and those hidden. Local; not committed."))

(defun record-active (repositories)
  "Keep REPOSITORIES, a fetch's, in place of the last ones. What is hidden
stays hidden."
  (write-active repositories (nth-value 1 (read-active))))

(defun active-projects ()
  "The repositories last found active that are neither on the watch list nor
hidden, the last pushed to first."
  (multiple-value-bind (repositories hidden) (read-active)
    (let ((known (append hidden (mapcar (lambda (project) (getf project :key)) (watch-list)))))
      (stable-sort (remove-if (lambda (repository)
                                (member (getf repository :key) known :test #'string-equal))
                              repositories)
                   #'string> :key (lambda (repository) (getf repository :pushed))))))

(defun hide-active (key)
  "Stop offering the repository KEY names, now and after later fetches."
  (multiple-value-bind (repositories hidden) (read-active)
    (write-active repositories (adjoin key hidden :test #'string-equal))
    t))

;;; What the desk does with an update

(defun find-update (id updates)
  (find id updates :key (lambda (update) (getf update :id)) :test #'string=))

(defun published-form (update)
  "UPDATE as updates.sexp keeps it: what the site prints, and nothing else."
  (loop for key in '(:id :kind :project :host :url :version :date :line)
        append (list key (getf update key))))

(defun clean-line (line)
  (shortened (string-trim '(#\Space #\Tab #\Newline #\Return)
                          (ppcre:regex-replace-all "\\s+" (or line "") " "))
             *line-limit*))

(defun change-inbox (id change)
  "Write the inbox with the update named ID replaced by what CHANGE returns
for it. Returns the new update, or NIL when the inbox has none by that name."
  (multiple-value-bind (updates about) (inbox)
    (let ((update (find-update id updates)))
      (when update
        (let ((new (funcall change (copy-list update))))
          (write-inbox (substitute new update updates) about)
          new)))))

(defun publish-update (id line)
  "Publish the update named ID with LINE, or change the line of one that is
published. Returns the update, or NIL when there is none or LINE is empty."
  (let ((line (clean-line line))
        (published (published-updates)))
    (when (plusp (length line))
      (let ((update (or (change-inbox id (lambda (update)
                                           (setf (getf update :line) line
                                                 (getf update :status) :published)
                                           update))
                        ;; Published from another machine: not in this inbox.
                        (let ((update (copy-list (find-update id published))))
                          (when update
                            (setf (getf update :line) line)
                            update)))))
        (when update
          (write-published (cons (published-form update)
                                 (remove id published :key (lambda (other) (getf other :id))
                                                      :test #'string=)))
          update)))))

(defun unpublish-update (id)
  "Take the update named ID off the site and put it back in the queue, its
line kept."
  (let* ((published (published-updates))
         (update (find-update id published)))
    (when update
      (write-published (remove update published))
      (or (change-inbox id (lambda (update)
                             (setf (getf update :status) :new)
                             update))
          (multiple-value-bind (updates about) (inbox)
            (write-inbox (cons (append update (list :status :new)) updates) about)))
      t)))

(defun set-status (id status)
  (change-inbox id (lambda (update)
                     (setf (getf update :status) status)
                     update)))

(defun dismiss-update (id)
  "Set the update named ID aside. A later fetch doesn't bring it back."
  (set-status id :dismissed))

(defun restore-update (id)
  "Put a dismissed update back in the queue."
  (set-status id :new))
