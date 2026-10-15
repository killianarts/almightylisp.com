(defpackage #:magazine/irc
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export
   ;; Where it listens
   #:*server* #:*port* #:*tls* #:*nick* #:*fallback-nick* #:*channels* #:*kept*
   ;; Listening
   #:start #:stop #:running-p #:enabled-p #:ensure-started
   ;; What it has heard
   #:status #:recent-lines))

(in-package #:magazine/irc)

;;;; A listener on IRC: what is said in a channel, as it is said.
;;;
;;; A thread connects to the server, joins *CHANNELS* and keeps the last
;;; *KEPT* lines said there, in memory. Nothing is written to disk, and
;;; nothing is ever said: the listener answers the server's pings and sends
;;; nothing else to anyone.
;;;
;;; It listens only when it is started: by START (the desk's Connect button
;;; calls it), or by ENSURE-STARTED in an image started with INTEL_IRC=true,
;;; which the homepage calls. While it runs, the homepage shows the newest
;;; lines; so the public site shows them only where that variable is set.
;;;
;;; The connection is kept: when it drops, or the server is quiet for longer
;;; than it ever is, the listener waits a while and connects again.

(defparameter *server* "irc.libera.chat")
(defparameter *port* 6697)
(defparameter *tls* t
  "True to talk to the server over TLS, as port 6697 expects.")

(defparameter *nick* "almighty-lisp-bot"
  "The name the listener asks for.")

(defparameter *fallback-nick* "almightylisp-bot"
  "The name it asks for when the server won't have *NICK*: a server may
allow fewer characters than that has. After this, a number is added.")

(defparameter *channels* '("#commonlisp")
  "The channels listened to.")

(defparameter *kept* 300
  "The newest lines kept.")

(defparameter *quiet-seconds* 600
  "A server that sends nothing for this long is taken to be gone.")

(defvar *lock* (bt:make-lock "irc"))
(defvar *thread* nil)
(defvar *stopping* nil)
(defvar *socket* nil)
(defvar *lines* nil "The lines kept, newest first.")
(defvar *counter* 0)
(defvar *status* (list :state :off)
  "How the listener is: :state is :off, :connecting, :listening or :waiting;
:nick the name it has; :note what last went wrong, as text.")

(defun enabled-p ()
  "True in an image that is to listen without being asked: one started with
INTEL_IRC=true."
  (string-equal (uiop:getenv "INTEL_IRC") "true"))

(defun running-p ()
  (and *thread* (bt:thread-alive-p *thread*)))

(defun status ()
  (bt:with-lock-held (*lock*) (copy-list *status*)))

(defun set-status (&rest changes)
  (bt:with-lock-held (*lock*)
    (loop for (key value) on changes by #'cddr
          do (setf (getf *status* key) value))))

(defun recent-lines (&optional (count *kept*))
  "The newest COUNT lines, oldest first. A line is a plist: :id, a number
that grows; :time, a universal time; :channel; :nick; :text; and :action,
true of a line said with /me."
  (bt:with-lock-held (*lock*)
    (reverse (subseq *lines* 0 (min count (length *lines*))))))

(defun keep-line (channel nick text action)
  (bt:with-lock-held (*lock*)
    (push (list :id (incf *counter*) :time (get-universal-time)
                :channel channel :nick nick :text text :action action)
          *lines*)
    (when (> (length *lines*) *kept*)
      (setf *lines* (subseq *lines* 0 *kept*)))))

;;; The wire: lines of UTF-8 ended by CR LF. What isn't UTF-8 is read
;;; anyway, its bad bytes replaced.

(defun send-line (stream format &rest arguments)
  (write-sequence (babel:string-to-octets
                   (format nil "~?~c~c" format arguments #\Return #\Newline)
                   :encoding :utf-8)
                  stream)
  (finish-output stream))

(defun receive-line (stream)
  "The next line from STREAM, or NIL at its end."
  (let ((bytes (make-array 512 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte) (return nil))
                   ((= byte 10)
                    (return (string-right-trim
                             '(#\Return)
                             (babel:octets-to-string (coerce bytes '(simple-array (unsigned-byte 8) (*)))
                                                     :encoding :utf-8 :errorp nil))))
                   (t (vector-push-extend byte bytes))))))

(defun parse-message (line)
  "LINE as the name of who sent it (NIL for the server itself), the command
and its parameters."
  (let* ((prefix (when (and (plusp (length line)) (char= (char line 0) #\:))
                   (subseq line 1 (or (position #\Space line) (length line)))))
         (rest (if prefix
                   (string-left-trim " " (subseq line (min (length line) (+ 2 (length prefix)))))
                   line))
         (trailing (search " :" rest))
         (words (remove "" (uiop:split-string (subseq rest 0 trailing) :separator " ")
                        :test #'string=)))
    (values (when prefix (subseq prefix 0 (or (position #\! prefix) (length prefix))))
            (first words)
            (append (rest words) (when trailing (list (subseq rest (+ trailing 2))))))))

(defun plain (text)
  "TEXT without the codes that colour and embolden it."
  (remove-if (lambda (character) (< (char-code character) 32))
             (ppcre:regex-replace-all (format nil "~c\\d{0,2}(?:,\\d{1,2})?" (code-char 3)) text "")))

(defun hear (nick channel text)
  "Keep what NICK said in CHANNEL, when it is one listened to."
  (when (member channel *channels* :test #'string-equal)
    (let ((ctcp (and (plusp (length text)) (char= (char text 0) (code-char 1)))))
      (cond ((not ctcp) (keep-line channel nick (plain text) nil))
            ;; /me does something; every other such message is to a program.
            ((eql 1 (search "ACTION " text))
             (keep-line channel nick (plain (subseq text 8)) t))))))

(defun next-nick (nick)
  "The name to ask for when NICK was refused."
  (cond ((string= nick *nick*) *fallback-nick*)
        (t (format nil "~a~d" (subseq *fallback-nick* 0 (min 14 (length *fallback-nick*)))
                   (1+ (random 9))))))

(defun open-connection ()
  (let* ((socket (usocket:socket-connect *server* *port* :element-type '(unsigned-byte 8)
                                                         :timeout *quiet-seconds*))
         (stream (usocket:socket-stream socket)))
    (values socket
            (if *tls*
                (cl+ssl:make-ssl-client-stream stream :hostname *server*)
                stream))))

(defun listen-once ()
  "Connect, join the channels and keep what is said until the connection
ends. Returns when it does; signals an error when it breaks."
  (multiple-value-bind (socket stream) (open-connection)
    (setf *socket* socket)
    (unwind-protect
         (let ((nick *nick*))
           (send-line stream "NICK ~a" nick)
           (send-line stream "USER ~a 0 * :Almighty Lisp listener (reads, never speaks)" "almighty")
           (loop for line = (receive-line stream)
                 while (and line (not *stopping*))
                 do (multiple-value-bind (who command parameters) (parse-message line)
                      (cond ((null command))
                            ((string= command "PING")
                             (send-line stream "PONG :~a" (or (first parameters) "")))
                            ;; Welcome: the name was taken.
                            ((string= command "001")
                             (set-status :nick nick)
                             (dolist (channel *channels*)
                               (send-line stream "JOIN ~a" channel)))
                            ;; The end of a channel's list of names: it is joined.
                            ((string= command "366")
                             (set-status :state :listening :note nil))
                            ;; The name is not allowed, or is someone's.
                            ((member command '("432" "433") :test #'string=)
                             (setf nick (next-nick nick))
                             (send-line stream "NICK ~a" nick))
                            ;; The channel won't have the listener.
                            ((member command '("471" "473" "474" "475" "477") :test #'string=)
                             (set-status :note (format nil "~a: ~a" (second parameters)
                                                       (car (last parameters)))))
                            ((string= command "PRIVMSG")
                             (hear who (first parameters) (or (second parameters) "")))
                            ((string= command "ERROR")
                             (error "The server closed the connection: ~a" (first parameters)))))))
      (setf *socket* nil)
      (ignore-errors (send-line stream "QUIT"))
      (ignore-errors (close stream))
      (ignore-errors (usocket:socket-close socket)))))

(defun listen-forever ()
  "Listen, and when the connection ends connect again: after a quarter of a
minute at first, then longer each time, up to five minutes."
  (let ((wait 15))
    (loop until *stopping*
          do (set-status :state :connecting)
             (handler-case
                 (progn (listen-once)
                        (unless *stopping* (set-status :note "The connection ended.")))
               (error (condition)
                 (set-status :note (princ-to-string condition))))
             (unless *stopping*
               ;; A connection that held is not held against the next.
               (setf wait (if (eq (getf (status) :state) :listening) 15 (min 300 (* 2 wait))))
               (set-status :state :waiting)
               (loop repeat wait until *stopping* do (sleep 1))))
    (set-status :state :off)))

(defun start ()
  "Start listening, unless the listener already is."
  (unless (running-p)
    (setf *stopping* nil)
    (set-status :state :connecting :note nil)
    (setf *thread* (bt:make-thread #'listen-forever :name "intel irc listener")))
  t)

(defun stop ()
  "Stop listening and leave. The lines kept stay."
  (setf *stopping* t)
  ;; The thread is waiting on the socket: closing it lets go.
  (let ((socket *socket*))
    (when socket (ignore-errors (usocket:socket-close socket))))
  (set-status :state :off)
  t)

(defun ensure-started ()
  "Start listening in an image that is to (see ENABLED-P)."
  (when (and (enabled-p) (not (running-p)))
    (start)))
