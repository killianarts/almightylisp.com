#!/usr/bin/env -S sbcl --script
;;;; Turn an article's org file into rich text for X's article editor.
;;;
;;;   dev/x-article.lisp content/magazine/some-article.org
;;;   dev/x-article.lisp some-article.org --html > out.html
;;;
;;; By default the body goes on the macOS clipboard as HTML, ready to paste
;;; into the editor's body; the title is printed, to paste into its own field.
;;; --html prints the HTML instead.
;;;
;;; X articles have headings, subheadings, bold, italic, strikethrough, links,
;;; lists, quotes and code blocks, and no underline, tables or inline code. So
;;; * and ** headings become headings and subheadings, _underline_ becomes
;;; bold, =code= becomes plain text, note blocks become quotes, and table rows
;;; become list items. Links to the site's own pages get the site's address.

(require :asdf)

(handler-bind ((warning #'muffle-warning))
  (load (merge-pathnames "../lib/almighty-press/src/org.lisp" *load-truename*)))

(defpackage #:x-article
  (:use #:cl #:almighty-press/org))

(in-package #:x-article)

(defparameter *site* "https://almightylisp.com")

(defun escape (text)
  (with-output-to-string (out)
    (loop for ch across text
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char ch out))))))

(defun absolute (target)
  "TARGET as a link X can follow, or NIL for one it can't, like #section."
  (cond ((char= (char target 0) #\/) (concatenate 'string *site* target))
        ((or (search "://" target) (string= "mailto:" target :end2 (min 7 (length target))))
         target)))

(defun inlines (text out)
  (labels ((walk (items)
             (dolist (item items)
               (if (stringp item)
                   (write-string (escape item) out)
                   (destructuring-bind (kind &rest rest) item
                     (flet ((wrap (tag)
                              (format out "<~a>" tag)
                              (walk (first rest))
                              (format out "</~a>" tag)))
                       (ecase kind
                         (:code (write-string (escape (first rest)) out))
                         ((:bold :underline) (wrap "strong"))
                         (:italic (wrap "em"))
                         (:strike (wrap "s"))
                         (:link
                          (let ((href (absolute (first rest))))
                            (if href
                                (progn (format out "<a href=\"~a\">" (escape href))
                                       (walk (second rest))
                                       (write-string "</a>" out))
                                (walk (second rest))))))))))))
    (walk (parse-inlines text))))

(defun paragraph (text out &optional (tag "p"))
  (format out "<~a>" tag)
  (inlines text out)
  (format out "</~a>~%" tag))

(defun pre (text out)
  (format out "<pre><code>~a</code></pre>~%" (escape text)))

(defun unescape-src-line (line)
  "LINE without the comma org puts before a * or #+ that starts a source line."
  (let ((i (position-if-not (lambda (ch) (member ch '(#\Space #\Tab))) line)))
    (if (and i
             (char= (char line i) #\,)
             (< (1+ i) (length line))
             (member (char line (1+ i)) '(#\* #\#)))
        (concatenate 'string (subseq line 0 i) (subseq line (1+ i)))
        line)))

(defun render-list (kind items out)
  (let ((tag (if (eq kind :ol) "ol" "ul")))
    (format out "<~a>~%" tag)
    (loop for (paragraphs sublists) in items
          do (write-string "<li>" out)
             (loop for (text . more) on paragraphs
                   do (inlines text out)
                      (when more (write-string "<br>" out)))
             (loop for (nil sub-kind sub-items) in sublists
                   do (render-list sub-kind sub-items out))
             (format out "</li>~%"))
    (format out "</~a>~%" tag)))

(defun render-block (block out)
  (ecase (first block)
    (:p (paragraph (second block) out))
    (:h (paragraph (second block) out "h3"))
    (:src
     (destructuring-bind (&key lines results &allow-other-keys) (rest block)
       (pre (format nil "~{~a~^~%~}" (mapcar #'unescape-src-line lines)) out)
       (when results
         (pre results out))))
    (:block
     (destructuring-bind (name label paragraphs) (rest block)
       (if (string= name "EXAMPLE")
           (pre (format nil "~{~a~^~%~}" paragraphs) out)
           (progn
             (write-string "<blockquote>" out)
             (unless (string= name "QUOTE")
               (format out "<p><strong>~a</strong></p>"
                       (escape (if (plusp (length label))
                                   label
                                   (string-capitalize name)))))
             (dolist (text paragraphs)
               (paragraph text out))
             (format out "</blockquote>~%")))))
    (:table
     (destructuring-bind (header rows) (rest block)
       (flet ((row (cells)
                (list (list (format nil "~{~a~^ · ~}" cells)) nil)))
         (when header
           (format out "<p><strong>~a</strong></p>~%"
                   (escape (format nil "~{~a~^ · ~}" header))))
         (render-list :ul (mapcar #'row rows) out))))
    (:list (render-list (second block) (third block) out))))

(defun article-html (document)
  (with-output-to-string (out)
    (let ((subtitle (document-keyword document "SUBTITLE"))
          (recap (document-keyword document "RECAP"))
          (intro (string-trim " " (join-lines (document-intro document)))))
      (when subtitle
        (format out "<p><em>~a</em></p>~%" (with-output-to-string (s) (inlines subtitle s))))
      (when recap
        (write-string "<blockquote>" out)
        (paragraph recap out)
        (format out "</blockquote>~%"))
      (when (plusp (length intro))
        (paragraph intro out)))
    (dolist (section (document-sections document))
      (paragraph (section-title section) out "h2")
      (dolist (block (section-blocks section))
        (render-block block out)))))

(defparameter *copy-script*
  "ObjC.import('AppKit');
function run(argv) {
  var html = $.NSString.stringWithContentsOfFileEncodingError(argv[0], $.NSUTF8StringEncoding, null);
  var data = html.dataUsingEncoding($.NSUTF8StringEncoding);
  var options = $({NSDocumentType: 'NSHTML', NSCharacterEncoding: $.NSUTF8StringEncoding});
  var text = $.NSAttributedString.alloc.initWithHTMLOptionsDocumentAttributes(data, options, null);
  var board = $.NSPasteboard.generalPasteboard;
  board.clearContents;
  board.setStringForType(html, $.NSPasteboardTypeHTML);
  board.setStringForType(text.string, $.NSPasteboardTypeString);
}"
  "JXA that puts the HTML file named by its argument on the clipboard, with a
plain-text version for apps that don't take HTML.")

(defun copy-html (html)
  (uiop:with-temporary-file (:stream stream :pathname path :type "html")
    (format stream "<meta charset=\"utf-8\">~%~a" html)
    (finish-output stream)
    (with-input-from-string (script *copy-script*)
      (uiop:run-program (list "osascript" "-l" "JavaScript" "-" (namestring path))
                        :input script :error-output t))))

(defun main (arguments)
  (let ((file (find-if-not (lambda (arg) (string= "--" arg :end2 (min 2 (length arg))))
                           arguments))
        (print-html (member "--html" arguments :test #'string=)))
    (unless file
      (format *error-output* "Usage: dev/x-article.lisp ARTICLE.org [--html]~%")
      (uiop:quit 2))
    (let* ((document (handler-case (read-org-file file)
                       (error (e)
                         (format *error-output* "~a: ~a~%" file e)
                         (uiop:quit 1))))
           (html (article-html document)))
      (cond (print-html (write-string html))
            (t (copy-html html)
               (format t "Copied the body of ~a.~%Title: ~a~%"
                       file (document-keyword document "TITLE" "(none)")))))))

(main (rest sb-ext:*posix-argv*))
