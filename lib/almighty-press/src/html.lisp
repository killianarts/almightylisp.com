(defpackage #:almighty-press/html
  (:use #:cl)
  (:local-nicknames (#:ah #:almighty-html)
                    (#:org #:almighty-press/org))
  (:export #:render-inlines #:render-lines))

(in-package #:almighty-press/html)

(defun render-inlines (text &key (href #'identity) (code-class "verb") (link-class "inline-link"))
  "TEXT with org's =code= and [[target][label]] as almighty-html elements.
HREF turns a link target into the href, e.g. a bare slug into a full path."
  (labels ((render (inline)
             (if (stringp inline)
                 inline
                 (ecase (first inline)
                   (:code (ah:</> (code :class code-class (second inline))))
                   (:link (ah:</> (a :class link-class :href (funcall href (second inline))
                                    (mapcar #'render (third inline)))))))))
    (mapcar #'render (org:parse-inlines text))))

(defun render-lines (lines)
  "LINES with a <br> between each, for titles with forced breaks."
  (loop for (line . more) on lines
        collect line
        when more collect (ah:</> (br))))
