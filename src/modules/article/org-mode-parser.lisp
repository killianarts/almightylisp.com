(defpackage #:article/org-mode-parser
  (:use #:cl)
  (:local-nicknames (#:org #:cl-org-mode)
                    (#:ah #:almighty-html))
  (:export #:org-string-to-html))

(in-package #:article/org-mode-parser)

;; Might have trouble with executable building.
(defparameter *temp-file-path* (asdf:system-relative-pathname :article "temp/"))

(defun parse-org-string (org-string)
  (uiop:with-temporary-file (:pathname file ; The symbol referring to the file.
                             :stream stream
                             :directory *temp-file-path*
                             :direction :output
                             :element-type 'character
                             :keep nil)
    (write-string org-string stream)
    (finish-output stream)
    (org::read-org-file file)))
#+nil
(org-string-to-html "* Heading
This text talks about =<html></html>=
** This is a level 2 header

Inside is some =<p>= tag stuff.")

#+nil
(org::read-org-file "../../../README.org")


(defgeneric render-org-node (node)
  (:documentation "Return HTML string for a single node (recursive).")
  (:method ((node null)) ""))

(defun org-string-to-html (org-string)
  "Convert Org-mode string to full HTML document."
  (let ((root (parse-org-string org-string)))
    (ah:render-to-string
     (ah:</>
      (raw! (render-org-node root))))))

;; Helper to render all children recursively
(defun render-children (node)
  (ah:</>
   (mapcar #'render-org-node (cl-org-mode::node.children node))))

;; === Node renderers ===
(defmethod render-org-node ((node cl-org-mode::org-file))
  (render-children node))

(defmethod render-org-node ((node cl-org-mode::outline-node))
  (let* ((level-indicator (cl-org-mode::node.heading-level-indicator node))
         (level (if level-indicator (1- (length level-indicator)) 1))
         (title (cl-org-mode::node.heading node))
         (tag (format nil "h~d" level)))
    (ah:render-to-string
     (ah:</>
      (<>
        ;; NOTE We create the h tag programmatically using create-element. We
        ;; require raw! or else the strings produced by text-nodes are
        ;; escaped--including the HTML tags we create. Instead, we rely on the
        ;; text nodes to escape their text--including the HTML tags we create.
        ;; Instead, we rely on the text nodes to escape their text
        
        ;; TODO almighty-html isn't designed for building up the HTML string
        ;; like this. Consider ways of improving this workflow.
        (ah::create-element (intern (string-upcase tag) :keyword) nil title)
        (raw! (render-children node)))))))

(defmethod render-org-node ((node cl-org-mode::text-node))
  (let ((text (string-right-trim '(#\Newline #\Space #\Tab)
                                 (cl-org-mode::node.text node))))
    (if (string= text "")
        ""
        (ah:render-to-string
         (ah:</>
          (p text))))))   ; simple escaping; add regex post-processing for links/bold if needed

(defmethod render-org-node ((node cl-org-mode::src-node))
  (let ((lang (or (cl-org-mode::node.emacs-mode node) "text"))
        (code (cl-org-mode::node.text node)))
    (ah:</>
     (ah::create-element :pre nil (ah::create-element :code `(:class ,lang) code)))))

;; Ignore closing delimiters etc.
(defmethod render-org-node ((node cl-org-mode::closing-delimiter-node)) "")

(defmethod render-org-node ((node cl-org-mode::properties-node))
  "")
(defmethod render-org-node ((node cl-org-mode::property-node))
  "")
