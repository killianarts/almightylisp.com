(in-package #:magazine/hypermedia/components)

;;;; An article's body: the blocks almighty-press reads from org.

(defun src-arg (block name)
  (cdr (assoc name (getf (rest block) :args) :test #'string=)))

(defun render-src-tag (key value)
  (when value
    (ah:</>
     (span :class "src-tag"
       (span :class "src-k" key)
       (span :class "src-v" value)))))

(defun render-src (block)
  "A source block, drawn as a plate. :buffer and :package are tags over its
top edge, the lines numbered in :highlight are marked, and #+RESULTS: is a stub
below a perforation. Each is left out when it isn't given."
  (let ((buffer (src-arg block "buffer"))
        (package (src-arg block "package"))
        (highlight (getf (rest block) :highlight))
        (lang (getf (rest block) :lang))
        (results (getf (rest block) :results)))
    (ah:</>
     (figure :class (ah:clsx "src" (when results "has-results"))
       (when (or buffer package)
         (ah:</>
          (figcaption :class "src-tags"
            (render-src-tag "Buffer" buffer)
            (render-src-tag "Package" package))))
       (div :class "src-body"
         (loop for line in (getf (rest block) :lines)
               for n from 1
               collect (ah:</>
                        (div :class (ah:clsx "src-line" (when (member n highlight) "is-hit"))
                          (code :class lang (if (string= line "") " " line))))))
       (when results
         (ah:</>
          (div :class "src-returns"
            (span :class "src-k" "Returns")
            (pre :class "src-result" results))))))))

(defun render-table (header rows)
  (flet ((cells (tag row)
           (mapcar (lambda (cell)
                     (if (eq tag :th)
                         (ah:</> (th (render-inlines cell)))
                         (ah:</> (td (render-inlines cell)))))
                   row)))
    (ah:</>
     (table :class "rule"
       (when header
         (ah:</> (thead (tr (cells :th header)))))
       (tbody
         (mapcar (lambda (row) (ah:</> (tr (cells :td row)))) rows))))))

(defun render-list (block)
  "A (:list kind items) block. An item's first paragraph is its text; any
more are set as paragraphs under it, then its sublists."
  (destructuring-bind (kind items) (rest block)
    (let ((rendered (mapcar (lambda (item)
                              (destructuring-bind (paragraphs sublists) item
                                (ah:</>
                                 (li
                                   (render-inlines (first paragraphs))
                                   (mapcar (lambda (more) (ah:</> (p (render-inlines more))))
                                           (rest paragraphs))
                                   (mapcar #'render-list sublists)))))
                            items)))
      (if (eq kind :ol)
          (ah:</> (ol :class "prose-list" rendered))
          (ah:</> (ul :class "prose-list" rendered))))))

(defun render-block (block)
  "A block in a section's text column. A note is also set here, under the
paragraph before it, for the screens that have no margin column."
  (ecase (first block)
    (:p (ah:</> (p :class "prose" (render-inlines (second block)))))
    (:h (ah:</> (h3 :class "subhead" (second block))))
    (:src (render-src block))
    (:table (render-table (second block) (third block)))
    (:list (render-list block))
    (:note (render-margin-note block :inline t))))

(defun render-margin-note (note &key inline)
  "A #+BEGIN_NOTE block. A note labelled Syntax is set in the code face."
  (destructuring-bind (label paragraphs) (rest note)
    (ah:</>
     (aside :class (ah:clsx "margin-note" (when inline "inline-note")
                            (when (string-equal label "Syntax") "syntax"))
       (p :class "margin-note-label" label)
       (mapcar (lambda (paragraph)
                 (ah:</> (p :class "margin-note-body" (render-inlines paragraph))))
               paragraphs)))))

;;;; Headline size
;;;
;;; A lead headline is set at up to 136px in a 902px box, which holds about
;;; nine uppercase characters a line. Longer titles step down in size until
;;; they wrap to *headline-lines* lines or fewer and no single word is wider
;;; than the box. The box grows with the headline; only this sets the limit.

(defparameter *headline-chars* 10
  "Uppercase characters that fit on one headline line at full size.")

(defparameter *headline-lines* 4
  "The most lines a headline may take before it is set smaller.")

(defun wrapped-line-count (lines capacity)
  "Lines the title takes when each author line is wrapped greedily at CAPACITY."
  (loop for line in lines
        sum (let ((count 1) (used 0))
              (dolist (word (press:split-words line) count)
                (let ((need (if (zerop used) (length word) (+ used 1 (length word)))))
                  (if (or (zerop used) (<= need capacity))
                      (setf used need)
                      (setf count (1+ count) used (length word))))))))

(defun headline-scale (lines)
  (let ((longest (reduce #'max (mapcan #'press:split-words lines) :key #'length :initial-value 1)))
    (loop for scale from 1.0 downto 0.5 by 0.05
          for capacity = (/ *headline-chars* scale)
          when (and (<= longest capacity)
                    (<= (wrapped-line-count lines capacity) *headline-lines*))
            return scale
          finally (return 0.5))))

(defun render-headline (lines &key class href)
  "A title set as a headline, scaled to fit; linked to HREF when given."
  (let ((text (press:render-lines lines))
        (style (format nil "--headline-scale: ~,2f" (headline-scale lines))))
    (ah:</>
     (h1 :class (ah:clsx "neo-headline" class) :style style
       (if href (ah:</> (a :href href text)) text)))))
