(in-package #:magazine/hypermedia/components)

;;;; An evolving article's body (#+LAYOUT: evolving).
;;;
;;; The page is an article page with different sections. Each section is
;;; written out plainly here: its heading, the notes and source blocks that
;;; can take the heading's place, and its paragraphs, each saying what is
;;; beside it. static/js/magazine-evolving.js builds the moving page out of
;;; that; without it the section reads as ordinary text.
;;;
;;;   section.neo-piece.ev
;;;     h2.ev-title
;;;     figure.ev-block#id       a source block: a <pre> for each step, and
;;;                              after it a pre.ev-returns for its #+RESULTS:
;;;     aside.ev-note#id         a note
;;;     div.ev-stop              a paragraph, list or table; or a paragraph
;;;                              ending in a colon with the list or table
;;;                              it introduces
;;;       data-block="id"        what is beside it; "" for the heading
;;;       data-frame="n"         the step it moves that block to
;;;       data-head="text"       the subheading over it
;;;       mark                   a [[step][...]] expression, with data-block
;;;                              and data-frame of its own
;;;
;;; model:section-evolution works out which paragraph has what.

(defun render-evolving-inlines (text steps)
  "TEXT's inline markup, as RENDER-INLINES sets it, with each [[step][...]]
link as a mark for the next of STEPS, a list of (id . frame)."
  (labels ((render (inline)
             (if (stringp inline)
                 inline
                 (let ((inner (lambda (inlines) (mapcar #'render inlines))))
                   (ecase (first inline)
                     (:code (ah:</> (code :class "verb" (second inline))))
                     (:bold (ah:</> (strong (funcall inner (second inline)))))
                     (:italic (ah:</> (em (funcall inner (second inline)))))
                     (:underline (ah:</> (span :class "underline" (funcall inner (second inline)))))
                     (:strike (ah:</> (del (funcall inner (second inline)))))
                     (:link
                      (if (string= (second inline) "step")
                          (destructuring-bind (id . frame) (pop steps)
                            (ah:</> (mark :data-block id :data-frame (princ-to-string frame)
                                      (funcall inner (third inline)))))
                          (ah:</> (a :class "inline-link" :href (inline-href (second inline))
                                    (funcall inner (third inline)))))))))))
    (mapcar #'render (press:parse-inlines text))))

(defun render-evolving-item (item)
  (ecase (first item)
    (:code
     (destructuring-bind (id name frames) (rest item)
       (ah:</>
        (figure :class "ev-block" :id id :data-name name
          (loop for (lines results) in frames
                collect (ah:</> (pre (format nil "~{~a~^~%~}" lines)))
                when results
                  collect (ah:</> (pre :class "ev-returns" results)))))))
    (:note
     (destructuring-bind (id label paragraphs) (rest item)
       (ah:</>
        (aside :class "ev-note" :id id :data-name label
          (mapcar (lambda (paragraph) (ah:</> (p (render-inlines paragraph))))
                  paragraphs)))))))

(defun render-evolving-stop (stop)
  (flet ((render (block)
           (ecase (first block)
             (:p (ah:</> (p (render-evolving-inlines (second block) (getf stop :steps)))))
             (:list (render-list block))
             (:table (render-table (second block) (third block))))))
    (let ((frame (getf stop :frame)))
      (ah:</>
       (div :class "ev-stop"
         :data-block (or (getf stop :item) "")
         :data-frame (when frame (princ-to-string frame))
         :data-head (getf stop :head)
         :data-sub (getf stop :sub)
         :data-sub-n (when (getf stop :sub) (princ-to-string (getf stop :sub-number)))
         (render (getf stop :block))
         (mapcar #'render (getf stop :more)))))))

(defun render-evolving-piece (section)
  (multiple-value-bind (items stops) (model:section-evolution section)
    (ah:</>
     (section :class "neo-piece ev" :id (model:section-id section)
       (h2 :class "ev-title" (model:section-title section))
       (mapcar #'render-evolving-item items)
       (mapcar #'render-evolving-stop stops)))))
