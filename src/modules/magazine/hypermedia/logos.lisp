(in-package #:magazine/hypermedia/components)

;;;; Series logos
;;;
;;; Drawn on a 48-unit grid from square nodes and 4.5-unit bars, the same
;;; geometry as Neo / Series logo in Figma. Each is one path filled with
;;; currentColor, so a stamp sets the colour with CSS.

(defun path-rect (x y w h)
  (format nil "M~,2f ~,2fh~,2fv~,2fh~,2fz" x y w h (- w)))

(defun path-poly (&rest points)
  "A closed polygon, wound clockwise so overlapping shapes union under nonzero."
  (let* ((pts (loop for (x y) on points by #'cddr collect (list x y)))
         (area (loop for (a b) on (append pts (list (first pts)))
                     while b
                     sum (- (* (first a) (second b)) (* (first b) (second a))))))
    (when (minusp area)
      (setf pts (reverse pts)))
    (format nil "M~{~{~,2f ~,2f~}~^L~}z" pts)))

(defun path-bar (ax ay bx by thick)
  (let* ((dx (- bx ax)) (dy (- by ay)) (len (sqrt (+ (* dx dx) (* dy dy))))
         (nx (* (/ (- dy) len) thick 0.5)) (ny (* (/ dx len) thick 0.5)))
    (path-poly (+ ax nx) (+ ay ny) (+ bx nx) (+ by ny)
               (- bx nx) (- by ny) (- ax nx) (- ay ny))))

(defun lattice-path ()
  (let ((n 11) (c 5.5))
    (let ((nodes (list (list 24 c) (list c 24) (list (- 48 c) 24) (list 24 (- 48 c)))))
      (destructuring-bind (top left right bottom) nodes
        (format nil "~{~a~}"
                (append
                 (list (apply #'path-bar (append top left (list 4.5)))
                       (apply #'path-bar (append top right (list 4.5)))
                       (apply #'path-bar (append left bottom (list 4.5)))
                       (apply #'path-bar (append right bottom (list 4.5))))
                 (loop for (x y) in nodes collect (path-rect (- x c) (- y c) n n))))))))

(defparameter *series-logos*
  (flet ((join (&rest parts) (format nil "~{~a~}" parts)))
    (list
     ;; A window, its title bar, and one view placed inside it.
     (list "MAC" "nonzero"
           (join (path-rect 0 4 48 11) (path-rect 0 4 5 40) (path-rect 43 4 5 40)
                 (path-rect 0 39 48 5) (path-rect 24 21 13 12)))
     ;; A restart loop around the condition that was signalled.
     (list "CND" "nonzero"
           (join (path-rect 17 3 29 5) (path-rect 41 3 5 42) (path-rect 2 40 44 5)
                 (path-rect 2 19 5 26) (path-poly -1.5 20 10.5 20 4.5 9)
                 (path-rect 17 17 12 12)))
     ;; C-x 2, then C-x 3 in the top window: two windows over one, each above
     ;; its mode line.
     (list "EMX" "evenodd"
           (join (path-rect 0 0 48 48) (path-rect 5 5 16 23)
                 (path-rect 27 5 16 23) (path-rect 5 33 38 10)))
     ;; The inheritance diamond CLOS turns into one precedence list.
     (list "CLS" "nonzero" (lattice-path))
     ;; Object, class, metaclass: each level a size up from the last.
     (list "MOP" "nonzero"
           (join (path-rect 8 1 32 17) (path-rect 21.75 18 4.5 6) (path-rect 13 24 22 11)
                 (path-rect 21.75 35 4.5 5) (path-rect 18 40 12 8)))
     ;; A request going out and a response coming back.
     (list "WEB" "nonzero"
           (join (path-rect 0 9 34 5.5) (path-poly 32 2 48 11.75 32 21.5)
                 (path-rect 14 33.5 34 5.5) (path-poly 16 26.5 0 36.25 16 46)))
     ;; A reticle on the frame being inspected.
     (list "DBG" "nonzero"
           (join (path-rect 16 16 16 16) (path-rect 21 0 6 11) (path-rect 21 37 6 11)
                 (path-rect 0 21 11 6) (path-rect 37 21 11 6)))
     ;; A system and the dependencies it loads.
     (list "SYS" "nonzero"
           (join (path-rect 17 0 14 13) (path-rect 21.75 13 4.5 8) (path-rect 4 19 40 4.5)
                 (path-rect 3.75 19 4.5 17) (path-rect 21.75 19 4.5 17)
                 (path-rect 39.75 19 4.5 17) (path-rect 0 35 12 13)
                 (path-rect 18 35 12 13) (path-rect 36 35 12 13)))
     ;; A passing check.
     (list "TST" "nonzero"
           (join (path-bar 3 25 18 40 8) (path-bar 15 40 45 8 8) (path-rect 13 35 9 9)))))
  "Code, fill rule, and path data for each series logo.")

(defun render-series-logo (code &key (class "series-logo"))
  "The logo for series CODE. A series without a drawn logo gets a plain square."
  (destructuring-bind (fill-rule path)
      (or (rest (assoc code *series-logos* :test #'string=))
          (list nil (path-rect 6 6 36 36)))
    (ah:</>
     (svg :class class :viewBox "0 0 48 48" :aria-hidden "true" :focusable "false"
       (path :d path :fill-rule fill-rule :fill "currentColor")))))

(defun render-series-stamp (code &key (tone "ink"))
  "The logo in a small square: ink stamps on paper, paper stamps on black bars."
  (ah:</>
   (span :class (ah:clsx "series-stamp" tone) (render-series-logo code))))
