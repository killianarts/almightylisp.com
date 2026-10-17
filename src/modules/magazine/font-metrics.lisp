(defpackage #:magazine/font-metrics
  (:use #:cl)
  (:export #:font #:load-font #:font-units-per-em #:font-ascender #:font-descender
           #:text-width))

(in-package #:magazine/font-metrics)

;;;; How wide a renderer will set a line of text, read from the font file.
;;;
;;; A line's width is the sum of its characters' advances, plus the letter
;;; spacing after each. Kerning is left out: for the fonts the preview images
;;; use, it moves a title by well under one percent (checked with hb-shape).
;;; Reads TrueType and OpenType files: only the head, hhea, maxp, hmtx and
;;; cmap tables, which both kinds have.

(defstruct (font (:constructor %make-font))
  units-per-em ascender descender
  ;; Character code to glyph index.
  (cmap (make-hash-table) :type hash-table)
  ;; Glyph index to advance, in font units.
  (advances #() :type vector))

(defun u16 (bytes at)
  (logior (ash (aref bytes at) 8) (aref bytes (1+ at))))

(defun s16 (bytes at)
  (let ((n (u16 bytes at)))
    (if (>= n #x8000) (- n #x10000) n)))

(defun u32 (bytes at)
  (logior (ash (u16 bytes at) 16) (u16 bytes (+ at 2))))

(defun read-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun table-offset (bytes tag)
  "Where the table TAG starts in BYTES."
  (or (loop for i below (u16 bytes 4)
            for at = (+ 12 (* 16 i))
            when (string= tag (map 'string #'code-char (subseq bytes at (+ at 4))))
              return (u32 bytes (+ at 8)))
      (error "The font has no ~a table." tag)))

(defun read-cmap-4 (bytes at table)
  "Fill TABLE from the format 4 subtable at AT: segments of codes, each
mapped by an offset or through the glyph array."
  (let* ((segments (/ (u16 bytes (+ at 6)) 2))
         (ends (+ at 14))
         (starts (+ ends (* 2 segments) 2))
         (deltas (+ starts (* 2 segments)))
         (ranges (+ deltas (* 2 segments))))
    (dotimes (i segments)
      (let* ((start (u16 bytes (+ starts (* 2 i))))
             (end (u16 bytes (+ ends (* 2 i))))
             (delta (u16 bytes (+ deltas (* 2 i))))
             (range-at (+ ranges (* 2 i)))
             (range (u16 bytes range-at)))
        ;; The last segment, for #xFFFF alone, maps nothing.
        (loop for code from start to (min end #xFFFE)
              for glyph = (if (zerop range)
                              (mod (+ code delta) #x10000)
                              (let ((g (u16 bytes (+ range-at range (* 2 (- code start))))))
                                (if (zerop g) 0 (mod (+ g delta) #x10000))))
              unless (zerop glyph)
                do (setf (gethash code table) glyph))))))

(defun read-cmap-12 (bytes at table)
  "Fill TABLE from the format 12 subtable at AT: runs of codes and glyphs."
  (loop repeat (u32 bytes (+ at 12))
        for group from (+ at 16) by 12
        do (loop for code from (u32 bytes group) to (u32 bytes (+ group 4))
                 for glyph from (u32 bytes (+ group 8))
                 do (setf (gethash code table) glyph))))

(defun read-cmap (bytes)
  "The font's Unicode character map: from its format 12 subtable when it has
one, which covers every plane, or else its format 4."
  (let* ((cmap (table-offset bytes "cmap"))
         (subtables (loop for i below (u16 bytes (+ cmap 2))
                          for at = (+ cmap 4 (* 8 i))
                          for platform = (u16 bytes at)
                          for encoding = (u16 bytes (+ at 2))
                          for offset = (+ cmap (u32 bytes (+ at 4)))
                          when (or (= platform 0)
                                   (and (= platform 3) (member encoding '(1 10))))
                            collect offset))
         (table (make-hash-table)))
    (let ((format-12 (find 12 subtables :key (lambda (at) (u16 bytes at))))
          (format-4 (find 4 subtables :key (lambda (at) (u16 bytes at)))))
      (cond (format-12 (read-cmap-12 bytes format-12 table))
            (format-4 (read-cmap-4 bytes format-4 table))
            (t (error "The font has no Unicode character map.")))
      table)))

(defun read-advances (bytes)
  "Each glyph's advance. Glyphs past hhea's numberOfHMetrics share the last
one's, as monospaced fonts' do."
  (let* ((hhea (table-offset bytes "hhea"))
         (hmtx (table-offset bytes "hmtx"))
         (metrics (u16 bytes (+ hhea 34)))
         (glyphs (u16 bytes (+ (table-offset bytes "maxp") 4)))
         (advances (make-array glyphs)))
    (dotimes (glyph glyphs advances)
      (setf (aref advances glyph)
            (u16 bytes (+ hmtx (* 4 (min glyph (1- metrics)))))))))

(defun load-font (path)
  (let* ((bytes (read-bytes path))
         (hhea (table-offset bytes "hhea")))
    (%make-font :units-per-em (u16 bytes (+ (table-offset bytes "head") 18))
                :ascender (s16 bytes (+ hhea 4))
                :descender (s16 bytes (+ hhea 6))
                :cmap (read-cmap bytes)
                :advances (read-advances bytes))))

(defun text-width (font text size &key (letter-spacing 0))
  "How wide TEXT is set in FONT at SIZE pixels, with LETTER-SPACING pixels
after each character. A character the font lacks counts as its .notdef."
  (let ((scale (/ size (font-units-per-em font))))
    (+ (* scale (loop for char across text
                      sum (aref (font-advances font)
                                (gethash (char-code char) (font-cmap font) 0))))
       (* letter-spacing (length text)))))
