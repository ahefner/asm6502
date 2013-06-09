(defpackage ichr
  (:use common-lisp)
  (:import-from asm6502 binary-file)
  (:export #:decode-chr #:encode-chr #:encode-gif
           #:write-gif #:read-gif))

(in-package :ichr)

;;;; NES character bitmap converter.

(defun decode-chr (array)
  (loop with num-tiles = (truncate (length array) 16)
        with columns = 16
        with rows = (ceiling num-tiles columns)
        with output = (make-array (list (* 8 rows) (* 8 columns))
                                  :initial-element 0
                                  :element-type '(unsigned-byte 8))
        for tile from 0 below num-tiles
        as otx = (* 8 (mod tile columns))
        as oty = (* 8 (truncate tile columns))
        do
        (dotimes (y 8)
          (dotimes (x 8)
            (setf (aref output (+ oty y) (+ otx x))
                  (logior
                   (ldb (byte 1 (- 7 x)) (aref array (+ (* tile 16) y)))
                   (ash (ldb (byte 1 (- 7 x)) (aref array (+ 8 (* tile 16) y))) 1)
                   ))))
        finally (return output)))

(defun linear-array (array)
  (coerce
   (make-array (array-total-size array)
               :element-type (array-element-type array)
               :displaced-to array)
   `(simple-array ,(array-element-type array) (*))))

(defun default-color-table ()
  (skippy:make-color-table
   :initial-contents
   (mapcar (lambda (x) (apply #'skippy:rgb-color x))
           '((0 0 0)
             (0 255 0)
             (255 0 0)
             (255 255 255)))))

(defun write-gif (filename pixels)
  (let* ((width (array-dimension pixels 1))
         (height (array-dimension pixels 0))
         (data-stream (skippy:make-data-stream
                       :width width
                       :height height
                       :color-table (default-color-table))))
    (skippy:add-image
     (skippy:make-image :width width :height height
                        :image-data (linear-array pixels))
     data-stream)
    (skippy:output-data-stream data-stream filename)))

(defun linear-to-matrix (linear width height)
  (let ((matrix (make-array (list height width)
                            :element-type (array-element-type linear))))
    (dotimes (y height matrix)
      (dotimes (x width)
        (setf (aref matrix y x) (aref linear (+ x (* y width))))))))

(defun read-gif (filename)
  (let* ((ds (skippy:load-data-stream filename))
         (img (elt (skippy:images ds) 0)))
    (values
     (linear-to-matrix (skippy:image-data img)
                       (skippy:width img)
                       (skippy:height img))
     (skippy:color-table ds))))

(defun encode-chr (pixels)
  (loop ;with sheet-width = (truncate (array-dimension pixels 1) 8)
        ;with sheet-height = (truncate (array-dimension pixels 0) 8)
        with output = (make-array ;;(* 16 sheet-width sheet-height)
                       0
                       :element-type '(unsigned-byte 8)
                       :fill-pointer t
                       :adjustable t)
        for y from 0 below (array-dimension pixels 0) by 8 ;sheet-height
        do
        (loop for x from 0 below (array-dimension pixels 1) by 8;sheet-width
              do
              (dotimes (plane 2)
                (dotimes (oy 8)
                  (vector-push-extend
                   (loop for ox from 0 below 8 summing (ash (ldb (byte 1 plane) (aref pixels (+ y oy) (+ x (- 7 ox)))) ox))
                   output))))
        finally (return output)))

(defun encode-gif (pathname)
  "Helper function: Equivalent to (encode-chr (read-gif pathname))"
  (encode-char (read-gif pathname)))
