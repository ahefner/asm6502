(defpackage ichr
  (:use common-lisp)
  (:import-from asm6502 binary-file)
  (:export #:decode-chr #:encode-chr #:encode-gif
           #:write-gif #:read-gif
	   #:chunkify
	   #:swizzle-rows
	   #:append-rows))

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

;;; Encode 2bpp 8x8 characters into NES bitplaned format
(defun encode-chr (pixels)
  (loop with output = (make-array
		       0
		       :element-type '(unsigned-byte 8)
		       :fill-pointer t
		       :adjustable t)
          for y from 0 below (array-dimension pixels 0) by 8 ;sheet-height
          do
             (loop for x from 0 below (array-dimension pixels 1) by 8 ;sheet-width
		   do
		      (dotimes (plane 2)
			(dotimes (oy 8)
			  (vector-push-extend
			   (loop for ox from 0 below 8 summing (ash (ldb (byte 1 plane) (aref pixels (+ y oy) (+ x (- 7 ox)))) ox))
			   output))))
          finally (return output)))

(defun encode-gif (pathname)
  "Helper function: Equivalent to (encode-chr (read-gif pathname))"
  (encode-chr (read-gif pathname)))

;;;; These are slightly more general utilities which I'm writing for the Atari
;;;; 8-bit but apply to some other chunky pixel situations.

(defun combine-pair (x y nbits)
  (dpb (ldb (byte nbits 0) x)
       (byte nbits nbits)
       (ldb (byte nbits 0) y)))

(defun chunkify-neighbors (nbits matrix)
  (assert (evenp (array-dimension matrix 1)))
  (let ((output (make-array (list (array-dimension matrix 0)
				  (ash (array-dimension matrix 1) -1)))))
    (dotimes (row (array-dimension matrix 0))
      (dotimes (col (array-dimension output 1))
	(setf (aref output row col)
	      (combine-pair (aref matrix row (* col 2))
			    (aref matrix row (1+ (* col 2)))
			    nbits))))
    output))

(defun is-power-of-two? (x)
  (and (integerp x)
       (> x 0)
       (zerop (logand x (1- x)))))

(defun chunkify (bpp byte-width matrix)
  (assert (is-power-of-two? bpp))
  (assert (<= bpp byte-width))
  (if (= bpp byte-width)
      matrix
      (chunkify
       (* 2 bpp)
       byte-width
       (chunkify-neighbors bpp matrix))))

;;; This will rearrange groups of 1 column by 'tile-height' row
;;; character cells in a 2D image such that the rows of the character
;;; are now adjacent columns in the output. Output width is multiplied
;;; by tile-height, height is divided by tile-height.
(defun swizzle-rows (tile-height matrix)
  (assert (zerop (mod (array-dimension matrix 0) tile-height)))
  (loop with output = (make-array (list (/ (array-dimension matrix 0) tile-height)
					(* (array-dimension matrix 1) tile-height)))
	for row upfrom 0
	for y0 below (array-dimension matrix 0) by tile-height 
	do
	   (loop for x0 from 0 below (array-dimension matrix 1) do
	     (loop for i from 0 below tile-height do
	       (setf (aref output row (+ i (* x0 tile-height)))
		     (aref matrix (+ y0 i) x0))))
	finally (return output)))

(defun append-rows (matrix)
  (loop with n = (array-total-size matrix)
	with result = (make-array n)
	for i below n
	do (setf (aref result i) (row-major-aref matrix i))
	finally (return result)))

