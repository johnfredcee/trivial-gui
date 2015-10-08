;;; -*- Mode: LISP; Syntax: COMMON-LISP;  Base: 10;  -*-

(in-package #:trivial-gui)

(defstruct (vec2 (:type (vector short-float)))
  (x-of 0.0 :type short-float)
  (y-of 0.0 :type short-float))

(defstruct (vec3 (:type (vector short-float)))
  (x-of 0.0 :type short-float)
  (y-of 0.0 :type short-float)
  (z-of 0.0 :type short-float))

(defstruct (vec4 (:type (vector short-float)))
  (x-of 0.0 :type short-float)
  (y-of 0.0 :type short-float)
  (z-of 0.0 :type short-float)
  (w-of 1.0 :type short-float))

(defun make-mat33 ()
	(make-array 9  :element-type 'short-float))

(defun mat33-aref (array row column)
	   (aref array (+ (* row 3) column)))

(defun mat33-aref-set (array row column value)
	   (setf (aref array (+ (* row 3) column)) value))

(defsetf mat33-aref mat33-aref-set)

(defun make-mat44 ()
	(make-array 12 :element-type 'short-float))

(defun mat44-aref (array row column)
	   (aref array (+ (* row 4) column)))

(defun mat44-aref-set (array row column value)
	   (setf (aref array (+ (* row 4) column)) value))

(defsetf mat44-aref mat44-aref-set)

(defun opposite-direction (v1)
  (map 'vector #'(lambda (x) (- x)) v1))

;; convientnt lisp-style aritmetic operators that mix vectors and scalars
(defun vec+ (&rest v)
  (labels ((sum (&optional v1 v2)
			 (typecase v2
			   (null v1)
			   (float v2 (map 'vector #'(lambda (x) (+ x v2)) v1))
			   (t (map 'vector #'+ v1 v2)))))
	(reduce #'sum v)))

(defun vec- (&rest v)
  (labels ((neg (v1)
			 (typecase v1
			   (null v1 v1)
			   (float v1 (- v1))
			   (t (map 'vector #'- v1))))
		   (sub (&optional v1 v2)
			 (typecase v2
			   (null v1 v1)
			   (float v2 (map 'vector #'(lambda (x) (- x v2)) v1))
			   (t (map 'vector #'- v1 v2)))))
	(case (length v)
	  (1 (neg (first v)))
	  (t (reduce #'sub v)))))

(defun vec* (&rest v)
  (labels ((mul (&optional v1 v2)
			 (typecase v2
			   (null v1)
			   (float v2 (map 'vector #'(lambda (x) (* x v2)) v1))
			   (t (map 'vector #'* v1 v2)))))
	(reduce #'mul v)))

(defun vec/ (&rest v)
  (labels ((div (&optional v1 v2)
			 (typecase v2
			   (null v1)
			   (float v2
					  (let ((v (/ 1.0 v2)))
						(map 'vector #'(lambda (x) (* x v)) v1)))
			   (t (map 'vector #'/ v1 v2)))))
	(reduce #'div v)))

(defun vec-min (&rest v)
  (labels ((vmin (&optional v1 v2)
			 (typecase v2
			   (null v1)
			   (float v2
					  (map 'vector #'(lambda (x) (min x v2)) v1))
			 (t (map 'vector #'min v1 v2)))))
	(reduce #'vmin v)))

(defun vec-max (&rest v)
  (labels ((vmax (&optional v1 v2)
			 (typecase v2
			   (null v1)
			   (float v2
					  (map 'vector #'(lambda (x) (max x v2)) v1))
			 (t (map 'vector #'max v1 v2)))))
	(reduce #'vmin v)))

;; inplace "fast" operations
(defun scale (s v)
 (map-into v #'(lambda (x) (* x s)) v))

(defun scale-and-add (s v c)
 (map-into v #'(lambda (x) (+ (* x s) c)) v))

(defun negate (v)
  (map-into v #'(lambda (x) (- x)) v)) 

(defun inverse (v)
  (map-into v #'(lambda (x) (/ 1.0 x)) v)) 

;; operations with scalar results
(defun squared-distance (v1 v2)
  (reduce #'+
		  (map 'vector #'(lambda (x y) (let ((diff (- x y))) (* diff diff))) v1 v2)))

(defun distance (v1 v2)
  (sqrt
   (reduce #'+
		  (map 'vector #'(lambda (x y) (let ((diff (- x y))) (* diff diff))) v1 v2))))

(defun squared-length (v)
  (reduce #'+
		  (map 'vector #'(lambda (x) (* x x)) v))) 

(defun length (v)
  (sqrt
   (reduce #'+
		  (map 'vector #'(lambda (x) (* x x)) v)))) 

(defun dot (v1 v2)
   (reduce #'+
		  (map 'vector #'(lambda (x y) (* x y)) v1 v2))) 

;; operations with vector results

(defun normal (v)
  (let ((len (reduce #'+ (map 'vector #'(lambda (x) (* x x)) v)))) 
	(when (not (zerop len))
	  (let ((lenr (/ 1.0 (sqrt len))))
		(map 'vector #'(lambda (x) (* x lenr)) v)))))

(defun lerp (v1 v2 t)
  (map 'vector #'(lambda (x y) (+ x (+ (* t (- y x)))) v1 v2)))

;; always oddly dimensionally specific
(defun cross (v1 v2)
  (make-vec3 :x-of (- (* (y-of v1) (z-of v2)) (* (z-of v1) (y-of v2)))
			 :y-of (- (* (z-of v1) (x-of v2)) (* (x-of v1) (z-of v2)))
			 :z-of (- (* (x-of v1) (y-of v2)) (* (z-of v1) (x-of v2)))))

;; hermite
;; bezier
;; random
