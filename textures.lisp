
;;; -*- Mode: LISP; Syntax: COMMON-LISP;  Base: 10;  -*-

(in-package #:trivial-gui)


;; will need to manage openGL textures created from vecto canvases and presented inside
;; boxes as buttons, etc.

(defstruct (texture (:conc-name tex-))
  (id 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defparameter *textures* (make-hash-table :test 'eql))

(defparameter *selected-texture* nil)

(defun select-texture (name)
  (let ((tex (gethash name *textures*)))
	(gl:bind-texture :texture-2d (tex-id tex))
	(setf *selected-texture* name)))

(defun deselect-texture ()
  (gl::bind-texture :texture-2d 0)
  (setf *selected-texture* nil))


(defun release-texture (name)
  (when (eql name *selected-texture*)
	(deselect-texture))
  (let ((tex (gethash name *textures*)))
	(gl:delete-textures (list (tex-id tex)))))

(defun release-all-textures ()
  (iterate 
	(for (name nil) in-hashtable *textures*)
   (release-texture name)))


(defun opticl-image-gl-tex (target level image &key border)
  (opticl:with-image-bounds (width height channels)
      image
      (let ((format
	     (case channels
	       (1 1)
	       (3 :bgr)
	       (4 :bgra))))
	(cffi:with-foreign-object (foreign-image :uint8 (* width height channels))
	  (case channels 
	    (1 (opticl:do-pixels (y x)
		   image
		   (setf (cffi:mem-aref foreign-image
					:uint8 (+ x (* y width)))
			 (opticl:pixel image y x))))		;; gray
	    (3 (opticl:do-pixels (y x)
		   image
		 (multiple-value-bind
		       (r g b)
		     (opticl:pixel image y x)
		 (let ((index (* (+ x (* y width)) 3)))
		   (setf (cffi:mem-aref foreign-image :uint8 index) b)
		   (setf (cffi:mem-aref foreign-image :uint8 (+ 1 index)) g)
		   (setf (cffi:mem-aref foreign-image :uint8 (+ 2 index)) r)))))
	    (4 (opticl:do-pixels (y x)
		   image
		 (multiple-value-bind
		       (r g b a)		     
		     (opticl:pixel image y x)
		 (let ((index (* (+ x (* y width)) 3)))
		   (setf (cffi:mem-aref foreign-image :uint8 index) b)
		   (setf (cffi:mem-aref foreign-image :uint8 (+ 1 index)) g)
		   (setf (cffi:mem-aref foreign-image :uint8 (+ 2 index)) r)
		   (setf (cffi:mem-aref foreign-image :uint8 (+ 3 index)) a))))))
	  (let ((name
			 (gl:tex-parameter :texture-2d :texture-min-filter :linear)))
		(%gl:tex-image-2d target level format width height (if border 1 0) format  :unsigned-byte foreign-image)
		(setf (gethash name *textures*) (make-texture :id name :width width :height height)))))))
	    
	
