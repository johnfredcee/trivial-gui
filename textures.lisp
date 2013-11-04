
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

(defun save-texture-data (name)
  "Create a texture from the current vecto canvas named using a keyword name."
  (assert (keywordp name)) 
  (let* ((image-data 
		 (zpng::image-data (vecto::image vecto::*graphics-state*)))
		(width
		 (zpng::width (vecto::image vecto::*graphics-state*)))
		(height
		 (zpng::height (vecto::image vecto::*graphics-state*)))
		(tex
		 (make-texture :id (car (gl::gen-textures 1)) :width width :height height)))
	(setf (gethash name *textures*) tex)
	(select-texture name)
	(gl:tex-parameter :texture-2d :texture-min-filter :linear)
	(gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte image-data)))

(defun release-texture (name)
  (when (eql name *selected-texture*)
	(deselect-texture))
  (let ((tex (gethash name *textures*)))
	(gl:delete-textures (list (tex-id tex)))))

(defun release-all-textures ()
  (iterate 
	(for (name nil) in-hashtable *textures*)
   (release-texture name)))
