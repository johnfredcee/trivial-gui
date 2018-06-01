
(in-package :trivial-gui)


(defun allocate-vertex-array (vertices)
  (let ((result
	 (gl:alloc-gl-array :float (* (length vertices) 3))))
    (dotimes (i (length vertices))
      (let ((array-index (* 3 i)))
	(setf (gl::glaref result array-index)       (aref vertices (list v 0)))
	(setf (gl::glaref result (+ array-index 1)) (aref vertices (list v 1)))
	(setf (gl::glaref result (+ array-index 2)) (aref vertices (list v 2)))
    result))
	  
(defun allocate-index-array (indices)
  (let ((result
	 (gl:alloc-gl-array :unsigned-short (length indexes))))
    (dotimes (i (length indices))
      (setf (gl::glaref result i) (aref indices i)))
    result))

(defun make-vertex-buffer-data (xmin xmax ymin ymax)
  (let
      ((vertices 
    (static-vectors:make-static-vector 12 :element-type 'single-float  :initial-contents vertices)))

(defun make-index-buffer-data (indices)
    (static-vectors:make-static-vector (length indices) :element-type '(unsigned-byte 16)  :initial-contents indices)))

(defgeneric render ((e gui-element)))

(defmethod render ((e gui-element))	 
  (let ((vertex-buffer (car (slot-value 'buffers-of e)))
	(index-buffer (cadr (slot-value 'buffers-of e))))
    (gl::bind-buffer :array-buffer vetex-buffer)
    (let ((arr (allocate-vertex-array #2A( ( xmin ymin 0f0)
					   ( xmin ymax 0f0)
					   ( xmax ymax 0f0)
					   ( xmax ymin 0f0)))))
      (gl::buffer-data :array-buffer :static-draw arr)
      (gl::free-gl-array arr))
    (gl::bind-buffer :element-array-buffer index-buffer)
    (let ((arr  #( 0 1 2  1 2 3)))
      (gl::buffer-data :element-array-buffer arr)
      (gl::free-gl-array))))
       
