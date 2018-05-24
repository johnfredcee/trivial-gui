;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TRIVIAL-GUI; Base: 10;  -*-

(in-package #:trivial-gui)

(defparameter *layouts* (make-hash-table :test 'eql))

(defun layout (name)
  (layout-updater (gethash name *layouts* (make-layout))))

(defun vertical-layout-updater (parent)
  (let* ((children (children-of parent))
		(interval (/ 1f0 (length children))))
	(iterate
	  (for child in-vector children with-index child-index)
	  (setf (slot-value child 'origin) (sb-cga:vec 0f0 (* interval child-index) 0f0))
	  (setf (slot-value child 'size) (sb-cga:vec 1f0 interval 0f0)))))

(setf (gethash :vertical *layouts*) #'vertical-layout-updater)

(defun horizontal-layout-updater (parent)
  (let* ((children (children-of parent))
		(interval (/ 1f0 (length children))))
	(iterate
	  (for child in-vector children with-index child-index)
	  (setf (slot-value child 'origin) (sb-cga:vec (* interval child-index) 0f0 0f0))
	  (setf (slot-value child 'size) (sb-cga:vec interval 1f0 0f0)))))

(setf (gethash :horizontal *layouts*) #'horizontal-layout-updater)

