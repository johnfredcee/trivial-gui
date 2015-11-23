
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TRIVIAL-GUI; Base: 10;  -*-

(in-package #:trivial-gui)

;; gui-element will be the root -- will need it's own internal coordinate system
;; there will need to be methods to scale units from viewport system to internal
;; for fitting things, etc.

;; -- helpers --------------------
(declaim (inline x-of))
(defun x-of (a)
  (aref a 0))

(declaim (inline y-of))
(defun y-of (a)
  (aref a 1))

;; -- classes --------------------
(defclass gui-element ()
  ((tfmatrix :reader transform-of)
   (inverse :reader inverse-transform-of)
   (origin :reader origin-of :initarg :origin :initform (sb-cga:vec 0f0 0f0 0f0))
;;   (orientation :reader orientation-of :initarg :orientation :initform 0f0)
   (size :reader size-of :initarg :size :initform #(0f0 0f0))))


(defun compute-transform (element)
  (let ((tfmat
	 (sb-cga::matrix (x-of (size-of element)) 0f0                       0f0 (x-of (origin-of element))
			 0f0                      (y-of (size-of element))  0f0 (y-of (origin-of element))
			 0f0                      0f0                       1f0 0f0
			 0f0                      0f0                       0f0 1f0)))
    (setf (slot-value element 'tfmatrix) tfmat)
    (setf (slot-value element 'inverse) (sb-cga:inverse-matrix tfmat))))


(defgeneric set-origin (element x y))

(defmethod set-origin ((element gui-element) x y)
  (setf (slot-value element 'origin) (sb-cga:vec x y 0.0))
  (compute-transform element))

;; (defgeneric set-orientation (element x y))

;; (defmethod set-orientation ((element gui-element) x y)
;;   )

(defgeneric set-scale (element x y))

(defmethod set-scale ((e gui-element) x y)
    (setf (slot-value e 'size) (sb-cga:vec x y 0.0))
    (compute-transform e))

(defmethod initialize-instance :after ((e gui-element) &key)
    (compute-transform e))
  
;; -- generic functiosn --------------------
(defgeneric render (element)
 (:documentation "Draw a gui - element"))

(defgeneric screen-to-element (element x y)
 (:documentation "Convert x,y from screen coordinates to internal element coordinates" ))

(defgeneric element-to-screen (element x y)
  (:documentation "Convert x,y from internal element coordinates to screen coordinates"))

;; -- gui element methods --------------------

(defmethod render ((e gui-element))
  (let ((origin (sb-cga:transform-point (sb-cga:vec 0.0 0.0 0.0) (transform-of e)))
	(extent (sb-cga:transform-point (sb-cga:vec 1.0 1.0 0.0) (transform-of e))))
    (draw-quad origin extent '(1.0 0.0 0.0))))

(defmethod screen-to-element ((e gui-element) x y)
  (let ((result (sb-cga:transform-point (sb-cga::vec x y 0.0) (transform-of e))))
    (values (x-of result) (y-of result))))

(defmethod element-to-screen ((e gui-element) x y)
  (let ((result (sb-cga:transform-point (sb-cga::vec x y 0.0) (inverse-transform-of e))))
    (values (x-of result) (y-of result))))

;; -- Gui implementation --------------------
(defparameter *gui* nil)

(defun make-gui-element (x y w h)
  (let ((element 
	 (make-instance 'gui-element :origin (sb-cga:vec x y 0f0) :size (sb-cga:vec w h 0f0))))
    (pushnew element *gui*)))

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
error. Remember to hit C in slime or pick the restart so errors don't kill the app."
  `(restart-case (progn ,@body) (continue () :report "Continue")))

(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl
working while trivial-gui runs"
  (continuable (let ((connection (or swank::*emacs-connection* (swank::default-connection)))) 
				 (when connection (swank::handle-requests connection t)))))

(defun draw-gui ()
  (when *gui*
	(mapcar #'render *gui*)))

(defun trivial-gui (&key (window-name "Trivial Gui") (window-width 800) (window-height 600))
  (glop:with-window (win window-name window-width window-height)
    (format t "Created window: ~S~%" win)
    (gl:clear-color 0.3 0.3 1.0 0)
    (loop while (glop:dispatch-events win :blocking nil) do
		 (update-swank)
		 (gl:clear :color-buffer)
		 ;; to do -- add an exit loop restart?
		 (with-simple-restart (skip-trivial-gui-loop "Skip Trivial GUI loop body")
		   (draw-gui))
         (gl:flush)
         (glop:swap-buffers win))))
