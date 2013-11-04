
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TRIVIAL-GUI; Base: 10;  -*-

(in-package #:trivial-gui)

;; gui-element will be the root -- will need it's own internal coordinate system
;; there will need to be methods to scale units from viewport system to internal
;; for fitting things, etc.

;; -- classes --------------------
(defclass gui-element ()
  ((origin :accessor origin-of :initarg :origin :initform '(0.0 0.0))
   (size :accessor size-of :initarg :size :initform '(0.0 0.0))))

;; -- generic functiosn --------------------
(defgeneric render (element)
 (:documentation "Draw a gui - element"))

(defgeneric screen-to-element (element x y)
 (:documentation "Convert x,y from screen coordinates to internal element coordinates" ))

(defgeneric element-to-screen (element x y)
  (:documentation "Convert x,y from internal element coordinates to screen coordinates"))

;; -- gui element methods --------------------

(defmethod render ((e gui-element))
  (let ((x (car (origin-of e)))
		(y (cadr (origin-of e)))
		(w (car (size-of e)))
		(z (cadr (size-of e))))
  (draw-quad  x y z w '(1.0 0.0 0.0))))

(defmethod screen-to-element ((e gui-element) x y)
  (destructuring-bind (ex ey) 
	  (origin-of e) 
	(destructuring-bind (w h)
		(size-of e)
	  (values
	   (/ (- x ex) w)
	   (/ (- y ey) h)))))

(defmethod element-to-screen ((e gui-element) x y)
  (destructuring-bind (ex ey) 
	  (origin-of e) 
	(destructuring-bind (w h)
		(size-of e)
	  (values
	   (+ ex (* x w))
	   (+ ey (* y h))))))

;; -- Gui implementation --------------------
(defparameter *gui* nil)

(defun make-gui-element (x y w h)
  (let ((element 
		 (make-instance 'gui-element :origin (list x y) :size (list w h))))
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
