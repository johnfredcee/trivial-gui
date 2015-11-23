
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TRIVIAL-GUI; Base: 10;  -*-

(in-package #:trivial-gui)

;; to map eye coordinates to clip coordinates
;; glMatrixMode(GL_PROJECTION);
;; glLoadIdentity();
;; glOrtho(0.0f, (GLfloat) width, 0.0f, (GLfloat) height, -1.0f, 1.0f);
;; glMatrixMode(GL_MODELVIEW);
;; glLoadIdentity();
;; glViewport(0, 0, width, height);

;; -- Glop overrides --------------------
 
(defmethod glop:on-key (window pressed keycode keysym text)
  (format t "Key ~:[released~;pressed~]: ~D (~S ~S)~%" pressed keycode keysym text)
  (format t "Key pressed: ~S~%" (glop:key-pressed keycode))
  (when (and (not pressed) (eq keysym :escape))
    (glop:push-close-event window))
  (case keysym
    (:h (glop:hide-cursor window))
    (:j (glop:show-cursor window))
    (:left (decf (glop:window-x window)))
    (:right (incf (glop:window-x window)))
    (:up (decf (glop:window-y window)))
    (:down (incf (glop:window-y window)))
    (:page-up (progn (incf (glop:window-width window) 10)
                     (incf (glop:window-height window) 10)))
    (:page-down (progn (decf (glop:window-width window) 10)
                       (decf (glop:window-height window) 10))))
  (when (and (not pressed) (eq keysym :f))
    (glop:toggle-fullscreen window))
  (when (and (not pressed) (eq keysym :g))
    (glop:set-fullscreen window)))

(defmethod glop:on-button (window pressed button)
  (declare (ignore window))
  (format t "Button ~:[released~;pressed~]: ~S~%" pressed button))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window x y dx dy))
  (format t "Mouse motion~%"))

(defmethod glop:on-resize (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h)
  (format t "Resize: ~Sx~S~%" w h))

(defmethod glop:on-draw (window)
  (declare (ignore window))
  (format t "Draw~%"))

(defmethod glop:on-close (window)
  (declare (ignore window))
  (format t "Close~%"))

;; -- Drawing primitives ---------------------

(defun draw-quad* (x y w h color)
  (gl:with-primitives :quads
	(let ((r (car color))
		  (g (cadr color))
		  (b (caddr color)))
	(gl:color r g b)
	(gl:vertex x (+ y h))
	(gl:color r g b)
	(gl:vertex (+ x w) (+ y h))
	(gl:color  r g b)
	(gl:vertex (+ x w) y)
	(gl:color r g b)
	(gl:vertex x y))))


(defun draw-quad (origin extent color)
  (draw-quad* (x-of origin)
	      (y-of origin)
	      (- (x-of extent) (x-of origin))
	      (- (y-of extent) (y-of origin))
	      color))
