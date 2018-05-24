
;;; -*- Mode: LISP; Syntax: COMMON-LISP;  Base: 10;  -*-

(in-package #:trivial-gui)


(defun load-shader-program (name)
  (let ((shader-filename
	 (file-namestring (merge-pathanmes name (merge-pathnames "shaders/" *project-data-path*)))))
    (vs (gl:create-shader :vertex-shader))
    (fs (gl:create-shader :fragment-shader))
    (gl:shader-source vs (slurp-file  (concatenate :string shader-filename ".vert")))
    (gl:compile-shader vs)
    (gl:shader-source fs (slurp-file (concatenate :string shader-filenamename ".frag")))
    (gl:compile-shader fs)
    (let ((result (gl:create-program)))
      (gl:attach-shader result vs)
      (gl:attach-shader result fs)
      (gl:link-program result)
      result)))
