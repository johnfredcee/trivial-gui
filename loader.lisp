

(in-package #:trivial-gui)

(defparameter *project-system-path* nil)
(defparameter *project-data-path* nil)

(defun set-project-system (name)
  "Set the name of the game project we are working with. This is the first thing you need to do. Returns the path of system subdirectory for holding data."
  (setf *project-system-path*
	(cl-fad:pathname-directory-pathname
	 (multiple-value-bind (foundp found-system pathname previous previous-time)
	     (asdf::system-source-directory (asdf:find-system name))
	   pathname)))
  (setf *project-data-path* (merge-pathnames "data/" *project-system-path*)))

(defun slurp-file (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
   returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun slurp-texture-file (path)
  (opticl::read-tga-file (path)))
