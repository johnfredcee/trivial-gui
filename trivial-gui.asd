
(asdf:defsystem #:trivial-gui
  :depends-on (#:cl-fad #:glop #:cl-opengl #:mathkit #:opticl #:cl-vectors #:iterate)
  :serial t
  :components ((:file "package")
	       (:file "glop")
	       (:file "loader")
	       (:file "shaders")
	       (:file "textures")	       
	       (:file "layout")
	       (:file "gui")))
