
(asdf:defsystem #:trivial-gui
  :depends-on (#:cl-fad #:geometry #:glop #:cl-opengl #:mathkit #:opticl #:static-vectors #:cl-vectors #:iterate)
  :serial t
  :components ((:file "package")
	       (:file "glop")
	       (:file "loader")
	       (:file "shaders")
	       (:file "textures")	       
	       (:file "layout")
	       (:file "gui")))
