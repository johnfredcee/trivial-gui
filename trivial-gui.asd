
(asdf:defsystem #:trivial-gui
  :depends-on (#:glop #:cl-opengl #:mathkit #:opticl #:cl-vectors #:iterate)
  :serial t
  :components ((:file "package")
	       (:file "glop")
	       (:file "textures")
	       (:file "gui")))
