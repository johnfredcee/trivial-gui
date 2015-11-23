
(asdf:defsystem #:trivial-gui
  :depends-on (#:glop #:cl-opengl #:mathkit #:vecto #:iterate)
  :serial t
  :components ((:file "package")
	       (:file "glop")
	       (:file "textures")
	       (:file "gui")))
