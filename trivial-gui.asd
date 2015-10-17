
(asdf:defsystem #:trivial-gui
  :depends-on (#:glop #:cl-opengl #:vecto #:iterate :sb-cga)
  :serial t
  :components ((:file "package")
			   (:file "textures")
			   (:file "gui")))
