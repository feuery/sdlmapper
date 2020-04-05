;;;; clinch-test.asd


(asdf:defsystem #:cl-opengl-test
  :description "Describe cl-opengl-test here"
  :author "Feuer <feuer@feuerx.net>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-arrows #:fset #:rutils #:cl-strings #:cl-ppcre #:usocket #:bordeaux-threads #:cl-fad #:sdl2 #:sdl2-image)
  :components ((:file "export")
	       (:file "qmapper_std")
	       (:file "script"
	       	:depends-on ("qmapper_std")
		)
	       (:file "tile"
		:depends-on (;; "root"
			     "qmapper_std"
			     ))
  	       
  	       (:file "layer"
	       	:depends-on ("tile"))
	       (:file "root"
		:depends-on ("script" 
			     "export"
			     "layer"))
	       (:file "editor_events"
		:depends-on ("qmapper_std"
			     "export"))
  	       (:file "engine_events"
		:depends-on ("qmapper_std"
			     "export"))
  	       (:file "obj" :depends-on ("qmapper_std"))
	       (:file "tileset"
	       	:depends-on ("qmapper_std"
	       		     "tile"
	       		     "root"
	       		     "export"))
	       
	       (:file "sprite"
	       	:depends-on ("tileset"))
	       (:file "engine"
		:depends-on ("qmapper_std"
			     "root"
			     "sprite"
			     "export"))
	       (:file "transitions"
	       	:depends-on ("qmapper_std"
	       		     "root"
	       		     "export"))
	       (:file "keyboard_loop"
		:depends-on ("qmapper_std"
			     "engine_events"
			     "root"
			     "export"))
  	       (:file "test-framework"
	       	:depends-on ("export"
	       		     "qmapper_std"
	       		     "root"))
  	       
  	       (:file "doc-server"
		:depends-on ("qmapper_std"
			     "root"
			     "script"
			     "export"
			     "root"))
	       (:file "editor_server"
		:depends-on ("qmapper_std"
			     "tileset"
			     "doc-server"))
	       
	       (:file "animatedsprite"
	       	:depends-on ("qmapper_std"
	       		     "sprite"
	       		     "tileset"
	       		     ;; "map"
	       		     "export"
	       		     "root"))
	       (:file "map"
	       	:depends-on ("qmapper_std"
	       		     "animatedsprite"
	       		     "engine"
	       		     "export"
	       		     "root"
	       		     "tileset"
	       		     "sprite"
	       		     "script"
	       		     "layer"
	       		     "tile"))
	       (:file "selection-tool"
		:depends-on ("qmapper_std"
			     "export"
			     "root"
			     "map"
			     "tile"
			     "editor_events"))
	       (:file "resize-test"
	       	:depends-on ("qmapper_std"
	       		     "test-framework"
	       		     "root"
	       		     "map"))
	       (:file "package")
	       (:file "cl-opengl-test")))
