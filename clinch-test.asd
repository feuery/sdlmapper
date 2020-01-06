;;;; clinch-test.asd


(asdf:defsystem #:clinch-test
  :description "Describe clinch-test here"
  :author "Feuer <feuer@feuerx.net>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:clinch #:cl-arrows #:clinch-freeimage #:fset #:rutils #:cl-strings #:cl-ppcre #:usocket #:bordeaux-threads #:cl-fad)
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
  	       (:file "tileset"
	       	:depends-on ("qmapper_std"
	       		     "tile"
	       		     "root"
	       		     "export"))
	       (:file "obj")
	       
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
	       (:file "clinch-test")))
