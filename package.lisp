;;;; package.lisp

(defpackage #:cl-opengl-test
  (:use #:cl #:qmapper.obj
	:multimethods
	:qmapper.root
	:qmapper.std
	:qmapper.tileset
	#:qmapper.app-state))
