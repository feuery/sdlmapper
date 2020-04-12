(defpackage :qmapper.app-state
  (:use :common-lisp
	:cl-arrows
	:qmapper.std)
  (:export :app-state :editor-state))

(in-package :qmapper.app-state)

(defvar app-state :editor
  "This tells whether app is showing the editor or the game. Valid values are :editor and :engine")

(defvar editor-state :tileset
  "This var controls if the editor is showing whatever current tileset or map is set as. Valid values are :tileset and :map. Map-state renders the selected tile too.")
