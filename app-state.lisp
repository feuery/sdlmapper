(defpackage :qmapper.app-state
  (:use :common-lisp
	:cl-arrows
	:qmapper.export
	:qmapper.std)
  (:export :dragged-table :app-state :*renderer* :*window* :editor-state))

(in-package :qmapper.app-state)

(defvar app-state :editor
  "This tells whether app is showing the editor or the game. Valid values are :editor and :engine")

(defvar editor-state :tileset
  "This var controls if the editor is showing whatever current tileset or map is set as. Valid values are :tileset and :map. Map-state renders the selected tile too.")

;; (setf editor-state :map)

(defvar dragged-table nil)


(defvar *renderer* nil
  "Contains the renderer used by sdl when doing image things")
(defvar *window* nil)

(defun-export! correct-document ()
  (if (equalp app-state :editor)
      qmapper.root:*document*
      qmapper.root:*engine-document*))
