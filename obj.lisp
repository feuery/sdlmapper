(defpackage #:qmapper.obj
  (:use #:cl
	#:cl-arrows))

(in-package #:qmapper.obj)

(defclass sprite ()
  ((texture :accessor sprite-texture)
   (entity :accessor sprite-entity)
   (projection :accessor sprite-projection)))

(export 'sprite)
(export 'sprite-entity)
(export 'sprite-texture)
(export 'sprite-projection)

(defun surface->texture (surface)
  (make-instance 'clinch:texture
		   :data (sdl2:surface-pixels surface)
		   :width  (sdl2:surface-width surface)
		   :height (sdl2:surface-height surface)
		   :stride 4
		   :format :rgb
		   :qtype  :unsigned-char))

(defmethod initialize-instance :after ((new-sprite sprite) &key texture-path)
  (let* ((sdl-surface (sdl2-image:load-image texture-path))
	 (texture (surface->texture sdl-surface)) ;; (clinch::make-texture-from-file texture-path))
	 (w (clinch:width texture))
	 (h (clinch:height texture)))
    (setf *surface* sdl-surface)
    (setf (sprite-texture new-sprite) texture)
    (setf (sprite-entity new-sprite) (make-instance 'clinch:entity
						    :shader-program (clinch:get-generic-single-texture-shader)
						    :indexes (make-instance 'clinch:index-buffer :data '(0 1 2
													 0 2 3))       ;; Add the index buffer
						    :attributes   `(("v" . ,(make-instance 'clinch:buffer 
											   :Stride 3
											   :data (map 'list (lambda (x)
													      (coerce x 'single-float))
												      (list (- w)   h 0.0
													    (- w)  (- h) 0.0
													    w  (- h) 0.0
													    w   h 0.0))))
								    ("tc1" . ,(make-instance 'clinch:buffer 
											     :Stride 2
											     :data (map 'list (lambda (x)
														(coerce x 'single-float))
													'(0.0   1.0
													  0.0   0.0
													  1.0   0.0
													  1.0   1.0)))))
						    :uniforms   `(("M". :model)
								  ("P" . :projection))))
    (setf (clinch:uniform (sprite-entity new-sprite) "t1") texture)
    (setf (clinch:uniform (sprite-entity new-sprite) "ambientTexture") texture)
    new-sprite))

(defgeneric render (entity))

(defmethod render ((entity sprite))
  (clinch:render (sprite-entity entity ) :projection clinch:*ortho-projection*))

(export 'render)
