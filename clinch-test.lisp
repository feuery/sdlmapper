;;;; clinch-test.lisp

(in-package #:clinch-test)

;; tutoriaali 3 pohjalta tää pitäne tehdä
;; is working file as I test features...please don't use. Use tutorial05 instead.

;; The projection matrix. 
(defparameter *projection* nil)

;; The triangle entity which connects the shader, vertexes and projection matrix.
(defparameter *triangle* nil)

;; The shader program. A gpu side program which writes to the screen.
;; (defparameter *shader-program* nil)


;; (defun make-shader-program ()
;;   ;; A normal shader program is made of two shaders, a vertex shader and a fragment shader.
;;   (let ((vert-source
;; 	       "
;; #version 130
;; // Vertex Shader Source Code

;; // A UNIFORM is single value which is passed to all programs in a run.
;; uniform mat4 P;

;; // An ATTRIBUTE is an array which gives one value for each vertex.
;; in vec3 v;
;; in vec3 colors;
;; // in vec3 textures;
;; out vec3 _colors;
;; // out vec3 _textures;

;;         void main() {
;;             gl_Position = P * vec4(v, 1);
;;             _colors = colors;
;; //            _textures = textures;
;;         }")
;; 	      (frag-source
;; 	       "
;; #version 130
;; // Fragment Shader Source Code

;; // fragment shader input
;; in vec3 _colors;
;; // in vec3 _textures;

;; // This returns the fragment's color.
;; out vec4 fragColor;
;;         void main() {

;;             // The triangle will just be white for now.
;;             fragColor = vec4(_colors, 1);
;;         }"))

;; 	  (make-instance 'clinch:shader-program
;; 			 :name "Shader01"
;; 			 :vertex-shader vert-source
;; 			 :fragment-shader frag-source
;; 			 :uniforms '(("P" :matrix))
;; 			 :attributes '(("v" :float)
;; 				       ("colors" :float)))))


(clinch:defevent clinch:*on-window-resized* (win width height ts)
  ;; redo the projection matrix here
  (format t "Window Resized: ~A ~A~%" width height))


(defparameter *texture* nil)

;; Run this once before the next on-idle call.
(clinch:defevent clinch:*next* ()
  (gl:clear-color 0  0 1 0)
  (gl:enable :blend :depth-test :line-smooth :point-smooth :texture-2d :cull-face)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)


  ;; Make the shader-program
  ;; (setf *shader-program* (make-shader-program))
  
  ;; create the triangle entity. 
  (setf *texture* 
	(clinch::make-texture-from-file 
	 "/home/feuer/Sync/qt-test/kaunis_tileset.jpeg"))
  (let ((w (clinch:width *texture*))
	(h (clinch:height *texture*)))

    (setf *triangle*
	  (make-instance 'clinch:entity
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
				       ("P" . :projection)
				       ;;("t1" . :place-holder)
				       ))))

  
  
  (setf (clinch:uniform *triangle* "t1") *texture*)
  (setf (clinch:uniform *triangle* "ambientTexture") *texture*))

;; Create an on-idle envent handler.
(clinch:defevent clinch:*on-idle* ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (clinch:render *triangle* :projection clinch:*ortho-projection*))

;; Start the window.
(clinch:init :init-controllers nil)
