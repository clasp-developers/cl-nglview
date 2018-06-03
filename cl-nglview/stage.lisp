(in-package :nglv)

(cl-jupyter:logg 2 "stage.lisp~%")

(defclass stage ()
  ((%view :initarg :view
	  :accessor view)))

(defmethod set-parameters ((stage stage) kwargs)
  (%remote-call (view stage) "setParameters"
		:target "Stage"
		:kwargs (camelize-dict kwargs)))


  
(cl-jupyter:logg 2 "end of stage.lisp~%")
