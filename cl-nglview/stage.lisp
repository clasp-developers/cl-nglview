(in-package :nglv)

(jupyter:inform :info nil "stage.lisp")

(defclass stage ()
  ((%view :initarg :view
	  :accessor view)))

(defmethod set-parameters ((stage stage) kwargs)
  (%remote-call (view stage) "setParameters"
		:target "Stage"
		:kwargs (camelize-dict kwargs)))


  
(jupyter:inform :info nil "end of stage.lisp")
