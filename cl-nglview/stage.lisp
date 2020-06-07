(in-package :nglview)

(defclass stage ()
  ((%view
     :accessor %view
     :initarg :%view)))

(defmethod set-parameters ((stage stage) kwargs)
  (%remote-call (view stage) "setParameters"
		:target "Stage"
		:kwargs (camelize-dict kwargs)))

