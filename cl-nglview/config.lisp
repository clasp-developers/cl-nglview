(in-package :nglv)

(defparameter +camera-types+ '("perspective" "orthographic"))


(defparameter *BACKENDS* (make-hash-table))
(jupyter:inform :info nil "widget-log config.lisp")

