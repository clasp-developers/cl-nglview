(in-package :nglv)

(jupyter:inform :info nil "base_adaptor.lisp")

(defclass Structure ()
  ((ext :accessor ext :initform "pdb")
   (params :accessor params :initform nil)
   (id :accessor id :initform (jupyter:make-uuid))))

(defmethod get-structure-string((Structure Structure))
  (error "If you are getting this error, it's because you made an instance of the Structure parent class. Please be more specific and create an instnace of a structure child class. Thanks!"))

(defclass Trajectory ()
  ((id :accessor id :initform (jupyter:make-uuid))
   (shown :accessor shown :type bool :initform t)))

(defmethod get-coordinates ((Trajectory Trajectory) index)
  (error "Error in get-coordinates: Not Implemented Error!!! Python code not implemented"))

(defmethod n-frames ((Trajectory Trajectory))
  (error "Error in n-frames: Not Implemented Error!!! Python code not implemented"))
