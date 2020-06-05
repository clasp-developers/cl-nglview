(in-package :nglv)


; p:Structure
(defclass structure ()
  ((ext
     :accessor ext
     :initarg :ext
     :initform "pdb")
   (params
     :accessor params
     :initform nil
     :type list)
   (id
     :accessor id
     :initform (jupyter:make-uuid))))

; p:get_structure_string
(defgeneric get-structure-string (instance)
  (:documentation "Get the structure string assocated with the instance"))


; p:Trajectory
(defclass trajectory ()
  ((id
     :accessor id
     :initform (jupyter:make-uuid))
   (shown
     :accessor shown
     :initform t
     :type bool)))

; p:get_coordinates
(defgeneric get-coordinates (instance index)
  (:documentation "Get the coordinates assocated with the instance"))

; p:n_frames
(defgeneric n-frames (instance)
  (:documentation "Get the number of frames assocated with the instance"))

