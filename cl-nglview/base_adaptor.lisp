(in-package :nglview)

(defclass component ()
  ((id
     :accessor id
     :initform (jupyter:make-uuid))
   (name
     :accessor name
     :initform "")))

(defmethod id ((value string))
  value)


; p:Structure
(defclass structure (component)
  ((ext
     :accessor ext
     :initarg :ext
     :initform "pdb")
   (params
     :accessor params
     :initform nil
     :type list)))

; p:get_structure_string
(defgeneric get-structure-string (instance)
  (:documentation "Get the structure string assocated with the instance"))


; p:Trajectory
(defclass trajectory (component)
  ((shown
     :accessor shown
     :initform t
     :type bool)))

; p:get_coordinates
(defgeneric get-coordinates (instance index)
  (:documentation "Get the coordinates assocated with the instance"))

; p:n_frames
(defgeneric n-frames (instance)
  (:documentation "Get the number of frames assocated with the instance"))

