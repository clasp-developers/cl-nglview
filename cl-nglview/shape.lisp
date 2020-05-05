(in-package :nglv)

(jupyter:inform :info nil "in shape.lisp~%")

(defclass shape ()
  ((view :initarg :view :accessor view :initform nil)
   (names :accessor names :type list :initform (list "mesh" "sphere" "ellipsoid" "cylinder" "cone" "arrow" "label" "text"))))

;;I would like to imitate shape.py::%-init--
(defmethod initialize-instance :after ((self shape) &key)
  (%make-func self (names self)))



(defmethod %make-func ((self shape) names)
  (flet ((make-func (name)
	   (lambda (this &rest args)
	     "check `add` method"
	     (apply #'add this name args)))
	 )
    (dolist (name names)
      (let* ((func-name (concatenate 'string "add_" name))
	     (func (make-func name)))))))
	

(defmethod add ((self shape) &rest args)
  (%add-shape (view self) args))

(defmethod add-buffer ((self shape) name &rest kwargs &key &allow-other-keys)
  (%remote-call (view self) "addBuffer"
		:target "Widget"
		:args name
		:kwargs kwargs))


(defun add-axes (widget)
  (let ((x-arrow (vector "arrow" #(0 0 0) #(10 0 0) #(1 0 0) 1.0))
        (y-arrow (vector "arrow" #(0 0 0) #(0 10 0) #(0 1 0) 1.0))
        (z-arrow (vector "arrow" #(0 0 0) #(0 0 10) #(0 0 1) 1.0)))
    (add-shape widget (vector x-arrow y-arrow z-arrow) :name "axes")))
