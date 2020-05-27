(in-package :nglv)

(defparameter +assembly-list+ '("default" "AU" "BU1" "UNITCELL" "SUPERCELL"))

(defparameter +surface-types+ '("vws" "sas" "ms" "ses"))


(defclass representation-control (vbox)
  ((parameters
     :accessor parameters
     :initform ()
     :type list
     :trait :list)
   (name
     :accessor name
     :initarg :name
     :initform nil
     :trait :unicode)
   (repr-index
     :accessor repr-index
     :initarg :repr-index
     :initform 0
     :type integer
     :trait :int)
   (component-index
     :accessor component-index
     :initarg :component-index
     :initform 0
     :type integer
     :trait :int)
   (%disable-update-parameters
     :accessor %disabled-update-parameters
     :initform nil
     :type bool)
   (%view
     :accessor %view
     :initarg :%view
     :initform nil))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:default-initargs
    :layout (make-instance 'jupyter-widgets:layout :width "auto" :flex-flow "row wrap")))

;Not an observer
(defmethod _on_change_widget_child_value ((self representation-control) change)
  (let ((owner (aref change "owner"))
	(new (aref change "new")))
    (setf (parameters self) (list (cons (camelize (ngl-description owner)) new))))
  (values))

;Observer for parameters
(defmethod %on-parameters-changed (object name new old)
  (unless (%disabled-update-parameters object)
      (setf (parameters object)  new))
  (update-representation (view object) :component (component-index object)
			 :repr-index (repr-index object) &rest (parameters object))
  (values))

; p:_on_name_changed
(defmethod on-trait-change ((instance representation) (name (eql :name)) type old-value new-value source)
  (declare (ignore type old-value source))
  (let ((new-name new))
    (if (string= new-name "surface")
	(loop for kid across (children object)
	   do
	     (when (string= (%ngl-type kid) "surface")
	       (setf (display (layout kid)) "flex")))
	(loop for kid across (children object)
	   do
	     (if (string= (%ngl-type kid) "surface")
		 (setf (display (layout kid)) "none")))))
  (values))

;Observer for repr-index
(defmethod %on-repr-index-changed (object name new old)
  (let ((c-string (concatenate 'string "c" (write-to-string (component-index object))))
	(r-string (write-to-string new)))
    (%update object c-string r-string)))

;Observer for component-index
(defmethod %on-component-index-changed (object name new old)
  (let ((c-string (concatenate 'string "c" (write-to-string new)))
	(r-string (write-to-string (repr-index object))))
    (%update self c-string r-string)))

(defmethod %update ((self representation-control) c-string r-string)
  (multiple-value-bind (name %repr-dict)
      (%get-name-and-repr-dict self c-string r-string)
    (setf (name self) name (%disable-update-parameters self) t)
    (loop for kid across (children self)
       do
	 (setf desc (camelize (ngl-description kid))))
    (error "Implement me!1 %update from representation.lisp")
    (setf (%disable-update-parameters self) nil)))
#|
    def _update(self, c_string, r_string):
        name, _repr_dict = self._get_name_and_repr_dict(c_string, r_string)
        self.name = name
        self._disable_update_parameters = True
        for kid in self.children:
            desc = py_utils._camelize(kid._ngl_description)
            if desc in _repr_dict:
                kid.value = _repr_dict.get(desc)
        self._disable_update_parameters = False
|#

(defun make-representation-toggle-button (instance name description value)
  (let ((widget (make-instance 'jupyter-widgets:toggle-button
                               :value value
                               :description description
                               :style (make-instance 'jupyter-widgets:description-style
                                                      :description-width +default-text-width+))))
    ; (jupyter-widgets:observe widget :value
    ;   (lambda (widget type nm old-value new-value source)
    ;     (declare (ignore widget type nm old-value source))
    ;     (setf (parameters (%view instance)) (list name (if new-value :true :false)))))
    ; (jupyter-widgets:observe (%view instance) :%ngl-full-stage-parameters
    ;   (lambda (view type nm old-value new-value source)
    ;     (declare (ignore view type nm old-value source))
    ;     (setf (jupyter-widgets:widget-value widget)
    ;       (getf new-value name (jupyter-widgets:widget-value widget)))))
    widget))

(defun make-representation-label-slider (instance name description value option-labels)
  (let ((widget (make-instance 'jupyter-widgets:selection-slider
                               :index (position value option-labels :test #'string=)
                               :%options-labels option-labels :description description
                               :layout (make-instance 'jupyter-widgets:layout
                                                      :width +default-slider-width+)
                               :style (make-instance 'jupyter-widgets:slider-style
                                                      :description-width +default-text-width+))))
    ; (jupyter-widgets:observe widget :index
    ;   (lambda (widget type nm old-value new-value source)
    ;     (declare (ignore widget type nm old-value source))
    ;     (setf (parameters (%view instance)) (list name (nth new-value option-labels)))))
    ; (jupyter-widgets:observe (%view instance) :%ngl-full-stage-parameters
    ;   (lambda (view type nm old-value new-value source)
    ;     (declare (ignore view type nm old-value source))
    ;     (setf (jupyter-widgets:widget-index widget)
    ;       (position (getf (%ngl-full-stage-parameters (%view instance)) name)
    ;                 option-labels :test #'string=))))
    widget))

(defun make-representation-label-dropdown (instance name description value option-labels)
  (let ((widget (make-instance 'jupyter-widgets:dropdown
                               :index (position value option-labels :test #'string=)
                               :%options-labels option-labels :description description
                               :layout (make-instance 'jupyter-widgets:layout
                                                      :width +default-slider-width+)
                               :style (make-instance 'jupyter-widgets:slider-style
                                                      :description-width +default-text-width+))))
    ; (jupyter-widgets:observe widget :index
    ;   (lambda (widget type nm old-value new-value source)
    ;     (declare (ignore widget type nm old-value source))
    ;     (setf (parameters (%view instance)) (list name (nth new-value option-labels)))))
    ; (jupyter-widgets:observe (%view instance) :%ngl-full-stage-parameters
    ;   (lambda (view type nm old-value new-value source)
    ;     (declare (ignore view type nm old-value source))
    ;     (setf (jupyter-widgets:widget-index widget)
    ;       (position (getf (%ngl-full-stage-parameters (%view instance)) name)
    ;                 option-labels :test #'string=))))
    widget))

(defun make-representation-slider (instance name description value min max inc)
  (let ((widget (make-instance (if (floatp inc) 'jupyter-widgets:float-slider 'jupyter-widgets:int-slider)
                               :value value
                               :min min :max max :step inc :description description
                               :layout (make-instance 'jupyter-widgets:layout
                                                      :width +default-slider-width+)
                               :style (make-instance 'jupyter-widgets:slider-style
                                                      :description-width +default-text-width+))))
    ; (jupyter-widgets:observe widget :value
    ;   (lambda (widget type nm old-value new-value source)
    ;     (declare (ignore widget type nm old-value source))
    ;     (setf (parameters (%view instance)) (list name new-value))))
    ; (jupyter-widgets:observe (%view instance) :%ngl-full-stage-parameters
    ;   (lambda (view type nm old-value new-value source)
    ;     (declare (ignore view type nm old-value source))
    ;     (setf (jupyter-widgets:widget-value widget)
    ;       (getf new-value name (jupyter-widgets:widget-value widget)))))
    widget))


; p:_make_widget
(defmethod initialize-instance :after ((instance representation-control) &rest initargs)
  (declare (ignore initargs))
  (with-slots (component-index repr-index) instance
    (let ((c-string (concatenate 'string "c" (write-to-string (component-index instance))))
          (r-string (write-to-string (repr-index instance))))
      (setf (jupyter-widgets:widget-children instance)
            (list (make-representation-slider instance :opacity "Opacity" 1 0 1 0.1)
                  (make-representation-label-dropdown instance :color-scheme "Color Scheme" " " +color-schemes+)
                  (make-representation-label-dropdown instance :assembly "Assembly" "default" +assembly-list+)
                  (make-representation-slider instance :probe-radius "Probe Radius" 1.4 0 5 0.1)
                  (make-representation-slider instance :isolevel "Isolevel" 2 0 10 0.1)
                  (make-representation-slider instance :smooth "Opacity" 2 0 10 1)
                  (make-representation-label-dropdown instance :surface-types "Surface Types" "smooth" +surface-types+)
                  (make-representation-slider instance :box-size "Box Size" 10 0 100 2)
                  (make-representation-slider instance :cutoff "Cutoff" 0 0 100 0.1)
                  (make-representation-toggle-button instance :wireframe "Wireframe" nil))))))

(defmethod %get-name-and-repr-dict ((self representation-control) c-string r-string)
  (flet ((read-dict (key dict)
	   (multiple-value-bind (value present-p)
	       (gethash key dict)
	     (if present-p
		 value
		 (return-from %get-name-and-repr-dict
		   (values "" (make-hash-table)))))))
    (let* ((repr-dict (%repr-dict (%view self)))
	   (inner (read-dict r-string (read-dict c-string %repr-dict))))
      (values (read-dict "name" inner)
	      (read-dict "parameters" inner)))))
