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
     :initform nil)
   (surface-layout
     :accessor surface-layout
     :initform (make-instance 'jupyter-widgets:layout)))
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
  (update-representation (%view object) (component-index object)
			 (repr-index object) &rest (parameters object))
  (values))

; p:_on_name_changed
(defmethod on-trait-change ((instance representation-control) (name (eql :name)) type old-value new-value source)
  (declare (ignore name type old-value source))
  (setf (jupyter-widgets:display (surface-layout instance))
        (if (equal "surface" new-value)
          "flex"
          "none")))

; p:_on_repr_index_changed
(defmethod on-trait-change ((instance representation-control) (name (eql :repr-index)) type old-value new-value source)
  (declare (ignore name type old-value new-value source))
  (%update instance))

; p:_on_component_index_changed
(defmethod on-trait-change ((instance representation-control) (name (eql :component-index)) type old-value new-value source)
  (declare (ignore name type old-value new-value source))
  (%update instance))

; p:_update
(defun %update (instance)
  (multiple-value-bind (name repr-dict)
                       (%get-name-and-repr-dict instance)
    (setf (name instance) name)))
  ;     (setf (jupyter-widgets:widget-value opacity-slider)
  ;           (jsown:val repr-dict "opacity")))))

(defun make-representation-toggle-button (instance name description value)
  (let ((widget (make-instance 'jupyter-widgets:toggle-button
                               :value value
                               :description description
                               :style (make-instance 'jupyter-widgets:description-style
                                                      :description-width +default-text-width+))))
    (jupyter-widgets:observe
      widget :value
      (lambda (inst nm type old-value new-value source)
        (declare (ignore inst nm type old-value source))
        (update-representation (%view instance) (component-index instance) (repr-index instance)
                               name new-value)))
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

(defun make-representation-label-dropdown (instance name description value option-labels &optional names)
  (let ((widget (make-instance 'jupyter-widgets:dropdown
                               :index (position value option-labels :test #'string=)
                               :%options-labels option-labels :description description
                               :layout (make-instance 'jupyter-widgets:layout
                                                      :width +default-slider-width+)
                               :style (make-instance 'jupyter-widgets:slider-style
                                                      :description-width +default-text-width+))))
    (when names
      (jupyter-widgets:observe instance :name
        (lambda (inst nm type old-value new-value source)
          (declare (ignore inst nm type old-value source))
          (setf (jupyter-widgets:widget-disabled widget)
                (not (member new-value names :test #'equal))))))

    (jupyter-widgets:observe
      widget :value
      (lambda (inst nm type old-value new-value source)
        (declare (ignore inst nm type old-value source))
        (update-representation (%view instance) (component-index instance) (repr-index instance)
                               name new-value)))
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

(defun make-representation-slider (instance name description value min max inc &optional names)
  (let ((widget (make-instance (if (floatp inc) 'jupyter-widgets:float-slider 'jupyter-widgets:int-slider)
                               :value value
                               :min min :max max :step inc :description description
                               :layout (make-instance 'jupyter-widgets:layout
                                                      :width +default-slider-width+)
                               :style (make-instance 'jupyter-widgets:slider-style
                                                      :description-width +default-text-width+))))
    (when names
      (jupyter-widgets:observe instance :name
        (lambda (inst nm type old-value new-value source)
          (declare (ignore inst nm type old-value source))
          (setf (jupyter-widgets:widget-disabled widget)
                (not (member new-value names :test #'equal))))))

    (jupyter-widgets:observe
      widget :value
      (lambda (inst nm type old-value new-value source)
        (declare (ignore inst nm type old-value source))
        (update-representation (%view instance) (component-index instance) (repr-index instance)
                               name new-value)))

    (jupyter-widgets:observe
      instance '(:repr-index :component-index)
      (lambda (inst type nm old-value new-value source)
        (declare (ignore inst type nm old-value source))
        (jupyter:inform :info inst "change ~A ~A" name new-value)
        (multiple-value-bind (n repr-dict)
                             (%get-name-and-repr-dict instance)
          (setf (jupyter-widgets:widget-value widget)
                (jsown:val repr-dict name)))))

    widget))


; p:_make_widget
(defmethod initialize-instance :after ((instance representation-control) &rest initargs)
  (declare (ignore initargs))
  ; (with-slots (opacity-slider)
  ;             instance
  ;   (setf (jupyter-widgets:widget-children instance)
  ;         (list opacity-slider))
  ;   (jupyter-widgets:observe
  ;     opacity-slider :value
  ;     (lambda (inst name type old-value new-value source)
  ;       (declare (ignore inst name type old-value source))
  ;       (update-representation (%view instance) (component-index instance) (repr-index instance)
  ;                              :opacity new-value)))))
  ; (with-slots (component-index repr-index) instance
  ;   (let ((c-string (concatenate 'string "c" (write-to-string (component-index instance))))
  ;         (r-string (write-to-string (repr-index instance))))
      (setf (jupyter-widgets:widget-children instance)
            (list (make-representation-slider instance :opacity "Opacity" 1 0 1 0.1)
                  (make-representation-label-dropdown instance :color-scheme "Color Scheme" " " +color-schemes+)
                  (make-representation-label-dropdown instance :assembly "Assembly" "default" +assembly-list+)
                  (make-representation-slider instance :probe-radius "Probe Radius" 1.4 0 5 0.1 '("surface"))
                  (make-representation-slider instance :isolevel "Isolevel" 2 0 10 0.1)
                  (make-representation-slider instance :smooth "Opacity" 2 0 10 1 '("surface"))
                  (make-representation-label-dropdown instance :surface-types "Surface Types" "smooth" +surface-types+ '("surface"))
                  (make-representation-slider instance :box-size "Box Size" 10 0 100 2 '("surface"))
                  (make-representation-slider instance :cutoff "Cutoff" 0 0 100 0.1)
                  (make-representation-toggle-button instance :wireframe "Wireframe" nil))))

; p:_get_name_and_repr_dict
(defun %get-name-and-repr-dict (instance)
  (handler-case
      (let ((dict (jsown:val (jsown:val (%ngl-repr-dict (%view instance))
                                        (format nil "c~A" (component-index instance)))
                             (write-string (repr-index instance)))))
        (values (jsown:val dict "name")
                (jsown:val dict "parameters")))
    (error () (values '(:obj) '(:obj)))))

