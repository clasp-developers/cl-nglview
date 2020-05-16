(in-package :nglv)

(defun strip (string)
  (string-trim #(#\Space #\Newline #\Return) string))

(defun javascript-true-p (v)
  (when (eq v t)
    t)
  nil)

(defclass trajectory-player (jupyter-widgets:has-traits)
  ((%step
     :accessor %step
     :initarg :step
     :initform 1 ;Original default is 0 but init makes it 1
     :type integer
     :trait :int)
   (sync-frame
     :accessor sync-frame
     :initarg :sync-frame
     :initform nil ;Original default is true but init makes it false
     :type boolean
     :trait :bool)
   (interpolate
     :accessor interpolate
     :initform nil
     :type bool
     :trait :bool)
   (delay
     :accessor delay
     :initarg :delay
     :initform 100.0 ;Original default is 0.0 but init makes it 100
     :type float
     :trait :float)
   (parameters
     :accessor parameters
     :initform nil
     :trait :dict)
   (iparams
     :accessor iparams
     :initform nil
     :trait :dict)
   (%interpolation-t
     :accessor %interpolation-t
     :initform 0.5 ;Original default is nil but init makes it 0.5
     :type float
     :trait :float)
   (%iterpolation-type
     :accessor %iterpolation-type
     :initform "linear" ;Original default is "" but init makes it "linear"
     :trait :unicode) ; TODO: Add validator for enum ("linear" "spline")
   (spin
     :accessor spin
     :initform nil
     :trait :bool)
   (%spin-x
     :accessor %spin-x
     :initform 1
     :type integer
     :trait :int)
   (%spin-y
     :accessor %spin-y
     :initform 0
     :type integer
     :trait :int)
   (%spin-z
     :accessor %spin-z
     :initform 0
     :type integer
     :trait :int)
   (%spin-speed
     :accessor %spin-speed
     :initform 0.005
     :type float
     :trait :float)
   (camera
     :accessor camera
     :initform "perspective"
     :trait :unicode) ; TODO: Add validator for enum ("perspective" "orthographic")
   (%render-params
     :accessor %render-params
     :initform nil
     :trait :dict)
   (%real-time-update
     :accessor %real-time-update
     :initform nil
     :trait :bool)
   (widget-tab
     :accessor widget-tab
     :initform nil
     :trait :widget)
   (widget-repr
     :accessor widget-repr
     :initform nil
     :trait :widget)
   (widget-repr-parameters
     :accessor widget-repr-parameters
     :initform nil
     :trait :dict)
   (widget-quick-repr
     :accessor widget-quick-repr
     :initform nil
     :trait :widget)
   (widget-general
     :accessor widget-general
     :initform nil
     :trait :widget)
   (widget-picked
     :accessor widget-picked
     :initform nil
     :trait :widget)
   (widget-preference
     :accessor widget-preference
     :initform nil
     :trait :widget)
   (widget-extra
     :accessor widget-extra
     :initform nil
     :trait :widget)
   (widget-help
     :accessor widget-help
     :initform nil
     :trait :widget)
   (widget-export-image
     :accessor widget-export-image
     :initform nil
     :trait :widget)
   (widget-component-slider
     :accessor widget-component-slider
     :initform nil
     :trait :widget)
   (widget-repr-slider
     :accessor widget-repr-slider
     :initform nil
     :trait :widget)
   (widget-repr-choices
     :accessor widget-choices
     :initform nil
     :trait :widget)
   (widget-repr-control-buttons
     :accessor widget-repr-control-buttons
     :initform nil
     :trait :widget)
   (widget-repr-add
     :accessor widget-repr-add
     :initform nil
     :trait :widget)
   (widget-accordion-repr-parameters
     :accessor widget-accordion-repr-parameters
     :initform nil
     :trait :dict)
   (widget-repr-parameters-dialog
     :accessor widget-repr-parameters-dialog
     :initform nil
     :trait :widget)
   (widget-repr-name
     :accessor widget-repr-name
     :initform nil
     :trait :widget)
   (widget-component-dropdown
     :accessor widget-component-dropdown
     :initform nil
     :trait :widget)
   (%view
     :accessor %view
     :initarg :%view
     :initform nil)
   (min-delay
     :accessor min-delay
     :initarg :min-delay
     :initform 40
     :type integer)
   (%widget-names
     :accessor widget-names
     :initarg :widget-names
     :type list
     :initform nil))
  (:metaclass jupyter-widgets:trait-metaclass))

(defmethod initialize-instance :after ((player trajectory-player) &key)
  ; The python code saves all widget field names into %widget-names then doesn't use that
  (setf (iparams player) (list (cons "t" (%interpolation-t player))
                               (cons "step" 1)
                               (cons "type" (%iterpolation-type player)))
        (%render-params player) (list (cons "factor" 4)
                                      (cons "antialias" t)
                                      (cons "trim" nil)
                                      (cons "transparent" nil)))
  player)

(defmethod %update-padding ((self trajectory-player) &optional (padding *DEFAULT-PADDING*))
  (with-slots (widget-general widget-repr widget-preference widget-repr-parameters widget-help widget-extra wiget-picked) self
    (let ((widget-collection (list widget-general widget-repr widget-preference widget-repr-parameters widget-help widget-extra widget-picked)))
      (dolist (widget widget-collection)
        (when widget
         (setf (padding (layout widget)) padding))))))

(defmethod %create-all-widgets ((self trajectory-player))
  (setf (widget-tab self) (%display self))
  (let ((old-index (jupyter-widgets:widget-selected-index (widget-tab self)))
        (new-index 0))
    (loop for (index) across (jupyter-widgets:widget-children (widget-tab self))
       do
         (setf (jupyter-widgets:widget-selected-index (widget-tab self)) new-index)
         (incf new-index))
    (setf (jupyter-widgets:widget-selected-index (widget-tab self)) old-index)))

(defmethod smooth ((self trajectory-player))
  (setf (interpolate self) t))

(defmethod jupyter-widgets:on-trait-change ((object trajectory-player) type (name (eql :camera)) old new)
  (let ((camera-type new))
    (when (slot-boundp object '%view)
      (%remote-call (%view object) "setParameters" :target "Stage" :kwargs (list (cons "cameraType" camera-type))))))

(defmethod frame ((self trajectory-player))
  (frame (%view self)))

(defmethod (setf frame) (value (self trajectory-player))
  (setf (frame (%view self)) value))

; p:update_sync_frame
(defmethod jupyter-widgets:on-trait-change ((object trajectory-player) type (name (eql :sync-frame)) old new)
  (declare (ignore type name))
  (when (slot-boundp object '%view)
    (if new
        (%set-sync-frame (%view object))
        (%set-unsync-frame (%view object)))))

; p:_update_delay
(defmethod jupyter-widgets:on-trait-change ((object trajectory-player) type (name (eql :delay)) old new)
  (when (slot-boundp object '%view)
    (%set-delay (%view object) new)))

; p:update_parameters
(defmethod jupyter-widgets:on-trait-change ((object trajectory-player) type (name (eql :parameters)) old new)
  (when (slot-boundp object 'sync-frame)
    (setf (sync-frame object) (get new "sync_frame" (sync-frame object))))
  (when (slot-boundp object 'delay)
    (setf (delay object) (get new "delay" (delay object))))
  (when (slot-boundp object '%step)
    (setf (%step object) (get new "step" (%step object)))))

; p:_interpolation_t_changed
(defmethod jupyter-widgets:on-trait-change ((object trajectory-player) type (name (eql :%interpolation-t)) old new)
  (let ((entry (jupyter:json-getf (iparams object) "t")))
    (if entry
        (setf (cdr entry) new)
        (setf (iparams object) (cons "t" new)))))

; p:_on_spin_changed
(defmethod jupyter-widgets:on-trait-change ((object trajectory-player) type (name (eql :spin)) old new)
  (when (slot-boundp object '%view)
    (if (javascript-true-p (spin object))
        (%set-spin (%view object) (list (%spin-x object) (%spin-y object) (%spin-z object)) (%spin-speed object))
        (%set-spin (%view object) nil nil))))

; p:_on_spin_x_changed
(defmethod jupyter-widgets:on-trait-change ((object trajectory-player) type (name (eql :%spin-x)) old new)
  (declare (ignore type name))
  (when (slot-boundp object '%view)
    (if (javascript-true-p (spin object))
        (%set-spin (%view object) (list (%spin-x object) (%spin-y object) (%spin-z object)) (%spin-speed object)))))

; p:_on_spin_y_changed
(defmethod jupyter-widgets:on-trait-change ((object trajectory-player) type (name (eql :%spin-y)) old new)
  (declare (ignore type name))
  (when (slot-boundp object '%view)
    (if (javascript-true-p (spin object))
        (%set-spin (%view object) (list (%spin-x object) (%spin-y object) (%spin-z object)) (%spin-speed object)))))

; p:_on_spin_z_changed
(defmethod jupyter-widgets:on-trait-change ((self trajectory-player) type (name (eql :%spin-z)) old new)
  (declare (ignore type name))
  (when (slot-boundp self '%view)
    (if (javascript-true-p (spin self))
        (%set-spin (%view self) (list (%spin-x self) (%spin-y self) (%spin-z self)) (%spin-speed self)))))

; p:_on_spin_speed_changed
(defmethod jupyter-widgets:on-trait-change ((self trajectory-player) type (name (eql :%spin-speed)) old new)
  (declare (ignore type name))
  (if (javascript-true-p (spin self))
      (%set-spin (%view self) (list (%spin-x self) (%spin-y self) (%spin-z self)) (%spin-speed self))))

; p:_display
(defun %display (self)
  (jupyter:inform :info nil "trajectory-player %display")
  (with-slots (widget-tab) self
    (unless widget-tab
      (setf widget-tab
            (make-instance 'jupyter-widgets:tab
            :children (list (%make-general-box self)
                            ;(%make-widget-repr self)
                            (%make-widget-preference self))
                            ;(%make-extra-box self))
            :%titles '("General"
            ;"Representation"
            "Preference")
            ;"Extra")
                             :selected-index 0))
          (jupyter:inform :info nil "trajectory-player %display exit")
          (setf
            (jupyter-widgets:widget-align-self (jupyter-widgets:widget-layout widget-tab)) "center")
            (jupyter:inform :info nil "trajectory-player %display exit")
          (setf
            (jupyter-widgets:widget-align-items (jupyter-widgets:widget-layout widget-tab)) "stretch"))
    (jupyter:inform :info nil "trajectory-player %display exit")
    widget-tab))

; p:_make_widget_tab
(defun %make-widget-tab (self)
  (%display self))

; p:_make_button_center
(defun %make-button-center (self)
  (make-instance 'jupyter-widgets:button
                 :description " Center"
                 :icon "fa-bullseye"
                 :on-click (lambda (button)
                             (declare (ignore button))
                             (center (%view self)))))

(defun make-preference-label-slider (instance name description value option-labels)
  (let ((widget (make-instance 'jupyter-widgets:selection-slider
                               :index (position (getf (ngl-full-stage-parameters (%view instance)) name value) option-labels)
                               :%options-labels option-labels :description description
                               :layout (make-instance 'jupyter-widgets:layout
                                                      :width +default-slider-width+)
                               :style (make-instance 'jupyter-widgets:slider-style
                                                      :description-width +default-text-width+))))
    (jupyter-widgets:observe widget :value
      (lambda (widget type nm old-value new-value)
        (declare (ignore widget type nm old-value))
        (setf (parameters (%view instance)) (list name (nth new-value option-labels)))))
    widget))

(defun make-preference-slider (instance name description value min max inc)
  (let ((widget (make-instance (if (floatp inc) 'jupyter-widgets:float-slider 'jupyter-widgets:int-slider)
                               :value (getf (ngl-full-stage-parameters (%view instance)) name value)
                               :min min :max max :step inc :description description
                               :layout (make-instance 'jupyter-widgets:layout
                                                      :width +default-slider-width+)
                               :style (make-instance 'jupyter-widgets:slider-style
                                                      :description-width +default-text-width+))))
    (jupyter-widgets:observe widget :value
      (lambda (widget type nm old-value new-value)
        (declare (ignore widget type nm old-value))
        (setf (parameters (%view instance)) (list name new-value))))
    widget))

; p:_make_widget_preference
(defun %make-widget-preference (self &key (width "100%"))
  (with-slots (widget-preference) self
    (setf widget-preference
          (make-instance 'jupyter-widgets:v-box
                         :children (list
                                     (make-preference-slider self :pan-speed "Pan Speed" 0.8 0 10 0.1)
                                     (make-preference-slider self :rotate-speed "Rotate Speed" 2 0 10 1)
                                     (make-preference-slider self :zoom-speed "Zoom Speed" 1.2 0 10 1)
                                     (make-preference-slider self :clip-dist "Clip Distance" 10 0 200 5)
                                     (make-preference-slider self :clip-far "Clip Far" 100 0 100 1)
                                     (make-preference-slider self :clip-near "Clip Near" 0 0 100 1)
                                     (make-preference-slider self :camera-fov "Camera FOV" 40 15 120 1)
                                     (make-preference-slider self :fog-far "Fog Far" 100 0 100 1)
                                     (make-preference-slider self :fog-near "Fog Near" 60 0 100 1)
                                     (make-preference-slider self :light-intensity "Light Intensity" 1 0 10 0.02)
                                     (make-preference-label-slider self :quality "Quality" "medium" '("low" "medium" "high"))
                                     (make-preference-slider self :sample-level "Sample Level" 1 -1 5 1))))
    widget-preference))

(defmethod %show-download-image ((self trajectory-player))
  (make-instance 'button
                 :description " Screenshot"
                 :icon "fa-camera"
                 :on-click (lambda (button)
                             (declare (ignore button))
                             (download-image (%view self)))))

(defmethod %make-button-url ((self trajectory-player) url description)
  (make-instance 'button
                 :description description
                 :on-click (lambda (button)
                             (declare (ignore button))
                             (display (jupyter:javascript (format +open-url-template+ url))))))

(defmethod %make-text-picked ((self trajectory-player))
  (let ((ta (Textarea :value (funcall dumps json (picked (%view self))) :description "Picked atom")))
    (setf (width (layout ta)) "300px")
    ta))

(defmethod %refresh ((self trajectory-player) component-slider repr-slideR)
  (%request-repr-parameters (%view self) :component (value component-slider) :repr-index (value repr-slider))
  (%update-repr-dict (%view self))
  (%handle-repr-dict-changed (%view self) :change (list (cons "new" (%repr-dict (%view self))))))


(defmethod %make-button-repr-control ((self trajectory-player) component-slider repr-slider repr-selection)
  (%make-autofit
    (make-instance 'jupyter-widgets:h-box
                   :children (list
                               ; refresh button
                               (make-instance 'button
                                              :description " Refresh"
                                              :tooltip "Get representation info"
                                              :icon "fa-refresh"
                                              :on-click (lambda (button)
                                                          (declare (ignore button))
                                                          (%refresh self component-slider repr-slider)))
                               ; Center button
                               (make-instance 'button
                                              :description " Center"
                                              :tooltip "center selected atoms"
                                              :icon "fa-bullseye"
                                              :%ngl-name "button-center-selection"
                                              :on-click (lambda (button)
                                                          (declare (ignore button))
                                                          (center (%view self)
                                                                  :selection (value repr-selection)
                                                                  :component (value component-slider))))
                               ; Hide/Show button
                               (make-instance 'button
                                              :description " Hide"
                                              :tooltip "Hide/Show current representation"
                                              :icon "fa-eye-slash"
                                              :on-click (lambda (button-hide)
                                                          (let ((component (value component-slider))
                                                                (repr-index (value repr-slider))
                                                                (hide nil))
                                                            (if (string= (description button-hide) "Hide")
                                                              (setf hide t
                                                                    (description button-hide) "Show")
                                                              (setf hide nil
                                                                    (description button-hide) "Hide"))
                                                            (%remote-call (%view self) "setVisibilityForRepr"
                                                                          :target "Widget"
                                                                          :args (list component repr-index (not hide))))))
                               ; Remove button
                               (make-instance 'button
                                              :description " Remove"
                                              :tooltip "Remove current representation"
                                              :icon "fa-trash"
                                              :on-click (lambda (button)
                                                          (declare (ignore button))
                                                          (%remove-representation (%view self)
                                                                                  :component (value component-slider)
                                                                                  :repr-index (value repr-slider))
                                                          (%request-repr-parameters (%view self)
                                                                                    :component (value component-slider)
                                                                                    :repr-index (value repr-slider))))
                               ; Representation Parameters button
                               (make-instance 'button
                                              :description " Dialog"
                                              :tooltip "Pop up representation parameters control dialog")))))

(defmethod %make-widget-repr ((self trajectory-player))
  (jupyter:inform :info nil "%make-widget-repr")
  (setf (widget-repr-name self) (make-instance 'text :value "" :description "representation")
	      (%ngl-name (widget-repr-name self)) "repr-name-text")
  (let ((repr-selection (make-instance 'text :value "" :description "selection")))
    (setf (%ngl-name repr-selection) "repr-selection"
	  (jupyter-widgets:widget-description-width repr-selection) +default-text-width+
	  (jupyter-widgets:widget-description-width (widget-repr-name self)) +default-text-width+)
    (let ((max-n-components (max (% (n-components (%view self)) 1) 0)))
      (setf (widget-component-slider self) (make-instance 'int-slider :value 0 :max max-n-components :min 0 :description "component")
	    (%ngl-name (widget-component-slider self)) "component-slider")
      (let ((cvalue " "))
	(setf (widget-component-dropdown self) (make-instance 'dropdown
							      :value cvalue
							      :options '((cvalue . nil))
							      :description "component")
	      (%ngl-name (widget-component-dropdown self)) "component_dropdown"
	      (widget-repr-slider self) (make-instance 'int-slider
						       :value 0
						       :description "representation"
						       :width +default-slider-width+))
	(setf (%ngl-name (widget-repr-slider self)) "repr_slider"
	      (visible (widget-repr-slider self)) t
	      (jupyter-widgets:widget-description-width (jupyter-widgets:widget-layout (widget-component-slider self))) +default-slider-width+
	      (jupyter-widgets:widget-description-width (jupyter-widgets:widget-layout (widget-repr-slider self))) +default-slider-width+
	      (jupyter-widgets:widget-description-width (jupyter-widgets:widget-layout (widget-component-dropdown self))) +default-text-width+
	      (max-width (widget-component-dropdown self)) +default-text-width+
	      (display (layout (widget-component-dropdown self))) "none"
	      (description (widget-component-dropdown self)) ""
	      (widget-accordion-repr-parameters self) (make-instance 'tab)
	      (widget-repr-parameters self) (%make-widget-repr-parameters self (widget-component-slider self) (widget-repr-slider self) (widget-repr-name self)))
	(setf (children (widget-accordion-repr-parameters self)) (list (widget-repr-parameters self) (make-instance 'box)))
	(set-title (widget-accordion-repr-parameters self) 0 "Parameters")
	(set-title (widget-accordion-repr-parameters self) 1 "Hide")
	(setf (selected-index (widget-accordion-repr-parameters self)) 1)
	(let ((checkbox-reprlist (make-instance 'checkbox
	:value nil
						:description "reprlist")))
	  (setf (%ngl-name checkbox-reprlist) "checkbox_reprlist"
		(widget-repr-choices self) (%make-repr-name-choices self (widget-component-slider self) (widget-repr-slider self)))
	  (setf (%ngl-name (widget-repr-choices self)) "reprlist_choices"
		(widget-repr-add self) (%make-add-widget-repr self (widget-component-slider self)))
	  (flet ((on-update-checkbox-reprlist (widget type name old new)
       (declare (ignore widget type name old))
		   (setf (visible (widget-repr-choices self)) new)
		 (values)))
	    (jupyter-widgets:observe checkbox-reprlist :value on-update-checkbox-reprlist)
	  (error "-make-widget-repr not finished!!")))))))
 #|
        def on_repr_name_text_value_changed(change):
            name = change['new'].strip()
            old = change['old'].strip()

            should_update = (self._real_time_update
                             and old and name
                             and name in REPRESENTATION_NAMES
                             and name != change['old'].strip())

            if should_update:
                component=self.widget_component_slider.value
                repr_index=self.widget_repr_slider.value
                self._view._remote_call('setRepresentation',
                                 target='Widget',
                                 args=[change['new'], {}, component, repr_index])
                self._view._request_repr_parameters(component, repr_index)

        def on_component_or_repr_slider_value_changed(change):
            self._view._request_repr_parameters(component=self.widget_component_slider.value,
                                                repr_index=self.widget_repr_slider.value)
            self.widget_component_dropdown.options = tuple(self._view._ngl_component_names)

            if self.widget_accordion_repr_parameters.selected_index >= 0:
                self.widget_repr_parameters.name = self.widget_repr_name.value
                self.widget_repr_parameters.repr_index = self.widget_repr_slider.value
                self.widget_repr_parameters.component_index = self.widget_component_slider.value

        def on_repr_selection_value_changed(change):
            if self._real_time_update:
                component = self.widget_component_slider.value
                repr_index = self.widget_repr_slider.value
                self._view._set_selection(change['new'],
                                          component=component,
                                          repr_index=repr_index)

        def on_change_component_dropdown(change):
            choice = change['new']
            if choice:
                 self.widget_component_slider.value = self._view._ngl_component_names.index(choice)

        self.widget_component_dropdown.observe(on_change_component_dropdown, names='value')

        self.widget_repr_slider.observe(on_component_or_repr_slider_value_changed, names='value')
        self.widget_component_slider.observe(on_component_or_repr_slider_value_changed, names='value')
        self.widget_repr_name.observe(on_repr_name_text_value_changed, names='value')
        repr_selection.observe(on_repr_selection_value_changed, names='value')

        self.widget_repr_control_buttons = self._make_button_repr_control(self.widget_component_slider,
        self.widget_repr_slider, repr_selection)

        blank_box = Box([Label("")])

        all_kids = [self.widget_repr_control_buttons,
                    blank_box,
                    self.widget_repr_add,
                    self.widget_component_dropdown,
                    self.widget_repr_name,
                    repr_selection,
                    self.widget_component_slider,
                    self.widget_repr_slider,
                    self.widget_repr_choices,
                    self.widget_accordion_repr_parameters
        ]

        vbox = VBox(all_kids)

        self._view._request_repr_parameters(component=self.widget_component_slider.value,
            repr_index=self.widget_repr_slider.value)

        self.widget_repr = _relayout_master(vbox, width='100%')

        self._refresh(self.widget_component_slider, self.widget_repr_slider)

        setattr(self.widget_repr, "_saved_widgets", [])
        for _box in self.widget_repr.children:
            if hasattr(_box, 'children'):
                for kid in _box.children:
                    self.widget_repr._saved_widgets.append(kid)

        return self.widget_repr
 |#

(defmethod %make-widget-repr-parameters ((self trajectory-player) component-slider repr-slider &optional (repr-name-text nil))
  (let ((name " "))
    (if repr-name-text
	(setf name (value repr-name-text)))
    (let ((widget (%display-repr (%view self)
		 :component (value component-slider)
		 :repr-index (value repr-slider)
		 :name name)))
      (setf (%ngl-name widget) "repr_parameters_box")
      widget)))
      
(defmethod %make-button-export-image ((self trajectory-player))
  (let ((slider-factor (make-instance 'int-slider
				      :value 4
				      :min 1
				      :max 10
				      :description "scale"))
	(checkbox-antialias (make-instance 'checkbox
					   :value t
					   :description "antialias"))
	(checkbox-trim (nilnstance 'checkbox
				      :value :false
				      :description "trim"))
	(checkbox-transpnil(make-instance 'checkbox
					     :value :false
					     :description "transparent"))
	(filename-text (make-instance 'text
				      :value "Screenshot"
				      :description "Filename"))
	(delay-text (make-instance 'float-text
				   :value 1
				   :description "delay (s)"
				   :tooltip "hello"))
	(start-text (make-instance 'int-text
				   :value 0
				   :description "start"))
	(stop-text (make-instance 'int-text
				  :value (count (%view self))
				  :description "stop"))
	(step-text (make-instance 'int-text
				  :value 1
				  :description "step")))
    (setf (max-width (layout start-text)) +default-text-width+
	  (max-width (layout stop-text)) +default-text-width+
	  (max-width (layout step-text)) +default-text-width+
	  (max-width (layout filename-text)) +default-text-width+
	  (max-width (layout delay-text)) +default-text-width+)
	  ; TODO: implement on-click-images in player.lisp!
    (let ((button-movie-images (make-instance 'button
					      :description "Export Images")))
       (flet ((download-image (filename)
	        (download-image (%view self)
	 		       :factor (value slider-factor)
	 		       :antialias (value checkbox-antialias)
	 		       :trim (value checkbox-trim)
	 		       :transparent (value checkbox-transparent)
	 		       :filename filename)))
	(let* ((vbox (make-instance 'vbox
				   :children (list button-movie-images
						     start-text
						     stop-text
						     step-text
						     delay-text
						     filename-text
						     slider-factor
						     checkbox-antialias
						     checkbox-trim
						     checkbox-transparent)))
	       (form-items (%relayout vbox make-form-item-layout))
	       (form (make-instance 'Box form-items :layout (%make-box-layout))))
	  form)))))
     #|
        @button_movie_images.on_click
        def on_click_images(button_movie_images):
            for i in range(start_text.value, stop_text.value, step_text.value):
                self._view.frame = i
                time.sleep(delay_text.value)
                download_image(filename=filename_text.value + str(i))
                time.sleep(delay_text.value)
 |#
	    
(defmethod %make-resize-notebook-slider ((self trajectory-player))
  (make-instance 'int-slider
                 :min 300 :max 2000 :description "resize notebook"
                 :on-trait-change (list
                                    (cons :value
                                          (lambda (instance type name old new)
                                            (declare (ignore instance type name old))
                                            (remote-call (%view self)
                                                         "resizeNotebook"
                                                         :target "Widget"
                                                         :args (list new)))))))

(defmethod %make-add-widget-repr ((self trajectory-player) component-slider)
  (let ((dropdown-repr-name (make-instance 'dropdown
					   :options *REPRESENTATION-NAMES*
					   :value "cartoon"))
	(repr-selection (make-instance 'text
				       :value "*"
				       :description ""))
	(repr-button (make-instance 'button
				    :description "Add"
				    :tooltip "Add representation. You can also hit Enter in selection box.")))
    (setf (layout repr-button) (make-instance 'jupyter-widgets:layout
					      :width "auto"
					      :flex "1 1 auto")
	  (width (layout dropdown-repr-name)) +default-text-width+
	  (width (layout repr-selection)) +default-text-width+)
    (flet ((on-click-or-submit (button-or-text-area)
	     (add-representation (%view self)
				 :selection (strip (value repr-selection))
				 :repr-type (value dropdown-repr-name)
				 :component (value component-slider))
	     (values)))
      (on-click repr-button on-click-or-submit)
      (on-submit repr-selection on-click-or-submit)
      (let ((add-repr-box (make-instance 'hbox
					 :children (list repr-button
							   dropdown-repr-name
							   repr-selection))))
	(setf (%ngl-name add-repr-box) "add_repr_box")
	add-repr-box))))
 


(defmethod %make-repr-playground ((self trajectory-player))
  (error "-make-repr-playground in player.lisp needs your help"))
   #|
    def _make_repr_playground(self):
        vbox = VBox()
        children = []

        rep_names = REPRESENTATION_NAMES[:]
        excluded_names = ['ball+stick', 'distance']
        for name in excluded_names:
            rep_names.remove(name)

        repr_selection = Text(value='*')
        repr_selection.layout.width = default.DEFAULT_TEXT_WIDTH
        repr_selection_box  = HBox([Label('selection'), repr_selection])
        setattr(repr_selection_box, 'value', repr_selection.value)

        for index, name in enumerate(rep_names):
            button = ToggleButton(description=name)

            def make_func():
                def on_toggle_button_value_change(change, button=button):
                    selection = repr_selection.value
                    new = change['new'] # True/False
                    if new:
                        self._view.add_representation(button.description, selection=selection)
                    else:
                        self._view._remove_representations_by_name(button.description)
                return on_toggle_button_value_change

            button.observe(make_func(), names='value')
            children.append(button)

        button_clear = Button(description='clear', button_style='info',
                icon='fa-eraser')

        @button_clear.on_click
        def on_clear(button_clear):
            self._view.clear()
            for kid in children:
                # unselect
                kid.value = False

        vbox.children = children + [repr_selection, button_clear]
        _make_autofit(vbox)
        self.widget_quick_repr = vbox
        return self.widget_quick_repr
 |#

(defmethod %make-repr-name-choices ((self trajectory-player) component-slider repr-slider)
  (let ((repr-choices (make-instance 'dropdown :options '((" " . "")))))
    (flet ((on-chose (widget type name old new)
       (declare (ignore widget type name old))
	     (let ((repr-name new)
		   (repr-index (index (options repr-choices))))
	       (setf (value repr-slider) repr-index)
	       (values))))
      (jupyter-widgets:observe repr-choices :value on-chose)
      (setf (width (layout repr-choices)) +default-text-width+
	    (widget-repre-choices self) repr-choices)
      (widget-repr-choices self)))
  (error "I don't think we have an observe or index function defined.  %make-repr-name-choices in player.lisp"))
 #|
    def _make_repr_name_choices(self, component_slider, repr_slider):
        repr_choices = Dropdown(options=[" ",])

        def on_chose(change):
            repr_name = change['new']
            repr_index = repr_choices.options.index(repr_name)
            repr_slider.value = repr_index

        repr_choices.observe(on_chose, names='value')
        repr_choices.layout.width = default.DEFAULT_TEXT_WIDTH

        self.widget_repr_choices = repr_choices
        return self.widget_repr_choices
 |#

(defmethod %make-drag-widget ((self trajectory-player))
  (error "only YOU can prevent this error message in %make-drag-widget in player.lisp"))
 #|
    def _make_drag_widget(self):
        button_drag = Button(description='widget drag: off', tooltip='dangerous')
        drag_nb = Button(description='notebook drag: off', tooltip='dangerous')
        button_reset_notebook = Button(description='notebook: reset', tooltip='reset?')
        button_dialog = Button(description='dialog', tooltip='make a dialog')
        button_split_half = Button(description='split screen', tooltip='try best to make a good layout')

        @button_drag.on_click
        def on_drag(button_drag):
            if button_drag.description == 'widget drag: off':
                self._view._set_draggable(True)
                button_drag.description = 'widget drag: on'
            else:
                self._view._set_draggable(False)
                button_drag.description = 'widget drag: off'

        @drag_nb.on_click
        def on_drag_nb(button_drag):
            if drag_nb.description == 'notebook drag: off':
                js_utils._set_notebook_draggable(True)
                drag_nb.description = 'notebook drag: on'
            else:
                js_utils._set_notebook_draggable(False)
                drag_nb.description = 'notebook drag: off'

        @button_reset_notebook.on_click
        def on_reset(button_reset_notebook):
            js_utils._reset_notebook()

        @button_dialog.on_click
        def on_dialog(button_dialog):
            self._view._remote_call('setDialog', target='Widget')

        @button_split_half.on_click
        def on_split_half(button_dialog):
            from nglview import js_utils
            import time
            js_utils._move_notebook_to_the_left()
            js_utils._set_notebook_width('5%')
            time.sleep(0.1)
            self._view._remote_call('setDialog', target='Widget')

        drag_box = HBox([button_drag, drag_nb, button_reset_notebook,
                        button_dialog, button_split_half])
        drag_box = _make_autofit(drag_box)
        self.widget_drag = drag_box
        return drag_box
 |#

(defmethod %make-spin-box ((self trajectory-player))
  (let ((checkbox-spin (apply #'make-instance 'checkbox (%spin-x self) :description "spin"))
	(spin-x-slide (apply #'make-instance 'int-slider (%spin-x self) :min -1 :max 1 :description "spin_x"))
	(spin-y-slide (apply #'make-instance 'int-slider (%spin-y self) :min -1 :max 1 :description "spin_y"))
	(spin-z-slide (apply #'make-instance 'int-slider (%spin-z self) :min -1 :max 1 :description "spin_z"))
	(spin-speed-slide (apply #'make-instance 'float-slider (%spin-speed self) :min 0 :max 0.2 :step 0.001 :description "spin speed")))
    (error "Only YOU can implement the link traitlet")
     #|
        link((checkbox_spin, 'value'), (self, 'spin'))
        link((spin_x_slide, 'value'), (self, '_spin_x'))
        link((spin_y_slide, 'value'), (self, '_spin_y'))
        link((spin_z_slide, 'value'), (self, '_spin_z'))
        link((spin_speed_slide, 'value'), (self, '_spin_speed'))
     |#

    (let ((spin-box (make-instance 'vbox :children (list checkbox-spin spin-x-slide spin-y-slide spin-z-slide spin-speed-slide))))
      (setf spin-box (%relayout-master spin-box :width "75%"))
      spin-box)))

(defmethod %make-widget-picked ((self trajectory-player))
  (setf (widget-picked self) (%make-text-picked self))
  (let ((picked-box (make-instance 'hbox :children (list (widget-picked self)))))
    (%relayout-master picked-box :width "75%")))

(defmethod %make-export-image-widget ((self trajectory-player))
  (if (not (widget-export-image self))
      (setf (widget-export-image self) (make-instance 'hbox :children (list (funcall (%make-button-export-image self))))))
      (widget-export-image self))
;;;HELP! This can't be right. I don't think my vector works properly.

(defmethod %make-extra-box ((self trajectory-player))
  (if (not (widget-extra self))
      (let* ((extra-list (list
			 (cons (%make-drag-widget self) "Drag")
			 (cons (%make-spin-box self) "Spin")
			 (cons (%make-widget-picked) "Picked")
			 (cons (%make-repr-playground) "Quick")
			 (cons (%make-export-image-widget) "Image")
			 (cons (%make-command-box self) "Command")))
	     (extra-box (%make-delay-tab extra-list :selected-index 0)))
	(setf (widget-extra self) extra-box)))
  (widget-extra self))

(defmethod %make-theme-box ((self trajectory-player))
  (if (not (widget-theme self))
      (setf (widget-theme self) (apply #'make-instance 'box :children (list (%make-button-theme self) (%make-button-reset-theme self :hide-toolbar nil) (%make-button-reset-theme self :hide-toolbar t) (%make-button-clean-error-output self)))))
  (widget-theme self))

(defun %make-general-box (self)
  (jupyter:inform :info nil "trajectory-player %make-general-box")
  (unless (widget-general self)
    (let ((step-slide (make-instance 'int-slider
                                     :value (%step self) :min -100 :max 100 :description "step"
                                     :layout (make-instance 'jupyter-widgets:layout
                                                            :width +default-slider-width+)
                                     :style (make-instance 'jupyter-widgets:slider-style
                                                           :description-width +default-text-width+)))
          (delay-text (make-instance 'int-slider
                                     :value (delay self) :min 10 :max 1000 :description "delay"
                                     :layout (make-instance 'jupyter-widgets:layout
                                                            :width +default-slider-width+)
                                     :style (make-instance 'jupyter-widgets:slider-style
                                                           :description-width +default-text-width+)))
          (toggle-button-interpolate (make-instance 'toggle-button
          :value (interpolate self) :description "Smoothing" :tooltip "smoothing trajectory"))
          (background-color-picker (make-instance 'color-picker :value "white" :description "background"
                               :style (make-instance 'jupyter-widgets:description-style
                                                      :description-width +default-text-width+)))
          (camera-type (make-instance 'dropdown :index (position (camera self) +camera-types+) :%options-labels +camera-types+ :description "camera"
                               :style (make-instance 'jupyter-widgets:description-style
                                                      :description-width +default-text-width+))))
      (jupyter-widgets:link step-slide :value self :step)
      (jupyter-widgets:link delay-text :value self :delay)
      (jupyter-widgets:link toggle-button-interpolate :value self :interpolate)
      (setf (jupyter-widgets:widget-index camera-type) (position (camera self) +camera-types+))
      (jupyter-widgets:observe camera-type :index
        (lambda (instance name type old new)
          (declare (ignore instance name type old))
          (setf (camera self) (nth new +camera-types+))))
      (jupyter-widgets:observe self :camera
        (lambda (instance name type old new)
          (declare (ignore instance name type old))
          (setf (jupyter-widgets:widget-index camera-type) (position new +camera-types+))))
      (jupyter-widgets:link background-color-picker :value (%view self) :background)
    (let* ((center-button (%make-button-center self))
           (render-button (%show-download-image self))
           (center-render-hbox (%make-autofit (make-instance 'jupyter-widgets:h-box :children (list toggle-button-interpolate center-button render-button))))
           (v0-left (make-instance 'vbox :children (list step-slide delay-text background-color-picker camera-type center-render-hbox))))
      ;(setf v0-left (%relayout-master v0-left :width "100%"))
      (setf (widget-general self) v0-left))))
  (widget-general self))

(defmethod %make-command-box ((self trajectory-player))
  (let ((widget-text-command (make-instance 'text)))
    (flet ((submit-command (_)
	     (let ((command (value widget-text-command)))
	       (execute js-utils command)
	       (setf (value widget-text-command) ""))))
	    ; FIXME: on_submit is deprecated. use continuous_update = false
      ; (cljw::on-submit widget-text-command #'submit-command)
      widget-text-command)))


(defmethod %create-all-tabs ((self trajectory-player))
  (let ((tab (display self))
	(index 0))
    (loop for child across (children tab)
       do
	 (setf (selected-index tab) index)
	 (incf index))
    (setf (widget-extra self) (%make-extra-box self)
	  index 0)
    (loop for child across (children (widget-extra self))
       do
	 (setf (selected-index (widget-extra self)) index)))
  (values))


(defmethod %simplify-repr-control ((self trajectory-player))
  (loop for widget in (%saved-widgets (widget-repr self))
     do
       (if (not (typep widget 'tab))
	   (setf (display (layout widget)) "none")))
  (setf (display (layout (widget-repr-choices self))) "flex"
	(selected-index (widget-accordion-repr-parameters self)) 0)
  (values))
