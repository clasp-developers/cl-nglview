(in-package :nglv)

(jupyter:inform :info nil "Loading widget.lisp~%")

(defmacro @observe (slot observer)
  nil)

(defparameter *frontend-version* "1.1.5") ;; must match to js/package.json and js/src/widget_ngl.js

(defparameter *excluded-callback-after-firing*
  (list "setUnSyncCamera" "setSelector" "setUnSyncFrame"
        "setDelay" "autoView" "_downloadImage" "_exportImage"
        "set_representation_from_backend"))

;; Save up to 8 previous picks
(defparameter *pick-history-depth* 16)

(defclass component-viewer ()
  ((%view :initarg :view :accessor view)
   (%index :initarg :index :accessor index)))


(defclass nglwidget (jupyter-widgets:dom-widget)
  ((%ngl-version :initarg :ngl-version
                 :accessor ngl-version
                 :trait :unicode
                 :initform "")
   (%image-data :initarg :image-data
                :trait :unicode
                :initform "")
   (frame :initarg :frame
          :accessor frame
          :type Integer
          :initform 0
          :trait :int)
   (count :initarg :count
          :accessor count
          :type Integer
          :initform 1
          :trait :int)
   (background :initarg :background
               :accessor background
               :trait :unicode
               :initform "white") ; I think this is deprecated
   (loaded :initarg :loaded
           :accessor loaded
           :type boolean
           :initform nil)
   (picked :initarg :picked
            :accessor picked
            :trait :dict
            :initform nil)
   (%pick-history :initarg :pick-history
                  :accessor pick-history
                  :type list
                  :initform nil)
   (n-components :initarg :n-components
                  :accessor n-components
                  :type integer
                  :initform 0
                  :trait :int)
   (%scene-position :initarg :scene-position
                    :accessor scene-position
                    :trait :dict
                    :initform nil)
   (%scene-rotation :initarg :scene-rotation
                    :accessor scene-rotation
                    :trait :dict
                    :initform nil)
   (%first-time-loaded :initarg :first-time-loaded
                       :accessor first-time-loaded
                       :type boolean
                       :initform T)
   ;; hack to always display movie
   (%n-dragged-files :initarg :n-dragged-files
                     :accessor n-dragged-files
                     :type integer
                     :initform 0
                     :trait :int)
   ;; TODO: remove %parameters?
   (%parameters :initarg :%parameters
                :accessor %parameters
                :initform nil) ; not synchronized https://github.com/drmeister/spy-ipykernel/blob/master/nglview/widget.py#L124
   (%ngl-full-stage-parameters :initarg :ngl-full-stage-parameters
                               :accessor ngl-full-stage-parameters
                               :trait :dict
                               :initform nil)
   (%ngl-full-stage-parameters-embed :initarg :ngl-full-stage-parameters-embed
                                     :accessor ngl-full-stage-parameters-embed
                                     :trait :dict
                                     :initform nil)
   (%ngl-original-stage-parameters :initarg :ngl-original-stage-parameters
                                   :accessor ngl-original-stage-parameters
                                   :trait :dict
                                   :initform nil)
   ;; Not sync'd
   (%coordinates-dict :initarg :coordinates-dict
                      :accessor coordinates-dict
                      :initform nil)
   (%camera-str :initarg :camera-str
                :accessor camera-str
                :trait :unicode
                :initform "orthographic") ; probably need validate for following values "perspective" "orthographic"
   (%camera-orientation :initarg :camera-orientation
                        :accessor camera-orientation
                        :type list
                        :initform nil
                        :trait :list)
   (%ngl-repr-dict :initarg :ngl-repr-dict
                   :accessor ngl-repr-dict
                   :trait :dict
                   :initform nil)
   (%ngl-component-ids :initarg :ngl-component-ids
                       :accessor ngl-component-ids
                       :type list
                       :initform nil)
   (%ngl-component-names :initarg :ngl-component-names
                         :accessor ngl-component-names
                         :type list
                         :initform nil)
   (%already-constructed :initarg :already-constructed
                         :accessor already-constructed
                         :type boolean
                         :initform nil)
   (%ngl-msg :initarg :ngl-msg
             :accessor ngl-msg
             :type (or string null)
             :initform nil)
   (%send-binary :initarg :send-binary
                 :accessor send-binary
                 :type boolean
                 :initform t)
   (%init-gui :initarg :init-gui
              :accessor init-gui
              :type boolean
              :initform nil)
   (%hold-image :initarg :hold-image
                :accessor hold-image
                :type boolean
                :initform :false)
   (%ngl-serialize :initarg :ngl-serialize
                   :accessor ngl-serialize
                   :type boolean
                   :initform :false
                   :trait :bool)
   (%ngl-msg-archive :initarg :ngl-msg-archive
                     :accessor ngl-msg-archive
                     :type list
                     :initform nil
                     :trait :list)
   (%ngl-coordinate-resource :initarg :ngl-coordinate-resource
                             :accessor ngl-coordinate-resource
                             :trait :dict
                             :initform nil)
   (%representations :initarg :representations
                     :accessor representations)
   ;; internal variables
   (%gui :initarg :%gui :accessor %gui :initform nil)
;;   (%init-gui :initarg :gui :accessor gui :initform nil) ;; WHY? does nglview does this
   (%theme :initarg :theme :accessor theme :initform "default")
   (%widget-image :initarg :widget-image :accessor widget-image
                  :initform (make-instance 'jupyter-widgets:image :width 900))
   (%image-array :initarg :image-array :accessor image-array :initform #())
   (%event :initarg :event :accessor event :initform (make-instance 'pythread:event))
   (%ngl-displayed-callbacks-before-loaded-reversed
    :initarg :ngl-displayed-callbacks-before-loaded-reversed
    :accessor ngl-displayed-callbacks-before-loaded-reversed
    :initform nil)
   (%ngl-displayed-callbacks-after-loaded-reversed
    :initarg :ngl-displayed-callbacks-after-loaded-reversed
    :accessor ngl-displayed-callbacks-after-loaded-reversed
    :initform nil)
   (%shape :initarg :shape :accessor shape
           :initform (make-instance 'shape))
   (%stage :initarg :stage :accessor stage)
   (%control :initarg :control :accessor control)
;;; FIXME:  Would this be a Clasp mp:PROCESS??
;;;   (%handle-msg-thread :initarg :handle-msg-thread :accessor handle-msg-thread :initform :threading.thread)
   #|
   self._handle_msg_thread = threading.Thread(target=self.on_msg,
   args=(self._ngl_handle_msg,))
   # # register to get data from JS side
   self._handle_msg_thread.daemon = True
   self._handle_msg_thread.start()
   |#
   ;; Only one remote-call-thread in pythread:*remote-call-thread*
   #+(or)(%remote-call-thread
    :initarg :remote-call-thread
    :accessor remote-call-thread
    :allocation :class
    :initform pythread:*remote-call-thread*)
   ;; Only one remote-call-thread-queue in pythread:*remote-call-thread-queue*
   #+(or)(%remote-call-thread-queue
    :initarg :remote-call-thread-queue
    :accessor remote-call-thread-queue
    :allocation :class
    :initform pythread:*remote-call-thread-queue*)
   (%handle-msg-thread
    :accessor handle-msg-thread
    :initform nil)
   ;; keep track but making copy
   ;;; FIXME - fix this nonsense below
#||
        self._set_unsync_camera()
        self.selector = str(uuid.uuid4()).replace('-', '')
        self._remote_call('setSelector', target='Widget', args=[self.selector,])
        self.selector = '.' + self.selector # for PlaceProxy
        self._place_proxy = PlaceProxy(child=None, selector=self.selector)
        self.player = trajectory-player(self)
        self._already_constructed = True
   ||#
   (%trajlist :initform nil
              :accessor trajlist)
   (%player :accessor player)
   (%init-representations :initarg :init-representations
                     :accessor init-representations
                     :initform nil)
   )
  (:default-initargs
   :%view-name "NGLView"
   :%view-module "nglview-js-widgets"
   :%view-module-version *frontend-version*
   :%model-name "NGLModel"
   :%model-module "nglview-js-widgets"
   :%model-module-version *frontend-version*)
  (:metaclass jupyter-widgets:trait-metaclass))

(jupyter-widgets:register-widget nglwidget)

(defun make-nglwidget (&rest kwargs-orig
                       &key (structure nil structure-p)
                         (representations nil representations-p)
                         (parameters nil parameters-p)
                         (gui nil gui-p)
                         (theme "default" theme-p)
                         &allow-other-keys)
  (jupyter:inform :info nil "make-nglwidget~%")
  (let ((kwargs (copy-list kwargs-orig)))
    (when structure-p (remf kwargs :structure))
    (when representations-p (remf kwargs :representations))
    (when parameters-p (remf kwargs :parameters))
    (when gui-p (remf kwargs :gui))
    (when theme-p (remf kwargs :theme))
    (let ((widget (apply #'make-instance 'nglwidget kwargs)))
      ; FIXME: Do we need to do this? Current method violates finalization rules by refering to widget
      ; (gctools:finalize widget #'(lambda () (format t "Finalizing widget~%") (widget-close widget)))
      (when gui-p (setf (init-gui widget) gui))
      (when theme-p (setf (theme widget) theme))
      (setf (stage widget) (make-instance 'stage :view widget))
;;;      (setf (control widget) (make-instance 'viewer-control :view widget))
      #+(or)(warn "What do we do about add_repr_method_shortcut")
      ;;; Handle messages - in Python they start a daemon - WHY????
      ; TWB - I don't think we need to do any of this. The kernel message loop should route messages
      ; to our on-comm-message
      ; (cl-ipykernel:on-msg widget '%ngl-handle-msg)
      ; #+(or)(progn
      ;         (jupyter:inform :info nil "Starting handle-msg-thread from process ~s~%" (bordeaux-threads:current-thread))
      ;         (setf (handle-msg-thread widget)
      ;               (mp:process-run-function
      ;                'handle-msg-thread
      ;                (lambda ()
      ;                  (cl-ipkernel:on-msg widget '%ngl-handle-msg)
      ;                  (jupyter:inform :info nil "Calling cl-ipykernel:on-msg widget with #'%ngl-handle-msg in process: ~s~%" (bordeaux-threads:current-thread))
      ;                  (loop
                         ;; yield while respecting callbacks to ngl-handle-msg
      ;                    (bordeaux-threads:thread-yield)
      ;                    (sleep 1) ;; I don't want to sleep here - do I?   Will it slow down callbacks to %ngl-handle-msg
      ;                    (format t "handle-msg-thread in process-yield loop ~s~%" (get-internal-real-time ) ))
      ;                  (jupyter:inform :info nil "Leaving handle-msg-thread - but this shouldn't happen - this thread should be killed!!!!!~%"))
      ;                cl-jupyter:*default-special-bindings*)))
      (when parameters-p (setf (parameters widget) parameters))
      (cond
        ((typep structure 'Trajectory)
         (let ((name (nglv::get-structure-name structure)))
           (add-trajectory widget structure :name name)))
        ((consp structure)
         (warn "Handle list of structures"))
        (structure
         (add-structure widget structure)))
      ;; call before setting representations
      (jupyter:inform :info nil "About to set representations -> ~a~%" representations)
      (when representations
        (check-type representations array)
        (setf (init-representations widget) representations))
      (%set-unsync-camera widget)
      (%remote-call widget "setSelector" :target "Widget" :args (list (jupyter:make-uuid)))
      (let ((new-player (make-instance 'trajectory-player :%view widget)))
        (setf (player widget) new-player))
      (setf (already-constructed widget) t)
      widget)))


(defmethod %set-serialization ((self nglwidget) &optional frame-range)
  (setf (ngl-serialize self) :true)
  (setf (ngl-msg-archive self)
         (mapcar (lambda (callback)
                   (ngl-msg callback))
                 (ngl-displayed-callbacks-after-loaded-reversed self)))
  (let ((resource (ngl-coordinate-resource self)))
    (when frame-range
      #|| ;; Finish set-serialization
      (loop for t-index from 0
      for traj in (trajlist self)
      do (setf (elt resource t-index) (list))
      (loop for 
      for f_index in range(*frame_range):
      if f_index < traj.n_frames:
      resource[t_index].append(encode_base64(traj.get_coordinates(f_index)))
      else:
      resource[t_index].append(encode_base64(
                            np.empty((0), dtype='f4')))
            resource['n_frames'] = len(resource[0])

        self._ngl_coordinate_resource = resource
        self._ngl_full_stage_parameters_embed = self._ngl_full_stage_parameters
      ||#
      )))

(defmethod %unset-serialization ((self nglwidget))
  (setf (ngl-serialize self) :false
        (ngl-msg-archive self) nil
        (ngl-coordinate-resource self) nil
        (ngl-full-stage-parameters-embed self) nil))



(defun parameters (widget)
  (%parameters widget))

#|
(defun (setf parameters) (params widget)
  (let ((params (camelize-dict params)))
    (setf (parameters widget) params)
    (%remote-call widget "setParameters"
		  :target "Widget"
		  :args params))
  params)
|#


  #|
   if isinstance(structure, Trajectory):
   name = py_utils.get_name(structure, kwargs)
   self.add_trajectory(structure, name=name)
   elif isinstance(structure, (list, tuple)):
   trajectories = structure
   for trajectory in trajectories:
   name = py_utils.get_name(trajectory, kwargs)
   self.add_trajectory(trajectory, name=name)
   else:
   if structure is not None:
   self.add_structure(structure, **kwargs)
   |#
#|   
   # call before setting representations
   self._set_initial_structure(self._init_structures)
   if representations:
   self._init_representations = representations
   else:
   self._init_representations = [
   {"type": "cartoon", "params": {
   "sele": "polymer"
   }},
   {"type": "ball+stick", "params": {
   "sele": "hetero OR mol"
   }},
   {"type": "ball+stick", "params": {
   "sele": "not protein and not nucleic"
   }}
   ]
   |#

#|
;;; Duplicate code
(defmethod %fire-callbacks ((widget nglwidget) callbacks)
  (loop for callback in callbacks
     do (progn
          (funcall (car callback) widget)
          (when (string= (cdr callback) "loadFile")
            (%wait-until-finished widget)))))
    
(defmethod %wait-until-finished ((widget nglwidget) &optional (timeout 0.0001))
  (pythread:clear (event widget))
  (loop
     (sleep timeout)
     (when (pythread:is-set (event widget))
       (return-from %wait-until-finished))))
|#



(defmethod (setf parameters) (params (widget nglwidget))
  (let ((params (camelize-dict params)))
    (setf (%parameters widget) params)
    (%remote-call widget "setParameters" :target "Widget" :args params))
  (values))




(defun camera (widget)
  (cond
    ((string= (camera-str widget) "orthographic")
     :orthographic)
    ((string= (camera-str widget) "perspective")
     :perspective)
    (t (error "Illegal value for %camera-str ~s - must be one of orthographic or perspective"))))

(defun (setf camera) (value widget)
  "Values:  :perspective or :orthographic"
  (checktype value (member :perspective :orthographic))
  (let ((camera-str (ecase value
                      (:perspective "perspective")
                      (:orthographic "orthographic"))))
    (setf (camera-str widget) camera-str)
    (%remote-call widget "setParameters"
                  :target "Stage"
                  :kwargs (plist-to-kwargs '(:camera-type camera-str)))))

(defmethod %set-camera-orientation ((self nglwidget) arr)
  (%remote-call self "set_camera_orientation"
                :target "Widget"
                :args (list arr)))

(defmethod %request-stage-parameters ((self nglwidget))
  (%remote-call self
                "requestUpdateStageParameters"
                :target "Widget"))

(defmethod jupyter-widgets:on-trait-change ((object nglwidget) type (name (eql :picked)) old new)
  (declare (ignore type name))
  (jupyter:inform :info nil "%on-picked called with name: ~s new: ~s old: ~s~%" name new old)
  (when (and new
             (dict-entry "atom" new)
             (slot-boundp self '%pick-history))
    (push new (pick-history self))
    (setf (pick-history self) (subseq (pick-history self) 0 (min *pick-history-depth* (length (pick-history self))))))
  (unless (player self)
    (warn "%on-picked (player self) of ~a is NIL" self))
  (when (and (player self) (widget-picked (player self)))
    (setf (value (widget-picked (player self))) (jsown:to-json new))))

(defmethod jupyter-widgets:on-trait-change ((object nglwidget) type (name (eql :background)) old new)
  (declare (ignore type name old))
  (setf (%parameters object) (list (cons "backgroundColor" new))))

(defmethod jupyter-widgets:on-trait-change ((self nglwidget) type (name (eql :%n-dragged-file)) old new)
  (declare (ignore type name old))
  (when (and (= (- new old) 1) (slot-boundp self '%ngl-component-ids))
    (setf (ngl-component-ids self) (append (ngl-component-ids self) (make-uuid t)))))

(defmethod jupyter-widgets:on-trait-change ((self nglwidget) type (name (eql :n-components)) old new)
  (declare (ignore type name old))
  (when (player self)
    (when (widget-repr (player self))
      (let ((component-slider (get-widget-by-name (widget-repr (player self)) "component_slider")))
        (when (>= (1- new) (min component-slider))
          (setf (max component-slider) (1- new))))
      (let ((component-dropdown (get-widget-by-name (widget-repr (player self)) "component_dropdown")))
        ;; component_dropdown.options = tuple(self._ngl_component_names)
        (setf (options component-dropdown) (ngl-component-names self))
        (when (= new 0)
          (setf (options component-dropdown) (list " ")
                (value component-dropdown) " "
                (max component-slider) 0)
          (let ((reprlist-choices (get-widget-by-name (widget-repr (player self)) "reprlist_choices")))
            (setf (options reprlist-choices) (list " ")))
          (let ((reprlist-slider (get-widget-by-name (widget-repr (player self)) "repr_slider")))
            (setf (max repr-slider) 0))
          (let ((repr-name-text (get-widget-by-name (widget-repr (player self)) "repr_name_text"))
                (repr-name-selection (get-widget-by-name (widget-repr (player self)) "repr_selection")))
            (setf (value repr-name-text) " "
                  (value repr-selection) " ")))))))

(defmethod jupyter-widgets:on-trait-change ((self nglwidget) type (name (eql :%ngl-repr-dict)) old new)
  (declare (ignore type name old))
  (when (and (slot-boundp self '%player) (player self) (widget-repr (player self)))
    (let* ((repr-slider (get-widget-by-name (widget-repr (player self)) "repr_slider"))
           (component-slider (get-widget-by-name (widget-repr (player self)) "component_slider"))
           (repr-name-text (get-widget-by-name (widget-repr (player self)) "repr_name_text"))
           (repr-selection (get-widget-by-name (widget-repr (player self)) "repr_selection"))
           (reprlist-choices (get-widget-by-name (widget-repr (player self)) "reprlist_choices"))
           (repr-names (get-repr-names-from-dict (ngl-repr-dict self) (value component-slider))))
      (if (and (consp new)
               (= (length new) 1)
               (consp (car new))
               (= (car (car new)) 0)
               (eq (cdr (car new)) nil))
          (setf (value repr-selection) "")
          (error "Finish implementing %handle-repr-dict-changed")
          #|
          if change['new'] == {0: {}}:
          repr_selection.value = ''
          else:
          options = tuple(
                    str(i) + '-' + name for (i, name) in enumerate(repr_names))
                reprlist_choices.options = options

                try:
                    value = reprlist_choices.options[repr_slider.value]
                    if isinstance(value, tuple):
                        # https://github.com/jupyter-widgets/ipywidgets/issues/1512
                        value = value[0]
                    reprlist_choices.value = value
                except IndexError:
                    if repr_slider.value == 0:
                        # works fine with ipywidgets 5.2.2
                        reprlist_choices.options = tuple([
                            ' ',
                        ])
                        reprlist_choices.value = ' '
                    else:
                        reprlist_choices.value = reprlist_choices.options[
                            repr_slider.value - 1]

                # e.g: 0-cartoon
                repr_name_text.value = reprlist_choices.value.split('-')[-1].strip()

                repr_slider.max = len(repr_names) - 1 if len(
                    repr_names) >= 1 else len(repr_names)
          |#))))

(defmethod %update-count ((widget nglwidget))
  (setf (count widget) (apply #'max (loop for traj in (trajlist widget) collect (n-frames traj))))
  (values))


(defmethod wait-until-finished ((widget nglwidget) &optional (timeout 1.0))
  (jupyter:inform :info nil "entered wait-until-finished~%")
  (pythread:clear (event widget))
  (loop
    (sleep timeout)
    (when (pythread:is-set (event widget))
      (return-from wait-until-finished))
    (jupyter:inform :info nil "woke wait-until-finished after timeout ~a continuing to wait~%" timeout)))

(defmethod %run-on-another-thread ((self nglwidget) func &rest args)
  (error "Finish %run-on-another-thread")
#|
      def _run_on_another_thread(self, func, *args):
        # use `event` to singal
        # func(*args)
        thread = threading.Thread(
            target=func,
            args=args, )
        thread.daemon = True
        thread.start()
        return thread
|#)

(defmethod (setf loaded) :after (new (object nglwidget))
  ;;;(setf (loaded widget) t)
  (jupyter:inform :info nil "entered on-loaded - firing before-loaded callbacks new -> ~a~%" new)
  (when new
    (when (slot-boundp widget '%ngl-displayed-callbacks-before-loaded-reversed)
      (%fire-callbacks widget (ngl-displayed-callbacks-before-loaded-reversed widget)))))

(defmethod %fire-callbacks ((widget nglwidget) callbacks)
  (jupyter:inform :info nil "%fire-callbacks entered in process ~s~%  callbacks: ~s~%" (bordeaux-threads:current-thread)
                   (loop for x in callbacks
                         collect (list (pythread:method-name x) (pythread:description x))))
  (flet ((_call ()
           (jupyter:inform :info nil "%fire-callbacks _call entered in process ~s~%" (bordeaux-threads:current-thread))
           (loop for callback in callbacks
                 do (progn
                      (jupyter:inform :info nil "      %fire-callback -> ~s in process ~s~%" (pythread:method-name callback) (bordeaux-threads:current-thread))
                      (pythread:fire-callback callback widget)
                      (when (string= (pythread:method-name callback) "loadFile")
                        (jupyter:inform :info nil "    Waiting until finished~%")
                        (wait-until-finished widget))))))
    (bordeaux-threads:make-thread (lambda () (_call))
                             :initial-bindings nil ; FIXME: cl-jupyter:*default-special-bindings*
                             :name 'fire-callbacks-thread))
  (jupyter:inform :info nil "Done %fire-callbacks~%"))

(defmethod %refresh-render ((widget nglwidget))
  "useful when you update coordinates for a single structure.

        Notes
        -----
        If you are visualizing a trajectory with more than 1 frame, you can use the
        player slider to trigger the refreshing.
        "
  (let ((current-frame (frame widget)))
    (setf (frame widget) (expt 10 6)
          (frame widget) current-frame)))

(defmethod sync-view ((widget nglwidget))
  "Call this if you want to sync multiple views of a single viewer
   Note: unstable feature"
  (jupyter:inform :info nil "entered sync-view~%")
  (let (new-callbacks)
    (loop for c in (reverse (ngl-displayed-callbacks-after-loaded-reversed widget))
          do (let (ngl-msg-kwargs-default-representation)
               (when (and (string= (pythread:method-name c) "loadFile")
                          (setf ngl-msg-kwargs-default-representation (assoc "defaultRepresentation" (cdr (assoc "kwargs" (ngl-msg c) :test #'string=)) :test #'string=)))
                 (rplacd ngl-msg-kwargs-default-representation :false)))
             (let ((msg (cons (cons "last_child" :true) (ngl-msg c))))
               (let ((callback (make-instance 'remote-call-callback
                                              :method-name (cdr (assoc "methodName" msg :test #'string=))
                                              :ngl-msg msg)))
                 (push callback new-callbacks))))
    (let* ((msg (list (cons "target" "Widget")
                      (cons "type" "call_method")
                      (cons "methodName" "set_representation_from_backend")
                      (cons "args" #())
                      (cons "kwargs" (list))
                      (cons "last_child" :true)))
           (callback (make-instance 'remote-call-callback
                                    :method-name "set_representation_from_backend"
                                    :ngl-msg msg)))
      (push callback new-callbacks)
      (%fire-callbacks widget (nreverse new-callbacks)))))


(defmethod %ipython-display ((widget nglwidget) &rest key &key &allow-other-keys)
  (if (first-time-loaded widget)
      (setf (first-time-loaded widget) nil)
      (sync-view widget))
  (when (init-gui widget)
    (when (not (gui widget))
      (setf (gui widget) (%display (player widget))))
    (display (gui widget)))
  (when (or (string= "dark" (theme widget)) (string= "oceans16" (theme widget)))
    (warn "how do we set the theme")
    (%remote-call widget "cleanOutput" :target "Widget"))
  (%ipython-display (place-proxy widget))
  (values))

(defmethod display ((widget nglwidget) &key (gui nil) (use-box nil))
  (if gui
      (if use-box
          (let ((box (apply #'make-instance 'BoxNGL widget (%display (player widget)))))
            (setf (%gui-style box) "row")
             box)
          (progn
            (display widget)
            (display (%display (player widget)))
            (values)))
      widget))


(defmethod %set-size ((self nglwidget) w h)
  (%remote-call self
                "setDraggable"
                :target "Widget"
                :args (list "")))

(defmethod %set-draggable ((widget nglwidget) &key (yes t))
  (if yes
      (%remote-call widget "setDraggable"
                    :target "Widget"
                    :args '(""))
      (%remote-call widget "setDraggable"
                    :target "Widget"
                    :args '("destroy"))))

(defmethod %set-sync-frame ((widget nglwidget))
  (%remote-call widget "setSyncFrame" :target "Widget"))

(defmethod %set-unsync-frame ((widget nglwidget))
  (%remote-call widget "setUnSyncFrame" :target "Widget"))

(defmethod %set-sync-camera ((widget nglwidget))
  (%remote-call widget "setSyncCamera" :target "Widget"))

(defmethod %set-unsync-camera ((widget nglwidget))
  (%remote-call widget "setUnSyncCamera" :target "Widget"))

(defmethod %set-delay ((widget nglwidget) delay)
  "unit of millisecond
  "
  (%remote-call widget "setDelay" :target "Widget"
                :args (list delay)))

(defmethod %set-spin ((widget nglwidget) axis angle)
  (%remote-call widget "setSpin"
                :target "Stage"
                :args (list axis angle)))

(defmethod %set-selection ((widget nglwidget) &key selection (component 0) (repr-index 0))
  (%remote-call widget "setSelection"
                :target "Representation"
                :args (list selection)
                :kwargs (list (cons "component_index" component)
                              (cons "repr_index" repr-index))))

(defmethod %set-color-by-residue ((widget nglwidget) &key colors (component-index 0) (repr-index 0))
  (%remote-call widget "setColorByResidue"
                :target "Widget"
                :args (list colors component-index repr-index)))

(defmethod %show-notebook-command-box ((self nglwidget))
  (%remote-call self
                "showNotebookCommandBox"
                :target "Widget"))

(defmethod %hide-notebook-command-box ((self nglwidget))
  (%remote-call self
                "hideNotebookCommandBox"
                :target "Widget"))

(defmethod color-by ((widget nglwidget) color-scheme &key (component 0))
  (let ((repr-names (get-repr-names-from-dict (ngl-repr-dict widget) component))
        (index 0))
    (loop for _ in repr-names
       do
         (update-representation widget
                                :component component
                                :repr-index index
                                :color-scheme color-scheme)
         (incf index)))
  (values))

;;; This performs the rest of the @representations.setter
(defmethod (setf representations) :after (value (self nglwidget))
  (loop for component in (ngl-component-ids self)
        do (set-representations reps (representations self))))

(defmethod update-representation ((widget nglwidget) &optional (component 0)
                                  (repr-index 0) &rest parameters)
  (let ((parameters (camelize-dict parameters))
        (kwargs (append
                 (list (cons "component_index" component)
                       (cons "repr_index" repr-index))
                 parameters)))
    (warn "How do we update kwargs")
    (%remote-call widget
                  "setParameters"
                  :target "Representation"
                  :kwargs kwargs)
    (%update-ngl-repr-dict widget)
    (values)))


(defmethod %update-repr-dict ((self nglwidget))
  (error "Finish %update-repr-dict")
#|
    def _update_repr_dict(self):
        """ Send a request to fronend to send representation parameters
        back.

        # TODO: sync or async
        """
        self._remote_call('request_repr_dict', target='Widget')
  |#
  )
(defmethod set-representations ((widget nglwidget) representations &key (component 0))
  (clear-representations widget :component component)
  (let ((kwargs ""))
    (loop for params in representations
       do
         (if t ; FIXME: (typep params 'cljw:dict)
             (progn
               (setf kwargs (aref params "params"))
               (warn "What to do about update kwargs")
               (%remote-call widget
                           "addRepresentations"
                           :target "compList"
                           :args (list (a params "type"))
                           :kwargs kwargs))
             (error "Params must be a dict"))))
  (values))

(defmethod remove-representation ((widget nglwidget) &key (component 0) (repr-index 0))
  (%remote-call widget
                "removeRepresentation"
                :target "Widget"
                :args (list component repr-index)))

(defmethod %remove-representations-by-name ((widget nglwidget) repr-name &key (component 0))
  (%remote-call widget
                "removeRepresentationsByName"
                :target "Widget"
                :args (list repr-name component))
  (values))

(defmethod %update-representations-by-name ((widget nglwidget) repr-name &optional (component 0) &rest kwargs)
  (setf kwargs (%camelize-dict kwargs))
  (%remote-call widget
                "updateRepresentationsByName"
                :target "Widget"
                :args (list repr-name component)
                :kwargs kwargs)
  (values))

(defmethod %display-repr ((widget nglwidget) &key (component 0) (repr-index 0) (name nil))
  (let ((c (concatenate 'string "c" (write-to-string component)))
        (r (write-to-string repr-index)))
    (warn "Figure out how to use handler-case")
    (setf name (aref (aref (aref (ngl-repr-dict widget) c) r) "name"))
    (make-instance 'RepresentationsControl :view widget
                   :component-index component
                   :repr-index repr-index
                   :name name)))

(defmethod %set-coordinates ((widget nglwidget) index)
  "Update coordinates for all trajectories at index-th frame"
  (when (and (slot-boundp widget '%trajlist) (trajlist widget))
    (let ((coordinates-dict ()))
      ;;
      ;; TODO: Do something for interpolation
      ;;
      (loop for trajectory in (trajlist widget)
            for traj-index = (position (id trajectory) (ngl-component-ids widget))
            do (push (cons traj-index (nglv::get-coordinates trajectory index)) coordinates-dict))
      (set-coordinates widget coordinates-dict))))

(defun ensure-simple-vector-float (coordinates)
  (if (typep coordinates '(simple-array single-float *))
      coordinates
      (error "Convert ~a to a simple-array of single-float" coordinates)))

(defmethod set-coordinates ((widget nglwidget) arr-dict)
  (jupyter:inform :info nil  "In nglview set-coordinates~%")
  (progn
    (setf (coordinates-dict widget) arr-dict)
    (if (null (send-binary widget))
        (error "Handle encode64 for set-coordinates")
        (let (buffers
              coordinates-meta)
          (loop for (index . arr) in (coordinates-dict widget)
                for byte-buffer = (core:coerce-memory-to-foreign-data (ensure-simple-vector-float arr))
                do (push byte-buffer buffers)
                do (jupyter:inform :info nil "number of xyz coords: ~a    number of bytes: ~a~%" (length arr) (clasp-ffi:foreign-data-size byte-buffer))
                do (push (cons (princ-to-string index) index) coordinates-meta))
          (let ((mytime (* (/ (get-internal-run-time) internal-time-units-per-second) 1000.0)))
            (jupyter-widgets:send-custom widget
                                         (jsown:new-js ("type" "binary_single")
                                                       ("data" coordinates-meta)
                                                       ("mytime" mytime))
                                         (coerce (nreverse buffers) 'vector)))))
    (values)))

(defmethod jupyter-widgets:on-trait-change ((object nglwidget) type (name (eql :frame)) old new)
  (when (slot-boundp object 'frame)
    (%set-coordinates object (frame object))))


(defmethod clear ((self nglwidget) &rest args)
  (apply #'clear-representations self args))

(defmethod clear-representations ((widget nglwidget) &key (component 0))
  (%remote-call widget
                "removeAllRepresentations"
                :target "compList"
                :kwargs (list (cons "component_index" component)))
  (values))

(defmethod add-shape ((self nglwidget) shapes &key (name "shape"))
  "add shape objects

        Parameters
        ----------
        shapes : vector of vectors
        name : str, default 'shape'
            name of given shape

        Notes
        -----
        Supported shape: 'mesh', 'sphere', 'ellipsoid', 'cylinder', 'cone', 'arrow'.
        
        See also
        --------
        {ngl_url}

        Examples
        --------
        (asdf:load-system :nglview)
        (defparameter *v* (make-instance 'nglv::nglwidget))
        (defparameter *sphere* (vector \"sphere\" #(0 0 9) #(1 0 0) 1.5))
        (defparameter *arrow* (vector \"arrow\" #(1 2 7) #(30 3 3) #(1 0 1) 1.0))
        (nglv::add-shape *v* (vector *sphere* *arrow*) :name \"my_shape\")
        "
  (%remote-call self "addShape"
                :target "Widget"
                :args (list name shapes)))

(defmethod add-representation ((self nglwidget) repr-type &rest kwargs &key (use-worker nil use-worker-p) (selection "all"))
  "Add structure representation (cartoon, licorice, ...) for given atom selection.

        Parameters
        ----------
        repr_type : str
            type of representation. Please see {ngl_url} for further info.
        selection : str or 1D array (atom indices) or any iterator that returns integer, default 'all'
            atom selection
        **kwargs: additional arguments for representation

        Example
        -------
        >>> import nglview as nv
        >>> 
        >>> t = (pt.datafiles.load_dpdp()[:].supej = pt.load(membrane_pdb)
                trajrpose('@CA'))
        >>> w = nv.show_pytraj(t)
        >>> w.add_representation('cartoon', selection='protein', color='blue')
        >>> w.add_representation('licorice', selection=[3, 8, 9, 11], color='red')
        >>> w

        Notes
        -----
        User can also use shortcut

        >>> w.add_cartoon(selection) # w.add_representation('cartoon', selection)
        "
  (when (string= repr-type "surface")
    (unless use-worker-p
      (setf (getf kwargs :use-worker) :false)))
  
  ;; avoid space sensitivity
  (setf repr-type (string-trim " " repr-type))
  ;; overwrite selection
  (setf selection (seq-to-string (string-trim " " selection)))
  (format *debug-io* "kwargs -> ~s~%" kwargs)
  (let* ((kwargs2 (dict-from-plist kwargs))
         (comp-assoc (assoc :component kwargs2))
         (component (prog1
                        (getf kwargs2 :component 0)
                      (remf kwargs2 :component))))
    #|for k, v in kwargs2.items():
    try:
    kwargs2[k] = v.strip()
    except AttributeError:
    # e.g.: opacity=0.4
    kwargs2[k] = v
    |#
    (let* ((params (dict-from-plist kwargs :remove '(:selection))))
      (push (cons "sele" selection) params)
      (push (cons "component_index" component) params)
;;;      (format t "kwargs -> ~s~%" params)
      (%remote-call self "addRepresentation"
                    :target "compList"
                    :args (list repr-type)
                    :kwargs params))))


(defmethod center ((widget nglwidget) &key (selection "*") (duration 0) (component 0))
  "center view for given atom selection

        Examples
        --------
        view.center(selection='1-4')
  "
  (%remote-call widget "autoView"
                :target "compList"
                :args (list selection duration)
                :kwargs (list (cons "component_index" component))))
  
(defmethod jupyter-widgets:on-trait-change ((object nglwidget) type (name (eql :%image-data)) old new)
  (declare (ignore type name old))
  ;;;(setf (_b64value (widget-image object)) new)
  (when (and (slot-boundp object '%hold-image) (hold-image object))
    (setf (image-array object) (concatenate 'string (image-array object) new))))

(defmethod render-image ((widget nglwidget) &key (frame nil) (factor 4) (antialias t) (trim nil) (transparent nil))
  (when frame
    (setf (frame widget) frame))
  (let ((params (list (cons "factor" factor)
                      (cons "antialias" antialias)
                      (cons "trim" trim)
                      (cons "transparent" transparent))))
    (%remote-call widget
                  "_exportImage"
                  :target "Widget"
                  :kwargs params))
  (values))

(defmethod download-image ((widget nglwidget) &key (filename "screenshot.png")
                                                (factor 4)
                                                (antialias t)
                                                (trim nil)
                                                (transparent nil))
  (let ((params (list (cons "factor" factor)
                      (cons "antialias" antialias)
                      (cons "trim" trim)
                      (cons "transparent" transparent))))
    (%remote-call widget
                  "_downloadImage"
                  :target "Widget"
                  :args (list filename)
                  :kwargs params))
  (values))

(defmethod jupyter-widgets:on-custom-message ((widget nglwidget) content buffers)
  (check-type buffers array)
  (jupyter:inform :info nil  "%ngl-handle-message in process ~s received content: ~s~%" (bordeaux-threads:current-thread) content)
  (setf (ngl-msg widget) content)
  (jupyter:inform :info nil "Just set ngl-msg to content~%")
  (let ((msg-type ([] content "type")))
    (jupyter:inform :info nil "    custom message msg-type -> ~s~%" msg-type)
    (cond
      ((string= msg-type "request_frame")
       (incf (frame widget) (%step (player widget)))
       (if (>= (frame widget) (count widget))
           (setf (frame widget) 0)
           (if (< (frame widget) 0)
               (setf (frame widget) (1- (count widget))))))
      ((string= msg-type "repr_parameters")
       (let* ((data-dict (dict-lookup "data" (ngl-msg widget))))
         (error "Finish implementing repr_parameters")))
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
                                        ;
      ((string= msg-type "request_loaded")
       (jupyter:inform :info nil "      handling request_loaded~%")
       (unless (loaded widget)
         (setf (loaded widget) nil))
       (setf (loaded widget) (eq ([] content "data") :true))
       (jupyter:inform :info nil "(loaded widget) -> ~a    ([] content \"data\") -> ~s~%" (loaded widget) ([] content "data")))
      ((string= msg-type "request_repr_dict")
       (setf (ngl-repr-dict widget) (dict-lookup "data" (ngl-msg widget))))
      ((string= msg-type "stage_parameters")
       (setf (ngl-full-stage-parameters widget) (dict-lookup "data" (ngl-msg widget))))
      ((string= msg-type "async_message")
       (jupyter:inform :info nil "%ngl-handle-msg - received async_message~%")
       (when (string= ([] content "data") "ok")
         (jupyter:inform :info nil "    setting event~%")
         (pythread:event-set (event widget))))
      (t
       (jupyter:inform :info nil "Handle ~a custom message with content: ~s~%" msg-type content))))
  (jupyter:inform :info nil "Leaving %ngl-handle-msg~%"))
    
#|    def _load_data(self, obj, **kwargs):
  '''

  Parameters
  ----------
  obj : nglview.Structure or any object having 'get-structure-string' method or
  string buffer (open(fn).read())
  '''
  kwargs2 = _camelize_dict(kwargs)

  try:
  is_url = FileManager(obj).is_url
  except NameError:
  is_url = False

  if 'defaultRepresentation' not in kwargs2:
  kwargs2['defaultRepresentation'] = True

  if not is_url:
  if hasattr(obj, 'get-structure-string'):
  blob = obj.get-structure-string()
  kwargs2['ext'] = obj.ext
  passing_buffer = True
  binary = False
  else:
  fh = FileManager(obj,
ext=kwargs.get('ext'),
compressed=kwargs.get('compressed'))
  # assume passing string
  blob = fh.read()
  passing_buffer = not fh.use_filename

  if fh.ext is None and passing_buffer:
  raise ValueError('must provide extension')

  kwargs2['ext'] = fh.ext
  binary = fh.is_binary
  use_filename = fh.use_filename

  if binary and not use_filename:
  # send base64
  blob = base64.b64encode(blob).decode('utf8')
  blob_type = 'blob' if passing_buffer else 'path'
  args=[{'type': blob_type, 'data': blob, 'binary': binary}]
  else:
  # is_url
  blob_type = 'url'
  url = obj
  args=[{'type': blob_type, 'data': url, 'binary': False}]

  name = py_utils.get_name(obj, kwargs2)
  self._ngl_component_names.append(name)
  self._remote_call("loadFile",
target='Stage',
args=args,
kwargs=kwargs2)
  |#

(defmethod %request-repr-parameters ((widget nglwidget) &key (component 0) (repr-index 0))
  (%remote-call widget
                "requestReprParameters"
                :target "Widget"
                :args (list component repr-index))
  (values))


(defmethod add-structure ((self nglwidget) structure &rest kwargs)
  (jupyter:inform :info nil "In add-structure  (loaded self) -> ~a   (already-constructed self) -> ~a~%" (loaded self) (already-constructed self))
  (if (not (typep structure 'Structure))
      (error "~s is not an instance of Structure" structure))
  (apply '%load-data self structure kwargs)
  (setf (ngl-component-ids self) (append (ngl-component-ids self) (list (id structure))))
  (when (> (n-components self) 1)
    (center self :component (- (length (ngl-component-ids self)) 1)))
  (%update-component-auto-completion self)
  structure)

(defmethod add-trajectory ((widget nglwidget) trajectory &rest kwargs)
  (jupyter:inform :info nil "entered add-trajectory~%")
  (let (#+(or)(backends *BACKENDS*)
        (package-name nil))
    ;;; Do stuff with backends
    (let ((trajectory trajectory))
      (apply '%load-data widget trajectory kwargs)
      (setf (shown trajectory) t)
      (setf (trajlist widget) (append (trajlist widget) (list trajectory)))
      (%update-count widget)
      (setf (ngl-component-ids widget) (append (ngl-component-ids widget) (list (id trajectory))))
      (%update-component-auto-completion widget)
      widget)))

(defmethod add-pdbid ((widget nglwidget) pdbid)
  (error " I want something like thif but what is .format(pdbid)??(add-component widget rcsb://{}.pdb.format(pdbid)"))


(defmethod add-component ((widget nglwidget) filename &rest kwargs)
  (jupyter:inform :info nil "entered add-component~%")
  (apply '%load-data widget filename kwargs)
  (append (ngl-component-ids widget) (list (make-uuid t)))
  (%update-component-auto-completion widget))

(defmethod %load-data ((widget nglwidget) obj &key kwargs)
  (jupyter:inform :info nil "entered %load-data~%")
  (check-type kwargs list)
  (let* ((kwargs2 (camelize-dict kwargs))
         (is-url (is-url (make-instance 'file-manager :src obj)))
         passing-buffer binary use-filename blob
         args blob-type)
    (unless (dict-entry "defaultRepresentation" kwargs2)
      (setf kwargs2 (dict-set-or-push "defaultRepresentation" kwargs2 :true)))
    (if (null is-url)
        (let ((structure-string (get-structure-string obj)))
          (if structure-string
              (setf blob structure-string
                    kwargs2 (dict-set-or-push "ext" kwargs2 (ext obj))
                    passing-buffer t
                    use-filename nil
                    binary :false)
              (error "Handle file-manager loads"))
          (if (and (eq binary :true) (not use-filename))
              (error "Handle blob decoding of base64 files"))
          (setf blob-type (if passing-buffer "blob" "path"))
          (setf args (list (list (cons "type" blob-type)
                                 (cons "data" blob)
                                 (cons "binary" binary)))))
        (setf blob-type "url"
              url obj
              args (list (list (cons "type" blob-type)
                               (cons "data" url)
                               (cons "binary" :false)))))
    (let ((name (get-name obj :dictargs kwargs2)))
      (setf (ngl-component-names widget) (append (ngl-component-names widget) (cons name nil)))
      (jupyter:inform :info nil "About to %remote-call widget loadFile~%")
      (jupyter:inform :info nil "  args: ~a~%" args)
      (jupyter:inform :info nil "  kwargs: ~a~%" kwargs)
      (%remote-call widget "loadFile"
                    :target "Stage"
                    :args args
                    :kwargs kwargs2)))
  (jupyter:inform :info nil "leaving %load-data~%"))
          
(defmethod remove-component ((widget nglwidget) c)
  (let ((component-id (if (typep component-id 'component-viewer)
                          (progn
                            (setf (view c) nil)
                            (id c))
                          c)))
    (%clear-component-auto-completion widget)
    (if (trajlist widget)
        (loop for traj in (trajlist widget)
              do (if (equal (id traj) component-id)
                     (remove traj (trajlist widget) :test #'equal))))
    (let ((component-index (aref (ngl-component-ids widget) component-id)))
      (remove component-id (ngl-component-ids widget) :test #'equal)
      (remove component-index (ngl-component-names))
      (error "Should that have been pop not remove???")
      (%remote-call widget
                    "removeComponent"
                    :target "Stage"
                    :args (list component-index)))))

(defmethod %remote-call ((widget nglwidget) method-name &key (target "Widget") args kwargs)
  "call NGL's methods from Common Lisp
        
        Parameters
        ----------
        method_name : str
        target : str, (member \"Stage\" \"Viewer\" \"compList\" \"StructureComponent\")
        args : list
        kwargs : alist
            if target is \"compList\", \"component_index\" could be passed
            to specify which component will call the method.

        Examples
        --------
        (%remote-call view \"loadFile\" :args '(\"1L2Y.pdb\")
                          :target \"Stage\" :kwargs '((\"defaultRepresentation\" . :true)))

        # perform centerView for 1-th component
        # component = Stage.compList[1];
        # component.centerView(true, \"1-12\");
        (%remote-call view \"centerView\"
                          :target \"component\"
                          :args (list :true, \"1-12\" )
                          :kwargs '((\"component_index\" . 1)))
        "
  (check-type args list)
  (check-type kwargs list)              ; alist
  (jupyter:inform :info nil "entered %remote-call ~a~%" method-name)
  (let ((msg (jsown:new-js
               ("target" target)
               ("type" "call_method")
               ("methodName" method-name)
               ("args" args)))
        (component-index (assoc "component_index" kwargs :test #'string=))
        (repr-index (assoc "repr_index" kwargs :test #'string=)))
    (when component-index
      (setf (jsown:val msg "component_index") (cdr component-index))
      (setf kwargs (remove component-index kwargs)))
    (when repr-index
      (setf (jsown:val msg "repr_index") (cdr repr-index))
      (setf kwargs (remove repr-index kwargs)))
    (setf (jsown:val msg "kwargs") kwargs)
    (let ((callback-maker (lambda (description)
                            (jupyter:inform :info nil "About to make-remote-call-callback ~a~%" description)
                            (pythread:make-remote-call-callback
                             :widget widget
                             :callback (lambda (widget)
                                         (jupyter:inform :info nil "Start %remote-call method-name -> ~a~%" method-name)
                                         (jupyter:inform :info nil "      %remote-call widget -> ~s~%" widget)
                                         (jupyter:inform :info nil "      %remote-call msg -> ~s~%" msg)
                                         (prog1
                                             (jupyter-widgets:send-custom widget msg)
                                           (jupyter:inform :info nil "    Done %remote-call method-name -> ~s~%" method-name)))
                             :method-name method-name
                             :description description
                             :ngl-msg msg))))
      (jupyter:inform :info nil "About to enqueue remote-call method-name -> ~s msg -> ~s  widget -> ~s~%"
                       method-name msg widget)
      (if (and (slot-boundp widget '%loaded) (loaded widget))
          (let ((callback (funcall callback-maker "remote-call-add")))
            (jupyter:inform :info nil "enqueing remote-call ~a~%" callback)
            (pythread:remote-call-add callback))
          (let ((callback (funcall callback-maker "before-loaded")))
            (if (slot-boundp widget '%ngl-displayed-callbacks-before-loaded-reversed)
                (push callback (ngl-displayed-callbacks-before-loaded-reversed widget))
                (setf (ngl-displayed-callbacks-before-loaded-reversed widget) (list callback)))))
      (when (not (member method-name *excluded-callback-after-firing* :test #'string=))
        (let ((callback (funcall callback-maker "after-loaded")))
          (if (slot-boundp widget '%ngl-displayed-callbacks-after-loaded-reversed)
              (push callback (ngl-displayed-callbacks-after-loaded-reversed widget))
              (setf (ngl-displayed-callbacks-after-loaded-reversed widget) (list callback)))))))
  (jupyter:inform :info nil "leaving %remote-call ~a~%" method-name)
  t)


(defmethod %get-traj-by-id ((widget nglwidget) itsid)
  (loop for traj in (trajlist widget)
     do
       (if (equal (id traj) itsid)
           (return traj)))
  nil)

(defmethod hide ((self ComponentViewer))
  "set invisibility for given components (by their indices)"
  (%remote-call (%view self)
                "setVisibility"
                :target "compList"
                :args (list nil)
                :kwargs (list (cons "component_index" (%index self))))
  (let ((traj (%get-traj-by-id (%view self) (id self))))
    (if traj
        (setf (shown traj) nil))
    (values)))


(defmethod show ((self ComponentViewer))
  "set invisibility for given components (by their indices)"
  (%remote-call (%view self)
                "setVisiblity"
                :target "compList"
                :args (list t)
                :kwargs (list (cons "component_index" (%index self))))
  (let ((traj (%get-traj-by-id (%view self) (id self))))
    (if traj
        (setf (shown traj) t))
    (values)))


(defmethod show-only ((self nglwidget) &optional (indices "all"))
  (error "Finish show-only")
  #|
  def show_only(self, indices='all'):
  """set visibility for given components (by their indices)

        Parameters
        ----------
        indices : {'all', array-like}, component index, default 'all'
        """
  traj_ids = set(traj.id for traj in self._trajlist)

  if indices == 'all':
  indices_ = set(range(self.n_components))
  else:
  indices_ = set(indices)

  for index, comp_id in enumerate(self._ngl_component_ids):
  if comp_id in traj_ids:
  traj = self._get_traj_by_id(comp_id)
  else:
  traj = None
  if index in indices_:
  args = [
  True,
  ]
  if traj is not None:
  traj.shown = True
  else:
  args = [
  False,
  ]
  if traj is not None:
  traj.shown = False

  self._remote_call(
                "setVisibility",
                target='compList',
                args=args,
                kwargs={'component_index': index})
  |#)



(defmethod %js-console ((widget nglwidget))
  (error "implement %js-console in widget.lisp"))

#|
  def _js_console(self):
  self.send(dict(type='get', data='any'))
  |#

(defmethod %get-full-params ((widget nglwidget))
  (error "Implement %get-full-params in widget.lisp"))
#|
  def _get_full_params(self):
  self.send(dict(type='get', data='parameters'))
  |#

(defmethod %display-image ((widget nglwidget))
  (error "help %display-image widget.lisp"))
#|
  def _display_image(self):
  '''for testing
  '''
  from IPython import display
  return display.Image(self._image_data)
  |#

(defmethod %clear-component-auto-completion ((widget nglwidget))
  (let ((index 0))
    (loop for id in (ngl-component-ids widget)
       do
         (let ((name (concatenate 'string "component_" (write-to-string index))))
           (incf index)
           (error "WE NEED A DELATTR IN %clear-component-auto-completion in widget.lisp")))))
#|
  def _clear_component_auto_completion(self):
  for index, _ in enumerate(self._ngl_component_ids):
  name = 'component_' + str(index)
  delattr(self, name)
  |#


(defmethod %update-component-auto-completion ((self NGLWidget))
  #+(or)(warn "Do something for %update-component-auto-completion")
  #+(or)(let ((trajids (loop for traj in (trajlist self) collect (id traj)))
              (index 0))
          (loop for cid in (ngl-component-ids widget)
             do (let ((comp (make-instance 'ComponentViewer :%view widget :%index index))
                      (name (concatenate 'string "component_" (write-to-string index))))
                  (incf index)
                  (error "We need a setattr function!!!!")
                  (error "we need an in function! Maybe we have one. %update-component-auto-completion in widget.lisp")))))

#|
  def _update_component_auto_completion(self):
  trajids = [traj.id for traj in self._trajlist]

  for index, cid in enumerate(self._ngl_component_ids):
  comp = ComponentViewer(self, index) 
  name = 'component_' + str(index)
  setattr(self, name, comp)


  if cid in trajids:
  traj_name = 'trajectory_' + str(trajids.index(cid))
  setattr(self, traj_name, comp)
  |#


(defmethod %-getitem-- ((widget nglwidget) index)
  "return ComponentViewer"
  (let ((positive-index (get-positive-index py-utils index (length (ngl-component-ids widget)))))
    (make-instance 'ComponentViewer :%view widget :%index positive-index))
  (error "Help! We don't have a py-utils thingy in %-getitem-- in widget.lisp"))


(defmethod %-iter-- ((widget nglwidget))
  "return ComponentViewer"
  (let ((index 0))
    (loop for item in (ngl-component-ids widget))
    (error "Implementer %-iter-- in widget.lisp")))
#|
  def __iter__(self):
  """return ComponentViewer
        """
  for i, _ in enumerate(self._ngl_component_ids):
  yield self[i]
  |#    

(defmethod detach ((widget nglwidget) &key (split nil))
  "detach player from its original container."
  (if (not (loaded widget))
      (error "must display view first"))
  (if split
      (js-utils-move-notebook-to-the-right js-utils))
  (%remote-call widget "setDialog" :target "Widget"))








;;; ----------------------------------------------------------------------------------------------------





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          

;;;Starting from the bottom down below. SCROLL!


(defmethod %set-place-proxy ((widget nglwidget) widget)
  (setf (child (%place-proxy widget)) widget)
  (values))

(defmacro pop-from-alist (key alist)
  (let ((k (gensym "KEY")) (a (gensym "ALIST"))
        (pair (gensym "PAIR")))
    `(let* ((,k ,key)
            (,a ,alist)
            (,pair (assoc ,k ,a)))
       (when ,pair
         (prog1 (cdr ,pair)
         (setf ,alist (remove ,pair ,a)))))))
(defmacro pop-from-hash-table (key table)
  (let ((k (gensym "KEY")) (tab (gensym "TABLE")))
    `(let ((,k ,key) (,tab ,table))
       (prog1 (gethash ,k ,tab)
         (remhash ,k ,tab)))))

(defmethod jupyter:on-comm-close :after ((widget nglwidget) data metadata buffers)
  (declare (ignore data metadata buffers))
  ;; (bordeaux-threads:destroy-thread (remote-call-thread widget))
  (when (handle-msg-thread widget)
    (bordeaux-threads:destroy-thread (handle-msg-thread widget)))
  ;;; FIXME: Kill handle-msg-thread 
  )


(defmethod %update-ngl-repr-dict ((self nglwidget))
  "Send a request to the frontend to send representation parameters back"
  (jupyter:inform :info nil "Called %update-ngl-repr-dict~%")
  (%remote-call self
                "request_repr_dict"
                :target "Widget"))


(defmethod representations-setter ((widget nglwidget) reps)
  (dolist (ngl-component-ids widget)
    (set-representations widget reps))
  (values))

(defmethod camera-setter ((widget nglwidget) value)
  (setf (camera-str widget) value)
  (%remote-call widget
                "setParameters"
                :target "Stage"
                :kwargs (list (cons "cameraType" (camera-str wiget))))
  (values))

