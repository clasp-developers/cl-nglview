(in-package :nglv)
;;;https://github.com/drmeister/spy-ipykernel/blob/master/nglview/layout.py#L8

(defun make-form-item-layout ();Alright I think I know what I'm doing here
  (make-instance 'jupyter-widgets:layout :display "flex" :flex-flow "row"
		 :justify-content "space-between"))

(defun %make-box-layout(&optional (width "100%"))
  (make-instance 'jupyter-widgets:layout :display "flex" :flex-flow "column"
		 :align-items "stretch" :width width))

(defun %relayout (box form-item-layout)
  (let ((form-items ()) (box2 nil))
    (loop for kid across (jupyter-widgets:widget-children box)
	 do
	 (let ((label-value ""))
	   (if (and (description kid) (not (or (typep kid 'button) (typep kid 'toggle-button))))
		(setf label-value (description kid) (description kid) ""))
	   (if (typep kid 'button)
		(setf box2 (make-instance 'jupyter-widgets:box :children (list kid) :layout form-item-layout))
		(setf box2 (make-instance 'jupyter-widgets:box :children (list (make-instance 'jupyter-widgets:label :value label-value) kid) :layout form-item-layout)))
	   (push box2 form-items)))))

(defun %relayout-master (box &key (width "20%"))
  (let* ((old-children (;;What does box.children[:]??
			))
	 (form-items (%relayout box (make-form-item-layout)))
	 (form (apply #'make-instance 'jupyter-widgets:box form-items :layout (%make-box-layout(:width width)))))
    (setf (%ngl-children form) old-children)
    form))

(defun %make-autofit (box)
  (jupyter:inform :info nil "autofit ~A" box)
  (setf (jupyter-widgets:widget-flex (jupyter-widgets:widget-layout box)) "1 1 auto"
        (jupyter-widgets:widget-width (jupyter-widgets:widget-layout box)) "auto")
  box)

(defun %make-delay-tab (box-factory &key (selected-index 0))
  (let ((tab (make-instance 'jupyter-widgets:tab
			    :children (loop for (box) in box-factory
					 collect (make-instance 'jupyter-widgets:box))))
	(i 0))
    
    (loop for (dummy . title) in box-factory
       do
	 (set-title tab i title)
	 (incf i))

    (if (not (children (aref (children tab) selected-index)))
	(setf (selected-index tab) -1))

    (flet ((on-update-selected-index (widget type name old new source)
       (declare (ignore widget type name old source))
	     (let ((index new))
	       (if (not (jupyter-widgets:widget-children (nth (jupyter-widgets:widget-children tab) index)))
		   (setf (jupyter-widgets:widget-children (nth (jupyter-widgets:widget-children tab) index)) (error "I don't know what to set it to")))
	       )))
      (jupyter-widgets:observe tab :selected-index on-update-selected-index)
      (setf (selected-index tab) selected-index)
      tab)))


    
 #|   """

    Parameters
    ----------
    box_factory : list of (func, tab_name)

    Example of box_factory: [(_make_gen_box, 'General'),
                             (_make_repr_box, 'Representation')]
    """

    tab = Tab([Box() for box, _ in box_factory])
    [tab.set_title(i, title) for i, (_, title) in enumerate(box_factory)]

    # trick
    if not tab.children[selected_index].children:
        tab.selected_index = -1

    def on_update_selected_index(change):
        index = change['new']
        if not tab.children[index].children:
            # make widget on demand
            tab.children[index].children = [box_factory[index][0](),]

    tab.observe(on_update_selected_index, names='selected_index')

    # trigger
    tab.selected_index = selected_index

    return tab
|#
