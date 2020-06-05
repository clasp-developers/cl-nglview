(in-package :nglv)

; p:ComponentViewer
(defclass component-viewer ()
  ((%view
     :accessor %view
     :initarg :%view
     :initform nil)
   (%index
     :accessor %index
     :initarg :%index
     :initform nil)))

#+(or)
(defmethod initialize-instance :after ((self component-viewer))
  (%add-repr-method-shortcut self (%view self))
  (%borrow-attribute self (%view self) (list "clear_representations"
                                             "_remove_representations_by_name"
                                             "_update_representations_by_name"
                                             "center_view"
                                             "center"
                                             "clear"
                                             "set_representations")
                     (list "get-structure-string"
                           "get_coodinates"
                           "n_frames")))

(defmethod id ((self component-viewer))
  (aref (ngl-component-ids (%view self)) (%index self)))

(defmethod add-representations ((self component-viewer) repr-type &optional (selection "all") &rest kwargs &key &allow-other-keys)
; TWB: Seems broken
;  (setf (aref kwargs "component") (%index self))
  (add-representation (%view self) :repr-type repr-type :selection selection kwargs))


(defmethod %borrow-attribute ((self component-viewer) view attributes &key (trajectory-atts nil))
  (declare (ignore trajectory-atts))
  (let ((traj (%get-traj-by-id view (id self))))
    (declare (ignore traj))
    (loop for attname in attributes
       do
         (let ((view-att nil))
           (declare (ignore view-att)))))
  (error "Help me!!!"))

