(in-package :nglview)

(defclass base-widget (jw:dom-widget)
  ((%msg-q ; _msg_q
     :accessor %msg-q
     :initform (clext.queue:make-queue 'base-widget))
   (%msg-ar ; p:_msg_ar
     :accessor %msg-ar
     :initform nil
     :trait :list)
   (%ready ; p:_ready
     :accessor %ready
     :initform nil
     :trait :bool)))

(defmethod on-trait-change (instance (name (eql :%ready)) type old-value new-value source)
  (declare (ignore name type old-value source))
  (when new-value
    (with-slots (%msg-q) instance
      (do ()
          ((clext:queue-emptyp %msg-q))
        (send instance (clext.dequeue %msg-q))))))
