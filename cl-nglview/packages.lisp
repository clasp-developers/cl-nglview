(jupyter:inform :info nil "nglview  Loading packages.lisp~%")

(defpackage "CLEXT.QUEUE"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS")
  (:export "QUEUE" "QUEUEP"
           "MAKE-QUEUE"
           "QUEUE-NAME"
           "QUEUE-COUNT"
           "QUEUE-EMPTYP"
           "ENQUEUE"
           "DEQUEUE"
           "TEST-QUEUE")
  (:documentation "Implements a thread-safe message queue."))

(defpackage #:nglv
  (:use #:cl)
  (:shadow #:count #:structure)
  (:export
   #:make-nglwidget
   #:nglwidget
   #:shape
   #:remote-call-thread-queue
   #:wait-until-finished
   #:show-aggregate
   #:show-structure-file
   #:trajectory
   #:get-structure-name
   #:get-structure-string
   #:ext
   #:trajectory
   #:structure
   #:add-axes
   #:append-coordinates
   #:get-coordinates
   #:n-frames
   #:set-coordinates
   #:coordinates-dict
   #:send-binary
   #:show-pdbid
   #:*backends*
   #:clear-representations
   #:add-representation
   ))

(defpackage #:pythread
  (:use #:cl)
  (:shadow #:set)
  (:export
   #:remote-call-callback
   #:make-remote-call-callback
   #:callback
   #:method-name
   #:description
   #:fire-callback
   #:remote-call-add
   #:remote-call-thread-run
   #:event
   #:event-set
   #:clear
   #:is-set
   #:*remote-call-thread*
   #:*remote-call-thread-queue*))
