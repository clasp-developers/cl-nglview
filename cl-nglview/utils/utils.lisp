(in-package :nglv)


(defun dict (&rest args)
  "Helper function for translating cl-jupyter-widgets to Common Lisp.
This just wraps LIST."
  (list* args))

(defun k= (key value)
  "Helper function for translating cl-jupyter-widgets to Common Lisp.
This wraps CONS."
  (cons key value))
  
