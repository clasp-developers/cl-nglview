(in-package :nglv)

; p:get_repr_names_from_dict
(defun get-repr-names-from-dict (repr-dict component)
  (handler-case
      (mapcar (lambda (pair)
                (jsown:val (cdr pair) "type"))
              (jsown:val repr-dict (write-string component)))
    (error () nil)))

