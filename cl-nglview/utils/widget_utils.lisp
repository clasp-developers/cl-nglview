(in-package :nglview)

(defun get-widget-by-name (box widget-name)
  "Search for the widget with the widget-name"
  (loop for widget across (children box)
        when (string= (ngl-name widget) widget-name)
          do (return-from get-widget-by-name widget)))


