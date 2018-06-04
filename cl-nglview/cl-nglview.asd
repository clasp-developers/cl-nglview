
(asdf:defsystem #:cl-nglview
    :description "The ngl widget for cl-jupyter with widgets"
    :version "0.1"
    :author "Kevin Esslinger, Alex Rose, Christian Schafmeister"
    :license "LGPL2. See LICENSE."
  :depends-on (:cl-jupyter
               :cl-jupyter-widgets
               :bordeaux-threads
               :trivial-http)
    :serial t
    :components (
                 (:file "packages")
                 (:file "config")
                 (:file "ngl-widgets")
                 (:file "queue")
                 (:file "utils")
                 (:file "shape")
                 (:file "pythread")
                 (:file "component")
                 (:file "stage")
                 (:file "widget")
                 (:file "base_adaptor")
                 (:file "adaptor")
                 (:file "show")
                 (:file "player")
                 (:file "parameters")
                 (:file "default")
                 ))
