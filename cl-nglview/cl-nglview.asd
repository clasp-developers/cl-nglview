
(asdf:defsystem #:cl-nglview
  :description "The ngl widget for common-lisp-jupyter"
  :version "0.1"
  :author "Kevin Esslinger, Alex Rose, Christian Schafmeister"
  :license "LGPL2. See LICENSE."
  :depends-on (:alexandria
               :common-lisp-jupyter
               :bordeaux-threads
               :jsown
               :trivial-garbage
               :trivial-http)
  :serial t
  :components (
               (:file "packages")
               (:file "config")
               (:file "color")
               (:module "utils"
                 :serial t
                 :components ((:file "js-utils")
                              (:file "py-utils")))
               (:file "ngl-widgets")
               (:file "queue")
               (:file "utils-local")
               (:file "layout")
               (:file "shape")
               (:file "pythread")
               (:file "component")
               (:file "stage")
               (:file "widget")
               (:file "base_adaptor")
               (:file "adaptor")
               (:file "show")
               (:file "representation")
               (:file "player")
               (:file "parameters")
               (:file "default")
               ))
