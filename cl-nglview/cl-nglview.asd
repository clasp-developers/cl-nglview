
(asdf:defsystem #:cl-nglview
  :description "The ngl widget for common-lisp-jupyter"
  :version "0.1"
  :author "Kevin Esslinger, Alex Rose, Christian Schafmeister"
  :license "LGPL2. See LICENSE."
  :depends-on (:alexandria
               :common-lisp-jupyter
               :drakma
               :bordeaux-threads
               :purl
               :trivial-garbage)
  :serial t
  :components ((:file "packages")
               (:file "color")
               (:module "utils"
                 :serial t
                 :components ((:file "js-utils")
                              (:file "py-utils")))
               (:file "default")
               (:file "parameters")
               (:file "queue")
               (:file "utils-local")
               (:file "layout")
               (:file "shape")
               (:file "pythread")
               (:file "stage")
               (:file "base_adaptor")
               (:file "adaptor")
               (:file "representation")
               (:file "player")
               (:file "widget")
               (:file "show")))

