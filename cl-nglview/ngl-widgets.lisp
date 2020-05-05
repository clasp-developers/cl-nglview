(in-package :nglv)

;;; Subclass jupyter-widgets to add %ngl-name

(defclass button (jupyter-widgets:button)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass text (jupyter-widgets:text)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass int-slider (jupyter-widgets:int-slider)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass dropdown (jupyter-widgets:dropdown)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass tab (jupyter-widgets:tab)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass checkbox (jupyter-widgets:checkbox)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass float-text (jupyter-widgets:float-text)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass int-text (jupyter-widgets:int-text)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass toggle-button (jupyter-widgets:toggle-button)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass color-picker (jupyter-widgets:color-picker)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass box (jupyter-widgets:box)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass vbox (jupyter-widgets:v-box)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

(defclass hbox (jupyter-widgets:h-box)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass jupyter-widgets:trait-metaclass))

