(in-package :nglv)

;;; Subclass cl-ipywidgets to add %ngl-name

(defclass button (cl-ipywidgets:button)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass text (cl-ipywidgets:text)
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass int-slider (cl-ipywidgets:int-slider) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass dropdown (cl-ipywidgets:dropdown) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass tab (cl-ipywidgets:tab) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass checkbox (cl-ipywidgets::checkbox) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass float-text (cl-ipywidgets::float-text) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass int-text (cl-ipywidgets::int-text) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass toggle-button (cl-ipywidgets:toggle-button) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass color-picker (cl-ipywidgets:color-picker) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))


(defclass box (cl-ipywidgets:box) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass vbox (cl-ipywidgets:vbox) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

(defclass hbox (cl-ipywidgets:hbox) 
  ((%ngl-name :initform nil :initarg :%ngl-name :accessor %ngl-name))
  (:metaclass traitlets:traitlet-class))

