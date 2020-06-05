(in-package :nglv)

; p:get_repr_names_from_dict
(defun get-repr-names-from-dict (repr-dict component)
  (handler-case
      (mapcar (lambda (pair)
                (jsown:val (cdr pair) "type"))
              (jsown:val repr-dict (write-string component)))
    (error () nil)))

; p:FileManager
(defclass file-manager ()
  ((src
     :accessor src
     :initarg :src)
   (cwd
     :accessor cwd
     :initarg :cwd)
   (compressed
     :accessor compressed
     :initarg :compressed
     :initform nil)
   (ext
     :accessor ext
     :initarg :ext
     :initform nil)
   (%unzip-backend :initarg :unzip-backend :accessor unzip-backend
                   :initform :a-dictionary-maps-extensions-to-backends))
  (:documentation   "FileManager is for internal use.

    If file is in the current folder or subfoler, use filename
    If not, open content

    Parameters
    ----------
    src : str or file-like object
        filename
    compressed : None or bool, default None
        user can specify if the given file is compressed or not.
        if None, FileManager will detect based on file extension
    "))

; p:read
(defun read-file (instance)
  (alexandria:read-file-into-string (src instance)))

(defun is-compressed (instance)
  nil)

(defun compressed-ext (instance)
  nil)

(defun use-filename (instance)
  t)

(defmethod ext ((instance file-manager))
  nil)

(defun is-filename (instance)
  t)

(defun use-binary (instance)
  nil)

; p:is_url
(defun is-url (instance)
  (handler-case
      (purl:url-scheme (src instance))
    (purl::malformed-url ()
      nil)))

