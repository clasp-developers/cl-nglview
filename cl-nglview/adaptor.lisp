(in-package :nglview)

;;;Register backend is something python does but we don't need

; p:FileStructure
(defclass file-structure (structure)
  ((path
     :accessor path
     :initarg :path
     :initform nil)
   (fm
     :accessor fm
     :initform nil))
  (:default-initargs
    :ext nil))

(defmethod initialize-instance :after ((instance file-structure) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (path fm ext) instance
    (unless ext
      (setf ext (pathname-type (pathname path))))
    (setf fm (make-instance 'file-manager :src path)))
  instance)

; p:get_structure_string
(defmethod get-structure-string ((instance file-structure))
  (read-file (fm instance)))


; p:TextStructure
(defclass text-structure (structure)
  ((text
     :accessor text
     :initarg :text
     :initform "")))

; p:get_structure_string
(defmethod get-structure-string ((self text-structure))
  (text self))


(defclass Rdkitstructure (structure)
  ((rdkit-mol :initarg :rdkit-mol :accessor rdkit-mol :initform nil)
   (ext :initarg :ext :accessor ext
	:initform "pdb")
   (path :accessor path :initform "")
   (params :accessor params :type list :initform ())))

(defmethod get-structure-string ((self RdkitStructure))
  (error "adaptor::get-structure-string Implement me!!!!"))
#|
 from rdkit import Chem
        fh = StringIO(Chem.MolToPDBBlock(self.-rdkit_mol))
        return fh.read()
|#


; p:PdbIdStructure
(defclass pdb-id-structure (structure)
  ((pdbid
     :accessor pdbid
     :initarg :pdbid
     :initform nil)
   (url
     :accessor url
     :initarg :url
     :initform "http://files.rcsb.org/view/~A.~A"))
  (:default-initargs
    :ext "pdb"))

; p:get-structure-string
(defmethod get-structure-string ((instance pdb-id-structure))
  (let ((drakma:*body-format-function* (lambda (headers external-format-in)
                                         (declare (ignore headers external-format-in))
                                         :utf-8)))
    (drakma:http-request (format nil (url instance) (pdbid instance) (ext instance)))))


(defclass ASEstructure (structure)
  ((ase-atoms :initarg :ase-atoms :accessor ase-atoms
	      :initform nil)
   (path :accessor path
	 :initform "")
   (params :initarg params :accessor params :type list :initform ())
   (ext :initarg :ext :accessor ext :initform "pdb")))

(defmethod get-structure-string ((self ASEStructure))
  (error "ASEStructure::get-structure-string help!!"))
#|
  def get-structure-string(self):
        with tempfolder():
            self.-ase_atoms.write('tmp.pdb')
            return open('tmp.pdb').read()
|#


(defclass Simpletrajtrajectory (trajectory structure)
  ((path :initarg :path :accessor path :initform nil)
   (structure-path :initarg :structure-path :accessor structure-path :initform path)
   (traj-cache :accessor traj-cache :initform nil);HELP!!! Please help
   (ext :accessor ext :initform nil) ;HELP!!! Please help me
   (params :accessor params :type list :initform nil)
   (trajectory :accessor trajectory :initform nil)
   (id :accessor id :initform (jupyter:make-uuid))))

(defmethod get-coordinates ((self SimpletrajTrajectory) index)
  (error "Implement me!!! get-coordinates SimpletrajTrajectory!"))
#|
    def get-coordinates(self, index):
        traj = self.traj_cache.get(os.path.abspath(self.path))
        frame = traj.get_frame(index)
        return frame["coords"]
|#

(defmethod get-structure-string ((self SimpletrajTrajectory))
  (error "help get-structure-string of simpletrajtrajectory"))
  ;;;return open(self._structure_path).read()

(defmethod n-frames ((self SimpletrajTrajectory))
  (error "n-frames simpletrajtrajectory needs some help"))
;;; traj = self.traj_cache.get(os.path.abspath(self.path))
;;; return traj.numframes

(defclass MDTrajtrajectory (trajectory structure)
  ((trajectory :initarg :trajectory :accessor trajectory
	       :initform nil)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (id :accessor id :initform (jupyter:make-uuid))))

(defmethod get-coordinates ((self MDTrajTrajectory) index)
  (* 10 (aref (xyz (trajectory self)) index)))

(defmethod n-frames ((self MDTrajTrajectory))
  (n-frames (trajectory self)))

(defmethod get-structure-string ((self MDTrajTrajectory))
  (error "Help the get-structure-String MDTrajTrajectory"))
#|
 def get-structure-string(self):
        fd, fname = tempfile.mkstemp()
        self.trajectory[0].save_pdb(fname)
        pdb_string = os.fdopen(fd).read()
        # os.close( fd )
        return pdb_string
|#

(defclass PyTrajtrajectory (trajectory structure)
  ((trajectory :initarg :trajectory :accessor trajectory
	       :initform nil)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (id :accessor id :initform (jupyter:make-uuid))))

(defmethod get-coordinates ((self PyTrajTrajectory) index)
  (xyz (aref (trajectory self) index)))

(defmethod n-frames ((self PyTrajTrajectory))
  (n-frames (trajectory self)))

(defmethod get-structure-string ((self PyTrajTrajectory))
  (error "PyTrajTrajectory get-structure-string error"))
#|
    def get-structure-string(self):
        fd, fname = tempfile.mkstemp(suffix=".pdb")
        self.trajectory[:1].save(fname, format="pdb", overwrite=True)
        pdb_string = os.fdopen(fd).read()
        # os.close( fd )
        return pdb_string

|#
;;;There's something fishy goin on here. Check python code listed below.
(defclass ParmEdtrajectory (trajectory structure)
  ((trajectory :initarg :trajectory :initform nil :accessor trajectory)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (xyz :accessor xyz :initform nil)
   (id :accessor id :initform (jupyter:make-uuid))
   (only-save-1st-model :accessor only-save-1st-model :type bool :initform t)))

#|

@register_backend('parmed')
class ParmEdtrajectory(trajectory, structure):
    '''ParmEd adaptor.
    '''

    def __init__(self, trajectory):
        self.trajectory = trajectory
        self.ext = "pdb"
        self.params = {}
        # only call get_coordinates once
        self._xyz = trajectory.get_coordinates()
        self.id = str(uuid.uuid4())
        self.only_save_1st_model = True

    def get_coordinates(self, index):
        return self._xyz[index]

    @property
    def n_frames(self):
        return len(self._xyz)

    def get-structure-string(self):
        fd, fname = tempfile.mkstemp(suffix=".pdb")
        # only write 1st model
        if self.only_save_1st_model:
            self.trajectory.save(
                fname, overwrite=True,
                coordinates=self.trajectory.coordinates)
        else:
            self.trajectory.save(fname, overwrite=True)
        pdb_string = os.fdopen(fd).read()
        # os.close( fd )
        return pdb_string
|#

;(defmethod initialize-instance :after ((self ParmEdTrajectory) &key)
;;  (setf (xyz self) ((get_coordinates

(defclass MDAnalysistrajectory (trajectory structure)
  ((atomgroup :initarg :atomgroup :accessor atomgroup)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (id :accessor id :initform (jupyter:make-uuid))))

#+(or)
(defmethod get-coordinates ((self MDAnalysisTrajectory) index)
  (aref (trajectory (universe (atomgroup self))) index)
  (positions (atoms (atomgroup self))))
#+(or)
(defmethod n-frames ((self MDAnalysisTrajectory))
  (n-frames (trajectory (universe (atomgroup self)))))

(defmethod get-structure-string ((self MDAnalysisTrajectory))
  (error "help MDAnalysisTrajectory get-structure-string"))

#|
  def get-structure-string(self):
        try:
            import MDAnalysis as mda
        except ImportError:
            raise ImportError(
                "'MDAnalysisTrajectory' requires the 'MDAnalysis' package"
            )
        u = self.atomgroup.universe
        u.trajectory[0]
        f = mda.lib.util.NamedStream(StringIO(), 'tmp.pdb')
        atoms = self.atomgroup.atoms
        # add PDB output to the named stream
        with mda.Writer(f, atoms.n_atoms, multiframe=False) as W:
            W.write(atoms)
        # extract from the stream
        pdb_string = f.read()
        return pdb_string

|#

(defclass HTMDtrajectory (trajectory)
  ((mol :initarg :mol :accessor mol)
   (ext :accessor ext :initform "pdb")
   (params :accessor params :type list :initform ())
   (id :accessor id :initform (jupyter:make-uuid))))

(defmethod get-coordinates ((self HTMDTrajectory) index)
  (error "help get-coordinates HTMDTrajectory"))
#|
    def get_coordinates(self, index):
        return np.squeeze(self.mol.coords[:, :, index])
|#

(defmethod n-frames ((self HTMDTrajectory))
  (n-frames (mol self)))

(defmethod get-structure ((self HTMDTrajectory))
  (error "help get-structure of HTMDTrajectory"))
#|
    def get-structure-string(self):
        import tempfile
        fd, fname = tempfile.mkstemp(suffix='.pdb')
        self.mol.write(fname)
        pdb_string = os.fdopen(fd).read()
        # os.close( fd )
        return pdb_string
|#
