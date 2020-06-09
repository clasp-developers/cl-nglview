(in-package :nglview)

(defun show-pdbid (pdbid &rest kwargs &key trajectory gui &allow-other-keys)
  (let ((structure (if trajectory
                     (make-instance 'pdb-id-trajectory :pdbid pdbid)
                     (make-instance 'pdb-id-structure :pdbid pdbid))))
    (values (apply #'make-nglwidget :structure structure :gui-style (when gui "ngl") kwargs)
            (id structure)
            structure)))

(defun show-url (url &rest kwargs &key &allow-other-keys)
  (let ((view (make-nglwidget)))
    (values view
            (apply #'add-component view url kwargs))))

(defun show-text (text &rest kwargs &key &allow-other-keys)
  (let ((structure (make-instance 'text-structure :text text)))
    (values (apply #'make-nglwidget :structure structure kwargs)
            (id structure)
            (structure))))

(defun show-ase (ase-atoms &rest kwargs &key &allow-other-keys)
  (let ((structure (make-instance 'ASEStructure :ase-atoms ase-atoms)))
    (values (apply #'make-nglwidget :structure structure kwargs)
            (id structure)
            (structure))))

(defun show-structure-file (path &rest kwargs &key &allow-other-keys)
  (let ((structure (make-instance 'file-structure :path path)))
    (values (apply #'make-nglwidget :structure structure kwargs)
            (id structure)
            (structure))))

(defun show-file (path &rest kwargs &key &allow-other-keys)
  (let ((view (make-nglwidget)))
    (values view
            (apply #'add-component view path kwargs))))

(defun show-simpletraj (traj &rest kwargs &key &allow-other-keys)
  (apply #'make-nglwidget :traj traj kwargs))

(defun show-mdtraj (mdtraj-trajectory &rest kwargs &key &allow-other-keys)
  (let ((structure-trajectory (apply #'make-instance 'MDTrajTrajectory mdtraj-trajectory)))
    (apply (make-nglwidget :structure-trajectory structure-trajectory kwargs))))

(defun show-pytraj (pytraj-trajectory &rest kwargs &key &allow-other-keys)
  (let ((trajlist nil))
    (if (or (typep pytraj-trajectory 'list) (typep pytraj-trajectory 'tuple))
      (setf trajlist pytraj-trajectory)
      (setf trajlist (list pytraj-trajectory)))
    (setf trajlist (loop for traj in trajlist
		      collect (make-instance 'PyTrajTrajectory :traj traj)))
    (apply #'make-nglwidget :trajlist trajlist kwargs)))

(defun show-parmed (parmed-structure &rest kwargs &key &allow-other-keys)
  (let ((structure-trajectory (make-instance 'ParmEdTrajectory :trajectory parmed-structure)))
    (apply #'make-nglwidget :structure structure-trajectory kwargs)))

(defun show-rdkit (rdkit-mol &rest kwargs &key &allow-other-keys)
  (declare (ignore rdkit-mol kwargs))
  (error "show::show-rdkit error!!! Implement me!!!!"))
#|
def show_rdkit(rdkit_mol, **kwargs):
    '''Show rdkit's Mol.

    Parameters
    ----------
    rdkit_mol : rdkit.Chem.rdchem.Mol
    kwargs : additional keyword argument

    Examples
    --------
    >>> import nglview as nv
    >>> from rdkit import Chem
    >>> from rdkit.Chem import AllChem
    >>> m = Chem.AddHs(Chem.MolFromSmiles('COc1ccc2[C@H](O)[C@@H](COc2c1)N3CCC(O)(CC3)c4ccc(F)cc4'))
    >>> AllChem.EmbedMultipleConfs(m, useExpTorsionAnglePrefs=True, useBasicKnowledge=True)
    >>> view = nv.show_rdkit(m)
    >>> view

    >>> # add component m2
    >>> # create file-like object
    >>> fh = StringIO(Chem.MolToPDBBlock(m2))
    >>> view.add_component(fh, ext='pdb')

    >>> # load as trajectory, need to have ParmEd
    >>> view = nv.show_rdkit(m, parmed=True)
    '''
    from rdkit import Chem
    fh = StringIO(Chem.MolToPDBBlock(rdkit_mol))

    try:
        use_parmed = kwargs.pop("parmed")
    except KeyError:
        use_parmed = False

    if not use_parmed:
        view = nglwidget()
        view.add_component(fh, ext='pdb', **kwargs)
        return view
    else:
        import parmed as pmd
        parm = pmd.load_rdkit(rdkit_mol)
        parm_nv = ParmEdTrajectory(parm)

        # set option for ParmEd
        parm_nv.only_save_1st_model = False

        # set option for NGL
        # wait for: https://github.com/arose/ngl/issues/126
        # to be fixed in NGLView
        # parm_nv.params = dict(firstModelOnly=True)
        return nglwidget(parm_nv, **kwargs)
|#

(defun show-mdanalysis (atomgroup &rest kwargs &key &allow-other-keys)
  (let ((structure-trajectory (make-instance 'MDAnalysisTrajectory :atomgroup atomgroup)))
    (apply #'make-nglwidget :structure structure-trajectory kwargs)))

(defun show-htmd (mol &rest kwargs &key &allow-other-keys)
  (let ((structure-trajectory (make-instance 'HTMDTrajectory :mol mol)))
    (apply #'make-nglwidget :structure structure-trajectory kwargs)))

#+(or)
(defun demo (&rest kwargs &key &allow-other-keys)
  (show-structure-file datafiles.PDB kwargs))

;;I don't really know if this is correct but in the demo they just do demo() so there are no args so like do i need the *args part?? I'm thinkin no so let's wing it.
#|
def demo(*args, **kwargs):
    from nglview import show_structure_file
    return show_structure_file(datafiles.PDB, *args, **kwargs)
|#
