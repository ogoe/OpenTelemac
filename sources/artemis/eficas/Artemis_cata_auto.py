
# coding: utf-8

from Accas import *
class DateJJMMAAAA:
  def __init__(self):
    self.ntuple=3

  def __convert__(self,valeur):
    if type(valeur) == types.StringType: return None
    if len(valeur) != self.ntuple: return None
    return valeur

  def info(self):
    return "Date : jj/mm/aaaa "

  __repr__=info
  __str__=info

class grma(GEOM):
  pass

import types
class Tuple:
  def __init__(self,ntuple):
    self.ntuple=ntuple

  def __convert__(self,valeur):
    if type(valeur) == types.StringType:
      return None
    if len(valeur) != self.ntuple:
      return None
    return valeur

  def info(self):
    return "Tuple de %s elements" % self.ntuple



JdC = JDC_CATA (code = 'TELEMAC',
                execmodul = None,
                )
# =======================================================================
# Catalog entry for the MAP function : c_pre_interfaceBody_mesh
# =======================================================================

# -----------------------------------------------------------------------
INPUT_OUTPUT,FILES = PROC(nom= "INPUT_OUTPUT,FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NAMES = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        GEOMETRY_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            fr = """Nom du fichier contenant le maillage du calcul a realiser.""",
            ang = """Name of the file which contains the computational mesh.""",
        ),
#       -----------------------------------
        FORTRAN_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = 'DEFAUT',
            fr = """Nom du fichier FORTRAN a soumettre.""",
            ang = """Name of the FORTRAN file used for the computation.""",
        ),
#       -----------------------------------
        STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier contenant les parametres du calcul a realiser.""",
            ang = """Name of the steering file used for the computation.""",
        ),
#       -----------------------------------
        BOUNDARY_CONDITIONS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            fr = """Nom du fichier contenant les types de conditions aux limites.
Ce fichier est construit de facon automatique par le mailleur et STBTEL
au moyen de couleurs affectees aux noeuds des frontieres du domaine
de calcul.""",
            ang = """Name of the boundary conditions file. It is automatically built
by STBTEL or by the mesh generator MATISSE.""",
        ),
#       -----------------------------------
        RESULTS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier dans lequel seront ecrits les resultats du calcul,
avec la periodicite donnee par le mot cle PERIODE POUR LES SORTIES
GRAPHIQUES.
Sur IBM, ce fichier est alloue automatiquement s''il n''existe pas,
avec les caracteristiques suivantes :
  Format d''enregistrement   : VBS
  Longueur d''enregistrement : X
  Taille de bloc            : 6204
  Nombre de pistes          : 50 en primaire, 10 en secondaire
La place memoire ainsi reservee est suffisante pour la plupart des
calculs de dimension moyenne.""",
            ang = """Name of the results file corresponding to the computations and
which contains the variables specified by the key-word
VARIABLES FOR GRAPHIC PRINTOUTS.""",
        ),
#       -----------------------------------
        BOTTOM_TOPOGRAPHY_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier eventuel contenant la bathymetrie associee au
maillage.
Si ce mot-cle est utilise, c''est cette bathymetrie qui sera
utilisee pour le calcul.""",
            ang = """Name of a potential bathymetry file. If this key-word is specified,
the bathymetry which it is defining is accounted for.""",
        ),
#       -----------------------------------
        TOMAWAC_DATA_FILE_1 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier binaire contenant un spectre issu de TOMAWAC.
Les donnees de ce fichier seront a lire sur le canal 30.""",
            ang = """Data file, written in binary mode, given a tomawac spectrum.
Data of this file must be read on unit 30.""",
        ),
#       -----------------------------------
        BINARY_DATA_FILE_1 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees, code en binaire, mis a la disposition de
l''utilisateur.
Les donnees de ce fichier seront a lire sur le canal 24.""",
            ang = """Data file, written in binary mode, at the disposal of the user.
Data of this file must be read on unit 24.""",
        ),
#       -----------------------------------
        BINARY_DATA_FILE_2 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees, code en binaire, mis a la disposition de
l''utilisateur.
Les donnees de ce fichier seront a lire sur le canal 25.""",
            ang = """Data file, written in binary mode, at the disposal of the user.
Data of this file must be read on unit 25.""",
        ),
#       -----------------------------------
        FORMATTED_DATA_FILE_1 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees formate mis a la disposition de l''utilisateur.
Les donnees de ce fichier seront a lire sur le canal 26.""",
            ang = """Data file, written in ASCII mode, at the disposal of the user.
Data of this file must be read on unit 26.""",
        ),
#       -----------------------------------
        FORMATTED_DATA_FILE_2 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees formate mis a la disposition de l''utilisateur.
Les donnees de ce fichier seront a lire sur le canal 27.""",
            ang = """Data file, written in ASCII mode, at the disposal of the user.
Data of this file must be read on unit 27.""",
        ),
#       -----------------------------------
        BINARY_RESULTS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Fichier des resultats, code en binaire, mis a la disposition de
l''utilisateur.
Les resultats a placer dans ce fichier seront a ecrire sur
le canal 28.""",
            ang = """Results file, written in binary mode, at the disposal of the user.
Data of this file must be written on unit 28.""",
        ),
#       -----------------------------------
        FORMATTED_RESULTS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Fichier des resultats formate mis a la disposition de l''utilisateur.
Les resultats a placer dans ce fichier seront a ecrire sur
le canal 29.""",
            ang = """Results file, written in ASCII mode, at the disposal of the user.
Data of this file must be written on unit 29.""",
        ),
#       -----------------------------------
        REFERENCE_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Fichier de resultats de reference pour la validation.  Les
resultats a placer dans ce fichier seront a ecrire sur le canal 22.""",
            ang = """Binary-coded result file for validation.
The results to be entered into this file shall be written on channel""",
        ),
#       -----------------------------------
        LIST_OF_FILES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=15, max=15,
            defaut = 'STEERING FILE;DICTIONARY;FORTRAN FILE;GEOMETRY FILE;BOUNDARY CONDITIONS FILE;RESULTS FILE;BOTTOM TOPOGRAPHY FILE;BINARY DATA FILE 1;BINARY DATA FILE 2;FORMATTED DATA FILE 1;FORMATTED DATA FILE 2;BINARY RESULTS FILE;FORMATTED RESULTS FILE;REFERENCE FILE;TOMAWAC DATA FILE 1',
            fr = """Liste des fichiers""",
            ang = """List of files""",
        ),
    ),
#   -----------------------------------
    TYPE_OF_BINARY = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        RESULTS_FILE_BINARY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ['IBM','I3E','STD'],
            defaut = 'STD',
            fr = """Type du binaire utilise pour l''ecriture du fichier des resultats.
Ce type depend de la machine sur laquelle le fichier a ete genere.
Les valeurs possibles sont les memes que pour le fichier de geometrie.""",
            ang = """Binary type used to write on the results file. This type depends on
the machine used to create this file. Allowed values are the same
as used for the geometry file.""",
        ),
#       -----------------------------------
        GEOMETRY_FILE_BINARY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ['IBM','I3E','STD'],
            defaut = 'STD',
            fr = """Type du binaire utilise pour l''ecriture du fichier de geometrie.
Ce type depend de la machine sur laquelle le fichier a ete genere.
Les valeurs possibles sont :
   - IBM, pour un fichier cree sur IBM,
   - I3E, pour un fichier cree sur HP,
   - STD, permet de prendre par defaut le type de binaire associe
          a la machine sur laquelle l''utilisateur travaille.
          Il s''agit alors d''ordres READ et WRITE normaux.""",
            ang = """Type of binary mode used for geometry file writing.
It depends on the machine used for the file generation.
Possible values are :
   - IBM : for a file created on IBM,
   - I3E : for a file created on HP,
   - STD : enables to take the default binary type associated to
           the machine on which the user is working.
           It then concerns usual READ and WRITE instructiions.""",
        ),
    ),
#   -----------------------------------
    STANDARD = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        GEOMETRY_FILE_STANDARD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["LEONARD","RUBENS","SELAFIN"],
            defaut = ["SELAFIN"],
            fr = """Adapte la lecture du FICHIER DE GEOMETRIE au standard choisi pour
celui-ci. Ce peut etre :
   - 1 : un maillage regulier au standard LEONARD,
   - 2 : un maillage quelconque au standard RUBENS,
   - 3 : un maillage quelconque au standard SELAFIN.""",
            ang = """Adapts the reading of the GEOMETRY FILE to the specific standard :
   - 1 : regular mesh on standard LEONARD
   - 2 : any mesh on standard RUBENS
   - 3 : any mesh on standard SELAFIN""",
        ),
#       -----------------------------------
        RESULTS_FILE_STANDARD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["LEONARD","RUBENS","SELAFIN"],
            defaut = ["SELAFIN"],
            fr = """Standard du fichier des resultats :
   - 1 : un maillage regulier au standard LEONARD,
   - 2 : un maillage quelconque au standard RUBENS,
   - 3 : un maillage quelconque au standard SELAFIN.""",
            ang = """Specific standard of the results file :
   - 1 : regular mesh on standard LEONARD
   - 2 : any mesh on standard RUBENS
   - 3 : any mesh on standard SELAFIN""",
        ),
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,INFORMATION = PROC(nom= "INPUT_OUTPUT,INFORMATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    COMPUTATIONAL_INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        TITLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = 'NO TITLE IN THE STEERING FILE',
            fr = """Titre du cas etudie.""",
            ang = """Title of the studied case.""",
        ),
#       -----------------------------------
        RELEASE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = 'V7P1',
            fr = """Numero de version des bibliotheques ARTEMIS TELEMAC2D UTILE DAMO
BIEF et HP.
Si ce nom commence par D il s''agit de l''option Debug (exemple DV1P0)
Si ce nom commence par F il s''agit de l''option Flowtrace.""",
            ang = """Number of the release of the ARTEMIS TELEMAC2D UTILE DAMO BIEF
and HP libraries.
If this number begins by D, it corresponds to the Debug option
(example : DV3P0).
If this number begins by F, it corresponds to the Flowtrace option.""",
        ),
    ),
#   -----------------------------------
    COMPUTATION_ENVIRONMENT = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        VECTOR_LENGTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """LONGUEUR DU VECTEUR POUR LES MACHINES VECTORIELLES""",
            ang = """VECTOR LENGTH ON VECTOR MACHINES""",
        ),
#       -----------------------------------
        USER_CRAY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = '',
            fr = """Userid CRAY de l''utilisateur.""",
            ang = """Userid CRAY of the user.""",
        ),
#       -----------------------------------
        PASSWORD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = '',
            fr = """Mot de passe associe a l''USER CRAY.""",
            ang = """Password associated to the CRAY Userid.""",
        ),
#       -----------------------------------
        LIBRARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = 'artemis,telemac,util,damo,bief,hp',
            fr = """Ensemble des bibliotheques utilises pour un calcul.""",
            ang = """Set of libraries required for an ARTEMIS computation.""",
        ),
#       -----------------------------------
        CPU_TIME = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = '10',
            fr = """Temps CPU (en secondes) alloue pour la realisation du calcul.
Attention, il s''agit bien d''une chaine de caracteres.""",
            ang = """CPU time (in sec) specified for a computation on CRAY.
Warning : it is written as a Character.""",
        ),
#       -----------------------------------
        MEMORY_SPACE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = '1500000W',
            fr = """Place memoire (en mots de 8 octets) reservee en machine pour la
realisation du calcul.""",
            ang = """Memory space (in words of 8 bytes) reserved for a computation on
CRAY.""",
        ),
#       -----------------------------------
        PRIORITY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = 'JOUR',
            fr = """Classe de facturation demandee pour le calcul : il y a trois
possibilites : jour, nuit et weekend.""",
            ang = """Type of invoice requested for CRAY computation : there are
3 possibilities : jour, nuit, and weekend.""",
        ),
#       -----------------------------------
        ACCOUNT_NUMBER = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """Numero du compte calcul sur lequel sera impute le cout
du calcul.""",
            ang = """Account number to which the cost of computation shall be
charged.""",
        ),
    ),
)
# -----------------------------------------------------------------------
NUMERICAL_PARAMETERS = PROC(nom= "NUMERICAL_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    DEBUGGER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Pour imprimer la sequence des appels, mettre 1""",
        ang = """If 1, calls of subroutines will be printed in the listing""",
    ),
#   -----------------------------------
    MATRIX_STORAGE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [3],
        fr = """1 : EBE classique  2 : EBE assemble  3 : par segment
       attention, avec 2, il faut une numerotation speciale des points""",
        ang = """1 : classical EBE  2 : assembled EBE  3 : edge by edge
       beware, with option 2, a special numbering of points is required""",
    ),
#   -----------------------------------
    MATRIX_VECTOR_PRODUCT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """1 : Ancien Produit  2 : Nouveau Produit Frontal""",
        ang = """1 : Classical Product  2 : New Frontal Product""",
    ),
#   -----------------------------------
    NUMBER_OF_PRIVATE_VARIABLES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Permet de fixer le nombre de variables privees""",
        ang = """Give the number of private variables""",
    ),
#   -----------------------------------
    PARALLEL_PROCESSORS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """NOMBRE DE PROCESSEURS EN CALCUL PARALLELE
0 : 1 machine, compilation sans bibliotheque de parallelisme
1 : 1 machine, compilation avec bibliotheque de parallelisme
2 : 2 processeurs ou machines en parallele
etc...""",
        ang = """NUMBER OF PROCESSORS FOR PARALLEL PROCESSING
0 : 1 machine, compiling without parallel library
1 : 1 machine, compiling with a parallel library
2 : 2 processors or machines in parallel
etc....""",
    ),
#   -----------------------------------
    GENERAL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        ORIGINAL_DATE_OF_TIME = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 3, max= 3,
            defaut = [0,0,0],
            fr = """Permet de fixer la date d''origine des temps du modele lors
de la prise en compte de la force generatrice de la maree.""",
            ang = """Give the date of the time origin of the model when taking into
account the tide generating force.""",
        ),
#       -----------------------------------
        PARTITIONING_TOOL = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            into = ['"METIS"','"SCOTCH"','"PARMETIS"','"PTSCOTCH"'],
            defaut = 'METIS',
            fr = """CHOIX DU PARTITIONNEUR
1 : METIS
2 : SCOTCH
3 : PARMETIS
4 : PTSCOTCH
etc...""",
            ang = """PARTITIONING TOOL SELECTION
1 : METIS
2 : SCOTCH
3 : PARMETIS
4 : PTSCOTCH
etc...""",
        ),
    ),
)
# -----------------------------------------------------------------------
DISSIPATION = PROC(nom= "DISSIPATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    GAMMAS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.88],
        fr = """Donne le coefficient Gammas dans le critere de la hauteur de
deferlement. Ne pas confondre avec le coefficient Gamma qui
intervient dans la formule du spectre de Jonswap""",
        ang = """Fixes the coefficient Gammas used in the criterion of the critical
breaking wave height. Do not confuse with coefficient Gamma
used in the JONSAP spectrum.""",
    ),
#   -----------------------------------
    BOTTOM_FRICTION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        LAW_OF_BOTTOM_FRICTION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Non utilise dans ARTEMIS. On le laisse par coherence avec TELEMAC2D""",
            ang = """Not used in ARTEMIS. It is kept for consistence with TELEMAC2D""",
        ),
#       -----------------------------------
        FRICTION = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Oui, si on veut prendre en compte le frottement sur le fond dans
la simulation.""",
            ang = """Yes, if one wants to include dissipation through bottom friction
in the computation.""",
        ),
#       -----------------------------------
        FRICTION_COEFFICIENT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """A ne pas confondre avec le FACTEUR DE FROTTEMENT.
Non utilise dans ARTEMIS. On le laisse par coherence avec TELEMAC2D""",
            ang = """Do not confuse with the FRICTION FACTOR.
Not used in ARTEMIS. It is let here for consistence with TELEMAC2D.""",
        ),
#       -----------------------------------
        FORMULATION = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            BOTTOM_FRICTION_LAW = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM', min=0, max='**',
                into = [Formulation de Kostense integrant le calcul de Ue (1986)",Formulation de Putnam \& Johnson (1949)"],
                defaut = [Formulation de Kostense integrant le calcul de Ue (1986)"],
                fr = """Utilise avec l''option FROTTEMENT = OUI.
Fixe le choix de la formulation du frottement :
  1 : Kostense et al., 1986
  2 : Putnam \& Johnson, 1949.""",
                ang = """Used with the option FRICTION = YES.
Fixes the formulation used for bottom friction law :
  1 : Kostense et al., 1986
  2 : Putnam \& Johnson, 1949.""",
            ),
        ),
#       -----------------------------------
        INFORMATION = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            FLUID_KINEMATIC_VISCOSITY = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.0E-6],
                fr = """viscosite cinematique du fluide (eau) en m2/s.
1.793E-6 : Pour une temperature de 0 C.
1.567E-6 : Pour une temperature de 4 C.
1.237E-6 : Pour une temperature de 12 C.
1.112E-6 : Pour une temperature de 16 C.
1.011E-6 : Pour une temperature de 20 C.
0.802E-6 : Pour une temperature de 30 C.
0.661E-6 : Pour une temperature de 40 C.
1.0E-6   : Valeur par defaut""",
                ang = """Kinematic viscosity of the fluid (water) in m2/s.
1.793E-6 : Pour une temperature de 0 C.
1.567E-6 : Pour une temperature de 4 C.
1.237E-6 : Pour une temperature de 12 C.
1.112E-6 : Pour une temperature de 16 C.
1.011E-6 : Pour une temperature de 20 C.
0.802E-6 : Pour une temperature de 30 C.
0.661E-6 : Pour une temperature de 40 C.
1.0E-6   : Valeur par defaut""",
            ),
#           -----------------------------------
            DIAMETER90 = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.15E-3],
                fr = """DIAM90 represente le diametre maximum, en m, de 90% en poids des
sediments.
1.0E-3   : Pour des sables tres grossiers
0.5E-3   : Pour des sables grossiers
0.25E-3  : Pour des sables moyens
0.125E-3 : Pour des sables fins
0.062E-3 : Pour des sables tres fins
0.15E-3  : Valeur par defaut""",
                ang = """DIAM90 is the maximum grain diameter, in m, which defines 90% of
the total weight of sediment.
1.0E-3   : Pour des sables tres grossiers
0.5E-3   : Pour des sables grossiers
0.25E-3  : Pour des sables moyens
0.125E-3 : Pour des sables fins
0.062E-3 : Pour des sables tres fins
0.15E-3  : Valeur par defaut""",
            ),
#           -----------------------------------
            DIAMETER50 = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.10E-3],
                fr = """DIAM50 represente le diametre maximum de 50% en poids des
sediments. En general, on a DIAM90 = 1.5 * DIAM50
DIAM50 est plus souvent donne dans des tables
0.66E-3  : Pour des sables tres grossiers
0.33E-3  : Pour des sables grossiers
0.17E-3  : Pour des sables moyens
0.083E-3 : Pour des sables fins
0.040E-3 : Pour des sables tres fins
0.10E-3  : Valeur par defaut""",
                ang = """DIAM50 is the maximum grain diameter, in m, which defines 50% of
the total weight of sediment. Usually, we have
DIAM90 = 1.5 * DIAM50. DIAM50 is a more common value used.
0.66E-3  : Pour des sables tres grossiers
0.33E-3  : Pour des sables grossiers
0.17E-3  : Pour des sables moyens
0.083E-3 : Pour des sables fins
0.040E-3 : Pour des sables tres fins
0.10E-3  : Valeur par defaut""",
            ),
#           -----------------------------------
            SEDIMENT_SPECIFIC_WEIGHT = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [2650.0],
                fr = """Masse volumique du sediment en Kg/m3.""",
                ang = """Sediment specific weight in Kg/m3.""",
            ),
#           -----------------------------------
            FLUID_SPECIFIC_MASS = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1000.0],
                fr = """Masse volumique du fluide (eau) en Kg/m3.""",
                ang = """Fluid specific weight (water) in Kg/m3.""",
            ),
#           -----------------------------------
            RIPPLES_COEFFICIENT = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.7],
                fr = """Specifie le coefficient de rides utilise dans la formule de
Van Rijn pour calculer le facteur de frottement.
1.0 : Pour des rides seules
0.7 : Pour des rides superposees a des vagues de sable""",
                ang = """Fixes the ripples coefficient used in the formulae of Van Rijn
to calculate the friction factor.
1.0 : Pour des rides seules
0.7 : Pour des rides superposees a des vagues de sable""",
            ),
        ),
#       -----------------------------------
        FORMULATION_OF_REGIME = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            HYDRAULIC_REGIME_IMPOSED = SIMP(statut ='f',
#           -----------------------------------
                typ = bool, min=0, max='**',
                defaut = [False],
                fr = """Utilise avec l''option FROTTEMENT = OUI.
Permet de choisir d''imposer le regime hydraulique dans le cas
d''un calcul automatique du facteur de frottement sur fonds sableux.""",
                ang = """Used with the option FRICTION = YES.
Enables to impose the hydraulic regime in the case of an automatic
calculation of the friction factor for sandy beds.""",
            ),
#           -----------------------------------
            HYDRAULIC_REGIME_TYPE = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM', min=0, max='**',
                into = ["regime laminaire","regime turbulent lisse","regime turbulent rugueux","regime transitoire"],
                defaut = ["regime laminaire"],
                fr = """Utilise si le mot-cle REGIME HYDRAULIQUE IMPOSE = OUI.
Determine le regime hydraulique""",
                ang = """Used with option HYDRAULIC REGIME IMPOSED = YES.
Determines the type of the hydraulic regime (laminar,
smooth-turbulent, rough-turbulent, transient).""",
            ),
        ),
#       -----------------------------------
        FORMULATION_OF_RUGOSITE = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            SKIN_ROUGHNESS_ONLY = SIMP(statut ='f',
#           -----------------------------------
                typ = bool, min=0, max='**',
                defaut = [False],
                fr = """Utilise avec l''option FROTTEMENT = OUI.
Permet de choisir de ne prendre en compte
que la rugosite de peau dans le cas d''un calcul automatique
du facteur de frottement sur fonds sableux.""",
                ang = """Used with the option FRICTION = YES.
Enables to restrict the total roughness to the skin roughnes
in the case of an automatic calculation of the friction
factor for sandy beds.""",
            ),
        ),
#       -----------------------------------
        FORMULATION_OF_FW = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            FRICTION_FACTOR_IMPOSED = SIMP(statut ='f',
#           -----------------------------------
                typ = bool, min=0, max='**',
                defaut = [False],
                fr = """Utilise avec l''option FROTTEMENT = OUI.
Oui, permet de choisir d''imposer un facteur de frottement, par un
mot-cle s''il est uniforme (voir le reel d''index 29) ou en
programmant dans le sous-programme FWSPEC.
Si Non, ARTEMIS considere par defaut que les fonds sont sableux,
et calcule automatiquement le facteur de frottement avec les
caracteristiques du sediment et de l''ecoulement.""",
                ang = """Used with the option FRICTION = YES.
Yes, enables the user to impose a friction factor, by a key-word
for a constant value (see real of index 29) or by programming in
the FWSPEC sub-routine for non-uniform value.
If Not, ARTEMIS automatically computes the friction factor assuming
that the bottom is sandy and uses the characteristics of sediment
and of motion.""",
            ),
#           -----------------------------------
            FRICTION_FACTOR = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.],
                fr = """Utilise si le mot-cle FACTEUR DE FROTTEMENT IMPOSE = OUI.
Fixe le facteur de frottement choisi uniforme sur le domaine""",
                ang = """Used with the option FRICTION FACTOR IMPOSED = YES.
Fixes the value of the friction factor uniform over the domain.""",
            ),
        ),
    ),
#   -----------------------------------
    INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        SUB_ITERATIONS_ACCURACY_FOR_DISSIPATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.E-2],
            fr = """Donne la precision requise pour les sous-iterations du calcul
du coefficient de dissipation.""",
            ang = """Fixes the accuracy requested for sub-iterations necessary to
determine the dissipation coefficients.""",
        ),
#       -----------------------------------
        MAXIMUM_OF_SUB_ITERATIONS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [15],
            fr = """Donne le nombre maximum admis de sous-iterations pour le calcul
du coefficient de dissipation""",
            ang = """Fixes the maximum number of sub-iterations for the computation
of dissipation.""",
        ),
#       -----------------------------------
        DISSIPATION_RELAXATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.5],
            fr = """Donne le coefficient de relaxation entre deux sous-iterations
pour le calcul du coefficient de dissipation.""",
            ang = """Fixes the relaxation coefficient used between two sub-iterations
for the computation of the dissipation term.""",
        ),
#       -----------------------------------
        MAXIMUM_OF_SUB_ITERATIONS_FOR_TETAP = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [15],
            fr = """Donne le nombre maximum admis de sous-iterations pour le calcul
automatique de tetap""",
            ang = """Fixes the maximum number of sub-iterations for the automatic
computation of tetap""",
        ),
#       -----------------------------------
        RELAXATION_ON_TETAP = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.],
            fr = """Donne le coefficient de relaxation entre deux sous-iterations
pour le calcul de l angle d incidence automatique.""",
            ang = """Fixes the relaxation coefficient used between two sub-iterations
for the computation of automatic tetap.""",
        ),
    ),
#   -----------------------------------
    BREAKING = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        BREAKING = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """oui, si l''on souhaite integrer le processus de deferlement
bathymetrique (voir reels index 18, 19, 20, 21, 22, 23
et entiers index 12, 13).""",
            ang = """Yes, if one wants to account for breaking process (see also
reals of index 18, 19, 20, 21, 22, 23, and integer of index
12, 13).""",
        ),
#       -----------------------------------
        FORMULATION_OF_DALLY = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            KDALLY = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.1],
                fr = """Donne le coefficient K dans la Formulation de la dissipation
par deferlement d''apres Dally et al., 1984""",
                ang = """Fixes the coefficient K used in the formulation of the dissipation
coefficient proposed by Dally et al. 1984.""",
            ),
#           -----------------------------------
            GDALLY = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.4],
                fr = """Donne le coefficient Gammad dans la Formulation de la dissipation par
Dally et al., 1984. Ne pas confondre avec Gamma (Formule de Jonswap) et
Gammas (Critere de deferlement)""",
                ang = """Fixes the Gamma coefficient used in the formulation of Dally et al.,
1984, for the dissipation coefficient in surf-breaking. Do not confuse
with the coefficient GAMMA used in the JONSWAP formulae and coefficient
gammas used to determine the breaking wave height criterion.""",
            ),
        ),
#       -----------------------------------
        FORMULATION = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            BREAKING_LAW = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM', min=0, max='**',
                into = ["BATTJES \& JANSSEN","DALLY"],
                defaut = ["BATTJES \& JANSSEN"],
                fr = """Specifie la formulation choisie pour le coefficient de dissipation
par deferlement. N''est effectif qu''en Houle reguliere.
   1 : Formulation de Battjes \& Jansen, 1978
   2 : Formulation de Dally et al., 1984
En Houle aleatoire, la seule formulation utilisee est celle de
Battjes \& Janssen, 1978""",
                ang = """Specifies the formulation choosen for calculating the dissipation
coefficient through breaking. Only effective for Monochromatic wave
mode.
   1 : Formulation of Battjes \& Janssen, 1978
   2 : Formulation of Dally et al., 1984
In random wave mode, the formulation of B \& J, 1978 is the only one
to be used.""",
            ),
        ),
#       -----------------------------------
        FORMULATION_DE_BATTJES = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            ALPHA = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.0],
                fr = """Donne le coefficient Alpha dans la Formulation de la dissipation
par deferlement en houle aleatoire d''apres Battjes \& Janssen""",
                ang = """Fixes the coefficient Alpha used in the formulation of the dissipation
coefficient through breaking proposed by Battjes \& Janssen, 1978
for random waves.""",
            ),
        ),
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,GRAPHICS_AND_LISTING = PROC(nom= "INPUT_OUTPUT,GRAPHICS_AND_LISTING",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    GRAPHIC_PRINTOUT_PERIOD = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Determine la periode, en nombre de periodes de houle,
d''impression des VARIABLES POUR LES SORTIES GRAPHIQUES (voir ce mot-
cle) dans le FICHIER DES RESULTATS""",
        ang = """Fixes the period, in number of wave periods, for the writing
of the VARIABLES FOR GRAPHIC PRINTOUTS (see this key-word) in the
RESULTS FILE""",
    ),
#   -----------------------------------
    LISTING_PRINTOUT_PERIOD = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Determine la periode, en nombre de periodes de houle,
d''impression des VARIABLES A IMPRIMER (voir ce mot-cle). Pour la mise
au point, il faut savoir que la sortie des resultats est effectuee
systematiquement sur le fichier de retour d''execution du code
(actuellement accessible par le menu 3.e de SPF sur IBM)""",
        ang = """Fixes the period, in number of wave periods, for the writing
of the VARIABLES TO BE PRINTED (see this key-word)""",
    ),
#   -----------------------------------
    WAVE_HEIGHTS_SMOOTHING = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """OUI si on souhaite lisser les hauteurs de houle
pour ameliorer le calcul des contraintes de radiation
(actif uniquement en houle reguliere).
Valeur par defaut = NON.""",
        ang = """YES when one wants to smooth the wave heights
to improve the radiation stresses computation
(only used in regular wave mode).
Default value = NO.""",
    ),
#   -----------------------------------
    INFORMATION,SOLVER = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        RELAXATION_COEFFICIENT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.4],
            fr = """  Non utilise dans la version 3.0 .
  Ce coefficient doit etre compris entre 0 et 2.
  coefficient de relaxation  dans le cas d''une resolution par la
  methode  de panchang et al.
  voir Solution of the Mild Slope Wave Problem by Iteration
       Applied Ocean Research, 1991, Vol. 13, No. 4.""",
            ang = """  Not used in version 3.0 .
  This coefficient is a real between 0 and 2.
  It is a relaxation coefficient used in the solving method proposed
  by Panchang et al.
  See  Solution of the Mild Slope Wave Problem by Iteration
       Applied Ocean Research, 1991, Vol. 13, No. 4.""",
        ),
#       -----------------------------------
        LISTING_PRINTOUT = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [True ],
            fr = """Sortie des resultats sur support papier.
Si l''on met NON le listing ne contient que l''en-tete et la mention
FIN NORMALE DU PROGRAMME
Commande a eviter.""",
            ang = """If NOT is specified for this key-word, the printout listing just
contains the head and the sentence END OF PROGRAM.
It is adviced not to use this way.""",
        ),
#       -----------------------------------
        INFORMATIONS_ABOUT_SOLVER = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [True ],
            fr = """Donne le nombre d''iterations necessaires a la convergence du solveur.""",
            ang = """Gives the iterations number which was necessary for the solver
to converge.""",
        ),
#       -----------------------------------
        VALIDATION = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Option utilisee principalement pour le dossier de validation. Le
fichier des resultats du calcul precedent est alors considere comme une
reference a laquelle on va comparer le calcul. La comparaison est
effectuee par le sous-programme VALIDA qui peut etre une comparaison
avec une solution exacte par exemple.""",
            ang = """This option is primarily used for the validation documents.
The PREVIOUS COMPUTATION FILE is then considered as a
reference which the computation is going to be compared with.
The comparison is made by the subroutine VALIDA, which can be
modified as to
so as to include, for example,a comparison with an exact solution.""",
        ),
#       -----------------------------------
        b_VALIDATIONG = BLOC(condition="VALIDATION == True",
#       -----------------------------------
#           -----------------------------------
            REFERENCE_FILE_FORMAT = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM',
                into = ['SERAFIN','SERAFIND','MED'],
                defaut = 'SERAFIN?',
                fr = """Format du fichier de resultats du calcul precedent.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
                ang = """Previous computation results file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
            ),
        ),
    ),
)
# -----------------------------------------------------------------------
NUMERICAL_PARAMETERS,SOLVER = PROC(nom= "NUMERICAL_PARAMETERS,SOLVER",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    MAXIMUM_NUMBER_OF_ITERATIONS_FOR_SOLVER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [60000],
        fr = """Les algorithmes utilises pour la resolution du systeme
matriciel etant iteratifs, il est necessaire de limiter le nombre
d''iterations autorisees""",
        ang = """Algorithms used for solving the matrix system are iterative.
It is then necessary to limit the maximum number of iterations""",
    ),
#   -----------------------------------
    PRECONDITIONING = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["no preconditioning","diagonal preconditioning","block-diagonal preconditioning","absolute value diagonal preconditioning","Crout preconditioning"],
        defaut = ["diagonal preconditioning"],
        fr = """Permet de preconditionner le systeme de l''etape de propagation afin
d''accelerer la convergence lors de sa resolution.
   - 0 : pas de preconditionnement,
   - 2 : preconditionnement diagonal.
   - 3 : preconditionnement bloc-diagonal.
   - 5 : preconditionnement diagonal en valeur absolue.
   - 7 : preconditionnement de Crout par element.
Certains preconditionnements sont cumulables
(les diagonaux 2 ou 3 avec les autres)
Pour cette raison on ne retient que les nombres premiers pour
designer les preconditionnements. Si l''on souhaite en cumuler
plusieurs on formera le produit des options correspondantes.""",
        ang = """Enables to apply preconditionning the matrix system to accelerate
the convergence of the solver.
   - 0 : no preconditionning
   - 2 : diagonal preconditionning
   - 3 : block-diagonal preconditionning
   - 5 : diagonal preconditionning in absolute value
   - 7 : Element Crout preconditionning.
Few of them can be combined
(numbers 2 or 3 with the other)
To combine some preconditionning, impose the product of the previous
numbers : example 6 means preconditionnig 2 and 3 applied.""",
    ),
#   -----------------------------------
    SOLVER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","CGSTAB","GMRES","direct"],
        defaut = ["direct"],
        fr = """Permet de choisir le solveur utilise pour la resolution de l''etape de
propagation. Toutes les methodes proposees actuellement s''apparentent
au Gradient Conjugue. Ce sont :
 1 : gradient conjugue
 2 : residu conjugue
 3 : gradient conjugue sur equation normale
 4 : erreur minimale
 5 : gradient conjugue carre (non programme)
 6 : gradient conjugue de type CGSTAB
 7 : GMRES
 8 : solveur direct""",
        ang = """Enables to choose the solver used for solving the matrix system.
They are :
 1 : conjugate gradient
 2 : conjugate residual
 3 : conjugate gradient on the normal equation
 4 : minimum error
 5 : squarred conjugate gradient (not programmed)
 6 : CGSTAB conjugate gradient
 7 : GMRES
 8 : direct solver""",
    ),
#   -----------------------------------
    SOLVER_OPTION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [3],
        fr = """Parametre definissant la dimension de l''espace de Krylov
pour le solveur 7 (GMRES)""",
        ang = """Defines the dimension of the Krylov space when using
the solver 7 (GMRES)""",
    ),
#   -----------------------------------
    SOLVER_ACCURACY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1.E-4],
        fr = """Precision demandee pour la resolution de l''equation de Berkhoff.""",
        ang = """Accuracy requested for the linear system solver.""",
    ),
#   -----------------------------------
    INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        DISCRETIZATION_IN_SPACE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """ NON ACTIVE POUR LE MOMENT""",
            ang = """NOT ACTIVE FOR THE MOMENT""",
        ),
#       -----------------------------------
        ZERO = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.E-12],
            fr = """Non active pour l''instant.""",
            ang = """Non active at the moment.""",
        ),
#       -----------------------------------
        BIDON_STRING = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = '',
            fr = """TABLEAU DE CARACTERES DE TAILLE :4
Place reservee pour eventuellement introduire
 de nouvelles
 chaines de caracteres (nouveaux fichiers...).""",
            ang = """Character Array of size : 4
Reserved to introduce new character strings (new file names...).""",
        ),
    ),
)
# -----------------------------------------------------------------------
PHYSICAL_PARAMETERS = PROC(nom= "PHYSICAL_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    RANDOM_WAVE = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_PERIODS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [5],
            fr = """Valeur utilisee avec l''option :
  HOULE ALEATOIRE MONODIRECTIONNELLE = OUI
  ou avec l''option
  HOULE ALEATOIRE MULTIDIRECTIONNELLE = OUI
Pour un calcul en houle aleatoire monodirectionnelle ou
multidirectionnelle, nombre de bandes d''egale energie servant a
discretiser le spectre d''energie en frequence.""",
            ang = """Used with otion :
   MONODIRECTIONAL RANDOM WAVE = YES
   or
   MULTIDIRECTIONAL RANDOM WAVE = YES
It fixes the number of iso-energy frequency bands which discretize
the enrgy spectrum.""",
        ),
#       -----------------------------------
        PEAK_PERIOD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [10.0],
            fr = """Valeur utilisee avec l''option :
  HOULE ALEATOIRE MONODIRECTIONNELLE = OUI
  ou avec l''option
  HOULE ALEATOIRE MULTIDIRECTIONNELLE = OUI
Fixe la periode de pic (en sec) du spectre d''energie.""",
            ang = """Used with otion :
   MONODIRECTIONAL RANDOM WAVE = YES
   or
   MULTIDIRECTIONAL RANDOM WAVE = YES
Fixes the peak period (in sec) of the energy spectrum""",
        ),
#       -----------------------------------
        GAMMA = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["Pierson-Moskowitz","JONSWAP moyen","any spectre"],
            defaut = ["JONSWAP moyen"],
            fr = """Valeur utilisee avec l''option :
  HOULE ALEATOIRE MONODIRECTIONNELLE = OUI
  ou avec l''option
  HOULE ALEATOIRE MULTIDIRECTIONNELLE = OUI
Indique la valeur de gamma pour le spectre d''energie
  GAMMA = 1   spectre de Pierson-Moskowitz
  GAMMA = 3.3 spectre de JONSWAP moyen (valeur par defaut).""",
            ang = """Used with otion :
   MONODIRECTIONAL RANDOM WAVE = YES
   or
   MULTIDIRECTIONAL RANDOM WAVE = YES
Fixes the gamma value tor the JONSWAP wave energy spectrum :
  GAMMA = 1  : Pierson-Moskowitz
  GAMMA = 3.3 : mean JONSWAP spectrum (default value).""",
        ),
#       -----------------------------------
        MINIMUM_SPECTRAL_PERIOD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.02],
            fr = """Valeur de la periode minimum voulue en secondes
si on veut tronquer le spectre pour le calcul
des periodes en houle aleatoire (voir PERALE).""",
            ang = """Minimum period value requested in seconds
if it is necessary to alter the energy spectrum
for the computation of the periods in the case
of random waves (see PERALE).""",
        ),
#       -----------------------------------
        MAXIMUM_SPECTRAL_PERIOD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [200.],
            fr = """Valeur de la periode maximum voulue en secondes
si on veut tronquer le spectre pour le calcul
des periodes en houle aleatoire (voir PERALE).""",
            ang = """Maximum period value requested in seconds
if it is necessary to alter the energy spectrum
for the computation of the periods in the case
of random waves (see PERALE).""",
        ),
    ),
#   -----------------------------------
    MONODIRECTIONAL_RANDOM_WAVE = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        MONODIRECTIONAL_RANDOM_WAVE = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """oui, si l''on veut effectuer un calcul en houle aleatoire
monodirectionnelle (voir reels index 12, 13 et entier index 10).""",
            ang = """Yes, if one wants to run computation in random monodirectional waves
(see reals key-words of index 12, 13 and integer of index 10).""",
        ),
    ),
#   -----------------------------------
    MULTIDIRECTIONAL_RANDOM_WAVE = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        MULTIDIRECTIONAL_RANDOM_WAVE = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """oui, si l''on veut effectuer un calcul en houle aleatoire
multidirectionnelle (voir les reels index 12, 13, 14, 15 et 16 et
les entiers index 10 et 11.""",
            ang = """Yes, if one wants to run computation in random multidirectional waves
(see reals key-words of index 12, 13 and integer of index 10).""",
        ),
#       -----------------------------------
        DONNEES = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            NUMBER_OF_DIRECTIONS = SIMP(statut ='f',
#           -----------------------------------
                typ = 'I', min=0, max='**',
                defaut = [5],
                fr = """Valeur utilisee avec l''option :
  HOULE ALEATOIRE MULTIDIRECTIONNELLE = OUI
Pour un calcul en houle aleatoire multidirectionnelle,
nombre de bandes d''egale energie servant a discretiser le spectre
directionnel d''energie.""",
                ang = """Used with the option :
  MULTIDIRECTIONAL RANDOM WAVE = YES
It fixes the number of iso-energy bands which discretizes the wave
directional spectrum.""",
            ),
#           -----------------------------------
            MINIMUM_ANGLE_OF_PROPAGATION = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [-180.],
                fr = """Valeur utilisee avec l''option :
  HOULE ALEATOIRE MULTIDIRECTIONNELLE = OUI
  indique la borne inferieure de l''intervalle des directions de
  propagation dans le cas d''une houle aleatoire multidirectionnelle
  L''angle est donne en degres et est compte positivement dans le sens
  direct a partir de l''axe x.""",
                ang = """Used with the option :
  MULTIDIRECTIONAL RANDOM WAVE = YES
Fixes the minimum value (in deg) of the directions range. It is
counted positively in the trigonometric sense relatively to the x
axis.""",
            ),
#           -----------------------------------
            MAXIMUM_ANGLE_OF_PROPAGATION = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [180.],
                fr = """Valeur utilisee avec l''option :
  HOULE ALEATOIRE MULTIDIRECTIONNELLE = OUI
  indique la borne superieure de l''intervalle des directions de
  propagation dans le cas d''une houle aleatoire multidirectionnelle
  L''angle est donne en degres et est compte positivement dans le sens
  direct a partir de l''axe x.""",
                ang = """Used with the option :
  MULTIDIRECTIONAL RANDOM WAVE = YES
Fixes the maximum value (in deg) of the directions range. It is
counted positively in the trigonometric sense relatively to the x
axis.""",
            ),
#           -----------------------------------
            S_EXPONENT = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [20.],
                fr = """Valeur utilisee avec l''option :
  HOULE ALEATOIRE MULTIDIRECTIONNELLE = OUI
  indique la valeur maximale de l''exposant s dans l''expression donnant
  la repartition directionnelle de la houle.
  Cette expression est celle donnee par Goda dans Random Seas and
  Design of Maritime Structures - University of Tokyo Press :
  G(f,teta) = G0 * (cos(teta/2))**2s. f est la frequence et teta est
  la direction de propagation de la houle.""",
                ang = """Used with the option :
  MULTIDIRECTIONAL RANDOM WAVE = YES
Fixes the maximum value of exponent S in the Goda formula used to
express the directional wave energy spreading.
See GODA Y., Random Seas and Design of Maritime Structures - Univ.
of Tokyo Press, 1987.""",
            ),
        ),
    ),
#   -----------------------------------
    PERIOD_SCANNING = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        PERIOD_SCANNING = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """oui, si l''on veut effectuer plusieurs calculs en balayant un
intervalle de periodes (voir reels index 8,9 et 10).""",
            ang = """Yes, if one wants to run computations by scanning a period range
(resonance computations, see also reals of index 8, 9, and 10).""",
        ),
#       -----------------------------------
        DONNEES = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            BEGINNING_PERIOD_FOR_PERIOD_SCANNING = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.],
                fr = """Valeur utilisee avec l''option :
  BALAYAGE EN PERIODE = OUI
  indique la borne gauche de l''intervalle de periodes a parcourir
  (pour par exemple rechercher les periodes de resonances).""",
                ang = """Used with the option :
  PERIOD SCANNING = YES
Fixes the minimum value (in sec) of the period range to be used for
the period scanning.""",
            ),
#           -----------------------------------
            ENDING_PERIOD_FOR_PERIOD_SCANNING = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.],
                fr = """Valeur utilisee avec l''option :
  BALAYAGE EN PERIODE = OUI
  indique la borne droite de l''intervalle de periodes a parcourir
  (pour par exemple rechercher les periodes de resonances).""",
                ang = """Used with the option :
  PERIOD SCANNING = YES
Fixes the maximum value (in sec) of the period range to be used for
the period scanning.""",
            ),
#           -----------------------------------
            STEP_FOR_PERIOD_SCANNING = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.],
                fr = """Valeur utilisee avec l''option :
  BALAYAGE EN PERIODE = OUI
  indique le pas a prendre pour effectuer le balayage en periodes
  (pour par exemple rechercher les periodes de resonances).""",
                ang = """Used with the option :
  PERIOD SCANNING = YES
Fixes the value of the period step (in sec) to be used for
the period scanning.""",
            ),
        ),
    ),
#   -----------------------------------
    INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        MINIMUM_VALUE_FOR_H = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.E-7],
            fr = """Fixe la valeur minimale de H
Non active pour l''instant.""",
            ang = """Fixes the minimum value of H
Non active at the moment.""",
        ),
#       -----------------------------------
        WAVE_PERIOD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [10.],
            fr = """Definit la periode de la houle en mode monochromatique.""",
            ang = """Defines the wave period for monochromatic mode.""",
        ),
#       -----------------------------------
        DIRECTION_OF_WAVE_PROPAGATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.0],
            fr = """Donne la direction du vecteur d''onde de la houle incidente. L''angle
est donne en degres et est compte positivement dans le sens direct
a partir de l''axe des x.
Il s''agit de la direction principale  de propagation.
Cette direction est la meme a toutes les frontieres maritimes.
Si l''utilisateur veut specifier des directions differentes sur
differentes frontieres, il doit le faire dans son FORTRAN dans le
sous-programme BORH en specifiant la variable TETAB.""",
            ang = """Fixes the direction towards the incident waves at boundaries go to.
It is counted in degress and positively in the trigonometric sense
relatively to the x axis.
This value is prescribed as a constant value along all the wave
incident type boundaries. If one wants to specify a non uniform value,
the user has to specify the value TETAB in the sub-routine BORH.""",
        ),
#       -----------------------------------
        ORIGINAL_HOUR_OF_TIME = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 3, max= 3,
            defaut = [0,0,0],
            fr = """Permet de fixer l''heure d''origine des temps du modele lors
de la prise en compte de la force generatrice de la maree.""",
            ang = """Give the time of the time origin of the model when taking into
account the tide generating force.""",
        ),
    ),
)
# -----------------------------------------------------------------------
EQUATIONS,SMOOTHINGS = PROC(nom= "EQUATIONS,SMOOTHINGS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    BOTTOM_TOPOGRAPHY_SMOOTHING = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Nombre de lissages effectues sur la topographie.
chaque lissage, effectue a l''aide d''une matrice de masse,
est conservatif.
Utilise lorsque les donnees de bathymetrie donnent des resultats
trop irreguliers apres interpolation.""",
        ang = """Number of smoothings done on the topography.
Each smoothing, using a mass matrix, is conservative.
It is used when bathymetric data provide too irregular results
after interpolation.""",
    ),
)
# -----------------------------------------------------------------------
INITIAL_CONDITIONS_EQUATIONS = PROC(nom= "INITIAL_CONDITIONS_EQUATIONS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    INITIAL_WATER_LEVEL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Valeur utilisee avec l''option CONDITIONS INITIALES : COTE CONSTANTE.""",
        ang = """Used with the option INITIAL CONDITIONS : CONSTANT ELEVATION.""",
    ),
#   -----------------------------------
    INITIAL_DEPTH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Valeur utilisee avec l''option :
  CONDITIONS INITIALES : HAUTEUR CONSTANTE.""",
        ang = """Value specified when using the option :
  INITIAL CONDITIONS : CONSTANT DEPTH.""",
    ),
#   -----------------------------------
    INITIAL_CONDITIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ['ZERO ELEVATION','CONSTANT ELEVATION','ZERO DEPTH','CONSTANT DEPTH','SPECIAL'],
        defaut = 'ZERO ELEVATION',
        fr = """Permet de definir les conditions initiales sur les hauteurs d''eau.
Les valeurs possibles sont :
   - COTE NULLE. Initialise la cote de surface libre a 0.
           Les hauteurs d''eau initiales sont alors retrouvees en
           faisant la difference entre les cotes de surface libre
           et du fond.
   - COTE CONSTANTE .Initialise la cote de surface libre a la
           valeur donnee par le mot-cle COTE INITIALE. Les hauteurs
           d''eau initiales sont calculees comme precedemment.
   - HAUTEUR NULLE .Initialise les hauteurs d''eau a 0.
   - HAUTEUR CONSTANTE. Initialise les hauteurs d''eau a la valeur
           donnee par le mot-cle HAUTEUR INITIALE.
   - PARTICULIERES. Les conditions initiales sur la hauteur d''eau
           doivent etre precisees dans le sous-programme CONDIH.""",
        ang = """Enables to define the initial conditions on water depths.
Allowable values are :
   - ZERO ELEVATION : fixes the free surface level to 0.
           Water depths are then equal to the difference between
           free surface level and bottom level.
   - CONSTANT ELEVATION : fixes the free surface level to the value
           specified by the key-word INITIAL WATER LEVEL. Water
           level are then computed as before.
   - ZERO DEPTH : initializes the water depths to 0.
   - CONSTANT DEPTH : initializes the water depths to the value
           specified by the key-word INITIAL DEPTH.
   - SPECIAL : initial conditions on water depths are to be
           precised in the sub-routine CONDIH.""",
    ),
)
# -----------------------------------------------------------------------
PHYSICAL_CONSTANTS = PROC(nom= "PHYSICAL_CONSTANTS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    GRAVITY_ACCELERATION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [9.81],
        fr = """Fixe la valeur de l''acceleration de la pesanteur.""",
        ang = """Fixes the gravity acceleration value.""",
    ),
)
# -----------------------------------------------------------------------
RESULTS = PROC(nom= "RESULTS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    VARIABLES_FOR_GRAPHIC_PRINTOUTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["wave height","wave phase","velocity u (free surface)(t=0)","velocity v (free surface)(t=0)","free surface elevation (t=0)","bottom elevation","still water height","phase velocity","group velocity","wave number","real potential","imaginal potential","prive(1,1)","prive(1,2)","prive(1,3)","prive(1,4)","first mean spectral period","second mean spectral period","third mean spectral period","force along X","force along Y","wave incidence radian","breaking rate","SXX stress","SXY stress","SYY stress"],
        defaut = [],
        fr = """Noms des variables que l''utilisateur veut ecrire dans le fichier des
resultats.
Le choix des separateurs est libre.
Les possibilites offertes sont les suivantes :
    - HS hauteur de la houle
    - PHAS phase de la houle
    - U0 vitesse u en surface      (a t=0)
    - V0 vitesse v en surface      (a t=0)
    - ZS cote de la surface libre  (a t=0)
    - ZF fond
    - HW hauteur d''eau au repos
    - C vitesse de phase
    - CG vitesse de groupe
    - K nombre d''onde
    - PHIR potentiel reel
    - PHII potentiel imaginaire
    - D prive(1,1) (variable 13)
    - E prive(1,2) (variable 14)
    - F prive(1,3) (variable 15)
    - G prive(1,4) (variable 16)
    - T01 premi�re p�riode moyenne spectrale
    - T02 deuxi�me p�riode moyenne spectrale
    - TM troisi�me p�riode moyenne spectrale
    - FX force en X
    - FY force en Y
    - INC incidence de la houle
    - QB taux de deferlement
    - SXX contrainte SXX
    - SXY contrainte SXY
    - SYY contrainte SYY
L''utilisateur dispose de 4 champs libres, qu''il peut
utiliser pour ecrire dans le fichier des resultats des variables
qu''il cree lui-meme. Ces variables propres a l''utlisateur doivent
etre calculees dans le sous-programme CALRES et le nom que l''on
desire leur donner doit etre ecrit dans le sous-programme NOMVAR.
Ces 4 champs sont :
   - D, E, F, G qui correspondent aux tableaux PRIVE(1,1), PRIVE(1,2),
     PRIVE(1,3), PRIVE(1,4). A la difference des variables
     precedentes, celles-ci sont conservees dans tout le programme, et
     peuvent donc etre reutilisees.
     Dans ce dernier cas ne pas oublier de donner une taille
     suffisante au tableau PRIVE,
     en precisant le parametre NPRIV (dans le programme principal).""",
        ang = """Names of the variables that the user wants to write in the results
file. Separators between variable names can be choosen free.
 The allowable values are :
   - HS=wave height
   - PHAS=wave phase
   - U0=velocity u (free surface at t=0)
   - V0=velocity v (free surface at t=0)
   - ZS=free surface elevation (at t=0)
   - ZF=bottom elevation
   - HW=still water height
   - C=phase velocity
   - CG=group velocity
   - K=wave number
   - PHIR=real potential
   - PHII=imaginal potential
   - D=prive(1,1)
   - E=prive(1,2)
   - F=prive(1,3)
   - G=prive(1,4)
   - T01=first mean spectral period
   - T02=second mean spectral period
   - TM=third mean spectral period
   - FX=force along X
   - FY=force along Y
   - INC=wave incidence radian
   - QB=breaking rate
   - SXX=SXX stress
   - SXY=SXY stress
   - SYY=SYY stress
The user has 4 free variables at his/her disposal to create other
variables by him/herself. These variables have to be computed
in the CALRES sub-routine, and the name that we want to attribute
has to be precibed in the NOMVAR sub-routine.
The 4 free variable fields are :
   - D, E, F, G which corresponds to the private arrays PRIVE(1,1),
     PRIVE(1,2), PRIVE(1,3) and PRIVE (1,4). Contrarily to the previous
     variables, these are conserved all through the computation, and can
     be used again.
     Do not forget to specify the number of private arrays you want to
     use in the principal programme (variable NPRIV).""",
    ),
#   -----------------------------------
    VARIABLES_TO_BE_PRINTED = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["wave height","wave phase","velocity u (free surface)(t=0)","velocity v (free surface)(t=0)","free surface elevation (t=0)","bottom elevation","still water height","phase velocity","group velocity","wave number","real potential","imaginal potential","prive(1,1)","prive(1,2)","prive(1,3)","prive(1,4)","first mean spectral period","second mean spectral period","third mean spectral period","force along X","force along Y","wave incidence radian","breaking rate","SXX stress","SXY stress","SYY stress"],
        defaut = [],
        fr = """Nom des variables que l''utilisateur desire ecrire a l''ecran.
Memes possibilites que pour les sorties graphiques.""",
        ang = """Name of variables taht the user whishes to write on the screen.
Possibilities are the same as for graphic outputs.""",
    ),
)
# -----------------------------------------------------------------------
CURRENT = PROC(nom= "CURRENT",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        SUB_ITERATIONS_ACCURACY_FOR_CURRENT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.E-2],
            fr = """Donne la precision requise pour les sous-iterations du calcul
du nombre d''onde en presencede courant (vecteur d''onde).""",
            ang = """Fixes the accuracy requested for sub-iterations necessary to
determine the wave vector.""",
        ),
    ),
#   -----------------------------------
    CURRENT = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        CURRENT = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """TRUE : PRISE EN COMPTE DE LA REFRACTION DE LA HOULE PAR
  LE COURANT.
  Modele retenu : Kostense et Al. (1988)""",
            ang = """TRUE : WAVE REFRACTION DUE TO CURRENT IS
  DESCRIBED USING KOSTENSE MODEL (1988)""",
        ),
    ),
)
# -----------------------------------------------------------------------
TETAP_CONVERGENCE = PROC(nom= "TETAP_CONVERGENCE",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        SUB_ITERATIONS_ACCURACY_FOR_TETAP = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.E-2],
            fr = """Donne la precision requise pour les sous-iterations du calcul
automatique de cos(TETAP).""",
            ang = """Fixes the accuracy requested for sub-iterations necessary to
determine value of TETAP (criterion on cos(TETAP).""",
        ),
    ),
)
# -----------------------------------------------------------------------
REFLEXION_ANGLE = PROC(nom= "REFLEXION_ANGLE",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    TETAP = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        AUTOMATIC_TETAP_CALCULATION = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """TRUE : CALCUL AUTO DES ANGLES TETAP
 (basee sur la direction de la vitesse)""",
            ang = """TRUE : AUTOMATIC CALCULATION OF TETAP
  (based on velocity direction)""",
        ),
    ),
)
# -----------------------------------------------------------------------
INCIDENT_WAVE_PHASE = PROC(nom= "INCIDENT_WAVE_PHASE",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    PHASE = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        AUTOMATIC_CALCULATION_OF_PHASE = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """TRUE : CALCUL AUTOMATIQUE DE LA PHASE
 (basee sur une profondeur de reference)""",
            ang = """TRUE : AUTOMATIC CALCULATION OF INCIDENTE PHASE
  (based on reference water depth)""",
        ),
    ),
)
# -----------------------------------------------------------------------
PHASE_DEFINITION = PROC(nom= "PHASE_DEFINITION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    REFERENCE_WATER_DEPTH = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        REFERENCE_WATER_DEPTH_FOR_AUTOMATIC_PHASE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [-1.0],
            fr = """PROFONDEUR DE REFERENCE POUR LE CALCUL DE LA PHASE.
 ESSAYEZ DE METTRE LA FRONTIERE INCIDENTE SUR UNE ZONE
 DE BATHYMETRIE HOMOGENE. LA PROFONDEUR A RENSEIGNER
 DOIT ETRE REPRESENTATIVE DE LA PROFONDEUR D EAU SUR
 LA FRONTIERE""",
            ang = """WATER DEPTH FOR AUTOMATIC INCIDENT PHASE CALCULATION.
 TRY TO PUT THE INCIDENT WAVE BOUNDARY ON A REGULAR
 TOPOGRAPHY ZONE. THE REFERENCE WATER DEPTH SHOULD BE
 REPRESENTATIVE OF THE WATER DEPTH ON THE BOUNDARY""",
        ),
    ),
)
# -----------------------------------------------------------------------
INFORMATION = PROC(nom= "INFORMATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    PHASE_DEFINITION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        PHASE_REFERENCE_COORDINATES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            defaut = [0,0],
            fr = """Coordonnees pour l origine des phases. Ne change rien
        aux hauteurs de vagues calculees""",
            ang = """Coordinates of reference point for phase.Will
         not change the wave height computed""",
        ),
    ),
)
# -----------------------------------------------------------------------
CHAINING = PROC(nom= "CHAINING",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    TOMAWAC = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        CHAINING_TOMAWAC_1 = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Oui, si on imposer un spectre tomawac unique sur la frontiere
onde incidente.""",
            ang = """Yes, if one wants to use a spectrum from TOMAWAC on the incident
boundary.""",
        ),
#       -----------------------------------
        NUMBER_OF_DIRECTIONS_IN_TOMAWAC_SPECTRUM = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Indique le nombre de directions dans le spectre importe
depuis TOMAWAC""",
            ang = """Give the number of direction in the TOMAWAC imported
spectrum""",
        ),
#       -----------------------------------
        NUMBER_OF_FREQUENCIES_IN_TOMAWAC_SPECTRUM = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Indique le nombre de frequences dans le spectre importe
depuis TOMAWAC""",
            ang = """Give the number of frequences in the TOMAWAC imported
spectrum""",
        ),
#       -----------------------------------
        INSTANT_FOR_READING_TOMAWAC_SPECTRUM = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """Indique l instant de calcul TOMAWAC associe au spectre
qui doit etre importe dans ARTEMIS""",
            ang = """Give the instant of the TOMAWAC computation at which we
want to import the spectrum for ARTEMIS""",
        ),
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,_INFORMATION = PROC(nom= "INPUT_OUTPUT,_INFORMATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    COMPUTATIONAL_INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        DESCRIPTION_DES_LIBRARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 6, max= 6,
            defaut = 'builds|PPP|lib|artemisMMMVVV.LLL;builds|PPP|lib|biefMMMVVV.LLL;builds|PPP|lib|hermesMMMVVV.LLL;builds|PPP|lib|damoMMMVVV.LLL;builds|PPP|lib|parallelMMMVVV.LLL;builds|PPP|lib|specialMMMVVV.LLL',
            fr = """Description des librairies de ARTEMIS""",
            ang = """ARTEMIS LIBRARIES description""",
        ),
#       -----------------------------------
        DEFAULT_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|artemisMMMVVV.exe',
            fr = """Executable par defaut de ARTEMIS""",
            ang = """Default executable for ARTEMIS""",
        ),
#       -----------------------------------
        DEFAULT_PARALLEL_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|artemisMMMVVV.exe',
            fr = """Executable parallele par defaut de Artemis""",
            ang = """Default parallel executable for Artemis""",
        ),
    ),
#   -----------------------------------
    COMPUTATION_ENVIRONMENT = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        DICTIONARY = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = 'artemis.dico',
            fr = """Dictionnaire des mots cles.""",
            ang = """Key word dictionary.""",
        ),
    ),
#   -----------------------------------
    CONTROL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        ORIGIN_COORDINATES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 2, max= 2,
            defaut = [0,0],
            fr = """Valeur en metres, utilise pour eviter les trops grands
nombres, transmis
dans le format Selafin mais pas d''autre traitement pour l''instant""",
            ang = """Value in metres, used to avoid large real numbers,
added in Selafin format, but so far no other treatment""",
        ),
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,_FILES = PROC(nom= "INPUT_OUTPUT,_FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    GEOMETRY_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de geometrie.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """Geometry file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
#   -----------------------------------
    RESULTS_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de resultats.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """Results file format. Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
#   -----------------------------------
    TOMAWAC_DATA_FILE_1_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de resultats du calcul precedent.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """Previous computation results file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
#   -----------------------------------
    BINARY_DATA_FILE_1_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de resultats du calcul precedent.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """Previous computation results file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
#   -----------------------------------
    BINARY_DATA_FILE_2_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de resultats du calcul precedent.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """Previous computation results file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
)
# -----------------------------------------------------------------------
TOPOGRAPHY_EFFECTS,EXTENDED_MILD_SLOPE_EQUATION = PROC(nom= "TOPOGRAPHY_EFFECTS,EXTENDED_MILD_SLOPE_EQUATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    RAPIDLY_VARYING_TOPOGRAPHY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["BERKHOFF SIMPLE","PRISE EN COMPTE TERME DE PENTE","PRISE EN COMPTE TERME DE COURBURE","PRISE EN COMPTE DES TERMES DE PENTE ET COURBURE"],
        defaut = ["BERKHOFF SIMPLE"],
        fr = """PRISE EN COMPTE DES FORTES PENTES ET COURBURES DANS BERKHOFF
   0=> BERKHOFF SIMPLE
   1=> PRISE EN COMPTE TERME PENTE en grad(H) **2
   2=> PRISE EN COMPTE TERME COURBURE en laplacien(H)
   3=> PRISE EN COMPTE DES TERMES PENTE ET COURBURE
   Modele retenu pour les fonctions E1 et E2 : Chamberlain
   et Porter (1995)""",
        ang = """EXTENSION OF MILD-SLOPE EQUATION WITH SECOND
          ORDER BOTTOM EFFECTS
   0=> MILD-SLOPE EQUATION
   1=> GRADIENT SECOND ORDER TERM  : grad(H) **2
   2=> CURVATURE SECOND ORDER TERM : laplacian(H)
   3=> GRADIENT + CURVATURE SECOND ORDER TERMS
   Model used for functions E1 and E2 expression : Chamberlain
   et Porter 1995""",
    ),
)
# -----------------------------------------------------------------------
GENERAL = PROC(nom= "GENERAL",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    CHECKING_THE_MESH = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Si oui on appelle le sous-programme checkmesh qui verifie
la coherence du maillage, points superposes, etc.""",
        ang = """if this key word is equal to yes, a call to subroutine
checkmesh will look for errors in the mesh, superimposed points, etc.""",
    ),
)
Ordre_des_commandes = (
'INPUT_OUTPUT,FILES',
'INPUT_OUTPUT,INFORMATION',
'NUMERICAL_PARAMETERS',
'DISSIPATION',
'INPUT_OUTPUT,GRAPHICS_AND_LISTING',
'NUMERICAL_PARAMETERS,SOLVER',
'PHYSICAL_PARAMETERS',
'EQUATIONS,SMOOTHINGS',
'INITIAL_CONDITIONS_EQUATIONS',
'PHYSICAL_CONSTANTS',
'RESULTS',
'CURRENT',
'TETAP_CONVERGENCE',
'REFLEXION_ANGLE',
'INCIDENT_WAVE_PHASE',
'PHASE_DEFINITION',
'INFORMATION',
'CHAINING',
'INPUT_OUTPUT,_INFORMATION',
'INPUT_OUTPUT,_FILES',
'TOPOGRAPHY_EFFECTS,EXTENDED_MILD_SLOPE_EQUATION',
'GENERAL')
