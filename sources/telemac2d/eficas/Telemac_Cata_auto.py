
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
INITIALIZATION = PROC(nom= "INITIALIZATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    TITLE = SIMP(statut ='o',
#   -----------------------------------
        typ = 'TXM',
        defaut = '',
        fr = """Titre du cas etudie. Ce titre figurera sur les dessins.""",
        ang = """Title of the case being considered. This title shall be marked on the
drawings.""",
    ),
#   -----------------------------------
    INPUT_FILES = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        GEOMETRY_FILE_FORMAT = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ['SERAFIN?','SERAFIND','MED'],
            defaut = 'SERAFIN?',
            fr = """ Format du fichier de geometrie. Les valeurs possibles sont : - SERAFIN
: format standard simple precision pour Telemac; - SERAFIND: format
standard double precision pour Telemac; - MED : format MED base sur
HDF5""",
            ang = """ Geometry file format. Possible values are: - SERAFIN : classical
single precision format in Telemac; - SERAFIND: classical double
precision format in Telemac; - MED : MED format based on HDF5""",
        ),
#       -----------------------------------
        GEOMETRY_FILE = SIMP(statut ='o',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            fr = """ Nom du fichier contenant le maillage du calcul a realiser.""",
            ang = """ Name of the file containing the mesh. This file may also contain the
topography and the friction coefficients.""",
        ),
#       -----------------------------------
        FORTRAN_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = 'DEFAUT',
            fr = """ Nom du fichier FORTRAN a soumettre.""",
            ang = """ Name of FORTRAN file to be submitted.""",
        ),
#       -----------------------------------
        BOTTOM_TOPOGRAPHY_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """ Nom du fichier eventuel contenant la bathymetrie associee au maillage.
Si ce mot-cle est utilise; c''est cette bathymetrie qui sera utilisee
pour le calcul.""",
            ang = """ Name of the possible file containing the bathymetric data. Where this
keyword is used, these bathymetric data shall be used in the
computation.""",
        ),
#       -----------------------------------
        BOTTOM_SMOOTHINGS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Nombre de lissages effectues sur la topographie. chaque lissage,
effectue a l''aide d''une matrice de masse, est conservatif. Utilise
lorsque les donnees de bathymetrie donnent des resultats trop
irreguliers apres interpolation.""",
            ang = """ Number of smoothings on bottom topography. each smoothing is mass
conservative. to be used when interpolation of bathymetry on the mesh
gives very rough results.""",
        ),
#       -----------------------------------
        BOUNDARY_CONDITIONS_FILE = SIMP(statut ='o',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            fr = """ Nom du fichier contenant les types de conditions aux limites. Ce
fichier est rempli de facon automatique par le mailleur au moyen de
couleurs affectees aux noeuds des frontieres du domaine de calcul.""",
            ang = """ Name of the file containing the types of boundary conditions. This
file is filled automatically by the mesh generator through through
colours that are assigned to the boundary nodes.""",
        ),
#       -----------------------------------
        VALIDATION = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Option utilisee principalement pour le dossier de validation. Le
fichier des resultats du calcul precedent est alors considere comme une
reference a laquelle on va comparer le calcul. La comparaison est
effectuee par le sous-programme VALIDA qui peut etre une comparaison
avec une solution exacte par exemple.""",
            ang = """ This option is primarily used for the validation documents. The
PREVIOUS COMPUTATION FILE is then considered as a reference which the
computation is going to be compared with. The comparison is made by the
subroutine VALIDA, which can be modified as to so as to include, for
example,a comparison with an exact solution.""",
        ),
#       -----------------------------------
        b_VALIDATIONG = BLOC(condition="VALIDATION == True",
#       -----------------------------------
#           -----------------------------------
            REFERENCE_FILE_FORMAT = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ['SERAFIN?','SERAFIND','MED'],
                defaut = 'SERAFIN?',
                fr = """ Format du fichier de resultats du calcul precedent. Les valeurs
possibles sont : - SERAFIN : format standard simple precision pour
Telemac; - SERAFIND: format standard double precision pour Telemac; -
MED : format MED base sur HDF5""",
                ang = """ Previous computation results file format. Possible values are: -
SERAFIN : classical single precision format in Telemac; - SERAFIND:
classical double precision format in Telemac; - MED : MED format based
on HDF5""",
            ),
#           -----------------------------------
            REFERENCE_FILE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de resultats de reference pour la validation. Les resultats a
placer dans ce fichier seront a ecrire sur le canal 22.""",
                ang = """ Binary-coded result file for validation. The results to be entered
into this file shall be written on channel 22.""",
            ),
        ),
#       -----------------------------------
        USER_FILES = FACT(statut='f',
#       -----------------------------------
#           -----------------------------------
            BINARY_DATA_FILE_1_FORMAT = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM',
                into = ['BIN','SERAFIN','SERAFIND','MED'],
                defaut = 'BIN',
                fr = """ Format du fichier de donnes binaire. Les valeurs possibles sont : -
BIN : format binaire standard - SERAFIN : format standard simple
precision pour Telemac; - SERAFIND: format standard double precision
pour Telemac; - MED : format MED base sur HDF5""",
                ang = """ Binary data file 1 format. Possible values are: - BIN : Standard
binary format - SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac; - MED : MED
format based on HDF5""",
            ),
#           -----------------------------------
            BINARY_DATA_FILE_1 = SIMP(statut ='f',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de donnees code en binaire mis a la disposition de
l''utilisateur. Les donnees de ce fichier seront a lire sur le canal
24.""",
                ang = """ Binary-coded data file made available to the user. The data in this
file shall be read on channel 24.""",
            ),
#           -----------------------------------
            BINARY_DATA_FILE_2_FORMAT = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM',
                into = ['BIN','SERAFIN','SERAFIND','MED'],
                defaut = 'BIN',
                fr = """ Format du fichier de donnees binaire 2. Les valeurs possibles sont : -
BIN : format binaire standard - SERAFIN : format standard simple
precision pour Telemac; - SERAFIND: format standard double precision
pour Telemac; - MED : format MED base sur HDF5""",
                ang = """ Binary data file 2 format. Possible values are: - BIN : Standard
binary format - SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac; - MED : MED
format based on HDF5""",
            ),
#           -----------------------------------
            BINARY_DATA_FILE_2 = SIMP(statut ='f',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de donnees code en binaire mis a la disposition de
l''utilisateur. Les donnees de ce fichier seront a lire sur le canal
25.""",
                ang = """ Binary-coded data file made available to the user. The data in this
file shall be read on channel 25.""",
            ),
#           -----------------------------------
            FORMATTED_DATA_FILE_1 = SIMP(statut ='f',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de donnees formate mis a la disposition de l''utilisateur. Les
donnees de ce fichier seront a lire sur le canal 26.""",
                ang = """ Formatted data file made available to the user. The data in this file
shall be read on channel 26.""",
            ),
#           -----------------------------------
            FORMATTED_DATA_FILE_2 = SIMP(statut ='f',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de donnees formate mis a la disposition de l''utilisateur. Les
donnees de ce fichier seront a lire sur le canal 27.""",
                ang = """ Formatted data file made available to the user. The data in this file
shall be read on channel 27.""",
            ),
        ),
    ),
#   -----------------------------------
    INITIAL_STATE = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        INITIAL_CONDITIONS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ['ZERO ELEVATION','CONSTANT ELEVATION','ZERO DEPTH','CONSTANT DEPTH','SPECIAL','PARTICULIERES','PARTICULAR','TPXO SATELLITE ALTIMETRY'],
            defaut = 'ZERO ELEVATION',
            fr = """ Permet de definir les conditions initiales sur les hauteurs d''eau.
Les valeurs possibles sont : - COTE NULLE. Initialise la cote de surface
libre a 0. Les hauteurs d''eau initiales sont alors retrouvees en
faisant la difference entre les cotes de surface libre et du fond. -
COTE CONSTANTE .Initialise la cote de surface libre a la valeur donnee
par le mot-cle COTE INITIALE. Les hauteurs d''eau initiales sont
calculees comme precedemment. - HAUTEUR NULLE .Initialise les hauteurs
d''eau a 0. - HAUTEUR CONSTANTE. Initialise les hauteurs d''eau a la
valeur donnee par le mot-cle HAUTEUR INITIALE. - PARTICULIERES. Les
conditions initiales sur la hauteur d''eau doivent etre precisees dans
le sous-programme CONDIN. - ALTIMETRIE SATELLITE TPXO. Les conditions
initiales sur la hauteur d''eau et les vitesses sont etiblies sur la
base des donnees satellite TPXO dont les 8 premiers constistuents ont
ete extrait et sauves dans le fichier BASE DE DONNEES DE MAREE.""",
            ang = """ Makes it possible to define the initial conditions with the water
depth. The possible values are as follows: - ZERO ELEVATION-.
Initializes the free surface elevation to 0. The initial water depths
are then found by computing the difference between the free surface and
the bottom. - CONSTANT ELEVATION-. Initializes the water elevation to
the value given by the keyword -INITIAL ELEVATION-. The initial water
depths are computed as in the previous case. - ZERO DEPTH-. Initializes
the water depths to 0. - CONSTANT DEPTH-. Initializes the water depths
to the value given by the key-word -INITIAL DEPTH-. - SPECIAL-. The
initial conditions with the water depth should be stated in the CONDIN
subroutine. - TPXO SATELITE ALTIMETRY. The initial conditions on the
free surface and velocities are established from the TPXO satellite
program data, the harmonic constituents of which are stored in the TIDE
DATA BASE file.""",
        ),
#       -----------------------------------
        b_INITIAL_CONDITIONSG = BLOC(condition="INITIAL_CONDITIONS == 'CONSTANT ELEVATION'",
#       -----------------------------------
#           -----------------------------------
            INITIAL_ELEVATION = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """ Valeur utilisee avec l''option : CONDITIONS INITIALES - COTE
CONSTANTE""",
                ang = """ Value to be used with the option : INITIAL CONDITIONS -CONSTANT
ELEVATION""",
            ),
        ),
#       -----------------------------------
        b_INITIAL_CONDITIONSH = BLOC(condition="INITIAL_CONDITIONS == 'CONSTANT DEPTH'",
#       -----------------------------------
#           -----------------------------------
            INITIAL_DEPTH = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """ Valeur utilisee avec l''option : CONDITIONS INITIALES :-HAUTEUR
CONSTANTE-""",
                ang = """ Value to be used along with the option: INITIAL CONDITIONS -CONSTANT
DEPTH-""",
            ),
        ),
#       -----------------------------------
        b_INITIAL_CONDITIONSI = BLOC(condition="INITIAL_CONDITIONS == 'TPXO SATELLITE ALTIMETRY'",
#       -----------------------------------
#           -----------------------------------
            ASCII_DATABASE_FOR_TIDE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Base de donnees de constantes harmoniques tirees du fichier du modele
de maree. Ancien nom en version 6.1 : BASE DE DONNEES DE MAREE""",
                ang = """ Tide data base of harmonic constituents extracted from the tidal model
file. Old name in 6.1 version: TIDE DATA BASE""",
            ),
        ),
    ),
#   -----------------------------------
    RESTART = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        COMPUTATION_CONTINUED = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Determine si le calcul en cours est independant de tout autre resultat
ou est une reprise effectuee a partir du resultat d''un calcul
precedent. NON : Il s''agit du premier passage pour ce calcul et il est
necessaire de definir un jeu complet de conditions initiales OUI : Il
s''agit d''une reprise de calcul : les conditions initiales sont
constituees par le dernier pas de temps du FICHIER DU CALCUL PRECEDENT
du fichier des parametres utilise pour soumettre le calcul. Par contre,
l''ensemble des donnees du fichier des parametres peuvent etre
redefinies ; ce qui offre la possibilite de changer par exemple, le pas
de temps, le modele de turbulence, le frottement, d''ajouter ou retirer
un traceur ... De meme, il est necessaire de definir des conditions aux
limites (sous-programme BORD ou valeurs placees dans le fichier des
parametres), qui peuvent egalement etre modifiees.""",
            ang = """ Determines whether the computation under way is independent result or
is following an earlier result. NO: It is the first run for this
computation and a whole set of initial conditions should be defined.
YES: It follows a former computation: the initial conditions consist in
the last time step of the PREVIOUS COMPUTATION FILE in the steering file
used for submitting the computation. All the data from the steering file
may be defined once again, which provides an opportunity to change, for
example, the time step, the turbulence model, the friction, to add or
remove a tracer... It is also possible to define new boundary
conditions.""",
        ),
#       -----------------------------------
        b_COMPUTATION_CONTINUEDG = BLOC(condition="COMPUTATION_CONTINUED == True",
#       -----------------------------------
#           -----------------------------------
            PREVIOUS_COMPUTATION_FILE_FORMAT = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ['SERAFIN?','SERAFIND','MED'],
                defaut = 'SERAFIN?',
                fr = """ Format du fichier de resultats du calcul precedent. Les valeurs
possibles sont : - SERAFIN : format standard simple precision pour
Telemac; - SERAFIND: format standard double precision pour Telemac; -
MED : format MED base sur HDF5""",
                ang = """ Previous computation results file format. Possible values are: -
SERAFIN : classical single precision format in Telemac; - SERAFIND:
classical double precision format in Telemac; - MED : MED format based
on HDF5""",
            ),
#           -----------------------------------
            PREVIOUS_COMPUTATION_FILE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Nom d''un fichier contenant les resultats d''un calcul precedent
realise sur le meme maillage et dont le dernier pas de temps enregistre
va fournir les conditions initiales pour une suite de de calcul.""",
                ang = """ Name of a file containing the results of an earlier computation which
was made on the same mesh. The last recorded time step will provid the
initial conditions for the new computation.""",
            ),
#           -----------------------------------
            RECORD_NUMBER_FOR_RESTART = SIMP(statut ='o',
#           -----------------------------------
                typ = 'I',
                defaut = 0,
                fr = """ En cas de suite de calcul, numero de l''enregistrement de depart dans
le fichier du calcul precedent. 0 signifie qu''on prend le dernier
enregistrement""",
                ang = """ In case of COMPUTATION CONTINUED, record number to start from in the
PREVIOUS COMPUTATION FILE""",
            ),
        ),
#       -----------------------------------
        INITIAL_TIME_SET_TO_ZERO = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Remet le temps a zero en cas de suite de calcul""",
            ang = """ Initial time set to zero in case of restart""",
        ),
    ),
#   -----------------------------------
    GENERAL = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        PARALLEL_PROCESSORS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ NOMBRE DE PROCESSEURS EN CALCUL PARALLELE 0 : 1 machine, compilation
sans bibliotheque de parallelisme 1 : 1 machine, compilation avec
bibliotheque de parallelisme 2 : 2 processeurs ou machines en parallele
etc...""",
            ang = """ NUMBER OF PROCESSORS FOR PARALLEL PROCESSING 0 : 1 machine, compiling
without parallel library 1 : 1 machine, compiling with a parallel
library 2 : 2 processors or machines in parallel etc....""",
        ),
#       -----------------------------------
        CHECKING_THE_MESH = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Si oui on appelle le sous-programme checkmesh qui verifie la coherence
du maillage, points superposes, etc.""",
            ang = """ if this key word is equal to yes, a call to subroutine checkmesh will
look for errors in the mesh, superimposed points, etc.""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_BOUNDARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 30,
            fr = """ nombre maximal de frontieres differentes dans le maillage. Sert au
dimensionnement de la memoire, a augmenter si necessaire""",
            ang = """ maximal number of boundaries in the mesh. Used for dimensioning
arrays. Can be increased if needed""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_SOURCES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 20,
            fr = """ nombre maximal de points sources dans le maillage. Sert au
dimensionnement de la memoire, a augmenter si necessaire""",
            ang = """ maximal number of punctual sources in the mesh. Used for dimensioning
arrays. Can be increased if needed""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 20,
            fr = """ nombre maximal de traceurs. Sert au dimensionnement de la memoire, a
augmenter si necessaire""",
            ang = """ maximal number of tracers. Used for dimensioning arrays. Can be
increased if needed""",
        ),
    ),
)
# -----------------------------------------------------------------------
BOUNDARY_CONDITIONS = PROC(nom= "BOUNDARY_CONDITIONS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    PRESCRIBED_ELEVATIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """ Valeurs des cotes imposees aux frontieres liquides entrantes. Lire la
partie du mode d''emploi consacree aux conditions aux limites""",
        ang = """ Values of prescribed elevations at the inflow boundaries. The section
about boundary conditions is to be read in the manual""",
    ),
#   -----------------------------------
    PRESCRIBED_FLOWRATES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """ Valeurs des debits imposes aux frontieres liquides entrantes. Lire la
partie du mode d''emploi consacree aux conditions aux limites""",
        ang = """ Values of prescribed flowrates at the inflow boundaries. The section
about boundary conditions is to be read in the manual""",
    ),
#   -----------------------------------
    PRESCRIBED_VELOCITIES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """ Valeurs des vitesses imposees aux frontieres liquides entrantes. Lire
la partie du mode d''emploi consacree aux conditions aux limites""",
        ang = """ Values of prescribed velocities at the liquid inflow boundaries. Refer
to the section dealing with the boundary conditions""",
    ),
#   -----------------------------------
    STAGE_DISCHARGE_CURVES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', max='**',
        into = ["no","Z(Q)","Q(Z) not programmed"],
        fr = """ Indique si une courbe de tarage doit etre utilisee pour une frontiere
0:non 1:Z(Q) 2: Q(Z) (2 non programme)""",
        ang = """ Says if a discharge-elevation curve must be used for a given boundary
:NO 1:Z(Q) 2: Q(Z) (2 not programmed)""",
    ),
#   -----------------------------------
    b_STAGE_DISCHARGE_CURVESG = BLOC(condition="STAGE_DISCHARGE_CURVES != 'no'",
#   -----------------------------------
#       -----------------------------------
        STAGE_DISCHARGE_CURVES_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """ Nom du fichier contenant les courbes de tarage""",
            ang = """ Name of the file containing stage-discharge curves""",
        ),
    ),
#   -----------------------------------
    VELOCITY_PROFILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', max='**',
        into = ["constant normal profile","u and v given in the conlim file","normal velocity given in ubor in the conlim file","velocity proportional to square root of depth","velocity proportional to square root of depth, variant"],
        fr = """ 1:profil normal constant 2:u et v donnes dans le fichier conlim
3:vitesse normale donnee dans ubor dans le fichier conlim 4:profil en
racine de la profondeur 5:profil en racine de la profondeur, variante""",
        ang = """ 1:constant normal profile 2:u and v given in the conlim file 3:normal
velocity given in ubor in the conlim file 4:sqrt(depth) profile
5:sqrt(depth) profile, variant""",
    ),
#   -----------------------------------
    OPTION_FOR_LIQUID_BOUNDARIES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', max='**',
        into = ["classical","Thompson method based on characteristics"],
        fr = """ On donne 1 entier par frontiere liquide 1 : conditions aux limites
classiques 2 : methode de Thompson avec calcul de caracteristiques""",
        ang = """ One integer per liquid boundary is given 1 : classical boundary
conditions 2 : Thompson method based on characteristics""",
    ),
#   -----------------------------------
    LIQUID_BOUNDARIES_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'),
        defaut = '',
        fr = """ Fichier de variations en temps des conditions aux limites. Les donnees
de ce fichier seront a lire sur le canal 12.""",
        ang = """ Variations in time of boundary conditions. Data of this file are read
on channel 12.""",
    ),
#   -----------------------------------
    ELEMENTS_MASKED_BY_USER = SIMP(statut ='o',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """ SI OUI REMPLIR LE SOUS-PROGRAMME MASKOB""",
        ang = """ IF YES REWRITE SUBROUTINE MASKOB""",
    ),
)
# -----------------------------------------------------------------------
GENERAL_PARAMETERS = PROC(nom= "GENERAL_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    DEBUGGER = SIMP(statut ='o',
#   -----------------------------------
        typ = 'I',
        defaut = 0,
        fr = """ Pour imprimer la sequence des appels, mettre 1""",
        ang = """ If 1, calls of subroutines will be printed in the listing""",
    ),
#   -----------------------------------
    TIME = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        TIME_STEP = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1.,
            fr = """ Definit le pas de temps en secondes. Remarque : Pour une bonne
precision; il est souhaitable de choisir le pas de temps de telle sorte
que le nombre de Courant de propagation soit inferieur a 2 ; voir 3.
Ceci peut etre realisable en hydraulique fluviale ; mais ne l''est
pratiquement jamais en hydraulique maritime ou l''on peut atteindre des
valeurs de 50.""",
            ang = """ Specifies the time step in seconds.""",
        ),
#       -----------------------------------
        NUMBER_OF_TIME_STEPS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Definit le nombre de pas de temps effectues lors de l''execution du
code.""",
            ang = """ Specifies the number of time steps performed when running the code.""",
        ),
#       -----------------------------------
        DURATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ duree de la simulation. alternative au parametre nombre de pas de
temps. On en deduit le nombre de pas de temps en prenant l''entier le
plus proche de (duree du calcul/pas de temps). Si le nombre de pas de
temps est aussi donne, on prend la plus grande valeur""",
            ang = """ duration of simulation. May be used instead of the parameter NUMBER OF
TIME STEPS. The nearest integer to (duration/time step) is taken. If
NUMBER OF TIME STEPS is also given, the greater value is taken""",
        ),
#       -----------------------------------
        ORIGINAL_DATE_OF_TIME = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I', min= 3, max= 3,
            defaut = [1900,1,1],
            fr = """ Permet de fixer la date d''origine des temps du modele lors de la
prise en compte de la force generatrice de la maree.""",
            ang = """ Give the date of the time origin of the model when taking into account
the tide generating force.""",
        ),
#       -----------------------------------
        ORIGINAL_HOUR_OF_TIME = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I', min= 3, max= 3,
            defaut = [0,0,0],
            fr = """ Permet de fixer l''heure d''origine des temps du modele lors de la
prise en compte de la force generatrice de la maree.""",
            ang = """ Give the time of the time origin of the model when taking into account
of the tide generator force.""",
        ),
#       -----------------------------------
        STOP_IF_A_STEADY_STATE_IS_REACHED = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ A UTILISER AVEC LE MOT-CLE : CRITERES D''ARRET""",
            ang = """ TO BE USED WITH THE KEY-WORD: STOP CRITERIA""",
        ),
#       -----------------------------------
        b_STOP_IF_A_STEADY_STATE_IS_REACHEDG = BLOC(condition="STOP_IF_A_STEADY_STATE_IS_REACHED == True",
#       -----------------------------------
#           -----------------------------------
            STOP_CRITERIA = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R', min= 3, max= 3,
                defaut = [1.E-4,1.E-4,1.E-4],
                fr = """ Criteres d''arret pour un ecoulement permanent ces coefficients sont
respectivement appliques a 1) U et V 2) H 3) T A utiliser avec le
mot-cle : ARRET SI UN ETAT PERMANENT EST ATTEINT""",
                ang = """ Stop criteria for a steady state These coefficients are applied
respectively to 1) U and V 2) H 3) T To be used with the key-word: STOP
IF A STEADY STATE IS REACHED""",
            ),
        ),
#       -----------------------------------
        CONTROL_OF_LIMITS = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Utiliser avec le mot-cle : valeurs limites, le programme s''arrete si
les limites sur u,v,h ou t sont depassees""",
            ang = """ Use with the key-word : limit values, the program is stopped if the
limits on u,v,h, or t are trespassed""",
        ),
#       -----------------------------------
        b_CONTROL_OF_LIMITSG = BLOC(condition="CONTROL_OF_LIMITS == True",
#       -----------------------------------
#           -----------------------------------
            LIMIT_VALUES = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R', min= 8, max= 8,
                defaut = [-1000.,9000.,-1000.,1000.,-1000.,1000.,-1000.,1000.],
                fr = """ Utilise avec le mot-cle CONTROLE DES LIMITES valeurs mini et maxi
acceptables pour H,U,V et T dans l''ordre suivant : min(H) max(H) min(U)
max(U) min(V) max(V) min(T) max(T)""",
                ang = """ To be used with the key-word CONTROL OF LIMITS min and max acceptable
values for H,U,V et T in the following order : min(H) max(H) min(U)
max(U) min(V) max(V) min(T) max(T)""",
            ),
        ),
#       -----------------------------------
        VARIABLE_TIME_STEP = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Pas de temps variable pour avoir un nombre de courant souhaite""",
            ang = """ Variable time-step to get a given Courant number""",
        ),
#       -----------------------------------
        b_VARIABLE_TIME_STEPG = BLOC(condition="VARIABLE_TIME_STEP == True",
#       -----------------------------------
#           -----------------------------------
            DESIRED_COURANT_NUMBER = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 1.,
                fr = """ Nombre de Courant souhaite en cas de pas de temps variable""",
                ang = """ Desired Courant number when VARIABLE TIME-STEP is set to YES""",
            ),
        ),
    ),
#   -----------------------------------
    LOCATION = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        ORIGIN_COORDINATES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I', min= 2, max= 2,
            defaut = [0,0],
            fr = """ Valeur en metres, utilise pour eviter les trops grands nombres,
transmis dans le format Selafin mais pas d''autre traitement pour
l''instant""",
            ang = """ Value in metres, used to avoid large real numbers, added in Selafin
format, but so far no other treatment""",
        ),
#       -----------------------------------
        SPHERICAL_COORDINATES = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Choix des coordonnees spheriques pour la realisation du calcul ( pour
les grands domaines de calcul). Attention : cette option est etroitement
liee au maillage qui doit avoir ete saisi sur une carte marine en
projection de Mercator. Il faut de plus relever sur la carte la LATITUDE
DU POINT ORIGINE (autre mot-cle) qui correspond dans le maillage a
l''ordonnee y = 0.""",
            ang = """ Selection of spherical coordinates to perform the computation (for
large computation domains). Warning: this option is closely related to
the mesh that should have been entered onto a nautical chart drawn as
per Mercator projection The LATITUDE OF ORIGIN POINT (another keyword),
which corresponds to ordinate y=0 in the mesh, must moreover be given.""",
        ),
#       -----------------------------------
        b_SPHERICAL_COORDINATESG = BLOC(condition="SPHERICAL_COORDINATES == True",
#       -----------------------------------
#           -----------------------------------
            LATITUDE_OF_ORIGIN_POINT = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 48.,
                fr = """ Determine l''origine utilisee pour le calcul de latitudes lorsque
l''on effectue un calcul en coordonnees spheriques.""",
                ang = """ Determines the origin used for computing latitudes when a computation
is made in spherical coordinates. this latitude is in particular used to
compute the Coriolis force. In cartesian coordinates, Coriolis
coefficient is considered constant.""",
            ),
        ),
#       -----------------------------------
        LONGITUDE_OF_ORIGIN_POINT = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ Fixe la valeur de la longitude du point origine du modele, lors de
l''utilisation de la force generatrice de la maree.""",
            ang = """ Give the value of the longitude of the origin point of the model, when
taking into account of the tide generator force.""",
        ),
#       -----------------------------------
        NORTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ Direction du nord en degres, par rapport a l''axe des y dans le sens
trigonometrique. Notation decimale 10.5 signifie 10 degres et trente
minutes.""",
            ang = """ Angle of the North with the y axis, in degrees. 10.5 means 10 degrees
and 30 minutes.""",
        ),
#       -----------------------------------
        SPATIAL_PROJECTION_TYPE = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["CARTESIAN, NOT GEOREFERENCED","MERCATOR","LATITUDE LONGITUDE"],
            defaut = "CARTESIAN, NOT GEOREFERENCED",
            fr = """ Option 2 ou 3 obligatoire pour les coordonnees spheriques Option 3 :
latitude et longitude en degres !""",
            ang = """ Option 2 or 3 mandatory for spherical coordinates Option 3: latitude
and longitude in degrees!""",
        ),
    ),
)
# -----------------------------------------------------------------------
PHYSICAL_PARAMETERS = PROC(nom= "PHYSICAL_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    FRICTION = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        MAXIMUM_NUMBER_OF_FRICTION_DOMAINS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 10,
            fr = """ nombre maximal de zones pouvant etre definies pour le frottement. Peut
etre augmente si necessaire""",
            ang = """ maximal number of zones defined for the friction. Could be increased
if needed""",
        ),
#       -----------------------------------
        FRICTION_DATA = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Lois de frottements definies par zone""",
            ang = """ Friction law defined by area""",
        ),
#       -----------------------------------
        FRICTION_DATA_FILE = SIMP(statut ='o',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """ fichier de donnees pour le frottement""",
            ang = """ friction data file""",
        ),
#       -----------------------------------
        LAW_OF_BOTTOM_FRICTION = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["NO FRICTION","HAALAND","CHEZY","STRICKLER","MANNING","NIKURADSE"],
            fr = """ selectionne le type de formulation utilisee pour le calcul du
frottement sur le fond. Les lois possibles sont les suivantes (cf. Note
de principe) : - 0 : pas de frottement sur le fond; 1 : formule de
Haaland 2 : formule de Chezy 3 : formule de STRICKLER 4 : formule de
MANNING 5 : formule de NIKURADSE""",
            ang = """ Selects the type of formulation used for the bottom friction. The
possible laws are as follows (refer to the Principle note): 0: no
friction against bottom, 1: Haaland''s formula 2: CHEZY''s formula 3:
STRICKLER''s formula 4: MANNING''s formula 5: NIKURADSE''s formula""",
        ),
#       -----------------------------------
        b_LAW_OF_BOTTOM_FRICTIONG = BLOC(condition="LAW_OF_BOTTOM_FRICTION != 'NO FRICTION'",
#       -----------------------------------
#           -----------------------------------
            FRICTION_COEFFICIENT = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 50.,
                fr = """ Fixe la valeur du coefficient de frottement pour la formulation
choisie. Attention; la signification de ce chiffre varie suivant la
formule choisie : 1 : coefficient lineaire 2 : coefficient de Chezy 3 :
coefficient de Strickler 4 : coefficient de Manning 5 : hauteur de
rugosite de Nikuradse""",
                ang = """ Sets the value of the friction coefficient for the selected
formulation. It is noteworthy that the meaning of this figure changes
according to the selected formula (Chezy, Strickler, etc.) : 1 : linear
coefficient 2 : Chezy coefficient 3 : Strickler coefficient 4 : Manning
coefficient 5 : Nikuradse grain size""",
            ),
        ),
#       -----------------------------------
        MANNING_DEFAULT_VALUE_FOR_COLEBROOK_WHITE_LAW = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.02,
            fr = """ valeur par defaut du manning pour la loi de frottement de
Colebrook-White (loi numero 7)""",
            ang = """ Manning default value for the friction law of Colebrook-White (law
number 7)""",
        ),
#       -----------------------------------
        DEPTH_IN_FRICTION_TERMS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["nodal","average"],
            defaut = "nodal",
            fr = """ 1 : nodale 2 : moyenne""",
            ang = """ 1: nodal 2: average""",
        ),
#       -----------------------------------
        NON_SUBMERGED_VEGETATION_FRICTION = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ calcul du frottement du a la vegetation non submergee""",
            ang = """ friction calculation of the non-submerged vegetation""",
        ),
#       -----------------------------------
        b_NON_SUBMERGED_VEGETATION_FRICTIONG = BLOC(condition="NON_SUBMERGED_VEGETATION_FRICTION == True",
#       -----------------------------------
#           -----------------------------------
            DIAMETER_OF_ROUGHNESS_ELEMENTS = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.006,
                fr = """ diametre des elements de frottements""",
                ang = """ diameter of roughness element""",
            ),
#           -----------------------------------
            SPACING_OF_ROUGHNESS_ELEMENTS = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.14,
                fr = """ espacement des elements de frottement""",
                ang = """ spacing of rouhness element""",
            ),
        ),
#       -----------------------------------
        LAW_OF_FRICTION_ON_LATERAL_BOUNDARIES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["NO FRICTION","HAALAND","CHEZY","STRICKLER","MANNING","NIKURADSE","LOG LAW","COLEBROOK-WHITE"],
            defaut = "NO FRICTION",
            fr = """ selectionne le type de formulation utilisee pour le calcul du
frottement sur les parois laterales. Les lois possibles sont les
suivantes (cf. Note de principe) : 0 : pas de frottement 1 : lineaire 2
: Chezy 3 : Strickler 4 : Manning 5 : formule de NIKURADSE 6 : loi en
log 7 : Colebrook-White""",
            ang = """ Selects the type of formulation used for the friction on lateral
boundaries. The possible laws are as follows (refer to the Principle
note): 0: no friction 1: linear 2: Chezy 3: Strickler 4: Manning 5:
NIKURADSE''s formula 6 : law log 7 : Colebrook-White""",
        ),
#       -----------------------------------
        b_LAW_OF_FRICTION_ON_LATERAL_BOUNDARIESG = BLOC(condition="LAW_OF_FRICTION_ON_LATERAL_BOUNDARIES != 'NO FRICTION'",
#       -----------------------------------
#           -----------------------------------
            ROUGHNESS_COEFFICIENT_OF_BOUNDARIES = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 100.,
                fr = """ Fixe la valeur du coefficient de frottement sur les frontieres solides
avec un regime turbulent rugueux sur les bords du domaine. meme
convention que pour le coefficient de frottement: 1 : non programme 2 :
coefficient de Chezy 3 : coefficient de Strickler 4 : coefficient de
Manning 5 : hauteur de rugosite de Nikuradse""",
                ang = """ Sets the value of the friction coefficient of the solid boundary with
the bed roughness option. Same meaning than friction coefficient""",
            ),
        ),
#       -----------------------------------
        DEFINITION_OF_ZONES = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Declenche l''appel a def\_zones, pour donner un numero de zone a
chaque point""",
            ang = """ Triggers the call to def\_zones to give a zone number to every point""",
        ),
#       -----------------------------------
        ZONES_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """ Fichier des zones avec sur chaque ligne numero de point numero de
zone""",
            ang = """ Zones file, with on every line: point number zone number""",
        ),
    ),
#   -----------------------------------
    METEOROLOGY = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        WIND = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Prise en compte ou non des effets du vent.""",
            ang = """ Determines whether the wind effects are to be taken into account or
not.""",
        ),
#       -----------------------------------
        b_WINDG = BLOC(condition="WIND == True",
#       -----------------------------------
#           -----------------------------------
            WIND_VELOCITY_ALONG_X = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """ Composante de la vitesse du vent suivant l''axe des x (m/s).""",
                ang = """ Wind velocity, component along x axis (m/s).""",
            ),
#           -----------------------------------
            WIND_VELOCITY_ALONG_Y = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """ Composante de la vitesse du vent suivant l''axe des y (m/s).""",
                ang = """ Wind velocity, component along y axis (m/s).""",
            ),
#           -----------------------------------
            THRESHOLD_DEPTH_FOR_WIND = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 1.,
                fr = """ Retire la force due au vent dans les petites profondeurs""",
                ang = """ Wind is not taken into account for small depths""",
            ),
#           -----------------------------------
            COEFFICIENT_OF_WIND_INFLUENCE = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """ Fixe la valeur du coefficient d''entrainement du vent (cf. Note de
principe).""",
                ang = """ Sets the value of the wind driving coefficient. Refer to principle
note.""",
            ),
#           -----------------------------------
            OPTION_FOR_WIND = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ["constant in time and space","variable in time given by formated file","variable in time and space given by formated file"],
                defaut = "constant in time and space",
                fr = """ donne les options pour introduire le vent: 1: constant en temps et en
espace (donne par le mot cle VITESSE ET DIRECTION DU VENT) 2: variable
en temps donne par fichier formate 3: variable en temps et en espace
donne par fichier formate""",
                ang = """ gives option for managing the wind: 1: constant in time and space,
given by keyword SPEED AND DIRECTION OF WIND 2: variable in time and
(constant in space), given by formated file 3: variable in time and
space""",
            ),
#           -----------------------------------
            b_OPTION_FOR_WINDG = BLOC(condition="OPTION_FOR_WIND == 1",
#           -----------------------------------
#               -----------------------------------
                SPEED_AND_DIRECTION_OF_WIND = SIMP(statut ='o',
#               -----------------------------------
                    typ = 'R', min= 2, max= 2,
                    defaut = [0.,0.],
                    fr = """ Donne la vitesse et la direction (en degres de 0 a 360, 0 etant y=0 et
x=+inf) du vent lorsqu ils sont consant en temps et en espace (mot cle
OPTION DU VENT = 1)""",
                    ang = """ gives the speed and direction (degre (from 0 to 360), 0 given y=0 anx
x=+infinity) when they are constant in time and space (keyword OPTION
FOR WIND = 1)""",
                ),
            ),
        ),
#       -----------------------------------
        AIR_PRESSURE = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Permet de decider si l''on prend ou non en compte l''influence d''un
champ de pression.""",
            ang = """ Provided to decide whether the influence of an atmosphere field is
taken into account or not.""",
        ),
#       -----------------------------------
        b_AIR_PRESSUREG = BLOC(condition="AIR_PRESSURE == True",
#       -----------------------------------
#           -----------------------------------
            VALUE_OF_ATMOSPHERIC_PRESSURE = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 100000.,
                fr = """ donne la valeur de la pression atmospherique lorsquelle est constante
en temps et en espace""",
                ang = """ gives the value of atmospheric pressure when it is contant in time and
space""",
            ),
        ),
#       -----------------------------------
        RAIN_OR_EVAPORATION = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Pour ajouter un apport ou une perte d''eau en surface. Voir le mot-cle
PLUIE OU EVAPORATION EN MM PAR JOUR""",
            ang = """ to add or remove water at the free surface. See the key-word RAIN OR
EVAPORATION IN MM PER DAY""",
        ),
#       -----------------------------------
        b_RAIN_OR_EVAPORATIONG = BLOC(condition="RAIN_OR_EVAPORATION == True",
#       -----------------------------------
#           -----------------------------------
            RAIN_OR_EVAPORATION_IN_MM_PER_DAY = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.0,
                fr = """ Pour ajouter un apport ou une perte d''eau en surface""",
                ang = """ to add or remove water at the free surface""",
            ),
        ),
    ),
#   -----------------------------------
    WAVE = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        WAVE_DRIVEN_CURRENTS = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Active la prise en compte des courants de houle""",
            ang = """ Wave driven currents are taken into account.""",
        ),
#       -----------------------------------
        b_WAVE_DRIVEN_CURRENTSG = BLOC(condition="WAVE_DRIVEN_CURRENTS == True",
#       -----------------------------------
#           -----------------------------------
            RECORD_NUMBER_IN_WAVE_FILE = SIMP(statut ='o',
#           -----------------------------------
                typ = 'I',
                defaut = 1,
                fr = """ Numero d enregistrement dans le fichier des courants de houle""",
                ang = """ Record number to read in the wave driven currents file""",
            ),
        ),
    ),
#   -----------------------------------
    ESTIMATION = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        PARAMETER_ESTIMATION = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ['','FRICTION','FROTTEMENT, STEADY'],
            defaut = '',
            fr = """ Liste des parametres a estimer, choix : FROTTEMENT ou FROTTEMENT,
PERMANENT""",
            ang = """ List of parameter to be estimated, choice : FRICTION or FRICTION,
STEADY""",
        ),
#       -----------------------------------
        COST_FUNCTION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            into = ["Computed with h,u,v","Computed with c,u,v"],
            defaut = "Computed with h,u,v",
            fr = """ 1 : calculee sur h, u , v 2 : calculee avec c, u , v""",
            ang = """ 1: computed with h, u , v 2: computed with c, u , v""",
        ),
#       -----------------------------------
        IDENTIFICATION_METHOD = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["list of tests","gradient simple","conj gradient","Lagrange interp."],
            defaut = "gradient simple",
            fr = """ 0 : plan d''experience 1 : gradient simple 2 : gradient conj. 3 :
interp. de Lagrange""",
            ang = """ 0 : list of tests 1: gradient 2 : conj. gradient 3 : lagrange interp.""",
        ),
#       -----------------------------------
        TOLERANCES_FOR_IDENTIFICATION = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 4, max= 4,
            defaut = [1.E-3,1.E-3,1.E-3,1.E-4],
            fr = """ 4 nombres : precision absolue sur H, U, V, et precision relative sur
la fonction cout""",
            ang = """ 4 numbers: absolute precision on H, U V, and relative precision on the
cost function""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_IDENTIFICATION = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 20,
            fr = """ chaque iteration comprend au moins un calcul direct et un calcul
adjoint""",
            ang = """ every iteration implies at least a direct and an adjoint computation""",
        ),
    ),
#   -----------------------------------
    SOURCES = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_SOURCES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """ Valeurs des abscisses des sources de debit et de traceur.""",
            ang = """ abscissae of sources of flowrate and/or tracer""",
        ),
#       -----------------------------------
        ORDINATES_OF_SOURCES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """ Valeurs des ordonnees des sources de debit et de traceur.""",
            ang = """ ordinates of sources of flowrate and/or tracer""",
        ),
#       -----------------------------------
        WATER_DISCHARGE_OF_SOURCES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """ Valeurs des debits des sources.""",
            ang = """ values of water discharge of sources""",
        ),
#       -----------------------------------
        VELOCITIES_OF_THE_SOURCES_ALONG_X = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """ Vitesses du courant a chacune des sources. Si elles ne sont pas
donnees, on considere que la vitesse est celle du courant""",
            ang = """ Velocities at the sources. If they are not given, the velocity of the
flow at this location is taken""",
        ),
#       -----------------------------------
        VELOCITIES_OF_THE_SOURCES_ALONG_Y = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """ Vitesses du courant a chacune des sources""",
            ang = """ Velocities at the sources""",
        ),
#       -----------------------------------
        TYPE_OF_SOURCES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["Normal","Dirac"],
            defaut = "Normal",
            fr = """ 1: Source portee par une base elements finis 2: Source portee par une
fonction de Dirac""",
            ang = """ 1: Source term multiplied by a finite element basis 2: Source term
multiplied by a Dirac function""",
        ),
#       -----------------------------------
        SOURCES_FILE = SIMP(statut ='o',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """ Nom du fichier contenant les informations variables en temps des
sources""",
            ang = """ Name of the file containing time-dependent information on sources""",
        ),
    ),
#   -----------------------------------
    WATER_QUALITY_INFO = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        WATER_QUALITY_PROCESS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ donne le numero du processus waq (1 ou 2 ou 3 ou 4 ou 5) 0- RIEN,
1-O2, 2-BIOMASS, 3-EUTRO 4-MICROPOL 5-THERMIC)""",
            ang = """ gives the waq process number (from 1 to 5) 0-NOTHING, 1-O2, 2-BIOMASS,
3-EUTRO 4-MICROPOL 5-THERMIC)""",
        ),
    ),
#   -----------------------------------
    CORIOLIS_EFFECT = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        CORIOLIS = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Prise en compte ou non de la force de Coriolis.""",
            ang = """ The Coriolis force is taken into account or ignored.""",
        ),
#       -----------------------------------
        CORIOLIS_COEFFICIENT = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ Fixe la valeur du coefficient de la force de Coriolis. Celui-ci doit
etre calcule en fonction de la latitude l par la formule FCOR = 2w
sin(l) , w etant la vitesse de rotation de la terre. w = 7.2921 10-5
rad/s Les composantes de la force de Coriolis sont alors : FU = FCOR x V
FV = - FCOR x U""",
            ang = """ Sets the value of the Coriolis force coefficient, in cartesian
coordinates. This coefficient, denoted FCOR in the code, should be equal
to 2 w sin(l)d where w denotes the earth angular speed of rotation and l
the latitude. w = 7.27 10-5 rad/sec The Coriolis force components are
then: FU = FCOR x V, FV = -FCOR x U In spherical coordinates, the
latitudes are known""",
        ),
    ),
#   -----------------------------------
    TSUNAMI = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        OPTION_FOR_TSUNAMI_GENERATION = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["No Tsunami","Tsunami generated on the basis of the Okada model 1992"],
            defaut = "No Tsunami",
            fr = """""",
            ang = """""",
        ),
#       -----------------------------------
        PHYSICAL_CHARACTERISTICS_OF_THE_TSUNAMI = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            defaut = [100.,210000.,75000.,13.6,81.,41.,110.,0.,0.,3.],
            fr = """""",
            ang = """""",
        ),
    ),
#   -----------------------------------
    OTHERS_PHY = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        WATER_DENSITY = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1000.,
            fr = """ Fixe la valeur de la masse volumique de l''eau.""",
            ang = """ set the value of water density""",
        ),
#       -----------------------------------
        GRAVITY_ACCELERATION = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 9.81,
            fr = """ Fixe la valeur de l''acceleration de la pesanteur.""",
            ang = """ Set the value of the acceleration due to gravity.""",
        ),
#       -----------------------------------
        VERTICAL_STRUCTURES = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Prise en compte de la force de trainee de structures verticales (il
faut alors remplir la subroutine DRAGFO)""",
            ang = """ drag forces from vertical structures are taken into account.
(subroutine DRAGFO must then be implemented)""",
        ),
    ),
)
# -----------------------------------------------------------------------
SECONDARY_CURRENTS_INFO = PROC(nom= "SECONDARY_CURRENTS_INFO",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    SECONDARY_CURRENTS = SIMP(statut ='o',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """ Pour prendre en compte les courants secondaires""",
        ang = """ Using the parametrisation for secondary currents""",
    ),
#   -----------------------------------
    b_SECONDARY_CURRENTSG = BLOC(condition="SECONDARY_CURRENTS == True",
#   -----------------------------------
#       -----------------------------------
        PRODUCTION_COEFFICIENT_FOR_SECONDARY_CURRENTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 7.071,
            fr = """ Une constante dans les termes de creation de Omega""",
            ang = """ A constant in the production terms of Omega""",
        ),
#       -----------------------------------
        DISSIPATION_COEFFICIENT_FOR_SECONDARY_CURRENTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 5.E-1,
            fr = """ Coefficient de dissipation de Omega""",
            ang = """ Coefficient of dissipation term of Omega""",
        ),
    ),
)
# -----------------------------------------------------------------------
TURBULENCE = PROC(nom= "TURBULENCE",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    VELOCITY_DIFFUSIVITY = SIMP(statut ='o',
#   -----------------------------------
        typ = 'R',
        defaut = 1.E-6,
        fr = """ Fixe de facon uniforme pour l''ensemble du domaine; la valeur du
coefficient de diffusion de viscosite globale (dynamique + turbulente).
Cette valeur peut avoir une influence non negligeable sur la forme et la
taille des recirculations.""",
        ang = """ Sets, in an even way for the whole domain, the value of the
coefficient of global (dynamic+turbulent) viscosity. this value may have
a significant effect both on the shapes and sizes of recirculation
zones.""",
    ),
#   -----------------------------------
    TURBULENCE_MODEL = SIMP(statut ='o',
#   -----------------------------------
        typ = 'TXM',
        into = ["CONSTANT VISCOSITY","ELDER","K-EPSILON MODEL","SMAGORINSKI","MIXING LENGTH","SPALART-ALLMARAS"],
        defaut = "CONSTANT VISCOSITY",
        fr = """ 3 choix sont possibles actuellement : viscosite constante (1) modele
de Elder (2) ou modele k-epsilon (3). Attention : si on choisit
l''option 1 il ne faut pas oublier d''ajuster la valeur du mot-cle
COEFFICIENT DE DIFFUSION DES VITESSES. si on choisit l''option 2 il ne
faut pas oublier d''ajuster les deux valeurs du mot-cle : COEFFICIENTS
ADIMENSIONNELS DE DISPERSION Si on choisit l''option 3; ce meme
parametre doit retrouver sa vraie valeur physique car elle est utilisee
comme telle dans le modele de turbulence""",
        ang = """ The current alternatives are as follows: constant viscosity (1)
elder''s model (2) or k-epsilon model (3). NOTE: when option 1 is
chosen, it should be kept in mind that the value of the keyword VELOCITY
DIFFUSIVITY has to be ajusted. When option 2 is chosen, the two values
of key-word : NON-DIMENSIONAL DISPERSION COEFFICIENTS are used When
option 3 is chosen, this parameter should recover its true physical
value, since it is used as such in the turbulence model.""",
    ),
#   -----------------------------------
    b_TURBULENCE_MODELG = BLOC(condition="TURBULENCE_MODEL == 'CONSTANT VISCOSITY'",
#   -----------------------------------
    ),
#   -----------------------------------
    b_TURBULENCE_MODELH = BLOC(condition="TURBULENCE_MODEL == 'Elder'",
#   -----------------------------------
#       -----------------------------------
        NON_DIMENSIONAL_DISPERSION_COEFFICIENTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            defaut = [6.,0.6],
            fr = """ coefficients longitudinal et transversal dans la formule de Elder.
Utilises uniquement avec le modele de turbulence 2""",
            ang = """ Longitudinal and transversal coefficients in elder s formula. Used
only with turbulence model number 2""",
        ),
    ),
#   -----------------------------------
    ACCURACY_OF_SPALART_ALLMARAS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1.E-9],
        fr = """Fixe la precision demandee sur le modele spalart-allmaras pour
le test d''arret dans l''etape de diffusion et termes sources de k et
epsilon.""",
        ang = """Sets the required accuracy for the model spalart-allmaras in
the diffusion and source-terms step of the k-epsilon model.""",
    ),
#   -----------------------------------
    INFORMATION_ABOUT_SPALART_ALLMARAS_MODEL = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = True,
        fr = """si oui les informations du solveur du modele spalart-allmaras
sont imprimees""",
        ang = """if yes, informations about solver of spalart-allmaras model
are printed to the listing""",
    ),
#   -----------------------------------
    SOLVER_INFO = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        SOLVER_FOR_K_EPSILON_MODEL = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["conjugate gradient","conjugate residuals","conjugate gradient on normal equation","minimum error","conjugate gradient squared","conjugate gradient squared stabilised (cgstab)","gmres (see option for the solver for k-epsilon model)","direct"],
            defaut = "conjugate gradient",
            fr = """ Permet de choisir le solveur utilise pour la resolution du systeme du
modele k-epsilon : 1 : gradient conjugue 2 : residu conjugue 3 :
gradient conjugue sur equation normale 4 : erreur minimale 5 : gradient
conjugue carre 6 : gradient conjugue carre stabilise (cgstab) 7 : gmres
(voir aussi option du solveur pour le modele k-epsilon) 8 : direct""",
            ang = """ Makes it possible to select the solver used for solving the system of
the k-epsilon model. 1: conjugate gradient 2: conjugate residuals 3:
conjugate gradient on normal equation 4: minimum error 5: conjugate
gradient squared 6: conjugate gradient squared stabilised (cgstab) 7:
gmres (see option for the solver for k-epsilon model) 8: direct""",
        ),
#       -----------------------------------
        OPTION_FOR_THE_SOLVER_FOR_K_EPSILON_MODEL = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 2,
            fr = """ si le solveur est GMRES (7) le mot cle est la dimension de l''espace
de KRILOV (valeurs conseillees entre 2 et 15)""",
            ang = """ WHEN GMRES (7) IS CHOSEN, DIMENSION OF THE KRYLOV SPACE TRY VALUES
BETWEEN 2 AND 15""",
        ),
#       -----------------------------------
        PRECONDITIONING_FOR_K_EPSILON_MODEL = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["diagonal","no preconditioning","crout","diagonal and crout"],
            defaut = "diagonal",
            fr = """ Permet de preconditionner le systeme relatif au modele k-epsilon 0 :
pas de preconditionnement; 2 : preconditionnement diagonal. 7 :
preconditionnement de Crout par element.""",
            ang = """ Preconditioning of the linear system in the diffusion step of the
k-epsilon model. 0: no preconditioning 2: diagonal preconditioning 7:
Crout''s preconditioning per element""",
        ),
    ),
#   -----------------------------------
    ACCURACY = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        ACCURACY_OF_K = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1.E-9,
            fr = """ Fixe la precision demandee sur k pour le test d''arret dans l''etape
de diffusion et termes sources du modele k-epsilon.""",
            ang = """ Sets the required accuracy for computing k in the diffusion and source
terms step of the k-epsilon model.""",
        ),
#       -----------------------------------
        ACCURACY_OF_EPSILON = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1.E-9,
            fr = """ Fixe la precision demandee sur epsilon pour le test d''arret dans
l''etape de diffusion et termes sources de k et epsilon.""",
            ang = """ Sets the required accuracy for computing epsilon in the diffusion and
source-terms step of the k-epsilon model.""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_K_AND_EPSILON = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 50,
            fr = """ Fixe le nombre maximum d''iterations accepte lors de la resolution du
systeme diffusion-termes sources du modele k-epsilon.""",
            ang = """ Sets the maximum number of iterations that are acceptable when solving
the diffusion source-terms step of the k-epsilon model.""",
        ),
    ),
#   -----------------------------------
    S_TURBULENCE = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        TURBULENCE_REGIME_FOR_SOLID_BOUNDARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            into = ["smooth","rough"],
            defaut = "rough",
            fr = """ Permet de choisir le regime de turbulence aux parois 1 : regime
turbulent lisse. 2 : regime turbulent rugueux.""",
            ang = """ Provided for selecting the type of friction on the walls 1: smooth 2:
rough""",
        ),
#       -----------------------------------
        INFORMATION_ABOUT_K_EPSILON_MODEL = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Donne le nombre d''iterations du solveur de l''etape de diffusion et
termes sources du modele k-epsilon.""",
            ang = """ Gives the number of iterations of the solver in the diffusion and
source terms step of the k-epsilon model.""",
        ),
#       -----------------------------------
        ADVECTION_OF_K_AND_EPSILON = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Prise en compte ou non de la convection de k et epsilon.""",
            ang = """ The k and epsilon advection is taken into account or ignored.""",
        ),
#       -----------------------------------
        b_ADVECTION_OF_K_AND_EPSILONG = BLOC(condition="ADVECTION_OF_K_AND_EPSILON == True",
#       -----------------------------------
#           -----------------------------------
            SCHEME_FOR_ADVECTION_OF_K_EPSILON = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM',
                into = ["NO ADVECTION","CHARACTERISTICS","EXPLICIT + SUPG","EXPLICIT LEO POSTMA","EXPLICIT + MURD SCHEME N","EXPLICIT + MURD SCHEME PSI","LEO POSTMA FOR TIDAL FLATS","N-SCHEME FOR TIDAL FLATS","ERIA SCHEME FOR TIDAL FLATS"],
                defaut = "CHARACTERISTICS",
                fr = """ Choix du schema de convection pour k et epsilon, remplace FORME DE LA
CONVECTION""",
                ang = """ Choice of the advection scheme for k and epsilon, replaces TYPE OF
ADVECTION""",
            ),
        ),
#       -----------------------------------
        SCHEME_OPTION_FOR_ADVECTION_OF_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Si present remplace et a priorite sur : OPTION POUR LES
CARACTERISTIQUES OPTION DE SUPG Si schema PSI ou N : 1=explicite
2=predicteur-correcteur 3=predicteur-correcteur deuxieme ordre en temps
4=implicite""",
            ang = """ If present replaces and has priority over: OPTION FOR CHARACTERISTICS
SUPG OPTION if N or PSI SCHEME: 1=explicit 2=predictor-corrector 3=
predictor-corrector second-order in time 4= implicit""",
        ),
#       -----------------------------------
        TIME_STEP_REDUCTION_FOR_K_EPSILON_MODEL = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 1.,
            fr = """ Coefficient reducteur du pas de temps pour le modele k-epsilon (qui
est normalement identique a celui du systeme hydrodynamique).
Utilisation deconseillee""",
            ang = """ Time step reduction coefficient for k-epsilon model (which is normally
same the same as that of the hydrodynamic system) Not recommended for
use.""",
        ),
    ),
)
# -----------------------------------------------------------------------
NUMERICAL_PARAMETERS = PROC(nom= "NUMERICAL_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    EQUATIONS = SIMP(statut ='o',
#   -----------------------------------
        typ = 'TXM',
        into = ['SAINT-VENANT FE','SAINT-VENANT FV','BOUSSINESQ'],
        defaut = 'SAINT-VENANT FE',
        fr = """ CHOIX DES EQUATIONS A RESOUDRE : SAINT-VENANT ELEMENTS FINIS,
SAINT-VENANT VOLUMES FINIS OU BOUSSINESQ 20 CARACTERES""",
        ang = """ CHOICE OF EQUATIONS TO SOLVE : SAINT-VENANT FINITE ELEMENTS,
SAINT-VENANT FINITE VOLUMES OR BOUSSINESQ 20 CHARACTERS""",
    ),
#   -----------------------------------
    TREATMENT_OF_THE_LINEAR_SYSTEM = SIMP(statut ='o',
#   -----------------------------------
        typ = 'TXM',
        into = ["coupled","Wave equation"],
        defaut = "coupled",
        fr = """ 1 : Traitement couple 2 : equation d onde""",
        ang = """ 1 : Coupled 2 : wave equation""",
    ),
#   -----------------------------------
    FINITE_VOLUME_SCHEME = SIMP(statut ='o',
#   -----------------------------------
        typ = 'TXM',
        into = ["Roe scheme","kinetic order 1","kinetic order 2","Zokagoa scheme order 1","Tchamen scheme order 1","HLLC scheme order 1","WAF scheme order 2"],
        defaut = "kinetic order 1",
        fr = """ 0 : schema de Roe 1 : cinetique ordre 1 2 : cinetique ordre 2 3 :
schema de Zokagoa 4 : schema de Tchamen 5 : HLLC ordre 1 6 : WAF ordre
2""",
        ang = """ 0: Roe scheme 1: kinetic order 1 2: kinetic order 2 3 : Zokagoa scheme
4 : Tchamen scheme 5 : HLLC order 1 6 : WAF order 2""",
    ),
#   -----------------------------------
    SOLVER_INFO = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        SOLVER = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["conjugate gradient on a normal equation","conjugate gradient","conjugate residual","minimum error","cgstab","gmres","direct"],
            defaut = "conjugate gradient on a normal equation",
            fr = """ Permet de choisir le solveur utilise pour la resolution de l''etape de
propagation. Toutes les methodes proposees actuellement s''apparentent
au Gradient Conjugue. Ce sont : 1 : gradient conjugue 2 : residu
conjugue 3 : gradient conjugue sur equation normale 4 : erreur minimale
5 : gradient conjugue carre (non programme) 6 : gradient conjugue carre
stabilise (cgstab) 7 : gmres (voir aussi option du solveur) 8 : direct""",
            ang = """ Makes it possible to select the solver used for solving the
propagation step. All the currently available methods are variations of
the Conjugate Gradient method. They are as follows: 1: conjugate
gradient 2: conjugate residual 3: conjugate gradient on a normal
equation 4: minimum error 5: conjugate gradient squared (not
implemented) 6: conjugate gradient squared stabilised (cgstab) 7: gmres
(see option for solver) 8: direct""",
        ),
#       -----------------------------------
        b_SOLVERG = BLOC(condition="SOLVER == 'GMRES'",
#       -----------------------------------
#           -----------------------------------
            SOLVER_OPTION = SIMP(statut ='o',
#           -----------------------------------
                typ = 'I',
                defaut = 2,
                fr = """ si le solveur est GMRES (7) le mot cle est la dimension de l''espace
de KRYLOV (valeurs conseillees entre 2 et 15)""",
                ang = """ WHEN GMRES (7) IS CHOSEN, DIMENSION OF THE KRYLOV SPACE TRY VALUES
BETWEEN 2 AND 15""",
            ),
        ),
#       -----------------------------------
        SOLVER_ACCURACY = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1.E-4,
            fr = """ Precision demandee pour la resolution de l''etape de propagation (cf.
Note de principe).""",
            ang = """ Required accuracy for solving the propagation step (refer to Principle
note).""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_SOLVER = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 100,
            fr = """ Les algorithmes utilises pour la resolution de l''etape de propagation
etant iteratifs; il est necessaire de limiter le nombre d''iterations
autorisees. Remarque : un maximum de 40 iterations par pas de temps
semble raisonnable.""",
            ang = """ Since the algorithms used for solving the propagation step are
iterative, the allowed number of iterations should be limited. NOTE: a
maximum number of 40 iterations per time step seems to be reasonable.""",
        ),
#       -----------------------------------
        CONTINUITY_CORRECTION = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Corrige les vitesses sur les points avec hauteur imposee ou
l''equation de continuite n''a pas ete resolue""",
            ang = """ Correction of the velocities on points with a prescribed elevation,
where the continuity equation has not been solved""",
        ),
#       -----------------------------------
        PRECONDITIONING = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["diagonal","no preconditioning","block-diagonal (4-9 matrices)","absolute value of diagonal","crout","gauss-seidel","diagonal and crout"],
            defaut = "diagonal",
            fr = """ Permet de preconditionner le systeme de l''etape de propagation afin
d''accelerer la convergence lors de sa resolution. - 0 : pas de
preconditionnement; - 2 : preconditionnement diagonal. - 3 :
preconditionnement diagonal-bloc - 7 : preconditionnement de Crout par
element ou segment -11 : preconditionnement de Gauss-Seidel par element
ou segment Certains preconditionnements sont cumulables (les diagonaux 2
ou 3 avec les autres) Pour cette raison on ne retient que les nombres
premiers pour designer les preconditionnements. Si l''on souhaite en
cumuler plusieurs on formera le produit des options correspondantes.""",
            ang = """ Choice of the preconditioning in the propagation step linear system
that the convergence is speeded up when it is being solved. 0: no
preconditioning 2: diagonal preconditioning 3: block-diagonal
preconditioning (systemes a 4 ou 9 matrices) 7: Crout''s preconditioning
per element or segment 11: Gauss-Seidel''s preconditioning per element
or segment Some operations (either 2 or 3 diagonal preconditioning) can
be performed concurrently with the others. Only prime numbers are
therefore kept to denote the preconditioning operations. When several of
them are to be performed concurrently, the product of relevant options
shall be made.""",
        ),
#       -----------------------------------
        C_U_PRECONDITIONING = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Changement de variable de H en C dans le systeme lineaire final""",
            ang = """ Change of variable from H to C in the final linear system""",
        ),
    ),
#   -----------------------------------
    DISCRETISATIONS_IMPLICITATION = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        DISCRETIZATIONS_IN_SPACE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', max='**',
            into = ["linear","quasi-bubble","quadratic"],
            defaut = ["linear","linear","linear","linear"],
            fr = """ Choix de la discretisation pour chaque variable ces coefficients sont
respectivement appliques a 1) U et V 2) H 3) T 4) K ET EPSILON (NON
PROGRAMME) 11 : lineaire 12 : quasi-bulle 13 : quadratique""",
            ang = """ Choice of space discretisation for every variable These coefficients
are applied respectively to 1) U and V 2) H 3) T 4) K and EPSILON (NOT
IMPLEMENTED) 11: linear 12: quasi-bubble 13: quadratic""",
        ),
#       -----------------------------------
        IMPLICITATION_FOR_DEPTH = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.55,
            fr = """ Fixe la valeur du coefficient d''implicitation sur C dans l''etape de
propagation (cf. Note de principe). Les valeurs inferieures a 0.5
donnent un schema instable.""",
            ang = """ Sets the value of the implicitation coefficient for C (the celerity of
waves) in the propagation step (refer to principle note). Values below
0.5 result in an unstable scheme.""",
        ),
#       -----------------------------------
        IMPLICITATION_FOR_VELOCITY = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.55,
            fr = """ Fixe la valeur du coefficient d''implicitation sur la vitesse dans
l''etape de propagation (cf. Note de principe). Les valeurs inferieures
a 0.5 donnent un schema instable.""",
            ang = """ Sets the value of the implicitation coefficient for velocity in the
propagation step (refer to principle note). Values below 0.5 result in
an unstable condition.""",
        ),
    ),
#   -----------------------------------
    PROPAGATION_INFO = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        PROPAGATION = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Prise en compte ou non de la propagation de la vitesse et de la
hauteur d''eau. La diffusion etant contenue dans cette etape sera
supprimee aussi.""",
            ang = """ Determines whether the propagation step is taken into account or not.
The diffusion being included in that step will be deleted as well.""",
        ),
#       -----------------------------------
        b_PROPAGATIONG = BLOC(condition="PROPAGATION == True",
#       -----------------------------------
#           -----------------------------------
            MEAN_DEPTH_FOR_LINEARIZATION = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """ Fixe la hauteur d''eau autour de laquelle s''effectue la linearisation
lorsque l''option PROPAGATION LINEARISEE est choisie.""",
                ang = """ Sets the water depth about which the linearization is made when the
LINEARIZED PROPAGATION OPTION is selected.""",
            ),
#           -----------------------------------
            INITIAL_GUESS_FOR_U = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ["zero","previous","extrapolation"],
                defaut = "previous",
                fr = """ Tir initial du solveur de l''etape de propagation. Offre la
possibilite de modifier la valeur initiale de U, a chaque iteration,
dans l''etape de propagation en utilisant les valeurs finales de cette
variable aux pas de temps precedents. Ceci peut permettre d''accelerer
la vitesse de convergence lors de la resolution du systeme. Trois
possibilites sont offertes : 0 : U = 0 1 : U = U(n) 2 : U = 2 U(n)-
U(n-1) (extrapolation)""",
                ang = """ Initial guess for the solver in the propagation step. Makes it
possible to modify the initial value of U, upon each iteration in the
propagation step, by using the ultimate values this variable had in the
earlier time steps. Thus, the convergence can be speeded up when the
system is being solved. 3 options are available: 0 : U = 0 1 : U = U(n)
2 : U = 2 U(n)- U(n-1) (extrapolation)""",
            ),
        ),
#       -----------------------------------
        INITIAL_GUESS_FOR_H = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["previous","zero","extrapolation"],
            defaut = "previous",
            fr = """ Tir initial du solveur de l''etape de propagation. Offre la
possibilite de modifier la valeur initiale de DH, accroissement de H, a
chaque iteration, dans l''etape de propagation en utilisant les valeurs
finales de cette variable aux pas de temps precedents. Ceci peut
permettre d''accelerer la vitesse de convergence lors de la resolution
du systeme. Trois possibilites sont offertes : 0 : DH = 0. 1 : DH = DHn
(valeur finale de DH au pas de temps precedent), 2 : DH = 2DHn - DHn-1
(extrapolation).""",
            ang = """ Initial guess for the solver in the propagation step. Makes it
possible to modify the initial value of C, upon each iteration in the
propagation step, by using the ultimate values this variable had in the
earlier time steps. Thus, the convergence can be speeded up when the
system is being solved. 3 options are available: 0: DH = 0 1: DH = DHn
(ultimate DH value in the next previous time step) 2: DH = 2DHn - DHn-1
(extrapolation)""",
        ),
#       -----------------------------------
        LINEARIZED_PROPAGATION = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Permet de lineariser l''etape de propagation; par exemple lors de la
realisation de cas tests pour lesquels on dispose d''une solution
analytique dans le cas linearise.""",
            ang = """ Provided for linearizing the propagation step, e.g. when performing
test-cases for which an analytical solution in the linearized case is
available.""",
        ),
    ),
#   -----------------------------------
    ADVECTION_INFO = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        ADVECTION = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Prise en compte ou non des termes de convection. En cas de reponse
positive; on peut encore supprimer certains termes de convection avec
les mots-cles CONVECTION DE ...""",
            ang = """ Are the advection terms taken into account or not? If YES, some
advection terms can still be deleted using the keywords -ADVECTION OF
..-""",
        ),
#       -----------------------------------
        ADVECTION_OF_H = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Prise en compte ou non de la convection de H. Il s''agit en fait dans
la version 2.0 de la convection de C""",
            ang = """ The advection of H is taken into account or ignored. Actually, in
version 2.0, the matter is about C advection.""",
        ),
#       -----------------------------------
        ADVECTION_OF_U_AND_V = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Prise en compte ou non de la convection de U et V.""",
            ang = """ The advection of U and V is taken into account or ignored.""",
        ),
#       -----------------------------------
        b_ADVECTION_OF_U_AND_VG = BLOC(condition="ADVECTION_OF_U_AND_V == True",
#       -----------------------------------
#           -----------------------------------
            SCHEME_FOR_ADVECTION_OF_VELOCITIES = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM',
                into = ["NO ADVECTION","CHARACTERISTICS","EXPLICIT + SUPG","EXPLICIT LEO POSTMA","EXPLICIT + MURD SCHEME N","EXPLICIT + MURD SCHEME PSI","N-SCHEME FOR TIDAL FLATS","N-SCHEME FOR TIDAL FLATS","ERIA SCHEME"],
                defaut = "CHARACTERISTICS",
                fr = """ Choix du schema de convection pour les vitesses, remplace FORME DE LA
CONVECTION""",
                ang = """ Choice of the advection scheme for the velocities, replaces TYPE OF
ADVECTION""",
            ),
        ),
#       -----------------------------------
        TYPE_OF_ADVECTION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 4, max= 4,
            into = ["CHARACTERISTICS","SUPG","CONSERVATIVE N-SCHEME","CONSERVATIVE N-SCHEME","CONSERVATIVE PSI-SCHEME","EDGE-BASED N-SCHEME","EDGE-BASED N-SCHEME","ERIA SCHEME"],
            defaut = ["CHARACTERISTICS","CONSERVATIVE PSI-SCHEME","CHARACTERISTICS","CHARACTERISTICS"],
            fr = """ Choix du schema de convection pour chaque variable ces coefficients
sont respectivement appliques a 1) U et V 2) H 3) T 4) K ET EPSILON 1 :
caracteristiques sur h 2 : SUPG 3 : Schema N conservatif 4 : Schema N
conservatif 5 : Schema PSI conservatif 6 : Schema PSI non conservatif 7
: schema N implicite non conservatif 13 : Schema N par segment 14 :
Schema N par segment Second integer must be 5""",
            ang = """ Choice of advection schemes for every variable These coefficients are
applied respectively to 1) U et V 2) H 3) T 4) K and EPSILON 1:
characteristics 2: SUPG 3: Conservative N-scheme 4: Conservative
N-scheme 5: Conservative PSI-scheme 6 : Non conservative PSI scheme 7 :
Implicit non conservative N scheme 13 : Edge-based N-scheme 14 :
Edge-based N-scheme Second integer must be 5""",
        ),
#       -----------------------------------
        OPTION_FOR_CHARACTERISTICS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["strong","weak"],
            defaut = "strong",
            fr = """ 1: forme forte 2: forme faible""",
            ang = """ 1: strong form 2: weak form""",
        ),
#       -----------------------------------
        SUPG_OPTION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 4, max= 4,
            defaut = [2,2,2,2],
            fr = """ 0:pas de decentrement SUPG
1:SUPG classique
2:SUPG modifiee
ces coefficients sont respectivement appliques a 1) U et V 2) H 3) T 4)
K ET EPSILON""",
            ang = """ 0:no upwinding 1: classical SUPG 2:modified SUPG These coefficients
are applied respectively to 1) U et V 2) H 3) T 4) K and EPSILON""",
        ),
#       -----------------------------------
        NUMBER_OF_GAUSS_POINTS_FOR_WEAK_CHARACTERISTICS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            into = ["1 point","3 points","6 points"],
            defaut = "3 points",
            fr = """ Voir les release notes 6.3""",
            ang = """ See release notes 6.3""",
        ),
#       -----------------------------------
        MASS_LUMPING_FOR_WEAK_CHARACTERISTICS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ Applique a la matrice de masse""",
            ang = """ To be applied to the mass matrix""",
        ),
#       -----------------------------------
        b_MAXIMUM_NUMBER_OF_ITERATIONS_FOR_ADVECTION_SCHEMESF = BLOC(condition="(ADVECTION_OF_TRACERS == True and SCHEME_FOR_ADVECTION_OF_TRACERS == 'EDGE-BASED N-SCHEME') or (ADVECTION_OF_K_AND_EPSILON == True and SCHEME_FOR_ADVECTION_OF_K_EPSILON == 'EDGE-BASED N-SCHEME') or (ADVECTION_OF_U_AND_V == True and SCHEME_FOR_ADVECTION_OF_VELOCITIES == 'EDGE-BASED N-SCHEME')",
#       -----------------------------------
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_ADVECTION_SCHEMES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 10,
            fr = """ Seulement pour schemes 13 et 14""",
            ang = """ Only for schemes 13 and 14""",
        ),
#       -----------------------------------
        UPWIND_COEFFICIENTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 4, max= 4,
            defaut = [1.,1.,1.,1],
            fr = """ Coefficients utilises par la methode S.U.P.G. ces coefficients sont
respectivement appliques a 1) U et V 2) H ou C 3) T 4) K ET EPSILON""",
            ang = """ Upwind coefficients used by the S.U.P.G. method These coefficients are
applied respectively to 1) U and V 2) H or C 3) T 4) K and epsilon""",
        ),
#       -----------------------------------
        MASS_LUMPING_ON_H = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ TELEMAC offre la possibilite d''effectuer du mass-lumping sur H ou U.
Ceci revient a ramener tout ou partie (suivant la valeur de ce
coefficient) des matrices AM1 (h) ou AM2 (U) et AM3 (V) sur leur
diagonale. Cette technique permet d''accelerer le code dans des
proportions tres importantes et de le rendre egalement beaucoup plus
stable. Cependant les solutions obtenues se trouvent lissees. Ce
parametre fixe le taux de mass-lumping effectue sur h.""",
            ang = """ TELEMAC provides an opportunity to carry out mass-lumping either on
C,H or on the velocity. This is equivalent to bringing the matrices AM1
(h) or AM2 (U) and AM3 (V) wholly or partly, back onto their diagonal.
Thanks to that technique, the code can be speeded up to a quite
significant extent and it can also be made much more stable. The
resulting solutions, however, become artificially smoothed. This
parameter sets the extent of mass-lumping that is performed on h.""",
        ),
#       -----------------------------------
        MASS_LUMPING_ON_VELOCITY = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ Fixe le taux de mass-lumping effectue sur la vitesse.""",
            ang = """ Sets the amount of mass-lumping that is performed on the velocity.""",
        ),
#       -----------------------------------
        SCHEME_OPTION_FOR_ADVECTION_OF_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Si present remplace et a priorite sur : OPTION POUR LES
CARACTERISTIQUES OPTION DE SUPG Si schema PSI ou N : 1=explicite
2=predicteur-correcteur 3=predicteur-correcteur deuxieme ordre en temps
4=implicite""",
            ang = """ If present replaces and has priority over: OPTION FOR CHARACTERISTICS
SUPG OPTION if N or PSI SCHEME: 1=explicit 2=predictor-corrector 3=
predictor-corrector second-order in time 4= implicit""",
        ),
#       -----------------------------------
        FREE_SURFACE_GRADIENT_COMPATIBILITY = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1.,
            fr = """ Des valeurs inferieures a 1 suppriment les oscillations parasites""",
            ang = """ Values less than 1 suppress spurious oscillations""",
        ),
#       -----------------------------------
        NUMBER_OF_SUB_ITERATIONS_FOR_NON_LINEARITIES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Permet de reactualiser, pour un meme pas de temps, les champs
convecteur et propagateur au cours de plusieurs sous-iterations. A la
premiere sous-iteration, ces champs sont donnes par C et le champ de
vitesses au pas de temps precedent. Aux iterations suivantes, ils sont
pris egaux au champ de vitesse obtenu a la fin de la sous-iteration
precedente. Cette technique permet d''ameliorer la prise en compte des
non linearites.""",
            ang = """ Used for updating, within one time step, the advection and propagation
field. upon the first sub-iteration, these fields are given by C and the
velocity field in the previous time step. At subsequent iterations, the
results of the previous sub-iteration is used to update the advection
and propagation field. The non-linearities can be taken into account
through this technique.""",
        ),
#       -----------------------------------
        b_TREATMENT_OF_FLUXES_AT_THE_BOUNDARIESF = BLOC(condition="(ADVECTION_OF_TRACERS == True and SCHEME_FOR_ADVECTION_OF_TRACERS in ['EDGE-BASED N-SCHEME','SUPG','CONSERVATIVE N-SCHEME','CONSERVATIVE PSI-SCHEME']) or (ADVECTION_OF_K_AND_EPSILON == True and SCHEME_FOR_ADVECTION_OF_K_EPSILON in ['EDGE-BASED N-SCHEME','SUPG','CONSERVATIVE N-SCHEME','CONSERVATIVE PSI-SCHEME']) or (ADVECTION_OF_U_AND_V == True and SCHEME_FOR_ADVECTION_OF_VELOCITIES in ['EDGE-BASED N-SCHEME','SUPG','CONSERVATIVE N-SCHEME','CONSERVATIVE PSI-SCHEME'])",
#       -----------------------------------
        ),
#       -----------------------------------
        TREATMENT_OF_FLUXES_AT_THE_BOUNDARIES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["Priority to prescribed values","Priority to fluxes"],
            defaut = "Priority to prescribed values",
            fr = """ Utilise pour les schemas SUPG, PSI et N, avec option 2, on ne retrouve
pas exactement les valeurs imposees des traceurs, mais le flux est
correct""",
            ang = """ Used so far only with the SUPG, PSI and N schemes. With option 2,
Dirichlet prescribed values are not obeyed, but the fluxes are correct""",
        ),
    ),
#   -----------------------------------
    DIFFUSION = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        DIFFUSION_OF_VELOCITY = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Permet de decider si l''on prend ou non en compte la diffusion des
vitesses.""",
            ang = """ Makes it possible to decide whether the diffusion of velocity (i.e.
viscosity) is taken into account or not.""",
        ),
#       -----------------------------------
        b_DIFFUSION_OF_VELOCITYG = BLOC(condition="DIFFUSION_OF_VELOCITY == True",
#       -----------------------------------
#           -----------------------------------
            IMPLICITATION_FOR_DIFFUSION_OF_VELOCITY = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 1.,
                fr = """ Fixe la valeur du coefficient d''implicitation sur les termes de
diffusion des vitesses""",
                ang = """ Sets the value of the implicitation coefficient for the diffusion of
velocity""",
            ),
#           -----------------------------------
            OPTION_FOR_THE_DIFFUSION_OF_VELOCITIES = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ["div( nu grad(U) )","1/h div ( h nu grad(U)"],
                defaut = "div( nu grad(U) )",
                fr = """ 1: Diffusion de la forme div( nu grad(U) ) 2: Diffusion de la forme
1/h div ( h nu grad(U) )""",
                ang = """ 1: Diffusion in the form div( nu grad(U) ) 2: Diffusion in the form
1/h div ( h nu grad(U) )""",
            ),
        ),
    ),
#   -----------------------------------
    OTHERS = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        MATRIX_STORAGE = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["classical EBE","Edge-based storage"],
            defaut = "Edge-based storage",
            fr = """ 1 : EBE classique 3 : Stockage par segments""",
            ang = """ 1 : classical EBE 3 : Edge-based storage""",
        ),
#       -----------------------------------
        MATRIX_VECTOR_PRODUCT = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ 1 : classique 2 : frontal attention, avec 2, il faut une numerotation
speciale des points""",
            ang = """ 1 : classic 2 : frontal beware, with option 2, a special numbering of
points is required""",
        ),
#       -----------------------------------
        NEWMARK_TIME_INTEGRATION_COEFFICIENT = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1.,
            fr = """ 1. : Euler explicite 0.5 : ordre 2 en temps""",
            ang = """ 1. : Euler explicit 0.5 : order 2 in time""",
        ),
#       -----------------------------------
        ZERO = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 1.E-10,
            fr = """ Non active pour l''instant.""",
            ang = """ Not yet implemented""",
        ),
    ),
)
# -----------------------------------------------------------------------
TIDAL_FLATS_INFO = PROC(nom= "TIDAL_FLATS_INFO",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    TIDAL_FLATS = SIMP(statut ='o',
#   -----------------------------------
        typ = bool,
        defaut = True,
        fr = """ permet de supprimer les tests sur les bancs decouvrants, dans les cas
ou l''on est certain qu''il n''y en aura pas. En cas de doute : oui""",
        ang = """ When no, the specific treatments for tidal flats are by-passed. This
spares time, but of course you must be sure that you have no tidal
flats""",
    ),
#   -----------------------------------
    b_TIDAL_FLATSG = BLOC(condition="TIDAL_FLATS == True",
#   -----------------------------------
#       -----------------------------------
        OPTION_FOR_THE_TREATMENT_OF_TIDAL_FLATS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["EQUATIONS SOLVED EVERYWHERE WITH CORRECTION ON TIDAL FLATS","DRY ELEMENTS FROZEN","LIKE 1 BUT WITH POROSITY (DEFINA METHOD)"],
            defaut = "EQUATIONS SOLVED EVERYWHERE WITH CORRECTION ON TIDAL FLATS",
            fr = """ Utilise si BANCS DECOUVRANTS est vrai 1 : EQUATIONS RESOLUES PARTOUT
AVEC CORRECTION SUR LES BANCS DECOUVRANTS 2 : GEL DES ELEMENTS
DECOUVRANTS 3 : COMME 1 MAIS AVEC POROSITE (METHODE DEFINA)""",
            ang = """ Used if TIDAL FLATS is true 1 : EQUATIONS SOLVED EVERYWHERE WITH
CORRECTION ON TIDAL FLATS 2 : DRY ELEMENTS FROZEN 3 : LIKE 1 BUT WITH
POROSITY (DEFINA METHOD)""",
        ),
#       -----------------------------------
        b_OPTION_FOR_THE_TREATMENT_OF_TIDAL_FLATSG = BLOC(condition="OPTION_FOR_THE_TREATMENT_OF_TIDAL_FLATS == 'EQUATIONS RESOLUES PARTOUT, CORRECTION SUR LES BANCS DECOUVRANTS'",
#       -----------------------------------
#           -----------------------------------
            TREATMENT_OF_NEGATIVE_DEPTHS = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ["SMOOTHING","FLUX CONTROL"],
                defaut = "SMOOTHING",
                fr = """ Seulement avec OPTION DE TRAITEMENT DES BANCS DECOUVRANTS = 1 0 : pas
de traitement 1 : lissage 2 : limitation des flux""",
                ang = """ Only with OPTION FOR THE TREATMENT OF TIDAL FLATS=1 0: no treatment
1:smoothing 2:flux control""",
            ),
        ),
#       -----------------------------------
        THRESHOLD_FOR_NEGATIVE_DEPTHS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ En dessous du seuil, les hauteurs negatives sont lissees""",
            ang = """ Below the threshold the negative depths are smoothed""",
        ),
#       -----------------------------------
        THRESHOLD_DEPTH_FOR_RECEDING_PROCEDURE = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ Si > 0., declenche la procedure de ressuyage qui evite le
franchissement parasite des digues mal discretisees""",
            ang = """ If > 0., will trigger the receding procedure that avoids overwhelming
of dykes which are too loosely discretised""",
        ),
#       -----------------------------------
        H_CLIPPING = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Determine si l''on desire ou non limiter par valeur inferieure la
hauteur d''eau H (dans le cas des bancs decouvrants par exemple).""",
            ang = """ Determines whether limiting the water depth H by a lower value
desirable or not. (for instance in the case of tidal flats) This
key-word may have an influence on mass conservation since the truncation
of depth is equivalent to adding mass.""",
        ),
#       -----------------------------------
        b_H_CLIPPINGG = BLOC(condition="H_CLIPPING == True",
#       -----------------------------------
#           -----------------------------------
            MINIMUM_VALUE_OF_DEPTH = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """ Fixe la valeur minimale de a lorsque l''option CLIPPING DE H est
activee.""",
                ang = """ Sets the minimum H value when option H CLIPPING is implemented. Not
fully implemented.""",
            ),
        ),
    ),
)
# -----------------------------------------------------------------------
TRACERS = PROC(nom= "TRACERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    SETTING = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Definit le nombre de traceurs.""",
            ang = """ Defines the number of tracers""",
        ),
#       -----------------------------------
        NAMES_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            fr = """ Noms des traceurs en 32 caracteres, 16 pour le nom 16 pour l''unite""",
            ang = """ Name of tracers in 32 characters, 16 for the name, 16 for the unit.""",
        ),
#       -----------------------------------
        INITIAL_VALUES_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            defaut = [0.,0.],
            fr = """ Fixe la valeur initiale du traceur.""",
            ang = """ Sets the initial value of the tracer.""",
        ),
#       -----------------------------------
        DENSITY_EFFECTS = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ PRISE EN COMPTE DU GRADIENT HORIZONTAL DE DENSITE LE TRACEUR EST ALORS
LA SALINITE""",
            ang = """ THE HORIZONTAL GRADIENT OF DENSITY IS TAKEN INTO ACCOUNT THE TRACER IS
THEN THE SALINITY""",
        ),
#       -----------------------------------
        b_DENSITY_EFFECTSG = BLOC(condition="DENSITY_EFFECTS == True",
#       -----------------------------------
#           -----------------------------------
            MEAN_TEMPERATURE = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 20.,
                fr = """ TEMPERATURE DE REFERENCE POUR LE CALCUL DES EFFETS DE DENSITE A
UTILISER AVEC LE MOT-CLE "EFFETS DE DENSITE".""",
                ang = """ REFERENCE TEMPERATURE FOR DENSITY EFFECTS TO BE USED WITH THE KEY-WORD
"DENSITY EFFECTS".""",
            ),
        ),
    ),
#   -----------------------------------
    BOUNDARY_CONDITIONS = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        PRESCRIBED_TRACERS_VALUES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """ Valeurs du traceur imposees aux frontieres liquides entrantes. Lire la
partie du mode d''emploi consacree aux conditions aux limites""",
            ang = """ Tracer values prescribed at the inflow boundaries. Read the usermanual
section dealing with the boundary conditions""",
        ),
    ),
#   -----------------------------------
    SOLVER_TRA = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        SOLVER_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","cgstab","gmres (see option for the solver for tracer diffusion)","direct"],
            defaut = ["conjugate gradient","conjugate gradient"],
            fr = """ 1 : gradient conjugue 2 : residu conjugue 3 : gradient conjugue sur
equation normale 4 : erreur minimale 5 : gradient conjugue carre""",
            ang = """ 1 : conjugate gradient 2 : conjugate gradient 3 : conjugate gradient
on a normal equation 4 : minimum error 5 : squared conjugate gradient 6
: cgstab 7 : gmres (see option for the solver for tracer diffusion) 8 :
direct""",
        ),
#       -----------------------------------
        SOLVER_OPTION_FOR_TRACERS_DIFFUSION = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 2,
            fr = """ si le solveur est GMRES (7) le mot cle est la dimension de l''espace
de KRILOV (valeurs conseillees entre 2 et 15)""",
            ang = """ WHEN GMRES (7) IS CHOSEN, DIMENSION OF THE KRYLOV SPACE TRY VALUES
BETWEEN 2 AND 15""",
        ),
#       -----------------------------------
        PRECONDITIONING_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["diagonal","no preconditioning ","crout","diagonal and crout"],
            defaut = "diagonal",
            fr = """ Permet de preconditionner le systeme relatif au traceur. Memes
definition et possibilites que pour le mot-cle PRECONDITIONNEMENT. 0 :
pas de preconditionnement; 2 : preconditionnement diagonal. 7 : Crout
par element""",
            ang = """ Preconditioning of the linear system in the tracer diffusion step.
Same definition and possibilities as for the keyword PRECONDITIONING 0:
no preconditioning 2: diagonal preconditioning 7: Crout''s
preconditioning per element.""",
        ),
    ),
#   -----------------------------------
    ACCURACY_TRA = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        ACCURACY_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1.E-6,
            fr = """ Fixe la precision demandee pour le calcul de la diffusion du traceur.""",
            ang = """ Sets the required accuracy for computing the tracer diffusion.""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 60,
            fr = """ Limite le nombre d''iterations du solveur a chaque pas de temps pour
le calcul de la diffusion du traceur.""",
            ang = """ Limits the number of solver iterations at each time step for the
diffusion of tracer.""",
        ),
    ),
#   -----------------------------------
    SOURCES_TRA = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        VALUES_OF_THE_TRACERS_AT_THE_SOURCES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """ Valeurs des traceurs a chacune des sources""",
            ang = """ Values of the tracers at the sources""",
        ),
    ),
#   -----------------------------------
    METEOROLOGY_TRA = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        VALUES_OF_TRACERS_IN_THE_RAIN = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """generalement ce traceur est la temperature, dans ce cas
cette valeur  est a modifier, sinon la valeur 0 est raisonnable""",
            ang = """most often, this tracer is temperature, in this case
this value should be modified, otherwise, default value of 0 seems
reasonable""",
        ),
    ),
#   -----------------------------------
    NUMERICAL = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        ADVECTION_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Prise en compte ou non de la convection du traceur passif.""",
            ang = """ The advection of the passive tracer is taken into account or ignored.""",
        ),
#       -----------------------------------
        b_ADVECTION_OF_TRACERSG = BLOC(condition="ADVECTION_OF_TRACERS == True",
#       -----------------------------------
#           -----------------------------------
            SCHEME_FOR_ADVECTION_OF_TRACERS = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ["NO ADVECTION","CHARACTERISTICS","EXPLICIT + SUPG","EXPLICIT LEO POSTMA","EXPLICIT + MURD SCHEME N","EXPLICIT + MURD SCHEME PSI","LEO POSTMA FOR TIDAL FLATS","N-SCHEME FOR TIDAL FLATS","ERIA SCHEME FOR TIDAL FLATS"],
                defaut = "CHARACTERISTICS",
                fr = """ Choix du schema de convection pour les traceurs, remplace FORME DE LA
CONVECTION""",
                ang = """ Choice of the advection scheme for the tracers, replaces TYPE OF
ADVECTION""",
            ),
        ),
#       -----------------------------------
        IMPLICITATION_COEFFICIENT_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.6,
            fr = """ Fixe la valeur du coefficient d''implicitation du traceur""",
            ang = """ Sets the value of the implicitation coefficient for the tracer""",
        ),
#       -----------------------------------
        DIFFUSION_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Prise en compte ou non de la diffusion du traceur passif.""",
            ang = """ The diffusion of the passive tracer is taken into account or ignored.""",
        ),
#       -----------------------------------
        b_DIFFUSION_OF_TRACERSG = BLOC(condition="DIFFUSION_OF_TRACERS == True",
#       -----------------------------------
#           -----------------------------------
            COEFFICIENT_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 1.E-6,
                fr = """ Fixe la valeur du coefficient de diffusion du traceur. L''influence de
ce parametre sur l''evolution du traceur dans le temps est importante.""",
                ang = """ Sets the value of the tracer diffusivity.""",
            ),
        ),
#       -----------------------------------
        OPTION_FOR_THE_DIFFUSION_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["div( nu grad(T) )","1/h div ( h nu grad(T)"],
            defaut = "div( nu grad(T) )",
            fr = """ 1: Diffusion de la forme div( nu grad(T) ) 2: Diffusion de la forme
1/h div ( h nu grad(T) )""",
            ang = """ 1: Diffusion in the form div( nu grad(T) ) 2: Diffusion in the form
1/h div ( h nu grad(T) )""",
        ),
#       -----------------------------------
        SCHEME_OPTION_FOR_ADVECTION_OF_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Si present remplace et a priorite sur : OPTION POUR LES
CARACTERISTIQUES OPTION DE SUPG Si schema PSI ou N : 1=explicite
2=predicteur-correcteur 3=predicteur-correcteur deuxieme ordre en temps
4=implicite""",
            ang = """ If present replaces and has priority over: OPTION FOR CHARACTERISTICS
SUPG OPTION if N or PSI SCHEME: 1=explicit 2=predictor-corrector 3=
predictor-corrector second-order in time 4= implicit""",
        ),
#       -----------------------------------
        MASS_LUMPING_ON_TRACERS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ Fixe le taux de mass-lumping effectue sur le traceur.""",
            ang = """ Sets the amount of mass-lumping that is performed on the tracer.""",
        ),
    ),
#   -----------------------------------
    DEGRADATION = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        LAW_OF_TRACERS_DEGRADATION = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            into = ["NO DEGRADATION","F(T90) LAW"],
            defaut = ["NO DEGRADATION","NO DEGRADATION"],
            fr = """ Prise en compte d''une loi de decroissance des traceurs""",
            ang = """ Take in account a law for tracers decrease""",
        ),
#       -----------------------------------
        b_LAW_OF_TRACERS_DEGRADATIONG = BLOC(condition="LAW_OF_TRACERS_DEGRADATION == 'F(T90) LAW'",
#       -----------------------------------
#           -----------------------------------
            COEFFICIENT_1_FOR_LAW_OF_TRACERS_DEGRADATION = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R', min= 2, max= 2,
                fr = """ Coefficient 1 de la loi de decroissance des traceurs""",
                ang = """ Coefficient 1 of law for tracers decrease""",
            ),
        ),
    ),
)
# -----------------------------------------------------------------------
PARTICLE_TRANSPORT = PROC(nom= "PARTICLE_TRANSPORT",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    DROGUES = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_DROGUES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Permet d''effectuer un suivi de flotteurs""",
            ang = """ Number of drogues in the computation. The user must then fill the
subroutine FLOT specifying the coordinates of the starting points, their
departure and arrival times. The trajectory of drogues is recorded in
the BINARY RESULTS FILE that must be given in the steering file""",
        ),
#       -----------------------------------
        b_NUMBER_OF_DROGUESG = BLOC(condition="NUMBER_OF_DROGUES != 0",
#       -----------------------------------
#           -----------------------------------
            DROGUES_FILE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)','Sauvegarde'),
                defaut = '',
                fr = """ Fichier de resultat avec les positions des flotteurs""",
                ang = """ Results file with positions of drogues""",
            ),
#           -----------------------------------
            PRINTOUT_PERIOD_FOR_DROGUES = SIMP(statut ='o',
#           -----------------------------------
                typ = 'I',
                defaut = 1,
                fr = """ Nombre de pas de temps entre 2 sorties de positions de flotteurs dans
le fichier des resultats binaire supplementaire N affecte pas la qualite
du calcul de la trajectoire""",
                ang = """ Number of time steps between 2 outputs of drogues positions in the
binary file""",
            ),
        ),
    ),
#   -----------------------------------
    ALGAES = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        ALGAE_TRANSPORT_MODEL = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Si oui, les flotteurs seront des algues""",
            ang = """ If yes, the floats or particles will be algae""",
        ),
#       -----------------------------------
        b_ALGAE_TRANSPORT_MODELG = BLOC(condition="ALGAE_TRANSPORT_MODEL == True",
#       -----------------------------------
#           -----------------------------------
            ALGAE_TYPE = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ["SPHERE","IRIDAEA FLACCIDA (CLOSE TO ULVA)","PELVETIOPSIS LIMITATA","GIGARTINA LEPTORHYNCHOS"],
                defaut = "SPHERE",
                fr = """ Type des algues. Pour le choix 1 les algues seront modelisees comme
des spheres, pour les autres choix voir Gaylord et al. (1994).""",
                ang = """ Algae type. For choice 1 the algae particles will be modeled as
spheres, and for the other choices see Gaylord et al. (1994)""",
            ),
#           -----------------------------------
            DIAMETER_OF_ALGAE = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.1,
                fr = """ Diametre des algues en m""",
                ang = """ Diametre of algae in m""",
            ),
#           -----------------------------------
            DENSITY_OF_ALGAE = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 1050.,
                fr = """ Masse volumique des algues en kg/m3""",
                ang = """ Density of algae in kg/m3""",
            ),
#           -----------------------------------
            THICKNESS_OF_ALGAE = SIMP(statut ='o',
#           -----------------------------------
                typ = 'R',
                defaut = 0.01,
                fr = """ Epaisseur des algues en m""",
                ang = """ Thickness of algae in m""",
            ),
        ),
    ),
#   -----------------------------------
    OIL_SPILL = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        OIL_SPILL_MODEL = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ POUR DECLENCHER LE MODELE DE DERIVE DE NAPPES, DANS CE CAS LE FICHIER
DE COMMANDES MIGRHYCAR EST NECESSAIRE""",
            ang = """ WILL TRIGGER THE OIL SPILL MODEL, IN THIS CASE THE MIGRHYCAR STEERING
FILE IS NEEDED""",
        ),
#       -----------------------------------
        b_OIL_SPILL_MODELG = BLOC(condition="OIL_SPILL_MODEL == True",
#       -----------------------------------
#           -----------------------------------
            OIL_SPILL_STEERING_FILE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Contient les donnees pour le modele de derive de nappes""",
                ang = """ Contains data for the oil spill model""",
            ),
        ),
    ),
#   -----------------------------------
    BROWNIAN_MOUVEMENT = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        STOCHASTIC_DIFFUSION_MODEL = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["No model","brownian movement"],
            defaut = "No model",
            fr = """ Pour les particules : flotteurs, hydrocarbures""",
            ang = """ Meant for particles: drogues, oil spills""",
        ),
    ),
#   -----------------------------------
    LAGRANGIAN_DRIFTS = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_LAGRANGIAN_DRIFTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Permet d''effectuer simultanement plusieurs calculs de derives
lagrangiennes initiees a des pas differents""",
            ang = """ Provided for performing several computations of lagrangian drifts
starting at different times. Add A and G in the VARIABLES FOR GRAPHIC
PRINTOUTS key-word""",
        ),
    ),
)
# -----------------------------------------------------------------------
CONSTRUCTION_WORKS = PROC(nom= "CONSTRUCTION_WORKS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    WEIRS = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_WEIRS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Nombre de seuils qui seront traites par des conditions aux limites.
Ces seuils doivent etre decrits comme des frontieres du domaine de
calcul, et leurs caracteristiques sont donnees dans le fichier de
donnees des seuils (voir la documentation ecrite)""",
            ang = """ Number of weirs that will be treated by boundary conditions. They must
be described as boundaries of the domain and their features are given in
the weir data file (see written documentation)""",
        ),
#       -----------------------------------
        b_NUMBER_OF_WEIRSG = BLOC(condition="NUMBER_OF_WEIRS != 0",
#       -----------------------------------
#           -----------------------------------
            WEIRS_DATA_FILE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de description des seuils presents dans le modele""",
                ang = """ Description of weirs existing in the model""",
            ),
#           -----------------------------------
            TYPE_OF_WEIRS = SIMP(statut ='o',
#           -----------------------------------
                typ = 'TXM',
                into = ["HORIZONTAL WITH SAME NUMBER OF NODES UPSTREAM/DOWNSTREAM","GENERAL"],
                defaut = "HORIZONTAL WITH SAME NUMBER OF NODES UPSTREAM/DOWNSTREAM",
                fr = """ Méthode de traitement des seuils. Deux Solutions:
- HORIZONTAL AVEC MEME NOMBRE DE NOEUDS AMONT/AVAL (Solution historique
  avec bord)
- GENERALE (Nouvelle solution avec pts sources)""",
                ang = """ Method for treatment of weirs. Two options:
- HORIZONTAL WITH SAME NUMBER OF NODES UPSTREAM/DOWNSTREAM (Historical
  solution with bord)
- GENERAL (New solution with sources points""",
            ),
        ),
    ),
#   -----------------------------------
    SIPHONS = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_SIPHONS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Nombre de siphons traites comme des termes sources ou puits. Ces
siphons doivent etre decrits comme des sources dans le fichier cas.
Leurs caracteristiques sont donnees dans le fichier de donnees des
siphons (voir la documentation ecrite)""",
            ang = """ Number of culverts treated as source terms. They must be described as
sources in the domain and their features are given in the culvert data
file (see written documentation)""",
        ),
#       -----------------------------------
        b_NUMBER_OF_SIPHONSG = BLOC(condition="NUMBER_OF_SIPHONS != 0",
#       -----------------------------------
#           -----------------------------------
            SIPHONS_DATA_FILE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de description des siphons presents dans le modele""",
                ang = """ Description of culvert existing in the model""",
            ),
        ),
    ),
#   -----------------------------------
    CULVERTS = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_CULVERTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Nombre de buses ou ponts traites comme des termes sources ou puits.
Ces buses doivent etre decrits comme des sources dans le fichier cas.
Leurs caracteristiques sont donnees dans le fichier de donnees des buses
(voir la documentation ecrite)""",
            ang = """ Number of culverts or bridges treated as source terms. They must be
described as sources in the domain and their features are given in the
culverts data file (see written documentation)""",
        ),
#       -----------------------------------
        b_NUMBER_OF_CULVERTSG = BLOC(condition="NUMBER_OF_CULVERTS != 0",
#       -----------------------------------
#           -----------------------------------
            CULVERTS_DATA_FILE = SIMP(statut ='f',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de description des buses/ponts presents dans le modele""",
                ang = """ Description of tubes/bridges existing in the model""",
            ),
        ),
    ),
#   -----------------------------------
    BREACHES = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        BREACH = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Prise en compte de breches dans le calcul par modification
altimetrique dans le maillage. La description des breches se fait avec
le fichier de donnees des breches.""",
            ang = """ Take in account some breaches during the computation by modifying the
bottom level of the mesh. Brech description is done with the breaches
data file.""",
        ),
#       -----------------------------------
        b_BREACHG = BLOC(condition="BREACH == True",
#       -----------------------------------
#           -----------------------------------
            BREACHES_DATA_FILE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Fichier de description des breches""",
                ang = """ Description of breaches""",
            ),
        ),
    ),
)
# -----------------------------------------------------------------------
TIDES = PROC(nom= "TIDES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    BOUNDARY_CONDITIONS = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        TIDAL_DATA_BASE = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["NO DEFAULT VALUE","JMJ","TPXO","MISCELLANEOUS (LEGOS-NEA, FES20XX, PREVIMER...)"],
            defaut = "NO DEFAULT VALUE",
            fr = """ Pour JMJ, renseigner la localisation du fichier bdd\_jmj et geofin
dans les mots-cles BASE DE DONNEES DE MAREE et FICHIER DU MODELE DE
MAREE. Pour TPXO, LEGOS-NEA, FES20XX et PREVIMER, l''utilisateur doit
telecharger les fichiers de constantes harmoniques sur internet""",
            ang = """ For JMJ, indicate the location of the files bdd\_jmj and geofin with
keywords TIDE DATA BASE and TIDAL MODEL FILE. For TPXO, LEGOS-NEA,
FES20XX and PREVIMER, the user has to download files of harmonic
constituents on the internet""",
        ),
#       -----------------------------------
        b_TIDAL_DATA_BASEG = BLOC(condition="TIDAL_DATA_BASE == 'TPXO'",
#       -----------------------------------
#           -----------------------------------
            MINOR_CONSTITUENTS_INFERENCE = SIMP(statut ='o',
#           -----------------------------------
                typ = bool,
                defaut = False,
                fr = """ Pour la base de donnees TPXO uniquement. Interpolation de composantes
harmoniques mineures a partir de celles lues dans les fichiers d''entree
lies aux mots-cles BASE BINAIRE 1 DE DONNEES DE MAREE et BASE BINAIRE 2
DE DONNEES DE MAREE""",
                ang = """ For TPXO tidal data base only. Inference of minor constituents from
the one read in input files linked to keywords BINARY DATABASE 1 FOR
TIDE and BINARY DATABASE 2 FOR TIDE""",
            ),
#           -----------------------------------
            BINARY_DATABASE_1_FOR_TIDE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Base de donnees binaire 1 tiree du fichier du modele de maree. Dans le
cas des donnees satellitaires de TPXO, ce fichier correspond aux donnees
de niveau d''eau, par exemple h\_tpxo7.2""",
                ang = """ Binary database 1 extracted from the tidal model file. In the case of
the TPXO satellite altimetry model, this file should be for free surface
level, for instance h\_tpxo7.2""",
            ),
#           -----------------------------------
            BINARY_DATABASE_2_FOR_TIDE = SIMP(statut ='o',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """ Base de donnees binaire 2 tiree du fichier du modele de maree. Dans le
cas des donnees satellitaires de TPXO, ce fichier correspond aux donnees
de vitesse de marrees, par exemple u\_tpxo7.2""",
                ang = """ Binary database 2 extracted from the tidal model file. In the case of
the TPXO satellite altimetry model, this file should be for tidal
velocities, for instance u\_tpxo7.2""",
            ),
        ),
#       -----------------------------------
        TIDAL_MODEL_FILE = SIMP(statut ='o',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """ Fichier de geometrie du modele dont sont extraites les constantes
harmoniques""",
            ang = """ Geometry file of the model from which harmonic constituents are
extracted""",
        ),
#       -----------------------------------
        HARMONIC_CONSTANTS_FILE = SIMP(statut ='o',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """ Constantes harmoniques extraites du fichier du modele de maree""",
            ang = """ Harmonic constants extracted from the tidalmodel file""",
        ),
    ),
#   -----------------------------------
    CALIBRATION = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        GEOGRAPHIC_SYSTEM = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ["NO DEFAULT VALUE","DEFINED BY USER","WGS84 LONGITUDE/LATITUDE IN REAL DEGREES","WGS84 NORTHERN UTM","WGS84 SOUTHERN UTM","LAMBERT","MERCATOR FOR TELEMAC"],
            defaut = "NO DEFAULT VALUE",
            fr = """ Systeme de coordonnees geographiques dans lequel est construit le
modele numerique. Indiquer la zone correspondante avec le mot-cle""",
            ang = """ Geographic coordinates system in which the numerical model is built.
Indicate the corresponding zone with the keyword""",
        ),
#       -----------------------------------
        b_GEOGRAPHIC_SYSTEMG = BLOC(condition="GEOGRAPHIC_SYSTEM in ['WGS84 NOTHERN UTM','WGS84 SOUTHERN UTM','LAMBERT']",
#       -----------------------------------
#           -----------------------------------
            ZONE_NUMBER_IN_GEOGRAPHIC_SYSTEM = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM',
                into = ["NO DEFAULT VALUE","LAMBERT 1 NORTH","LAMBERT 2 CENTER","LAMBERT 3 SOUTH","LAMBERT 4 CORSICA","LAMBERT 2 EXTENDED","UTM ZONE, E.G."],
                defaut = "NO DEFAULT VALUE",
                fr = """ Numero de zone (fuseau ou type de projection) lors de l''utilisation
d''une projection plane. Indiquer le systeme geographique dans lequel
est construit le modele numerique avec le mot-cle SYSTEME GEOGRAPHIQUE""",
                ang = """ Number of zone when using a plane projection. Indicate the geographic
system in which the numerical model is built with the keyword GEOGRAPHIC
SYSTEM""",
            ),
        ),
#       -----------------------------------
        COEFFICIENT_TO_CALIBRATE_SEA_LEVEL = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """ Coefficient pour ajuster le niveau de mer""",
            ang = """ Coefficient to calibrate the sea level""",
        ),
#       -----------------------------------
        COEFFICIENT_TO_CALIBRATE_TIDAL_RANGE = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 1.,
            fr = """ Coefficient pour ajuster le marnage de l''onde de maree aux frontieres
maritimes""",
            ang = """ Coefficient to calibrate the tidal range of tidal wave at tidal open
boundary conditions""",
        ),
#       -----------------------------------
        COEFFICIENT_TO_CALIBRATE_TIDAL_VELOCITIES = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R',
            defaut = 999999.,
            fr = """ Coefficient pour ajuster les composantes de vitesse de l''onde de
maree aux frontieres maritimes. La valeur par defaut 999999. signifie
que c''est la racine carree du COEFFICIENT DE CALAGE DU MARNAGE qui est
prise""",
            ang = """ Coefficient to calibrate the tidal velocities of tidal wave at tidal
open boundary conditions. Default value 999999. means that the square
root of COEFFICIENT TO CALIBRATE TIDAL RANGE is taken""",
        ),
    ),
#   -----------------------------------
    PHYSICAL_PARAMETERS = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        TIDE_GENERATING_FORCE = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Active la prise en compte de la force generatrice de la maree""",
            ang = """ The tide generating force is taken into account.""",
        ),
#       -----------------------------------
        b_TIDE_GENERATING_FORCEG = BLOC(condition="TIDE_GENERATING_FORCE == True",
#       -----------------------------------
        ),
#       -----------------------------------
        OPTION_FOR_TIDAL_BOUNDARY_CONDITIONS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            into = ["No tide","Real tide (recommended methodology)","Astronomical tide","Mean spring tide","Mean tide","Mean neap tide","Astronomical neap tide","Real tide (methodology before 2010)"],
            fr = """ Option pour les conditions aux limites de maree. Pour des marees
reelles, l option 1 est recommandee. Depuis la version 7.1, ce mot-cle
est un tableau avec une valeur donnee par frontiere liquide, separee par
point-virgules. Ceci permet d''avoir des conditions de maree (ou pas)
calculees sur des frontieres liquides avec vitesses ou hauteur d eau
imposees. Ca evite un conflit lors de l utilisation de seuils dans le
domaine. 0 est le code pour des conditions autres que des conditions de
maree. ATTENTION depuis la version 7.1 ! Les anciens modeles doivent
etre changes si la frontiere de maree n a pas le numero 1. Dans ce cas,
le mot-cle doit etre change et plus de valeurs doivent etre donnees.
Calage possible par les mots-cles COEFFICIENT POUR CALAGE EN MARNAGE et
COEFFICIENT POUR CALAGE EN NIVEAU.""",
            ang = """ Option for tidal boundary conditions. For real tides, option 1 is
recommended. This keyword has been an array with a value given per
liquid boundary, separated by semicolons, since version 7.1. This
enables to have tidal conditions (or not) computed on liquid boundaries
with prescribed velocities or depths, avoiding a clash when using weirs
in the domain. 0 codes for conditions other than tidal. BEWARE since
version 7.1! Old models must be changed if their tidal boundary is not
number 1. In that case this keyword must be changed and more values
given. Possible calibration with the keywords COEFFICIENT TO ADJUST
TIDAL RANGE, COEFFICENT TO CALIBRATE TIDAL VELOCITIES, and COEFFICIENT
TO ADJUST SEA LEVEL.""",
        ),
#       -----------------------------------
        b_OPTION_FOR_TIDAL_BOUNDARY_CONDITIONSG = BLOC(condition="OPTION_FOR_TIDAL_BOUNDARY_CONDITIONS == 'No Tide'",
#       -----------------------------------
        ),
    ),
)
# -----------------------------------------------------------------------
COUPLING = PROC(nom= "COUPLING",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    COUPLING_WITH = SIMP(statut ='o',
#   -----------------------------------
        typ = 'TXM',
        into = ['SISYPHE','TOMAWAC','DELWAQ'],
        defaut = '',
        fr = """ Liste des codes avec lesquels on couple Telemac-2D SISYPHE : couplage
interne avec Sisyphe TOMAWAC : couplage interne avec Tomawac DELWAQ :
sortie de fichiers de resultats pour Delwaq""",
        ang = """ List of codes to be coupled with Telemac-2D SISYPHE : internal
coupling with Sisyphe TOMAWAC : internal coupling with Tomawac DELWAQ:
will yield results file for Delwaq""",
    ),
#   -----------------------------------
    NAMES_OF_CLANDESTINE_VARIABLES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min= 2, max= 2,
        fr = """ Noms de variables qui ne sont pas utilisees par TELEMAC; mais qui
doivent etre conservees lors de son execution. Ceci peut etre utilise
entre autres lors du couplage de TELEMAC avec un autre code. Les
variables clandestines sont alors des variables propres a l''autre code
et sont rendues dans le fichier de resultats.""",
        ang = """ Names of variables that are not used by TELEMAC, but should be
preserved when it is being run. This keyword may be used, for instance
when it if TELEMAC is coupled with another code. Thus, the clandestine
variables belong to the other code and are given back in the results
file.""",
    ),
#   -----------------------------------
    DELWAQ = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        COUPLING_DIRECTORY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """ Nom complet du dossier d echange des fichiers pour couplage de codes""",
            ang = """ Name with full path of the directory where the files will be exchanged
for coupling""",
        ),
#       -----------------------------------
        DELWAQ_PRINTOUT_PERIOD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Periode de sortie des resultats pour Delwaq""",
            ang = """ Printout period for Delwaq file""",
        ),
#       -----------------------------------
        VOLUMES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        EXCHANGE_AREAS_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        VERTICAL_FLUXES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        SALINITY_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        BOTTOM_SURFACES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        EXCHANGES_BETWEEN_NODES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        NODES_DISTANCES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        TEMPERATURE_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        VELOCITY_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        DIFFUSIVITY_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        DELWAQ_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats pour le couplage avec Delwaq""",
            ang = """ Results file for coupling with Delwaq""",
        ),
#       -----------------------------------
        SALINITY_FOR_DELWAQ = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Decide de la sortie de la salinite pour Delwaq""",
            ang = """ Triggers output of salinity for Delwaq""",
        ),
#       -----------------------------------
        TEMPERATURE_FOR_DELWAQ = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Decide de la sortie de la temperature pour Delwaq""",
            ang = """ Triggers output of temperature for Delwaq""",
        ),
#       -----------------------------------
        VELOCITY_FOR_DELWAQ = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Decide de la sortie de la vitesse pour Delwaq""",
            ang = """ Triggers output of velocity for Delwaq""",
        ),
#       -----------------------------------
        DIFFUSIVITY_FOR_DELWAQ = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Decide de la sortie du coefficient de diffusion pour Delwaq""",
            ang = """ Triggers output of diffusion for Delwaq""",
        ),
    ),
#   -----------------------------------
    SISYPHE = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        SISYPHE_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """ Fichier des parametres de Sisyphe en cas de couplage interne""",
            ang = """ Sisyphe parameter file in case of internal coupling""",
        ),
#       -----------------------------------
        COUPLING_PERIOD_FOR_SISYPHE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ pour eviter de faire le couplage a chaque pas de temps""",
            ang = """ to avoid coupling at every time-step""",
        ),
    ),
#   -----------------------------------
    TOMAWAC = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        TOMAWAC_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """ Fichier des parametres de Tomawac en cas de couplage interne""",
            ang = """ Tomawac parameter file in case of internal coupling""",
        ),
#       -----------------------------------
        COUPLING_PERIOD_FOR_TOMAWAC = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ pour eviter de faire le couplage a chaque pas de temps""",
            ang = """ to avoid coupling at every time-step""",
        ),
    ),
#   -----------------------------------
    WAQTEL = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        WAQTEL_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """ fichier des parametres physiques pour les processus de qualite d eau
(internes non ceux de DELWAQ)""",
            ang = """ file for physical parameters of waq processes (local ones of
Telemac-tracer not those of DELWAQ)""",
        ),
    ),
)
# -----------------------------------------------------------------------
OUTPUT_FILES = PROC(nom= "OUTPUT_FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    RESULTS_FILES = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_FIRST_TIME_STEP_FOR_GRAPHIC_PRINTOUTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Determine le nombre de pas de temps a partir duquel debute l''ecriture
des resultats dans le FICHIER DES RESULTATS.""",
            ang = """ Determines the number of time steps after which the results are first
written into the RESULTS FILE.""",
        ),
#       -----------------------------------
        GRAPHIC_PRINTOUT_PERIOD = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Determine la periode en nombre de pas de temps d''impression des
VARIABLES POUR LES SORTIES GRAPHIQUES (voir ce mot-cle) dans le FICHIER
DES RESULTATS.""",
            ang = """ Determines, in number of time steps, the printout period for the
VARIABLES FOR GRAPHIC PRINTOUTS in the RESULTS FILE.""",
        ),
#       -----------------------------------
        VARIABLES_FOR_GRAPHIC_PRINTOUTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["velocity along x axis (m/s)","velocity along y axis (m/s)","wave celerity (m/s)","water depth (m)","free surface elevation (m)","bottom elevation (m)","Froude number","scalar flowrate of fluid (m2/s)","tracer 1 etc.","turbulent kinetic energy in k-epsilon model (J/kg)","dissipation of turbulent energy (W/kg)","turbulent viscosity (m2/s)","flowrate along x axis (m2/s)","flowrate along y axis (m2/s)","scalar velocity (m/s)","wind along x axis (m/s)","wind along y axis (m/s)","air pressure (Pa)","friction coefficient","drift along x (m)","drift along y (m)","Courant number ","supplementary variable N","supplementary variable O","supplementary variable R","supplementary variable Z","maximum elevation","time of maximum elevation","maximum velocity","time of maximum velocity","friction velocity","gradient 1, etc. "],
            defaut = ["velocity along x axis (m/s)","velocity along y axis (m/s)","water depth (m)","bottom elevation (m)"],
            fr = """ Noms des variables que l''utilisateur veut ecrire dans le fichier des
resultats. Chaque variable est representee par une lettre. Le choix des
separateurs est libre. Les possibilites offertes sont les suivantes :
 - U : vitesse suivant l''axe des x (m/s),
 - V : vitesse suivant l''axe des y (m/s),
 - C : celerite des ondes (m/s),
 - H : hauteur d''eau (m),
 - S : cote de surface libre (m),
 - B : cote du fond (m),
 - F : nombre de Froude,
 - Q : debit scalaire du fluide (m2/s),
 - Tn: traceur, avec n le numero du traceur,
 - K : energie turbulente du modele k-epsilon (J/kg),
 - E : dissipation de l''energie turbulente (W/kg),
 - D : viscosite turbulente du modele k-epsilon (m2/s),
 - I : debit suivant l''axe des x (m2/s),
 - J : debit suivant l''axe des y (m2/s),
 - M : vitesse scalaire (m/s),
 - X : vent suivant l''axe des x (m/s),
 - Y : vent suivant l''axe des y (m/s),
 - P : pression atmospherique (Pa),
 - W : coefficient de frottement sur le fond,
 - A : derive en x (m),
 - G : derive en y (m),
 - L : coefficient de frottement sur le fond,
 - Gn: gradient differencie, avec n le numero de reference du gradient.
L''utilisateur dispose egalement de 4 champs libres, qu''il peut
utiliser pour ecrire dans le fichier des resultats des variables qu''il
cree lui-meme. Ces variables propres a l''utlisateur doivent etre
calculees dans le sous-programme PRERES et le nom que l''on desire leur
donner doit etre ecrit dans le sous-programme NOMVAR. Ces 7 champs sont
:
 - N, O, R, Z qui correspondent aux tableaux PRIVE(1,1), PRIVE(1,2),
 PRIVE(1,3), PRIVE(1,4).
A la difference des variables precedentes, celles-ci sont conservees
dans tout le programme, et peuvent donc etre reutilisees.  Dans ce
dernier cas ne pas oublier de donner une taille suffisante au tableau
PRIVE (dans le programme principal). Il est ainsi possible de limiter,
par exemple, la taille des fichiers de resultats pour de tres gros
calculs. Cependant, il faut etre conscient du fait que, dans
l''eventualite d''une reprise de calcul, le code doit disposer, dans le
fichier des resultats, des informations necessaires a sa poursuite, a
savoir :
 - les vitesses U et V,
 - les hauteurs d''eau H,
 - les cotes du fond B.
Toutefois, TELEMAC peut recalculer certaines de ces variables a
partir d''autres qui lui seront fournies (par exemple, il recalculera H
a partir de S et B).""",
            ang = """ Names of variables the user wants to write into the results file. Each
variable is represented by a letter. The separators can be freely
selected. The available capabilities are as follows:
 - U : velocity along x axis (m/s),
 - V : velocity along y axis (m/s),
 - C : wave celerity (m/s),
 - H : water depth (m),
 - S : free surface elevation (m),
 - B : bottom elevation (m),
 - F : Froude number,
 - Q : scalar flowrate of fluid (m2/s),
 - Tn : tracer, with n the tracer number,
 - K : turbulent kinetic energy in k-epsilon model (J/kg),
 - E : dissipation of turbulent energy (W/kg),
 - D : turbulent viscosity of k-epsilon model (m2/s),
 - I : flowrate along x axis (m2/s),
 - J : flowrate along y axis (m2/s),
 - M : scalar velocity (m/s),
 - X : wind along x axis (m/s) Y : wind along y axis (m/s),
 - P : air pressure (Pa),
 - W : friction coefficient ,
 - A : drift along x,
 - G : drift along y,
 - L : nombre de courant,
 - Gn : differentiated gradient, with n the gradient reference number.
Four other variables are also made available to the
user who may use them for writing into the file the results of variables
he creates himself. These user-specific variables should be computed in
the subroutine PRERES and their desired name should be written into the
subroutine NOMVAR. These seven variables are as follows:
- N, O, R, Z
which correspond to arrays PRIVE(1,1) up to PRIVE(1, Unlike the
preceding variables, they are preserved throughout the program, so that
they can be used again.
In the latter case, do not forget to provide the
array PRIVE with sufficiently large dimensions (in FORTRAN file). With
this key-word, one can limit the size of the RESULTS FILE. It should be
kept in mind, however, that if a computation has to be continued, the
RESULTS FILE should contain the appropriate information for running the
code,i.e.:
 - velocities U and V,
 - water depths H,
 - bottom elevations B.
TELEMAC, however, can compute some of these variables from others for
example, it will compute H from S and B.""",
        ),
#       -----------------------------------
        NUMBER_OF_PRIVATE_ARRAYS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Nombre de tableaux mis a disposition de l utilisateur""",
            ang = """ Number of arrays for own user programming""",
        ),
#       -----------------------------------
        NAMES_OF_PRIVATE_VARIABLES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            fr = """ Noms des variables privees en 32 caracteres, 16 pour le nom 16 pour
l''unite. Elles correspondent au bloc PRIVE et peuvent etre lues dans le
fichier de geometrie si elles y sont presentes avec leur nom""",
            ang = """ Name of private variables in 32 characters, 16 for the name, 16 for
the unit. They are stored in the block PRIVE and can be read in the
geometry file if they are here with their name""",
        ),
#       -----------------------------------
        RESULTS_FILE_FORMAT = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM',
            into = ['SERAFIN?','SERAFIND','MED'],
            defaut = 'SERAFIN?',
            fr = """ Format du fichier de resultats. Les valeurs possibles sont : - SERAFIN
: format standard simple precision pour Telemac; - SERAFIND: format
standard double precision pour Telemac; - MED : format MED base sur
HDF5""",
            ang = """ Results file format. Possible values are: - SERAFIN : classical single
precision format in Telemac; - SERAFIND: classical double precision
format in Telemac; - MED : MED format based on HDF5""",
        ),
#       -----------------------------------
        RESULTS_FILE = SIMP(statut ='o',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Nom du fichier dans lequel seront ecrits les resultats du calcul avec
la periodicite donnee par le mot cle PERIODE POUR LES SORTIES
GRAPHIQUES.""",
            ang = """ Name of the file into which the computation results shall be written,
the periodicity being given by the key-word: GRAPHIC PRINTOUT PERIOD.""",
        ),
#       -----------------------------------
        BINARY_RESULTS_FILE_FORMAT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            into = ['BIN','SERAFIN','SERAFIND','MED'],
            defaut = 'BIN',
            fr = """ Format du fichier de resultats binaire. Les valeurs possibles sont : -
SERAFIN : format standard simple precision pour Telemac; - SERAFIND:
format standard double precision pour Telemac; - MED : format MED base
sur HDF5""",
            ang = """ Binary results file format. Possible values are: - SERAFIN : classical
single precision format in Telemac; - SERAFIND: classical double
precision format in Telemac; - MED : MED format based on HDF5""",
        ),
#       -----------------------------------
        BINARY_RESULTS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats code en binaire mis a la disposition de
l''utilisateur. Les resultats a placer dans ce fichier seront a ecrire
sur le canal 28.""",
            ang = """ Additional binary-coded result file made available to the user. The
results to be entered into this file shall be written on channel 28.""",
        ),
#       -----------------------------------
        FORMATTED_RESULTS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ Fichier de resultats formate mis a la disposition de l''utilisateur.
Les resultats a placer dans ce fichier seront a ecrire sur le canal 29.""",
            ang = """ Formatted file of results made available to the user. The results to
be entered into this file shall be written on channel 29.""",
        ),
    ),
#   -----------------------------------
    CONTROL_SECTION = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        CONTROL_SECTIONS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 2, max= 2,
            fr = """ Couples de points (numeros globaux dans le maillage) entre lesquels
les debits instantanes et cumules seront donnes.""",
            ang = """ Couples of points (global numbers in the mesh) defining sections where
the instantaneous and cumulated discharges will be given""",
        ),
#       -----------------------------------
        PRINTING_CUMULATED_FLOWRATES = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ IMPRESSION DU FLUX CUMULE A TRAVERS LES SECTIONS DE CONTROLE""",
            ang = """ PRINTING THE CUMULATED FLOWRATES THROUGH CONTROL SECTIONS""",
        ),
#       -----------------------------------
        COMPATIBLE_COMPUTATION_OF_FLUXES = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ FLUX A TRAVERS LES SECTIONS DE CONTROLE, CALCUL COMPATIBLE AVEC
L''IMPERMEABILITE SOUS FORME FAIBLE""",
            ang = """ FLOWRATES THROUGH CONTROL SECTIONS, COMPUTATION COMPATIBLE WITH THE
WEAK FORMULATION OF NO-FLUX BOUNDARY CONDITION""",
        ),
#       -----------------------------------
        SECTIONS_INPUT_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """ sections input file, partitioned""",
            ang = """ sections input file, partitioned""",
        ),
#       -----------------------------------
        SECTIONS_OUTPUT_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """ sections output file, written by the master""",
            ang = """ sections output file, written by the master""",
        ),
    ),
#   -----------------------------------
    LISTING = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_FIRST_TIME_STEP_FOR_LISTING_PRINTOUTS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """ Determine le nombre de pas de temps a partir duquel debute l''ecriture
des resultats dans le listing.""",
            ang = """ Determines the number of time steps after which the results are first
written into the listing.""",
        ),
#       -----------------------------------
        LISTING_PRINTOUT_PERIOD = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Determine la periode en nombre de pas de temps d''impression des
VARIABLES A IMPRIMER (voir ce mot-cle) Pour la mise au point, il faut
savoir que la sortie des resultats est effectuee systematiquement sur le
fichier de retour d''execution du code (actuellement accessible par le
menu 3.f de SPF sur IBM, et dans le fichier !CAS.SORTIE sur station de
travail)""",
            ang = """ Determines, in number of time steps, the printout period of the
VARIABLES TO BE PRINTED The results are systematically printed out on
the listing file (file CAS.SORTIE at the workstation).""",
        ),
#       -----------------------------------
        LISTING_FOR_PRINTOUT_PERIOD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """ Determine la periode en nombre de pas de temps d''impression des
VARIABLES A IMPRIMER (voir ce mot-cle) Pour la mise au point, il faut
savoir que la sortie des resultats est effectuee systematiquement sur le
fichier de retour d''execution du code (actuellement accessible par le
menu 3.f de SPF sur IBM, et dans le fichier !CAS.SORTIE sur station de
travail)""",
            ang = """ Determines, in number of time steps, the printout period of the
VARIABLES TO BE PRINTED The results are systematically printed out on
the listing file (file CAS.SORTIE at the workstation).""",
        ),
#       -----------------------------------
        LISTING_PRINTOUT = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Sortie des resultats sur support papier. Si l''on met NON le listing
ne contient que l''entete et la mention FIN NORMALE DU PROGRAMME
Commande a eviter""",
            ang = """ Result printout on hard copy. When NO is selected, the listing only
includes the heading and the phrase "NORMAL END OF PROGRAM" In addition,
the options MASS BALANCE and VALIDATION are inhibited. Not recommended
for use.""",
        ),
#       -----------------------------------
        VARIABLES_TO_BE_PRINTED = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["velocity along x axis (m/s)","velocity along y axis (m/s)","wave celerity (m/s)","water depth (m)","free surface elevation (m)","bottom elevation (m)","Froude number","scalar flowrate of fluid (m2/s)","tracer 1, etc.","turbulent kinetic energy in k-epsilon model (J/kg)","dissipation of turbulent energy (W/kg)","turbulent viscosity of k-epsilon model (m2/s)","flowrate along x axis (m2/s)","flowrate along y axis (m2/s)","scalar velocity (m/s)","wind along x axis (m/s)","wind along y axis (m/s)","air pressure (Pa)","friction coefficient","drift along x (m)","drift along y (m)","nombre de courants ","supplementary variable N","supplementary variable O","supplementary variable R","supplementary variable Z","gradient 1, etc."],
            defaut = [],
            fr = """ Nom des variables que l''utilisateur desire ecrire a l''ecran. Meme
possibilites que pour les sorties graphiques.""",
            ang = """""",
        ),
#       -----------------------------------
        MASS_BALANCE = SIMP(statut ='o',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """ Determine si l''on effectue ou non le bilan de masse sur le domaine.
Cette procedure calcule a chaque pas de temps : - les flux aux entrees
et sorties du domaine; - le flux global a travers l''ensemble des parois
du domaine (liquides ou solides) - l''erreur relative sur la masse pour
ce pas de temps. En fin de listing, on trouve l''erreur relative sur la
masse pour l''ensemble du calcul. Il ne s''agit que d''un calcul
indicatif car il n''existe pas d''expression compatible du debit en
formulation c,u,v.""",
            ang = """ Determines whether a check of the mass-balance over the domain is
mader or not. This procedures computes the following at each time step:
the domain inflows and outflows, the overall flow across all the
boundaries, the relative error in the mass for that time step. The
relative error in the mass over the whole computation can be found at
the end of the listing.""",
        ),
#       -----------------------------------
        INFORMATION_ABOUT_SOLVER = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = True,
            fr = """ Donne a chaque pas de temps le nombre d''iterations necessaires a la
convergence du solveur de l''etape de propagation.""",
            ang = """ if YES, prints the number of iterations that have been necessar to get
the solution of the linear system.""",
        ),
#       -----------------------------------
        LIST_OF_POINTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'I', min= 2, max= 2,
            fr = """ Liste de points remarquables pour les impressions""",
            ang = """ List of remarkable points for printouts""",
        ),
#       -----------------------------------
        NAMES_OF_POINTS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            fr = """ Noms des points remarquables pour les impressions""",
            ang = """ Names of remarkable points for printouts""",
        ),
    ),
#   -----------------------------------
    FOURIER = FACT(statut='f',
#   -----------------------------------
#       -----------------------------------
        FOURIER_ANALYSIS_PERIODS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """ Liste des periodes que l''on veut analyser""",
            ang = """ List of periods to be analysed""",
        ),
#       -----------------------------------
        TIME_RANGE_FOR_FOURIER_ANALYSIS = SIMP(statut ='o',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            defaut = [0.,0.],
            fr = """ Pour le calcul du marnage et de la phase de la maree""",
            ang = """ For computing tidal range and phase of tide""",
        ),
    ),
)
# -----------------------------------------------------------------------
MISC = PROC(nom= "MISC",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NUMBER_OF_CORRECTIONS_OF_DISTRIBUTIVE_SCHEMES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """ Pour les options avec predicteur-correcteur""",
        ang = """ For predictor-corrector options""",
    ),
#   -----------------------------------
    NUMBER_OF_SUB_STEPS_OF_DISTRIBUTIVE_SCHEMES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """ Pour les options predicteur-correcteur avec schema localement
implicite""",
        ang = """ Only for implicit scheme with predictor-corrector""",
    ),
#   -----------------------------------
    WAVE_ENHANCED_FRICTION_FACTOR = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """ Active la prise en compte des interactions non-lineaires entre la
houle et les courant pour le calcul du courant de houle (cf OConnor and
Yoo, 1988, Coast Eng.12.)""",
        ang = """ Wave friction enhancement for the calculation of the wave generated
longshore current (cf OConnor and Yoo, 1988, Coast Eng.12.)""",
    ),
#   -----------------------------------
    LOCAL_NUMBER_OF_THE_POINT_TO_CALIBRATE_HIGH_WATER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 0,
        fr = """ Numero local du point entre 1 et le nombre de points de frontiere
maritime (du FICHIER DES CONSTANTES HARMONIQUES) ou les conditions aux
limites de maree sont calculees avec les bases de donnees JMJ, NEA, FES,
PREVIMER (sauf les bases de type TPXO). Les ondes de maree sont
dephasees par rapport a ce point pour debuter le calcul par une pleine
mer (en marees schematiques seulement).""",
        ang = """ Local number between 1 and the number of tidal boundary points (of the
HARMONIC CONSTANTS FILE) where the tidal boundary conditions are
computed with JMJ, NEA, FES, PREVIMER databases (except TPXO-type
databases). The tidal constituents have their phase shifted with respect
to this point to start the simulation with a high water (for schematic
tides only).""",
    ),
#   -----------------------------------
    GLOBAL_NUMBER_OF_THE_POINT_TO_CALIBRATE_HIGH_WATER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 0,
        fr = """ Numero global du point par rapport auquel les ondes de maree sont
dephasees pour debuter le calcul par une pleine mer (en marees
schematiques seulement). Ne concerne que les bases de constantes
harmoniques de type TPXO.""",
        ang = """ Global number of the point with respect to which the tidal
constituents have their phase shifted to start the calculation with a
high water (for schematic tides only). Only harmonic constants databases
like TPXO are concerned.""",
    ),
#   -----------------------------------
    PSI_SCHEME_OPTION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ["explicit","predictor-corrector"],
        defaut = "explicit",
        fr = """ 1: explicite 2: predicteur-correcteur""",
        ang = """ 1: explicit 2: predictor-corrector""",
    ),
#   -----------------------------------
    PROPAGATION_OPTION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 3,
        fr = """ Non active pour l''instant.""",
        ang = """ Not yet implemented.""",
    ),
#   -----------------------------------
    FREE_INTEGER_20 = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 3,
        fr = """""",
        ang = """""",
    ),
#   -----------------------------------
    VECTOR_LENGTH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """ LONGUEUR DU VECTEUR POUR LES MACHINES VECTORIELLES""",
        ang = """ VECTOR LENGTH ON VECTOR MACHINES""",
    ),
#   -----------------------------------
    LANGUAGE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ["FRANCAIS","ANGLAIS"],
        defaut = "ANGLAIS",
        fr = """ 1 : FRANCAIS 2 : ANGLAIS""",
        ang = """ 1: FRENCH 2: ENGLISH""",
    ),
#   -----------------------------------
    OPTION_FOR_CULVERTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """Option pour le traitement des buses. Il existe deux formulations
dans Telemac""",
        ang = """Option for the treatment of culverts. There are two options in
Telemac""",
    ),
#   -----------------------------------
    RAINFALL_RUNOFF_MODEL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ["No infiltration","CN runoff model"],
        defaut = ,
        fr = """Option pour modele pluie-debit. Les options disponibles sont:
  0 : Pas d infiltration (fonction de base)
  1 : Modele CN (Curve Number du SCS)""",
        ang = """Option for the rainfall-runoff model. Available options are:
  1 : No infiltration
  2 : CN runoff model (Curve Number method of the SCS)""",
    ),
#   -----------------------------------
    ANTECEDENT_MOISTURE_CONDITIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 2,
        fr = """Donne les conditions d humidite precedant un episode de pluie pour
le modele CN du SCS. Les options disponibles sont:
  1 : conditions precedentes seches
  2 : conditions precedentes normales
  3 : conditions prcedentes mouillees
ce mot cle est uniquement utile pour le modele pluie-débit 1 (CN)""",
        ang = """Gives the antecedent moisture conditions before a rainfall
 event for the SCS CN runoff model. Available options are:
  1 : dry antecedent conditions
  2 : normal antecedent conditions
  3 : wet antecedent conditions
this keyword is only usefull for runoff model 1 (SCS CN model)""",
    ),
#   -----------------------------------
    DURATION_OF_RAIN_OR_EVAPORATION_IN_HOURS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 1.E6,
        fr = """Donne la duree de la pluie en heure, par defaut pluie infinie""",
        ang = """Gives the duration of the rain in hour,
default value is infinite""",
    ),
#   -----------------------------------
    FLUXLINE = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """FLUXLINE""",
        ang = """Use Fluxline to compute flux over lines""",
    ),
#   -----------------------------------
    ASCII_ATMOSPHERIC_DATA_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'),
        defaut = '',
        fr = """Fichier de donnees en ascii contenant les informations
atmospheriques variables en temps""",
        ang = """Ascii data file containing the atmospheric data varying in
time""",
    ),
#   -----------------------------------
    BINARY_ATMOSPHERIC_DATA_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'),
        defaut = '',
        fr = """Fichier de donnees code en binaire contenant les informations
atmospheriques variables en temps et en espace sur le maillage""",
        ang = """Binary-coded data file containing the atmospheric data varying in
time and space on the mesh""",
    ),
#   -----------------------------------
    BINARY_ATMOSPHERIC_DATA_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN?','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier binaire de donn\E9es atmospheriques.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """Binary atmospheric file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
#   -----------------------------------
    FLUXLINE_INPUT_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'),
        defaut = '',
        fr = """Nom du fichier de fluxline, avec des donnees sur les sections""",
        ang = """Name of the Fluxline file, with data on cross-sections""",
    ),
#   -----------------------------------
    OPTION_OF_THE_HYDROSTATIC_RECONSTRUCTION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """ Donne l option de la reconstruction hydrostatique (option utile
uniquement pour les volumes finis): 1: option d Audusse, 2: option de
Noelle""",
        ang = """ Gives the option for hydrostatic reconstruction (used only for finite
volumes): 1: option of Audusse, 2: option of Noelle""",
    ),
#   -----------------------------------
    NUMBER_OF_DIFFERENTIATORS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """ Definit le nombre de differentiateurs utilisateurs.""",
        ang = """ Defines the number of user differentiators""",
    ),
#   -----------------------------------
    OPTION_FOR_INITIAL_ABSTRACTION_RATIO = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """ Donne le ratio entre pertes initiales IA et la retention potenti
maximale S pour le modele pluie-debit SCS CN. Les options disponibles so
1 : IA/S = 0.2 (methode standard)   2 : IA/S = 0.05 (methode revisee,
cf. Woodward, Hawkins et al. 2003. A cette option les coefficients CN
fournis en entree sont alors automatiquement corriges, cf.  manuel
utilisateur). Ce mot cle est uniquement utile pour le modele pluie-debit
1 (CN)""",
        ang = """ Gives the ratio for Initial Abstraction to Maximal Potential Retention
S for the SCS CN runoff model. Available options are:   1 : IA/S = 0.2
(standard method) 2 : IA/S = 0.05 (revised method, see Woodward, Hawkins
et al. 2003. With this option the CN values given in input are
automatically convers see user manual). This keyword is only useful for
runoff model 1 (SCS CN model)""",
    ),
#   -----------------------------------
    NAMES_OF_DIFFERENTIATORS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min= 2, max= 2,
        fr = """ Noms des differentiateurs utilisateurs en 32 caracteres, 16 pour le
nom, 16 pour l''unite""",
        ang = """ Name of user differentiators in 32 characters, 16 for the name, 16 for
the unit.""",
    ),
#   -----------------------------------
    CONVERGENCE_STUDY = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Active une etude de convergence par rapport a une
solution analytique sur un maillage fin""",
        ang = """Activates a convergence study compared
to an analytical solution on a fine mesh""",
    ),
#   -----------------------------------
    REFINEMENT_LEVELS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 0,
        fr = """Donne le nombre de raffinements que l''utilisateur
veut utiliser pour l''etude de convergence
(en activant CONVERGENCE). Chaque niveau multiplie par 4 le
nombre d''elements.""",
        ang = """Gives the number of refinement levels that the
user wants to use in the convergence study (when activating
CONVERGENCE). Each level multiplies the number of elements by
4""",
    ),
)
# -----------------------------------------------------------------------
INTERNAL = PROC(nom= "INTERNAL",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    STEERING_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'),
        defaut = '',
        fr = """ Nom du fichier contenant les parametres du calcul a realiser.""",
        ang = """ Name of the file containing the parameters of the computation Written
by the user.""",
    ),
#   -----------------------------------
    DICTIONARY = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'),
        defaut = 'telemac2d.dico',
        fr = """ Dictionnaire des mots cles.""",
        ang = """ Key word dictionary.""",
    ),
#   -----------------------------------
    PARTITIONING_TOOL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['METIS','SCOTCH','PARMETIS','PTSCOTCH'],
        defaut = 'METIS',
        fr = """ CHOIX DU PARTITIONNEUR 1 : METIS 2 : SCOTCH 3 : PARMETIS 4 : PTSCOTCH
etc...""",
        ang = """ PARTITIONING TOOL SELECTION 1 : METIS 2 : SCOTCH 3 : PARMETIS 4 :
PTSCOTCH etc...""",
    ),
#   -----------------------------------
    RELEASE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        defaut = 'V7P1',
        fr = """ Numero de version des bibliotheques utilisees par TELEMAC. SUR UNE
STATION DE TRAVAIL 5 versions sont donnees correspondant a :
TELEMAC,DAMO,UTILE,BIEF,HP""",
        ang = """ version number of the libraries used by TELEMAC. ON A WORKSTATION 5
numbers are given, corresponding to the libraries called:
TELEMAC,DAMO,UTILE,BIEF,HP""",
    ),
#   -----------------------------------
    LIST_OF_FILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=47, max=47,
        defaut = 'STEERING FILE;DICTIONARY;FORTRAN FILE;GEOMETRY FILE;BOUNDARY CONDITIONS FILE;RESULTS FILE;PREVIOUS COMPUTATION FILE;BOTTOM TOPOGRAPHY FILE;BINARY DATA FILE 1;BINARY DATA FILE 2;FORMATTED DATA FILE 1;FORMATTED DATA FILE 2;BINARY RESULTS FILE;FORMATTED RESULTS FILE;REFERENCE FILE;LIQUID BOUNDARIES FILE;FRICTION DATA FILE;VOLUMES DELWAQ FILE;EXCHANGE AREAS DELWAQ FILE;VERTICAL FLUXES DELWAQ FILE;SALINITY DELWAQ FILE;VELOCITY DELWAQ FILE;DIFFUSIVITY DELWAQ FILE;BOTTOM SURFACES DELWAQ FILE;EXCHANGES BETWEEN NODES DELWAQ FILE;NODES DISTANCES DELWAQ FILE;TEMPERATURE DELWAQ FILE;DELWAQ STEERING FILE;STAGE-DISCHARGE CURVES FILE;SOURCES FILE;SECTIONS INPUT FILE;SECTIONS OUTPUT FILE;OIL SPILL STEERING FILE;HARMONIC CONSTANTS FILE;TIDAL MODEL FILE;ASCII DATABASE FOR TIDE;BINARY DATABASE 1 FOR TIDE;BINARY DATABASE 2 FOR TIDE;WEIRS DATA FILE;SIPHONS DATA FILE;CULVERTS DATA FILE;BREACHES DATA FILE;DROGUES FILE;ZONES FILE;FLUXLINE INPUT FILE;ASCII ATMOSPHERIC DATA FILE;BINARY ATMOSPHERIC DATA FILE',
        fr = """ Noms des fichiers exploites par le code""",
        ang = """ File names of the used files""",
    ),
#   -----------------------------------
    DESCRIPTION_OF_LIBRARIES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=10, max=10,
        defaut = 'builds|PPP|lib|telemac2dMMMVVV.LLL;builds|PPP|lib|sisypheMMMVVV.LLL;builds|PPP|lib|tomawacMMMVVV.LLL;builds|PPP|lib|dredgesimMMMVVV.LLL;builds|PPP|lib|waqtelMMMVVV.LLL;builds|PPP|lib|biefMMMVVV.LLL;builds|PPP|lib|hermesMMMVVV.LLL;builds|PPP|lib|damoMMMVVV.LLL;builds|PPP|lib|parallelMMMVVV.LLL;builds|PPP|lib|specialMMMVVV.LLL',
        fr = """ Description des librairies de T2D""",
        ang = """ LIBRARIES description""",
    ),
#   -----------------------------------
    DEFAULT_EXECUTABLE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        defaut = 'builds|PPP|bin|telemac2dMMMVVV.exe',
        fr = """ Executable par defaut de T2D""",
        ang = """ Default executable for T2D""",
    ),
#   -----------------------------------
    DEFAULT_PARALLEL_EXECUTABLE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        defaut = 'builds|PPP|bin|telemac2dMMMVVV.exe',
        fr = """ Executable parallele par defaut de T2D""",
        ang = """ Default parallel executable for T2D""",
    ),
)
Ordre_des_commandes = (
'INITIALIZATION',
'BOUNDARY_CONDITIONS',
'GENERAL_PARAMETERS',
'PHYSICAL_PARAMETERS',
'SECONDARY_CURRENTS_INFO',
'TURBULENCE',
'NUMERICAL_PARAMETERS',
'TIDAL_FLATS_INFO',
'TRACERS',
'PARTICLE_TRANSPORT',
'CONSTRUCTION_WORKS',
'TIDES',
'COUPLING',
'OUTPUT_FILES',
'MISC',
'INTERNAL')
