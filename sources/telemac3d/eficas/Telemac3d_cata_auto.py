
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
NUMERICAL_PARAMETERS = PROC(nom= "NUMERICAL_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    INITIAL_VALUES_OF_TRACERS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """Fixe la valeur initiale des traceurs.""",
        ang = """Sets the initial values of tracers.""",
    ),
#   -----------------------------------
    THRESHOLD_FOR_VISCOSITY_CORRECTION_ON_TIDAL_FLATS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.2],
        fr = """Pour les profondeurs inferieures, la viscosite sera
progressivement reduite. Voir le sous-programme
\telfile{VISCLIP}.""",
        ang = """Below the threshold, viscosity will be progressively
cancelled. See subroutine \telfile{VISCLIP}.""",
    ),
#   -----------------------------------
    THRESHOLD_FOR_SEDIMENT_FLUX_CORRECTION_ON_TIDAL_FLATS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.2],
        fr = """Pour les profondeurs inferieures a cette valeur limite, le flux
sedimentaire sera nul. Voir le sous-programme \telfile{FLUSED}.""",
        ang = """Below this limiting depth, all sediment erosion rates are set
to zero. See subroutine \telfile{FLUSED}.""",
    ),
#   -----------------------------------
    PRESCRIBED_FLOWRATES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=10, max=10,
        fr = """Fixe le debit sur les frontieres a debit impose.""",
        ang = """Sets the value for flow rate on flow
rate-imposed boundaries.""",
    ),
#   -----------------------------------
    PRESCRIBED_ELEVATIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=10, max=10,
        fr = """Fixe la cote sur les frontieres a cote imposee.""",
        ang = """Sets the elevation on elevation-imposed boundaries.""",
    ),
#   -----------------------------------
    PRESCRIBED_VELOCITIES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=10, max=10,
        fr = """Fixe la vitesse sur les frontieres a vitesse imposee.""",
        ang = """Sets the magnitude of velocity on velocity-imposed boundaries.""",
    ),
#   -----------------------------------
    DEBUGGER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Pour imprimer la sequence des appels, mettre 1.""",
        ang = """If 1, additional writings will be printed in the listing,
in particular the calls of subroutines.""",
    ),
#   -----------------------------------
    TREATMENT_OF_NEGATIVE_DEPTHS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Seulement avec \telkey{OPTION DE TRAITEMENT DES BANCS DECOUVRANTS}
= 1. Les choix possibles sont :
\begin{itemize}
\item 0 : pas de traitement ;
\item 1 : lissage ;
\item 2 : limitation des flux.
\end{itemize}""",
        ang = """Only with \telkey{OPTION FOR THE TREATMENT OF TIDAL FLATS}
= 1. Possible choices are:
\begin{itemize}
\item 0: no treatment,
\item 1: smoothing,
\item 2: flux control.
\end{itemize}""",
    ),
#   -----------------------------------
    BYPASS_VOID_VOLUMES = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Accelere les schemas de convection distributifs et volumes finis
en cas de bancs decouvrants ou de transformation sigma generalisee.""",
        ang = """Will speed-up distributive and finite volumes advection
schemes in case of tidal flats or generalised sigma transformation.""",
    ),
#   -----------------------------------
    VELOCITY_PROJECTED_ON_SOLID_LATERAL_BOUNDARIES = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [True ],
        fr = """$\vec{U}.\vec{n} = 0$ sur les parois laterales solides est force
en fin de boucle en temps.""",
        ang = """Will ensure $\vec{U}.\vec{n} = 0$ on solid lateral boundaries
by a projection at the end of time loop.""",
    ),
#   -----------------------------------
    VELOCITY_PROJECTED_ON_BOTTOM = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [True ],
        fr = """$\vec{U}.\vec{n} = 0$ sur le fond est force en fin de boucle
en temps.""",
        ang = """Will ensure $\vec{U}.\vec{n} = 0$ on bottom by a projection
at the end of time loop.""",
    ),
#   -----------------------------------
    VERTICAL_VELOCITY_DERIVATIVES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Mode de calcul des derivees des vitesses suivant $z$ :
\begin{itemize}
\item 1 : derivee lineaire (classique) ;
\item 2 : derivee logarithmique (mieux pour profils logarithmiques).
\end{itemize}""",
        ang = """Way of computing the velocity derivatives along $z$:
\begin{itemize}
\item 1: linear derivative (classic),
\item 2: logarithmic derivative (better for logarithmic profiles).
\end{itemize}""",
    ),
#   -----------------------------------
    RESTART_MODE = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Declenche le remplissage du
\telkey{FICHIER POUR SUITE}, qui permet une suite de calcul
parfaite, contrairement au \telkey{FICHIER DES RESULTATS 3D}.""",
        ang = """Triggers the filling of the \telkey{RESTART FILE},
which ensures a perfect restart of a computation,
unlike using the \telkey{3D RESULT FILE}.""",
    ),
#   -----------------------------------
    VALUES_OF_TRACERS_IN_THE_RAIN = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """Fixe la valeur des traceurs dans la pluie.""",
        ang = """Sets the value of the tracers in the rain.""",
    ),
#   -----------------------------------
    COEFFICIENT_TO_CALIBRATE_TIDAL_RANGE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1.],
        fr = """Coefficient pour ajuster le marnage de l''onde de maree
aux frontieres maritimes.""",
        ang = """Coefficient to calibrate the tidal range of tidal wave
at tidal open boundary conditions.""",
    ),
#   -----------------------------------
    COEFFICIENT_TO_CALIBRATE_TIDAL_VELOCITIES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [999999.],
        fr = """Coefficient pour ajuster les composantes de vitesse
de l''onde de maree aux frontieres maritimes.
La valeur par defaut 999999. signifie que c''est la racine carree
du \telkey{COEFFICIENT DE CALAGE DU MARNAGE} qui est prise.""",
        ang = """Coefficient to calibrate the tidal velocities of tidal wave
at tidal open boundary conditions.
Default value 999999. means that the square root of
\telkey{COEFFICIENT TO CALIBRATE TIDAL RANGE} is taken.""",
    ),
#   -----------------------------------
    COEFFICIENT_TO_CALIBRATE_SEA_LEVEL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Coefficient pour ajuster le niveau de mer.
Ce coefficient correspond d''habitude au niveau moyen de la mer
ou une valeur proche.""",
        ang = """Coefficient to calibrate the sea level.
This coefficient usually corresponds to the mean sea level
or a close value.""",
    ),
#   -----------------------------------
    TIDAL_DATA_BASE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["NO DEFAULT VALUE","JMJ","TPXO","MISCELLANEOUS (LEGOS-NEA, FES20XX, PREVIMER...)"],
        defaut = ["NO DEFAULT VALUE"],
        fr = """Fournit le nom de la base de donnees utilisee pour la generation
automatique des conditions aux limites. Les choix possibles sont :
\begin{itemize}
\item 1 : JMJ ;
\item 2 : TPXO ;
\item 3 : divers (LEGOS-NEA, FES20XX, PREVIMER).
\end{itemize}
Pour JMJ, renseigner la localisation du fichier bdd\_jmj et
geofin dans les mots-cles \telkey{BASE ASCII DE DONNEES DE MAREE} et
\telkey{FICHIER DU MODELE DE MAREE.
Pour TPXO, LEGOS-NEA, FES20XX et PREVIMER, l''utilisateur
doit telecharger les fichiers de constantes harmoniques sur internet.""",
        ang = """Gives the name of the data base used to automatically generate
the boundary conditions. Possible choices are:
\begin{itemize}
\item 1: JMJ,
\item 2: TPXO,
\item 3: MISCELLANEOUS (LEGOS-NEA, FES20XX, PREVIMER...).
\end{itemize}
For JMJ, indicate the location of the files bdd\_jmj and geofin
with keywords \telkey{ASCII DATABASE FOR TIDE} and
\telkey{TIDAL MODEL FILE}. For TPXO, LEGOS-NEA,
FES20XX and PREVIMER, the user has to download files of harmonic
constituents on the internet.""",
    ),
#   -----------------------------------
    GEOGRAPHIC_SYSTEM = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["NO DEFAULT VALUE","DEFINED BY USER","WGS84 LONGITUDE/LATITUDE IN REAL DEGREES","WGS84 NORTHERN UTM","WGS84 SOUTHERN UTM","LAMBERT","MERCATOR PROJECTION"],
        defaut = ["NO DEFAULT VALUE"],
        fr = """Systeme de coordonnees geographiques dans lequel est construit
le modele numerique.
Indiquer la zone correspondante avec le mot-cle.
Indique le systeme de coordonnees geographiques dans lequel est
construit le modele numerique. Les choix possibles sont :
\begin{itemize}
\item 0 : defini par l''utilisateur ;
\item 1 : WGS84 longitude/latitude en degres reels ;
\item 2 : WGS84 nord UTM ;
\item 3 : WGS84 sud UTM ;
\item 4 : Lambert ;
\item 5 : projection Mercator.
\end{itemize}""",
        ang = """Geographic coordinates system in which the numerical model is
built. Indicate the corresponding zone with the keyword.
The possible choices are:
\begin{itemize}
\item 0: defined by the user,
\item 1: WGS84 longitude/latitude in real degrees,
\item 2: WGS84 Northern UTM,
\item 3: WGS84 Southern UTM,
\item 4: Lambert,
\item 5: Mercator projection.
\end{itemize}""",
    ),
#   -----------------------------------
    ZONE_NUMBER_IN_GEOGRAPHIC_SYSTEM = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["NO DEFAULT VALUE","LAMBERT 1 NORTH","LAMBERT 2 CENTER","LAMBERT 3 SOUTH","LAMBERT 4 CORSICA","LAMBERT 2 EXTENDED","UTM ZONE, E.G."],
        defaut = ["NO DEFAULT VALUE"],
        fr = """Numero de zone (fuseau ou type de projection) lors de
l''utilisation d''une projection plane. Indiquer le systeme
geographique dans lequel est construit le modele numerique avec le
mot-cle \telkey{SYSTEME GEOGRAPHIQUE}.
Les choix possibles sont :
\begin{itemize}
\item 1 : Lambert 1 nord ;
\item 2 : Lambert 2 centre ;
\item 3 : Lambert 3 sud ;
\item 4 : Lambert 4 Corse ;
\item 22 : Lambert 2 etendu ;
\item X : Valeur UTM de la zone WGS84 (X est le numero de la zone).
\end{itemize}""",
        ang = """Number of zone when using a plane projection.
Indicate the geographic system in which the numerical model is built
with the keyword \telkey{GEOGRAPHIC SYSTEM}.
Possible choices are:
\begin{itemize}
\item 1: Lambert 1 north,
\item 2: Lambert 2 center,
\item 3: Lambert 3 south,
\item 4: Lambert 4 Corsica,
\item 22: Lambert 22 extended,
\item X: UTM zone with WGS84 (X is the number of the zone).
\end{itemize}""",
    ),
#   -----------------------------------
    MINOR_CONSTITUENTS_INFERENCE = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Pour les solutions developpees par OSU (ex. TPXO) uniquement.
Interpolation de composantes harmoniques mineures
a partir de celles lues dans les fichiers d''entree
lies aux mots-cles \telkey{BASE BINAIRE 1 DE DONNEES DE MAREE}
et \telkey{BASE BINAIRE 2 DE DONNEES DE MAREE}.""",
        ang = """For tidal solutions developed by OSU (e.g. TPXO) only.
Inference of minor constituents from the ones read in input files
linked to keywords \telkey{BINARY DATABASE 1 FOR TIDE}
and \telkey{BINARY DATABASE 2 FOR TIDE}.""",
    ),
#   -----------------------------------
    MAXIMUM_NUMBER_OF_ITERATIONS_FOR_ADVECTION_SCHEMES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [10],
        fr = """Limite le nombre d''iterations pour les schemas de convection,
seulement pour schemes 13 et 14.""",
        ang = """Limits the number of solver iterations for the advection
schemes, only for schemes 13 and 14.""",
    ),
#   -----------------------------------
    NUMBER_OF_GAUSS_POINTS_FOR_WEAK_CHARACTERISTICS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["1 point","3 points","6 points"],
        defaut = ["3 points"],
        fr = """Voir les release notes v6.3.
\begin{itemize}
\item 1 : 1 point,
\item 3 : 3 points,
\item 6 : 6 points.
\end{itemize}""",
        ang = """See release notes v6.3.
\begin{itemize}
\item 1: 1 point,
\item 3: 3 points,
\item 6: 6 points.
\end{itemize}""",
    ),
#   -----------------------------------
    OPEN_BOUNDARY_CONDITIONS_ON_THE_BED = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Determine s''il y a des conditions ouvertes sur le fond.""",
        ang = """Defines if there are open boundary conditions
on the bed.""",
    ),
#   -----------------------------------
    PRESCRIBED_FLOWRATES_ON_THE_BED = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=10, max=10,
        defaut = [0.,0.,0.,0.,0.,0.,0.,0.,0.,0.],
        fr = """Fixe le debit sur les frontieres a debit impose du fond.""",
        ang = """Sets the value for flow rate on flow
rate-imposed bed boundaries.""",
    ),
#   -----------------------------------
    DURATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_TIME_STEPS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Definit le nombre de pas de temps effectues lors de
l''execution du code.""",
            ang = """Specifies the number of time steps performed when running
the code.""",
        ),
#       -----------------------------------
        TIME_STEP = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.],
            fr = """Definit le pas de temps en secondes.
Remarque : Pour une bonne precision, il est souhaitable de choisir
le pas de temps de telle sorte que le nombre de Courant de propagation
soit inferieur a 2, voire 3.
Ceci peut etre realisable en hydraulique fluviale, mais ne l''est
pratiquement jamais en hydraulique maritime ou l''on peut atteindre
des valeurs de 50.""",
            ang = """Specifies the time step in seconds.""",
        ),
#       -----------------------------------
        DURATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """Duree de la simulation en secondes. Alternative au parametre
\telkey{NOMBRE DE PAS DE TEMPS}.
On en deduit le nombre de pas de temps en prenant l''entier le
plus proche de (duree du calcul/pas de temps).
Si le \telkey{NOMBRE DE PAS DE TEMPS} est aussi donne,
on prend la plus grande valeur.""",
            ang = """Sets the duration of the simulation in seconds.
May be used instead of the parameter \telkey{NUMBER OF TIME STEPS}.
The nearest integer to (duration/time step) is taken.
If \telkey{NUMBER OF TIME STEPS} is also given,
the greater value is taken.""",
        ),
#       -----------------------------------
        FREE_SURFACE_GRADIENT_COMPATIBILITY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.],
            fr = """Des valeurs comprises entre 0 et 1 peuvent supprimer les
oscillations parasites.""",
            ang = """Values between 0 and 1 may suppress spurious oscillations.""",
        ),
    ),
#   -----------------------------------
    GENERAL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_HORIZONTAL_LEVELS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [2],
            fr = """Definit le nombre de plans du maillage entre le fond et la
surface. Vaut au moins 2.""",
            ang = """Gives the number of planes from bottom to free surface. Must
be at least 2.""",
        ),
#       -----------------------------------
        NUMBER_OF_SUB_ITERATIONS_FOR_NON_LINEARITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Permet de reactualiser, pour un meme pas de temps, les champs
convecteur et propagateur au cours de plusieurs sous-iterations. A la
premiere sous-iteration, ces champs sont donnes par $C$ et le champ de
vitesses au pas de temps precedent. Aux iterations suivantes, ils sont
pris egaux au champ de vitesse obtenu a la fin de la sous-iteration
precedente. Cette technique permet d''ameliorer la prise en compte des
non linearites.""",
            ang = """Used for updating, within one time step, the advection and
propagation fields.
Upon the first sub-iteration, these fields are given by
$C$ and the velocity field in the previous time step. At subsequent
iterations, the results of the previous sub-iteration is used to
update the advection and propagation field.
The non-linearities can be taken into account through this technique.""",
        ),
#       -----------------------------------
        COUPLING_PERIOD_FOR_TOMAWAC = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Fixe la periode de couplage avec le module \tomawac,
en nombre de pas de temps.
Par defaut, il est couple a chaque pas de temps.""",
            ang = """Sets the coupling period with the \tomawac module, in number
of time steps. By default, it is coupled at every time step.""",
        ),
#       -----------------------------------
        COUPLING_PERIOD_FOR_SISYPHE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Fixe la periode de couplage avec le module \sisyphe,
en nombre de pas de temps.
Par defaut, il est couple a chaque pas de temps.""",
            ang = """Sets the coupling period with the \sisyphe module, in number
of time steps. By default, it is coupled at every time step.""",
        ),
#       -----------------------------------
        PARTITIONING_TOOL = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            into = ['METIS','SCOTCH','PARMETIS','PTSCOTCH'],
            defaut = 'METIS',
            fr = """Choix du partitionneur :
\begin{itemize}
\item 1 : METIS ;
\item 2 : SCOTCH ;
\item 3 : PARMETIS ;
\item 4 : PTSCOTCH.
\end{itemize}""",
            ang = """Partitioning tool selection:
\begin{itemize}
\item 1: METIS,
\item 2: SCOTCH,
\item 3: PARMETIS,
\item 4: PTSCOTCH.
\end{itemize}""",
        ),
#       -----------------------------------
        ZERO = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.E-10],
            fr = """Non active pour l''instant.""",
            ang = """Not used so far.""",
        ),
#       -----------------------------------
        TIDAL_FLATS = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [True ],
            fr = """Permet de supprimer les tests sur les bancs decouvrants, dans
les cas ou l''on est certain qu''il n''y en aura pas.
En cas de doute : .TRUE.""",
            ang = """When NO, the specific treatments for tidal flats are by-passed.
This spares time, but of course you must be sure that you have no
tidal flats.""",
        ),
#       -----------------------------------
        RAIN_OR_EVAPORATION = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Pour ajouter un apport ou une perte d''eau en surface.
Voir le mot-cle \telkey{PLUIE OU EVAPORATION EN MM PAR JOUR}.""",
            ang = """Enables to add or remove water at the free surface.
See the keyword \telkey{RAIN OR EVAPORATION IN MM PER DAY}.""",
        ),
#       -----------------------------------
        HYDROSTATIC_INCONSISTENCY_FILTER = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Permet de filtrer les inconsistances hydrostatiques.""",
            ang = """Allows to filter hydrostatic inconsistencies.""",
        ),
#       -----------------------------------
        SPHERICAL_COORDINATES = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Choix des coordonnees spheriques pour la realisation du calcul
(pour les grands domaines de calcul).
Attention : cette option est etroitement liee au maillage qui doit avoir
ete saisi sur une carte marine en projection de Mercator. Il faut de
plus relever sur la carte la \telkey{LATITUDE DU POINT ORIGINE}
qui correspond dans le maillage a l''ordonnee $y$ = 0.""",
            ang = """Selection of spherical coordinates to perform the computation
(for large computation domains).
Warning: this option is closely related to the mesh that should have
been entered onto a nautical chart drawn as per Mercator projection
The \telkey{LATITUDE OF ORIGIN POINT}, which corresponds to
ordinate $y$ = 0 in the mesh, must moreover be given.""",
        ),
#       -----------------------------------
        NUMBER_OF_CULVERTS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Nombre de buses ou ponts traites comme des termes sources ou
puits. Ces buses doivent etre decrites comme des sources dans le
fichier cas. Leurs caracteristiques sont donnees dans le
\telkey{FICHIER DE DONNEES DES BUSES} (voir la documentation ecrite).""",
            ang = """Number of culverts, tubes or bridges treated as source terms.
They must be described as sources in the domain and their features
are given in the \telkey{CULVERTS DATA FILE} (see written
documentation).""",
        ),
#       -----------------------------------
        OPTION_FOR_CULVERTS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """Option pour le traitement des buses. Il existe deux formulations
dans \tel.""",
            ang = """Option for the treatment of culverts. There are two options
in \tel.""",
        ),
#       -----------------------------------
        VECTOR_LENGTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Longueur du vecteur pour les machines vectorielles.""",
            ang = """Vector length on vector machines.""",
        ),
#       -----------------------------------
        NUMBER_OF_PRIVATE_ARRAYS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Nombre de tableaux mis a disposition de l''utilisateur.""",
            ang = """Number of arrays for own user programming.""",
        ),
#       -----------------------------------
        RECORD_NUMBER_FOR_RESTART = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """En cas de suite de calcul, numero de l''enregistrement
de depart dans le fichier du calcul precedent. 0 signifie
que l''on prend le dernier enregistrement""",
            ang = """In case of \telkey{COMPUTATION CONTINUED}, record number to
start from in the \telkey{PREVIOUS COMPUTATION FILE}.
0 means that the last record is taken.""",
        ),
#       -----------------------------------
        MATRIX_STORAGE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [3],
            fr = """Permet de definir la methode de stockage des matrices.
Les choix possibles sont :
\begin{itemize}
\item 1 : EBE classique ;
\item 3 : stockage par segments.
\end{itemize}""",
            ang = """Defines the method to store matrices. The possible choices are:
\begin{itemize}
\item 1: classical EBE,
\item 3: edge-based storage.
\end{itemize}""",
        ),
#       -----------------------------------
        DENSITY_LAW = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Donne le type de loi de densite utilisee dans le cas de
l''utilisation de traceurs actifs. Les choix possibles sont :
\begin{itemize}
\item 0 : rien ;
\item 1 : fonction de temperature ;
\item 2 : fonction de la salinite ;
\item 3 : fonction de temperature et salinite ;
\item 4 : BETA donne.
\end{itemize}""",
            ang = """Gives the type of the law of density used in the case of
active tracers. The possible choices are:
\begin{itemize}
\item 0: nothing,
\item 1: function of temperature,
\item 2: function of salinity,
\item 3: function of temperature and salinity,
\item 4: function of BETA given coefficient.
\end{itemize}""",
        ),
#       -----------------------------------
        ORIGINAL_DATE_OF_TIME = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 3, max= 3,
            defaut = [1900,1,1],
            fr = """Permet de fixer la date d''origine des temps du modele lorsque
la maree est prise en compte (force generatrice de la maree et/ou les
conditions aux limites de maritimes.
Egalement utilise en chainage avec DELWAQ.""",
            ang = """Enables to set the date of the time origin of the model when
taking into account of the tide (tide generator force and/or the tidal
boundary conditions).
Also used when chaining with DELWAQ.""",
        ),
#       -----------------------------------
        ORIGINAL_HOUR_OF_TIME = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 3, max= 3,
            defaut = [0,0,0],
            fr = """Permet de fixer l''heure d''origine des temps du modele lorsque
la maree est prise en compte (force generatrice de la maree et/ou les
conditions aux limites de maritimes.
Egalement utilise en chainage avec DELWAQ.""",
            ang = """Enables to set the time of the time origin of the model when
taking into account of the tide (tide generator force and/or the tidal
boundary conditions).
Also used when chaining with DELWAQ.""",
        ),
#       -----------------------------------
        OPTION_FOR_THE_DIFFUSION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Fixe le type de resolution de l''etape de diffusion.
Les choix possibles sont :
\begin{itemize}
\item 1 : diffusion implicite ;
\item 2 : decouplee horizontale-verticale.
\end{itemize}""",
            ang = """Option to solve the diffusion step. Possible choices are:
\begin{itemize}
\item 1: implicit diffusion,
\item 2: splitting horizontal-vertical.
\end{itemize}""",
        ),
#       -----------------------------------
        MASS_LUMPING_FOR_DIFFUSION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """Mass-lumping de la matrice de masse dans la diffusion.""",
            ang = """Mass-lumping of the mass-matrix in the diffusion step.""",
        ),
#       -----------------------------------
        MESH_TRANSFORMATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Permet que specifier la methode de repartition des plans
verticaux du maillage. Les choix possibles sont :
\begin{itemize}
\item 0 : utilisateur (sous-programme \telfile{CALCOT} a programmer) ;
\item 1 : sigma ;
\item 2 : zstar ;
\item 3 : plans fixes ;
\item 5 : adaptatif.
\end{itemize}
Ce mot-clef doit etre coherent avec le sous-programme
\telfile{CONDIM}.""",
            ang = """Specifies the distribution of vertical planes of the mesh.
Possible choices are:
\begin{itemize}
\item 0: user defined (then subroutine \telfile{CALCOT} to be
implemented),
\item 1: sigma,
\item 2: zstar,
\item 3: horizontal fixed planes,
\item 5: adaptive mesh.
\end{itemize}
This keyword must comply with what is done in \telkey{CONDIM}
subroutine.""",
        ),
#       -----------------------------------
        SUPG_OPTION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 4, max= 4,
            defaut = [1,0,1,1],
            fr = """Permet de specifier le type de decentrement utilise.
Les choix possibles sont :
\begin{itemize}
\item 0 : pas de decentrement SUPG ;
\item 1 : SUPG classique ;
\item 2 : SUPG modifiee.
\end{itemize}
Ces coefficients sont respectivement appliques a :
\begin{itemize}
\item $U$, $V$ et $W$ ;
\item $H$ ;
\item $T$ ;
\item $k$ et $\epsilon$.
\end{itemize}""",
            ang = """Specifies the type of upwinding used.
Possible choices are:
\begin{itemize}
\item 0: no upwinding,
\item 1: classical SUPG,
\item 2: modified SUPG.
\end{itemize}
These coefficients are applied respectively to:
\begin{itemize}
\item 1) $U$, $V$ and $W$,
\item 2) $H$,
\item 3) $T$,
\item 4) $k$ and $\epsilon$.
\end{itemize}""",
        ),
#       -----------------------------------
        INITIAL_TIME_SET_TO_ZERO = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Remet le temps a zero en cas de suite de calcul.""",
            ang = """Initial time set to zero in case of restart.""",
        ),
#       -----------------------------------
        RAIN_OR_EVAPORATION_IN_MM_PER_DAY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """Pour ajouter un apport ou une perte d''eau en surface.""",
            ang = """Specifies the amount of water to add or remove at the
free surface.""",
        ),
#       -----------------------------------
        RATIO_BETWEEN_SKIN_FRICTION_AND_MEAN_DIAMETER = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 3.0,
            fr = """Ratio pour le calcul du frottement de peau.
rugosite de peau = ratio $\times$ diametre moyen.""",
            ang = """ Ratio for the computation of skin friction.
skin roughness = ratio $\times$ mean diameter.""",
        ),
#       -----------------------------------
        ELEMENTS_MASKED_BY_USER = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Si OUI, remplir le sous-programme \telfile{MASKOB}.""",
            ang = """If YES, fill in the subroutine \telfile{MASKOB}.""",
        ),
#       -----------------------------------
        WAVE_DRIVEN_CURRENTS = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Active la prise en compte des courants de houle
(voir le sous-programme \telfile{TRISOU}).""",
            ang = """Wave driven currents are taken into account,
see subroutine \telfile{TRISOU}.""",
        ),
#       -----------------------------------
        RECORD_NUMBER_IN_WAVE_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Numero d''enregistrement a lire par \telemac{3d} dans le
fichier des courants de houle.""",
            ang = """Record number to be read by \telemac{3d} in the wave driven
currents file.""",
        ),
#       -----------------------------------
        OPTION_FOR_TIDAL_BOUNDARY_CONDITIONS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            into = ["No tide","Real tide (recommended methodology)","Astronomical tide","Mean spring tide","Mean tide","Mean neap tide","Astronomical neap tide","Real tide (methodology before 2010)"],
            fr = """Option pour les conditions aux limites de maree.
Pour des marees reelles, l''option 1 est recommandee.
Depuis la version 7.1, ce mot-cle est un tableau avec une valeur
donnee par frontiere liquide, separee par point-virgules.
Ceci permet d''avoir des conditions de maree (ou pas) calculees
sur des frontieres liquides avec vitesses ou hauteur d''eau imposees.
Ca evite un conflit lors de l''utilisation de seuils dans le domaine.
0 est le code pour des conditions autres que des conditions de maree.
ATTENTION depuis la version 7.1 !
Les anciens modeles doivent etre changes si la frontiere de maree
n''a pas le numero 1. Dans ce cas, le mot-cle doit etre change et
plus de valeurs doivent etre donnees.
Calage possible par les mots-cles
\telkey{COEFFICIENT POUR CALAGE EN MARNAGE},
\telkey{COEFFICIENT DE CALAGE DES VITESSES DE COURANT},
et \telkey{COEFFICIENT POUR CALAGE EN NIVEAU}.
Les choix possibles sont :
\begin{itemize}
\item 0 : Pas de maree ;
\item 1 : Maree reelle (methodologie recommandee) ;
\item 2 : Maree de vive-eau exceptionnelle (coef. presque 120) ;
\item 3 : Maree de vive-eau moyenne (coef. presque 95) ;
\item 4 : Maree moyenne (coef. presque 70) ;
\item 5 : Maree de morte-eau moyenne (coef. presque 45) ;
\item 6 : Maree de morte-eau exceptionnelle (coef. presque 20) ;
\item 7 : Maree reelle (methodologie d avant 2010).
\end{itemize}""",
            ang = """Option for tidal boundary conditions.
For real tides, option 1 is recommended.
This keyword has been an array with a value given per liquid boundary,
separated by semicolons, since version 7.1.
This enables to have tidal conditions (or not) computed
on liquid boundaries with prescribed velocities or depths,
avoiding a clash when using weirs in the domain.
0 codes for conditions other than tidal.
BEWARE since version 7.1!
Old models must be changed if their tidal boundary is not number 1.
In that case this keyword must be changed and more values given.
Possible calibration with the keywords
\telkey{COEFFICIENT TO CALIBRATE TIDAL RANGE},
\telkey{COEFFICIENT TO CALIBRATE TIDAL VELOCITIES},
and \telkey{COEFFICIENT TO CALIBRATE SEA LEVEL}.
Possible choices are:
\begin{itemize}
\item 0: No tide,
\item 1: Real tide (recommended methodology),
\item 2: Astronomical tide,
\item 3: Mean spring tide,
\item 4: Mean tide,
\item 5: Mean neap tide,
\item 6: Astronomical neap tide,
\item 7: Real tide (methodology before 2010).
\end{itemize}""",
        ),
#       -----------------------------------
        TIDE_GENERATING_FORCE = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Active la prise en compte de la force generatrice de la maree""",
            ang = """The tide generating force is taken into account.""",
        ),
#       -----------------------------------
        NUMBER_OF_2D_PRIVATE_ARRAYS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Nombre de tableaux 2D mis a disposition de l utilisateur
dans le bloc PRIVE2D. Il doit etre inferieur ou egal a 4.""",
            ang = """Number of 2D arrays for own user programming
in block \telfile{PRIVE2D}. It has to be lower or equal to 4.""",
        ),
#       -----------------------------------
        LOCAL_NUMBER_OF_THE_POINT_TO_CALIBRATE_HIGH_WATER = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Numero local du point entre 1 et le nombre de points
de frontiere maritime (du FICHIER DES CONSTANTES HARMONIQUES)
ou les conditions aux limites de maree sont calculees
avec les bases de donnees JMJ, NEA, FES, PREVIMER
(sauf les bases de type TPXO).
Les ondes de maree sont dephasees par rapport a ce point
pour debuter le calcul par une pleine mer
(en marees schematiques seulement).""",
            ang = """Local number between 1 and the number of tidal boundary points
(of the \telkey{HARMONIC CONSTANTS FILE}) where the tidal boundary
conditions are computed with JMJ, NEA, FES, PREVIMER databases
(except TPXO-type databases).
The tidal constituents have their phase shifted with respect to
this point to start the simulation with a high water
(for schematic tides only).""",
        ),
#       -----------------------------------
        GLOBAL_NUMBER_OF_THE_POINT_TO_CALIBRATE_HIGH_WATER = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Numero global du point par rapport auquel
les ondes de maree sont dephasees
pour debuter le calcul par une pleine mer
(en marees schematiques seulement).
Ne concerne que les bases de constantes harmoniques de type TPXO.""",
            ang = """Global number of the point with respect to which
the tidal constituents have their phase shifted
to start the calculation with a high water
(for schematic tides only).
Only harmonic constants databases like TPXO are concerned.""",
        ),
#       -----------------------------------
        NUMBER_OF_CORRECTIONS_OF_DISTRIBUTIVE_SCHEMES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Pour les options avec predicteur-correcteur.""",
            ang = """For predictor-corrector options.""",
        ),
#       -----------------------------------
        NUMBER_OF_SUB_STEPS_OF_DISTRIBUTIVE_SCHEMES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Pour les options predicteur-correcteur
avec schema localement implicite.""",
            ang = """Only for implicit scheme with predictor-corrector.""",
        ),
#       -----------------------------------
        CONTINUITY_CORRECTION_ON_OPEN_BOUNDARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """Modifie les vitesses libres sur les frontieres ouvertes
pour avoir un meilleur champ a divergence nulle.""",
            ang = """Changes the free velocities on open boundaries to get
a better divergence-free field.""",
        ),
    ),
#   -----------------------------------
    VELOCITY_CELERITY_DEPTH = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        INITIAL_GUESS_FOR_DEPTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["zero","previous","extrapolation"],
            defaut = ["previous"],
            fr = """Tir initial du solveur de l''etape de propagation.
Offre la possibilite de modifier la valeur initiale de $\delta h$,
accroissement de $h$, a chaque iteration,
dans l''etape de propagation en utilisant les valeurs
finales de cette variable aux pas de temps precedents. Ceci peut
permettre d''accelerer la vitesse de convergence lors de la resolution
du systeme. Trois possibilites sont offertes :
\begin{itemize}
\item 0 : $\delta h$ = 0,
\item 1 : $\delta h$ = $\delta h_n$ (valeur finale de $\delta h$
 au pas de temps precedent),
\item 2 : $\delta h$ = 2 $\delta h_n$ - $\delta h_{n-1}$
(extrapolation).
\end{itemize}""",
            ang = """Initial guess for the solver in the propagation step.
Makes it possible to modify the initial value of $\delta h$, upon each
iteration in the propagation step, by using the ultimate values this
variable had in the earlier time steps. Thus, the convergence can be
speeded up when the system is being solved. 3 options are available:
\begin{itemize}
\item 0: $\delta h$ = 0,
\item 1: $\delta h$ = $\delta h_n$  (ultimate $\delta h$ value
in the next previous time step),
\item 2: $\delta h$ = 2 $\delta h_n$ - $\delta h_{n-1}$ (extrapolation).
\end{itemize}""",
        ),
#       -----------------------------------
        MINIMAL_VALUE_FOR_DEPTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [-1000.],
            fr = """Fixe la valeur minimale de $H$.""",
            ang = """Sets the minimum water depth value $H$.""",
        ),
#       -----------------------------------
        IMPLICITATION_FOR_DEPTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.55],
            fr = """Fixe la valeur du coefficient d''implicitation sur la hauteur
d''eau dans l''etape de propagation (cf. Note de principe).
Les valeurs inferieures a 0.5 donnent un schema instable.""",
            ang = """Sets the value of the implicitation coefficient for water
depth in the propagation step (cf. Principe note).
The values lower than 0.5 give an instable scheme.""",
        ),
#       -----------------------------------
        IMPLICITATION_FOR_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.],
            fr = """Fixe la valeur du coefficient d''implicitation sur la vitesse
dans l''etape de propagation (cf.  Note de principe).
Les valeurs inferieures a 0.5 donnent un schema instable.""",
            ang = """Sets the value of the implicitation coefficient
for the velocity
in the propagation step (cf. Principe note).
The values lower than 0.5 give an instable scheme.""",
        ),
#       -----------------------------------
        MASS_LUMPING_FOR_DEPTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """\telemac{3d} offre la possibilite d''effectuer du
mass-lumping sur $H$ ou $U$.
Ceci revient a ramener tout ou partie (suivant la valeur de ce
coefficient) des matrices \telfile{AM1 (H)} ou \telfile{AM2 (U)}
et \telfile{AM3 (V)} sur leur diagonale.
Cette technique permet d''accelerer le code dans des proportions tres
importantes et de le rendre egalement beaucoup plus stable. Cependant
les solutions obtenues se trouvent lissees.
Ce parametre fixe le taux de mass-lumping effectue sur $H$.
Utilisation deconseillee.""",
            ang = """\telemac{3d} offers the posibility to perform mass-lumping
on $H$ or $U$.
This gathers all or part (given the value of the coefficient)
of the \telfile{AM1(H)} or \telfile{AM2(Ut)} and \telfile{AM3(V)}
matrices on their diagonal.
This technique can speed-up the code a lot and also render it
more stable.
Yet, the solutions are smoothened.
This parameter sets the mass-lumping amount done for $H$.
Not recommended for use.""",
        ),
#       -----------------------------------
        MASS_LUMPING_FOR_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """Fixe le taux de mass-lumping effectue sur la vitesse.
Utilisation deconseillee.""",
            ang = """Sets the amount of mass-lumping that is performed on
the velocity. Not recommended for use.""",
        ),
    ),
#   -----------------------------------
    K_EPSILON_MODEL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_DIFFUSION_OF_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [200],
            fr = """Fixe le nombre maximum d''iterations accepte lors de la
resolution du systeme diffusion-termes sources du modele
$k$-$\epsilon$.""",
            ang = """Limits the number of solver iterations for the diffusion of
$k$-$\epsilon$.""",
        ),
    ),
)
# -----------------------------------------------------------------------
EQUATIONS,_TRACER = PROC(nom= "EQUATIONS,_TRACER",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NUMBER_OF_TRACERS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Definit le nombre de traceurs.""",
        ang = """Defines the number of tracers.""",
    ),
#   -----------------------------------
    STANDARD_VALUES_FOR_TRACERS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """Valeur du traceur pour laquelle la densite est donnee.""",
        ang = """Reference value of tracers corresponding to the given density.""",
    ),
)
# -----------------------------------------------------------------------
EQUATIONS = PROC(nom= "EQUATIONS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NON_HYDROSTATIC_VERSION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Permet de specifier s''il y a utilisation ou non de la version
non-hydrostatique.""",
        ang = """Specifies the use of the non-hydrostatic code version or not.""",
    ),
#   -----------------------------------
    2D_CONTINUATION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Permet d''utiliser un \telkey{FICHIER DES RESULTATS 2D}
stocke dans le \telkey{FICHIER POUR SUITE 2D} comme fichier de
conditions initiales.""",
        ang = """Enables to use a \telkey{2D RESULT FILE} in
\telkey{FILE FOR 2D CONTINUATION} as initial conditions file.""",
    ),
#   -----------------------------------
    DYNAMIC_PRESSURE_IN_WAVE_EQUATION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Definit si une estimation du gradient de pression dynamique
est prise en compte dans l''equation d''onde.""",
        ang = """Defines if an estimated pressure gradient is taken into
account in the wave equation.""",
    ),
#   -----------------------------------
    DYNAMIC_BOUNDARY_CONDITION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Si OUI, impose une vitesse en surface
selon la condition a la limite dynamique.""",
        ang = """If YES, will set at the free surface a
velocity obeying the dynamic boundary condition.""",
    ),
#   -----------------------------------
    GENERAL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        PRECONDITIONING_FOR_VERTICAL_VELOCITY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["no preconditioning","diagonal","diagonal condensed","diagonal with absolute values","Crout","Gauss-Seidel EBE","Matrix defined by the user","diagonal and Crout","direct solver on the vertical","diagonal condensed and Crout","diagonal and direct solver on the vertical"],
            defaut = ["diagonal"],
            fr = """Permet de preconditionner le systeme relatif
au calcul de la vitesse verticale. Les choix possibles sont :
\begin{itemize}
\item 0 : aucun ;
\item 2 : diagonal ;
\item 3 : diagonal avec matrice condensee ;
\item 5 : diagonal avec valeurs absolues ;
\item 7 : Crout ;
\item 11 : Gauss-Seidel EBE ;
\item 13 : matrice fournie par l''utilisateur ;
\item 14 : diagonal et Crout ;
\item 17 : solveur direct sur la verticale ;
\item 21 : diagonal condensee et Crout ;
\item 34 : diagonal et solveur direct sur la verticale.
\end{itemize}""",
            ang = """Choice of preconditioning for the diffusion of
vertical velocity. Possible choices are:
\begin{itemize}
\item 0: no preconditioning,
\item 2: diagonal,
\item 3: diagonal with the condensed matrix,
\item 5: diagonal with absolute values,
\item 7: Crout,
\item 11: Gauss-Seidel EBE,
\item 13: matrix defined by the user,
\item 14: diagonal and Crout,
\item 17: direct solver on the vertical,
\item 21: diagonal condensed and Crout,
\item 34: diagonal and direct solver on the vertical.
\end{itemize}""",
        ),
#       -----------------------------------
        SOLVER_FOR_VERTICAL_VELOCITY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","cgstab","gmres","direct solver"],
            defaut = ["conjugate gradient"],
            fr = """Permet de choisir le solveur utilise pour le calcul de
la vitesse verticale $W$. Les choix possibles sont :
\begin{itemize}
\item 1 : gradient conjugue ;
\item 2 : residu conjugue ;
\item 3 : gradient conjugue sur equation normale ;
\item 4 : erreur minimale ;
\item 5 : gradient conjugue carre ;
\item 6 : CGSTAB ;
\item 7 : GMRES ;
\item 8 : solveur direct.
\end{itemize}""",
            ang = """Choice of the solver for the diffusion of vertical velocity
$W$. Possible choices are:
\begin{itemize}
\item 1: conjugate gradient,
\item 2: conjugate residual,
\item 3: conjugate gradient on a normal equation,
\item 4: minimum error,
\item 5: squared conjugate gradient,
\item 6: CGSTAB,
\item 7: GMRES,
\item 8: direct solver.
\end{itemize}""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_VERTICAL_VELOCITY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [100],
            fr = """Fixe le nombre maximum d''iterations accepte lors de la
resolution du calcul de la vitesse verticale.""",
            ang = """Limits the number of solver iterations for the diffusion of
 vertical velocity.""",
        ),
#       -----------------------------------
        PARALLEL_PROCESSORS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Nombre de processeurs pour la decomposition en parallele.
La valeur 0 correspond a un calcul scalaire.""",
            ang = """Number of processors for domain partition.
Value 0 corresponds to a scalar computation.""",
        ),
#       -----------------------------------
        TREATMENT_ON_TIDAL_FLATS_FOR_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["FORCED TO ZERO","VALUE BEFORE MASKED"],
            defaut = ["FORCED TO ZERO"],
            fr = """Traitement sur les bancs decouvrants a l''etape de diffusion.
\begin{itemize}
\item 0 : forcage a zero ;
\item 1 : valeur avant masquage.
\end{itemize}""",
            ang = """Treatment of tidal flats at the diffusion step for velocities.
\begin{itemize}
\item 0: forced to zero,
\item 1: value before masked.
\end{itemize}""",
        ),
#       -----------------------------------
        TREATMENT_ON_TIDAL_FLATS_FOR_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["FORCED TO ZERO","VALUE BEFORE MASKED"],
            defaut = ["FORCED TO ZERO"],
            fr = """Traitement sur les bancs decouvrants a l''etape de diffusion.
\begin{itemize}
\item 0 : forcage a zero ;
\item 1 : valeur avant masquage.
\end{itemize}""",
            ang = """Treatment of tidal flats at the diffusion step for tracers.
\begin{itemize}
\item 0: forced to zero,
\item 1: value before masked.
\end{itemize}""",
        ),
#       -----------------------------------
        BOUNDARY_CONDITION_ON_THE_BOTTOM = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["LOG LAW FOR VELOCITIES ON BOTTOM","NO SLIP FOR VELOCITIES ON BOTTOM"],
            defaut = ["LOG LAW FOR VELOCITIES ON BOTTOM"],
            fr = """Specifie le type de conditions aux limites au fond.
Les choix possibles sont :
\item 1 : conditions de Neumann pour les vitesses au fond;
\item 2 : vitesses nulles au fond. Va de pair logiquement avec
 un bon raffinement du maillage au fond""",
            ang = """Specifies the type of boundary conditions on the bottom
layer. Possible choices are:
\begin{itemize}
\item 1: Neumann conditions on velocity on bottom,
\item 2: velocities will be set to 0. Should be linked to
a refined mesh near the bottom.
\end{itemize}""",
        ),
#       -----------------------------------
        TREATMENT_ON_TIDAL_FLATS_FOR_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["FORCED TO ZERO","VALUE BEFORE MASKED"],
            defaut = ["FORCED TO ZERO"],
            fr = """Traitement sur les bancs decouvrants a l''etape de diffusion
pour $k$ et $\epsilon$.
\begin{itemize}
\item 0 : forcage a zero ;
\item 1 : valeur avant masquage.
\end{itemize}""",
            ang = """Treatment of tidal flats at the diffusion step for $k$ and
$\epsilon$.
\begin{itemize}
\item 0: forced to zero,
\item 1: value before masked.
\end{itemize}""",
        ),
#       -----------------------------------
        ACCURACY_FOR_VERTICAL_VELOCITY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.E-6],
            fr = """Fixe la precision demandee pour le calcul de la vitesse
verticale.""",
            ang = """Sets the accuracy needed for the computation
of the vertical velocity.""",
        ),
#       -----------------------------------
        SPATIAL_PROJECTION_TYPE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["CARTESIAN, NOT GEOREFERENCED","MERCATOR","LATITUDE LONGITUDE"],
            defaut = ["MERCATOR"],
            fr = """Permet de specifier le type de projection spatiale utilisee dans
le cas de l''utilisation des coordonnees spheriques par exemple.
Les choix possibles sont :
\begin{itemize}
\item 1 : Lambert Cartesien non georeference ;
\item 2 : Mercator ;
\item 3 : Latitude/longitude (exprimees en degres).
\end{itemize}
Option 2 ou 3 obligatoire pour les coordonnees spheriques.
Option 3 : latitude et longitude en degres !
Dans le cas de l''option 3, \telemac{3d} convertit les informations
latitude/longitude a l''aide de la projection de Mercator.""",
            ang = """Specifies the type of spatial projection used
(for example when using spherical coordinates).
Possible choices are:
\begin{itemize}
\item 1: Cartesian, not georeferenced,
\item 2: Mercator,
\item 3: latitude/longitude (in degrees).
\end{itemize}
Option 2 or 3 mandatory for spherical coordinates. Option 3: latitude
and longitude in degrees! When using option 3, the coordinates are
automatically
treated by \telemac{3d} using Mercator projection.""",
        ),
#       -----------------------------------
        LATITUDE_OF_ORIGIN_POINT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """Donne la valeur de la latitude du point origine du maillage
(pour la projection de Mercator, voir le mot cle
\telkey{SYSTEME GEOGRAPHIQUE}).""",
            ang = """Gives the value of the latitude of the origin point of the
mesh (for the Mercator projection, see the keyword
\telkey{GEOGRAPHIC SYSTEM}).""",
        ),
#       -----------------------------------
        LONGITUDE_OF_ORIGIN_POINT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """Donne la valeur de la longitude du point origine du maillage
(pour la projection de Mercator, voir le mot cle
\telkey{SYSTEME GEOGRAPHIQUE}).""",
            ang = """Gives the value of the longitude of the origin point of the
mesh (for the Mercator projection, see the keyword
\telkey{GEOGRAPHIC SYSTEM}).""",
        ),
#       -----------------------------------
        NORTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """Angle que fait le nord,
dans le sens trigonometrique, avec L, axe Oy.""",
            ang = """Angle of North, counted counter-clockwise, with Oy.""",
        ),
    ),
#   -----------------------------------
    TURBULENCE_MODEL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        MIXING_LENGTH_MODEL = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Permet de specifier le modele de longueur utilise pour la
turbulence verticale. Les choix possibles sont :
\begin{itemize}
\item 1: Prandtl ;
\item 3: Nezu et Nakawaga ;
\item 5: Quetin ;
\item 6: Tsanis.
\end{itemize}
4 (jet) a ete supprime.""",
            ang = """Specifies the mixing length model used for vertical turbulence.
Possible choices are:
\begin{itemize}
\item 1: Prandtl,
\item 3: Nezu and Nakawaga,
\item 5: Quetin,
\item 6: TsaniS.
\end{itemize}
4 (jet) has been suppressed.""",
        ),
#       -----------------------------------
        VERTICAL_TURBULENCE_MODEL = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["CONSTANT VISCOSITY","MIXING LENGTH","K-EPSILON MODEL","SMAGORINSKI",,,"K-OMEGA MODEL"],
            defaut = ["CONSTANT VISCOSITY"],
            fr = """Permet de specifier le modele de turbulence horizontal.
Les choix possibles sont :
\begin{itemize}
\item 1 : viscosite constante ;
\item 2 : longueur de melange ;
\item 3 : modele $k$-$\epsilon$ ;
\item 4 : Smagorinski ;
\item 7 : $k$-$\omega$ model.
\end{itemize}
Attention : si on choisit l''option 1, il ne faut pas oublier d''ajuster
la valeur du mot cle \telkey{COEFFICIENT DE DIFFUSION}\ldots
Si on choisit les autres options, ce meme parametre doit retrouver sa
vraie valeur physique car elle est utilisee comme telle dans le modele
de turbulence.
Si on choisit l''option 3 ou 7, ce meme parametre doit retrouver sa
vraie valeur physique, en general environ $10^{-6}$
car elle est utilisee comme telle dans le modele de turbulence.""",
            ang = """Specifies the horizontal turbulence model.
The available choices are:
\begin{itemize}
\item 1: constant viscosity,
\item 2: mixing length,
\item 3: $k$-$\epsilon$ model,
\item 4: Smagorinski,
\item 7: $k$-$\omega$ model.
\end{itemize}
Caution: if option 1 is chosen, give the right
\telkey{COEFFICIENT FOR\ldots\ DIFFUSION OF VELOCITIES}\ldots\
If option 3 ou 7 is chosen, this parameter must get its real physical
value of molecular diffusivity, generally about $10^{-6}$
because it is used as well in the turbulence model.""",
        ),
#       -----------------------------------
        HORIZONTAL_TURBULENCE_MODEL = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["CONSTANT VISCOSITY","K-EPSILON MODEL","SMAGORINSKI",,,"K-OMEGA MODEL"],
            defaut = ["CONSTANT VISCOSITY"],
            fr = """Permet de specifier le modele de turbulence horizontal.
Les choix possibles sont :
\begin{itemize}
\item 1 : viscosite constante ;
\item 3 : modele $k$-$\epsilon$ ;
\item 4 : Smagorinski ;
\item 7 : $k$-$\omega$ model.
\end{itemize}
Attention : si on choisit l''option 1, il ne faut pas oublier d''ajuster
la valeur du mot cle \telkey{COEFFICIENT DE DIFFUSION}\ldots
Si on choisit les autres options, ce meme parametre doit retrouver sa
vraie valeur physique car elle est utilisee comme telle dans le modele
de turbulence.
Si on choisit l''option 3 ou 7, ce meme parametre doit retrouver sa
vraie valeur physique, en general environ $10^{-6}$
car elle est utilisee comme telle dans le modele de turbulence.""",
            ang = """Specifies the horizontal turbulence model.
The available choices are:
\begin{itemize}
\item 1: constant viscosity,
\item 3: $k$-$\epsilon$ model,
\item 4: Smagorinski,
\item 7: $k$-$\omega$ model.
\end{itemize}
Caution: if option 1 is chosen, give the right
\telkey{COEFFICIENT FOR\ldots\ DIFFUSION OF VELOCITIES}\ldots\
If option 3 ou 7 is chosen, this parameter must get its real physical
value of molecular diffusivity, generally about $10^{-6}$
because it is used as well in the turbulence model.""",
        ),
#       -----------------------------------
        K_EPSILON_MODEL = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            TURBULENCE_REGIME_FOR_THE_BOTTOM = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM', min=0, max='**',
                into = ["smooth","rough","rough compatibility with old versions"],
                defaut = ["rough"],
                fr = """Permet de definir le regime de turbulence pour le fond dans le
cas du modele de longueur de melange ou du modele $k$-$\epsilon$ :
\begin{itemize}
\item 1 : lisse ;
\item 2 : rugueux ;
\item 3 : rugueux (compatibilite avec anciennes versions).
\end{itemize}""",
                ang = """Defines the turbulence regime for the bottom in the case of a
$k$-$\epsilon$ or mixing-length model:
\begin{itemize}
\item 1: smooth,
\item 2: rough,
\item 3: rough also (for compatibility with old versions).
\end{itemize}""",
            ),
#           -----------------------------------
            TURBULENCE_REGIME_FOR_LATERAL_SOLID_BOUNDARIES = SIMP(statut ='f',
#           -----------------------------------
                typ = 'TXM', min=0, max='**',
                into = ["smooth","rough"],
                defaut = ["rough"],
                fr = """Definit le regime de turbulence pour les parois laterales :
\begin{itemize}
\item 1 : lisse ;
\item 2 : rugueux.
\end{itemize}""",
                ang = """Defines the turbulence regime for the lateral boundaries:
\begin{itemize}
\item 1: smooth,
\item 2: rough.
\end{itemize}""",
            ),
        ),
#       -----------------------------------
        CONSTANT_VISCOSITY = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            COEFFICIENT_FOR_HORIZONTAL_DIFFUSION_OF_VELOCITIES = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.E-6],
                fr = """Fixe de facon uniforme pour l''ensemble du domaine;
la valeur du coefficient de diffusion de viscosite globale (dynamique +
turbulente). Cette valeur peut avoir une influence non negligeable sur
la forme et la taille des recirculations.""",
                ang = """Sets, in an even way for the whole domain, the value of the
coefficient of global (dynamic+turbulent) viscosity
for the horizontal direction. This value may
have a significant effect both on the shapes and sizes of
recirculation zones.""",
            ),
#           -----------------------------------
            COEFFICIENT_FOR_VERTICAL_DIFFUSION_OF_VELOCITIES = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.E-6],
                fr = """Fixe de facon uniforme pour l''ensemble du domaine;
la valeur du coefficient de diffusion de viscosite globale (dynamique +
turbulente). Cette valeur peut avoir une influence non negligeable sur
la forme et la taille des recirculations.""",
                ang = """Sets, in an even way for the whole domain, the value of the
coefficient of global (dynamic+turbulent) viscosity
for the horizontal direction. This value may
have a significant effect both on the shapes and sizes of
recirculation zones.""",
            ),
#           -----------------------------------
            COEFFICIENT_FOR_HORIZONTAL_DIFFUSION_OF_TRACERS = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min= 2, max= 2,
                fr = """Fixe les valeurs de coefficients de diffusion horizontal des
traceurs.  L''influence de ce parametre sur l''evolution des traceurs
dans le temps est importante.
C est un tableau depuis la version 7.1, avec une valeur par traceur,
separation par un point virgule.""",
                ang = """Sets the values of the horizontal diffusion of tracers.
These values may have a significant effect on the evolution of
tracers in time.
Since version 7.1, it has been an array, with one value per tracer,
separated by semicolons.""",
            ),
#           -----------------------------------
            COEFFICIENT_FOR_VERTICAL_DIFFUSION_OF_TRACERS = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min= 2, max= 2,
                fr = """Fixe les valeurs de coefficients de diffusion vertical des
traceurs.  L''influence de ce parametre sur l''evolution des traceurs
dans le temps est importante.
C est un tableau depuis la version 7.1, avec une valeur par traceur,
separation par un point virgule.""",
                ang = """Sets the values of the vertical diffusion of tracers.
These values may have a significant effect on the evolution of
tracers in time.
Since version 7.1, it has been an array, with one value per tracer,
separated by semicolons.""",
            ),
#           -----------------------------------
            ACCURACY_FOR_DIFFUSION_OF_SEDIMENT = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.E-6],
                fr = """Fixe la precision demandee pour le calcul de la diffusion
des sediments.""",
                ang = """Sets the accuracy needed for the computation of the
diffusion of sediments.""",
            ),
#           -----------------------------------
            ACCURACY_FOR_DIFFUSION_OF_VELOCITIES = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.E-5],
                fr = """Fixe la precision demandee pour le calcul de la diffusion
de la vitesse.""",
                ang = """Sets the accuracy needed for the computation of the
diffusion of the velocities.""",
            ),
#           -----------------------------------
            ACCURACY_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.E-6],
                fr = """Fixe la precision demandee pour le calcul de la diffusion
des traceurs.""",
                ang = """Sets the accuracy needed for the computation of
the diffusion of the tracers.""",
            ),
#           -----------------------------------
            ACCURACY_FOR_DIFFUSION_OF_K_EPSILON = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.E-6],
                fr = """Fixe la precision demandee pour le calcul de la diffusion
du $k$-$\epsilon$.""",
                ang = """Sets the accuracy needed for the computation of the
diffusion of the $k$-$\epsilon$ model.""",
            ),
#           -----------------------------------
            ACCURACY_FOR_PPE = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [1.E-4],
                fr = """Fixe la precision pour l''equation de Poisson.""",
                ang = """Sets the precision needed for the computation of the Poisson
Pressure Equation.""",
            ),
        ),
    ),
#   -----------------------------------
    SMOOTHINGS = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        NUMBER_OF_BOTTOM_SMOOTHINGS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """Nombre de lissages effectues sur la topographie.
Chaque lissage, effectue a l''aide d''une matrice de masse,
est conservatif.
Utilise lorsque les donnees de bathymetrie donnent des resultats
trop irreguliers apres interpolation.""",
            ang = """Number of smoothings on bottom topography.
Each smoothing is mass conservative.
To be used when interpolation of bathymetry on the mesh gives
very rough results.""",
        ),
    ),
#   -----------------------------------
    FRICTION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        LAW_OF_FRICTION_ON_LATERAL_BOUNDARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["NO FRICTION","COEFFICIENT TAKEN IN BOUNDARY CONDITIONS FILE","IDEM","IDEM","IDEM","NIKURADSE"],
            defaut = ["NO FRICTION"],
            fr = """Selectionne le type de formulation utilisee pour le calcul
du frottement sur les parois laterales.
Les lois possibles sont les suivantes (cf. Note de principe) :
\begin{itemize}
\item 0 : pas de frottement, ou \telfile{AUBOR} donne par le
\telkey{FICHIER DES CONDITIONS AUX LIMITES} ;
\item 5 : formule de Nikuradse.
\end{itemize}""",
            ang = """Selects the type of formulation used for the friction on
lateral boundaries. The possible laws are as follows (refer to the
Principle note):
\begin{itemize}
\item 0: no friction, or \telfile{AUBOR} given by the
\telkey{BOUNDARY CONDITION FILE},
\item 5: Nikuradse''s formula.
\end{itemize}""",
        ),
#       -----------------------------------
        FRICTION_COEFFICIENT_FOR_THE_BOTTOM = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [60.],
            fr = """Fixe la valeur du coefficient de frottement au fond,
si constant.""",
            ang = """Friction coefficient on the bottom, if constant.""",
        ),
#       -----------------------------------
        FRICTION_COEFFICIENT_FOR_LATERAL_SOLID_BOUNDARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [60.],
            fr = """Fixe la valeur du coefficient de frottement sur les parois,
si constant.""",
            ang = """Friction coefficient on the lateral boundaries, if constant.""",
        ),
#       -----------------------------------
        LAW_OF_BOTTOM_FRICTION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["NO FRICTION","HAALAND","CHEZY","STRICKLER","MANNING","NIKURADSE"],
            defaut = ["CHEZY"],
            fr = """Selectionne le type de formulation utilisee pour le calcul
du frottement sur le fond.
Les lois possibles sont les suivantes (cf. Note de principe) :
\begin{itemize}
\item 0 : pas de frottement sur le fond ;
\item 1 : formule de Haaland ;
\item 2 : formule de Chezy ;
\item 3 : formule de Strickler ;
\item 4 : formule de Manning ;
\item 5 : formule de Nikuradse.
\end{itemize}""",
            ang = """Selects the type of formulation used for the bottom friction.
The possible laws are as follows (refer to the Principle note):
\begin{itemize}
\item 0: no friction against bottom,
\item 1: Haaland''s formula,
\item 2: Chezy''s formula,
\item 3: Strickler''s formula,
\item 4: Manning''s formula,
\item 5: Nikuradse''s formula.
\end{itemize}""",
        ),
    ),
#   -----------------------------------
    PROPAGATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        PRECONDITIONING_FOR_PROPAGATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["no preconditioning","diagonal","diagonal condensed","diagonal with absolute values","Crout","Gauss-Seidel EBE","Matrix defined by the user","diagonal and Crout","direct solver on the vertical","diagonal condensed and Crout","diagonal and direct solver on the vertical"],
            defaut = ["diagonal"],
            fr = """Permet de preconditionner le systeme de l''etape de propagation
afin d''accelerer la convergence lors de sa resolution. Les choix
possibles sont :
\begin{itemize}
\item 0 : aucun ;
\item 2 : diagonal ;
\item 3 : diagonal avec matrice condensee ;
\item 5 : diagonal avec valeurs absolues ;
\item 7 : Crout ;
\item 11 : Gauss-Seidel EBE ;
\item 13 : matrice fournie par l''utilisateur ;
\item 14 : diagonal et Crout ;
\item 17 : solveur direct sur la verticale ;
\item 21 : diagonal condensee et Crout ;
\item 34 : diagonal et solveur direct sur la verticale.
\end{itemize}
Certains preconditionnements sont cumulables
(les diagonaux 2 ou 3 avec les autres).
Pour cette raison on ne retient que les nombres premiers pour
designer les preconditionnements. Si l''on souhaite en cumuler
plusieurs on formera le produit des options correspondantes.""",
            ang = """Choice of the preconditioning in the propagation step linear
system that the convergence is speeded up when it is being solved.
Possible choices are:
\begin{itemize}
\item 0: no preconditioning,
\item 2: diagonal,
\item 3: diagonal with the condensed matrix,
\item 5: diagonal with absolute values,
\item 7: Crout,
\item 11: Gauss-Seidel EBE,
\item 13: matrix defined by the user,
\item 14: diagonal and Crout,
\item 17: direct solver on the vertical,
\item 21: diagonal condensed and Crout,
\item 34: diagonal and direct solver on the vertical.
\end{itemize}
Some operations (either 2 or 3 diagonal preconditioning) can be
performed concurrently with the others.
Only prime numbers are therefore kept to denote the preconditioning
operations. When several of them are to be performed concurrently,
the product of relevant options shall be done.""",
        ),
#       -----------------------------------
        SOLVER_FOR_PROPAGATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","cgstab","gmres","direct solver"],
            defaut = ["conjugate gradient"],
            fr = """Permet de choisir le solveur utilise pour la resolution de
l''etape de propagation.
Les choix possibles sont :
\begin{itemize}
\item 1 : gradient conjugue ;
\item 2 : residu conjugue ;
\item 3 : gradient conjugue sur equation normale ;
\item 4 : erreur minimale ;
\item 5 : gradient conjugue carre ;
\item 6 : CGSTAB ;
\item 7 : GMRES ;
\item 8 : solveur direct.
\end{itemize}""",
            ang = """Choice of the solver for the propagation equation.
Possible choices are:
\begin{itemize}
\item 1: conjugate gradient,
\item 2: conjugate residual,
\item 3: conjugate gradient on a normal equation,
\item 4: minimum error,
\item 5: squared conjugate gradient,
\item 6: CGSTAB,
\item 7: GMRES,
\item 8: direct solver.
\end{itemize}""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_PROPAGATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [200],
            fr = """Les algorithmes utilises pour la resolution de l''etape de
propagation etant iteratifs; il est necessaire de limiter le nombre
d''iterations autorisees.
Remarque : un maximum de 40 iterations par pas de temps semble
raisonnable.""",
            ang = """Since the algorithms used for solving the propagation step are
iterative, the allowed number of iterations should be limited.
NOTE: a maximum number of 40 iterations per time step seems to be
reasonable.""",
        ),
#       -----------------------------------
        ACCURACY_FOR_PROPAGATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.E-6],
            fr = """Fixe la precision demandee pour l''etape de propagation.""",
            ang = """Sets the accuracy needed for the computation
of the propagation step.""",
        ),
#       -----------------------------------
        LINEARIZED_PROPAGATION = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Permet de lineariser l''etape de propagation,
par exemple lors de la realisation de cas tests pour lesquels on dispose
d''une solution analytique dans le cas linearise.""",
            ang = """Provided for linearizing the propagation step, e.g. when
performing test-cases for which an analytical solution in the linearized
case is available.""",
        ),
    ),
#   -----------------------------------
    WIND_INFO = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        WIND = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Prise en compte ou non des effets du vent.""",
            ang = """Determines whether the wind effects are to be taken into
account or not.""",
        ),
#       -----------------------------------
        NUMERICS_VALUES = FACT(statut='o',
#       -----------------------------------
#           -----------------------------------
            COEFFICIENT_OF_WIND_INFLUENCE = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.],
                fr = """Fixe la valeur du coefficient d''entrainement du vent.
Voir le manuel utilisateur pour la valeur a donner.""",
                ang = """Sets the value of the wind driving coefficient.
See the User Manual for the value to give.""",
            ),
#           -----------------------------------
            WIND_VELOCITY_ALONG_X = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.],
                fr = """Composante de la vitesse du vent suivant
l''axe des $x$ (m/s), si constante.""",
                ang = """Wind velocity, component along $x$ axis (m/s), if constant.""",
            ),
#           -----------------------------------
            WIND_VELOCITY_ALONG_Y = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R', min=0, max='**',
                defaut = [0.],
                fr = """Composante de la vitesse du vent suivant
l''axe des $y$ (m/s), si constante.""",
                ang = """Wind velocity, component along $y$ axis (m/s), if constant.""",
            ),
#           -----------------------------------
            THRESHOLD_DEPTH_FOR_WIND = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R',
                defaut = 1.,
                fr = """Retire la force due au vent dans les petites profondeurs""",
                ang = """Wind is not taken into account for depths smaller
than this value.""",
            ),
        ),
    ),
#   -----------------------------------
    DEPTH = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        MEAN_DEPTH_FOR_LINEARIZATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """Fixe la hauteur d''eau autour de laquelle s''effectue la
linearisation lorsque l''option \telkey{PROPAGATION LINEARISEE} est
choisie.""",
            ang = """Sets the water depth about which the linearization is done when
the \telkey{LINEARIZED PROPAGATION} option is selected.""",
        ),
    ),
#   -----------------------------------
    CORIOLIS_INFO = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        CORIOLIS = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Prise en compte ou non de la force de Coriolis.""",
            ang = """The Coriolis force is taken into account or ignored.""",
        ),
    ),
#   -----------------------------------
    AIR_PRESSURE_INFO = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        AIR_PRESSURE = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Permet de decider si l''on prend ou non en compte l''influence
d''un champ de pression.""",
            ang = """Sets whether the influence of an atmosphere
pressure field is taken into account or not.""",
        ),
    ),
#   -----------------------------------
    SOURCES = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        VELOCITIES_OF_THE_SOURCES_ALONG_Z = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Permet de specifier la composante selon $z$ de la vitesse aux
sources. Si rien n''est specifie, les sources diffusent sans vitesse
dans toutes les directions (cf. cas de validation source).""",
            ang = """Specifies the compoment along $z$ of the velocities of the
sources. If nothing is specified, the sources diffuse without any
velocity in every direction (cf. validation case source).""",
        ),
#       -----------------------------------
        ABSCISSAE_OF_SOURCES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Nombres reels donnant les abscisses d eventuelles sources de
debit (en metres). La source sera placee au noeud du maillage le plus
proche.""",
            ang = """Floats giving the abscissae of potential sources of flow rates
(in meters). The source will be located at the nearest node in the
mesh.""",
        ),
#       -----------------------------------
        ORDINATES_OF_SOURCES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Nombres reels donnant les ordonnees d''eventuelles sources de
debit (en metres). La source sera placee au noeud du maillage le plus
proche.""",
            ang = """Floats giving the ordinates of potential sources of flow rates
(in meters). The source will be located at the nearest node in the
mesh.""",
        ),
#       -----------------------------------
        ELEVATIONS_OF_SOURCES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Fixe la hauteur des sources.
Les sources sont automatiquement recalees sur le plan le plus proche.
L''utilisation d''un plan fixe est alors conseillee afin d''eviter que
le plan le plus proche ne change en cas de variation de la hauteur
d''eau locale.""",
            ang = """Sets the height of the sources.
The source will be located at the nearest plane in the mesh.
The use of a fixed plane is then recommended to avoid the change
of the nearest plane in case of variation of local water height.""",
        ),
#       -----------------------------------
        WATER_DISCHARGE_OF_SOURCES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Specifie le debit de chaque source.
Un debit positif signifie qu''il s''agit d''un apport de fluide.""",
            ang = """Specifies the discharge for every source.
A positive discharge means that fluid is added.""",
        ),
#       -----------------------------------
        VALUE_OF_THE_TRACERS_AT_THE_SOURCES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Fixe la valeur des traceurs aux sources
toutes les sources pour le premier traceur
puis toutes les sources du deuxieme traceur, etc.
(cf. manuel utilisateur).
Par exemple, s''il y a 3 traceurs (T1, T2 et T3) et 2 sources
(S1 et S2), la syntaxe suivante est utilisee :\\
S1\_T1;S1\_T2;S1\_T3;S2\_T1;S2\_T2;S2\_T3\\
10.0; 10.0; 0.0;  0.0; 10.0; 10.0""",
            ang = """Sets the value of the tracers at the sources.
All sources for the first tracer, then
all sources for the second tracer, etc.
(see user manual).
For example, if there are 3 tracers (T1, T2 and T3)
and 2 sources (S1 and S2), the following syntax is used:\\
S1\_T1;S1\_T2;S1\_T3;S2\_T1;S2\_T2;S2\_T3\\
10.0; 10.0; 0.0;  0.0; 10.0; 10.0""",
        ),
#       -----------------------------------
        PRESCRIBED_TRACERS_VALUES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Determine la valeur imposee des traceurs a la premiere
frontiere, puis a la deuxieme etc. suivant la meme logique que pour les
\telkey{VALEURS DES TRACEURS DES SOURCES}.""",
            ang = """Determines the imposed value of tracers at the first boundary,
then at the second, and so on, with the same logic as
\telkey{VALUE OF THE TRACERS AT THE SOURCES}.""",
        ),
#       -----------------------------------
        VELOCITIES_OF_THE_SOURCES_ALONG_X = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Permet de specifier la composante selon $x$ de la vitesse aux
sources. Si rien n''est specifie, les sources diffusent sans vitesse
dans toutes les directions (cf. cas de validation source).""",
            ang = """Specifies the compoment along $x$ of the velocities of the
sources. If nothing is specified, the sources diffuse without any
velocity in every direction (cf. validation case source).""",
        ),
#       -----------------------------------
        VELOCITIES_OF_THE_SOURCES_ALONG_Y = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            fr = """Permet de specifier la composante selon $y$ de la vitesse aux
sources. Si rien n''est specifie, les sources diffusent sans vitesse
dans toutes les directions (cf. cas de validation source).""",
            ang = """Specifies the compoment along y of the velocities of the
sources.  If nothing is specified, the sources diffuse without any
velocity in every direction (cf. validation case source).""",
        ),
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,_GRAPHICS_AND_LISTING = PROC(nom= "INPUT_OUTPUT,_GRAPHICS_AND_LISTING",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NUMBER_OF_DROGUES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Permet d''effectuer un suivi de flotteurs.
Fixe le nombre de flotteurs a traiter lors du calcul.
Il est alors necessaire de mettre a jour le sous-programme
\telfile{FLOT3D} afin de fournir les informations sur les
positions de largage et les temps de suivi des flotteurs.""",
        ang = """Number of drogues in the computation.
The user must then fill the subroutine \telfile{FLOT3D}
specifying the coordinates of the starting points,
their departure and arrival times.
The trajectory of drogues is recorded in the
\telkey{DROGUES FILE} that must be given in the steering file.""",
    ),
#   -----------------------------------
    PRINTOUT_PERIOD_FOR_DROGUES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Nombre de pas de temps entre 2 sorties de positions de
flotteurs dans le fichier des resultats binaire supplementaire
N affecte pas la qualite du calcul de la trajectoire""",
        ang = """Number of time steps between 2 outputs of drogues
positions in the binary file.
It does not disturb the quality of the computation of the trajectory.""",
    ),
#   -----------------------------------
    GRAPHIC_PRINTOUT_PERIOD = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Determine la periode en nombre de pas de temps d''impression des
\telkey{VARIABLES POUR LES SORTIES GRAPHIQUES 2D ou 3D}
(voir ces mot-cles) dans le \telkey{FICHIER DES RESULTATS 2D ou 3D}.""",
        ang = """Determines, in number of time steps, the printout period for
the \telkey{VARIABLES FOR 2D (or 3D) GRAPHIC PRINTOUTS}
in the \telkey{2D or 3D RESULT FILE}.""",
    ),
#   -----------------------------------
    LISTING_PRINTOUT_PERIOD = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Determine la periode en nombre de pas de temps d''impression
des ''VARIABLES A IMPRIMER''. Pour la mise au point,
il faut savoir que la sortie des resultats est effectuee
systematiquement sur le listing.""",
        ang = """Determines, in number of time steps, the printout period of
the VARIABLES TO BE PRINTED.
The results are systematically printed out on the listing file.""",
    ),
#   -----------------------------------
    NUMBER_OF_FIRST_TIME_STEP_FOR_GRAPHIC_PRINTOUTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Determine le numero de pas de temps a partir duquel debute
l''ecriture des resultats dans le \telkey{FICHIER DES RESULTATS 2D}
ou \telkey{3D}''.""",
        ang = """Determines the number of time steps after which the results
are first written into the \telkey{2D} or \telkey{3D RESULT FILE}.""",
    ),
#   -----------------------------------
    NUMBER_OF_FIRST_TIME_STEP_FOR_LISTING_PRINTOUTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Determine le numero de pas de temps a partir duquel debute
l''ecriture des resultats dans le listing.""",
        ang = """Determines the number of time steps after which the results
are first written into the listing.""",
    ),
#   -----------------------------------
    VARIABLES_FOR_3D_GRAPHIC_PRINTOUTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ["velocity along x axis (m/s)","velocity along y axis (m/s)","velocity along z axis (m/s)","elevation z (m)","concentration for tracer 1","concentration for tracer 2","concentration for tracer 3","concentration for tracer 4","concentration for tracer 5","concentration for tracer 6","concentration for tracer 7","concentration for tracer 8","concentration for tracer 9","concentrations for tracers from 1 to 9","concentrations for tracers from 10 to 99","viscosity for U and V along x axis (m2/s)","viscosity for U and V along y axis (m2/s)","viscosity for U and V along z axis (m2/s)","viscosity for tracers along x axis (m2/s)","viscosity for tracers along y axis (m2/s)","viscosity for tracers along z axis (m2/s)","Richardson number in case of mixing length model","turbulent energie for k-epsilon model (J/kg)","dissipation of turbulent energie (W/kg)","dynamic pressure (multiplied by DT/RHO)","hydrostatic pressure (in Pascals)","relative density","private variable 1","private variable 2","private variable 3","private variable 4"],
        defaut = ,
        fr = """Noms des variables que l''utilisateur veut ecrire dans
le \telkey{FICHIER DES RESULTATS 3D}.
Le choix des separateurs est libre.
Les possibilites offertes sont les suivantes :
\begin{itemize}
\item U   : vitesse suivant l''axe des $x$ (m/s) ;
\item V   : vitesse suivant l''axe des $y$ (m/s) ;
\item W   : vitesse suivant l''axe des $z$ (m/s) ;
\item Z   : cote $z$ (m) ;
\item TAx : concentrations des traceurs ;
\item NUX : viscosite pour $U$ et $V$ suivant l''axe des $x$ (m$^2$/s) ;
\item NUY : viscosite pour $U$ et $V$ suivant l''axe des $y$ (m$^2$/s) ;
\item NUZ : viscosite pour $U$ et $V$ suivant l''axe des $z$ (m$^2$/s) ;
\item NAX : viscosites pour les traceurs suivant l''axe des $x$
(m$^2$/s) ;
\item NAY : viscosites pour les traceurs suivant l''axe des $y$
(m$^2$/s) ;
\item NAZ : viscosites pour les traceurs suivant l''axe des $z$
(m$^2$/s) ;
\item RI  : nombre de Richardson en cas de modele de longueur de
melange ;
\item K   : energie turbulente du modele k-epsilon (J/kg) ;
\item EPS : dissipation de l''energie turbulente (W/kg) ;
\item DP  : pression dynamique (multipliee par DT/RHO) ;
\item PH  : pression hydrostatique (en Pascals) ;
\item RHO : densite relative ;
\item P1  : variable privee 1 ;
\item P2  : variable privee 2 ;
\item P3  : variable privee 3 ;
\item P4  : variable privee 4.
\end{itemize}""",
        ang = """Names of variables to be written in the
\telkey{3D RESULT FILE}. Free choice of separator. You can ask for:
\begin{itemize}
\item U  : velocity along $x$ (m/s),
\item V  : velocity along $y$ (m/s),
\item W  : velocity along $z$ (m/s),
\item Z  : elevation $z$ (m),
\item TAx: concentration of tracers,
\item NUX: viscosity for $U$ and $V$ along $x$ (m$^2$/s),
\item NUY: viscosity for $U$ and $V$ along $y$ (m$^2$/s),
\item NUZ: viscosity for $U$ and $V$ along $z$ (m$^2$/s),
\item NAX: viscosity for tracers along $x$ (m$^2$/s),
\item NAY: viscosity for tracers along $y$ (m$^2$/s),
\item NAZ: viscosity for tracers along $z$ (m$^2$/s),
\item RI : Richardson number for mixing length model,
\item K  : turbulent kinetic energy for $k$-$\epsilon$ model (J/kg),
\item EPS: dissipation of turbulent kinetic energy (W/kg),
\item DP : dynamic pressure (multiplied by DT/RHO),
\item PH : hydrostatic pressure (Pa),
\item RHO: relative density,
\item P1 : private variable 1,
\item P2 : private variable 2,
\item P3 : private variable 3,
\item P4 : private variable 4.
\end{itemize}""",
    ),
#   -----------------------------------
    VARIABLES_FOR_2D_GRAPHIC_PRINTOUTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ["depth averaged velocity along x axis (m/s)","depth averaged velocity along y axis (m/s)","celerity (m/s)","water depth (m)","free surface elevation (m)","bottom elevation (m)","TAx concentrations for tracers, x is the tracer number","Froude number","scalar discharge (m2/s)","discharge along x (m2/s)","discharge along y (m2/s)","norm of velocity (m/s)","wind along x axis (m/s)","wind along y axis (m/s)","atmospheric pressure (Pa)","friction coefficient","non erodible bottom elevation (m)","thickness of the sediment bed layer (m)","erosion rate (kg/m2/s)","deposition flux (kg/m2/s)"," evolution du lit","work array PRIVE 1","work array PRIVE 2","work array PRIVE 3","work array PRIVE 4","solid discharge (m2/s)","solid discharge along x (m2/s)","solid discharge along y (m2/s)","friction velocity (m/s)","maximum value of the free surface                                       elevation during the computation (m)","time corresponding to this maximum elevation (s)"],
        defaut = ,
        fr = """Noms des variables que l''utilisateur veut ecrire dans
le \telkey{FICHIER DES RESULTATS 2D}.
Chaque variable est representee par une lettre.
Le choix des separateurs est libre.
Les possibilites offertes sont les suivantes :
\begin{itemize}
\item U : vitesse moyenne suivant l''axe des x (m/s) ;
\item V : vitesse moyenne suivant l''axe des y (m/s) ;
\item C : celerite (m/s) ;
\item H : hauteur d''eau (m) ;
\item S : cote de surface libre (m) ;
\item B : cote du fond (m) ;
\item F : nombre de Froude ;
\item Q : debit scalaire (m$^2$/s) ;
\item I : debit suivant x (m$^2$/s) ;
\item J : debit suivant y (m$^2$/s) ;
\item M : norme de la vitesse (m/s) ;
\item X : vent suivant l''axe des x (m/s) ;
\item Y : vent suivant l''axe des y (m/s) ;
\item P : pression atmospherique (Pa) ;
\item W : coefficient de frottement ;
\item RB : cote des fonds non erodables (m) ;
\item FD : epaisseur des depots frais (m) ;
\item EF : flux d''erosion (kg/m$2^$/s) ;
\item DP : probabilite de depot (kg/m$2^$/s) ;
\item PRIVE1 : tableau de travail PRIVE 1 ;
\item PRIVE2 : tableau de travail PRIVE 2 ;
\item PRIVE3 : tableau de travail PRIVE 3 ;
\item PRIVE4 : tableau de travail PRIVE 4 ;
\item US : vitesse de frottement (m/s) ;
\item MAXZ : valeur maximum de la cote de l eau au cours du calcul (m) ;
\item TMXZ : temps correspondant a ce niveau maximum (s).
\end{itemize}""",
        ang = """Names of variables that may be written in the
\telkey{2D RESULT FILE}.
Every variable is represented by a group of letters with
any separator between them , ; or blank
possibilities are the following:
\begin{itemize}
\item U: depth averaged velocity along x axis (m/s),
\item V: depth averaged velocity along y axis (m/s),
\item C: celerity (m/s),
\item H: water depth (m),
\item S: free surface elevation (m),
\item B: bottom elevation (m),
\item F: Froude number,
\item Q: scalar discharge (m$^2$/s),
\item I: discharge along x (m$^2$/s),
\item J: discharge along y (m$^2$/s),
\item M: norm of velocity (m/s),
\item X: wind along x axis (m/s),
\item Y: wind along y axis (m/s),
\item P: atmospheric pressure (Pa),
\item W: friction coefficient,
\item RB: non erodible bottom elevation (m),
\item FD: thickness of the fresh deposits (m),
\item EF: erosion rate (kg/m$^2$/s),
\item DP: probability of deposition (kg/m$^2$/s),
\item PRIVE1: work array PRIVE 1,
\item PRIVE2: work array PRIVE 2,
\item PRIVE3: work array PRIVE 3,
\item PRIVE4: work array PRIVE 4,
\item US: friction velocity (m/s),
\item MAXZ: maximum value of the free surface
elevation during the computation (m),
\item TMXZ: time corresponding to this maximum elevation (s).
\end{itemize}""",
    ),
#   -----------------------------------
    MASS_BALANCE = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Determine si l''on effectue ou non le bilan de masse
sur le domaine.
Cette procedure calcule a chaque pas de temps :
\begin{itemize}
\item les flux aux entrees et sorties du domaine ;
\item le flux global a travers l''ensemble des parois du domaine
(liquides ou solides) ;
\item l''erreur relative sur la masse pour ce pas de temps.
\end{itemize}
En fin de listing, on trouve l''erreur relative sur la masse pour
l''ensemble du calcul.
Il ne s''agit que d''un calcul indicatif car il n''existe pas
d''expression compatible du debit en formulation c,u,v.""",
        ang = """Determines whether a check of the mass-balance over
the domain is done or not.
This procedures computes the following at each time step:
\begin{itemize}
\item the domain inflows and outflows,
\item the overall flow across all the boundaries,
\item the relative error in the mass for that time step.
\end{itemize}
The relative error in the mass over the whole computation can be found
at the end of the listing.""",
    ),
#   -----------------------------------
    INFORMATION_ABOUT_MASS_BALANCE_FOR_EACH_LISTING_PRINTOUT = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [True ],
        fr = """Donne a chaque sortie listing une information sur le bilan de
masse.""",
        ang = """Gives the information about mass-balance
at every \telkey{LISTING PRINTOUT PERIOD}.""",
    ),
#   -----------------------------------
    LISTING_PRINTOUT = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [True ],
        fr = """Sortie des resultats sur support papier.
Si l''on met NON le listing ne contient que l''entete et la mention
FIN NORMALE DU PROGRAMME. Commande a eviter""",
        ang = """Result printout on hard copy.
When NO is selected, the listing only includes the heading and the
phrase "NORMAL END OF PROGRAM".
In addition, the options \telkey{MASS-BALANCE} and
\telkey{VALIDATION} are inhibited. Not recommended for use.""",
    ),
#   -----------------------------------
    SALINITY_FOR_DELWAQ = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Decide de la sortie de la salinite pour DELWAQ.""",
        ang = """Triggers the output of salinity for DELWAQ.""",
    ),
#   -----------------------------------
    TEMPERATURE_FOR_DELWAQ = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Decide de la sortie de la temperature pour DELWAQ.""",
        ang = """Triggers the output of temperature for DELWAQ.""",
    ),
#   -----------------------------------
    VELOCITY_FOR_DELWAQ = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Decide de la sortie de la vitesse pour DELWAQ.""",
        ang = """Triggers the output of velocity for DELWAQ.""",
    ),
#   -----------------------------------
    DIFFUSION_FOR_DELWAQ = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Decide de la sortie de la diffusion pour DELWAQ.""",
        ang = """Triggers the output of diffusion for DELWAQ.""",
    ),
)
# -----------------------------------------------------------------------
EQUATIONS,_ADVECTION = PROC(nom= "EQUATIONS,_ADVECTION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    ADVECTION_STEP = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [True ],
        fr = """Prise en compte ou non des termes de convection.
En cas de reponse positive,
on peut encore supprimer certains termes de convection avec
les mots-cles \telkey{SCHEMA POUR LA CONVECTION...}""",
        ang = """Takes into account the advection terms or not.
If YES, some advection terms can still be ignored with the keywords
\telkey{SCHEME FOR ADVECTION OF...}""",
    ),
#   -----------------------------------
    GENERAL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        SCHEME_FOR_ADVECTION_OF_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["NO ADVECTION","CHARACTERISTICS","EXPLICIT + SUPG","EXPLICIT LEO POSTMA","EXPLICIT + MURD SCHEME N","EXPLICIT + MURD SCHEME PSI","LEO POSTMA FOR TIDAL FLATS","N-SCHEME FOR TIDAL FLATS"],
            defaut = ["CHARACTERISTICS"],
            fr = """Fixe le schema utilise pour la convection des vitesses.
Les choix possibles sont :
\begin{itemize}
\item 0 : pas de convection ;
\item 1 : caracteristiques ;
\item 2 : explicite + SUPG ;
\item 3 : explicite Leo Postma ;
\item 4 : explicite + MURD schema N ;
\item 5 : explicite + MURD schema PSI ;
\item 13 : Leo Postma pour bancs decouvrants ;
\item 14 : schema N pour bancs decouvrants.
\end{itemize}""",
            ang = """Sets the advection scheme for the velocities.
Possible choices are:
\begin{itemize}
\item 0: no convection,
\item 1: characteristics,
\item 2: explicit + SUPG,
\item 3: explicit Leo Postma,
\item 4: explicit + MURD scheme N,
\item 5: explicit + MURD scheme PSI,
\item 13: Leo Postma for tidal flats,
\item 14: N-scheme for tidal flats.
\end{itemize}""",
        ),
#       -----------------------------------
        SCHEME_FOR_ADVECTION_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            into = ["NO ADVECTION","CHARACTERISTICS","EXPLICIT + SUPG","EXPLICIT LEO POSTMA","EXPLICIT + MURD SCHEME N","EXPLICIT + MURD SCHEME PSI","LEO POSTMA FOR TIDAL FLATS","N-SCHEME FOR TIDAL FLATS"],
            fr = """Fixe le schema utilise pour la convection des traceurs.
Les choix possibles sont :
\begin{itemize}
\item 0 : pas de convection ;
\item 1 : caracteristiques ;
\item 2 : explicite + SUPG ;
\item 3 : explicite Leo Postma ;
\item 4 : explicite + MURD schema N ;
\item 5 : explicite + MURD schema PSI ;
\item 13 : Leo Postma pour bancs decouvrants ;
\item 14 : schema N pour bancs decouvrants.
\end{itemize}""",
            ang = """Sets the advection scheme for the tracers.
Possible choices are:
\begin{itemize}
\item 0: no convection,
\item 1: characteristics,
\item 2: explicit + SUPG,
\item 3: explicit Leo Postma,
\item 4: explicit + MURD scheme N,
\item 5: explicit + MURD scheme PSI,
\item 13: Leo Postma for tidal flats,
\item 14: N-scheme for tidal flats.
\end{itemize}""",
        ),
#       -----------------------------------
        SCHEME_FOR_ADVECTION_OF_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["NO ADVECTION","CHARACTERISTICS","EXPLICIT + SUPG","EXPLICIT LEO POSTMA","EXPLICIT + MURD SCHEME N","EXPLICIT + MURD SCHEME PSI","LEO POSTMA FOR TIDAL FLATS","N-SCHEME FOR TIDAL FLATS"],
            defaut = ["CHARACTERISTICS"],
            fr = """Fixe le schema utilise pour la convection du modele
$k$-$\epsilon$. Les choix possibles sont :
\begin{itemize}
\item 0 : pas de convection ;
\item 1 : caracteristiques ;
\item 2 : explicite + SUPG ;
\item 3 : explicite Leo Postma ;
\item 4 : explicite + MURD schema N ;
\item 5 : explicite + MURD schema PSI ;
\item 13 : Leo Postma pour bancs decouvrants ;
\item 14 : schema N pour bancs decouvrants.
\end{itemize}""",
            ang = """Sets the advection scheme for the $k$-$\epsilon$ model.
Possible choices are:
\begin{itemize}
\item 0: no convection,
\item 1: characteristics,
\item 2: explicit + SUPG,
\item 3: explicit Leo Postma,
\item 4: explicit + MURD scheme N,
\item 5: explicit + MURD scheme PSI,
\item 13: Leo Postma for tidal flats,
\item 14: N-scheme for tidal flats.
\end{itemize}""",
        ),
#       -----------------------------------
        SCHEME_FOR_ADVECTION_OF_DEPTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["CONSERVATIVE"],
            defaut = ["CONSERVATIVE"],
            fr = """Le schema conservatif (5) est desormais impose.""",
            ang = """The conservative scheme (5) is now mandatory.""",
        ),
#       -----------------------------------
        SCHEME_OPTION_FOR_ADVECTION_OF_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """Si present remplace et a priorite sur :
\telkey{OPTION POUR LES CARACTERISTIQUES} et
\telkey{OPTION DE SUPG}.
Si schema PSI ou N :
\begin{itemize}
\item 1 : explicite ;
\item 2 : predicteur-correcteur ;
\item 3 : predicteur-correcteur deuxieme ordre en temps ;
\item 4 : implicite.
\end{itemize}""",
            ang = """If present replaces and has priority over:
\telkey{OPTION FOR CHARACTERISTICS} and
\telkey{SUPG OPTION}.
If N or PSI scheme:
\begin{itemize}
\item 1: explicit,
\item 2: predictor-corrector,
\item 3: predictor-corrector second-order in time,
\item 4: implicit.
\end{itemize}""",
        ),
#       -----------------------------------
        SCHEME_OPTION_FOR_ADVECTION_OF_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """Si present remplace et a priorite sur :
\telkey{OPTION POUR LES CARACTERISTIQUES} et
\telkey{OPTION DE SUPG}.
Si schema PSI ou N :
\begin{itemize}
\item 1 : explicite ;
\item 2 : predicteur-correcteur ;
\item 3 : predicteur-correcteur deuxieme ordre en temps ;
\item 4 : implicite.
\end{itemize}""",
            ang = """If present replaces and has priority over:
\telkey{OPTION FOR CHARACTERISTICS} and
\telkey{SUPG OPTION}.
If N or PSI scheme:
\begin{itemize}
\item 1: explicit,
\item 2: predictor-corrector,
\item 3: predictor-corrector second-order in time,
\item 4: implicit.
\end{itemize}""",
        ),
#       -----------------------------------
        SCHEME_OPTION_FOR_ADVECTION_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 1,
            fr = """Si present remplace et a priorite sur :
\telkey{OPTION POUR LES CARACTERISTIQUES} et
\telkey{OPTION DE SUPG}.
Si schema PSI ou N :
\begin{itemize}
\item 1 : explicite ;
\item 2 : predicteur-correcteur ;
\item 3 : predicteur-correcteur deuxieme ordre en temps ;
\item 4 : implicite.
\end{itemize}""",
            ang = """If present replaces and has priority over:
\telkey{OPTION FOR CHARACTERISTICS} and
\telkey{SUPG OPTION}.
If N or PSI scheme:
\begin{itemize}
\item 1: explicit,
\item 2: predictor-corrector,
\item 3: predictor-corrector second-order in time,
\item 4: implicit.
\end{itemize}""",
        ),
    ),
)
# -----------------------------------------------------------------------
EQUATIONS,_DIFFUSION = PROC(nom= "EQUATIONS,_DIFFUSION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    DIFFUSION_STEP = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [True ],
        fr = """Prise en compte ou non des termes de diffusion.
En cas de reponse positive,
on peut encore supprimer certains termes de diffusion avec
les mots-cles \telkey{SCHEMA POUR LA DIFFUSION...}""",
        ang = """Takes into account the diffusion terms or not.
If YES, some diffusion terms can still be ignored with the keywords
\telkey{SCHEME FOR DIFFUSION OF...}""",
    ),
#   -----------------------------------
    GENERAL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        SCHEME_FOR_DIFFUSION_OF_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["NO DIFFUSION","IMPLICIT"],
            defaut = ["IMPLICIT"],
            fr = """Permet de specifier si l''on utilise ou non la diffusion
des vitesses horizontales $U$ et $V$.
Les choix possibles sont :
\begin{itemize}
\item 0 : pas de diffusion,
\item 1 : implicite.
\end{itemize}""",
            ang = """Monitors the choice of the diffusion scheme
for velocities.
Possible choices are:
\begin{itemize}
\item 0: no diffusion,
\item 1: implicit.
\end{itemize}""",
        ),
#       -----------------------------------
        SCHEME_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            into = ["NO DIFFUSION","IMPLICIT","VERTICAL DIFFUSION ONLY"],
            defaut = "IMPLICIT",
            fr = """Permet de specifier si l''on utilise ou non la diffusion
des traceurs
Les choix possibles sont :
\begin{itemize}
\item 0 : pas de diffusion,
\item 1 : implicite;
\item 2 : diffusion verticale seulement.
\end{itemize}""",
            ang = """Monitors the choice of the diffusion scheme
for tracers.
Possible choices are:
\begin{itemize}
\item 0: no diffusion,
\item 1: implicit,
\item 2: vertical diffusion only.
\end{itemize}""",
        ),
#       -----------------------------------
        SCHEME_FOR_DIFFUSION_OF_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["NO DIFFUSION","IMPLICIT"],
            defaut = ["IMPLICIT"],
            fr = """Permet de specifier si l''on utilise ou non la diffusion
de $k$ et $\epsilon$.
Les choix possibles sont :
\begin{itemize}
\item 0 : pas de diffusion,
\item 1 : implicite.
\end{itemize}""",
            ang = """Monitors the choice of the diffusion scheme
for $k$ and $\epsilon$.
Possible choices are:
\begin{itemize}
\item 0: no diffusion,
\item 1: implicit.
\end{itemize}""",
        ),
#       -----------------------------------
        PRECONDITIONING_FOR_DIFFUSION_OF_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["no preconditioning","diagonal","diagonal condensed","diagonal with absolute values","Crout","Gauss-Seidel EBE","Matrix defined by the user","diagonal and Crout","direct solver on the vertical","diagonal condensed and Crout","diagonal and direct solver on the vertical"],
            defaut = ["diagonal"],
            fr = """Permet de preconditionner le systeme relatif
a la diffusion des vitesses. Les choix possibles sont :
\begin{itemize}
\item 0 : aucun ;
\item 2 : diagonal ;
\item 3 : diagonal avec matrice condensee ;
\item 5 : diagonal avec valeurs absolues ;
\item 7 : Crout ;
\item 11 : Gauss-Seidel EBE ;
\item 13 : matrice fournie par l''utilisateur ;
\item 14 : diagonal et Crout ;
\item 17 : solveur direct sur la verticale ;
\item 21 : diagonal condensee et Crout ;
\item 34 : diagonal et solveur direct sur la verticale.
\end{itemize}""",
            ang = """Choice of preconditioning for the diffusion of
velocities. Possible choices are:
\begin{itemize}
\item 0: no preconditioning,
\item 2: diagonal,
\item 3: diagonal with the condensed matrix,
\item 5: diagonal with absolute values,
\item 7: Crout,
\item 11: Gauss-Seidel EBE,
\item 13: matrix defined by the user,
\item 14: diagonal and Crout,
\item 17: direct solver on the vertical,
\item 21: diagonal condensed and Crout,
\item 34: diagonal and direct solver on the vertical.
\end{itemize}""",
        ),
#       -----------------------------------
        PRECONDITIONING_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            into = ["no preconditioning","diagonal","diagonal condensed","diagonal with absolute values","Crout","Gauss-Seidel EBE","Matrix defined by the user","diagonal and Crout","direct solver on the vertical","diagonal condensed and Crout","diagonal and direct solver on the vertical"],
            fr = """Permet de preconditionner le systeme relatif
a la diffusion des traceurs. Les choix possibles sont :
\begin{itemize}
\item 0 : aucun ;
\item 2 : diagonal ;
\item 3 : diagonal avec matrice condensee ;
\item 5 : diagonal avec valeurs absolues ;
\item 7 : Crout ;
\item 11 : Gauss-Seidel EBE ;
\item 13 : matrice fournie par l''utilisateur ;
\item 14 : diagonal et Crout ;
\item 17 : solveur direct sur la verticale ;
\item 21 : diagonal condensee et Crout ;
\item 34 : diagonal et solveur direct sur la verticale.
\end{itemize}""",
            ang = """Choice of preconditioning for the diffusion of tracers.
Possible choices are:
\begin{itemize}
\item 0: no preconditioning,
\item 2: diagonal,
\item 3: diagonal with the condensed matrix,
\item 5: diagonal with absolute values,
\item 7: Crout,
\item 11: Gauss-Seidel EBE,
\item 13: matrix defined by the user,
\item 14: diagonal and Crout,
\item 17: direct solver on the vertical,
\item 21: diagonal condensed and Crout,
\item 34: diagonal and direct solver on the vertical.
\end{itemize}""",
        ),
#       -----------------------------------
        PRECONDITIONING_FOR_DIFFUSION_OF_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["no preconditioning","diagonal","diagonal condensed","diagonal with absolute values","Crout","Gauss-Seidel EBE","Matrix defined by the user","diagonal and Crout","direct solver on the vertical","diagonal condensed and Crout","diagonal and direct solver on the vertical"],
            defaut = ["diagonal"],
            fr = """Permet de preconditionner le systeme relatif
a la diffusion du modele $k$-$\epsilon$. Les choix possibles sont :
\begin{itemize}
\item 0 : aucun ;
\item 2 : diagonal ;
\item 3 : diagonal avec matrice condensee ;
\item 5 : diagonal avec valeurs absolues ;
\item 7 : Crout ;
\item 11 : Gauss-Seidel EBE ;
\item 13 : matrice fournie par l''utilisateur ;
\item 14 : diagonal et Crout ;
\item 17 : solveur direct sur la verticale ;
\item 21 : diagonal condensee et Crout ;
\item 34 : diagonal et solveur direct sur la verticale.
\end{itemize}""",
            ang = """Choice of preconditioning for the diffusion of
the $k$-$\epsilon$ model. Possible choices are:
\begin{itemize}
\item 0: no preconditioning,
\item 2: diagonal,
\item 3: diagonal with the condensed matrix,
\item 5: diagonal with absolute values,
\item 7: Crout,
\item 11: Gauss-Seidel EBE,
\item 13: matrix defined by the user,
\item 14: diagonal and Crout,
\item 17: direct solver on the vertical,
\item 21: diagonal condensed and Crout,
\item 34: diagonal and direct solver on the vertical.
\end{itemize}""",
        ),
#       -----------------------------------
        SOLVER_FOR_DIFFUSION_OF_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","cgstab","gmres","direct solver"],
            defaut = ["conjugate gradient"],
            fr = """Permet de choisir le solveur utilise pour la resolution
de la diffusion des vitesses $U$ et $V$.
Les choix possibles sont :
\begin{itemize}
\item 1 : gradient conjugue ;
\item 2 : residu conjugue ;
\item 3 : gradient conjugue sur equation normale ;
\item 4 : erreur minimale ;
\item 5 : gradient conjugue carre ;
\item 6 : CGSTAB ;
\item 7 : GMRES ;
\item 8 : solveur direct.
\end{itemize}""",
            ang = """Choice of the solver for the diffusion of velocities
$U$ and $V$.
Possible choices are:
\begin{itemize}
\item 1: conjugate gradient,
\item 2: conjugate residual,
\item 3: conjugate gradient on a normal equation,
\item 4: minimum error,
\item 5: squared conjugate gradient,
\item 6: CGSTAB,
\item 7: GMRES,
\item 8: direct solver.
\end{itemize}""",
        ),
#       -----------------------------------
        SOLVER_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","cgstab","gmres","direct solver"],
            fr = """Permet de choisir le solveur utilise pour la resolution de
la diffusion des traceurs.
Les choix possibles sont :
\begin{itemize}
\item 1 : gradient conjugue ;
\item 2 : residu conjugue ;
\item 3 : gradient conjugue sur equation normale ;
\item 4 : erreur minimale ;
\item 5 : gradient conjugue carre ;
\item 6 : CGSTAB ;
\item 7 : GMRES ;
\item 8 : solveur direct.
\end{itemize}""",
            ang = """Choice of the solver for the diffusion of tracers.
Possible choices are:
\begin{itemize}
\item 1: conjugate gradient,
\item 2: conjugate residual,
\item 3: conjugate gradient on a normal equation,
\item 4: minimum error,
\item 5: squared conjugate gradient,
\item 6: CGSTAB,
\item 7: GMRES,
\item 8: direct solver.
\end{itemize}""",
        ),
#       -----------------------------------
        SOLVER_FOR_DIFFUSION_OF_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","cgstab","gmres","direct solver"],
            defaut = ["conjugate gradient"],
            fr = """Permet de choisir le solveur utilise pour la resolution de
la diffusion du modele $k$-$\epsilon$.
Les choix possibles sont :
\begin{itemize}
\item 1 : gradient conjugue ;
\item 2 : residu conjugue ;
\item 3 : gradient conjugue sur equation normale ;
\item 4 : erreur minimale ;
\item 5 : gradient conjugue carre ;
\item 6 : CGSTAB ;
\item 7 : GMRES ;
\item 8 : solveur direct.
\end{itemize}""",
            ang = """Choice of the solver for the diffusion of $k$ and $\epsilon$.
Possible choices are:
\begin{itemize}
\item 1: conjugate gradient,
\item 2: conjugate residual,
\item 3: conjugate gradient on a normal equation,
\item 4: minimum error,
\item 5: squared conjugate gradient,
\item 6: CGSTAB,
\item 7: GMRES,
\item 8: direct solver.
\end{itemize}""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_DIFFUSION_OF_VELOCITIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [60],
            fr = """Limite le nombre d''iterations du solveur a chaque pas
de temps pour le calcul de la diffusion de la vitesse.""",
            ang = """Limits the number of solver iterations for the diffusion of
velocities.""",
        ),
#       -----------------------------------
        PRECONDITIONING_FOR_PPE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["no preconditioning","diagonal","diagonal condensed","diagonal with absolute values","Crout","Gauss-Seidel EBE","Matrix defined by the user","diagonal and Crout","direct solver on the vertical","diagonal condensed and Crout","diagonal and direct solver on the vertical"],
            defaut = ["diagonal"],
            fr = """Preconditionnement pour l''equation de Poisson.
Les choix possibles sont :
\begin{itemize}
\item 0 : aucun ;
\item 2 : diagonal ;
\item 3 : diagonal avec matrice condensee ;
\item 5 : diagonal avec valeurs absolues ;
\item 7 : Crout ;
\item 11 : Gauss-Seidel EBE ;
\item 13 : matrice fournie par l''utilisateur ;
\item 14 : diagonal et Crout ;
\item 17 : solveur direct sur la verticale ;
\item 21 : diagonal condensee et Crout ;
\item 34 : diagonal et solveur direct sur la verticale.
\end{itemize}""",
            ang = """Preconditioning for the Poisson Pressure Equation.
Possible choices are:
\begin{itemize}
\item 0: no preconditioning,
\item 2: diagonal,
\item 3: diagonal with the condensed matrix,
\item 5: diagonal with absolute values,
\item 7: Crout,
\item 11: Gauss-Seidel EBE,
\item 13: matrix defined by the user,
\item 14: diagonal and Crout,
\item 17: direct solver on the vertical,
\item 21: diagonal condensed and Crout,
\item 34: diagonal and direct solver on the vertical.
\end{itemize}""",
        ),
#       -----------------------------------
        SOLVER_FOR_PPE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","cgstab","gmres","direct solver"],
            defaut = ["conjugate gradient"],
            fr = """Permet de choisir le solveur utilise pour la resolution de
l''equation de Poisson.
Les choix possibles sont :
\begin{itemize}
\item 1 : gradient conjugue ;
\item 2 : residu conjugue ;
\item 3 : gradient conjugue sur equation normale ;
\item 4 : erreur minimale ;
\item 5 : gradient conjugue carre ;
\item 6 : CGSTAB ;
\item 7 : GMRES ;
\item 8 : solveur direct.
\end{itemize}""",
            ang = """Choice of the solver for the Poisson Pressure Equation.
Possible choices are:
\begin{itemize}
\item 1: conjugate gradient,
\item 2: conjugate residual,
\item 3: conjugate gradient on a normal equation,
\item 4: minimum error,
\item 5: squared conjugate gradient,
\item 6: CGSTAB,
\item 7: GMRES,
\item 8: direct solver.
\end{itemize}""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_PPE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [100],
            fr = """Limite le nombre d iterations pour l''equation de Poisson.""",
            ang = """Limits the number of solver iterations for the Poisson
Pressure Equation.""",
        ),
    ),
#   -----------------------------------
    TRACER = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_DIFFUSION_OF_SEDIMENT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [60],
            fr = """Limite le nombre d''iterations du solveur a chaque pas
de temps pour le calcul de la diffusion du sediment.""",
            ang = """Limits the number of solver iterations for the diffusion of
sediment.""",
        ),
#       -----------------------------------
        MAXIMUM_NUMBER_OF_ITERATIONS_FOR_DIFFUSION_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [60],
            fr = """Limite le nombre d''iterations du solveur a chaque pas
de temps pour le calcul de la diffusion des traceurs.""",
            ang = """Limits the number of solver iterations for the diffusion of
tracers.""",
        ),
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
        fr = """Fixe la valeur de l''acceleration de la pesanteur en m/s$^2$.""",
        ang = """Sets the value of the acceleration due to gravity in m/s$^2$.""",
    ),
#   -----------------------------------
    CORIOLIS_COEFFICIENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Fixe la valeur du coefficient de la force de Coriolis.
Celui-ci doit etre calcule en fonction de la latitude $l$
par la formule
$FCOR = 2 \omega sin(l)$ ,
$\omega$ etant la vitesse de rotation de la terre.
$\omega$ = 7.29 10-5 rad/s.\\
Les composantes de la force de Coriolis sont alors :\\
$FU =  FCOR \times V,$\\
$FV = -FCOR \times U.$
Lorsqu''on utilise les coordonnees spheriques, le coefficient de
Coriolis est calcule automatiquement.""",
        ang = """Sets the value of the Coriolis force coefficient,
in cartesian coordinates.
This coefficient, denoted \telfile{FCOR} in the code, should be equal to
$2 \omega \sin(l)$  where $\omega$ denotes the earth angular speed of
rotation and $l$ the latitude. $\omega$ = 7.29 10-5 rad/s.\\
The Coriolis force components are then:\\
$FU =  FCOR \times V,$\\
$FV = -FCOR \times U.$\\
When using the spherical coordinates, the Coriolis coefficient is
automatically computed.""",
    ),
#   -----------------------------------
    BETA_EXPANSION_COEFFICIENT_FOR_TRACERS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """Unite : K$^{-1}$.
Ce coefficient permet de definir l''evolution de la densite de
l''eau en fonction de la concentration en traceur lors de
l''utilisation de la valeur 4 du mot cle
\telkey{LOI DE DENSITE}.""",
        ang = """Unit: K$^{-1}$.
This coefficient is used to define the evolution of the water density
with respect to the tracer concentration when using
\telkey{LAW OF DENSITY} = 4.""",
    ),
#   -----------------------------------
    AVERAGE_WATER_DENSITY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1025.],
        fr = """Valeur de la densite moyenne dans le domaine, voir
\telfile{DRSURR}.""",
        ang = """Average water density in the domain, see subroutine
\telfile{DRSURR}.""",
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,_INFORMATION = PROC(nom= "INPUT_OUTPUT,_INFORMATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    COMPUTATIONAL_INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        TITLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = '',
            fr = """Titre du cas etudie.""",
            ang = """Title of the case being considered.""",
        ),
#       -----------------------------------
        RELEASE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = 'V7P1',
            fr = """Numero de version des bibliotheques utilisees par TELEMAC.
Si ce nom commence par D il s''agit de l''option Debug (exemple DV2P2)
Si ce nom commence par F il s''agit de l''option Flowtrace""",
            ang = """""",
        ),
#       -----------------------------------
        COMPUTATION_CONTINUED = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Determine si le calcul en cours est independant de tout autre
resultat ou est une reprise effectuee a partir du resultat d''un calcul
precedent.
\begin{itemize}
\item NON : Il s''agit du premier passage pour ce calcul et il est
necessaire de definir un jeu complet de conditions initiales
\item OUI : Il s''agit d''une reprise de calcul :
les conditions initiales sont constituees par le dernier pas de
temps du \telkey{FICHIER DU CALCUL PRECEDENT} du fichier des parametres
utilise pour soumettre le calcul.
\end{itemize}
Par contre, l''ensemble des donnees du fichier des parametres
peuvent etre redefinies, ce qui offre la possibilite de changer
par exemple, le pas de temps, le modele de turbulence, le
frottement, d''ajouter ou retirer un traceur\ldots\\
De meme, il est necessaire de definir des conditions aux limites
(sous-programme \telfile{BORD3D} ou valeurs placees dans le fichier des
parametres), qui peuvent egalement etre modifiees.\\
Afin d''obtenir une suite de calcul parfaite, l''utilisateur doit
activer le \telkey{MODE SUITE} dans un calcul precedent afin de generer
le fichier a partir duquel le calcul suivant commence
(\telkey{FICHIER POUR SUITE}).""",
            ang = """Determines whether the computation under way is independent
or is following an earlier result.
\begin{itemize}
\item NO: It is the first run for this computation and a whole set of
initial conditions should be defined,
\item YES: It follows a former computation:
the initial conditions consist in the last time step of the
\telkey{PREVIOUS COMPUTATION FILE} defined in the steering file
used for submitting the computation.
\end{itemize}
All the data from the steering file may be defined once again, which
provides an opportunity to change, for example, the time step,
the turbulence model, the friction, to add or remove a tracer\ldots\\
It is also possible to define new boundary conditions
(in the subroutine \telfile{BORD3D} or values defined
in the steering file).\\
In order to get a perfect continued computation, the user has to
activate the \telkey{RESTART MODE} in a previous computation to generate
the file from which the following computation starts
(\telkey{RESTART FILE}).""",
        ),
#       -----------------------------------
        VALIDATION = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Option utilisee principalement pour le dossier de validation. Le
\telkey{FICHIER DE REFERENCE} est alors considere comme une
reference a laquelle on va comparer le calcul. La comparaison
est effectuee par le sous-programme VALIDA qui peut etre une comparaison
avec une solution exacte par exemple.""",
            ang = """This option is primarily used for the validation documents.
The \telkey{REFERENCE FILE} is then considered as a
reference which the computation is going to be compared with.
The comparison is done by the subroutine VALIDA, which can be
modified so as to include, for example, a comparison with an exact
solution.""",
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
                fr = """Format du \telkey{FICHIER DE REFERENCE}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}""",
                ang = """Format of the \telkey{REFERENCE FILE}.
Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}""",
            ),
#           -----------------------------------
            REFERENCE_FILE = SIMP(statut ='f',
#           -----------------------------------
                typ = ('Fichier','All Files (*)'),
                defaut = '',
                fr = """Fichier de resultats de reference pour la validation.""",
                ang = """Binary-coded result file for validation.""",
            ),
        ),
#       -----------------------------------
        DESCRIPTION_OF_LIBRARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=11, max=11,
            defaut = 'builds|PPP|lib|telemac3dMMMVVV.LLL;builds|PPP|lib|telemac2dMMMVVV.LLL;builds|PPP|lib|tomawacMMMVVV.LLL;builds|PPP|lib|sisypheMMMVVV.LLL;builds|PPP|lib|nestorMMMVVV.LLL;builds|PPP|lib|waqtelMMMVVV.LLL;builds|PPP|lib|biefMMMVVV.LLL;builds|PPP|lib|hermesMMMVVV.LLL;builds|PPP|lib|damoMMMVVV.LLL;builds|PPP|lib|parallelMMMVVV.LLL;builds|PPP|lib|specialMMMVVV.LLL',
            fr = """Description des bibliotheques de \telemac{3d}.""",
            ang = """Libraries description of \telemac{3d}.""",
        ),
#       -----------------------------------
        DEFAULT_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|telemac3dMMMVVV.exe',
            fr = """Executable par defaut de \telemac{3d}.""",
            ang = """Default executable for \telemac{3d}.""",
        ),
#       -----------------------------------
        DEFAULT_PARALLEL_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|telemac3dMMMVVV.exe',
            fr = """Executable parallele par defaut de \telemac{3d}.""",
            ang = """Default parallel executable for \telemac{3d}.""",
        ),
    ),
#   -----------------------------------
    CONTROL = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        DELWAQ_PRINTOUT_PERIOD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """Periode de sortie des resultats pour DELWAQ.""",
            ang = """Printout period for DELWAQ files.""",
        ),
#       -----------------------------------
        ORIGIN_COORDINATES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min= 2, max= 2,
            defaut = [0,0],
            fr = """Valeurs en metres, utilise pour eviter les trop grands nombres,
transmis dans le format SERAFIN mais pas d''autre traitement pour
l''instant.""",
            ang = """Values in metres, used to avoid large real numbers,
added in SERAFIN format, but so far no other treatment.""",
        ),
#       -----------------------------------
        OIL_SPILL_MODEL = SIMP(statut ='f',
#       -----------------------------------
            typ = bool, min=0, max='**',
            defaut = [False],
            fr = """Pour declencher le modele de nappes d''hydrocarbures,
dans ce cas le
\telkey{FICHIER DE COMMANDES HYDROCARBURES} est necessaire.""",
            ang = """Will trigger the oil spill model, in this case
the \telkey{OIL SPILL STEERING FILE} is needed.""",
        ),
    ),
#   -----------------------------------
    COMPUTATION_ENVIRONMENT = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        DICTIONARY = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = 'telemac3d.dico',
            fr = """Dictionnaire des mots cles.""",
            ang = """Key word dictionary.""",
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
        fr = """Format du \telkey{FICHIER DE GEOMETRIE}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}""",
        ang = """Format of the \telkey{GEOMETRY FILE}.
Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}""",
    ),
#   -----------------------------------
    2D_RESULT_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du \telkey{FICHIER DES RESULTATS 2D}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}""",
        ang = """Format of the \telkey{2D RESULT FILE}. Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}""",
    ),
#   -----------------------------------
    3D_RESULT_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du \telkey{FICHIER DES RESULTATS 3D}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}""",
        ang = """Format of the \telkey{3D RESULT FILE}. Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}""",
    ),
#   -----------------------------------
    PREVIOUS_COMPUTATION_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du \telkey{FICHIER DU CALCUL PRECEDENT}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}""",
        ang = """Format of the \telkey{PREVIOUS COMPUTATION FILE}.
Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}""",
    ),
#   -----------------------------------
    BINARY_DATA_FILE_1_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du \telkey{FICHIER DE DONNEES BINAIRE 1}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}""",
        ang = """Format of the \telkey{BINARY DATA FILE 1}.
Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}""",
    ),
#   -----------------------------------
    BINARY_ATMOSPHERIC_DATA_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du \telkey{FICHIER BINAIRE DE DONNEES ATMOSPHERIQUES}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}""",
        ang = """Format of the \telkey{BINARY ATMOSPHERIC DATA FILE}.
Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}""",
    ),
#   -----------------------------------
    BINARY_BOUNDARY_DATA_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du \telkey{FICHIER BINAIRE DE DONNEES DE FRONTIERE}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour Telemac;
\item SERAFIND: format standard double precision pour Telemac;
\item MED     : format MED base sur HDF5.
\end{itemize}""",
        ang = """Format of the \telkey{BINARY BOUNDARY DATA FILE}.
Possible values are:
\begin{itemize}
\item SERAFIN : classical single precision format in Telemac;
\item SERAFIND: classical double precision format in Telemac;
\item MED     : MED format based on HDF5.
\end{itemize}""",
    ),
#   -----------------------------------
    RESTART_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du \telkey{FICHIER POUR SUITE}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}
Seul les formats double precision assurent une suite parfaite.""",
        ang = """Format of the \telkey{RESTART FILE}.
Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}
Only double precision formats ensure a perfect restart.""",
    ),
#   -----------------------------------
    FILE_FOR_2D_CONTINUATION_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du \telkey{FICHIER POUR SUITE 2D}.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour \tel ;
\item SERAFIND: format standard double precision pour \tel ;
\item MED     : format MED double precision base sur HDF5.
\end{itemize}""",
        ang = """Format of the \telkey{FILE FOR 2D CONTINUATION}.
Possible choices are:
\begin{itemize}
\item SERAFIN : classical single precision format in \tel,
\item SERAFIND: classical double precision format in \tel,
\item MED     : MED double precision format based on HDF5.
\end{itemize}""",
    ),
#   -----------------------------------
    NAMES = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        GEOMETRY_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            fr = """Nom du fichier contenant le maillage du calcul a realiser.""",
            ang = """Name of the file containing the mesh. This file may also
contain the topography and the friction coefficients.""",
        ),
#       -----------------------------------
        FORTRAN_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = 'DEFAUT',
            fr = """Nom du fichier FORTRAN a soumettre, contenant les
sous-programmes specifiques au modele.""",
            ang = """Name of the FORTRAN file to be submitted, including specific
subroutines of the model.""",
        ),
#       -----------------------------------
        STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier contenant les parametres du calcul a realiser.""",
            ang = """Name of the file containing the parameters of the computation.
Written by the user.""",
        ),
#       -----------------------------------
        BOUNDARY_CONDITIONS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            fr = """Nom du fichier contenant les types de conditions aux limites.
Ce fichier est rempli de facon automatique par le mailleur au moyen de
couleurs affectees aux noeuds des frontieres du domaine de calcul.""",
            ang = """Name of the file containing the types of boundary conditions.
This file is filled automatically by the mesh generator through
colours that are assigned to the boundary nodes.""",
        ),
#       -----------------------------------
        PREVIOUS_COMPUTATION_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom d''un fichier contenant les resultats d''un calcul precedent
realise sur le meme maillage et dont le dernier pas de temps enregistre
va fournir les conditions initiales pour une suite de de calcul.
Dans le cas d''une suite de calcul que l''on souhaite parfaite,
le \telkey{FICHIER DU CALCUL PRECEDENT} doit etre le
\telkey{FICHIER POUR SUITE} du dernier calcul, ce dernier fichier
etant alors un fichier de sortie du dernier calcul.
Le \telkey{FORMAT DU FICHIER DU CALCUL PRECEDENT} et le
\telkey{FORMAT DU FICHIER POUR SUITE} doivent alors etre mis a
 ''SERAFIND'' ou ''MED''.""",
            ang = """Name of a file containing the results of an earlier computation
which was made on the same mesh. The last recorded time step will
provide the initial conditions for the new computation.
In case of a perfect continued computation, the
\telkey{PREVIOUS COMPUTATION FILE} has to be the \telkey{RESTART FILE}
of the last computation.
This last file is then an output file of the last computation.
The \telkey{PREVIOUS COMPUTATION FILE FORMAT} and the
\telkey{RESTART FILE FORMAT} have to be set with ''SERAFIND''
or ''MED''.""",
        ),
#       -----------------------------------
        3D_RESULT_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier dans lequel seront ecrits les resultats 3D du
calcul avec la periodicite donnee par le mot cle \telkey{PERIODE POUR
LES SORTIES GRAPHIQUES}.""",
            ang = """Name of the file into which the 3D results of the computation
are written, the periodicity being given by the keyword:
\telkey{GRAPHIC PRINTOUT PERIOD}.""",
        ),
#       -----------------------------------
        BOTTOM_TOPOGRAPHY_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier eventuel contenant la bathymetrie associee au
maillage.
Si ce mot-cle est utilise, c''est cette bathymetrie qui sera utilisee
pour le calcul.""",
            ang = """Name of the possible file containing the bathymetric data.
Where this keyword is used, these bathymetric data shall be used in
the computation.""",
        ),
#       -----------------------------------
        2D_RESULT_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier dans lequel seront ecrits les resultats 2D du
calcul avec la periodicite donnee par le mot cle \telkey{PERIODE POUR
LES SORTIES GRAPHIQUES}.""",
            ang = """Name of the file into which the 2D results of the computation
are written with a period given by the keyword
\telkey{GRAPHIC PRINTOUT PERIOD}.""",
        ),
#       -----------------------------------
        FORMATTED_DATA_FILE_1 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees formate mis a la disposition de
l''utilisateur.""",
            ang = """Formatted data file available to the user.""",
        ),
#       -----------------------------------
        FORMATTED_DATA_FILE_2 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees formate mis a la disposition de
l''utilisateur.""",
            ang = """Formatted data file available to the user.""",
        ),
#       -----------------------------------
        BINARY_DATA_FILE_1 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees code en binaire mis a la disposition
de l''utilisateur.""",
            ang = """Data file in binary mode available to the user.""",
        ),
#       -----------------------------------
        BINARY_DATA_FILE_2 = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees code en binaire mis a la disposition
de l''utilisateur.""",
            ang = """Data file in binary mode available to the user.""",
        ),
#       -----------------------------------
        ASCII_ATMOSPHERIC_DATA_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees ASCII contenant les informations
atmospheriques variables en temps.""",
            ang = """ASCII data file containing the atmospheric data varying in
time.""",
        ),
#       -----------------------------------
        BINARY_ATMOSPHERIC_DATA_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees code en binaire contenant les informations
atmospheriques variables en temps et en espace sur le maillage.""",
            ang = """Binary-coded data file containing the atmospheric data varying
in time and space on the mesh.""",
        ),
#       -----------------------------------
        BINARY_BOUNDARY_DATA_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Fichier de donnees code en binaire contenant les informations
de conditions aux limites variables en temps et en espace
provenant de jeux de donnees externes par exemple.""",
            ang = """Binary-coded data file containing the boundary conditions data
varying in time and space.""",
        ),
#       -----------------------------------
        CULVERTS_DATA_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Fichier de description des buses/ponts presents dans le modele.""",
            ang = """Description of culverts/bridges existing in the model.""",
        ),
#       -----------------------------------
        SEDIMENTOLOGICAL_RESULT_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier dans lequel seront ecrits les variables
decrivant le fond vaseux (epaisseurs et concentrations\ldots)
avec la periodicite donnee par le mot cle \telkey{PERIODE POUR
LES SORTIES GRAPHIQUES}.""",
            ang = """Name of the file into which the sedimentological computation
results (thickness and concentration of the mud bed\ldots) shall be
written, the periodicity being given by the keyword
\telkey{GRAPHIC PRINTOUT PERIOD}.""",
        ),
#       -----------------------------------
        PREVIOUS_COMPUTATION_SEDIMENTOLOGICAL_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom d''un fichier contenant les variables sedimentologiques
decrivant le fond vaseux, resultats d''un calcul precedent realise
sur le meme maillage et dont le dernier pas de temps enregistre
va fournir les conditions initiales pour une suite de de calcul.""",
            ang = """Name of a file containing the sedimentological parameters
(thickness and concentration of the bed\ldots), results of an earlier
computation which was made on the same mesh. The last recorded time
step will provide the initial conditions for the new computation.""",
        ),
#       -----------------------------------
        NAMES_OF_TRACERS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 2, max= 2,
            fr = """Noms des traceurs en 32 caracteres, 16 pour le nom,
16 pour l''unite.""",
            ang = """Name of tracers in 32 characters, 16 for the name,
16 for the unit.""",
        ),
#       -----------------------------------
        LIQUID_BOUNDARIES_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Fichier de variations en temps des conditions aux limites.""",
            ang = """File containing the variations in time of boundary conditions.""",
        ),
#       -----------------------------------
        SISYPHE_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """Fichier des parametres de \sisyphe en cas de couplage
interne.""",
            ang = """\sisyphe parameter file in case of internal coupling.""",
        ),
#       -----------------------------------
        COUPLING_WITH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """Liste des codes avec lesquels on couple TELEMAC-3D :
\begin{itemize}
\item \sisyphe : couplage interne avec \sisyphe ;
\item \tomawac : couplage interne avec \tomawac ;
\item WAQTEL : couplage interne avec WAQTEL ;
\item DELWAQ : sortie de fichiers de resultats pour Delwaq.
\end{itemize}""",
            ang = """List of codes to be coupled with TELEMAC-3D:
\begin{itemize}
\item \sisyphe: internal coupling with \sisyphe,
\item \tomawac: internal coupling with \tomawac,
\item WAQTEL: internal coupling with WAQTEL,
\item DELWAQ: will yield results file for DELWAQ.
\end{itemize}""",
        ),
#       -----------------------------------
        VOLUMES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        EXCHANGE_AREAS_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        VERTICAL_FLUXES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        SALINITY_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        BOTTOM_SURFACES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        EXCHANGES_BETWEEN_NODES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        NODES_DISTANCES_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        TEMPERATURE_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        VELOCITY_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        DIFFUSIVITY_DELWAQ_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats pour le couplage avec DELWAQ.""",
            ang = """Results file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        DELWAQ_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de commande pour le couplage avec DELWAQ.""",
            ang = """Steering file for coupling with DELWAQ.""",
        ),
#       -----------------------------------
        STAGE_DISCHARGE_CURVES_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier contenant les courbes de tarage.""",
            ang = """Name of the file containing stage-discharge curves.""",
        ),
#       -----------------------------------
        SOURCES_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier contenant les informations variables
en temps des sources.""",
            ang = """Name of the file containing time-dependent
information on sources.""",
        ),
#       -----------------------------------
        BINARY_RESULTS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats code en binaire mis a la disposition
de l''utilisateur.""",
            ang = """Additional binary-coded result file made available
to the user.""",
        ),
#       -----------------------------------
        FORMATTED_RESULTS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultats formate mis a la disposition de
l''utilisateur.""",
            ang = """Formatted file of results made available to the user.""",
        ),
#       -----------------------------------
        TOMAWAC_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """Fichier des parametres de \tomawac en cas de couplage
interne.""",
            ang = """\tomawac parameter file in case of internal coupling.""",
        ),
#       -----------------------------------
        OIL_SPILL_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Contient les donnees pour le
modele de nappes d''hydrocarbures.""",
            ang = """Contains data for the oil spill model.""",
        ),
#       -----------------------------------
        RESTART_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier dans lequel seront ecrits les resultats du
dernier calcul pour obtenir une suite de calcul parfaite.
C''est donc un fichier de sortie pour le calcul en cours,
qui servira de fichier d''entree lors de la suite de calcul que l''on
souhaite parfaite (le mot-cle \telkey{FICHIER DU CALCUL PRECEDENT}
est alors utilise).
Le \telkey{FORMAT DU FICHIER POUR SUITE} et le
\telkey{FORMAT DU FICHIER DU CALCUL PRECEDENT} doivent alors etre mis a
 ''SERAFIND'' ou ''MED''.""",
            ang = """Name of the file into which the last computation results shall
be written in order to get a perfect continued computation.
It is then an output file for the current computation,
which will be used as an input file when a continued computation
is expected to be perfect (the keyword
\telkey{PREVIOUS COMPUTATION FILE} is then used).
The \telkey{RESTART FILE FORMAT} and the
\telkey{PREVIOUS COMPUTATION FILE FORMAT} have to be set with
 ''SERAFIND'' or ''MED''.""",
        ),
#       -----------------------------------
        HARMONIC_CONSTANTS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier contenant les constantes harmoniques extraites
du fichier du modele de maree (JMJ)
ou autres atlas (FES, NEA, PREVIMER).""",
            ang = """Name of the file containing the harmonic constants extracted
from the tidal model file (JMJ) or other atlases (FES, NEA, PREVIMER).""",
        ),
#       -----------------------------------
        TIDAL_MODEL_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Fichier de geometrie du modele dont sont extraites
les constantes harmoniques (JMJ seulement).""",
            ang = """Geometry file of the model from which harmonic constituents
are extracted (JMJ only).""",
        ),
#       -----------------------------------
        ASCII_DATABASE_FOR_TIDE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom de la base de donnees de constantes harmoniques
tirees du \telkey{FICHIER DU MODELE DE MAREE}.""",
            ang = """File name for the tide data base of harmonic constituents
extracted from the \telkey{TIDAL MODEL FILE}.""",
        ),
#       -----------------------------------
        BINARY_DATABASE_1_FOR_TIDE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier de la base de donnees binaire 1. Dans le cas des
donnees satellitaires de l''OSU (type TPXO), ce fichier correspond aux
donnees de niveau d''eau, par exemple h\_tpxo7.2.""",
            ang = """File name for the binary database 1 of tidal harmonic
constants. In the case of the OSU satellite altimetry model (TPXO type),
this file should be for free surface level, for instance h\_tpxo7.2.""",
        ),
#       -----------------------------------
        BINARY_DATABASE_2_FOR_TIDE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier de la base de donnees binaire 2. Dans le cas des
donnees satellitaires de l''OSU (type TPXO), ce fichier correspond aux
donnees de vitesse de marees, par exemple u\_tpxo7.2.""",
            ang = """File name for the binary database 2 of tidal harmonic
constants. In the case of the OSU satellite altimetry model (TPXO type),
this file should be for tidal velocities, for instance u\_tpxo7.2.""",
        ),
#       -----------------------------------
        DROGUES_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Fichier de resultat avec les positions des flotteurs.""",
            ang = """Results file with positions of drogues.""",
        ),
#       -----------------------------------
        FILE_FOR_2D_CONTINUATION = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Fichier utilise en cas de suite 2D.""",
            ang = """File to be used in case of 2D continuation.""",
        ),
#       -----------------------------------
        NAMES_OF_2D_PRIVATE_VARIABLES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 4, max= 4,
            fr = """Noms des variables dans les tableaux prives 2D en 32
caracteres, 16 pour le nom 16 pour l''unite. Elles seront lues dans le
\telkey{FICHIER DE GEOMETRIE} si elles y sont.
Nombre maximum de 4 noms.""",
            ang = """Name of variables in 2D private arrays in 32 characters,
16 for the name, 16 for the unit. If present, will be read
in the \telkey{GEOMETRY FILE}. Maximum number of 4 names.""",
        ),
#       -----------------------------------
        WAQTEL_STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """Fichier des parametres physiques pour les
processus de qualite d''eau (internes, pas ceux de DELWAQ).""",
            ang = """File for physical parameters of water quality processes
(local ones of \telemac-TRACER not those of DELWAQ).""",
        ),
    ),
#   -----------------------------------
    TYPE_OF_BINARY = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        3D_RESULT_FILE_BINARY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ['STD','IBM','I3E'],
            defaut = 'STD',
            fr = """Type du binaire utilise pour l''ecriture du
\telkey{FICHIER DES RESULTATS 3D}.
Ce type depend de la machine sur laquelle le fichier a ete genere.
Les valeurs possibles sont :
\begin{itemize}
\item IBM pour un fichier cree sur IBM ;
\item I3E pour un fichier cree sur HP ;
\item STD.
\end{itemize}
Il s''agit alors d''ordres READ et WRITE normaux.""",
            ang = """Binary file type used for writing the
\telkey{3D RESULT FILE}.
This type depends on the machine on which the file was generated.
The possible values are as follows:
\begin{itemize}
\item IBM, for a file on an IBM (from a CRAY),
\item I3E, for a file on an HP (from a CRAY),
\item STD, binary type of the machine on which the user is working.
\end{itemize}
In that case, normal READ and WRITE commands are used.""",
        ),
#       -----------------------------------
        2D_RESULT_FILE_BINARY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ['STD','IBM','I3E'],
            defaut = 'STD',
            fr = """Type du binaire utilise pour l''ecriture du
\telkey{FICHIER DES RESULTATS 2D}.
Ce type depend de la machine sur laquelle le fichier a ete genere.
Les valeurs possibles sont :
\begin{itemize}
\item IBM pour un fichier cree sur IBM ;
\item I3E pour un fichier cree sur HP ;
\item STD.
\end{itemize}
Il s''agit alors d''ordres READ et WRITE normaux.""",
            ang = """Binary file type used for writing the
\telkey{2D RESULT FILE}.
This type depends on the machine on which the file was generated.
The possible values are as follows:
\begin{itemize}
\item IBM, for a file on an IBM (from a CRAY),
\item I3E, for a file on an HP (from a CRAY),
\item STD, binary type of the machine on which the user is working.
\end{itemize}
In that case, normal READ and WRITE commands are used.""",
        ),
#       -----------------------------------
        SEDIMENTOLOGICAL_RESULT_FILE_BINARY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ['STD','IBM','I3E'],
            defaut = 'STD',
            fr = """Type du binaire utilise pour l''ecriture du fichier
des resultats sedimentologiques.
Ce type depend de la machine sur laquelle le fichier a ete genere.
Les valeurs possibles sont :
\begin{itemize}
\item IBM pour un fichier cree sur IBM ;
\item I3E pour un fichier cree sur HP ;
\item STD.
\end{itemize}
Il s''agit alors d''ordres READ et WRITE normaux.""",
            ang = """Binary file type used for writing the results file.
This type depends on the machine on which the file was generated.
The possible values are as follows:
\begin{itemize}
\item IBM, for a file on an IBM (from a CRAY),
\item I3E, for a file on an HP (from a CRAY),
\item STD, binary type of the machine on which the user is working.
\end{itemize}
In that case, normal READ and WRITE commands are used.""",
        ),
    ),
)
# -----------------------------------------------------------------------
EQUATIONS,_PROPAGATION = PROC(nom= "EQUATIONS,_PROPAGATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    PROPAGATION_STEP = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [True ],
        fr = """Prise en compte ou non des termes de propagation.
Cette etape doit actuellement etre effectuee.""",
        ang = """Takes into account the propagation terms or not.
This step has to be done currently.""",
    ),
)
# -----------------------------------------------------------------------
SEDIMENT_INFO = PROC(nom= "SEDIMENT_INFO",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    SEDIMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Permet de prendre en compte le transport sedimentaire.""",
        ang = """If YES, sediment transport is modelled.""",
    ),
#   -----------------------------------
    SOLVER_FOR_DIFFUSION_OF_THE_SEDIMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["conjugate gradient","conjugate residual","conjugate gradient on a normal equation","minimum error","squared conjugate gradient","cgstab","gmres","direct solver"],
        defaut = ["conjugate gradient on a normal equation"],
        fr = """Permet de choisir le solveur utilise pour la resolution de
la diffusion du sediment.
Les choix possibles sont :
\begin{itemize}
\item 1 : gradient conjugue ;
\item 2 : residu conjugue ;
\item 3 : gradient conjugue sur equation normale ;
\item 4 : erreur minimale ;
\item 5 : gradient conjugue carre ;
\item 6 : CGSTAB ;
\item 7 : GMRES ;
\item 8 : solveur direct.
\end{itemize}""",
        ang = """Choice of the solver for the sediment equation.
Possible choices are:
\begin{itemize}
\item 1: conjugate gradient,
\item 2: conjugate residual,
\item 3: conjugate gradient on a normal equation,
\item 4: minimum error,
\item 5: squared conjugate gradient,
\item 6: CGSTAB,
\item 7: GMRES,
\item 8: direct solver.
\end{itemize}""",
    ),
#   -----------------------------------
    PRECONDITIONING_FOR_DIFFUSION_OF_THE_SEDIMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["no preconditioning","diagonal","diagonal condensed","diagonal with absolute values","Crout","Gauss-Seidel EBE","Matrix defined by the user","diagonal and Crout","direct solver on the vertical","diagonal condensed and Crout","diagonal and direct solver on the vertical"],
        defaut = ["diagonal"],
        fr = """Permet de preconditionner le systeme relatif
a la diffusion du sediment. Les choix possibles sont :
\begin{itemize}
\item 0 : aucun ;
\item 2 : diagonal ;
\item 3 : diagonal avec matrice condensee ;
\item 5 : diagonal avec valeurs absolues ;
\item 7 : Crout ;
\item 11 : Gauss-Seidel EBE ;
\item 13 : matrice fournie par l''utilisateur ;
\item 14 : diagonal et Crout ;
\item 17 : solveur direct sur la verticale ;
\item 21 : diagonal condensee et Crout ;
\item 34 : diagonal et solveur direct sur la verticale.
\end{itemize}
Certains preconditionnements sont cumulables
(les diagonaux 2 ou 3 avec les autres).
Pour cette raison on ne retient que les nombres premiers pour
designer les preconditionnements. Si l''on souhaite en cumuler
plusieurs on formera le produit des options correspondantes.""",
        ang = """Choice of the preconditioning in the sediment diffusion
system that the convergence is speeded up when it is being solved.
Possible choices are:
\begin{itemize}
\item 0: no preconditioning,
\item 2: diagonal,
\item 3: diagonal with the condensed matrix,
\item 5: diagonal with absolute values,
\item 7: Crout,
\item 11: Gauss-Seidel EBE,
\item 13: matrix defined by the user,
\item 14: diagonal and Crout,
\item 17: direct solver on the vertical,
\item 21: diagonal condensed and Crout,
\item 34: diagonal and direct solver on the vertical.
\end{itemize}
Some operations (either 2 or 3 diagonal preconditioning) can be
performed concurrently with the others.
Only prime numbers are therefore kept to denote the preconditioning
operations. When several of them are to be performed concurrently,
the product of relevant options shall be done.""",
    ),
#   -----------------------------------
    DENSITY_OF_THE_SEDIMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [2650.],
        fr = """Fixe la valeur de la masse volumique du sediment (kg/m$^3$).""",
        ang = """Value of the sediment density (kg/m$^3$).""",
    ),
#   -----------------------------------
    CRITICAL_SHEAR_STRESS_FOR_DEPOSITION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.2],
        fr = """Fixe la valeur de la contrainte de cisaillement au fond
au dessous de laquelle se produit le depot des sediments cohesifs.""",
        ang = """Value of the critical bottom shear stress under which
deposition of cohesive sediments occurs.""",
    ),
#   -----------------------------------
    BED_LAYERS_THICKNESS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [5.E-3],
        fr = """Epaisseur de reference pour creer de nouvelles couches
de vase. Ce parametre est utilise seulement dans le cas
\telkey{OPTION DU MODELE DE TASSEMENT} = 2
(modele de Gibson (Lenormant)). Avec ce modele, le sediment
qui se depose sur le fond est tout d''abord stocke dans une couche
tampon appelee couche des depots frais. C''est seulement quand
l''epaisseur de cette couche atteint la valeur donnee par le mot
cle \telkey{EPAISSEUR DES COUCHES DU FOND VASEUX} qu''une nouvelle
couche est cree au niveau du lit de vase.""",
        ang = """Reference thickness considered for the creation of new
bed layers.
This parameter is used if \telkey{CONSOLIDATION MODEL} = 2
(Gibson model (Lenormant)).
With this model, the sediment which settles on
the bottom arrives at first in the fresh deposit layer. When
the thickness of this layer is equal to the
\telkey{BED LAYERS THICKNESS},
a new mud layer is added to the mud bed.""",
    ),
#   -----------------------------------
    TIME_STEP_FOR_CONSOLIDATION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1200.],
        fr = """Valeur du pas de temps pour le modele de consolidation qui
peut etre plus grand que le pas de temps hydrodynamique car le
phenomene est tres lent. Ce parametre est utilise si
\telkey{OPTION DU MODELE DE TASSEMENT} = 1 (Modele multicouches
empirique) ou 2 (Modele de Gibson (Lenormant)).""",
        ang = """Time step for the modelling consolidation, which can
be greater than the hydrodynamic time step. This parameter is
used if \telkey{CONSOLIDATION MODEL} = 1 (Empirical multilayer model)
or 2 (Gibson model (Lenormant)).""",
    ),
#   -----------------------------------
    MAXIMUM_CONCENTRATION_OF_THE_CONSOLIDATED_MUD = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [500.],
        fr = """Concentration maximale pouvant etre atteinte par une couche
de vase lors du tassement.
Ce parametre est utilise si \telkey{OPTION DU MODELE DE TASSEMENT} = 2
(Modele de Gibson - Lenormant).""",
        ang = """Maximum concentration which may be reached by a mud layer
during consolidation.
This value is used if \telkey{CONSOLIDATION MODEL} = 2
(Gibson model (Lenormant)).""",
    ),
#   -----------------------------------
    EROSION_COEFFICIENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [2.E-3],
        fr = """Valeur du coefficient d''erosion utilise dans la formule
de Partheniades en kg/m$^2$/s.""",
        ang = """Value of the erosion coefficient used in Partheniades
formula in kg/m$^2$/s.""",
    ),
#   -----------------------------------
    CONSTANT_SEDIMENT_SETTLING_VELOCITY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.01],
        fr = """Vitesse de chute constante en m/s (> 0 depuis v6.3).
Valeur imposee si
\telkey{INFLUENCE DE LA TURBULENCE SUR LA VITESSE DE CHUTE}
= NON.""",
        ang = """Constant sediment settling velocity in m/s (>0 since v6.3).
Prescribed value if
\telkey{INFLUENCE OF TURBULENCE ON SETTLING VELOCITY} = NO.""",
    ),
#   -----------------------------------
    RESIDENCE_TIME_FOR_MUD = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=30, max=30,
        fr = """Tableau contenant les temps de sejour en heure et centieme
relatifs a chacune des couches discretisant le fond vaseux
(la premiere valeur correspond a la couche du fond et la derniere
correspond a la couche superficielle).
Valeurs necessaires si \telkey{OPTION DU MODELE DE TASSEMENT} = 1
(Modele multicouches empirique).""",
        ang = """Array which contains the residence times of the mud bed
layers (the first value is related to the bottom layer and the
last one to the top layer).
These values are needed when \telkey{CONSOLIDATION MODEL} = 1
(Empirical multilayer model).""",
    ),
#   -----------------------------------
    COHESIVE_SEDIMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Permet de dire si le sediment est cohesif ou non.""",
        ang = """Tells if the sediment is cohesive or not.""",
    ),
#   -----------------------------------
    MIXED_SEDIMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Si OUI, calcul en sediments mixtes, il y aura un sediment
cohesif et un sediment non cohesif.""",
        ang = """If YES, calculation of mixed sediment transport, there will be
one cohesive sediment and one non cohesive sediment.""",
    ),
#   -----------------------------------
    SETTLING_VELOCITY_OF_SANDS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Vitesse de chute du sediment non cohesif.""",
        ang = """Non cohesive sediment settling velocity.""",
    ),
#   -----------------------------------
    INITIAL_PERCENTAGE_OF_NON_COHESIVE_SEDIMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Pourcentage initial du sediment non cohesif (mixte).""",
        ang = """Initial percentage of non cohesive sediment (mixed sediments).""",
    ),
#   -----------------------------------
    ADVECTION_DIFFUSION_SCHEME_WITH_SETTLING_VELOCITY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Choix de schema vertical pour la diffusion et le depot du
sediment :
\begin{itemize}
\item 0 : Diffusion implicite ;
\item 1 : Schema implicite de convection-diffusion
(tridiagonal matrix solver) ;
\item 2 : Convection faible  \telfile{sed\_fall.f}
\end{itemize}""",
        ang = """Choice of the vertical scheme for diffusion and settling of
sediment:
\begin{itemize}
\item 0: Implicit-diffusion scheme,
\item 1: Implicit-convection scheme (Tridiagonal matrix solver),
\item 2: \telfile{set\_fall.f}
\end{itemize}""",
    ),
#   -----------------------------------
    HINDERED_SETTLING = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Decide si la formulation entravee doit etre utilisee
pour calculer la vitesse de chute de la vase.""",
        ang = """Decides if hindered formulation is to be used to
compute settling velocity for mud.""",
    ),
#   -----------------------------------
    WEAK_SOIL_CONCENTRATION_FOR_MUD = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.0],
        fr = """Concentration limite en kg/m$^3$ au-dela de laquelle
la couche de vase fluide devient solide.
Cette valeur est demandee lorsque
\telkey{VITESSE DE CHUTE ENTRAVEE} = OUI.""",
        ang = """The sediment concentration at which sediment
forms a weak soil in kg/m$^3$. These values are needed when
\telkey{HINDERED SETTLING} = YES.""",
    ),
#   -----------------------------------
    THRESHOLD_CONCENTRATION_FOR_HINDERED_SETTLING = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.0],
        fr = """Concentration de sediment a laquelle la chute entravee est
initiee. Ces valeurs sont necessaires lorsque
\telkey{VITESSE DE CHUTE ENTRAVEE} = OUI.""",
        ang = """The sediment concentration at which hindered settling is
initiated. These values are needed when
\telkey{HINDERED SETTLING} = YES.""",
    ),
#   -----------------------------------
    HINDERED_SETTLING_FORMULA = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Type de vitesse de chute entravee :
\begin{itemize}
\item 1 : Whitehouse et al. (2000) - fonctionne ;
\item 2 : Winterwerp (1999) - ne fonctionne pas actuellement.
\end{itemize}""",
        ang = """Type of hindered settling:
\begin{itemize}
\item 1: Whitehouse et al. (2000) - working,
\item 2: Winterwerp (1999) - not currently working.
\end{itemize}""",
    ),
#   -----------------------------------
    FLOCCULATION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Decide si la formulation entravee doit etre utilisee
pour calculer la vitesse de chute pour la vase.""",
        ang = """Decides if hindered formulation is to be used to
compute settling velocity for mud.""",
    ),
#   -----------------------------------
    FLOCCULATION_FORMULA = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Formule pour floculation :
\begin{itemize}
\item 1: Van Leussen ;
\item 2: Soulsby et  al. (2013).
\end{itemize}""",
        ang = """Type of flocculation formula:
\begin{itemize}
\item 1: Van Leussen,
\item 2: Soulsby et  al. (2013).
\end{itemize}""",
    ),
#   -----------------------------------
    FLOCCULATION_COEFFICIENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.3],
        fr = """Coefficient intervenant dans la modelisation de l''influence de
la turbulence sur la floculation, il intervient plus precisement dans
le terme de formation des flocs par les contraintes turbulentes
(coefficient $a$ de la formule de Van Leussen).
Valeur a imposer si
\telkey{INFLUENCE DE LA TURBULENCE SUR LA VITESSE DE CHUTE}
= OUI.""",
        ang = """When the influence of turbulence on the settling velocity
is modelled, this coefficient traduces the formation of flocs by
turbulence (coefficient $a$ of Van Leussen formula).
Value to be imposed if
\telkey{INFLUENCE OF TURBULENCE ON SETTLING VELOCITY} = YES.""",
    ),
#   -----------------------------------
    COEFFICIENT_RELATIVE_TO_FLOC_DESTRUCTION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.09],
        fr = """Coefficient intervenant dans la modelisation de l''influence de
la turbulence sur la floculation, il intervient plus precisement dans
le terme de destruction des flocs par les contraintes turbulentes
(coefficient $b$ de la formulede Van Leussen).
Valeur a imposer si
\telkey{INFLUENCE DE LA TURBULENCE SUR LA VITESSE DE CHUTE}
= OUI.""",
        ang = """When the influence of turbulence on the settling velocity
is modelled, this coefficient traduces the breaking of flocs by
turbulence (coefficient $b$ of Van Leussen formula).
Value to be imposed if
\telkey{INFLUENCE OF TURBULENCE ON SETTLING VELOCITY} = YES.""",
    ),
#   -----------------------------------
    CONSOLIDATION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Logique pour la prise en compte du tassement des depots vaseux
a l''aide d''un modele multicouches : les couches discretisant le fond
sont caracterisees par leur temps de sejour, temps au bout duquel la
vase presente dans cette couche bascule dans la couche suivante plus
consolidee.""",
        ang = """If this key word is equal to YES, consolidation is simulated
thanks to a multi-layers model: the bed layers are characterized by
their residence time which is the time after which the quantity of
mud which remains in a layer goes into a more consolidated layer.""",
    ),
)
# -----------------------------------------------------------------------
PHYSICS = PROC(nom= "PHYSICS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NON_COHESIVE_BED_POROSITY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 0.4,
        fr = """La concentration massique du lit \telfile{CFDEP} est definie par
\telfile{CFDEP} = (1-\telfile{XKV}) $\times$ \telfile{RHOS}.
Ce parametre est utilise pour les sediments non-cohesifs.""",
        ang = """The bed volume concentration
\telfile{CFDEP} = (1-\telfile{XKV}) $\times$ \telfile{RHOS}
is used to calculate the bed evolution of non-cohesive sand transport.""",
    ),
#   -----------------------------------
    READ_CRITICAL_BED_SHEAR_STRESS_PER_LAYER = SIMP(statut ='f',
#   -----------------------------------
        typ = bool, min=0, max='**',
        defaut = [False],
        fr = """Lecture de la contrainte critique d''erosion a
partir du \telkey{FICHIER DE GEOMETRIE}.""",
        ang = """Decides if erosion shear stress at each layer is
read from \telkey{GEOMETRY FILE}.""",
    ),
)
# -----------------------------------------------------------------------
EQUATIONS,_INITIAL_CONDITIONS = PROC(nom= "EQUATIONS,_INITIAL_CONDITIONS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    INITIAL_CONDITIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['ZERO ELEVATION','CONSTANT ELEVATION','ZERO DEPTH','CONSTANT DEPTH','SPECIAL','PARTICULAR','TPXO SATELLITE ALTIMETRY'],
        defaut = 'ZERO ELEVATION',
        fr = """Permet de definir les conditions initiales sur
les hauteurs d''eau notamment.
Les valeurs possibles sont :
\begin{itemize}
\item COTE NULLE : Initialise la cote de surface libre a 0.
           Les hauteurs d''eau initiales sont alors retrouvees en
           faisant la difference entre les cotes de surface libre
           et du fond ;
\item COTE CONSTANTE : Initialise la cote de surface libre a la
           valeur donnee par le mot-cle COTE INITIALE. Les hauteurs
           d''eau initiales sont calculees comme precedemment ;
\item HAUTEUR NULLE : Initialise les hauteurs d''eau a 0 ;
\item HAUTEUR CONSTANTE : Initialise les hauteurs d''eau a la valeur
           donnee par le mot-cle HAUTEUR INITIALE ;
\item ALTIMETRIE SATELLITE TPXO : Les conditions initiales sur la
hauteur d''eau et les vitesses sont etablies sur la base des donnees
satellite TPXO dont les 8 premieres composantes ont ete extraites et
sauvees dans le fichier ;
\telkey{BASE BINAIRE DE DONNEES DE MAREE} ;
\item PARTICULIERES : Les conditions initiales sur la hauteur d''eau
doivent etre precisees dans le sous-programme \telfile{CONDIN}.
\end{itemize}""",
        ang = """Makes it possible to define the initial conditions of
the water depth.
The possible values are as follows:
\begin{itemize}
\item ZERO ELEVATION: Initializes the free surface elevation to 0.
The initial water depths are then found by computing the difference
between the free surface and the bottom,
\item CONSTANT ELEVATION: Initializes the water elevation to the value
given by the keyword \telkey{INITIAL ELEVATION}.
The initial water depths are computed as in the previous case,
\item ZERO DEPTH: Initializes the water depths to 0.
\item CONSTANT DEPTH: Initializes the water depths to the value given
by the keyword \telkey{INITIAL DEPTH},
\item TPXO SATELITE ALTIMETRY: The initial conditions on the free
surface and velocities are established from the satellite program
data given by the harmonic constants database coming from OSU
(e.g. TPXO),
\item SPECIAL or PARTICULAR: The initial conditions with the water depth
should be stated in the \telfile{CONDIN} subroutine.
\end{itemize}""",
    ),
#   -----------------------------------
    INITIAL_ELEVATION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Valeur utilisee avec l''option :
\telkey{CONDITIONS INITIALES} : ''COTE CONSTANTE''.""",
        ang = """Value to be used with the option :
\telkey{INITIAL CONDITIONS} : ''CONSTANT ELEVATION''.""",
    ),
#   -----------------------------------
    INITIAL_DEPTH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Valeur utilisee avec l''option :
\telkey{CONDITIONS INITIALES} : ''HAUTEUR CONSTANTE''.""",
        ang = """Value to be used along with the option:
\telkey{INITIAL CONDITIONS} : ''CONSTANT DEPTH''.""",
    ),
#   -----------------------------------
    OPTION_FOR_THE_TREATMENT_OF_TIDAL_FLATS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Utilise si \telkey{BANCS DECOUVRANTS} est vrai.
Les choix possibles sont :
\begin{itemize}
\item 1 : equations resolues partout avec correction sur les bancs
decouvrants ;
\item 2 : gel des elements decouvrants.
\end{itemize}""",
        ang = """Used if \telkey{TIDAL FLATS} is true. Possible choices are:
\begin{itemize}
\item 1: equations solved everywhere with correction on tidal flats,
\item 2: dry elements frozen.
\end{itemize}""",
    ),
#   -----------------------------------
    IMPLICITATION_FOR_DIFFUSION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1.],
        fr = """Fixe la valeur du coefficient d''implication pour l''etape de
diffusion.
Dans le cas de \telkey{OPTION POUR LA DIFFUSION} = 2, cette valeur est
ecrasee a 0 et un traitement particulier est fait pour la diffusion.""",
        ang = """Sets the value of the implicitation coefficient for the
diffusion step.
When \telkey{OPTION FOR THE DIFFUSION} = 2, this value is changed at 0
and a specific treatment is done for the diffusion.""",
    ),
)
# -----------------------------------------------------------------------
FILES = PROC(nom= "FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    ELEMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        defaut = 'PRISM',
        fr = """Permet de specifier le type d''element utilise pour le calcul.
Les choix possibles sont :
\begin{itemize}
\item PRISME : maillages de triangles empiles ;
\item TETRAEDRE : decoupage en tetraedres des prismes.
\end{itemize}""",
        ang = """Specifies the type of elements used in the computation.
The possible choices are:
\begin{itemize}
\item PRISM: superimposed meshes of triangles,
\item TETRAHEDRON: the same but prisms are split into tetrahedrons.
\end{itemize}""",
    ),
#   -----------------------------------
    LIST_OF_FILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=46, max=46,
        defaut = 'STEERING FILE;DICTIONARY;FORTRAN FILE;GEOMETRY FILE;BOUNDARY CONDITIONS FILE;PREVIOUS COMPUTATION FILE;3D RESULT FILE;BOTTOM TOPOGRAPHY FILE;2D RESULT FILE;FORMATTED DATA FILE 1;FORMATTED DATA FILE 2;BINARY DATA FILE 1;BINARY DATA FILE 2;SEDIMENTOLOGICAL RESULT FILE;PREVIOUS COMPUTATION SEDIMENTOLOGICAL FILE;REFERENCE FILE;RESULT FILE FOR SUBIEF-3D;LIQUID BOUNDARIES FILE;VOLUMES DELWAQ FILE;EXCHANGE AREAS DELWAQ FILE;VERTICAL FLUXES DELWAQ FILE;SALINITY DELWAQ FILE;BOTTOM SURFACES DELWAQ FILE;EXCHANGES BETWEEN NODES DELWAQ FILE;NODES DISTANCES DELWAQ FILE;TEMPERATURE DELWAQ FILE;VELOCITY DELWAQ FILE;DIFFUSIVITY DELWAQ FILE;DELWAQ STEERING FILE;STAGE-DISCHARGE CURVES FILE;SOURCES FILE;BINARY RESULTS FILE;FORMATTED RESULTS FILE;RESTART FILE;OIL SPILL STEERING FILE;HARMONIC CONSTANTS FILE;TIDAL MODEL FILE;ASCII DATABASE FOR TIDE;BINARY DATABASE 1 FOR TIDE;BINARY DATABASE 2 FOR TIDE;DROGUES FILE;FILE FOR 2D CONTINUATION;CULVERTS DATA FILE;ASCII ATMOSPHERIC DATA FILE;BINARY ATMOSPHERIC DATA FILE;BINARY BOUNDARY DATA FILE',
        fr = """Noms des fichiers exploites par le code.""",
        ang = """File names of the used files.""",
    ),
)
# -----------------------------------------------------------------------
PARAMETER_FOR_BED_MATERIAL = PROC(nom= "PARAMETER_FOR_BED_MATERIAL",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    MEAN_DIAMETER_OF_THE_SEDIMENT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = .01,
        fr = """Valeur du diametre D50 pour les sediments non cohesifs.""",
        ang = """Sets the value of the diameter D50 for non cohesive sediments.""",
    ),
)
# -----------------------------------------------------------------------
EQUATIONS,_BOUNDARY_CONDITIONS = PROC(nom= "EQUATIONS,_BOUNDARY_CONDITIONS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    VELOCITY_PROFILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min= 2, max= 2,
        into = ["constant normal profile","ubor and vbor given in the      conlim file","normal velocity given in ubor in the conlim           file","Velocity=square root elevation","like 4 with virtual depth, see help"],
        fr = """Permet de specifier le type de profil horizontal de vitesse.
Les choix possibles sont :
\begin{itemize}
\item 1 : profil normal constant ;
\item 2 : $u$ et $v$ donnes dans le
\telkey{FICHIER DES CONDITIONS AUX LIMITES} ;
\item 3 : vitesse normale donnee dans \telfile{UBOR} dans le
\telkey{FICHIER DES CONDITIONS AUX LIMITES} ;
\item 4 : vitesse normale en $\sqrt{h}$ ;
\item 5 : comme 4 mais hauteur virtuelle calculee avec
la surface libre la plus basse de la frontiere.
\end{itemize}""",
        ang = """Specifies the type of horizontal profile of velocities.
Possible choices are:
\begin{itemize}
\item 1: constant normal profile,
\item 2: $u$ and $v$ given in the
\telkey{BOUNDARY CONDITION FILE},
\item 3: normal velocity given in \telfile{UBOR} in the
\telkey{BOUNDARY CONDITION FILE},
\item 4: normal velocity in $\sqrt{h}$,
\item 5: like 4 but virtual depth based on
the lowest elevation of the boundary.
\end{itemize}""",
    ),
#   -----------------------------------
    TREATMENT_OF_FLUXES_AT_THE_BOUNDARIES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min= 2, max= 2,
        into = ["Priority to prescribed values","Priority to fluxes"],
        fr = """Option s''utilisant uniquement pour les schemas SUPG, PSI et N.
Les choix possibles sont :
\begin{itemize}
\item 1 : priorite aux valeurs imposees ;
\item 2 : priorite aux flux.
\end{itemize}
Avec l''option 2, on ne retrouve pas exactement les valeurs imposees
des traceurs, mais le flux est correct.""",
        ang = """Used so far only with the SUPG, PSI and N schemes.
Possible choices are:
\begin{itemize}
\item 1: priority to prescribed values,
\item 2: priority to fluxes.
\end{itemize}
With option 2, Dirichlet prescribed values are not obeyed,
but the fluxes are correct.""",
    ),
#   -----------------------------------
    VELOCITY_VERTICAL_PROFILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min= 2, max= 2,
        into = ["User defined","Constant","Logarithmic"],
        fr = """Permet de specifier le type de profil vertical de vitesse.
Les choix possibles sont :
\begin{itemize}
\item 0 : programmation utilisateur ;
\item 1 : constant ;
\item 2 : logarithmique.
\end{itemize}""",
        ang = """Specifies the type of vertical profile of velocity.
Possible choices are:
\begin{itemize}
\item 0: defined by user,
\item 1: constant,
\item 2: logarithmic.
\end{itemize}""",
    ),
#   -----------------------------------
    TRACERS_VERTICAL_PROFILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min= 2, max= 2,
        into = ["User defined","Constant","Constant or Rouse if sediment", "Normalized Rouse profile and imposed conc", "Modified Rouse profile accounting for molecular viscosity"],
        fr = """Permet de specifier le type de profil de concentration des
traceurs sur la verticale. Les choix possibles sont :
\begin{itemize}
\item 0 : Programmation utilisateur ;
\item 1 : Constant ;
\item 2 : Rouse equilibrium concentration ;
\item 3 : Rouse (normalise) et concentration imposee.
\item 4 : Rouse modifie avec viscosite moleculaire.
\end{itemize}""",
        ang = """Specifies the type of profiles of tracer concentration on the
vertical. Possible choices are:
\begin{itemize}
\item 0: user defined,
\item 1: constant,
\item 2: Rouse equilibrium, constant (diluted tracer)
or Rouse (sediment),
\item 3: Rouse (normalized) and imposed concentration.
\item 4: Rouse modified with molecular viscosity.
\end{itemize}""",
    ),
#   -----------------------------------
    OPTION_FOR_LIQUID_BOUNDARIES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min= 2, max= 2,
        fr = """On donne un entier par frontiere liquide.
Les choix possibles sont :
\begin{itemize}
\item 1 : conditions aux limites classiques ;
\item 2 : methode de Thompson avec calcul de caracteristiques.
\end{itemize}""",
        ang = """One integer per liquid boundary is given.
Possible choices are:
\begin{itemize}
\item 1: classical boundary conditions,
\item 2: Thompson method based on characteristics.
\end{itemize}""",
    ),
)
# -----------------------------------------------------------------------
SEDIMENT_TRANSPORT = PROC(nom= "SEDIMENT_TRANSPORT",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    PHYSICAL_PARAMETERS = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        SHIELDS_PARAMETER = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.047,
            fr = """Utilise pour determiner la valeur de la contrainte critique
d''entrainement.""",
            ang = """Used to determine the critical bed shear stress value.""",
        ),
    ),
)
# -----------------------------------------------------------------------
BOUNDARY_CONDITIONS = PROC(nom= "BOUNDARY_CONDITIONS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    STAGE_DISCHARGE_CURVES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=10, max=10,
        fr = """Indique si une courbe de tarage doit etre utilisee
pour une frontiere (une valeur par frontiere liquide) :
\begin{itemize}
\item 0 : non ;
\item 1 : Z(Q) ;
\item 2 : Q(Z). Pas encore programme.
\end{itemize}""",
        ang = """Specifies if a discharge-elevation curve must be used
for a given boundary (one value per open boundary):
\begin{itemize}
\item 0: no,
\item 1: Z(Q),
\item 2: Q(Z). Not yet implemented.
\end{itemize}""",
    ),
)
# -----------------------------------------------------------------------
EQUATIONS,_SOURCES = PROC(nom= "EQUATIONS,_SOURCES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    OPTION_FOR_WIND = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """Donne les options pour introduire le vent :
\begin{itemize}
\item 1 : constant en temps et en espace (donne par les mots cles
\telkey{VITESSE DU VENT SUIVANT X} et \telkey{VITESSE DU VENT SUIVANT Y}
) ;
\item 2 : variable en temps donne par fichier formate ;
\item 3 : variable en temps et en espace donne par fichier formate
ou un fichier binaire.
\end{itemize}""",
        ang = """Gives the option for managing the wind:
\begin{itemize}
\item 1: constant in time and space, given by the keywords
\telkey{WIND VELOCITY ALONG X} and \telkey{WIND VELOCITY ALONG Y},
\item 2: variable in time and constant in space, given by formatted
file,
\item 3: variable in time and space, given by formatted file or by
a binary file.
\end{itemize}""",
    ),
#   -----------------------------------
    TYPE_OF_SOURCES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["Normal","Dirac"],
        defaut = ["Normal"],
        fr = """Definit comment les sources sont calculees :
\begin{itemize}
\item 1 : Source portee par une base elements finis ;
\item 2 : Source portee par une fonction de Dirac
(recommande quand il y a beaucoup de sources).
\end{itemize}""",
        ang = """Defines how the sources are computed:
\begin{itemize}
\item 1: Source term multiplied by a finite element basis,
\item 2: Source term multiplied by a Dirac function
(recommended with high numbers of sources).
\end{itemize}""",
    ),
#   -----------------------------------
    TRACER = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        WATER_QUALITY_PROCESS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I',
            defaut = 0,
            fr = """Donne le numero du processus de qualite d''eau
(de 1 a 5) :
\begin{itemize}
\item 0 : rien ;
\item 1 : O2 ;
\item 2 : BIOMASS ;
\item 3 : EUTRO ;
\item 4 : MICROPOL ;
\item 5 : THERMIC.
\end{itemize}""",
            ang = """Gives the water quality process number (from 1 to 5):
\begin{itemize}
\item 0: nothing,
\item 1: O2,
\item 2: BIOMASS,
\item 3: EUTRO,
\item 4: MICROPOL,
\item 5: THERMIC.
\end{itemize}""",
        ),
    ),
)
# -----------------------------------------------------------------------
FRICTION = PROC(nom= "FRICTION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    SKIN_FRICTION_CORRECTION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 0,
        fr = """Prise en compte du frottement de peau :
\begin{itemize}
\item 0 : pas de correction (TAUP = TOB) voir aussi
\telkey{RATIO BETWEEN SKIN FRICTION AND MEAN DIAMETER} :
\telfile{KSPRATIO} ;
\item 1 : fond plat (KSP = \telfile{KSPRATIO} $\times$ \telfile{D50}) ;
\item 2 : prise en compte des rides (non programme).
\end{itemize}""",
        ang = """Formula to predict the skin bed roughness:
\begin{itemize}
\item 0: No correction (TAUP = TOB) see also
\telkey{RATIO ENTRE LA RUGOSITE DE PEAU ET LE DIAMETRE MOYEN}
\telfile{KSPRATIO},
\item 1: Flat bed (KSP = \telfile{KSPRATIO} $\times$ \telfile{D50}),
\item 2: Ripple correction factor (not yet implemented).
\end{itemize}""",
    ),
)
# -----------------------------------------------------------------------
SUSPENSION = PROC(nom= "SUSPENSION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    REFERENCE_CONCENTRATION_FORMULA = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """\begin{itemize}
\item 1 : formule de Zyserman et Fredsoe, formule d''equilibre ;
\item 3 : formule de Van Rijn (1987).
\end{itemize}""",
        ang = """\begin{itemize}
\item 1: Zyserman and Fredsoe, equilibrium formula,
\item 3: Van Rijn formula (1987).
\end{itemize}""",
    ),
)
# -----------------------------------------------------------------------
PHYSICAL_PARAMETERS = PROC(nom= "PHYSICAL_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    OPTION_FOR_CHARACTERISTICS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["strong","weak"],
        defaut = ["strong"],
        fr = """Les choix possibles sont :
\begin{itemize}
\item 1: forme forte ;
\item 2: forme faible.
\end{itemize}""",
        ang = """Possible choices are:
\begin{itemize}
\item 1: strong form,
\item 2: weak form.
\end{itemize}""",
    ),
#   -----------------------------------
    MASS_LUMPING_FOR_WEAK_CHARACTERISTICS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """Fixe le taux de mass-lumping qui est applique a la matrice de
masse lors de l''utilisation des caracteristiques faibles.""",
        ang = """Sets the amount of mass-lumping that is applied to the mass
matrix when using weak characteristics.""",
    ),
#   -----------------------------------
    VALUE_OF_ATMOSPHERIC_PRESSURE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 100000.,
        fr = """Donne la valeur de la pression atmospherique lorsqu''elle est
constante en temps et en espace.""",
        ang = """Gives the value of atmospheric pressure when it is constant
in time and space.""",
    ),
)
# -----------------------------------------------------------------------
CONSOLIDATION_INFO = PROC(nom= "CONSOLIDATION_INFO",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NUMBER_OF_SEDIMENT_BED_LAYERS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Structure verticale du lit cohesif, le nombre de couches
doit etre inferieur a 20.""",
        ang = """Number of cohesive sediment bed layers, should be less
than 20.""",
    ),
#   -----------------------------------
    CONSOLIDATION_MODEL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """Choix du modele de tassement :
\begin{itemize}
\item 1 : Modele multicouches empirique ;
\item 2 : Modele de Gibson (Lenormant).
\end{itemize}""",
        ang = """Choice of the consolidation model:
\begin{itemize}
\item 1: Empirical multilayer model,
\item 2: Gibson model (Lenormant).
\end{itemize}""",
    ),
)
# -----------------------------------------------------------------------
COHESIVE_SEDIMENT_INFO = PROC(nom= "COHESIVE_SEDIMENT_INFO",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    MUD_CONCENTRATIONS_PER_LAYER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """Concentration du lit de vase en g/L (definie par couches) en
commencant par la couche du fond.""",
        ang = """Dry density of the mud-bed layers in g/L starting
form the bottom upwards.""",
    ),
#   -----------------------------------
    CRITICAL_EROSION_SHEAR_STRESS_OF_THE_MUD_LAYERS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """Taux critique d erosion de la vase (N/m$^2$).
Doit etre defini pour chaque couche en commencant par la couche de
fond.""",
        ang = """Critical erosion shear stress of the mud per layer
(N/m$^2$).
Needs to be defined for each layer (N/m$^2$),
starting from the condolidated bottom layer upwards.""",
    ),
#   -----------------------------------
    INITIAL_THICKNESS_OF_SEDIMENT_LAYERS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 2, max= 2,
        fr = """Epaisseurs initiales des sediments (m).""",
        ang = """Sediment layers thickness (m) for initialisation.""",
    ),
)
# -----------------------------------------------------------------------
TURBULENCE = PROC(nom= "TURBULENCE",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    KARMAN_CONSTANT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.4],
        fr = """Valeur de la constante de Von Karman.""",
        ang = """Value of Von Karman''s constant.""",
    ),
#   -----------------------------------
    PRANDTL_NUMBER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1.0],
        fr = """Rapport entre viscosite et diffusivite turbulente.""",
        ang = """Ratio between eddy viscosity and eddy diffusivity.""",
    ),
#   -----------------------------------
    FICTITIOUS_BED_LEVEL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [2.0],
        fr = """Rapport entre le fond fictif et la hauteur de
la premiere  maille utilisee par le modele de turbulence
$k$-$\epsilon$ et pour le transport du sable.""",
        ang = """Ratio between the fictitious bed and
the grid size above the bed.""",
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
        fr = """Si OUI on appelle le sous-programme \telfile{CHECKMESH}
qui verifie la coherence du maillage, points superposes, etc.""",
        ang = """If this key word is equal to YES, a call to subroutine
\telfile{CHECKMESH} will look for errors in the mesh,
superimposed points, etc.""",
    ),
#   -----------------------------------
    MAXIMUM_NUMBER_OF_BOUNDARIES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [30],
        fr = """Nombre maximal de frontieres differentes dans le maillage.
Sert au dimensionnement de la memoire, a augmenter si necessaire.""",
        ang = """Maximal number of boundaries in the mesh.
Used for dimensioning arrays. Can be increased if needed.""",
    ),
#   -----------------------------------
    MAXIMUM_NUMBER_OF_SOURCES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [20],
        fr = """Nombre maximal de points sources dans le maillage,
incluant les sources ponctuelles et 2 fois le nombre de buses.
Sert au dimensionnement de la memoire, a augmenter si necessaire.""",
        ang = """Maximal number of source points in the mesh, including
punctual sources and twice the number of culverts.
Used for dimensioning arrays. Can be increased if needed.""",
    ),
#   -----------------------------------
    MAXIMUM_NUMBER_OF_TRACERS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [20],
        fr = """Nombre maximal de traceurs.
Sert au dimensionnement de la memoire, a augmenter si necessaire.""",
        ang = """Maximal number of tracers.
Used for dimensioning arrays. Can be increased if needed.""",
    ),
#   -----------------------------------
    MAXIMUM_NUMBER_OF_BOUNDARIES_ON_THE_BED = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [30],
        fr = """Nombre maximal de frontieres liquides sur le fond.
Sert au dimensionnement de la memoire, a augmenter si necessaire.""",
        ang = """Maximal number of liquid boundaries on the bed.
Used for dimensioning arrays. Can be increased if needed.""",
    ),
)
# -----------------------------------------------------------------------
TURBULENCE_MODEL = PROC(nom= "TURBULENCE_MODEL",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    BOUNDARY_CONDITIONS = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        OPTION_FOR_THE_BOUNDARY_CONDITIONS_OF_K_EPSILON = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ["no turbulence","Hans and Burchard"],
            defaut = ["no turbulence"],
            fr = """Calcul des conditions aux limites laterales sur $k$ et
$\epsilon$. Les choix possibles sont :
\begin{itemize}
\item 1: pas de turbulence = les valeurs minimales \telfile{KMIN}
et \telfile{EMIN} definies dans \telfile{CSTKEP} ;
\item 2: formule de Hans et Burchard.
\end{itemize}""",
            ang = """Computation of the lateral boundary conditions of $k$
and $\epsilon$. Possible choices are:
\begin{itemize}
\item 1: no turbulence = the minimum values \telfile{KMIN} and
\telfile{EMIN} defined in \telfile{CSTKEP},
\item 2: Hans and Burchard formula.
\end{itemize}""",
        ),
    ),
)
Ordre_des_commandes = (
'NUMERICAL_PARAMETERS',
'EQUATIONS,_TRACER',
'EQUATIONS',
'INPUT_OUTPUT,_GRAPHICS_AND_LISTING',
'EQUATIONS,_ADVECTION',
'EQUATIONS,_DIFFUSION',
'PHYSICAL_CONSTANTS',
'INPUT_OUTPUT,_INFORMATION',
'INPUT_OUTPUT,_FILES',
'EQUATIONS,_PROPAGATION',
'SEDIMENT_INFO',
'PHYSICS',
'EQUATIONS,_INITIAL_CONDITIONS',
'FILES',
'PARAMETER_FOR_BED_MATERIAL',
'EQUATIONS,_BOUNDARY_CONDITIONS',
'SEDIMENT_TRANSPORT',
'BOUNDARY_CONDITIONS',
'EQUATIONS,_SOURCES',
'FRICTION',
'SUSPENSION',
'PHYSICAL_PARAMETERS',
'CONSOLIDATION_INFO',
'COHESIVE_SEDIMENT_INFO',
'TURBULENCE',
'GENERAL',
'TURBULENCE_MODEL')
