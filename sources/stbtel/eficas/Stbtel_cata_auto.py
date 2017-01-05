
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
GENERALITIES = PROC(nom= "GENERALITIES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    MAXIMUM_NUMBER_OF_BATHYMETRIC_POINTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 20000,
        fr = """Permet de dimensionner le tableau utilise pour la lecture, dans les
\telkey{FICHIERS DES FONDS}, des points releves a la table a
digitaliser.""",
        ang = """Designed for dimensioning the array that is used for reading, in the
\telkey{BOTTOM TOPOGRAPHY FILES}, the points recorded at the digitizing
tablet.""",
    ),
#   -----------------------------------
    VECTOR_LENGTH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """Permet de fixer, sur machine vectorielle, la longueur du vecteur.""",
        ang = """Designed for dimensioning the vector length on vector machine.""",
    ),
#   -----------------------------------
    NUMBER_OF_VERTICES_OF_THE_POLYGON_TO_EXTRACT_THE_MESH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 0,
        fr = """Lorsque l''on souhaite extraire une partie du maillage, permet de
definir le nombre de sommets du polygone a l''interieur duquel le
maillage sera effectivement extrait.\\
\begin{WarningBlock}{Attention :}
Ce polygone doit etre convexe et les coordonnees des sommets donnes dans
l''ordre trigonometrique.  \end{WarningBlock}""",
        ang = """When you want to extract a piece of the mesh, this key-word fixes
the number of vertices of a polygon inside of which the mesh will be
finally extracted.\\
\begin{WarningBlock}{Warning:}
This polygon should have a convex shape and the coordinates of the
vertices be given with an anti clock wise order.
\end{WarningBlock}""",
    ),
#   -----------------------------------
    NUMBER_OF_VERTICES_OF_THE_POLYGON_TO_REFINE_THE_MESH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 0,
        fr = """Lorsque l''on souhaite raffiner une partie du maillage,
permet de definir le nombre de sommets du polygone a l''interieur
duquel le maillage sera effectivement raffine.\\
\begin{WarningBlock}{Attention:}
Ce polygone doit etre convexe et les coordonnees des
sommets donnes dans l''ordre trigonometrique.
\end{WarningBlock}""",
        ang = """When you want to refine a piece of the mesh, this key-word fixes
the number of vertices of a polygon inside of which the mesh will be
finally refined.\\
\begin{WarningBlock}{ATTENTION:}
This polygon should have a convex shape and the coordinates
of the vertices be given with an anti clock wise order.
\end{WarningBlock}""",
    ),
#   -----------------------------------
    PARALLEL_PROCESSORS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I',
        defaut = 0,
        fr = """Nombre de processeurs en calcul parallele
\begin{itemize}
\item 0 : 1 machine, compilation sans bibliotheque de parallelisme
\item 1 : 1 machine, compilation avec bibliotheque de parallelisme
\item 2 : 2 processeurs ou machines en parallele
\item etc\ldots
\end{itemize}""",
        ang = """Number of processors for parallel processing
\begin{itemize}
\item 0 : 1 machine, compiling without parallel library
\item 1 : 1 machine, compiling with a parallel library
\item 2 : 2 processors or machines in parallel
\item etc\ldots
\end{itemize}""",
    ),
#   -----------------------------------
    MINIMUM_DISTANCE_BETWEEN_TWO_POINTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 1.E-5,
        fr = """Distance (en metres)en dessous de laquelle deux noeuds sont consideres
comme confondus par \stbtel, lors de la verification des resultats
fournis par le mailleur. Lorsque deux noeuds sont confondus, l''un
d''entre eux est elimine, et tous les noeuds du maillage sont
renumerotes.""",
        ang = """Distance (in meters) below which two nodes are considered as identical
by \stbtel when the results supplied by the mesh generator are being
checked. When two nodes occur at the same place, one of them is
eliminated and all the mesh nodes are renumbered.""",
    ),
#   -----------------------------------
    MINIMUM_DISTANCE_AT_BOUNDARY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 0.,
        fr = """L''interpolation de la bathymetrie sur les noeuds du maillage est
realisee de la facon suivante. Pour chaque noeud du maillage, on
decoupe le plan en 4 quadrans. Dans chacun de ces quadrans, on cherche
le point releve a la table a digitaliser le plus proche. On affecte
alors au noeud considere une profondeur correspondant a la moyenne,
ponderee par la distance a ce noeud, des profondeurs en chacun des 4
points precedemment trouves.\\
On verifie toutefois que, lors de la recherche des points, les
frontieres du domaine ne sont pas franchies, de maniere a ne pas
introduire d''aberration dans la bathymetrie.\\
Ce mot-cle permet alors de definir la distance minimale aux frontieres
du domaine en dessous de laquelle on refuse de prendre en compte les
points releves.""",
        ang = """The bathymetric data at the mesh nodes are interpolated. At each mesh
node, the plane is cut into 4 quadrants in each of which, among the
points recorded at the digitizing tablet, the closest one to the node
being considered is searched for.\\
This node is thenn given a depth corresponding to the mean depth at
each of the 4 points previously found, these depths being weighted
by the distance to the node.\\
When searching for the points in the quadrants, however, one shall
make sure the boundaries aare not overstepped in order to prevent
aberrations from being introduced into the bathymetric data.\\
The keyword can then be used for specifying the minimum distance to
the boundaries below which the recorded points should be ignored.""",
    ),
#   -----------------------------------
    BOTTOM_CORRECTION_OF_TRIGRID = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 0.,
        fr = """Fixe la valeur a ajouter a la bathymetrie lue dans le fichier
gemere par Trigrid.""",
        ang = """Value to be added at the bottom value read in the Trigrid file""",
    ),
#   -----------------------------------
    ABSCISSAE_OF_THE_VERTICES_OF_THE_POLYGON_TO_EXTRACT_THE_MESH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 9, max= 9,
        defaut = [0,0,0,0,0,0,0,0,0],
        fr = """Lorsque l''on souhaite extraire une partie du maillage,
permet de definir les abscisses des sommets du polygone a l''interieur
duquel le maillage sera effectivement extrait.
\begin{WarningBlock}{Attention :}
ce polygone doit etre convexe et les coordonnees des
sommets donnes dans l''ordre trigonometrique.
\end{WarningBlock}""",
        ang = """When you want to extract a piece of the mesh, this key-word fixes
the abscissae of the vertices of a polygon inside of which the mesh
will be finally extracted.\\
\begin{WarningBlock}{Warning:}
This polygon should have a convex shape and the coordinates
of the vertices be given with an anti clock wise order.
\end{WarningBlock}""",
    ),
#   -----------------------------------
    ORDINATES_OF_THE_VERTICES_OF_THE_POLYGON_TO_EXTRACT_THE_MESH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 9, max= 9,
        defaut = [0,0,0,0,0,0,0,0,0],
        fr = """Lorsque l''on souhaite extraire une partie du maillage,
permet de definir les ordonnees des sommets du polygone a l''interieur
duquel le maillage sera effectivement extrait.\\
\begin{WarningBlock}{Attention :}
Ce polygone doit etre convexe et les coordonnees des
sommets donnes dans l''ordre trigonometrique.
\end{WarningBlock}""",
        ang = """When you want to extract a piece of the mesh, this key-word fixes
the ordinates of the vertices of a polygon inside of which the mesh
will be finally extracted.\\
\begin{WarningBlock}{Warning:}
This polygon should have a convex shape and the coordinates
of the vertices be given with an anti clock wise order.
\end{WarningBlock}""",
    ),
#   -----------------------------------
    ABSCISSAE_OF_THE_VERTICES_OF_THE_POLYGON_TO_REFINE_THE_MESH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 9, max= 9,
        defaut = [0,0,0,0,0,0,0,0,0],
        fr = """Lorsque l''on souhaite raffiner une partie du maillage,
permet de definir les abscisses des sommets du polygone a l''interieur
duquel le maillage sera effectivement raffine.\\
\begin{WarningBlock}{Attention :}
ce polygone doit etre convexe et les coordonnees des
sommets donnes dans l''ordre trigonometrique.
\end{WarningBlock}""",
        ang = """When you want to refine a piece of the mesh, this key-word fixes
the abscissae of the vertices of a polygon inside of which the mesh
will be finally refined.\\
\begin{WarningBlock}{Warning:}
This polygon should have a convex shape and the coordinates
of the vertices be given with an anti clock wise order.
\end{WarningBlock}""",
    ),
#   -----------------------------------
    ORDINATES_OF_THE_VERTICES_OF_THE_POLYGON_TO_REFINE_THE_MESH = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 9, max= 9,
        defaut = [0,0,0,0,0,0,0,0,0],
        fr = """Lorsque l''on souhaite raffiner une partie du maillage,
permet de definir les ordonnees des sommets du polygone a l''interieur
duquel le maillage sera effectivement raffine.\\
\begin{WarningBlock}{Attention :}
ce polygone doit etre convexe et les coordonnees des
sommets donnes dans l''ordre trigonometrique.
\end{WarningBlock}""",
        ang = """When you want to refine a piece of the mesh, this key-word fixes
the ordinates of the vertices of a polygon inside of which the mesh
will be finally refined.\\
\begin{WarningBlock}{Warning:}
This polygon should have a convex shape and the coordinates
of the vertices be given with an anti clock wise order.
\end{WarningBlock}""",
    ),
#   -----------------------------------
    OVERSTRESSED_TRIANGLES_CUTTING = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Un triangle surcontraint est tel que ses trois noeuds soient situes
sur une frontiere du domaine de calcul. La presence de tels triangles
peut entrainer des instabilites lors des calculs realises par
TELEMAC 2D.\\
Cette option permet, en creant un noeud place au barycentre de des
triangles surcontraints, d''eviter de tels problemes.""",
        ang = """An overstressed triangle is one whose three nodes are located along a
boundary of the computational domain. The occurrence of such triangles
may bring about instabilities in the computations made by TELEMAC 2D.
Such problems can be prevented by this option, through the creation of
a node at the geometric centres of the overstressed triangles.""",
    ),
#   -----------------------------------
    WRITING_NODE_COLOURS = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Option non activee.""",
        ang = """Option not activated""",
    ),
#   -----------------------------------
    ELIMINATION_OF_BACKWARD_DEPENDENCIES = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = True,
        fr = """Permet de renumeroter les noeuds du maillage de maniere a eliminer les
dependances arrieres et autoriser ainsi le forcage de la vectorisation
lorsque les calculs TELEMAC 2D son effectues sur CRAY.\\
\begin{WarningBlock}{Attention :}
Un nombre minimum d''environ 500 noeuds est requis pour
l''activation de cette option.
\end{WarningBlock}""",
        ang = """Provides for renumbering of the mesh nodes in order to eliminate the
backward dependencies, thereby enabling a forced vectorisation when
the TELEMAC 2D computations are made on a CRAY.\\
\begin{WarningBlock}{Warning:}
About 500 nodes is the least number required for activating
this option.
\end{WarningBlock}""",
    ),
#   -----------------------------------
    CUTTING_ELEMENTS_IN_FOUR = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Decoupe chaque element du mayage en quatre elements homothetiques
en joignant les milieux des aretes.""",
        ang = """Cuts every element of the mesh in four homothetic elements
by joigning the middle points of each side.""",
    ),
#   -----------------------------------
    BATHYMETRY_IN_THE_UNIVERSAL_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Permet de relire la bathymetrie directement dans le fichier de
maillage (Trigrid ou Fasttabs).""",
        ang = """The bathymetry will be read in the mesh file (Trigrid or Fasttabs).""",
    ),
#   -----------------------------------
    NODES_RENUMBERING = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Permet d''utiliser le nouveau type de stockage des matrices.""",
        ang = """Necessary to use the new storage scheme for the matrix.""",
    ),
#   -----------------------------------
    BOUNDARY_CONDITIONS_IN_THE_ADDITIONAL_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Permet de relire les conditions limites dans le fichier
additionnel (Fasttabs).""",
        ang = """The boundary condition will be read in the additional file
 (Fasttabs).""",
    ),
#   -----------------------------------
    PROJECTION_AFTER_EXTRACTION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = True,
        fr = """Lors d''une extraction de maillage suivant un polygone, projette ou
non le maillage extrait sur les aretes du polygone.""",
        ang = """When a mesh is extracted inside a polygon, indicates whether the mesh
should be projected through the faces of the polygon or not.""",
    ),
#   -----------------------------------
    DRY_LIMIT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 0.1,
        fr = """Fixe la valeur du seuil (en metre) de hauteur d''eau en dessous
duquel un noeud est considere comme sec.""",
        ang = """Limit of water depth value (in meter) under which the node is
considered as dry node.""",
    ),
#   -----------------------------------
    DRY_ELEMENTS_ELIMINATION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Lors du traitement d''un fichier de resultats issu de \telemac{2D},
permet d''activer l''elimination des elements secs.""",
        ang = """When using a \telemac{2D} results file, this keyword activates the
dry elements elimination.""",
    ),
#   -----------------------------------
    PARTIALLY_DRY_ELEMENTS_ELIMINATION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Lors de l''elimination des elements secs, permet de specifier si
on traite les elements parteillement secs (au moins un noeud sec).""",
        ang = """When removing dry elements, specify if the partially dry elements are
treated (at least one dry node).""",
    ),
#   -----------------------------------
    STORAGE_OF_ALL_TIME_STEPS = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Lors du traitement d''elimination des elements secs, permet de
specifier si on restocke tous les pas de temps dans le fichier
resultat""",
        ang = """When treating dry elements elimination, specify that
all time steps are to be stored in the results file.""",
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,_FILES = PROC(nom= "INPUT_OUTPUT,_FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NAMES = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        FORTRAN_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = 'DEFAUT',
            fr = """Nom du fichier Fortran a soumettre.
Il ne sert a priori qu''a dimensionner les tableaux utilises par
\stbtel, mais peut contenir des sous-programmes modifies ou propres a
l''utilisateur.""",
            ang = """Name of Fortran file to be entered.
It is a priori only designed for dimensioning the arrays that are
used by \stbtel, but it may contain either modified or user-written
subroutines.""",
        ),
#       -----------------------------------
        STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier contenant les references des fichiers et les options
du calcul a realiser.""",
            ang = """Name of the file that contains the file references and of options for
the computation to be made.""",
        ),
#       -----------------------------------
        UNIVERSAL_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier construit par le mailleur, a partir duquel \stbtel va
travailler.""",
            ang = """Name of the file created by the mesh generator, and from which \stbtel
will work.""",
        ),
#       -----------------------------------
        GEOMETRY_FILE_FOR_TELEMAC = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Nom du fichier qui contiendra le maillage mis au format SELAFIN, et
qui servira pour les calculs \telemac{2D}.""",
            ang = """Name of the file that will contain the mesh data to SELAFIN format,
and to be used in \telemac{2D} computations.""",
        ),
#       -----------------------------------
        BOUNDARY_CONDITIONS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Nom du fichier qui contiendra les conditions aux limites lues dans le
\telkey{FICHIER UNIVERSEL}, et qui servira pour les calculs
\telemac{2D}. (les conditions aux limites sont definies lors de la
realisation du maillage, au moyen de couleurs affectees aux noeuds des
frontieres du domaine de calcul).""",
            ang = """Name of the file that will contain the boundary conditions being read
from the \telkey{UNIVERSAL FILE}, and to be used in \telemac{2D}
computations. (The boundary conditions are defined when preparing the
meshes, through colours that are allotted to the nodes of the
computation domain boundaries).""",
        ),
#       -----------------------------------
        BOTTOM_TOPOGRAPHY_FILES = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min= 5, max= 5,
            defaut = ';;;;',
            fr = """Nom du fichier contenant la bathymetrie (au standard SINUSX), qui
servira pour la definition, par interpolation, de la profondeur en
chaque point du maillage.""",
            ang = """Name of the file containing the bathymetric points (to SINUSX
standard), to be used, through interpolation, for defining the depth
at each point of the mesh.""",
        ),
#       -----------------------------------
        MESH_ADDITIONAL_DATA_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier additionnel. Ce fichier a une signification
differente en fonction du mailleur choisi.
\begin{itemize}
\item Trigrid : Fichier contenant la table de connectivites
(obligatoire).
\item Fasttabs : Fichier contenant les conditions limites (optionnel).
\end{itemize}""",
            ang = """Name of the additionql file. The meaning of this file depend on the
type of mesh generator.
\begin{itemize}
\item Trigrid : containing the connectivity table built (mandatory).
\item Fasttabs  : boundary condition file built by Fasttabs (optional).
\end{itemize}""",
        ),
#       -----------------------------------
        INPUT_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier a convertir.""",
            ang = """Name of the file to convert.""",
        ),
#       -----------------------------------
        OUTPUT_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Nom du fichier converti""",
            ang = """Name of the converted file""",
        ),
#       -----------------------------------
        BOUNDARY_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier des conditions limites""",
            ang = """Name of the boundary condition file""",
        ),
#       -----------------------------------
        LOG_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier complementaire pour le format UNV""",
            ang = """Name of the complementary file for the UNV format""",
        ),
#       -----------------------------------
        OUTPUT_BOUNDARY_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Nom du fichier des conditions limites pour le fichier converti""",
            ang = """Name of the boundary file for the converted file""",
        ),
#       -----------------------------------
        OUTPUT_LOG_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'),
            defaut = '',
            fr = """Nom du fichier complementaire pour le fichier converti""",
            ang = """Name of the complementary file for the converted file""",
        ),
    ),
#   -----------------------------------
    STANDARD_I/O = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        BINARY_STANDARD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            into = ['STD','IBM','I3E'],
            defaut = 'STD',
            fr = """Adapte l''ecriture du \teleky{FICHIER DE GEOMETRIE POUR TELEMAC} au
standard binaire choisi pour celui-ci. Ce peut etre :
\begin{itemize}
\item IBM : binaire IBM,
\item I3E : binaire HP,
\item STD : prend par defaut le binaire de la machine sur laquelle
            l''utilisateur travaille. Ce sont alors des ordres READ et
            WRITE normaux qui sont utilises.
\end{itemize}""",
            ang = """Matches the writing of the \telkey{GEOMETRY FILE FOR TELEMAC} to the
binary standard chosen for the latter. It will be selected among the
following:
\begin{itemize}
\item IBM: IBM binary,
\item I3E: HP binary,
\item STD: takes by default the binary on the computer with which
          the user is working. The normal READ and WRITE commands
          are then used.
\end{itemize}""",
        ),
    ),
)
# -----------------------------------------------------------------------
MESH_GENERATOR = PROC(nom= "MESH_GENERATOR",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    MESH_GENERATOR = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SUPERTAB4','SUPERTAB6','MASTER2','SIMAIL','SELAFIN','TRIGRID','FASTTABS'],
        defaut = 'MASTER2',
        fr = """Nom du mailleur utilise pour la creation du \telkey{FICHIER UNIVERSEL}.
Ce peut etre :
\begin{itemize}
\item SUPERTAB6 (version 6 du mailleur SUPERTAB),
\item SUPERTAB4 (version 4 du mailleur SUPERTAB),
\item MASTER2 (version 2 du mailleur MASTER-SERIES),
\item SIMAIL,
\item SELAFIN (afin de modifier un maillage deja utilise, comme pour :
\begin{itemize}
\item interpoler de nouveau fonds
\item eliminer des dependances arrieres
\item coupe triangles surcontraints),
\end{itemize}
\item TRIGRID,
\item FASTTABS.
\end{itemize}""",
        ang = """Name of the mesh generator used for preparing the \telkey{UNIVERSAL
FILE}. It will be selected among the following:
\begin{itemize}
\item SUPERTAB6 (version 6 of SUPERTAB mesh generator),
\item SUPERTAB4 (version 4 of SUPERTAB mesh generator),
\item MASTER2 (version 2 of MASTER-SERIES mesh generator),
\item SIMAIL,
\item SELAFIN (in order to modify a mesh already used, as for example :
\begin{itemize}
\item to interpolate a new bathymetry
\item to eliminate backward dependencies
\item to cut overstressed triangles ),
\end{itemize}
\item TRIGRID,
\item FASTTABS.
\end{itemize}""",
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,_INFORMATION = PROC(nom= "INPUT_OUTPUT,_INFORMATION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    COMPUTATION_ENVIRONMENT = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        BIBLIOTHEQUES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """""",
            ang = """""",
        ),
#       -----------------------------------
        RELEASE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'V7P1',
            fr = """Numeros de version respectivement des bibliotheques
        stbtel,damo,util,hp""",
            ang = """Version numbers of STBTEL, DAMO, UTIL, HP libraries, respectively.""",
        ),
#       -----------------------------------
        DICTIONARY = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = 'stbtel.dico',
            fr = """Dictionnaire des mots cles.""",
            ang = """Key word dictionary.""",
        ),
    ),
#   -----------------------------------
    COMPUTATIONAL_INFORMATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        DESCRIPTION_OF_LIBRARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 6, max= 6,
            defaut = 'builds|PPP|lib|stbtelMMMVVV.LLL;builds|PPP|lib|biefMMMVVV.LLL;builds|PPP|lib|hermesMMMVVV.LLL;builds|PPP|lib|damoMMMVVV.LLL;builds|PPP|lib|parallelMMMVVV.LLL;builds|PPP|lib|specialMMMVVV.LLL',
            fr = """Description des librairies de \stbtel""",
            ang = """Description of \stbtel libraries""",
        ),
#       -----------------------------------
        DEFAULT_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|stbtelMMMVVV.exe',
            fr = """Executable par defaut de \stbtel""",
            ang = """Default executable for \stbtel""",
        ),
#       -----------------------------------
        DEFAULT_PARALLEL_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|stbtelMMMVVV.exe',
            fr = """Executable parallele par defaut de \stbtel""",
            ang = """Default parallel executable for \stbtel""",
        ),
    ),
)
# -----------------------------------------------------------------------
CONVERTER_INFO = PROC(nom= "CONVERTER_INFO",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    DEBUG = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Permet d activer le mode debuggage.""",
        ang = """Activate the debug mode.""",
    ),
#   -----------------------------------
    CONVERTER = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Permet d activer le module de conversion.""",
        ang = """Activate the conversion module.""",
    ),
#   -----------------------------------
    BOUNDARY_CONDITION_IN_SERAFIN_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Fichier de conditions limites dans le cas d''une conversion depuis
SERAFIN.""",
        ang = """Boundary condition file when converting from SERAFIN.""",
    ),
#   -----------------------------------
    INPUT_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED','UNV','CGNS'],
        defaut = 'SERAFIN',
        fr = """Specifie le format du fichier d entree""",
        ang = """Specify input file format""",
    ),
#   -----------------------------------
    OUTPUT_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED','UNV','CGNS','VTK','CGNS'],
        defaut = 'SERAFIN',
        fr = """Specifie le format du fichier de sortie""",
        ang = """Specify output file format""",
    ),
#   -----------------------------------
    TRANSLATION_INFO = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        TRANSLATION = SIMP(statut ='f',
#       -----------------------------------
            typ = bool,
            defaut = False,
            fr = """Permet d activer la translation de maillage.""",
            ang = """Activate the translation of the mesh.""",
        ),
#       -----------------------------------
        b_TRANSLATIONG = BLOC(condition="TRANSLATION == True",
#       -----------------------------------
#           -----------------------------------
            X_TRANSLATION = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """Translation selon x""",
                ang = """Translation on the x axes""",
            ),
#           -----------------------------------
            Y_TRANSLATION = SIMP(statut ='f',
#           -----------------------------------
                typ = 'R',
                defaut = 0.,
                fr = """Translation selon y""",
                ang = """Translation on the y axes""",
            ),
        ),
    ),
)
# -----------------------------------------------------------------------
FILES = PROC(nom= "FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    LIST_OF_FILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=14, max=14,
        defaut = 'UNIVERSAL FILE;GEOMETRY FILE FOR TELEMAC;BOUNDARY CONDITIONS FILE;MESH ADDITIONAL DATA FILE;BOTTOM TOPOGRAPHY FILES;FORTRAN FILE;STEERING FILE;DICTIONARY;INPUT FILE;OUTPUT FILE;BOUNDARY FILE;LOG FILE;OUTPUT BOUNDARY FILE;OUTPUT LOG FILE',
        fr = """Noms des fichiers exploites par le code""",
        ang = """File names of the used files""",
    ),
)
Ordre_des_commandes = (
'GENERALITIES',
'INPUT_OUTPUT,_FILES',
'MESH_GENERATOR',
'INPUT_OUTPUT,_INFORMATION',
'CONVERTER_INFO',
'FILES')
