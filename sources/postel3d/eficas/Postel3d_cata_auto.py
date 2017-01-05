
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
INPUT_OUTPUT,_GRAPHICS_AND_LISTING = PROC(nom= "INPUT_OUTPUT,_GRAPHICS_AND_LISTING",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    NUMBER_OF_HORIZONTAL_CROSS_SECTIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Permet de definir simultanement plusieurs coupes horizontales.
La valeur maximale autorisee est 9.""",
        ang = """Allow multiple horizontal sections. The maximum value is 9""",
    ),
#   -----------------------------------
    NUMBER_OF_VERTICAL_CROSS_SECTIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Permet de definir simultanement plusieurs coupes verticales.
La valeur maximale autorisee est 9.""",
        ang = """Allow multiple vertical sections. The maximum value is 9""",
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSG = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 1",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_1 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_1 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSH = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 2",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_2 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_2 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSI = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 3",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_3 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_3 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSJ = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 4",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_4 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_4 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSK = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 5",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_5 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_5 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSL = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 6",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_6 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_6 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSM = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 7",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_7 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_7 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSN = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 8",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_8 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_8 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    b_NUMBER_OF_VERTICAL_CROSS_SECTIONSO = BLOC(condition="NUMBER_OF_VERTICAL_CROSS_SECTIONS == 9",
#   -----------------------------------
#       -----------------------------------
        ABSCISSAE_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_9 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        ORDINATES_OF_THE_VERTICES_OF_VERTICAL_CROSS_SECTION_9 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=10, max=10,
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
    ),
#   -----------------------------------
    NUMBER_OF_FIRST_RECORD_FOR_CROSS_SECTIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Seuls les enregistrements au-dela de ce numero seront traites
pour les coupes.""",
        ang = """Only records after that time will be in the cross sections""",
    ),
#   -----------------------------------
    PRINTOUT_PERIOD_FOR_CROSS_SECTIONS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [1],
        fr = """Periode en nombre d''enregistrements entre 2 coupes.""",
        ang = """Period in number of records between two cross sections""",
    ),
#   -----------------------------------
    REFERENCE_LEVEL_FOR_EACH_HORIZONTAL_CROSS_SECTION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min= 9, max= 9,
        defaut = [0,1,2,3,4,5,6,7,8],
        fr = """Chaque coupe horizontale sera parallele a son plan de reference.
Ainsi il est possible de faire des coupes par exemple :
\begin{itemize}
\item a telle distance au-dessus du fond,
\item a telle distance sous la surface,
\item suivant un plan intermediaire \ldots
\end{itemize}
Le plan 0 correspond au plan parfaitement horizontal a la cote 0.""",
        ang = """Each horizontal cross section will be parallel to its reference
plane. It is then possible to make cross section which are:
\begin{itemize}
\item at a chosen distance above the bottom,
\item at a chosen distance below the surface,
\item referenced to an inbetween plane \ldots
\end{itemize}
Plane 0 correspond to the plane perfecly horizontal to the heigh 0.""",
    ),
#   -----------------------------------
    NUMBER_OF_NODES_FOR_VERTICAL_CROSS_SECTION_DISCRETIZATION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [120],
        fr = """Il s''agit du nombre de points suivant l''horizontale.""",
        ang = """It is the number of points along the horizontal""",
    ),
)
# -----------------------------------------------------------------------
GRAPHIC = PROC(nom= "GRAPHIC",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    ELEVATION_FROM_REFERENCE_LEVEL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 9, max= 9,
        defaut = [0.,0.,0.,0.,0.,0.,0.,0.,0.],
        fr = """Decalage entre la coupe et son plan de reference, ceci pour
chaque coupe horizontale.""",
        ang = """Gap between the cross sections and its reference plane, this
must be defined for cross section""",
    ),
#   -----------------------------------
    DISTORSION_BETWEEN_VERTICAL_AND_HORIZONTAL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min= 9, max= 9,
        defaut = [1.,1.,1.,1.,1.,1.,1.,1.,1.],
        fr = """Rapport entre echelles verticale et horizontale pour chaque
coupe verticale.""",
        ang = """Ratio between vertical and horizontal scale for each vertical
cross section""",
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,_FILES = PROC(nom= "INPUT_OUTPUT,_FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    3D_RESULT_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de resultats.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour Telemac;
\item SERAFIND: format standard double precision pour Telemac;
\item MED     : format MED base sur HDF5
\end{itemize}""",
        ang = """Results file format. Possible values are:
\begin{itemize}
\item SERAFIN : classical single precision format in Telemac;
\item SERAFIND: classical double precision format in Telemac;
\item MED     : MED format based on HDF5
\end{itemize}""",
    ),
#   -----------------------------------
    HORIZONTAL_CROSS_SECTION_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de resultats.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour Telemac;
\item SERAFIND: format standard double precision pour Telemac;
\item MED     : format MED base sur HDF5
\end{itemize}""",
        ang = """Results file format. Possible values are:
\begin{itemize}
\item SERAFIN : classical single precision format in Telemac;
\item SERAFIND: classical double precision format in Telemac;
\item MED     : MED format based on HDF5
\end{itemize}""",
    ),
#   -----------------------------------
    VERTICAL_CROSS_SECTION_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de resultats.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour Telemac;
\item SERAFIND: format standard double precision pour Telemac;
\item MED     : format MED base sur HDF5
\end{itemize}""",
        ang = """Results file format. Possible values are:
\begin{itemize}
\item SERAFIN : classical single precision format in Telemac;
\item SERAFIND: classical double precision format in Telemac;
\item MED     : MED format based on HDF5
\end{itemize}""",
    ),
#   -----------------------------------
    GEOMETRY_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier de geometrie.
Les valeurs possibles sont :
\begin{itemize}
\item SERAFIN : format standard simple precision pour Telemac;
\item SERAFIND: format standard double precision pour Telemac;
\item MED     : format MED base sur HDF5
\end{itemize}""",
        ang = """Geometry file format.
Possible values are:
\begin{itemize}
\item SERAFIN : classical single precision format in Telemac;
\item SERAFIND: classical double precision format in Telemac;
\item MED     : MED format based on HDF5
\end{itemize}""",
    ),
#   -----------------------------------
    NAMES = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        FORTRAN_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = 'DEFAUT',
            fr = """Nom du fichier FORTRAN a soumettre.\\
Il ne sert a priori qu''a dimensionner les tableaux utilises par
\postel3d, mais peut contenir des sous-programmes modifies ou propres
a l''utilisateur.""",
            ang = """Name of FORTRAN file to be submitted.\\
It is supposed to be used only to dimension the array used by \postel3d
but can also contain subroutines modified by the user.""",
        ),
#       -----------------------------------
        STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier contenant les references des fichiers et
les options du calcul a realiser.""",
            ang = """Name of the file containing the parameters of the computation
Written by the user.""",
        ),
#       -----------------------------------
        3D_RESULT_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier des resultats 3D obtenu par un calcul avec
\telemac{3D}.""",
            ang = """Name of the 3d result file generated by a \telemac{3D} run.""",
        ),
#       -----------------------------------
        HORIZONTAL_CROSS_SECTION_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Nom generique des fichiers des coupes horizontales.
Le fichier contenant la coupe i aura pour nom ce nom generique suivi
de l''extension ''.i''.""",
            ang = """Generic name for the horizontal cross sections file.
The file containing the cross section i name will be the generic
followed by the extension''.i''.""",
        ),
#       -----------------------------------
        VERTICAL_CROSS_SECTION_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
            defaut = '',
            fr = """Nom generique des fichiers des coupes verticales.
Le fichier contenant la coupe i au j ieme pas de temps enregistre aura
pour nom ce nom generique suivi de l''extension ''.i.j''.""",
            ang = """Generic name for the vertical cross sections file.  The file
containing the cross section i for the j time step name will be the
generic followed by the extension''.i.j''.""",
        ),
#       -----------------------------------
        GEOMETRY_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'), min=0, max='**',
            defaut = '',
            fr = """Nom du fichier de geometrie""",
            ang = """Name of the geometry file""",
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
            fr = """Type du binaire utilise pour l''ecriture du fichier des
resultats 3d.\\
Ce type depend de la machine sur laquelle le fichier a ete genere.
Les valeurs possibles sont :
\begin{itemize}
\item IBM: pour un fichier crEE sur IBM;
\item I3E: pour un fichier crEE sur HP;
\item STD: il s''agit alors d''ordres READ et WRITE normaux.
\end{itemize}""",
            ang = """Binary file type used for writing the results file.
This type depends on the machine on which the file was generated.
The possible values are as follows:
\begin{itemize}
\item IBM: for a file on an IBM (from a CRAY)
\item I3E: for a file on an HP (from a CRAY)
\item STD: binary type of the machine on which the user is working. In
that case, normal READ and WRITE commands are used
\end{itemize}""",
        ),
#       -----------------------------------
        CROSS_SECTION_FILE_BINARY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ['STD','I3E'],
            defaut = 'STD',
            fr = """Type du binaire utilise pour l''ecriture des fichiers des
coupes.\\
Ce type depend de la machine sur laquelle le fichier a ete genere.
Les valeurs possibles sont les memes que pour le fichier des
resultats 3D.""",
            ang = """Binary file type used for writing the cross section files.
This type depends on the machine on which the file was generated.
The possible values are as follows:
\begin{itemize}
\item I3E, for a file on an HP (from a CRAY)
\item STD, binary type of the machine on which the user is working. In
that case, normal READ and WRITE commands are used.
\end{itemize}""",
        ),
#       -----------------------------------
        GEOMETRY_FILE_BINARY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            into = ['STD','I3E'],
            defaut = 'STD',
            fr = """Type du binaire utilise pour l''ecriture des fichiers des
coupes.\\
Ce type depend de la machine sur laquelle le fichier a ete genere.
Les valeurs possibles sont les memes que pour le fichier des
resultats 3D.""",
            ang = """Binary file type used for writing the cross section files.
This type depends on the machine on which the file was generated.
The possible values are as follows:
\begin{itemize}
\item I3E, for a file on an HP (from a CRAY)
\item STD, binary type of the machine on which the user is working. In
    that case, normal READ and WRITE commands are used.
\end{itemize}""",
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
        RELEASE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min=0, max='**',
            defaut = 'V7P1',
            fr = """Tout est dans le titre""",
            ang = """It is all said in the title""",
        ),
#       -----------------------------------
        DESCRIPTION_OF_LIBRARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 6, max= 6,
            defaut = 'builds|PPP|lib|postel3dMMMVVV.LLL;builds|PPP|lib|biefMMMVVV.LLL;builds|PPP|lib|hermesMMMVVV.LLL;builds|PPP|lib|damoMMMVVV.LLL;builds|PPP|lib|parallelMMMVVV.LLL;builds|PPP|lib|specialMMMVVV.LLL',
            fr = """Description des librairies de \postel3d""",
            ang = """\postel3d libraries description""",
        ),
#       -----------------------------------
        DEFAULT_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|postel3dMMMVVV.exe',
            fr = """Executable par defaut de \postel3d""",
            ang = """Default executable for \postel3d""",
        ),
#       -----------------------------------
        DEFAULT_PARALLEL_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|postel3dMMMVVV.exe',
            fr = """Executable parallele par defaut de \postel3d""",
            ang = """Default parallel executable for \postel3d""",
        ),
    ),
#   -----------------------------------
    COMPUTATION_ENVIRONMENT = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        DICTIONARY = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = 'postel3d.dico',
            fr = """Dictionnaire des mots cles.""",
            ang = """Key word dictionary.""",
        ),
    ),
)
# -----------------------------------------------------------------------
FILES = PROC(nom= "FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    LIST_OF_FILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min= 8, max= 8,
        defaut = 'STEERING FILE;DICTIONARY;FORTRAN FILE;GEOMETRY FILE;STEERING FILE;3D RESULT FILE;HORIZONTAL CROSS SECTION FILE;VERTICAL CROSS SECTION FILE',
        fr = """Noms des fichiers exploites par le code""",
        ang = """File names of the used files""",
    ),
)
Ordre_des_commandes = (
'INPUT_OUTPUT,_GRAPHICS_AND_LISTING',
'GRAPHIC',
'INPUT_OUTPUT,_FILES',
'INPUT_OUTPUT,_INFORMATION',
'FILES')
