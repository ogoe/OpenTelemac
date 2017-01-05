
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
INPUT_OUTPUT,_FILES = PROC(nom= "INPUT_OUTPUT,_FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    RESULTS_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier des resultats.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """results file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
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
        ang = """geometry file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
#   -----------------------------------
    HYDRODYNAMIC_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier hydrodynamique.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """hydrodynamic file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
#   -----------------------------------
    REFERENCE_FILE_FORMAT = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['SERAFIN','SERAFIND','MED'],
        defaut = 'SERAFIN?',
        fr = """Format du fichier hydrodynamique.
Les valeurs possibles sont :
- SERAFIN : format standard simple precision pour Telemac;
- SERAFIND: format standard double precision pour Telemac;
- MED     : format MED base sur HDF5""",
        ang = """hydrodynamic file format.
Possible values are:
- SERAFIN : classical single precision format in Telemac;
- SERAFIND: classical double precision format in Telemac;
- MED     : MED format based on HDF5""",
    ),
#   -----------------------------------
    NAMES = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        STEERING_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = '',
            fr = """Nom du fichier contenant les parametres du calcul
QE a realiser.""",
            ang = """Name of the file containing parameters of the WAQ
computation Written by the user.""",
        ),
#       -----------------------------------
        FORTRAN_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = 'DEFAUT',
            fr = """Nom du fichier FORTRAN a soumettre.""",
            ang = """Name of FORTRAN file to be submitted.""",
        ),
#       -----------------------------------
        BOUNDARY_CONDITIONS_FILE = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            fr = """Nom du fichier contenant les types de conditions aux limites.
Ce fichier est rempli de facon automatique par le mailleur au moyen de
couleurs affectees aux noeuds des frontieres du domaine de calcul.""",
            ang = """Name of the file containing the types of boundary conditions.
This file is filled automatically by the mesh generator through
through colours that are assigned to the boundary nodes.""",
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
        WAQ_CASE_TITLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = '',
            fr = """Titre du cas etudie. Ce titre sera inscrit dans les sorties.""",
            ang = """Title of the case being considered.
This title shall be marked on the printouts.""",
        ),
#       -----------------------------------
        RELEASE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'V7P0',
            fr = """Numero de version des bibliotheques utilisees par WAQTEL.""",
            ang = """Release of the libraries used by WAQTEL.""",
        ),
#       -----------------------------------
        DESCRIPTION_OF_LIBRARIES = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM', min= 5, max= 5,
            defaut = 'builds|PPP|lib|biefMMMVVV.LLL;builds|PPP|lib|damoMMMVVV.LLL;builds|PPP|lib|hermesMMMVVV.LLL;builds|PPP|lib|parallelMMMVVV.LLL;builds|PPP|lib|specialMMMVVV.LLL',
            fr = """Description des librairies de WAQ""",
            ang = """LIBRARIES description""",
        ),
#       -----------------------------------
        DEFAULT_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|waqtelMMMVVV.exe',
            fr = """Executable par defaut de WAQ""",
            ang = """Default executable for WAQ""",
        ),
#       -----------------------------------
        DEFAULT_PARALLEL_EXECUTABLE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'TXM',
            defaut = 'builds|PPP|bin|waqtelMMMVVV.exe',
            fr = """Executable parallele par defaut de WAQ""",
            ang = """Default parallel executable for WAQ""",
        ),
    ),
#   -----------------------------------
    COMPUTATION_ENVIRONMENT = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        DICTIONARY = SIMP(statut ='f',
#       -----------------------------------
            typ = ('Fichier','All Files (*)'),
            defaut = 'waqtel.dico',
            fr = """Dictionnaire des mots cles.""",
            ang = """Key word dictionary.""",
        ),
    ),
)
# -----------------------------------------------------------------------
IN_OUT,WQ = PROC(nom= "IN_OUT,WQ",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    WATER_QUALITY_PRINTOUT_PERIOD = SIMP(statut ='o',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """periode pour les sorties graphiques QE""",
        ang = """graphic outputs period for waq""",
    ),
)
# -----------------------------------------------------------------------
INPUT_OUTPUT,_GRAPHICS_AND_LISTING = PROC(nom= "INPUT_OUTPUT,_GRAPHICS_AND_LISTING",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    VARIABLES_FOR_WAQ_PRINTOUTS = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['to edit !!!'],
        defaut = '',
        fr = """Noms des variables que l''utilisateur veut ecrire dans
le fichier des resultats QE.
Chaque variable est representee par une lettre.""",
        ang = """Names of variables the user wants to write
into the graphic results file.""",
    ),
)
# -----------------------------------------------------------------------
RESULTS = PROC(nom= "RESULTS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    WAQ_VARIABLES_TO_BE_PRINTED = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ['� editer'],
        defaut = '',
        fr = """Nom des variables que l''utilisateur desire ecrire sur
le listing. Meme possibilites que pour les sorties graphiques.""",
        ang = """Names of variables the user wants to write on the listing.
Each variable is represented by a letter in the same manner as
it is done in the graphic results file.""",
    ),
#   -----------------------------------
    RESULTS_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)','Sauvegarde'), min=0, max='**',
        fr = """Nom du fichier dans lequel seront ecrits les resultats avec
une periodicite donnee par le mot cle PERIODE DE SORTIE QUALITE D EAU.""",
        ang = """Name of the file into wich the computation results shall be
written, the periodicity being given by the keyword
WAQ PRINTOUT PERIOD.""",
    ),
#   -----------------------------------
    MASS_BALANCE = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Determine si oui ou non le bilan de masse est realise.""",
        ang = """Determines whether a check of the mass-balance over the domain
is made or not""",
    ),
)
# -----------------------------------------------------------------------
DATA_FILES = PROC(nom= "DATA_FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    GEOMETRY_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'), min=0, max='**',
        defaut = '',
        fr = """fichier de geometrie, pareil que celui de telemac2d""",
        ang = """geometry file same as the telemac2d one""",
    ),
#   -----------------------------------
    HYDRODYNAMIC_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'), min=0, max='**',
        defaut = '',
        fr = """fichier des donnees hydrodynamiqes provenant de telemac2d""",
        ang = """hydrodynamic data file coming from telemac2d""",
    ),
#   -----------------------------------
    REFERENCE_FILE = SIMP(statut ='f',
#   -----------------------------------
        typ = ('Fichier','All Files (*)'), min=0, max='**',
        defaut = '',
        fr = """Nom du fichier servant a valider le calcul.
Si VALIDATION = OUI, les resultats du calcul vont etre
comparees aux valeurs contenues dans ce fichier.
La comparaison est effectuee par le sous-programme VALIDA.
� implementer""",
        ang = """Name of the file used to validate the computation.
If VALIDATION = YES, the results of the computation will be
compared with the values of this file. The comparison is
made by the subroutine BIEF\_VALIDA. (not implemented yet)""",
    ),
)
# -----------------------------------------------------------------------
MISCELLANEOUS = PROC(nom= "MISCELLANEOUS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    VALIDATION = SIMP(statut ='f',
#   -----------------------------------
        typ = bool,
        defaut = False,
        fr = """Option utilisee principalement pour le dossier de validation.
Si ce mot-cle vaut OUI, les resultats du calcul vont alors etre
compares aux valeurs du fichier de reference.""",
        ang = """This option is primarily used for the validation
documents. If this keyword is equal to YES, the REFERENCE FILE
is then considered as a reference which the computation is
going to be compared with.""",
    ),
#   -----------------------------------
    b_VALIDATIONG = BLOC(condition="VALIDATION == True",
#   -----------------------------------
    ),
#   -----------------------------------
    DEBUGGER = SIMP(statut ='f',
#   -----------------------------------
        typ = 'I', min=0, max='**',
        defaut = [0],
        fr = """Pour imprimer la sequence des appels, mettre 1""",
        ang = """If 1, calls of subroutines will be printed in the listing""",
    ),
)
# -----------------------------------------------------------------------
PHYSICS = PROC(nom= "PHYSICS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    WATER_DENSITY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 999.972,
        fr = """Fixe la valeur de la masse volumique de l''eau.""",
        ang = """sets the value of water density.""",
    ),
#   -----------------------------------
    KINEMATIC_WATER_VISCOSITY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R',
        defaut = 1.E-6,
        fr = """Definit la viscosite cinematique de l''eau.
         M/S2""",
        ang = """Specifies the water kinematic viscosity.
         M/S2""",
    ),
)
# -----------------------------------------------------------------------
SUSPENSION = PROC(nom= "SUSPENSION",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    DISPERSION_ALONG_THE_FLOW = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1.E-2],
        fr = """""",
        ang = """""",
    ),
#   -----------------------------------
    DISPERSION_ACROSS_THE_FLOW = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [1.E-2],
        fr = """""",
        ang = """""",
    ),
)
# -----------------------------------------------------------------------
WAQ_PARAMETERS = PROC(nom= "WAQ_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    EUTROPHICATION = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        CONSTANT_OF_DEGRADATION_OF_ORGANIC_LOAD_K120 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.35],
            fr = """en J-1""",
            ang = """in J-1""",
        ),
#       -----------------------------------
        CONSTANT_FOR_THE_NITRIFICATION_KINETIC_K520 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.35],
            fr = """EN J-1""",
            ang = """IN J-1""",
        ),
#       -----------------------------------
        OXYGENE_PRODUCED_BY_PHOTOSYNTHESIS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.15],
            fr = """EN MgO2/MicroGChLA""",
            ang = """IN MgO2/MicroGChLA""",
        ),
#       -----------------------------------
        CONSUMED_OXYGEN_BY_NITRIFICATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [5.2],
            fr = """EN MgO2/MgNH4""",
            ang = """IN MgO2/MgNH4""",
        ),
#       -----------------------------------
        BENTHIC_DEMAND = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.1],
            fr = """EN gO2/m2/J""",
            ang = """IN gO2/m2/J""",
        ),
#       -----------------------------------
        K2_REAERATION_COEFFICIENT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.9],
            fr = """EN J-1""",
            ang = """IN J-1""",
        ),
#       -----------------------------------
        FORMULA_FOR_COMPUTING_K2 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [1],
            fr = """DONNE LE CHOIX DE CALCUL DE LA FORMULE DE K2 DE LA
REAERATION NATURELLE, LES OPTIONS SONT LES SUIVANTS:
  0- K2 CONSTANT, VALEUR DE K2=0.9
  1- FORMULE DE TENESSEE VALLEY AUTHORITY
  2- FORMULE DE OWENS ET AL.
  3- FORMULE DE CHURCHILL ET AL.
  4- FORMULE DE O CONNOR \& DOBBINS
  5- FOURMULA OF ??""",
            ang = """GIVES HOW TO CUMPUTE THE REAERATION COEFFICIENT K2
OPTIONS ARE:
  0- K2 CONSTANT, IN THIS CASE K2=0.9
  1- FORMULA OF THE TENESSEE VALLEY AUTHORITY
  2- FORMULA OF OWENS ET AL.
  3- FORMULA OF CHURCHILL ET AL.
  4- FORMULA OF O CONNOR \& DOBBINS
  5- FORMULA OF ??""",
        ),
#       -----------------------------------
        O2_SATURATION_DENSITY_OF_WATER_(CS) = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [11.],
            fr = """EN Mg/l""",
            ang = """IN Mg/l""",
        ),
#       -----------------------------------
        FORMULA_FOR_COMPUTING_CS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """EN J-1, LES OPTIONS SONT LES SUIVANTES:
   0: CONSTANTE
   1: FORMULE DE ELMORE \& HAYES
   2: FORMULE DE MONTGOMERY""",
            ang = """IN J-1, HERE ARE AVAILABLE OPTIONS
   0: CONSTANT
   1: ELMORE \& HAYES FORMULA
   2: MONTGOMERY FORMULA""",
        ),
#       -----------------------------------
        SEDIMENTATION_VELOCITY_OF_ORGANIC_PHOSPHORUS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """EN M/S""",
            ang = """IN M/S""",
        ),
#       -----------------------------------
        SEDIMENTATION_VELOCITY_OF_NON_ALGAL_NITROGEN = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """EN M/S""",
            ang = """IN M/S""",
        ),
#       -----------------------------------
        MAXIMUM_ALGAL_GROWTH_RATE_AT_20C = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 2.,
            fr = """TAUX DE CROISSANCE ALGALE MAXIAMALE A 20C""",
            ang = """MAXIMUM ALGAL GROWTH RATE AT 20C""",
        ),
#       -----------------------------------
        SECCHI_DEPTH = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """EN M""",
            ang = """IN M""",
        ),
#       -----------------------------------
        VEGETAL_TURBIDITY_COEFFICIENT_WITHOUT_PHYTO = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """COEFFICIENT DE TURBIDITE VEGETALE SANS
        PHYTOPLANCTONS - EN m-1""",
            ang = """COEFFICIENT OF VEGATAL TURBIDITY WITHOUT
         PHYTOPLANKTON - in m-1""",
        ),
#       -----------------------------------
        PARAMETER_OF_CALIBRATION_OF_SMITH_FORMULA = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [120.],
            fr = """EN W/m2""",
            ang = """IN W/m2""",
        ),
#       -----------------------------------
        CONSTANT_OF_HALF_SATURATION_WITH_PHOSPHATE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.005],
            fr = """EN mgP/l""",
            ang = """IN mgP/l""",
        ),
#       -----------------------------------
        CONSTANT_OF_HALF_SATURATION_WITH_NITROGEN = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.03],
            fr = """en mgN/l""",
            ang = """in mgN/l""",
        ),
#       -----------------------------------
        ALGAL_TOXICITY_COEFFICIENTS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            defaut = [1.,0.],
            fr = """ALPHA1 ET ALPHA2""",
            ang = """ALPHA1 AND ALPHA2""",
        ),
#       -----------------------------------
        RESPIRATION_RATE_OF_ALGAL_BIOMASS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.05],
            fr = """EN J-1, POUR 20 C""",
            ang = """IN J-1, FOR 20 c""",
        ),
#       -----------------------------------
        PROPORTION_OF_PHOSPHORUS_WITHIN_PHYTO_CELLS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.0025],
            fr = """EN Mgp/microgchla""",
            ang = """IN Mgp/microgchla""",
        ),
#       -----------------------------------
        PERCENTAGE_OF_PHYSPHORUS_ASSIMILABLE_IN_DEAD_PHYTO = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.5],
            fr = """EN POURCENTAGE""",
            ang = """IN PERCENTAGE""",
        ),
#       -----------------------------------
        RATE_OF_TRANSFORMATION_OF_POR_TO_PO4 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.03],
            fr = """EN J-1""",
            ang = """IN J-1""",
        ),
#       -----------------------------------
        PROPORTION_OF_NITROGEN_WITHIN_PHYTO_CELLS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.0035],
            fr = """(fn IN DOCS) EN Mgp/microgchla""",
            ang = """IN Mgp/microgchla""",
        ),
#       -----------------------------------
        PERCENTAGE_OF_NITROGEN_ASSIMILABLE_IN_DEAD_PHYTO = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.5],
            fr = """(dtn IN DOC) EN POURCENTAGE""",
            ang = """IN PERCENTAGE""",
        ),
#       -----------------------------------
        COEFFICIENTS_OF_ALGAL_MORTALITY_AT_20C = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            defaut = [0.1,0.003],
            fr = """""",
            ang = """""",
        ),
#       -----------------------------------
        SEDIMENTATION_VELOCITY_OF_ORGANIC_LOAD = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """EN M/S""",
            ang = """IN M/S""",
        ),
#       -----------------------------------
        CONSTANT_OF_DEGRADATION_OF_ORGANIC_LOAD_K1 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.25],
            fr = """en J-1""",
            ang = """in J-1""",
        ),
#       -----------------------------------
        CONSTANT_OF_NITRIFICATION_KINETIC_K4 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.35],
            fr = """en J-1""",
            ang = """in J-1""",
        ),
#       -----------------------------------
        PHOTOSYNTHESIS_P = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.],
            fr = """en mgO2/J/l""",
            ang = """in mgO2/J:l""",
        ),
#       -----------------------------------
        VEGERAL_RESPIRATION_R = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.06],
            fr = """en mgO2/J/l""",
            ang = """in mgO2/J/l""",
        ),
#       -----------------------------------
        WATER_TEMPERATURE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [7.],
            fr = """en $^{circ}$C, TEMPERATURE MOYENNE DE L EAU
     NECESSAIRE POUR CALCULER LES VALEURS DE CS""",
            ang = """in $^{circ}$C, MEAN TEMPERATURE NECESARY FOR
      COMPUTING DIFFERENT VALUES OF CS""",
        ),
#       -----------------------------------
        EROSION_RATE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """""",
            ang = """""",
        ),
#       -----------------------------------
        SEDIMENTATION_CRITICAL_STRESS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [5.],
            fr = """en PA""",
            ang = """in PA""",
        ),
#       -----------------------------------
        CRITICAL_STRESS_OF_RESUSPENSION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1000.],
            fr = """en PA""",
            ang = """in PA""",
        ),
#       -----------------------------------
        SEDIMENT_SETTLING_VELOCITY = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [6.E-6],
            fr = """en M/S""",
            ang = """in M/S""",
        ),
#       -----------------------------------
        EXPONENETIAL_DESINTEGRATION_CONSTANT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.13E-7],
            fr = """en S-1, loi de decroissance exponentielle comme celle de la
         radioactivite""",
            ang = """in S-1, exponential decrease law like the one of radioactivity""",
        ),
#       -----------------------------------
        COEFFICIENT_OF_DISTRIBUTION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1775.],
            fr = """en M3/KG ou l/g""",
            ang = """in M3/KG or l/g""",
        ),
#       -----------------------------------
        CONSTANT_OF_DESORPTION_KINETIC = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [2.5E-7],
            fr = """en S-1""",
            ang = """in S-1""",
        ),
#       -----------------------------------
        WATER_SPECIFIC_HEAT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [4180.],
            fr = """en J/KG$^{circ}$C""",
            ang = """in J/KG$^{circ}$C""",
        ),
#       -----------------------------------
        AIR_SPECIFIC_HEAT = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1005.],
            fr = """en J/KG$^{circ}$C""",
            ang = """in J/KG$^{circ}$C""",
        ),
#       -----------------------------------
        COEFFICIENTS_OF_AERATION_FORMULA = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            defaut = [0.002,0.0012],
            fr = """""",
            ang = """""",
        ),
#       -----------------------------------
        COEFFICIENT_OF_CLOUDING_RATE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.2,
            fr = """""",
            ang = """""",
        ),
#       -----------------------------------
        COEFFICIENTS_FOR_CALIBRATING_ATMOSPHERIC_RADIATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.97,
            fr = """""",
            ang = """""",
        ),
#       -----------------------------------
        COEFFICIENTS_FOR_CALIBRATING_SURFACE_WATER_RADIATION = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.97,
            fr = """""",
            ang = """""",
        ),
    ),
#   -----------------------------------
    BIOMASS = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        RATE_OF_TRANSFORMATION_OF_NOR_TO_NO3 = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [0.],
            fr = """TAUX DE TRANSFORMATION DU NOR EN NO3 PAR LE BIAIS DE LA
        MINERALISATION BACTERIENNE EN J-1""",
            ang = """RATE OF TRANSFOMATION OF NOR TO NO3 BY BACTERIA MINERALIZATION
        IN J-1""",
        ),
#       -----------------------------------
        SUNSHINE_FLUX_DENSITY_ON_WATER_SURFACE = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R',
            defaut = 0.,
            fr = """DENSITE DE FLUX DU RAYONNEMENT SOLAIRE A LA SURFACE DE L EAU
        EN W/m2""",
            ang = """DENSITY OF SUNSHINE FLUX ON THE WATER SURFACE
        IN W/m2""",
        ),
    ),
#   -----------------------------------
    SOURCES = FACT(statut='o',
#   -----------------------------------
#       -----------------------------------
        WEIR_REAERATION_COEFFICIENT_RS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min=0, max='**',
            defaut = [1.0],
            fr = """EN J-1""",
            ang = """IN J-1""",
        ),
#       -----------------------------------
        FORMULA_FOR_COMPUTING_RS = SIMP(statut ='f',
#       -----------------------------------
            typ = 'I', min=0, max='**',
            defaut = [0],
            fr = """DONNE LE CHOIX DE CALCUL DE LA FORMULE DE RS DE LA
REAERATION NATURELLE AU NIVEAU DES SEUILS,
LES OPTIONS SONT LES SUIVANTS:
  0- RS CONSTANT, VALEUR DE RS=1.0
  1- FORMULE DE GAMESON 1
  2- FORMULE DE GAMESON 2
  3- FORMULE DE WRL1
  4- FORMULE DE WRL2""",
            ang = """GIVES HOW TO CUMPUTE THE WEIR REAERATION COEFFICIENT RS
OPTIONS ARE:
  0- RS CONSTANT, IN THIS CASE RS=1.0
  1- FORMULA OF GAMESON 1
  2- FORMULA OF GAMESON 2
  3- FORMULA OF WRL 1
  4- FORMULA OF WRL2""",
        ),
#       -----------------------------------
        COEFFICIENTS_A_AND_B_FOR_RS_FORMULA = SIMP(statut ='f',
#       -----------------------------------
            typ = 'R', min= 2, max= 2,
            defaut = [1.2,0.7],
            fr = """COEFFICIIENTS INTERVENANT DANS LE CALCUL DE RS
A EST ENTRE 0.65 (EAU TR�S POLLUEE) ET 1.8 (EAU TR�S CLAIRE)
B VARIE BEAUCOUP (VOIR TABLEAU DANS LA DOC)""",
            ang = """COEFFICIENTS NEEDED FOR THE CALUCLATION OF RS
A IS BETWEEN 0.65(VERY POLLUTED WATER AND 1.8 (VERY CLEAR WATER))""",
        ),
    ),
)
# -----------------------------------------------------------------------
PHYSICAL_PARAMETERS = PROC(nom= "PHYSICAL_PARAMETERS",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    ATMOSPHERE_WATER_EXCHANGE_MODEL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min=0, max='**',
        into = ["NO MODEL","LINEARISED FORMULA AT THE FREE SURFACE","MODEL WITH COMPLETE BALANCE"],
        defaut = ["NO MODEL"],
        fr = """Choix du modele d echanges entre l eau et l atmosphere""",
        ang = """Choice of the atmosphere-water exchange model.""",
    ),
#   -----------------------------------
    LIGHTNESS_OF_THE_SKY = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ["VERY BRIGHT, PURE SKY","MODERATELY BRIGHT SKY","FOGGY LIKE THE SKY OF INDUSTRIAL AREA "],
        defaut = "MODERATELY BRIGHT SKY",
        fr = """degre de clarte (purete) du ciel""",
        ang = """how the sky is bright (pure).""",
    ),
#   -----------------------------------
    COEFFICIENT_TO_CALIBRATE_THE_ATMOSPHERE_WATER_EXCHANGE_MODEL = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.0025],
        fr = """Valeur du coefficient de calage pour la fonction de vent
dans les modeles d echanges eau-atmosphere
(formule linearisee a la surface ou bilan complet).
Une valeur comprise entre 0.0017 et 0.0035 est conseillee""",
        ang = """Value of the calibration coefficient for the wind function
of the atmosphere-water exchange models
(linearised formula at the free surface or complete balance).
A value between 0.0017 and 0.0035 is advised""",
    ),
#   -----------------------------------
    EVAPORATION_RATE = SIMP(statut ='f',
#   -----------------------------------
        typ = 'R', min=0, max='**',
        defaut = [0.],
        fr = """taux d evaporation- meme unite que la pluie en m3/s/m2""",
        ang = """rate of evaporation - same unit as rainfall in m3/s/m2""",
    ),
#   -----------------------------------
    FORMULA_OF_ATMOSPHERIC_RADIATION = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM',
        into = ["IDSO AND JACKSON (1969)","SWINBANK (1963)","BRUTSAERT (1975)","YAJIMA TONO DAM (2014)"],
        defaut = "SWINBANK (1963)",
        fr = """Formule au choix pour le calcul du rayonnement atmospherique.
Voir GLM.""",
        ang = """Formula to be chosen to compute the atmospheric radiation.
See GLM.""",
    ),
)
# -----------------------------------------------------------------------
BIOMASS,WQ = PROC(nom= "BIOMASS,WQ",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    METHOD_OF_COMPUTATION_OF_RAY_EXCTINCTION_COEFFICIENT = SIMP(statut ='o',
#   -----------------------------------
        typ = 'I',
        defaut = 1,
        fr = """Choix de la methode de calcul du coefficient d extinction
       du rayonnement solaire- les choix sont:
        1 - formule d Atkins (1.7/secchi)
        2 - formule de Moss""",
        ang = """choice of the method of calculation of the extinction of
        sun ray - the choices are :
        1- Atkins formula
        2- Moss formula""",
    ),
)
# -----------------------------------------------------------------------
FILES = PROC(nom= "FILES",op = None,
# -----------------------------------------------------------------------
#   -----------------------------------
    LIST_OF_FILES = SIMP(statut ='f',
#   -----------------------------------
        typ = 'TXM', min= 7, max= 7,
        defaut = 'STEERING FILE;RESULTS FILE;GEOMETRY FILE;BOUNDARY CONDITIONS FILE;FICHIER HYDRODYNAMIQUE;REFERENCE FILE;DICTIONARY',
        fr = """Noms des fichiers exploites par le code""",
        ang = """File names of the used files""",
    ),
)
Ordre_des_commandes = (
'INPUT_OUTPUT,_FILES',
'INPUT_OUTPUT,_INFORMATION',
'IN_OUT,WQ',
'INPUT_OUTPUT,_GRAPHICS_AND_LISTING',
'RESULTS',
'DATA_FILES',
'MISCELLANEOUS',
'PHYSICS',
'SUSPENSION',
'WAQ_PARAMETERS',
'PHYSICAL_PARAMETERS',
'BIOMASS,WQ',
'FILES')
