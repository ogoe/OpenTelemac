C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DECLARATION OF PRINICIPAL ARTEMIS VARIABLES

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF_DEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Internal(s)
!>    </th><td> ALEMON, ALEMUL, ALFABJ, ALFAP, ALFAPT, AM1, AM2, AM3, APHI1B, APHI2B, APHI3B, APHI4B, ARTBI1, ARTBI2, ARTCAS, ARTCLI, ARTFO1, ARTFO2, ARTFON, ARTGEO, ARTRBI, ARTREF, ARTRES, ARTRFO, ART_FILES, BALAYE, BINGEO, BINRES, BM1, BM2, BPHI1B, BPHI2B, BPHI3B, BPHI4B, C, CDTINI, CG, CGT, COTINI, COURANT, CPHI1B, CPHI2B, CPHI3B, CPHI4B, CTT, CURRENTX, CURRENTY, CV1, CV2, DALE, DEBLIQ, DEBSOL, DEFERL, DIAM50, DIAM90, DIM, DISESP, DPHI1B, DPHI2B, DPHI3B, DPHI4B, ENTFW, ENTREG, ENTRUG, EPSDIS, EQUA, EXPOS, FFON, FINLIQ, FINSOL, FORMFR, FROTTE, FW, FWCOEF, FX, FY, GAMMA, GAMMAS, GDALLY, GRAV, H, HALE, HAUTIN, HB, HBT, HHO, HMIN, HMU, HMUANC, IBREAK, IELM, IELM0, IELMB, IELMB0, IKLE, INCI, INFOGR, ISOLVE, IT1, IT2, IT3, I_ORIG, J_ORIG, K, KDALLY, KFROT, KN1, KN2, KNANC1, KNANC2, KP1BOR_TOT, KT, LEOPRD, LIDIR, LIDIRT, LIHBOR, LIHBORT, LISFON, LISHOU, LISPRD, LISTIN, LIUBOR, LIVBOR, LV, LVMAC, MARDAT, MARTIM, MASK1, MASK1T, MASK2, MASK2T, MASK3, MASK3T, MASK4, MASK4T, MASKEL, MAT, MAXFRO, MAXLU_ART, MAXVAR, MBOR, MCOS, MESH, MSIN, MSK, MU, MU2, MVEAU, MVSED, MXELVS, MXPTVS, NBOR_TOT, NDALE, NELEM, NELMAX, NFRLIQ, NFRSOL, NITDIS, NITMAX, NPALE, NPMAX, NPOIN, NPOIN_TOT, NPRIV, NPTFR, NPTFRX, NPTFR_TOT, NUMLIQ, OMEGA, OPTASS, PALE, PER, PERDEB, PERFIN, PERPAS, PERPIC, PHAS, PHIB, PHII, PHIIB, PHIR, PHIRB, PMAX, PMIN, PRIVE, PRODUC, PTINIG, PTINIL, QB, REGIDO, RELAX, RELDIS, RHS, RICOEF, RP, RPT, S, SBID, SLVART, SORIMP, SORLEO, SPHERI, STDGEO, STDRES, SXX, SXY, SYY, T01, T02, T1, T10, T11, T12, T2, T3, T4, T5, T6, T7, T8, T9, TB, TBBD, TBD1, TBD2, TBD3, TBD4, TETAB, TETABT, TETAH, TETAP, TETAPT, TETMAX, TETMIN, TEXTE, TEXTPR, TITCAS, TM, TYPELM, U0, UC, UNK, V0, VALID, VARCLA, VARDES, VARIMP, VARSOR, VC, VISCO, W1, WR, X, XT, Y, YT, ZF
!>   </td></tr>
!>     </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
      MODULE DECLARATIONS_ARTEMIS
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF_DEF

!> @brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
C
      INTEGER NPTFR_TOT
!> @brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
C
      INTEGER NPOIN_TOT
!> @brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
C
      INTEGER, ALLOCATABLE :: KP1BOR_TOT(:)
!> @brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
C
      INTEGER, ALLOCATABLE :: NBOR_TOT(:)
!> @brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
C
      INTEGER, ALLOCATABLE :: LIHBORT(:)
!> @brief TO MANAGE THE GLOBAL BUILDING OF THE BOUNDARY
C
      INTEGER, ALLOCATABLE :: LIDIRT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: MASK1T(:),MASK2T(:),MASK3T(:),MASK4T(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: XT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: YT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: CGT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: KT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: CTT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: RPT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: ALFAPT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: HBT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: TETABT(:)
!> @brief
C
      DOUBLE PRECISION, ALLOCATABLE :: TETAPT(:)
C
C       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
C
C       1) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       2) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       3) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       4) INTEGERS
C       5) LOGICAL VALUES
C       6) REALS
C       7) STRINGS
C       8) SLVCFG STRUCTURES
C       9) MESH STRUCTURE
C      10) ALIASES
C
C-----------------------------------------------------------------------
C
C       1) VECTORS
C
C-----------------------------------------------------------------------
C
!> @brief REAL PART OF WAVE POTENTIAL
C partie reelle du potentiel
      TYPE(BIEF_OBJ), TARGET :: PHIR
!> @brief IMAGINARY PART OF WAVE POTENTIAL
C partie imaginaire du potentiel
      TYPE(BIEF_OBJ), TARGET :: PHII
!> @brief WATER DEPTH AT REST
C hauteur d'eau au repos
      TYPE(BIEF_OBJ), TARGET :: H
!> @brief WAVE NUMBER
C nombre d'onde
      TYPE(BIEF_OBJ), TARGET :: K
!> @brief PHASE CELERITY
C vitesse de phase
      TYPE(BIEF_OBJ), TARGET :: C
!> @brief GROUP CELERITY
C vitesse de groupe
      TYPE(BIEF_OBJ), TARGET :: CG
!> @brief WAVE HEIGHT (REGULAR MODE)
C hauteur de la houle
      TYPE(BIEF_OBJ), TARGET :: HHO
!> @brief WAVE PHASE (REGULAR MODE)
C phase de la houle
      TYPE(BIEF_OBJ), TARGET :: PHAS
!> @brief SURFACE WAVE VELOCITY COMPONENT
C vitesse en surface (a t=0)              !!!!! really? !!!!!
      TYPE(BIEF_OBJ), TARGET :: U0
!> @brief SURFACE WAVE VELOCITY COMPONENT
C vitesse en surface (a t=0)              !!!!! really? !!!!!
      TYPE(BIEF_OBJ), TARGET :: V0
!> @brief MEAN COSINE OF WAVE DIRECTION
C moyennes des cosinus de la direction de houle
      TYPE(BIEF_OBJ), TARGET :: MCOS
!> @brief MEAN SINE OF WAVE DIRECTION
C moyennes des sinus de la direction de houle
      TYPE(BIEF_OBJ), TARGET :: MSIN
!> @brief WAVE INCIDENCE (OR DIRECTION)
C incidence de la houle
      TYPE(BIEF_OBJ), TARGET :: INCI
!> @brief FREE SURFACE ELEVATION
C cote de la surface libre
      TYPE(BIEF_OBJ), TARGET :: S
!> @brief BOTTOM ELEVATION
C cote du fond
      TYPE(BIEF_OBJ), TARGET :: ZF
!> @brief FRICTION FACTOR
C coefficient de frottement (variable en espace)
      TYPE(BIEF_OBJ), TARGET :: FW
!> @brief WAVE HEIGHT (RANDOM WAVE)
C hauteur de la houle aleatoire
      TYPE(BIEF_OBJ), TARGET :: HALE
!> @brief WAVE PERIODS ARRAY (RANDOM MODE)
C tableau des periodes de discretisation du spectre pour un calcul en houle aleatoire multidirectionnelle
      TYPE(BIEF_OBJ), TARGET :: PALE
!> @brief REFLEXION COEFFICIENT
C coefficient de reflexion des parois
      TYPE(BIEF_OBJ), TARGET :: RP
!> @brief ANGLE OF WAVE ATTACK (FROM X AXIS)
C angle d'attaque de la houle sur les limites - pas seulement les parois (compte par rapport a a la normale exterieure dans le sens direct)some sources say (compte par rapport a l'axe des x)
      TYPE(BIEF_OBJ), TARGET :: TETAP
!> @brief DEPHASING CAUSED BY THE WALLS
C dephasage induit par la paroi entre l'onde reflechie et l'onde incidente (si alfap est positif, l'onde reflechie est en retard)
      TYPE(BIEF_OBJ), TARGET :: ALFAP
!> @brief INCIDENT WAVE HEIGHT AT THE BOUNDARY
C hauteur de la houle aux frontieres ouvertes
      TYPE(BIEF_OBJ), TARGET :: HB
!> @brief INCIDENT WAVE DIRECTION AT THE BOUNDARY
C angle d'attaque de la houle aux frontieres ouvertes (compte par rapport a l'axe des x dans le sens direct)
      TYPE(BIEF_OBJ), TARGET :: TETAB
!> @brief REAL PART OF INCIDENT WAVE AT THE BOUNDARY
C partie reelle du potentiel impose au bord (dirichlet)
      TYPE(BIEF_OBJ), TARGET :: PHIRB
!> @brief IMAGINARY PART OF INCIDENT WAVE AT THE BOUNDARY
C partie imaginaire du potentiel impose au bord (dirichlet)
      TYPE(BIEF_OBJ), TARGET :: PHIIB
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI1B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI1B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI1B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI1B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI2B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI2B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI2B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI2B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI3B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI3B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI3B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI3B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: APHI4B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: BPHI4B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: CPHI4B
!> @brief COEFFICIENT FOR BOUNDARY CONDITIONS
C coefficient pour les conditions aux limites
      TYPE(BIEF_OBJ), TARGET :: DPHI4B
!> @brief WORKING ARRAY
C tableau de travail
      TYPE(BIEF_OBJ), TARGET :: W1
!> @brief INTEGER WORKING ARRAY
C
      TYPE(BIEF_OBJ), TARGET :: IT1
!> @brief INTEGER WORKING ARRAY
C
      TYPE(BIEF_OBJ), TARGET :: IT2
!> @brief INTEGER WORKING ARRAY
C
      TYPE(BIEF_OBJ), TARGET :: IT3
!> @brief VOID STRUCTURE
C
      TYPE(BIEF_OBJ), TARGET :: SBID
!> @brief RIGHT MEMBER OF SYSTEM TO BE SOLVED
C
      TYPE(BIEF_OBJ), TARGET :: CV1
!> @brief RIGHT MEMBER OF SYSTEM TO BE SOLVED
C
      TYPE(BIEF_OBJ), TARGET :: CV2
!> @brief WAVE DISSIPATION QUANTITY
C tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: MU
!> @brief WAVE DISSIPATION QUANTITY
C tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: MU2
!> @brief WAVE DISSIPATION QUANTITY
C tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: QB
!> @brief WAVE DISSIPATION QUANTITY
C tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: HMU
!> @brief WAVE DISSIPATION QUANTITY
C tableau pour la dissipation
      TYPE(BIEF_OBJ), TARGET :: HMUANC
!> @brief RADIATION STRESSES QUANTITY
C tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SXX
!> @brief RADIATION STRESSES QUANTITY
C tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SXY
!> @brief RADIATION STRESSES QUANTITY
C tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: SYY
!> @brief RADIATION STRESSES QUANTITY
C tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: FX
!> @brief RADIATION STRESSES QUANTITY
C tableau pour les contraintes de radiation
      TYPE(BIEF_OBJ), TARGET :: FY
!> @brief MEAN WAVE PERIOD
C periode moyenne issue du moment d'ordre 1
      TYPE(BIEF_OBJ), TARGET :: T01
!> @brief MEAN WAVE PERIOD
C periode moyenne issue du moment d'ordre 2
      TYPE(BIEF_OBJ), TARGET :: T02
!> @brief MEAN WAVE PERIOD
C periode moyenne issue du moment d'ordre 1
      TYPE(BIEF_OBJ), TARGET :: TM
!> @brief WAVE DIRECTIONS AT THE BOUNDARY (RANDOM MODE)
C
      TYPE(BIEF_OBJ), TARGET :: DALE
!> @brief BOUNDARY CONDITION TYPE
C type de conditions aux limites sur u
      TYPE(BIEF_OBJ), TARGET :: LIUBOR
!> @brief BOUNDARY CONDITION TYPE
C type de conditions aux limites sur v
      TYPE(BIEF_OBJ), TARGET :: LIVBOR
!> @brief BOUNDARY CONDITION TYPE
C type de conditions aux limites sur h
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: LIDIR
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!> @brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO INCIDENT WAVES (KINC)
C masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK1
!> @brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO FREE EXIT (KSORT)
C masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK2
!> @brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO SOLID BOUNDARY (KLOG)
C masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK3
!> @brief MASKS FOR BOUNDARY NODES, CORRESPONDS TO IMPOSED WAVES (KENT)
C masque pour les points de bord
      TYPE(BIEF_OBJ), TARGET :: MASK4
!> @brief FLOW
C courant
      TYPE(BIEF_OBJ), TARGET :: UC
!> @brief FLOW
C courant
      TYPE(BIEF_OBJ), TARGET :: VC
!> @brief RELATIVE ANGULAR FREQUENCY
C pulsation relative
      TYPE(BIEF_OBJ), TARGET :: WR
!> @brief WAVE VECTOR
C vecteur d'onde
      TYPE(BIEF_OBJ), TARGET :: KN1
!> @brief WAVE VECTOR
C vecteur d'onde
      TYPE(BIEF_OBJ), TARGET :: KN2
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: KNANC1,KNANC2
C
C-----------------------------------------------------------------------
C
C       2) MATRICES
C
C-----------------------------------------------------------------------
C
!> @brief MATRICE FOR SYSTEM SOLVING
C
      TYPE(BIEF_OBJ), TARGET :: AM1
!> @brief MATRICE FOR SYSTEM SOLVING
C
      TYPE(BIEF_OBJ), TARGET :: AM2
!> @brief MATRICE FOR SYSTEM SOLVING
C
      TYPE(BIEF_OBJ), TARGET :: AM3
!> @brief MATRICE FOR SYSTEM SOLVING
C
      TYPE(BIEF_OBJ), TARGET :: BM1
!> @brief MATRICE FOR SYSTEM SOLVING
C
      TYPE(BIEF_OBJ), TARGET :: BM2
!> @brief MATRICE FOR SYSTEM SOLVING
C
      TYPE(BIEF_OBJ), TARGET :: MBOR
C
C-----------------------------------------------------------------------
C
C       3) BLOCKS
C
C-----------------------------------------------------------------------
C
C
!> @brief BLOCK OF POTENTIAL VECTORS
C
      TYPE(BIEF_OBJ), TARGET :: PHIB
!> @brief BLOCK OF WORKING ARRAYS
C
      TYPE(BIEF_OBJ), TARGET :: TB
!> @brief BLOCK OF WORKING ARRAYS
C
      TYPE(BIEF_OBJ), TARGET :: TBBD
!> @brief BLOCK OF PRIVATE VECTORS
C tableaux reserves a l'utilisateur
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!> @brief BLOCK OF MATRICES
C
      TYPE(BIEF_OBJ), TARGET :: MAT
!> @brief BLOCK OF UNKNOWN VECTORS
C
      TYPE(BIEF_OBJ), TARGET :: UNK
!> @brief BLOCK OF RIGHT HAND SIDE VECTORS IN SOLVING SYSTEM
C
      TYPE(BIEF_OBJ), TARGET :: RHS
!> @brief BLOCK OF VARIABLES FOR OUTPUT
C
      TYPE(BIEF_OBJ), TARGET :: VARSOR
C
C-----------------------------------------------------------------------
C
C       4) INTEGERS
C
C-----------------------------------------------------------------------
C
!> @brief
C maximum de variables de sortie
      INTEGER, PARAMETER :: MAXVAR = 100
!> @brief
C maximum de frontieres liquides
      INTEGER, PARAMETER :: MAXFRO = 100
!> @brief ORIGIN COORDINATE
C coordonnee de l'origine
      INTEGER I_ORIG
!> @brief ORIGIN COORDINATE
C coordonnee de l'origine
      INTEGER J_ORIG
!> @brief GRAPHIC PRINTOUT PERIOD
C periode de sortie graphique
      INTEGER LEOPRD
!> @brief LISTING PRINTOUT PERIOD
C periode de sortie listing
      INTEGER LISPRD
!> @brief MAXIMUM NUMBER OF ITERATIONS FOR SOLVER
C maximum d'iterations pour le solveur
      INTEGER NITMAX
!> @brief GEOMETRY FILE STANDARD
C standard du fichier de geometrie
      INTEGER STDGEO
!> @brief RESULTS FILE STANDARD
C standard du fichier des resultats
      INTEGER STDRES
!> @brief SOLVER OPTION
C option du solveur
      INTEGER ISOLVE(2)
!> @brief BOTTOM TOPOGRAPHY SMOOTHINGS
C nombre de lissages du fond
      INTEGER LISFON
!> @brief DISCRETISATION IN SPACE
C discretisation en espace
      INTEGER DISESP
!> @brief NUMBER OF DISCRETISED PERIODS
C nombre de periodes de discretisation du spectre de houle
      INTEGER NPALE
!> @brief NUMBER OF DISCRETISED DIRECTIONS
C nombre de directions de discretisation du spectre de houle
      INTEGER NDALE
!> @brief MATRIX STORAGE
C stockage des matrices
      INTEGER OPTASS
!> @brief BREAKING LAW
C formulation du deferlement
      INTEGER IBREAK
!> @brief MAXIMUM OF SUB-ITERATIONS
C maximum de sous-iterations
      INTEGER NITDIS
!> @brief VECTOR LENGTH
C longueur du vecteur
      INTEGER LVMAC
!> @brief LAW OF BOTTOM FRICTION
C loi de frottement sur le fond
      INTEGER KFROT
!> @brief BOTTOM FRICTION LAW
C formulation du frottement de fond
      INTEGER FORMFR
!> @brief HYDRAULIC REGIME TYPE
C type du regime hydraulique
      INTEGER REGIDO
!> @brief MATRIX-VECTOR PRODUCT
C produit matrice-vecteur
      INTEGER PRODUC
!> @brief NUMBER OF PRIVATE ARRAYS
C nombre de tableaux prives
      INTEGER NPRIV
!> @brief
C
      INTEGER PTINIG
!> @brief
C
      INTEGER PTINIL
!> @brief
C type d'element
      INTEGER IELM
!> @brief
C
      INTEGER IELM0
!> @brief
C type d'element de bord
      INTEGER IELMB
!> @brief
C
      INTEGER IELMB0
!> @brief ORIGINAL DATE OF TIME
C date de l'origine des temps
      INTEGER MARDAT(3)
!> @brief ORIGINAL HOUR OF TIME
C heure de l'origine des temps
      INTEGER MARTIM(3)
!> @brief
C
      INTEGER NFRLIQ
!> @brief
C
      INTEGER NFRSOL
!> @brief
C
      INTEGER DEBLIQ(MAXFRO)
!> @brief
C
      INTEGER FINLIQ(MAXFRO)
!> @brief
C
      INTEGER DEBSOL(MAXFRO)
!> @brief
C
      INTEGER FINSOL(MAXFRO)
C
C-----------------------------------------------------------------------
C
C       5) LOGICAL VALUES
C
C-----------------------------------------------------------------------
C
!> @brief LISTING PRINTOUT
C si oui, sortie listing
      LOGICAL LISTIN
!> @brief
C
      LOGICAL INFOGR
!> @brief PERIOD SCANNING
C si oui, balayage en periodes
      LOGICAL BALAYE
!> @brief MONODIRECTIONAL RANDOM WAVE
C si oui, houle aleatoire monodirectionnelle
      LOGICAL ALEMON
!> @brief MULTIDIRECTIONAL RANDOM WAVE
C si oui, houle aleatoire multidirectionnelle
      LOGICAL ALEMUL
!> @brief
C
      LOGICAL MSK
!> @brief
C
      LOGICAL SPHERI
!> @brief BREAKING
C si oui, deferlement
      LOGICAL DEFERL
!> @brief FRICTION
C si oui, frottement
      LOGICAL FROTTE
!> @brief FRICTION FACTOR IMPOSED
C si oui, facteur de frottement impose
      LOGICAL ENTFW
!> @brief HYDRAULIC REGIME IMPOSED
C si oui, regime hydraulique impose
      LOGICAL ENTREG
!> @brief SKIN ROUGHNESS ONLY
C si oui, rugosite de peau seule
      LOGICAL ENTRUG
!> @brief WAVE HEIGHTS SMOOTHING
C si oui, lissage des hauteurs de houle
      LOGICAL LISHOU
!> @brief
C
      LOGICAL SORLEO(MAXVAR)
!> @brief
C
      LOGICAL SORIMP(MAXVAR)
!> @brief VALIDATION
C si oui, validation
      LOGICAL VALID
!> @brief
C
      LOGICAL COURANT
C
C-----------------------------------------------------------------------
C
C       6) REALS
C
C-----------------------------------------------------------------------
C
!> @brief GRAVITY ACCELERATION
C acceleration de la pesanteur
      DOUBLE PRECISION GRAV
!> @brief MINIMUM VALUE FOR H
C valeur minimum de h
      DOUBLE PRECISION HMIN
!> @brief WAVE PERIOD
C periode de la houle en cours de calcul
      DOUBLE PRECISION PER
!> @brief ANGULAR FREQUENCY
C pulsation de la houle
      DOUBLE PRECISION OMEGA
!> @brief DIRECTION OF WAVE PROPAGATION
C direction principale de propagation de la houle
      DOUBLE PRECISION TETAH
!> @brief INITIAL WATER LEVEL
C cote initiale
      DOUBLE PRECISION COTINI
!> @brief INITIAL DEPTH
C hauteur initiale
      DOUBLE PRECISION HAUTIN
!> @brief BEGINNING PERIOD FOR PERIOD SCANNING
C periode de debut pour le balayage en periode
      DOUBLE PRECISION PERDEB
!> @brief ENDING PERIOD FOR PERIOD SCANNING
C periode de fin pour le balayage en periode
      DOUBLE PRECISION PERFIN
!> @brief STEP FOR PERIOD SCANNING
C pas pour le balayage en periode
      DOUBLE PRECISION PERPAS
!> @brief PEAK PERIOD
C periode de pic
      DOUBLE PRECISION PERPIC
!> @brief GAMMA
C gamma
      DOUBLE PRECISION GAMMA
!> @brief MINIMUM ANGLE OF PROPAGATION
C valeur minimum de l'angle de propagation
      DOUBLE PRECISION TETMIN
!> @brief MAXIMUM ANGLE OF PROPAGATION
C valeur maximum de l'angle de propagation
      DOUBLE PRECISION TETMAX
!> @brief S EXPONENT
C exposant s dans la formule du spectre
      DOUBLE PRECISION EXPOS
!> @brief
C
      DOUBLE PRECISION RELAX
!> @brief FRICTION COEFFICIENT
C coefficient de frottement
      DOUBLE PRECISION FFON
!> @brief SUB-ITERATIONS ACCURACY
C precision sur les sous-iterations
      DOUBLE PRECISION EPSDIS
!> @brief DISSIPATION RELAXATION
C relaxation sur la dissipation
      DOUBLE PRECISION RELDIS
!> @brief ALPHA
C alpha
      DOUBLE PRECISION ALFABJ
!> @brief GAMMAS
C gammas
      DOUBLE PRECISION GAMMAS
!> @brief
C
      DOUBLE PRECISION KDALLY
!> @brief
C
      DOUBLE PRECISION GDALLY
!> @brief FLUID KINEMATIC VISCOSITY
C viscosite cinematique du fluide
      DOUBLE PRECISION VISCO
!> @brief DIAMETER90
C diametre90
      DOUBLE PRECISION DIAM90
!> @brief DIAMETER50
C diametre50
      DOUBLE PRECISION DIAM50
!> @brief SEDIMENT SPECIFIC WEIGHT
C masse volumique du sediment
      DOUBLE PRECISION MVSED
!> @brief FLUID SPECIFIC MASS
C masse volumique du fluide
      DOUBLE PRECISION MVEAU
!> @brief FRICTION FACTOR
C coefficient de frottement constant impose
      DOUBLE PRECISION FWCOEF
!> @brief RIPPLES COEFFICIENT
C coefficient de rides
      DOUBLE PRECISION RICOEF
!> @brief MINIMUM SPECTRAL PERIOD
C periode minimum du spectre
      DOUBLE PRECISION PMIN
!> @brief MAXIMUM SPECTRAL PERIOD
C periode maximum du spectre
      DOUBLE PRECISION PMAX
!> @brief
C courant : valeurs en x
      DOUBLE PRECISION CURRENTX
!> @brief
C courant : valeurs en y
      DOUBLE PRECISION CURRENTY
C
C-----------------------------------------------------------------------
C
C       7) STRINGS
C
C-----------------------------------------------------------------------
C
!> @brief TITLE
C titre de l'etude
      CHARACTER*72 TITCAS
!> @brief
C
      CHARACTER*72 VARDES
!> @brief VARIABLES TO BE PRINTED
C variables a imprimer
      CHARACTER*72 VARIMP
!> @brief INITIAL CONDITIONS
C conditions initiales
      CHARACTER*72 CDTINI
!> @brief GEOMETRY FILE BINARY
C binaire du fichier de geometrie
      CHARACTER*3 BINGEO
!> @brief RESULTS FILE BINARY
C binaire du fichier des resultats
      CHARACTER*3 BINRES
!> @brief
C
      CHARACTER*20 EQUA
!> @brief
C
      CHARACTER*32 VARCLA(10)
!> @brief
C
      CHARACTER*32 TEXTE(MAXVAR)
!> @brief
C
      CHARACTER*32 TEXTPR(MAXVAR)
C
C-----------------------------------------------------------------------
C
C       8) SLVCFG STRUCTURES
C
C-----------------------------------------------------------------------
C
!> @brief SLVCFG STRUCTURE
C
      TYPE(SLVCFG) :: SLVART
C
C-----------------------------------------------------------------------
C
C       9) MESH STRUCTURE
C
C-----------------------------------------------------------------------
C
!> @brief MESH STRUCTURE
C
      TYPE(BIEF_MESH) :: MESH
C
C-----------------------------------------------------------------------
C
C      10) ALIASES
C
C-----------------------------------------------------------------------
C
C       DECLARATION OF POINTERS FOR ALIASES.
C       TARGETS ARE DEFINED IN POINT_ARTEMIS
C
C       ALIASES FOR WORKING VECTORS IN TB AND TBBD
C
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T1
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T2
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T3
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T4
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T5
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T6
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T7
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T8
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T9
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T10
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T11
!> @brief WORKING VECTOR IN TB
C
      TYPE(BIEF_OBJ),POINTER :: T12
!> @brief WORKING VECTOR IN TBBD
C
      TYPE(BIEF_OBJ),POINTER :: TBD1
!> @brief WORKING VECTOR IN TBBD
C
      TYPE(BIEF_OBJ),POINTER :: TBD2
!> @brief WORKING VECTOR IN TBBD
C
      TYPE(BIEF_OBJ),POINTER :: TBD3
!> @brief WORKING VECTOR IN TBBD
C
      TYPE(BIEF_OBJ),POINTER :: TBD4
C
C       USEFUL COMPONENTS IN STRUCTURE MESH
C
!> @brief
C table de connectivite
      TYPE(BIEF_OBJ), POINTER :: IKLE
!> @brief
C coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X
!> @brief
C coordonnees des points du maillage
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Y
!> @brief
C nombre d'elements du maillage
      INTEGER, POINTER        :: NELEM
!> @brief
C
      INTEGER, POINTER        :: NELMAX
!> @brief
C nombre de points frontiere
      INTEGER, POINTER        :: NPTFR
!> @brief
C
      INTEGER, POINTER        :: NPTFRX
!> @brief
C
      INTEGER, POINTER        :: DIM
!> @brief
C
      INTEGER, POINTER        :: TYPELM
!> @brief
C nombre de points du maillage
      INTEGER, POINTER        :: NPOIN
!> @brief
C
      INTEGER, POINTER        :: NPMAX
!> @brief
C
      INTEGER, POINTER        :: MXPTVS
!> @brief
C
      INTEGER, POINTER        :: MXELVS
!> @brief
C
      INTEGER, POINTER        :: LV
C
C-----------------------------------------------------------------------
C
C      10) ART_FILES AND ASSOCIATED
C
C-----------------------------------------------------------------------
C
!> @brief
C
      INTEGER, PARAMETER :: MAXLU_ART = 44
!> @brief NAME OF THE GEOMETRY FILE
C nom du fichier de geometrie
      INTEGER :: ARTGEO
!> @brief NAME OF THE STEERING FILE
C nom du fichier des parametres
      INTEGER :: ARTCAS
!> @brief NAME OF THE BOUNDARY CONDITIONS FILE
C nom du fichier des conditions aux limites
      INTEGER :: ARTCLI
!> @brief NAME OF THE BOTTOM TOPOGRAPHY FILE
C nom du fichier des fonds
      INTEGER :: ARTFON
!> @brief NAME OF THE RESULTS FILE
C nom du fichier des resultats
      INTEGER :: ARTRES
!> @brief NAME OF THE BINARY RESULTS FILE
C nom du fichier des resultats binaire
      INTEGER :: ARTRBI
!> @brief NAME OF THE FORMATTED RESULTS FILE
C nom du fichier des resultats formate
      INTEGER :: ARTRFO
!> @brief NAME OF THE REFERENCE FILE
C nom du fichier de reference
      INTEGER :: ARTREF
!> @brief NAME OF THE BINARY DATA FILE 1
C nom du fichier de donnees binaire 1
      INTEGER :: ARTBI1
!> @brief NAME OF THE BINARY DATA FILE 2
C nom du fichier de donnees binaire 2
      INTEGER :: ARTBI2
!> @brief NAME OF THE FORMATTED DATA FILE 1
C nom du fichier de donnees formate 1
      INTEGER :: ARTFO1
!> @brief NAME OF THE FORMATTED DATA FILE 2
C nom du fichier de donnees formate 2
      INTEGER :: ARTFO2
!> @brief
C
      TYPE(BIEF_FILE) :: ART_FILES(MAXLU_ART)
C
      SAVE
C
      END MODULE DECLARATIONS_ARTEMIS
C
C
C#######################################################################
C