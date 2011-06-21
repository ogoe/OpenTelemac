!                    *****************************
                     MODULE DECLARATIONS_TELEMAC3D
!                    *****************************
!
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DECLARATION OF PRINCIPAL TELEMAC3D VARIABLES
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
C
C       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
C
C       (1) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       (2) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       (3) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
C       (4) INTEGERS
C       (5) LOGICAL VALUES
C       (6) REALS
C       (7) STRINGS
C       (8) SLVCFG STRUCTURES
C       (9) MESH STRUCTURE
C      (10) ALIASES
C
C-----------------------------------------------------------------------
C (1) VECTORS (REAL AND INTEGER)
C-----------------------------------------------------------------------
C
!
!     3D VELOCITY COMPONENTS AT PREVIOUS TIMESTEP (TIME N)
!
      TYPE(BIEF_OBJ), TARGET :: UN,VN,WN
!
!     3D VELOCITY COMPONENTS AFTER ADVECTION
!
      TYPE(BIEF_OBJ), TARGET :: UC,VC,WC
!
!     3D VELOCITY COMPONENTS AFTER DIFFUSION
!
      TYPE(BIEF_OBJ), TARGET :: UD,VD,WD
!
!     3D VELOCITY COMPONENTS AT CURRENT TIMESTEP (TIME N+1)
!
      TYPE(BIEF_OBJ), TARGET :: U,V,W
!
!     EXPLICIT SOURCE TERMS ON VELOCITIES
!
      TYPE(BIEF_OBJ), TARGET :: S0U,S0V,S0W
!
!     IMPLICIT SOURCE TERMS ON VELOCITIES U
!
      TYPE(BIEF_OBJ), TARGET :: S1U,S1V,S1W
! 
!     RIGHT-HAND SIDE ON VELOCITIES EQUATIONS
!
      TYPE(BIEF_OBJ), TARGET :: SMU,SMV
!
!     PRESCRIBED VELOCITY COMPONENTS ON THE BOTTOM
!
      TYPE(BIEF_OBJ), TARGET :: UBORF,VBORF,WBORF
!
!     PRESCRIBED VELOCITY COMPONENTS ON THE LATERAL BOUNDARY
!
      TYPE(BIEF_OBJ), TARGET :: UBORL,VBORL,WBORL
!
!     PRESCRIBED VELOCITY COMPONENTS AT THE FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: UBORS,VBORS,WBORS
!
!     ORIGINAL PRESCRIBED VALUES AT THE FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: TRBORSAVE,UBORSAVE,VBORSAVE
      TYPE(BIEF_OBJ), TARGET :: WBORSAVE,KBORSAVE,EBORSAVE
!
!     (FRICTION VELOCITY)**2 FOR BOTTOM AND LATERAL BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: UETCAR,UETCAL
!
!     LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, ON THE BOTTOM: NU*DU/DN = AUBORF*U + BUBORF
!
      TYPE(BIEF_OBJ), TARGET :: AUBORF,BUBORF
!
!     LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DU/DN = AUBORL*U + BUBORL
!
      TYPE(BIEF_OBJ), TARGET :: AUBORL,BUBORL
!
!     LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DU/DN = AUBORS*U + BUBORS
!
      TYPE(BIEF_OBJ), TARGET :: AUBORS,BUBORS
!
!     LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, ON THE BOTTOM: NU*DV/DN = AVBORF*V + BVBORF
!
      TYPE(BIEF_OBJ), TARGET :: AVBORF,BVBORF
!
!     LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DV/DN = AVBORL*V + BVBORL
!
      TYPE(BIEF_OBJ), TARGET :: AVBORL,BVBORL
!
!     LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DV/DN = AVBORS*V + BVBORS
!
      TYPE(BIEF_OBJ), TARGET :: AVBORS,BVBORS
!
!     LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, ON THE BOTTOM: NU*DW/DN = AWBORF*W + BWBORF
!
      TYPE(BIEF_OBJ), TARGET :: AWBORF,BWBORF
!
!     LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DW/DN = AWBORL*W + BWBORL
!
      TYPE(BIEF_OBJ), TARGET :: AWBORL,BWBORL
!
!     LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DW/DN = AWBORS*W + BWBORS
!
      TYPE(BIEF_OBJ), TARGET :: AWBORS,BWBORS
!
!     TYPES OF BOUNDARY CONDITIONS FOR VELOCITY COMPONENTS ON THE BOTTOM
!
      TYPE(BIEF_OBJ), TARGET :: LIUBOF,LIVBOF,LIWBOF
!
!     TYPES OF BOUNDARY CONDITIONS FOR U ON THE LATERAL BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: LIUBOL,LIVBOL,LIWBOL
!
!     TYPES OF BOUNDARY CONDITIONS FOR VELOCITY COMPONENTS AT THE FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: LIUBOS,LIVBOS,LIWBOS
!
!     'COLOUR' OF BOUNDARY NODES (TAKEN IN BOUNDARY CONDITIONS FILE)
!
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!
!     PLANE NUMBER OF LAST PLANE WHICH HAS A CRUSHED LAYER ABOVE IT
!
      TYPE(BIEF_OBJ), TARGET :: IPBOT
!
!     SIGMA-TRANSFORMED VERTICAL VELOCITY COMPONENT
!
      TYPE(BIEF_OBJ), TARGET :: WS
!
!     DYNAMIC PRESSURE AT TIME N+1 AND TIME N (NON-HYDROSTATIC)
!
      TYPE(BIEF_OBJ), TARGET :: DP,DPN
!
!     HYDROSTATIC PRESSURE (NON-HYDROSTATIC)
!
      TYPE(BIEF_OBJ), TARGET :: PH
!
!     PRESCRIBED DYNAMIC PRESSURE ON BOTTOM, LATERAL BOUNDARIES AND FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: PBORF, PBORL, PBORS
!
!     TYPE OF BOUNDARY CONDITIONS FOR DYNAMIC PRESSURE
!
      TYPE(BIEF_OBJ), TARGET :: LIPBOF, LIPBOL, LIPBOS
!
!     K OF K-EPSILON MODEL AT PREVIOUS TIMESTEP (TIME N)
!
      TYPE(BIEF_OBJ), TARGET :: AKN
!
!     K OF K-EPSILON MODEL AFTER ADVECTION
!
      TYPE(BIEF_OBJ), TARGET :: AKC
!
!     K OF K-EPSILON MODEL AT TIME N+1
!
      TYPE(BIEF_OBJ), TARGET :: AK
!
!     EXPLICIT SOURCE TERMA FOR K IN K-EPSILON MODEL
!
      TYPE(BIEF_OBJ), TARGET :: S0AK
!
!     IMPLICIT SOURCE TERMA FOR K IN K-EPSILON MODEL
!
      TYPE(BIEF_OBJ), TARGET :: S1AK
!> @brief PRESCRIBED K OF K-EPSILON MODEL ON THE BOTTOM
C k du modele k-epsilon impose au fond
      TYPE(BIEF_OBJ), TARGET :: KBORF
!> @brief PRESCRIBED K OF K-EPSILON MODEL ON THE LATERAL BOUNDARY
C k du modele k-epsilon impose sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: KBORL
!> @brief PRESCRIBED K OF K-EPSILON MODEL AT THE FREE SURFACE
C k du modele k-epsilon impose en surface
      TYPE(BIEF_OBJ), TARGET :: KBORS
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, ON THE BOTTOM
C
      TYPE(BIEF_OBJ), TARGET :: AKBORF
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, ON THE BOTTOM
C
      TYPE(BIEF_OBJ), TARGET :: BKBORF
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, ON THE LATERAL BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: AKBORL
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, ON THE LATERAL BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: BKBORL
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, AT THE FREE SURFACE
C
      TYPE(BIEF_OBJ), TARGET :: AKBORS
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, AT THE FREE SURFACE
C
      TYPE(BIEF_OBJ), TARGET :: BKBORS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR K OF K-EPSILON MODEL ON THE BOTTOM
C types de conditions aux limites au fond sur k du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIKBOF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR K OF K-EPSILON MODEL ON THE LATERAL BOUNDARIES
C types de conditions aux limites sur les parois laterales sur k du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIKBOL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR K OF K-EPSILON MODEL AT THE FREE SURFACE
C types de conditions aux limites en surface sur k du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIKBOS
!> @brief EPSILON OF K-EPSILON MODEL AT PREVIOUS TIMESTEP (TIME N)
C epsilon (dissipation turbulente) du modele k-epsilon au pas de temps precedent (n)
      TYPE(BIEF_OBJ), TARGET :: EPN
!> @brief EPSILON OF K-EPSILON MODEL AFTER ADVECTION
C epsilon (dissipation turbulente) du modele k-epsilon apres convection
      TYPE(BIEF_OBJ), TARGET :: EPC
!> @brief EPSILON OF K-EPSILON MODEL AT TIME N+1
C epsilon (dissipation turbulente) du modele k-epsilon au temps n+1
      TYPE(BIEF_OBJ), TARGET :: EP
!> @brief
C terme source explicite pour epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: S0EP
!> @brief
C terme source implicite pour epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: S1EP
!> @brief PRESCRIBED EPSILON OF K-EPSILON MODEL ON THE BOTTOM
C epsilon du modele k-epsilon impose au fond
      TYPE(BIEF_OBJ), TARGET :: EBORF
!> @brief PRESCRIBED EPSILON OF K-EPSILON MODEL ON THE LATERAL BOUNDARY
C epsilon du modele k-epsilon impose sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: EBORL
!> @brief PRESCRIBED EPSILON OF K-EPSILON MODEL AT THE FREE SURFACE
C epsilon du modele k-epsilon impose en surface
      TYPE(BIEF_OBJ), TARGET :: EBORS
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, ON THE BOTTOM
C
      TYPE(BIEF_OBJ), TARGET :: AEBORF
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, ON THE BOTTOM
C
      TYPE(BIEF_OBJ), TARGET :: BEBORF
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, ON THE LATERAL BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: AEBORL
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, ON THE LATERAL BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: BEBORL
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, AT THE FREE SURFACE
C
      TYPE(BIEF_OBJ), TARGET :: AEBORS
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, AT THE FREE SURFACE
C
      TYPE(BIEF_OBJ), TARGET :: BEBORS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR EPSILON OF K-EPSILON MODEL ON THE BOTTOM
C types de conditions aux limites au fond sur epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIEBOF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR EPSILON OF K-EPSILON MODEL ON THE LATERAL BOUNDARIES
C types de conditions aux limites sur les parois laterales sur epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIEBOL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR EPSILON OF K-EPSILON MODEL AT THE FREE SURFACE
C types de conditions aux limites en surface sur epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIEBOS
!> @brief 2D (VERTICALLY INTEGRATED) VELOCITY COMPONENT
      TYPE(BIEF_OBJ), TARGET :: U2D
!> @brief 2D (VERTICALLY INTEGRATED) VELOCITY COMPONENT
C composante 2d de la vitesse
      TYPE(BIEF_OBJ), TARGET :: V2D
!> @brief
      TYPE(BIEF_OBJ), TARGET :: UBOR2D,VBOR2D
!> @brief
      TYPE(BIEF_OBJ), TARGET :: UN2D,VN2D
!> @brief
      TYPE(BIEF_OBJ), TARGET :: FU,FV
!> @brief  WAVE STRESSES FROM ARTEMIS OR TOMAWAC
      TYPE(BIEF_OBJ), TARGET :: FXH,FYH
!> @brief
      TYPE(BIEF_OBJ), TARGET :: FLBOR,FLBLIM
!> @brief ADVECTION FIELD
      TYPE(BIEF_OBJ), TARGET :: UCONV,VCONV,WCONV
!> @brief VERTICAL VELOCITY IN TRANSFORMED MESH
      TYPE(BIEF_OBJ), TARGET :: WSCONV
!> @brief ADVECTION FIELD FOR CHARACTERISTICS
      TYPE(BIEF_OBJ), TARGET :: UCONVC,VCONVC
!> @brief PSEUDO-VISCOSITY IN WAVE EQUATION
      TYPE(BIEF_OBJ), TARGET :: NUWAVE
!> @brief
      TYPE(BIEF_OBJ), TARGET :: ZCONV
!> @brief D**-1 IN WAVE EQUATION (SEE BOOK)
      TYPE(BIEF_OBJ), TARGET :: DM1
!> @brief EDGE BY EDGE FLUXES
      TYPE(BIEF_OBJ), TARGET :: FLODEL
!> @brief
      TYPE(BIEF_OBJ), TARGET :: FLOPAR
!> @brief LIMITATION OF FLUXES
      TYPE(BIEF_OBJ), TARGET :: FLULIM
!> @brief VOLUME AROUND POINTS AT TIME N+1
C volume de controle a l'instant n+1
      TYPE(BIEF_OBJ), TARGET :: VOLU,VOLUPAR
!> @brief VOLUME AROUND POINTS AT TIME N
      TYPE(BIEF_OBJ), TARGET :: VOLUN,VOLUNPAR
!> @brief
      TYPE(BIEF_OBJ), TARGET :: VOLUT
!> @brief
      TYPE(BIEF_OBJ), TARGET :: VOLU3D,VOLU3DPAR
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: VOLU2D
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: V2DPAR
!> @brief INVERSE OF INTEGRAL OF BASES IN 2D
C inverse du volume des bases en 2d
      TYPE(BIEF_OBJ), TARGET :: UNSV2D
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: UNSV3D
!> @brief
C flux interieur par noeud
      TYPE(BIEF_OBJ), TARGET :: FLUINT
! 
!     FLUX AT BOUNDARIES, AND ASSEMBLED FORM IN PARALLEL
!
      TYPE(BIEF_OBJ), TARGET :: FLUEXT,FLUEXTPAR
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: FLINT2
!> @brief CHOL    ABSOLUTE VALUE OF THE VORTICITY
C
      TYPE(BIEF_OBJ), TARGET :: ROTAT
!> @brief RELATIVE DENSITY = (RHO-RHO0)/RHO0
C (rho-rho0)/rho0
      TYPE(BIEF_OBJ), TARGET :: DELTAR
!> @brief RICHARDSON NUMBER
C nombre de richardson
      TYPE(BIEF_OBJ), TARGET :: RI
!> @brief FRICTION COEFFICIENT ON THE BOTTOM
C coefficient de rugosite du fond
      TYPE(BIEF_OBJ), TARGET :: RUGOF
!> @brief FRICTION COEFFICIENT ON THE LATERAL BOUNDARY
C coefficient de rugosite des parois laterales
      TYPE(BIEF_OBJ), TARGET :: RUGOL
!> @brief FRICTION COEFFICIENT VALUES
C coefficient de frottement pour k-epsilon
      TYPE(BIEF_OBJ), TARGET :: CF
!> @brief WIND VELOCITY
C
      TYPE(BIEF_OBJ), TARGET :: WIND
!> @brief ATMOSPHERIC PRESSURE
C
      TYPE(BIEF_OBJ), TARGET :: PATMOS
!> @brief PARAMETERS FOR GLOBAL MASS AND FLUX BALANCES
C
      TYPE(BIEF_OBJ), TARGET :: MASINI
!> @brief
C masse au pas en cours
      TYPE(BIEF_OBJ), TARGET :: MASSE
!> @brief
C masse au pas precedent
      TYPE(BIEF_OBJ), TARGET :: MASSEN
!> @brief FLUX
C flux entre les 2 pas de temps
      TYPE(BIEF_OBJ), TARGET :: FLUX
!     CUMULATED FLUXES OF TRACERS SINCE BEGINNING OF COMPUTATION
      TYPE(BIEF_OBJ), TARGET :: FLUCUM
!> @brief DEPTH AT TIME N+1
C hauteur d'eau au temps n+1
      TYPE(BIEF_OBJ), TARGET :: H
!> @brief DEPTH AT TIME N
C hauteur d'eau au temps n
      TYPE(BIEF_OBJ), TARGET :: HN
!> @brief
C hauteur d'eau de propagation
      TYPE(BIEF_OBJ), TARGET :: HPROP
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: DH
!> @brief
C second membre pour la hauteur d'eau
      TYPE(BIEF_OBJ), TARGET :: SMH
!> @brief PRESCRIBED DEPTH ON LATERAL BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: HBOR
!
!     RAIN IN M/S MULTIPLIED BY VOLU2D, AND VERSION ASSEMBLED IN PARALLEL
!
      TYPE(BIEF_OBJ), TARGET :: PLUIE,PARAPLUIE
!
!     BLOCK OF VARIABLES TO BE ADVECTED BY CHARACTERISTICS
!
      TYPE(BIEF_OBJ), TARGET :: FN3D,FC3D
!
!     BLOCK OF VARIOUS BIEF_OBJ STRUCTURES FOR VARIABLES TO BE ADVECTED
!
      TYPE(BIEF_OBJ), TARGET :: BL_FN,BL_FC,BL_S0F,BL_FSC
      TYPE(BIEF_OBJ), TARGET :: BL_BOL,BL_BORL
!
!     VERTICAL COORDINATES FOR PROPAGATION STEP
!
      TYPE(BIEF_OBJ), TARGET :: ZPROP
!
!     BOTTOM ELEVATION
!
      TYPE(BIEF_OBJ), TARGET :: ZF
!
!     BOTTOM COORDINATES GIVEN PER ELEMENT
!
      TYPE(BIEF_OBJ), TARGET :: ZFE
!> @brief Z: PERCENTUAL MESH PLANES
C position relative des plans horizontaux
      TYPE(BIEF_OBJ), TARGET :: ZSTAR
!> @brief Z: DISTRIBUTION
C
      TYPE(BIEF_OBJ), TARGET :: ZT
!> @brief DH/DT
C
      TYPE(BIEF_OBJ), TARGET :: DSSUDT
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: ZPLANE
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: TRANSF_PLANE
!> @brief COORDINATES IN THE TRANSFORMED MESH FOR THE METHOD OF CHARACTERISTICS
C
      TYPE(BIEF_OBJ), TARGET :: ZCHAR
!> @brief MASK OF ELEMENTS
C masquage des elements
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!> @brief MASK OF NODES
C masquage des points
      TYPE(BIEF_OBJ), TARGET :: MASKPT
!> @brief 3D MASK OF BOUNDARY ELEMENTS
C masque 3d sur les bords lateraux
      TYPE(BIEF_OBJ), TARGET :: MASKBR
!> @brief POSITIONS OF FLOATING BODIES
C positions successives des flotteurs
      TYPE(BIEF_OBJ), TARGET :: XFLOT
!> @brief POSITIONS OF FLOATING BODIES
C positions successives des flotteurs
      TYPE(BIEF_OBJ), TARGET :: YFLOT
!> @brief POSITIONS OF FLOATING BODIES
C positions successives des flotteurs
      TYPE(BIEF_OBJ), TARGET :: ZFLOT
!> @brief
C z des flotteurs dans le maillage transforme
      TYPE(BIEF_OBJ), TARGET :: ZSFLOT
!> @brief
C coordonnees barycentriques instantannees, au pied des courbes caracteristiques, des flotteurs en 2dh
      TYPE(BIEF_OBJ), TARGET :: SHPFLO
!> @brief
C coordonnees barycentriques instantannees, au pied des courbes caracteristiques, des flotteurs en 1dv
      TYPE(BIEF_OBJ), TARGET :: SHZFLO
!> @brief TIME(STEP) FOR INITIAL RELEASE OF FLOATING BODIES
C numero du pas de temps de largage de chaque flotteur
      TYPE(BIEF_OBJ), TARGET :: DEBFLO
!> @brief TIME(STEP) FOR END OF FOLLOW UP OF FLOATING BODIES
C numero du pas de temps de fin de calcul de derive pour chaque flotteur
      TYPE(BIEF_OBJ), TARGET :: FINFLO
!> @brief
C numeros des elements 2dh, au pied des courbes caracteristiques, dans lesquels se trouve a cet instant chacun des flotteurs
      TYPE(BIEF_OBJ), TARGET :: ELTFLO
!> @brief
C numeros des plans, au pied des courbes caracteristiques, dans lesquels se trouve a cet instant chacun des flotteurs
      TYPE(BIEF_OBJ), TARGET :: ETAFLO
!> @brief
C table de connectivite bidon utilisee pour la sortie des trajectoires sous forme de maillage
      TYPE(BIEF_OBJ), TARGET :: IKLFLO
!> @brief WORKING ARRAY
C tableau de travail
      TYPE(BIEF_OBJ), TARGET :: TRAFLO
!> @brief TYPE OF BOUNDARY CONDITIONS ON DEPTH
C types de conditions aux limites sur h
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!> @brief NUMBER OF LIQUID BOUNDARIES
C
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!> @brief PROPAGATION BC TYPES (TELEMAC2D'S PROPAG)
C
      TYPE(BIEF_OBJ), TARGET :: LIMPRO
!> @brief SECOND MEMBERS (RIGHT HAND SIDE) FOR THE LINEAR EQUATIONS 3D
C seconds membres
      TYPE(BIEF_OBJ), TARGET :: SEM3D
!> @brief SECOND MEMBERS (RIGHT HAND SIDE) FOR THE LINEAR EQUATIONS 2D
C seconds membres
      TYPE(BIEF_OBJ), TARGET :: SEM2D
!> @brief ELEMENT-ORIENTED WORKING ARRAY
C tableau de travail par element 2d
      TYPE(BIEF_OBJ), TARGET :: TE1
!> @brief ELEMENT-ORIENTED WORKING ARRAY
C tableau de travail par element 2d
      TYPE(BIEF_OBJ), TARGET :: TE2
!> @brief ELEMENT-ORIENTED WORKING ARRAY
C tableau de travail par element 2d
      TYPE(BIEF_OBJ), TARGET :: TE3
!> @brief PIECE-WISE LINEAR FREE SURFACE
C
      TYPE(BIEF_OBJ), TARGET :: ZFLATS
!> @brief VOID VECTOR STRUCTURE
C structure vide
      TYPE(BIEF_OBJ), TARGET :: SVIDE
!
!     RIGHT HAND SIDE OF CONTINUITY EQUATION WHEN SOURCES
!
      TYPE(BIEF_OBJ), TARGET :: SOURCES
!> @brief
C vitesse de chute du sediment
      TYPE(BIEF_OBJ), TARGET :: WCHU
!> @brief THICKNESS OF SOLID FRACTION OF THE BED LAYER ( EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS )
C epaisseurs des mailles discretisant le lit ( epai=dz/(1+ivide) )
      TYPE(BIEF_OBJ), TARGET :: EPAI
!> @brief VOID RATIO (GIBSON MODEL ONLY)
C indice des vides aux points du maillage (modele de gibson)
      TYPE(BIEF_OBJ), TARGET :: IVIDE
!> @brief TIME COUNTER FOR CONSOLIDATION MODEL (MULTILAYER MODEL)
C compteur de temps (modele multi-couches)
      TYPE(BIEF_OBJ), TARGET :: TEMP
!
!     CONCENTRATION OF MUD BED LAYER (MULTILAYER MODEL)
!
      TYPE(BIEF_OBJ), TARGET :: CONC
!
!     THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!
      TYPE(BIEF_OBJ), TARGET :: HDEP
!> @brief EROSION FLUX
C flux d'erosion en chaque point 2d
      TYPE(BIEF_OBJ), TARGET :: FLUER
!> @brief PROBABILITY OF DEPOSIT
C probabilite de depot en chaque point 2d
      TYPE(BIEF_OBJ), TARGET :: PDEPO
!> @brief RIDIG BED ELEVATION
C cote du fond rigide
      TYPE(BIEF_OBJ), TARGET :: ZR
!> @brief
C concentration d'equilibre
      TYPE(BIEF_OBJ), TARGET :: CREF
!> @brief
C diametre moyen des grains
      TYPE(BIEF_OBJ), TARGET :: DMOY
!> @brief NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
C nombre de points discretisant le fond vaseux sur une verticale
      TYPE(BIEF_OBJ), TARGET :: NPF
C
C-----------------------------------------------------------------------
C (2) MATRICES
C-----------------------------------------------------------------------
C
!> @brief
C matrice supg non symetrique
      TYPE(BIEF_OBJ), TARGET :: MSUPG
!> @brief
C matrice murd non symetrique
      TYPE(BIEF_OBJ), TARGET :: MMURD
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: MDIFF
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: MURD_TF
!> @brief 3D WORKING MATRIX
C matrice de travail
      TYPE(BIEF_OBJ), TARGET :: MTRA1
!> @brief 3D WORKING MATRIX
C matrice de travail
      TYPE(BIEF_OBJ), TARGET :: MTRA2
!> @brief 2D MATRIX
      TYPE(BIEF_OBJ), TARGET :: MBOR2D
!> @brief 2D MATRIX
C matrice de travail 2dh
      TYPE(BIEF_OBJ), TARGET :: MATR2H
C
C-----------------------------------------------------------------------
C (3) BLOCKS
C-----------------------------------------------------------------------
C
!> @brief
C traceurs au pas precedent
      TYPE(BIEF_OBJ), TARGET :: TAN
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: TAC
!> @brief CONCENTRATION OF TRACERS
C traceurs au pas en cours
      TYPE(BIEF_OBJ), TARGET :: TA
!
!     EXPLICIT SOURCE TERM FOR TRACERS
!
      TYPE(BIEF_OBJ), TARGET :: S0TA
!> @brief
C terme source explicite, implicite pour les traceurs
      TYPE(BIEF_OBJ), TARGET :: S1TA
!> @brief PRESCRIBED TRACERS ON THE BOTTOM
C
      TYPE(BIEF_OBJ), TARGET :: TABORF
!> @brief PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
C
      TYPE(BIEF_OBJ), TARGET :: TABORL
!> @brief PRESCRIBED TRACERS AT THE FREE SURFACE
C
      TYPE(BIEF_OBJ), TARGET :: TABORS
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
C loi log sur traceurs : atabo*ta + btabo, au fond
      TYPE(BIEF_OBJ), TARGET :: ATABOF
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
C loi log sur traceurs : atabo*ta + btabo, au fond
      TYPE(BIEF_OBJ), TARGET :: BTABOF
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
C loi log sur traceurs : atabo*ta + btabo, sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: ATABOL
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
C loi log sur traceurs : atabo*ta + btabo, sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: BTABOL
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
C loi log sur traceurs : atabo*ta + btabo, en surface
      TYPE(BIEF_OBJ), TARGET :: ATABOS
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
C loi log sur traceurs : atabo*ta + btabo, en surface
      TYPE(BIEF_OBJ), TARGET :: BTABOS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR TRACERS ON THE BOTTOM
C type de conditions aux limites sur traceurs au fond
      TYPE(BIEF_OBJ), TARGET :: LITABF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR TRACERS ON THE LATERAL BOUNDARY
C type de conditions aux limites sur traceurs sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: LITABL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR TRACERS AT THE FREE SURFACE
C type de conditions aux limites sur traceurs en surface
      TYPE(BIEF_OBJ), TARGET :: LITABS
!> @brief VISCOSITY
C coefficients de viscosite pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: VISCVI
!> @brief DIFFUSIVITY FOR TRACERS
C coefficients de viscosite pour les traceurs
      TYPE(BIEF_OBJ), TARGET :: VISCTA
!> @brief BOTTOM GRADIENTS
C
      TYPE(BIEF_OBJ), TARGET :: GRADZF
!> @brief FREE SURFACE GRADIENT (BLOCK OF 2 COMPONENTS)
C
      TYPE(BIEF_OBJ), TARGET :: GRADZS
!> @brief
C
      TYPE(BIEF_OBJ), TARGET :: GRADZN
!> @brief 2D MASK, FOR TELEMAC2D COMPATIBILITY
C masque pour les segments 2d
      TYPE(BIEF_OBJ), TARGET :: MASK
!> @brief BLOCKS OF ARRAYS FOR THE USER
C
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!
!     BLOCKS OF ARRAYS FOR THE USER
!
      TYPE(BIEF_OBJ), TARGET :: MAT2D, TM1
!> @brief
C structure de tableaux de travail 3d
      TYPE(BIEF_OBJ), TARGET :: TRAV3
!> @brief
C structure de tableaux de travail 2d
      TYPE(BIEF_OBJ), TARGET :: TRAV2
!> @brief
C structure de tableaux de travail d'entiers
      TYPE(BIEF_OBJ), TARGET :: ITRAV3
!
      TYPE(BIEF_OBJ), TARGET :: VARSOR, VARCL
!
      TYPE(BIEF_OBJ), TARGET :: VARSO3
!
!     VALUES OF ADVECTED VARIABLES AT SOURCES
!
      TYPE(BIEF_OBJ), TARGET :: U_SCE,V_SCE,W_SCE,AK_SCE,EP_SCE,TA_SCE
!
!     VOID BLOCK FOR CALL COMPATIBILITY
!
      TYPE(BIEF_OBJ), TARGET :: BVIDE
C
C-----------------------------------------------------------------------
C (4) INTEGERS
C-----------------------------------------------------------------------
C KEYWORDS AND PARAMETERS
C
!> @brief
C maximum de points sources
      INTEGER, PARAMETER :: MAXSCE = 100
!> @brief
C maximum de frontieres liquides
      INTEGER, PARAMETER :: MAXFRO = 100
!> @brief
C maximum number of tracers
      INTEGER, PARAMETER :: MAXTRA = 20
!> @brief
C maximum de variables de sortie
      INTEGER, PARAMETER :: MAXVAR = 100
!> @brief
C
      INTEGER, PARAMETER :: MAXVA3 = 100
!> @brief NUMBER OF TIME STEPS
C nombre total de pas de temps
      INTEGER NIT
!> @brief NUMBER OF HORIZONTAL PLANES
C nombre de plans horizontaux
      INTEGER NPLAN
!> @brief NUMBER OF TRACERS
C nombre de traceurs actifs
      INTEGER NTRAC
!> @brief
C
      INTEGER NTRACER
!> @brief PRINTOUT PERIOD FOR FLOATING BODIES
C periode pour les sorties de flotteurs
      INTEGER FLOPRD
!> @brief NUMBER OF DROGUES
C nombre de flotteurs
      INTEGER NFLOT
!> @brief
      INTEGER GRAPRD
!> @brief LISTING PRINTOUT PERIOD
C periode pour les sorties listing
      INTEGER LISPRD
!> @brief NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
C numero du premier pas de temps pour les sorties graphiques
      INTEGER GRADEB
!> @brief NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
C numero du premier pas de temps pour les sorties listing
      INTEGER LISDEB
!> @brief NUMBER OF BOTTOM SMOOTHINGS
C nombre de lissages du fond
      INTEGER LISFON
!> @brief NUMBER OF SUB ITERATIONS FOR NON LINEARITIES
C nombre de sous iterations pour les non linearites
      INTEGER NSOUSI
!> @brief
C numero du plan intermediaire
      INTEGER NPLINT
!> @brief HORIZONTAL TURBULENCE MODEL
C modele de turbulence horizontal
      INTEGER ITURBH
!> @brief VERTICAL TURBULENCE MODEL
C modele de turbulence vertical
      INTEGER ITURBV
!> @brief TURBULENCE MODEL FOR THE BOTTOM
C regime de turbulence pour le fond
      INTEGER LISRUF
!> @brief TURBULENCE MODEL FOR LATERAL SOLID BOUNDARIES
C regime de turbulence pour les parois laterales
      INTEGER LISRUL
!> @brief INITIAL GUESS FOR DEPTH
C ordre du tir initial pour la hauteur
      INTEGER IORDRH
!> @brief SPATIAL PROJECTION TYPE
C type de projection spatiale
      INTEGER PROTYP
!> @brief
C nombre de points sources
      INTEGER NSCE
!> @brief
C adresses des points sources dans le maillage 2d
      INTEGER ISCE(MAXSCE)
!> @brief
C adresses des plans des points sources trouves
      INTEGER KSCE(MAXSCE)
!> @brief
C
      INTEGER NREJEU
!> @brief SCHEME FOR ADVECTION OF VELOCITIES
C schema pour la convection des vitesses
      INTEGER SCHCVI
!
!     SCHEME FOR ADVECTION OF TRACERS
!
      INTEGER SCHCTA(MAXTRA)
!
!     SCHEME FOR ADVECTION OF K-EPSILON
!
      INTEGER SCHCKE
!
!     SCHEME FOR ADVECTION OF DEPTH
!
      INTEGER SCHCH
!
!     SCHEME FOR DIFFUSION OF VELOCITIES
!
      INTEGER SCHDVI
!
!     SCHEME FOR DIFFUSION OF TRACERS
!
      INTEGER SCHDTA
!
!     SCHEME FOR DIFFUSION OF K-EPSILON
!
      INTEGER SCHDKE
!> @brief MAXIMUM NUMBER OF RECORDS OF FLOATING BODY POSITIONS
C nombre maximal d'enregistrements des positions successives des flotteurs
      INTEGER NITFLO
!> @brief TREATMENT ON TIDAL FLATS FOR VELOCITIES
C traitement sur les bancs decouvrants pour les vitesses
      INTEGER TRBAVI
!> @brief TREATMENT ON TIDAL FLATS FOR TRACERS
C traitement sur les bancs decouvrants pour les traceurs
      INTEGER TRBATA
!> @brief TREATMENT ON TIDAL FLATS FOR K-EPSILON
C traitement sur les bancs decouvrants pour le k-epsilon
      INTEGER TRBAKE
!> @brief NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
C
      INTEGER NDEBIT
!> @brief NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
C
      INTEGER NCOTE
!> @brief NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
C
      INTEGER NVIT
!> @brief ORIGINAL DATE OF TIME
C tableau contenant la date de l'origine des temps
      INTEGER MARDAT(3)
!> @brief ORIGINAL HOUR OF TIME
C tableau contenant l'heure de l'origine des temps
      INTEGER MARTIM(3)
!> @brief VECTOR LENGTH
C longueur du vecteur
      INTEGER LVMAC
!> @brief NUMBER OF ARRAYS IN BLOCK PRIVE
C nombre de tableaux prives
      INTEGER NPRIV
!> @brief RANK OF TEMPERATURE IN TRACERS
C
      INTEGER IND_T
!> @brief RANK OF SALINITY
C
      INTEGER IND_S
!> @brief
C
      INTEGER NDP
!> @brief LAW OF BOTTOM FRICTION
C loi de frottement sur le fond
      INTEGER KFROT
!> @brief LAW OF FRICTION ON LATERAL BOUNDARIES
C loi de frottement sur les parois laterales
      INTEGER KFROTL
!> @brief MATRIX STORAGE
C stockage des matrices
      INTEGER OPTASS
!> @brief
C
      INTEGER PRODUC
!> @brief OPTION FOR THE TREATMENT OF TIDAL FLATS
C option de traitement des bancs decouvrants
      INTEGER OPTBAN
!> @brief TREATMENT OF NEGATIVE DEPTHS
C traitement des hauteurs negatives
      INTEGER OPT_HNEG
!> @brief
C
      INTEGER OPDVIT,OPTSOU
!> @brief SUPG OPTION
C option de supg
      INTEGER OPTSUP(4)
!> @brief
C
      INTEGER OPTASS2D
!> @brief 3D DISCRETISATION TYPE
C type de discretisation 3d
      INTEGER IELM3
!> @brief 2DH DISCRETISATION TYPE
C type de discretisation 2dh
      INTEGER IELM2H
!> @brief 2DV DISCRETISATION TYPE
C type de discretisation 2dv
      INTEGER IELM2V
!> @brief
C
      INTEGER IELM0, IELMH, IELMU, IELM1, IELMX
!> @brief NUMBER OF LAYERS OF 3D ELEMENTS (NPLAN - 1)
C nombre d'etages sur la verticale (NPLAN - 1)
      INTEGER NETAGE
!> @brief
C nombre de variables traitees dans le bilan
      INTEGER NVBIL
!> @brief MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED (GIBSON MODEL)
C nombre maximum de plans horizontaux discretisant le fond vaseux (modele de gibson)
      INTEGER NPFMAX
!> @brief NUMBER OF LAYERS WITHIN THE BED (MULTILAYER MODEL)
C nombre de couches discretisant le fond vaseux (modele de tassement multi-couches)
      INTEGER NCOUCH
!> @brief MIXING LENGTH MODEL
C modele de longueur de melange
      INTEGER MIXING
!> @brief DAMPING FUNCTION
C fonction d'amortissement
      INTEGER DAMPING
!> @brief VELOCITY PROFILES
C profils de vitesse
      INTEGER PROFVEL(MAXFRO)
!> @brief TREATMENT OF FLUXES AT THE BOUNDARIES
C traitement des flux aux frontieres
      INTEGER DIRFLU(MAXFRO)
!> @brief VELOCITY VERTICAL PROFILES
C profils de vitesse sur la verticale
      INTEGER VERPROVEL(MAXFRO)
!> @brief TRACERS VERTICAL PROFILES
C profils des traceurs sur la verticale
      INTEGER VERPROTRA(MAXFRO*MAXTRA)
! 
!     NUMBER OF LIQUID BOUNDARIES, OF SOLID BOUNDARIES
!     FIRST AND LAST POINTS OF THESE BOUNDARIES
!
      INTEGER NFRLIQ,NFRSOL
      INTEGER DEBLIQ(MAXFRO),FINLIQ(MAXFRO)
      INTEGER DEBSOL(MAXFRO),FINSOL(MAXFRO)
!
!     CHOICE OF MESH TRANSFORMATION
!
      INTEGER TRANSF
!
!     OPTION FOR THE DIFFUSION
!
      INTEGER OPTDIF
!
!     CHOICE OF DENSITY LAW
!
      INTEGER DENLAW
!> @brief DELWAQ PRINTOUT PERIOD
C periode de sortie pour delwaq
      INTEGER WAQPRD
!> @brief ORIGIN COORDINATES
C coordonnees de l'origine
      INTEGER I_ORIG
!> @brief ORIGIN COORDINATES
C coordonnees de l'origine
      INTEGER J_ORIG
!> @brief NUMBER OF POINTS GIVEN FOR EACH DISCHARGE-ELEVATIONS CURVES
C
      INTEGER PTS_CURVES(MAXFRO)
!> @brief STAGE-DISCHARGE CURVES
C courbes de tarage
      INTEGER STA_DIS_CURVES(MAXFRO)
!> @brief KEYWORD DEBUGGER
C debugger
      INTEGER DEBUG
!> @brief SKIN FRICTION
      INTEGER ICR
!> @brief Equilibrium ttransport formula
      INTEGER ICQ
!> @brief RECORD NUMBER IN THE WAVE DRIVEN CURRENTS FILE
C numero de l'enregistrement dans le fichier de houle
      INTEGER NPTH
!> @brief GEOMETRY FILE NUMBER
C fichier de geometrie
      INTEGER T3DGEO
!> @brief BOUNDARY CONDITIONS FILE NUMBER
C fichier des conditions aux limites
      INTEGER T3DCLI
!> @brief PREVIOUS COMPUTATION FILE NUMBER
C fichier du calcul precedent
      INTEGER T3DPRE
!> @brief 3D RESULT FILE NUMBER
C fichier des resultats 3d
      INTEGER T3DRES
!> @brief BOTTOM TOPOGRAPHY FILE NUMBER
C fichier des fonds
      INTEGER T3DFON
!> @brief FILE NUMBER FOR SCOPE
C fichier pour scope
      INTEGER T3DSCO
!> @brief 2D RESULT FILE NUMBER
C fichier des resultats 2d
      INTEGER T3DHYD
!> @brief FORMATTED DATA FILE 1
C fichier de donnees formate 1
      INTEGER T3DFO1
!> @brief FORMATTED DATA FILE 2
C fichier de donnees formate 2
      INTEGER T3DFO2
!> @brief BINARY DATA FILE 1
C fichier de donnees binaire 1
      INTEGER T3DBI1
!> @brief BINARY DATA FILE 2
C fichier de donnees binaire 2
      INTEGER T3DBI2
!> @brief
C
      INTEGER T3DSED
!> @brief
C
      INTEGER T3DSUS
!> @brief REFERENCE FILE NUMBER
C fichier de reference
      INTEGER T3DREF
!
!     LIQUID BOUNDARIES FILE NUMBER
!
      INTEGER T3DIMP
!
!     FILES FOR DELWAQ
!
      INTEGER T3DDL1,T3DDL2,T3DDL3,T3DDL4,T3DDL5,T3DDL6,T3DDL7,T3DDL8
      INTEGER T3DDL9,T3DL10,T3DL11
!
!     STAGE-DISCHARGE CURVES FILE NUMBER
!
      INTEGER T3DPAR
!
!     SOURCES FILE NUMBER
!
      INTEGER T3DVEF
!
!     BINARY RESULTS FILE NUMBER
!
      INTEGER T3DRBI
!
!     FORMATTED RESULTS FILE NUMBER
!
      INTEGER T3DRFO
!
!     MIGRHYCAR STEERING FILE
!
      INTEGER T3DMIG
!
!     NUMBER OF VARIABLES TO BE ADVECTED BY A GIVEN SCHEME
!     AND THEIR LIST IN THE LIST OF ADVECTED VARIABLES
!     S_ADV: ASSOCIATED ADVECTION SCHEME
!     NOM_ADV: RANK TO FIND THE NAME IN TEXT3 
!
      INTEGER N_ADV(0:15),LIST_ADV(100,0:15),S_ADV(5+MAXTRA)
      INTEGER NOM_ADV(5+MAXTRA)
!
!     COUPLING PERIODS FOR SISYPHE AND TOMAWAC 
!
      INTEGER PERCOU_SIS,PERCOU_WAC
!
!     BOUNDARY CONDITION ON THE BOTTOM 
!
      INTEGER BC_BOTTOM
C
C-----------------------------------------------------------------------
C (5) LOGICAL VALUES
C-----------------------------------------------------------------------
C
C LOGICAL STEERING PARAMETERS
C
      LOGICAL DEBU,   PROP
!
!     IF YES, CORIOLIS
!
      LOGICAL CORIOL
!
!     IF YES, WIND TAKEN INTO ACCOUNT
!
      LOGICAL VENT
!
!     AIR PRESSURE
!
      LOGICAL ATMOS
!> @brief IF YES, SEDIMENT
C si oui, sediment
      LOGICAL SEDI
!> @brief IF YES, TIDAL FLATS
C si oui, traitement des bancs decouvrants
      LOGICAL BANDEC
!> @brief
C si oui, "propagaton linearisee"
      LOGICAL PROLIN
!> @brief
C
      LOGICAL BILMAS
!> @brief
C
      LOGICAL INFMAS
!> @brief
C
      LOGICAL SIGMAG
!> @brief IF YES, HYDROSTATIC INCONSISTENCY FILTER
C si oui, filtre les inconsistances hydrostatiques
      LOGICAL INCHYD
!> @brief
      LOGICAL SORIMP(MAXVAR), SORG2D(MAXVAR)
!> @brief
      LOGICAL SORIM3(MAXVA3), SORG3D(MAXVA3)
!> @brief IF YES, 2D CONTINUATION
C si oui, suite 2d
      LOGICAL SUIT2
!> @brief IF YES, RAIN OR EVAPORATION
C si oui, pluie ou evaporation
      LOGICAL RAIN
!> @brief IF YES, STABILISED INITIAL CONDITION
C si oui, consolidation initiale stabilisee
      LOGICAL CONSOL
!> @brief IF YES, ELEMENTS MASKED BY USER
C si oui, elements masques par l'utilisateur
      LOGICAL MSKUSE
!> @brief DIF(I) WILL SAY IF DIFFUSION SOLVER I IS USED FOR AT LEAST ONE OF THE VARIABLES
      LOGICAL DIF(0:2)
!> @brief
      LOGICAL SPHERI
!> @brief IF YES, THERE ARE MASKED ELEMENTS
C si oui, presence d'elements masques
      LOGICAL MSK
!> @brief
      LOGICAL CLIPH
!> @brief IF YES, LISTING PRINTOUT
C si oui, sortie listing
      LOGICAL LISTIN
!> @brief IF YES, INFORMATION PRINTED ON LISTING
C si oui, informations a restituer sur le listing
      LOGICAL INFOGR
!> @brief IF YES, QUASI-BUBBLE OPTION
C si oui, option quasi-bulle
      LOGICAL QUABUB
!> @brief IF YES, VARIABLES FOR SUBIEF3D
C si oui, variables pour subief3d
      LOGICAL VARSUB
!> @brief IF YES, VALIDATION
C si oui, validation
      LOGICAL VALID
!> @brief IF YES, MULTILAYER CONSOLIDATION MODEL
C si oui, modele de tassement multi-couches
      LOGICAL TASSE
!> @brief IF YES, GIBSON CONSOLIDATION MODEL
C si oui, modele de tassement de gibson
      LOGICAL GIBSON
!> @brief IF YES, INFLUENCE OF TURBULENCE ON SETTLING VELOCITY
C si oui, influence de la turbulence sur la vitesse de chute
      LOGICAL TURBWC
!> @brief IF YES, COHESIVE SEDIMENT
C si oui, sediment cohesif
      LOGICAL SEDCO
!> @brief IF YES, NON-HYDROSTATIC VERSION
C si oui, version non-hydrostatique
      LOGICAL NONHYD
!> @brief
C
      LOGICAL CONPRO
!> @brief FOR INITIALISATION OF K-EPSILON (SET IN CONDIM)
C
      LOGICAL AKEP
!> @brief FOR INITIALISATION OF K-OMEGA (SET IN CONDIM)
C
      LOGICAL AKOM
!> @brief IF YES, INITIAL TIME RESET TO ZERO IN A COMPUTATION CONTINUED
C si oui, remise a zero du temps
      LOGICAL RAZTIM
!> @brief IF YES, DYNAMIC PRESSURE IN WAVE EQUATION
C si oui, pression dynamique dans l'equation d'onde
      LOGICAL DPWAVEQ
!
!     IF YES, DYNAMIC BOUNDARY CONDITION
!
      LOGICAL CLDYN
!
!     IF YES, SALINITY FOR DELWAQ
!
      LOGICAL SALI_DEL
!
!     IF YES, TEMPERATURE FOR DELWAQ
!
      LOGICAL TEMP_DEL
!
!     IF YES, VELOCITY FOR DELWAQ
!
      LOGICAL VELO_DEL
!
!     IF YES, DIFFUSION FOR DELWAQ
!
      LOGICAL DIFF_DEL
!
!     IF YES, WAVE DRIVEN CURRENTS
!
      LOGICAL COUROU
!
!     IF YES, BYPASS VOID VOLUMES
!
      LOGICAL BYPASS
!
!     PROJECTION OF VELOCITY ON LATERAL SOLID BOUNDARIES
!
      LOGICAL VELPROLAT
!
!     PROJECTION OF VELOCITY ON BOTTOM
!
      LOGICAL VELPROBOT
!
!     FOR CALCULATING FLUXES OF ADVECTED VARIABLES
!
      LOGICAL CALCFLU(5+MAXTRA)
!
!     FOR TAKING INTO ACCOUNT RAIN IN ADVECTION OF VARIABLES
!
      LOGICAL CALCRAIN(5+MAXTRA)
!
!     OIL SPILL MODEL
!
      LOGICAL SPILL_MODEL
!
!-----------------------------------------------------------------------
! (6) REALS
!-----------------------------------------------------------------------
!
!     TIME
!
      DOUBLE PRECISION AT
!
!     TIMESTEP
!
      DOUBLE PRECISION DT
!
!     DURATION
!
      DOUBLE PRECISION DUREE
!
!     GRAVITY ACCELERATION
!
      DOUBLE PRECISION GRAV
!
!     CORIOLIS COEFFICIENT
!
      DOUBLE PRECISION FCOR
!
!     DRAG COEFFICIENT OF WIND
!
      DOUBLE PRECISION FAIR
!
!     WIND VELOCITY ALONG X, AND Y
!
      DOUBLE PRECISION FUAIR,FVAIR
!
!     AIR TEMPERATURE
!
      DOUBLE PRECISION TAIR
!> @brief WATER DENSITY AT REFERENCE CONCENTRATION
C masse volumique de reference de l'eau
      DOUBLE PRECISION RHO0
!> @brief FRICTION COEFFICIENT FOR THE BOTTOM
C coefficient de frottement pour le fond
      DOUBLE PRECISION RUGOF0
!> @brief FRICTION COEFFICIENT FOR LATERAL SOLID BOUNDARIES
C coefficient de frottement pour les parois laterales
      DOUBLE PRECISION RUGOL0
!> @brief ZERO
C zero (plus petite valeur non nulle autorisee)
      DOUBLE PRECISION ZERO
!> @brief MINIMAL VALUE FOR DEPTH
C valeur minimale pour la hauteur
      DOUBLE PRECISION HMIN
!> @brief MEAN DEPTH FOR LINEARIZATION
C profondeur moyenne pour la linearisation
      DOUBLE PRECISION HAULIN
!> @brief COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
C coefficient de diffusion horizontale des vitesses
      DOUBLE PRECISION DNUVIH
!> @brief COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
C coefficient de diffusion verticale des vitesses
      DOUBLE PRECISION DNUVIV
!> @brief COEFFICIENT FOR HORIZONTAL DIFFUSION OF TRACERS
C coefficient de diffusion horizontale des traceurs
      DOUBLE PRECISION DNUTAH
!> @brief COEFFICIENT FOR VERTICAL DIFFUSION OF TRACERS
C coefficient de diffusion verticale des traceurs
      DOUBLE PRECISION DNUTAV
!> @brief INITIAL DEPTH
C hauteur initiale
      DOUBLE PRECISION HAUTIN
!> @brief INITIAL ELEVATION
C cote initiale
      DOUBLE PRECISION COTINI
!> @brief RAIN OR EVAPORATION IN MM PER DAY
C pluie ou evaporation en mm par jour
      DOUBLE PRECISION RAIN_MMPD
!> @brief IMPLICITATION FOR DEPTH
C taux d'implicitation pour la hauteur
      DOUBLE PRECISION TETAH
!> @brief IMPLICITATION FOR VELOCITIES
C taux d'implicitation pour les vitesses
      DOUBLE PRECISION TETAU
!> @brief
C
      DOUBLE PRECISION TETAD
!> @brief IMPLICITATION FOR DIFFUSION
C taux d'implicitation pour la diffusion (diagonale si optdif = 2)
      DOUBLE PRECISION TETADI
!> @brief MASS-LUMPING FOR DEPTH
C mass-lumping pour la hauteur
      DOUBLE PRECISION AGGLOH
!> @brief MASS-LUMPING FOR DIFFUSION
C mass-lumping pour la diffusion
      DOUBLE PRECISION AGGLOD
!> @brief MASS-LUMPING FOR VELOCITIES
C mass-lumping pour les vitesses
      DOUBLE PRECISION AGGLOU
!> @brief
C cote du plan intermediaire de reference
      DOUBLE PRECISION COTINT
!> @brief ARRAY OF PRESCRIBED FLOWRATES
C debits imposes
      DOUBLE PRECISION DEBIMP(MAXFRO)
!> @brief ARRAY OF PRESCRIBED ELEVATIONS
C cotes imposees
      DOUBLE PRECISION COTIMP(MAXFRO)
!> @brief ARRAY OF PRESCRIBED VELOCITIES
C vitesses imposees
      DOUBLE PRECISION VITIMP(MAXFRO)
!> @brief BETA EXPANSION COEFFICIENT FOR TRACERS
C coefficient de dilatation volumique beta pour les traceurs
      DOUBLE PRECISION BETAC(MAXTRA)
!> @brief REFERENCE CONCENTRATION OF TRACERS
C valeurs de reference des traceurs
      DOUBLE PRECISION T0AC(MAXTRA)
!> @brief INITIAL VALUES OF TRACERS
C valeurs initiales des traceurs
      DOUBLE PRECISION TRAC0(MAXTRA)
!> @brief DENSITY OF THE SEDIMENT
C masse volumique du sediment
      DOUBLE PRECISION RHOS
!> @brief CRITICAL SHEAR STRESS FOR DEPOSITION
C contrainte critique de depot
      DOUBLE PRECISION TOCD
!> @brief CONCENTRATION (G/L) OF FRESH DEPOSITS
C concentration (g/l) des depots frais
      DOUBLE PRECISION CFDEP
!> @brief REFERENCE BED LAYER THICKNESS FOR NEW LAYER CREATION
C epaisseur de reference pour creer de nouvelles couches du fond vaseux
      DOUBLE PRECISION EPAI0
!> @brief TIMESTEP FOR CONSOLIDATION
C pas de temps de la consolidation
      DOUBLE PRECISION DTC
!> @brief CONCENTRATION (G/L) OF THE CONSOLIDATED MUD
C concentration (g/l) de la vase tassee
      DOUBLE PRECISION CFMAX
!> @brief EROSION COEFFICIENT (EMPIRICAL PARTHENIADES COEFFICIENT)
C coefficient d'erosion (loi de partheniades)
      DOUBLE PRECISION MPART
!> @brief CRITICAL SHEAR STRESS FOR EROSION (FRESH DEPOSIT)
C contrainte critique d'erosion
      DOUBLE PRECISION TOCE
!> @brief FLOCULATION COEFFICIENT
C coefficient traduisant la formation des flocs
      DOUBLE PRECISION TURBA
!> @brief COEFFICIENT RELATIVE TO FLOC DESTRUCTION
C coefficient traduisant la destruction des flocs
      DOUBLE PRECISION TURBB
!> @brief CONSOLIDATION TIME SCALE (ONLY FOR MULTILAYER MODEL)
C temps de sejour de la vase dans les couches (modele multi-couches)
      DOUBLE PRECISION TREST(30)
!> @brief CONSTANT SEDIMENT SETTLING VELOCITY (M/S)
C vitesse de chute constante (m/s)
      DOUBLE PRECISION WCHU0
!> @brief MEAN DIAMETER OF THE SEDIMENT
C diametre moyen des grains
      DOUBLE PRECISION D50
!> @brief GEOGRAPHICAL LATITUDE IN GRAD, POSITIVE FOR NORTHERN AND NEGATIVE ON SOUTHERN HEMISPHERE
C
      DOUBLE PRECISION PHILAT
!> @brief UPWIND COEFFICIENT (BETWEEN 0 AND 1)
C
      DOUBLE PRECISION DELTA
!> @brief HORIZONTAL CORIOLIS PARAMETERS
C
      DOUBLE PRECISION FHOR
!> @brief VERTICAL CORIOLIS PARAMETERS
C
      DOUBLE PRECISION FVER
!> @brief LATITUDE OF THE ORIGIN POINT
C latitude du point origine
      DOUBLE PRECISION LATIT
!> @brief LONGITUDE OF THE ORIGIN POINT
C longitude du point origine
      DOUBLE PRECISION LONGIT
!> @brief NORTH
C nord
      DOUBLE PRECISION NORD
!> @brief FREE SURFACE GRADIENT COMPATIBILITY IN WAVE EQUATION
C compatibilite du gradient de surface libre
      DOUBLE PRECISION TETAZCOMP
!> @brief ABSCISSAE F SOURCES
C abscisses des sources
      DOUBLE PRECISION XSCE(MAXSCE)
!> @brief ORDINATES OF SOURCES
C ordonnees des sources
      DOUBLE PRECISION YSCE(MAXSCE)
!> @brief ELEVATIONS OF SOURCES
C cotes des points sources donnes
      DOUBLE PRECISION ZSCE(MAXSCE)
!> @brief VELOCITIES OF THE SOURCES ALONG X
C vitesse des sources selon x
      DOUBLE PRECISION USCE(MAXSCE)
!> @brief VELOCITIES OF THE SOURCES ALONG Y
C vitesse des sources selon y
      DOUBLE PRECISION VSCE(MAXSCE)
!> @brief WATER DISCHARGE OF SOURCES TAKEN FROM STEERING FILE
C debits des sources dans le fichier cas
      DOUBLE PRECISION QSCE(MAXSCE)
!> @brief WATER DISCHARGE OF SOURCES COMPUTED WITH T3D_DEBSCE (VARIATIONS IN TIME)
C debits des sources calcule dans t3d_debsce
      DOUBLE PRECISION QSCE2(MAXSCE)
!> @brief VALUE OF THE TRACERS AT THE SOURCES TAKEN FROM STEERING FILE
C valeurs des traceurs des sources dans le fichier cas
      DOUBLE PRECISION TASCE(MAXSCE,MAXTRA)
!> @brief PRESCRIBED VALUES OF TRACERS AT LIQUID BOUNDARIES
C valeurs imposees des traceurs
      DOUBLE PRECISION TRACER(MAXFRO*MAXTRA)
!> @brief RATIO BETWEEN SKIN FRICTION AND MEAN DIAMETER
C ratio entre la rugosite de peau et le diametre moyen des grains
      DOUBLE PRECISION KSPRATIO
!> @brief SHIELDS PARAMETER
C parametre de shields
      DOUBLE PRECISION AC
!
!     FLUX AT BOUNDARIES
!
      DOUBLE PRECISION FLUX_BOUNDARIES(MAXFRO)
!
!     FOR STORING DISCHARGE-ELEVATION CURVES
!
      DOUBLE PRECISION, ALLOCATABLE :: QZ(:,:,:)
!
!     THRESHOLD DEPTH FOR WIND
!
      DOUBLE PRECISION HWIND
!
!     CUMULATED FLUXES OF WATER SINCE BEGINNING OF COMPUTATION
!
      DOUBLE PRECISION FLUXTOTCUM
!
!     INITIAL, PAST AND PRESENT MASS OF WATER
!
      DOUBLE PRECISION MASINI_WATER, MASSEN_WATER,MASSE_WATER
C
C-----------------------------------------------------------------------
C (7) STRINGS
C-----------------------------------------------------------------------
C
!     TITLE
      CHARACTER(LEN=72) TITCAS
!> @brief
C
      CHARACTER(LEN=72) SORT3D,SORT2D
!> @brief
C
      CHARACTER(LEN=72) VARIMP
!> @brief
C
      CHARACTER(LEN=72) VARIM3
!> @brief INITIAL CONDITIONS
C conditions initiales
      CHARACTER(LEN=72) CDTINI
!> @brief ELEMENT
C element
      CHARACTER(LEN=72) ELEMENT
!> @brief
C
      CHARACTER(LEN=3)  BINGEO
!> @brief
C binaire du fichier de resultats
      CHARACTER(LEN=3)  BINRES
!> @brief
C
      CHARACTER(LEN=3)  BINPRE
!> @brief
C
      CHARACTER(LEN=3)  BINHYD
!> @brief
C
      CHARACTER(LEN=20) EQUA
!> @brief
C
      CHARACTER(LEN=32) VARCLA(10)
!> @brief NAMES OF VARIABLES RECOGNISED FROM RESULTS AND GEOMETRY FILES
C noms des variables reconnues dans les fichiers de resultat et de geometrie
      CHARACTER(LEN=32) TEXTE(MAXVAR)
!> @brief NAMES OF VARIABLES RECOGNISED FROM PREVIOUS COMPUTATION FILE
C noms des variables reconnues dans le fichier de calcul precedent
      CHARACTER(LEN=32) TEXTPR(MAXVAR)
!> @brief
C
      CHARACTER(LEN=32) TEXT3(MAXVA3)
!> @brief
C
      CHARACTER(LEN=32) TEXTP3(MAXVA3)
!> @brief
C binaire du fichier des resultats sedimentologiques
      CHARACTER(LEN=3) BIRSED
!> @brief
C binaire du fichier du calcul sedimentologique precedent
      CHARACTER(LEN=3) BISUIS
!> @brief NAMES OF TRACERS
C noms des traceurs
      CHARACTER(LEN=32) NAMETRAC(32)
C
C-----------------------------------------------------------------------
C (8) SLVCFG STRUCTURES
C-----------------------------------------------------------------------
C
!> @brief SOLVER FOR DIFFUSION OF VELOCITIES
C solveur pour la diffusion des vitesses
      TYPE(SLVCFG) :: SLVDVI
!> @brief SOLVER FOR PROPAGATION
C solveur pour la propagation
      TYPE(SLVCFG) :: SLVPRO
!> @brief SOLVER FOR DIFFUSION OF K-EPSILON
C solveur pour la diffusion du k-epsilon
      TYPE(SLVCFG) :: SLVDKE
!> @brief SOLVER FOR DIFFUSION OF TRACERS
C solveur pour la diffusion des traceurs
      TYPE(SLVCFG) :: SLVDTA(MAXTRA)
!> @brief SOLVER FOR VERTICAL VELOCITY COMPONENT
C solveur pour la vitesse verticale
      TYPE(SLVCFG) :: SLVW
!> @brief SOLVER FOR DIFFUSION OF THE SEDIMENT
C solveur pour la diffusion du sediment
      TYPE(SLVCFG) :: SLVDSE
!> @brief SOLVER FOR PPE (NON-HYDROSTATIC)
C solveur pour ppe
      TYPE(SLVCFG) :: SLVPOI
!> @brief SOLVER FOR PROJECTION (NON-HYDROSTATIC)
C solveur pour la projection
      TYPE(SLVCFG) :: SLVPRJ
C
c-----------------------------------------------------------------------
C (9) MESH STRUCTURE(S)
C-----------------------------------------------------------------------
C 2 SEPARATE MESHES, 2D AS USUAL AND 3D WITH SIGMA-MESH SPECIFIC
C FEATURES, SEE ALMESH.F
C
!> @brief 2D MESH WITH SIGMA-MESH SPECIFIC FEATURES
C maillage 2d
      TYPE(BIEF_MESH) :: MESH2D
!> @brief 3D MESH WITH SIGMA-MESH SPECIFIC FEATURES
C maillage 3d
      TYPE(BIEF_MESH) :: MESH3D
C
C-----------------------------------------------------------------------
C (10) ALIASES
C-----------------------------------------------------------------------
C
C     DECLARATION OF POINTERS FOR ALIASES
C     TARGETS ARE ALLOCATED AND POINTED TO IN POINT_TELEMAC3D
C
C     ALIASES FOR WORKING VECTORS, REAL 3D, INTEGER 3D, REAL 2D
C
!
!     BIEF_OBJ STRUCTURES FOR ARRAYS OF DIMENSION NPOIN3
!
      TYPE(BIEF_OBJ), POINTER :: T3_01,T3_02,T3_03,T3_04,T3_05
      TYPE(BIEF_OBJ), POINTER :: T3_06,T3_07,T3_08,T3_09,T3_10
      TYPE(BIEF_OBJ), POINTER :: T3_11,T3_12,T3_13,T3_14,T3_15
      TYPE(BIEF_OBJ), POINTER :: T3_16,T3_17,T3_18
!
!     BIEF_OBJ STRUCTURES FOR INTEGER ARRAYS
!
      TYPE(BIEF_OBJ), POINTER :: IT1, IT2, IT3, IT4
!
!     BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
!
      TYPE(BIEF_OBJ), POINTER :: T2_01,T2_02,T2_03,T2_04,T2_05
      TYPE(BIEF_OBJ), POINTER :: T2_06,T2_07,T2_08,T2_09,T2_10
      TYPE(BIEF_OBJ), POINTER :: T2_11,T2_12,T2_13,T2_14,T2_15
      TYPE(BIEF_OBJ), POINTER :: T2_16,T2_17,T2_18,T2_19,T2_20
      TYPE(BIEF_OBJ), POINTER :: T2_21
!
!     2D NODE COORDINATES
!
      TYPE(BIEF_OBJ), POINTER :: X2,Y2
!
!
!
      TYPE(BIEF_OBJ), POINTER :: Z2
!
!     3D NODE COORDINATES
!
      TYPE(BIEF_OBJ), POINTER :: X3,Y3,Z3
!
!     2D WORKING MATRIX ALLOCATED WITH THE MESH
!
      TYPE(BIEF_OBJ), POINTER :: MTRA2D
!> @brief 2D ELEMENT-ORIENTED WORKING FIELD ALLOCATED WITH THE MESH
C tableau de travail
      TYPE(BIEF_OBJ), POINTER :: W2
!> @brief 3D ELEMENT-ORIENTED WORKING FIELD ALLOCATED WITH THE MESH
C tableau de travail
      TYPE(BIEF_OBJ), POINTER :: W1
!> @brief BASE TRIANGLE SURFACES
C
      TYPE(BIEF_OBJ), POINTER :: SURFA2
!> @brief TRIANGLE SURFACES, IN 3D
C
      TYPE(BIEF_OBJ), POINTER :: SURFA3
!> @brief LATERAL BOUNDARY NORMAL VECTORS DEFINED AT THE NODES
C composantes du vecteur normal aux points frontieres
      TYPE(BIEF_OBJ), POINTER :: XNEBOR2
!> @brief LATERAL BOUNDARY NORMAL VECTORS DEFINED AT THE NODES
C composantes du vecteur normal aux points frontieres
      TYPE(BIEF_OBJ), POINTER :: YNEBOR2
!> @brief 2D NORMAL VECTORS DEFINED PER BOUNDARY SEGMENT
C
      TYPE(BIEF_OBJ), POINTER :: XSGBOR2
!> @brief 2D NORMAL VECTORS DEFINED PER BOUNDARY SEGMENT
C
      TYPE(BIEF_OBJ), POINTER :: YSGBOR2
!> @brief 3D NORMAL VECTORS DEFINED PER BOUNDARY ELEMENT
C
      TYPE(BIEF_OBJ), POINTER :: XSGBOR3
!> @brief 3D NORMAL VECTORS DEFINED PER BOUNDARY ELEMENT
C
      TYPE(BIEF_OBJ), POINTER :: YSGBOR3
!> @brief 3D NORMAL VECTORS DEFINED PER BOUNDARY ELEMENT
C
      TYPE(BIEF_OBJ), POINTER :: ZSGBOR3
!> @brief CONNECTIVITY TABLES IN 2D  : (ELEMENT NUMBER AND LOCAL NODE NUMBER) --> GLOBAL NODE NUMBER
C table de connectivite pour les elements 2d
      TYPE(BIEF_OBJ), POINTER :: IKLE2
!> @brief CONNECTIVITY TABLES IN 3D : (ELEMENT NUMBER AND LOCAL NODE NUMBER) --> GLOBAL NODE NUMBER
C table de connectivite pour les elements 3d
      TYPE(BIEF_OBJ), POINTER :: IKLE3
!> @brief CONNECTIVITY TABLES IN 2D : (NODE BOUNDARY NUMBER) --> GLOBAL NODE NUMBER
C correspondance numerotation frontiere et numerotation globale en 2d
      TYPE(BIEF_OBJ), POINTER :: NBOR2
!> @brief CONNECTIVITY TABLES IN 3D : (NODE BOUNDARY NUMBER) --> GLOBAL NODE NUMBER
C correspondance numerotation frontiere et numerotation globale en 3d
      TYPE(BIEF_OBJ), POINTER :: NBOR3
!> @brief COORDINATES OF POINTS IN THE 3D MESH
C coordonnees du maillage 3d
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X
!> @brief COORDINATES OF POINTS IN THE 3D MESH
C coordonnees du maillage 3d
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Y
!> @brief COORDINATES OF POINTS IN THE 3D MESH
C coordonnees du maillage 3d
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Z
!> @brief NUMBER OF ELEMENTS IN THE 2D MESH
C nombre total d'elements dans le maillage 2d
      INTEGER, POINTER:: NELEM2
!> @brief NUMBER OF ELEMENTS IN THE 3D MESH
C nombre total d'elements dans le maillage 3d
      INTEGER, POINTER:: NELEM3
!> @brief MAXIMUM NUMBER OF ELEMENTS IN THE 2D MESH
C nombre maximal d'elements dans le maillage 2d
      INTEGER, POINTER:: NELMAX2
!> @brief MAXIMUM NUMBER OF ELEMENTS IN THE 3D MESH
C nombre maximal d'elements dans le maillage 3d
      INTEGER, POINTER:: NELMAX3
!> @brief NUMBER OF BOUNDARY POINTS IN THE 2D MESH
C nombre de points frontiere du maillage 2d
      INTEGER, POINTER:: NPTFR2
!> @brief NUMBER OF BOUNDARY POINTS IN THE 3D MESH
C nombre de points sur les cotes 3d
      INTEGER, POINTER:: NPTFR3
!> @brief
C
      INTEGER, POINTER:: NELEB, NELEBX
!> @brief MAXIMUM NUMBER OF BOUNDARY POINTS IN THE 2D MESH
C
      INTEGER, POINTER:: NPTFRX2
!> @brief MAXIMUM NUMBER OF BOUNDARY POINTS IN THE 3D MESH
C
      INTEGER, POINTER:: NPTFRX3
!> @brief DIMENSION OF 2D SPACE
C
      INTEGER, POINTER:: DIM2
!> @brief DIMENSION OF 3D SPACE
C
      INTEGER, POINTER:: DIM3
!> @brief TYPE OF 2D ELEMENT
C
      INTEGER, POINTER:: TYPELM2
!> @brief TYPE OF 3D ELEMENT
C
      INTEGER, POINTER:: TYPELM3
!> @brief NUMBER OF POINTS IN THE 2D MESH
C nombre de points du maillage 2d
      INTEGER, POINTER:: NPOIN2
!> @brief NUMBER OF POINTS IN THE 3D MESH
C nombre de points du maillage 3d
      INTEGER, POINTER:: NPOIN3
!> @brief MAXIMUM NUMBER OF POINTS IN THE 2D MESH
C
      INTEGER, POINTER:: NPMAX2
!> @brief MAXIMUM NUMBER OF POINTS IN THE 3D MESH
C
      INTEGER, POINTER:: NPMAX3
!> @brief MAXIMUM NUMBER OF POINTS NEIGHBOURS OF A POINT IN 2D
C
      INTEGER, POINTER:: MXPTVS2
!> @brief MAXIMUM NUMBER OF POINTS NEIGHBOURS OF A POINT IN 3D
C
      INTEGER, POINTER:: MXPTVS3
!> @brief MAXIMUM NUMBER OF ELEMENTS NEIGHBOURS OF A POINT IN 2D
C
      INTEGER, POINTER:: MXELVS2
!> @brief MAXIMUM NUMBER OF ELEMENTS NEIGHBOURS OF A POINT IN 3D
C
      INTEGER, POINTER:: MXELVS3
!> @brief VECTOR LENGTH OF THE MACHINE (LV2=LV3, FOR SIGMA MESH)
C longueur du vecteur pour la vectorisation
      INTEGER, POINTER:: LV
C
C-----------------------------------------------------------------------
C (11) CONSTANTS INITIALISED IN CSTKEP AND CREF_FREDSOE (FOR ZREF)
C-----------------------------------------------------------------------
C
!> @brief VON KARMAN CONSTANT
C constante de karman
      DOUBLE PRECISION :: KARMAN
!> @brief K-EPSILON CONSTANT
C constante du modele k-epsilon
      DOUBLE PRECISION :: CMU
!> @brief K-EPSILON CONSTANT
C constante du modele k-epsilon
      DOUBLE PRECISION :: C1
!> @brief K-EPSILON CONSTANT
C constante du modele k-epsilon
      DOUBLE PRECISION :: C2
!> @brief K-EPSILON CONSTANT
C constante du modele k-epsilon
      DOUBLE PRECISION :: SIGMAK
!> @brief K-EPSILON CONSTANT
C constante du modele k-epsilon
      DOUBLE PRECISION :: SIGMAE
!> @brief K-EPSILON CONSTANT
C constante du modele k-epsilon
      DOUBLE PRECISION :: VIRT
!> @brief SCHMIDT NUMBER
C
      DOUBLE PRECISION :: SCHMIT
!> @brief MINIMUM K
C k minimum en cas de clipping
      DOUBLE PRECISION :: KMIN
!> @brief MAXIMUM K
C k maximum en cas de clipping
      DOUBLE PRECISION :: KMAX
!> @brief MINIMUM EPSILON
C epsilon minimum en cas de clipping
      DOUBLE PRECISION :: EMIN
!> @brief MAXIMUM EPSILON
C epsilon maximum en cas de clipping
      DOUBLE PRECISION :: EMAX
!
!     PRANDTL NUMBER
!
      DOUBLE PRECISION :: PRANDTL
!
!     K-OMEGA MODEL CONSTANTS
!
      DOUBLE PRECISION :: ALPHA,BETA,BETAS,OMSTAR
!
!
!
      TYPE(BIEF_OBJ), TARGET :: ZREF
C-----------------------------------------------------------------------
C
C      12) TELEMAC-3D FILES
C
C-----------------------------------------------------------------------
C
!
!     MAXIMUM NUMBER OF FILES
!
      INTEGER, PARAMETER :: MAXLU_T3D = 44
!
!     DATA STRUCTURE WITH DATA ON FILES
!
      TYPE(BIEF_FILE) :: T3D_FILES(MAXLU_T3D)
C
      SAVE
C
      END MODULE DECLARATIONS_TELEMAC3D
