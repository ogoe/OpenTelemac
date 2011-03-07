!                    *****************************
                     MODULE DECLARATIONS_TELEMAC3D
!                    *****************************
!
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
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
!
!       NOTE: THIS MODULE IS ORGANISED IN 10 PARTS
!
!       (1) VECTORS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       (2) MATRICES (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       (3) BLOCKS (WILL BE DECLARED AS BIEF_OBJ STRUCTURES)
!       (4) INTEGERS
!       (5) LOGICAL VALUES
!       (6) REALS
!       (7) STRINGS
!       (8) SLVCFG STRUCTURES
!       (9) MESH STRUCTURE
!      (10) ALIASES
!
!-----------------------------------------------------------------------
! (1) VECTORS (REAL AND INTEGER)
!-----------------------------------------------------------------------
!
!> @brief 3D VELOCITY COMPONENT AT PREVIOUS TIMESTEP (TIME N)
! composantes de la vitesse au pas de temps precedent (n)
      TYPE(BIEF_OBJ), TARGET :: UN
!> @brief 3D VELOCITY COMPONENT AT PREVIOUS TIMESTEP (TIME N)
! composantes de la vitesse au pas de temps precedent (n)
      TYPE(BIEF_OBJ), TARGET :: VN
!> @brief 3D VELOCITY COMPONENT AT PREVIOUS TIMESTEP (TIME N)
! composantes de la vitesse au pas de temps precedent (n)
      TYPE(BIEF_OBJ), TARGET :: WN
!> @brief 3D VELOCITY COMPONENT AFTER ADVECTION
! composantes de la vitesse apres convection
      TYPE(BIEF_OBJ), TARGET :: UC
!> @brief 3D VELOCITY COMPONENT AFTER ADVECTION
! composantes de la vitesse apres convection
      TYPE(BIEF_OBJ), TARGET :: VC
!> @brief 3D VELOCITY COMPONENT AFTER ADVECTION
! composantes de la vitesse apres convection
      TYPE(BIEF_OBJ), TARGET :: WC
!> @brief 3D VELOCITY COMPONENT AFTER DIFFUSION
! composantes de la vitesse apres diffusion
      TYPE(BIEF_OBJ), TARGET :: UD
!> @brief 3D VELOCITY COMPONENT AFTER DIFFUSION
! composantes de la vitesse apres diffusion
      TYPE(BIEF_OBJ), TARGET :: VD
!> @brief 3D VELOCITY COMPONENT AFTER DIFFUSION
! composantes de la vitesse apres diffusion
      TYPE(BIEF_OBJ), TARGET :: WD
!> @brief 3D VELOCITY COMPONENT
! composantes de la vitesse au temps n+1
      TYPE(BIEF_OBJ), TARGET :: U
!> @brief 3D VELOCITY COMPONENT
! composantes de la vitesse au temps n+1
      TYPE(BIEF_OBJ), TARGET :: V
!> @brief 3D VELOCITY COMPONENT
! composantes de la vitesse au temps n+1
      TYPE(BIEF_OBJ), TARGET :: W
!> @brief EXPLICIT SOURCE TERMS ON VELOCITIES U
! termes sources explicites pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: S0U
!> @brief EXPLICIT SOURCE TERMS ON VELOCITIES V
! termes sources explicites pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: S0V
!> @brief EXPLICIT SOURCE TERMS ON VELOCITIES W
! termes sources explicites pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: S0W
!> @brief IMPLICIT SOURCE TERMS ON VELOCITIES U
! termes sources imlicites pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: S1U
!> @brief IMPLICIT SOURCE TERMS ON VELOCITIES V
! termes sources imlicites pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: S1V
!> @brief IMPLICIT SOURCE TERMS ON VELOCITIES W
! termes sources imlicites pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: S1W
!> @brief
! second membre pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: SMU
!> @brief
! second membre pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: SMV
!> @brief PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
! vitesses u imposees au fond
      TYPE(BIEF_OBJ), TARGET :: UBORF
!> @brief PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
! vitesses v imposees au fond
      TYPE(BIEF_OBJ), TARGET :: VBORF
!> @brief PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
! vitesses w imposees au fond
      TYPE(BIEF_OBJ), TARGET :: WBORF
!> @brief PRESCRIBED VELOCITY ALONG X ON THE LATERAL BOUNDARY
! vitesses u imposees sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: UBORL
!> @brief PRESCRIBED VELOCITY ALONG Y ON THE LATERAL BOUNDARY
! vitesses v imposees sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: VBORL
!> @brief PRESCRIBED VELOCITY ALONG Z ON THE LATERAL BOUNDARY
! vitesses w imposees sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: WBORL
!> @brief PRESCRIBED VELOCITY ALONG X AT THE FREE SURFACE
! vitesses u imposees en surface
      TYPE(BIEF_OBJ), TARGET :: UBORS
!> @brief PRESCRIBED VELOCITY ALONG Y AT THE FREE SURFACE
! vitesses v imposees en surface
      TYPE(BIEF_OBJ), TARGET :: VBORS
!> @brief PRESCRIBED VELOCITY ALONG Z AT THE FREE SURFACE
! vitesses w imposees en surface
      TYPE(BIEF_OBJ), TARGET :: WBORS
!> @brief WORK ARRAY ON VERTICAL BOUNDARIES, FOR SAVING VALUES AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: TRBORSAVE
!> @brief WORK ARRAY ON VERTICAL BOUNDARIES, FOR SAVING VALUES AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: UBORSAVE
!> @brief WORK ARRAY ON VERTICAL BOUNDARIES, FOR SAVING VALUES AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: VBORSAVE
!> @brief WORK ARRAY ON VERTICAL BOUNDARIES, FOR SAVING VALUES AT TIME N
!
      TYPE(BIEF_OBJ), TARGET :: WBORSAVE
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: KBORSAVE,EBORSAVE
!> @brief (FRICTION VELOCITY)**2 FOR BOTTOM
! u etoile au carre
      TYPE(BIEF_OBJ), TARGET :: UETCAR
!> @brief (FRICTION VELOCITY)**2 FOR LATERAL BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: UETCAL
!> @brief LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, ON THE BOTTOM: NU*DU/DN = AUBORF*U + BUBORF
!
      TYPE(BIEF_OBJ), TARGET :: AUBORF
!> @brief LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, ON THE BOTTOM: NU*DU/DN = AUBORF*U + BUBORF
!
      TYPE(BIEF_OBJ), TARGET :: BUBORF
!> @brief LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DU/DN = AUBORL*U + BUBORL
!
      TYPE(BIEF_OBJ), TARGET :: AUBORL
!> @brief LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DU/DN = AUBORL*U + BUBORL
!
      TYPE(BIEF_OBJ), TARGET :: BUBORL
!> @brief LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DU/DN = AUBORS*U + BUBORS
!
      TYPE(BIEF_OBJ), TARGET :: AUBORS
!> @brief LOGARITHMIC LAW FOR U-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DU/DN = AUBORS*U + BUBORS
!
      TYPE(BIEF_OBJ), TARGET :: BUBORS
!> @brief LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, ON THE BOTTOM: NU*DV/DN = AVBORF*V + BVBORF
!
      TYPE(BIEF_OBJ), TARGET :: AVBORF
!> @brief LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, ON THE BOTTOM: NU*DV/DN = AVBORF*V + BVBORF
!
      TYPE(BIEF_OBJ), TARGET :: BVBORF
!> @brief LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DV/DN = AVBORL*V + BVBORL
!
      TYPE(BIEF_OBJ), TARGET :: AVBORL
!> @brief LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DV/DN = AVBORL*V + BVBORL
!
      TYPE(BIEF_OBJ), TARGET :: BVBORL
!> @brief LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DV/DN = AVBORS*V + BVBORS
!
      TYPE(BIEF_OBJ), TARGET :: AVBORS
!> @brief LOGARITHMIC LAW FOR V-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DV/DN = AVBORS*V + BVBORS
!
      TYPE(BIEF_OBJ), TARGET :: BVBORS
!> @brief LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, ON THE BOTTOM: NU*DW/DN = AWBORF*W + BWBORF
!
      TYPE(BIEF_OBJ), TARGET :: AWBORF
!> @brief LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, ON THE BOTTOM: NU*DW/DN = AWBORF*W + BWBORF
!
      TYPE(BIEF_OBJ), TARGET :: BWBORF
!> @brief LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DW/DN = AWBORL*W + BWBORL
!
      TYPE(BIEF_OBJ), TARGET :: AWBORL
!> @brief LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, ON THE LATERAL BOUNDARIES: NU*DW/DN = AWBORL*W + BWBORL
!
      TYPE(BIEF_OBJ), TARGET :: BWBORL
!> @brief LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DW/DN = AWBORS*W + BWBORS
!
      TYPE(BIEF_OBJ), TARGET :: AWBORS
!> @brief LOGARITHMIC LAW FOR W-VELOCITY COMPONENT, AT THE FREE SURFACE: NU*DW/DN = AWBORS*W + BWBORS
!
      TYPE(BIEF_OBJ), TARGET :: BWBORS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR U ON THE BOTTOM
! types de conditions aux limites sur u au fond
      TYPE(BIEF_OBJ), TARGET :: LIUBOF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR V ON THE BOTTOM
! types de conditions aux limites sur v au fond
      TYPE(BIEF_OBJ), TARGET :: LIVBOF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR W ON THE BOTTOM
! types de conditions aux limites sur w au fond
      TYPE(BIEF_OBJ), TARGET :: LIWBOF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR U ON THE LATERAL BOUNDARIES
! types de conditions aux limites sur u sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: LIUBOL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR V ON THE LATERAL BOUNDARIES
! types de conditions aux limites sur v sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: LIVBOL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR W ON THE LATERAL BOUNDARIES
! types de conditions aux limites sur w sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: LIWBOL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR U AT THE FREE SURFACE
! types de conditions aux limites sur u en surface
      TYPE(BIEF_OBJ), TARGET :: LIUBOS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR V AT THE FREE SURFACE
! types de conditions aux limites sur v en surface
      TYPE(BIEF_OBJ), TARGET :: LIVBOS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR W AT THE FREE SURFACE
! types de conditions aux limites sur w en surface
      TYPE(BIEF_OBJ), TARGET :: LIWBOS
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: BOUNDARY_COLOUR
!> @brief PLANE NUMBER
!
      TYPE(BIEF_OBJ), TARGET :: IPBOT
!> @brief SIGMA-TRANSFORMED VERTICAL VELOCITY COMPONENT
!
      TYPE(BIEF_OBJ), TARGET :: WS
!> @brief DYNAMIC PRESSURE AT TIME N+1 (NON-HYDROSTATIC)
!
      TYPE(BIEF_OBJ), TARGET :: DP
!> @brief DYNAMIC PRESSURE AT TIME N (NON-HYDROSTATIC)
!
      TYPE(BIEF_OBJ), TARGET :: DPN
!> @brief HYDROSTATIC PRESSURE (NON-HYDROSTATIC)
!
      TYPE(BIEF_OBJ), TARGET :: PH
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: PBORF, PBORL, PBORS
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: LIPBOF, LIPBOL, LIPBOS
!> @brief K OF K-EPSILON MODEL AT PREVIOUS TIMESTEP (TIME N)
! k (energie turbulente) du modele k-epsilon au pas de temps precedent (n)
      TYPE(BIEF_OBJ), TARGET :: AKN
!> @brief K OF K-EPSILON MODEL AFTER ADVECTION
! k (energie turbulente) du modele k-epsilon apres convection
      TYPE(BIEF_OBJ), TARGET :: AKC
!> @brief K OF K-EPSILON MODEL AT TIME N+1
! k (energie turbulente) du modele k-epsilon au temps n+1
      TYPE(BIEF_OBJ), TARGET :: AK
!> @brief
! terme source explicite pour k du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: S0AK
!> @brief
! terme source implicite pour k du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: S1AK
!> @brief PRESCRIBED K OF K-EPSILON MODEL ON THE BOTTOM
! k du modele k-epsilon impose au fond
      TYPE(BIEF_OBJ), TARGET :: KBORF
!> @brief PRESCRIBED K OF K-EPSILON MODEL ON THE LATERAL BOUNDARY
! k du modele k-epsilon impose sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: KBORL
!> @brief PRESCRIBED K OF K-EPSILON MODEL AT THE FREE SURFACE
! k du modele k-epsilon impose en surface
      TYPE(BIEF_OBJ), TARGET :: KBORS
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, ON THE BOTTOM
!
      TYPE(BIEF_OBJ), TARGET :: AKBORF
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, ON THE BOTTOM
!
      TYPE(BIEF_OBJ), TARGET :: BKBORF
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, ON THE LATERAL BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: AKBORL
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, ON THE LATERAL BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: BKBORL
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, AT THE FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: AKBORS
!> @brief LOGARITHMIC LAW FOR K OF K-EPSILON MODEL, AT THE FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: BKBORS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR K OF K-EPSILON MODEL ON THE BOTTOM
! types de conditions aux limites au fond sur k du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIKBOF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR K OF K-EPSILON MODEL ON THE LATERAL BOUNDARIES
! types de conditions aux limites sur les parois laterales sur k du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIKBOL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR K OF K-EPSILON MODEL AT THE FREE SURFACE
! types de conditions aux limites en surface sur k du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIKBOS
!> @brief EPSILON OF K-EPSILON MODEL AT PREVIOUS TIMESTEP (TIME N)
! epsilon (dissipation turbulente) du modele k-epsilon au pas de temps precedent (n)
      TYPE(BIEF_OBJ), TARGET :: EPN
!> @brief EPSILON OF K-EPSILON MODEL AFTER ADVECTION
! epsilon (dissipation turbulente) du modele k-epsilon apres convection
      TYPE(BIEF_OBJ), TARGET :: EPC
!> @brief EPSILON OF K-EPSILON MODEL AT TIME N+1
! epsilon (dissipation turbulente) du modele k-epsilon au temps n+1
      TYPE(BIEF_OBJ), TARGET :: EP
!> @brief
! terme source explicite pour epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: S0EP
!> @brief
! terme source implicite pour epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: S1EP
!> @brief PRESCRIBED EPSILON OF K-EPSILON MODEL ON THE BOTTOM
! epsilon du modele k-epsilon impose au fond
      TYPE(BIEF_OBJ), TARGET :: EBORF
!> @brief PRESCRIBED EPSILON OF K-EPSILON MODEL ON THE LATERAL BOUNDARY
! epsilon du modele k-epsilon impose sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: EBORL
!> @brief PRESCRIBED EPSILON OF K-EPSILON MODEL AT THE FREE SURFACE
! epsilon du modele k-epsilon impose en surface
      TYPE(BIEF_OBJ), TARGET :: EBORS
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, ON THE BOTTOM
!
      TYPE(BIEF_OBJ), TARGET :: AEBORF
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, ON THE BOTTOM
!
      TYPE(BIEF_OBJ), TARGET :: BEBORF
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, ON THE LATERAL BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: AEBORL
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, ON THE LATERAL BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: BEBORL
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, AT THE FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: AEBORS
!> @brief LOGARITHMIC LAW FOR EPSILON OF K-EPSILON MODEL, AT THE FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: BEBORS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR EPSILON OF K-EPSILON MODEL ON THE BOTTOM
! types de conditions aux limites au fond sur epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIEBOF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR EPSILON OF K-EPSILON MODEL ON THE LATERAL BOUNDARIES
! types de conditions aux limites sur les parois laterales sur epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIEBOL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR EPSILON OF K-EPSILON MODEL AT THE FREE SURFACE
! types de conditions aux limites en surface sur epsilon du modele k-epsilon
      TYPE(BIEF_OBJ), TARGET :: LIEBOS
!> @brief 2D (VERTICALLY INTEGRATED) VELOCITY COMPONENT
      TYPE(BIEF_OBJ), TARGET :: U2D
!> @brief 2D (VERTICALLY INTEGRATED) VELOCITY COMPONENT
! composante 2d de la vitesse
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
! volume de controle a l'instant n+1
      TYPE(BIEF_OBJ), TARGET :: VOLU,VOLUPAR
!> @brief VOLUME AROUND POINTS AT TIME N
      TYPE(BIEF_OBJ), TARGET :: VOLUN,VOLUNPAR
!> @brief
      TYPE(BIEF_OBJ), TARGET :: VOLUT
!> @brief
      TYPE(BIEF_OBJ), TARGET :: VOLU3D,VOLU3DPAR
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: VOLU2D
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: V2DPAR
!> @brief INVERSE OF INTEGRAL OF BASES IN 2D
! inverse du volume des bases en 2d
      TYPE(BIEF_OBJ), TARGET :: UNSV2D
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: UNSV3D
!> @brief
! flux interieur par noeud
      TYPE(BIEF_OBJ), TARGET :: FLUINT
!
!     FLUX AT BOUNDARIES, AND ASSEMBLED FORM IN PARALLEL
!
      TYPE(BIEF_OBJ), TARGET :: FLUEXT,FLUEXTPAR
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: FLINT2
!> @brief CHOL    ABSOLUTE VALUE OF THE VORTICITY
!
      TYPE(BIEF_OBJ), TARGET :: ROTAT
!> @brief RELATIVE DENSITY = (RHO-RHO0)/RHO0
! (rho-rho0)/rho0
      TYPE(BIEF_OBJ), TARGET :: DELTAR
!> @brief RICHARDSON NUMBER
! nombre de richardson
      TYPE(BIEF_OBJ), TARGET :: RI
!> @brief FRICTION COEFFICIENT ON THE BOTTOM
! coefficient de rugosite du fond
      TYPE(BIEF_OBJ), TARGET :: RUGOF
!> @brief FRICTION COEFFICIENT ON THE LATERAL BOUNDARY
! coefficient de rugosite des parois laterales
      TYPE(BIEF_OBJ), TARGET :: RUGOL
!> @brief FRICTION COEFFICIENT VALUES
! coefficient de frottement pour k-epsilon
      TYPE(BIEF_OBJ), TARGET :: CF
!> @brief WIND VELOCITY
!
      TYPE(BIEF_OBJ), TARGET :: WIND
!> @brief ATMOSPHERIC PRESSURE
!
      TYPE(BIEF_OBJ), TARGET :: PATMOS
!> @brief PARAMETERS FOR GLOBAL MASS AND FLUX BALANCES
!
      TYPE(BIEF_OBJ), TARGET :: MASINI
!> @brief
! masse au pas en cours
      TYPE(BIEF_OBJ), TARGET :: MASSE
!> @brief
! masse au pas precedent
      TYPE(BIEF_OBJ), TARGET :: MASSEN
!> @brief FLUX
! flux entre les 2 pas de temps
      TYPE(BIEF_OBJ), TARGET :: FLUX
!     CUMULATED FLUXES OF TRACERS SINCE BEGINNING OF COMPUTATION
      TYPE(BIEF_OBJ), TARGET :: FLUCUM
!> @brief DEPTH AT TIME N+1
! hauteur d'eau au temps n+1
      TYPE(BIEF_OBJ), TARGET :: H
!> @brief DEPTH AT TIME N
! hauteur d'eau au temps n
      TYPE(BIEF_OBJ), TARGET :: HN
!> @brief
! hauteur d'eau de propagation
      TYPE(BIEF_OBJ), TARGET :: HPROP
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: DH
!> @brief
! second membre pour la hauteur d'eau
      TYPE(BIEF_OBJ), TARGET :: SMH
!> @brief PRESCRIBED DEPTH ON LATERAL BOUNDARIES
!
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
! position relative des plans horizontaux
      TYPE(BIEF_OBJ), TARGET :: ZSTAR
!> @brief Z: DISTRIBUTION
!
      TYPE(BIEF_OBJ), TARGET :: ZT
!> @brief DH/DT
!
      TYPE(BIEF_OBJ), TARGET :: DSSUDT
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: ZPLANE
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: TRANSF_PLANE
!> @brief COORDINATES IN THE TRANSFORMED MESH FOR THE METHOD OF CHARACTERISTICS
!
      TYPE(BIEF_OBJ), TARGET :: ZCHAR
!> @brief MASK OF ELEMENTS
! masquage des elements
      TYPE(BIEF_OBJ), TARGET :: MASKEL
!> @brief MASK OF NODES
! masquage des points
      TYPE(BIEF_OBJ), TARGET :: MASKPT
!> @brief 3D MASK OF BOUNDARY ELEMENTS
! masque 3d sur les bords lateraux
      TYPE(BIEF_OBJ), TARGET :: MASKBR
!> @brief POSITIONS OF FLOATING BODIES
! positions successives des flotteurs
      TYPE(BIEF_OBJ), TARGET :: XFLOT
!> @brief POSITIONS OF FLOATING BODIES
! positions successives des flotteurs
      TYPE(BIEF_OBJ), TARGET :: YFLOT
!> @brief POSITIONS OF FLOATING BODIES
! positions successives des flotteurs
      TYPE(BIEF_OBJ), TARGET :: ZFLOT
!> @brief
! z des flotteurs dans le maillage transforme
      TYPE(BIEF_OBJ), TARGET :: ZSFLOT
!> @brief
! coordonnees barycentriques instantannees, au pied des courbes caracteristiques, des flotteurs en 2dh
      TYPE(BIEF_OBJ), TARGET :: SHPFLO
!> @brief
! coordonnees barycentriques instantannees, au pied des courbes caracteristiques, des flotteurs en 1dv
      TYPE(BIEF_OBJ), TARGET :: SHZFLO
!> @brief TIME(STEP) FOR INITIAL RELEASE OF FLOATING BODIES
! numero du pas de temps de largage de chaque flotteur
      TYPE(BIEF_OBJ), TARGET :: DEBFLO
!> @brief TIME(STEP) FOR END OF FOLLOW UP OF FLOATING BODIES
! numero du pas de temps de fin de calcul de derive pour chaque flotteur
      TYPE(BIEF_OBJ), TARGET :: FINFLO
!> @brief
! numeros des elements 2dh, au pied des courbes caracteristiques, dans lesquels se trouve a cet instant chacun des flotteurs
      TYPE(BIEF_OBJ), TARGET :: ELTFLO
!> @brief
! numeros des plans, au pied des courbes caracteristiques, dans lesquels se trouve a cet instant chacun des flotteurs
      TYPE(BIEF_OBJ), TARGET :: ETAFLO
!> @brief
! table de connectivite bidon utilisee pour la sortie des trajectoires sous forme de maillage
      TYPE(BIEF_OBJ), TARGET :: IKLFLO
!> @brief WORKING ARRAY
! tableau de travail
      TYPE(BIEF_OBJ), TARGET :: TRAFLO
!> @brief TYPE OF BOUNDARY CONDITIONS ON DEPTH
! types de conditions aux limites sur h
      TYPE(BIEF_OBJ), TARGET :: LIHBOR
!> @brief NUMBER OF LIQUID BOUNDARIES
!
      TYPE(BIEF_OBJ), TARGET :: NUMLIQ
!> @brief PROPAGATION BC TYPES (TELEMAC2D'S PROPAG)
!
      TYPE(BIEF_OBJ), TARGET :: LIMPRO
!> @brief SECOND MEMBERS (RIGHT HAND SIDE) FOR THE LINEAR EQUATIONS 3D
! seconds membres
      TYPE(BIEF_OBJ), TARGET :: SEM3D
!> @brief SECOND MEMBERS (RIGHT HAND SIDE) FOR THE LINEAR EQUATIONS 2D
! seconds membres
      TYPE(BIEF_OBJ), TARGET :: SEM2D
!> @brief ELEMENT-ORIENTED WORKING ARRAY
! tableau de travail par element 2d
      TYPE(BIEF_OBJ), TARGET :: TE1
!> @brief ELEMENT-ORIENTED WORKING ARRAY
! tableau de travail par element 2d
      TYPE(BIEF_OBJ), TARGET :: TE2
!> @brief ELEMENT-ORIENTED WORKING ARRAY
! tableau de travail par element 2d
      TYPE(BIEF_OBJ), TARGET :: TE3
!> @brief PIECE-WISE LINEAR FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: ZFLATS
!> @brief VOID VECTOR STRUCTURE
! structure vide
      TYPE(BIEF_OBJ), TARGET :: SVIDE
!
!     RIGHT HAND SIDE OF CONTINUITY EQUATION WHEN SOURCES
!
      TYPE(BIEF_OBJ), TARGET :: SOURCES
!> @brief
! vitesse de chute du sediment
      TYPE(BIEF_OBJ), TARGET :: WCHU
!> @brief THICKNESS OF SOLID FRACTION OF THE BED LAYER ( EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS )
! epaisseurs des mailles discretisant le lit ( epai=dz/(1+ivide) )
      TYPE(BIEF_OBJ), TARGET :: EPAI
!> @brief VOID RATIO (GIBSON MODEL ONLY)
! indice des vides aux points du maillage (modele de gibson)
      TYPE(BIEF_OBJ), TARGET :: IVIDE
!> @brief TIME COUNTER FOR CONSOLIDATION MODEL (MULTILAYER MODEL)
! compteur de temps (modele multi-couches)
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
! flux d'erosion en chaque point 2d
      TYPE(BIEF_OBJ), TARGET :: FLUER
!> @brief PROBABILITY OF DEPOSIT
! probabilite de depot en chaque point 2d
      TYPE(BIEF_OBJ), TARGET :: PDEPO
!> @brief RIDIG BED ELEVATION
! cote du fond rigide
      TYPE(BIEF_OBJ), TARGET :: ZR
!> @brief
! concentration d'equilibre
      TYPE(BIEF_OBJ), TARGET :: CREF
!> @brief
! diametre moyen des grains
      TYPE(BIEF_OBJ), TARGET :: DMOY
!> @brief NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
! nombre de points discretisant le fond vaseux sur une verticale
      TYPE(BIEF_OBJ), TARGET :: NPF
!
!-----------------------------------------------------------------------
! (2) MATRICES
!-----------------------------------------------------------------------
!
!> @brief
! matrice supg non symetrique
      TYPE(BIEF_OBJ), TARGET :: MSUPG
!> @brief
! matrice murd non symetrique
      TYPE(BIEF_OBJ), TARGET :: MMURD
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: MDIFF
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: MURD_TF
!> @brief 3D WORKING MATRIX
! matrice de travail
      TYPE(BIEF_OBJ), TARGET :: MTRA1
!> @brief 3D WORKING MATRIX
! matrice de travail
      TYPE(BIEF_OBJ), TARGET :: MTRA2
!> @brief 2D MATRIX
      TYPE(BIEF_OBJ), TARGET :: MBOR2D
!> @brief 2D MATRIX
! matrice de travail 2dh
      TYPE(BIEF_OBJ), TARGET :: MATR2H
!
!-----------------------------------------------------------------------
! (3) BLOCKS
!-----------------------------------------------------------------------
!
!> @brief
! traceurs au pas precedent
      TYPE(BIEF_OBJ), TARGET :: TAN
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: TAC
!> @brief CONCENTRATION OF TRACERS
! traceurs au pas en cours
      TYPE(BIEF_OBJ), TARGET :: TA
!
!     EXPLICIT SOURCE TERM FOR TRACERS
!
      TYPE(BIEF_OBJ), TARGET :: S0TA
!> @brief
! terme source explicite, implicite pour les traceurs
      TYPE(BIEF_OBJ), TARGET :: S1TA
!> @brief PRESCRIBED TRACERS ON THE BOTTOM
!
      TYPE(BIEF_OBJ), TARGET :: TABORF
!> @brief PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
!
      TYPE(BIEF_OBJ), TARGET :: TABORL
!> @brief PRESCRIBED TRACERS AT THE FREE SURFACE
!
      TYPE(BIEF_OBJ), TARGET :: TABORS
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
! loi log sur traceurs : atabo*ta + btabo, au fond
      TYPE(BIEF_OBJ), TARGET :: ATABOF
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
! loi log sur traceurs : atabo*ta + btabo, au fond
      TYPE(BIEF_OBJ), TARGET :: BTABOF
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
! loi log sur traceurs : atabo*ta + btabo, sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: ATABOL
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
! loi log sur traceurs : atabo*ta + btabo, sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: BTABOL
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
! loi log sur traceurs : atabo*ta + btabo, en surface
      TYPE(BIEF_OBJ), TARGET :: ATABOS
!> @brief LOGARITHMIC LAW FOR TRACERS: NU*DTA/DN = ATABO*TA + BTABO
! loi log sur traceurs : atabo*ta + btabo, en surface
      TYPE(BIEF_OBJ), TARGET :: BTABOS
!> @brief TYPES OF BOUNDARY CONDITIONS FOR TRACERS ON THE BOTTOM
! type de conditions aux limites sur traceurs au fond
      TYPE(BIEF_OBJ), TARGET :: LITABF
!> @brief TYPES OF BOUNDARY CONDITIONS FOR TRACERS ON THE LATERAL BOUNDARY
! type de conditions aux limites sur traceurs sur les parois laterales
      TYPE(BIEF_OBJ), TARGET :: LITABL
!> @brief TYPES OF BOUNDARY CONDITIONS FOR TRACERS AT THE FREE SURFACE
! type de conditions aux limites sur traceurs en surface
      TYPE(BIEF_OBJ), TARGET :: LITABS
!> @brief VISCOSITY
! coefficients de viscosite pour la vitesse
      TYPE(BIEF_OBJ), TARGET :: VISCVI
!> @brief DIFFUSIVITY FOR TRACERS
! coefficients de viscosite pour les traceurs
      TYPE(BIEF_OBJ), TARGET :: VISCTA
!> @brief BOTTOM GRADIENTS
!
      TYPE(BIEF_OBJ), TARGET :: GRADZF
!> @brief FREE SURFACE GRADIENT (BLOCK OF 2 COMPONENTS)
!
      TYPE(BIEF_OBJ), TARGET :: GRADZS
!> @brief
!
      TYPE(BIEF_OBJ), TARGET :: GRADZN
!> @brief 2D MASK, FOR TELEMAC2D COMPATIBILITY
! masque pour les segments 2d
      TYPE(BIEF_OBJ), TARGET :: MASK
!> @brief BLOCKS OF ARRAYS FOR THE USER
!
      TYPE(BIEF_OBJ), TARGET :: PRIVE
!
!     BLOCKS OF ARRAYS FOR THE USER
!
      TYPE(BIEF_OBJ), TARGET :: MAT2D, TM1
!> @brief
! structure de tableaux de travail 3d
      TYPE(BIEF_OBJ), TARGET :: TRAV3
!> @brief
! structure de tableaux de travail 2d
      TYPE(BIEF_OBJ), TARGET :: TRAV2
!> @brief
! structure de tableaux de travail d'entiers
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
!
!-----------------------------------------------------------------------
! (4) INTEGERS
!-----------------------------------------------------------------------
! KEYWORDS AND PARAMETERS
!
!> @brief
! maximum de points sources
      INTEGER, PARAMETER :: MAXSCE = 100
!> @brief
! maximum de frontieres liquides
      INTEGER, PARAMETER :: MAXFRO = 100
!> @brief
! maximum number of tracers
      INTEGER, PARAMETER :: MAXTRA = 20
!> @brief
! maximum de variables de sortie
      INTEGER, PARAMETER :: MAXVAR = 100
!> @brief
!
      INTEGER, PARAMETER :: MAXVA3 = 100
!> @brief NUMBER OF TIME STEPS
! nombre total de pas de temps
      INTEGER NIT
!> @brief NUMBER OF HORIZONTAL PLANES
! nombre de plans horizontaux
      INTEGER NPLAN
!> @brief NUMBER OF TRACERS
! nombre de traceurs actifs
      INTEGER NTRAC
!> @brief
!
      INTEGER NTRACER
!> @brief PRINTOUT PERIOD FOR FLOATING BODIES
! periode pour les sorties de flotteurs
      INTEGER FLOPRD
!> @brief NUMBER OF DROGUES
! nombre de flotteurs
      INTEGER NFLOT
!> @brief
      INTEGER GRAPRD
!> @brief LISTING PRINTOUT PERIOD
! periode pour les sorties listing
      INTEGER LISPRD
!> @brief NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
! numero du premier pas de temps pour les sorties graphiques
      INTEGER GRADEB
!> @brief NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
! numero du premier pas de temps pour les sorties listing
      INTEGER LISDEB
!> @brief NUMBER OF BOTTOM SMOOTHINGS
! nombre de lissages du fond
      INTEGER LISFON
!> @brief NUMBER OF SUB ITERATIONS FOR NON LINEARITIES
! nombre de sous iterations pour les non linearites
      INTEGER NSOUSI
!> @brief
! numero du plan intermediaire
      INTEGER NPLINT
!> @brief HORIZONTAL TURBULENCE MODEL
! modele de turbulence horizontal
      INTEGER ITURBH
!> @brief VERTICAL TURBULENCE MODEL
! modele de turbulence vertical
      INTEGER ITURBV
!> @brief TURBULENCE MODEL FOR THE BOTTOM
! regime de turbulence pour le fond
      INTEGER LISRUF
!> @brief TURBULENCE MODEL FOR LATERAL SOLID BOUNDARIES
! regime de turbulence pour les parois laterales
      INTEGER LISRUL
!> @brief INITIAL GUESS FOR DEPTH
! ordre du tir initial pour la hauteur
      INTEGER IORDRH
!> @brief SPATIAL PROJECTION TYPE
! type de projection spatiale
      INTEGER PROTYP
!> @brief
! nombre de points sources
      INTEGER NSCE
!> @brief
! adresses des points sources dans le maillage 2d
      INTEGER ISCE(MAXSCE)
!> @brief
! adresses des plans des points sources trouves
      INTEGER KSCE(MAXSCE)
!> @brief
!
      INTEGER NREJEU
!> @brief SCHEME FOR ADVECTION OF VELOCITIES
! schema pour la convection des vitesses
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
! nombre maximal d'enregistrements des positions successives des flotteurs
      INTEGER NITFLO
!> @brief TREATMENT ON TIDAL FLATS FOR VELOCITIES
! traitement sur les bancs decouvrants pour les vitesses
      INTEGER TRBAVI
!> @brief TREATMENT ON TIDAL FLATS FOR TRACERS
! traitement sur les bancs decouvrants pour les traceurs
      INTEGER TRBATA
!> @brief TREATMENT ON TIDAL FLATS FOR K-EPSILON
! traitement sur les bancs decouvrants pour le k-epsilon
      INTEGER TRBAKE
!> @brief NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
!
      INTEGER NDEBIT
!> @brief NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!
      INTEGER NCOTE
!> @brief NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
!
      INTEGER NVIT
!> @brief ORIGINAL DATE OF TIME
! tableau contenant la date de l'origine des temps
      INTEGER MARDAT(3)
!> @brief ORIGINAL HOUR OF TIME
! tableau contenant l'heure de l'origine des temps
      INTEGER MARTIM(3)
!> @brief VECTOR LENGTH
! longueur du vecteur
      INTEGER LVMAC
!> @brief NUMBER OF ARRAYS IN BLOCK PRIVE
! nombre de tableaux prives
      INTEGER NPRIV
!> @brief RANK OF TEMPERATURE IN TRACERS
!
      INTEGER IND_T
!> @brief RANK OF SALINITY
!
      INTEGER IND_S
!> @brief
!
      INTEGER NDP
!> @brief LAW OF BOTTOM FRICTION
! loi de frottement sur le fond
      INTEGER KFROT
!> @brief LAW OF FRICTION ON LATERAL BOUNDARIES
! loi de frottement sur les parois laterales
      INTEGER KFROTL
!> @brief MATRIX STORAGE
! stockage des matrices
      INTEGER OPTASS
!> @brief
!
      INTEGER PRODUC
!> @brief OPTION FOR THE TREATMENT OF TIDAL FLATS
! option de traitement des bancs decouvrants
      INTEGER OPTBAN
!> @brief TREATMENT OF NEGATIVE DEPTHS
! traitement des hauteurs negatives
      INTEGER OPT_HNEG
!> @brief
!
      INTEGER OPDVIT,OPTSOU
!> @brief SUPG OPTION
! option de supg
      INTEGER OPTSUP(4)
!> @brief
!
      INTEGER OPTASS2D
!> @brief 3D DISCRETISATION TYPE
! type de discretisation 3d
      INTEGER IELM3
!> @brief 2DH DISCRETISATION TYPE
! type de discretisation 2dh
      INTEGER IELM2H
!> @brief 2DV DISCRETISATION TYPE
! type de discretisation 2dv
      INTEGER IELM2V
!> @brief
!
      INTEGER IELM0, IELMH, IELMU, IELM1, IELMX
!> @brief NUMBER OF LAYERS OF 3D ELEMENTS (NPLAN - 1)
! nombre d'etages sur la verticale (NPLAN - 1)
      INTEGER NETAGE
!> @brief
! nombre de variables traitees dans le bilan
      INTEGER NVBIL
!> @brief MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED (GIBSON MODEL)
! nombre maximum de plans horizontaux discretisant le fond vaseux (modele de gibson)
      INTEGER NPFMAX
!> @brief NUMBER OF LAYERS WITHIN THE BED (MULTILAYER MODEL)
! nombre de couches discretisant le fond vaseux (modele de tassement multi-couches)
      INTEGER NCOUCH
!> @brief MIXING LENGTH MODEL
! modele de longueur de melange
      INTEGER MIXING
!> @brief DAMPING FUNCTION
! fonction d'amortissement
      INTEGER DAMPING
!> @brief VELOCITY PROFILES
! profils de vitesse
      INTEGER PROFVEL(MAXFRO)
!> @brief TREATMENT OF FLUXES AT THE BOUNDARIES
! traitement des flux aux frontieres
      INTEGER DIRFLU(MAXFRO)
!> @brief VELOCITY VERTICAL PROFILES
! profils de vitesse sur la verticale
      INTEGER VERPROVEL(MAXFRO)
!> @brief TRACERS VERTICAL PROFILES
! profils des traceurs sur la verticale
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
! periode de sortie pour delwaq
      INTEGER WAQPRD
!> @brief ORIGIN COORDINATES
! coordonnees de l'origine
      INTEGER I_ORIG
!> @brief ORIGIN COORDINATES
! coordonnees de l'origine
      INTEGER J_ORIG
!> @brief NUMBER OF POINTS GIVEN FOR EACH DISCHARGE-ELEVATIONS CURVES
!
      INTEGER PTS_CURVES(MAXFRO)
!> @brief STAGE-DISCHARGE CURVES
! courbes de tarage
      INTEGER STA_DIS_CURVES(MAXFRO)
!> @brief KEYWORD DEBUGGER
! debugger
      INTEGER DEBUG
!> @brief SKIN FRICTION
      INTEGER ICR
!> @brief RECORD NUMBER IN THE WAVE DRIVEN CURRENTS FILE
! numero de l'enregistrement dans le fichier de houle
      INTEGER NPTH
!> @brief GEOMETRY FILE NUMBER
! fichier de geometrie
      INTEGER T3DGEO
!> @brief BOUNDARY CONDITIONS FILE NUMBER
! fichier des conditions aux limites
      INTEGER T3DCLI
!> @brief PREVIOUS COMPUTATION FILE NUMBER
! fichier du calcul precedent
      INTEGER T3DPRE
!> @brief 3D RESULT FILE NUMBER
! fichier des resultats 3d
      INTEGER T3DRES
!> @brief BOTTOM TOPOGRAPHY FILE NUMBER
! fichier des fonds
      INTEGER T3DFON
!> @brief FILE NUMBER FOR SCOPE
! fichier pour scope
      INTEGER T3DSCO
!> @brief 2D RESULT FILE NUMBER
! fichier des resultats 2d
      INTEGER T3DHYD
!> @brief FORMATTED DATA FILE 1
! fichier de donnees formate 1
      INTEGER T3DFO1
!> @brief FORMATTED DATA FILE 2
! fichier de donnees formate 2
      INTEGER T3DFO2
!> @brief BINARY DATA FILE 1
! fichier de donnees binaire 1
      INTEGER T3DBI1
!> @brief BINARY DATA FILE 2
! fichier de donnees binaire 2
      INTEGER T3DBI2
!> @brief
!
      INTEGER T3DSED
!> @brief
!
      INTEGER T3DSUS
!> @brief REFERENCE FILE NUMBER
! fichier de reference
      INTEGER T3DREF
!> @brief RESULT FILE NUMBER FOR SUBIEF-3D
! fichier des resultats pour subief-3d
      INTEGER T3DLEO
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
!
!-----------------------------------------------------------------------
! (5) LOGICAL VALUES
!-----------------------------------------------------------------------
!
! LOGICAL STEERING PARAMETERS
!
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
! si oui, sediment
      LOGICAL SEDI
!> @brief IF YES, TIDAL FLATS
! si oui, traitement des bancs decouvrants
      LOGICAL BANDEC
!> @brief
! si oui, "propagaton linearisee"
      LOGICAL PROLIN
!> @brief
!
      LOGICAL BILMAS
!> @brief
!
      LOGICAL INFMAS
!> @brief
!
      LOGICAL SIGMAG
!> @brief IF YES, HYDROSTATIC INCONSISTENCY FILTER
! si oui, filtre les inconsistances hydrostatiques
      LOGICAL INCHYD
!> @brief
      LOGICAL SORIMP(MAXVAR), SORG2D(MAXVAR)
!> @brief
      LOGICAL SORIM3(MAXVA3), SORG3D(MAXVA3)
!> @brief IF YES, 2D CONTINUATION
! si oui, suite 2d
      LOGICAL SUIT2
!> @brief IF YES, RAIN OR EVAPORATION
! si oui, pluie ou evaporation
      LOGICAL RAIN
!> @brief IF YES, STABILISED INITIAL CONDITION
! si oui, consolidation initiale stabilisee
      LOGICAL CONSOL
!> @brief IF YES, ELEMENTS MASKED BY USER
! si oui, elements masques par l'utilisateur
      LOGICAL MSKUSE
!> @brief DIF(I) WILL SAY IF DIFFUSION SOLVER I IS USED FOR AT LEAST ONE OF THE VARIABLES
      LOGICAL DIF(0:2)
!> @brief
      LOGICAL SPHERI
!> @brief IF YES, THERE ARE MASKED ELEMENTS
! si oui, presence d'elements masques
      LOGICAL MSK
!> @brief
      LOGICAL CLIPH
!> @brief IF YES, LISTING PRINTOUT
! si oui, sortie listing
      LOGICAL LISTIN
!> @brief IF YES, INFORMATION PRINTED ON LISTING
! si oui, informations a restituer sur le listing
      LOGICAL INFOGR
!> @brief IF YES, QUASI-BUBBLE OPTION
! si oui, option quasi-bulle
      LOGICAL QUABUB
!> @brief IF YES, VARIABLES FOR SUBIEF3D
! si oui, variables pour subief3d
      LOGICAL VARSUB
!> @brief IF YES, VALIDATION
! si oui, validation
      LOGICAL VALID
!> @brief IF YES, MULTILAYER CONSOLIDATION MODEL
! si oui, modele de tassement multi-couches
      LOGICAL TASSE
!> @brief IF YES, GIBSON CONSOLIDATION MODEL
! si oui, modele de tassement de gibson
      LOGICAL GIBSON
!> @brief IF YES, INFLUENCE OF TURBULENCE ON SETTLING VELOCITY
! si oui, influence de la turbulence sur la vitesse de chute
      LOGICAL TURBWC
!> @brief IF YES, COHESIVE SEDIMENT
! si oui, sediment cohesif
      LOGICAL SEDCO
!> @brief IF YES, NON-HYDROSTATIC VERSION
! si oui, version non-hydrostatique
      LOGICAL NONHYD
!> @brief
!
      LOGICAL CONPRO
!> @brief FOR INITIALISATION OF K-EPSILON (SET IN CONDIM)
!
      LOGICAL AKEP
!> @brief FOR INITIALISATION OF K-OMEGA (SET IN CONDIM)
!
      LOGICAL AKOM
!> @brief IF YES, INITIAL TIME RESET TO ZERO IN A COMPUTATION CONTINUED
! si oui, remise a zero du temps
      LOGICAL RAZTIM
!> @brief IF YES, DYNAMIC PRESSURE IN WAVE EQUATION
! si oui, pression dynamique dans l'equation d'onde
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
!-----------------------------------------------------------------------
! (6) REALS
!-----------------------------------------------------------------------
!
!> @brief TIME
! valeur du temps en cours
      DOUBLE PRECISION AT
!> @brief TIMESTEP
! pas de temps
      DOUBLE PRECISION DT
!> @brief DURATION
! duree du calcul
      DOUBLE PRECISION DUREE
!> @brief GRAVITY ACCELERATION
! acceleration de la pesanteur
      DOUBLE PRECISION GRAV
!> @brief CORIOLIS COEFFICIENT
! coefficient de coriolis
      DOUBLE PRECISION FCOR
!> @brief DRAG COEFFICIENT OF WIND
! coefficient d'influence du vent
      DOUBLE PRECISION FAIR
!> @brief WIND VELOCITY ALONG X
! vitesse du vent suivant x
      DOUBLE PRECISION FUAIR
!> @brief WIND VELOCITY ALONG Y
! vitesse du vent suivant y
      DOUBLE PRECISION FVAIR
!> @brief AIR TEMPERATURE
! temperature de l'air
      DOUBLE PRECISION TAIR
!> @brief WATER DENSITY AT REFERENCE CONCENTRATION
! masse volumique de reference de l'eau
      DOUBLE PRECISION RHO0
!> @brief FRICTION COEFFICIENT FOR THE BOTTOM
! coefficient de frottement pour le fond
      DOUBLE PRECISION RUGOF0
!> @brief FRICTION COEFFICIENT FOR LATERAL SOLID BOUNDARIES
! coefficient de frottement pour les parois laterales
      DOUBLE PRECISION RUGOL0
!> @brief ZERO
! zero (plus petite valeur non nulle autorisee)
      DOUBLE PRECISION ZERO
!> @brief MINIMAL VALUE FOR DEPTH
! valeur minimale pour la hauteur
      DOUBLE PRECISION HMIN
!> @brief MEAN DEPTH FOR LINEARIZATION
! profondeur moyenne pour la linearisation
      DOUBLE PRECISION HAULIN
!> @brief COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
! coefficient de diffusion horizontale des vitesses
      DOUBLE PRECISION DNUVIH
!> @brief COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
! coefficient de diffusion verticale des vitesses
      DOUBLE PRECISION DNUVIV
!> @brief COEFFICIENT FOR HORIZONTAL DIFFUSION OF TRACERS
! coefficient de diffusion horizontale des traceurs
      DOUBLE PRECISION DNUTAH
!> @brief COEFFICIENT FOR VERTICAL DIFFUSION OF TRACERS
! coefficient de diffusion verticale des traceurs
      DOUBLE PRECISION DNUTAV
!> @brief INITIAL DEPTH
! hauteur initiale
      DOUBLE PRECISION HAUTIN
!> @brief INITIAL ELEVATION
! cote initiale
      DOUBLE PRECISION COTINI
!> @brief RAIN OR EVAPORATION IN MM PER DAY
! pluie ou evaporation en mm par jour
      DOUBLE PRECISION RAIN_MMPD
!> @brief IMPLICITATION FOR DEPTH
! taux d'implicitation pour la hauteur
      DOUBLE PRECISION TETAH
!> @brief IMPLICITATION FOR VELOCITIES
! taux d'implicitation pour les vitesses
      DOUBLE PRECISION TETAU
!> @brief
!
      DOUBLE PRECISION TETAD
!> @brief IMPLICITATION FOR DIFFUSION
! taux d'implicitation pour la diffusion (diagonale si optdif = 2)
      DOUBLE PRECISION TETADI
!> @brief MASS-LUMPING FOR DEPTH
! mass-lumping pour la hauteur
      DOUBLE PRECISION AGGLOH
!> @brief MASS-LUMPING FOR DIFFUSION
! mass-lumping pour la diffusion
      DOUBLE PRECISION AGGLOD
!> @brief MASS-LUMPING FOR VELOCITIES
! mass-lumping pour les vitesses
      DOUBLE PRECISION AGGLOU
!> @brief
! cote du plan intermediaire de reference
      DOUBLE PRECISION COTINT
!> @brief ARRAY OF PRESCRIBED FLOWRATES
! debits imposes
      DOUBLE PRECISION DEBIMP(MAXFRO)
!> @brief ARRAY OF PRESCRIBED ELEVATIONS
! cotes imposees
      DOUBLE PRECISION COTIMP(MAXFRO)
!> @brief ARRAY OF PRESCRIBED VELOCITIES
! vitesses imposees
      DOUBLE PRECISION VITIMP(MAXFRO)
!> @brief BETA EXPANSION COEFFICIENT FOR TRACERS
! coefficient de dilatation volumique beta pour les traceurs
      DOUBLE PRECISION BETAC(MAXTRA)
!> @brief REFERENCE CONCENTRATION OF TRACERS
! valeurs de reference des traceurs
      DOUBLE PRECISION T0AC(MAXTRA)
!> @brief INITIAL VALUES OF TRACERS
! valeurs initiales des traceurs
      DOUBLE PRECISION TRAC0(MAXTRA)
!> @brief DENSITY OF THE SEDIMENT
! masse volumique du sediment
      DOUBLE PRECISION RHOS
!> @brief CRITICAL SHEAR STRESS FOR DEPOSITION
! contrainte critique de depot
      DOUBLE PRECISION TOCD
!> @brief CONCENTRATION (G/L) OF FRESH DEPOSITS
! concentration (g/l) des depots frais
      DOUBLE PRECISION CFDEP
!> @brief REFERENCE BED LAYER THICKNESS FOR NEW LAYER CREATION
! epaisseur de reference pour creer de nouvelles couches du fond vaseux
      DOUBLE PRECISION EPAI0
!> @brief TIMESTEP FOR CONSOLIDATION
! pas de temps de la consolidation
      DOUBLE PRECISION DTC
!> @brief CONCENTRATION (G/L) OF THE CONSOLIDATED MUD
! concentration (g/l) de la vase tassee
      DOUBLE PRECISION CFMAX
!> @brief EROSION COEFFICIENT (EMPIRICAL PARTHENIADES COEFFICIENT)
! coefficient d'erosion (loi de partheniades)
      DOUBLE PRECISION MPART
!> @brief CRITICAL SHEAR STRESS FOR EROSION (FRESH DEPOSIT)
! contrainte critique d'erosion
      DOUBLE PRECISION TOCE
!> @brief FLOCULATION COEFFICIENT
! coefficient traduisant la formation des flocs
      DOUBLE PRECISION TURBA
!> @brief COEFFICIENT RELATIVE TO FLOC DESTRUCTION
! coefficient traduisant la destruction des flocs
      DOUBLE PRECISION TURBB
!> @brief CONSOLIDATION TIME SCALE (ONLY FOR MULTILAYER MODEL)
! temps de sejour de la vase dans les couches (modele multi-couches)
      DOUBLE PRECISION TREST(30)
!> @brief CONSTANT SEDIMENT SETTLING VELOCITY (M/S)
! vitesse de chute constante (m/s)
      DOUBLE PRECISION WCHU0
!> @brief MEAN DIAMETER OF THE SEDIMENT
! diametre moyen des grains
      DOUBLE PRECISION D50
!> @brief GEOGRAPHICAL LATITUDE IN GRAD, POSITIVE FOR NORTHERN AND NEGATIVE ON SOUTHERN HEMISPHERE
!
      DOUBLE PRECISION PHILAT
!> @brief UPWIND COEFFICIENT (BETWEEN 0 AND 1)
!
      DOUBLE PRECISION DELTA
!> @brief HORIZONTAL CORIOLIS PARAMETERS
!
      DOUBLE PRECISION FHOR
!> @brief VERTICAL CORIOLIS PARAMETERS
!
      DOUBLE PRECISION FVER
!> @brief LATITUDE OF THE ORIGIN POINT
! latitude du point origine
      DOUBLE PRECISION LATIT
!> @brief LONGITUDE OF THE ORIGIN POINT
! longitude du point origine
      DOUBLE PRECISION LONGIT
!> @brief NORTH
! nord
      DOUBLE PRECISION NORD
!> @brief FREE SURFACE GRADIENT COMPATIBILITY IN WAVE EQUATION
! compatibilite du gradient de surface libre
      DOUBLE PRECISION TETAZCOMP
!> @brief ABSCISSAE F SOURCES
! abscisses des sources
      DOUBLE PRECISION XSCE(MAXSCE)
!> @brief ORDINATES OF SOURCES
! ordonnees des sources
      DOUBLE PRECISION YSCE(MAXSCE)
!> @brief ELEVATIONS OF SOURCES
! cotes des points sources donnes
      DOUBLE PRECISION ZSCE(MAXSCE)
!> @brief VELOCITIES OF THE SOURCES ALONG X
! vitesse des sources selon x
      DOUBLE PRECISION USCE(MAXSCE)
!> @brief VELOCITIES OF THE SOURCES ALONG Y
! vitesse des sources selon y
      DOUBLE PRECISION VSCE(MAXSCE)
!> @brief WATER DISCHARGE OF SOURCES TAKEN FROM STEERING FILE
! debits des sources dans le fichier cas
      DOUBLE PRECISION QSCE(MAXSCE)
!> @brief WATER DISCHARGE OF SOURCES COMPUTED WITH T3D_DEBSCE (VARIATIONS IN TIME)
! debits des sources calcule dans t3d_debsce
      DOUBLE PRECISION QSCE2(MAXSCE)
!> @brief VALUE OF THE TRACERS AT THE SOURCES TAKEN FROM STEERING FILE
! valeurs des traceurs des sources dans le fichier cas
      DOUBLE PRECISION TASCE(MAXSCE,MAXTRA)
!> @brief PRESCRIBED VALUES OF TRACERS AT LIQUID BOUNDARIES
! valeurs imposees des traceurs
      DOUBLE PRECISION TRACER(MAXFRO*MAXTRA)
!> @brief RATIO BETWEEN SKIN FRICTION AND MEAN DIAMETER
! ratio entre la rugosite de peau et le diametre moyen des grains
      DOUBLE PRECISION KSPRATIO
!> @brief SHIELDS PARAMETER
! parametre de shields
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
!
!-----------------------------------------------------------------------
! (7) STRINGS
!-----------------------------------------------------------------------
!
!     TITLE
      CHARACTER(LEN=72) TITCAS
!> @brief
!
      CHARACTER(LEN=72) SORT3D,SORT2D
!> @brief
!
      CHARACTER(LEN=72) VARIMP
!> @brief
!
      CHARACTER(LEN=72) VARIM3
!> @brief INITIAL CONDITIONS
! conditions initiales
      CHARACTER(LEN=72) CDTINI
!> @brief ELEMENT
! element
      CHARACTER(LEN=72) ELEMENT
!> @brief
!
      CHARACTER(LEN=3)  BINGEO
!> @brief
! binaire du fichier de resultats
      CHARACTER(LEN=3)  BINRES
!> @brief
!
      CHARACTER(LEN=3)  BINPRE
!> @brief
!
      CHARACTER(LEN=3)  BINHYD
!> @brief
!
      CHARACTER(LEN=20) EQUA
!> @brief
!
      CHARACTER(LEN=32) VARCLA(10)
!> @brief NAMES OF VARIABLES RECOGNISED FROM RESULTS AND GEOMETRY FILES
! noms des variables reconnues dans les fichiers de resultat et de geometrie
      CHARACTER(LEN=32) TEXTE(MAXVAR)
!> @brief NAMES OF VARIABLES RECOGNISED FROM PREVIOUS COMPUTATION FILE
! noms des variables reconnues dans le fichier de calcul precedent
      CHARACTER(LEN=32) TEXTPR(MAXVAR)
!> @brief
!
      CHARACTER(LEN=32) TEXT3(MAXVA3)
!> @brief
!
      CHARACTER(LEN=32) TEXTP3(MAXVA3)
!> @brief
! binaire du fichier des resultats sedimentologiques
      CHARACTER(LEN=3) BIRSED
!> @brief
! binaire du fichier du calcul sedimentologique precedent
      CHARACTER(LEN=3) BISUIS
!> @brief NAMES OF TRACERS
! noms des traceurs
      CHARACTER(LEN=32) NAMETRAC(32)
!
!-----------------------------------------------------------------------
! (8) SLVCFG STRUCTURES
!-----------------------------------------------------------------------
!
!> @brief SOLVER FOR DIFFUSION OF VELOCITIES
! solveur pour la diffusion des vitesses
      TYPE(SLVCFG) :: SLVDVI
!> @brief SOLVER FOR PROPAGATION
! solveur pour la propagation
      TYPE(SLVCFG) :: SLVPRO
!> @brief SOLVER FOR DIFFUSION OF K-EPSILON
! solveur pour la diffusion du k-epsilon
      TYPE(SLVCFG) :: SLVDKE
!> @brief SOLVER FOR DIFFUSION OF TRACERS
! solveur pour la diffusion des traceurs
      TYPE(SLVCFG) :: SLVDTA(MAXTRA)
!> @brief SOLVER FOR VERTICAL VELOCITY COMPONENT
! solveur pour la vitesse verticale
      TYPE(SLVCFG) :: SLVW
!> @brief SOLVER FOR DIFFUSION OF THE SEDIMENT
! solveur pour la diffusion du sediment
      TYPE(SLVCFG) :: SLVDSE
!> @brief SOLVER FOR PPE (NON-HYDROSTATIC)
! solveur pour ppe
      TYPE(SLVCFG) :: SLVPOI
!> @brief SOLVER FOR PROJECTION (NON-HYDROSTATIC)
! solveur pour la projection
      TYPE(SLVCFG) :: SLVPRJ
!
!-----------------------------------------------------------------------
! (9) MESH STRUCTURE(S)
!-----------------------------------------------------------------------
! 2 SEPARATE MESHES, 2D AS USUAL AND 3D WITH SIGMA-MESH SPECIFIC
! FEATURES, SEE ALMESH.F
!
!> @brief 2D MESH WITH SIGMA-MESH SPECIFIC FEATURES
! maillage 2d
      TYPE(BIEF_MESH) :: MESH2D
!> @brief 3D MESH WITH SIGMA-MESH SPECIFIC FEATURES
! maillage 3d
      TYPE(BIEF_MESH) :: MESH3D
!
!-----------------------------------------------------------------------
! (10) ALIASES
!-----------------------------------------------------------------------
! DECLARATION OF POINTERS FOR ALIASES
! TARGETS ARE ALLOCATED AND POINTED TO IN POINT_TELEMAC3D
!
! ALIASES FOR WORKING VECTORS, REAL 3D, INTEGER 3D, REAL 2D
!
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_01
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_02
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_03
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_04
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_05
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_06
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_07
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_08
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_09
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_10
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_11
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_12
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_13
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_14
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_15
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_16
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_17
!> @brief
! tableau de travail de dimension npoin3
      TYPE(BIEF_OBJ), POINTER :: T3_18
!> @brief
!
      TYPE(BIEF_OBJ), POINTER :: IT1, IT2, IT3, IT4
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_01
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_02
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_03
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_04
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_05
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_06
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_07
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_08
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_09
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_10
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_11
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_12
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_13
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_14
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_15
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_16
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_17
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_18
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_19
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_20
!> @brief BIEF_OBJ STRUCTURE FOR LOCAL WORK, DIMENSION NPOIN2
! tableau de travail de dimension npoin2
      TYPE(BIEF_OBJ), POINTER :: T2_21
!> @brief X NODE COORDINATE: BASE MESH
!
      TYPE(BIEF_OBJ), POINTER :: X2
!> @brief Y NODE COORDINATE: BASE MESH
!
      TYPE(BIEF_OBJ), POINTER :: Y2
!> @brief Z NODE COORDINATE: BASE MESH
!
      TYPE(BIEF_OBJ), POINTER :: Z2
!> @brief X NODE COORDINATES: 3D SIGMA MESH
!
      TYPE(BIEF_OBJ), POINTER :: X3
!> @brief Y NODE COORDINATES: 3D SIGMA MESH
!
      TYPE(BIEF_OBJ), POINTER :: Y3
!> @brief Z NODE COORDINATES: 3D SIGMA MESH
!
      TYPE(BIEF_OBJ), POINTER :: Z3
!> @brief 2D WORKING MATRIX ALLOCATED WITH THE MESH
!
      TYPE(BIEF_OBJ), POINTER :: MTRA2D
!> @brief 2D ELEMENT-ORIENTED WORKING FIELD ALLOCATED WITH THE MESH
! tableau de travail
      TYPE(BIEF_OBJ), POINTER :: W2
!> @brief 3D ELEMENT-ORIENTED WORKING FIELD ALLOCATED WITH THE MESH
! tableau de travail
      TYPE(BIEF_OBJ), POINTER :: W1
!> @brief BASE TRIANGLE SURFACES
!
      TYPE(BIEF_OBJ), POINTER :: SURFA2
!> @brief TRIANGLE SURFACES, IN 3D
!
      TYPE(BIEF_OBJ), POINTER :: SURFA3
!> @brief LATERAL BOUNDARY NORMAL VECTORS DEFINED AT THE NODES
! composantes du vecteur normal aux points frontieres
      TYPE(BIEF_OBJ), POINTER :: XNEBOR2
!> @brief LATERAL BOUNDARY NORMAL VECTORS DEFINED AT THE NODES
! composantes du vecteur normal aux points frontieres
      TYPE(BIEF_OBJ), POINTER :: YNEBOR2
!> @brief 2D NORMAL VECTORS DEFINED PER BOUNDARY SEGMENT
!
      TYPE(BIEF_OBJ), POINTER :: XSGBOR2
!> @brief 2D NORMAL VECTORS DEFINED PER BOUNDARY SEGMENT
!
      TYPE(BIEF_OBJ), POINTER :: YSGBOR2
!> @brief 3D NORMAL VECTORS DEFINED PER BOUNDARY ELEMENT
!
      TYPE(BIEF_OBJ), POINTER :: XSGBOR3
!> @brief 3D NORMAL VECTORS DEFINED PER BOUNDARY ELEMENT
!
      TYPE(BIEF_OBJ), POINTER :: YSGBOR3
!> @brief 3D NORMAL VECTORS DEFINED PER BOUNDARY ELEMENT
!
      TYPE(BIEF_OBJ), POINTER :: ZSGBOR3
!> @brief CONNECTIVITY TABLES IN 2D  : (ELEMENT NUMBER AND LOCAL NODE NUMBER) --> GLOBAL NODE NUMBER
! table de connectivite pour les elements 2d
      TYPE(BIEF_OBJ), POINTER :: IKLE2
!> @brief CONNECTIVITY TABLES IN 3D : (ELEMENT NUMBER AND LOCAL NODE NUMBER) --> GLOBAL NODE NUMBER
! table de connectivite pour les elements 3d
      TYPE(BIEF_OBJ), POINTER :: IKLE3
!> @brief CONNECTIVITY TABLES IN 2D : (NODE BOUNDARY NUMBER) --> GLOBAL NODE NUMBER
! correspondance numerotation frontiere et numerotation globale en 2d
      TYPE(BIEF_OBJ), POINTER :: NBOR2
!> @brief CONNECTIVITY TABLES IN 3D : (NODE BOUNDARY NUMBER) --> GLOBAL NODE NUMBER
! correspondance numerotation frontiere et numerotation globale en 3d
      TYPE(BIEF_OBJ), POINTER :: NBOR3
!> @brief COORDINATES OF POINTS IN THE 3D MESH
! coordonnees du maillage 3d
      DOUBLE PRECISION, DIMENSION(:), POINTER :: X
!> @brief COORDINATES OF POINTS IN THE 3D MESH
! coordonnees du maillage 3d
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Y
!> @brief COORDINATES OF POINTS IN THE 3D MESH
! coordonnees du maillage 3d
      DOUBLE PRECISION, DIMENSION(:), POINTER :: Z
!> @brief NUMBER OF ELEMENTS IN THE 2D MESH
! nombre total d'elements dans le maillage 2d
      INTEGER, POINTER:: NELEM2
!> @brief NUMBER OF ELEMENTS IN THE 3D MESH
! nombre total d'elements dans le maillage 3d
      INTEGER, POINTER:: NELEM3
!> @brief MAXIMUM NUMBER OF ELEMENTS IN THE 2D MESH
! nombre maximal d'elements dans le maillage 2d
      INTEGER, POINTER:: NELMAX2
!> @brief MAXIMUM NUMBER OF ELEMENTS IN THE 3D MESH
! nombre maximal d'elements dans le maillage 3d
      INTEGER, POINTER:: NELMAX3
!> @brief NUMBER OF BOUNDARY POINTS IN THE 2D MESH
! nombre de points frontiere du maillage 2d
      INTEGER, POINTER:: NPTFR2
!> @brief NUMBER OF BOUNDARY POINTS IN THE 3D MESH
! nombre de points sur les cotes 3d
      INTEGER, POINTER:: NPTFR3
!> @brief
!
      INTEGER, POINTER:: NELEB, NELEBX
!> @brief MAXIMUM NUMBER OF BOUNDARY POINTS IN THE 2D MESH
!
      INTEGER, POINTER:: NPTFRX2
!> @brief MAXIMUM NUMBER OF BOUNDARY POINTS IN THE 3D MESH
!
      INTEGER, POINTER:: NPTFRX3
!> @brief DIMENSION OF 2D SPACE
!
      INTEGER, POINTER:: DIM2
!> @brief DIMENSION OF 3D SPACE
!
      INTEGER, POINTER:: DIM3
!> @brief TYPE OF 2D ELEMENT
!
      INTEGER, POINTER:: TYPELM2
!> @brief TYPE OF 3D ELEMENT
!
      INTEGER, POINTER:: TYPELM3
!> @brief NUMBER OF POINTS IN THE 2D MESH
! nombre de points du maillage 2d
      INTEGER, POINTER:: NPOIN2
!> @brief NUMBER OF POINTS IN THE 3D MESH
! nombre de points du maillage 3d
      INTEGER, POINTER:: NPOIN3
!> @brief MAXIMUM NUMBER OF POINTS IN THE 2D MESH
!
      INTEGER, POINTER:: NPMAX2
!> @brief MAXIMUM NUMBER OF POINTS IN THE 3D MESH
!
      INTEGER, POINTER:: NPMAX3
!> @brief MAXIMUM NUMBER OF POINTS NEIGHBOURS OF A POINT IN 2D
!
      INTEGER, POINTER:: MXPTVS2
!> @brief MAXIMUM NUMBER OF POINTS NEIGHBOURS OF A POINT IN 3D
!
      INTEGER, POINTER:: MXPTVS3
!> @brief MAXIMUM NUMBER OF ELEMENTS NEIGHBOURS OF A POINT IN 2D
!
      INTEGER, POINTER:: MXELVS2
!> @brief MAXIMUM NUMBER OF ELEMENTS NEIGHBOURS OF A POINT IN 3D
!
      INTEGER, POINTER:: MXELVS3
!> @brief VECTOR LENGTH OF THE MACHINE (LV2=LV3, FOR SIGMA MESH)
! longueur du vecteur pour la vectorisation
      INTEGER, POINTER:: LV
!
!-----------------------------------------------------------------------
! (11) CONSTANTS INITIALISED IN CSTKEP AND CREF_FREDSOE (FOR ZREF)
!-----------------------------------------------------------------------
!
!> @brief VON KARMAN CONSTANT
! constante de karman
      DOUBLE PRECISION :: KARMAN
!> @brief K-EPSILON CONSTANT
! constante du modele k-epsilon
      DOUBLE PRECISION :: CMU
!> @brief K-EPSILON CONSTANT
! constante du modele k-epsilon
      DOUBLE PRECISION :: C1
!> @brief K-EPSILON CONSTANT
! constante du modele k-epsilon
      DOUBLE PRECISION :: C2
!> @brief K-EPSILON CONSTANT
! constante du modele k-epsilon
      DOUBLE PRECISION :: SIGMAK
!> @brief K-EPSILON CONSTANT
! constante du modele k-epsilon
      DOUBLE PRECISION :: SIGMAE
!> @brief K-EPSILON CONSTANT
! constante du modele k-epsilon
      DOUBLE PRECISION :: VIRT
!> @brief SCHMIDT NUMBER
!
      DOUBLE PRECISION :: SCHMIT
!> @brief MINIMUM K
! k minimum en cas de clipping
      DOUBLE PRECISION :: KMIN
!> @brief MAXIMUM K
! k maximum en cas de clipping
      DOUBLE PRECISION :: KMAX
!> @brief MINIMUM EPSILON
! epsilon minimum en cas de clipping
      DOUBLE PRECISION :: EMIN
!> @brief MAXIMUM EPSILON
! epsilon maximum en cas de clipping
      DOUBLE PRECISION :: EMAX
!> @brief PRANDTL NUMBER
! nombre de prandtl
      DOUBLE PRECISION :: PRANDTL
!> @brief K-OMEGA CONSTANT
! constante du modele k-omega
      DOUBLE PRECISION :: ALPHA
!> @brief K-OMEGA CONSTANT
! constante du modele k-omega
      DOUBLE PRECISION :: BETA
!> @brief K-OMEGA CONSTANT
! constante du modele k-omega
      DOUBLE PRECISION :: BETAS
!> @brief K-OMEGA CONSTANT
! constante du modele k-omega
      DOUBLE PRECISION :: OMSTAR
!> @brief
!
      DOUBLE PRECISION :: ZREF
!
!-----------------------------------------------------------------------
!
!      12) TELEMAC-3D FILES
!
!-----------------------------------------------------------------------
!
!> @brief
!
      INTEGER, PARAMETER :: MAXLU_T3D = 44
!> @brief
!
      TYPE(BIEF_FILE) :: T3D_FILES(MAXLU_T3D)
!
      SAVE
!
      END MODULE DECLARATIONS_TELEMAC3D