!                      ******************************
                       MODULE DECLARATIONS_POSTEL3D
!                      ******************************
!
!***********************************************************************
!  POSTEL3D VERSION 6.0
!***********************************************************************
!=======================================================================
! TELEMAC-3D BEST VERSION NUMBER
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!=======================================================================
!
!  DECLARATION OF THE GLOBAL DATA STRUCTURE IN TELEMAC-3D
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
! 3D VELOCITY COMPONENTS
!
        TYPE(BIEF_OBJ), TARGET :: U, V, W
!
!-----------------------------------------------------------------------
! (2) MATRICES
!-----------------------------------------------------------------------
! NONE
!-----------------------------------------------------------------------
! (3) BLOCKS
!-----------------------------------------------------------------------
!
!
        TYPE(BIEF_OBJ), TARGET :: TAB
!
!
! 2D OUTPUT COMPATIBILITY - OUTPUT VARIABLES ORGANISED IN BLOCKS
!TH POUR BIENTOT, AVEC LE NOUVEAU FORMAT
!TH        TYPE(BIEF_OBJ), TARGET :: VARSOR, VARCL
!
!-----------------------------------------------------------------------
! (4) INTEGERS
!-----------------------------------------------------------------------
! KEY WORDS AND PARAMETERS
!
!       MAXIMUM DE VARIABLES DE SORTIE
        INTEGER, PARAMETER :: MAXVAR = 100
!
! PREVIOUS COMMON MITINT: INTEGER STEERING PARAMETERS
!
      INTEGER NPLAN  , NTRAC  , NTRPA , NVAR(2), NVA3
      INTEGER NR3D , NCOU2 , NENRE
!
      INTEGER IELM3, IELM2H, IELM2V
      INTEGER IELM0, IELMH, IELMU, IELM1, IELMX
      INTEGER SORG3D
      INTEGER IM,JM,NPLINT
      INTEGER NUPRSO,PESOGR,NC2DH,NC2DV
      INTEGER NPLREF(9),NSEG(9)
!
!      NOMBRE MAX DE COUPES
        INTEGER, PARAMETER :: MAXCOU = 9
!      NOMBRE MAX DE POINTS POUR LES COUPES VERTICALES
        INTEGER, PARAMETER :: MAXPTS = 50
!
!-----------------------------------------------------------------------
! (5) LOGICAL VALUES
!-----------------------------------------------------------------------
!
      LOGICAL SIGMAG
      LOGICAL SPHERI
      LOGICAL VARSUB
!
!-----------------------------------------------------------------------
! (6) REALS
!-----------------------------------------------------------------------
!
! PREVIOUS COMMON MITREA, REAL STEERING PARAMETERS PLUS NEW ONES
!
      DOUBLE PRECISION HMIN,  COTINT
!
!TH  A VOIR SI ON MET LE PARAMETRE
!TH  EN DUR POUR L'INSTANT
!      DOUBLE PRECISION HREF(MAXCOU),DISTOR(MAXCOU)
!      DOUBLE PRECISION X2DV(MAXPTS,MAXCOU),Y2DV(MAXPTS,MAXCOU)
      DOUBLE PRECISION HREF(9),DISTOR(9)
!TH      DOUBLE PRECISION ZSTAR(5)
      DOUBLE PRECISION X2DV(50,9),Y2DV(50,9)
!
!-----------------------------------------------------------------------
! (7) STRINGS
!-----------------------------------------------------------------------
!
! PREVIOUS MITCAR
! CHANGES: NOMSUI -> NOMPRE ; NOMR3D -> NOMRES ; NOMR2D -> NOMRBI
!     (SEE MODULE FOR TELEMAC DEFINITIONS)
! CONSEQUENTLY BINR3D -> BINRES ;  BINSUI -> BINPRE ; BINR2D -> BINRBI
!
!TH  ON LAISSE TOUT EN ATTENDANT
!
      CHARACTER(LEN=72) TITCAS, SORT3D, SORT2D, VARIMP
      CHARACTER(LEN=3)  BINGEO, BINRES, BINPRE, BINRBI , BINR3D , BINCOU
!
      CHARACTER(LEN=20) EQUA
      CHARACTER(LEN=32) VARCLA(10), TEXTE(MAXVAR), TEXTPR(MAXVAR)
      CHARACTER(LEN=32) TEXTLU(100)
!
!
!-----------------------------------------------------------------------
! (8) SLVCFG STRUCTURES
!-----------------------------------------------------------------------
! NONE
!-----------------------------------------------------------------------
! (9) MESH STRUCTURE(S)
!-----------------------------------------------------------------------
! TWO SEPARATE MESHES, 2D AS USUAL AND 3D WITH SIGMA-MESH SPECIFIC
! FEATURES, SEE ALMESH.F
!
        TYPE(BIEF_MESH) :: MESH2D, MESH3D
!
!-----------------------------------------------------------------------
! (10) ALIASES
!-----------------------------------------------------------------------
! DECLARATION OF POINTERS FOR ALIASES
! TARGETS ARE ALLOCATED AND POINTED TO IN POINT_POSTEL3D.
!
! USEFUL POINTERS FOR OFTEN USED COMPONENTS IN 2D AND 3D MESH STRUCTURES
!
! X,Y,Z NODE COORDINATES: BASE MESH AND 3D SIGMA MESH
!
        TYPE(BIEF_OBJ), POINTER :: X2, Y2, Z2, X3, Y3, Z3
!
!TH SUREMENT PLEIN DE CHOSES A VIRER
!TH
        TYPE(BIEF_OBJ), POINTER :: XNEBOR2, YNEBOR2
        TYPE(BIEF_OBJ), POINTER :: XNEBOR3, YNEBOR3, ZNEBOR3
!
! 2D AND 3D LATERAL BOUNDARY NORMAL VECTORS DEFINED
! PER BOUNDARY SEGMENT (2D) OR BOUNDARY ELEMENT (3D)
!
        TYPE(BIEF_OBJ), POINTER :: XSGBOR2, YSGBOR2
        TYPE(BIEF_OBJ), POINTER :: XSGBOR3, YSGBOR3, ZSGBOR3
!
! CONNECTIVITY TABLES 2D AND 3D
! (ELEMENT NUMBER AND LOCAL NODE NUMBER) --> GLOBAL NODE NUMBER
!
        TYPE(BIEF_OBJ), POINTER :: IKLE2, IKLE3
!
! TABLES CONNECTING (NODE BOUNDARY NUMBER) --> GLOBAL NODE NUMBER
!
        TYPE(BIEF_OBJ), POINTER :: NBOR2, NBOR3
!
! REAL FIELD POINTERS FOR NODE COORDINATES
!
        DOUBLE PRECISION, DIMENSION(:), POINTER :: X,Y,Z
!
! A NUMBER OF EXTREMELY USEFUL INTEGERS DESCRIBING THE MESH STRUCTURE
! SEE ALMESH.F AND POINT_TELEMAC3D.F
!
        INTEGER, POINTER :: NELEM2, NELEM3
!
        INTEGER, POINTER :: NELMAX2
        INTEGER, POINTER :: NELMAX3 ! PREVIOUSLY NELMA3
!
        INTEGER, POINTER :: NPTFR2  ! PREVIOUSLY SIMPLY NPTFR
        INTEGER, POINTER :: NPTFR3
        INTEGER, POINTER :: NELEB, NELEBX
!
        INTEGER, POINTER :: NPTFRX2, NPTFRX3
        INTEGER, POINTER :: DIM2, DIM3
        INTEGER, POINTER :: TYPELM2, TYPELM3
        INTEGER, POINTER :: NPOIN2, NPOIN3
        INTEGER, POINTER :: NPMAX2, NPMAX3
        INTEGER, POINTER :: MXPTVS2, MXPTVS3
        INTEGER, POINTER :: MXELVS2, MXELVS3
!
!     NEW FILE FORMATS
!
      TYPE(BIEF_FILE) :: POS_FILES(100)
      INTEGER POSPRE,POSHOR,POSVER,POSGEO
!
!-----------------------------------------------------------------------
! SAVE ALL - IMPORTANT
!
      SAVE
!
      END MODULE DECLARATIONS_POSTEL3D
