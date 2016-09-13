!                    ***************
                     MODULE BIEF_DEF
!                    ***************
!
!
!***********************************************************************
! BIEF
!***********************************************************************
!
!brief    DEFINITION OF THE BIEF STRUCTURES
!+
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        09/05/2014
!+        V7P0
!+   Integer I8 buffers added in the mesh structure.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        22/03/2016
!+        V7P2
!+   Adding component STOXEBE for the storage option of off-diagonal
!+   terms of EBE matrices.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!=======================================================================
!
!     COMMON VALUES FOR PARALLEL MODE
!
!     NPTIR: NUMBER OF INTERFACE POINTS OF THE SUB-DOMAIN
!     IPID: PROCESSOR NUMBER
!     NCSIZE: NUMBER OF PROCESSORS
      INTEGER NPTIR,IPID,NCSIZE,NHALO
!
!     MAXIMUM GEOMETRICAL MULTIPLICITY OF A NODE
      INTEGER, PARAMETER :: NBMAXNSHARE =  10
!     FOR A DOMAIN, MAXIMUM NUMBER OF DOMAIN NEIGHBOURS/2
!     BE CAREFUL: VARIABLE USED IN PARTEL.F TOO
      INTEGER, PARAMETER :: NBMAXDSHARE = 80
!
!=======================================================================
!
!  STRUCTURE OF POINTER TO A BIEF_OBJ, TO HAVE ARRAYS OF POINTERS
!  IN THE BIEF_OBJ STRUCTURE FOR BLOCKS
!
!  BIEF RELEASE 6.0
!
!=======================================================================
!
!       THIS IS NECESSARY IN FORTRAN 90 TO HAVE ARRAYS OF POINTERS
!       LIKE THE COMPONENT ADR BELOW, WHICH ENABLES TO BUILD BLOCKS
!       WHICH ARE ARRAYS OF POINTERS TO BIEF_OBJ STRUCTURES
!
        TYPE POINTER_TO_BIEF_OBJ
          TYPE(BIEF_OBJ), POINTER :: P
        END TYPE POINTER_TO_BIEF_OBJ
!
!=======================================================================
!
!  STRUCTURE OF VECTOR, MATRIX OR BLOCK IN A SINGLE OBJECT : BIEF_OBJ
!
!=======================================================================
!
        TYPE BIEF_OBJ
!
!-----------------------------------------------------------------------
!
!       HEADER COMMON TO ALL OBJECTS
!
!         KEY : ALWAYS 123456 TO CHECK MEMORY OVERWRITING
          INTEGER KEY
!
!         TYPE: 2: VECTOR,  3: MATRIX,  4: BLOCK
          INTEGER TYPE
!
!         Contains the name of its father (i.e the bloc that created it)
!         If father is XXXXXX the bief_obj was created on its own
          CHARACTER(LEN=6) FATHER
!
!         NAME: FORTRAN NAME OF OBJECT IN 6 CHARACTERS
          CHARACTER(LEN=6) NAME
!
!-----------------------------------------------------------------------
!
!       FOR VECTORS
!
!
!         NAT: NATURE (1:DOUBLE PRECISION  2:INTEGER)
          INTEGER NAT
!
!         ELM: TYPE OF ELEMENT
          INTEGER ELM
!
!         DIM1: FIRST DIMENSION OF VECTOR
          INTEGER DIM1
!
!         MAXDIM1: MAXIMUM SIZE PER DIMENSION
          INTEGER MAXDIM1
!
!         DIM2: SECOND DIMENSION OF VECTOR
          INTEGER DIM2
!
!         MAXDIM2: MAXIMUM SECOND DIMENSION OF VECTOR
          INTEGER MAXDIM2
!
!         DIMDISC: TYPE OF ELEMENT IF VECTOR IS DISCONTINUOUS AT
!                  THE BORDER BETWEEN ELEMENTS, OR 0 IF NOT
          INTEGER DIMDISC
!
!         STATUS:
!         0: ANY ARRAY
!         1: VECTOR DEFINED ON A MESH, NO CHANGE OF DISCRETISATION
!         2: VECTOR DEFINED ON A MESH, CHANGE OF DISCRETISATION ALLOWED
          INTEGER STATUS
!
!         TYPR: TYPE OF VECTOR OF REALS
!         '0' : NIL   '1' : EQUAL TO 1  'Q' : NO SPECIFIC PROPERTY
          CHARACTER*1 TYPR
!
!         TYPI: TYPE OF VECTOR OF INTEGERS
!         '0' : NIL   '1' : EQUAL TO 1  'Q' : NO SPECIFIC PROPERTY
          CHARACTER*1 TYPI
!
!         POINTER TO DOUBLE PRECISION 1-DIMENSION ARRAY
!         DATA ARE STORED HERE FOR A DOUBLE PRECISION VECTOR
          DOUBLE PRECISION, POINTER,DIMENSION(:)::R
!
!         POINTER TO INTEGER 1-DIMENSION ARRAY
!         DATA ARE STORED HERE FOR AN INTEGER VECTOR
          INTEGER, POINTER,DIMENSION(:)::I
!
!-----------------------------------------------------------------------
!
!       FOR MATRICES
!
!         STO: TYPE OF STORAGE  1: CLASSICAL EBE   3: EDGE-BASED STORAGE
          INTEGER STO
!
!         STOX: ORDER OF STORAGE OF OFF-DIAGONAL TERMS
!         FOR EBE: 1=(NELMAX,NDP)  2=(NDP,NELMAX)
          INTEGER STOX
!
!         ELMLIN: TYPE OF ELEMENT OF ROW
          INTEGER ELMLIN
!
!         ELMCOL: TYPE OF ELEMENT OF COLUMN
          INTEGER ELMCOL
!
!         TYPDIA: TYPE OF DIAGONAL
!         '0' : NIL   'I' : IDENTITY  'Q' : NO SPECIFIC PROPERTY
          CHARACTER(LEN=1) TYPDIA
!
!         TYPEXT: TYPE OF EXTRA-DIAGONAL TERMS
!         '0' : NIL   'S' : SYMMETRY  'Q' : NO SPECIFIC PROPERTY
          CHARACTER(LEN=1) TYPEXT
!
!         POINTER TO A BIEF_OBJ FOR DIAGONAL
          TYPE(BIEF_OBJ), POINTER :: D
!
!         POINTER TO A BIEF_OBJ FOR EXTRA-DIAGONAL TERMS
          TYPE(BIEF_OBJ), POINTER :: X
!
!         PRO: TYPE OF MATRIX-VECTOR PRODUCT
          INTEGER PRO
!
!-----------------------------------------------------------------------
!
!       FOR BLOCKS
!
!         BLOCKS ARE IN FACT ARRAYS OF POINTERS TO BIEF_OBJ STRUCTURES
!         ADR(I)%P WILL BE THE I-TH BIEF_OBJ OBJECT
!
!         N: NUMBER OF OBJECTS IN THE BLOCK
          INTEGER N
!         MAXBLOCK: MAXIMUM NUMBER OF OBJECTS IN THE BLOCK
          INTEGER MAXBLOCK
!         ADR: ARRAY OF POINTERS TO OBJECTS (WILL BE OF SIZE MAXBLOCK)
          TYPE(POINTER_TO_BIEF_OBJ), POINTER, DIMENSION(:) :: ADR
!
!-----------------------------------------------------------------------
!
        END TYPE BIEF_OBJ
!
!
!=======================================================================
!
!  STRUCTURE OF MESH : BIEF_MESH
!
!=======================================================================
!
        TYPE BIEF_MESH
!
!         1) A HEADER
!
!         NAME: NAME OF MESH IN 6 CHARACTERS
          CHARACTER(LEN=6) NAME
!
!         2) A SERIES OF INTEGER VALUES (DECLARED AS POINTERS TO ENABLE
!                                        ALIASES)
!
!         NELEM: NUMBER OF ELEMENTS IN MESH
          INTEGER, POINTER :: NELEM
!
!         NELMAX: MAXIMUM NUMBER OF ELEMENTS ENVISAGED
          INTEGER, POINTER :: NELMAX
!
!         NPTFR: NUMBER OF 1D BOUNDARY NODES, EVEN IN 3D
          INTEGER, POINTER :: NPTFR
!
!         NPTFRX: NUMBER OF 1D BOUNDARY NODES, EVEN IN 3D
          INTEGER, POINTER :: NPTFRX
!
!         NELEB: NUMBER OF BOUNDARY ELEMENTS (SEGMENTS IN 2D)
!         IN 3D WITH PRISMS:
!         NUMBER OF LATERAL BOUNDARY ELEMENTS FOR SIGMA MESH
          INTEGER, POINTER :: NELEB
!
!         NELEBX: MAXIMUM NELEB
          INTEGER, POINTER :: NELEBX
!
!         NSEG: NUMBER OF SEGMENTS IN THE MESH
          INTEGER, POINTER :: NSEG
!
!         NSEGBOR: NUMBER OF BORDER SEGMENTS IN THE MESH
          INTEGER, POINTER :: NSEGBOR
!
!         DIM1: DIMENSION OF DOMAIN (2 OR 3)
          INTEGER, POINTER :: DIM1
!
!         TYPELM: TYPE OF ELEMENT (10 FOR TRIANGLES, 40 FOR PRISMS)
          INTEGER, POINTER :: TYPELM
!
!         TYPELM: TYPE OF ELEMENT (10 FOR TRIANGLES, 40 FOR PRISMS)
          INTEGER, POINTER :: TYPELMBND
!
!         NPOIN: NUMBER OF VERTICES (OR LINEAR NODES) IN THE MESH
          INTEGER, POINTER :: NPOIN
!
!         NPMAX: MAXIMUM NUMBER OF VERTICES IN THE MESH
          INTEGER, POINTER :: NPMAX
!
!         MXPTVS: MAXIMUM NUMBER OF POINTS ADJACENT TO 1 POINT
          INTEGER, POINTER :: MXPTVS
!
!         MXELVS: MAXIMUM NUMBER OF ELEMENTS ADJACENT TO 1 POINT
          INTEGER, POINTER :: MXELVS
!
!         LV: MAXIMUM VECTOR LENGTH ALLOWED ON VECTOR COMPUTERS,
!             DUE TO ELEMENT NUMBERING
          INTEGER, POINTER :: LV
!
!         NDS: NUMBERS OF NODES, ELEMENTS, SEGMENTS, OF DIFFERENT
!              TYPES OF DISCRETISATION
          INTEGER, POINTER :: NDS(:,:)
!
!
!         3) A SERIES OF BIEF_OBJ TO STORE INTEGER ARRAYS
!
!         IKLE: CONNECTIVITY TABLE IKLE(NELMAX,NDP) AND KLEI(NDP,NELMAX)
          TYPE(BIEF_OBJ), POINTER :: IKLE,KLEI
!
!         IFABOR: TABLE GIVING ELEMENTS BEHIND FACES OF A TRIANGLE
          TYPE(BIEF_OBJ), POINTER :: IFABOR
!
!         NELBOR: ELEMENTS OF THE BOUNDARY
          TYPE(BIEF_OBJ), POINTER :: NELBOR
!
!         NULONE: LOCAL NUMBER OF BOUNDARY POINTS FOR BOUNDARY ELEMENTS
          TYPE(BIEF_OBJ), POINTER :: NULONE
!
!         KP1BOR: POINTS FOLLOWING AND PRECEDING A BOUNDARY POINT
          TYPE(BIEF_OBJ), POINTER :: KP1BOR
!
!         NBOR: GLOBAL NUMBER OF BOUNDARY POINTS
          TYPE(BIEF_OBJ), POINTER :: NBOR
!
!         IKLBOR: CONNECTIVITY TABLE FOR BOUNDARY POINTS
          TYPE(BIEF_OBJ), POINTER :: IKLBOR
!
!         IFANUM: FOR STORAGE 2, NUMBER OF SEGMENT IN ADJACENT ELEMENT
!         OF A TRIANGLE
          TYPE(BIEF_OBJ), POINTER :: IFANUM
!
!         IKLEM1: ADRESSES OF NEIGHBOURS OF POINTS FOR FRONTAL
!         MATRIX-VECTOR PRODUCT
          TYPE(BIEF_OBJ), POINTER :: IKLEM1
!
!         LIMVOI: FOR FRONTAL MATRIX-VECTOR PRODUCT, ADDRESSES OF POINTS
!         WITH A GIVEN NUMBER OF NEIGHBOURS
          TYPE(BIEF_OBJ), POINTER :: LIMVOI
!
!         NUBO: FOR FINITE VOLUMES, GLOBAL NUMBERS OF VERTICES OF SEGMENTS
          TYPE(BIEF_OBJ), POINTER :: NUBO
!
!         FOR SEGMENT-BASED STORAGE
!
!         GLOSEG: GLOBAL NUMBERS OF VERTICES OF SEGMENTS
          TYPE(BIEF_OBJ), POINTER :: GLOSEG
!         ELTSEG: SEGMENTS FORMING AN ELEMENT
          TYPE(BIEF_OBJ), POINTER :: ELTSEG
!         ORISEG: ORIENTATION OF SEGMENTS FORMING AN ELEMENT 1:ANTI 2:CLOCKWISE
          TYPE(BIEF_OBJ), POINTER :: ORISEG

!         FOR UNSTRUCTURED 3D (IELM = 31 SO FAR)
!         GLOSEGBOR: GLOBAL NUMBERS OF VERTICES OF SEGMENTS
          TYPE(BIEF_OBJ), POINTER :: GLOSEGBOR
!         ELTSEGBOR: SEGMENTS FORMING AN ELEMENT
          TYPE(BIEF_OBJ), POINTER :: ELTSEGBOR
!         ORISEGBOR: ORIENTATION OF SEGMENTS FORMING AN ELEMENT 1:ANTI 2:CLOCKWISE
          TYPE(BIEF_OBJ), POINTER :: ORISEGBOR
!
!         FOR THE METHOD OF CHARACTERISTICS
!
!         ELTCAR: STARTING ELEMENT FOR TREATING A POINT
!         IF 0, IT IS IN ANOTHER SUBDOMAIN
!
          TYPE(BIEF_OBJ), POINTER :: ELTCAR
!
!         SERIES OF ARRAYS FOR PARALLEL MODE
!         HERE GLOBAL MEANS NUMBER IN THE WHOLE DOMAIN
!              LOCAL  MEANS NUMBER IN THE SUB-DOMAIN
!
!         KNOLG: GIVES THE INITIAL GLOBAL NUMBER OF A LOCAL POINT
          TYPE(BIEF_OBJ), POINTER :: KNOLG
!         NACHB: NUMBERS OF PROCESSORS CONTAINING A GIVEN POINT
          TYPE(BIEF_OBJ), POINTER :: NACHB
!         ISEG: GLOBAL NUMBER OF FOLLOWING OR PRECEDING POINT IN THE BOUNDARY
!         IF IT IS IN ANOTHER SUB-DOMAIN.
          TYPE(BIEF_OBJ), POINTER :: ISEG
!         ADDRESSES IN ARRAYS SENT BETWEEN PROCESSORS
          TYPE(BIEF_OBJ), POINTER :: INDPU
!
!         DIMENSION NHP(NBMAXNSHARE,NPTIR)
!         NHP(IZH,IR) IS THE GLOBAL NUMBER IN THE SUB-DOMAIN OF A POINT
!         WHOSE NUMBER IS IR IN THE INTERFACE WITH THE IZ-TH HIGHER RANK PROCESSOR
          TYPE(BIEF_OBJ), POINTER :: NHP
!         NHM IS LIKE NHP, BUT WITH LOWER RANK PROCESSORS
          TYPE(BIEF_OBJ), POINTER :: NHM
!
!         FOR FINITE VOLUMES AND KINETIC SCHEMES
          TYPE(BIEF_OBJ), POINTER :: JMI
!         ELEMENTAL HALO NEIGHBOURHOOD DESCRIPTION IN PARALLEL
!         IFAPAR(6,NELEM2)
!         IFAPAR(1:3,IELEM): PROCESSOR NUMBERS BEHIND THE 3 ELEMENT EDGES
!                            NUMBER FROM 0 TO NCSIZE-1
!         IFAPAR(4:6,IELEM): -LOCAL- ELEMENT NUMBERS BEHIND THE 3 EDGES
!                            IN THE NUMBERING OF PARTITIONS THEY BELONG TO
          TYPE(BIEF_OBJ), POINTER :: IFAPAR
!
!         4) A SERIES OF BIEF_OBJ TO STORE REAL ARRAYS
!
!         XEL: COORDINATES X PER ELEMENT
          TYPE(BIEF_OBJ), POINTER :: XEL
!
!         YEL: COORDINATES Y PER ELEMENT
          TYPE(BIEF_OBJ), POINTER :: YEL
!
!         ZEL: COORDINATES Z PER ELEMENT
          TYPE(BIEF_OBJ), POINTER :: ZEL
!
!         SURFAC: AREAS OF ELEMENTS (IN 2D)
          TYPE(BIEF_OBJ), POINTER :: SURFAC
!
!         SURDET: 1/DET OF ISOPARAMETRIC TRANSFORMATION
          TYPE(BIEF_OBJ), POINTER :: SURDET
!
!         LGSEG: LENGTH OF 2D BOUNDARY SEGMENTS
          TYPE(BIEF_OBJ), POINTER :: LGSEG
!
!         XSGBOR: NORMAL X TO 1D BOUNDARY SEGMENTS
          TYPE(BIEF_OBJ), POINTER :: XSGBOR
!
!         YSGBOR: NORMAL Y TO 1D BOUNDARY SEGMENTS
          TYPE(BIEF_OBJ), POINTER :: YSGBOR
!
!         ZSGBOR: NORMAL Z TO 1D BOUNDARY SEGMENTS
          TYPE(BIEF_OBJ), POINTER :: ZSGBOR
!
!         XNEBOR: NORMAL X TO 1D BOUNDARY POINTS
          TYPE(BIEF_OBJ), POINTER :: XNEBOR
!
!         YNEBOR: NORMAL Y TO 1D BOUNDARY POINTS
          TYPE(BIEF_OBJ), POINTER :: YNEBOR
!
!         ZNEBOR: NORMAL Z TO 1D BOUNDARY POINTS
          TYPE(BIEF_OBJ), POINTER :: ZNEBOR
!
!         X: COORDINATES OF POINTS
          TYPE(BIEF_OBJ), POINTER :: X
!
!         Y: COORDINATES OF POINTS
          TYPE(BIEF_OBJ), POINTER :: Y
!
!         Z: COORDINATES OF POINTS
          TYPE(BIEF_OBJ), POINTER :: Z
!
!         COSLAT: LATITUDE COSINE
          TYPE(BIEF_OBJ), POINTER :: COSLAT
!
!         SINLAT: LATITUDE SINE
          TYPE(BIEF_OBJ), POINTER :: SINLAT
!
!         DISBOR: DISTANCE TO 1D BOUNDARIES
          TYPE(BIEF_OBJ), POINTER :: DISBOR
!
!         M: WORKING MATRIX
          TYPE(BIEF_OBJ), POINTER :: M
!
!         MSEG: WORKING MATRIX FOR SEGMENT-BASED STORAGE
          TYPE(BIEF_OBJ), POINTER :: MSEG
!
!         W: WORKING ARRAY FOR A NON-ASSEMBLED VECTOR
          TYPE(BIEF_OBJ), POINTER :: W
!
!         WI8: WORKING ARRAY FOR A NON-ASSEMBLED VECTOR STORED IN INTEGERS
          INTEGER(KIND=K8), POINTER :: WI8(:)
!
!         T: WORKING ARRAY FOR AN ASSEMBLED VECTOR
          TYPE(BIEF_OBJ), POINTER :: T
!
!         TI8: WORKING ARRAY FOR AN ASSEMBLED VECTOR STORED IN INTEGERS
          INTEGER(KIND=K8), POINTER :: TI8(:)
!
!         VNOIN: FOR FINITE VOLUMES
          TYPE(BIEF_OBJ), POINTER :: VNOIN
!
!         XSEG: X COORDINATE OF FOLLOWING OR PRECEDING POINT IN THE BOUNDARY
!         IF IT IS IN ANOTHER SUB-DOMAIN
          TYPE(BIEF_OBJ), POINTER :: XSEG
!
!         YSEG: Y COORDINATE OF FOLLOWING OR PRECEDING POINT IN THE BOUNDARY
!         IF IT IS IN ANOTHER SUB-DOMAIN
          TYPE(BIEF_OBJ), POINTER :: YSEG
!
!         IFAC: MULTIPLICATION FACTOR FOR POINTS IN THE BOUNDARY FOR
!               DOT PRODUCT. FAC=1 ON 1 SUBDOMAIN AND 0 FOR OTHERS
          TYPE(BIEF_OBJ), POINTER :: IFAC
!
!         FOR PARALLEL MODE AND NON BLOCKING COMMUNICATION (SEE PARINI.F)
!
!         NUMBER OF PROCESSORS WITH POINTS IN COMMON WITH THE SUB-DOMAIN
          INTEGER       , POINTER :: NB_NEIGHB
!         FOR ANY NEIGHBOURING PROCESSOR, NUMBER OF POINTS
!         SHARED WITH IT
          TYPE(BIEF_OBJ), POINTER :: NB_NEIGHB_PT
!         RANK OF PROCESSORS WITH WHICH TO COMMUNICATE FOR POINTS
          TYPE(BIEF_OBJ), POINTER :: LIST_SEND
!         NH_COM(DIM1NHCOM,NB_NEIGHB)
!         WHERE DIM1NHCOM IS THE MAXIMUM NUMBER OF POINTS SHARED
!         WITH ANOTHER PROCESSOR (OR SLIGHTLY MORE FOR 16 BYTES ALIGNMENT)
!         NH_COM(I,J) IS THE GLOBAL NUMBER IN THE SUB-DOMAIN OF I-TH
!         POINT SHARED WITH J-TH NEIGHBOURING PROCESSOR
          TYPE(BIEF_OBJ), POINTER :: NH_COM
!
!         NUMBER OF NEIGHBOURING PROCESSORS WITH EDGES
!         IN COMMON WITH THE SUB-DOMAIN
          INTEGER       , POINTER :: NB_NEIGHB_SEG
!         FOR ANY NEIGHBOURING PROCESSOR, NUMBER OF EDGES
!         SHARED WITH IT
          TYPE(BIEF_OBJ), POINTER :: NB_NEIGHB_PT_SEG
!         RANK OF PROCESSORS WITH WHICH TO COMMUNICATE FOR EDGES
          TYPE(BIEF_OBJ), POINTER :: LIST_SEND_SEG
!         LIKE NH_COM BUT FOR EDGES
          TYPE(BIEF_OBJ), POINTER :: NH_COM_SEG
!
!         WILL BE USED AS BUFFER BY MPI IN PARALLEL
!
          TYPE(BIEF_OBJ), POINTER :: BUF_SEND
          TYPE(BIEF_OBJ), POINTER :: BUF_RECV
          INTEGER(KIND=K8), POINTER :: BUF_SENDI8(:)
          INTEGER(KIND=K8), POINTER :: BUF_RECVI8(:)
!
!         FOR FINITE VOLUMES AND KINETIC SCHEMES
!
          TYPE(BIEF_OBJ), POINTER :: CMI,DPX,DPY
          TYPE(BIEF_OBJ), POINTER :: DTHAUT,AIRST
!
!         CENTER OF MASS OF ELEMENTS NEIGHBORING AN EDGE
!
          TYPE(BIEF_OBJ), POINTER :: COORDG
!
        END TYPE BIEF_MESH
!
!=======================================================================
!
!  STRUCTURE OF SOLVER CONFIGURATION
!
!=======================================================================
!
        TYPE SLVCFG
!
!         SLV: CHOICE OF SOLVER
          INTEGER SLV
!
!         NITMAX: MAXIMUM NUMBER OF ITERATIONS
          INTEGER NITMAX
!
!         PRECON: TYPE OF PRECONDITIONING
          INTEGER PRECON
!
!         KRYLOV: DIMENSION OF KRYLOV SPACE FOR GMRES SOLVER
          INTEGER KRYLOV
!
!         EPS: ACCURACY
          DOUBLE PRECISION EPS
!
!         ZERO: TO CHECK DIVISIONS BY ZERO
          DOUBLE PRECISION ZERO
!
!         OK: IF PRECISION EPS HAS BEEN REACHED
          LOGICAL OK
!
!         NIT: NUMBER OF ITERATIONS IF PRECISION REACHED
          INTEGER NIT
!
        END TYPE SLVCFG
!
!=======================================================================
!
!  STRUCTURE OF FILE
!
!=======================================================================
!
        TYPE BIEF_FILE
!
!         LU: LOGICAL UNIT TO OPEN THE FILE
          INTEGER LU
!
!         NAME: NAME OF FILE
          CHARACTER(LEN=144) NAME
!
!         TELNAME: NAME OF FILE IN TEMPORARY DIRECTORY
          CHARACTER(LEN=6) TELNAME
!
!         FMT: FORMAT (SERAFIN, MED, ETC.)
          CHARACTER(LEN=8) FMT
!
!         ACTION: READ, WRITE OR READWRITE
          CHARACTER(LEN=9) ACTION
!
!         BINASC: ASC FOR ASCII OR BIN FOR BINARY
          CHARACTER(LEN=3) BINASC
!
!         TYPE: KIND OF FILE
          CHARACTER(LEN=12) TYPE
!
        END TYPE BIEF_FILE
!
!=======================================================================
!
!  STRUCTURE OF CONTROL SECTION
!
!  ADDED BY JACEK JANKOWSKI, BAW KARLSRUHE,
!  USED BY TELEMAC-2D AND SISYPHE
!
!=======================================================================
!
      TYPE CHAIN_TYPE
        INTEGER :: NPAIR(2)
        DOUBLE PRECISION :: XYBEG(2), XYEND(2)
        CHARACTER(LEN=24) :: DESCR
        INTEGER :: NSEG
        INTEGER, POINTER :: LISTE(:,:)
      END TYPE CHAIN_TYPE
!
!=======================================================================
!
!  STRUCTURES FOR OIL SPILLS
!
!=======================================================================
!
      TYPE COMPO
        DOUBLE PRECISION::SOL
        DOUBLE PRECISION::MASS
        DOUBLE PRECISION::TB
      END TYPE COMPO

      TYPE OIL_PART
        INTEGER::STATE
        INTEGER::ELTOIL,ETAOIL
        INTEGER::TPSECH
        INTEGER::ID
        DOUBLE PRECISION::XOIL,YOIL,ZOIL
        DOUBLE PRECISION::MASS0
        DOUBLE PRECISION::MASS
        DOUBLE PRECISION::MASS_EVAP,MASS_DISS
        DOUBLE PRECISION::SURFACE,SHZOIL
        DOUBLE PRECISION,DIMENSION(3)::SHPOIL
        TYPE(COMPO),DIMENSION(:),ALLOCATABLE::COMPO
        TYPE(COMPO),DIMENSION(:),ALLOCATABLE::HAP
      END TYPE OIL_PART
!
!=======================================================================
!
!  STRUCTURE OF WEIRS : WEIRS
!
!=======================================================================
!
      TYPE WEIR_ELEMENT
        INTEGER :: NUM_GLO
        INTEGER :: N_1A_1, N_1A_2
        INTEGER :: N_1B_1, N_1B_2
        INTEGER :: N_2A_1, N_2A_2
        INTEGER :: N_2B_1, N_2B_2
        INTEGER :: NB_NEIGH
        INTEGER, DIMENSION(NBMAXNSHARE) :: LIST_NEIGH
        DOUBLE PRECISION :: Z1, Z2
        DOUBLE PRECISION :: WIDTH
        DOUBLE PRECISION :: Q, Q0
      END TYPE WEIR_ELEMENT
!
      TYPE WEIR_ELEMENT_PROC
        INTEGER :: NUM_NEIGH
        INTEGER :: NB_ELEM
        INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_ELEM
      END TYPE WEIR_ELEMENT_PROC
!
      TYPE WEIR_NODES
        INTEGER :: NUM_GLO
        INTEGER :: NB_NEIGH
        INTEGER, DIMENSION(NBMAXNSHARE) :: LIST_NEIGH
        INTEGER, DIMENSION(NBMAXNSHARE) :: NUM_LOC
        DOUBLE PRECISION :: QN, ZFN, HN
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TRAC
      END TYPE WEIR_NODES
!
      TYPE WEIR_NODES_PROC
        INTEGER :: NUM_NEIGH
        INTEGER :: NB_NODES
        INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_NODES
        INTEGER, DIMENSION(:), ALLOCATABLE :: NUM_GLO
        INTEGER, DIMENSION(:), ALLOCATABLE :: NUM_LOC
      END TYPE WEIR_NODES_PROC
!
!=======================================================================
!
      END MODULE BIEF_DEF
