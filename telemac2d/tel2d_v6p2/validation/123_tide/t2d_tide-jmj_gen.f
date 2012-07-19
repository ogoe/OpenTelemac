!
!
!   THIS FORTRAN FILE CONTAINS SUBROUTINES:
!   - TIDAL_MODEL_T2D FOR IMPOSITION OF BOUNDARY CONDITIONS FOR TIDE
!     (FREE SURFACE AND VELOCITIES)
!   - CORFON FOR TIDAL FLATS TREATMENT
!
!                    **************************
                     SUBROUTINE TIDAL_MODEL_T2D
!                    **************************
!
!
!***********************************************************************
! TELEMAC2D   V6P2                                   28/10/2010
!***********************************************************************
!
!brief    FINDS TIDAL BOUNDARY CONDITIONS AT THE OPEN SEA BOUNDARIES
!+
!
!history  C-T PHAM (LNHE)
!+        30/05/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |-->|   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
      USE TPXO
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,NODALCORR,ICALHW
      DOUBLE PRECISION XSHIFT,YSHIFT
      LOGICAL TIDALBCGEN
!
!-----------------------------------------------------------------------
!
!     PARAMETERS FOR TIDAL BOUNDARY CONDITIONS
!
!     CTIDEV: COEFFICIENT TO CALIBRATE THE VELOCITIES, DEFAULT = SQRT(CTIDE)
!     ACCORDING TO DV, IF A CORRECTION COEFFICIENT CTIDE IS APPLIED
!     FOR WATER DEPTHS, ANOTHER ONE MUST BE APPLIED FOR VELOCITIES
!     = SQRT(CTIDE)
!
      IF(CTIDEV.EQ.999999.D0) CTIDEV = SQRT(CTIDE)
!
!     TIDALBCGEN: LOGICAL FOR GENERATION OF TIDAL BOUNDARY CONDITIONS OR NOT
!                 CURRENTLY WORKS ONLY FOR SCALAR COMPUTATIONS
!                 FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      TIDALBCGEN = .TRUE.
!
!     NODALCORR (OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION)
!             IN SUBROUTINE BORD_TIDE
!             DEFAULT = 1 CURRENTLY (FROZEN AT THE DATE MARDAT)
!     IN THE STEERING FILE, THE KEYWORD 'ORIGINAL DATE OF TIME' HAVE TO BE SET
!     WARNING, FORMAT: YEAR, MONTH, DAY
!
!     OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION F
!     0: NOT FROZEN, COMPUTED AT EACH TIME STEP
!     1: FROZEN WITH VALUE AT THE BEGINNING OF THE SIMULATION
!     2: FROZEN WITH VALUE AT THE MIDDLE OF THE YEAR OF THE SIMULATION
!     (SINGLE POSSIBILITY FOR TIDALTYPE = 7)
!
      NODALCORR = 1
      IF(TIDALTYPE.EQ.7) NODALCORR = 2
!
!     ICALHW: NUMBER THAT MAY BE CHOSEN BY THE USER TO CALIBRATE HIGH WATER
!             OR AUTOMATICALLY CHOSEN, IN CASE OF THE MODELLING OF A SCHEMATIC TIDE
!             IN SUBROUTINE BORD_TIDE
!             DEFAULT = 0 (AUTOMATICALLY CHOSEN)
!             FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      ICALHW = 0
!
!     OPTIONAL SHIFT OF COORDINATES
!     FOR JMJ DATA BASE ONLY AT THE MOMENT
!
      XSHIFT =  +7000.D0
      YSHIFT = +11400.D0
!
!     FILES:
!
!     T2DBDD: TIDE DATA BASE
!     T2DHAR: HARMONIC CONSTANTS FILE
!     T2DTID: TIDAL MODEL FILE
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALDB.EQ.1) THEN
        IF(TIDALBCGEN) THEN
          CALL BORD_TIDAL_BC(MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                       NPTFR,KENT,KENTU,
     &                       MESH,GEOSYST,NUMZONE,LAMBD0,PHI0,
     &                       TIDALTYPE,BOUNDARY_COLOUR,MAXFRO,
     &                       T2D_FILES(T2DBDD)%LU,
     &                       T2D_FILES(T2DTID)%LU,
     &                       T2D_FILES(T2DHAR)%LU,XSHIFT,YSHIFT)
        ENDIF
!
        CALL BORD_TIDE(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                 NPOIN,NPTFR,AT,NCOTE,NVITES,
     &                 NUMLIQ%I,KENT,KENTU,
     &                 T2D_FILES(T2DIMP)%NAME,TIDALTYPE,
     &                 CTIDE,MSL,CTIDEV,NODALCORR,T2D_FILES(T2DHAR)%LU,
     &                 BOUNDARY_COLOUR,
     &                 HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                 MARDAT,MARTIM)
      ELSEIF(TIDALDB.EQ.2) THEN
        CALL BORD_TIDE_TPXO(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                      NPOIN,NPTFR,AT,NCOTE,NVITES,
     &                      NUMLIQ%I,KENT,KENTU,MESH,
     &                      T2D_FILES(T2DIMP)%NAME,TIDALTYPE,
     &                      CTIDE,MSL,CTIDEV,NODALCORR,
     &                      BOUNDARY_COLOUR,
     &                      HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                      MARDAT,MARTIM,T2D_FILES,T2DBB1,T2DBB2,
     &                      X,Y,GEOSYST,NUMZONE,LAMBD0,PHI0,INTMICON)
      ELSEIF(TIDALDB.EQ.3) THEN
        CALL BORD_TIDE_LEGOS(ZF%R,MESH%NBOR%I,LIHBOR%I,LIUBOR%I,
     &                       NPOIN,NPTFR,AT,NCOTE,NVITES,
     &                       NUMLIQ%I,KENT,KENTU,
     &                       T2D_FILES(T2DIMP)%NAME,TIDALTYPE,
     &                       CTIDE,MSL,CTIDEV,NODALCORR,
     &                       T2D_FILES(T2DHAR)%LU,BOUNDARY_COLOUR,
     &                       HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     &                       MARDAT,MARTIM)
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO K=1,NPTFR
        IF(NUMTIDE%I(K).GT.0) THEN
!         POSSIBLE SMOOTHING AT THE BEGINNING
          IF(AT.LT.1800.D0) THEN
            UBTIDE%R(K) = UBTIDE%R(K)*(AT/1800.D0)
            VBTIDE%R(K) = VBTIDE%R(K)*(AT/1800.D0)
          ENDIF
          IF(LIUBOR%I(K).EQ.KENTU) THEN
            UBOR%R(K) = UBTIDE%R(K)
            VBOR%R(K) = VBTIDE%R(K)
            U%R(MESH%NBOR%I(K)) = UBOR%R(K)
            V%R(MESH%NBOR%I(K)) = VBOR%R(K)
          ENDIF
          IF(LIHBOR%I(K).EQ.KENT) THEN
            HBOR%R(K) = HBTIDE%R(K)
            H%R(MESH%NBOR%I(K)) = HBOR%R(K)
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE CORFON
!                    *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
!
!history  J-M HERVOUET (LNHE)
!+        01/03/1990
!+        V5P2
!+
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
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
      INTEGER          N,I,FIRST,LAST
!
!-----------------------------------------------------------------------
!
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  SUPPRESS TIDAL FLATS ON MARITIME BOUNDARIES
!  PARAMETERS FIRST AND LAST THAT MAY BE CHANGED!!!
      FIRST = 250
      LAST  = 339
!
      DO N=1,MESH%NPTFR
        I=BOUNDARY_COLOUR%I(N)
        IF(I.GE.FIRST.AND.I.LE.LAST) THEN
          ZF%R(MESH%NBOR%I(N)) = MIN(0.D0,ZF%R(MESH%NBOR%I(N)))
        ENDIF
      ENDDO
!
      IF(LNG.EQ.1) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
          WRITE(LU,*)
        ENDIF
      ENDIF
      IF(LNG.EQ.2) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
          WRITE(LU,*)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ************************
                     SUBROUTINE BORD_TIDAL_BC
!                    ************************
!
     &(NBOR,LIHBOR,LIUBOR,NPTFR,
     & KENT,KENTU,MESH,GEOSYST,NUMZONE,LAMBD0,PHI0,TIDALTYPE,
     & BOUNDARY_COLOUR,MAXFRO,NFO2,NBI2,NRFO,XSHIFT,YSHIFT)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   18/11/2010
!***********************************************************************
!
!brief    GENERATES HARMONIC CONSTANTS FOR BOUNDARY CONDITIONS WITH TIDES
!+
!
!history  C-T PHAM (LNHE)
!+        18/11/2010
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BOUNDARY_COLOUR|-->| AN INTEGER LINKED TO BOUNDARY POINTS
!|                |   | BY DEFAULT THE LAST LINE OF BOUNDARY CONDITIONS 
!|                |   | FILE, HENCE THE GLOBAL BOUNDARY NUMBER, BUT CAN 
!|                |   | BE CHANGED BY USER.
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| GEOSYST        |-->| TYPE OF GEOGRAPHIC SYSTEM (WGS84 LONG/LAT, UTM OR LAMBERT)
!| LAMBD0         |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| MAXFRO         |-->| MAXIMUM NUMBER OF BOUNDARIES
!| MESH           |-->| MESH STRUCTURE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NFO2           |-->| LOGICAL UNIT OF TIDE DATA BASE FILE
!| NBI2           |-->| LOGICAL UNIT OF TIDAL MODEL FILE
!| NRFO           |-->| LOGICAL UNIT OF HARMONIC CONSTANTS FILE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMZONE        |-->| NUMBER OF ZONE WHEN PLANE PROJECTION (UTM OR LAMBERT)
!| PHI0           |-->| LONGITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| TIDALTYPE      |-->| TYPE OF TIDE TO MODEL
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_BORD_TIDAL_BC => BORD_TIDAL_BC 
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPTFR,NFO2,NBI2,NRFO
      INTEGER, INTENT(IN)            :: KENT,KENTU,MAXFRO
      INTEGER, INTENT(IN)            :: GEOSYST,NUMZONE,TIDALTYPE
      INTEGER, INTENT(IN)            :: LIHBOR(NPTFR),LIUBOR(NPTFR)
      INTEGER, INTENT(IN)            :: NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)   :: XSHIFT,YSHIFT,LAMBD0,PHI0
      TYPE(BIEF_OBJ), INTENT(IN)     :: BOUNDARY_COLOUR
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,I,J,NNBTIDE,NELEM,ECKEN,NDUM,NBV1,NBV2,PARAM(10),NPOIN2
      INTEGER N1,N2,N3,IELEP
      INTEGER, PARAMETER :: NPOINJMJ=15350
      INTEGER CJMJ(NPOINJMJ,24)
      INTEGER, ALLOCATABLE :: IPOBO(:),NBTIDE(:),IKLESA(:,:)
      INTEGER, ALLOCATABLE :: FIRSTTIDE(:),LASTTIDE(:)
!
      DOUBLE PRECISION X1,X2,X3,Y1,Y2,Y3,PI,DTR,RTD,REARTH
      DOUBLE PRECISION XM,YM,DIS,A1,A2,A3,DISEL,ZP,XL,YL
      DOUBLE PRECISION XO,YO,ALPHA
      DOUBLE PRECISION AF(25),PF(25),COEF(24),COEFM(24),BETA
      DOUBLE PRECISION, ALLOCATABLE :: SURFAC(:)
      DOUBLE PRECISION, ALLOCATABLE :: XTIDE(:),YTIDE(:),ZTIDE(:)
      DOUBLE PRECISION, ALLOCATABLE :: LAMBDA(:),PHI(:)
      DOUBLE PRECISION, ALLOCATABLE :: XBTIDE(:),YBTIDE(:)
!
      REAL TPS
      REAL, ALLOCATABLE :: XTIDER(:),YTIDER(:),ZTIDER(:)
!
      LOGICAL DEJA_TBC
      DATA    DEJA_TBC /.FALSE./
!
      SAVE FIRSTTIDE,LASTTIDE
      SAVE DEJA_TBC
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA_TBC) THEN
!
      IF(NCSIZE.GT.1) THEN
        IF(LNG.EQ.1) THEN
          WRITE (LU,*) 'LA SUBROUTINE BORD_TIDAL_BC'
          WRITE (LU,*) 'N EST PAS ENCORE IMPLEMENTEE EN MODE PARALLELE'
          WRITE (LU,*) 'AGIR EN 2 ETAPES :'
          WRITE (LU,*) 
     &       '- MODE SEQUENTIEL POUR GENERER LES CONSTANTES HARMONIQUES'
          WRITE (LU,*)
     &       '- ENSUITE MODE SCALAIRE OU PARALLELE POUR LA SIMULATION'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE (LU,*) 'SUBROUTINE BORD_TIDAL_BC'
          WRITE (LU,*) 'HAS NOT BEEN IMPLEMENTED IN PARALLEL MODE YET'
          WRITE (LU,*) 'PLEASE USE 2 STEPS:'
          WRITE (LU,*)  '- SCALAR MODE TO GENERATE HARMONIC CONSTANT'
          WRITE (LU,*)  '- THEN SCALAR OR PARALLEL MODE FOR SIMULATION'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      PI = ACOS(-1.D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI
      REARTH = 6.37D6
!
!-----------------------------------------------------------------------
!
!  SPECIFIC VALUES FOR THE EXAMPLE OF A GEOGRAPHIC SYSTEM DEFINED BY
!  THE USER
!
      XO =  1.2D0
      YO = 50.D0
!  ANGLE BETWEEN EAST AXIS ---> X AXIS (TRIGONOMETRIC DEGREES)
      ALPHA = 40.D0
      ALPHA = ALPHA*DTR ! IN RADIANS
!
!-----------------------------------------------------------------------
!
!     OPEN (57,FILE='../coord_liquid_nodes_Mercator_JMJ.txt')
!
      ALLOCATE(XBTIDE(NPTFR))
      ALLOCATE(YBTIDE(NPTFR))
      ALLOCATE(NBTIDE(NPTFR))
!
      ALLOCATE(LAMBDA(NPTFR))
      ALLOCATE(PHI(NPTFR))
!
      ALLOCATE(FIRSTTIDE(MAXFRO))
      ALLOCATE(LASTTIDE(MAXFRO))
!
      I = 0
      J = 0
!
!  LOOP ON ALL BOUNDARY POINTS
!
      DO K=1,NPTFR
!
!  LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
        IF(LIHBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU) THEN
          I = I + 1
!
          NBTIDE(I) = K
          XBTIDE(I) = MESH%X%R(NBOR(K))
          YBTIDE(I) = MESH%Y%R(NBOR(K))
!
c$$$            IF(BOUNDARY_COLOUR%I(K).EQ.1) THEN
          IF(    K.EQ.1
     &       .OR.(LIHBOR(K-1).NE.KENT.AND.LIUBOR(K-1).NE.KENTU)) THEN
            J = J + 1
            FIRSTTIDE(J) = K
          ENDIF
!
          IF(    K.EQ.NPTFR
     &       .OR.(LIHBOR(K+1).NE.KENT.AND.LIUBOR(K+1).NE.KENTU)) THEN
            LASTTIDE(J)  = K
          ENDIF
        ENDIF
      ENDDO
!  NUMBER OF LIQUID BOUNDARY POINTS WITH TIDE
      NNBTIDE = I
!
      WRITE(NRFO,'(I4)') J
!
      DO I=1,J
        WRITE(NRFO,'(I8,I10)') FIRSTTIDE(I),LASTTIDE(I)
      ENDDO
!
!     READ JMJ DATABASE MESH
!
!     MESH FILE, READ UNTIL 10 PARAMETERS:
!
      REWIND(NBI2)
!
      READ(NBI2)
      READ(NBI2) NBV1,NBV2
      DO I=1,NBV1+NBV2
        READ(NBI2)
      ENDDO
!     READING OF 10 PARAMETERS AND DATE
      READ(NBI2) (PARAM(I),I=1,10)
      IF(PARAM(10).EQ.1) READ(NBI2) (PARAM(I),I=1,6)
!  5: 4 PARAMETERS
      READ(NBI2) NELEM,NPOIN2,ECKEN,NDUM
!  DYNAMIC ALLOCATIONS OF ARRAYS
      ALLOCATE(IKLESA(3,NELEM))
      ALLOCATE(IPOBO(NPOIN2))
!  X AND Y
      ALLOCATE(XTIDER(NPOIN2))
      ALLOCATE(YTIDER(NPOIN2))
      ALLOCATE(ZTIDER(NPOIN2))
      ALLOCATE(XTIDE(NPOIN2))
      ALLOCATE(YTIDE(NPOIN2))
      ALLOCATE(ZTIDE(NPOIN2))
      ALLOCATE(SURFAC(NELEM))
!  6: IKLE
      READ(NBI2)  ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
!  7: IPOBO
      READ(NBI2)  (IPOBO(I),I=1,NPOIN2)
!  8: X AND Y
      READ(NBI2)  (XTIDER(I),I=1,NPOIN2)
      READ(NBI2)  (YTIDER(I),I=1,NPOIN2)
      READ(NBI2)  TPS
      READ(NBI2)  (ZTIDER(I),I=1,NPOIN2)
!
      DO I=1,NPOIN2
        XTIDE(I) = DBLE(XTIDER(I))
        YTIDE(I) = DBLE(YTIDER(I))
        ZTIDE(I) = DBLE(ZTIDER(I))
      ENDDO
!
      DO J=1,NELEM
        X1 = XTIDE(IKLESA(1,J))
        X2 = XTIDE(IKLESA(2,J))
        X3 = XTIDE(IKLESA(3,J))
        Y1 = YTIDE(IKLESA(1,J))
        Y2 = YTIDE(IKLESA(2,J))
        Y3 = YTIDE(IKLESA(3,J))
        SURFAC(J) = 0.5D0 * ( (X2-X1)*(Y3-Y1) - (X3-X1)*(Y2-Y1) )
      ENDDO
!
      READ (NFO2,*)
      READ (NFO2,*)
      READ (NFO2,*)
      READ (NFO2,*)
      READ (NFO2,*)
      READ (NFO2,*)

      DO J=1,NPOINJMJ
        READ (NFO2,*)
        READ (NFO2,'(2(I7,I5,2(I6,I5)))') (CJMJ(J,I),I= 1,12)
        READ (NFO2,'(2(I7,I5,2(I6,I5)))') (CJMJ(J,I),I=13,24)
      ENDDO
!
!  WGS84 NORTHERN OR SOUTHERN UTM
      IF(GEOSYST.EQ.2.OR.GEOSYST.EQ.3) THEN
        CALL CONV_MERCATOR_TO_DEGDEC(NNBTIDE,
     &                               XBTIDE(1:NNBTIDE),
     &                               YBTIDE(1:NNBTIDE),
     &                               LAMBDA(1:NNBTIDE),PHI(1:NNBTIDE),
     &                               GEOSYST,NUMZONE,PHI0,LAMBD0)
!  NTF LAMBERT
      ELSEIF(GEOSYST.EQ.4) THEN
        CALL CONV_LAMBERT_TO_DEGDEC(NNBTIDE,
     &                              XBTIDE(1:NNBTIDE),YBTIDE(1:NNBTIDE),
     &                              LAMBDA(1:NNBTIDE),PHI(1:NNBTIDE),
     &                              NUMZONE)
      ELSEIF(GEOSYST.EQ.0) THEN
!  DEFINED BY THE USER
!  THIS IS AN EXAMPLE
        DO K=1,NNBTIDE
          XL = XBTIDE(K)
          YL = YBTIDE(K)
!  ROTATION WITH ALPHA ANGLE HERE
          XM=XL*COS(ALPHA)-YL*SIN(ALPHA)
          YL=XL*SIN(ALPHA)+YL*COS(ALPHA)
          XL=XM
!  TRANSLATION AND CONVERSION INTO REAL DEGREES
          LAMBDA(K) = XO+XL/REARTH/COS(YO*DTR)*RTD
          PHI(K)    = YO+YL/REARTH            *RTD
        ENDDO
      ENDIF
!
      DO K=1,NNBTIDE
        XL=LAMBDA(K)
        YL=PHI(K)
!
!  CONVERSION FROM REAL DEGREES TO MERCATOR TELEMAC
!
        XM=REARTH*DTR*XL
        YM=REARTH*(LOG(TAN((0.5D0*YL   +45.D0)*DTR))
     &            -LOG(TAN((0.5D0*48.D0+45.D0)*DTR)))
!
!  POSSIBLE TRANSLATION TO FIT THE COASTLINES OF THE TWO MODELS (JMJ AND LOCAL)!!!
!  DEFAULT: TO COMMENT THE FOLLOWING TWO LINES!
!
        XM=XM+XSHIFT
        YM=YM+YSHIFT
!
!       WRITE(57,'(F15.2,F16.2)') XM,YM
!
!  INTERPOLATION (FINITE ELEMENTS)
!  MAY BE CHANGED IN THE FUTURE WITH A MORE EFFICIENT INTERPOLATION ALGORITHM
!
        DIS=-9.D99
!
        J=1
        N1=IKLESA(1,J)
        N2=IKLESA(2,J)
        N3=IKLESA(3,J)
!
        A1 =         XM*YTIDE(N2) - XTIDE(N2)*YM + XTIDE(N2)*YTIDE(N3)
     &      - XTIDE(N3)*YTIDE(N2) + XTIDE(N3)*YM -        XM*YTIDE(N3)
        A2 =         XM*YTIDE(N3) - XTIDE(N3)*YM + XTIDE(N3)*YTIDE(N1)
     &      - XTIDE(N1)*YTIDE(N3) + XTIDE(N1)*YM -        XM*YTIDE(N1)
        A3 =         XM*YTIDE(N1) - XTIDE(N1)*YM + XTIDE(N1)*YTIDE(N2)
     &      - XTIDE(N2)*YTIDE(N1) + XTIDE(N2)*YM -        XM*YTIDE(N2)
!
        DO WHILE(     .NOT.(A1.GE.0.D0.AND.A2.GE.0.D0.AND.A3.GE.0.D0)
     &           .AND.(J.LE.NELEM))
          DISEL=MIN(A1,A2,A3)/SURFAC(J)
          IF(DISEL.GT.DIS) THEN
            DIS=DISEL
            IELEP=J
          ENDIF
!
          J=J+1
          N1=IKLESA(1,J)
          N2=IKLESA(2,J)
          N3=IKLESA(3,J)
!
          A1 =         XM*YTIDE(N2) - XTIDE(N2)*YM + XTIDE(N2)*YTIDE(N3)
     &        - XTIDE(N3)*YTIDE(N2) + XTIDE(N3)*YM -        XM*YTIDE(N3)
          A2 =         XM*YTIDE(N3) - XTIDE(N3)*YM + XTIDE(N3)*YTIDE(N1)
     &        - XTIDE(N1)*YTIDE(N3) + XTIDE(N1)*YM -        XM*YTIDE(N1)
          A3 =         XM*YTIDE(N1) - XTIDE(N1)*YM + XTIDE(N1)*YTIDE(N2)
     &        - XTIDE(N2)*YTIDE(N1) + XTIDE(N2)*YM -        XM*YTIDE(N2)
        ENDDO
!
        IF(J.EQ.NELEM+1) THEN
          WRITE(LU,*) 'ERROR WHEN INTERPOLATION, K=',
     &                BOUNDARY_COLOUR%I(NBTIDE(K)),' DIS=',DIS
!
          J=IELEP
          N1=IKLESA(1,J)
          N2=IKLESA(2,J)
          N3=IKLESA(3,J)
!
          A1 =         XM*YTIDE(N2) - XTIDE(N2)*YM + XTIDE(N2)*YTIDE(N3)
     &        - XTIDE(N3)*YTIDE(N2) + XTIDE(N3)*YM -        XM*YTIDE(N3)
          A2 =         XM*YTIDE(N3) - XTIDE(N3)*YM + XTIDE(N3)*YTIDE(N1)
     &        - XTIDE(N1)*YTIDE(N3) + XTIDE(N1)*YM -        XM*YTIDE(N1)
          A3 =         XM*YTIDE(N1) - XTIDE(N1)*YM + XTIDE(N1)*YTIDE(N2)
     &        - XTIDE(N2)*YTIDE(N1) + XTIDE(N2)*YM -        XM*YTIDE(N2)
        ENDIF
!
        ZP=0.5D0*(ZTIDE(N1)*A1+ZTIDE(N2)*A2+ZTIDE(N3)*A3)/SURFAC(J)
        WRITE (NRFO,'(I5,F12.2)') BOUNDARY_COLOUR%I(NBTIDE(K)),ZP
!
        DO I=1,24
          COEF(I)=0.0005D0*(CJMJ(N1,I)*A1+CJMJ(N2,I)*A2+CJMJ(N3,I)*A3)/
     &            SURFAC(J)
        ENDDO
!
!  BETA: ANGLE BETWEEN LAMBERT AND MERCATOR-JMJ REFERENCES (EAST OR X AXES, TRIGONOMETRIC)
!  THIS ANGLE (IN DEGREES) MAY BE CHANGED
!
        BETA = 0.D0  ! DEFAULT=0 DEGREES
        BETA = -4.D0
!
!  RECOMMENDED: REAL TIDE (RECOMMENDED METHODOLOGY) OR SCHEMATIC TIDES: 1<=TIDALTYPE<=6
!  TIDALTYPE = 1 TO 6: MAGNITUDE AND PHASE
!
        IF(TIDALTYPE.GE.1.AND.TIDALTYPE.LE.6) THEN
          DO I=1,12
            J=I+I-1
            AF(I)=SQRT(COEF(J)**2+COEF(J+1)**2)
            IF (AF(I).GT.1.D-9) PF(I)=ATAN2(COEF(J+1),COEF(J))*RTD
            PF(I) = MOD(PF(I) + BETA,360.D0)
            IF (PF(I).LT.0.D0) PF(I)=PF(I)+360.D0
          ENDDO
!
          WRITE (NRFO,'(3(F9.3,F7.1))') (AF(I),PF(I),I=1,3)
          WRITE (NRFO,'(3(F9.3,F7.1))') (AF(I),PF(I),I=4,6)
          WRITE (NRFO,'(3(F9.3,F7.1))') (AF(I),PF(I),I=7,9)
          WRITE (NRFO,'(3(F9.3,F7.1))') (AF(I),PF(I),I=10,12)
!
!  REAL TIDES, METHODOLOGY BEFORE 2010: TIDALTYPE = 7
!  TIDALTYPE = 7: PROJECTIONS ON X AND Y AXES
!
        ELSEIF(TIDALTYPE.GE.7) THEN
!  DEGREES TO RADIANS CONVERSION OF BETA
          BETA = BETA*DTR
!  COEFM WORKING COPY
          DO I=1,24
            COEFM(I)=COEF(I)
          ENDDO
!  X COMPONENTS: ODD; Y COMPONENTS: EVEN
          DO I=1,12
            COEF(2*I-1) = COEFM(2*I-1)*COS(BETA)-COEFM(2*I)*SIN(BETA)
            COEF(2*I)   = COEFM(2*I-1)*SIN(BETA)+COEFM(2*I)*COS(BETA)
          ENDDO
!
          WRITE (NRFO,'(6(F9.3))') (COEF(I),I=1,6)
          WRITE (NRFO,'(6(F9.3))') (COEF(I),I=7,12)
          WRITE (NRFO,'(6(F9.3))') (COEF(I),I=13,18)
          WRITE (NRFO,'(6(F9.3))') (COEF(I),I=19,24)
        ENDIF
      ENDDO
!
!     CLOSE (57)
!
      DEJA_TBC = .TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
