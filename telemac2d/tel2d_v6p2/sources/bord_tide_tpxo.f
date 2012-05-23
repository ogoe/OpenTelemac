!                    *************************
                     SUBROUTINE BORD_TIDE_TPXO
!                    *************************
!
     &(ZF,NBOR,LIHBOR,LIUBOR,NPOIN,NPTFR,TEMPS,NCOTE,NVITES,
     & NUMLIQ,KENT,KENTU,MESH,NOMIMP,TIDALTYPE,CTIDE,MSL,CTIDEV,
     & NODALCORR,NFOT,BOUNDARY_COLOUR,HBTIDE,UBTIDE,VBTIDE,NUMTIDE,
     & ICALHW,MARDAT,MARTIM,
     & T2D_FILES,T2DBB1,T2DBB2,X,Y,GEOSYST,NUMZONE,LAMBD0,PHI0)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   07/05/2012
!***********************************************************************
!
!brief    MODIFIES THE BOUNDARY CONDITIONS ARRAYS FOR TIDES
!+                WHEN THEY VARY IN TIME.
!+        BASED ON TPXO (FROM HRW)
!+
!
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+   Addition of the TPXO tidal model by calling CONDI_TPXO
!+      (the TPXO model being coded in DECLARATIONS_TPXO)
!
!history  C-T PHAM (LNHE)
!+        07/05/2012
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BOUNDARY_COLOUR|-->| AN INTEGER LINKED TO BOUNDARY POINTS
!|                |   | BY DEFAULT THE LAST LINE OF BOUNDARY CONDITIONS 
!|                |   | FILE, HENCE THE GLOBAL BOUNDARY NUMBER, BUT CAN 
!|                |   | BE CHANGED BY USER.
!| CTIDE          |-->| COEFFICIENT TO CALIBRATE THE TIDAL RANGE
!| CTIDEV         |-->| COEFFICIENT TO CALIBRATE THE VELOCITIES
!| GEOSYST        |-->| TYPE OF GEOGRAPHIC SYSTEM (WGS84 LONG/LAT, UTM OR LAMBERT)
!| HBTIDE         |<->| WATER DEPTH ON TIDAL BOUNDARY CONDITIONS
!| ICALHW         |<->| NUMBER THAT MAY BE CHOSEN BY THE USER
!|                |   | TO CALIBRATE HIGH WATER OR AUTOMATICALLY CHOSEN
!|                |   | IN CASE OF THE MODELLING OF A SCHEMATIC TIDE
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| LAMBD0         |-->| LATITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MARDAT         |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR,MINUTE,SECOND)
!| MESH           |<->| MESH STRUCTURE
!| MSL            |---| COEFFICIENT TO CALIBRATE THE SEA LEVEL
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| NFOT           |-->| LOGICAL UNIT OF HARMONIC CONSTANTS FILE
!| NODALCORR      |-->| OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION F
!| NOMIMP         |-->| NAME OF LIQUID BOUNDARIES FILE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| NUMTIDE        |<->| NUMBER OF THE TIDAL BOUNDARY
!|                |   | ASSOCIATED TO EACH POINT OF THE BOUNDARY
!| NUMZONE        |-->| NUMBER OF ZONE WHEN PLANE PROJECTION (UTM OR LAMBERT)
!| NVITES         |-->| NUMBER OF BOUNDARIES WITH VELOCITY PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| PHI0           |-->| LONGITUDE OF ORIGIN POINT (KEYWORD, IN DEGREES)
!| T2DBB1         |-->| ADDRESS OF DATA BASE 1 IN T2D_FILES
!| T2DBB2         |-->| ADDRESS OF DATA BASE 2 IN T2D_FILES
!| T2D_FILES      |-->| ARRAY OF FILES
!| TEMPS          |-->| TIME IN SECONDS
!| TIDALTYPE      |-->| TYPE OF TIDE TO MODEL
!| UBTIDE         |<->| VELOCITY ON TIDAL BOUNDARY CONDITIONS
!| VBTIDE         |<->| VELOCITY ON TIDAL BOUNDARY CONDITIONS
!| X              |-->| COORDINATES X OF THE NODES OF THE MESH
!| Y              |-->| COORDINATES Y OF THE NODES OF THE MESH
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : INTMICON
      USE TPXO
c$$$      USE INTERFACE_TELEMAC2D, EX_BORD_TIDE_TPXO => BORD_TIDE_TPXO
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,NCOTE,NVITES,NFOT
      INTEGER, INTENT(IN)             :: T2DBB1,T2DBB2,GEOSYST,NUMZONE
      INTEGER, INTENT(IN)             :: KENT,KENTU,NODALCORR
      INTEGER, INTENT(IN)             :: LIHBOR(NPTFR),LIUBOR(NPTFR)
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: TIDALTYPE,MARDAT(3),MARTIM(3)
      INTEGER, INTENT(INOUT)          :: ICALHW
      DOUBLE PRECISION, INTENT(IN)    :: TEMPS,CTIDE,MSL,CTIDEV
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0,PHI0
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      TYPE(BIEF_FILE), INTENT(IN)     :: T2D_FILES(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: BOUNDARY_COLOUR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: NUMTIDE,UBTIDE,VBTIDE,HBTIDE
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      CHARACTER(LEN=144), INTENT(IN)  :: NOMIMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,I,J,K,IPOIN,IERR,NC,N,M,NCON
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPTFRL,NPTFRL
      INTEGER, ALLOCATABLE :: CCIND(:)
      INTEGER, ALLOCATABLE :: MASKT(:,:),MASKU(:,:),MASKV(:,:)
      INTEGER, ALLOCATABLE :: TPXO_NFR(:)
!
      DOUBLE PRECISION PI,DTR,RTD
      DOUBLE PRECISION Z
      DOUBLE PRECISION STIME_MJD
      DOUBLE PRECISION XM,YM,XL,YL,XO,YO,ALPHA,RADIUS
      DOUBLE PRECISION TPXO_LAT_DUMMY
      DOUBLE PRECISION, ALLOCATABLE :: LAT(:),LON(:)
      DOUBLE PRECISION PH_LIM(2),TH_LIM(2)
      REAL PH_LIM_R(2),TH_LIM_R(2)
      COMPLEX(KIND(1.D0)), ALLOCATABLE :: TPXO_BOR(:,:,:)
      COMPLEX, ALLOCATABLE :: ZT(:,:,:)
      COMPLEX, ALLOCATABLE :: UT(:,:,:), VT(:,:,:)
      COMPLEX, ALLOCATABLE :: UV(:,:,:)
      COMPLEX(KIND(1.D0)), ALLOCATABLE :: ZCON(:)
      CHARACTER(LEN=4) C_ID_MOD(TPXO_NCMX)
      CHARACTER(LEN=4) C_ID(TPXO_NCMX)
!
!     N,M: SIZES OF THE GRID SUPPORTING THE TPXO MODEL
!     NC: NUMBER OF CONSTITUENTS AVAILABLE IN THE FILE
!     NCON: NUMBER OF CONSTITUENTS TURNED ON
!     MASKT,MASKU,MASKV MASKS TO FILTER VALID AND INVALID (U,V,H) VALUES
!     STIME_MJD: DAYS IN MODIFIED JULIAN DAYS
!     PH_LIM,TH_LIM: MIN AND MAX RANGES FOR PHASES AND PERIODES
!     ZT,UT,VT,UV,ZCON: PHASES AND PERIODS FOR U, V AND H
!     HERE DEFINED AS COMPLEX
!     C_ID_MOD INDICES OF AVAILABLE CONTITUENTS AMONGST THE ALL POSSIBLE
!     CCIND: INDICES OF AVAILABLE CONTITUENTS AMONGST THE ALL POSSIBLE
!
      LOGICAL DEJA
      DATA    DEJA /.FALSE./
!
      SAVE  
!
      INTRINSIC TAN,ATAN
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI      
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
!  RADIUS: RADIUS OF THE EARTH
      RADIUS = 6371000.D0
!
!-----------------------------------------------------------------------
!
!
!  TEST TO CHECK CORRECT VALUES FOR TIDALTYPE
!
      IF(.NOT.DEJA) THEN
        IF(TIDALTYPE.LT.1.OR.TIDALTYPE.GT.6) THEN
          IF(LNG.EQ.1) THEN
            WRITE (LU,*) 'MAUVAISE VALEUR POUR TIDALTYPE =',TIDALTYPE
            WRITE (LU,*) 'ELLE DOIT ETRE COMPRISE ENTRE 1 ET 6'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE (LU,*) 'UNEXPECTED VALUE FOR TIDALTYPE=',TIDALTYPE
            WRITE (LU,*) 'IT MUST BE CHOSEN BETWEEN 1 AND 6'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(.NOT.DEJA) THEN
!
!-----------------------------------------------------------------------
!
!     ALLBORD_TPXO REPLACED HERE
!
!brief    Prepare a level boundary filter to store the TPXO constituents
!+        at the boundary. In particular,
!+        count NPTFRL and ALLOCATE and set the filter TPXO_NFR
!
!note     Passing MESH, LIHBOR and LIUBOR as arguments allows
!+        this SUBROUTINE to be called from TELEMAC-2D or TELEMAC-3D
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+        Implementation and generalised for interfacing with
!+        TELEMAC-2D AND 3D
!
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!     NPTFRL: NUMBERS OF BOUNDARY POINTS WHERE TIDE IS PRESCRIBED
!
      ALLOCATE( TPXO_NFR(NPOIN) )
      DO K=1,NPOIN
         TPXO_NFR(K) = 0
      ENDDO
      NPTFRL = 0
      DO K = 1,NPTFR
         IF( LIHBOR(K).EQ.KENT.OR.LIUBOR(K).EQ.KENTU ) THEN
            NPTFRL = NPTFRL + 1
            TPXO_NFR(NBOR(K)) = NPTFRL
         ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(T2D_FILES(T2DBB1)%NAME(1:1).EQ.' ' .OR.
     &   T2D_FILES(T2DBB2)%NAME(1:1).EQ.' ') THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'FICHIERS TPXO NON DISPONIBLES'
        IF(LNG.EQ.2) WRITE(LU,*) 'TPXO FILES NOT AVAILABLE'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,*) 'INITIALISATION BASEE SUR TPXO :'
      IF(LNG.EQ.2) WRITE(LU,*) 'INITIALISATION BASED ON TPXO:'
!
!-----------------------------------------------------------------------
!
!     READ AVAILABLE DIMENSIONS
!
      REWIND(T2D_FILES(T2DBB1)%LU)
      READ(T2D_FILES(T2DBB1)%LU) N,M,NC,TH_LIM_R,PH_LIM_R,C_ID_MOD(1:NC)
!
!-----------------------------------------------------------------------
!
!     COPY OF TH_LIM AND PH_LIM IN DOUBLE PRECISION
!
      TH_LIM(1) = TH_LIM_R(1)
      TH_LIM(2) = TH_LIM_R(2)
      PH_LIM(1) = PH_LIM_R(1)
      PH_LIM(2) = PH_LIM_R(2)
!
!-----------------------------------------------------------------------
!
!     GET ALL AVAILABLE CONSTITUENTS AND SET THEIR INDICES
!
      NCON = NC
      DO IC = 1,NC
        C_ID(IC) = C_ID_MOD(IC)
      ENDDO
      ALLOCATE( CCIND(NCON) )
      CALL DEF_CID( NCON,C_ID,CCIND )
!
!-----------------------------------------------------------------------
!
!     PREPARE STORAGE ON LEVEL BOUNDARIES
!
      ALLOCATE( TPXO_BOR(3,NPTFRL,NCON) )
      DO K=1,NCON
         DO J=1,NPTFRL
            DO I=1,3
               TPXO_BOR(I,J,K) = CMPLX(0.D0,0.D0,KIND(1.D0))
            ENDDO
         ENDDO
      ENDDO
!
      TPXO_LAT_DUMMY = 0.D0
!
!-----------------------------------------------------------------------
!
      STIME_MJD = DATE_MJD( MARDAT(2),MARDAT(3),MARDAT(1) ) +
     &            MARTIM(1)/24.D0+MARTIM(2)/1440.D0+MARTIM(3)/86400.D0
!
!-----------------------------------------------------------------------
!
      ALLOCATE( LON(NPOIN) )
      ALLOCATE( LAT(NPOIN) )
!
!  WGS84 NORTHERN OR SOUTHERN UTM, OR MERCATOR FOR TELEMAC
!  WARNING!!! IN TELEMAC DICO, LAMBD0 IS LATITUDE AND PHI0 IS LONGITUDE
!  LAMBD0 AND PHI0 ARE NOT USED FOR GEOSYST = 2 OR 3
      IF(GEOSYST.EQ.2.OR.GEOSYST.EQ.3.OR.GEOSYST.EQ.5) THEN
        CALL CONV_MERCATOR_TO_DEGDEC(NPOIN,X(1:NPOIN),Y(1:NPOIN),
     &                               LON(1:NPOIN),LAT(1:NPOIN),
     &                               GEOSYST,NUMZONE,PHI0,LAMBD0)
!  NTF LAMBERT
      ELSEIF(GEOSYST.EQ.4) THEN
        CALL CONV_LAMBERT_TO_DEGDEC(NPOIN,X(1:NPOIN),Y(1:NPOIN),
     &                              LON(1:NPOIN),LAT(1:NPOIN),
     &                              NUMZONE)
      ELSEIF(GEOSYST.EQ.0) THEN
!  DEFINED BY THE USER
!  THIS IS AN EXAMPLE
        DO K=1,NPOIN
          XL = X(K)
          YL = Y(K)
!  ROTATION WITH ALPHA ANGLE HERE
          XM=XL*COS(ALPHA)-YL*SIN(ALPHA)
          YL=XL*SIN(ALPHA)+YL*COS(ALPHA)
          XL=XM
!  TRANSLATION AND CONVERSION INTO REAL DEGREES
          LON(K) = XO+XL/RADIUS/COS(YO*DTR)*RTD
          LAT(K) = YO+YL/RADIUS            *RTD
        ENDDO
      ENDIF
!
      DO I = 1,NPOIN
         IF( LON(I).GT.PH_LIM(2) ) LON(I) = LON(I) - 360.D0
         IF( LON(I).LT.PH_LIM(1) ) LON(I) = LON(I) + 360.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE H-FILE
!
      IF(LNG.EQ.1) WRITE(LU,*) ' - OBTENTION DES NIVEAUX'
      IF(LNG.EQ.2) WRITE(LU,*) ' - ACQUIRING LEVELS'
!
      ALLOCATE( ZT(NCON,N,M), MASKT(N,M) )
      DO J=1,M
         DO I=1,N
            MASKT(I,J) = 0
         ENDDO
      ENDDO
!
      DO IC = 1,NCON
         REWIND(T2D_FILES(T2DBB1)%LU)
         READ(T2D_FILES(T2DBB1)%LU)  ! HEADER LINE
         DO K = 1,IC-1
            READ(T2D_FILES(T2DBB1)%LU)
         ENDDO
         READ(T2D_FILES(T2DBB1)%LU) ( ( ZT(IC,I,J), I=1,N ), J=1,M )
         WHERE( ZT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKT = 1
      ENDDO
!
!     INTERPOLATE TPXO IN SPACE
!
      IF(LNG.EQ.1) WRITE(LU,*) ' - INTERPOLATION DES NIVEAUX'
      IF(LNG.EQ.2) WRITE(LU,*) ' - INTERPOLATING LEVELS'
!
      ALLOCATE( ZCON(NCON) )
      DO IPOIN = 1,NPOIN
!
         CALL INTERPT( ZT,NCON,N,M,MASKT,TH_LIM,PH_LIM,
     &                 LAT(IPOIN),LON(IPOIN),ZCON,IERR,'z' )
         IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
               TPXO_BOR(1,TPXO_NFR(IPOIN),K) = ZCON(K)
            ENDDO
         ENDIF
!
      ENDDO
      DEALLOCATE( ZCON,ZT,MASKT )
!
!-----------------------------------------------------------------------
!
!     GET DATA FROM THE U-FILE
!
      IF(LNG.EQ.1) WRITE(LU,*) ' - OBTENTION DES VITESSES'
      IF(LNG.EQ.2) WRITE(LU,*) ' - ACQUIRING VELOCITIES'
!
      ALLOCATE( UT(NCON,N,M),VT(NCON,N,M),MASKU(N,M),MASKV(N,M) )
      DO J=1,M
         DO I=1,N
            MASKU(I,J) = 0
            MASKV(I,J) = 0
         ENDDO
      ENDDO
!
      ALLOCATE( UV(2,N,M) )
      DO IC = 1,NCON
         REWIND(T2D_FILES(T2DBB2)%LU)
         READ(T2D_FILES(T2DBB2)%LU)  ! HEADER LINE
         DO K = 1,IC-1
            READ(T2D_FILES(T2DBB2)%LU)
         ENDDO
         READ(T2D_FILES(T2DBB2)%LU) UV
         DO J=1,M
            DO I=1,N
               UT(IC,I,J) = UV(1,I,J)
               VT(IC,I,J) = UV(2,I,J)
            ENDDO
         ENDDO
         WHERE( UT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKU = 1
         WHERE( VT(IC,:,:).NE.CMPLX(0.D0,0.D0) ) MASKV = 1
      ENDDO
      DEALLOCATE( UV )
!
!     INTERPOLATE TPXO IN SPACE
!
      IF(LNG.EQ.1) WRITE(LU,*) ' - INTERPOLATION DES VITESSES'
      IF(LNG.EQ.2) WRITE(LU,*) ' - INTERPOLATING VELOCITIES'
!
      ALLOCATE( ZCON(NCON) )
      DO IPOIN = 1,NPOIN
!
         CALL INTERPT(UT,NCON,N,M,MASKU,TH_LIM,PH_LIM,
     &                LAT(IPOIN),LON(IPOIN),ZCON,IERR,'u')
         IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
!     VELOCITY READ IN CM/S!
               TPXO_BOR(2,TPXO_NFR(IPOIN),K) = ZCON(K)*0.01D0
            ENDDO
          ENDIF
!
         CALL INTERPT(VT,NCON,N,M,MASKV,TH_LIM,PH_LIM,
     &                LAT(IPOIN),LON(IPOIN),ZCON,IERR,'v')
         IF( TPXO_NFR(IPOIN).NE.0 ) THEN
            DO K=1,NCON
!     VELOCITY READ IN CM/S!
               TPXO_BOR(3,TPXO_NFR(IPOIN),K) = ZCON(K)*0.01D0
            ENDDO
         ENDIF
!
      ENDDO
      DEALLOCATE( UT,VT,ZCON,MASKU,MASKV )
      DEALLOCATE( LON,LAT )
!
      IF(LNG.EQ.1) WRITE(LU,*) 'FIN DE L''INITIALISATION TPXO'
      IF(LNG.EQ.2) WRITE(LU,*) 'END OF TPXO INITIALISATION'
!
      DEJA = .TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!
!
!  LOOP ON ALL BOUNDARY POINTS
!
      DO K=1,NPTFR
!
        IPTFR=BOUNDARY_COLOUR%I(K)
!
!     LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
!
        IF(LIHBOR(K).EQ.KENT) THEN
!         BEGINNING OF PRESCRIBED DEPTHS
!
          IF(NCOTE.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
!
!  TYPE OF TIDE TO MODEL
!  1: REAL TIDE
!  2: ASTRONOMICAL TIDE      (COEF. NEARLY 120)
!  3: MEAN SPRING TIDE       (COEF. NEARLY 95)
!  4: MEAN TIDE              (COEF. NEARLY 70)
!  5: MEAN NEAP TIDE         (COEF. NEARLY 45)
!  6: ASTRONOMICAL NEAP TIDE (COEF. NEARLY 20)
!
            IF(TIDALTYPE.EQ.1) THEN
              Z = CTIDE*TPXO_PTIDE(1,NBOR(K),TPXO_NFR,TPXO_BOR,C_ID,
     &                             NCON,CCIND,TPXO_LAT_DUMMY,
     &                             STIME_MJD+TEMPS/86400.D0,INTMICON)
     &          + MSL
              HBTIDE%R(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
c$$$            ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
            ENDIF
!         ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
          ENDIF
        ENDIF
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
c$$$      IF(LIUBOR(K).EQ.KENTU) THEN
      IF(LIUBOR(K).EQ.KENTU.AND.
     &  (NVITES.NE.0.OR.NOMIMP(1:1).NE.' ')) THEN
!
!       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
!
!         BEGINNING OF PRESCRIBED VELOCITIES
!
!  TYPE OF TIDE TO MODEL
!  1: REAL TIDE
!  2: ASTRONOMICAL TIDE      (COEF. NEARLY 120)
!  3: MEAN SPRING TIDE       (COEF. NEARLY 95)
!  4: MEAN TIDE              (COEF. NEARLY 70)
!  5: MEAN NEAP TIDE         (COEF. NEARLY 45)
!  6: ASTRONOMICAL NEAP TIDE (COEF. NEARLY 20)
!
          IF(TIDALTYPE.EQ.1) THEN
c$$$           IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
            UBTIDE%R(K) =
     &              CTIDEV*TPXO_PTIDE(2,NBOR(K),TPXO_NFR,TPXO_BOR,C_ID,
     &                                NCON,CCIND,TPXO_LAT_DUMMY,
     &                                STIME_MJD+TEMPS/86400.D0,INTMICON)
            VBTIDE%R(K) =
     &              CTIDEV*TPXO_PTIDE(3,NBOR(K),TPXO_NFR,TPXO_BOR,C_ID,
     &                                NCON,CCIND,TPXO_LAT_DUMMY,
     &                                STIME_MJD+TEMPS/86400.D0,INTMICON)
c$$$           ENDIF
c$$$          ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
            ENDIF
!
        ENDIF
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
