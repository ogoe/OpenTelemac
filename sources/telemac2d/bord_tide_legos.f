!                    **************************
                     SUBROUTINE BORD_TIDE_LEGOS
!                    **************************
!
     &(ZF,NBOR,LIHBOR,LIUBOR,NPOIN,NPTFR,TEMPS,DT,NCOTE,NVITES,
     & NUMLIQ,KENT,KENTU,NOMIMP,TIDALTYPE,CTIDE,MSL,
     & CTIDEV,NODALCORR,NFOT,
     & BOUNDARY_COLOUR,HBTIDE,UBTIDE,VBTIDE,NUMTIDE,ICALHW,
     & MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   12/01/2012
!***********************************************************************
!
!brief    MODIFIES THE BOUNDARY CONDITIONS ARRAYS FOR TIDES
!+                WHEN THEY VARY IN TIME.
!+
!
!history  C-T PHAM (LNHE)
!+        12/01/2012
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
!| DT             |-->| TIME STEP
!| HBTIDE         |<->| WATER DEPTH ON TIDAL BOUNDARY CONDITIONS
!| ICALHW         |<->| NUMBER THAT MAY BE CHOSEN BY THE USER
!|                |   | TO CALIBRATE HIGH WATER OR AUTOMATICALLY CHOSEN
!|                |   | IN CASE OF THE MODELLING OF A SCHEMATIC TIDE
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| MARDAT         |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR,MINUTE,SECOND)
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
!| NVITES         |-->| NUMBER OF BOUNDARIES WITH VELOCITY PRESCRIBED
!|                |   | AS GIVEN IN THE PARAMETER FILE
!| TEMPS          |-->| TIME IN SECONDS
!| TIDALTYPE      |-->| TYPE OF TIDE TO MODEL
!| UBTIDE         |<->| VELOCITY ON TIDAL BOUNDARY CONDITIONS
!| VBTIDE         |<->| VELOCITY ON TIDAL BOUNDARY CONDITIONS
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD_TIDE_LEGOS => BORD_TIDE_LEGOS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,NCOTE,NVITES,NFOT
      INTEGER, INTENT(IN)             :: KENT,KENTU,NODALCORR
      INTEGER, INTENT(IN)             :: LIHBOR(NPTFR),LIUBOR(NPTFR)
      INTEGER, INTENT(IN)             :: NUMLIQ(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: TIDALTYPE,MARDAT(3),MARTIM(3)
      INTEGER, INTENT(INOUT)          :: ICALHW
      DOUBLE PRECISION, INTENT(IN)    :: TEMPS,CTIDE,MSL,CTIDEV,DT
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
      TYPE(BIEF_OBJ), INTENT(IN)      :: BOUNDARY_COLOUR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: NUMTIDE,UBTIDE,VBTIDE,HBTIDE
      CHARACTER(LEN=144), INTENT(IN)  :: NOMIMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IERR,I,J
!
!-----------------------------------------------------------------------
!
      INTEGER, PARAMETER   :: LEGOS_NCMX = 47
      INTEGER IPTFR,IPTFRL,NPTFRL,NTIDE,NWAVES
      INTEGER, ALLOCATABLE :: FIRSTTIDE(:),LASTTIDE(:),SHIFTTIDE(:)
      INTEGER, ALLOCATABLE :: INDW(:)
!
      DOUBLE PRECISION PI,DTR
      DOUBLE PRECISION SUMH,SUMU,SUMV
      DOUBLE PRECISION, ALLOCATABLE :: AH(:,:),PH(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AU(:,:),PU(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AV(:,:),PV(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: LON(:,:),LAT(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: UPV(:),FF(:),OMEGA(:),PHCALHW(:)
!
      CHARACTER(LEN=4) TEXT
      CHARACTER(LEN=4), ALLOCATABLE :: NAMEWAVE(:)
!
      CHARACTER(LEN=4) LEGOS_CONSTID(LEGOS_NCMX)
      DATA LEGOS_CONSTID /'2MK6','2MN6','2MS6','2N2 ','2Q1 ',
     &                    '2SM2','2SM6','E2  ','J1  ','K1  ',
     &                    'K2  ','KJ2 ','L2  ','La2 ','M1  ',
     &                    'M2  ','M4  ','M6  ','Mf  ','MK3 ',
     &                    'MK4 ','MKS2','Mm  ','MN4 ','MO3 ',
     &                    'MP1 ','MS4 ','MSK6','MSN2','MSN6',
     &                    'MSqm','Mtm ','Mu2 ','N2  ','Nu2 ',
     &                    'O1  ','P1  ','Q1  ','R2  ','Ro1 ',
     &                    'S2  ','S4  ','Sig1','SK4 ','SN4 ',
     &                    'T2  ','Z0  '/
!
      LOGICAL DEJA
      DATA    DEJA /.FALSE./
!
      SAVE  
!
!-----------------------------------------------------------------------
!
      PI  = ATAN(1.D0)*4.D0
      DTR = PI/180.D0
!
!  TEST TO CHECK CORRECT VALUES FOR TIDALTYPE
!
      IF(.NOT.DEJA) THEN
c$$$        IF(TIDALTYPE.LT.1.OR.TIDALTYPE.GT.6) THEN
        IF(TIDALTYPE.NE.1) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'MAUVAISE VALEUR POUR TIDALTYPE =',TIDALTYPE
            WRITE(LU,*) 'ELLE DOIT ETRE EGALE A 1 ACTUELLEMENT'
            WRITE(LU,*) 'AVEC LA BASE DE DONNEES DE MAREE LEGOS-NEA'
c$$$            WRITE(LU,*) 'ELLE DOIT ETRE COMPRISE ENTRE 1 ET 6'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'UNEXPECTED VALUE FOR TIDALTYPE=',TIDALTYPE
            WRITE(LU,*) 'IT MUST BE CHOSEN EQUAL TO 1 CURRENTLY'
            WRITE(LU,*) 'WITH LEGOS-NEA TIDAL DATA BASE'
c$$$            WRITE(LU,*) 'IT MUST BE CHOSEN BETWEEN 1 AND 6'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!  MAGNITUDES AND PHASES ARE READ IN TIDAL FILE
!  TIDAL FILE IS OBTAINED BY APPLYING BORD_TIDAL_BC SUBROUTINE
!  WHICH INTERPOLATES JMJ MODEL RESULTS ON THE CURRENT MESH
!
!  NTIDE:  NUMBER OF THE TIDAL BOUNDARIES
!  NPTFRL: NUMBERS OF BOUNDARY POINTS WHERE TIDE IS PRESCRIBED
!
      IF(.NOT.DEJA) THEN
!
        REWIND NFOT
!
        READ(NFOT,*,END=2) NTIDE
        DO K=1,NTIDE
          READ(NFOT,*,END=2)
        ENDDO
!
        READ(NFOT,*,END=2) NPTFRL,NWAVES,TEXT
!
        IF(NWAVES.GT.LEGOS_NCMX) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'NOMBRE D ONDES PRESENTES DANS LE FICHIER DE'
            WRITE(LU,*) 'CONSTANTES HARMONIQUES SUPERIEUR A',LEGOS_NCMX
            WRITE(LU,*) 'CERTAINES ONDES NE SONT PAS PREVUES.'
            WRITE(LU,*) 'LE FICHIER EST A AJUSTER'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'NUMBER OF WAVES IN THE HARMONIC CONSTITUENTS'
            WRITE(LU,*) 'FILE GREATER THAN',LEGOS_NCMX
            WRITE(LU,*) 'SOME WAVES ARE NOT EXPECTED. THE FILE IS TO'
            WRITE(LU,*) 'BE ADJUSTED'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
2     CONTINUE
!
      IF(.NOT.DEJA) THEN
        ALLOCATE(FIRSTTIDE(NTIDE),STAT=IERR)
        ALLOCATE(LASTTIDE(NTIDE), STAT=IERR)
        ALLOCATE(SHIFTTIDE(NTIDE),STAT=IERR)
!
        ALLOCATE(AH(NPTFRL,NWAVES),STAT=IERR)
        ALLOCATE(PH(NPTFRL,NWAVES),STAT=IERR)
        ALLOCATE(AU(NPTFRL,NWAVES),STAT=IERR)
        ALLOCATE(PU(NPTFRL,NWAVES),STAT=IERR)
        ALLOCATE(AV(NPTFRL,NWAVES),STAT=IERR)
        ALLOCATE(PV(NPTFRL,NWAVES),STAT=IERR)
        ALLOCATE(LON(NPTFRL,NWAVES),STAT=IERR)
        ALLOCATE(LAT(NPTFRL,NWAVES),STAT=IERR)
        ALLOCATE(NAMEWAVE(NWAVES),STAT=IERR)
        ALLOCATE(INDW(NWAVES),STAT=IERR)
        IF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
          ALLOCATE(PHCALHW(NWAVES),STAT=IERR)
        ENDIF
        ALLOCATE(UPV(LEGOS_NCMX),STAT=IERR)
        ALLOCATE(FF(LEGOS_NCMX),STAT=IERR)
        ALLOCATE(OMEGA(LEGOS_NCMX),STAT=IERR)
      ENDIF
!
!  COMPUTE THE FIRST AND LAST INDICES OF THE OPEN LIQUID BOUNDARY WITH TIDE TO PRESCRIBE
!
      IF(.NOT.DEJA) THEN
        REWIND NFOT
!
        READ(NFOT,*)
        DO I=1,NTIDE
          READ(NFOT,*,END=4) FIRSTTIDE(I),LASTTIDE(I)
        ENDDO
4       CONTINUE
!
!  SHIFTS WHEN CHANGING TIDAL BOUNDARY
!
        SHIFTTIDE(1) = 0
!
        DO I=2,NTIDE
          SHIFTTIDE(I) = LASTTIDE(I-1) - FIRSTTIDE(I-1) + 1 
     &                 + SHIFTTIDE(I-1)
        ENDDO
!
        READ(NFOT,*) NPTFRL,NWAVES,TEXT
!
!  READING OF TIDAL DATA AT THE FIRST TIME STEP
!
        DO I=1,NWAVES
          READ(NFOT,*)NAMEWAVE(I)
          DO IPTFRL = 1,NPTFRL
            READ(NFOT,*) LON(IPTFRL,I),LAT(IPTFRL,I),
     &                   AH(IPTFRL,I),PH(IPTFRL,I),
     &                   AU(IPTFRL,I),PU(IPTFRL,I),
     &                   AV(IPTFRL,I),PV(IPTFRL,I)
          ENDDO
        ENDDO
!
        DO K = 1,NWAVES
          INDW(K) = 0
          DO I = 1,LEGOS_NCMX
            IF(NAMEWAVE(K).EQ.LEGOS_CONSTID(I)) THEN
               INDW(K) = I
               EXIT
            ENDIF
          ENDDO
          IF(INDW(K).EQ.0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'LEGOS : ATTENTION :' //
     &         'COMPOSANTE ID ',NAMEWAVE(K),' N''EST PAS PERMISE'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'LEGOS : WARNING:' //
     &         'CONSTITUENT ID ',NAMEWAVE(K),' IS NOT ALLOWED'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!
!       POTENTIAL SPECIFIC TREATMENTS
!
        IF(TIDALTYPE.EQ.1) THEN
!
!         DEGREES TO RADIANS CONVERSIONS
          DO I=1,NWAVES
            DO IPTFRL = 1,NPTFRL
              PH(IPTFRL,I) = PH(IPTFRL,I)*DTR
              PU(IPTFRL,I) = PU(IPTFRL,I)*DTR
              PV(IPTFRL,I) = PV(IPTFRL,I)*DTR
            ENDDO
          ENDDO
!
        ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
!
!         ARBITRARY CHOICE
          IF(ICALHW.EQ.0) ICALHW = NPTFRL/2
!
!  CALIBRATION WITH RESPECT TO HIGH WATER!!!
!  PHASES FOR HEIGHTS ARE READ IN JMJ TIDAL FILE
!  EXCEPT M4: 2*PHM2 MOD 360 IS APPLIED
! --------------------------------------------------
!
          DO I=1,NWAVES
            PHCALHW(I) = PH(ICALHW,I)
            DO IPTFRL = 1,NPTFRL
              PH(IPTFRL,I) = (PH(IPTFRL,I) - PHCALHW(I))*DTR
              PU(IPTFRL,I) = (PU(IPTFRL,I) - PHCALHW(I))*DTR
              PV(IPTFRL,I) = (PV(IPTFRL,I) - PHCALHW(I))*DTR
            ENDDO
          ENDDO
!
        ENDIF
!
!  NUMBER OF THE TIDAL BOUNDARY ASSOCIATED TO EACH POINT OF THE BOUNDARY
!  REMAINS 0 IF POINT IS NOT ON AN OPEN BOUNDARY WITH TIDE
!
        DO K=1,NPTFR
          NUMTIDE%I(K) = 0
          IPTFR=BOUNDARY_COLOUR%I(K)
          DO I=1,NTIDE
            IF(IPTFR.GE.FIRSTTIDE(I).AND.IPTFR.LE.LASTTIDE(I)) THEN
              NUMTIDE%I(K) = I
            ENDIF
          ENDDO
        ENDDO
!
!  FOR THE SIMULATION OF REAL TIDES, NODAL FACTOR CORRECTIONS ARE COMPUTED
!  WITH SCHUREMAN FORMULAE
!  SCHUREMAN P. (1971). MANUAL OF HARMONIC ANALYSIS AND PREDICTION OF TIDES
!
        IF(TIDALTYPE.EQ.1) THEN
!
          CALL NODALUPV_SCHUREMAN(UPV,OMEGA,MARDAT,MARTIM)
!  TEMPS-DT RATHER THAN TEMPS BECAUSE THE FIRST CALL TO BORD_TIDE_LEGOS
!  IS AT THE FIRST TIME STEP
          CALL NODALF_SCHUREMAN(FF,NODALCORR,TEMPS-DT,DEJA,
     &                          MARDAT,MARTIM)
!
        ENDIF
!
        DEJA = .TRUE.
!
      ENDIF
!
      IF(TIDALTYPE.EQ.1.AND.NODALCORR.EQ.0) THEN
        CALL NODALF_SCHUREMAN(FF,NODALCORR,TEMPS,DEJA,
     &                        MARDAT,MARTIM)
      ENDIF
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
          IF(NUMTIDE%I(K).GT.0) THEN
            IPTFRL=IPTFR-FIRSTTIDE(NUMTIDE%I(K))+1
     &                  +SHIFTTIDE(NUMTIDE%I(K))
!
!  TYPE OF TIDE TO MODEL
!  1: REAL TIDE (RECOMMENDED METHODOLOGY)
!  2: ASTRONOMICAL TIDE      (COEF. NEARLY 120)
!  3: MEAN SPRING TIDE       (COEF. NEARLY 95)
!  4: MEAN TIDE              (COEF. NEARLY 70)
!  5: MEAN NEAP TIDE         (COEF. NEARLY 45)
!  6: ASTRONOMICAL NEAP TIDE (COEF. NEARLY 20)
!
            IF(TIDALTYPE.EQ.1) THEN
              SUMH = 0.D0
              DO I=1,NWAVES
                J = INDW(I)
                IF(J.NE.0) THEN
                  SUMH = SUMH + FF(J)*AH(IPTFRL,I)
     &                         *COS( TEMPS*OMEGA(J)-PH(IPTFRL,I)+UPV(J))
                ENDIF
              ENDDO
c$$$            ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
            ELSEIF(TIDALTYPE.EQ.2) THEN
              SUMH = 0.D0
              DO I=1,NWAVES
                J = INDW(I)
                IF(J.NE.0) THEN
                  SUMH = SUMH + AH(IPTFRL,I)
     &                         *COS(TEMPS*OMEGA(J)-PH(IPTFRL,I))
                ENDIF
              ENDDO
            ENDIF
!
            IF(TIDALTYPE.EQ.1.OR.TIDALTYPE.EQ.2) THEN
              HBTIDE%R(K) = -ZF(NBOR(K)) + CTIDE*SUMH + MSL
c$$$            ELSEIF(TIDALTYPE.EQ.2.AND.TIDALTYPE.LE.6) THEN
            ENDIF
!
          ENDIF
!         ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
        ENDIF
!
!  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
!                    PROVIDED BY THE USER.
!
      IF(LIUBOR(K).EQ.KENTU) THEN
!
!       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
!
!         BEGINNING OF PRESCRIBED VELOCITIES
!
          IF(NUMTIDE%I(K).GT.0) THEN
            IPTFRL=IPTFR-FIRSTTIDE(NUMTIDE%I(K))+1
     &                  +SHIFTTIDE(NUMTIDE%I(K))
!
!  TYPE OF TIDE TO MODEL
!  1: REAL TIDE (RECOMMENDED METHODOLOGY)
!  2: ASTRONOMICAL TIDE      (COEF. NEARLY 120)
!  3: MEAN SPRING TIDE       (COEF. NEARLY 95)
!  4: MEAN TIDE              (COEF. NEARLY 70)
!  5: MEAN NEAP TIDE         (COEF. NEARLY 45)
!  6: ASTRONOMICAL NEAP TIDE (COEF. NEARLY 20)
!
            IF(TIDALTYPE.EQ.1) THEN
              SUMU = 0.D0
              SUMV = 0.D0
              DO I=1,NWAVES
                J = INDW(I)
                IF(J.NE.0) THEN
                  SUMU = SUMU + FF(J)*AU(IPTFRL,I)
     &                         *COS( TEMPS*OMEGA(J)-PU(IPTFRL,I)+UPV(J))
                  SUMV = SUMV + FF(J)*AV(IPTFRL,I)
     &                         *COS( TEMPS*OMEGA(J)-PV(IPTFRL,I)+UPV(J))
                ENDIF
              ENDDO
c$$$            ELSEIF(TIDALTYPE.GE.2.AND.TIDALTYPE.LE.6) THEN
            ELSEIF(TIDALTYPE.EQ.2) THEN
              SUMU = 0.D0
              SUMV = 0.D0
              DO I=1,NWAVES
                J = INDW(I)
                IF(J.NE.0) THEN
                  SUMU = SUMU + AU(IPTFRL,I)
     &                         *COS(TEMPS*OMEGA(J)-PU(IPTFRL,I))
                  SUMV = SUMV + AV(IPTFRL,I)
     &                         *COS(TEMPS*OMEGA(J)-PV(IPTFRL,I))
                ENDIF
              ENDDO
            ENDIF
!
            IF(TIDALTYPE.EQ.1.OR.TIDALTYPE.EQ.2) THEN
              UBTIDE%R(K) = CTIDEV*SUMU
              VBTIDE%R(K) = CTIDEV*SUMV
c$$$            ELSEIF(TIDALTYPE.EQ.2.AND.TIDALTYPE.LE.6) THEN
            ENDIF
!
          ENDIF
        ENDIF
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
