!                    ********************
                     SUBROUTINE PROSOU_FV
!                    ********************
!
     &(FU,FV,SMH,    UN,VN,HN,GRAV,NORD,
     & FAIR,WINDX,WINDY,VENT,HWIND,CORIOL,FCOR,
     & SPHERI,YASMH,YASMO,COSLAT,SINLAT,AT,LT,DT,
     & NREJET,NREJEU,DSCE,ISCE,T1,MESH,MSK,MASKEL,
     & MAREE,MARDAT,MARTIM,PHI0,OPTSOU,
     & COUROU,NPTH,VARCL,NVARCL,VARCLA,UNSV2D,
     & FXWAVE,FYWAVE,RAIN,RAIN_MMPD,PLUIE,
     & T2D_FILES,T2DBI1,BANDEC,OPTBAN,
     & NSIPH,ENTSIP,SORSIP,DSIP,USIP,VSIP,
     & NBUSE,ENTBUS,SORBUS,DBUS,UBUS,VBUS,
     & TYPSEUIL,NWEIRS,N_NGHB_W_NODES,NDGA1,NDGB1,NBOR)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    PREPARES THE SOURCE TERMS IN THE CONTINUITY EQUATION
!+                AND IN THE DYNAMIC EQUATIONS FOR FINITE VOLUMES.
!+        ARE TAKEN INTO ACCOUNT :
!+
!+              - SECONDARY CURRENT
!+
!+              - WIND
!+
!+              - CORIOLIS FORCE (DONE IN SOURCE_MOMENT)
!+
!+              - TIDAL FORCE
!+
!+              - SOURCES AND SINKS
!+
!+              - WEIRS (IF TYPSEUIL=2)
!code
!+    RESPECTIVE TERMS ARE:
!+    ==========================
!+
!+     * WIND
!+       ---------
!+                                 1                         2      2
!+                FU           =  --- * F    * U    * SQRT( U    + V   )
!+                  VENT           H     AIR    AIR          AIR    AIR
!+
!+                                 1                         2      2
!+                FV           =  --- * F    * V    * SQRT( U    + V   )
!+                  VENT           H     AIR    AIR          AIR    AIR
!+
!+           WHERE :
!+                  UAIR   :  WIND VELOCITY ALONG X
!+                  VAIR   :  WIND VELOCITY ALONG Y
!+                  FAIR   :  AIR FRICTION COEFFICIENT
!+
!+     * CORIOLIS FORCE
!+       ---------------------
!+
!+                FU           =  + FCOR * V
!+                  CORIOLIS
!+
!+                FV           =  - FCOR * U
!+                  CORIOLIS
!+
!+           WHERE :
!+                  U       :  FLOW VELOCITY ALONG X
!+                  V       :  FLOW VELOCITY ALONG Y
!+                  FCOR    :  CORIOLIS PARAMETER
!
!note     BOTTOM FRICTION IS TAKEN INTO ACCOUNT IN THE VOLFIN
!+         (IN MAJZZ), IT IS SEMI-IMPLICIT.
!note  IF SOURCES OR SINKS TERMS ARE ADDED TO THE CONTINUITY EQUATION,
!+         IT IS IDENTIFIED WITH VARIABLE YASMH (SET TO TRUE).
!
!history RIADH ATA (EDF R&D - LNHE)
!+       25/01/2017
!+       V7P3
!+       Adaptatiopn from existing subroutine prosou
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| BANDEC         |-->| IF YES, TIDAL FLATS OR DRY ZONES
!| CORIOL         |-->| IF YES, CORIOLIS FORCE
!| COSLAT         |-->| COSINUS OF LATITUDE (SPHERICAL COORDINATES)
!| COUROU         |-->| IF YES, WAVE DRIVEN CURRENTS TAKEN INTO ACCOUNT
!| DSIP           |-->| DISCHARGE OF TUBES
!| DSCE           |-->| DISCHARGE OF POINT SOURCES
!| DSIP           |-->| DISCHARGE OF CULVERTS
!| DT             |-->| TIME STEP IN SECONDS
!| ENTBUS         |-->| INDICES OF ENTRY OF TUBES IN GLOBAL NUMBERING
!| ENTSIP         |-->| INDICES OF ENTRY OF CULVERTS IN GLOBAL NUMBERING
!| FAIR           |-->| FRICTION COEFFICIENT FOR WIND
!| FCOR           |-->| CORIOLIS PARAMETER
!| FU             |<->| SOURCE TERMS ON VELOCITY U
!| FV             |<->| SOURCE TERMS ON VELOCITY V
!| FXWAVE         |<->| FORCING OF WAVES ALONG X
!| FYWAVE         |<->| FORCING OF WAVES ALONG Y
!| GRAV           |-->| GRAVITY
!| HN             |-->| DEPTH AT TIME T(N)
!| HWIND          |-->| MINIMUM DEPTH FOR TAKING WIND INTO ACCOUNT
!| ISCE           |-->| NEAREST POINTS TO SOURCES
!| LT             |-->| TIME STEP NUMBER
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MAREE          |-->| IF YES, TAKES THE TIDAL FORCE INTO ACCOUNT
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| INDICES OF BOUNDARY POINTS
!| NBUSE          |-->| NUMBER OF TUBES
!| NORD           |-->| DIRECTION OF NORTH WITH RESPECT TO Y AXIS
!|                |   | (TRIGONOMETRIC SENSE) IN DEGREES.
!| N_NGHB_W_NODES |-->| NUMBER OF NEIGHBOURS PROCESSORS IN CASE OF // (FOR WEIRS NODES)
!| NPTH           |-->| RECORD NUMBER IN THE WAVE CURRENTS FILE
!| NREJET         |-->| NUMBER OF POINT SOURCES
!| NREJEU         |-->| NUMBER OF POINT SOURCES WITH GIVEN VELOCITY
!|                |   | IF NREJEU=0 VELOCITY OF SOURCES IS TAKEN EQUAL
!|                |   | TO VELOCITY.
!| NSIPH          |-->| NUMBER OF CULVERTS
!| NDGA1,NDGB1    |-->| INDICES OF POINTS OF WEIRS
!| NVARCL         |-->| NUMBER OF CLANDESTINE VARIABLES
!| NWEIRS         |-->| NUMBER OF WEIRS
!| OPTBAN         |-->| OPTION FOR THE TREATMENT OF TIDAL FLATS
!| OPTSOU         |-->| OPTION FOR THE TREATMENT OF SOURCES
!| PHI0           |-->| LATITUDE OF ORIGIN POINT
!| PLUIE          |-->| BIEF_OBJ STRUCTURE WITH RAIN OR EVAPORATION.
!| RAIN           |-->| IF YES, RAIN OR EVAPORATION TAKEN INTO ACCOUNT
!| RAIN_MMPD      |-->| RAIN OR EVAPORATION IN MM PER DAY
!| SINLAT         |-->| SINUS OF LATITUDE (SPHERICAL COORDINATES)
!| SMH            |-->| SOURCE TERM IN CONTINUITY EQUATION
!| SORBUS         |-->| INDICES OF TUBES EXITS IN GLOBAL NUMBERING
!| SORSIP         |-->| INDICES OF CULVERTS EXISTS IN GLOBAL NUMBERING
!| SPHERI         |-->| IF TRUE : SPHERICAL COORDINATES
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2D_FILES      |-->| BIEF_FILE STRUCTURE WITH ALL TELEMAC-2D FILES
!| T2D_BI1        |-->| RANK OF BINARY FILE 1
!| TYPSEUIL       |-->| TYPE OS WEIRS (ONLY TYPSEUIL=2 IS MANAGE HERE)
!| UBUS           |-->| VELOCITY U AT TUBE EXTREMITY
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| USIP           |-->| VELOCITY U AT CULVERT EXTREMITY
!| VBUS           |-->| VELOCITY V AT TUBE EXTREMITY
!| VARCL          |<->| BLOCK OF CLANDESTINE VARIABLES
!| VARCLA         |-->| NAMES OF CLANDESTINE VARIABLES
!| VENT           |-->| IF YES, WIND IS TAKEN INTO ACCOUNT
!| VSIP           |-->| VELOCITY V AT CULVERT EXTREMITY
!| WINDX          |-->| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |-->| SECOND COMPONENT OF WIND VELOCITY
!| YASMH          |<->| IF TRUE SMH IS TAKEN INTO ACCOUNT
!| YASMO          |<->| IF TRUE FU AND FV ARE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!     FOR SEEING OTHER VARIABLES IN DECLARATIONS_TELEMAC2D:
      USE DECLARATIONS_TELEMAC2D, ONLY : WNODES_PROC,WNODES,U,V,H,
     &                                   RAIN_HDUR,
     &                                   SECCURRENTS,NTRAC,SEC_R,CN,
     &                                   SEC_TAU,T2,T3,T7,ROEAU,CF,S,
     &                                   IELMU,T,ACCROF,RUNOFFOPT,AMC,
     &                                   T2DFO2,ZF,ZFSLOP
      USE INTERFACE_TELEMAC2D, EX_PROSOU_FV => PROSOU_FV
      USE M_COUPLING_ESTEL3D
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     WORKING ARRAYS
!
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1
!
!-----------------------------------------------------------------------
!
!     VECTORS
!
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: FU,FV,SMH,FXWAVE,FYWAVE
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: MASKEL,UN,VN,HN,UNSV2D
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: WINDX,WINDY,COSLAT,SINLAT
!
!-----------------------------------------------------------------------
!
!     MESH STRUCTURE
!
      TYPE(BIEF_MESH)  , INTENT(INOUT) :: MESH
!
!-----------------------------------------------------------------------
!
      INTEGER          , INTENT(IN)    :: NVARCL,LT,NREJET,NREJEU,OPTSOU
      INTEGER          , INTENT(IN)    :: NPTH,T2DBI1,OPTBAN
      INTEGER          , INTENT(IN)    :: MARDAT(3),MARTIM(3)
      INTEGER          , INTENT(IN)    :: ISCE(NREJET)
      DOUBLE PRECISION , INTENT(IN)    :: HWIND,AT,FAIR,FCOR
      DOUBLE PRECISION , INTENT(IN)    :: DSCE(NREJET)
      DOUBLE PRECISION , INTENT(IN)    :: GRAV,NORD,PHI0,RAIN_MMPD,DT
      CHARACTER(LEN=32), INTENT(IN)    :: VARCLA(NVARCL)
      LOGICAL          , INTENT(IN)    :: VENT,MAREE,CORIOL,SPHERI,MSK
      LOGICAL          , INTENT(IN)    :: COUROU,RAIN,BANDEC
      LOGICAL          , INTENT(INOUT) :: YASMH,YASMO
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: VARCL,PLUIE
      TYPE(BIEF_FILE)  , INTENT(IN)    :: T2D_FILES(*)
!
      INTEGER          , INTENT(IN)    :: NSIPH
      INTEGER          , INTENT(IN)    :: ENTSIP(NSIPH),SORSIP(NSIPH)
      DOUBLE PRECISION , INTENT(IN)    :: DSIP(NSIPH)
      DOUBLE PRECISION , INTENT(IN)    :: USIP(NSIPH,2),VSIP(NSIPH,2)
!
      INTEGER          , INTENT(IN)    :: NBUSE
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE),SORBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: DBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: UBUS(2,NBUSE),VBUS(2,NBUSE)
!
      INTEGER          , INTENT(IN)    :: TYPSEUIL,NWEIRS,N_NGHB_W_NODES
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: NBOR,NDGA1,NDGB1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I,IELMH,IELM1,NPOIN,IR,ERR,NP,K
!
      DOUBLE PRECISION PI,WROT,WD,ATH,RAIN_MPS,SURDT,XX
!
      CHARACTER(LEN=32) NOMX,NOMY
      CHARACTER(LEN=8) :: FFORMAT
      INTEGER :: FILE_ID, IREC
      LOGICAL DEJALU,OKX,OKY,OKC
      DATA DEJALU /.FALSE./
!
      INTRINSIC SQRT,MAX,ACOS
!
!-----------------------------------------------------------------------
!  EXTRACTS X COORDINATES, NUMBER OF POINTS P1
!                          AND P1 ELEMENT OF THE MESH
!-----------------------------------------------------------------------
!
      IELM1 = MESH%X%ELM
      NPOIN = MESH%NPOIN
!
!-----------------------------------------------------------------------
!  INITIALISES
!-----------------------------------------------------------------------
!
      CALL CPSTVC(UN,FU)
      CALL CPSTVC(VN,FV)
      CALL OS( 'X=0     ' , X=FU )
      CALL OS( 'X=0     ' , X=FV )
      YASMO = .FALSE.
!
!=======================================================================
!
!  SECONDARY CURRENTS
!
      IF(SECCURRENTS) THEN
        YASMO=.TRUE.
!
!       TAU_SEC
        DO K=1,NPOIN
          XX=H%R(K)*T%ADR(NTRAC)%P%R(K)
     &                        *SQRT(0.5D0*CF%R(K)*(U%R(K)**2+V%R(K)**2))
!         SEC_TAU IS USED ONLY FOR OUTPUTS
          SEC_TAU%R(K)=ROEAU*XX
!         ROEAU NOT CONSIDERED IN TAU_SEC, IT AVOIDS A DIVISION LATER
          T1%R(K) = XX*H%R(K)
        ENDDO
!       COMPUTING THE GRADIENTS OF H*TAU_SEC
!       WITH FACTOR V2DPAR=1/UNSV2D, REMOVED LATER
        CALL VECTOR(T2,'=','GRADF          X',IELMU,
     &              1.D0,T1,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(T3,'=','GRADF          Y',IELMU,
     &              1.D0,T1,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM (T2, 2, MESH)
          CALL PARCOM (T3, 2, MESH)
        ENDIF
!
        DO K=1,NPOIN
          IF(H%R(K).GE.1.D-4) THEN
            XX=1.D0/MAX(SQRT(U%R(K)**2+V%R(K)**2),1.D-12)
            T2%R(K)=UNSV2D%R(K)*(T2%R(K)*V%R(K)-T3%R(K)*U%R(K))*XX
            T7%R(K)=XX*(2.D0*T1%R(K)*SEC_R%R(K)+T2%R(K))/H%R(K)
!           FORCES ALONG X AND Y
            FU%R(K) = FU%R(K) - U%R(K)*T7%R(K)
            FV%R(K) = FV%R(K) - V%R(K)*T7%R(K)
          ENDIF
        ENDDO
!
      ENDIF
!
!=======================================================================
!
!  WIND
!
!                               1                         2     2
!              FU           =  --- * F    * U    * SQRT( U   + V    )
!                VENT           H     AIR    AIR          AIR   AIR
!
!
!                               1                         2     2
!              FV           =  --- * F    * V    * SQRT( U   + V    )
!                VENT           H     AIR    AIR          AIR   AIR
!
!
      IF(VENT) THEN
        YASMO=.TRUE.
!       TEMPORARY TREATMENT OF TIDAL FLATS
!       THE WIND EFFECT IS ONLY CONSIDERED IF THE WATER DEPTH IS
!       GREATER THAN 1 M.
!
        DO N=1,NPOIN
          IF (HN%R(N).GT.HWIND) THEN
            WD = SQRT( WINDX%R(N)**2 + WINDY%R(N)**2 )
            FU%R(N) = FU%R(N) + FAIR * WINDX%R(N) * WD / HN%R(N)
            FV%R(N) = FV%R(N) + FAIR * WINDY%R(N) * WD / HN%R(N)
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
! CORIOLIS FORCE ==> DONE IN SOURCE_MOMENT
!=======================================================================
!
!
!                FU           =  + FCOR * V
!                  CORIOLIS
!
!                FV           =  - FCOR * U
!                  CORIOLIS
!=======================================================================
!    TIDAL FORCES
!=======================================================================
!
      IF(SPHERI.AND.MAREE) THEN
!
            CALL MARAST(MARDAT,MARTIM,PHI0,NPOIN,AT,
     &                  FU%R,FV%R,MESH%X%R,SINLAT%R,COSLAT%R,GRAV)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  THE SECOND MEMBERS ARE PROPERLY DISCRETISED
!
      IELMU=UN%ELM
!
      IF(IELMU.NE.IELM1) THEN
        CALL CHGDIS(FU,IELM1,IELMU,MESH)
        CALL CHGDIS(FV,IELM1,IELMU,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IELMH=HN%ELM
      CALL CPSTVC(HN,SMH)
      YASMH=.FALSE.
      CALL OS('X=0     ',X=SMH)
!=======================================================================
!     RAIN-EVAPORATION
!=======================================================================
      IF(RAIN) THEN
!
        YASMH=.TRUE.
!
!       RAIN OR EVAPORATION = 0 FOR AT > RAIN_HDUR
!
        IF(AT.LE.RAIN_HDUR*3600.D0) THEN
          RAIN_MPS=RAIN_MMPD/86400000.D0
        ELSE
          RAIN_MPS=0.D0
        ENDIF
        SURDT=1.D0/DT
        IF(BANDEC) THEN
          IF(RUNOFFOPT.EQ.0)THEN
!           EVAPORATION (TENTATIVELY...) LIMITED BY AVAILABLE WATER
            DO I=1,NPOIN
              PLUIE%R(I)=MAX(RAIN_MPS,-MAX(HN%R(I),0.D0)*SURDT)
            ENDDO
          ELSEIF(RUNOFFOPT.EQ.1)THEN
            CALL RUNOFF_SCS_CN(PLUIE,T1%R,T2%R,T3%R,ACCROF,RAIN_MPS,AMC,
     &                         CN,ZF,ZFSLOP,RAIN_HDUR,T2D_FILES,T2DFO2,
     &                         NPOIN,MASKEL,MSK,IELM1,MESH)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,221)
            IF(LNG.EQ.2) WRITE(LU,222)
221         FORMAT(1X,'PROSOU_FV : MODELE PLUIE-DEBIT NON IMPLEMENTE',/,
     &             1X,'         LES OPTIONS DISPONIBLES SONT :',/,
     &             1X,'         0 : PAS D''INFILTRATION',/,
     &             1X,'         1 : MODELE SCS CN')
222         FORMAT(1X,'PROSOU_FV : RUNOFF MODEL NOT IMPLEMENTED YET',/,
     &             1X,'         AVAILABLE OPTIONS ARE:',/,
     &             1X,'         0 : NO INFILTRATION',/,
     &             1X,'         1 : SCS CN MODEL')
!
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          IF(RUNOFFOPT.EQ.1) THEN
            IF(LNG.EQ.1) WRITE(LU,223)
            IF(LNG.EQ.2) WRITE(LU,224)
223         FORMAT(1X,'PROSOU_FV : LE MODELE PLUIE-DEBIT SCS CN',/,
     &             1X,'          NECESSITE D''ACTIVER L''OPTION',/,
     &             1X,'          BANCS DECOUVRANTS')
224         FORMAT(1X,'PROSOU_FV: TIDAL FLATS  MUST BE ACTIVATED',/,
     &             1X,'            WITH SCS CN RUNOFF MODEL')
!
            CALL PLANTE(1)
            STOP
          ENDIF
          CALL OS('X=C     ',X=PLUIE,C=RAIN_MPS)
        ENDIF
      ENDIF
!
!     SOURCES
!
      IF(NREJET.GT.0) THEN
!
        YASMH = .TRUE.
        YASMO = .TRUE.
!
!       SOURCE TERMS IN THE CONTINUITY EQUATION
!       BEWARE, SMH IS ALSO USED FOR TRACER
!
        DO I = 1 , NREJET
          IR = ISCE(I)
!         THE TEST IS USEFUL IN PARALLEL MODE, WHEN THE POINT SOURCE
!         IS NOT IN THE SUB-DOMAIN
          IF(IR.GT.0) THEN
            IF(OPTSOU.EQ.1) THEN
!            LINEAR VERSION NOT COMAPTIBLE WITH FV
!             SMH%R(IR)=SMH%R(IR)+DSCE(I)*UNSV2D%R(IR)
              IF(LNG.EQ.1) WRITE(LU,322)
              IF(LNG.EQ.2) WRITE(LU,323)
              CALL PLANTE(1)
              STOP
            ELSE
!             "DIRAC" VERSION
              SMH%R(IR)=SMH%R(IR)+DSCE(I)
            ENDIF
          ENDIF
        ENDDO
!
!       SOURCE TERMS IN THE MOMENTUM EQUATIONS
!       EXPLICIT TREATMENT OF MOMENTUM CONTRIBUTIONS TO THE SOURCES
!
        IF(NREJEU.GT.0) THEN
          DO I = 1 , NREJEU
            IR = ISCE(I)
!           THE TEST IS USEFUL IN PARALLEL MODE, WHEN THE POINT SOURCE
!           IS NOT IN THE SUB-DOMAIN
            IF(IR.GT.0) THEN
!             MOMENTUM ADDED BY THE SOURCE
!      -      MOMENTUM TAKEN BY THE SOURCE
              FU%R(IR)=FU%R(IR) + (VUSCE(AT,I)-UN%R(IR))*
     &        DSCE(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
              FV%R(IR)=FV%R(IR) + (VVSCE(AT,I)-VN%R(IR))*
     &        DSCE(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
            ENDIF
          ENDDO
        ENDIF
!
      ENDIF
!
!     CULVERTS
!
      IF(NSIPH.GT.0) THEN
!
        YASMH = .TRUE.
        YASMO = .TRUE.
!
        DO I = 1 , NSIPH
        IR = ENTSIP(I)
          IF(IR.GT.0) THEN
            IF(OPTSOU.EQ.1) THEN
!             LINEAR VERSION NOT COMAPTIBLE WITH FV
!             SMH%R(IR)=SMH%R(IR)-DSIP(I)*UNSV2D%R(IR)
              IF(LNG.EQ.1) WRITE(LU,322)
              IF(LNG.EQ.2) WRITE(LU,323)
              CALL PLANTE(1)
              STOP
            ELSE
!             "DIRAC" VERSION
              SMH%R(IR)=SMH%R(IR)-DSIP(I)
            ENDIF
            FU%R(IR) = FU%R(IR) - (USIP(I,1)-UN%R(IR))*
     &      DSIP(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
            FV%R(IR) = FV%R(IR) - (VSIP(I,1)-VN%R(IR))*
     &      DSIP(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
          ENDIF
          IR = SORSIP(I)
          IF(IR.GT.0) THEN
            IF(OPTSOU.EQ.1) THEN
!             "NORMAL" VERSION
!              SMH%R(IR)=SMH%R(IR)+DSIP(I)*UNSV2D%R(IR)
              IF(LNG.EQ.1) WRITE(LU,322)
              IF(LNG.EQ.2) WRITE(LU,323)
              CALL PLANTE(1)
              STOP
            ELSE
!             "DIRAC" VERSION
              SMH%R(IR)=SMH%R(IR)+DSIP(I)
            ENDIF
            FU%R(IR) = FU%R(IR) + (USIP(I,2)-UN%R(IR))*
     &      DSIP(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
            FV%R(IR) = FV%R(IR) + (VSIP(I,2)-VN%R(IR))*
     &      DSIP(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
          ENDIF
        ENDDO
      ENDIF
!
!     TUBES OR BRIDGES
!
      IF(NBUSE.GT.0) THEN
!
        YASMH = .TRUE.
        YASMO = .TRUE.
!
        DO I = 1 , NBUSE
          IR = ENTBUS(I)
          IF(IR.GT.0) THEN
            IF(OPTSOU.EQ.1) THEN
!             LINEAR VERSION NOT COMAPTIBLE WITH FV
!             SMH%R(IR)=SMH%R(IR)-DBUS(I)*UNSV2D%R(IR)
              IF(LNG.EQ.1) WRITE(LU,322)
              IF(LNG.EQ.2) WRITE(LU,323)
              CALL PLANTE(1)
              STOP
            ELSE
!             "DIRAC" VERSION
              SMH%R(IR)=SMH%R(IR)-DBUS(I)
            ENDIF
            FU%R(IR) = FU%R(IR) - (UBUS(1,I)-UN%R(IR))*
     &      DBUS(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
            FV%R(IR) = FV%R(IR) - (VBUS(1,I)-VN%R(IR))*
     &      DBUS(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
          ENDIF
          IR = SORBUS(I)
          IF(IR.GT.0) THEN
            IF(OPTSOU.EQ.1) THEN
!             LINEAR VERSION NOT COMAPTIBLE WITH FV
!             SMH%R(IR)=SMH%R(IR)-DBUS(I)*UNSV2D%R(IR)
              IF(LNG.EQ.1) WRITE(LU,322)
              IF(LNG.EQ.2) WRITE(LU,323)
              CALL PLANTE(1)
              STOP
            ELSE
!             "DIRAC" VERSION
              SMH%R(IR)=SMH%R(IR)+DBUS(I)
            ENDIF
            FU%R(IR) = FU%R(IR) + (UBUS(2,I)-UN%R(IR))*
     &      DBUS(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
            FV%R(IR) = FV%R(IR) + (VBUS(2,I)-VN%R(IR))*
     &      DBUS(I)*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
          ENDIF
        ENDDO
      ENDIF
!
!     WEIRS (ONLY IF TYPSEUIL=2)
!
      IF(NWEIRS.GT.0.AND.TYPSEUIL.EQ.2) THEN
!
        YASMH = .TRUE.
        YASMO = .TRUE.
!
        DO N=1,N_NGHB_W_NODES
          IF(WNODES_PROC(N)%NUM_NEIGH.EQ.IPID) GOTO 50
        ENDDO
50      CONTINUE
        DO I=1, WNODES_PROC(N)%NB_NODES
          IR = WNODES_PROC(N)%NUM_LOC(I)
          K  = WNODES_PROC(N)%LIST_NODES(I)
          SMH%R(IR) = SMH%R(IR) + WNODES(K)%QN * UNSV2D%R(IR)
! QUANTITY OF MOVEMENTS NOT TAKEN INTO ACCOUNT FOR THE MOMENT
! The following lines generate instability and crash
! Probably because we would like to impose velocities accross  solid boundaries!
!
!         FU%R(IR) = FU%R(IR) + (UWEIRA%ADR(N)%P%R(I)-UN%R(IR))*
!     &      WNODES(K)%QN*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
!         FV%R(IR) = FV%R(IR) + (VWEIRA%ADR(N)%P%R(I)-VN%R(IR))*
!     &      WNODES(K)%QN*UNSV2D%R(IR)/MAX(HN%R(IR),0.1D0)
        ENDDO
      ENDIF
!
322   FORMAT(1X,'PROSOU_FV: UNIQUMENT LES SOURCES DE TYPE DIRAC',/,
     &             1X,'     SONT IMPLEMENTEES AVEC LES VOLUMES FINIS',/,
     &             1X,'     METTRE: ',/,
     &             1X,'     TYPE DE SOURCES = 2')
323   FORMAT(1X,'PROSOU_FV: ONLY SOURCES WITH DIRAC OPTION ',/,
     &       1X,'           ARE IMPLEMENTED WITH FINITE VOLUMES  '  ,/,
     &       1X,'           PLEASE SET ',/,
     &       1X,'           "TYPE OF SOURCES = 2"           ')
!
!=======================================================================
!
!  WAVE DRIVEN CURRENTS
!
!
!                FU        =  FXWAVE
!                  COUROU
!
!                FV        =  FYWAVE
!                  COUROU
!
!       FXWAVE AND FYWAVE ARE TAKEN IN A RESULTS FILE FROM
!       ARTEMIS OR TOMAWAC
!
!       BEWARE   : 1. MESHES MUST BE THE SAME
!       ---------
!
!                  2. STATIONARY FORCING
!
      IF(COUROU) THEN
        YASMO = .TRUE.
!
!       WITH NO COUPLING, TAKING THE WAVE STRESSES ONCE FOR ALL
!       IN A BINARY DATA FILE
!
        IF(.NOT.DEJALU.AND..NOT.INCLUS(COUPLING,'TOMAWAC')) THEN
!
            ! Records numbering starts from 0
            IREC = NPTH - 1
!           NBI1 : BINARY DATA FILE 1
            NOMX='FORCE FX        M/S2            '
            NOMY='FORCE FY        M/S2            '
            FFORMAT = T2D_FILES(T2DBI1)%FMT
            FILE_ID = T2D_FILES(T2DBI1)%LU
            CALL GET_MESH_NPOIN(FFORMAT, FILE_ID, TRIANGLE_ELT_TYPE,
     &                          NP, ERR)
            CALL CHECK_CALL(ERR,'PROSOU:GET_MESH_NPOIN')
            CALL FIND_VARIABLE(FFORMAT, FILE_ID, NOMX, FXWAVE%R, NPOIN,
     &                     ERR, RECORD=IREC, TIME_RECORD=ATH)
            OKX = ERR.EQ.0
            CALL FIND_VARIABLE(FFORMAT, FILE_ID, NOMY, FYWAVE%R, NPOIN,
     &                     ERR, RECORD=IREC, TIME_RECORD=ATH)
            OKY = ERR.EQ.0
            IF(.NOT.OKX.OR..NOT.OKY) THEN
!             SECOND TRY (OLD VERSIONS OF ARTEMIS OR TOMAWAC)
              NOMX='FORCE_FX                      '
              NOMY='FORCE_FY                      '
              CALL FIND_VARIABLE(FFORMAT, FILE_ID,NOMX, FXWAVE%R, NPOIN,
     &                       ERR, RECORD=IREC,TIME_RECORD=ATH)
              OKX = ERR.EQ.0
              CALL FIND_VARIABLE(FFORMAT, FILE_ID,NOMY, FYWAVE%R, NPOIN,
     &                       ERR, RECORD=IREC,TIME_RECORD=ATH)
              OKY = ERR.EQ.0
            ENDIF
!           CLANDESTINE VARIABLES FROM TOMAWAC TO SISYPHE
            IF(NVARCL.GT.0) THEN
              DO I=1,NVARCL
                CALL FIND_VARIABLE(FFORMAT, FILE_ID,
     &                         VARCLA(I)(1:16),VARCL%ADR(I)%P%R, NPOIN,
     &                          ERR,RECORD=IREC,TIME_RECORD=ATH)
                IF(ERR.NE.0) THEN
                  IF(LNG.EQ.1) WRITE(LU,7) VARCLA(I)(1:16)
                  IF(LNG.EQ.2) WRITE(LU,8) VARCLA(I)(1:16)
7               FORMAT(1X,'PROSOU : VARIABLE CLANDESTINE :',/,1X,A16,/,
     &                 1X,'         NON TROUVEE',/,1X,
     &                    '         DANS LE FICHIER DE HOULE')
8               FORMAT(1X,'PROSOU : CLANDESTINE VARIABLE:',/,1X,A16,/,
     &                 1X,'         NOT FOUND',/,1X,
     &                    '         IN THE WAVE RESULTS FILE')
                CALL PLANTE(1)
                STOP
                ENDIF
              ENDDO
            ENDIF
!
          IF(.NOT.OKX.OR..NOT.OKY) THEN
            IF(LNG.EQ.1) WRITE(LU,5)
            IF(LNG.EQ.2) WRITE(LU,6)
5           FORMAT(1X,'PROSOU : FORCE FX OU FY NON TROUVES',/,1X,
     &                '         DANS LE FICHIER DE HOULE')
6           FORMAT(1X,'PROSOU: FORCE FX OR FY NOT FOUND',/,1X,
     &                '         IN THE WAVE RESULTS FILE')
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(NP.NE.NPOIN) THEN
            IF(LNG.EQ.1) WRITE(LU,95)
            IF(LNG.EQ.2) WRITE(LU,96)
 95         FORMAT(1X,'PROSOU : SIMULATION DES COURANTS DE HOULE.',/,
     &             1X,'LES MAILLAGES HOULE ET COURANTS SONT ',/,
     &             1X,'DIFFERENTS : PAS POSSIBLE POUR LE MOMENT.')
 96         FORMAT(1X,'PROSOU: WAVE DRIVEN CURRENTS MODELLING.',/,
     &             1X,'WAVE AND CURRENT MODELS MESHES ARE ',/,
     &             1X,'DIFFERENT : NOT POSSIBLE AT THE MOMENT.')
!
            CALL PLANTE(1)
            STOP
          ENDIF
!         WRITES OUT TO THE LISTING
          IF(LNG.EQ.1) WRITE(LU,115) ATH
          IF(LNG.EQ.2) WRITE(LU,116) ATH
115       FORMAT(1X,/,1X,'PROSOU : COURANTS DE HOULE',/,
     &                1X,'         LECTURE AU TEMPS ',F10.3,/)
116       FORMAT(1X,/,1X,'PROSOU: WAVE DRIVEN CURRENTS MODELLING',/,
     &                1X,'         READING FILE AT TIME ',F10.3,/)
          IF(IELMU.NE.IELM1) THEN
            CALL CHGDIS(FXWAVE,IELM1,IELMU,MESH)
            CALL CHGDIS(FYWAVE,IELM1,IELMU,MESH)
          ENDIF
          DEJALU = .TRUE.
!
        ENDIF
!
!       ADDS INTO FU AND FV
!
        IF(INCLUS(COUPLING,'TOMAWAC')) THEN
          IF(IELMU.NE.IELM1) THEN
            CALL CHGDIS(FXWAVE,IELM1,IELMU,MESH)
            CALL CHGDIS(FYWAVE,IELM1,IELMU,MESH)
          ENDIF
        ENDIF
        CALL OS('X=X+Y   ',X=FU,Y=FXWAVE)
        CALL OS('X=X+Y   ',X=FV,Y=FYWAVE)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TAKES SEEPAGE IN THE SOIL INTO ACCOUNT
!     COMMUNICATES WITH ESTEL-3D
!
!     GETS SOURCE TERM FROM ESTEL-3D TO ACCOUNT FOR SEEPAGE
!     CALLS THE INFILTRATION ROUTINE
!
      CALL INFILTRATION_GET(SMH%R,UNSV2D%R,YASMH)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
