!                    ************************
                     SUBROUTINE RUNOFF_SCS_CN
!                    ************************
!
!
     & (PLUIE,ACCFA,ACCIA,ACCROFF,ACCROF_OLD,RAIN_MPS,AMC,CN,ZF,ZFSLOP,
     &  RAIN_HDUR,FILES,FO2,NPOIN,MASKEL,MSK,IELM,MESH)
!
!***********************************************************************
! TELEMAC2D   V7P2                                        15/06/2016
!***********************************************************************
!
!brief    RAINFALL-RUNOFF CALCULATION BASED ON THE SCS METHOD FOR
!+        ABSTRACTIONS (REFERENCE: APPLIED HYDROLOGY,
!+        CHOW, MAIDMENT, MAYS, McGraw-Hill Publishing 1988).
!+        SPATIALLY VARIABLE CURVE NUMBER DEFINED IN FORMATTED DATA FILE
!+        OR ON THE MESH.
!+        EXAMPLES OF RAINFALL DEFINED BY:
!+        - IDF PARAMETERS (CDS-TYPE HYETOGRAPH)
!+        - HYETOGRAPH READ IN FORMATTED DATA FILE
!
!
!history  PIERRE-LOUIS LIGIER (SWECO)
!+        24/07/2016
!+        V7P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACCFA          |<->| ACCUMULATED CONTINUING ABSTRACTION
!| ACCIA          |<->| ACCUMULATED INITIAL ABSTRACTION
!| ACCROFF        |<->| ACCUMULATED RUNOFF AT TIME AT
!| ACCROF_OLD     |<->| ACCUMULATED RUNOFF AT LAST TIME STEP
!| AMC            |-->| ANTECEDENT MOISTURE CONDITIONS FOR SCS CN MODEL
!|                |   | OPTIONS FOR ANTECEDENT MOISTURE CONDITIONS:
!|                |   |   +> 1: DRY ANTECEDENT MOISTURE CONDITIONS
!|                |   |   +> 2: NORMAL ANTECEDENT MOISTURE CONDITIONS
!|                |   |   +> 3: WET ANTECEDENT MOISTURE CONDITIONS
!| CN             |-->| CURVE NUMBER
!| COUPLING       |-->| STRING WITH THE LIST OF COUPLED PROGRAMMES
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| FO2            |-->| LOGICAL UNIT OF THE FORMATTED DATA FILE
!| IELM           |-->| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS.
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| PLUIE          |-->| BIEF_OBJ STRUCTURE WITH RAIN OR EVAPORATION.
!| RAIN_HDUR      |-->| RAIN OR EVAPORATION DURATION IN HOURS
!| RAIN_MPS       |<->| RAIN OR EVAPORATION IN M PER SECONDS
!| ZF             |-->| BOTTOM ELEVATION
!| ZFSLOP         |<->| BOTTOM SLOPE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC,   ONLY: COUPLING
      USE DECLARATIONS_TELEMAC2D, ONLY: DT,LT,AT,HN,T5,T6,T7,T8,T9,T10,
     &                                  ENTET,T2DFO1
      USE INTERFACE_TELEMAC2D, EX_RUNOFF_SCS_CN => RUNOFF_SCS_CN
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            , INTENT(IN)    :: NPOIN,AMC,FO2,IELM
      LOGICAL            , INTENT(IN)    :: MSK
      DOUBLE PRECISION   , INTENT(IN)    :: RAIN_MPS,RAIN_HDUR
      DOUBLE PRECISION   , INTENT(INOUT) :: ACCIA(NPOIN),ACCFA(NPOIN)
      DOUBLE PRECISION   , INTENT(INOUT) :: ACCROFF(NPOIN)
      TYPE(BIEF_OBJ)     , INTENT(IN)    :: ZF,MASKEL
      TYPE(BIEF_OBJ)     , INTENT(INOUT) :: PLUIE,ACCROF_OLD,CN,ZFSLOP
      TYPE(BIEF_FILE)    , INTENT(IN)    :: FILES(*)
      TYPE(BIEF_MESH)    , INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,UL
!     TO CHANGE TO KEYWORD, IF NECESSARY
      INTEGER, PARAMETER ::RAINDEF=1
      LOGICAL STEEPSLOPECOR
!
      DOUBLE PRECISION RAIN_MPS_GEO,PEAK_TIME,CC
      DOUBLE PRECISION, PARAMETER::EPS=1.E-6
      DOUBLE PRECISION A,B,C,R,RELT,IMMH,RFM,RF_HDUR
      DOUBLE PRECISION AT1,AT2,MM_AT2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     INITIALIZATION
      IF(LT.EQ.1)THEN
        CALL OV('X=C     ',T5%R,T5%R,T5%R,0.D0,NPOIN)
        CALL OV('X=C     ',T6%R,T6%R,T6%R,0.D0,NPOIN)
        CALL OV('X=C     ',T7%R,T7%R,T7%R,0.D0,NPOIN)
        CALL OV('X=C     ',ACCROF_OLD%R,ACCROFF,ACCROFF,0.D0,NPOIN)
      ENDIF
      CALL OV('X=C     ',ACCIA,ACCIA,ACCIA,0.D0,NPOIN)
      CALL OV('X=C     ',ACCFA,ACCFA,ACCFA,0.D0,NPOIN)
      CALL OV('X=C     ',ACCROFF,ACCROFF,ACCROFF,0.D0,NPOIN)
!
!-----------------------------------------------------------------------
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     OPTIONS FOR RAINFALL DEFINITION:
!     -------------------------------
!     +> 1: STANDARD RAINFALL (CONSTANT VALUE IN MM/DAY, KEYWORD)
!     +> 2: RAINFALL DEFINED AS A CDS-TYPE HYETOGRAPH BY IDF PARAMETERS
!     +> 3: RAINFALL DEFINED AS A BLOCK-TYPE HYETOGRAPH READ IN A
!           FORMATTED DATA FILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     1. STANDARD RAINFALL (CONSTANT VALUE IN MM/DAY GIVEN BY KEYWORD)
!     ================================================================
      IF(RAINDEF.EQ.1) THEN
!       RAINFALL AT TIME AT OVER ONE TIME-STEP, M
        RFM = RAIN_MPS * DT
!
!     2. EXAMPLE A: CDS-TYPE HYETOGRAPH DEFINED BY IDF PARAMETERS
!     ===========================================================
      ELSEIF(RAINDEF.EQ.2) THEN
!
!       EXAMPLE A: IDF CURVE OF TYPE I = A / (T**B + C), MM/H
!       RAINFALL STARTS AT AT = 0.D0 SECONDS
!       RAINFALL DURATION DEFINED IN HOURS BY RAIN_HDUR (KEYWORD)

!       IDF CONSTANTS, MM/H
        A = 59.9916D0
        B = 0.9737D0
        C = 0.2235D0
!
        IF(LT.EQ.1) THEN
          RF_HDUR = RAIN_HDUR * A / (RAIN_HDUR**B + C)
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'RUNOFF_SCS_CN: VOLUME TOTAL DE PRECIPITATION'
            WRITE(LU,*) '               D''APRES DONNEES UTILISATEUR:'
            WRITE(LU,*)                 RF_HDUR, 'MM'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'RUNOFF_SCS_CN: TOTAL RAINFALL VOLUME ACCORDING'
            WRITE(LU,*) '               TO USER DATA:', RF_HDUR, 'MM'
          ENDIF
        ENDIF
!
!       PEAK DECENTERING PARAMETER R, 0. =< R =< 1.
!       FOR SYMMETRICAL RAINFALL (PEAK AT RAIN_HDUR/2: R = 0.5)
        R = 0.5D0
        PEAK_TIME=R*RAIN_HDUR*3600.D0
!
!       TIME RELATIVE TO PEAK, SECONDS
        IF(AT.LT.(PEAK_TIME)) THEN
          RELT = ABS(AT-PEAK_TIME) / MAX(R,EPS)
        ELSE
          RELT = ABS(AT-PEAK_TIME) / MAX((1.D0-R),EPS)
        ENDIF
!
!       RAINFALL INTENSITY AT TIME AT, MM/H
!       EQUATION BELOW VALID ONLY FOR IDF CURVE OF TYPE I = A / (T**B + C)
        IF(AT.LE.(RAIN_HDUR*3600.D0)) THEN
          IMMH = A*((1.D0-B)*(RELT/3600.D0)**B + C ) /
     &           ((RELT/3600.D0)**B + C)**2
        ELSE
!         FORCE RAINFALL = 0 FOR AT>RAIN_HDUR, IDF RELATIONSHIP NO MORE VALID
          IMMH = 0.D0
        ENDIF
!       RAINFALL AT TIME AT OVER ONE TIME-STEP RFM, M
        RFM = (IMMH / 1000.D0 / 3600.D0) * DT
!
!
!     3. EXAMPLE B: BLOCK-TYPE HYETOGRAPH READ IN A FORMATTED DATA FILE
!     =================================================================
      ELSEIF(RAINDEF.EQ.3) THEN
!
!       THE HYETOGRAPH IS DEFINED IN A FORMATTED DATA FILE WITH THE
!       FOLLOWING STRUCTURE:
!
!       #HYETOGRAPH FILE
!       #T (s) RAINFALL (mm)
!       0. 0.
!       3600. 10.
!       7200. 20.
!       etc...
!
!       NOTE THAT THE KEYWORD 'DURATION OF RAIN OR EVAPORATION IN HOURS'
!       IS NOT TAKEN INTO ACCOUNT IN THIS EXAMPLE.
!
!       THE BLOCK-TYPE DEFINITION ASSUMES THAT THE RAINFALL RATE IS
!       CONSTANT BETWEEN THE GIVEN TIME STEPS. FOR THE FILE EXAMPLE
!       ABOVE, THE PROGRAM WILL THEN ASSUME THAT THERE IS A 10 MM
!       RAINFALL BETWEEN T = 0 AND 3600 S, THEN 20 MM BETWEEN 3600 AND
!       7200 S AND SO ON.
!
!       THE HYETOGRAPH IS READ FROM FORMATTED DATA FILE 1 (T2DFO1)
        UL = FILES(T2DFO1)%LU
        REWIND(UL)
!
!       JUMPING TWO LINES OF COMMENTS
        READ(UL,*)
        READ(UL,*)
!       READING THE FIRST TWO LINES OF DATA
        READ(UL,*) AT1 !HERE WE DON'T NEED TO READ THE RAINFALL QUANTITY
        READ(UL,*) AT2,MM_AT2
!
        IF(AT.LT.AT1) THEN
          WRITE(LU,*)' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'RUNOFF_SCS_CN : DEBUT TARDIF DU FICHIER'
            WRITE(LU,*)'                HYETOGRAMME'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'RUNOFF_SCS_CN : LATE BEGINNING OF HYETOGRAPH'
            WRITE(LU,*)'                FILE'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
10      CONTINUE
        IF(AT.GE.AT1.AND.AT.LE.AT2) THEN
!         RAINFALL AT TIME AT OVER ONE TIME-STEP RFM, M
          RFM = (MM_AT2 / 1000.D0 / MAX((AT2-AT1),EPS)) * DT
        ELSE
          AT1=AT2
          READ(UL,*,ERR=100,END=200) AT2,MM_AT2
          GO TO 10
!
100       CONTINUE
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'RUNOFF_SCS_CN : ERREUR DANS LE FICHIER'
            WRITE(LU,*)'                HYETOGRAMME'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'RUNOFF_SCS_CN : ERROR IN THE HYETOGRAPH FILE'
          ENDIF
          CALL PLANTE(1)
          STOP
!
200       CONTINUE
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'RUNOFF_SCS_CN : FIN PREMATUREE DU FICHIER'
            WRITE(LU,*)'                HYETOGRAMME'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'RUNOFF_SCS_CN : HYETOGRAPH FILE TOO SHORT'
          ENDIF
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
!    RAINFALL DEFINITION OPTION NOT IMPLEMENTED
      ELSE
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'RUNOFF_SCS_CN : OPTION POUR LA DEFINITION DE LA'
          WRITE(LU,*)'                PLUIE NON VALIDE               '
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)'RUNOFF_SCS_CN : OPTION OF RAIN DEFINITION NOT'
          WRITE(LU,*)'                IMPLEMENTED YET              '
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     CHECK THAT RAINFALL IS POSITIVE (EVAPORATION NOT SUPPORTED)
      IF(RFM.LT.0.D0) THEN
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'RUNOFF_SCS_CN : PRECIPITATION NEGATIVE'
          WRITE(LU,*)'                TROUVEE AU TEMPS', AT
          WRITE(LU,*)'                EVAPORATION NON PRISE EN COMPTE'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)'RUNOFF_SCS_CN : NEGATIVE RAINFALL FOUND'
          WRITE(LU,*)'                AT TIME', AT
          WRITE(LU,*)'                EVAPORATION NOT SUPPORTED'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CN PARAMETERS:
!     =============
!
!     - CN VALUE FOR NORMAL ANTECEDENT MOISTURE CONDITIONS (CN2) GIVEN
!       IN FORMATED FILE 2, HEREAFTER AN INTERPOLATION OF THESE CN
!       ON THE MESH IS ACHIEVED
!
      IF(LT.EQ.1)THEN
        CALL HYDROMAP(CN%R,MESH%X%R,MESH%Y%R,MESH%NPOIN,FILES(FO2)%LU,
     &                MESH%NBOR%I,MESH%KP1BOR%I,MESH%NPTFR)
      ENDIF
!
!       NOTE: CN VALUE FOR NORMAL ANTECEDENT MOISTURE CONDITIONS (CN2)
!       ****  CAN ALSO BE READ FROM A USER VARIABLE STORED IN THE
!             GEOMETRY FILE USING THE FOLLOWING KEYWORDS:
!              +>  NUMBER OF PRIVATE VARIABLES
!              +>  NAMES OF PRIVATE VARIABLES
!             IN THE EXAMPLE BELOW CN IS READ FROM PRIVE%ADR(1)%P%R:
!
!      CALL OV('X=Y     ',CN%R,PRIVE%ADR(1)%P%R,PRIVE%ADR(1)%P%R,
!     &                   0.D0,NPOIN)
!
!     - OPTION FOR STEEP SLOPE CORRECTION:
!       +> REFERENCE: Huang, Gallichand, Wang and Goulet. A modification
!                     to the Soil Conservation Service curve number
!                     method for steep slopes in the Loess Plateau of
!                     China. Hydrological Processes 20, 579-589 (2006).
!       +> CORRECTION FOR SLOPES BETWEEN 0.14 AND 1.4 M/M DEFINED BY A
!          CORRECTION FACTOR CN2A/CN2 (VARIABLE TCN2A_CN2=T10%R)
!       +> TERRAIN SLOPE (M/M) COMPUTED BY SUBROUTINE ZSLOPE
!
!     TO ACTIVATE OPTION FOR STEEP SLOPE CORRECTION: STEEPSLOPECOR = .TRUE.
      STEEPSLOPECOR = .FALSE. !CAN BE A KEYWORD?
!
      IF(STEEPSLOPECOR) THEN
!
        IF(LT.EQ.1.OR.INCLUS(COUPLING,'SISYPHE'))THEN
!         COMPUTE THE BOTTOM SLOPE
          CALL ZSLOPE(ZFSLOP,ZF,T8,T9,MSK,MASKEL,IELM,MESH)
!         COMPUTE STEEP SLOPE CORRECTION COEFFICIENT CN2A_CN2 (STOCKED IN T10)
          CC=(322.79D0+15.63D0*1.4D0)/(1.4D0+323.52D0)
          DO I=1,NPOIN
            IF(ZFSLOP%R(I).GE.0.14D0.AND.ZFSLOP%R(I).LE.1.4D0) THEN
              T10%R(I) = (322.79D0+15.63D0*ZFSLOP%R(I))
     &                    /(ZFSLOP%R(I)+323.52D0)
            ELSEIF(ZFSLOP%R(I).GT.1.4D0) THEN
!             FOR SLOPES > 1.4, CN2A_CN2 = CC (MAX VALUE FOR SLOPE = 1.4)
              T10%R(I) = CC
            ELSE
!             FOR SLOPES < 0.14, NO CORRECTION (CN2A_CN2 = 1)
              T10%R(I) = 1.D0
            ENDIF
          ENDDO
        ENDIF
      ELSE
!       CN2A_CN2 = 1 IF STEEPSLOPECOR = .FALSE.
        CALL OS('X=C     ',X=T10,C=1.D0)
      ENDIF
!
!***********************************************************************
!
!       CHECK THAT CN IS NOT GREATER THAN 100
        DO I=1,NPOIN
          IF((CN%R(I)-100.D0).GT.1.D-6) THEN
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'RUNOFF_SCS_CN : IL Y A AU MOINS UN NOEUD'
              WRITE(LU,*) '                AVEC CN > 100 DANS DONNEES'
              WRITE(LU,*) '                D''ENTREE. PAR EXEMPLE:'
              WRITE(LU,*) '                NOEUD:',I,'WITH CN=',CN%R(I)
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'RUNOFF_SCS_CN : AT LEAST ONE NODE WITH'
              WRITE(LU,*) '                CN VALUE > 100 FOUND IN'
              WRITE(LU,*) '                INPUT DATA. FOR INSTANCE:'
              WRITE(LU,*) '                NODE:',I,'WITH CN=',CN%R(I)
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!
!       CHECK THAT CN IS NOT NEGATIVE
          IF(CN%R(I).LT.0.D0) THEN
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'RUNOFF_SCS_CN : IL Y A AU MOINS UN NOEUD'
              WRITE(LU,*) '                AVEC CN NEGATIF DANS DONNEES'
              WRITE(LU,*) '                D''ENTREE. PAR EXEMPLE:'
              WRITE(LU,*) '                NOEUD:',I,'WITH CN=',CN%R(I)
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'RUNOFF_SCS_CN : AT LEAST ONE NODE WITH'
              WRITE(LU,*) '                NEGATIVE CN VALUE FOUND IN'
              WRITE(LU,*) '          INPUT DATA. FOR INSTANCE:'
              WRITE(LU,*) '                NODE:',I,'WITH CN=',CN%R(I)
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
!       COMPUTE CN DEPENDING ON THE CHOSEN AMC OPTION (GIVEN BY KEYWORD)
!       CN IS FORCED TO 100 MAXIMUM IN CASE OF STEEP SLOPE CORRECTION
!
        IF(AMC.EQ.1) THEN
          DO I=1,NPOIN
            CN%R(I)=4.2D0*MIN(100.D0,CN%R(I)*T10%R(I))/
     &             (10.D0 - 0.058D0 *
     &             MIN(100.D0,CN%R(I)*T10%R(I)))
          ENDDO
        ELSEIF(AMC.EQ.2) THEN
          DO I=1,NPOIN
            CN%R(I) = MIN(100.D0,CN%R(I)*T10%R(I))
          ENDDO
        ELSEIF(AMC.EQ.3) THEN
          DO I=1,NPOIN
            CN%R(I) = 23.D0 * MIN(100.D0,CN%R(I)*T10%R(I))/
     &               (10.D0 + 0.13D0* MIN(100.D0,CN%R(I)*T10%R(I)))
          ENDDO
        ELSE
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'RUNOFF_SCS_CN : OPTION AMC INCORRECTE: ',AMC
            WRITE(LU,*) '                CHOIX POSSIBLES: 1, 2 OU 3'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'RUNOFF_SCS_CN : INVALID AMC OPTION: ',AMC
            WRITE(LU,*) '                AVAILABLE OPTIONS: 1, 2 OR 3'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       POTENTIAL MAXIMAL RETENTION (POTMAXRET), M (STOCKED IN T5)
!       INITIAL ABSTRACTION IA, M (STOCKED IN T6)
!
        CC=25.4D0/1000.D0
!
        DO I=1,NPOIN
!          POTMAXRET(I) = 25.4D0*(1000.D0/CN%R(I)-10.D0)/1000.D0
!          IA(I) = POTMAXRET(I) * 0.2D0
           T5%R(I)=CC*(1000.D0/MAX(CN%R(I),EPS)-10.D0)
           T6%R(I)=0.2D0*T5%R(I)
        ENDDO
!
!
!-----------------------------------------------------------------------
!
!     ABSTRACTION CALCULATION
!     =======================
!
!     Description of the abstraction calculation (see reference)
!     ----------------------------------------------------------
!
!     In a first step, the (accumulated) rainfall volume ACCRF is
!     entirely stored in the "initial abstraction" reservoir IA, which
!     is defined by the method as 20% of the total maximal retention
!     POTMAXRET (see above).
!     While ACCRF =< IA, all the rainfall volume is stored in the ground
!     (in ACCIA) and there is no runoff.
!     In a second step, ie. when the accumulated rainfall ACCRF has
!     become larger than the initial abstraction IA, the rainfall volume
!     is divided in two parts:
!       1. A first part stored in the ground in a second abstraction
!          step called continuing abstraction (FA). Its accumulated
!          value, ACCFA, tends towards POTMAXRET when ACCRF tends to
!          infinity.
!       2. The remaining volume is not infiltrated and becomes direct
!          runoff (ACCROFF = ACCRF - ACCIA - ACCFA).
!
!
!     ACCUMULATED RAINFALL AT TIME AT (ACCRF), M (ACCRF STOCKED IN  T7)
!     ACCRF = ACCRF + RFM
      CALL OV('X=X+C   ',T7%R,T7%R,T7%R,RFM,NPOIN)
!
!     ACCUMULATED INITIAL ABSTRACTION AT TIME AT (ACCIA), M
!
      DO I=1,NPOIN
        IF(T7%R(I).LT.T6%R(I)) THEN !IF ACCRF<IA
!         ACCIA = ACCRF
          ACCIA(I) = T7%R(I)
        ELSE
!         ACCIA = IA
          ACCIA(I) = T6%R(I)
        ENDIF
      ENDDO
!
!     ACCUMULATED FA AT TIME AT (ACCFA), M
      DO I=1,NPOIN
        IF(T7%R(I).GT.T6%R(I)) THEN !IF ACCRF>IA
!         ACCFA = POTMAXRET * (ACCRF - IA) / (ACCRF - IA + POTMAXRET)
          ACCFA(I)=T5%R(I)*(T7%R(I)-T6%R(I))/
     &            (T7%R(I)-T6%R(I)+T5%R(I))
        ELSE
          ACCFA(I) = 0.D0
        ENDIF
      ENDDO
!
!     ACCUMULATED RUNOFF AT TIME AT (ACCROFF), M
!     ACCROFF = ACCRF (=T7) - ACCIA - ACCFA
      CALL OV('X=Y-Z   ',ACCROFF,T7%R,ACCIA,0.D0,NPOIN)
      CALL OV('X=X-Y   ',ACCROFF,ACCFA,ACCFA,0.D0,NPOIN)
!
!     HYETOGRAPH RAIN_MPS_GEO, M/S
      DO I=1,NPOIN
        RAIN_MPS_GEO = (ACCROFF(I) - ACCROF_OLD%R(I))/DT
        PLUIE%R(I)=MAX(RAIN_MPS_GEO,-MAX(HN%R(I),0.D0)/DT)
      ENDDO
!
!     ACCUMULATED RAINFALL PRINTED TO THE LISTING (INDEPENDENT OF NODE NUMBER)
      IF(ENTET) THEN
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'RUNOFF_SCS_CN : PLUIE BRUTE CUMULEE :',
     &                                                       T7%R(1),'M'
          WRITE(LU,*) ' '
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'RUNOFF_SCS_CN : ACCUMULATED RAINFALL :',
     &                                                       T7%R(1),'M'
          WRITE(LU,*) ' '
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     KEEP ACCROFF IN ACCROF_OLD
      CALL OV('X=Y     ',ACCROF_OLD%R,ACCROFF,ACCROFF,0.D0,NPOIN)
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
