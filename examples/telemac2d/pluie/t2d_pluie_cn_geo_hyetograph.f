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
      USE DECLARATIONS_TELEMAC2D, ONLY: DT,LT,AT,HN,T5,T6,T7,T8,T9,T10,
     &                                  IASCNOPT,ENTET,T2DFO1,PRIVE
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
!********** SPECIFIC TO THIS CASE
      INTEGER, PARAMETER ::RAINDEF=3
!********** END OF SPECIFIC TO THIS CASE
      LOGICAL STEEPSLOPECOR
!
      DOUBLE PRECISION RAIN_MPS_GEO,PEAK_TIME,CC,IA_S
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
!     RAINFALL DEFINITION OPTION NOT IMPLEMENTED
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
!********** SPECIFIC TO THIS CASE
!      IF(LT.EQ.1)THEN
!        CALL HYDROMAP(CN%R,MESH%X%R,MESH%Y%R,MESH%NPOIN,FILES(FO2)%LU,
!     &                MESH%NBOR%I,MESH%KP1BOR%I,MESH%NPTFR)
!      ENDIF
!********** END SPECIFIC TO THIS CASE
!
!       NOTE: CN VALUE FOR NORMAL ANTECEDENT MOISTURE CONDITIONS (CN2)
!       ****  CAN ALSO BE READ FROM A USER VARIABLE STORED IN THE
!             GEOMETRY FILE USING THE FOLLOWING KEYWORDS:
!              +>  NUMBER OF PRIVATE VARIABLES
!              +>  NAMES OF PRIVATE VARIABLES
!             IN THE EXAMPLE BELOW CN IS READ FROM PRIVE%ADR(1)%P%R:
!
!********** SPECIFIC TO THIS CASE
      IF(LT.EQ.1) THEN
        CALL OV('X=Y     ',CN%R,PRIVE%ADR(1)%P%R,PRIVE%ADR(1)%P%R,
     &                     0.D0,NPOIN)
      ENDIF
!********** END OF SPECIFIC TO THIS CASE
!
!     CHECK THAT CN IS NOT GREATER THAN 100
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
!     CHECK THAT CN IS NOT NEGATIVE
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
            WRITE(LU,*) '                INPUT DATA. FOR INSTANCE:'
            WRITE(LU,*) '                NODE:',I,'WITH CN=',CN%R(I)
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!     - OPTION FOR STEEP SLOPE CORRECTION:
!       +> REFERENCE: Huang, Gallichand, Wang and Goulet. A modification
!                     to the Soil Conservation Service curve number
!                     method for steep slopes in the Loess Plateau of
!                     China. Hydrological Processes 20, 579-589 (2006).
!       +> CORRECTION FOR SLOPES BETWEEN 0.14 AND 1.4 M/M DEFINED BY A
!          CORRECTION FACTOR CN2A/CN2 (VARIABLE TCN2A_CN2=T10%R)
!       +> TERRAIN SLOPE (M/M) COMPUTED BY SUBROUTINE ZSLOPE
!       +> WARNING: THE STEEP SLOPE CORRECTION IS PERFORMED AT THE
!          *******  BEGINNING OF THE COMPUTATION ONLY - DOES NOT TAKE
!                   INTO ACOUNT TERRAIN EVOLUTIONS IN CASE OF COUPLING
!                   WITH SISYPHE.
!
!     TO ACTIVATE OPTION FOR STEEP SLOPE CORRECTION: STEEPSLOPECOR = .TRUE.
      STEEPSLOPECOR = .FALSE. !CAN BE A KEYWORD?
!
      IF(STEEPSLOPECOR) THEN
!
        IF(LT.EQ.1)THEN
!        IF(LT.EQ.1.OR.INCLUS(COUPLING,'SISYPHE'))THEN
!        PL: CORRECTION IN CASE OF COUPLING REMOVED! IN THAT CASE
!            THE CORRECTION MUST BE DONE ON THE INITIAL CN VALUE,
!            NOT ON THE VALUE AT PREVIOUS DT
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
!     - OPTION FOR INITIAL ABSTRACTION RATIO:
!       +> REFERENCE: Woodward, Hawkins, Jiang, Hjelmfelt, Van Mullem
!                     and Quan. Runoff Curve Number Method: Examination
!                     of the initial abstraction ratio. World Water and
!                     Environmental Resources Congress 2003.
!       +> TWO OPTIONS DEFINED IN KEYWORD 'OPTION FOR INITIAL ABSTRACTION
!          RATIO':
!          - OPTION 1: IA/S = 0.2 (STANDARD METHOD) - DEFAULT
!          - OPTION 2: IA/S = 0.05 (FROM ABOVE REFERENCE) WITH
!                      AUTOMATIC CONVERSION OF CN COEFFICIENTS (INPUT CN
!                      VALUES MUST BE GIVEN ACCORDING TO THE STANDARD
!                      METHOD)
!
      IF(IASCNOPT.EQ.1) THEN
        IA_S = 0.2D0
      ELSEIF(IASCNOPT.EQ.2) THEN
        IA_S = 0.05D0
        IF(LT.EQ.1) THEN
          DO I=1,NPOIN
            CN%R(I) = 100.D0 / (1.879D0*
     &                (100.D0/MAX(CN%R(I),EPS)-1.D0)**1.15D0+1.D0)
          ENDDO
        ENDIF
      ELSE
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'RUNOFF_SCS_CN : OPTION POUR RATIO DES PERTES'
          WRITE(LU,*) '                INITIALES INCORRECTE: ', IASCNOPT
          WRITE(LU,*) '                CHOIX POSSIBLES: 1 OU 2'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'RUNOFF_SCS_CN : INVALID OPTION FOR INITIAL'
          WRITE(LU,*) '                ABSTRACTION RATIO: ', IASCNOPT
          WRITE(LU,*) '                AVAILABLE OPTIONS: 1 OR 2'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!***********************************************************************
!
!     COMPUTE CN DEPENDING ON THE CHOSEN AMC OPTION (GIVEN BY KEYWORD)
!     CN IS FORCED TO 100 MAXIMUM IN CASE OF STEEP SLOPE CORRECTION
!
      IF(LT.EQ.1) THEN
!
      IF(AMC.EQ.1) THEN
        DO I=1,NPOIN
          CN%R(I)=4.2D0*MIN(100.D0,CN%R(I)*T10%R(I))/
     &           (10.D0 - 0.058D0 *
     &           MIN(100.D0,CN%R(I)*T10%R(I)))
        ENDDO
      ELSEIF(AMC.EQ.2) THEN
        DO I=1,NPOIN
          CN%R(I) = MIN(100.D0,CN%R(I)*T10%R(I))
        ENDDO
      ELSEIF(AMC.EQ.3) THEN
        DO I=1,NPOIN
          CN%R(I) = 23.D0 * MIN(100.D0,CN%R(I)*T10%R(I))/
     &             (10.D0 + 0.13D0* MIN(100.D0,CN%R(I)*T10%R(I)))
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
      ENDIF
!
!     POTENTIAL MAXIMAL RETENTION (POTMAXRET), M (STOCKED IN T5)
!     INITIAL ABSTRACTION IA, M (STOCKED IN T6)
!
      CC=25.4D0/1000.D0
!
      DO I=1,NPOIN
!       POTMAXRET(I) = 25.4D0*(1000.D0/CN%R(I)-10.D0)/1000.D0
!       IA(I) = POTMAXRET(I) * IA_S
        T5%R(I)=CC*(1000.D0/MAX(CN%R(I),EPS)-10.D0)
        T6%R(I)=IA_S*T5%R(I)
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
          WRITE(LU,40)T7%R(1)
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,50)T7%R(1)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!*****VALIDATION: SAVING PARAMETERS TO RESULTS FILE**********
      DO I=1,NPOIN
!       COEFFICIENT CN        => USER VARIABLE 'O' (NO. 24)
        PRIVE%ADR(2)%P%R(I)=CN%R(I)
!       ACCUMULATED RAINFALL  => USER VARIABLE 'R' (NO. 25)
        PRIVE%ADR(3)%P%R(I)=T7%R(I)
!       ACCUMULATED RUNOFF    => USER VARIABLE 'Z' (NO. 26)
        PRIVE%ADR(4)%P%R(I)=ACCROFF(I)
      ENDDO
!************************************************************
!     KEEP ACCROFF IN ACCROF_OLD
      CALL OV('X=Y     ',ACCROF_OLD%R,ACCROFF,ACCROFF,0.D0,NPOIN)
!
!-----------------------------------------------------------------------
!
40    FORMAT(/,80('-'),/,5X,'RUNOFF_SCS_CN : PLUIE BRUTE CUMULEE : ',
     &        G16.7,' M'/,80('-'),/)
50    FORMAT(/,80('-'),/,5X,'RUNOFF_SCS_CN : ACCUMULATED RAINFALL : ',
     &        G16.7,' M'/,80('-'),/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ***************************
                     SUBROUTINE NOMVAR_TELEMAC2D
!                    ***************************
!
     &(TEXTE,TEXTPR,MNEMO,NPERIAF,NTRAC,NAMETRAC,N_NAMES_PRIV,
     & NAMES_PRIVE,SECCURRENTS,NADVAR,NAMES_ADVAR)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (IN TEXTPR).
!+
!+                TEXTE AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!history  J-M HERVOUET (LNHE)
!+        31/08/2007
!+        V5P8
!+   First version.
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
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Secondary flow correction: add variables
!+   tau_s, Omega/h and r^{-1} for visualization
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/07/2015
!+        V7P1
!+   Now taking into account names of private arrays given by user.
!
!history J-M HERVOUET (EDF LAB, LNHE)
!+        20/07/2016
!+        V7P2
!+   Elder model of turbulence is asked, the name of viscosity is
!+   replaced by longitudinal dispersion.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO          |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| N_NAMES_PRIV   |-->| NUMBER OF NAMES OF PRIVATE VARIABLES GIVEN
!| NAMES_PRIVE    |-->| NAME OF PRIVATE VARIABLES GIVEN BY USER
!| NAMETRAC       |-->| NAME OF TRACERS (GIVEN BY KEYWORDS)
!| NPERIAF        |-->| NUMBER OF PERIODS FOR FOURRIER ANALYSIS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TEXTE          |<--| SEE ABOVE
!| TEXTPR         |<--| SEE ABOVE
!| SECCURRENTS    |-->| IF YES SECONDARY CURRENTS COMPUTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY : IND_SEC,ITURB
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPERIAF,NTRAC,N_NAMES_PRIV
      INTEGER, INTENT(IN)              :: NADVAR
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(*),NAMES_PRIVE(4)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMES_ADVAR(*)
      LOGICAL, INTENT(IN)              :: SECCURRENTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2) I_IN_2_LETTERS(34)
      DATA I_IN_2_LETTERS /'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ',
     &                     '10','11','12','13','14','15','16','17','18',
     &                     '19','20','21','22','23','24','25','26','27',
     &                     '28','29','30','31','32','33','34'/
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.2) THEN
!
      TEXTE (1 ) = 'VELOCITY U      M/S             '
      TEXTE (2 ) = 'VELOCITY V      M/S             '
      TEXTE (3 ) = 'CELERITY        M/S             '
      TEXTE (4 ) = 'WATER DEPTH     M               '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'FROUDE NUMBER                   '
      TEXTE (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTE (9 ) = 'EX TRACER                       '
      TEXTE (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTE (12) = 'LONG. DISPERSIONM2/S            '
      ELSE
        TEXTE (12) = 'VISCOSITY       M2/S            '
      ENDIF
      TEXTE (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTE (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTE (15) = 'SCALAR VELOCITY M/S             '
      TEXTE (16) = 'WIND ALONG X    M/S             '
      TEXTE (17) = 'WIND ALONG Y    M/S             '
      TEXTE (18) = 'AIR PRESSURE    PASCAL          '
      TEXTE (19) = 'BOTTOM FRICTION                 '
      TEXTE (20) = 'DRIFT ALONG X   M               '
      TEXTE (21) = 'DRIFT ALONG Y   M               '
      TEXTE (22) = 'COURANT NUMBER                  '
      TEXTE (23) = 'VARIABLE 23     UNIT   ??       '
      TEXTE (24) = 'CN COEFFICIENT                  '
      TEXTE (25) = 'ACC. RAINFALL   M               '
      TEXTE (26) = 'ACC. RUNOFF     M               '
      TEXTE (27) = 'HIGH WATER MARK M               '
      TEXTE (28) = 'HIGH WATER TIME S               '
      TEXTE (29) = 'HIGHEST VELOCITYM/S             '
      TEXTE (30) = 'TIME OF HIGH VELS               '
      TEXTE (31) = 'FRICTION VEL.   M/S             '
      TEXTE (32) = 'TAU_S           NA              '
      TEXTE (33) = '1/R             1/M             '
!
! TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
! FROM ANOTHER CODE WITH DIFFERENT VARIABLE NAMES, WHICH MUST
! BE GIVEN HERE:
!
      TEXTPR (1 ) = 'VELOCITY U      M/S             '
      TEXTPR (2 ) = 'VELOCITY V      M/S             '
      TEXTPR (3 ) = 'CELERITY        M/S             '
      TEXTPR (4 ) = 'WATER DEPTH     M               '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'FROUDE NUMBER                   '
      TEXTPR (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTPR (9 ) = 'EX TRACER                       '
      TEXTPR (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTPR(12) = 'LONG. DISPERSIONM2/S            '
      ELSE
        TEXTPR(12) = 'VISCOSITY       M2/S            '
      ENDIF
      TEXTPR (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTPR (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTPR (15) = 'SCALAR VELOCITY M/S             '
      TEXTPR (16) = 'WIND ALONG X    M/S             '
      TEXTPR (17) = 'WIND ALONG Y    M/S             '
      TEXTPR (18) = 'AIR PRESSURE    PASCAL          '
      TEXTPR (19) = 'BOTTOM FRICTION                 '
      TEXTPR (20) = 'DRIFT ALONG X   M               '
      TEXTPR (21) = 'DRIFT ALONG Y   M               '
      TEXTPR (22) = 'COURANT NUMBER                  '
      TEXTPR (23) = 'VARIABLE 23     UNIT   ??       '
      TEXTPR (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTPR (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTPR (26) = 'VARIABLE 26     UNIT   ??       '
      TEXTPR (27) = 'HIGH WATER MARK M               '
      TEXTPR (28) = 'HIGH WATER TIME S               '
      TEXTPR (29) = 'HIGHEST VELOCITYM/S             '
      TEXTPR (30) = 'TIME OF HIGH VELS               '
      TEXTPR (31) = 'FRICTION VEL.   M/S             '
      TEXTPR (32) = 'TAU_S           NA              '
      TEXTPR (33) = '1/R             1/M             '
!
!-----------------------------------------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
      TEXTE (1 ) = 'VITESSE U       M/S             '
      TEXTE (2 ) = 'VITESSE V       M/S             '
      TEXTE (3 ) = 'CELERITE        M/S             '
      TEXTE (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'FROUDE                          '
      TEXTE (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTE (9 ) = 'EX TRACEUR                      '
      TEXTE (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTE (12) = 'DISPERSION LONG.M2/S            '
      ELSE
        TEXTE (12) = 'VISCOSITE TURB. M2/S            '
      ENDIF
      TEXTE (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTE (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTE (15) = 'VITESSE SCALAIREM/S             '
      TEXTE (16) = 'VENT X          M/S             '
      TEXTE (17) = 'VENT Y          M/S             '
      TEXTE (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTE (19) = 'FROTTEMENT                      '
      TEXTE (20) = 'DERIVE EN X     M               '
      TEXTE (21) = 'DERIVE EN Y     M               '
      TEXTE (22) = 'NBRE DE COURANT                 '
      TEXTE (23) = 'VARIABLE 23     UNITES ??       '
      TEXTE (24) = 'COEFFICIENT CN                  '
      TEXTE (25) = 'PLUIE CUMULEE   M               '
      TEXTE (26) = 'RUISSELL. CUMULEM               '
      TEXTE (27) = 'COTE MAXIMUM    M               '
      TEXTE (28) = 'TEMPS COTE MAXI S               '
      TEXTE (29) = 'VITESSE MAXIMUM M/S             '
      TEXTE (30) = 'T VITESSE MAXI  S               '
      TEXTE (31) = 'VITESSE DE FROT.M/S             '
      TEXTE (32) = 'TAU_S           NA              '
      TEXTE (33) = '1/R             1/M             '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
      TEXTPR (1 ) = 'VITESSE U       M/S             '
      TEXTPR (2 ) = 'VITESSE V       M/S             '
      TEXTPR (3 ) = 'CELERITE        M/S             '
      TEXTPR (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'FROUDE                          '
      TEXTPR (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTPR (9 ) = 'EX TRACEUR                      '
      TEXTPR (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      IF(ITURB.EQ.2) THEN
        TEXTPR(12) = 'DISPERSION LONG.M2/S            '
      ELSE
        TEXTPR(12) = 'VISCOSITE TURB. M2/S            '
      ENDIF
      TEXTPR (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTPR (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTPR (15) = 'VITESSE SCALAIREM/S             '
      TEXTPR (16) = 'VENT X          M/S             '
      TEXTPR (17) = 'VENT Y          M/S             '
      TEXTPR (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTPR (19) = 'FROTTEMENT                      '
      TEXTPR (20) = 'DERIVE EN X     M               '
      TEXTPR (21) = 'DERIVE EN Y     M               '
      TEXTPR (22) = 'NBRE DE COURANT                 '
      TEXTPR (23) = 'VARIABLE 23     UNITES ??       '
      TEXTPR (24) = 'VARIABLE 24     UNITES ??       '
      TEXTPR (25) = 'VARIABLE 25     UNITES ??       '
      TEXTPR (26) = 'VARIABLE 26     UNITES ??       '
      TEXTPR (27) = 'COTE MAXIMUM    M               '
      TEXTPR (28) = 'TEMPS COTE MAXI S               '
      TEXTPR (29) = 'VITESSE MAXIMUM M/S             '
      TEXTPR (30) = 'T VITESSE MAXI  S               '
      TEXTPR (31) = 'VITESSE DE FROT.M/S             '
      TEXTPR (32) = 'TAU_S           NA              '
      TEXTPR (33) = '1/R             1/M             '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIASES FOR THE VARIABLES IN THE STEERING FILE
!
!     UVCHSBFQTKEDIJMXYPWAGLNORZ
!     VELOCITY COMPONENT U
      MNEMO(1)   = 'U       '
!     VELOCITY COMPONENT V
      MNEMO(2)   = 'V       '
!     CELERITY
      MNEMO(3)   = 'C       '
!     WATER DEPTH
      MNEMO(4)   = 'H       '
!     FREE SURFACE ELEVATION
      MNEMO(5)   = 'S       '
!     BOTTOM ELEVATION
      MNEMO(6)   = 'B       '
!     FROUDE
      MNEMO(7)   = 'F       '
!     FLOW RATE
      MNEMO(8)   = 'Q       '
!     EX TRACER
      MNEMO(9)   = '?       '
!     TURBULENT ENERGY
      MNEMO(10)   = 'K       '
!     DISSIPATION
      MNEMO(11)   = 'E       '
!     TURBULENT VISCOSITY
      MNEMO(12)   = 'D       '
!     FLOWRATE ALONG X
      MNEMO(13)   = 'I       '
!     FLOWRATE ALONG Y
      MNEMO(14)   = 'J       '
!     SPEED
      MNEMO(15)   = 'M       '
!     WIND COMPONENT X
      MNEMO(16)   = 'X       '
!     WIND COMPONENT Y
      MNEMO(17)   = 'Y       '
!     ATMOSPHERIC PRESSURE
      MNEMO(18)   = 'P       '
!     FRICTION
      MNEMO(19)   = 'W       '
!     DRIFT IN X
      MNEMO(20)   = 'A       '
!     DRIFT IN Y
      MNEMO(21)   = 'G       '
!     COURANT NUMBER
      MNEMO(22)   = 'L       '
!     VARIABLE 23
      MNEMO(23)   = 'N       '
!     VARIABLE 24
      MNEMO(24)   = 'O       '
!     VARIABLE 25
      MNEMO(25)   = 'R       '
!     VARIABLE 26
      MNEMO(26)   = 'Z       '
!     VARIABLE 27
      MNEMO(27)   = 'MAXZ    '
!     VARIABLE 28
      MNEMO(28)   = 'TMXZ    '
!     VARIABLE 29
      MNEMO(29)   = 'MAXV    '
!     VARIABLE 30
      MNEMO(30)   = 'TMXV    '
!     VARIABLE 31
      MNEMO(31)   = 'US      '
!
      MNEMO(32)   = 'TAU_S   '
!
      MNEMO(33)   = '1/R     '
!
!-----------------------------------------------------------------------
!
!     FOURIER ANALYSES
!
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
          IF(LNG.EQ.1) THEN
            TEXTE(34+NTRAC+2*(I-1)) =  'AMPLI PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTE(35+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
            TEXTPR(34+NTRAC+2*(I-1)) =  'AMPLI PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTPR(35+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
          ELSE
            TEXTE(34+NTRAC+2*(I-1)) =  'AMPLI PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTE(35+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
            TEXTPR(34+NTRAC+2*(I-1)) =  'AMPLI PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTPR(35+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
          ENDIF
          MNEMO(34+NTRAC+2*(I-1)) = 'AMPL'//I_IN_2_LETTERS(I)//'  '
          MNEMO(35+NTRAC+2*(I-1)) = 'PHAS'//I_IN_2_LETTERS(I)//'  '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          TEXTE(33+I)  = NAMETRAC(I)
          TEXTPR(33+I) = NAMETRAC(I)
          MNEMO(33+I)  = 'T'//I_IN_2_LETTERS(I)//'   '
        ENDDO
!       OMEGA FOR SECONDARY CURRENTS
        IF(SECCURRENTS) THEN
          TEXTE(33+IND_SEC) = NAMETRAC(IND_SEC)
          TEXTPR(33+IND_SEC)= NAMETRAC(IND_SEC)
          MNEMO(33+IND_SEC) = 'OMEGA   '
        ENDIF
      ENDIF
      IF(N_NAMES_PRIV.GT.0) THEN
        DO I=1,N_NAMES_PRIV
          TEXTE(22+I)  = NAMES_PRIVE(I)
          TEXTPR(22+I) = NAMES_PRIVE(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
