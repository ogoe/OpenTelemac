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
!brief    RAINFALL/RUNOFF CALCULATION BASED ON THE SCS METHOD FOR
!+        ABSTRACTIONS (REFERENCE: APPLIED HYDROLOGY, 
!+        CHOW, MAIDMENT, MAYS, McGraw-Hill Publishing 1988).
!+        SPATIALLY VARIABLE CURVE NUMBER DEFINED ON THE MESH.
!+        RAINFALL DEFINED BY HYETOGRAPH
!
!
!history  PIERRE-LOUIS LIGIER (SWECO)
!+        27/05/2016
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACCFA          |<->| ACCFA
!| ACCIA          |<->| ACCIA 
!| ACCROF         |<->| ACCROF AT TIME AT
!| ACCROF_OLD     |<->| ACCROF OF LAST TIME STEP
!| AMC            |-->| ANTECEDENT MOISTURE CONDITIONS
!|                |-->| OPTION FOR ANTECEDENT MOISTURE CONDITIONS: 
!|                |-->|   +> 1: DRY ANTECEDENT MOISTURE CONDITIONS
!|                |-->|   +> 2: NORMAL ANTECEDENT MOISTURE CONDITIONS 
!|                |-->|   +> 3: WET ANTECEDENT MOISTURE CONDITIONS
!| CN             |-->| CURVE NUMBER
!| COUPLING       |-->| STRING WITH THE LIST OF COUPLED PROGRAMMES
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| FO2            |-->| LOGICAL UNIT OF THE FORMATTED DATA FILE
!| IELM           |-->| TYPE OF ELEMENT
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| PLUIE          |-->| BIEF_OBJ STRUCTURE WITH RAIN OR EVAPORATION.
!| RAIN_HDUR      |-->| RAIN DURATION IN HOURS
!| RAIN_MPS       |<->| RAIN OR EVAPORATION IN M PER SECONDS
!| ZF             |-->| BOTTOM ELEVATION
!| ZFSLOP         |<->| BOTTOM SLOPE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC,   ONLY: COUPLING
      USE DECLARATIONS_TELEMAC2D, ONLY: DT,LT,AT,HN,T5,T6,T7,
     &                                  ENTET,T8,T9,T10,DEBUG
      USE INTERFACE_TELEMAC2D, EX_RUNOFF_SCS_CN => RUNOFF_SCS_CN
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            , INTENT(IN)    :: NPOIN,AMC,FO2,IELM
      LOGICAL, INTENT(IN)                :: MSK
      DOUBLE PRECISION   , INTENT(IN)    :: RAIN_MPS,RAIN_HDUR
      DOUBLE PRECISION   , INTENT(INOUT) :: ACCIA(NPOIN),ACCFA(NPOIN)
      DOUBLE PRECISION   , INTENT(INOUT) :: ACCROFF(NPOIN)
      TYPE(BIEF_OBJ)     , INTENT(IN)    :: ZF,MASKEL
      TYPE(BIEF_OBJ)     , INTENT(INOUT) :: PLUIE,ACCROF_OLD,CN,ZFSLOP
      TYPE(BIEF_FILE), INTENT(IN)        :: FILES(*)
      TYPE(BIEF_MESH), INTENT(INOUT)     :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!     TO CHANGE TO KEYWORD, IF NECESSARY
      INTEGER, PARAMETER ::RAINDEF=1
      LOGICAL STEEPSLOPECOR
!
      DOUBLE PRECISION RAIN_MPS_GEO,PEAK_TIME,CC
      DOUBLE PRECISION, PARAMETER::EPS=1.E-6
      DOUBLE PRECISION A,B,C,R,RELT,IMMH,RFM,RF_HDUR
!
      IF(DEBUG.GT.0)WRITE(LU,*) 'IN RUNOFF_SCS_CN:  1'
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
!     TWO OPTIONS FOR RAINFALL DEFINITION:
!     -----------------------------------
!     +> 1: STANDARD RAINFALL (CONSTANT VALUE IN MM/DAY, KEYWORD)
!     +> 2: RAINFALL DEFINED BY HYETOGRAPH
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     1. STANDARD RAINFALL (CONSTANT VALUE IN MM/DAY)
!     ============================================
      IF(RAINDEF.EQ.1)THEN
!       RAINFALL AT TIME AT OVER ONE TIME-STEP, M
        RFM = RAIN_MPS * DT 
!
!     2. EXAMPLE: RAINFALL DEFINED BY HYETOGRAPH USING AN IDF CURVE 
!     ============================================
      ELSEIF(RAINDEF.EQ.2)THEN
!
!       EXAMPLE: IDF CURVE OF TYPE I = A / (T**B + C) MM/H
!       RAINFALL STARTS AT AT = 0.D0 SECONDS
!
!       RAIN DURATION, H
!        RAIN_HDUR = 6.D0
!       IDF CONSTANTS, MM/H
        A = 59.9916D0
        B = 0.9737D0
        C = 0.2235D0
!
        IF(LT.EQ.1) THEN
          RF_HDUR = RAIN_HDUR * A / (RAIN_HDUR**B + C)
          WRITE(LU,*) 'RUNOFF_SCS_CN: TOTAL RAINFALL VOLUME ACCORDING '
          WRITE(LU,*) 'TO USER DATA:', RF_HDUR, 'MM'
        ENDIF
!
!       PEAK DECENTERING PARAMETER R, 0. =< R =< 1. 
!       FOR SYMMETRICAL RAINFALL (PEAK AT RAIN_HDUR/2): R = 0.5
        R = 0.5D0
        PEAK_TIME=R*RAIN_HDUR*3600.D0
!
!       TIME RELATIVE TO PEAK, SECONDS
!       ***PLLI: CORRECTION TO AVOID DIVISION BY ZERO
        IF(AT.LT.(PEAK_TIME)) THEN
          RELT = ABS(AT-PEAK_TIME) / MAX(R,EPS)
        ELSE
          RELT = ABS(AT-PEAK_TIME) / MAX((1.D0-R),EPS)
        ENDIF
        IF(DEBUG.GT.0)WRITE(LU,*) 'IN RUNOFF_SCS_CN:  2'
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
!       END OF EXAMPLE
!
      ELSE
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
        WRITE(LU,*) '  RUNOFF_SCS_CN:'
        WRITE(LU,*) '  NEGATIVE RAINFALL FOUND AT TIME',AT
        WRITE(LU,*) '  EVAPORATION NOT SUPPORTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(DEBUG.GT.0)WRITE(LU,*) 'IN RUNOFF_SCS_CN:  3'
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
      IF(DEBUG.GT.0)WRITE(LU,*) 'IN RUNOFF_SCS_CN:  4'
!
!
!     - OPTION FOR STEEP SLOPE CORRECTION:
!       +> REFERENCE: Huang, Gallichand, Wang and Goulet. A modification 
!                     to the Soil Conservation Service curve number 
!                     method for steep slopes in the Loess Plateau of
!                     China. Hydrological Processes 20, 579-589 (2006).
!       +> CORRECTION FOR SLOPES BETWEEN 0.14 AND 1.4 M/M DEFINED BY A 
!          CORRECTION FACTOR CN2A/CN2 (VARIABLE TCN2A_CN2=10%R)
!       +> TERRAIN SLOPE STORED IN THE GEOMETRY FILE AS AN ADDITIONAL 
!          VARIABLE USING KEYWORD 'NAMES OF PRIVATE VARIABLES' (M/M)
!
!     TO ACTIVATE OPTION FOR STEEP SLOPE CORRECTION: STEEPSLOPECOR = .TRUE.
      STEEPSLOPECOR = .TRUE. !CAN BE A KEYWORD?
!
      IF(STEEPSLOPECOR) THEN
!
        IF(LT.EQ.1.OR.INCLUS(COUPLING,'SISYPHE'))THEN
!         COMPUTE THE BOTTOM SLOPE  
          CALL ZSLOPE(ZFSLOP,ZF,T8,T9,MSK,MASKEL,IELM,MESH)
!         COMPUTE CORRECTION OF SLOPE
          CC=(322.79D0+15.63D0*1.4D0)/(1.4D0+323.52D0)
          DO I=1,NPOIN
            IF(ZFSLOP%R(I).GE.0.14D0) THEN
              T10%R(I) = (322.79D0+15.63D0*ZFSLOP%R(I))
     &                    /(ZFSLOP%R(I)+323.52D0)
            ELSEIF(ZFSLOP%R(I).GT.1.4D0) THEN
              T10%R(I) = CC
            ELSE
              T10%R(I) = 1.D0
            ENDIF
          ENDDO
        ENDIF
      ELSE
!     CN2A_CN2 = 1 IF STEEPSLOPECOR = .FALSE.
        CALL OS('X=C     ',X=T10,C=1.D0)
      ENDIF
      IF(DEBUG.GT.0)WRITE(LU,*) 'IN RUNOFF_SCS_CN:  5'
!
!***********************************************************************
!
!       CHECK THAT CN IS NOT GREATER THAN 100
!       DO YOU NEED TO CHECK IF CN>0 OR NOT? ***PLLI: CN CAN BE EQUAL TO 0
!    (TO MODEL 100% INFILTRATION), BUT CANNOT BE NEGATIVE, SEE TEST BELOW.
        DO I=1,NPOIN
          IF((CN%R(I)-100.D0).GT.1.D-6) THEN
            WRITE(LU,*) '  RUNOFF_SCS_CN:'
            WRITE(LU,*) '  ERROR. AT LEAST ONE NODE WITH CN VALUE'
            WRITE(LU,*) '  GREATER THAN 100 FOUND IN GEOMETRY FILE.'
            WRITE(LU,*) '  FOR INSTANCE NODE:',I,'WITH CN=',CN%R(I)
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(CN%R(I).LT.0.D0) THEN
            WRITE(LU,*) '  RUNOFF_SCS_CN:'
            WRITE(LU,*) '  ERROR. AT LEAST ONE NODE WITH NEGATIVE'
            WRITE(LU,*) '  CN VALUE FOUND IN GEOMETRY FILE.'
            WRITE(LU,*) '  FOR INSTANCE NODE:',I,'WITH CN=',CN%R(I)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
!       COMPUTE CN DEPENDING ON THE CHOSEN AMC OPTION
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
          WRITE(LU,*) '  RUNOFF_SCS_CN:'
          WRITE(LU,*) '  INVALID AMC OPTION: ',AMC
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       POTENTIAL MAXIMAL RETENTION (POTMAXRET), M (STOCKED IN T5)
!       INITIAL ABSTRACTION IA, M (STOCKED IN T6)
!       SOME OPTIMIZATIONS
        CC=25.4D0/1000.D0
!       
        DO I=1,NPOIN
!          POTMAXRET(I) = 25.4D0*(1000.D0/CN%R(I)-10.D0)/1000.D0
!          IA(I) = POTMAXRET(I) * 0.2D0
           T5%R(I)=CC*(1000.D0/MAX(CN%R(I),EPS)-10.D0)
           T6%R(I)=0.2D0*T5%R(I)
        ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*) 'IN RUNOFF_SCS_CN:  6'
!
!-----------------------------------------------------------------------
!
!     ABSTRACTION CALCULATION 
!     =======================
!
!     ACCUMULATED RAINFALL AT TIME AT (ACCRF), M (***PLLI: STOCKED IN  T7)
!     ACCRF = ACCRF + RFM
      CALL OV('X=X+C   ',T7%R,T7%R,T7%R,RFM,NPOIN) 
!
!     ACCUMULATED INITIAL ABSTRACTION AT TIME AT (ACCIA) M  (ACCIA STOCKED IN T5)
!      I DO NOT UNDERSTAND THE FOLLOWING SECTION?
!
!     PLLI: explanation of the abstraction method (see the reference I sent for a more complete description)
!     In a first step, the (accumulated) rainfall volume is entirely stored in the
!     "initial abstraction" reservoir IA, which is defined by the method as 20% of the total maximal 
!     retention POTMAXRET.
!     So while ACCRF =< IA, all the rainfall volume is stored in the ground (in ACCIA) and there is no runoff.
!     In a second step, ie. when the accumulated rainfall ACCRF has become larger than the initial
!     abstraction IA, the rainfall volume is divided in two parts:
!       - a first one stored in the ground using a second abstraction method called "FA". 
!         This abstraction FA tends towards POTMAXRET when accumulated rainfall tends to infinity
!       - the remaining volume is not infiltrated and becomes direct runoff (ACCROFF = ACCRF - ACCIA - ACCFA)
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
       IF(LNG.EQ.1)THEN
         WRITE(LU,*) 'RUNOFF_SCS_CN: PLUIE ACCUMULEE (M3)', T7%R(1)
       ELSEIF(LNG.EQ.2)THEN
         WRITE(LU,*) 'RUNOFF_SCS_CN: ACCUMULATED RAINFALL (M3)', T7%R(1)
       ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     KEEP ACCROFF IN ACCROFF_OLD
      CALL OV('X=Y     ',ACCROF_OLD%R,ACCROFF,ACCROFF,0.D0,NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*) 'IN RUNOFF_SCS_CN:  9'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
