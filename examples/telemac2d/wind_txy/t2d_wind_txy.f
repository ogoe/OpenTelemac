!                    ****************
                     SUBROUTINE METEO
!                    ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,X,Y,AT,LT,NPOIN,VENT,ATMOS,
     & HN,TRA01,GRAV,ROEAU,NORD,PRIVE,FO1,FILES,LISTIN,PATMOS_VALUE,
     & AWATER_QUALITY,PLUIE,AOPTWIND,AWIND_SPD)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES ATMOSPHERIC PRESSURE AND WIND VELOCITY FIELDS
!+               (IN GENERAL FROM INPUT DATA FILES).
!
!warning  CAN BE ADAPTED BY USER
!
!history  J-M HERVOUET (LNHE)
!+        02/01/2004
!+        V5P4
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        30/01/2013
!+        V6P3
!+   Now 2 options with an example for reading a file. Extra arguments.
!
!history  C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Reading a file of meteo data for exchange with atmosphere
!+   Only the wind is used here
!
!history R.ATA (LNHE)
!+        09/11/2014
!+        V7P0
!+  introducion of water quality option + pluie is introduced as
!+   an optional parameter + remove of my_option which is replaced
!+   by a new keyword + value of patmos managed also with a new keyword
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        07/01/2015
!+        V7P0
!+  Adding optional arguments to remove USE DECLARATIONS_TELEMAC2D.
!
!history R.ATA (LNHE)
!+        16/11/2015
!+        V7P0
!+  Adding USE WAQTEL...
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        16/02/2015
!+        V7P0
!+   Shifting the stations coordinates removed in case of wind varying
!+   in time and space (option 99). Managing the divisions by 0 is now
!+   done by subroutine IDWM_T2D, and this does not spoil parallelism.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| ATMOS          |-->| YES IF PRESSURE TAKEN INTO ACCOUNT
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!| FO1            |-->| LOGICAL UNIT OF THE FORMATTED DATA FILE
!| FUAIR          |<->| VELOCITY OF WIND ALONG X, IF CONSTANT
!| FVAIR          |<->| VELOCITY OF WIND ALONG Y, IF CONSTANT
!| GRAV           |-->| GRAVITY ACCELERATION
!| HN             |-->| DEPTH
!| LISTIN         |-->| IF YES, PRINTS INFORMATION
!| LT             |-->| ITERATION NUMBER
!| NORD           |-->| DIRECTION OF NORTH, COUNTER-CLOCK-WISE
!|                |   | STARTING FROM VERTICAL AXIS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PATMOS         |<--| ATMOSPHERIC PRESSURE
!| PRIVE          |-->| USER WORKING ARRAYS (BIEF_OBJ BLOCK)
!| ROEAU          |-->| WATER DENSITY
!| TRA01          |-->| WORKING ARRAY
!| VENT           |-->| YES IF WIND TAKEN INTO ACCOUNT
!| WINDX          |<--| FIRST COMPONENT OF WIND VELOCITY
!| WINDY          |<--| SECOND COMPONENT OF WIND VELOCITY
!| X              |-->| ABSCISSAE OF POINTS
!| Y              |-->| ORDINATES OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY: PVAP,RAY3,NWIND,NEBU,TAIR,
     &                              HREL,RAINFALL,EVAPORATION,ATMOSEXCH
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: LT,NPOIN,FO1
      LOGICAL, INTENT(IN)             :: ATMOS,VENT,LISTIN
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: PATMOS(NPOIN),TRA01(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AT,GRAV,ROEAU,NORD,PATMOS_VALUE
      DOUBLE PRECISION, INTENT(INOUT) :: FUAIR,FVAIR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PRIVE
      TYPE(BIEF_FILE), INTENT(IN)     :: FILES(*)
!     OPTIONAL
      LOGICAL, INTENT(IN)          ,OPTIONAL :: AWATER_QUALITY
      TYPE(BIEF_OBJ), INTENT(INOUT),OPTIONAL :: PLUIE
      INTEGER, INTENT(IN)          ,OPTIONAL :: AOPTWIND
      DOUBLE PRECISION, INTENT(IN) ,OPTIONAL :: AWIND_SPD(2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL WATER_QUALITY
      INTEGER UL,OPTWIND
      DOUBLE PRECISION AT1,AT2,FUAIR1,FUAIR2,FVAIR1,FVAIR2,COEF
      DOUBLE PRECISION UAIR,VAIR,WIND_SPD(2)
!     EXCHANGE WITH ATMOSPHERE
      DOUBLE PRECISION PATM,WW,PI
!
      DOUBLE PRECISION, PARAMETER :: EPS = 1.D-3
!
! ######################################################################
! IDWM WIND INTERPOLATION CUSTOM VARIABLES
! ######################################################################
!
      INTEGER I, NUMSTA, NUMPOINTS, A, B, J, K, JUNK
      DOUBLE PRECISION THETA_RAD
!
!     COORDINATES OF THE STATIONS UTM
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XX, YY, AT_WIND
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: OUT_WSPD, OUT_WDIR
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: WIND, POINTS
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INPSTA_S
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: INPSTA_D
!
! ######################################################################
!
!-----------------------------------------------------------------------
!
!     DATA THAT YOU DECLARE AND READ HERE ONCE IN A FILE MAY HAVE TO BE
!     KEPT BECAUSE THIS SUBROUTINE IS CALLED AT EVERY TIME STEP.
!     WITHOUT THE SAVE COMMAND, ALL LOCAL DATA ARE FORGOTTEN IN THE NEXT
!     CALL.
!
      SAVE
!
!-----------------------------------------------------------------------
!
!     DEFAULT VALUES OF PARAMETERS WHEN THEY ARE NOT GIVEN
!
      WATER_QUALITY=.FALSE.
      IF(PRESENT(AWATER_QUALITY)) WATER_QUALITY=AWATER_QUALITY
      OPTWIND=1
      IF(PRESENT(AOPTWIND)) OPTWIND=AOPTWIND
      WIND_SPD(1)=0.D0
      WIND_SPD(2)=0.D0
      IF(PRESENT(AWIND_SPD)) THEN
        WIND_SPD(1)=AWIND_SPD(1)
        WIND_SPD(2)=AWIND_SPD(2)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     AT FIRST TIMESTEP
!
      IF(LT.EQ.0) THEN
!
        UL = FILES(FO1)%LU
        PI = ACOS(-1.D0)
!
!       ATMOSPHERIC PRESSURE
!
        IF(ATMOS.OR.WATER_QUALITY) THEN
          CALL OV( 'X=C     ' , PATMOS,Y,Y,PATMOS_VALUE,NPOIN )
        ENDIF
!
!       WIND :
!
        IF(VENT.OR.WATER_QUALITY) THEN
          IF(OPTWIND.EQ.1)THEN
!           IN THIS CASE THE WIND IS CONSTANT, VALUE GIVEN IN STEERING FILE.
            CALL OV( 'X=C     ' ,WINDX,WINDX,WINDX, FUAIR , NPOIN )
            CALL OV( 'X=C     ' ,WINDY,WINDY,WINDY, FVAIR , NPOIN )
          ELSEIF(OPTWIND.EQ.2) THEN
!           JUMPING TWO LINES OF COMMENTS
            READ(UL,*)
            READ(UL,*)
!           READING THE FIRST TWO LINES OF DATA
            READ(UL,*) AT1,FUAIR1,FVAIR1
            IF(AT.LT.AT1) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) 'METEO'
              IF(LNG.EQ.1) WRITE(LU,*) 'DEBUT TARDIF DU FICHIER DE VENT'
              IF(LNG.EQ.2) WRITE(LU,*) 'LATE BEGINNING OF THE WIND FILE'
              CALL PLANTE(1)
              STOP
            ENDIF
!
! ######################################################################
! IDWM WIND INTERPOLATION; THIS IS EXECUTED ONLY ONCE AT THE START
! ######################################################################
!
          ELSEIF(OPTWIND.EQ.3) THEN
        ! READ BLANK LINE AT BEGINING OF FILE
            READ(UL,*)
        ! READ NUMSTA AND NUMPOINTS
            READ(UL,*) NUMSTA, NUMPOINTS

          !ALLOCATE THE ARRAYS
            ALLOCATE(XX(NUMSTA), YY(NUMSTA), AT_WIND(NUMPOINTS))
            ALLOCATE(WIND(NUMPOINTS,NUMSTA*2+5), POINTS(NPOIN,2))
            ALLOCATE(INPSTA_S(NUMSTA,3), INPSTA_D(NUMSTA,3))
            ALLOCATE(OUT_WSPD(NPOIN), OUT_WDIR(NPOIN))
!
          ! READ STATION COORDINATES
            DO B = 1,NUMSTA
            READ(UL,*) XX(B), YY(B)
           !WRITE(*,*) XX(B), YY(B)
            ENDDO
!
          ! READ THE WIND TIME SERIES FROM THE INPUT FILE
          ! FIRST COLUMN IS TIME IN SECONDS, REST OF COLUMNS ARE WSPD
          ! AND WDIR FOR EACH STATION READ
            DO A = 1,NUMPOINTS
              READ(UL,*) (WIND(A,B), B=1,NUMSTA*2+1)
            ENDDO
!
          ! EXTRACT AT_WIND FROM WIND(A,B); FIRST COLUMN IS TIME IN SECONDS
            DO A = 1,NUMPOINTS
              AT_WIND(A) = WIND(A,1)
            ENDDO
!
          ! ASSEMBLE THE POINTS ARRAY FOR IDWM FUNCTION
            DO I = 1,NPOIN
              POINTS(I,1) = X(I)
              POINTS(I,2) = Y(I)
            ENDDO
!
! #######################################################################
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR THE REMAINING TIME STEPS
!
      IF(VENT.OR.WATER_QUALITY) THEN
!
!       WATER QUALITY
!
        IF(WATER_QUALITY.AND.FILES(FO1)%NAME(1:1).NE.' ')THEN
          IF(ATMOSEXCH.EQ.0)THEN
            CALL INTERPMETEO2(NWIND,UAIR,VAIR,TAIR,PATM,NEBU,RAINFALL,
     &                        PVAP,RAY3,AT,UL)
!
            CALL OV('X=C     ',WINDX,WINDX,WINDX,UAIR,NPOIN)
            CALL OV('X=C     ',WINDY,WINDY,WINDY,VAIR,NPOIN)
            CALL OV('X=C     ',PATMOS,PATMOS,PATMOS,PATM,NPOIN)
            IF(PRESENT(PLUIE))THEN
              CALL OS('X=C     ',X = PLUIE, C=RAINFALL) ! MM/S
            ENDIF
!
!           HEAT EXCHANGE WITH ATMOSPHERE
!
          ELSEIF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
            CALL INTERPMETEO(WW,UAIR,VAIR,TAIR,PATM,
     &                       HREL,NEBU,RAINFALL,EVAPORATION,AT,UL)
            CALL OV('X=C     ',WINDX,Y,Y,UAIR,NPOIN)
            CALL OV('X=C     ',WINDY,Y,Y,VAIR,NPOIN)
!
            CALL OV('X=C     ',PATMOS,Y,Y,PATM,NPOIN)
          ENDIF
!
!      NO HEAT EXHANGE NEITHER WATER_QUALITY
!
        ELSE
!
!         WIND VARYING IN TIME CONSTANT IN SPACE
!
          IF(OPTWIND.EQ.2)THEN
10          CONTINUE
            IF(AT.GE.AT1.AND.AT.LT.AT2) THEN
              IF(AT2-AT1.GT.1.D-6) THEN
                COEF=(AT-AT1)/(AT2-AT1)
              ELSE
                COEF=0.D0
              ENDIF
              UAIR=FUAIR1+COEF*(FUAIR2-FUAIR1)
              VAIR=FVAIR1+COEF*(FVAIR2-FVAIR1)
              IF(LISTIN) THEN
                IF(LNG.EQ.1) WRITE(LU,*) 'VENT A T=',AT,' UAIR=',UAIR,
     &                                                  ' VAIR=',VAIR
                IF(LNG.EQ.2) WRITE(LU,*) 'WIND AT T=',AT,' UAIR=',UAIR,
     &                                                   ' VAIR=',VAIR
              ENDIF
            ELSE
              AT1=AT2
              FUAIR1=FUAIR2
              FVAIR1=FVAIR2
              READ(UL,*,ERR=100,END=200) AT2,FUAIR2,FVAIR2
              GO TO 10
!
!-----------------------------------------------------------------------
!
100           CONTINUE
              WRITE(LU,*) ' '
              WRITE(LU,*) 'METEO'
              IF(LNG.EQ.1) WRITE(LU,*) 'ERREUR DANS LE FICHIER DE VENT'
              IF(LNG.EQ.2) WRITE(LU,*) 'ERROR IN THE WIND FILE'
              CALL PLANTE(1)
              STOP
200           CONTINUE
              WRITE(LU,*) ' '
              WRITE(LU,*) 'METEO'
              IF(LNG.EQ.1)WRITE(LU,*)'FIN PREMATUREE DU FICHIER DE VENT'
              IF(LNG.EQ.2)WRITE(LU,*) 'WIND FILE TOO SHORT'
              CALL PLANTE(1)
              STOP
!
!-----------------------------------------------------------------------
!
            ENDIF
!
            CALL OV('X=C     ',WINDX,Y,Y,UAIR,NPOIN)
            CALL OV('X=C     ',WINDY,Y,Y,VAIR,NPOIN)
!
            FUAIR = UAIR
            FVAIR = VAIR
!
!         WIND VARYING IN TIME AND SPACE
!
          ELSEIF(OPTWIND.EQ.3)THEN
!            IF(LNG.EQ.1) THEN
!              WRITE(LU,*) 'CETTE OPTION N EST PAS ENCORE PROGRAMMEE'
!              WRITE(LU,*) 'VOIR CAS DE VALIDATION WIND_TXY '
!              WRITE(LU,*) 'DANS LE DOSSIER EXAMPLES/TELEMAC2D'
!            ELSE
!              WRITE(LU,*) 'THIS OPTION IS NOT IMPLEMENTED YET'
!              WRITE(LU,*) 'SEE VALIDATION CASE WIND_TXY '
!              WRITE(LU,*) 'LOCATED AT THE FOLDER EXAMPLES/TELEMAC2D'
!            ENDIF
!            CALL PLANTE(1)
!            STOP
!
! #######################################################################
!         IDWM WIND INTERPOLATION CODE
! #######################################################################
!
            PI = ACOS(-1.D0)
!
!       ASSEMBLE THE ARRAYS OF X,Y,WNDSPD AND X,Y,WNDDIR FOR EACH ITERATION
            DO A = 1,NUMPOINTS
              IF(AT_WIND(A).EQ.AT) THEN
                DO B = 1,NUMSTA
              ! ASSEMBLE THE ARRAYS FOR THIS TIME STEP
                  INPSTA_D(B,1) = XX(B)
                  INPSTA_D(B,2) = YY(B)
                  INPSTA_D(B,3) = WIND(A,B*2+1)
                  INPSTA_S(B,1) = XX(B)
                  INPSTA_S(B,2) = YY(B)
                  INPSTA_S(B,3) = WIND(A,B*2)
                ENDDO
              ENDIF
            ENDDO
!
            CALL IDWM_T2D(INPSTA_S,POINTS,OUT_WSPD,NPOIN,NUMSTA)
            CALL IDWM_T2D(INPSTA_D,POINTS,OUT_WDIR,NPOIN,NUMSTA)
!
!       CONVERT OUT_WSPD AND OUT_WDIR TO WINDX AND WINDY
            DO K = 1,NPOIN
              IF(OUT_WDIR(K).GE.0.D0.AND.OUT_WSPD(K).GE.0.D0) THEN
                IF(OUT_WDIR(K).GE.0.D0.AND.OUT_WDIR(K).LE.90.D0) THEN
                  THETA_RAD = OUT_WDIR(K)*PI/180.D0
                  WINDX(K) = -SIN(THETA_RAD)*OUT_WSPD(K)
                  WINDY(K) = -COS(THETA_RAD)*OUT_WSPD(K)
                ENDIF
!
                IF(OUT_WDIR(K).GT.90.D0.AND.OUT_WDIR(K).LE.180.D0) THEN
                  THETA_RAD = (180.D0-OUT_WDIR(K))*PI/180.D0
                  WINDX(K) = -SIN(THETA_RAD)*OUT_WSPD(K)
                  WINDY(K) =  COS(THETA_RAD)*OUT_WSPD(K)
                ENDIF
!
                IF(OUT_WDIR(K).GT.180.D0.AND.OUT_WDIR(K).LE.270.D0) THEN
                  THETA_RAD = (OUT_WDIR(K)-180.D0)*PI/180.D0
                  WINDX(K) = SIN(THETA_RAD)*OUT_WSPD(K)
                  WINDY(K) = COS(THETA_RAD)*OUT_WSPD(K)
                ENDIF
!
                IF(OUT_WDIR(K).GT.270.D0.AND.OUT_WDIR(K).LE.360.D0) THEN
                  THETA_RAD = (360.D0-OUT_WDIR(K))*PI/180.D0
                  WINDX(K) =  SIN(THETA_RAD)*OUT_WSPD(K)
                  WINDY(K) = -COS(THETA_RAD)*OUT_WSPD(K)
                ENDIF
              ELSE
                WINDX(K) = -999.D0
                WINDY(K) = -999.D0
                WRITE(*,*) 'NO WIND DATA FOR TIME ', AT
              ENDIF
            ENDDO !K
!
! #######################################################################
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END


!                    *******************
                     SUBROUTINE IDWM_T2D
!                    *******************
!
     &(ELEV,POINTS,IDWM,NPOIN,NUMSTA)
!
!***********************************************************************
! TELEMAC2D   V7P0
!***********************************************************************
!
!brief    USES INVERSE DISTANCE WEIGHTING METHOD TO COMPUTE A WIND FIELD
!+               THAT VARIES IN TIME AND SPACE
!
!history  P. PRODANOVIC (RIGGS ENGINEERING LTD)
!+        23/04/2014
!+        V7P0
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        16/02/2015
!+        V7P0
!+   Managing the divisions by 0 + optimization.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VARIABLES THAT ARE USED BY THE SUBROUTINE TO PASS DATA IN AND OUT
      INTEGER,INTENT(IN) :: NPOIN, NUMSTA
      DOUBLE PRECISION, INTENT(IN) :: POINTS(NPOIN,2),ELEV(NUMSTA,3)
      DOUBLE PRECISION, INTENT(INOUT) :: IDWM(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N, M, I, J, K
!     WEIGHTS, DENOMINATOR, DISTANCE
      DOUBLE PRECISION W1, W2, W3, W4, DEN, DIST
!     CURRENT MINS
      DOUBLE PRECISION MIN1CUR, MIN2CUR, MIN3CUR, MIN4CUR
!     LOCATIONS OF THE MININIMS (USED FOR ARRAY REFERENCING)
      INTEGER MIN1LOC, MIN2LOC, MIN3LOC, MIN4LOC
!
!-----------------------------------------------------------------------
!
!     DEFINE N AND M, THIS IS DONE TO HARMONIZE PASTING OF CODE
      N = NUMSTA
      M = NPOIN
!
!     MAIN LOOP TO DO THE INTERPOLATION
!
      DO J = 1, M
!
!       SET MIN_PREV VARS TO LARGE NUMBERS
!
        MIN1CUR = 1.D30
        MIN2CUR = 1.D30
        MIN3CUR = 1.D30
        MIN4CUR = 1.D30
!
!       INITIALIZE MIN_LOC
!
        MIN1LOC = -1
        MIN2LOC = -1
        MIN3LOC = -1
        MIN4LOC = -1
!
        DO I = 1, N
!
          IF(ELEV(I,3).GT.0.D0) THEN
!
            DIST=(ELEV(I,1)-POINTS(J,1))**2+(ELEV(I,2)-POINTS(J,2))**2
!
!           FIND MIN DIST IN EACH OF THE FOUR QUADRANTS
!           2 | 1
!           -----
!           3 | 4
!
!          QUADRANT 1
            IF(ELEV(I,1).GE.POINTS(J,1).AND.
     &         ELEV(I,2).GE.POINTS(J,2)      ) THEN
              IF(DIST.LT.MIN1CUR) THEN
                MIN1CUR = DIST
                MIN1LOC = I
              ENDIF
            ENDIF
!           QUADRANT 2
            IF(ELEV(I,1).LT.POINTS(J,1).AND.
     &         ELEV(I,2).GE.POINTS(J,2)      ) THEN
              IF(DIST.LT.MIN2CUR) THEN
                MIN2CUR = DIST
                MIN2LOC = I
              ENDIF
            ENDIF
!           QUADRANT 3
            IF(ELEV(I,1).LT.POINTS(J,1).AND.
     &         ELEV(I,2).LT.POINTS(J,2)      ) THEN
              IF(DIST.LT.MIN3CUR) THEN
                MIN3CUR = DIST
                MIN3LOC = I
              ENDIF
            ENDIF
!           QUADRANT 4
            IF(ELEV(I,1).GT.POINTS(J,1).AND.
     &         ELEV(I,2).LT.POINTS(J,2)      ) THEN
              IF(DIST.LT.MIN4CUR) THEN
                MIN4CUR = DIST
                MIN4LOC = I
              ENDIF
            ENDIF
!
          ENDIF
!
        ENDDO
!
!       CALCULATE WEIGHTS
!
!       AVOIDING DIVISIONS BY 0
!
        MIN1CUR=MAX(MIN1CUR,1.D-6)
        MIN2CUR=MAX(MIN2CUR,1.D-6)
        MIN3CUR=MAX(MIN3CUR,1.D-6)
        MIN4CUR=MAX(MIN4CUR,1.D-6)
!
        DEN = 1.D0/MIN1CUR+1.D0/MIN2CUR+1.D0/MIN3CUR+1.D0/MIN4CUR
!
!       IN CASE WHEN ALL INPUT DATA IS MISSING, ALSO OUTPUT MISSING
!
        IF(MIN1LOC.EQ.-1.AND.MIN2LOC.EQ.-1.AND.
     &     MIN3LOC.EQ.-1.AND.MIN4LOC.EQ.-1      ) THEN
          IDWM(J) = -999.D0
        ELSE
          IDWM(J) = 0.D0
          IF(MIN1LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN1LOC,3)/MIN1CUR
          ENDIF
          IF(MIN2LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN2LOC,3)/MIN2CUR
          ENDIF
          IF(MIN3LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN3LOC,3)/MIN3CUR
          ENDIF
          IF(MIN4LOC.GT.0) THEN
            IDWM(J)=IDWM(J)+ELEV(MIN4LOC,3)/MIN4CUR
          ENDIF
          IDWM(J) = IDWM(J)/DEN
        ENDIF
!
      ENDDO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
      INTEGER I
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
      DO I=1,NPOIN
        ZF%R(I) = -2.D0
      ENDDO
!
!-----------------------------------------------------------------------
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
