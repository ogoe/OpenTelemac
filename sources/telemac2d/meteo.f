!                    ****************
                     SUBROUTINE METEO
!                    ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,X,Y,AT,LT,NPOIN,VENT,ATMOS,
     & HN,TRA01,GRAV,ROEAU,NORD,PRIVE,FO1,FILES,LISTIN,
     & AWATER_QUALITY,PLUIE,AATMOSEXCH,AOPTWIND,AWIND_SPD,APATMOS_VALUE)
!
!***********************************************************************
! TELEMAC2D   V7P0
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
      USE DECLARATIONS_WAQTEL ,ONLY : PVAP,RAY3,TAIR,NEBU,NWIND
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
      DOUBLE PRECISION, INTENT(IN)    :: AT,GRAV,ROEAU,NORD
      DOUBLE PRECISION, INTENT(INOUT) :: FUAIR,FVAIR
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PRIVE
      TYPE(BIEF_FILE), INTENT(IN)     :: FILES(*)
!     OPTIONAL
      LOGICAL, INTENT(IN)          ,OPTIONAL :: AWATER_QUALITY
      TYPE(BIEF_OBJ), INTENT(INOUT),OPTIONAL :: PLUIE
      INTEGER, INTENT(IN)          ,OPTIONAL :: AATMOSEXCH,AOPTWIND
      DOUBLE PRECISION, INTENT(IN) ,OPTIONAL :: AWIND_SPD(2)
      DOUBLE PRECISION, INTENT(IN) ,OPTIONAL :: APATMOS_VALUE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL WATER_QUALITY
      INTEGER UL,OPTWIND,ATMOSEXCH
      DOUBLE PRECISION AT1,AT2,FUAIR1,FUAIR2,FVAIR1,FVAIR2,COEF
      DOUBLE PRECISION UAIR,VAIR,PATMOS_VALUE,WIND_SPD(2)
!     EXCHANGE WITH ATMOSPHERE
      DOUBLE PRECISION HREL,RAINFALL,PATM,WW,PI
!
      DOUBLE PRECISION, PARAMETER :: EPS = 1.D-3
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
      ATMOSEXCH=0
      IF(PRESENT(AATMOSEXCH)) ATMOSEXCH=AATMOSEXCH
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
      PATMOS_VALUE=0.D0
      IF(PRESENT(APATMOS_VALUE)) PATMOS_VALUE=APATMOS_VALUE
!
!-----------------------------------------------------------------------
!
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
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FOR THE REMAINING TIME STEPS
!
      IF(VENT.OR.WATER_QUALITY.OR.ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
!
!       WATER QUALITY
!
        IF(WATER_QUALITY.AND.FILES(FO1)%NAME(1:1).NE.' ')THEN
          CALL INTERPMETEO2(NWIND,UAIR,VAIR,TAIR,PATM,NEBU,RAINFALL,
     &                      PVAP,RAY3,AT,UL)
!
          CALL OV('X=C     ',WINDX,WINDX,WINDX,UAIR,NPOIN)
          CALL OV('X=C     ',WINDY,WINDY,WINDY,VAIR,NPOIN)
          CALL OV('X=C     ',PATMOS,PATMOS,PATMOS,PATM,NPOIN)
          IF(PRESENT(PLUIE))THEN
            CALL OS('X=C     ',X = PLUIE, C=RAINFALL) ! MM/S
          ENDIF
!
!       HEAT EXCHANGE WITH ATMOSPHERE
!
        ELSEIF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
          IF(VENT.OR.ATMOS) THEN
            CALL INTERPMETEO(WW,UAIR,VAIR,
     &                       TAIR,PATM,HREL,NEBU,RAINFALL,AT,UL)
          ENDIF
!
          IF(VENT) THEN
            CALL OV('X=C     ',WINDX,Y,Y,UAIR,NPOIN)
            CALL OV('X=C     ',WINDY,Y,Y,VAIR,NPOIN)
          ENDIF
!
          IF(ATMOS) THEN
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
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'CETTE OPTION N EST PAS ENCORE PROGRAMMEE'
              WRITE(LU,*) 'VOIR CAS DE VALIDATION WIND_TXY '
              WRITE(LU,*) 'DANS LE DOSSIER EXAMPLES/TELEMAC2D'
            ELSE
              WRITE(LU,*) 'THIS OPTION IS NOT IMPLEMENTED YET'
              WRITE(LU,*) 'SEE VALIDATION CASE WIND_TXY '
              WRITE(LU,*) 'LOCATED AT THE FOLDER EXAMPLES/TELEMAC2D'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

