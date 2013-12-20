!                    ****************
                     SUBROUTINE METEO
!                    ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,X,Y,AT,LT,NPOIN,VENT,ATMOS,
     & HN,TRA01,GRAV,ROEAU,NORD,PRIVE,FO1,FILES,LISTIN)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT,LT          |-->| TIME, ITERATION NUMBER
!| ATMOS          |-->| YES IF PRESSURE TAKEN INTO ACCOUNT
!| FUAIR          |-->| VELOCITY OF WIND ALONG X, IF CONSTANT
!| FVAIR          |-->| VELOCITY OF WIND ALONG Y, IF CONSTANT
!| GRAV           |-->| GRAVITY ACCELERATION
!| HN             |-->| DEPTH
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
      DOUBLE PRECISION, INTENT(IN)    :: FUAIR,FVAIR,AT,GRAV,ROEAU,NORD
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PRIVE
      TYPE(BIEF_FILE), INTENT(IN)     :: FILES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER MY_OPTION,UL
      DOUBLE PRECISION P0,Z(1),AT1,AT2,FUAIR1,FUAIR2,FVAIR1,FVAIR2,COEF
      DOUBLE PRECISION UAIR,VAIR
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
!     CHOOSE YOUR OPTION !!!
!
!     1: CONSTANTS GIVEN BY THE KEYWORDS:
!        AIR PRESSURE (GIVEN HERE AS P0, NO KEYWORD)
!        WIND VELOCITY ALONG X (HERE FUAIR)
!        WIND VELOCITY ALONG Y (HERE FVAIR)
!        THEY WILL BE SET ONCE FOR ALL BEFORE THE FIRST ITERATION (LT=0)
!
!     2: TIME VARYING CONSTANT IN SPACE WIND COMPONENTS OF VELOCITY 
!        GIVEN IN THE FILE FO1_WIND DECLARED AS 
!        FORMATTED DATA FILE 1 = FO1_WIND 
!
!-----------------------------------------------------------------------
!
      MY_OPTION = 1
!
!-----------------------------------------------------------------------
!
!     AT FIRST TIMESTEP
!
      IF(LT.EQ.0) THEN
!
        UL=FILES(FO1)%LU
!
!-----------------------------------------------------------------------
!
!       ATMOSPHERIC PRESSURE
!
        IF(ATMOS) THEN
          P0 = 100000.D0
          CALL OV( 'X=C     ' , PATMOS , Y , Z , P0 , NPOIN )
        ENDIF
!
!-----------------------------------------------------------------------
!
!       WIND : IN THIS CASE THE WIND IS CONSTANT,
!              VALUE GIVEN IN STEERING FILE.
!
!       MAY REQUIRE A ROTATION,
!       DEPENDING ON THE SYSTEM IN WHICH THE WIND VELOCITY WAS SUPPLIED
!
        IF(VENT) THEN
          CALL OV( 'X=C     ' , WINDX , Y , Z , FUAIR , NPOIN )
          CALL OV( 'X=C     ' , WINDY , Y , Z , FVAIR , NPOIN )
        ENDIF
!
        IF(MY_OPTION.EQ.2) THEN
!         JUMPING TWO LINES OF COMMENTS
          READ(UL,*,ERR=100,END=200)
          READ(UL,*,ERR=100,END=200)
!         READING THE FIRST TWO LINES OF DATA
          READ(UL,*,ERR=100,END=200) AT1,FUAIR1,FVAIR1
          READ(UL,*,ERR=100,END=200) AT2,FUAIR2,FVAIR2
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(MY_OPTION.EQ.2.AND.VENT) THEN
!
!       JUMPING TWO LINES OF COMMENTS
!
10      CONTINUE
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
     &                                              ' VAIR=',VAIR
            IF(LNG.EQ.2) WRITE(LU,*) 'WIND AT T=',AT,' UAIR=',UAIR,
     &                                               ' VAIR=',VAIR
          ENDIF
        ELSE
          AT1=AT2
          FUAIR1=FUAIR2
          FVAIR1=FVAIR2
          READ(UL,*,ERR=100,END=200) AT2,FUAIR2,FVAIR2
          GO TO 10
        ENDIF
!
        CALL OV('X=C     ',WINDX,Y,Z,UAIR,NPOIN)
        CALL OV('X=C     ',WINDY,Y,Z,VAIR,NPOIN)    
!
      ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
! 
100   CONTINUE
      WRITE(LU,*) ' '
      WRITE(LU,*) 'METEO'
      IF(LNG.EQ.1) WRITE(LU,*) 'ERREUR DANS LE FICHIER DE VENT'
      IF(LNG.EQ.2) WRITE(LU,*) 'ERROR IN THE WIND FILE'
      CALL PLANTE(1)
      STOP  
200   CONTINUE
      WRITE(LU,*) ' '
      WRITE(LU,*) 'METEO'
      IF(LNG.EQ.1) WRITE(LU,*) 'FIN PREMATUREE DU FICHIER DE VENT'
      IF(LNG.EQ.2) WRITE(LU,*) 'WIND FILE TOO SHORT'
      CALL PLANTE(1)
      STOP           
!
!-----------------------------------------------------------------------
!
      RETURN
      END
