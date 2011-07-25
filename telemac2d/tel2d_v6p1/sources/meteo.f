!                    ****************
                     SUBROUTINE METEO
!                    ****************
!
     &(PATMOS,WINDX,WINDY,FUAIR,FVAIR,X,Y,AT,LT,NPOIN,VENT,ATMOS,
     & HN,TRA01,GRAV,ROEAU,NORD,PRIVE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES ATMOSPHERIC PRESSURE AND WIND VELOCITY FIELDS
!+               (IN GENERAL FROM INPUT DATA FILES).
!
!warning  MUST BE ADAPTED BY USER
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
      INTEGER, INTENT(IN)             :: LT,NPOIN
      LOGICAL, INTENT(IN)             :: ATMOS,VENT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: WINDX(NPOIN),WINDY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: PATMOS(NPOIN),TRA01(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FUAIR,FVAIR,AT,GRAV,ROEAU,NORD
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PRIVE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P0,Z(1)
!
!-----------------------------------------------------------------------
!
!     BEWARE, HERE ONLY ONE COMPUTATION AT FIRST TIMESTEP
!
      IF(LT.EQ.0) THEN
!
!-----------------------------------------------------------------------
!
!     ATMOSPHERIC PRESSURE
!
      IF(ATMOS) THEN
        P0 = 100000.D0
        CALL OV( 'X=C     ' , PATMOS , Y , Z , P0 , NPOIN )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     WIND : IN THIS CASE THE WIND IS CONSTANT,
!            VALUE GIVEN IN STEERING FILE.
!
!     MAY REQUIRE A ROTATION,
!     DEPENDING ON THE SYSTEM IN WHICH THE WIND VELOCITY WAS SUPPLIED
!
      IF(VENT) THEN
        CALL OV( 'X=C     ' , WINDX , Y , Z , FUAIR , NPOIN )
        CALL OV( 'X=C     ' , WINDY , Y , Z , FVAIR , NPOIN )
      ENDIF
!
!     END OF IF(LT.EQ.0)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
