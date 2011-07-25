!                    **************************
                     SUBROUTINE BEDLOAD_MEYER !
!                    **************************
!
     &  (TETAP, HIDING, HIDFAC, DENS, GRAV, DM, AC,
     &   ACP, QSC, SLOPEFF, COEFPN)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    MEYER-PETER BEDLOAD TRANSPORT FORMULATION.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+        V5P1
!+
!
!history  C.VILLARET
!+        **/10/2003
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
!!history  U.MERKEL R.KOPMAN
!+        15/03/2011
!+        V6P1
!+
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACP            |<->| MODIFIED SHIELDS PARAMETER
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HIDFAC         |-->| HIDING FACTOR FORMULAS
!| HIDING         |-->| HIDING FACTOR CORRECTION 
!| QSC            |<->| BED LOAD TRANSPORT 
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT 
!| TETAP          |-->| ADIMENSIONAL SKIN FRICTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_MEYER => BEDLOAD_MEYER
      USE BIEF
      USE DECLARATIONS_SISYPHE, only : MPM_ARAY
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, HIDING
      INTEGER,          INTENT(IN)    :: HIDFAC, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, COEFPN


      ! 3/ LOCAL VARIABLES
      ! ------------------
      DOUBLE PRECISION :: C2


!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      CALL CPSTVC(QSC,ACP)
      CALL OS('X=C     ', X=ACP, C=AC)

      ! **************************************** !
      ! 0 - SLOPE EFFECT: SOULBY FORMULATION     ! (_IMP_)
      ! **************************************** !
      IF(SLOPEFF == 2) THEN
        CALL OS('X=XY    ', X=ACP, Y=COEFPN )
      ENDIF

      ! **************************************** !
      ! III - BEDLOAD TRANSPORT CORRECTED        ! (_IMP_)
      !       FOR EXTENDED GRAIN SIZE            ! (_IMP_)
	  !       WITH VARIABLE MPM_COEFFICIENT      !
      ! **************************************** !
      C2 = SQRT(GRAV*DENS*DM**3)
      IF ((HIDFAC == 1) .OR. (HIDFAC == 2) ) THEN
         CALL OS('X=XY    ', X=ACP, Y=HIDING)
         CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
         CALL OS('X=+(Y,C)', X=QSC, Y=QSC , C=0.D0)
         CALL OS('X=Y**C  ', X=QSC, Y=QSC , C=1.5D0)
         CALL OS('X=CX    ', X=QSC, C=C2)
         CALL OS('X=XY    ', X=QSC, Y=MPM_ARAY) 
      ELSE
          CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
          CALL OS('X=+(Y,C)', X=QSC, Y=QSC, C=0.D0)
         CALL OS('X=Y**C  ', X=QSC, Y=QSC, C=1.5D0)
         CALL OS('X=CX    ', X=QSC, C=C2)
         CALL OS('X=XY    ', X=QSC, Y=HIDING)
         CALL OS('X=XY    ', X=QSC, Y=MPM_ARAY) 
      ENDIF

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE BEDLOAD_MEYER
C
C#######################################################################
C
