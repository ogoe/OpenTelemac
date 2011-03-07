!                    **************************
                     SUBROUTINE BEDLOAD_MEYER !
!                    **************************
!
     &  (TETAP, HIDING, HIDFAC, DENS, GRAV, DM, AC,
     &   ACP, QSC, SLOPEFF, COEFPN)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |---|
!| ACP            |---|
!| COEFPN         |---|
!| DENS           |---|
!| DM             |---|
!| GRAV           |---|
!| HIDFAC         |---|
!| HIDING         |---|
!| QSC            |---|
!| SLOPEFF        |---|
!| TETAP          |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_MEYER => BEDLOAD_MEYER
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, HIDING
      INTEGER,          INTENT(IN)    :: HIDFAC, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, COEFPN
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      DOUBLE PRECISION :: C2
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
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
      ! **************************************** !
      C2 = 8.D0 * SQRT(GRAV*DENS*DM**3)
      IF ((HIDFAC == 1) .OR. (HIDFAC == 2) ) THEN
         CALL OS('X=XY    ', X=ACP, Y=HIDING)
         CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
         CALL OS('X=+(Y,C)', X=QSC, Y=QSC , C=0.D0)
         CALL OS('X=Y**C  ', X=QSC, Y=QSC , C=1.5D0)
         CALL OS('X=CX    ', X=QSC, C=C2)
      ELSE
          CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
          CALL OS('X=+(Y,C)', X=QSC, Y=QSC, C=0.D0)
         CALL OS('X=Y**C  ', X=QSC, Y=QSC, C=1.5D0)
         CALL OS('X=CX    ', X=QSC, C=C2)
         CALL OS('X=XY    ', X=QSC, Y=HIDING)
      ENDIF
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_MEYER