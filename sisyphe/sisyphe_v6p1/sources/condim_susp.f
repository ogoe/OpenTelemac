!                    **********************
                     SUBROUTINE CONDIM_SUSP
!                    **********************
!
     &(CS,CS0,NSICLA,X,Y,AT,NPOIN)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE SUSPENDED SEDIMENT CONCENTRATION
!+               (CONDIM_SISYPHE.F IS READ EVEN IF CHARR=NO).
!
!history  M. GONZALES DE LINARES
!+        2004
!+        V5P9
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
!| AT             |---|
!| CS             |---|
!| CS0            |---|
!| NPOIN          |---|
!| NSICLA         |---|
!| X              |---|
!| Y              |---|
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
      INTEGER, INTENT(IN)           :: NPOIN,NSICLA
      DOUBLE PRECISION,INTENT(IN)   :: AT,CS0(NSICLA)
      DOUBLE PRECISION,INTENT(IN)   :: X(NPOIN),Y(NPOIN)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  --------------------------------------------------------------
!  INITIALISES THE ARRAYS THAT HAVE NOT BEEN READ IN THE RESULTS FILE:
!  --------------------------------------------------------------
!
      IF(NSICLA.GT.0) THEN
        DO I=1,NSICLA
          CALL OS('X=C     ',X=CS%ADR(I)%P,C=CS0(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END