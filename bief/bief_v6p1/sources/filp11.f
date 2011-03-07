!                    *****************
                     SUBROUTINE FILP11
!                    *****************
!
     &( F , C , XSOM , YSOM , NSOM , X , Y , NPOIN )
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES A FUNCTION TO A CONSTANT VALUE
!+                INSIDE OF A POLYGON.
!
!history  C MOULIN (LNH)
!+        06/12/94
!+        V5P1
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
!| C              |---|
!| F              |---|
!| NPOIN          |---|
!| NSOM           |---|
!| X              |---|
!| XSOM           |---|
!| Y              |---|
!| YSOM           |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FILP11 => FILP11
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSOM , NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: F(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*) , Y(*)
      DOUBLE PRECISION, INTENT(IN) :: XSOM(NSOM) , YSOM(NSOM) , C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      DO 10 I = 1 , NPOIN
!
        IF(INPOLY(X(I),Y(I),XSOM,YSOM,NSOM)) F(I) = C
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END