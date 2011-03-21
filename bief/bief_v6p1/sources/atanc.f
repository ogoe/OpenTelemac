!                    *******************************
                     DOUBLE PRECISION FUNCTION ATANC
!                    *******************************
!
     &(A,B)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES AN ARC TANGENT BETWEEN 0 AND 2 PI.
!
!history  E. DAVID (LHF)
!+        12/07/95
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
!| A              |-->| TANGENT OF THE ANGLE
!| B              |-->| AN ANGLE CLOSE TO THE RESULT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: A,B
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PISUR2,D
!
      INTRINSIC ACOS,ATAN
!
!---------------------------------------------------------------
!
      PISUR2 = 0.5D0 * ACOS(-1.D0)
      D = ATAN(A)
!
10    CONTINUE
      IF (D-B.LT.-0.5D0*PISUR2) THEN
        D = D + PISUR2
        GO TO 10
      ENDIF
20    CONTINUE
      IF (D-B.GT.0.5D0*PISUR2) THEN
        D = D - PISUR2
        GO TO 20
      ENDIF
!
      ATANC = D
!
!---------------------------------------------------------------
!
      RETURN
      END
