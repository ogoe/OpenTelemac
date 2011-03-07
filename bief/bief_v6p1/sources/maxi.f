!                    ***************
                     SUBROUTINE MAXI
!                    ***************
!
     &( XMAX , IMAX , X , NPOIN )
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    LOOKS FOR THE GREATEST VALUE IN ARRAY X
!+                OF DIMENSION NPOIN.
!
!history  E. PELTIER   (LNH)
!+        17/08/94
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
!| IMAX           |<--| INDICE DU MAXIMUM
!| NPOIN          |-->| DIMENSION DU TABLEAU
!| X              |-->| TABLEAU DES VALEURS
!| XMAX           |<--| MAXIMUM TROUVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN
      INTEGER, INTENT(INOUT)          :: IMAX
      DOUBLE PRECISION, INTENT(INOUT) :: XMAX
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      XMAX = X(1)
      IMAX = 1
!
      DO 10 I = 2 , NPOIN
!
        IF(X(I).GT.XMAX) THEN
          IMAX = I
          XMAX = X(I)
        ENDIF
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END