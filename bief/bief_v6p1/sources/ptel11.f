!                    *****************
                     SUBROUTINE PTEL11
!                    *****************
!
     &(XEL,X,IKLE,NELMAX,NELEM)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    GOES FROM A VECTOR BY POINTS TO A VECTOR BY ELEMENTS.
!+                CASE OF A P1 TRIANGLE.
!
!history  J-M HERVOUET (LNH)
!+        10/01/95
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
!| IKLE           |---| 
!| NELEM          |---| 
!| NELMAX         |---| 
!| X              |-->| VECTEUR PAR POINTS
!| XEL            |<--| VECTEUR SUR LES ELEMENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,IKLE(NELMAX,3)
!
!-----------------------------------------------------------------------
!
!     VECTORS STRUCTURES
!
      DOUBLE PRECISION, INTENT(IN)    :: X(*)
      DOUBLE PRECISION, INTENT(INOUT) :: XEL(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
!
!-----------------------------------------------------------------------
!
      DO 10 IELEM = 1,NELEM
!
        XEL(IELEM,1)=X(IKLE(IELEM,1))
        XEL(IELEM,2)=X(IKLE(IELEM,2))
        XEL(IELEM,3)=X(IKLE(IELEM,3))
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END