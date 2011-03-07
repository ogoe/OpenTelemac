!                    *****************
                     SUBROUTINE MT01BB
!                    *****************
!
     &( A11 , A12 , A13 , A14 ,
     &        A22 , A23 , A24 ,
     &              A33 , A34 ,
     &                    A44 ,
     &  XMUL,SURFAC,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MASS MATRIX FOR QUASI-BUBBLE TRIANGLES.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; C MOULIN (LNH)
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
!| A11,A12        |<--| ELEMENTS DE LA MATRICE
!| A13            |---| 
!| A14            |---| 
!| A22            |---| 
!| A23            |---| 
!| A24            |---| 
!| A33            |---| 
!| A34            |---| 
!| A44            |---| 
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| SURFAC         |-->| SURFACE DES TRIANGLES.
!| XMUL           |-->| FACTEUR MULTIPLICATIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT01BB => MT01BB
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) ::                      A44(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM
      DOUBLE PRECISION XSUR36
!
!=======================================================================
!
      XSUR36 = XMUL / 36.D0
!
      DO 5 IELEM = 1 , NELEM
!
!  EXTRADIAGONAL TERMS
!
         A12(IELEM) =     SURFAC(IELEM)*XSUR36
         A13(IELEM) =     A12(IELEM)
         A23(IELEM) =     A12(IELEM)
         A14(IELEM) = 2 * A12(IELEM)
         A24(IELEM) =     A14(IELEM)
         A34(IELEM) =     A14(IELEM)
!
!  DIAGONAL TERMS
!
         A11(IELEM) = 2 * A14(IELEM)
         A22(IELEM) =     A11(IELEM)
         A33(IELEM) =     A11(IELEM)
         A44(IELEM) = 3 * A14(IELEM)
!
5     CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END