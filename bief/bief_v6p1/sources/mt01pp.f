!                    *****************
                     SUBROUTINE MT01PP
!                    *****************
!
     &( T,XM,XMUL,Z,SURFAC,IKLE,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MASS MATRIX FOR P1 PRISMS.
!code
!+    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!+
!+                                 /
!+                    A    = XMUL /  (P *P )*J(X,Y) DXDY
!+                     I J       /S    I  J
!+
!+    BY ELEMENTARY CELL (REAL MESH)
!+
!+    J(X,Y) : JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        21/05/2010
!+        V6P0
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
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| SURFAC         |-->| SURFACE DES TRIANGLES.
!| T              |---| 
!| XM             |---| 
!| XMUL           |-->| FACTEUR MULTIPLICATIF
!| Z              |-->| COORDONNEES DES POINTS DANS L'ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT01PP => MT01PP
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,6)
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,30)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,Z(*),SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM
      DOUBLE PRECISION SUR360,COEF,H1,H2,H3,HT,Z41,Z52,Z63
!
      DOUBLE PRECISION EPSILON
      DATA EPSILON/1.D-3/
!
!-----------------------------------------------------------------------
!
      SUR360 = XMUL / 360.D0
!
!   LOOP ON THE ELEMENTS
!
      DO 1 IELEM = 1,NELEM
!
         COEF = SURFAC(IELEM) * SUR360
!
!        TREATS HERE THE DRY ZONES
!
         H1 = MAX(Z(IKLE(IELEM,4)) - Z(IKLE(IELEM,1)),EPSILON) * COEF
         H2 = MAX(Z(IKLE(IELEM,5)) - Z(IKLE(IELEM,2)),EPSILON) * COEF
         H3 = MAX(Z(IKLE(IELEM,6)) - Z(IKLE(IELEM,3)),EPSILON) * COEF
         HT = H1 + H2 + H3
!
!-----------------------------------------------------------------------
!
!  EXTRA-DIAGONAL TERMS
!
         XM(IELEM,4)  = H1 + H2 + HT
         XM(IELEM,5)  = H1 + H3 + HT
         XM(IELEM,9)  = H2 + H3 + HT
         XM(IELEM,7)  = XM(IELEM,4)
         XM(IELEM,10) = XM(IELEM,5)
         XM(IELEM,11) = XM(IELEM,9)
!
         XM(IELEM,3)  =  4*H1 + HT + HT
         XM(IELEM,8)  =  4*H2 + HT + HT
         XM(IELEM,12) =  4*H3 + HT + HT
!
         XM(IELEM,1)  = XM(IELEM,4) + XM(IELEM,4)
         XM(IELEM,2)  = XM(IELEM,5) + XM(IELEM,5)
         XM(IELEM,6)  = XM(IELEM,9) + XM(IELEM,9)
         XM(IELEM,13) = XM(IELEM,1)
         XM(IELEM,14) = XM(IELEM,2)
         XM(IELEM,15) = XM(IELEM,6)
!
!  DIAGONAL TERMS
!
         T(IELEM,1) = XM(IELEM,3)  + XM(IELEM,3)
         T(IELEM,2) = XM(IELEM,8)  + XM(IELEM,8)
         T(IELEM,3) = XM(IELEM,12) + XM(IELEM,12)
         T(IELEM,4) = T(IELEM,1)
         T(IELEM,5) = T(IELEM,2)
         T(IELEM,6) = T(IELEM,3)
!
!   END OF THE LOOP ON THE ELEMENTS
!
1     CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END