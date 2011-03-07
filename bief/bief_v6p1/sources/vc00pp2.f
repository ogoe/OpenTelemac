!                    ******************
                     SUBROUTINE VC00PP2
!                    ******************
!
     &( XMUL,Z,SURFAC,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     &  W1,W2,W3,W4,W5,W6)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!+                  /OMEGA
!+
!+    IN A FORM COMPATIBLE WITH MASS-LUMPING IN 2D   !!!!!!!!!!!!!!!!!!!
!+    (COMPARE WITH VC00PP)
!+
!+    PSI(I) IS A BASE OF TYPE P1 PRISM
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)
!+        11/10/04
!+        V5P5
!+   IDEA AND THEORY: ASTRID DECOENE 
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
!| IKLE2          |---| 
!| IKLE3          |---| 
!| IKLE4          |---| 
!| IKLE5          |---| 
!| IKLE6          |---| 
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| SURFAC         |-->| SURFACE DES ELEMENTS.
!| W2             |---| 
!| W3             |---| 
!| W4             |---| 
!| W5             |---| 
!| W6             |---| 
!| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
!| Z              |-->| COORDONNEES DES POINTS DANS L'ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: Z(*),SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT)::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT)::W4(NELMAX),W5(NELMAX),W6(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION XSUR6,H1,H2,H3,COEF
!
!-----------------------------------------------------------------------
!
      XSUR6  = XMUL/6.D0
!
!   LOOP ON THE ELEMENTS
!
      DO 3 IELEM = 1 , NELEM
!
         H1 = Z(IKLE4(IELEM)) - Z(IKLE1(IELEM))
         H2 = Z(IKLE5(IELEM)) - Z(IKLE2(IELEM))
         H3 = Z(IKLE6(IELEM)) - Z(IKLE3(IELEM))
!
         COEF = XSUR6 * SURFAC(IELEM)
!
         W1(IELEM) = COEF * H1
         W2(IELEM) = COEF * H2
         W3(IELEM) = COEF * H3
         W4(IELEM) = W1(IELEM)
         W5(IELEM) = W2(IELEM)
         W6(IELEM) = W3(IELEM)
!
3     CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END