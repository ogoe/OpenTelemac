!                    *****************
                     SUBROUTINE VC00FF
!                    *****************
!
     &(XMUL,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,IKLE4,NBOR,NELEM,NELMAX,W1,W2,W3,W4)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /
!+    VEC(I) = XMUL  /    PSI(I)  D(OMEGA)
!+                  /OMEGA
!+
!+    PSI(I) IS A BASE OF TYPE P1 SEGMENT
!+
!+    F IS A VECTOR OF TYPE IELMF
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
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
!| IKLE1          |-->| FIRST POINT OF QUADRILATERAL
!| IKLE2          |-->| SECOND POINT OF QUADRILATERAL
!| IKLE3          |-->| THIRD POINT OF QUADRILATERAL
!| IKLE4          |-->| FOURTH POINT OF QUADRILATERAL
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: NBOR(*)
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I4
      DOUBLE PRECISION XSUR24,H1,H2,AL
!
      INTRINSIC SQRT
!
!***********************************************************************
!
! NOTE ON PARALLELISM : IN PARALLEL MODE NELEM MAY BE WRONG AS BOUNDARY
!                       ELEMENTS MAY BE IN ANOTHER SUB-DOMAIN. IN THIS
!                       CASE WE HAVE I1=I2 AND THUS AL=0.D0, SO WRONG
!                       ELEMENTS DO NOT CONTRIBUTE.
!
!***********************************************************************
!
      XSUR24 = XMUL/24.D0
!
!   LOOP ON THE BOUNDARY SIDES
!
      DO 1 IELEM = 1,NELEM
!
!   GLOBAL NUMBERING OF THE SIDE NODES
!
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
!
         AL = SQRT((X(NBOR(I2))-X(NBOR(I1)))**2
     &            +(Y(NBOR(I2))-Y(NBOR(I1)))**2) * XSUR24
!
         H1 = Z(NBOR(I4)) - Z(NBOR(I1))
         H2 = Z(NBOR(I3)) - Z(NBOR(I2))
!
         W1(IELEM) = (3.D0*H1+H2)*AL
         W2(IELEM) = (3.D0*H2+H1)*AL
         W3(IELEM) = (3.D0*H2+H1)*AL
         W4(IELEM) = (3.D0*H1+H2)*AL
!
1     CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
