!                    *****************
                     SUBROUTINE VC05AA
!                    *****************
!
     &(XMUL,SW,W,SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,W1,W2,W3 )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                    /         ->
!+    VEC(I) = XMUL  /    (U,V).N  PSI(I) D(GAMMA)
!+                  /GAMMA
!+
!+    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
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
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| SURFAC         |-->| AREA OF TRIANGLES
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC05AA => VC05AA
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT):: W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
!
!-----------------------------------------------------------------------
!
!     STRUCTURES OF W AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN)   :: SW
      DOUBLE PRECISION, INTENT(IN) :: W(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMW,IELEM,N1,N2,N3
      DOUBLE PRECISION XSUR12,A,WT
!
!-----------------------------------------------------------------------
!
      IELMW=SW%ELM
!
!-----------------------------------------------------------------------
!
!     W LINEAR BY TRIANGLES
!
      IF(IELMW.EQ.11) THEN
!
         XSUR12 = XMUL/12.D0
!
!   LOOP ON THE BOUNDARY SIDES
!
         DO 1 IELEM = 1,NELEM
!
!  LOCAL NUMBERING OF THE SIDE NODES
!
            N1 = IKLE1(IELEM)
            N2 = IKLE2(IELEM)
            N3 = IKLE3(IELEM)
!
            WT = W(N1) + W(N2) + W(N3)
            A = SURFAC(IELEM) * XSUR12
!
            W1(IELEM) = A * (WT + W(N1))
            W2(IELEM) = A * (WT + W(N2))
            W3(IELEM) = A * (WT + W(N3))
!
1        CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
         IF (LNG.EQ.1) WRITE(LU,100) IELMW,SW%NAME
         IF (LNG.EQ.2) WRITE(LU,101) IELMW,SW%NAME
100      FORMAT(1X,'VC05AA (BIEF) :',/,
     &          1X,'DISCRETISATION DE W NON PREVUE : ',1I6,
     &          1X,'NOM REEL : ',A6)
101      FORMAT(1X,'VC05AA (BIEF) :',/,
     &          1X,'DISCRETIZATION OF W NOT AVAILABLE:',1I6,
     &          1X,'REAL NAME: ',A6)
         CALL PLANTE(1)
         STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
