!                    *****************
                     SUBROUTINE MT07AA
!                    *****************
!
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SF,F,SURFAC,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE MASS MATRIX
!+                WITH LOCAL MASS-LUMPING ACCORDING TO A LOCAL
!+                COEFFICIENT TETA (P0 FUNCTION) (HERE THE FUNCTION F).
!+
!+            THE ELEMENT IS THE P1 TRIANGLE.
!code
!+            M = TETA     * MASS MATRIX
!+              + (1-TETA) * DIAGONAL MATRIX
!+
!+            THIS MATRIX IS SUBSEQUENTLY MULTIPLIED BY XMUL
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        30/06/93
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
!| A22            |---| 
!| A23            |---| 
!| A33            |---| 
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| SURFAC         |-->| SURFACE DES TRIANGLES.
!| XMUL           |-->| FACTEUR MULTIPLICATIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT07AA => MT07AA
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMF,IELEM
!
      DOUBLE PRECISION SUR12,DET,T
!
!-----------------------------------------------------------------------
!
      SUR12 = XMUL/12.D0
!
!-----------------------------------------------------------------------
!
      IELMF = SF%ELM
!
!  CASE WHERE F IS CONSTANT BY ELEMENT
!
      IF(IELMF.EQ.10) THEN
!
      DO 1 IELEM = 1 , NELEM
!
      DET = SURFAC(IELEM) * SUR12
      T   = F(IELEM)
!
!***********************************************************************
!
!  ELEMENTS OFF THE DIAGONAL
!
      A12(IELEM) = T * DET
      A13(IELEM) = T * DET
      A23(IELEM) = T * DET
!
!  DIAGONAL TERMS
!
      A11(IELEM) = ( DET + DET ) * (2.D0 - T)
      A22(IELEM) = ( DET + DET ) * (2.D0 - T)
      A33(IELEM) = ( DET + DET ) * (2.D0 - T)
!
!   END OF THE LOOP ON THE ELEMENTS
!
1     CONTINUE
!
!-----------------------------------------------------------------------
!
!     OTHER TYPES OF DISCRETISATION OF F
!
      ELSE
!
       IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100    FORMAT(1X,'MT07AA (BIEF) :',/,
     &        1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &        1X,'NOM REEL : ',A6)
101    FORMAT(1X,'MT07AA (BIEF) :',/,
     &        1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
     &        1X,'REAL NAME: ',A6)
       CALL PLANTE(0)
       STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END