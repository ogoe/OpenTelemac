!                    *******************
                     SUBROUTINE MT02AA_2
!                    *******************
!
     &( A11 , A12 , A13 ,
     &        A22 , A23 ,
     &              A33 ,
     &  XMUL,SU,SV,U,V,
     &  XEL,YEL,SURFAC,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE DIFFUSION TERM FOR ESTEL2D.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        28/11/94
!+        V5P8
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
!| A11            |<--| ELEMENTS OF MATRIX
!| A12            |<--| ELEMENTS OF MATRIX
!| A13            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SURFAC         |-->| AREA OF TRIANGLES
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| U              |-->| FUNCTION U USED IN THE FORMULA
!| V              |-->| FUNCTION V USED IN THE FORMULA
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT02AA_2 => MT02AA_2
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
!     STRUCTURE OF U AND V
      TYPE(BIEF_OBJ)  , INTENT(IN) :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM,IELMNU,IELMNV,ISOU,ISOV
!
      DOUBLE PRECISION X2,X3,Y2,Y3
      DOUBLE PRECISION KSAT1,KSAT2,KSAT3
      DOUBLE PRECISION SOM,XSUR12
!
!=======================================================================
!
!     EXTRACTS THE TYPE OF ELEMENT FOR VISCOSITY
!
      IELMNU = SU%ELM
      ISOU   = SU%DIM2
!
      IELMNV = SV%ELM
      ISOV   = SV%DIM2
!
      XSUR12 = XMUL / 12.D0
!
!-----------------------------------------------------------------------
! TESTS THE TYPES OF U AND V
! U (KR) : P0 AND DIM 3 (BECAUSE DISCONTINUOUS P1) - V (KS) : P0 AND DIM 3
!-----------------------------------------------------------------------
!
      IF(IELMNU.EQ.10.AND.ISOU.EQ.3.AND.SU%DIMDISC.EQ.11
     &   .AND.
     &   IELMNV.EQ.10.AND.ISOV.EQ.3) THEN
!
      DO 4 IELEM = 1 , NELEM
!
! THE 3 TERMS OF MATRIX V (KS IS SYMMETRICAL)
!
        KSAT1=SV%R(IELEM)
        KSAT2=SV%R(IELEM+NELEM)
        KSAT3=SV%R(IELEM+2*NELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
         X2  =  XEL(IELEM,2)
         X3  =  XEL(IELEM,3)
!
         Y2  =  YEL(IELEM,2)
         Y3  =  YEL(IELEM,3)
!
!   INITIALISES THE INTERMEDIATE VARIABLES
!
         SOM = ( SU%R(IELEM+2*NELEM)
     &      +   SU%R(IELEM+NELEM)
     &      +   SU%R(IELEM) ) * XSUR12 / SURFAC(IELEM)
!
!  DIAGONAL TERMS
!
      A11(IELEM) = (KSAT1*Y2**2-2*KSAT1*Y2*Y3+KSAT1*Y3**2+KSAT2*X2**2-
     &  2*KSAT2*X2*X3+KSAT2*X3**2-2*KSAT3*Y2*X2+2*KSAT3*Y2*X3+
     &  2*KSAT3*X2*Y3-2*KSAT3*Y3*X3)*SOM
!
      A22(IELEM) = (KSAT1*Y3**2+KSAT2*X3**2-2*KSAT3*Y3*X3)*SOM
!
      A33(IELEM) = (KSAT1*Y2**2+KSAT2*X2**2-2*KSAT3*Y2*X2)*SOM
!
!  EXTRADIAGONAL TERMS
!
      A12(IELEM) = -(-KSAT1*Y2*Y3+KSAT1*Y3**2-KSAT2*X2*X3+KSAT2*X3**2+
     &          KSAT3*X2*Y3-2*KSAT3*Y3*X3+KSAT3*Y2*X3)*SOM
!
      A13(IELEM) = -(KSAT1*Y2**2-KSAT1*Y2*Y3+KSAT2*X2**2-KSAT2*X2*X3-
     &          2*KSAT3*Y2*X2+KSAT3*Y2*X3+KSAT3*X2*Y3)*SOM
!
      A23(IELEM) = (-KSAT1*Y2*Y3-KSAT2*X2*X3+KSAT3*Y2*X3+KSAT3*X2*Y3)*
     &          SOM
!
!   END OF THE LOOP ON THE ELEMENTS
!
4     CONTINUE
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,10)
        IF (LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'MT02AA_2 (BIEF) : TYPES NON PREVUS')
11      FORMAT(1X,
     &  'MT02AA_2 (BIEF) : TYPES NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
