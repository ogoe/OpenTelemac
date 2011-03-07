!                    *****************
                     SUBROUTINE MT99AA
!                    *****************
!
     &( A11 , A12 , A13 ,
     &  A21 , A22 , A23 ,
     &  A31 , A32 , A33 ,
     &  XMUL,SF,F,XEL,YEL,
     &  SURFAC,IKLE1,IKLE2,IKLE3,NELEM,NELMAX,FORMUL,TDIA,TEXT)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                       /     DF             D
!+          A    = XMUL /  F * -- * PSI2(J) * --( PSI1(I) ) D(O
!+           I J       /S      DX             DX
!+
!+     BY ELEMENT - THE ELEMENT IS THE P1 TRIANGLE
!+
!+     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        28/11/94
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
!| A21            |---| 
!| A22            |---| 
!| A23            |---| 
!| A31            |---| 
!| A32            |---| 
!| A33            |---| 
!| FORMUL         |---| 
!| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!| IKLE2          |---| 
!| IKLE3          |---| 
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| SURFAC         |-->| SURFACE DES TRIANGLES.
!| TDIA           |---| 
!| TEXT           |---| 
!| XMUL           |-->| FACTEUR MULTIPLICATIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT99AA => MT99AA
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
      CHARACTER(LEN=1), INTENT(INOUT) :: TDIA,TEXT
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A21(*),A22(*),A23(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A31(*),A32(*),A33(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,3),YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELEM
      DOUBLE PRECISION SUR48,DET,F1,F2,F3,X2,X3,Y2,Y3,F123,SUR24
!
!=======================================================================
!
      SUR24 = XMUL/24.D0
      SUR48 = XMUL/48.D0
!
!-----------------------------------------------------------------------
!
      IF(SF%ELM.NE.11) THEN
!
        IF (LNG.EQ.1) WRITE(LU,2000) SF%ELM
        IF (LNG.EQ.2) WRITE(LU,2001) SF%ELM
2000    FORMAT(1X,'MT99AA (BIEF) : TYPE DE F : ',I6,' NON PREVU')
2001    FORMAT(1X,'MT99AA (BIEF) : TYPE OF F:',I6,' NOT IMPLEMENTED')
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(FORMUL(8:16).EQ.'     0XX0') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO 1 IELEM = 1 , NELEM
!
      DET = SUR48 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(Y2-Y3)*(2*F1+F2+F3)*DET
      A12(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*Y3*(2*F1+F2+F3)*DET
      A13(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*Y2*(2*F1+F2+F3)*DET
      A21(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(Y2-Y3)*(F1+2*F2+F3)*DET
      A22(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*Y3*(F1+2*F2+F3)*DET
      A23(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*Y2*(F1+2*F2+F3)*DET
      A31(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(Y2-Y3)*(F1+F2+2*F3)*DET
      A32(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*Y3*(F1+F2+2*F3)*DET
      A33(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*Y2*(F1+F2+2*F3)*DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
1     CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     0YY0') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO 2 IELEM = 1 , NELEM
!
      DET = SUR48 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*(-X2+X3)*(2*F1+F2+F3)*DET
      A12(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)*X3*(2*F1+F2+F3)*DET
      A13(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*X2*(2*F1+F2+F3)*DET
      A21(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*(-X2+X3)*(F1+2*F2+F3)*DET
      A22(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)*X3*(F1+2*F2+F3)*DET
      A23(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*X2*(F1+2*F2+F3)*DET
      A31(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*(-X2+X3)*(F1+F2+2*F3)*DET
      A32(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)*X3*(F1+F2+2*F3)*DET
      A33(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*X2*(F1+F2+2*F3)*DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
2     CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     XX00') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO 3 IELEM = 1 , NELEM
!
      DET = SUR48 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
      A11(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*2*DET
      A12(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*DET
      A13(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*DET
      A21(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*DET
      A22(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*2*DET
      A23(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*DET
      A31(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*DET
      A32(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*DET
      A33(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)**2*2*DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
3     CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     0X0Y') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO 5 IELEM = 1 , NELEM
!
      DET = SUR48 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2+X3)*(2*F1+F2+F3)*DET
      A12(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2+X3)*(F1+2*F2+F3)*DET
      A13(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(-X2+X3)*(F1+F2+2*F3)*DET
      A21(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*X3*(2*F1+F2+F3)*DET
      A22(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*X3*(F1+2*F2+F3)*DET
      A23(IELEM)=(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*X3*(F1+F2+2*F3)*DET
      A31(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*X2*(2*F1+F2+F3)*DET
      A32(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*X2*(F1+2*F2+F3)*DET
      A33(IELEM)=-(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*X2*(F1+F2+2*F3)*DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
5     CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     XY00') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO 6 IELEM = 1 , NELEM
!
      DET = SUR48 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*2*DET
      A12(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*DET
      A13(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*DET
      A21(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*DET
      A22(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*2*DET
      A23(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*DET
      A31(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*DET
      A32(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*DET
      A33(IELEM)=
     &(-Y2*F1+Y3*F1-Y3*F2+Y2*F3)*(X2*F1-X3*F1+X3*F2-X2*F3)*2*DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
6     CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     YY00') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO 7 IELEM = 1 , NELEM
!
      DET = SUR48 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*2*DET
      A12(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*DET
      A13(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*DET
      A21(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*DET
      A22(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*2*DET
      A23(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*DET
      A31(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*DET
      A32(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*DET
      A33(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)**2*2*DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
7     CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     0Y0X') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO 9 IELEM = 1 , NELEM
!
      DET = SUR48 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
!
      A11(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*(Y2-Y3)*(2*F1+F2+F3)*DET
      A12(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*(Y2-Y3)*(F1+2*F2+F3)*DET
      A13(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*(Y2-Y3)*(F1+F2+2*F3)*DET
      A21(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*Y3*(2*F1+F2+F3)*DET
      A22(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*Y3*(F1+2*F2+F3)*DET
      A23(IELEM)=-(X2*F1-X3*F1+X3*F2-X2*F3)*Y3*(F1+F2+2*F3)*DET
      A31(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)*Y2*(2*F1+F2+F3)*DET
      A32(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)*Y2*(F1+2*F2+F3)*DET
      A33(IELEM)=(X2*F1-X3*F1+X3*F2-X2*F3)*Y2*(F1+F2+2*F3)*DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
9     CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'00XX+00YY') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO 10 IELEM = 1 , NELEM
!
      DET = SUR24 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
      F123 = F2**2+F1*F2+F2*F3+F3**2+F1**2+F1*F3
!
!  ELEMENTS OUTSIDE OF THE DIAGONAL
!
      A12(IELEM) = (Y2*Y3-Y3**2+X2*X3-X3**2)*F123*DET
      A13(IELEM) = -(Y2**2-Y2*Y3+X2**2-X2*X3)*F123*DET
      A23(IELEM) = -(Y2*Y3+X2*X3)*F123*DET
!
!  DIAGONAL TERMS
!
      A11(IELEM) = (Y2**2-2*Y2*Y3+Y3**2+X2**2-2*X2*X3+X3**2)*F123*DET
      A22(IELEM) = (Y3**2+X3**2)*F123*DET
      A33(IELEM) = (Y2**2+X2**2)*F123*DET
!
!  SYMMETRIES
!
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
!
!   END OF THE LOOP ON THE ELEMENTS
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     00XX') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      DET = SUR24 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
      F123 = F2**2+F1*F2+F2*F3+F3**2+F1**2+F1*F3
!
!  ELEMENTS OUTSIDE OF THE DIAGONAL
!
      A12(IELEM) = (Y2*Y3-Y3**2)*F123*DET
      A13(IELEM) = -(Y2**2-Y2*Y3)*F123*DET
      A23(IELEM) = -(Y2*Y3)*F123*DET
!
!  DIAGONAL TERMS
!
      A11(IELEM) = (Y2**2-2*Y2*Y3+Y3**2)*F123*DET
      A22(IELEM) = (Y3**2)*F123*DET
      A33(IELEM) = (Y2**2)*F123*DET
!
!  SYMMETRIES
!
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     00YY') THEN
!
      TDIA='Q'
      TEXT='Q'
!     SYMMETRY NOT TAKEN INTO ACCOUNT
!     TEXT='S'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      DET = SUR24 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
      F123 = F2**2+F1*F2+F2*F3+F3**2+F1**2+F1*F3
!
!  ELEMENTS OUTSIDE OF THE DIAGONAL
!
      A12(IELEM) = (X2*X3-X3**2)*F123*DET
      A13(IELEM) = -(X2**2-X2*X3)*F123*DET
      A23(IELEM) = -(X2*X3)*F123*DET
!
!  DIAGONAL TERMS
!
      A11(IELEM) = (X2**2-2*X2*X3+X3**2)*F123*DET
      A22(IELEM) = (X3**2)*F123*DET
      A33(IELEM) = (X2**2)*F123*DET
!
!  SYMMETRIES
!
      A21(IELEM) = A12(IELEM)
      A31(IELEM) = A13(IELEM)
      A32(IELEM) = A23(IELEM)
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(8:16).EQ.'     00XY') THEN
!
      TDIA='Q'
      TEXT='Q'
!
!   LOOP ON THE ELEMENTS
!
      DO IELEM = 1 , NELEM
!
      DET = SUR24 / SURFAC(IELEM)
!
!   INITIALISES THE GEOMETRICAL VARIABLES
!
      X2 = XEL(IELEM,2)
      X3 = XEL(IELEM,3)
      Y2 = YEL(IELEM,2)
      Y3 = YEL(IELEM,3)
!
      F1 = F(IKLE1(IELEM))
      F2 = F(IKLE2(IELEM))
      F3 = F(IKLE3(IELEM))
      F123 = F2**2+F1*F2+F2*F3+F3**2+F1**2+F1*F3
!
      A11(IELEM) =  (Y2-Y3)*(-X2+X3)*F123*DET
      A12(IELEM) =      Y3 *(-X2+X3)*F123*DET
      A13(IELEM) =  -Y2    *(-X2+X3)*F123*DET
      A21(IELEM) = -(Y2-Y3)*     X3 *F123*DET
      A22(IELEM) =     -Y3 *     X3 *F123*DET
      A23(IELEM) =   Y2    *     X3 *F123*DET
      A31(IELEM) =  (Y2-Y3)*  X2    *F123*DET
      A32(IELEM) =      Y3 *  X2    *F123*DET
      A33(IELEM) =  -Y2    *  X2    *F123*DET
!
!   END OF THE LOOP ON THE ELEMENTS
!
      ENDDO
!
!   CASE NOT IMPLEMENTED
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
        IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
1000    FORMAT(1X,'MT99AA (BIEF) : MATRICE NON PREVUE : ',A16)
1001    FORMAT(1X,'MT99AA (BIEF) : MATRIX NOT IMPLEMENTED:',A16)
        CALL PLANTE(0)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END