!                    *****************
                     SUBROUTINE MT06FF
!                    *****************
!
     &( A11 , A12 , A13 , A14 ,
     &        A22 , A23 , A24 ,
     &              A33 , A34 ,
     &                    A44 ,
     &  XMUL,SF,F,X,Y,Z,IKLE1,IKLE2,IKLE3,IKLE4,NBOR,NELEM,NELMAX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!code
!+                              /
!+                    A    =   /  F * (P *P )*J(X,Y) DXDY
!+                     I J    /S        I  J
!+
!+     BY ELEMENTARY CELL; !! THE ELEMENT IS THE Q1 QUADRILATERAL, IN A PRISM MESH !!
!+
!+     J(X,Y): JACOBIAN OF THE ISOPARAMETRIC TRANSFORMATION
!
!warning  THE JACOBIAN MUST BE POSITIVE
!
!history  J-M HERVOUET (LNH)
!+        18/08/94
!+        V5P3
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
!| A14            |<--| ELEMENTS OF MATRIX
!| A22            |<--| ELEMENTS OF MATRIX
!| A23            |<--| ELEMENTS OF MATRIX
!| A24            |<--| ELEMENTS OF MATRIX
!| A33            |<--| ELEMENTS OF MATRIX
!| A34            |<--| ELEMENTS OF MATRIX
!| A44            |<--| ELEMENTS OF MATRIX
!| F              |-->| FUNCTION F USED IN THE FORMULA
!| IKLE1          |-->| FIRST POINTS OF QUADRILATERALS
!| IKLE2          |-->| SECOND POINTS OF QUADRILATERALS
!| IKLE3          |-->| THIRD POINTS OF QUADRILATERALS
!| IKLE4          |-->| FOURTH POINTS OF QUADRILATERALS
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| XMUL           |-->| MULTIPLICATION FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MT06FF => MT06FF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NBOR(*),NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(INOUT) :: A11(*),A12(*),A13(*),A14(*)
      DOUBLE PRECISION, INTENT(INOUT) ::        A22(*),A23(*),A24(*)
      DOUBLE PRECISION, INTENT(INOUT) ::               A33(*),A34(*)
      DOUBLE PRECISION, INTENT(INOUT) ::                      A44(*)
!
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!     STRUCTURE OF F
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC SQRT
!
!     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
!
      INTEGER IELMF,I1,I2,I3,I4,IELEM
!
      DOUBLE PRECISION SUR720,AL,S1,S2,S11112,S11122,S11222,S12222
      DOUBLE PRECISION F14,F23,F1114,F2223,F2333,F1444
!
!**********************************************************************
!
      IELMF=SF%ELM
!
!-----------------------------------------------------------------------
!
!     F LINEAR BY BOUNDARY SIDE
!
      IF(IELMF.EQ.71) THEN
!
         SUR720  = XMUL/720.D0
!
!   LOOP ON THE BOUNDARY SIDES
!
         DO 1 IELEM = 1,NELEM
!
!  GLOBAL NUMBERING OF THE SIDE VERTICES
!
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
!
            AL = SQRT((X(NBOR(I2))-X(NBOR(I1)))**2
     &               +(Y(NBOR(I2))-Y(NBOR(I1)))**2) * SUR720
!
            S1 = (Z(NBOR(I4)) - Z(NBOR(I1))) * AL
            S2 = (Z(NBOR(I3)) - Z(NBOR(I2))) * AL
            S11112 = S1 + S1 + S1 + S1 + S2
            S11122 = S1 + S1 + S1 + S2 + S2
            S11222 = S1 + S1 + S2 + S2 + S2
            S12222 = S1 + S2 + S2 + S2 + S2
!
            F14 = F(I1) + F(I4)
            F23 = F(I2) + F(I3)
            F1114 = F(I1) + F(I1) + F14
            F2223 = F(I2) + F(I2) + F23
            F2333 = F23 + F(I3) + F(I3)
            F1444 = F14 + F(I4) + F(I4)
!
!  DIAGONAL TERMS
!
            A11(IELEM) = 3*F1114*S11112 + F2223*S11122
            A22(IELEM) = 3*F2223*S12222 + F1114*S11222
            A33(IELEM) = 3*F2333*S12222 + F1444*S11222
            A44(IELEM) = 3*F1444*S11112 + F2333*S11122
!
!  ELEMENTS OFF THE DIAGONAL
!
            A12(IELEM) = F1114*S11122 + F2223*S11222
            A13(IELEM) =   F14*S11122 +   F23*S11222
            A14(IELEM) = 3*F14*S11112 +   F23*S11122
            A23(IELEM) = 3*F23*S12222 +   F14*S11222
            A24(IELEM) = A13(IELEM)
            A34(IELEM) = F2333*S11222 + F1444*S11122
!
1        CONTINUE
!
!-----------------------------------------------------------------------
!
!     OTHER TYPES OF DISCRETISATION OF F
!
      ELSE
!
         IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
         IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
100      FORMAT(1X,'MT06FF (BIEF) :',/,
     &          1X,'DISCRETISATION DE F NON PREVUE : ',1I6,
     &          1X,'NOM REEL : ',A6)
101      FORMAT(1X,'MT06FF (BIEF) :',/,
     &          1X,'DISCRETIZATION OF F NOT AVAILABLE:',1I6,
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
