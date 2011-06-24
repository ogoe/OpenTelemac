!                    *****************
                     SUBROUTINE VC13TT
!                    *****************
!
     &( XMUL,SF,F,X,Y,Z,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,
     &  W1,W2,W3,W4,ICOORD , FORMUL )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)
!+
!+                       /            DF
!+    VEC(I)  =  XMUL   /     ( P  *( --  )) D(OMEGA)
!+                     /OMEGA    I    DX
!+
!+    P   IS A LINEAR BASE
!+     I
!+
!+    F IS A VECTOR OF TYPE P1 OR OTHER
!+
!+    IN THIS CASE THE ELEMENT IS A LINEAR TETRAHEDRON
!
!note     IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!+                     HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!+                     P1, BUT GIVEN BY ELEMENTS.
!+                     THE SIZE OF F SHOULD THEN BE : F(NELMAX,3).
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM - REAL MESH
!
!history  J-M HERVOUET (LNH)
!+        25/03/02
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| FORMUL         |-->| SEE AT THE END OF THE SUBROUTINE
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINT OF TETRAHEDRA
!| IKLE2          |-->| SECOND POINT OF TETRAHEDRA
!| IKLE3          |-->| THIRD POINT OF TETRAHEDRA
!| IKLE4          |-->| FOURTH POINT OF TETRAHEDRA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF !, EX_VC13TT => VC13TT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,ICOORD
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF
      DOUBLE PRECISION X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4,XSUR24
      INTEGER I1,I2,I3,I4
!
!-----------------------------------------------------------------------
!
      XSUR24 = XMUL/24.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
!
!=======================================================================
!
!     F IS LINEAR
!
      IF(IELMF.EQ.31.OR.IELMF.EQ.51) THEN
!
      IF(ICOORD.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!  DERIVATIVE WRT X
!
      DO 3 IELEM = 1 , NELEM
!
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
!
!  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
         Y2  =  Y(I2) - Y(I1)
         Y3  =  Y(I3) - Y(I1)
         Y4  =  Y(I4) - Y(I1)
         Z2  =  Z(I2) - Z(I1)
         Z3  =  Z(I3) - Z(I1)
         Z4  =  Z(I4) - Z(I1)
!
         W1(IELEM)=(  (F(I2)-F(I1))*(Y3*Z4-Y4*Z3)
     &               +(F(I3)-F(I1))*(Z2*Y4-Y2*Z4)
     &               +(F(I4)-F(I1))*(Y2*Z3-Z2*Y3)  )*XSUR24
!
         W2(IELEM)=W1(IELEM)
         W3(IELEM)=W1(IELEM)
         W4(IELEM)=W1(IELEM)
!
3     CONTINUE
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!-----------------------------------------------------------------------
!
!  DERIVATIVE WRT Y
!
      DO IELEM = 1 , NELEM
!
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
!
!  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
         X2  =  X(I2) - X(I1)
         X3  =  X(I3) - X(I1)
         X4  =  X(I4) - X(I1)
         Z2  =  Z(I2) - Z(I1)
         Z3  =  Z(I3) - Z(I1)
         Z4  =  Z(I4) - Z(I1)
!
         W1(IELEM)=(  (F(I2)-F(I1))*(X4*Z3-X3*Z4)
     &               +(F(I3)-F(I1))*(X2*Z4-Z2*X4)
     &               +(F(I4)-F(I1))*(Z2*X3-X2*Z3)  )*XSUR24
!
         W2(IELEM)=W1(IELEM)
         W3(IELEM)=W1(IELEM)
         W4(IELEM)=W1(IELEM)
!
      ENDDO
!
      ELSEIF(ICOORD.EQ.3) THEN
!
!-----------------------------------------------------------------------
!
!  DERIVATIVE WRT Z
!
      DO 5 IELEM = 1 , NELEM
!
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
!
!  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
         X2  =  X(I2) - X(I1)
         X3  =  X(I3) - X(I1)
         X4  =  X(I4) - X(I1)
         Y2  =  Y(I2) - Y(I1)
         Y3  =  Y(I3) - Y(I1)
         Y4  =  Y(I4) - Y(I1)
!
         W1(IELEM)=(  (F(I2)-F(I1))*(X3*Y4-X4*Y3)
     &               +(F(I3)-F(I1))*(Y2*X4-X2*Y4)
     &               +(F(I4)-F(I1))*(X2*Y3-Y2*X3)  )*XSUR24
!
         W2(IELEM)=W1(IELEM)
         W3(IELEM)=W1(IELEM)
         W4(IELEM)=W1(IELEM)
!
5     CONTINUE
!
      ELSE
!
!-----------------------------------------------------------------------
!
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
200       FORMAT(1X,'VC13TT (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'VC13TT (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(1)
          STOP
!
      ENDIF
!
!=======================================================================
!
      ELSE
!
!=======================================================================
!
       IF (LNG.EQ.1) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,102) IELMF,SF%NAME
101    FORMAT(1X,'VC13TT (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,' CAS NON PREVU',/,
     &        1X,'NOM REEL DE F : ',A6)
102    FORMAT(1X,'VC13TT (BIEF) :',/,
     &        1X,'DISCRETISATION OF F : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF F: ',A6)
       CALL PLANTE(1)
       STOP
!
      ENDIF
!
!=======================================================================
!
!  HYDROSTATIC INCONSISTENCIES
!
!     IF(FORMUL(6:6).EQ.'2') THEN
!
!     DO IELEM = 1 , NELEM
!
!        I1 = IKLE1(IELEM)
!        I2 = IKLE2(IELEM)
!        I3 = IKLE3(IELEM)
!        I4 = IKLE4(IELEM)
!        I5 = IKLE5(IELEM)
!        I6 = IKLE6(IELEM)
!
!        IF(MAX(Z(I1),Z(I2),Z(I3)).GT.MIN(Z(I4),Z(I5),Z(I6))) THEN
!          W1(IELEM)=0.D0
!          W2(IELEM)=0.D0
!          W3(IELEM)=0.D0
!          W4(IELEM)=0.D0
!          W5(IELEM)=0.D0
!          W6(IELEM)=0.D0
!        ENDIF
!
!     ENDDO
!
!     ENDIF
!
!=======================================================================
!
      RETURN
      END
