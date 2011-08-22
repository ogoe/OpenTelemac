!                    *****************
                     SUBROUTINE VC13AA
!                    *****************
!
     &( XMUL,SF,F,XEL,YEL,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3 , ICOORD )
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
!
!note     IMPORTANT : IF F IS OF TYPE P0, THE RESULT IS 0.
!+                     HERE, IF F IS P0, IT REALLY MEANS THAT F IS
!+                     P1, BUT GIVEN BY ELEMENTS.
!+                     THE SIZE OF F SHOULD THEN BE : F(NELMAX,3).
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        09/12/94
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
!| F              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| ICOORD         |-->| 1: DERIVATIVE ALONG X, 2: ALONG Y
!| IKLE1          |-->| FIRST POINT OF TRIANGLES
!| IKLE2          |-->| SECOND POINT OF TRIANGLES
!| IKLE3          |-->| THIRD POINT OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SF             |-->| BIEF_OBJ STRUCTURE OF F
!| SURFAC         |-->| AREA OF TRIANGLES
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| XEL            |-->| ABSCISSAE OF POINTS IN THE MESH, PER ELEMENT
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| YEL            |-->| ORDINATES OF POINTS IN THE MESH, PER ELEMENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF  !, EX_VC13AA => VC13AA
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
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
!
!     STRUCTURE OF F AND REAL DATA
!
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
      DOUBLE PRECISION, INTENT(IN) :: F(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IELMF,DISCF
      DOUBLE PRECISION XSUR6,XSUR18,F1,F2,F3,F4,X2,X3,Y2,Y3
!
!-----------------------------------------------------------------------
!
      XSUR6 = XMUL / 6.D0
      XSUR18= XMUL /18.D0
!
!-----------------------------------------------------------------------
!
      IELMF=SF%ELM
      DISCF = SF%DIMDISC
!
!-----------------------------------------------------------------------
!
!     F IS LINEAR
!
      IF(IELMF.EQ.11) THEN
!
!  X COORDINATE
!
      IF(ICOORD.EQ.1) THEN
!
      DO 1 IELEM = 1 , NELEM
!
        W1(IELEM) = ( YEL(IELEM,2) *
     &                (F(IKLE1(IELEM))-F(IKLE3(IELEM)))
     &              + YEL(IELEM,3) *
     &                (F(IKLE2(IELEM))-F(IKLE1(IELEM))) ) * XSUR6
        W2(IELEM) = W1(IELEM)
        W3(IELEM) = W1(IELEM)
!
1     CONTINUE
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!  Y COORDINATE
!
      DO 2 IELEM = 1 , NELEM
!
         W1(IELEM) = ( XEL(IELEM,2) *
     &                 (F(IKLE3(IELEM))-F(IKLE1(IELEM)))
     &               + XEL(IELEM,3) *
     &                 (F(IKLE1(IELEM))-F(IKLE2(IELEM))) ) * XSUR6
         W2(IELEM)  =  W1(IELEM)
         W3(IELEM)  =  W1(IELEM)
!
2     CONTINUE
!
      ELSE
!
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
200       FORMAT(1X,'VC13AA (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &              1I6,' VERIFIER ICOORD')
201       FORMAT(1X,'VC13AA (BIEF) : IMPOSSIBLE COMPONENT ',
     &              1I6,' CHECK ICOORD')
          CALL PLANTE(0)
          STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     F IS QUASI-BUBBLE
!
      ELSEIF(IELMF.EQ.12) THEN
!
!  X COORDINATE
!
      IF(ICOORD.EQ.1) THEN
!
      DO 5 IELEM = 1 , NELEM
!
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
!
        W1(IELEM)=(Y2*(-3*F4-2*F3+F2)+Y3*(3*F4-F3+2*F2)) * XSUR18
        W2(IELEM)=(-3*Y2*F3+Y3*(-3*F4+F3+4*F2)) * XSUR18
        W3(IELEM)=(Y2*(3*F4-4*F3-F2)+3*Y3*F2) * XSUR18
!
5     CONTINUE
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!  Y COORDINATE
!
      DO 6 IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
!
        F1 = F(IKLE1(IELEM))
        F2 = F(IKLE2(IELEM)) - F1
        F3 = F(IKLE3(IELEM)) - F1
        F4 = F(IKLE4(IELEM)) - F1
!
        W1(IELEM)=(X2*(3*F4+2*F3-F2)+X3*(-3*F4+F3-2*F2)) * XSUR18
        W2(IELEM)=(3*X2*F3+X3*(3*F4-F3-4*F2)) * XSUR18
        W3(IELEM)=(X2*(-3*F4+4*F3+F2)-3*X3*F2) * XSUR18
!
6     CONTINUE
!
      ELSE
!
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,201) ICOORD
          CALL PLANTE(0)
          STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     BEWARE: HERE F IS LINEAR BUT DISCONTINUOUS BETWEEN THE ELEMENTS
!
      ELSEIF(IELMF.EQ.10.AND.DISCF.EQ.12) THEN
!
!  X COORDINATE
!
      IF(ICOORD.EQ.1) THEN
!
      DO 7 IELEM = 1 , NELEM
!
        Y2 = YEL(IELEM,2)
        Y3 = YEL(IELEM,3)
!
        F1 = F(IELEM)
        F2 = F(IELEM+  NELMAX)-F1
        F3 = F(IELEM+2*NELMAX)-F1
        F4 = F(IELEM+3*NELMAX)-F1
!
        W1(IELEM)=(Y2*(-3*F4-2*F3+F2)+Y3*(3*F4-F3+2*F2)) * XSUR18
        W2(IELEM)=(-3*Y2*F3+Y3*(-3*F4+F3+4*F2)) * XSUR18
        W3(IELEM)=(Y2*(3*F4-4*F3-F2)+3*Y3*F2) * XSUR18
!
7     CONTINUE
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!  Y COORDINATE
!
      DO 8 IELEM = 1 , NELEM
!
        X2 = XEL(IELEM,2)
        X3 = XEL(IELEM,3)
!
        F1 = F(IELEM)
        F2 = F(IELEM+  NELMAX)-F1
        F3 = F(IELEM+2*NELMAX)-F1
        F4 = F(IELEM+3*NELMAX)-F1
!
        W1(IELEM)=(X2*(3*F4+2*F3-F2)+X3*(-3*F4+F3-2*F2)) * XSUR18
        W2(IELEM)=(3*X2*F3+X3*(3*F4-F3-4*F2)) * XSUR18
        W3(IELEM)=(X2*(-3*F4+4*F3+F2)-3*X3*F2) * XSUR18
!
8     CONTINUE
!
      ELSE
!
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,200) ICOORD
          CALL PLANTE(0)
          STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELMF.EQ.10.AND.DISCF.EQ.11) THEN
!
!  X COORDINATE
!
      IF(ICOORD.EQ.1) THEN
!
      DO 3 IELEM = 1 , NELEM
!
        W1(IELEM) = ( YEL(IELEM,2) * (F(IELEM)-F(IELEM+2*NELMAX))
     &              + YEL(IELEM,3) * (F(IELEM+NELMAX)-F(IELEM)))* XSUR6
        W2(IELEM) = W1(IELEM)
        W3(IELEM) = W1(IELEM)
!
3     CONTINUE
!
      ELSEIF(ICOORD.EQ.2) THEN
!
!  Y COORDINATE
!
      DO 4 IELEM = 1 , NELEM
!
         W1(IELEM) = ( XEL(IELEM,2) * (F(IELEM+2*NELMAX)-F(IELEM))
     &               + XEL(IELEM,3) * (F(IELEM)-F(IELEM+NELMAX)))*XSUR6
         W2(IELEM)  =  W1(IELEM)
         W3(IELEM)  =  W1(IELEM)
!
4     CONTINUE
!
      ELSE
!
          IF (LNG.EQ.1) WRITE(LU,200) ICOORD
          IF (LNG.EQ.2) WRITE(LU,200) ICOORD
          CALL PLANTE(0)
          STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
       IF (LNG.EQ.1) WRITE(LU,101) IELMF,SF%NAME
       IF (LNG.EQ.2) WRITE(LU,102) IELMF,SF%NAME
101    FORMAT(1X,'VC13AA (BIEF) :',/,
     &        1X,'DISCRETISATION DE F : ',1I6,' CAS NON PREVU',/,
     &        1X,'NOM REEL DE F : ',A6)
102    FORMAT(1X,'VC13AA (BIEF) :',/,
     &        1X,'DISCRETISATION OF F : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF F: ',A6)
       CALL PLANTE(0)
       STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
