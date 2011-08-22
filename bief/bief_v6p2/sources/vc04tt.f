!                    *****************
                     SUBROUTINE VC04TT
!                    *****************
!
     &(XMUL,SU,SV,SW,U,V,W,X,Y,Z,
     & IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,W1,W2,W3,W4,FORMUL)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!code
!+                  /        D(PSII)       D(PSII)
!+      V  = XMUL  /     U * ------- + V * -------   D(OMEGA)
!+       I        /OMEGA       DX            DY
!+
!+                  /            D(PSII*)           D(PSII*)
!+         = XMUL  /     H * U * -------- + H * V * --------   D(OMEGA*)
!+                /OMEGA*           DX                 DY
!+
!+    PSII IS OF TYPE P1 TETRAHEDRON
!+
!+    REAL MESH HERE, CAN BE REGARDED AS A COMPUTATION
!+    IN A TRANSFORMED MESH, BUT WITH H IN THE INTEGRAL.
!
!warning  THE JACOBIAN MUST BE POSITIVE
!warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM
!
!history  J-M HERVOUET (LNH)
!+        22/03/02
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
!| FORMUL         |-->| STRING WITH FORMULA OF VECTOR
!| IKLE1          |-->| FIRST POINT OF TETRAHEDRA
!| IKLE2          |-->| SECOND POINT OF TETRAHEDRA
!| IKLE3          |-->| THIRD POINT OF TETRAHEDRA
!| IKLE4          |-->| FOURTH POINT OF TETRAHEDRA
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| SU             |-->| BIEF_OBJ STRUCTURE OF U
!| SV             |-->| BIEF_OBJ STRUCTURE OF V
!| SW             |-->| BIEF_OBJ STRUCTURE OF W
!| U              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| V              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W              |-->| FUNCTION USED IN THE VECTOR FORMULA
!| W1             |<--| RESULT IN NON ASSEMBLED FORM
!| W2             |<--| RESULT IN NON ASSEMBLED FORM
!| W3             |<--| RESULT IN NON ASSEMBLED FORM
!| W4             |<--| RESULT IN NON ASSEMBLED FORM
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XMUL           |-->| MULTIPLICATION COEFFICIENT
!| Z              |-->| ELEVATIONS OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_VC04TT => VC04TT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
!
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
!
!     STRUCTURES OF U, V AND REAL DATA
!
      TYPE(BIEF_OBJ),   INTENT(IN) :: SU,SV,SW
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XSUR24,X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,Q1,Q2,Q3,Q4
      INTEGER I1,I2,I3,I4,IELEM,IELMU,IELMV,IELMW
!
!**********************************************************************
!
      XSUR24 = XMUL / 24.D0
!
!-----------------------------------------------------------------------
!
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMW=SW%ELM
!
!-----------------------------------------------------------------------
!
!   HORIZONTAL TERMS
!
      IF(FORMUL(14:16).EQ.'HOR') THEN
!
!   LOOP ON THE ELEMENTS
!
      IF((IELMU.EQ.31.AND.IELMV.EQ.31).OR.
     &   (IELMU.EQ.51.AND.IELMV.EQ.51)     ) THEN
!
!-----------------------------------------------------------------------
!
!  U AND V DISCRETISED IN P1 PRISM:
!
      DO 3 IELEM = 1 , NELEM
!
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
!
         X2 = X(I2) - X(I1)
         X3 = X(I3) - X(I1)
         X4 = X(I4) - X(I1)
!
         Y2 = Y(I2) - Y(I1)
         Y3 = Y(I3) - Y(I1)
         Y4 = Y(I4) - Y(I1)
!
         Z2 = Z(I2) - Z(I1)
         Z3 = Z(I3) - Z(I1)
         Z4 = Z(I4) - Z(I1)
!
         U1 = U(I1)
         U2 = U(I2)
         U3 = U(I3)
         U4 = U(I4)
         V1 = V(I1)
         V2 = V(I2)
         V3 = V(I3)
         V4 = V(I4)
!
         W1(IELEM) = XSUR24*(
     &U3*Z2*Y3+V4*X3*Z4-U4*Y3*Z4+U4*Z2*Y3+U4*Y4*Z3-U4*Z2*Y4-U4*
     &Y2*Z3+U1*Y4*Z3+V1*Z2*X4-V4*X2*Z4-U3*Z2*Y4-U2*Y2*Z3+V4*X2*Z3+V4*Z2*
     &X4-V4*X4*Z3+V2*Z2*X4-U2*Z2*Y4+U2*Y2*Z4+U2*Y4*Z3+U2*Z2*Y3-V1*Z2*X3+
     &V1*X2*Z3-V1*X4*Z3+V1*X3*Z4+U3*Y2*Z4+U3*Y4*Z3-V3*X2*Z4-V3*X4*Z3+V3*
     &X3*Z4-V3*Z2*X3+V3*X2*Z3+V3*Z2*X4+U1*Z2*Y3-U1*Y2*Z3+U1*Y2*Z4-V2*X2*
     &Z4-V2*X4*Z3-V2*Z2*X3+V2*X2*Z3-U3*Y2*Z3+V2*X3*Z4-V4*Z2*X3-U1*Y3*Z4+
     &U4*Y2*Z4-U3*Y3*Z4-V1*X2*Z4-U1*Z2*Y4-U2*Y3*Z4)
!
         W2(IELEM) = XSUR24*(
     &-U4*Y4*Z3+U4*Y3*Z4-V4*X3*Z4+V4*X4*Z3+U1*Y3*Z4-U1*Y4*Z3-V3
     &*X3*Z4+V3*X4*Z3+U3*Y3*Z4-U3*Y4*Z3-V1*X3*Z4+V1*X4*Z3+U2*Y3*Z4-U2*Y4
     &*Z3-V2*X3*Z4+V2*X4*Z3)
!
         W3(IELEM) = XSUR24*(
     &U4*Z2*Y4-U4*Y2*Z4-V4*Z2*X4+U3*Z2*Y4+V4*X2*Z4-V1*Z2*X4-U1*
     &Y2*Z4-V3*Z2*X4+V3*X2*Z4-U3*Y2*Z4+V1*X2*Z4-U2*Y2*Z4+U2*Z2*Y4-V2*Z2*
     &X4+V2*X2*Z4+U1*Z2*Y4)
!
         W4(IELEM) = XSUR24*(
     &U4*Y2*Z3-U4*Z2*Y3+V4*Z2*X3-V4*X2*Z3+U1*Y2*Z3-U1*Z2*Y3+U3*
     &Y2*Z3-U3*Z2*Y3-V3*X2*Z3+V3*Z2*X3-V1*X2*Z3+V1*Z2*X3-U2*Z2*Y3+U2*Y2*
     &Z3-V2*X2*Z3+V2*Z2*X3)
!
3     CONTINUE
!
!-----------------------------------------------------------------------
!
!     ELSEIF(IELMU.EQ.  ) THEN
!
!-----------------------------------------------------------------------
!
      ELSE
!
!-----------------------------------------------------------------------
!
       IF (LNG.EQ.1) WRITE(LU,101) IELMU,SU%NAME
       IF (LNG.EQ.2) WRITE(LU,102) IELMU,SU%NAME
101    FORMAT(1X,'VC04TT (BIEF) :',/,
     &        1X,'DISCRETISATION DE U ET V : ',1I6,' CAS NON PREVU',/,
     &        1X,'NOM REEL DE U : ',A6)
102    FORMAT(1X,'VC04TT (BIEF) :',/,
     &        1X,'DISCRETISATION OF U ET V : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF U : ',A6)
       CALL PLANTE(1)
       STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(FORMUL(14:16).EQ.'VER') THEN
!
      IF(IELMW.NE.31.AND.IELMW.NE.51) THEN
        IF (LNG.EQ.1) WRITE(LU,301)
        IF (LNG.EQ.2) WRITE(LU,302)
301     FORMAT(1X,'VC04TT (BIEF) :',/,
     &         1X,'CAS NON PREVU (IELMW.NE.31 ET .NE.51)')
302     FORMAT(1X,'VC04TT (BIEF) :',/,
     &         1X,'UNEXPECTED CASE (IELMW.NE.31 AND .NE.51)')
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO IELEM = 1 , NELEM
!
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
!
         X2 = X(I2) - X(I1)
         X3 = X(I3) - X(I1)
         X4 = X(I4) - X(I1)
!
         Y2 = Y(I2) - Y(I1)
         Y3 = Y(I3) - Y(I1)
         Y4 = Y(I4) - Y(I1)
!
         Q1 = W(I1)
         Q2 = W(I2)
         Q3 = W(I3)
         Q4 = W(I4)
!
         W1(IELEM) = (
     &         X4*Y3*Q4-Y2*X4*Q4+X2*Y4*Q4-X2*Y3*Q4-X3*Y4*Q4+Y2*X3*Q4-X2*
     &Y3*Q3-Y2*X4*Q3+X2*Y4*Q1-X3*Y4*Q3+X2*Y4*Q3+Y2*X3*Q3-X3*Y4*Q2+X4*Y3*
     &Q2+X2*Y4*Q2-Y2*X4*Q2-X2*Y3*Q2+Y2*X3*Q2+Y2*X3*Q1-Y2*X4*Q1-X2*Y3*Q1+
     &X4*Y3*Q3+X4*Y3*Q1-X3*Y4*Q1 )*XSUR24
!
         W2(IELEM) = (
     &         -X4*Y3*Q4+X3*Y4*Q4+X3*Y4*Q3+X3*Y4*Q2-X4*Y3*Q2-X4*Y3*Q3-X4
     &*Y3*Q1+X3*Y4*Q1 )*XSUR24
!
         W3(IELEM) = (
     &         Y2*X4*Q4-X2*Y4*Q4+Y2*X4*Q3-X2*Y4*Q1-X2*Y4*Q3-X2*Y4*Q2+Y2*
     &X4*Q2+Y2*X4*Q1 )*XSUR24
!
         W4(IELEM) = (
     &         X2*Y3*Q4-Y2*X3*Q4+X2*Y3*Q3-Y2*X3*Q3+X2*Y3*Q2-Y2*X3*Q2-Y2*
     &X3*Q1+X2*Y3*Q1 )*XSUR24
!
      ENDDO
!
      ELSE
!
!-----------------------------------------------------------------------
!
       IF (LNG.EQ.1) WRITE(LU,201) FORMUL
       IF (LNG.EQ.2) WRITE(LU,202) FORMUL
201    FORMAT(1X,'VC04TT (BIEF) :',/,
     &        1X,'IL MANQUE HOR OU VER EN FIN DE FORMULE : ',A16)
202    FORMAT(1X,'VC04TT (BIEF):',/,
     &        1X,'HOR OR VER LACKING AT THE END OF THE FORMULA : ',A16)
       CALL PLANTE(1)
       STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
