!                    *****************
                     SUBROUTINE MV0404
!                    *****************
!
     &(OP, X , DA,TYPDIA,
     & XA12,XA13,XA14,XA21,XA23,XA24,XA31,XA32,XA34,XA41,XA42,XA43,
     & TYPEXT, Y,C,IKLE1,IKLE2,IKLE3,IKLE4,NPOIN,NELEM,W1,W2,W3,W4)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MATRIX VECTOR OPERATIONS FOR Q1 QUADRILATERALS.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND MATRIX M.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   THESE OPERATIONS ARE DIFFERENT DEPENDING ON THE DIAGONAL TYPE
!+   AND THE TYPE OF EXTRADIAGONAL TERMS.
!+
!+   IMPLEMENTED OPERATIONS:
!+
!+      OP = 'X=AY    '  : X = AY
!+      OP = 'X=CAY   '  : X = CAY
!+      OP = 'X=-AY   '  : X = - AY
!+      OP = 'X=X+AY  '  : X = X + AY
!+      OP = 'X=X-AY  '  : X = X - AY
!+      OP = 'X=X+CAY '  : X = X + C AY
!+      OP = 'X=TAY   '  : X = TA Y (TRANSPOSE OF A)
!+      OP = 'X=-TAY  '  : X = - TA Y (- TRANSPOSE OF A)
!+      OP = 'X=X+TAY '  : X = X + TA Y
!+      OP = 'X=X-TAY '  : X = X - TA Y
!+      OP = 'X=X+CTAY'  : X = X + C TA Y
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
!| C              |-->| CONSTANTE DONNEE
!| DA             |-->| DIAGONALE DE LA MATRICE
!| IKLE2          |---| 
!| IKLE3          |---| 
!| IKLE4          |---| 
!| NELEM          |-->| NOMBRE D'ELEMENTS.
!| NPOIN          |-->| NOMBRE DE POINTS.
!| OP             |-->| OPERATION A EFFECTUER
!| TYPDIA         |-->| TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
!|                |   | TYPDIA = 'Q' : DIAGONALE QUELCONQUE
!|                |   | TYPDIA = 'I' : DIAGONALE IDENTITE.
!|                |   | TYPDIA = '0' : DIAGONALE NULLE.
!| TYPEXT         |-->| TYPEXT = 'Q' : QUELCONQUES.
!|                |   | TYPEXT = 'S' : SYMETRIQUES.
!|                |   | TYPEXT = '0' : NULS.
!| W2             |---| 
!| W3             |---| 
!| W4             |---| 
!| X              |<--| VECTEUR IMAGE
!| XA13           |---| 
!| XA14           |---| 
!| XA21           |---| 
!| XA23           |---| 
!| XA24           |---| 
!| XA31           |---| 
!| XA32           |---| 
!| XA34           |---| 
!| XA41           |---| 
!| XA42           |---| 
!| XA43           |---| 
!| Y              |-->| VECTEUR OPERANDE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MV0404 => MV0404
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN
!
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*),IKLE4(*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*),W4(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: XA12(*),XA13(*),XA14(*)
      DOUBLE PRECISION, INTENT(IN) :: XA21(*),XA23(*),XA24(*)
      DOUBLE PRECISION, INTENT(IN) :: XA31(*),XA32(*),XA34(*)
      DOUBLE PRECISION, INTENT(IN) :: XA41(*),XA42(*),XA43(*)
      DOUBLE PRECISION, INTENT(IN) :: C
!
      CHARACTER(LEN=8), INTENT(IN) :: OP
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'X=AY    ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO IELEM = 1 , NELEM
             W1(IELEM) =     XA12(IELEM) * Y(IKLE2(IELEM))
     &                     + XA13(IELEM) * Y(IKLE3(IELEM))
     &                     + XA14(IELEM) * Y(IKLE4(IELEM))
             W2(IELEM) =     XA21(IELEM) * Y(IKLE1(IELEM))
     &                     + XA23(IELEM) * Y(IKLE3(IELEM))
     &                     + XA24(IELEM) * Y(IKLE4(IELEM))
             W3(IELEM) =     XA31(IELEM) * Y(IKLE1(IELEM))
     &                     + XA32(IELEM) * Y(IKLE2(IELEM))
     &                     + XA34(IELEM) * Y(IKLE4(IELEM))
             W4(IELEM) =     XA41(IELEM) * Y(IKLE1(IELEM))
     &                     + XA42(IELEM) * Y(IKLE2(IELEM))
     &                     + XA43(IELEM) * Y(IKLE3(IELEM))
           END DO
!
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
!
         ELSE
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , Z  , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CAY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 20 IELEM = 1 , NELEM
             W1(IELEM) =  C * (   XA12(IELEM) * Y(IKLE2(IELEM))
     &                          + XA13(IELEM) * Y(IKLE3(IELEM))
     &                          + XA14(IELEM) * Y(IKLE4(IELEM)) )
             W2(IELEM) =  C * (   XA21(IELEM) * Y(IKLE1(IELEM))
     &                          + XA23(IELEM) * Y(IKLE3(IELEM))
     &                          + XA24(IELEM) * Y(IKLE4(IELEM)) )
             W3(IELEM) =  C * (   XA31(IELEM) * Y(IKLE1(IELEM))
     &                          + XA32(IELEM) * Y(IKLE2(IELEM))
     &                          + XA34(IELEM) * Y(IKLE4(IELEM)) )
             W4(IELEM) =  C * (   XA41(IELEM) * Y(IKLE1(IELEM))
     &                          + XA42(IELEM) * Y(IKLE2(IELEM))
     &                          + XA43(IELEM) * Y(IKLE3(IELEM)) )
20         CONTINUE
!
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
!
         ELSE
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=CYZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=CY    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , Z  , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-AY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).NE.'S') THEN
!
           DO 21 IELEM = 1 , NELEM
             W1(IELEM) =   - XA12(IELEM) * Y(IKLE2(IELEM))
     &                     - XA13(IELEM) * Y(IKLE3(IELEM))
     &                     - XA14(IELEM) * Y(IKLE4(IELEM))
             W2(IELEM) =   - XA21(IELEM) * Y(IKLE1(IELEM))
     &                     - XA23(IELEM) * Y(IKLE3(IELEM))
     &                     - XA24(IELEM) * Y(IKLE4(IELEM))
             W3(IELEM) =   - XA31(IELEM) * Y(IKLE1(IELEM))
     &                     - XA32(IELEM) * Y(IKLE2(IELEM))
     &                     - XA34(IELEM) * Y(IKLE4(IELEM))
             W4(IELEM) =   - XA41(IELEM) * Y(IKLE1(IELEM))
     &                     - XA42(IELEM) * Y(IKLE2(IELEM))
     &                     - XA43(IELEM) * Y(IKLE3(IELEM))
21         CONTINUE
!
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
!
         ELSE
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , Z  , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+AY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 40  IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) + XA12(IELEM) * Y(IKLE2(IELEM))
     &                             + XA13(IELEM) * Y(IKLE3(IELEM))
     &                             + XA14(IELEM) * Y(IKLE4(IELEM))
             W2(IELEM) = W2(IELEM) + XA21(IELEM) * Y(IKLE1(IELEM))
     &                             + XA23(IELEM) * Y(IKLE3(IELEM))
     &                             + XA24(IELEM) * Y(IKLE4(IELEM))
             W3(IELEM) = W3(IELEM) + XA31(IELEM) * Y(IKLE1(IELEM))
     &                             + XA32(IELEM) * Y(IKLE2(IELEM))
     &                             + XA34(IELEM) * Y(IKLE4(IELEM))
             W4(IELEM) = W4(IELEM) + XA41(IELEM) * Y(IKLE1(IELEM))
     &                             + XA42(IELEM) * Y(IKLE2(IELEM))
     &                             + XA43(IELEM) * Y(IKLE3(IELEM))
40         CONTINUE
!
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-AY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 60 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) - XA12(IELEM) * Y(IKLE2(IELEM))
     &                             - XA13(IELEM) * Y(IKLE3(IELEM))
     &                             - XA14(IELEM) * Y(IKLE4(IELEM))
             W2(IELEM) = W2(IELEM) - XA21(IELEM) * Y(IKLE1(IELEM))
     &                             - XA23(IELEM) * Y(IKLE3(IELEM))
     &                             - XA24(IELEM) * Y(IKLE4(IELEM))
             W3(IELEM) = W3(IELEM) - XA31(IELEM) * Y(IKLE1(IELEM))
     &                             - XA32(IELEM) * Y(IKLE2(IELEM))
     &                             - XA34(IELEM) * Y(IKLE4(IELEM))
             W4(IELEM) = W4(IELEM) - XA41(IELEM) * Y(IKLE1(IELEM))
     &                             - XA42(IELEM) * Y(IKLE2(IELEM))
     &                             - XA43(IELEM) * Y(IKLE3(IELEM))
60         CONTINUE
!
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 80 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM)
     &               + C * (      XA12(IELEM) * Y(IKLE2(IELEM))
     &                          + XA13(IELEM) * Y(IKLE3(IELEM))
     &                          + XA14(IELEM) * Y(IKLE4(IELEM)) )
             W2(IELEM) = W2(IELEM)
     &               + C * (      XA21(IELEM) * Y(IKLE1(IELEM))
     &                          + XA23(IELEM) * Y(IKLE3(IELEM))
     &                          + XA24(IELEM) * Y(IKLE4(IELEM)) )
             W3(IELEM) = W3(IELEM)
     &               + C * (      XA31(IELEM) * Y(IKLE1(IELEM))
     &                          + XA32(IELEM) * Y(IKLE2(IELEM))
     &                          + XA34(IELEM) * Y(IKLE4(IELEM)) )
             W4(IELEM) = W4(IELEM)
     &               + C * (      XA41(IELEM) * Y(IKLE1(IELEM))
     &                          + XA42(IELEM) * Y(IKLE2(IELEM))
     &                          + XA43(IELEM) * Y(IKLE3(IELEM)) )
80         CONTINUE
!
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+CYZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+CY   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 100 IELEM = 1 , NELEM
             W1(IELEM) =   + XA21(IELEM) * Y(IKLE2(IELEM))
     &                     + XA31(IELEM) * Y(IKLE3(IELEM))
     &                     + XA41(IELEM) * Y(IKLE4(IELEM))
             W2(IELEM) =   + XA12(IELEM) * Y(IKLE1(IELEM))
     &                     + XA32(IELEM) * Y(IKLE3(IELEM))
     &                     + XA42(IELEM) * Y(IKLE4(IELEM))
             W3(IELEM) =   + XA13(IELEM) * Y(IKLE1(IELEM))
     &                     + XA23(IELEM) * Y(IKLE2(IELEM))
     &                     + XA43(IELEM) * Y(IKLE4(IELEM))
             W4(IELEM) =   + XA14(IELEM) * Y(IKLE1(IELEM))
     &                     + XA24(IELEM) * Y(IKLE2(IELEM))
     &                     + XA34(IELEM) * Y(IKLE3(IELEM))
100        CONTINUE
!
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
!
         ELSE
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-TAY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 101 IELEM = 1 , NELEM
             W1(IELEM) =   - XA21(IELEM) * Y(IKLE2(IELEM))
     &                     - XA31(IELEM) * Y(IKLE3(IELEM))
     &                     - XA41(IELEM) * Y(IKLE4(IELEM))
             W2(IELEM) =   - XA12(IELEM) * Y(IKLE1(IELEM))
     &                     - XA32(IELEM) * Y(IKLE3(IELEM))
     &                     - XA42(IELEM) * Y(IKLE4(IELEM))
             W3(IELEM) =   - XA13(IELEM) * Y(IKLE1(IELEM))
     &                     - XA23(IELEM) * Y(IKLE2(IELEM))
     &                     - XA43(IELEM) * Y(IKLE4(IELEM))
             W4(IELEM) =   - XA14(IELEM) * Y(IKLE1(IELEM))
     &                     - XA24(IELEM) * Y(IKLE2(IELEM))
     &                     - XA34(IELEM) * Y(IKLE3(IELEM))
101        CONTINUE
!
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
!
         ELSE
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+TAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 120 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) + XA21(IELEM) * Y(IKLE2(IELEM))
     &                             + XA31(IELEM) * Y(IKLE3(IELEM))
     &                             + XA41(IELEM) * Y(IKLE4(IELEM))
             W2(IELEM) = W2(IELEM) + XA12(IELEM) * Y(IKLE1(IELEM))
     &                             + XA32(IELEM) * Y(IKLE3(IELEM))
     &                             + XA42(IELEM) * Y(IKLE4(IELEM))
             W3(IELEM) = W3(IELEM) + XA13(IELEM) * Y(IKLE1(IELEM))
     &                             + XA23(IELEM) * Y(IKLE2(IELEM))
     &                             + XA43(IELEM) * Y(IKLE4(IELEM))
             W4(IELEM) = W4(IELEM) + XA14(IELEM) * Y(IKLE1(IELEM))
     &                             + XA24(IELEM) * Y(IKLE2(IELEM))
     &                             + XA34(IELEM) * Y(IKLE3(IELEM))
120        CONTINUE
!
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-TAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 140 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) - XA21(IELEM) * Y(IKLE2(IELEM))
     &                             - XA31(IELEM) * Y(IKLE3(IELEM))
     &                             - XA41(IELEM) * Y(IKLE4(IELEM))
             W2(IELEM) = W2(IELEM) - XA12(IELEM) * Y(IKLE1(IELEM))
     &                             - XA32(IELEM) * Y(IKLE3(IELEM))
     &                             - XA42(IELEM) * Y(IKLE4(IELEM))
             W3(IELEM) = W3(IELEM) - XA13(IELEM) * Y(IKLE1(IELEM))
     &                             - XA23(IELEM) * Y(IKLE2(IELEM))
     &                             - XA43(IELEM) * Y(IKLE4(IELEM))
             W4(IELEM) = W4(IELEM) - XA14(IELEM) * Y(IKLE1(IELEM))
     &                             - XA24(IELEM) * Y(IKLE2(IELEM))
     &                             - XA34(IELEM) * Y(IKLE3(IELEM))
140        CONTINUE
!
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CTAY') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
           DO 160 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM)
     &                 + C * (    + XA21(IELEM) * Y(IKLE2(IELEM))
     &                            + XA31(IELEM) * Y(IKLE3(IELEM))
     &                            + XA41(IELEM) * Y(IKLE4(IELEM)) )
             W2(IELEM) = W2(IELEM)
     &                 + C * (    + XA12(IELEM) * Y(IKLE1(IELEM))
     &                            + XA32(IELEM) * Y(IKLE3(IELEM))
     &                            + XA42(IELEM) * Y(IKLE4(IELEM)) )
             W3(IELEM) = W3(IELEM)
     &                 + C * (    + XA13(IELEM) * Y(IKLE1(IELEM))
     &                            + XA23(IELEM) * Y(IKLE2(IELEM))
     &                            + XA43(IELEM) * Y(IKLE4(IELEM)) )
             W4(IELEM) = W4(IELEM)
     &                 + C * (    + XA14(IELEM) * Y(IKLE1(IELEM))
     &                            + XA24(IELEM) * Y(IKLE2(IELEM))
     &                            + XA34(IELEM) * Y(IKLE3(IELEM)) )
160        CONTINUE
!
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
!
         ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,3000) OP
        IF (LNG.EQ.2) WRITE(LU,3001) OP
        CALL PLANTE(0)
        STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
1000  FORMAT(1X,'MV0404 (BIEF) : TERMES EXTRADIAG. TYPE INCONNU: ',A1)
1001  FORMAT(1X,'MV0404 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2000  FORMAT(1X,'MV0404 (BIEF) : DIAGONALE : TYPE INCONNU: ',A1)
2001  FORMAT(1X,'MV0404 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3000  FORMAT(1X,'MV0404 (BIEF) : OPERATION INCONNUE : ',A8)
3001  FORMAT(1X,'MV0404 (BIEF) : UNKNOWN OPERATION : ',A8)
!
!-----------------------------------------------------------------------
!
      END