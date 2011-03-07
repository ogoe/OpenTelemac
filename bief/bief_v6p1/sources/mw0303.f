!                    *****************
                     SUBROUTINE MW0303
!                    *****************
!
     &(OP, X , DA,TYPDIA,XAS,TYPEXT, Y,C,
     & IKLEM1,DIMIKM,LIMVOI,MXPTVS,NPMAX,NPOIN,TRAV)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MATRIX VECTOR PRODUCT FOR P1 TRIANGLES.
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
!+      OP = 'X=-AY   '  : X = -AY
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
!| DIMIKM         |-->| PREMIERE DIMENSION DE IKLEM1
!| IKLEM1         |-->| 
!| LIMVOI         |-->| 
!| MXPTVS         |-->| 
!| NPMAX          |-->| NOMBRE MAXIMUM DE POINTS.
!| NPOIN          |-->| NOMBRE DE POINTS.
!| OP             |-->| OPERATION A EFFECTUER
!| TRAV           |-->| TABLEAU DE TRAVAIL.
!| TYPDIA         |-->| TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
!|                |   | TYPDIA = 'Q' : DIAGONALE QUELCONQUE
!|                |   | TYPDIA = 'I' : DIAGONALE IDENTITE.
!|                |   | TYPDIA = '0' : DIAGONALE NULLE.
!| TYPEXT         |-->| TYPE DES TERMES EXTRADIAGONAUX
!|                |   | TYPEXT = 'Q' : QUELCONQUES.
!|                |   | TYPEXT = 'S' : SYMETRIQUES.
!|                |   | TYPEXT = '0' : NULS.
!| X              |<--| VECTEUR IMAGE
!| XAS            |-->| TERMES EXTRA-DIAGONAUX DE LA MATRICE
!| Y              |-->| VECTEUR OPERANDE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MW0303 => MW0303
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: DIMIKM,MXPTVS,NPMAX,NPOIN
      INTEGER, INTENT(IN) :: IKLEM1(DIMIKM,4,2),LIMVOI(MXPTVS,2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(*),TRAV(*)
      DOUBLE PRECISION, INTENT(IN)    :: DA(*),Y(*)
      DOUBLE PRECISION, INTENT(IN)    :: XAS(*),C
!
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      CHARACTER(LEN=1), INTENT(IN)    :: TYPDIA,TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!
!   TREATMENT SPECIFIC TO THE TRANSPOSITION:
!
      I = 1
      IF(OP(3:3).EQ.'T'.OR.OP(4:4).EQ.'T'.OR.
     &   OP(5:5).EQ.'T'.OR.OP(6:6).EQ.'T') I = 3
!
!-----------------------------------------------------------------------
!
!   MATRIX VECTOR PRODUCT, SIMPLE FUNCTION OF THE SHAPE OF THE MATRIX:
!
      IF(TYPEXT(1:1).EQ.'S'.OR.TYPEXT(1:1).EQ.'Q') THEN
!
        IF(TYPEXT(1:1).EQ.'Q') THEN
        CALL OPASS('X=WY    ',TRAV,XAS,IKLEM1(1,I,1),
     &             Y,IKLEM1(1,I+1,1),LIMVOI,MXPTVS,NPMAX)
        ELSEIF(TYPEXT(1:1).EQ.'S') THEN
        CALL OPASS('X=WY    ',TRAV,XAS,IKLEM1(1,I,2),
     &             Y,IKLEM1(1,I+1,2),LIMVOI,MXPTVS,NPMAX)
        ENDIF
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+YZ  ', TRAV , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', TRAV , Y , Z , C , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
      ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', TRAV , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', TRAV , Y , Z , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', TRAV , Y , Z , 0.D0 , NPOIN )
         ELSE
            IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
            IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
            CALL PLANTE(0)
            STOP
         ENDIF
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
!-----------------------------------------------------------------------
!
!   IMPLEMENTED OPERATIONS:
!
      IF(OP(1:8).EQ.'X=AY    '.OR.OP(1:8).EQ.'X=TAY   ') THEN
         CALL OV ('X=Y     ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=-AY   '.OR.OP(1:8).EQ.'X=-TAY  ') THEN
         CALL OV ('X=-Y    ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X+AY  '.OR.OP(1:8).EQ.'X=X+TAY ') THEN
         CALL OV ('X=X+Y   ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X-AY  '.OR.OP(1:8).EQ.'X=X-TAY ') THEN
         CALL OV ('X=X-Y   ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X+CAY '.OR.OP(1:8).EQ.'X=X+CTAY') THEN
         CALL OV ('X=X+CY  ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=CAY   ') THEN
         CALL OV ('X=CY    ', X , TRAV , Z , C , NPOIN )
      ELSE
         IF (LNG.EQ.1) WRITE(LU,3000) OP
         IF (LNG.EQ.2) WRITE(LU,3001) OP
         CALL PLANTE(0)
         STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
1000  FORMAT(1X,'MW0303 (BIEF) : TERMES EXTRADIAG. TYPE INCONNU: ',A1)
1001  FORMAT(1X,'MW0303 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2000  FORMAT(1X,'MW0303 (BIEF) : DIAGONALE : TYPE INCONNU: ',A1)
2001  FORMAT(1X,'MW0303 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3000  FORMAT(1X,'MW0303 (BIEF) : OPERATION INCONNUE : ',A8)
3001  FORMAT(1X,'MW0303 (BIEF) : UNKNOWN OPERATION : ',A8)
!
      END