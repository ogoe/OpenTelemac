!                    ****************
                     SUBROUTINE DES41
!                    ****************
!
     &(X, XA1 ,XA2 ,XA3 ,XA4 ,XA5 ,
     &    XA6 ,XA7 ,XA8 ,XA9 ,XA10,
     &    XA11,XA12,XA13,XA14,XA15,
     &    IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     &    NELEM,NELMAX,NPOIN,LV)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE SYSTEM L X = B (ELEMENT: P1 PRISM).
!code
!+            B IS THE SAME AS X TO START WITH
!+
!+            L IS THE LOWER PART OF THE MATRIX, BUILT IN
!+            SUBROUTINE DECLDU.
!+
!+            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:
!+
!+            LE * DE * UE
!+
!+            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!+
!+                                                T
!+            IF THE MATRIX IS SYMMETRICAL : LE =  UE
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
!| IKLE2          |---| 
!| IKLE3          |---| 
!| IKLE4          |---| 
!| IKLE5          |---| 
!| IKLE6          |---| 
!| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| NPOIN          |-->| DIMENSION DES TABLEAUX
!| X              |---| 
!| XA10           |---| 
!| XA11           |---| 
!| XA12           |---| 
!| XA13           |---| 
!| XA14           |---| 
!| XA15           |---| 
!| XA2            |---| 
!| XA3            |---| 
!| XA4            |---| 
!| XA5            |---| 
!| XA6            |---| 
!| XA7            |---| 
!| XA8            |---| 
!| XA9            |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: NPOIN,NELEM,NELMAX,LV
      DOUBLE PRECISION , INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION , INTENT(IN)    :: XA1(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA2(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA3(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA4(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA5(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA6(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA7(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA8(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA9(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA10(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA11(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA12(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA13(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA14(NELMAX)
      DOUBLE PRECISION , INTENT(IN)    :: XA15(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE1(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE2(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE3(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE4(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE5(NELMAX)
      INTEGER          , INTENT(IN)    :: IKLE6(NELMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IB
!
      INTRINSIC MIN
!
!-----------------------------------------------------------------------
!
! RESUMES INVERSIONS OF THE LOWER TRIANGULAR MATRICES
!
!-----------------------------------------------------------------------
! LOOP IN SCALAR MODE (LV=1) OR WITH FORCED VECTORISATION
!-----------------------------------------------------------------------
!
      IF(LV.EQ.1) THEN
!
!  SCALAR MODE
!
      DO 10 IELEM = 1 , NELEM
        X(IKLE2(IELEM))=X(IKLE2(IELEM))-XA1(IELEM)*X(IKLE1(IELEM))
        X(IKLE3(IELEM))=X(IKLE3(IELEM))-XA2(IELEM)*X(IKLE1(IELEM))
     &                                 -XA6(IELEM)*X(IKLE2(IELEM))
        X(IKLE4(IELEM))=X(IKLE4(IELEM))-XA3(IELEM)*X(IKLE1(IELEM))
     &                                 -XA7(IELEM)*X(IKLE2(IELEM))
     &                                -XA10(IELEM)*X(IKLE3(IELEM))
        X(IKLE5(IELEM))=X(IKLE5(IELEM))-XA4(IELEM)*X(IKLE1(IELEM))
     &                                 -XA8(IELEM)*X(IKLE2(IELEM))
     &                                -XA11(IELEM)*X(IKLE3(IELEM))
     &                                -XA13(IELEM)*X(IKLE4(IELEM))
        X(IKLE6(IELEM))=X(IKLE6(IELEM))-XA5(IELEM)*X(IKLE1(IELEM))
     &                                 -XA9(IELEM)*X(IKLE2(IELEM))
     &                                -XA12(IELEM)*X(IKLE3(IELEM))
     &                                -XA14(IELEM)*X(IKLE4(IELEM))
     &                                -XA15(IELEM)*X(IKLE5(IELEM))
10    CONTINUE
!
      ELSE
!
!  VECTOR MODE
!
      DO 20 IB = 1,(NELEM+LV-1)/LV
!VOCL LOOP,NOVREC
!DIR$ IVDEP
      DO 30 IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        X(IKLE2(IELEM))=X(IKLE2(IELEM))-XA1(IELEM)*X(IKLE1(IELEM))
        X(IKLE3(IELEM))=X(IKLE3(IELEM))-XA2(IELEM)*X(IKLE1(IELEM))
     &                                 -XA6(IELEM)*X(IKLE2(IELEM))
        X(IKLE4(IELEM))=X(IKLE4(IELEM))-XA3(IELEM)*X(IKLE1(IELEM))
     &                                 -XA7(IELEM)*X(IKLE2(IELEM))
     &                                -XA10(IELEM)*X(IKLE3(IELEM))
        X(IKLE5(IELEM))=X(IKLE5(IELEM))-XA4(IELEM)*X(IKLE1(IELEM))
     &                                 -XA8(IELEM)*X(IKLE2(IELEM))
     &                                -XA11(IELEM)*X(IKLE3(IELEM))
     &                                -XA13(IELEM)*X(IKLE4(IELEM))
        X(IKLE6(IELEM))=X(IKLE6(IELEM))-XA5(IELEM)*X(IKLE1(IELEM))
     &                                 -XA9(IELEM)*X(IKLE2(IELEM))
     &                                -XA12(IELEM)*X(IKLE3(IELEM))
     &                                -XA14(IELEM)*X(IKLE4(IELEM))
     &                                -XA15(IELEM)*X(IKLE5(IELEM))
30    CONTINUE
20    CONTINUE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END