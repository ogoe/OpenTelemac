!                    ****************
                     SUBROUTINE PUOG1
!                    ****************
!
     &(X, A,B ,DITR,MESH,COPY)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE VECTOR X = U B     (ELEMENT BY ELEMENT).
!code
!+            MATRIX L IS HERE THE RESULT OF A DECOMPOSITION
!+            PERFORMED IN SUBROUTINE DECLDU.
!+
!+            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM:
!+
!+            LE X DE X UE
!+
!+            LE : LOWER TRIANGULAR WITH 1 ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1 ON THE DIAGONAL
!+
!+                                                T
!+            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!
!code
!+-----------------------------------------------------------------------
!+  MEANING OF IELM :
!+
!+  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!+
!+  11 : P1 TRIANGLE            3                       YES
!+  12 : QUASI-BUBBLE TRIANGLE  4                       YES
!+  21 : Q1 QUADRILATERAL       4                       YES
!+  41 : TELEMAC-3D PRISMS      6                       YES
!+
!+-----------------------------------------------------------------------
!
!history  J-M HERVOUET (LNH)
!+        23/12/94
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
!| A              |<--| MATRICE A SOUS FORME LDU
!| B              |<--| SECOND MEMBRE DU SYSTEME A RESOUDRE.
!| COPY           |-->| SI .TRUE. B EST RECOPIE SUR X.
!|                |   | AU PREALABLE.
!| DITR           |-->| CARACTERE  'D' : ON CALCULE AVEC A
!|                |   | 'T' : ON CALCULE AVEC A TRANSPOSEE
!| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!| X              |<--| SOLUTION DU SYSTEME AX = B
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PUOG1 => PUOG1
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=1), INTENT(IN) :: DITR
!
      LOGICAL, INTENT(IN) :: COPY
!
!-----------------------------------------------------------------------
!
!  VECTORS STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X,B
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!-----------------------------------------------------------------------
!
!  MATRIX STRUCTURE
!
      TYPE(BIEF_OBJ), INTENT(IN) :: A
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM,NPOIN,NELEM,NELMAX
      CHARACTER(LEN=1) :: TYPX
!
!-----------------------------------------------------------------------
!
      TYPX  = A%TYPEXT
      NPOIN = A%D%DIM1
      IELM  = A%ELMLIN
      NELEM = MESH%NELEM
      NELMAX= MESH%NELMAX
      CALL CPSTVC(B,X)
!
!-----------------------------------------------------------------------
!
! 1) DESCENT WITH RECOPY OF B IN X
!
      CALL TNOMER(X%R,A%X%R,TYPX,
     &     B%R,MESH%IKLE%I,NELEM,NELMAX,NPOIN,IELM,DITR,COPY,MESH%LV)
!
!-----------------------------------------------------------------------
!
      RETURN
      END