!                    *****************
                     SUBROUTINE GODOWN
!                    *****************
!
     &(X, A,B ,DITR,MESH,COPY)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE SYSTEM L X = B (ELEMENT BY ELEMENT).
!code
!+            THE MATRIX L IS HERE THE RESULT OF A DECOMPOSITION
!+            DONE IN SUBROUTINE DECLDU
!+
!+            EACH ELEMENTARY MATRIX WAS FACTORISED IN THE FORM:
!+
!+            LE X DE X UE
!+
!+            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!+            DE : DIAGONAL
!+            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
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
      USE BIEF, EX_GODOWN => GODOWN
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: B,A
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
      CHARACTER(LEN=1), INTENT(IN)  :: DITR
      LOGICAL, INTENT(IN) :: COPY
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER S,SA,I
!
!-----------------------------------------------------------------------
!
      IF(X%TYPE.EQ.4) THEN
        S = X%N
      ELSE
        S = 0
      ENDIF
!
!     CASE WHERE THE SYSTEM IS A BLOCK BUT WHERE ONLY ONE
!     PRECONDITIONING MATRIX IS USED
!
      IF(A%TYPE.EQ.3) THEN
        SA = 0
      ELSEIF(A%TYPE.EQ.4) THEN
        SA = A%N
      ELSE
        IF (LNG.EQ.1) WRITE(LU,300) A%TYPE
        IF (LNG.EQ.2) WRITE(LU,400) A%TYPE
300     FORMAT(1X,'GODOWN (BIEF) :',1I6,' TYPE DE A NON PREVU.')
400     FORMAT(1X,'GODOWN (BIEF) :',1I6,' UNEXPECTED TYPE FOR A.')
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(S.EQ.0.AND.SA.EQ.0) THEN
!
!     CASE WHERE A IS A SIMPLE MATRIX AND X A SIMPLE VECTOR
!
        CALL GODWN1(X, A,B ,DITR,MESH,COPY)
!
      ELSEIF(S.GT.0.AND.S.EQ.SA) THEN
!
!     CASE WHERE BLOCK A ONLY HAS DIAGONALS
!
        DO 10 I=1,S
          CALL GODWN1(X%ADR(I)%P,
     &                A%ADR(I)%P,
     &                B%ADR(I)%P,
     &                DITR,MESH,COPY)
10      CONTINUE
!
      ELSEIF(S.GT.0.AND.S**2.EQ.SA) THEN
!
!     CASE WHERE BLOCK A HAS AS MANY MATRICES AS THE WHOLE SYSTEM :
!     ONLY CONSIDERS THE DIAGONALS
!
        DO 11 I=1,S
          CALL GODWN1(X%ADR(I)%P,
     &                A%ADR(1+(S+1)*(I-1))%P,
     &                B%ADR(I)%P,
     &                DITR,MESH,COPY)
11      CONTINUE
!
!     CASE WHERE A IS A SINGLE MATRIX AND X A BLOCK
!
      ELSEIF(S.GT.0.AND.SA.EQ.0) THEN
!
        DO 12 I=1,S
          CALL GODWN1(X%ADR(I)%P,
     &                A,
     &                B%ADR(I)%P,
     &                DITR,MESH,COPY)
12      CONTINUE
!
      ELSE
        IF (LNG.EQ.1) WRITE(LU,301)
        IF (LNG.EQ.2) WRITE(LU,401)
301     FORMAT(1X,'GODOWN (BIEF) : CAS NON PREVU')
401     FORMAT(1X,'GODOWN (BIEF) : UNEXPECTED CASE')
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END