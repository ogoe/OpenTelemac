!                    ***************
                     SUBROUTINE LUMP
!                    ***************
!
     &(DIAG,A,MESH,XMUL)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SUMS UP THE TERMS OF MATRIX A, BY LINE.
!+
!+            MULTIPLIES THE RESULT BY XMUL.
!+
!+            SIMPLY DOES DIAG = A X (VECTOR EQUAL TO XMUL).
!
!history  J-M HERVOUET (LNH)
!+        08/12/94
!+        V5P5
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
!| A              |-->| MATRICE
!| DIAG           |<--| VECTEUR RESULTAT.
!| MESH           |-->| BLOC DES TABLEAUX ENTIERS DU MAILLAGE.
!| XMUL           |-->| COEFFICIENT MULTIPLICATEUR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_LUMP => LUMP
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
      TYPE(BIEF_OBJ), INTENT(IN)     :: A
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: DIAG
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      DOUBLE PRECISION C
!
!-----------------------------------------------------------------------
!
      IF(A%ELMLIN.NE.A%ELMCOL) THEN
        IF (LNG.EQ.1) WRITE(LU,50)
        IF (LNG.EQ.2) WRITE(LU,51)
50      FORMAT(1X,'LUMP (BIEF) : A N''EST PAS UNE MATRICE CARREE')
51      FORMAT(1X,'LUMP (BIEF) : A IS NOT A SQUARE MATRIX')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL CPSTVC(A%D,DIAG)
!
!-----------------------------------------------------------------------
!
!  BUILDS A VECTOR THAT IS XMUL EVERYWHERE
!
      CALL OS( 'X=C     ', X=DIAG , C=XMUL )
!
!  DIAG IS THE PRODUCT OF A BY THIS VECTOR
!  DIAG HERE PLAYS THE ROLE OF X AND Y (CAN BE DONE)
!
      CALL MATVEC('X=AY    ',DIAG,A,DIAG,C,MESH,.TRUE.)
!
!-----------------------------------------------------------------------
!
      RETURN
      END