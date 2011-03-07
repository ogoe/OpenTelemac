!                    *****************
                     SUBROUTINE FILTER
!                    *****************
!
     &(VEC,BLDMAT,T1,T2,
     & A,FORMUL,
     & XMUL,F,G,H,U,V,W,
     & MESH,MSK,MASKEL,N)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    FILTERS A VECTOR USING A MATRIX.
!+
!+            FOR EXAMPLE, THE USE OF A MASS MATRIX YIELDS
!+                SMOOTHING.
!
!note     IF BLDMAT=.FALSE. MATRIX A IS GIVEN, IT IS NOT RE-BUILT.
!
!history  J-M HERVOUET (LNH)
!+        24/04/97
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
!| A              |<->| MATRICE (DONNEE OU CONSTRUITE SUIVANT BLDMAT)
!| BLDMAT         |-->| LOGIQUE : ON CONSTRUIT LA MATRICE OU PAS.
!| F,G,H,U,V,W    |-->| FONCTIONS INTERVENANT DANS LA MATRICE
!| FORMUL         |-->| FORMULE DECRIVANT LA MATRICE
!|                |   | (MEMES CONVENTIONS QUE DANS MATRIX)
!| MSK,MASKEL     |-->| LOGIQUE ET TABLEAU POUR LE MASQUAGE
!| N              |-->| NOMBRE DE FOIS OU ON FAIT L'OPERATION.
!| T1             |-->| TABLEAU DE TRAVAIL.
!| T2             |<->| TABLEAU DE TRAVAIL. MATRICE A MASS-LUMPEE
!|                |   | EN SORTIE (VOIR AUSSI XMUL)
!| VEC            |<->| VECTEUR A FILTRER
!| XMUL           |-->| FACTEUR MULTIPLICATIF NON NUL
!|                |   | N'A AUCUNE INFLUENCE SAUF SUR T2.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FILTER => FILTER
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: N
      DOUBLE PRECISION, INTENT(IN)  :: XMUL
      LOGICAL, INTENT(IN)           :: BLDMAT,MSK
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VEC,A,T1,T2
      TYPE(BIEF_OBJ), INTENT(IN)    :: F,G,H,U,V,W,MASKEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION C
!
!-----------------------------------------------------------------------
!
      DO 10 I=1,N
!
!  COMPUTES THE MATRIX ACCORDING TO THE GIVEN FORMULATION (OPTIONAL)
!
      IF(BLDMAT.AND.I.EQ.1) THEN
!
          CALL MATRIX(A,'M=N     ',FORMUL,VEC%ELM,VEC%ELM,
     &                XMUL,F,G,H,U,V,W,MESH,MSK,MASKEL)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE PRODUCT A * VEC (WITH ASSEMBLY)
!
      CALL MATVEC( 'X=AY    ',T1,A,VEC,C,MESH)
      IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
!
!-----------------------------------------------------------------------
!
!  COMPRESSES A ON ITS DIAGONAL
!
      IF(I.EQ.1) THEN
        CALL LUMP(T2,A,MESH,XMUL)
        IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
        CALL OS('X=1/Y   ',T2,T2,T2,C,IOPT=2,INFINI=0.D0,ZERO=1.D-20)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPUTES F = A * F / (ASSEMBLED A)
!
!  CHECKS DIVISIONS BY 0 CAUSED BY EXTERNAL POINTS IN
!  THE LEONARD FORMAT, WHICH CAN HAVE 0 VALUES
!
      CALL OS('X=YZ    ',X=VEC,Y=T1,Z=T2)
!
!-----------------------------------------------------------------------
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END