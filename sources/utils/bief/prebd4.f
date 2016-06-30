!                    *****************
                     SUBROUTINE PREBD4
!                    *****************
!
     &(X1,X2,A11,A12,A21,A22,B1,B2,D11,D12,D21,D22,
     & MESH,PREXSM,DIADON)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BLOCK-DIAGONAL PRECONDITIONING OF A SYSTEM A X = B.
!
!history  J.M. HERVOUET (LNH)
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
!| A11            |<->| TERM (1,1) OF MATRIX
!| A12            |<->| TERM (1,2) OF MATRIX
!| A21            |<->| TERM (2,1) OF MATRIX
!| A22            |<->| TERM (2,2) OF MATRIX
!| B1             |<->| FIRST RIGHT-HAND SIDE
!| B2             |<->| SECOND RIGHT-HAND SIDE
!| D11            |<--| DIAGONAL MATRIX
!| D12            |<--| DIAGONAL MATRIX
!| D21            |<--| DIAGONAL MATRIX
!| D22            |<--| DIAGONAL MATRIX
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X1,X2 AND B1,B2
!| X1             |<->| FIRST INITIAL GUESS
!| X2             |-->| SECOND INITIAL GUESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PREBD4 => PREBD4
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: PREXSM,DIADON
!
!-----------------------------------------------------------------------
!
!  VECTOR STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,B2,D11,D12,D21,D22
      TYPE(BIEF_OBJ), INTENT(IN)    :: X2,B1
!
!-----------------------------------------------------------------------
!
!  MATRIX STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A21,A22
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,NPOIN1,NPOIN2
      DOUBLE PRECISION C
!
!-----------------------------------------------------------------------
!
      NPOIN1 = X1%DIM1
      NPOIN2 = X2%DIM1
!
      IF(NPOIN2.NE.NPOIN1) THEN
        IF(LNG.EQ.1) WRITE(LU,100)
        IF(LNG.EQ.2) WRITE(LU,200)
100     FORMAT(1X,'PREBD4 (BIEF) : MATRICES RECTANGULAIRES',/,1X,
     &  'PRECONDITIONNEMENT BLOC-DIAGONAL IMPOSSIBLE DANS CE CAS')
200     FORMAT(1X,'PREBD4 (BIEF) : RECTANGULAR MATRICES',/,1X,
     &  'BLOCK-DIAGONAL PRECONDITIONING IMPOSSIBLE IN THIS CASE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  PREPARES THE DIAGONALS:
!
      IF(.NOT.DIADON) THEN
!
        CALL OS( 'X=Y     ' , D11 , A11%D , D11 , C )
        CALL OS( 'X=Y     ' , D12 , A12%D , D12 , C )
        CALL OS( 'X=Y     ' , D21 , A21%D , D21 , C )
        CALL OS( 'X=Y     ' , D22 , A22%D , D22 , C )
!
!  TEST TO REDUCE TO DIAGONAL PRECONDITIONING
!
!       CALL OS( 'X=C     ' , D12 , A12%D , Z , 0.D0 )
!       CALL OS( 'X=C     ' , D21 , A21%D , Z , 0.D0 )
!
!  END OF TEST TO REDUCE TO DIAGONAL PRECONDITIONING
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  L D U FACTORISATION OF THE DIAGONAL BLOCK:
!
!     ONLY D11 INVERTED IS NOW USED
      CALL OS( 'X=1/Y   ' , D11 , D11 , D11 , C)
!
      DO I = 1,NPOIN1
!
        D21%R(I) = D21%R(I) * D11%R(I)
        D22%R(I) = D22%R(I) - D21%R(I) * D12%R(I)
        D12%R(I) = D12%R(I) * D11%R(I)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
! CHANGE OF VARIABLES:
!
      IF(PREXSM) THEN
!
        CALL OS( 'X=X+YZ  ' , X1 , X2 , D12 , C )
!
      ENDIF
!
!  COMPUTES THE SQUARE ROOT
!  INVERTS D11,D22,D33
!  (THEY ARE ONLY USED IN THIS FORM FROM NOW ON)
!
!     INVERSION OF D11 ALREADY PERFORMED
      CALL OS( 'X=1/Y   ' , D22 , D22 , D22 , C )
      CALL OS( 'X=SQR(Y)' , D11 , D11 , D11 , C )
      CALL OS( 'X=SQR(Y)' , D22 , D22 , D22 , C )
!
!=======================================================================
! MULTIPLIES A ON THE LEFT BY L INVERTED
!=======================================================================
!
! A21 :
      CALL OM( 'M=M-DN  ' , A21 , A11 , D21 , C , MESH)
! A22 :
      CALL OM( 'M=M-DN  ' , A22 , A12 , D21 , C , MESH)
!
!=======================================================================
! MULTIPLIES A ON THE RIGHT BY U INVERTED
!=======================================================================
!
! A12 :
      CALL OM( 'M=M-ND  ' , A12 , A11 , D12 , C , MESH)
! A22 :
      CALL OM( 'M=M-ND  ' , A22 , A21 , D12 , C , MESH)
!
!-----------------------------------------------------------------------
!
! NEW SECOND MEMBER
!
      IF(PREXSM) THEN
!
      DO I = 1,NPOIN1
        B2%R(I) = B2%R(I) - D21%R(I) * B1%R(I)
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
