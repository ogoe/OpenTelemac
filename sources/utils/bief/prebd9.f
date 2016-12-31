!                    *****************
                     SUBROUTINE PREBD9
!                    *****************
!
     &(X1,X2,X3,A11,A12,A13,A21,A22,A23,A31,A32,A33,
     & B1,B2,B3,D11,D12,D13,D21,D22,D23,D31,D32,D33,
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
!| ...            |<->| ...
!| A33            |<->| TERM (3,3) OF MATRIX
!| B1             |<->| FIRST RIGHT-HAND SIDE
!| B2             |<->| SECOND RIGHT-HAND SIDE
!| B3             |<->| THIRD RIGHT-HAND SIDE
!| D11            |<--| DIAGONAL MATRIX
!| ...            |<--| ...
!| D33            |<--| DIAGONAL MATRIX
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X1,X2 AND B1,B2
!| X1             |<->| FIRST INITIAL GUESS
!| X2             |-->| SECOND INITIAL GUESS
!| X3             |-->| THIRD INITIAL GUESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PREBD9 => PREBD9
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
      TYPE(BIEF_OBJ), INTENT(IN)    :: X3,B1
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2,B2,B3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: D11,D12,D13,D21,D22,D23
      TYPE(BIEF_OBJ), INTENT(INOUT) :: D31,D32,D33
!
!-----------------------------------------------------------------------
!
!  MATRIX STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A13,A21,A22
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A23,A31,A32,A33
!
!-----------------------------------------------------------------------
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,NPOIN1,NPOIN2,NPOIN3
      DOUBLE PRECISION C
!
!-----------------------------------------------------------------------
!
      NPOIN1 = X1%DIM1
      NPOIN2 = X2%DIM1
      NPOIN3 = X3%DIM1
!
      IF(NPOIN2.NE.NPOIN1.AND.NPOIN3.NE.NPOIN1) THEN
        IF(LNG.EQ.1) WRITE(LU,100)
        IF(LNG.EQ.2) WRITE(LU,200)
100     FORMAT(1X,'PREBD9 (BIEF) : MATRICES RECTANGULAIRES',/,1X,
     &  'PRECONDITIONNEMENT BLOC-DIAGONAL IMPOSSIBLE DANS CE CAS')
200     FORMAT(1X,'PREBD9 (BIEF) : RECTANGULAR MATRICES',/,1X,
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
        CALL OS( 'X=Y     ' , D11 , A11%D ,D11,C)
        CALL OS( 'X=Y     ' , D12 , A12%D ,D12,C)
        CALL OS( 'X=Y     ' , D13 , A13%D ,D13,C)
        CALL OS( 'X=Y     ' , D21 , A21%D ,D21,C)
        CALL OS( 'X=Y     ' , D22 , A22%D ,D22,C)
        CALL OS( 'X=Y     ' , D23 , A23%D ,D23,C)
        CALL OS( 'X=Y     ' , D31 , A31%D ,D31,C)
        CALL OS( 'X=Y     ' , D32 , A32%D ,D32,C)
        CALL OS( 'X=Y     ' , D33 , A33%D ,D33,C)
!
!  TEST TO REDUCE TO DIAGONAL PRECONDITIONING
!
!       CALL OS( 'X=Y     ' , D11 , A11%D ,Z,C   )
!       CALL OS( 'X=C     ' , D12 , A12%D ,Z,0.D0)
!       CALL OS( 'X=C     ' , D13 , A13%D ,Z,0.D0)
!       CALL OS( 'X=C     ' , D21 , A21%D ,Z,0.D0)
!       CALL OS( 'X=Y     ' , D22 , A22%D ,Z,C   )
!       CALL OS( 'X=C     ' , D23 , A23%D ,Z,0.D0)
!       CALL OS( 'X=C     ' , D31 , A31%D ,Z,0.D0)
!       CALL OS( 'X=C     ' , D32 , A32%D ,Z,0.D0)
!       CALL OS( 'X=Y     ' , D33 , A33%D ,Z,C   )
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
      CALL OS( 'X=1/Y   ' , D11 , D11 , D11 , C )
!
      DO I = 1,NPOIN1
!
        D21%R(I) =  D21%R(I) * D11%R(I)
        D31%R(I) =  D31%R(I) * D11%R(I)
        D22%R(I) =  D22%R(I) - D21%R(I) * D12%R(I)
!
      ENDDO
!
!     ONLY D22 INVERTED IS NOW USED
      CALL OS( 'X=1/Y   ' , D22 , D22 , D22 , C )
!
      DO I = 1,NPOIN1
!
        D32%R(I) = (D32%R(I) - D31%R(I) * D12%R(I)) * D22%R(I)
        D23%R(I) =  D23%R(I) - D21%R(I) * D13%R(I)
        D33%R(I) =  D33%R(I)
     &             -D31%R(I)*D13%R(I)-D32%R(I)*D23%R(I)
        D12%R(I) =  D12%R(I) * D11%R(I)
        D13%R(I) =  D13%R(I) * D11%R(I)
        D23%R(I) =  D23%R(I) * D22%R(I)
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
        CALL OS( 'X=X+YZ  ' , X1 , X3 , D13 , C )
        CALL OS( 'X=X+YZ  ' , X2 , X3 , D23 , C )
!
      ENDIF
!
!  COMPUTES THE SQUARE ROOT
!  INVERTS D11,D22,D33
!  (THEY ARE ONLY USED IN THIS FORM FROM NOW ON)
!
!     INVERSION OF D11 ALREADY PERFORMED
!     INVERSION OF D22 ALREADY PERFORMED
      CALL OS( 'X=1/Y   ' , D33 , D33 , D33 , C )
      CALL OS( 'X=SQR(Y)' , D11 , D11 , D11 , C )
      CALL OS( 'X=SQR(Y)' , D22 , D22 , D22 , C )
      CALL OS( 'X=SQR(Y)' , D33 , D33 , D33 , C )
!
!=======================================================================
! MULTIPLIES A ON THE LEFT BY L INVERTED
!=======================================================================
!
!  A21 :
      CALL OM( 'M=M-DN  ' , A21 , A11 , D21 , C , MESH)
!  A22 :
      CALL OM( 'M=M-DN  ' , A22 , A12 , D21 , C , MESH)
!  A23 :
      CALL OM( 'M=M-DN  ' , A23 , A13 , D21 , C , MESH)
!  A31 :
      CALL OM( 'M=M-DN  ' , A31 , A11 , D31 , C , MESH)
      CALL OM( 'M=M-DN  ' , A31 , A21 , D32 , C , MESH)
!  A32 :
      CALL OM( 'M=M-DN  ' , A32 , A12 , D31 , C , MESH)
      CALL OM( 'M=M-DN  ' , A32 , A22 , D32 , C , MESH)
!  A33 :
      CALL OM( 'M=M-DN  ' , A33 , A13 , D31 , C , MESH)
      CALL OM( 'M=M-DN  ' , A33 , A23 , D32 , C , MESH)
!
!=======================================================================
! MULTIPLIES A ON THE RIGHT BY U INVERTED
!=======================================================================
!
!  A12 :
      CALL OM( 'M=M-ND  ' , A12 , A11 , D12 , C , MESH)
!  A22 :
      CALL OM( 'M=M-ND  ' , A22 , A21 , D12 , C , MESH)
!  A32 :
      CALL OM( 'M=M-ND  ' , A32 , A31 , D12 , C , MESH)
!  A13 :
      CALL OM( 'M=M-ND  ' , A13 , A11 , D13 , C , MESH)
      CALL OM( 'M=M-ND  ' , A13 , A12 , D23 , C , MESH)
!  A23 :
      CALL OM( 'M=M-ND  ' , A23 , A21 , D13 , C , MESH)
      CALL OM( 'M=M-ND  ' , A23 , A22 , D23 , C , MESH)
!  A33 :
      CALL OM( 'M=M-ND  ' , A33 , A31 , D13 , C , MESH)
      CALL OM( 'M=M-ND  ' , A33 , A32 , D23 , C , MESH)
!
!-----------------------------------------------------------------------
!
! NEW SECOND MEMBER
!
      IF(PREXSM) THEN
!
      DO I = 1,NPOIN1
        B2%R(I) = B2%R(I)-D21%R(I)*B1%R(I)
        B3%R(I) = B3%R(I)-D31%R(I)*B1%R(I)-D32%R(I)*B2%R(I)
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
