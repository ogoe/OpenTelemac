!                    *****************
                     SUBROUTINE PRECD4
!                    *****************
!
     &(X1,X2,A11,A12,A21,A22,
     & B1,B2,D1,D2,MESH,PRECON,PREXSM,DIADON)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!+               (SEE EXPLANATIONS IN PRECDT).
!+
!+            A IS A 4-MATRIX BLOCK HERE.
!
!history  J-M HERVOUET (LNHE)
!+        06/07/2009
!+        V6P0
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
!| D1             |<--| DIAGONAL MATRIX
!| D2             |<--| DIAGONAL MATRIX
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PRECON         |-->| CHOICE OF PRECONDITIONING
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X1,X2 AND B1,B2
!| X1             |<->| FIRST INITIAL GUESS
!| X2             |-->| SECOND INITIAL GUESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PRECD4 => PRECD4
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: PRECON
!
      LOGICAL, INTENT(IN) :: PREXSM,DIADON
!
!-----------------------------------------------------------------------
!
!  VECTOR STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2,B1,B2,D1,D2
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
      DOUBLE PRECISION C
!
!-----------------------------------------------------------------------
!
!  PREPARES THE DIAGONALS:
!
      IF(.NOT.DIADON) THEN
!
!  COMPUTES THE SQUARE ROOTS OF THE ABSOLUTE VALUES
!
        IF(PRECON.EQ.5) THEN
          CALL OS( 'X=ABS(Y)' , D1 , A11%D , D1 , C )
          CALL OS( 'X=ABS(Y)' , D2 , A22%D , D2 , C )
        ELSE
          CALL OS( 'X=Y     ' , D1 , A11%D , D1 , C )
          CALL OS( 'X=Y     ' , D2 , A22%D , D2 , C )
        ENDIF
!
!  PARALLEL MODE: COMPLETE DIAGONAL BEFORE TAKING THE SQUARE ROOT
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(D1,2,MESH)
          CALL PARCOM(D2,2,MESH)
        ENDIF
!
        CALL OS( 'X=SQR(Y)' , D1 , D1 , D1 , C )
        CALL OS( 'X=SQR(Y)' , D2 , D2 , D2 , C )
!
!-----------------------------------------------------------------------
!                                                    -1
!  CHANGE OF VARIABLES (D1,D2 AND D3 ACTUALLY HOLD D1 ,...)
!
        IF(PREXSM) THEN
          CALL OS( 'X=XY    ' , X1 , D1 , D1 , C )
          CALL OS( 'X=XY    ' , X2 , D2 , D2 , C )
        ENDIF
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE INVERSE OF THE SQUARE ROOTS OF THE DIAGONALS
!  THIS GIVES BACK TRUE D1 AND D2 AND NOT D1 AND D2 INVERTED
!
        CALL OS( 'X=1/Y   ' , D1 , D1 , D1 , C , 2 , 1.D0 , 1.D-10)
        CALL OS( 'X=1/Y   ' , D2 , D2 , D2 , C , 2 , 1.D0 , 1.D-10)
!
      ELSE
!
!  CASE WHERE D IS GIVEN, CHANGE OF VARIABLES
!  CHANGE OF VARIABLE (D1,D2 REALLY HOLD D1,D2)
!
        IF(PREXSM) THEN
          CALL OS( 'X=Y/Z   ' , X1 , X1 , D1 , C )
          CALL OS( 'X=Y/Z   ' , X2 , X2 , D2 , C )
        ENDIF
!
      ENDIF
!
!=======================================================================
! PRECONDITIONING OF A11 :
!=======================================================================
!
      CALL OM( 'M=DMD   ' , A11,A11 ,D1,C,MESH)
!
!=======================================================================
! PRECONDITIONING OF A12 :
!=======================================================================
!
      CALL OM( 'M=DM    ' , A12,A12 ,D1,C,MESH)
      CALL OM( 'M=MD    ' , A12,A12 ,D2,C,MESH)
!
!=======================================================================
! PRECONDITIONING OF A21 :
!=======================================================================
!
      CALL OM( 'M=DM    ' , A21,A21 ,D2,C,MESH)
      CALL OM( 'M=MD    ' , A21,A21 ,D1,C,MESH)
!
!=======================================================================
! PRECONDITIONING OF A22 :
!=======================================================================
!
      CALL OM( 'M=DMD   ' , A22,A22 ,D2,C,MESH)
!
!=======================================================================
!
!     CASES WHERE THE DIAGONALS ARE KNOWN
!     (VALID ONLY WITH ONE SINGLE DOMAIN)
!
      IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) THEN
!
!       IF PRECON = 2 OR 3
        IF(2*(PRECON/2).EQ.PRECON.AND..NOT.DIADON) THEN
          A11%TYPDIA='I'
          A22%TYPDIA='I'
        ELSEIF(3*(PRECON/3).EQ.PRECON.AND..NOT.DIADON) THEN
          A11%TYPDIA='I'
          A22%TYPDIA='I'
          A12%TYPDIA='0'
          A21%TYPDIA='0'
        ENDIF
!
      ENDIF
!
!=======================================================================
!
! PRECONDITIONING OF THE SECOND MEMBER
!
      IF(PREXSM) THEN
        CALL OS( 'X=XY    ' , B1 , D1 , D1 , C )
        CALL OS( 'X=XY    ' , B2 , D2 , D2 , C )
      ENDIF
!
!=======================================================================
!
      RETURN
      END
