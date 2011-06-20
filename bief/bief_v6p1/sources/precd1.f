!                    *****************
                     SUBROUTINE PRECD1
!                    *****************
!
     &(X,A,B,D,MESH,PRECON,PREXSM,DIADON)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DIAGONAL PRECONDITIONING OF A SYSTEM A X = B
!+               (SEE EXPLANATIONS IN PRECDT).
!+
!+            A IS A SIMPLE MATRIX HERE.
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
!| A              |-->| BLOCK OF MATRICES
!| B              |-->| BLOCK OF RIGHT-HAND SIZES
!| D              |<--| BLOCK OF DIAGONALS
!| DIADON         |-->| .TRUE. : DIAGONALS ARE GIVEN
!| MESH           |-->| MESH STRUCTURE
!| PRECON         |-->| CHOICE OF PRECONDITIONING
!| PREXSM         |-->| .TRUE. : PRECONDITIONING X AND B
!| X              |<->| BLOCK OF UNKNOWN VECTORS IN THE SYSTEM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PRECD1 => PRECD1
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X,B,D
!
!-----------------------------------------------------------------------
!
!  MATRIX STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A
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
!  COMPUTES THE SQUARE ROOTS OF THE ABSOLUTE VALUES OR OF THE VALUES
!
        IF(PRECON.EQ.5) THEN
          CALL OS( 'X=ABS(Y)' , X=D , Y=A%D )
        ELSE
          CALL OS( 'X=Y     ' , X=D , Y=A%D )
        ENDIF
!
!  PARALLEL MODE: COMPLETE DIAGONAL BEFORE TAKING THE SQUARE ROOT
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(D,2,MESH)
        ENDIF
!
        CALL OS( 'X=SQR(Y)' , X=D , Y=D )
!
!-----------------------------------------------------------------------
!                                         -1
!  CHANGE OF VARIABLES (D ACTUALLY HOLDS D  )
!
        IF(PREXSM) CALL OS( 'X=XY    ' , X , D , D , C )
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE INVERSE OF THE SQUARE ROOTS OF THE DIAGONALS
!  THIS GIVES BACK TRUE D AND NOT D INVERTED
!
        CALL OS( 'X=1/Y   ' , D , D , D , C , 2 , 1.D0 , 1.D-10 )
!
      ELSE
!
!  CASE WHERE D IS GIVEN, CHANGE OF VARIABLES
!  CHANGE OF VARIABLE (D REALLY HOLDS D)
!
        IF(PREXSM) THEN
          CALL OS( 'X=Y/Z   ' , X=X , Y=X , Z=D )
        ENDIF
!
      ENDIF
!
!=======================================================================
! PRECONDITIONING OF A:
!=======================================================================
!
      CALL OM( 'M=DMD   ' , A , A , D , C , MESH )
!     IF PRECON = 2 OR 3
      IF((2*(PRECON/2).EQ.PRECON.OR.3*(PRECON/3).EQ.PRECON).AND.
     &                                                 .NOT.DIADON) THEN
!       VALID ONLY WITH ONE SINGLE DOMAIN
        IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) A%TYPDIA='I'
      ENDIF
!
!=======================================================================
!
! PRECONDITIONING OF THE SECOND MEMBER
!
      IF(PREXSM) CALL OS( 'X=XY    ' , X=B , Y=D )
!
!=======================================================================
!
      RETURN
      END
