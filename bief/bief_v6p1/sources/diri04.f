!                    *****************
                     SUBROUTINE DIRI04
!                    *****************
!
     &(X1,X2,
     & A11,A12,A21,A22,
     & SM1,SM2,T1,T2,T3,T4,
     & XBOR1,XBOR2,LIDIR1,LIDIR2,
     & MESH,KDIR,MSK,MASKPT)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    TREATS THE DIRICHLET POINTS FOR THE FOLLOWING
!+                SYSTEM (BLOCK OF 4 MATRICES):
!code
!+         (     A11          A12              )  ( X1 )   ( SM1 )
!+         (                                   )  (    ) = (     )
!+         (     A21          A22              )  ( X2 )   ( SM2 )
!
!history  J-M HERVOUET (LNH)
!+        30/01/95
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
!| A12            |---|
!| A21            |---|
!| A22            |---|
!| KDIR           |-->| CONDITION A LA LIMITE DE TYPE DIRICHLET
!| LIDIR2         |---|
!| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS
!|                |   | =1. : NORMAL   =0. : POINT MASQUE.
!| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| SM1,SM2        |-->| SECONDS MEMBRES DU SYSTEME.
!| T1,T2,T3,T4    |-->| TABLEAUX DE TRAVAIL DU SYSTEME
!| XBOR2          |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DIRI04 => DIRI04
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X1,X2,SM1,SM2,T1,T2,T3,T4
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A21,A22
      TYPE(BIEF_OBJ), INTENT(IN)    :: XBOR1,XBOR2,MASKPT
      INTEGER, INTENT(IN)           :: KDIR,LIDIR1(*),LIDIR2(*)
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      LOGICAL, INTENT(IN)           :: MSK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C,Z(1)
!
      CHARACTER*1 STODIA
!
!-----------------------------------------------------------------------
!
! 1) BUILDS ARRAYS T1,T2 CONTAINING:
!    THE X1 AND X2 IMPOSED VALUES IF THE POINT IS OF TYPE DIRICHLET
!    0 OTHERWISE
!
!    X1,X2  3 TAKE THEIR DIRICHLET VALUE
!
!=======================================================================
!
!   BOUNDARY CONDITION FOR X1 : "XBOR1" IMPOSED
!
      CALL CPSTVC(X1,T1)
      CALL OS ( 'X=C     ' , T1 , T1 , T1 , 0.D0 )
      CALL OSDBIF ( 'X=Y     ',T1,XBOR1,LIDIR1,KDIR,MESH)
!
!-----------------------------------------------------------------------
!
!   BOUNDARY CONDITIONS FOR X2 : "XBOR2" IMPOSED
!
      CALL CPSTVC(X2,T2)
      CALL OS  ( 'X=C     ' , T2 , T2 , T2 , 0.D0 )
      CALL OSDBIF ( 'X=Y     ',T2,XBOR2,LIDIR2,KDIR,MESH)
!
!=======================================================================
!
!   2) COMPUTES THE PRODUCT OF THE MATRIX FOR THE SYSTEM TO SOLVE
!      AND T1,T2
!      THE RESULT IS DEDUCTED FROM THE SECOND MEMBERS
!
      CALL MATVEC('X=AY    ',T3,A11,T1,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T3,A12,T2,C,MESH,LEGO=.TRUE. )
      CALL MATVEC('X=AY    ',T4,A21,T1,C,MESH,LEGO=.FALSE.)
      CALL MATVEC('X=X+AY  ',T4,A22,T2,C,MESH,LEGO=.TRUE. )
!
      CALL CPSTVC(X1,SM1)
      CALL CPSTVC(X2,SM2)
      CALL OS( 'X=X-Y   ' , SM1 , T3 , T3 , C )
      CALL OS( 'X=X-Y   ' , SM2 , T4 , T4 , C )
!
!=======================================================================
!
!  SECOND MEMBERS OF THE EQUATIONS FOR DIRICHLET POINTS
!  PREPARES THE LINEAR SYSTEM
!
      CALL DIRAUX(SM1,A11%D,XBOR1,T1,X1,LIDIR1,KDIR,MESH )
      CALL DIRAUX(SM2,A22%D,XBOR2,T2,X2,LIDIR2,KDIR,MESH )
!
      IF(MSK) THEN
        CALL OV( 'X=XY    ',SM1%R,MASKPT%R,Z,C,SM1%DIM1)
        CALL OV( 'X=XY    ', X1%R,MASKPT%R,Z,C,X1%DIM1)
        CALL OV( 'X=XY    ', T1%R,MASKPT%R,Z,C,T1%DIM1)
        CALL OV( 'X=XY    ',SM2%R,MASKPT%R,Z,C,SM2%DIM1)
        CALL OV( 'X=XY    ', X2%R,MASKPT%R,Z,C,X2%DIM1)
        CALL OV( 'X=XY    ', T2%R,MASKPT%R,Z,C,T2%DIM1)
      ENDIF
!
!=======================================================================
!
!   ERASES THE LINES AND COLUMNS FOR DIRICHLET POINTS
!
!   IT'S EQUIVALENT TO A DIAGONAL PRECONDITIONING WITH ARRAYS
!   T1,T2,T3
!
!   DOES NOT ALTER A11,A22,A33 DIAGONALS
!   BY GIVING THEM A DUMMY TYPE : '0'
!
!
!=======================================================================
! A11 PRECONDITIONING :
!=======================================================================
!
      STODIA = A11%TYPDIA
      A11%TYPDIA='0'
      CALL OM( 'M=DMD   ' , A11,A11 ,T1,C,MESH)
      A11%TYPDIA=STODIA
!
!=======================================================================
! A12 PRECONDITIONING :
!=======================================================================
!
      CALL OM( 'M=DM    ' , A12,A12 ,T1,C,MESH)
      CALL OM( 'M=MD    ' , A12,A12 ,T2,C,MESH)
!
!=======================================================================
! A21 PRECONDITIONING :
!=======================================================================
!
      CALL OM( 'M=DM    ' , A21,A21 ,T2,C,MESH)
      CALL OM( 'M=MD    ' , A21,A21 ,T1,C,MESH)
!
!=======================================================================
! A22 PRECONDITIONING :
!=======================================================================
!
      STODIA = A22%TYPDIA
      A22%TYPDIA='0'
      CALL OM( 'M=DMD   ' , A22,A22 ,T2,C,MESH)
      A22%TYPDIA=STODIA
!
!-----------------------------------------------------------------------
!
      RETURN
      END