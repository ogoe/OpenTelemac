!                    *****************
                     SUBROUTINE MATBOU
!                    *****************
!
     &(MESH,M1,M2,A11,A12,A21,A22,SMU,SMV,VR,VS,H0,MSK,MASKEL,S)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE MATRICES FOR THE RESOLUTION OF HELMHOLTZ
!+                EQUATIONS (STEPS 1 AND 3 OF THE ALGORITHM FOR BOUSSINESQ).
!+
!+
!+      A11 MULTIPLIES U IN THE EQUATION FOR U
!+
!+      A12 MULTIPLIES V IN THE EQUATION FOR U
!+
!+      A21 MULTIPLIES U IN THE EQUATION FOR V
!+
!+      A22 MULTIPLIES V IN THE EQUATION FOR V
!+
!+      SMU IS THE SECOND MEMBER IN THE EQUATION FOR U
!+
!+      SMV IS THE SECOND MEMBER IN THE EQUATION FOR V.
!
!history  J-M HERVOUET (LNHE)     ; C MOULIN (LNH)
!+        17/08/1994
!+        V5P2
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
!| A11            |---|
!| A12            |---|
!| A21            |---|
!| A22            |---|
!| H0             |---|
!| M1             |---|
!| M2             |---|
!| MASKEL         |---|
!| MESH           |---|
!| MSK            |---|
!| S              |---|
!| SMU,SMV        |<--| SECONDS MEMBRES DU SYSTEME.
!| VR             |---|
!| VS             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: MSK
!
!  STRUCTURES OF MATRICES
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: A11,A12,A21,A22,M1,M2
!
!  MESH STRUCTURE
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!  STRUCTURES OF VECTORS
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SMU,SMV
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,H0,S,VR,VS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELMU,IELMH
!
      DOUBLE PRECISION SL1,SL11,C
!
      CHARACTER*16 FORMUL
!
!-----------------------------------------------------------------------
!
      IELMH=H0%ELM
      IELMU=VR%ELM
!
!     MASS MATRIX (FOR COMPUTATION OF A11 AND OF SECOND MEMBER SMU)
!
      FORMUL='MATMAS          '
      SL1 = 1.D0
      CALL MATRIX(M1,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL1,S,S,S,S,S,S,MESH,MSK,MASKEL)
!
!------------------------------------------------------------------
!
!     SECOND MEMBER IN THE EQUATION FOR U
!
      CALL MATVEC( 'X=AY    ',SMU,M1,VR,C,MESH)
!
!------------------------------------------------------------------
!
!     MATRIX FOR U IN THE EQUATION FOR U (MASS MATRIX ALREADY
!     COMPUTED)
!
      FORMUL='FFBT        0XX0'
      SL11 = 1.D0 / 6.D0
      CALL MATRIX(A11,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        0YY0'
      SL11 = 2.D0 / 3.D0
      CALL MATRIX(A11,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        XX00'
      SL11 = 1.D0 / 2.D0
      CALL MATRIX(A11,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     FORMUL='FFBT        0X0X'
      FORMUL='FFBT        0XX0'
      SL11 = 1.D0 / 2.D0
      CALL MATRIX(A11,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT   00XX+00YY'
      SL11 = 1.D0 / 3.D0
      CALL MATRIX(A11,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     ADDS THE MASS MATRIX
!
      CALL OM( 'M=M+N   ' , A11 , M1 , S , C , MESH )
!
!------------------------------------------------------------------
!
!     MATRIX FOR V IN THE EQUATION FOR U
!
      FORMUL='FFBT        0Y0X'
      SL11 = 1.D0 / 2.D0
      CALL MATRIX(A12,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        XY00'
      SL11 = 1.D0 / 2.D0
      CALL MATRIX(A12,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     FORMUL='FFBT        0XY0'
      FORMUL='FFBT        0X0Y'
      SL11 = -1.D0 / 2.D0
      CALL MATRIX(A12,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!------------------------------------------------------------------
!
!     MASS MATRIX (FOR COMPUTATION OF A22 AND OF SECOND MEMBER SMV)
!
      FORMUL='MATMAS          '
      SL1 = 1.D0
      CALL MATRIX(M2,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL1,S,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     SECOND MEMBER SMV
!
      CALL MATVEC( 'X=AY    ',SMV,M2,VS,C,MESH)
!
!------------------------------------------------------------------
!
!     MATRIX FOR V IN THE EQUATION FOR V
!
      FORMUL='FFBT        0YY0'
      SL11 = 1.D0 / 6.D0
      CALL MATRIX(A22,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        0XX0'
      SL11 = 2.D0 / 3.D0
      CALL MATRIX(A22,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        YY00'
      SL11 = 1.D0 / 2.D0
      CALL MATRIX(A22,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     FORMUL='FFBT        0Y0Y'
      FORMUL='FFBT        0YY0'
      SL11 = 1.D0 / 2.D0
      CALL MATRIX(A22,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT   00XX+00YY'
      SL11 = 1.D0 / 3.D0
      CALL MATRIX(A22,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     ADDS THE MASS MATRIX
!
      CALL OM( 'M=M+N   ' , A22 , M2 , S , C , MESH )
!
!------------------------------------------------------------------
!
!     MATRIX FOR U IN THE EQUATION FOR V
!
      FORMUL='FFBT        0X0Y'
      SL11 = 1.D0 / 2.D0
      CALL MATRIX(A21,'M=N     ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
      FORMUL='FFBT        XY00'
      SL11 = 1.D0 / 2.D0
      CALL MATRIX(A21,'M=M+N   ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!     FORMUL='FFBT        0YX0'
      FORMUL='FFBT        0Y0X'
      SL11 = -1.D0 / 2.D0
      CALL MATRIX(A21,'M=M+TN  ',FORMUL,IELMU,IELMU,
     &            SL11,H0,S,S,S,S,S,MESH,MSK,MASKEL)
!
!------------------------------------------------------------------
!
      RETURN
      END