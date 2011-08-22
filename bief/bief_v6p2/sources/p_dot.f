!                    *******************************
                     DOUBLE PRECISION FUNCTION P_DOT
!                    *******************************
!
     &(NPOIN,X,Y,FAC)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SCALAR PRODUCT OF VECTORS X AND Y (SIZE NPOIN)
!+                TAKING PARALLELISM INTO ACCOUNT.
!
!history  REINHARD HINKELMANN (HANNOVER UNI.)
!+
!+
!+
!
!history  J-M HERVOUET (LNH)
!+        24/04/97
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
!| FAC            |-->| FAC=1/(NUMBER OF NEIGHBOURING SUB-DOMAINS)
!| NPOIN          |-->| SIZE OF X AND Y
!| X              |-->| VECTOR
!| Y              |-->| VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_P_DOT => P_DOT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN) :: NPOIN
!
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),FAC(NPOIN)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      P_DOT = 0.D0
!
      DO 10 I = 1 , NPOIN
       P_DOT = P_DOT + X(I) * Y(I) * FAC(I)
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
