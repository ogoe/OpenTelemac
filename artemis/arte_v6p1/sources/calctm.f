!                    *****************
                     SUBROUTINE CALCTM
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES VARIOUS ESTIMATES OF THE MEAN WAVE
!+                PERIOD :
!+                    T01 = M0/M1;
!+                    T02 = SQRT(M0/M2);
!+                    TM.
!+
!+           (DEFINITIONS IN THE LIST OF PARAMETERS ESTABLISHED
!+                BY THE IAHR)
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION PONDER,RADDEG
!
      DOUBLE PRECISION BID
!
      INTRINSIC SQRT, ATAN2, DMOD, ABS, COS, SIN
!
!-----------------------------------------------------------------------
!
! STRUCTURES
!
!
!-----------------------------------------------------------------------
!
      PONDER = 1.D0/DBLE(NDALE*NPALE)
      RADDEG = 180.D0/3.141592654D0
!
!=======================================================================
! COMPUTES M0 MOMENTUM AND STORES IT IN T2
!=======================================================================
!
      CALL OS( 'X=YZ    ', X=T1 , Y=HHO  , Z=HHO )
      CALL OS( 'X=CX    ', T1 , SBID  , SBID  , PONDER )
      CALL OS( 'X=Y+Z   ', T2 , HALE , T1   , BID )
!
!=======================================================================
! T01 = M0 / M1
!=======================================================================
!
      CALL OS( 'X=Y     ', T3 , T01 , SBID  , BID )
      CALL OS( 'X=Y/Z   ', T01, T2  , T3   , BID )
!
!=======================================================================
! T02 = SQRT( M0 / M2 )
!=======================================================================
!
      CALL OS( 'X=Y     ', T3 , T02 , SBID , BID )
      CALL OS( 'X=Y/Z   ', T1 , T2  , T3  , BID )
      CALL OS( 'X=SQR(Y)', T02, T1  , SBID , BID )
!
!=======================================================================
! TM =  MT1 / M0
!=======================================================================
!
      CALL OS( 'X=Y     ', T3 , TM , SBID , BID )
      CALL OS( 'X=Y/Z   ', TM , T3 , T2  , BID )
!
!
!=======================================================================
! MEAN DIRECTION: INCI
!=======================================================================
!
      CALL OS( 'X=A(Y,Z)',INCI , MSIN, MCOS , BID )
!
      RETURN
      END
