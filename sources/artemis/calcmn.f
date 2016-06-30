!                    *****************
                     SUBROUTINE CALCMN
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES APPROXIMATE VALUES FOR THE MOMENTUMS M0, M1,
!+                M2 OF THE WAVE SPECTRUM TO CALCULATE THE MEAN PERIOD
!+                AND DIRECTION.
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION FREQ, FREQ2, PONDER
!
      DOUBLE PRECISION BID
!
      INTRINSIC SQRT, ATAN2, MOD, ABS, COS, SIN
!
!-----------------------------------------------------------------------
!
! STRUCTURES
!
!-----------------------------------------------------------------------
!
      FREQ = 1.D0/PER
      FREQ2 = FREQ * FREQ
!      PONDER = 1.D0/DBLE(NDALE*NPALE)
      PONDER= 1D0
!
!=======================================================================
! M1 = INTEGRAL OF ( F * S(F) * DF )
!=======================================================================
!
      CALL OS( 'X=YZ    ', X=T1 , Y=HHO  , Z=HHO )
      CALL OS( 'X=CY    ', T2, T1  , SBID , FREQ )
      CALL OS( 'X=CX    ', T2, SBID , SBID , PONDER )
      CALL OS( 'X=X+Y   ', T01, T2 , SBID , 1.D0 )
!
!=======================================================================
! M2 = INTEGRAL OF ( F**2 * S(F) * DF )
!=======================================================================
!
      CALL OS( 'X=CY    ', T2, T1  , SBID , FREQ2 )
      CALL OS( 'X=CX    ', T2, SBID , SBID , PONDER )
      CALL OS( 'X=X+Y   ', T02, T2 , SBID , 1.D0 )
!
!=======================================================================
! MT1 = INTEGRAL OF ( T * S(F) * DF )
!=======================================================================
!
      CALL OS( 'X=CY    ', T2 , T1  , SBID , PER )
      CALL OS( 'X=CX    ', T2 , SBID , SBID , PONDER )
      CALL OS( 'X=X+Y   ', TM , T2  , SBID , BID )
!
!=======================================================================
! MCOS = INTEGRAL OF ( COS(INCI) * S(F) * DF )
!=======================================================================
!
      CALL OS( 'X=COS(Y)',  T2 , INCI , SBID , BID )
      CALL OS( 'X=CXY   ',  T2 , T1   , SBID , PONDER )
      CALL OS( 'X=X+Y   ', MCOS, T2   , SBID , BID )
!
!=======================================================================
! MSIN = INTEGRAL OF ( SIN(INCI) * S(F) * DF )
!=======================================================================
!
      CALL OS( 'X=SIN(Y)',  T2 , INCI , SBID , BID )
      CALL OS( 'X=CXY   ',  T2 , T1   , SBID , PONDER )
      CALL OS( 'X=X+Y   ', MSIN, T2   , SBID , BID )
!
      RETURN
      END
