!                    *****************
                     SUBROUTINE CALRE2
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES MEAN PARAMETERS OF THE WAVE SPECTRUM
!+               (RANDOM SEAS) :
!+                    K : MEAN WAVE NUMBER;
!+                    C : MEAN PHASE CELERITY;
!+                    CG : MEAN GROUP CELERITY.
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
      INTEGER I
!
      DOUBLE PRECISION PI,DEUXPI, DHTEST
!
      DOUBLE PRECISION BID
!
      INTRINSIC SQRT, ATAN2, DMOD, ABS, COS, SIN
!
!-----------------------------------------------------------------------
!
      PI = 3.1415926535897932384626433D0
      DEUXPI = 2.D0*PI
      GRAV = 9.81D0
!-----------------------------------------------------------------------
!
!   COMPUTES THE WAVE NUMBER: K
!   USING AN EXPLICIT FORMULATION (SEE EDF'S EXCELLENT REPORT BY
!   F. DHELLEMMES 'PRECIS SUR LES VAGUES' )
!
!-----------------------------------------------------------------------
!
! MEAN OMEGA STORED IN T1
!
      CALL OS( 'X=1/Y   ', T1 , T01  , SBID  , BID )
      CALL OS( 'X=CX    ', T1 , SBID  , SBID  , DEUXPI )
!
! OMEGA**2 * H / GRAV
!
      CALL OS( 'X=YZ    ', X=T2 , Y=T1 , Z=T1 )
      CALL OS( 'X=CXY   ', T2 , H  , SBID , 1.D0/GRAV )
!
!     INITIALISES DHTEST
!
      DHTEST = 1.D6
!
      DO 100 I=1,NPOIN
         T1%R(I) = 1.D0 + T2%R(I) *( 0.6522D0 +
     &                    T2%R(I) *( 0.4622D0 +
     &                    T2%R(I) *
     &                    T2%R(I) *( 0.0864D0 +
     &                    T2%R(I) *( 0.0675D0 ) )))
         T1%R(I) = SQRT( T2%R(I)*(T2%R(I) + 1.D0/T1%R(I)) )
         K%R(I)  = T1%R(I)/H%R(I)
         DHTEST  = MIN( DHTEST , H%R(I) )
100   CONTINUE
!
!
!=======================================================================
! COMPUTES MEAN C
!=======================================================================
!
      CALL OS( 'X=1/Y   ', T1 , T01  , SBID  , BID )
      CALL OS( 'X=CX    ', T1 , SBID  , SBID  , DEUXPI )
      CALL OS( 'X=Y/Z   ', C  , T1   , K    , BID )
!
!
!=======================================================================
! COMPUTES MEAN CG
!=======================================================================
!
      DO 200 I=1,NPOIN
         CG%R(I) = C%R(I)/2.D0 *
     &             (1.D0 + 2.D0*K%R(I)*H%R(I)/SINH(2.D0*K%R(I)*H%R(I)))
200   CONTINUE
!
      RETURN
      END
