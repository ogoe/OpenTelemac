!                    *****************
                     SUBROUTINE INCIDE
!                    *****************
!
     &(COTOND,H,C0,PATMOS,ATMOS,ZF,MESH,LT,AT,GRAV,ROEAU,PRIVE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE INCIDENT WAVE IMPOSED AT THE BOUNDARY.
!code
!+     IN EACH NODE OF INCIDENT WAVE, IT IS NECESSARY TO GIVE:
!+
!+     ( 1 - NINC . NBOR ) A COS ( PHI - OMEGA T)
!+
!+     WHERE :
!+
!+     NINC  : DIRECTION OF THE INCIDENT WAVE
!+     NBOR  : NORMAL TO THE WALL (XSGBOR AND YSGBOR)
!+     A     : WAVE AMPLITUDE
!+     OMEGA : WAVE ANGULAR FREQUENCY
!+     PHI   : WAVE PHASE
!+     T     : TIME
!
!history  J-M HERVOUET (LNHE)
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
!| AT             |-->| TIME IN SECONDS
!| ATMOS          |-->| IF YES, ATMOSPHERIC PRESSURE IN PATMOS
!| C0             |-->| REFERENCE CELERITY
!| COTOND         |<--| ELEVATION OF RESULTING WAVE
!| GRAV           |-->| GRAVITY
!| H              |-->| WATER DEPTH
!| LT             |-->| TIME STEP
!| MESH           |-->| MESH STRUCTURE
!| PATMOS         |-->| ATMOSPHERIC PRESSURE
!| PRIVE          |-->| BLOCK OF PRIVATE ARRAYS
!| ROEAU          |-->| WATER DENSITY
!| ZF             |-->| ELEVATION OF BOTTOM
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
      INTEGER, INTENT(IN)            :: LT
      LOGICAL, INTENT(IN)            :: ATMOS
      DOUBLE PRECISION, INTENT(IN)   :: AT,GRAV,ROEAU
      TYPE(BIEF_OBJ), INTENT(IN)     :: PATMOS,H,C0,ZF,PRIVE
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: COTOND
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI
!
!-----------------------------------------------------------------------
!
      CALL OS( 'X=C     ' , X=COTOND , C=0.D0 )
!
!     PI = 3.141592653589D0
!     T=200.D0
!     W=2.*PI/T
!     A=0.25
!
!      DO 10 K=261,271
!       COTOND%R(K) = 2.*A*SIN(W*AT)
!10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
