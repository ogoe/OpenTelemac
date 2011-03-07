!                    *****************
                     SUBROUTINE WETDRY
!                    *****************
!
     &(ETA1,Z1,H1,U1,V1,ETA2,Z2,H2,U2,V2,EPS)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EPS            |-->| TOLERANCE FOR WATER DEPTH
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
      DOUBLE PRECISION, INTENT(IN)    :: EPS
      DOUBLE PRECISION, INTENT(INOUT)   :: ETA1,Z1,H1,U1,V1
      DOUBLE PRECISION, INTENT(INOUT)   :: ETA2,Z2,H2,U2,V2
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION ENG
      DOUBLE PRECISION, PARAMETER :: DEUG=19.62D0
!
!***********************************************************************
      ENG = 0.0D0
! FIRST CASE:
      IF(ETA1.LT.Z2)THEN
        IF((H1.GT.EPS).AND.(H2.LT.EPS))THEN
           IF(U1.NE.0.0D0)THEN
!             COMPUTE THE ENERGY:U^2/2g + H + Z
              ENG = (U1*U1/DEUG) + ETA1
!             ENG MUST BE > Z2
              IF(ENG.GT.Z2)THEN
                Z2   = ETA1
                ETA2 = ETA1
                H2 = 0.0D0
                U2 = 0.0D0
                V2 = 0.0D0
              ENDIF
           ENDIF
        ENDIF
      ENDIF
! SECOND CASE :
      IF(ETA2.LT.Z1)THEN
        IF((H1.LT.EPS).AND.(H2.GT.EPS))THEN
           IF(U2.NE.0.0D0)THEN
!             COMPUTE THE ENERGY:U^2/2g + H + Z
              ENG = (U2*U2/DEUG) + ETA2
!             ENG MUST BE > Z1
              IF(ENG.GT.Z1)THEN
                Z1   = ETA2
                ETA1 = ETA2
                H1 = 0.0D0
                U1 = 0.0D0
                V1 = 0.0D0
              ENDIF
           ENDIF
        ENDIF
      ENDIF
      END SUBROUTINE