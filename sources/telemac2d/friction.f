!                    *******************
                     SUBROUTINE FRICTION
!                    *******************
!
     &(NS,G,DT,UA,H,QU,QV,CF)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FRICTION TERM.
!
!history  INRIA
!+
!+        V5P4
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
!| CF             |-->| THE FRICTION COEFFICIENT
!| DT             |-->| TIME STEP
!| G              |-->| GRAVITY
!| H              |-->| WATER DEPTH AT TN
!| NS             |-->| TOTAL NUMBER OF NODES
!| QU             |-->| HU AT TIME TN
!| QV             |-->| HV AT TIME TN
!| UA             |<->| (H,HU,HV) AT TN+1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS
      DOUBLE PRECISION, INTENT(IN)    :: G,DT
      DOUBLE PRECISION, INTENT(IN)    :: CF(NS)
      DOUBLE PRECISION, INTENT(IN)    :: H(NS),QU(NS),QV(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: UA(3,NS)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS
      DOUBLE PRECISION AKAP,AKAP1,STRIC2
!
!-----------------------------------------------------------------------
!
      DO IS =1,NS
!
        STRIC2=CF(IS)**2
!
! FH-FRDATA
!        IF(H(IS).LE.1.D-12.OR.UA(1,IS).LE.1.D-12)  THEN
        IF((H(IS)   .LE.1.D-12).OR.
     &     (UA(1,IS).LE.1.D-12).OR.
     &     (CF(IS)  .LE.1.D-12)    ) THEN
! FH-FRDATA
          AKAP=0.D0
        ELSE
          AKAP= G*DT*SQRT(QU(IS)**2+QV(IS)**2)/
     &         (STRIC2*H(IS)*UA(1,IS)**(4.D0/3.D0))
        ENDIF
!
        AKAP1=1.D0/(1.D0+AKAP)
        UA(2,IS) = AKAP1*UA(2,IS)
        UA(3,IS) = AKAP1*UA(3,IS)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
