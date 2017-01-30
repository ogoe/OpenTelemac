!                    *************************
                     SUBROUTINE INIT_COMPO_COH
!                    *************************
!
     &(ES,CONC_VASE,CONC,NPOIN,NOMBLAY,NSICLA,AVAIL,AVA0)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NLAYMAX
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPOIN,NOMBLAY,NSICLA
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION  EPAI_VASE(NLAYMAX),EPAI_SABLE(NLAYMAX)

      INTEGER I,J
!
!-----------------------------------------------------------------------

      EPAI_VASE(1)=0.01D0
      EPAI_VASE(2)=0.02D0
      EPAI_VASE(3)=0.03995D0
      EPAI_VASE(4)=0.0437D0
      EPAI_VASE(5)=0.0517D0
      EPAI_VASE(6)=0.1259D0
      EPAI_VASE(7)=0.4889D0
      EPAI_VASE(8)=1.5071D0
      EPAI_VASE(9)=0.86410D0
!
!-----------------------------------------------------------------------
!
!     INITIALISING THE NUMBER OF LAYERS
!

!     BY DEFAULT : UNIFORM BED COMPOSITION (KEY WORDS)
!
      DO I=1,NPOIN
        DO J= 1,NOMBLAY
          CONC(I,J)=CONC_VASE(J)
          ES(I,J)  =EPAI_VASE(J)
        ENDDO
      ENDDO
!

!
!-----------------------------------------------------------------------
!
      RETURN
      END

