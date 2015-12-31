!                    ************************
                     SUBROUTINE ALGAE_GROWTH
!                    ************************
!
     &(ALG,CMAX,RAY,GT,NUTR,TOX,NPOIN )
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES THE GROWTH RATE OF ALGAE
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALG            |<--| ALGAE GROWTH
!| CMAX           |-->| MAXIMAL GROWTH RATE AT 20C
!| RAY            |-->| EFFECT OF SUNSHINE in [0,1]
!| GT             |-->| EFFECT OF OF TEMPERATURE ON ALGAE GROWTH 
!|                |   | GT=T/20,  T: WATER TEMPERATURE 
!| NUTR           |-->| EFFECT OF PHOSPHORED AND NITROGENIOUS NUTRIMENTS
!| TOX            |-->| COEFFICIENT OF WATER TOXICITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_WAQTEL, EX_ALGAE_GROWTH => ALGAE_GROWTH
!
      IMPLICIT NONE
      INTEGER LNG,LU,I
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN):: NPOIN
      DOUBLE PRECISION, INTENT(IN):: CMAX,RAY(NPOIN),GT,NUTR(NPOIN),TOX
      DOUBLE PRECISION, INTENT(INOUT)::ALG(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DO I=1,NPOIN
        ALG(I)=CMAX*RAY(I)*GT*NUTR(I)*TOX
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
