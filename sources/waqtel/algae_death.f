!                    ***********************
                     SUBROUTINE ALGAE_DEATH
!                    ***********************
!
     &(ALD,CMOR,TRR,TRESP,GT,TOX,NPOIN )
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES THE DISAPPEARANCE RATE OF ALGAE
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALD            |<--| ALGAE DEATH
!| CMOR           |-->| COEFFICIENTS OF ALGAE DEATH RATE AT 20C
!| RAY            |-->| EFFECT OF SUNSHINE in [0,1]
!| GT             |-->| EFFECT OF OF TEMPERATURE ON ALGAE GROWTH
!|                |   | GT=T/20,  T: WATER TEMPERATURE
!| TOX            |-->| COEFFICIENT OF WATER TOXICITY
!| TR             |-->| TRACER (PHYTOPLANCTOPN BIOMASS)
!| TRESP          |-->| RESPIRATION RATE OF ALGAL BIOMASS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_WAQTEL, EX_ALGAE_DEATH=>ALGAE_DEATH
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN):: NPOIN
      DOUBLE PRECISION, INTENT(IN):: CMOR(2),TRR(NPOIN),TOX,TRESP,GT
      DOUBLE PRECISION, INTENT(INOUT)::ALD(NPOIN)
!     LOCAL VARIABLES
      INTEGER I
      DOUBLE PRECISION CC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CC=TRESP+CMOR(1)+TOX
!
      DO I=1,NPOIN
        ALD(I)=GT*(CC+CMOR(2)*TRR(I))
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
