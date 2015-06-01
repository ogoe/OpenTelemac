!                    **********************
                      SUBROUTINE CALCS_BIOMASS
!                    **********************
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                        21/09/2014
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR BIOMASS WAQ PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION
!

!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!
!
      IF(LNG.EQ.1)THEN
        WRITE(LU,*) 'BIOMASS: NOT IMPLEMENTED YET'
      ELSE
        WRITE(LU,*) 'BIOMASS: NOT IMPLEMENTED YET'
      ENDIF
      CALL PLANTE(1)
      STOP


!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
