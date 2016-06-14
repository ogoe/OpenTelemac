!                       ***********************
                        SUBROUTINE YASMI_WAQ 
!                       ***********************
     &  (NTRAC,YASMI)
! 
! 
!*********************************************************************** 
! TELEMAC2D   V7P0                                  
!*********************************************************************** 
! 
!brieF tells which rtacers will have implicit source terms
! 
! 
!history  R.ATA
!+        12/02/2016 
!+        V7P2
!+        CREATION 
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NTRAC          |-->| ACTUAL NUMBER OF TRACER (OLD TRACER + ADDTR)
!| YASMI          |<--| LOGICS FOR IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
      USE BIEF
      USE DECLARATIONS_WAQTEL
      USE INTERFACE_WAQTEL, EX_YASMI_WAQ => YASMI_WAQ
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
!  
      INTEGER          , INTENT(IN   )::  NTRAC
      LOGICAL          , INTENT(INOUT)::  YASMI(*)
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 

      INTEGER :: NTRAC1
!
!-----------------------------------------------------------------------
!     
      NTRAC1 = NTRAC - ADDTR
      SELECT CASE(WAQPROCESS)
!
!       O2 MODULE
!
        CASE(1)
!         DISSOLVED O2
          YASMI(NTRAC1+1)=.FALSE.
!         ORGANIC LOAD
          YASMI(NTRAC1+2)=.TRUE.
!         NH4 LOAD  
          YASMI(NTRAC1+3)=.TRUE.
!
!       BIOMASS MODULE
!
        CASE(2)
!         PHYTO BIOMASS 
          YASMI(NTRAC1+1)=.FALSE.
!         DISSOLVED PO4
          YASMI(NTRAC1+2)=.FALSE.
!         POR NON ASSIM   
          YASMI(NTRAC1+3)=.FALSE.
!         DISSOLVED NO3
          YASMI(NTRAC1+4)=.FALSE.
!         NO3 NON ASSIM
          YASMI(NTRAC1+5)=.FALSE.
!
!       EUTRO MODULE
!
        CASE(3)
!         PHYTO BIOMASS 
          YASMI(NTRAC1+1)=.FALSE.
!         DISSOLVED PO4
          YASMI(NTRAC1+2)=.FALSE.
!         POR NON ASSIM   
          YASMI(NTRAC1+3)=.FALSE.
!         DISSOLVED NO3
          YASMI(NTRAC1+4)=.FALSE.
!         NOR NON ASSIM  
          YASMI(NTRAC1+5)=.FALSE.
!         CHARGE NH4 
          YASMI(NTRAC1+6)=.TRUE.
!         ORGANIC LOAD  
          YASMI(NTRAC1+7)=.TRUE.
!         DISSOLVED O2 
          YASMI(NTRAC1+8)=.FALSE.
!
!       MICROPOL MODULE
!
        CASE(4)
!         SUSPENDED LOAD  
          YASMI(NTRAC1+1)=.FALSE.
!         BED SEDIMENTS
          YASMI(NTRAC1+2)=.FALSE.
!         MICRO POLLUTANT
          YASMI(NTRAC1+3)=.TRUE.
!         ABS. SUSP. LOAD
          YASMI(NTRAC1+4)=.TRUE.
!         ABSORB. BED SED
          YASMI(NTRAC1+5)=.FALSE.
!
!      THERMIC MODULE
!
        CASE(5)
          YASMI(NTRAC1+1)=.FALSE.
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,10)WAQPROCESS
          ELSE
            WRITE(LU,20)WAQPROCESS
          ENDIF
          CALL PLANTE(1)
          STOP
                         
      END SELECT
!
      RETURN
!----------------------------------------------------------------------- 
!     MESSAGES 
10    FORMAT(1X,'YASMI_WAQ: MODULE WAQ INCONNU : ',I4) 
20    FORMAT(1X,'YASMI_WAQ: UNKNOWN WAQ MODULE : ',I4) 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END                   
