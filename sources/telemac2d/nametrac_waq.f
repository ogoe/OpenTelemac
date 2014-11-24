!                       **************************
                        SUBROUTINE NAMETRAC_WAQ 
!                       **************************
     &  (NAMETRAC,WAQ,SECCURRENT,WAQPROCESS,NTRAC,IND_SEC)
! 
! 
!*********************************************************************** 
! TELEMAC2D   V7P0                                  
!*********************************************************************** 
! 
!brieF gives names to tracers added by the water quality
! 
! 
!history  R.ATA
!+        12/09/2014 
!+        V7P0 
!+        CREATION 
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMETRAC       |<--| ARRAY OF NAMES OF TRACERS
!| WAQPROCESS     |-->| WAQ PROCESS 
!| WAQ            |-->| THERE IS WAQ OR NOT
!| SECCURRENT     |-->| THERE IS SECONDARY CURRENTS OR NOT
!| NTRAC          |-->| OLD NUMBER OF TRACER (BEFORE WAQ RTACERS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
! 
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*) 
      INTEGER, INTENT(IN)             ::  WAQPROCESS,NTRAC,IND_SEC
      LOGICAL, INTENT(IN)             ::  WAQ,SECCURRENT
!
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
!   
      IF(SECCURRENT)THEN
        NAMETRAC(IND_SEC) ='OMEGA           NA              '
      ENDIF  
      IF(WAQ)THEN
        SELECT CASE(WAQPROCESS)
!         O2 MODULE
          CASE(1)
            IF(LNG.EQ.1)THEN
              NAMETRAC(NTRAC+1) = 'O2 DISSOUS      '
     &                         // '   mgO2/l       '
              NAMETRAC(NTRAC+2) = 'CHARGE ORGANIQUE'
     &                         // '   mgO2/l       '
              NAMETRAC(NTRAC+3) = 'CHARGE NH4      '
     &                         // '   mgNH4/l      '
            ELSE
              NAMETRAC(NTRAC+1) = 'DISSOLVED O2    '
     &                         // '   mgO2/l       '
              NAMETRAC(NTRAC+2) = 'ORGANIC LOAD    '
     &                         // '   mgO2/l       '
              NAMETRAC(NTRAC+3) = 'NH4 LOAD        '
     &                         // '   mgNH4/l      '
            ENDIF 
!         BIOMASS MODULE
          CASE(2)
            IF(LNG.EQ.1)THEN
              NAMETRAC(NTRAC+1) = 'BIOMASSE PHYTO  '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+2) = 'PO4 DISSOUS     '
     &                         // '   mg/l         '
              NAMETRAC(NTRAC+3) = 'POR NON ASSIMILE'
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+4) = 'NO3 DISSOUS     '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+5) = 'NO3 NON ASSIMILE'
     &                        //  '   mg/l         '
            ELSE
              NAMETRAC(NTRAC+1) = 'PHYTO BIOMASS   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+2) = 'DISSOLVED PO4   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+3) = 'POR NON ASSIM   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+4) = 'DISSOLVED NO3   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+5) = 'NO3 NON ASSIM   '
     &                        //  '   mg/l         '
            ENDIF 
!         EUTRO MODULE
          CASE(3)
            IF(LNG.EQ.1)THEN
              NAMETRAC(NTRAC+1) = 'O2 DISSOUS      '
     &                        //  '   mgO2/l       '
              NAMETRAC(NTRAC+2) = 'BIOMASSE PHYTO  '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+3) = 'PO4 DISSOUS     '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+4) = 'POR NON ASSIMILE'
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+5) = 'NO3 DISSOUS     '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+6) = 'CHARGE NH4      '
     &                         // '   mgNH4/l      '
              NAMETRAC(NTRAC+7) = 'NOR NON ASSIMILE'
     &                         // '   mg/l         '
              NAMETRAC(NTRAC+8) = 'CHARGE ORGANIQUE'
     &                         // '   mgO2/l       '
            ELSE
              NAMETRAC(NTRAC+1) = 'DISSOLVED O2    '
     &                        //  '   mgO2/l       '
              NAMETRAC(NTRAC+2) = 'PHYTO BIOMASS   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+3) = 'DISSOLVED PO4   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+4) = 'POR NON ASSIMIL '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+5) = 'DISSOLVED NO3   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+6) = 'CHARGE NH4      '
     &                        //  '   mgNH4/l      '
              NAMETRAC(NTRAC+7) = 'NOR NON ASSIM   '
     &                        //    '   mg/l         '
              NAMETRAC(NTRAC+8) = 'ORGANIC CHARGE  '
     &                        //  '   mgO2/l       '
            ENDIF 
!         MICROPOL MODULE
          CASE(4)
            IF(LNG.EQ.1)THEN
              NAMETRAC(NTRAC+1) = 'MAT. EN SUSP.   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+2) = 'SEDIMENT DU FOND'
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+3) = 'MICRO POLLUANT  '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+4) = 'MES ABSORBEE    '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+5) = 'SED. FOND. ABS. '
     &                        //  '   mg/l         '
            ELSE
              NAMETRAC(NTRAC+1) = 'SUSPENDED LOAD  '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+2) = 'BED SEDIMENTS   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+3) = 'MICRO POLLUTANT '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+4) = 'ABS. SUSP. LOAD.'
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC+5) = 'ABSORB. BED SED.'
     &                        //  '   mg/l         '
            ENDIF 
!         THERMIC MODULE
          CASE(5)
            NAMETRAC(NTRAC+1) = '  TEMPERATURE   '
     &                      //  '      °C        '
          CASE DEFAULT
            IF(LNG.EQ.1) THEN
              WRITE(LU,10)WAQPROCESS
            ELSE
              WRITE(LU,20)WAQPROCESS
            ENDIF
            CALL PLANTE(1)
            STOP
                         
        END SELECT
      ENDIF
!
      RETURN
!----------------------------------------------------------------------- 
!     MESSAGES 
10    FORMAT(1X,'NAMETRAC_WAQ: MODULE WAQ INCONNU : ',I4) 
20    FORMAT(1X,'NAMETRAC_WAQ: UNKNOWN WAQ MODULE : ',I4) 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END                   
