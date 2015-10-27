!                       ***********************
                        SUBROUTINE NAMETRAC_WAQ 
!                       ***********************
     &  (TEXTE,TEXTPR,NAMETRAC,NTRAC,IND_T,WAQPROCESS)
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
!| TEXTE          |-->| NAME OF THE FRENCH OUTPUT VARIABLES
!| WAQPROCESS     |-->| NAME OF THE ENGLISH OUTPUT VARIABLE
!| NAMETRAC       |<--| ARRAY OF NAMES OF TRACERS
!| NTRAC          |-->| ACTUAL NUMBER OF TRACER (OLD TRACER + MAXWAQTRAC)
!| IND_T          |-->| INDEX OF THE TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
      USE DECLARATIONS_WAQTEL
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
! 
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*) 
      INTEGER, INTENT(IN)             ::  WAQPROCESS,NTRAC
      INTEGER, INTENT(INOUT)          ::  IND_T
      CHARACTER(LEN=32), INTENT(INOUT)::  TEXTE(*),TEXTPR(*)
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 

      INTEGER :: I,NTRAC1
!
!-----------------------------------------------------------------------
!     
      NTRAC1 = NTRAC - ADDTR
      SELECT CASE(WAQPROCESS)
!       O2 MODULE
        CASE(1)
          IF(LNG.EQ.1)THEN
            NAMETRAC(NTRAC1+1) = 'O2 DISSOUS      '
     &                       // '   mgO2/l       '
            NAMETRAC(NTRAC1+2) = 'CHARGE ORGANIQUE'
     &                       // '   mgO2/l       '
            NAMETRAC(NTRAC1+3) = 'CHARGE NH4      '
     &                       // '   mgNH4/l      '
          ELSE
            NAMETRAC(NTRAC1+1) = 'DISSOLVED O2    '
     &                       // '   mgO2/l       '
            NAMETRAC(NTRAC1+2) = 'ORGANIC LOAD    '
     &                       // '   mgO2/l       '
            NAMETRAC(NTRAC1+3) = 'NH4 LOAD        '
     &                       // '   mgNH4/l      '
          ENDIF 
          ADDTR = 3
!       BIOMASS MODULE
        CASE(2)
          IF(LNG.EQ.1)THEN
            NAMETRAC(NTRAC1+1) = 'BIOMASSE PHYTO  '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+2) = 'PO4 DISSOUS     '
     &                       // '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'POR NON ASSIMILE'
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'NO3 DISSOUS     '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'NO3 NON ASSIMILE'
     &                      //  '   mg/l         '
          ELSE
            NAMETRAC(NTRAC1+1) = 'PHYTO BIOMASS   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+2) = 'DISSOLVED PO4   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'POR NON ASSIM   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'DISSOLVED NO3   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'NO3 NON ASSIM   '
     &                      //  '   mg/l         '
          ENDIF 
          ADDTR = 5
!       EUTRO MODULE
        CASE(3)
          IF(LNG.EQ.1)THEN
            NAMETRAC(NTRAC1+1) = 'O2 DISSOUS      '
     &                      //  '   mgO2/l       '
            NAMETRAC(NTRAC1+2) = 'BIOMASSE PHYTO  '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'PO4 DISSOUS     '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'POR NON ASSIMILE'
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'NO3 DISSOUS     '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+6) = 'CHARGE NH4      '
     &                       // '   mgNH4/l      '
            NAMETRAC(NTRAC1+7) = 'NOR NON ASSIMILE'
     &                       // '   mg/l         '
            NAMETRAC(NTRAC1+8) = 'CHARGE ORGANIQUE'
     &                       // '   mgO2/l       '
          ELSE
            NAMETRAC(NTRAC1+1) = 'DISSOLVED O2    '
     &                      //  '   mgO2/l       '
            NAMETRAC(NTRAC1+2) = 'PHYTO BIOMASS   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'DISSOLVED PO4   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'POR NON ASSIMIL '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'DISSOLVED NO3   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+6) = 'CHARGE NH4      '
     &                      //  '   mgNH4/l      '
            NAMETRAC(NTRAC1+7) = 'NOR NON ASSIM   '
     &                      //    '   mg/l         '
            NAMETRAC(NTRAC1+8) = 'ORGANIC CHARGE  '
     &                      //  '   mgO2/l       '
          ENDIF 
          ADDTR = 8
!       MICROPOL MODULE
        CASE(4)
          IF(LNG.EQ.1)THEN
            NAMETRAC(NTRAC1+1) = 'MAT. EN SUSP.   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+2) = 'SEDIMENT DU FOND'
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'MICRO POLLUANT  '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'MES ABSORBEE    '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'SED. FOND. ABS. '
     &                      //  '   mg/l         '
          ELSE
            NAMETRAC(NTRAC1+1) = 'SUSPENDED LOAD  '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+2) = 'BED SEDIMENTS   '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'MICRO POLLUTANT '
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'ABS. SUSP. LOAD.'
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'ABSORB. BED SED.'
     &                      //  '   mg/l         '
          ENDIF 
          ADDTR = 5
!       THERMIC MODULE
        CASE(5)
          ! Only adding temperature if it is not already in the tracers
          IF(IND_T.EQ.0)THEN  
            IND_T = NTRAC1+1
            NAMETRAC(NTRAC1+1) = '  TEMPERATURE   '
     &                      //  '      °C        '
          ENDIF
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
      ! Update texte and textepr
      DO I=1,ADDTR
        TEXTE(33+I+NTRAC1)  = NAMETRAC(I+NTRAC1)
        TEXTPR(33+I+NTRAC1) = NAMETRAC(I+NTRAC1)
      ENDDO
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
