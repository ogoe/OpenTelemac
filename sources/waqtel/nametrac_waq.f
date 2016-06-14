!                       ***********************
                        SUBROUTINE NAMETRAC_WAQ
!                       ***********************
     &  (TEXTE,TEXTPR,NAMETRAC,NTRAC,IND_T,
     &   MAXTRA,ICONVFT,VISCT,DIMM)
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
!| DIMM           |-->| 2D OR 3D
!| ICONVFT        |<--| ARRAY OF ADVECTION OPTIONS
!| IND_T          |-->| INDEX OF THE TEMPERATURE
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| TEXTE          |-->| NAME OF THE FRENCH OUTPUT VARIABLES
!| NAMETRAC       |<--| ARRAY OF NAMES OF TRACERS
!| NTRAC          |-->| ACTUAL NUMBER OF TRACER (OLD TRACER + ADDTR)
!| VISCT          |<--| VISCOSITY OF TRACERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL
      USE INTERFACE_WAQTEL, EX_NAMETRAC_WAQ => NAMETRAC_WAQ
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   )::  NTRAC,MAXTRA,DIMM
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
      CHARACTER(LEN=32), INTENT(INOUT)::  TEXTE(*),TEXTPR(*)
      INTEGER          , INTENT(INOUT)::  ICONVFT(MAXTRA),IND_T
      TYPE(BIEF_OBJ)   , INTENT(INOUT)::  VISCT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!

      INTEGER :: I,NTRAC1,OUTPUT_SIZE
!
!-----------------------------------------------------------------------
!
      NTRAC1 = NTRAC - ADDTR
      SELECT CASE(WAQPROCESS)
!       O2 MODULE
        CASE(1)
          IF(LNG.EQ.1)THEN
            NAMETRAC(NTRAC1+1) = 'O2 DISSOUS      '
     &                        // '   mgO2/l       '
            NAMETRAC(NTRAC1+2) = 'CHARGE ORGANIQUE'
     &                        // '   mgO2/l       '
            NAMETRAC(NTRAC1+3) = 'CHARGE NH4      '
     &                        // '   mgNH4/l      '
          ELSE
            NAMETRAC(NTRAC1+1) = 'DISSOLVED O2    '
     &                        // '   mgO2/l       '
            NAMETRAC(NTRAC1+2) = 'ORGANIC LOAD    '
     &                        // '   mgO2/l       '
            NAMETRAC(NTRAC1+3) = 'NH4 LOAD        '
<<<<<<< .working
     &                       // '   mgNH4/l      '
          ENDIF
=======
     &                        // '   mgNH4/l      '
          ENDIF
>>>>>>> .merge-right.r7666
!       BIOMASS MODULE
        CASE(2)
          IF(LNG.EQ.1)THEN
            NAMETRAC(NTRAC1+1) = 'BIOMASSE PHYTO  '
     &                       //  ' micro_g/l      '
            NAMETRAC(NTRAC1+2) = 'PO4 DISSOUS     '
     &                        // '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'POR NON ASSIMILE'
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'NO3 DISSOUS     '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'NOR NON ASSIMILE'
     &                       //  '   mg/l         '
          ELSE
            NAMETRAC(NTRAC1+1) = 'PHYTO BIOMASS   '
     &                       //  '  micro_g/l     '
            NAMETRAC(NTRAC1+2) = 'DISSOLVED PO4   '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'POR NON ASSIM   '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'DISSOLVED NO3   '
<<<<<<< .working
     &                      //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'NO3 NON ASSIM   '
     &                      //  '   mg/l         '
          ENDIF
=======
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'NOR NON ASSIM   '
     &                       //  '   mg/l         '
          ENDIF
!
>>>>>>> .merge-right.r7666
!       EUTRO MODULE
!
        CASE(3)
          IF(LNG.EQ.1)THEN
<<<<<<< .working
              NAMETRAC(NTRAC1+1) = 'BIOMASSE PHYTO  '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC1+2) = 'PO4 DISSOUS     '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC1+3) = 'POR NON ASSIMILE'
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC1+4) = 'NO3 DISSOUS     '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC1+5) = 'NOR NON ASSIMILE'
     &                         // '   mg/l         '
              NAMETRAC(NTRAC1+6) = 'CHARGE NH4      '
     &                         // '   mgNH4/l      '
              NAMETRAC(NTRAC1+7) = 'CHARGE ORGANIQUE'
     &                         // '   mgO2/l       '
              NAMETRAC(NTRAC1+8) = 'O2 DISSOUS      '
     &                        //  '   mgO2/l       '
            ELSE
              NAMETRAC(NTRAC1+1) = 'PHYTO BIOMASS   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC1+2) = 'DISSOLVED PO4   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC1+3) = 'POR NON ASSIMIL '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC1+4) = 'DISSOLVED NO3   '
     &                        //  '   mg/l         '
              NAMETRAC(NTRAC1+5) = 'NOR NON ASSIM   '
     &                        //    '   mg/l         '
              NAMETRAC(NTRAC1+6) = 'CHARGE NH4      '
     &                        //  '   mgNH4/l      '
              NAMETRAC(NTRAC1+7) = 'ORGANIC CHARGE  '
     &                        //  '   mgO2/l       '
              NAMETRAC(NTRAC1+8) = 'DISSOLVED O2    '
     &                        //  '   mgO2/l       '
          ENDIF
=======
            NAMETRAC(NTRAC1+1) = 'BIOMASSE PHYTO  '
     &                       //  '  micro_g/l     '
            NAMETRAC(NTRAC1+2) = 'PO4 DISSOUS     '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'POR NON ASSIMILE'
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'NO3 DISSOUS     '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'NOR NON ASSIMILE'
     &                        // '   mg/l         '
            NAMETRAC(NTRAC1+6) = 'CHARGE NH4      '
     &                        // '   mgNH4/l      '
            NAMETRAC(NTRAC1+7) = 'CHARGE ORGANIQUE'
     &                        // '   mgO2/l       '
            NAMETRAC(NTRAC1+8) = 'O2 DISSOUS      '
     &                       //  '   mgO2/l       '
          ELSE
            NAMETRAC(NTRAC1+1) = 'PHYTO BIOMASS   '
     &                       //  '  micro_g/l     '
            NAMETRAC(NTRAC1+2) = 'DISSOLVED PO4   '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'POR NON ASSIMIL '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'DISSOLVED NO3   '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'NOR NON ASSIM   '
     &                     //    '   mg/l         '
            NAMETRAC(NTRAC1+6) = 'NH4 LOAD        '
     &                       //  '   mgNH4/l      '
            NAMETRAC(NTRAC1+7) = 'ORGANIC LOAD    '
     &                       //  '   mgO2/l       '
            NAMETRAC(NTRAC1+8) = 'DISSOLVED O2    '
     &                       //  '   mgO2/l       '
          ENDIF
!
>>>>>>> .merge-right.r7666
!       MICROPOL MODULE
!
        CASE(4)
          IF(LNG.EQ.1)THEN
            NAMETRAC(NTRAC1+1) = 'MAT. EN SUSP.   '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+2) = 'SEDIMENT DU FOND'
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'MICRO POLLUANT  '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'MES ABSORBEE    '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'SED. FOND. ABS. '
     &                       //  '   mg/l         '
          ELSE
            NAMETRAC(NTRAC1+1) = 'SUSPENDED LOAD  '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+2) = 'BED SEDIMENTS   '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+3) = 'MICRO POLLUTANT '
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+4) = 'ABS. SUSP. LOAD.'
     &                       //  '   mg/l         '
            NAMETRAC(NTRAC1+5) = 'ABSORB. BED SED.'
<<<<<<< .working
     &                      //  '   mg/l         '
          ENDIF
!         TRACER 2 IS NOT ADVECTED NEIHTER DIFFUSED
          ICONVFT(NTRAC1+5)=0
=======
     &                       //  '   mg/l         '
          ENDIF
!         TRACERS 2 AND 5 ARE NOT ADVECTED NEITHER DIFFUSED
!          ICONVFT(NTRAC1+2)=0
!          ICONVFT(NTRAC1+5)=0
          CALL OS( 'X=0     ' , X=VISCT%ADR(NTRAC1+2)%P)
>>>>>>> .merge-right.r7666
          CALL OS( 'X=0     ' , X=VISCT%ADR(NTRAC1+5)%P)
!
!      THERMIC MODULE
!
        CASE(5)
          ! Only adding temperature if it is not already in the tracers
          IF(IND_T.EQ.0)THEN
            IND_T = NTRAC1+1
            NAMETRAC(NTRAC1+1) = '  TEMPERATURE   '
     &                       //  '      �C        '
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
! WARNING: OUTPUT_SIZE IS HARDCODED TO BE CHANGED WHENEVER THE NUMBER
!          OF GRAPHICAL OUTPUT VARIABLES CHENGES
      IF(DIMM.EQ.2)THEN
        OUTPUT_SIZE=33
      ELSEIF(DIMM.EQ.3)THEN
        OUTPUT_SIZE=28
      ENDIF
      DO I=1,ADDTR
        TEXTE( OUTPUT_SIZE+I+NTRAC1) = NAMETRAC(I+NTRAC1)
        TEXTPR(OUTPUT_SIZE+I+NTRAC1) = NAMETRAC(I+NTRAC1)
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
