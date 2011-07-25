!                    *************************
                     SUBROUTINE NOMVAR_SISYPHE
!                    *************************
!
     &( TEXTE ,TEXTPR , MNEMO , NSICLA , UNIT )
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND
!+                GEOMETRY FILES.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/95
!+
!+
!
!history  M. GONZALES DE LINARES; C.VILLARET
!+        2003
!+
!+
!
!history  JMH
!+        03/11/2009
!+        V6P0
!+   MODIFIED AFTER JACEK JANKOWSKI DEVELOPMENTS
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
!| MNEMO          |<--| SYMBOLS TO SPECIFY THE VARIABLES FOR OUTPUT
!|                |   | IN THE STEERING FILE
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| TEXTE          |<--| NAMES OF VARIABLES (PRINTOUT)
!| TEXTPR         |<--| NAMES OF VARIABLES (INPUT)
!| UNIT           |-->| LOGICAL, FILE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE, ONLY : MAXVAR,NSICLM,NLAYMAX,NOMBLAY,
     &                                 NPRIV
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)         :: NSICLA
      CHARACTER*8, INTENT(INOUT)  :: MNEMO(MAXVAR)
      CHARACTER*32, INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      LOGICAL, INTENT(IN)         :: UNIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,ADD
!
      CHARACTER(LEN=32) TEXTE_AVAI(NLAYMAX*NSICLM),TEXTE_QS(NSICLM)
      CHARACTER(LEN=32) TEXTE_CS(NSICLM),TEXTE_QSC(NSICLM)
      CHARACTER(LEN=32) TEXTE_QSS(NSICLM),TEXTE_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_AVAI(NLAYMAX*NSICLM),MNEMO_QS(NSICLM)
      CHARACTER(LEN=8)  MNEMO_CS(NSICLM),MNEMO_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_QSC(NSICLM),MNEMO_QSS(NSICLM)
      CHARACTER(LEN=2)  CLA
      CHARACTER(LEN=1)  LAY
!
!-----------------------------------------------------------------------
! CV 3010 +1
      ADD=27+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+NOMBLAY
!V      ADD=26+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+NOMBLAY
      IF(ADD.GT.MAXVAR) THEN
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'NOMVAR_SISYPHE : MAXVAR DOIT VALOIR AU MOINS ',ADD
        ENDIF
        IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'NOMVAR_SISYPHE: MAXVAR SHOULD BE AT LEAST ',ADD
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!     2 3RD FRACTION MEANS FRACTION OF SEDIMENT OF CLASS 3 IN 2ND LAYER
!
      IF(NOMBLAY.GT.9.OR.NSICLA.GT.99) THEN
        WRITE (LU,*) 'REPROGRAM NOMVAR_SISYPHE DUE TO CONSTANT FORMATS'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,NSICLA
        DO J=1,NOMBLAY
          K=(I-1)*NOMBLAY+J
          WRITE(LAY,'(I1)') J
          IF(I.LT.10) THEN
            WRITE(CLA,'(I1)') I
          ELSE
            WRITE(CLA,'(I2)') I
          ENDIF
          TEXTE_AVAI(K) = TRIM('FRAC LAY '//LAY//' CL '//CLA)
          MNEMO_AVAI(K) = TRIM(LAY//'A'//CLA)
        ENDDO
      ENDDO
!
      DO J=1,NSICLA
        IF(J<10) THEN
          WRITE(CLA,'(I1)') J
        ELSE
          WRITE(CLA,'(I2)') J
        ENDIF
        TEXTE_QS(J)  = TRIM('QS CLASS '//CLA)
        TEXTE_QSC(J) = TRIM('QS BEDLOAD CL'//CLA)
        TEXTE_QSS(J) = TRIM('QS SUSP. CL'//CLA)
        IF(UNIT) THEN
          TEXTE_CS(J) = TRIM('CONC MAS CL'//CLA)
          TEXTE_CS(J)(17:19) = 'G/L'
        ELSE
          TEXTE_CS(J) = TRIM('CONC VOL CL'//CLA)
        ENDIF
        MNEMO_QS(J)  = TRIM('QS'//CLA)
        MNEMO_QSC(J) = TRIM('QSBL'//CLA)
        MNEMO_QSS(J) = TRIM('QSS'//CLA)
        MNEMO_CS(J)  = TRIM('CS'//CLA)
      ENDDO
!
      DO K=1,NOMBLAY
        WRITE(LAY,'(I1)') K
!V        TEXTE_ES(K)(1:16)  = 'LAY. '//LAY//' THICKNESS'
        TEXTE_ES(K)(1:16)  = 'LAYER'//LAY//' THICKNESS'
        TEXTE_ES(K)(17:32) = 'M               '
        MNEMO_ES(K) = LAY//'ES     '
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.2) THEN
!
!       ENGLISH VERSION
!
        TEXTE(01) = 'VELOCITY U      M/S             '
        TEXTE(02) = 'VELOCITY V      M/S             '
        TEXTE(03) = 'WATER DEPTH     M               '
        TEXTE(04) = 'FREE SURFACE    M               '
        TEXTE(05) = 'BOTTOM          M               '
        TEXTE(06) = 'FLOWRATE Q      M3/S/M          '
        TEXTE(07) = 'FLOWRATE QX     M3/S/M          '
        TEXTE(08) = 'FLOWRATE QY     M3/S/M          '
        TEXTE(09) = 'RIGID BED       M               '
        TEXTE(10) = 'FRICTION COEFT                  '
        TEXTE(11) = 'BED SHEAR STRESSN/M2        '
        TEXTE(12) = 'WAVE HEIGHT HM0 M               '
        TEXTE(13) = 'PEAK PERIOD TPR5S               '
        TEXTE(14) = 'MEAN DIRECTION  DEG             '
        TEXTE(15) = 'SOLID DISCH     M2/S            '
        TEXTE(16) = 'SOLID DISCH X   M2/S            '
        TEXTE(17) = 'SOLID DISCH Y   M2/S            '
        TEXTE(18) = 'EVOLUTION       M               '
        TEXTE(19) = 'RUGOSITE TOTALE M               '
        TEXTE(20) = 'FROT. PEAU MU                   '
!V 2010
        TEXTE(21) = 'MEAN DIAMETER M                 '
! CV 2010 +1
        ADD=NSICLA*(NOMBLAY+2)
        TEXTE(22+ADD)='QS BEDLOAD      M2/S            '
        TEXTE(23+ADD)='QS BEDLOAD X    M2/S            '
        TEXTE(24+ADD)='QS BEDLOAD Y    M2/S            '
        TEXTE(25+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(26+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(27+ADD)='QS SUSPENSION Y M2/S            '
!
      ELSE
!
!       FRENCH VERSION
!
        TEXTE(01)  = 'VITESSE U       M/S             '
        TEXTE(02)  = 'VITESSE V       M/S             '
        TEXTE(03)  = 'HAUTEUR D''EAU   M              '
        TEXTE(04)  = 'SURFACE LIBRE   M               '
        TEXTE(05)  = 'FOND            M               '
        TEXTE(06)  = 'DEBIT           M3/S/M          '
        TEXTE(07)  = 'DEBIT QX        M3/S/M          '
        TEXTE(08)  = 'DEBIT QY        M3/S/M          '
        TEXTE(09)  = 'FOND RIGIDE     M               '
        TEXTE(10)  = 'COEFT FROTTEMENT                '
        TEXTE(11)  = 'FROTTEMENT TOB  N/M2            '
        TEXTE(12)  = 'HAUTEUR HM0     M               '
        TEXTE(13)  = 'PERIODE PIC TPR5S               '
        TEXTE(14)  = 'DIRECTION MOY   DEG             '
        TEXTE(15)  = 'DEBIT SOLIDE    M2/S            '
        TEXTE(16)  = 'DEBIT SOLIDE X  M2/S            '
        TEXTE(17)  = 'DEBIT SOLIDE Y  M2/S            '
        TEXTE(18)  = 'EVOLUTION       M               '
        TEXTE(19)  = 'RUGOSITE TOTALE.M               '
        TEXTE(20)  = 'CORR FROTT PEAU MU              '
        TEXTE(21)  = 'DIAMETRE MOYEN  M               '
!
        ADD=NSICLA*(NOMBLAY+2)
        TEXTE(22+ADD)='QS CHARRIAGE    M2/S            '
        TEXTE(23+ADD)='QS CHARRIAGE X  M2/S            '
        TEXTE(24+ADD)='QS CHARRIAGE Y  M2/S            '
        TEXTE(25+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(26+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(27+ADD)='QS SUSPENSION Y M2/S            '
!
      ENDIF
!
!     AVAIL: ALL LAYERS OF CLASS 1, THEN ALL LAYERS OF CLASS 2, ETC.
!            SAME ORDER AS IN POINT_SISYPHE
!
      DO J=1,NOMBLAY
        DO I=1,NSICLA
!V 2010    +1
          TEXTE(21+(I-1)*NOMBLAY+J) = TEXTE_AVAI((I-1)*NOMBLAY+J)
          MNEMO(21+(I-1)*NOMBLAY+J) = MNEMO_AVAI((I-1)*NOMBLAY+J)
        ENDDO
      ENDDO
!
      DO I=1,NSICLA
!V 2010    +1
        TEXTE(21+I+NOMBLAY*NSICLA)     = TEXTE_QS(I)
        MNEMO(21+I+NOMBLAY*NSICLA)     = MNEMO_QS(I)
        TEXTE(21+I+(NOMBLAY+1)*NSICLA) = TEXTE_CS(I)
        MNEMO(21+I+(NOMBLAY+1)*NSICLA) = MNEMO_CS(I)
        TEXTE(27+I+NSICLA*(NOMBLAY+2)) = TEXTE_QSC(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+2)) = MNEMO_QSC(I)
        TEXTE(27+I+NSICLA*(NOMBLAY+3)) = TEXTE_QSS(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+3)) = MNEMO_QSS(I)
      ENDDO
!
!V 2010    +1
      DO I=1,NOMBLAY
        TEXTE(27+I+NSICLA*(NOMBLAY+4)) = TEXTE_ES(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+4)) = MNEMO_ES(I)
      ENDDO
!
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY
      TEXTE(28+ADD)='PRIVE 1                         '
      TEXTE(29+ADD)='PRIVE 2                         '
      TEXTE(30+ADD)='PRIVE 3                         '
      TEXTE(31+ADD)='PRIVE 4                         '
!     NPRIV MAY BE GREATER THAN 4
!     TEXTE(31+ADD)='PRIVE 5                         '
!
!V 2010 +1
      DO I=1,31+NSICLA*(NOMBLAY+4)+NOMBLAY
        TEXTPR(I)=TEXTE(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     OTHER NAMES FOR OUTPUT VARIABLES (STEERING FILE)
!
!     VELOCITY U
      MNEMO(1)   = 'U       '
!     VELOCITY V
      MNEMO(2)   = 'V       '
!     WATER DEPTH
      MNEMO(3)   = 'H       '
!     FREE SURFACE
      MNEMO(4)   = 'S       '
!     BOTTOM
      MNEMO(5)   = 'B       '
!     SCALAR FLOW RATE
      MNEMO(6)   = 'Q       '
!     SCALAR FLOW RATE X
      MNEMO(7)   = 'I       '
!     SCALAR FLOW RATE Y
      MNEMO(8)   = 'J       '
!     RIGID BED
      MNEMO(9)   = 'R       '
!     FRICTION COEFFICIENT
      MNEMO(10)   = 'CHESTR  '
!     MEAN BOTTOM FRICTION
      MNEMO(11)   = 'TOB     '
!     WAVE HEIGHT
      MNEMO(12)   = 'W       '
!     PEAK PERIOD
      MNEMO(13)   = 'X       '
!     WAVE DIRECTION
      MNEMO(14)   = 'THETAW  '
!     SOLID DISCHARGE
      MNEMO(15)   = 'M       '
!     SOLID DISCHARGE X
      MNEMO(16)   = 'N       '
!     SOLID DISCHARGE Y
      MNEMO(17)   = 'P       '
!     EVOLUTION
      MNEMO(18)   = 'E       '
!     KS
      MNEMO(19)   = 'KS      '
!     MU
      MNEMO(20)   = 'MU      '
! CV 2010
      MNEMO(21)   = 'D50     '
! +1
      MNEMO(22+NSICLA*(NOMBLAY+2)) = 'QSBL    '
      MNEMO(23+NSICLA*(NOMBLAY+2)) = 'QSBLX   '
      MNEMO(24+NSICLA*(NOMBLAY+2)) = 'QSBLY   '
      MNEMO(25+NSICLA*(NOMBLAY+2)) = 'QSSUSP  '
      MNEMO(26+NSICLA*(NOMBLAY+2)) = 'QSSUSPX '
      MNEMO(27+NSICLA*(NOMBLAY+2)) = 'QSSUSPY '
!
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY
      MNEMO(28+ADD) = 'A       '
      MNEMO(29+ADD) = 'G       '
      MNEMO(30+ADD) = 'L       '
      MNEMO(31+ADD) = 'O       '
!     THE NUMBER OF PRIVATE ARRAYS IS A KEYWORD
!     MNEMO(31+ADD) = '????????'
!
!----------------------------
! CV 2010: +1
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY+27+MAX(NPRIV,4)
      IF(ADD.LT.MAXVAR) THEN
        DO I=ADD+1,MAXVAR
          MNEMO(I) =' '
          TEXTE(I) =' '
          TEXTPR(I)=' '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
