C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE VARIABLE NAMES FOR THE RESULTS AND
!>                GEOMETRY FILES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MNEMO, NSICLA, TEXTE, TEXTPR, UNIT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_SISYPHE::NOMBLAY NOMBLAY@endlink, 
!> @link DECLARATIONS_SISYPHE::NPRIV NPRIV@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADD, CLA, I, J, K, LAY, MNEMO_AVAI, MNEMO_CS, MNEMO_ES, MNEMO_QS, MNEMO_QSC, MNEMO_QSS, TEXTE_AVAI, TEXTE_CS, TEXTE_ES, TEXTE_QS, TEXTE_QSC, TEXTE_QSS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>LECDON_SISYPHE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 03/11/2009
!> </td><td> JMH
!> </td><td> MODIFIED AFTER JACEK JANKOWSKI DEVELOPMENTS
!>           FOR RESTARTS WITH GRADED SEDIMENTS
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 2003
!> </td><td> M. GONZALES DE LINARES; C.VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/09/95
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MNEMO
!></td><td><--</td><td>SYMBOLS TO SPECIFY THE VARIABLES FOR OUTPUT
!>                  IN THE STEERING FILE
!>    </td></tr>
!>          <tr><td>NSICLA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEXTE
!></td><td><--</td><td>NAMES OF VARIABLES (PRINTOUT)
!>    </td></tr>
!>          <tr><td>TEXTPR
!></td><td><--</td><td>NAMES OF VARIABLES (INPUT)
!>    </td></tr>
!>          <tr><td>UNIT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NOMVAR_SISYPHE
     &( TEXTE ,TEXTPR , MNEMO , NSICLA , UNIT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MNEMO          |<--| SYMBOLS TO SPECIFY THE VARIABLES FOR OUTPUT
C|                |   | IN THE STEERING FILE
C| NSICLA         |---| 
C| TEXTE          |<--| NAMES OF VARIABLES (PRINTOUT)
C| TEXTPR         |<--| NAMES OF VARIABLES (INPUT)
C| UNIT           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_SISYPHE, ONLY : MAXVAR,NSICLM,NLAYMAX,NOMBLAY,
     &                                 NPRIV
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)         :: NSICLA
      CHARACTER*8, INTENT(INOUT)  :: MNEMO(MAXVAR)
      CHARACTER*32, INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      LOGICAL, INTENT(IN)         :: UNIT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J,K,ADD
C
      CHARACTER(LEN=32) TEXTE_AVAI(NLAYMAX*NSICLM),TEXTE_QS(NSICLM)
      CHARACTER(LEN=32) TEXTE_CS(NSICLM),TEXTE_QSC(NSICLM)
      CHARACTER(LEN=32) TEXTE_QSS(NSICLM),TEXTE_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_AVAI(NLAYMAX*NSICLM),MNEMO_QS(NSICLM)
      CHARACTER(LEN=8)  MNEMO_CS(NSICLM),MNEMO_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_QSC(NSICLM),MNEMO_QSS(NSICLM)
      CHARACTER(LEN=2)  CLA
      CHARACTER(LEN=1)  LAY
C
C-----------------------------------------------------------------------
C CV 3010 +1
      ADD=27+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+NOMBLAY
CV      ADD=26+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+NOMBLAY
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
C
C-----------------------------------------------------------------------
C     2 3RD FRACTION MEANS FRACTION OF SEDIMENT OF CLASS 3 IN 2ND LAYER
C
      IF(NOMBLAY.GT.9.OR.NSICLA.GT.99) THEN
        WRITE (LU,*) 'REPROGRAM NOMVAR_SISYPHE DUE TO CONSTANT FORMATS'
        CALL PLANTE(1)
        STOP
      ENDIF
C
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
C
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
C
      DO K=1,NOMBLAY
        WRITE(LAY,'(I1)') K
CV        TEXTE_ES(K)(1:16)  = 'LAY. '//LAY//' THICKNESS'
        TEXTE_ES(K)(1:16)  = 'LAYER'//LAY//' THICKNESS'
        TEXTE_ES(K)(17:32) = 'M               '
        MNEMO_ES(K) = LAY//'ES     '
      ENDDO
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.2) THEN
C
C       ENGLISH VERSION
C
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
        TEXTE(11) = 'BOTTOM FRICTION N/M2            '
        TEXTE(12) = 'WAVE HEIGHT HM0 M               '
        TEXTE(13) = 'PEAK PERIOD TPR5S               '
        TEXTE(14) = 'MEAN DIRECTION  DEG             '
        TEXTE(15) = 'SOLID DISCH     M2/S            '
        TEXTE(16) = 'SOLID DISCH X   M2/S            '
        TEXTE(17) = 'SOLID DISCH Y   M2/S            '
        TEXTE(18) = 'EVOLUTION       M               '
        TEXTE(19) = 'RUGOSITE TOTALE M               '
        TEXTE(20) = 'FROT. PEAU MU                   '
CV 2010
        TEXTE(21) = 'MEAN DIAMETER M                 '
C CV 2010 +1
        ADD=NSICLA*(NOMBLAY+2)
        TEXTE(22+ADD)='QS BEDLOAD      M2/S            '
        TEXTE(23+ADD)='QS BEDLOAD X    M2/S            '
        TEXTE(24+ADD)='QS BEDLOAD Y    M2/S            '
        TEXTE(25+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(26+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(27+ADD)='QS SUSPENSION Y M2/S            '
C
      ELSE
C
C       FRENCH VERSION
C
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
C
        ADD=NSICLA*(NOMBLAY+2)
        TEXTE(22+ADD)='QS CHARRIAGE    M2/S            '
        TEXTE(23+ADD)='QS CHARRIAGE X  M2/S            '
        TEXTE(24+ADD)='QS CHARRIAGE Y  M2/S            '
        TEXTE(25+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(26+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(27+ADD)='QS SUSPENSION Y M2/S            '
C
      ENDIF
C
C     AVAIL: ALL LAYERS OF CLASS 1, THEN ALL LAYERS OF CLASS 2, ETC.
C            SAME ORDER AS IN POINT_SISYPHE
C
      DO J=1,NOMBLAY
        DO I=1,NSICLA
CV 2010    +1    
          TEXTE(21+(I-1)*NOMBLAY+J) = TEXTE_AVAI((I-1)*NOMBLAY+J)
          MNEMO(21+(I-1)*NOMBLAY+J) = MNEMO_AVAI((I-1)*NOMBLAY+J)
        ENDDO
      ENDDO
C
      DO I=1,NSICLA
CV 2010    +1 
        TEXTE(21+I+NOMBLAY*NSICLA)     = TEXTE_QS(I)
        MNEMO(21+I+NOMBLAY*NSICLA)     = MNEMO_QS(I)
        TEXTE(21+I+(NOMBLAY+1)*NSICLA) = TEXTE_CS(I)
        MNEMO(21+I+(NOMBLAY+1)*NSICLA) = MNEMO_CS(I)
        TEXTE(27+I+NSICLA*(NOMBLAY+2)) = TEXTE_QSC(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+2)) = MNEMO_QSC(I)
        TEXTE(27+I+NSICLA*(NOMBLAY+3)) = TEXTE_QSS(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+3)) = MNEMO_QSS(I)
      ENDDO
C
CV 2010    +1
      DO I=1,NOMBLAY
        TEXTE(27+I+NSICLA*(NOMBLAY+4)) = TEXTE_ES(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+4)) = MNEMO_ES(I)
      ENDDO
C
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY
      TEXTE(28+ADD)='PRIVE 1                         '
      TEXTE(29+ADD)='PRIVE 2                         '
      TEXTE(30+ADD)='PRIVE 3                         '
      TEXTE(31+ADD)='PRIVE 4                         '
C     NPRIV MAY BE GREATER THAN 4
C     TEXTE(31+ADD)='PRIVE 5                         '
C
CV 2010 +1
      DO I=1,31+NSICLA*(NOMBLAY+4)+NOMBLAY
        TEXTPR(I)=TEXTE(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
C     OTHER NAMES FOR OUTPUT VARIABLES (STEERING FILE)
C
C     VELOCITY U
      MNEMO(1)   = 'U       '
C     VELOCITY V
      MNEMO(2)   = 'V       '
C     WATER DEPTH
      MNEMO(3)   = 'H       '
C     FREE SURFACE
      MNEMO(4)   = 'S       '
C     BOTTOM
      MNEMO(5)   = 'B       '
C     SCALAR FLOW RATE
      MNEMO(6)   = 'Q       '
C     SCALAR FLOW RATE X
      MNEMO(7)   = 'I       '
C     SCALAR FLOW RATE Y
      MNEMO(8)   = 'J       '
C     RIGID BED
      MNEMO(9)   = 'R       '
C     FRICTION COEFFICIENT
      MNEMO(10)   = 'CHESTR  '
C     MEAN BOTTOM FRICTION
      MNEMO(11)   = 'TOB     '
C     WAVE HEIGHT
      MNEMO(12)   = 'W       '
C     PEAK PERIOD
      MNEMO(13)   = 'X       '
C     WAVE DIRECTION
      MNEMO(14)   = 'THETAW  '
C     SOLID DISCHARGE
      MNEMO(15)   = 'M       '
C     SOLID DISCHARGE X
      MNEMO(16)   = 'N       '
C     SOLID DISCHARGE Y
      MNEMO(17)   = 'P       '
C     EVOLUTION
      MNEMO(18)   = 'E       '
C     KS
      MNEMO(19)   = 'KS      '
C     MU
      MNEMO(20)   = 'MU      '
C CV 2010
      MNEMO(21)   = 'D50     '
C +1
      MNEMO(22+NSICLA*(NOMBLAY+2)) = 'QSBL    '
      MNEMO(23+NSICLA*(NOMBLAY+2)) = 'QSBLX   '
      MNEMO(24+NSICLA*(NOMBLAY+2)) = 'QSBLY   '
      MNEMO(25+NSICLA*(NOMBLAY+2)) = 'QSSUSP  '
      MNEMO(26+NSICLA*(NOMBLAY+2)) = 'QSSUSPX '
      MNEMO(27+NSICLA*(NOMBLAY+2)) = 'QSSUSPY '
C
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY
      MNEMO(28+ADD) = 'A       '
      MNEMO(29+ADD) = 'G       '
      MNEMO(30+ADD) = 'L       '
      MNEMO(31+ADD) = 'O       '
C     THE NUMBER OF PRIVATE ARRAYS IS A KEYWORD
C     MNEMO(31+ADD) = '????????'
C
C----------------------------
C CV 2010: +1
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY+27+MAX(NPRIV,4)
      IF(ADD.LT.MAXVAR) THEN
        DO I=ADD+1,MAXVAR
          MNEMO(I) =' '
          TEXTE(I) =' '
          TEXTPR(I)=' '
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
