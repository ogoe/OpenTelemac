C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FLUXES THROUGH CONTROL SECTIONS
!>                AND ADDS THEM UP TO OBTAIN OSCILLATING VOLUMES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note    THIS SUBROUTINE PRINTS OUT DISCHARGES THROUGH CONTROL
!>           SECTIONS. YOU CAN REWRITE IT TO DIVERT THESE PRINTOUTS
!>           TO A FILE OR TO CHANGE THE FORMAT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF_DEF, DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHARR, CTRLSC, FLX, FLXC, FLXS, INFO, NCSIZE, NSEC, NSEG, SUSP, TPS, VOLNEG, VOLNEGC, VOLNEGS, VOLPOS, VOLPOSC, VOLPOSS
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::IPID IPID@endlink<hr>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::CHAIN CHAIN@endlink, 
!> @link DECLARATIONS_SISYPHE::SISSEO SISSEO@endlink, 
!> @link DECLARATIONS_SISYPHE::SIS_FILES SIS_FILES@endlink, 
!> @link DECLARATIONS_SISYPHE::TITCA TITCA@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ERR, FMTZON, II, INIT, ISEC, NSEO, OLD_METHOD, WORK
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), P_DMAX(), P_DMIN(), P_DSUM(), P_IMIN()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUSEC_SISYPHE()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 27/12/2006
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHARR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CTRLSC
!></td><td>--></td><td>NUMBERS OF POINTS IN THE CONTROL SECTIONS
!>    </td></tr>
!>          <tr><td>FLX
!></td><td>--></td><td>FLUXES THROUGH CONTROL SECTIONS
!>    </td></tr>
!>          <tr><td>FLXC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLXS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>IF YES : INFORMATION IS PRINTED
!>    </td></tr>
!>          <tr><td>NCSIZE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEC
!></td><td>--></td><td>NUMBER OF CONTROL SECTIONS
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SUSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TPS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLNEG
!></td><td>--></td><td>CUMULATED NEGATIVE VOLUME THROUGH SECTIONS
!>    </td></tr>
!>          <tr><td>VOLNEGC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLNEGS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLPOS
!></td><td>--></td><td>CUMULATED POSITIVE VOLUME THROUGH SECTIONS
!>    </td></tr>
!>          <tr><td>VOLPOSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLPOSS
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUXPR_SISYPHE
     &(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,NSEG,NCSIZE,
     & FLXS,VOLNEGS,VOLPOSS,SUSP,FLXC,VOLNEGC,VOLPOSC,CHARR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHARR          |---| 
C| CTRLSC         |-->| NUMBERS OF POINTS IN THE CONTROL SECTIONS
C| FLX            |-->| FLUXES THROUGH CONTROL SECTIONS
C| FLXC           |---| 
C| FLXS           |---| 
C| INFO           |-->| IF YES : INFORMATION IS PRINTED
C| NCSIZE         |---| 
C| NSEC           |-->| NUMBER OF CONTROL SECTIONS
C| NSEG           |---| 
C| SUSP           |---| 
C| TPS            |---| 
C| VOLNEG         |-->| CUMULATED NEGATIVE VOLUME THROUGH SECTIONS
C| VOLNEGC        |---| 
C| VOLNEGS        |---| 
C| VOLPOS         |-->| CUMULATED POSITIVE VOLUME THROUGH SECTIONS
C| VOLPOSC        |---| 
C| VOLPOSS        |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF_DEF, ONLY: IPID
      USE DECLARATIONS_SISYPHE, ONLY:
     &          SIS_FILES,SISSEO,CHAIN,TITCA
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: NSEC,NCSIZE
      INTEGER, INTENT(IN)          :: CTRLSC(*)
      INTEGER, INTENT(IN)          :: NSEG(NSEC)
      LOGICAL, INTENT(IN)          :: INFO,SUSP,CHARR
      DOUBLE PRECISION, INTENT(IN) :: FLX(NSEC),TPS
      DOUBLE PRECISION, INTENT(IN) :: VOLNEG(NSEC),VOLPOS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: FLXS(NSEC),FLXC(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEGS(NSEC),VOLPOSS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEGC(NSEC),VOLPOSC(NSEC)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: WORK(:)
      DOUBLE PRECISION P_DMAX,P_DMIN,P_DSUM
      INTEGER                        P_IMIN
      EXTERNAL         P_DMAX,P_DMIN,P_DSUM,P_IMIN
C
      INTEGER ISEC,II,ERR
      CHARACTER(LEN=16) :: FMTZON='(4(1X,1PG21.14))'
      LOGICAL :: OLD_METHOD=.FALSE.
      LOGICAL, SAVE :: INIT=.TRUE.
      INTEGER, SAVE :: NSEO
C
C-----------------------------------------------------------------------
C
      IF (.NOT.ALLOCATED(CHAIN)) OLD_METHOD=.TRUE.
C
      IF(INFO) THEN
C
      IF (OLD_METHOD) THEN !JAJ #### FOLLOW FLUXPR.F OF BIEF BLINDLY
C
      IF(NCSIZE.LE.1) THEN
C
      DO ISEC = 1,NSEC
C
      IF(LNG.EQ.1) WRITE(LU,130) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &                                FLX(ISEC),
     &                                VOLNEG(ISEC),
     &                                VOLPOS(ISEC)
      IF(LNG.EQ.2) WRITE(LU,131) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &                                FLX(ISEC),
     &                                VOLNEG(ISEC),
     &                                VOLPOS(ISEC)
130   FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (ENTRE LES POINTS ',1I5,' ET ',1I5,')',//,5X,
     &               'DEBIT :                     ',G16.7,/,5X,
     &               'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &               'CUMUL DES DEBITS POSITIFS : ',G16.7)
131   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE:                 ',G16.7,/,5X,
     &               'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &               'CUMULATED POSITIVE VOLUME: ',G16.7)
      IF(SUSP) THEN
        IF(LNG.EQ.1) WRITE(LU,1301) FLXS(ISEC),
     &                              VOLNEGS(ISEC),
     &                              VOLPOSS(ISEC)
        IF(LNG.EQ.2) WRITE(LU,1302) FLXS(ISEC),
     &                              VOLNEGS(ISEC),
     &                              VOLPOSS(ISEC)
1301    FORMAT(5X,'DEBIT EN SUSPENSION :       ',G16.7,/,5X,
     &            'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &            'CUMUL DES DEBITS POSITIFS : ',G16.7)
1302    FORMAT(5X,'DISCHARGE IN SUSPENSION:   ',G16.7,/,5X,
     &            'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &            'CUMULATED POSITIVE VOLUME: ',G16.7)
      ENDIF
      IF(CHARR) THEN
        IF(LNG.EQ.1) WRITE(LU,1303) FLXC(ISEC),
     &                              VOLNEGC(ISEC),
     &                              VOLPOSC(ISEC)
        IF(LNG.EQ.2) WRITE(LU,1304) FLXC(ISEC),
     &                              VOLNEGC(ISEC),
     &                              VOLPOSC(ISEC)
1303    FORMAT(5X,'DEBIT EN CHARRIAGE :        ',G16.7,/,5X,
     &            'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &            'CUMUL DES DEBITS POSITIFS : ',G16.7)
1304    FORMAT(5X,'BEDLOAD DISCHARGE:         ',G16.7,/,5X,
     &            'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &            'CUMULATED POSITIVE VOLUME: ',G16.7)
      ENDIF
C
      ENDDO
C
      ELSE
C
      DO ISEC = 1,NSEC
C     SECTIONS ACROSS 2 SUB-DOMAINS WILL HAVE NSEG=0 OR -1
C     AND -1 WANTED HERE FOR RELEVANT MESSAGE
      II=P_IMIN(NSEG(ISEC))
C
      IF(II.GE.0) THEN
C
      IF(LNG.EQ.1) WRITE(LU,130) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &                 P_DMIN(FLX(ISEC))+P_DMAX(FLX(ISEC)),
     &                                P_DMIN(VOLNEG(ISEC)),
     &                                P_DMAX(VOLPOS(ISEC))
      IF(LNG.EQ.2) WRITE(LU,131) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &                 P_DMIN(FLX(ISEC))+P_DMAX(FLX(ISEC)),
     &                                P_DMIN(VOLNEG(ISEC)),
     &                                P_DMAX(VOLPOS(ISEC))
      IF(SUSP) THEN
        IF(LNG.EQ.1) WRITE(LU,1301)
     &              P_DMIN(FLXS(ISEC))+P_DMAX(FLXS(ISEC)),
     &                              P_DMIN(VOLNEGS(ISEC)),
     &                              P_DMAX(VOLPOSS(ISEC))
        IF(LNG.EQ.2) WRITE(LU,1302)
     &              P_DMIN(FLXS(ISEC))+P_DMAX(FLXS(ISEC)),
     &                              P_DMIN(VOLNEGS(ISEC)),
     &                              P_DMAX(VOLPOSS(ISEC))
      ENDIF
      IF(CHARR) THEN
        IF(LNG.EQ.1) WRITE(LU,1303)
     &              P_DMIN(FLXC(ISEC))+P_DMAX(FLXC(ISEC)),
     &                              P_DMIN(VOLNEGC(ISEC)),
     &                              P_DMAX(VOLPOSC(ISEC))
        IF(LNG.EQ.2) WRITE(LU,1304)
     &              P_DMIN(FLXC(ISEC))+P_DMAX(FLXC(ISEC)),
     &                              P_DMIN(VOLNEGC(ISEC)),
     &                              P_DMAX(VOLPOSC(ISEC))
      ENDIF
C
      ELSE
C
      IF(LNG.EQ.1) WRITE(LU,134) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1))
      IF(LNG.EQ.2) WRITE(LU,135) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1))
134   FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (ENTRE LES POINTS ',1I5,' ET ',1I5,')',//,5X,
     &               'A CHEVAL SUR DEUX SOUS-DOMAINES, PAS DE CALCUL')
135   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'ACROSS TWO SUB-DOMAINS, NO COMPUTATION')
      ENDIF
C
      ENDDO
C
      ENDIF ! NCSIZE
C
C-----------------------------------------------------------------------
C CHAIN ALLOCATED, I.E. SERIAL OR PARALLEL CASE FROM SECTIONS INPUT FILE
C       WE CAN APPLY CO-ORDINATES INSTEAD AND/OR NAMES OF SECTIONS

      ELSE ! .NOT.OLD_METHOD
C
        DO ISEC = 1,NSEC
C
          IF(LNG.EQ.1) WRITE(LU,230) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                 P_DSUM(FLX(ISEC)),P_DSUM(VOLNEG(ISEC)),
     &                                   P_DSUM(VOLPOS(ISEC))
          IF(LNG.EQ.2) WRITE(LU,231) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                 P_DSUM(FLX(ISEC)),P_DSUM(VOLNEG(ISEC)),
     &                                   P_DSUM(VOLPOS(ISEC))
230       FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (NOM ',A,')',//,5X,
     &               'DEBIT :                     ',G16.7,/,5X,
     &               'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &               'CUMUL DES DEBITS POSITIFS : ',G16.7)
231       FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (NAME ',A,')',//,5X,
     &               'DISCHARGE:                 ',G16.7,/,5X,
     &               'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &               'CUMULATED POSITIVE VOLUME: ',G16.7)
          IF(SUSP) THEN
            IF(LNG.EQ.1) WRITE(LU,2301)
     &              P_DSUM(FLXS(ISEC)),P_DSUM(VOLNEGS(ISEC)),
     &                                 P_DSUM(VOLPOSS(ISEC))
            IF(LNG.EQ.2) WRITE(LU,2302)
     &              P_DSUM(FLXS(ISEC)),P_DSUM(VOLNEGS(ISEC)),
     &                                 P_DSUM(VOLPOSS(ISEC))
2301        FORMAT(5X,'DEBIT EN SUSPENSION :       ',G16.7,/,5X,
     &            'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &            'CUMUL DES DEBITS POSITIFS : ',G16.7)
2302        FORMAT(5X,'DISCHARGE IN SUSPENSION:   ',G16.7,/,5X,
     &            'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &            'CUMULATED POSITIVE VOLUME: ',G16.7)
          ENDIF
          IF(CHARR) THEN
            IF(LNG.EQ.1) WRITE(LU,2303)
     &              P_DSUM(FLXC(ISEC)),P_DSUM(VOLNEGC(ISEC)),
     &                                 P_DSUM(VOLPOSC(ISEC))
            IF(LNG.EQ.2) WRITE(LU,2304)
     &              P_DSUM(FLXC(ISEC)),P_DSUM(VOLNEGC(ISEC)),
     &                                 P_DSUM(VOLPOSC(ISEC))
2303        FORMAT(5X,'DEBIT EN CHARRIAGE :        ',G16.7,/,5X,
     &            'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &            'CUMUL DES DEBITS POSITIFS : ',G16.7)
2304        FORMAT(5X,'BEDLOAD DISCHARGE:         ',G16.7,/,5X,
     &            'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &            'CUMULATED POSITIVE VOLUME: ',G16.7)
          ENDIF
C
        ENDDO
C
      ENDIF ! IF OLD_METHOD
C
      ENDIF ! IF INFO
C !JAJ ####
C-----------------------------------------------------------------------
C MASTER WRITES A NICE SECTIONS OUTPUT FILE, THE HEADER ONLY ONCE
C NOTE: PROGRAMMED FOR THE BEDL LOAD DISCHARGE ONLY (IF CHARR IMPLIED)
C
      IF ( (.NOT.OLD_METHOD) .AND. CHARR .AND.
     &     (TRIM(SIS_FILES(SISSEO)%NAME).NE.'') ) THEN
        IF (INIT) THEN
          INIT=.FALSE.
          IF ((NCSIZE.GT.1 .AND. IPID.EQ.0).OR.(NCSIZE.LE.1)) THEN
            NSEO=SIS_FILES(SISSEO)%LU
            WRITE(NSEO,*) 'TITLE = "BEDLOAD DISCHARGES FOR ',
     &                     TRIM(TITCA),'"'
            WRITE(NSEO,*) 'VARIABLES = T',
     &           (' '//TRIM(CHAIN(ISEC)%DESCR), ISEC=1,NSEC)
          ENDIF
          IF (NCSIZE.GT.1) THEN
            ALLOCATE (WORK(NSEC), STAT=ERR)
            IF (ERR.NE.0) THEN
              WRITE(LU,*) 'FLUXPR_SISYPHE: ERROR ALLOCATING WORK:',ERR
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDIF
        ! DEADLOCK WITH WRITE AND P_DSUM IN AN IMPLIED WRITE LOOP
        ! BECAUSE IT IS ONLY MASTER TO WRITE THE MESSAGE...
        IF (NCSIZE.GT.1) THEN
          DO ISEC=1,NSEC
            WORK(ISEC)=P_DSUM(FLXC(ISEC))
          END DO
          IF (IPID.EQ.0)
     &      WRITE (NSEO, FMT=FMTZON) TPS, (WORK(ISEC), ISEC=1,NSEC)
        ELSE
          WRITE (NSEO, FMT=FMTZON) TPS, (FLXC(ISEC), ISEC=1,NSEC)
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE FLUXPR_SISYPHE
C
C#######################################################################
C