!                    *************************
                     SUBROUTINE FLUXPR_SISYPHE
!                    *************************
!
     &(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,NSEG,NCSIZE,
     & FLXS,VOLNEGS,VOLPOSS,SUSP,FLXC,VOLNEGC,VOLPOSC,CHARR)
!
!***********************************************************************
! SISYPHE   V7P0                                         21/07/2011
!***********************************************************************
!
!brief    COMPUTES FLUXES THROUGH CONTROL SECTIONS
!+                AND ADDS THEM UP TO OBTAIN OSCILLATING VOLUMES.
!
!note     THIS SUBROUTINE PRINTS OUT DISCHARGES THROUGH CONTROL
!+           SECTIONS. YOU CAN REWRITE IT TO DIVERT THESE PRINTOUTS
!+           TO A FILE OR TO CHANGE THE FORMAT.
!
!history  J-M HERVOUET (LNHE)
!+        27/12/2006
!+        V5P7
!+
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
!history  C. VILLARET & J-M HERVOUET (EDF LAB, LNHE)
!+        18/04/2014
!+        V7P0
!+   Printing information on bedload and suspension, and writing the
!+   sections output file with both old and new methods.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        06/05/2014
!+        V7P0
!+   A use of P_DSUM removed in the scalar section (caused a stop with
!+   some compilers).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHARR          |-->| LOGICAL, BEDLOAD OR NOT
!| CTRLSC         |-->| NUMBERS OF POINTS IN THE CONTROL SECTIONS
!| FLX            |-->| FLUXES THROUGH CONTROL SECTIONS
!| FLXC           |-->| BEDLOAD DISCHARGE
!| FLXS           |-->| SUSPENDED LOAD DISCHARGE
!| INFO           |-->| IF YES : INFORMATION IS PRINTED
!| NCSIZE         |-->| NUMBER OF PROCESSORS (PARALLEL)
!| NSEC           |-->| NUMBER OF CONTROL SECTIONS
!| NSEG           |-->| NUMBER OF SEGMENTS PER CONTROL SECTION
!| SUSP           |-->| LOGICAL, SUSPENSION OR NOT
!| TPS            |-->| TIME
!| VOLNEG         |-->| CUMULATED NEGATIVE VOLUME THROUGH SECTIONS
!| VOLNEGC        |-->| CUMULATED NEGATIVE VOLUME FOR THE BEDLOAD
!| VOLNEGS        |-->| CUMULATED NEGATIVE VOLUME FOR THE SUSPENSION
!| VOLPOS         |-->| CUMULATED POSITIVE VOLUME THROUGH SECTIONS
!| VOLPOSC        |-->| CUMULATED POSITIVE VOLUME FOR THE BEDLOAD
!| VOLPOSS        |-->| CUMULATED POSITIVE VOLUME FOR THE SUSPENDED LOAD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: IPID
      USE DECLARATIONS_SISYPHE, ONLY: SIS_FILES,SISSEO,CHAIN
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NSEC,NCSIZE
      INTEGER, INTENT(IN)          :: CTRLSC(*)
      INTEGER, INTENT(IN)          :: NSEG(NSEC)
      LOGICAL, INTENT(IN)          :: INFO,SUSP,CHARR
      DOUBLE PRECISION, INTENT(IN) :: FLX(NSEC),TPS
      DOUBLE PRECISION, INTENT(IN) :: VOLNEG(NSEC),VOLPOS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: FLXS(NSEC),FLXC(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEGS(NSEC),VOLPOSS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEGC(NSEC),VOLPOSC(NSEC)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: WORK(:),WORKB(:)
      DOUBLE PRECISION P_DMAX,P_DMIN,P_DSUM

      INTEGER                        P_IMIN
      EXTERNAL         P_DMAX,P_DMIN,P_DSUM,P_IMIN
!
      INTEGER ISEC,II,ERR,NSEO
      CHARACTER(LEN=16) :: FMTZON='(4(1X,1PG21.14))'
      LOGICAL :: OLD_METHOD=.FALSE.
      LOGICAL, SAVE :: INIT=.TRUE.
!
!-----------------------------------------------------------------------
!
      NSEO=SIS_FILES(SISSEO)%LU
!
!-----------------------------------------------------------------------
!
      IF(.NOT.ALLOCATED(CHAIN)) OLD_METHOD=.TRUE.
!
!     DONE ONCE FOR ALL
!
      IF(INIT.AND.(TRIM(SIS_FILES(SISSEO)%NAME).NE.'') ) THEN
!
        IF(NCSIZE.GT.1) THEN
          ALLOCATE (WORK(NSEC), STAT=ERR)
          IF(ERR.NE.0) THEN
            WRITE(LU,*) 'FLUXPR_SISYPHE: ERROR ALLOCATING WORK:',ERR
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(CHARR.AND.SUSP) THEN
            ALLOCATE (WORKB(NSEC), STAT=ERR)
            IF(ERR.NE.0) THEN
              WRITE(LU,*) 'FLUXPR_SISYPHE: ERROR ALLOCATING WORK:',ERR
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDIF
!
        INIT=.FALSE.
!
        IF(CHARR.AND..NOT.SUSP) THEN
          WRITE(NSEO,*) ' INTEGRATED BEDLOAD DISCHARGES '
          WRITE(NSEO,*) ' VARIABLES = TIME(S) QC(M3/S) FOR',
     &           (' '//TRIM(CHAIN(ISEC)%DESCR), ISEC=1,NSEC)
        ENDIF
!
        IF(SUSP.AND..NOT.CHARR) THEN
          WRITE(NSEO,*) ' INTEGRATED SUSPENDED LOAD DISCHARGES '
          WRITE(NSEO,*) ' VARIABLES = TIME   QS (M3/S) FOR SECTIONS '
     &           ,(II,II=1,NSEC)
        ENDIF
!
        IF(CHARR.AND.SUSP) THEN
          WRITE(NSEO,*) ' INTEGRATED BEDLOAD AND SUSPENDED LOAD '
          WRITE(NSEO,*) 'VARIABLES = TIME , QC FOR ',
     &           (' '//TRIM(CHAIN(ISEC)%DESCR), ISEC=1,NSEC), ' QS FOR',
     &           (' '//TRIM(CHAIN(ISEC)%DESCR), ISEC=1,NSEC)
        ENDIF
!
        WRITE(NSEO,100)(II , II= 1 ,NSEC)
100     FORMAT(' TIME',' SECTION:',I2, 'SECTION: ',I2)
!
      ENDIF
!
      IF(INFO) THEN
!
      IF(OLD_METHOD) THEN
!
        IF(NCSIZE.LE.1) THEN
!
!         SCALAR MODE
!
          DO ISEC = 1,NSEC
!
            IF(LNG.EQ.1) WRITE(LU,130) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                 CTRLSC(2+2*(ISEC-1)),
     &                                 FLX(ISEC),VOLNEG(ISEC),
     &                                 VOLPOS(ISEC)
            IF(LNG.EQ.2) WRITE(LU,131) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                 CTRLSC(2+2*(ISEC-1)),
     &                                 FLX(ISEC),VOLNEG(ISEC),
     &                                 VOLPOS(ISEC)
!
130         FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (ENTRE LES POINTS ',1I5,' ET ',1I5,')',//,5X,
     &               'DEBIT :                     ',G16.7,/,5X,
     &               'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &               'CUMUL DES DEBITS POSITIFS : ',G16.7)
131         FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE:                 ',G16.7,/,5X,
     &               'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &               'CUMULATED POSITIVE VOLUME: ',G16.7)
            IF(SUSP) THEN
              IF(LNG.EQ.1) WRITE(LU,1301) FLXS(ISEC),
     &                                    VOLNEGS(ISEC),
     &                                    VOLPOSS(ISEC)
              IF(LNG.EQ.2) WRITE(LU,1302) FLXS(ISEC),
     &                                    VOLNEGS(ISEC),
     &                                    VOLPOSS(ISEC)
            ENDIF
!
1301        FORMAT(5X,'DEBIT EN SUSPENSION :       ',G16.7,/,5X,
     &                'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &                'CUMUL DES DEBITS POSITIFS : ',G16.7)
1302        FORMAT(5X,'DISCHARGE IN SUSPENSION:   ',G16.7,/,5X,
     &                'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &                'CUMULATED POSITIVE VOLUME: ',G16.7)
            IF(CHARR) THEN
              IF(LNG.EQ.1) WRITE(LU,1303) FLXC(ISEC),
     &                                    VOLNEGC(ISEC),
     &                                    VOLPOSC(ISEC)
              IF(LNG.EQ.2) WRITE(LU,1304) FLXC(ISEC),
     &                                    VOLNEGC(ISEC),
     &                                    VOLPOSC(ISEC)
1303          FORMAT(5X,'DEBIT EN CHARRIAGE :        ',G16.7,/,5X,
     &                  'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &                  'CUMUL DES DEBITS POSITIFS : ',G16.7)
1304          FORMAT(5X,'BEDLOAD DISCHARGE:         ',G16.7,/,5X,
     &                  'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &                  'CUMULATED POSITIVE VOLUME: ',G16.7)
            ENDIF
!
          ENDDO
!
        ELSE
!
!         PARALLEL MODE
!
          DO ISEC = 1,NSEC
!
!           SECTIONS ACROSS 2 SUB-DOMAINS WILL HAVE NSEG=0 OR -1
!           AND -1 WANTED HERE FOR RELEVANT MESSAGE
!
            II=P_IMIN(NSEG(ISEC))
            IF(II.GE.0) THEN
!
              IF(LNG.EQ.1) WRITE(LU,130) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                   CTRLSC(2+2*(ISEC-1)),
     &                 P_DMIN(FLX(ISEC))+P_DMAX(FLX(ISEC)),
     &                                   P_DMIN(VOLNEG(ISEC)),
     &                                   P_DMAX(VOLPOS(ISEC))
              IF(LNG.EQ.2) WRITE(LU,131) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                   CTRLSC(2+2*(ISEC-1)),
     &                 P_DMIN(FLX(ISEC))+P_DMAX(FLX(ISEC)),
     &                                   P_DMIN(VOLNEG(ISEC)),
     &                                   P_DMAX(VOLPOS(ISEC))
!
              IF(SUSP) THEN
                IF(LNG.EQ.1) WRITE(LU,1301)
     &              P_DMIN(FLXS(ISEC))+P_DMAX(FLXS(ISEC)),
     &                                 P_DMIN(VOLNEGS(ISEC)),
     &                                 P_DMAX(VOLPOSS(ISEC))
                IF(LNG.EQ.2) WRITE(LU,1302)
     &              P_DMIN(FLXS(ISEC))+P_DMAX(FLXS(ISEC)),
     &                                 P_DMIN(VOLNEGS(ISEC)),
     &                                 P_DMAX(VOLPOSS(ISEC))
              ENDIF
              IF(CHARR) THEN
                IF(LNG.EQ.1) WRITE(LU,1303)
     &               P_DMIN(FLXC(ISEC))+P_DMAX(FLXC(ISEC)),
     &                                  P_DMIN(VOLNEGC(ISEC)),
     &                                  P_DMAX(VOLPOSC(ISEC))
                IF(LNG.EQ.2) WRITE(LU,1304)
     &               P_DMIN(FLXC(ISEC))+P_DMAX(FLXC(ISEC)),
     &                                  P_DMIN(VOLNEGC(ISEC)),
     &                                  P_DMAX(VOLPOSC(ISEC))
              ENDIF
!
!           OLD METHOD AND SECTION ON SEVERAL SUB-DOMAIN
!           IN THIS CASE NOTHING IS COMPUTED
!
            ELSE
!
              IF(LNG.EQ.1) WRITE(LU,134) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                        CTRLSC(2+2*(ISEC-1))
              IF(LNG.EQ.2) WRITE(LU,135) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                        CTRLSC(2+2*(ISEC-1))
134           FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (ENTRE LES POINTS ',1I5,' ET ',1I5,')',//,5X,
     &               'A CHEVAL SUR DEUX SOUS-DOMAINES, PAS DE CALCUL')
135           FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'ACROSS TWO SUB-DOMAINS, NO COMPUTATION')
!
            ENDIF
!
          ENDDO
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!       NEW METHOD
!       CHAIN ALLOCATED, I.E. SERIAL OR PARALLEL CASE FROM SECTIONS INPUT FILE
!       WE CAN APPLY CO-ORDINATES INSTEAD AND/OR NAMES OF SECTIONS
!
        DO ISEC = 1,NSEC
!
          IF(NCSIZE.GT.1) THEN
            IF(LNG.EQ.1) WRITE(LU,230) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                   P_DSUM(FLX(ISEC)),P_DSUM(VOLNEG(ISEC)),
     &                                     P_DSUM(VOLPOS(ISEC))
            IF(LNG.EQ.2) WRITE(LU,231) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                   P_DSUM(FLX(ISEC)),P_DSUM(VOLNEG(ISEC)),
     &                                     P_DSUM(VOLPOS(ISEC))
          ELSE
            IF(LNG.EQ.1) WRITE(LU,230) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                   FLX(ISEC),VOLNEG(ISEC),VOLPOS(ISEC)
            IF(LNG.EQ.2) WRITE(LU,231) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                   FLX(ISEC),VOLNEG(ISEC),VOLPOS(ISEC)
          ENDIF
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
            IF(NCSIZE.GT.1) THEN
              IF(LNG.EQ.1) WRITE(LU,2301)
     &                P_DSUM(FLXS(ISEC)),P_DSUM(VOLNEGS(ISEC)),
     &                                   P_DSUM(VOLPOSS(ISEC))
              IF(LNG.EQ.2) WRITE(LU,2302)
     &                P_DSUM(FLXS(ISEC)),P_DSUM(VOLNEGS(ISEC)),
     &                                   P_DSUM(VOLPOSS(ISEC))
            ELSE
              IF(LNG.EQ.1) WRITE(LU,2301)
     &                FLXS(ISEC),VOLNEGS(ISEC),VOLPOSS(ISEC)
              IF(LNG.EQ.2) WRITE(LU,2302)
     &                FLXS(ISEC),VOLNEGS(ISEC),VOLPOSS(ISEC)
            ENDIF
2301        FORMAT(5X,'DEBIT EN SUSPENSION :       ',G16.7,/,5X,
     &            'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &            'CUMUL DES DEBITS POSITIFS : ',G16.7)
2302        FORMAT(5X,'DISCHARGE IN SUSPENSION:   ',G16.7,/,5X,
     &            'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,

     &            'CUMULATED POSITIVE VOLUME: ',G16.7)
          ENDIF
!
          IF(CHARR) THEN
            IF(NCSIZE.GT.1) THEN
              IF(LNG.EQ.1) WRITE(LU,2303)
     &                P_DSUM(FLXC(ISEC)),P_DSUM(VOLNEGC(ISEC)),
     &                                   P_DSUM(VOLPOSC(ISEC))
              IF(LNG.EQ.2) WRITE(LU,2304)
     &                P_DSUM(FLXC(ISEC)),P_DSUM(VOLNEGC(ISEC)),
     &                                   P_DSUM(VOLPOSC(ISEC))
            ELSE
              IF(LNG.EQ.1) WRITE(LU,2303)
     &                FLXC(ISEC),VOLNEGC(ISEC),VOLPOSC(ISEC)
              IF(LNG.EQ.2) WRITE(LU,2304)
     &                FLXC(ISEC),VOLNEGC(ISEC),VOLPOSC(ISEC)
            ENDIF
2303        FORMAT(5X,'DEBIT EN CHARRIAGE :        ',G16.7,/,5X,
     &            'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &            'CUMUL DES DEBITS POSITIFS : ',G16.7)
2304        FORMAT(5X,'BEDLOAD DISCHARGE:         ',G16.7,/,5X,
     &            'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &            'CUMULATED POSITIVE VOLUME: ',G16.7)
          ENDIF
!
        ENDDO
!
!
      ENDIF ! IF OLD_METHOD
!
!     A SECTIONS OUTPUT FILE HAS BEEN GIVEN, IT IS FILLED
!
      IF(TRIM(SIS_FILES(SISSEO)%NAME).NE.'') THEN
!
!       ONLY BEDLOAD
!
        IF(CHARR.AND..NOT.SUSP) THEN
          IF(NCSIZE.GT.1) THEN
            DO ISEC=1,NSEC
              WORK(ISEC)=P_DSUM(FLXC(ISEC))
            ENDDO
!           IN // ONLY PROCESSOR 0 WRITES THE FILE
            IF(IPID.EQ.0) THEN
              WRITE(NSEO,FMT=FMTZON) TPS,(WORK(ISEC),ISEC=1,NSEC)
            ENDIF
          ELSE
            WRITE(NSEO,FMT=FMTZON) TPS,(FLXC(ISEC),ISEC=1,NSEC)
          ENDIF
        ENDIF
!
!       ONLY SUSPENSION
!
        IF(SUSP.AND..NOT.CHARR) THEN
          IF(NCSIZE.GT.1) THEN
            DO ISEC=1,NSEC
              WORK(ISEC)=P_DSUM(FLXS(ISEC))
            ENDDO
!           IN // ONLY PROCESSOR 0 WRITES THE FILE
            IF(IPID.EQ.0) THEN
              WRITE (NSEO,FMT=FMTZON) TPS,(WORK(ISEC),ISEC=1,NSEC)
            ENDIF
          ELSE
            WRITE(NSEO,FMT=FMTZON) TPS,(FLXS(ISEC),ISEC=1,NSEC)
          ENDIF
        ENDIF
!
!       BOTH BEDLOAD AND SUSPENSION
!
        IF(SUSP.AND.CHARR) THEN
          IF(NCSIZE.GT.1) THEN
            DO ISEC=1,NSEC
              WORK(ISEC) = P_DSUM(FLXC(ISEC))
              WORKB(ISEC)= P_DSUM(FLXS(ISEC))
            ENDDO
            IF(IPID.EQ.0) THEN
              WRITE (NSEO,FMT=FMTZON) TPS,(WORK(ISEC),ISEC=1,NSEC),
     &                                    (WORKB(ISEC), ISEC=1,NSEC)
            ENDIF
          ELSE
            WRITE (NSEO,FMT=FMTZON) TPS,(FLXC(ISEC),ISEC=1,NSEC),
     &                                  (FLXS(ISEC),ISEC=1,NSEC)
          ENDIF
        ENDIF
!
      ENDIF
!
!     IF(INFO)...
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE FLUXPR_SISYPHE
