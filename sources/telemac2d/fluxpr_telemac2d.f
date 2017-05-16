!                    ***************************
                     SUBROUTINE FLUXPR_TELEMAC2D
!                    ***************************
!
     &(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,NSEG,NCSIZE,CUMFLO)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES FLUXES THROUGH CONTROL SECTIONS
!+                AND SUMS THESE UP TO EVALUATE OSCILLATING VOLUMES.
!
!note     PRINTOUTS OF DISCHARGES THROUGH CONTROL SECTIONS ARE DONE
!+            IN THIS ROUTINE. YOU CAN REWRITE IT TO DIVERT THESE
!+            PRINTOUTS TO A FILE OR TO CHANGE THE FORMAT
!
!history  J-M HERVOUET (LNHE)
!+        25/03/1999
!+        V5P5
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CTRLSC         |-->| NUMBERS OF POINTS IN THE CONTROL SECTIONS
!| CUMFLO         |-->| KEYWORD: PRINTING CUMULATED FLOWRATES
!| FLX            |-->| FLUXES THROUGH CONTROL SECTIONS
!| INFO           |-->| IF YES : INFORMATION IS PRINTED
!| NCSIZE         |-->| NUMBER OF PROCESSORS
!| NSEC           |-->| NUMBER OF CONTROL SECTIONS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| TPS            |-->| TIME IN SECONDS
!| VOLNEG         |-->| CUMULATED NEGATIVE VOLUME THROUGH SECTIONS
!| VOLPOS         |-->| CUMULATED POSITIVE VOLUME THROUGH SECTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: IPID
      USE DECLARATIONS_TELEMAC2D, ONLY: T2D_FILES,T2DSEO,CHAIN,TITCAS,
     &                                  WORK_FPR, OLD_METHOD_FPR,
     &                                  INIT_FPR, NSEO_FPR
      USE DECLARATIONS_SPECIAL
!##> JR @ RWTH: ALLOW COMPILERS TO CHECK PARALLEL INTERFACE
      USE INTERFACE_PARALLEL, ONLY : P_DMAX,P_DMIN,P_DSUM,P_IMIN
!##< JR @ RWTH
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NSEC,NCSIZE
      INTEGER, INTENT(IN)          :: CTRLSC(*)
      INTEGER, INTENT(IN)          :: NSEG(NSEC)
      LOGICAL, INTENT(IN)          :: INFO,CUMFLO
      DOUBLE PRECISION, INTENT(IN) :: FLX(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEG(NSEC),VOLPOS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: TPS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!##> JR @ RWTH: INTERFACE CHECKED SO NO NEED FOR EXTERNALS
!      DOUBLE PRECISION P_DMAX,P_DMIN, P_DSUM
!      INTEGER                        P_IMIN
!      EXTERNAL         P_DMAX,P_DMIN,P_DSUM,P_IMIN
!##< JR @ RWTH
!
      INTEGER ISEC,II,ERR
      CHARACTER(LEN=16), PARAMETER :: FMTZON='(4(1X,1PG21.14))'
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
      DOUBLE PRECISION :: DTMP1,DTMP2,DTMP3,DTMP4
!< JR @ RWTH
!
!-----------------------------------------------------------------------
!
      IF (.NOT.ALLOCATED(CHAIN)) OLD_METHOD_FPR=.TRUE.
!
      IF(INFO) THEN
!
      IF (OLD_METHOD_FPR) THEN ! FOLLOW FLUXPR.F OF BIEF BLINDLY
!
      IF(NCSIZE.LE.1) THEN
!
      DO ISEC = 1,NSEC
!
      IF(CUMFLO) THEN
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
      ELSE
      IF(LNG.EQ.1) WRITE(LU,136) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &                                FLX(ISEC)
      IF(LNG.EQ.2) WRITE(LU,137) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &                                FLX(ISEC)
      ENDIF
130   FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (ENTRE LES POINTS ',1I5,' ET ',1I5,')',//,5X,
     &               'DEBIT : '                    ,G16.7,/,5X,
     &               'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &               'CUMUL DES DEBITS POSITIFS : ',G16.7)
131   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7,/,5X,
     &               'NEGATIVE VOLUME THROUGH THE SECTION: ',G16.7,/,5X,
     &               'POSITIVE VOLUME THROUGH THE SECTION: ',G16.7)
136   FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (ENTRE LES POINTS ',1I5,' ET ',1I5,')',//,5X,
     &               'DEBIT : '                    ,G16.7)
137   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7)
!
      ENDDO
!
      ELSE
!
      DO ISEC = 1,NSEC
!     SECTIONS ACROSS 2 SUB-DOMAINS WILL HAVE NSEG=0 OR -1
!     AND -1 WANTED HERE FOR RELEVANT MESSAGE.
      II=P_IMIN(NSEG(ISEC))
!
      IF(II.GE.0) THEN
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
            DTMP1 = P_DMIN(FLX(ISEC))
            DTMP2 = P_DMAX(FLX(ISEC))
            DTMP3 = P_DMIN(VOLNEG(ISEC))
            DTMP4 = P_DMAX(VOLPOS(ISEC))
!< JR @ RWTH
!
      IF(LNG.EQ.1) WRITE(LU,132) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &                                DTMP1+DTMP2,DTMP3,DTMP4
      IF(LNG.EQ.2) WRITE(LU,133) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &                                DTMP1+DTMP2,DTMP3,DTMP4
132   FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (ENTRE LES POINTS ',1I5,' ET ',1I5,')',//,5X,
     &               'DEBIT : '                    ,G16.7,/,5X,
     &               'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &               'CUMUL DES DEBITS POSITIFS : ',G16.7)
133   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7,/,5X,
     &               'NEGATIVE VOLUME THROUGH THE SECTION: ',G16.7,/,5X,
     &               'POSITIVE VOLUME THROUGH THE SECTION: ',G16.7)
!
      ELSE
!
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
!
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
! CHAIN ALLOCATED, I.E. SERIAL OR PARALLEL CASE FROM SECTIONS INPUT FILE
!       WE CAN APPLY CO-ORDINATES INSTEAD AND/OR NAMES OF SECTIONS
!
      ELSE
        IF(NCSIZE.LE.1) THEN ! SERIAL
          DO ISEC = 1,NSEC
            IF(CUMFLO) THEN
              IF(LNG.EQ.1) WRITE(LU,230) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                           FLX(ISEC),VOLNEG(ISEC),VOLPOS(ISEC)
              IF(LNG.EQ.2) WRITE(LU,231) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                           FLX(ISEC),VOLNEG(ISEC),VOLPOS(ISEC)
            ELSE
              IF(LNG.EQ.1) WRITE(LU,236) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                           FLX(ISEC)
              IF(LNG.EQ.2) WRITE(LU,237) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                           FLX(ISEC)
            ENDIF
230   FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (NOM ',A,')',//,5X,
     &               'DEBIT : '                    ,G16.7,/,5X,
     &               'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &               'CUMUL DES DEBITS POSITIFS : ',G16.7)
231   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (NAME ',A,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7,/,5X,
     &               'NEGATIVE VOLUME THROUGH THE SECTION: ',G16.7,/,5X,
     &               'POSITIVE VOLUME THROUGH THE SECTION: ',G16.7)
236   FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (NOM ',A,')',//,5X,
     &               'DEBIT : '                    ,G16.7)
237   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (NAME ',A,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7)
          ENDDO
!
        ELSE
!
          DO ISEC = 1,NSEC
!
!> JR @ RWTH: ALGORITHMIC DIFFERENTIATION
            DTMP1 = P_DSUM(FLX(ISEC))
            DTMP2 = P_DSUM(VOLNEG(ISEC))
            DTMP3 = P_DSUM(VOLPOS(ISEC))
!< JR @ RWTH
!
            IF(LNG.EQ.1) WRITE(LU,232) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                                 DTMP1,DTMP2,DTMP3
            IF(LNG.EQ.2) WRITE(LU,233) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                                 DTMP1,DTMP2,DTMP3
232         FORMAT(1X,/,1X,'SECTION DE CONTROLE ',1I2,
     &               ' (NOM ',A,')',//,5X,
     &               'DEBIT : '                    ,G16.7,/,5X,
     &               'CUMUL DES DEBITS NEGATIFS : ',G16.7,/,5X,
     &               'CUMUL DES DEBITS POSITIFS : ',G16.7)
233         FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (NAME ',A,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7,/,5X,
     &               'NEGATIVE VOLUME THROUGH THE SECTION: ',G16.7,/,5X,
     &               'POSITIVE VOLUME THROUGH THE SECTION: ',G16.7)
!
          ENDDO
        ENDIF
!
      ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! MASTER WRITES A NICE SECTIONS OUTPUT FILE, THE HEADER ONLY ONCE
!
      IF ( (.NOT.OLD_METHOD_FPR) .AND.
     &      (TRIM(T2D_FILES(T2DSEO)%NAME).NE.'') ) THEN
        IF (INIT_FPR) THEN
          INIT_FPR=.FALSE.
          IF ((NCSIZE.GT.1 .AND. IPID.EQ.0).OR.(NCSIZE.LE.1)) THEN
            NSEO_FPR=T2D_FILES(T2DSEO)%LU
            IF(LNG.EQ.1) THEN
              WRITE(NSEO_FPR,*) 'TITRE = "FLUX POUR ',TRIM(TITCAS),'"'
            ELSEIF(LNG.EQ.2) THEN
              WRITE(NSEO_FPR,*) 'TITLE = "FLUXES FOR ',TRIM(TITCAS),'"'
            ENDIF
            WRITE(NSEO_FPR,*) 'VARIABLES = TIME',
     &         (' '//TRIM(CHAIN(ISEC)%DESCR),ISEC=1,NSEC)
          ENDIF
          IF (NCSIZE.GT.1) THEN
            ALLOCATE (WORK_FPR(NSEC), STAT=ERR)
            IF (ERR.NE.0) THEN
              WRITE(LU,*)
     &          'FLUXPR_TELEMAC2D: ERROR ALLOCATING WORK_FPR:',ERR
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDIF
        ! DEADLOCK WITH WRITE AND P_DSUM IN AN IMPLIED WRITE LOOP
        ! BECAUSE IT IS ONLY MASTER TO WRITE THE MESSAGE...
        IF (NCSIZE.GT.1) THEN
          DO ISEC=1,NSEC
            WORK_FPR(ISEC) = P_DSUM(FLX(ISEC))
          END DO
          IF (IPID.EQ.0)
     &      WRITE (NSEO_FPR, FMT=FMTZON) TPS,
     &                     (WORK_FPR(ISEC), ISEC=1,NSEC)
        ELSE
          WRITE (NSEO_FPR, FMT=FMTZON) TPS, (FLX(ISEC), ISEC=1,NSEC)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
