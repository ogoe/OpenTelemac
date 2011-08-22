!                    *****************
                     SUBROUTINE FLUXPR
!                    *****************
!
     &(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,NSEG,NCSIZE,CUMFLO)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES FLUXES THROUGH CONTROL SECTIONS
!+                AND SUMS THEM UP TO OBTAIN OSCILLATING VOLUMES.
!
!note     PRINTOUTS OF DISCHARGES THROUGH CONTROL SECTIONS ARE DONE
!+            IN THIS ROUTINE. YOU CAN REWRITE IT TO DIVERT THESE
!+            PRINTOUTS TO A FILE OR TO CHANGE THE FORMAT
!
!history  J-M HERVOUET (LNH)
!+        25/03/99
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
!| CUMFLO         |-->| KEY-WORD: PRINTING CUMULATED FLOWRATES
!| FLX            |-->| FLUXES THROUGH CONTROL SECTIONS
!| INFO           |-->| IF YES : INFORMATION IS PRINTED
!| NCSIZE         |-->| NUMBER OF PROCESSORS
!| NSEC           |-->| NUMBER OF CONTROL SECTIONS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| TPS            |-->| TIME
!| VOLNEG         |-->| CUMULATED NEGATIVE VOLUME THROUGH SECTIONS
!| VOLPOS         |-->| CUMULATED POSITIVE VOLUME THROUGH SECTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NSEC,NCSIZE
      INTEGER, INTENT(IN)          :: CTRLSC(*)
      INTEGER, INTENT(IN)          :: NSEG(NSEC)
      LOGICAL, INTENT(IN)          :: INFO,CUMFLO
      DOUBLE PRECISION, INTENT(IN) :: FLX(NSEC),TPS
      DOUBLE PRECISION, INTENT(IN) :: VOLNEG(NSEC),VOLPOS(NSEC)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      INTEGER                        P_IMIN
      EXTERNAL         P_DMAX,P_DMIN,P_IMIN
!
      INTEGER ISEC,II
!
!-----------------------------------------------------------------------
!
      IF(INFO) THEN
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
!     AND -1 WANTED HERE FOR RELEVANT MESSAGE
      II=P_IMIN(NSEG(ISEC))
!
      IF(II.GE.0) THEN
!
      IF(LNG.EQ.1) WRITE(LU,132) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &              P_DMIN(FLX(ISEC))+P_DMAX(FLX(ISEC)),
     &                                P_DMIN(VOLNEG(ISEC)),
     &                                P_DMAX(VOLPOS(ISEC))
      IF(LNG.EQ.2) WRITE(LU,133) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1)),
     &              P_DMIN(FLX(ISEC))+P_DMAX(FLX(ISEC)),
     &                                P_DMIN(VOLNEG(ISEC)),
     &                                P_DMAX(VOLPOS(ISEC))
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
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
