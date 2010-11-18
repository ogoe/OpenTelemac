C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FLUXES THROUGH CONTROL SECTIONS
!>                AND SUMS THEM UP TO OBTAIN OSCILLATING VOLUMES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     PRINTOUTS OF DISCHARGES THROUGH CONTROL SECTIONS ARE DONE
!>            IN THIS ROUTINE. YOU CAN REWRITE IT TO DIVERT THESE
!>            PRINTOUTS TO A FILE OR TO CHANGE THE FORMAT

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CTRLSC, CUMFLO, FLX, INFO, NCSIZE, NSEC, NSEG, TPS, VOLNEG, VOLPOS
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> II, ISEC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> P_DMAX(), P_DMIN(), P_IMIN()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUSEC()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 25/03/99
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CTRLSC
!></td><td>--></td><td>NUMBERS OF POINTS IN THE CONTROL SECTIONS
!>    </td></tr>
!>          <tr><td>CUMFLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLX
!></td><td>--></td><td>FLUXES THROUGH CONTROL SECTIONS
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
!>          <tr><td>TPS
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>VOLNEG
!></td><td>--></td><td>CUMULATED NEGATIVE VOLUME THROUGH SECTIONS
!>    </td></tr>
!>          <tr><td>VOLPOS
!></td><td>--></td><td>CUMULATED POSITIVE VOLUME THROUGH SECTIONS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUXPR
     &(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,NSEG,NCSIZE,CUMFLO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CTRLSC         |-->| NUMBERS OF POINTS IN THE CONTROL SECTIONS
C| CUMFLO         |---| 
C| FLX            |-->| FLUXES THROUGH CONTROL SECTIONS
C| INFO           |-->| IF YES : INFORMATION IS PRINTED
C| NCSIZE         |---| 
C| NSEC           |-->| NUMBER OF CONTROL SECTIONS
C| NSEG           |---| 
C| TPS            |-->| TEMPS
C| VOLNEG         |-->| CUMULATED NEGATIVE VOLUME THROUGH SECTIONS
C| VOLPOS         |-->| CUMULATED POSITIVE VOLUME THROUGH SECTIONS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: NSEC,NCSIZE
      INTEGER, INTENT(IN)          :: CTRLSC(*)
      INTEGER, INTENT(IN)          :: NSEG(NSEC)
      LOGICAL, INTENT(IN)          :: INFO,CUMFLO
      DOUBLE PRECISION, INTENT(IN) :: FLX(NSEC),TPS
      DOUBLE PRECISION, INTENT(IN) :: VOLNEG(NSEC),VOLPOS(NSEC)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION P_DMAX,P_DMIN
      INTEGER                        P_IMIN
      EXTERNAL         P_DMAX,P_DMIN,P_IMIN
C
      INTEGER ISEC,II
C
C-----------------------------------------------------------------------
C
      IF(INFO) THEN
C
      IF(NCSIZE.LE.1) THEN
C
      DO ISEC = 1,NSEC
C
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
      ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C