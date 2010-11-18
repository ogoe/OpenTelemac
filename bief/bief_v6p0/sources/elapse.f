C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRINTS THE DURATION BETWEEN TWO TIMES GIVEN BY THE
!>                CALL TO DATE_AND_TIME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> TDEB, TFIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DAY, GREG, HOURS, J1, J2, M, MINUTES, MONTH, NDAY, TD, TF, TT, Y, YEAR
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_TELEMAC2D(), HOMERE_TELEMAC3D()

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
!>      <td><center> 5.3                                       </center>
!> </td><td> 20/09/2001
!> </td><td> J-M HERVOUET (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>TDEB,TFIN
!></td><td>--></td><td>TIMES OF BEGINNING AND END
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ELAPSE
     &(TDEB,TFIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| TDEB,TFIN      |-->| TIMES OF BEGINNING AND END
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: TDEB(8),TFIN(8)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER YEAR,MONTH,DAY,NDAY,J1,J2,HOURS,MINUTES
      INTEGER GREG,Y,M
      DOUBLE PRECISION TD,TF,TT
C
      INTRINSIC INT
C
      PARAMETER (GREG=15+31*(10+12*1582))
C
C-----------------------------------------------------------------------
C
C     COMPUTES THE NUMBER OF DAYS (USES THE JULIAN DAY SYSTEM)
C
      YEAR=TDEB(1)
      MONTH=TDEB(2)
      DAY=TDEB(3)
      IF(MONTH.GT.2) THEN
       Y=YEAR
       M=MONTH+1
      ELSE
       Y=YEAR-1
       M=MONTH+13
      ENDIF
C
      J1=INT(365.25D0*Y)+INT(30.6001D0*M)+DAY+1720995
      IF(DAY+31*(MONTH+12*YEAR).GE.GREG) THEN
        J1=J1+2-INT(0.01D0*Y)+INT(0.25D0*INT(0.01D0*Y))
      ENDIF
C
      YEAR=TFIN(1)
      MONTH=TFIN(2)
      DAY=TFIN(3)
      IF(MONTH.GT.2) THEN
       Y=YEAR
       M=MONTH+1
      ELSE
       Y=YEAR-1
       M=MONTH+13
      ENDIF
C
      J2=INT(365.25D0*Y)+INT(30.6001D0*M)+DAY+1720995
      IF(DAY+31*(MONTH+12*YEAR).GE.GREG) THEN
        J2=J2+2-INT(0.01D0*Y)+INT(0.25D0*INT(0.01D0*Y))
      ENDIF
C
      NDAY=J2-J1
C
      TD = 3600.D0*TDEB(5) + 60.D0*TDEB(6) + TDEB(7)
      TF = 3600.D0*TFIN(5) + 60.D0*TFIN(6) + TFIN(7)
      IF(TF.LT.TD) THEN
        NDAY=NDAY-1
        TF=TF+86400.D0
      ENDIF
      TT=TF-TD
      HOURS=INT(TT/3600.D0)
      TT=TT-3600.D0*HOURS
      MINUTES=INT(TT/60.D0)
      TT=TT-60.D0*MINUTES
C
C---------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
C
      WRITE(LU,*)   'DUREE DU CALCUL : '
      IF(NDAY.GT.0) THEN
        WRITE(LU,*) '                  ',NDAY,' JOURS'
      ENDIF
      IF(HOURS.GT.0) THEN
        WRITE(LU,*) '                  ',HOURS,' HEURES'
      ENDIF
      IF(MINUTES.GT.0) THEN
        WRITE(LU,*) '                  ',MINUTES,' MINUTES'
      ENDIF
      WRITE(LU,*)   '                  ',INT(TT),' SECONDES'
C
      ELSE
C
      WRITE(LU,*)   'ELAPSE TIME : '
      IF(NDAY.GT.0) THEN
        WRITE(LU,*) '                  ',NDAY,' DAYS'
      ENDIF
      IF(HOURS.GT.0) THEN
        WRITE(LU,*) '                  ',HOURS,' HOURS'
      ENDIF
      IF(MINUTES.GT.0) THEN
        WRITE(LU,*) '                  ',MINUTES,' MINUTES'
      ENDIF
      WRITE(LU,*)   '                  ',INT(TT),' SECONDS'
C
      ENDIF
C
C---------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C