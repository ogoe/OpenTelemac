C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TIME ELAPSED SINCE 31/12/1899.
!>                EXPRESSES IT IN JULIAN CENTURIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DAY, HOUR, MIN, MONTH, SEC, YEAR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> GREG, J, M, Y, YEAR2
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ASTRO(), TSLOC()

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
!> </td><td> 03/09/2010
!> </td><td> JMH (EDF-LNHE)
!> </td><td> For consistency, YEAR is now INTENT(IN) only
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 12/07/1995
!> </td><td> E. DAVID (LHF) 76 33 42 08
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>DAY
!></td><td>--></td><td>JOUR.
!>    </td></tr>
!>          <tr><td>HOUR
!></td><td>--></td><td>HEURE EN TEMPS UNIVERSEL.
!>    </td></tr>
!>          <tr><td>MIN
!></td><td>--></td><td>MINUTE EN TEMPS UNIVERSEL.
!>    </td></tr>
!>          <tr><td>MONTH
!></td><td>--></td><td>MOIS.
!>    </td></tr>
!>          <tr><td>SEC
!></td><td>--></td><td>SECONDE EN TEMPS UNIVERSEL.
!>    </td></tr>
!>          <tr><td>YEAR
!></td><td>--></td><td>ANNEE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
              DOUBLE PRECISION FUNCTION JULTIM
     &(YEAR,MONTH,DAY,HOUR,MIN,SEC,AT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| DAY            |-->| JOUR.
C| HOUR           |-->| HEURE EN TEMPS UNIVERSEL.
C| MIN            |-->| MINUTE EN TEMPS UNIVERSEL.
C| MONTH          |-->| MOIS.
C| SEC            |-->| SECONDE EN TEMPS UNIVERSEL.
C| YEAR           |-->| ANNEE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN) :: MONTH,DAY,HOUR,MIN,SEC,YEAR
      DOUBLE PRECISION, INTENT(IN) :: AT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER GREG,Y,M,YEAR2
      DOUBLE PRECISION J
C
      INTRINSIC INT
C
      PARAMETER (GREG=15+31*(10+12*1582))
C
C-----------------------------------------------------------------------
C
      YEAR2=YEAR
      IF(YEAR2.EQ.0) THEN
        IF (LNG.EQ.1) WRITE (LU,100)
        IF(LNG.EQ.2)  WRITE (LU,101)
        STOP
      ENDIF
      IF(YEAR2.LT.0) YEAR2=YEAR2+1
C
      IF (MONTH.GT.2) THEN
       Y=YEAR2
       M=MONTH+1
      ELSE
       Y=YEAR2-1
       M=MONTH+13
      ENDIF
C
      J=INT(365.25D0*Y)+INT(30.6001D0*M)+DAY+1720995.D0
      IF(DAY+31*(MONTH+12*YEAR2).GE.GREG) THEN
        J=J+2-INT(0.01D0*Y)+INT(0.25D0*INT(0.01D0*Y))
      ENDIF
      J=J-2415020.5D0
      JULTIM=(J+(HOUR+(MIN+(SEC+AT)/60.D0)/60.D0)/24.D0)/36525.D0
C
C---------------------------------------------------------------
C
100   FORMAT (//,10X,'**********************************',
     &         /,10X,'       FONCTION JULTIM',
     &         /,10X,' LA VALEUR DE L''ANNEE EST NULLE',
     &         /,10X,' CALCUL IMPOSSIBLE ...'
     &         /,10X,'**********************************')
101   FORMAT (//,10X,'**********************************',
     &         /,10X,'       JULTIM FUNCTION',
     &         /,10X,' THE VALUE FOR THE YEAR IS ZERO',
     &         /,10X,' COMPUTATION NOT POSSIBLE ...'
     &         /,10X,'**********************************')
C
C---------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C