C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE LOCAL SIDEREAL TIME IN RADIAN FOR
!>                THE GIVEN DATE IN UNIVERSAL TIME.
!>  @code
!>     IN THIS FUNCTION, THE COMPUTATION TIME MUST BE SPLIT IN:
!>
!>     - TIME UNTIL 0:00 OF THE SAME DAY,
!>     - TIME LEFT UNTIL THE PRECISE DATE IN SECONDS.
!>
!>     THIS REQUIRES A COMBINATION BETWEEN THE STARTING DATE
!>     AND THE COMPUTING TIME AT:
!>
!>     COMPUTES THE TIME IN JULIAN CENTURY AT 0:00 OF THE SAME DAY
!>
!>     AT1 : NUMBER OF DAYS IN SECONDS TO ADD TO THE REFERENCE
!>           DATE (YEAR, MONTH, DAY) TO GET TO THE COMPUTATION
!>           DATE AT TIME (0H, 0MIN, 0SEC).
!>     ATR : NUMBER OF SECONDS BETWEEN THE DATE REPRESENTED BY AT1
!>           AND THE EXACT COMPUTATION DATE. ATR ACTUALLY CORRESPONDS
!>           TO TU EXPRESSED IN SECONDS.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DAY, HOUR, MINUTE, MONTH, SEC, YEAR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AT1, ATR, T, TETA, TETA0, UT
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_TSLOC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> JULTIM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MARAST()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 02/06/08
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
!>          <tr><td>MINUTE
!></td><td>---</td><td>
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
                        DOUBLE PRECISION FUNCTION TSLOC
     & (YEAR,MONTH,DAY,HOUR,MINUTE,SEC,AT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| DAY            |-->| JOUR.
C| HOUR           |-->| HEURE EN TEMPS UNIVERSEL.
C| MIN            |-->| MINUTE EN TEMPS UNIVERSEL.
C| MINUTE         |---| 
C| MONTH          |-->| MOIS.
C| SEC            |-->| SECONDE EN TEMPS UNIVERSEL.
C| YEAR           |-->| ANNEE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_TSLOC => TSLOC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: MONTH,DAY,HOUR,MINUTE,SEC
      INTEGER, INTENT(INOUT)       :: YEAR
      DOUBLE PRECISION, INTENT(IN) :: AT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION T,TETA,TETA0,UT
      DOUBLE PRECISION AT1,ATR
C
      INTRINSIC ACOS,INT
C
C-----------------------------------------------------------------------
C
      ATR = AT + ( HOUR * 60.D0 + MINUTE ) * 60.D0 + SEC
      AT1 = INT ( ATR / ( 24.D0 * 3600.D0 ) ) * ( 24.D0 * 3600.D0 )
      ATR = ATR - AT1
      T = JULTIM(YEAR,MONTH,DAY,0,0,0,AT1)
C
C COMPUTES THE SIDEREAL TIME WRT GREENWICH AT 0:00 (IN HOURS)
C
      TETA0 = 6.6460656D0 + 2400.051262D0 * T + 0.00002581D0 * T**2
C
C COMPUTES THE SIDEREAL TIME WRT GREENWICH AT TU
C
      UT = ATR / 3600.D0
      TETA = TETA0 + UT*1.002737908D0
C
C COMPUTES THE LOCAL SIDEREAL TIME IN RADIANS WITHOUT TAKING LONGITUDE
C INTO ACCOUNT
C
      TSLOC = TETA * ACOS(-1.D0) / 12.D0
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C