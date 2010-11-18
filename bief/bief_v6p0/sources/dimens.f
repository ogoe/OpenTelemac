C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE DIMENSION OF AN ELEMENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  COULD ALSO CREATE A DATA ARRAY TO SPEED UP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALMESH(), ASSEX3(), MATRIX(), MATVEC(), OM(), OSBD(), OSDB(), OSDBIF(), VECTOR()

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
!> </td><td> 13/02/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        INTEGER FUNCTION DIMENS
     &( IELM )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE D'ELEMENT.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: IELM
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      IF(IELM.EQ.0 .OR.
     &   IELM.EQ.1 .OR.
     &   IELM.EQ.2) THEN
C
        DIMENS = 1
C
      ELSEIF(IELM.EQ.10.OR.
     &       IELM.EQ.11.OR.
     &       IELM.EQ.12.OR.
     &       IELM.EQ.13.OR.
     &       IELM.EQ.14.OR.
     &       IELM.EQ.70.OR.
     &       IELM.EQ.71.OR.
     &       IELM.EQ.80.OR.
     &       IELM.EQ.81.OR.
     &       IELM.EQ.61.OR.
     &       IELM.EQ.60.OR.
     &       IELM.EQ.20.OR.
     &       IELM.EQ.21) THEN
C
        DIMENS = 2
C
      ELSEIF(IELM.EQ.30.OR.
     &       IELM.EQ.31.OR.
     &       IELM.EQ.40.OR.
     &       IELM.EQ.41.OR.
     &       IELM.EQ.50.OR.
     &       IELM.EQ.51    ) THEN
C
        DIMENS = 3
C
      ELSE
        IF(LNG.EQ.1) WRITE(LU,100) IELM
        IF(LNG.EQ.2) WRITE(LU,101) IELM
100     FORMAT(1X,'DIMENS (BIEF) : ',1I6,' ELEMENT NON PREVU')
101     FORMAT(1X,'DIMENS (BIEF) : ',1I6,' ELEMENT NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C