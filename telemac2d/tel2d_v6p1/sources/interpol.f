C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES RO = -B/2A, MINIMUM OF THE FUNCTION :
!>                A * (RO**2) + B * RO +C.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> JCOUT1, JCOUT2, JCOUT3, R02, R03, RO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEFA, COEFB, COEFC, ROMAX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 05/10/2000
!> </td><td> A. LEOPARDI (UNINA)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 27/04/1993
!> </td><td> E. BARROS
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>JCOUT1
!></td><td>--></td><td>J(RO)
!>    </td></tr>
!>          <tr><td>JCOUT2
!></td><td>--></td><td>J(R02)
!>    </td></tr>
!>          <tr><td>JCOUT3
!></td><td>--></td><td>J(R03)
!>    </td></tr>
!>          <tr><td>R02
!></td><td>--></td><td>COEFFICIENT
!>    </td></tr>
!>          <tr><td>R03
!></td><td>--></td><td>COEFFICIENT
!>    </td></tr>
!>          <tr><td>RO
!></td><td><--</td><td>COEFFICIENT OF DESC
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INTERPOL
     &(RO,R02,R03,JCOUT1,JCOUT2,JCOUT3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| JCOUT1         |-->| J(RO)
C| JCOUT2         |-->| J(R02)
C| JCOUT3         |-->| J(R03)
C| R02            |-->| COEFFICIENT
C| R03            |-->| COEFFICIENT
C| RO             |<--| COEFFICIENT OF DESC
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION , INTENT(IN)    :: R02,R03,JCOUT1,JCOUT2,JCOUT3
      DOUBLE PRECISION , INTENT(INOUT) :: RO
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION COEFA,COEFB,COEFC
      DOUBLE PRECISION ROMAX
C
      INTRINSIC ABS
C
C-----------------------------------------------------------------------
C
      COEFA = ((JCOUT1*(R02-R03))+(R03*JCOUT2)-(JCOUT3*R02))/(R02*R03
     &     *(R02-R03))
C
      COEFB = ((-JCOUT1*((R02*R02)-(R03*R03)))-(JCOUT2*R03*R03)
     &     + (JCOUT3*R02*R02))/(R02*R03*(R02-R03))
C
      COEFC = JCOUT1
C
      IF(COEFA.LE.0.D0) THEN
        WRITE(LU,*) 'INTERPOL : COEFFICIENT A LESS THAN ZERO:',COEFA
        CALL PLANTE(1)
        STOP
      ENDIF
C
      RO = - COEFB / (2.D0 * COEFA)
C
C     CAPS THE VALUE OF RO :
C
      IF(ABS(R02).GE.ABS(R03)) THEN
        ROMAX = 2.D0*R02
      ELSEIF(ABS(R03).GE.ABS(R02)) THEN
        ROMAX = 2.D0*R03
      ENDIF
C
      IF(ABS(RO).GT.ABS(ROMAX)) THEN
        WRITE(LU,*) 'INTERPOL : LIMIT VALUE OF RHO'
        RO = ROMAX
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C