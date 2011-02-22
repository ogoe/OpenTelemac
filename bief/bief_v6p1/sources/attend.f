C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WAITS FOR A PERIOD OF TIME: ISEC IN SECONDS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  PLEASE ADD OR REMOVE COMMENTS ACCORDING TO YOUR COMPILER

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ISEC
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> T1, T2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> TIME_IN_SECONDS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAITFOR()

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
!> </td><td> 08/02/2001
!> </td><td> NATHALY BARBRY (UNIVERSITE DE CAEN); J-M HERVOUET (LNHE) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>D
!></td><td>--></td><td>DUREE
!>    </td></tr>
!>          <tr><td>ISEC
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ATTEND(ISEC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| D             |-->| DUREE
C| ISEC           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C-----------------------------------------------------------------------
C     VERSION F90 STANDARD
C-----------------------------------------------------------------------
C
      INTEGER, INTENT(IN) :: ISEC
      INTEGER T1,T2
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
      T1 = TIME_IN_SECONDS()
      T2 = T1
C                            .AND. : WHEN CLOCK RESET TO ZERO
      DO WHILE (T2.LT.T1+ISEC.AND.T2.GE.T1)
        T2 = TIME_IN_SECONDS()
      END DO
C
C-----------------------------------------------------------------------
C     VERSION F95 NAG
C-----------------------------------------------------------------------
C
C     USE F90_UNIX_PROC
C     INTEGER, INTENT(IN) :: ISEC
C     CALL SLEEP(ISEC)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C