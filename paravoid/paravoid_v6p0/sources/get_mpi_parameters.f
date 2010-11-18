C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> P_COMM_WORLD, P_INTEGER, P_REAL8, P_SUCCESS, P_UB
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ECRSPE(), STREAMLINE(), STREAMLINE_TOMAWAC(), TOMAWAC_MPI()

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
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>P_COMM_WORLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>P_INTEGER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>P_REAL8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>P_SUCCESS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>P_UB
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GET_MPI_PARAMETERS
     &(P_INTEGER,P_REAL8,P_UB,P_COMM_WORLD,P_SUCCESS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| P_COMM_WORLD   |---| 
C| P_INTEGER      |---| 
C| P_REAL8        |---| 
C| P_SUCCESS      |---| 
C| P_UB           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(OUT) :: P_INTEGER,P_REAL8,P_UB
      INTEGER, INTENT(OUT) :: P_COMM_WORLD,P_SUCCESS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     HERE VALUES OF MPI 1, NOT USED ANYWAY
C
      P_INTEGER=28
      P_REAL8=11
      P_UB=16
      P_COMM_WORLD=91
      P_SUCCESS=0
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C