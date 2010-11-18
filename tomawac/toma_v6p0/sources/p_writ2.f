C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BUFFER, DEST, NBYTES, TYPE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

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
!>      <td><center>                                           </center>
!> </td><td> 17/12/96
!> </td><td> J-M HERVOUET (LNH)
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 08/06/96
!> </td><td> REINHARD HINKELMANN (HANOVER)
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center> 5.0                                       </center>
!> </td><td> **/06/96
!> </td><td> HANS HERRMANN (HANOVER)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BUFFER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBYTES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TYPE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE P_WRIT2
     &(BUFFER, NBYTES, DEST, TYPE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BUFFER         |---| 
C| DEST           |---| 
C| NBYTES         |---| 
C| TYPE           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NBYTES,DEST,TYPE
      INTEGER BUFFER(*)
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_WRIT VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_WRIT IN ITS VOID VERSION'
C
C----------------------------------------------------------------------
C
      STOP
      END
C
C#######################################################################
C