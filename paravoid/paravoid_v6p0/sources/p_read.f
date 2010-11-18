C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BUFFER, NBYTES, SOURCE, TYPE
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
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 17/12/1996                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 08/06/1996                                              </td>
!>    <td> REINHARD HINKELMANN (HANOVER)                           </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center> 5.6                                    </center></td>
!>    <td> **/06/1996                                              </td>
!>    <td> HANS HERRMANN (HANOVER)                                 </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BUFFER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBYTES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SOURCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TYPE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE P_READ
     &(BUFFER,NBYTES,SOURCE,TYPE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BUFFER         |---| 
C| NBYTES         |---| 
C| SOURCE         |---| 
C| TYPE           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NBYTES, SOURCE, TYPE
      DOUBLE PRECISION BUFFER(*)
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_READ VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_READ IN ITS VOID VERSION'
C
C-----------------------------------------------------------------------
C
      STOP
      END

C
C#######################################################################
C