C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       END OF MPI.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IER
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> MPI_BARRIER, MPI_COMM_WORLD, MPI_FINALIZE
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BIEF_CLOSE_FILES(), PLANTE()

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
!>    <td><center> 6.0                                    </center></td>
!>    <td> 16/06/2009                                              </td>
!>    <td> J-M HERVOUET (LNHE)                                     </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 28/12/1999                                              </td>
!>    <td> J.A. JANKOWSKI (BAW KARLSRUHE)                          </td>
!>    <td> RELEASE 5.0 MODIFIED                                    </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> **/10/1999                                              </td>
!>    <td> RAINER JOHANNI (SGI MUNICH)                             </td>
!>    <td> ADAPTED FOR MPI                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE P_EXIT
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INCLUDE 'mpif.h'
C
      INTEGER IER
C
C-----------------------------------------------------------------------
C
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) WRITE(LU,*) 'SORTIE DE MPI'
      IF(LNG.EQ.2) WRITE(LU,*) 'EXITING MPI'
      WRITE(LU,*) ' '
C
C     AVOIDS EXITING BEFORE EVERYTHING IS DONE IN OTHER PROCESSORS
C
      CALL MPI_BARRIER(MPI_COMM_WORLD,IER)
C
C     EXITS
C
      CALL MPI_FINALIZE(IER)
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_EXIT: ERREUR DANS MPI_FINALIZE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_EXIT: ERROR IN MPI_FINALIZE'
        WRITE(LU,*) 'MPI ERROR ',IER
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