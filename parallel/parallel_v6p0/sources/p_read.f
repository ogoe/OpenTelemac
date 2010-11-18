C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       RECEIVES DATA.

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
!>     <tr><th> Internal(s)
!>    </th><td> IER, STATUS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> MPI_BYTE, MPI_COMM_WORLD, MPI_RECV
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
!>    <td> 28/12/1999                                              </td>
!>    <td> J.A. JANKOWSKI (BAW KARLSRUHE)                          </td>
!>    <td> RELEASE 5.0 MODIFIED                                    </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> **/10/1999                                              </td>
!>    <td> RAINER JOHANNI (SGI MUNICH)                             </td>
!>    <td> ADAPTED FOR MPI                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 08/01/1997                                              </td>
!>    <td> HANS HERRMANN (HANOVRE)                                 </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 17/12/1996                                              </td>
!>    <td> J-M MERVOUET (LNH)                                      </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 08/06/1996                                              </td>
!>    <td> REINHARD HINKELMANN (HANOVER)                           </td>
!>    <td> MODIFIED                                                </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BUFFER
!></td><td>--></td><td>ZONE TAMPON POUR LES DONNEES
!>                  BUFFER / PUFFERFELD
!>    </td></tr>
!>          <tr><td>NBYTES
!></td><td>--></td><td>NOMBRE DE BYTES A TRANSMETTRE
!>                  LENGTH IN BYTES / LAENGE IN BYTES
!>    </td></tr>
!>          <tr><td>SOURCE
!></td><td>--></td><td>ORIGINE DES DONNEES
!>                  TID OF THE SENDER / KNOTEN-ID DES SENDER
!>    </td></tr>
!>          <tr><td>TYPE
!></td><td>--></td><td>TYPE DES DONNEES (MSGTAG DE PVM)
!>                  0 - STRING
!>                  1 - BYTE1
!>                  2 - INTEGER2
!>                  3 - INTEGER4
!>                  4 - REAL4
!>                  5 - COMPLEX8
!>                  6 - REAL8
!>                  7 - COMPLEX16
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE P_READ
     &(BUFFER,NBYTES,SOURCE,TYPE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BUFFER         |-->| ZONE TAMPON POUR LES DONNEES
C|                |   | BUFFER / PUFFERFELD
C| NBYTES         |-->| NOMBRE DE BYTES A TRANSMETTRE
C|                |   | LENGTH IN BYTES / LAENGE IN BYTES
C| SOURCE         |-->| ORIGINE DES DONNEES
C|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
C| TYPE           |-->| TYPE DES DONNEES (MSGTAG DE PVM)
C|                |   | 0 - STRING
C|                |   | 1 - BYTE1
C|                |   | 2 - INTEGER2
C|                |   | 3 - INTEGER4
C|                |   | 4 - REAL4
C|                |   | 5 - COMPLEX8
C|                |   | 6 - REAL8
C|                |   | 7 - COMPLEX16
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INCLUDE 'mpif.h'
C
      INTEGER NBYTES,SOURCE,TYPE,STATUS(MPI_STATUS_SIZE),IER
      DOUBLE PRECISION BUFFER(*)
C
C-----------------------------------------------------------------------
C RECEIVES DATA
C
      CALL MPI_RECV(BUFFER,NBYTES,MPI_BYTE,SOURCE,4711,
     &              MPI_COMM_WORLD,STATUS,IER)
C
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_READ: ERREUR IN MPI_RECV'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_READ: ERROR IN MPI_RECV'
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