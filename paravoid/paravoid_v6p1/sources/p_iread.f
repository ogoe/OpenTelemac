!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @brief
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BUFFER, IREQ, ITAG, NBYTES, SOURCE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IER
!>   </td></tr>
!>     </table>
!
!>  @par Called by
!><br>PARACO(), TOMAWAC_MPI_TOOLS()
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
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
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BUFFER
!></td><td>--></td><td>ZONE TAMPON POUR LES DONNEES
!>                  BUFFER / PUFFERFELD
!>    </td></tr>
!>          <tr><td>IREQ
!></td><td>--></td><td>NUMERO DE REQUEST POUR MPI_IRECV
!>    </td></tr>
!>          <tr><td>ITAG
!></td><td>--></td><td>MESSAGE TAG
!>    </td></tr>
!>          <tr><td>NBYTES
!></td><td>--></td><td>NOMBRE DE BYTES A TRANSMETTRE
!>                  LENGTH IN BYTES / LAENGE IN BYTES
!>    </td></tr>
!>          <tr><td>SOURCE
!></td><td>--></td><td>ORIGINE DES DONNEES
!>                  TID OF THE SENDER / KNOTEN-ID DES SENDER
!>    </td></tr>
!>     </table>
!
!#######################################################################
!
                        SUBROUTINE P_IREAD
     &(BUFFER,NBYTES,SOURCE,ITAG,IREQ)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BUFFER         |-->| ZONE TAMPON POUR LES DONNEES
!|                |   | BUFFER / PUFFERFELD
!| IREQ           |-->| NUMERO DE REQUEST POUR MPI_IRECV
!| ITAG           |-->| MESSAGE TAG
!| NBYTES         |-->| NOMBRE DE BYTES A TRANSMETTRE
!|                |   | LENGTH IN BYTES / LAENGE IN BYTES
!| SOURCE         |-->| ORIGINE DES DONNEES
!|                |   | TID OF THE SENDER / KNOTEN-ID DES SENDER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NBYTES,SOURCE,ITAG,IREQ,IER
      DOUBLE PRECISION BUFFER(*)
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_IREAD VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_IREAD IN ITS VOID VERSION'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!#######################################################################
!