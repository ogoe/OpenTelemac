C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXCHANGES VALUES BETWEEN PROCESSORS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BUFFER, DEST, IREQ, ITAG, NBYTES
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IER
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TOMAWAC_MPI_TOOLS()

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
!>          <tr><td>BUFFER
!></td><td>--></td><td>ZONE TAMPON POUR LES DONNEES
!>                  BUFFER / PUFFERFELD
!>    </td></tr>
!>          <tr><td>DEST
!></td><td>--></td><td>DESTINATION DES DONNEES
!>                  TID OF THE DEST.  / KNOTEN-ID DES EMPFAENGERS
!>    </td></tr>
!>          <tr><td>IREQ
!></td><td>--></td><td>NUMERO DE REQUEST POUR MPI_ISEND
!>    </td></tr>
!>          <tr><td>ITAG
!></td><td>--></td><td>MESSAGE TAG
!>    </td></tr>
!>          <tr><td>NBYTES
!></td><td>--></td><td>NOMBRE DE BYTES A TRANSMETTRE
!>                  LENGTH IN BYTES / LAENGE IN BYTES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE P_IWRIT_C
     &(BUFFER,NBYTES,DEST,ITAG,IREQ)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BUFFER         |-->| ZONE TAMPON POUR LES DONNEES
C|                |   | BUFFER / PUFFERFELD
C| DEST           |-->| DESTINATION DES DONNEES
C|                |   | TID OF THE DEST.  / KNOTEN-ID DES EMPFAENGERS
C| IREQ           |-->| NUMERO DE REQUEST POUR MPI_ISEND
C| ITAG           |-->| MESSAGE TAG
C| NBYTES         |-->| NOMBRE DE BYTES A TRANSMETTRE
C|                |   | LENGTH IN BYTES / LAENGE IN BYTES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NBYTES,DEST,ITAG,IREQ,IER
      CHARACTER(LEN=*) BUFFER
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_IWRIT VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_IWRIT IN ITS VOID VERSION'
C
C----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C