C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       REDUCTION OF A VECTOR OF LOGICALS WITH DIFFUSION OF
!>                THE RESULT TO ALL THE PROCESSORS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IARG1, LARG2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IER, LAUX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     <tr><th> Unknown(s)
!>    </th><td> MPI_ALLREDUCE, MPI_COMM_WORLD, MPI_LOGICAL, MPI_LOR
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
!>    <td><center> 5.9                                    </center></td>
!>    <td> 01/07/2006                                              </td>
!>    <td> O.BOITEAU (SINETICS)                                    </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IARG1
!></td><td>--></td><td>TAILLE DU VECTEUR
!>    </td></tr>
!>          <tr><td>LARG2
!></td><td><-></td><td>CONTRIBUTION DU PROCESSEUR APPELANT.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                            SUBROUTINE P_LSUM
     &(IARG1,LARG2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IARG1          |-->| TAILLE DU VECTEUR
C| LARG2          |<->| CONTRIBUTION DU PROCESSEUR APPELANT.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INCLUDE 'mpif.h'
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: IARG1
      LOGICAL, DIMENSION(IARG1), INTENT(INOUT) :: LARG2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL, DIMENSION(:), ALLOCATABLE :: LAUX
      INTEGER IER,I
C
C-----------------------------------------------------------------------
C
      ALLOCATE(LAUX(IARG1),STAT=IER)
      IF (IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*)'P_LSUM: ERREUR DANS ALLOCATION MEMOIRE'
        IF(LNG.EQ.2) WRITE(LU,*)'P_LSUM: ERROR IN MEMORY ALLOCATION'
        CALL PLANTE(1)
        STOP
      ENDIF
C
      DO I=1,IARG1
        LAUX(I)=LARG2(I)
      ENDDO
C
      CALL MPI_ALLREDUCE(LAUX,LARG2,IARG1,MPI_LOGICAL,
     &                   MPI_LOR,MPI_COMM_WORLD,IER)
C
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_LSUM: ERREUR DANS MPI_ALLREDUCE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_LSUM: ERROR IN MPI_ALLREDUCE'
        WRITE(LU,*) 'MPI ERROR: ',IER
        CALL PLANTE(1)
        STOP
      ENDIF
C
      DEALLOCATE(LAUX)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C