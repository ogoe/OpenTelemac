C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       RE-BUILDS THE GLOBAL MESH BOUNDARY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USE ONLY IN PARALLEL

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MESH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IER, NPOIN_MAX, NPTFR_TOT, POSITION, P_ISUM, TEMP1, TEMP2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     <tr><th> Unknown(s)
!>    </th><td> MPI_ALLREDUCE, MPI_COMM_WORLD, MPI_INTEGER, MPI_MAX
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
!>    <td><center> 6.0                                    </center></td>
!>    <td> 16/01/2010                                              </td>
!>    <td> C. DENIS (SINETICS)                                     </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
      SUBROUTINE BUILD_GLOBAL_FRONT(MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MESH           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
      INTEGER :: IER,I,POSITION
      INTEGER, ALLOCATABLE :: TEMP1(:)
      INTEGER, ALLOCATABLE :: TEMP2(:)
      INTEGER :: NPOIN_MAX
      INTEGER :: NPTFR_TOT
      INTEGER P_ISUM
      COMMON/INFO/LNG,LU
      INTEGER LNG,LU
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      IF(NCSIZE.LE.1) THEN
         IF(LNG.EQ.1) WRITE(LU,2018)
         IF(LNG.EQ.2) WRITE(LU,2019)
 2018    FORMAT(1X,'BUILD_GLOBAL_FRONT,',/,1X,
     &        'A UTILISER SEULEMENT EN MODE PARALLELE',///)
 2019    FORMAT(1X,'BULID_GLOBAL_FRONT,',/,1X,
     &        'USE ONLY IN THE PARALLEL VERSION',///)
         CALL PLANTE(1)
         STOP
      ENDIF
      NPOIN_MAX=P_ISUM(MESH%NPOIN)
      WRITE(35,*) 'NPOIN', MESH%NPOIN, NPOIN_MAX
      STOP
      ALLOCATE(TEMP1(NPOIN_MAX),STAT=IER)
      IF (IER .NE. 0) STOP 'ERREUR'
      ALLOCATE(TEMP2(NPOIN_MAX))
      IF (IER .NE. 0) STOP 'ERREUR'
      TEMP1(:)=0
      TEMP2(:)=0
      DO I=1,MESH%NPTFR
         POSITION=MESH%KNOLG%I(MESH%NBOR%I(I))
         TEMP1(POSITION)=1
      END DO
      CALL MPI_ALLREDUCE(TEMP1,TEMP2,NPOIN_MAX,MPI_INTEGER,
     &     MPI_MAX,
     &     MPI_COMM_WORLD,IER)

      NPTFR_TOT=0
      DO I=1,NPOIN_MAX
         IF (TEMP2(I) .NE. 0)  THEN
            NPTFR_TOT=NPTFR_TOT+1
         END IF
      END DO
C$$$      MESH%NPTFR=NPTFR_TOT
C$$$      DEALLOCATE(MESH%NBOR%I)
C$$$      ALLOCATE(MESH%NBOR%I(NPTFR)
C$$$      NBOR%I(:)=0
C$$$      DO I=1,MESH%NPTFR
C$$$         IF (TEMP2(I) .NE. 0) THEN
C$$$              MESH%NPTFR=NPTFR_TOT
C$$$
C$$$
C$$$
C$$$      NPTFR_TOT=0

      WRITE(*,*) 'NPTFR_TOT',NPTFR_TOT
      DEALLOCATE(TEMP1)
      DEALLOCATE(TEMP2)
      END
C
C#######################################################################
C