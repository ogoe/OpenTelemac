C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CALLS FUNCTION MPI_ALLTOALLV.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I1, I10, I2, I3, I4, I5, I6, I7, I8, I9
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> MPI_ALLTOALLV
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TOMAWAC_MPI(), TOMAWAC_MPI_TOOLS()

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
!>    <td> 27/10/2009                                              </td>
!>    <td> C. DENIS (SINETICS)                                     </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>I1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I10
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I9
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE  P_MPI_ALLTOALLV_TOMA1
     &(I1,I2,I3,I4,I5,I6,I7,I8,I9,I10)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I1             |---| 
C| I10            |---| 
C| I2             |---| 
C| I3             |---| 
C| I4             |---| 
C| I5             |---| 
C| I6             |---| 
C| I7             |---| 
C| I8             |---| 
C| I9             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE CHARAC_TYPE
      INTEGER :: MYPID          ! PARTITION OF THE TRACEBACK ORIGIN (HEAD)
      INTEGER :: NEPID          ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO
      INTEGER :: INE            ! THE LOCAL 2D ELEMENT NR THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION
      INTEGER :: KNE            ! THE LOCAL LEVEL THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION
      INTEGER :: IOR            ! THE POSITION OF THE TRAJECTORY -HEAD- IN MYPID [THE 2D/3D NODE OF ORIGIN]
      INTEGER :: ISP,NSP        ! NUMBERS OF RUNGE-KUTTA PASSED AS COLLECTED AND TO FOLLOW AT ALL
      DOUBLE PRECISION :: XP,YP,ZP ! THE (X,Y,Z)-POSITION NOW
      DOUBLE PRECISION :: DX,DY,DZ ! THE (X,Y,Z)-POSITION NOW
      DOUBLE PRECISION :: BASKET(10) ! VARIABLES INTERPOLATED AT THE FOOT
      END TYPE CHARAC_TYPE

      TYPE(CHARAC_TYPE) ::  I1(*), I5(*)
      INTEGER, INTENT(IN) ::  I2(*),I3(*),I4,I6(*),I7(*)
      INTEGER, INTENT(IN) :: I8,I9,I10
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C


      CALL MPI_ALLTOALLV(I1,I2,I3,I4,I5,I6,I7,I8,I9,I10)
C
      IF(I10.NE.0) THEN
        WRITE(LU,*) 'P_MPI_ALLTOALLV:'
        WRITE(LU,*) 'MPI ERROR ',I10
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