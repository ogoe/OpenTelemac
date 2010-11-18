C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I1, I2, I3, I4, I5, I6, I7, I8
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>STREAMLINE_TOMAWAC()

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
!>    <td> 18/08/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MPI_ALLTOALL
     &(I1,I2,I3,I4,I5,I6,I7,I8)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I1             |---| 
C| I2             |---| 
C| I3             |---| 
C| I4             |---| 
C| I5             |---| 
C| I6             |---| 
C| I7             |---| 
C| I8             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: I1(*),I2,I3,I4(*),I5,I6,I7,I8
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE MPI_ALLTOALL VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALLING VOID MPI_ALLTOALL'
C
C----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

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

!>  @par Called by
!><br>STREAMLINE_TOMAWAC()

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
!>    <td> 18/08/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
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
                        SUBROUTINE MPI_ALLTOALLV
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
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: I1(*),I2(*),I3(*),I4,I5(*),I6(*),I7(*)
      INTEGER, INTENT(IN) :: I8,I9,I10
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE MPI_ALLTOALL VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALLING VOID MPI_ALLTOALL'
C
C----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I1, I2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>STREAMLINE_TOMAWAC()

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
!>    <td> 18/08/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
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
!>          <tr><td>I2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MPI_TYPE_COMMIT
     &(I1,I2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I1             |---| 
C| I2             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: I1,I2
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE MPI_TYPE_COMMIT VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALLING VOID MPI_TYPE_COMMIT'
C
C----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I1, I2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>STREAMLINE_TOMAWAC()

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
!>    <td> 18/08/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
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
!>          <tr><td>I2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MPI_TYPE_EXTENT
     &(I1,I2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I1             |---| 
C| I2             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: I1,I2
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE MPI_TYPE_EXTENT VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALLING VOID MPI_TYPE_EXTENT'
C
C----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I1, I2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>STREAMLINE_TOMAWAC()

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
!>    <td> 18/08/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
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
!>          <tr><td>I2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MPI_TYPE_FREE
     &(I1,I2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I1             |---| 
C| I2             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: I1,I2
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE MPI_TYPE_FREE VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALLING VOID MPI_TYPE_FREE'
C
C----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I1, I2, I3
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>STREAMLINE_TOMAWAC()

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
!>    <td> 18/08/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
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
!>          <tr><td>I2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I3
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MPI_TYPE_LB
     &(I1,I2,I3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I1             |---| 
C| I2             |---| 
C| I3             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: I1,I2,I3
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE MPI_TYPE_LB VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALLING VOID MPI_TYPE_LB'
C
C----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I1, I2, I3, I4, I5, I6
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>STREAMLINE_TOMAWAC()

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
!>    <td> 18/08/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MPI_TYPE_STRUCT
     &(I1,I2,I3,I4,I5,I6)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I1             |---| 
C| I2             |---| 
C| I3             |---| 
C| I4             |---| 
C| I5             |---| 
C| I6             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: I1,I5,I6
      INTEGER, INTENT(IN) :: I2(I1),I3(I1),I4(I1)
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE MPI_TYPE_STRUCT VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALLING VOID MPI_TYPE_STRUCT'
C
C----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> I1, I2, I3
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>STREAMLINE_TOMAWAC()

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
!>    <td> 18/08/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
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
!>          <tr><td>I2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I3
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MPI_TYPE_UB
     &(I1,I2,I3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| I1             |---| 
C| I2             |---| 
C| I3             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: I1,I2,I3
C
C----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE MPI_TYPE_UB VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALLING VOID MPI_TYPE_UB'
C
C----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C