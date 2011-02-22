C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       REBUILDS THE BOUNDARY OF THE MESH (GLOBAL NODES).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  TO BE USED ONLY IN PARALLEL MODE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, CG, CGT, CTT, K, KNOLG, KT, NPOIN, NPOIN_TOT, X, XT, Y, YT
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IER, TEMP1, TEMP2, TEMP3, TMP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> MPI_ALLREDUCE, MPI_COMM_WORLD, MPI_MAX, MPI_REAL8
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS()

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
!>          <tr><td>C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CGT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CTT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>K
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNOLG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN_TOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
      SUBROUTINE BUILD_GLOBAL_BOUND
     &     (KNOLG,NPOIN,NPOIN_TOT,X,Y,K,C,CG,XT,YT,KT,CTT,CGT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |---| 
C| CG             |---| 
C| CGT            |---| 
C| CTT            |---| 
C| K             |---| 
C| KNOLG          |---| 
C| KT             |---| 
C| NPOIN          |---| 
C| NPOIN_TOT      |---| 
C| X             |---| 
C| XT             |---| 
C| Y             |---| 
C| YT             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INCLUDE 'mpif.h'
C
      INTEGER, INTENT(IN) :: NPOIN_TOT
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN), DIMENSION(NPOIN) :: KNOLG
      DOUBLE PRECISION, INTENT(IN), DIMENSION(NPOIN)  :: X, Y, K, C,CG
      DOUBLE PRECISION , INTENT(OUT), DIMENSION(NPOIN_TOT) :: XT,YT,KT,
     &     CTT,CGT
      INTEGER :: I
      INTEGER :: IER
      INTEGER, ALLOCATABLE :: TEMP1(:)
      INTEGER, ALLOCATABLE :: TEMP2(:)
      DOUBLE PRECISION, ALLOCATABLE :: TEMP3(:)
      DOUBLE PRECISION :: TMP
      ALLOCATE(TEMP3(NPOIN_TOT))
      YT(:)=0.0
      XT(:)=0.0
      CTT(:)=0.0
      CGT(:)=0.0
      KT(:)=0.0
C     XT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=X(I)
      END DO

      CALL MPI_ALLREDUCE(TEMP3,XT,NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(XT .EQ. -HUGE(TMP))
         XT=0.0
      END WHERE
C     YT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=Y(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,YT, NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(YT .EQ. -HUGE(TMP))
         YT=0.0
      END WHERE
C     CT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=C(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,CTT, NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(CTT .EQ. -HUGE(TMP))
         CTT=0.0
      END WHERE
C     CGT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=CG(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,CGT, NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(CGT .EQ.-HUGE(TMP))
         CGT=0.0
      END WHERE
C     KT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=K(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,KT, NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(KT .EQ. -HUGE(TMP))
         KT=0.0
      END WHERE
      DEALLOCATE(TEMP3)
      END
C
C#######################################################################
C