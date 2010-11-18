C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       REBUILDS THE BOUNDARY OF THE MESH (GLOBAL NODES).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USE ONLY IN PARALLEL; EMPTY SHELL IN SCALAR MODE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, CG, CGT, CTT, K, KNOLG, KT, LIHBOR, LIHBORT, NBOR, NBOR_TOT, NPOIN, NPOIN_TOT, NPTFR, NPTFR_TOT, X, XT, Y, YT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
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
!>          <tr><td>LIHBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIHBORT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR_TOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN_TOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR_TOT
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
     &(KNOLG,NPOIN,NPOIN_TOT,NPTFR,NPTFR_TOT,
     & X,Y,K,C,CG,LIHBOR,XT,YT,KT,CTT,CGT,LIHBORT,NBOR,NBOR_TOT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |---| 
C| CG             |---| 
C| CGT            |---| 
C| CTT            |---| 
C| K             |---| 
C| KNOLG          |---| 
C| KT             |---| 
C| LIHBOR         |---| 
C| LIHBORT        |---| 
C| NBOR           |---| 
C| NBOR_TOT       |---| 
C| NPOIN          |---| 
C| NPOIN_TOT      |---| 
C| NPTFR          |---| 
C| NPTFR_TOT      |---| 
C| X             |---| 
C| XT             |---| 
C| Y             |---| 
C| YT             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: NPOIN_TOT
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER, INTENT(IN) :: NPTFR_TOT
C
      INTEGER, INTENT(IN), DIMENSION(NPOIN) :: KNOLG
      DOUBLE PRECISION, INTENT(IN), DIMENSION(NPOIN)  :: X, Y, K, C,CG
      DOUBLE PRECISION , INTENT(IN), DIMENSION(NPOIN_TOT) :: XT,YT,KT,
     &                                                       CTT,CGT
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR)
      INTEGER, INTENT(IN) :: LIHBORT(NPTFR_TOT)
      INTEGER, INTENT(IN) :: NBOR(NPTFR), NBOR_TOT(NPTFR_TOT)
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,*) 'BUILD_GLOBAL_BOUND VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'BUILD_GLOBAL_BOUND IN ITS VOID VERSION'
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C