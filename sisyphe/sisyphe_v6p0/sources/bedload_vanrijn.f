C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       VAN RIJN BEDLOAD TRANSPORT FORMULATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, DENS, DM, DSTAR, GRAV, MU, NPOIN, QSC, TETAP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C1, C2, I, T
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_VANRIJN
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_FORMULA()

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
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> **/**/2004
!> </td><td> C. VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> **/10/2001
!> </td><td> BUI MINH DUC
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DENS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DSTAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETAP
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_VANRIJN !
     &  (TETAP,MU, NPOIN, DM, DENS, GRAV, DSTAR, AC, QSC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| DENS           |---| 
C| DM             |---| 
C| DSTAR          |---| 
C| GRAV           |---| 
C| MU             |---| 
C| NPOIN          |---| 
C| QSC            |---| 
C| TETAP          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_VANRIJN => BEDLOAD_VANRIJN
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)  :: TETAP,MU
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: DM, DENS, GRAV, DSTAR, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: C1, C2, T

!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      C1 = DENS * GRAV * DM
      C2 = 0.053D0 * SQRT(DM**3.0D0*DENS*GRAV) * DSTAR**(-0.3D0)

      DO I = 1, NPOIN

         ! ****************************** !
         ! I - TRANSPORT STAGE PARAMETER  !
         ! ****************************** !
         IF(TETAP%R(I) .LE. AC) THEN
            T=0.D0
         ELSE
            T = (TETAP%R(I)-AC)/MAX(AC,1.D-06)
         ENDIF


         ! ***************************** !
         ! II - BEDLOAD TRANSPORT RATE   !
         ! ***************************** !
         QSC%R(I) = C2 * (T**2.1D0)

       ENDDO

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE BEDLOAD_VANRIJN
C
C#######################################################################
C