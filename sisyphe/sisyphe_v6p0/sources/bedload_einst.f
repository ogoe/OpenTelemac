C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EINSTEIN-BROWN BEDLOAD TRANSPORT FORMULATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DENS, DM, DSTAR, GRAV, NPOIN, QSC, TETAP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CEINST, I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_EINST
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
!> </td><td> **/10/2003
!> </td><td> C.VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 11/09/1995
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
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
        SUBROUTINE BEDLOAD_EINST ! (_IMP)
     &  (TETAP, NPOIN, DENS, GRAV, DM, DSTAR, QSC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DENS           |---| 
C| DM             |---| 
C| DSTAR          |---| 
C| GRAV           |---| 
C| NPOIN          |---| 
C| QSC            |---| 
C| TETAP          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_EINST => BEDLOAD_EINST
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, DSTAR
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: CEINST


!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!



      ! **************************** !
      ! II - BEDLOAD TRANSPORT       ! (_IMP_)
      ! **************************** !
      CEINST = 36.D0/(DSTAR**3)
      CEINST = SQRT(2.D0/3.D0+CEINST) -  SQRT(CEINST)
      CEINST = CEINST * SQRT(DENS*GRAV*(DM**3))
      DO I = 1, NPOIN

         IF (TETAP%R(I) < 2.5D-3) THEN
            QSC%R(I) = 0.D0
         ELSE IF (TETAP%R(I) < 0.2D0) THEN
            QSC%R(I) = 2.15D0* CEINST * EXP(-0.391D0/TETAP%R(I))
         ELSE
            QSC%R(I) = 40.D0 * CEINST * (TETAP%R(I)**3.D0)
         ENDIF

      ENDDO

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE BEDLOAD_EINST
C
C#######################################################################
C