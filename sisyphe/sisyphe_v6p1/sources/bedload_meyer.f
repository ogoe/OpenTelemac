C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MEYER-PETER BEDLOAD TRANSPORT FORMULATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACP, COEFPN, DENS, DM, GRAV, HIDFAC, HIDING, QSC, SLOPEFF, TETAP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_MEYER
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), OS()
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
!>          <tr><td>AC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ACP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEFPN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DENS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDFAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDING
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SLOPEFF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETAP
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
       SUBROUTINE BEDLOAD_MEYER !
     &  (TETAP, HIDING, HIDFAC, DENS, GRAV, DM, AC,
     &   ACP, QSC, SLOPEFF, COEFPN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACP            |---| 
C| COEFPN         |---| 
C| DENS           |---| 
C| DM             |---| 
C| GRAV           |---| 
C| HIDFAC         |---| 
C| HIDING         |---| 
C| QSC            |---| 
C| SLOPEFF        |---| 
C| TETAP          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_MEYER => BEDLOAD_MEYER
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, HIDING
      INTEGER,          INTENT(IN)    :: HIDFAC, SLOPEFF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, COEFPN


      ! 3/ LOCAL VARIABLES
      ! ------------------
      DOUBLE PRECISION :: C2


!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      CALL CPSTVC(QSC,ACP)
      CALL OS('X=C     ', X=ACP, C=AC)

      ! **************************************** !
      ! 0 - SLOPE EFFECT: SOULBY FORMULATION     ! (_IMP_)
      ! **************************************** !
      IF(SLOPEFF == 2) THEN
        CALL OS('X=XY    ', X=ACP, Y=COEFPN )
      ENDIF

      ! **************************************** !
      ! III - BEDLOAD TRANSPORT CORRECTED        ! (_IMP_)
      !       FOR EXTENDED GRAIN SIZE            ! (_IMP_)
      ! **************************************** !
      C2 = 8.D0 * SQRT(GRAV*DENS*DM**3)
      IF ((HIDFAC == 1) .OR. (HIDFAC == 2) ) THEN
         CALL OS('X=XY    ', X=ACP, Y=HIDING)
         CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
         CALL OS('X=+(Y,C)', X=QSC, Y=QSC , C=0.D0)
         CALL OS('X=Y**C  ', X=QSC, Y=QSC , C=1.5D0)
         CALL OS('X=CX    ', X=QSC, C=C2)
      ELSE
          CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP)
          CALL OS('X=+(Y,C)', X=QSC, Y=QSC, C=0.D0)
         CALL OS('X=Y**C  ', X=QSC, Y=QSC, C=1.5D0)
         CALL OS('X=CX    ', X=QSC, C=C2)
         CALL OS('X=XY    ', X=QSC, Y=HIDING)
      ENDIF

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE BEDLOAD_MEYER
C
C#######################################################################
C