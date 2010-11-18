C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ENGELUND-HANSEN BEDLOAD TRANSPORT FORMULATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  FORMULATION IS DIFFERENT FROM THAT IN BEDLOAD_ENGEL

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, DENS, DM, GRAV, NPOIN, QSC, TETA, TETAP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CENGEL, I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_ENGEL_OLD
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
!>      <td><center> 5.8                                       </center>
!> </td><td> 11/07/2007
!> </td><td> J-M HERVOUET
!> </td><td> DELETED OS REFERENCES
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> **/11/2003
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
!>          <tr><td>CF
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
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETAP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCMOY
!></td><td>--></td><td>NORM OF VELOCITY
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BEDLOAD_ENGEL_OLD
     &(TETAP,CF,NPOIN,GRAV,DM,DENS,TETA,QSC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |---| 
C| DENS           |---| 
C| DM             |---| 
C| GRAV           |---| 
C| NPOIN          |---| 
C| QSC            |---| 
C| TETA           |---| 
C| TETAP          |---| 
C| UCMOY          |-->| NORM OF VELOCITY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_ENGEL_OLD => BEDLOAD_ENGEL_OLD
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP,CF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DM, DENS
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          :: I
      DOUBLE PRECISION :: CENGEL
!
      INTRINSIC SQRT
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C     ADIMENSIONAL SKIN STRESS: TETAP
!
C     ADIMENSIONAL TOTAL STRESS
!
      DO I = 1, NPOIN
        IF(TETAP%R(I) <= 0.06D0) THEN
          TETA%R(I) = 0.D0
        ELSEIF(TETAP%R(I) <  0.384D0) THEN
          TETA%R(I) = SQRT( 2.5D0 * (TETAP%R(I) - 0.06D0))
        ELSEIF(TETAP%R(I) <  1.080D0) THEN
          TETA%R(I) = 1.066D0 * TETAP%R(I)**0.176D0
        ELSE
          TETA%R(I) = TETAP%R(I)
        ENDIF
      ENDDO
!
C     BEDLOAD TRANSPORT
!
      CENGEL = 0.1D0*SQRT(DENS*GRAV*DM**3)
      DO I=1,NPOIN
        QSC%R(I)=CENGEL*SQRT(TETA%R(I)**5)/MAX(CF%R(I),1.D-6)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C