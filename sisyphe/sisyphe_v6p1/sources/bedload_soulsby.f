C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOULSBY & VAN RIJN BEDLOAD TRANSPORT FORMULATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> D90, DENS, DM, DSTAR, GRAV, HMIN, HN, NPOIN, QSC, QSS, UCMOY, UW
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ASB, ASS, CD, COEF, I, TRA, UCR, VTOT, Z0
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_SOULSBY
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
!> </td><td> **/11/2003
!> </td><td> C.VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> 22/05/2001
!> </td><td> SOGREAH
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>D90
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
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_SOULSBY ! (_IMP_)
     &  (UCMOY,HN, UW, NPOIN, DENS, GRAV, DM, DSTAR, HMIN, D90, QSC,
     &   QSS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| D90            |---| 
C| DENS           |---| 
C| DM             |---| 
C| DSTAR          |---| 
C| GRAV           |---| 
C| HMIN           |---| 
C| HN             |---| 
C| NPOIN          |---| 
C| QSC            |---| 
C| QSS            |---| 
C| UCMOY          |---| 
C| UW             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_SOULSBY => BEDLOAD_SOULSBY
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)  :: HN, UCMOY, UW
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: DENS, GRAV, DM, DSTAR, HMIN, D90
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: I
      DOUBLE PRECISION            :: COEF, ASS, ASB, CD
      DOUBLE PRECISION            :: UCR, VTOT, TRA
      DOUBLE PRECISION, PARAMETER :: Z0=0.006D0


!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! ************************* !
      ! I - SUSPENSION COEFFCIENT !
      ! ************************* !
      COEF = (DENS *GRAV*DM)**1.2D0
      ASS  = 0.012D0*DM*(DSTAR**(-0.6D0))/COEF


      DO I = 1, NPOIN


         ! *************************** !
         ! III - BEDLOAD COEFFICIENT   !
         ! *************************** !
         ASB = 0.005D0*HN%R(I)*(DM/MAX(HN%R(I),DM))**1.2D0 / COEF


         ! ********************************** !
         ! IV - ROUGHNESS COEFFICIENT CD      !
         !      SOULSBY: Z0=0.006 --> KS=18CM !
         ! ********************************** !
         CD = (0.4D0 / (LOG(MAX(HN%R(I),Z0)/Z0)-1.D0))**2


         ! ************************************************ !
         ! V - CRTITICAL CURRENT SPEED UCR                  !
         ! ************************************************ !
         IF (DM < 0.0005D0) THEN
            UCR = 0.19D0*(DM**0.1D0)*LOG10(4.D0*MAX(HN%R(I),D90)/D90)
         ELSE
            UCR = 8.50D0*(DM**0.6D0)*LOG10(4.D0*MAX(HN%R(I),D90)/D90)
         ENDIF


         ! ************************************************* !
         ! VI - SPEED INDUCED BY THE CURRENT AND WAVES       !
         ! ************************************************* !
         VTOT = SQRT(UCMOY%R(I)**2+(0.018D0/CD)*UW%R(I)**2)


         ! *********************************************** !
         ! VII - SUSPENDED AND BEDLOAD TRANSPORT           !
         ! *********************************************** !
         IF (VTOT > UCR) THEN
            TRA     = UCMOY%R(I)  * (VTOT - UCR )**2.4D0
            QSS%R(I)= ASS * TRA
            QSC%R(I)= ASB * TRA
         ELSE
            QSS%R(I) = 0.D0
            QSC%R(I) = 0.D0
         ENDIF
      ENDDO

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE BEDLOAD_SOULSBY
C
C#######################################################################
C