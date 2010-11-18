C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRICTION COEFFICIENT UNDER
!>                WAVE AND CURRENT COMBINED ACTION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALPHAW, CF, FCW, FW, NPOIN, TOB, TOBW, UCMOY, UW, XMVE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AX, BX, CSAL, CSAL1, CSAL2, CSAL3, I, LOGF, MX, NX, PX, QX, TAUCW, TX, UCW2, ZERO
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_INTERACT
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_BAILARD(), BEDLOAD_DIBWAT()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 23/09/2010
!> </td><td> C. VILLARET (LNHE)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.7                                       </center>
!> </td><td> 01/10/2003
!> </td><td> C. VILLARET (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHAW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FCW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOBW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE BEDLOAD_INTERACT
     &(UCMOY,TOBW,TOB,ALPHAW,FW,CF,UW,NPOIN,XMVE,FCW)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHAW         |---| 
C| CF             |---| 
C| FCW            |---| 
C| FW             |---| 
C| NPOIN          |---| 
C| TOB            |---| 
C| TOBW           |---| 
C| UCMOY          |---| 
C| UW             |---| 
C| XMVE           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_INTERACT => BEDLOAD_INTERACT
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      !
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      !
      TYPE(BIEF_OBJ),   INTENT(IN)  :: UCMOY, TOBW, TOB, ALPHAW
      TYPE(BIEF_OBJ),   INTENT(IN)  :: FW, CF, UW
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: XMVE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FCW
      !
      ! 3/ LOCAL VARIABLES
      ! ------------------
      !
      INTEGER                     :: I
      DOUBLE PRECISION            :: TX, LOGF
      DOUBLE PRECISION            :: CSAL,CSAL1, CSAL2, CSAL3
      DOUBLE PRECISION            :: AX, MX, NX, BX, PX, QX
      DOUBLE PRECISION            :: UCW2, TAUCW,ZERO
C
      INTRINSIC MAX
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ZERO = 1.D-6
!
      DO I = 1, NPOIN
!
        TX = TOB%R(I) / MAX((TOB%R(I) + TOBW%R(I)),ZERO)
!
        LOGF  = LOG10(2.D0*MAX(FW%R(I),ZERO)/MAX(CF%R(I),ZERO))
        CSAL  = ABS(COS(ALPHAW%R(I)))
        CSAL1 = CSAL**0.82D0
        CSAL3 = CSAL**2.70D0
!
        AX = -0.07D0 + 1.87D0*CSAL1 + (-0.34D0 - 0.12D0*CSAL1)*LOGF
        MX =  0.72D0 - 0.33D0*CSAL1 + ( 0.08D0 + 0.34D0*CSAL1)*LOGF
        NX =  0.78D0 - 0.23D0*CSAL1 + ( 0.12D0 - 0.12D0*CSAL1)*LOGF
!
        BX =  0.27D0 + 0.51D0*CSAL3 + (-0.10D0 - 0.24D0*CSAL3)*LOGF
        PX = -0.75D0 + 0.13D0*CSAL3 + ( 0.12D0 + 0.02D0*CSAL3)*LOGF
        QX =  0.89D0 + 0.40D0*CSAL3 + ( 0.50D0 - 0.28D0*CSAL3)*LOGF
!
        IF(TX.LE.ZERO) THEN
          TAUCW = TOBW%R(I)
        ELSEIF(TX.LT.1.D0) THEN
          TAUCW = (1.D0 + BX * TX**PX * (1.D0 - TX)**QX)*TOB%R(I)*TX
     &          + (1.D0 + AX * TX**MX * (1.D0 - TX)**NX)*TOBW%R(I)
        ELSE
          TAUCW = TOB%R(I)
        ENDIF
!
        UCW2 = (UCMOY%R(I)**2 + 0.5D0 * UW%R(I)**2) * XMVE
        FCW%R(I) = TAUCW / MAX(UCW2,1.D-10)
!
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C