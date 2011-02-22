C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES QUADRATIC VELOCITIES AND PERIODS
!>               (CASE WITH WAVES).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NPOIN, PI, TW, TW1, TW2, UCW, UW, UW1, UW2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ACOSMRAP, ACOSPRAP, I, RAP, SQRTRAP, UCMOY, ZERO
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_CALCDW
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_DIBWAT()

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
!> </td><td> C. VILLARET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TW1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TW2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_CALCDW ! (_IMP_)
     &  (UCW, UW, TW, NPOIN, PI, UW1, UW2, TW1, TW2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPOIN          |---| 
C| PI             |---| 
C| TW             |---| 
C| TW1            |---| 
C| TW2            |---| 
C| UCW            |---| 
C| UW             |---| 
C| UW1            |---| 
C| UW2            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_CALCDW => BEDLOAD_CALCDW
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
C     2/ GLOBAL VARIABLES
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UCW, UW, TW
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: PI
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: UW1, UW2, TW1, TW2
!
C     3/ LOCAL VARIABLES
!
      INTEGER                     :: I
      DOUBLE PRECISION            :: UCMOY, RAP
      DOUBLE PRECISION            :: ACOSMRAP, ACOSPRAP, SQRTRAP
      DOUBLE PRECISION, PARAMETER :: ZERO = 1.D-06
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      DO I = 1,NPOIN
         UCMOY = ABS(UCW%R(I))
         ! ****************** !
         !    I - WAVES ONLY  ! (_IMP_)
         ! ****************** !
         IF (UCMOY <= ZERO) THEN
            UW1%R(I) = UW%R(I)
            UW2%R(I) = UW%R(I)
            TW1%R(I) = TW%R(I) / 2.D0
            TW2%R(I) = TW%R(I) / 2.D0
         ELSE
            RAP = UW%R(I) / UCMOY
            ! ******************** !
            ! II - WAVES ARE PREDOMINANT ! (_IMP_)
            ! ******************** !
            IF (RAP > 1.D0) THEN
               ACOSMRAP = ACOS(-1.D0/RAP)
               ACOSPRAP = ACOS( 1.D0/RAP)
               SQRTRAP  = SQRT(1.D0-1.D0/RAP**2)
               TW1%R(I) = TW%R(I)*ACOSMRAP / PI
               TW2%R(I) = TW%R(I)*ACOSPRAP / PI
               UW1%R(I) = 2.D0*UCMOY**2 + UW%R(I)**2
     &                  + 3.D0*UCMOY*UW%R(I)*SQRTRAP/ACOSMRAP
               UW1%R(I) = SQRT(UW1%R(I))
               UW2%R(I) = 2.D0*UCMOY**2 + UW%R(I)**2
     &                  - 3.D0*UCMOY*UW%R(I)*SQRTRAP/ACOSPRAP
               UW2%R(I) = SQRT(UW2%R(I))

            ! ********************** !
            ! III - CURRENTS ARE PREDOMINANT ! (_IMP_)
            ! ********************** !
            ELSE
               UW1%R(I) = UCW%R(I)*SQRT(2.D0 + RAP**2)
               UW2%R(I) = ZERO
               TW1%R(I) = TW%R(I)
               TW2%R(I) = ZERO
            ENDIF
         ENDIF
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END SUBROUTINE BEDLOAD_CALCDW
C
C#######################################################################
C