C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE THETAC ANGLE (FLOW DIRECTION).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NPOIN, PI, QU, QV, THETAC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, LOCAL_ZERO
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_DIRECTION
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
!>      <td><center> 5.4                                       </center>
!> </td><td> 01/10/2003
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
!>          <tr><td>QU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>THETAC
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_DIRECTION ! (_IMP_)
     &  (QU, QV, NPOIN, PI, THETAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPOIN          |---| 
C| PI             |---| 
C| QU             |---| 
C| QV             |---| 
C| THETAC         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_DIRECTION => BEDLOAD_DIRECTION
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),  INTENT(IN)  :: QU, QV
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: PI
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: THETAC


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: LOCAL_ZERO = 1.D-6


!======================================================================!
!======================================================================!
C                    DECLARES TYPES AND DIMENSIONS                     !
!======================================================================!
!======================================================================!

      DO I = 1, NPOIN

         IF (ABS(QU%R(I)) <= LOCAL_ZERO) THEN

            IF (QV%R(I) < = LOCAL_ZERO) THEN
               THETAC%R(I) = -PI*0.5D0
            ELSE
               THETAC%R(I) =  PI*0.5D0
            ENDIF

         ELSE

            THETAC%R(I) = ATAN(QV%R(I) / QU%R(I))

            IF (QU%R(I) < 0.D0) THEN
               THETAC%R(I) = PI + THETAC%R(I)
            ENDIF

         ENDIF

      END DO

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE BEDLOAD_DIRECTION
C
C#######################################################################
C