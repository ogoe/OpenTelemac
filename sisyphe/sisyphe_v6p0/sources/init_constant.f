C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SETS THE CONSTANTS USED BY SISYPHE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> KARIM_HOLLY_YANG, KARMAN, PI
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 11/01/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>KARIM_HOLLY_YANG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PI
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE INIT_CONSTANT
     &(KARIM_HOLLY_YANG,KARMAN,PI)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| KARIM_HOLLY_YAN|---| 
C| KARMAN         |---| 
C| PI             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(INOUT) :: KARIM_HOLLY_YANG
      DOUBLE PRECISION, INTENT(INOUT) :: KARMAN
      DOUBLE PRECISION, INTENT(INOUT) :: PI
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
!
C KARIM, HOLLY & YANG CONSTANT
! ----------------------------
!
      KARIM_HOLLY_YANG = 0.85D0
!
C VON KARMAN CONSTANT
! -------------------
!
      KARMAN = 0.4D0
!
C PARTHENIADES CONSTANT : EXPRESSED IN M/S
! ---------------------
C THIS VALUE IS NOW GIVEN IN USER_KRONE_PART
C      PARTHENIADES = 2.D-5/XMVS
!
C PI
! --
!
      PI =ACOS(-1.D0)
!
!======================================================================!
!
      RETURN
      END SUBROUTINE
C
C#######################################################################
C