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
!>      <td><center> 5.6                                       </center>
!> </td><td>
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
      MODULE FRICTION_DEF
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      TYPE POINTER_TO_FRICTION
C         SEQUENCE
         TYPE(FRICTION_OBJ), POINTER :: P
      END TYPE POINTER_TO_FRICTION
C
      TYPE FRICTION_OBJ
C         SEQUENCE
         INTEGER          :: GNUMB(2) ! GLOBAL NUMBER OF THE ZONE
         INTEGER          :: RTYPE(2) ! TYPE OF LAW USED

         ! USE REAL BECAUSE CHESTR IS SAVED AS SIMPLE PRECISION IN SELAFIN DATA
         ! --------------------------------------------------------------------
         DOUBLE PRECISION :: RCOEF(2) ! FRICTION PARAMETER
         DOUBLE PRECISION :: NDEF(2)  ! DEFAULT MANNING (FOR C-W LAW)
         DOUBLE PRECISION :: DP       ! DIAMETER OF ROUGHNESS ELEMENT
         DOUBLE PRECISION :: SP       ! SPACING OF ROUGHNESS ELEMENT
         TYPE(POINTER_TO_FRICTION), POINTER, DIMENSION(:) :: ADR
      END TYPE FRICTION_OBJ
C
      END MODULE FRICTION_DEF

C
C#######################################################################
C