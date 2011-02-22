C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       UPWINDS THE ADVECTION TERM OF VERTICAL VELOCITY.
!>  @code
!>      A DIFFUSION TERM WITH DIFFUSION COEFFICIENT ABS(WCC)*DZ/2
!>      IS ADDED TO THE MATRIX M. FORMULA IS OBTAINED BY SIMPLIFYING
!>      THE Z PART OF DIFFUSION MATRIX BUILT IN SUBROUTINE MT02PP
!>      DZ THEN VANISHES.<br>
!>      THIS IS USED IN DIFF3D FOR SEDIMENT SETTLING VELOCITY AND
!>      FOR VERTICAL UPWINDING IN SUPG METHOD.
!>  @endcode

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
!> </td><td> 12/12/05
!> </td><td> J.M. HERVOUET  (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE UPWIND
     &(M,WCC,DELTA,MESH2D,MESH3D,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DELTA          |-->| UPWIND COEFFICIENT (BETWEEN 0 AND 1)
C| M             |<->| MATRIX
C| MESH2D         |-->| 2D MESH
C| MESH3D         |-->| 3D MESH
C| NPLAN          |---| 
C| WCC            |-->| VERTICAL VELOCITY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: M
      TYPE(BIEF_OBJ), INTENT(IN)     :: WCC
      DOUBLE PRECISION, INTENT(IN)   :: DELTA
      TYPE(BIEF_MESH), INTENT(IN)    :: MESH3D,MESH2D
      INTEGER, INTENT(IN)            :: NPLAN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
      IF(M%STO.EQ.1) THEN
        CALL UPWINDEBE(M%D%R,M%X%R,MESH3D%IKLE%I,
     &                 MESH3D%NELMAX,
     &                 MESH3D%NELEM,MESH2D%NELEM,
     &                 MESH2D%SURFAC%R,NPLAN,WCC%R,M%TYPEXT,DELTA)
      ELSEIF(M%STO.EQ.3) THEN
        CALL UPWINDSEG(M%D%R,M%X%R,
     &                 MESH3D%IKLE%I,MESH3D%NELMAX,
     &                 MESH3D%NELEM,MESH2D%NELEM,MESH2D%SURFAC%R,
     &                 NPLAN,WCC%R,MESH2D%NSEG,MESH3D%NSEG,M%TYPEXT,
     &                 DELTA)
      ELSE
        WRITE(LU,*) 'UNEXPECTED STORAGE FOR MATRIX M IN UPWIND'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
