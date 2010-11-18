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

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DELTA, M, MESH2D, MESH3D, NPLAN, WCC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), UPWINDEBE(), UPWINDSEG()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DIFF3D(), PRECON()

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

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DELTA
!></td><td>--></td><td>UPWIND COEFFICIENT (BETWEEN 0 AND 1)
!>    </td></tr>
!>          <tr><td>M
!></td><td><-></td><td>MATRIX
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>--></td><td>2D MESH
!>    </td></tr>
!>          <tr><td>MESH3D
!></td><td>--></td><td>3D MESH
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WCC
!></td><td>--></td><td>VERTICAL VELOCITY
!>    </td></tr>
!>     </table>
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