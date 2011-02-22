C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GETS THE TRIDIAGONAL PART OF A DIFFUSION MATRIX ON
!>                PRISMS AND REMOVES IT FROM THE INITIAL MATRIX.
!>  @code
!>            IF MTRI IS THIS TRIDIAGONAL PART, M THE RESULT AND MDIF
!>            THE DIFFUSION MATRIX, THIS SUBROUTINE DOES:
!>
!>            M = TETA * MTRI
!>            MDIF CHANGED INTO (1-TETA) * MDIF
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> M, MDIFF, MESH3D, NPLAN, NPOIN2, NSEG2D, TETA
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_GETTRI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> GETTRIEBE(), GETTRISEG(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAVE_EQUATION()

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
!> </td><td> 16/06/05
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>M
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MDIFF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GETTRI
     &(M,MDIFF,TETA,MESH3D,NPLAN,NPOIN2,NSEG2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| M             |---| 
C| MDIFF          |---| 
C| MESH3D         |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| NSEG2D         |---| 
C| TETA           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_GETTRI => GETTRI
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
      INTEGER, INTENT(IN) :: NPLAN,NPOIN2,NSEG2D
C
      DOUBLE PRECISION, INTENT(IN)    :: TETA
      DOUBLE PRECISION, INTENT(INOUT) :: M(NPOIN2,*)
C
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: MDIFF
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH3D
C
C-----------------------------------------------------------------------
C
      IF(MDIFF%STO.EQ.1) THEN
C
        CALL GETTRIEBE(M,MDIFF%D%R,MDIFF%X%R,TETA,
     &                 MESH3D%IKLE%I,
     &                 MESH3D%NPOIN,MESH3D%NELEM,MESH3D%NELMAX,MESH3D)
C
      ELSEIF(MDIFF%STO.EQ.3) THEN
C
        CALL GETTRISEG(M,MDIFF%D%R,MDIFF%X%R,TETA,
     &                 MESH3D%NPOIN,MESH3D,
     &                 MESH3D%NSEG,NSEG2D,NPLAN,NPOIN2)
C
      ELSE
C
        WRITE(LU,*) 'UNKNOWN STORAGE FOR MDIFF IN GETTRI'
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C