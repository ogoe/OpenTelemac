C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CHANGES THE DISCRETISATION OF A VECTOR
!>                FROM 11 TO 13 HERE (LINEAR TO QUADRATIC).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIM1, DIM2, IKLE, NELEM, NELMAX, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IDIM, IELEM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CHGDIS()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 06/02/08
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DIM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR A MODIFIER.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CG1113
     &(X,DIM1,DIM2,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIM1           |---| 
C| DIM2           |---| 
C| IKLE           |-->| TABLE DE CONNECTIVITE.
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS.
C| X             |<--| VECTEUR A MODIFIER.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,DIM1,DIM2
      DOUBLE PRECISION, INTENT(INOUT) :: X(DIM1,DIM2)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,6)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IDIM
C
C-----------------------------------------------------------------------
C
CVOCL LOOP,NOVREC
CDIR$ IVDEP
      DO IDIM  = 1 , DIM2
      DO IELEM = 1 , NELEM
C
        X(IKLE(IELEM,4),IDIM) = 0.5D0 * ( X(IKLE(IELEM,1),IDIM)
     &                                  + X(IKLE(IELEM,2),IDIM) )
        X(IKLE(IELEM,5),IDIM) = 0.5D0 * ( X(IKLE(IELEM,2),IDIM)
     &                                  + X(IKLE(IELEM,3),IDIM) )
        X(IKLE(IELEM,6),IDIM) = 0.5D0 * ( X(IKLE(IELEM,3),IDIM)
     &                                  + X(IKLE(IELEM,1),IDIM) )
C
      ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C