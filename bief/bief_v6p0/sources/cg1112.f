C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CHANGES THE DISCRETISATION OF A VECTOR
!>                FROM 11 TO 12 HERE.

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
!>    </th><td> IDIM, IELEM, TIERS
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 09/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
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
                        SUBROUTINE CG1112
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
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,4)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IDIM
C
      DOUBLE PRECISION TIERS
C
C-----------------------------------------------------------------------
C
      TIERS = 1.D0/3.D0
C
C-----------------------------------------------------------------------
C
CVOCL LOOP,NOVREC
CDIR$ IVDEP
      DO 20 IDIM  = 1 , DIM2
      DO 10 IELEM = 1 , NELEM
C
        X(IKLE(IELEM,4),IDIM) = TIERS * ( X(IKLE(IELEM,1),IDIM)
     &                                  + X(IKLE(IELEM,2),IDIM)
     &                                  + X(IKLE(IELEM,3),IDIM) )
C
10    CONTINUE
20    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C