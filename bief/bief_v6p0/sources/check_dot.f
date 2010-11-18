C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IN PARALLEL MODE, PRINTS THE EUCLIDIAN NORM OF A VECTOR,
!>                WHICH HAS NOT BEEN ASSEMBLED WITH PARCOM. E.G. A RIGHT
!>                HAND SIDE BEFORE CALLING SOLVE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MESH, T, TEXTE, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), P_DOTS()
!>   </td></tr>
!>     </table>

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
!> </td><td> 18/11/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MESH STRUCTURE
!>    </td></tr>
!>          <tr><td>T
!></td><td><-></td><td>A WORK BIEF_OBJ STRUCTURE
!>    </td></tr>
!>          <tr><td>TEXTE
!></td><td>--></td><td>A TEXT TO BE PRINTED
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>THE BIEF_OBJ STRUCTURE WITH THE VECTOR
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CHECK_DOT
     &(X,T,TEXTE,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MESH           |-->| MESH STRUCTURE
C| T             |<->| A WORK BIEF_OBJ STRUCTURE
C| TEXTE          |-->| A TEXT TO BE PRINTED
C| X             |-->| THE BIEF_OBJ STRUCTURE WITH THE VECTOR
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=*) :: TEXTE
      TYPE(BIEF_OBJ)   :: X,T
      TYPE(BIEF_MESH)  :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CALL OS('X=Y     ',X=T,Y=X)
      IF(NCSIZE.GT.1) CALL PARCOM(T,2,MESH)
      WRITE(LU,*) TEXTE,'=',P_DOTS(T,T,MESH)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C