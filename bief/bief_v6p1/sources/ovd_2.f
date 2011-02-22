C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BETWEEN OS AND OVD WHEN 2-DIMENSION VECTORS ARE INVOLVED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, DIM1, DIMX, DIMY, DIMZ, INFINI, IOPT, NPOIN, OP, X, Y, Z, ZERO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OVD()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>OS()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 29/11/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>DIM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFINI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IOPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>CHAINE DE CARACTERES INDIQUANT L'OPERATION
!>                  A EFFECTUER.
!>    </td></tr>
!>          <tr><td>X , DIMX
!></td><td><--</td><td>STRUCTURE RESULTAT ET DIMENSION A TRAITER
!>    </td></tr>
!>          <tr><td>Y , DIMY
!></td><td>--></td><td>STRUCTURE OPERANDE ...
!>    </td></tr>
!>          <tr><td>Z , DIMZ
!></td><td>--></td><td>STRUCTURE OPERANDE ...
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE OVD_2
     & ( OP , X , DIMX , Y , DIMY , Z , DIMZ , C , DIM1 , NPOIN ,
     &   IOPT , INFINI, ZERO )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| DIM1           |---| 
C| INFINI         |---| 
C| IOPT           |---| 
C| NPOIN          |---| 
C| OP             |-->| CHAINE DE CARACTERES INDIQUANT L'OPERATION
C|                |   | A EFFECTUER.
C| X , DIMX       |<--| STRUCTURE RESULTAT ET DIMENSION A TRAITER
C| Y , DIMY       |-->| STRUCTURE OPERANDE ...
C| Z , DIMZ       |-->| STRUCTURE OPERANDE ...
C| ZERO           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN)    :: DIMX,DIMY,DIMZ,DIM1,NPOIN,IOPT
      DOUBLE PRECISION, INTENT(IN)    :: C,INFINI,ZERO
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(INOUT) :: X(DIM1,*)
      DOUBLE PRECISION, INTENT(IN)    :: Y(DIM1,*),Z(DIM1,*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CALL OVD( OP , X(1,DIMX) , Y(1,DIMY) , Z(1,DIMZ) , C , NPOIN ,
     &          IOPT , INFINI , ZERO )
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C