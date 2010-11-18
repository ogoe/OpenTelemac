C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE SETTLING VELOCITY AS A FUNCTION
!>                OF TEMPERATURE, SALINITY AND CONCENTRATION OF
!>                SUSPENDED SEDIMENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> WCHU_CONST, W_SED
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 01/08/91
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES IN THE 3D MESH
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN THE 2D MESH
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS IN THE 3D MESH
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NUMBER OF ARRAYS IN BLOCK PRIVE
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NUMBER OF TRACERS
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>BLOCK OF ARRAYS FOR THE USER
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>TRACERS
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPONENTS OF VELOCITY
!>    </td></tr>
!>          <tr><td>WC
!></td><td><--</td><td>SETTLING VELOCITY
!>    </td></tr>
!>          <tr><td>WCHU_CONST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W_SED
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDINATES OF POINTS IN THE MESH
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VITCHU
     & (W_SED, WCHU_CONST)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH
C| NPOIN2         |-->| NUMBER OF POINTS IN THE 2D MESH
C| NPOIN3         |-->| NUMBER OF POINTS IN THE 3D MESH
C| NPRIV          |-->| NUMBER OF ARRAYS IN BLOCK PRIVE
C| NTRAC          |-->| NUMBER OF TRACERS
C| PRIVE          |-->| BLOCK OF ARRAYS FOR THE USER
C| TA             |-->| TRACERS
C| U,V,W          |-->| COMPONENTS OF VELOCITY
C| WC             |<--| SETTLING VELOCITY
C| WCHU_CONST     |---| 
C| W_SED          |---| 
C| X,Y,Z          |-->| COORDINATES OF POINTS IN THE MESH
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)  :: WCHU_CONST
      TYPE(BIEF_OBJ), INTENT(INOUT) :: W_SED
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     CONSTANT VALUE GIVEN HERE
!
      CALL OS( 'X=C     ' , X=W_SED , C=WCHU_CONST )
!
      RETURN
      END SUBROUTINE VITCHU

C
C#######################################################################
C