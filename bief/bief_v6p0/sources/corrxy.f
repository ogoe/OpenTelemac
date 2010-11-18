C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE COORDINATES OF THE POINTS IN THE MESH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; COMMENTED LINES ARE AN EXAMPLE
!>  @code
!>  EXAMPLE : MULTIPLIES BY A CONSTANT (SCALES THE MESH)
!>            CHANGES THE ORIGIN
!>
!>      DO I = 1 , NPOIN
!>         X(I) = 3.D0 * X(I) + 100.D0
!>         Y(I) = 5.D0 * Y(I) - 50.D0
!>      ENDDO
!>  @endcode

!>  @warning  DO NOT PERFORM ROTATIONS AS IT WILL CHANGE
!>            THE NUMBERING OF THE LIQUID BOUNDARIES

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NPOIN, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CORRXY
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>READGEO3()

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
!> </td><td> 17/10/05
!> </td><td> EMILE RAZAFINDRAKOTO (LNHE) 01 30 87 74 03
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE .
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORRXY
     & (X,Y,NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| X,Y            |-->| COORDONNEES DU MAILLAGE .
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CORRXY => CORRXY
C
C
C     OTHER DATA ARE AVAILABLE WITH THE DECLARATIONS OF EACH PROGRAM
C
C     USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
C     THE CALLING PROGRAM AND THE NEEDED MODIFICATION
C     BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
C     ALL THE DATA STRUCTURE OF THIS CODE IS AVAILABLE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
       INTEGER I
C
C-----------------------------------------------------------------------
C
C  EXAMPLE : MULTIPLIES BY A CONSTANT (SCALES THE MESH)
C            CHANGES THE ORIGIN
C
C      DO I = 1 , NPOIN
C         X(I) = 3.D0 * X(I) + 100.D0
C         Y(I) = 5.D0 * Y(I) - 50.D0
C      ENDDO
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'CORRXY (BIEF) : PAS DE MODIFICATION DES COORDONNEES'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*)'CORRXY (BIEF):NO MODIFICATION OF COORDINATES'
        WRITE(LU,*)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C