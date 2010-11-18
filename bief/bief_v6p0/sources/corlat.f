C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE LATITUDE OF THE POINTS IN THE MESH.
!><br>            CASE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; LINES COMMENTED WITH 'CEX' ARE AN EXAMPLE IN TELEMAC-2D
!>  @code
!>  EXAMPLE : MULTIPLIES BY A CONSTANT
!>
!>    DO I = 1 , NPOIN
!>      X(I) = X(I) * 1.D0
!>      Y(I) = Y(I) * 1.D0
!>    ENDDO
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CORLAT
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 01/03/90
!> </td><td> J-M HERVOUET
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
                        SUBROUTINE CORLAT
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| X,Y            |-->| COORDONNEES DU MAILLAGE .
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CORLAT => CORLAT
CEX   USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
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
CEX   INTEGER I
C
C-----------------------------------------------------------------------
C
C  EXAMPLE : MULTIPLIES BY A CONSTANT
C
CEX   DO I = 1 , NPOIN
CEX     X(I) = X(I) * 1.D0
CEX     Y(I) = Y(I) * 1.D0
CEX   ENDDO
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CORLAT : PAS DE MODIFICATION DE LA LATITUDE'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CORLAT :NO MODIFICATION OF LATITUDE'
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C