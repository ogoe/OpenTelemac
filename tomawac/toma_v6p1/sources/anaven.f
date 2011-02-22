C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SPECIFIES AN ANALYTICAL WIND
!>               (CAN BE VARIABLE IN TIME).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!>  @code
!>      DO IP=1,NPOIN2
!>        UV(IP)=VX_CTE
!>        VV(IP)=VY_CTE
!>      ENDDO
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DDC, NPOIN2, UV, VV, VX_CTE, VY_CTE, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IP
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIW(), SEMIMP()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 07/06/95
!> </td><td> M. BENOIT (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS DU CALCUL
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DE DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>UV,VV
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE VENT INITIAL
!>    </td></tr>
!>          <tr><td>VX_CTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VY_CTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE 2D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ANAVEN
     &( UV    , VV    , X     , Y     , NPOIN2, AT    , DDC   , VX_CTE,
     &  VY_CTE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS DU CALCUL
C| DDC            |-->| DATE DE DEBUT DU CALCUL
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| UV,VV          |<--| COMPOSANTES DU CHAMP DE VENT INITIAL
C| VX_CTE         |---| 
C| VY_CTE         |---| 
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE 2D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION AT    , DDC   , VX_CTE, VY_CTE
      DOUBLE PRECISION X (NPOIN2)    , Y (NPOIN2)
      DOUBLE PRECISION UV(NPOIN2)    , VV(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  IP
C
C
      DO IP=1,NPOIN2
        UV(IP)=VX_CTE
        VV(IP)=VY_CTE
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C