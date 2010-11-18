C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SPECIFIES A ! STATIONARY ! ANALYTICAL CURRENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!>  @code
!>      UCONST=0.D0
!>      VCONST=0.D0
!>
!>      DO 100 IP=1,NPOIN2
!>        UC(IP)=UCONST
!>        VC(IP)=VCONST
!>  100 CONTINUE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NPOIN2, UC, VC, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IP, UCONST, VCONST
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIW()

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
!> </td><td> 07/06/2001
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>UC,VC
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE COURANT
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE 2D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ANACOS
     &( UC    , VC    , X     , Y     , NPOIN2 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| UC,VC          |<--| COMPOSANTES DU CHAMP DE COURANT
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
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION UC(NPOIN2) , VC(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UCONST, VCONST
C
C
      UCONST=0.D0
      VCONST=0.D0
C
      DO 100 IP=1,NPOIN2
        UC(IP)=UCONST
        VC(IP)=VCONST
  100 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C