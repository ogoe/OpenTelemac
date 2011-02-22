C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SPECIFIES AN ANALYTICAL TIDE :
!>                WATER LEVEL AND CURRENT SPEED ARE VARIABLE IN TIME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!>  @code
!>      UCONST=0.D0
!>      VCONST=0.D0
!>
!>      DO 100 IP=1,NPOIN2
!>        UC(IP)   = UCONST
!>        VC(IP)   = VCONST
!>        ZM(IP)   = 0.D0
!>        DZHDT(IP)= 0.D0
!>  100 CONTINUE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DDC, DZHDT, LT, NPOIN2, UC, VC, X, Y, ZM, ZM1, ZM2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IP, J, UCONST, VCONST
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIW(), CORMAR()

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
!>      <td><center> 5.0                                       </center>
!> </td><td>
!> </td><td>
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
!>          <tr><td>DZHDT
!></td><td><--</td><td>VARIATION TEMPORELLE DE LA HAUTEUR DE MAREE
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>UC,VC
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE COURANT DE LA MAREE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>ZM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZM1
!></td><td><--</td><td>HAUTEUR DE LA MAREE PAR RAPPORT A ZREPOS A T
!>    </td></tr>
!>          <tr><td>ZM2
!></td><td><--</td><td>HAUTEUR DE LA MAREE PAR RAPPORT A ZREPOS A T2
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ANAMAR
     &( UC  , VC  , ZM  , ZM1 , ZM2 , DZHDT , X  , Y  , NPOIN2 ,
     &  AT  , DDC , LT  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS DU CALCUL
C| DDC            |-->| DATE DE DEBUT DU CALCUL
C| DZHDT          |<--| VARIATION TEMPORELLE DE LA HAUTEUR DE MAREE
C| LT             |---| 
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| UC,VC          |<--| COMPOSANTES DU CHAMP DE COURANT DE LA MAREE
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE 2D
C| ZM             |---| 
C| ZM1            |<--| HAUTEUR DE LA MAREE PAR RAPPORT A ZREPOS A T
C| ZM2            |<--| HAUTEUR DE LA MAREE PAR RAPPORT A ZREPOS A T2
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
      INTEGER  LT
      DOUBLE PRECISION AT    , DDC
      DOUBLE PRECISION X (NPOIN2), Y (NPOIN2), ZM1(NPOIN2), ZM2(NPOIN2)
      DOUBLE PRECISION UC(NPOIN2), VC(NPOIN2), DZHDT(NPOIN2),ZM(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER          IP, I, J
      DOUBLE PRECISION UCONST, VCONST
C
C-----------------------------------------------------------------------
C     EXAMPLE 1
C-----------------------------------------------------------------------
C
      UCONST=0.D0
      VCONST=0.D0
C
      DO 100 IP=1,NPOIN2
        UC(IP)   = UCONST
        VC(IP)   = VCONST
        ZM(IP)   = 0.D0
        DZHDT(IP)= 0.D0
  100 CONTINUE
C
C
      RETURN
      END
C
C#######################################################################
C