C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DEFINES ZONES IN THE MESH. THE RESULT MUST BE :
!><br>            NZONE : THE NUMBER OF ZONES,
!><br>            ZONE : STRUCTURE OF SIZE NPOIN STATING THE ZONE NUMBER
!>                       OF EVERY POINT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D(), TELEMAC2D()

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
!>      <td><center> 5.3                                       </center>
!> </td><td> 17/08/2001
!> </td><td> J-M HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td><--</td><td>MATRICE
!>    </td></tr>
!>          <tr><td>LISFON
!></td><td>--></td><td>NOMBRE DE LISSAGES DU FOND.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU PRIVE POUR L'UTILISATEUR.
!>    </td></tr>
!>          <tr><td>T1,2
!></td><td>--></td><td>TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
!>    </td></tr>
!>          <tr><td>W1
!></td><td>--></td><td>TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
!>    </td></tr>
!>          <tr><td>X,Y,(Z)
!></td><td>--></td><td>COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><-></td><td>FOND A MODIFIER.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DEF_ZONES
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<--| MATRICE
C| LISFON         |-->| NOMBRE DE LISSAGES DU FOND.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| PRIVE          |-->| TABLEAU PRIVE POUR L'UTILISATEUR.
C| T1,2           |-->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C| W1             |-->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C| X,Y,(Z)        |-->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C| ZF             |<->| FOND A MODIFIER.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C-----------------------------------------------------------------------
C
C     NZONE = ???
C     ZONE%I(I) = ???
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C