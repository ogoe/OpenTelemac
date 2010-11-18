C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       REDEFINES THE CHARACTERISTICS OF THE SOURCES
!>                WITHOUT USING THE STEERING FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE
!>  @code
!>     EXAMPLE
!>
!>     NREJET = ?  (UNTIL MAXSCE)
!>     NREJEU = NREJET (IF VELOCITIES GIVEN)
!>
!>     DO I=1,NREJET
!>
!>       XSCE(I) = ???
!>       YSCE(I) = ???
!>       DSCE(I) = ???
!>       TSCE(I) = ???
!>       USCE(I) = ???
!>       VSCE(I) = ???
!>
!>     ENDDO
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> 26/10/1994
!> </td><td> J-M HERVOUET LNH 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DSCE
!></td><td><--</td><td>DEBITS DES SOURCES (ENTRANT POSITIF)
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS DES NOEUDS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>MAXSCE
!></td><td>--></td><td>TAILLE MAXI DES TABLEAUX QUI FINISSENT EN SCE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NREJET
!></td><td><--</td><td>NOMBRE DE SOURCES/PUITS
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU DE TRAVAIL DE L'UTILISATEUR.
!>    </td></tr>
!>          <tr><td>TSCE
!></td><td><--</td><td>VALEURS DU TRACEUR AUX SOURCES (FACULTATIF)
!>                  TSCE N'EST PRIS EN COMPTE QUE SI LE DEBIT
!>                  DE LA SOURCE EST ENTRANT (POSITIF).
!>    </td></tr>
!>          <tr><td>USCE,VSCE
!></td><td><--</td><td>VITESSES AUX SOURCES.
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XSCE,YSCE
!></td><td><--</td><td>COORDONNEES DES SOURCES.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>FOND.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SOURCE_TELEMAC2D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DSCE           |<--| DEBITS DES SOURCES (ENTRANT POSITIF)
C| IKLE           |-->| NUMEROS DES NOEUDS DE CHAQUE ELEMENT.
C| MAXSCE         |-->| TAILLE MAXI DES TABLEAUX QUI FINISSENT EN SCE
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NREJET         |<--| NOMBRE DE SOURCES/PUITS
C| PRIVE          |-->| TABLEAU DE TRAVAIL DE L'UTILISATEUR.
C| TSCE           |<--| VALEURS DU TRACEUR AUX SOURCES (FACULTATIF)
C|                |   | TSCE N'EST PRIS EN COMPTE QUE SI LE DEBIT
C|                |   | DE LA SOURCE EST ENTRANT (POSITIF).
C| USCE,VSCE      |<--| VITESSES AUX SOURCES.
C| X,Y            |-->| COORDONNEES DU MAILLAGE.
C| XSCE,YSCE      |<--| COORDONNEES DES SOURCES.
C| ZF             |-->| FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C     USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
C
      INTEGER I
C
C-----------------------------------------------------------------------
C
C     EXAMPLE
C
C     NREJET = ?  (UNTIL MAXSCE)
C     NREJEU = NREJET (IF VELOCITIES GIVEN)
C
C     DO I=1,NREJET
C
C       XSCE(I) = ???
C       YSCE(I) = ???
C       DSCE(I) = ???
C       TSCE(I) = ???
C       USCE(I) = ???
C       VSCE(I) = ???
C
C     ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C