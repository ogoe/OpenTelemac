C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES OUT ADDITIONAL OUTPUT REQUIRED BY THE USER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note         THIS SUBROUTINE IS CALLED IN THE SAME PLACES AS THE
!>                MAIN TELEMAC2D OUTPUT SUBROUTINE (NAMED DESIMP),
!>                I.E. CALLED TWICE:
!>
!>  @note   (1) ONCE PER RUN, WHEN LTL==0, INDEPENDENTLY OF WHETHER
!>             'OUTPUT OF INITIAL CONDITIONS : YES' IS SET OR NOT
!>  @note   (2) EACH TIME STEP JUST AFTER DESIMP-OUTPUT

!>  @warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ATL, GRADEBL, GRAPRDL, LISDEBL, LISPRDL, LTL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
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
!>      <td><center> 5.4                                       </center>
!> </td><td> **/08/2003
!> </td><td> JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ATL
!></td><td>--></td><td>TEMPS DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>GRADEBL
!></td><td>--></td><td>1ER PAS DE TEMPS POUR LES SORTIES GRAPHIQUES
!>    </td></tr>
!>          <tr><td>GRAPRDL
!></td><td>--></td><td>PERIODE DE SORTIE SUR LE FICHIER DE RESULTAT
!>    </td></tr>
!>          <tr><td>LISDEBL
!></td><td>--></td><td>1ER PAS DE TEMPS POUR LES SORTIES LISTING
!>    </td></tr>
!>          <tr><td>LISPRDL
!></td><td>--></td><td>PERIODE DE SORTIE LISTING
!>    </td></tr>
!>          <tr><td>LTL
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE UTIMP_TELEMAC2D
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ATL            |-->| TEMPS DU PAS DE TEMPS
C| GRADEBL        |-->| 1ER PAS DE TEMPS POUR LES SORTIES GRAPHIQUES
C| GRAPRDL        |-->| PERIODE DE SORTIE SUR LE FICHIER DE RESULTAT
C| LISDEBL        |-->| 1ER PAS DE TEMPS POUR LES SORTIES LISTING
C| LISPRDL        |-->| PERIODE DE SORTIE LISTING
C| LTL            |-->| NUMERO DU PAS DE TEMPS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
!
!***********************************************************************
C USER OUTPUT
!
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP_TELEMAC2D
C
C#######################################################################
C