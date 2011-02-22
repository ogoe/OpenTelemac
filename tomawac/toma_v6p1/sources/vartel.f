C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOWS THE USE OF A VARIABLE READ FROM A TELEMAC FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEPTH, F, NF, NPLAN, NPOIN2, TRA01, UC, VAR, VC, X, Y, ZREPOS
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!> </td><td> 09/06/95
!> </td><td> F. MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEPTH
!></td><td><-></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>F
!></td><td><-></td><td>SPECTRE DE VARIANCE
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>UC,VC
!></td><td><-></td><td>CHAMPS DE COURANT
!>    </td></tr>
!>          <tr><td>VAR
!></td><td>--></td><td>VARIABLE RECUPEREE DANS LE FICHIER TELEMAC
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>ZREPOS
!></td><td><-></td><td>COTE INITIALE DU PLAN D'EAU AU REPOS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VARTEL
     &( VAR, X, Y, DEPTH, UC, VC, ZREPOS, TRA01, F, NPLAN, NF, NPOIN2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEPTH          |<->| HAUTEUR D'EAU
C| F             |<->| SPECTRE DE VARIANCE
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| TRA01          |<->| TABLEAU DE TRAVAIL
C| UC,VC          |<->| CHAMPS DE COURANT
C| VAR            |-->| VARIABLE RECUPEREE DANS LE FICHIER TELEMAC
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE 2D
C| ZREPOS         |<->| COTE INITIALE DU PLAN D'EAU AU REPOS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER  NPOIN2,NPLAN,NF
C
      DOUBLE PRECISION F (NPOIN2,NPLAN,NF) , TRA01(NPOIN2)
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2) , DEPTH(NPOIN2)
      DOUBLE PRECISION UC(NPOIN2) , VC(NPOIN2) , VAR(NPOIN2)
      DOUBLE PRECISION ZREPOS
C
C-----------------------------------------------------------------------
C
C     USE THE VARIABLE 'VAR' TO INITIALISES THE ARRAYS
C
C     FOR EXAMPLE :
C
C     CALL OV( 'X=Y     ' , DEPTH , VAR , X , 0.D0 , NPOIN2)
C
      RETURN
      END
C
C#######################################################################
C