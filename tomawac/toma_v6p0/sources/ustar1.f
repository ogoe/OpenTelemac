C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRICTION VELOCITY AND ROUGHNESS LENGTH
!>                FOR ALL THE NODES IN THE 2D MESH.
!>                BASED ON JANSSEN (1989, 1991).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   COMPUTES TAUT FROM UVENT AND TAUW IN SUBROUTINE 'TAUTOT'.

!>  @reference JANSSEN P.A.E.M (1989) :
!>                     "WIND-INDUCED STRESS AND THE DRAG OF AIR
!>                      FLOW OVER SEA WAVES". JPO, VOL 19, PP 745-754.

!>  @reference JANSSEN P.A.E.M (1991) :
!>                     "QUASI-LINEAR THEORY OF WIND-WAVE GENERATION
!>                      APPLIED TO WAVE FORECASTING". JPO, VOL 21, PP 1631-1642.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALPHA, CDRAG, GRAVIT, NPOIN2, TAUWAV, USTAR, UV, VV, XKAPPA, Z0, ZVENT
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IP, ITR, ITRMAX, ITRMIN, SEUIL, TAUT, TAUW, USMIN, USTEMP, UVENT, X
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> TAUTOT()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SEMIMP(), WAC()

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
!> </td><td> 25/04/95
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHA
!></td><td>--></td><td>CONSTANTE DE LA LOI DE CHARNOCK
!>    </td></tr>
!>          <tr><td>CDRAG
!></td><td>--></td><td>COEFFICIENT DE TRAINEE
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>TAUWAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUWAV(
!></td><td>--></td><td>TABLEAU DES CONTRAINTES DUES A LA HOULE
!>    </td></tr>
!>          <tr><td>USTAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USTAR(
!></td><td><--</td><td>TABLEAU DES VITESSES DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>UV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UV(
!></td><td>--></td><td>TABLEAU DES COMPOSANTES OUEST-EST DU VENT
!>    </td></tr>
!>          <tr><td>VV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VV(
!></td><td>--></td><td>TABLEAU DES COMPOSANTES SUD-NORD  DU VENT
!>    </td></tr>
!>          <tr><td>XKAPPA
!></td><td>--></td><td>CONSTANTE DE VON KARMAN
!>    </td></tr>
!>          <tr><td>Z0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z0(
!></td><td><--</td><td>TABLEAU DES LONGUEURS DE RUGOSITE
!>    </td></tr>
!>          <tr><td>ZVENT
!></td><td>--></td><td>COTE A LAQUELLE EST MESURE LE VENT (M)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE USTAR1
     &( USTAR , Z0    , TAUWAV, UV    , VV    , CDRAG , ALPHA , XKAPPA,
     &  ZVENT , GRAVIT, NPOIN2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHA          |-->| CONSTANTE DE LA LOI DE CHARNOCK
C| CDRAG          |-->| COEFFICIENT DE TRAINEE
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| TAUWAV         |---| 
C| TAUWAV(        |-->| TABLEAU DES CONTRAINTES DUES A LA HOULE
C| USTAR          |---| 
C| USTAR(         |<--| TABLEAU DES VITESSES DE FROTTEMENT
C| UV             |---| 
C| UV(            |-->| TABLEAU DES COMPOSANTES OUEST-EST DU VENT
C| VV             |---| 
C| VV(            |-->| TABLEAU DES COMPOSANTES SUD-NORD  DU VENT
C| XKAPPA         |-->| CONSTANTE DE VON KARMAN
C| Z0             |---| 
C| Z0(            |<--| TABLEAU DES LONGUEURS DE RUGOSITE
C| ZVENT          |-->| COTE A LAQUELLE EST MESURE LE VENT (M)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION USTAR(NPOIN2) , Z0(NPOIN2) , TAUWAV(NPOIN2)
      DOUBLE PRECISION    UV(NPOIN2) , VV(NPOIN2)
      DOUBLE PRECISION CDRAG , ALPHA , XKAPPA , ZVENT, GRAVIT
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  ITRMIN, ITRMAX, ITR   , IP
      DOUBLE PRECISION TAUT  , UVENT , TAUW  , USMIN , SEUIL , X
      DOUBLE PRECISION USTEMP
C
C
      USMIN =1.D-6
      SEUIL =1.D-7
      ITRMIN=1
      ITRMAX=15
C
C.....MAIN LOOP ON THE NODES OF THE 2D MESH
C     """""""""""""""""""""""""""""""""""""""""""""""""""""
      DO IP=1,NPOIN2
C
C.......COMPUTES THE TOTAL STRESS
C       """""""""""""""""""""""""""""""
        UVENT=SQRT(UV(IP)**2+VV(IP)**2)
        TAUW =TAUWAV(IP)
        CALL TAUTOT
     &( TAUT  , UVENT , TAUW  , CDRAG , ALPHA , XKAPPA, ZVENT , SEUIL ,
     &  GRAVIT, ITR   , ITRMIN, ITRMAX)
C
C.......COMPUTES THE FRICTION VELOCITY
C       """""""""""""""""""""""""""""""""""
        USTAR(IP)=SQRT(TAUT)
C
C.......COMPUTES TEH ROUGHNESS LENGTH
C       """"""""""""""""""""""""""""""""""
        USTEMP=MAX(USTAR(IP),USMIN)
        X     =MIN(TAUWAV(IP)/USTEMP**2,0.999D0)
        Z0(IP)=ALPHA*USTEMP**2/(GRAVIT*SQRT(1.D0-X))
C
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C