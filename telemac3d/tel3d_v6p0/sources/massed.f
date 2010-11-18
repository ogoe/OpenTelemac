C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PERFORMS INITIAL RELATIVE MASS BALANCE FOR
!>                THE SEDIMENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFDEP, CONC, EPAI, GIBSON, HDEP, IELM3, IKLE2, IVIDE, MASKEL, MASSE, MESH3, MSK, NCOUCH, NELEM2, NELEM3, NPF, NPFMAX, NPLAN, NPOIN2, NPOIN3, NTRAC, NVBIL, RHOS, S, SURFAC, TA, TASSE, TRA02, TRAV1, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM2, IPF, IPOIN, L1, L2, L3, MASSE1, MASSE6
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 26/08/92
!> </td><td> C.LE NORMANT(LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFDEP
!></td><td>--></td><td>CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
!>    </td></tr>
!>          <tr><td>CONC
!></td><td>--></td><td>CONCENTRATIONS DES COUCHES DU FOND VASEUX
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td>--></td><td>TAILLE DES MAILLES DU FOND EN
!>                  COORDONNEES MATERIELLES (EPAI=DZ/(1+IVIDE))
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>LOGIQUE POUR MODELE DE GIBSON
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td>--></td><td>HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TABLE DE CONNECTIVITE POUR LES POINTS DU FOND
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td>--></td><td>INDICE DES VIDES AUX POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS SECS
!>    </td></tr>
!>          <tr><td>MASSE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBVIL
!></td><td>--></td><td>DIMENSION DU TABLEAU MASSE
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NOMBRE D'ELEMENTS 3D
!>    </td></tr>
!>          <tr><td>NPF
!></td><td>--></td><td>NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!>                  DISCRETISANT LE FOND VASEUX
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS SUR LA VERTICALE
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS ACTIFS
!>    </td></tr>
!>          <tr><td>NVBIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>MASSE VOLUMIQUE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACES DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>CONCENTRATION DU SEDIMENT EN SUSPENSION
!>    </td></tr>
!>          <tr><td>TASSE
!></td><td>--></td><td>LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV1,02
!></td><td><-></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MASSED
     & (MASSE, TA, X, Y, Z, IVIDE, EPAI, CONC, HDEP,
     & SURFAC, TRAV1, TRA02, S, IKLE2, MESH3, IELM3,
     & NPLAN, NELEM2, NELEM3, NPOIN2, NPOIN3, NTRAC, NVBIL,
     & NPFMAX, NCOUCH, NPF, TASSE, GIBSON, RHOS, CFDEP, MSK, MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFDEP          |-->| CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
C| CONC           |-->| CONCENTRATIONS DES COUCHES DU FOND VASEUX
C| EPAI           |-->| TAILLE DES MAILLES DU FOND EN
C|                |   | COORDONNEES MATERIELLES (EPAI=DZ/(1+IVIDE))
C| GIBSON         |-->| LOGIQUE POUR MODELE DE GIBSON
C| HDEP           |-->| HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
C| IELM3          |---| 
C| IKLE2          |-->| TABLE DE CONNECTIVITE POUR LES POINTS DU FOND
C| IVIDE          |-->| INDICE DES VIDES AUX POINTS DU MAILLAGE
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS SECS
C| MASSE          |---| 
C| MESH3          |---| 
C| MSK            |---| 
C| NBVIL          |-->| DIMENSION DU TABLEAU MASSE
C| NCOUCH         |---| 
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
C| NPF            |-->| NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX
C| NPLAN          |-->| NOMBRE DE PLANS SUR LA VERTICALE
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| NVBIL          |---| 
C| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
C| S             |---| 
C| SURFAC         |-->| SURFACES DES ELEMENTS.
C| TA             |-->| CONCENTRATION DU SEDIMENT EN SUSPENSION
C| TASSE          |-->| LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
C| TRA02          |---| 
C| TRAV1,02       |<->| TABLEAUX DE TRAVAIL
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPFMAX, NELEM2, NELEM3, NPOIN2, NPOIN3
      INTEGER, INTENT(IN)    :: IELM3, NPLAN, NCOUCH
      INTEGER, INTENT(IN)    :: NTRAC, NVBIL
      INTEGER, INTENT(IN)    :: IKLE2(NELEM2,3), NPF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSE(NVBIL)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3) ,Y(NPOIN3) ,Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: HDEP(NPOIN2), CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(IN)      :: TA   ! CALLED AS TA%ADR(NTRAC)%
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TRAV1, S
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH3
      DOUBLE PRECISION, INTENT(IN)    :: RHOS, CFDEP
      LOGICAL, INTENT(IN)             :: MSK, TASSE, GIBSON
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER L1, L2, L3, IPOIN, IPF, IELEM2
      DOUBLE PRECISION MASSE1, MASSE6
!
!=======================================================================
!
C MASS OF MUDDY DEPOSITS ON THE RIGID BED (MASSE6)
!
!=======================================================================
!
      CALL OV('X=CY    ', TRA02 , HDEP , Z , CFDEP , NPOIN2)
!
      IF(TASSE) THEN
        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=0.D0
          DO IPF=1,NCOUCH
            TRA02(IPOIN)=TRA02(IPOIN)+CONC(IPF)*EPAI(IPF,IPOIN)
          ENDDO
        ENDDO
      ELSEIF(GIBSON) THEN
        DO IPOIN=1,NPOIN2
          DO IPF=1,NPF(IPOIN)-1
            TRA02(IPOIN)=TRA02(IPOIN)+RHOS*EPAI(IPF,IPOIN)
          ENDDO
        ENDDO
      ENDIF
      MASSE6=0.D0
      DO IELEM2=1,NELEM2
        L1=IKLE2(IELEM2,1)
        L2=IKLE2(IELEM2,2)
        L3=IKLE2(IELEM2,3)
        MASSE6=MASSE6+SURFAC(IELEM2)
     &                            *(TRA02(L1)+TRA02(L2)+TRA02(L3))/3.D0
      ENDDO
      WRITE(LU,*)
     &      'MASSE INITIALE DES DEPOTS VASEUX :                       ',
     &                MASSE6
!
!=======================================================================
!
C TOTAL MASS OF SEDIMENT IN THE DOMAIN (MASSE1)
!
!=======================================================================
!
      MASSE1=MASSE(1+NTRAC)+MASSE6
            WRITE(LU,*)
     &      'MASSE TOTALE DE SEDIMENTS DANS LE DOMAINE:               ',
     &                MASSE1
!
!=======================================================================
!
      RETURN
      END
C
C#######################################################################
C