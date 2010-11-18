C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE RELATIVE MASS BALANCE FOR THE
!>                SEDIMENT DURING A TIMESTEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, CFDEP, CONC, DT, EPAI, FLUER, GIBSON, HDEP, IKLE2, INFO, IVIDE, LT, MASSE1, NCOUCH, NELEM2, NPF, NPFMAX, NPOIN2, NPOIN3, NTRAC, NVBIL, PDEPOT, RHOS, SURFAC, TA, TASSE, TRA01, TRA02, U, V, W, WC, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FLUX, IELEM2, IPF, IPLAN, IPOIN, IPTFR, L1, L2, L3, MASSE3, MASSE4, MASSE5, MASSE6, MTOTAL
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
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>CFDEP
!></td><td>--></td><td>CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
!>    </td></tr>
!>          <tr><td>CONC
!></td><td>--></td><td>CONCENTRATION DES COUCHES DU FOND
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td>--></td><td>TAILLE DES MAILLES DU FOND EN
!>                  COORDONNEES MATERIELLES (EPAI=DZ/(1+IVIDE))
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td>--></td><td>FLUX D'EROSION EN CHAQUE POINT 2D
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>LOGIQUE POUR MODELE DE GIBSON
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td>--></td><td>HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TABLE DE CONNECTIVITE POUR LES POINTS DU FOND
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON FAIT LES IMPRESSIONS
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td>--></td><td>INDICE DES VIDES AUX POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>MASK
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>MASSE1
!></td><td><-></td><td>MASSE DU SEDIMENTEN SUSPENSION
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
!>                  (MODELE DE TASSEMENT MULTICOUCHES)
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NPF
!></td><td>--></td><td>NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!>                  DISCRETISANT LE FOND VASEUX(MODELE DE GIBSON
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
!>          <tr><td>PDEPOT
!></td><td>--></td><td>PROBABILITE DE DEPOT EN CHAQUE POINT 2D
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>MASSE VOLUMIQUE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACES DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>TA(1,NTRAC)
!></td><td>--></td><td>CONCENTRATION DU SEDIMANT EN SUSPENSION
!>    </td></tr>
!>          <tr><td>TASSE
!></td><td>--></td><td>LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
!>    </td></tr>
!>          <tr><td>TRA01,02
!></td><td><-></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>VITESSE AU PAS DE TEMPS PRESENT
!>    </td></tr>
!>          <tr><td>WC
!></td><td>--></td><td>VITESSE DE CHUTE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SED3D
     & (MASSE1,U,V,W,WC,TA,X,Y,Z,
     &  IVIDE,EPAI,HDEP,CONC,FLUER,PDEPOT,SURFAC,TRA01,TRA02,
     &  IKLE2,NELEM2,NPOIN2,NPOIN3,NTRAC,NVBIL,NPFMAX,NCOUCH,
     &  NPF,LT,AT,DT,INFO,TASSE,GIBSON,RHOS,CFDEP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| CFDEP          |-->| CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
C| CONC           |-->| CONCENTRATION DES COUCHES DU FOND
C| DT             |-->| PAS DE TEMPS
C| EPAI           |-->| TAILLE DES MAILLES DU FOND EN
C|                |   | COORDONNEES MATERIELLES (EPAI=DZ/(1+IVIDE))
C| FLUER          |-->| FLUX D'EROSION EN CHAQUE POINT 2D
C| GIBSON         |-->| LOGIQUE POUR MODELE DE GIBSON
C| HDEP           |-->| HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
C| IKLE2          |-->| TABLE DE CONNECTIVITE POUR LES POINTS DU FOND
C| INFO           |-->| LOGIQUE INDIQUANT SI ON FAIT LES IMPRESSIONS
C| IVIDE          |-->| INDICE DES VIDES AUX POINTS DU MAILLAGE
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| MASK           |<->| TABLEAU DE TRAVAIL
C| MASSE1         |<->| MASSE DU SEDIMENTEN SUSPENSION
C| NCOUCH         |-->| NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
C|                |   | (MODELE DE TASSEMENT MULTICOUCHES)
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NPF            |-->| NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX(MODELE DE GIBSON
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| NVBIL          |---| 
C| PDEPOT         |-->| PROBABILITE DE DEPOT EN CHAQUE POINT 2D
C| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
C| SURFAC         |-->| SURFACES DES ELEMENTS.
C| TA(1,NTRAC)    |-->| CONCENTRATION DU SEDIMANT EN SUSPENSION
C| TASSE          |-->| LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
C| TRA01,02       |<->| TABLEAUX DE TRAVAIL
C| TRA02          |---| 
C| U,V,W          |-->| VITESSE AU PAS DE TEMPS PRESENT
C| WC             |-->| VITESSE DE CHUTE DU SEDIMENT
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPFMAX, NCOUCH, NELEM2, NPOIN2
      INTEGER, INTENT(IN) :: NPOIN3, NTRAC, LT, NVBIL
!
      INTEGER, INTENT(IN) :: IKLE2(NELEM2,3)
      INTEGER, INTENT(IN) :: NPF(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: MASSE1(NVBIL)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN3), V(NPOIN3), W(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: HDEP(NPOIN2), FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: PDEPOT(NPOIN2), CONC(NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3), TRA02(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT,RHOS,CFDEP
!
      LOGICAL, INTENT(IN)             :: INFO , TASSE , GIBSON
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION MASSE3, MASSE4, MASSE5, MASSE6
      DOUBLE PRECISION FLUX, MTOTAL
!
      INTEGER IPTFR, IPLAN, L1, L2, L3, IPOIN, IPF, IELEM2
!
      INTRINSIC SQRT
!
!=======================================================================
!
C COMPUTES THE MASS OF ERODED SEDIMENTS (MASSE3)
C DURING THE TIMESTEP
!
!=======================================================================
!
      FLUX=0.D0
!
      DO IELEM2=1,NELEM2
!
        L1=IKLE2(IELEM2,1)
        L2=IKLE2(IELEM2,2)
        L3=IKLE2(IELEM2,3)
        FLUX=FLUX+SURFAC(IELEM2)*(FLUER(L1)+FLUER(L2)+FLUER(L3))/3.D0
!
       ENDDO
!
       MASSE3=FLUX*DT
!
C            IF (INFO) WRITE(LU,*)
C     &      'MASSE DE SEDIMENTS ERODEE AU COURS DU PAS DE TEMPS:      '
C     &                MASSE3
!
!=======================================================================
!
C COMPUTES THE MASS OF DEPOSITED SEDIMENTS (MASSE4) DURING THE TIMESTEP
!
!=======================================================================
!
      FLUX=0.D0
!
      DO IELEM2=1,NELEM2
!
        L1=IKLE2(IELEM2,1)
        L2=IKLE2(IELEM2,2)
        L3=IKLE2(IELEM2,3)
        FLUX=FLUX-SURFAC(IELEM2)*(PDEPOT(L1)*WC(L1)*TA(L1)+
     &                            PDEPOT(L2)*WC(L2)*TA(L2)+
     &                            PDEPOT(L3)*WC(L3)*TA(L3))/3.D0
      ENDDO
!
      MASSE4=FLUX*DT
!
C            IF (INFO) WRITE(LU,*)
C     &      'MASSE DE SEDIMENTS DEPOSEE AU COURS DU PAS DE TEMPS:     '
C     &                MASSE4
!
!=======================================================================
!
C COMPUTES THE MASS OF SEDIMENT EXCHANGED (MASSE5)
C BETWEEN THE MUDDY BED AND THE FLUID DURING THE TIMESTEP
!
!=======================================================================
!
C MASSE5=MASSE4-MASSE3
!
      MASSE5=MASSE4-MASSE3
      IF(INFO) THEN
        IF(MASSE5.LE.1.D-8) THEN
              WRITE(LU,*)
     &      'MASSE NETTE DE SEDIMENTS QUI PART EN SUSPENSION:         ',
     &             -MASSE5
        ELSE
              WRITE(LU,*)
     &      'MASSE NETTE DE SEDIMENTS QUI SE DEPOSENT       :         ',
     &              MASSE5
        ENDIF
      ENDIF
!
!=======================================================================
!
C COMPUTES THE MASS OF MUDDY DEPOSITS ON THE RIGID BOTTOM (MASSE6)
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
      ELSEIF (GIBSON) THEN
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
      IF (INFO) WRITE(LU,*)
     &      'MASSE TOTALE DES DEPOTS VASEUX:                          ',
     &                MASSE6
!
!=======================================================================
!
C TOTAL MASS OF SEDIMENTS IN THE DOMAIN (MTOTAL)
!
!=======================================================================
!
      MTOTAL=MASSE1(1+NTRAC)+MASSE6
            IF (INFO) WRITE(LU,*)
     &      'MASSE TOTALE DE SEDIMENTS DANS LE DOMAINE:               ',
     &                MTOTAL
!
!
!=======================================================================
!
      RETURN
      END SUBROUTINE SED3D
C
C#######################################################################
C