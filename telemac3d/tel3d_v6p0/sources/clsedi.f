C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXPRESSES THE BOUNDARY CONDITIONS FOR THE SEDIMENT,
!>                AT THE BOTTOM AND SURFACE (FOR COHESIVE SEDIMENT OR NOT).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ATABOF, ATABOS, BTABOF, BTABOS, CF, CFDEP, CONC, CREF, DELTAR, DENSI, DMOY, DT, EPAI, FLUER, GIBSON, GRADZFX, GRADZFY, GRADZSX, GRADZSY, GRAV, HDEP, HN, ITURBV, IVIDE, KLOG, KSPRATIO, LITABF, LITABS, MPART, NCOUCH, NPF, NPFMAX, NPLAN, NPOIN2, NPOIN3, PDEPOT, PRIVE, RHO0, RHOS, SEDCO, TA, TASSE, TOB, TOCD, TOCE, TRA03, UETCAR, WC, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, C, HCLIP, I, IPOIN, KSP, MU, ZERO
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CLSEDI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ERODC(), ERODE(), ERODNC(), FLUSED()
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
!>      <td><center> 5.0                                       </center>
!> </td><td> 12/09/07
!> </td><td> C LE NORMANT (LNH)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/06/03
!> </td><td> CAMILLE LEQUETTE
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>ATABO,BTABO
!></td><td><--</td><td>LOI LOG SUR TRACEURS ACTIFS:ATABO*TA + BTABO
!>    </td></tr>
!>          <tr><td>ATABOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ATABOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BTABOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BTABOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CFDEP
!></td><td>--></td><td>CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
!>    </td></tr>
!>          <tr><td>CONC
!></td><td>--></td><td>CONCENTRATIONS DES COUCHES DU FOND VASEUX
!>    </td></tr>
!>          <tr><td>CREF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DELTAR
!></td><td>--></td><td>DELTA RHO SUR RHO0 = (RHO-RHO0)/RHO0
!>    </td></tr>
!>          <tr><td>DENSI
!></td><td>--></td><td>DENSITE DE L'EAU
!>    </td></tr>
!>          <tr><td>DMOY
!></td><td>--></td><td>DIAMETRE MOYEN DES GRAINS
!>    </td></tr>
!>          <tr><td>DNUTAV
!></td><td>--></td><td>VISCOSITE LAMINAIRE DU TRACEUR
!>    </td></tr>
!>          <tr><td>DNUVIV
!></td><td>--></td><td>VISCOSITE LAMINAIRE DE L'EAU
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS HYDRAULIQUE
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td><-></td><td>TAILLE DES MAILLES DU FOND EN
!>                  COORDONNEES MATERIELLES (EPAI=DZ/(1+IVIDE))
!>    </td></tr>
!>          <tr><td>F,  S
!></td><td>---</td><td>F : FOND     S : SURFACE
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td><--</td><td>FLUX D'EROSION EN CHAQUE POINT 2D
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>LOGIQUE POUR MODELE DE GIBSON
!>    </td></tr>
!>          <tr><td>GRADZFX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADZFY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADZSX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADZSY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>CONSTANTE GRAVITATIONNELLE
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td><-></td><td>HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU A L'INSTANT N
!>    </td></tr>
!>          <tr><td>ITURBV
!></td><td>--></td><td>MODELE DE TURBULENCE  VERTICAL
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td><-></td><td>INDICE DES VIDES AUX POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT UTILISEE
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>INDICATEUR DE PAROI SOLIDE
!>    </td></tr>
!>          <tr><td>KSPRATIO
!></td><td>--></td><td>RATIO RUGOSITE DE PEAU / DIAMETRE DES GRAINS
!>    </td></tr>
!>          <tr><td>LITA,BF
!></td><td><-></td><td>TYPE COND. LIMITES SUR TA         : FOND
!>    </td></tr>
!>          <tr><td>LITA,BS
!></td><td><-></td><td>TYPE COND. LIMITES SUR TA         : SURFACE
!>    </td></tr>
!>          <tr><td>LITABF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LITABS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MPART
!></td><td>--></td><td>COEFFICIENT D'EROSION (LOI DE PARTHENIADES)
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
!>                  (MODELE DE TASSEMENT MULTICOUCHES)
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
!>                  DISCRETISANT LE FOND VASEUX(MODELE DE GIBSON)
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NOMBRE DE TABLEAUX DE DIMENSION NPOIN3
!>                  RESERVES A L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>PDEPOT
!></td><td><--</td><td>PROBABILITE DE DEPOT EN CHAQUE POINT 2D
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAUX RESERVES A L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>RHO0
!></td><td>--></td><td>DENSITE DE REFERENCE DE L'EAU
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>MASSE VOLUMIQUE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>RUGOF0
!></td><td>--></td><td>COEFFICIENT DE NIKURADSE TOTAL
!>    </td></tr>
!>          <tr><td>SEDCO
!></td><td>--></td><td>LOGIQUE POUR SEDIMENT COHESIF
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>CONCENTRATION EN SEDIMENT
!>    </td></tr>
!>          <tr><td>TASSE
!></td><td>--></td><td>LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>CONTRAINTE DE FROTTEMENT AU FOND
!>    </td></tr>
!>          <tr><td>TOCD
!></td><td>--></td><td>CONTRAINTE CRITIQUE DE DEPOT
!>    </td></tr>
!>          <tr><td>TOCE
!></td><td>--></td><td>CONTRAINTE CRITIQUE D'EROSION
!>    </td></tr>
!>          <tr><td>TRA03
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>UETCAR
!></td><td>--></td><td>U ETOILE CARRE
!>    </td></tr>
!>          <tr><td>VARIABLE NON COHESIF
!></td><td>---</td><td>
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
                        SUBROUTINE CLSEDI
     &( ATABOF , BTABOF , ATABOS , BTABOS , TA     ,
     &  WC     , GRADZFX, GRADZFY, GRADZSX, GRADZSY,
     &  X      , Y      , Z      , HN     , DELTAR ,
     &  TOB    , DENSI  , TRA03  ,  IVIDE  , EPAI   ,
     &  CONC   , HDEP   , FLUER  , PDEPOT , LITABF ,
     &  LITABS , KLOG   , NPOIN3 , NPOIN2 , NPLAN  ,
     &  NPFMAX , NCOUCH , NPF    , ITURBV , DT     , RHO0   ,
     &  RHOS   , CFDEP  , TOCD   , MPART  , TOCE   , TASSE  ,
     &  GIBSON , PRIVE  , UETCAR ,
     &  GRAV   , SEDCO  , DMOY   , CREF   , CF,
     &  AC     , KSPRATIO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |-->| 
C| ATABO,BTABO    |<--| LOI LOG SUR TRACEURS ACTIFS:ATABO*TA + BTABO
C| ATABOF         |---| 
C| ATABOS         |---| 
C| BTABOF         |---| 
C| BTABOS         |---| 
C| CF             |---| 
C| CFDEP          |-->| CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
C| CONC           |-->| CONCENTRATIONS DES COUCHES DU FOND VASEUX
C| CREF           |---| 
C| DELTAR         |-->| DELTA RHO SUR RHO0 = (RHO-RHO0)/RHO0
C| DENSI          |-->| DENSITE DE L'EAU
C| DMOY           |-->| DIAMETRE MOYEN DES GRAINS
C| DNUTAV         |-->| VISCOSITE LAMINAIRE DU TRACEUR
C| DNUVIV         |-->| VISCOSITE LAMINAIRE DE L'EAU
C| DT             |-->| PAS DE TEMPS HYDRAULIQUE
C| EPAI           |<->| TAILLE DES MAILLES DU FOND EN
C|                |   | COORDONNEES MATERIELLES (EPAI=DZ/(1+IVIDE))
C| F,  S          |---| F : FOND     S : SURFACE
C| FLUER          |<--| FLUX D'EROSION EN CHAQUE POINT 2D
C| GIBSON         |-->| LOGIQUE POUR MODELE DE GIBSON
C| GRADZFX        |---| 
C| GRADZFY        |---| 
C| GRADZSX        |---| 
C| GRADZSY        |---| 
C| GRAV           |-->| CONSTANTE GRAVITATIONNELLE
C| HDEP           |<->| HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
C| HN             |-->| HAUTEUR D'EAU A L'INSTANT N
C| ITURBV         |-->| MODELE DE TURBULENCE  VERTICAL
C| IVIDE          |<->| INDICE DES VIDES AUX POINTS DU MAILLAGE
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KFROT          |-->| LOI DE FROTTEMENT UTILISEE
C| KLOG           |-->| INDICATEUR DE PAROI SOLIDE
C| KSPRATIO       |-->| RATIO RUGOSITE DE PEAU / DIAMETRE DES GRAINS
C| LITA,BF        |<->| TYPE COND. LIMITES SUR TA         : FOND
C| LITA,BS        |<->| TYPE COND. LIMITES SUR TA         : SURFACE
C| LITABF         |---| 
C| LITABS         |---| 
C| MPART          |-->| COEFFICIENT D'EROSION (LOI DE PARTHENIADES)
C| NCOUCH         |-->| NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
C|                |   | (MODELE DE TASSEMENT MULTICOUCHES)
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
C| NPF            |-->| NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX(MODELE DE GIBSON)
C| NPLAN          |---| 
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NPRIV          |-->| NOMBRE DE TABLEAUX DE DIMENSION NPOIN3
C|                |   | RESERVES A L'UTILISATEUR
C| PDEPOT         |<--| PROBABILITE DE DEPOT EN CHAQUE POINT 2D
C| PRIVE          |-->| TABLEAUX RESERVES A L'UTILISATEUR
C| RHO0           |-->| DENSITE DE REFERENCE DE L'EAU
C| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
C| RUGOF0         |-->| COEFFICIENT DE NIKURADSE TOTAL
C| SEDCO          |-->| LOGIQUE POUR SEDIMENT COHESIF
C| TA             |-->| CONCENTRATION EN SEDIMENT
C| TASSE          |-->| LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
C| TOB            |-->| CONTRAINTE DE FROTTEMENT AU FOND
C| TOCD           |-->| CONTRAINTE CRITIQUE DE DEPOT
C| TOCE           |-->| CONTRAINTE CRITIQUE D'EROSION
C| TRA03          |<->| TABLEAU DE TRAVAIL
C| UETCAR         |-->| U ETOILE CARRE
C| VARIABLE NON CO|---| 
C| WC             |-->| VITESSE DE CHUTE DU SEDIMENT
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CLSEDI => CLSEDI
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN2, NPOIN3, KLOG , NPFMAX
      INTEGER, INTENT(IN) :: NCOUCH, ITURBV,  NPLAN
!
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOF(NPOIN2), BTABOF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOS(NPOIN2), BTABOS(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: WC(NPOIN3), DELTAR(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: PRIVE,TOB,CREF
      TYPE(BIEF_OBJ), INTENT(IN)    :: DMOY,HN,CF
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: DENSI(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA03(NPOIN2),UETCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2),FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: PDEPOT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZFX(NPOIN2),GRADZFY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZSX(NPOIN2),GRADZSY(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: DT    , RHO0   , RHOS
      DOUBLE PRECISION, INTENT(IN) :: CFDEP , TOCD   , GRAV
      DOUBLE PRECISION, INTENT(IN) :: MPART , TOCE
!
      INTEGER, INTENT(INOUT) :: LITABF(NPOIN2), LITABS(NPOIN2)
      INTEGER, INTENT(INOUT) :: NPF(NPOIN2)
!
      LOGICAL, INTENT(IN)             :: TASSE , GIBSON , SEDCO
      DOUBLE PRECISION, INTENT(IN)    :: AC, KSPRATIO
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C CV MODIFS 05/08
      DOUBLE PRECISION KSP, A, C, ZERO, HCLIP, MU
C... END CV MODIFS
      INTEGER IPOIN,I
!
!-----------------------------------------------------------------------

      ZERO = 1.D-06
!
      DO IPOIN=1,NPOIN2
C V6P0 : CV MODIFS
C CORRECTION FOR SKIN ROUGHNESS
!
        KSP=KSPRATIO *DMOY%R(IPOIN)
!
C       COMPUTES THE FLUID DENSITY
!
        DENSI(IPOIN) = (DELTAR(IPOIN)+1.D0)*RHO0
!
C       COMPUTES THE STRESS AT THE BOTTOM
!
        TOB%R(IPOIN) = DENSI(IPOIN)*UETCAR(IPOIN)
!
C CORRECTION FOR SKIN FRICTION
C  CV : V6P0 KSP=KSPRATIO *DMOY
C SEE TOB_SISYPHE FOR SKIN FRICTION COMPUTATION
!
        IF(.NOT.SEDCO) THEN
         IF((CF%R(IPOIN) > ZERO).AND.(HN%R(IPOIN).GT.KSP)) THEN
            HCLIP=MAX(HN%R(IPOIN),KSP)
C CV        A = 2.5D0*LOG(12.D0*HCLIP)/ KSP
            A = 2.5D0*LOG(12.D0*HCLIP/ KSP)
            C =2.D0/A**2
            MU = C/CF%R(IPOIN)
         ELSE
            MU=0.D0
         ENDIF
        ENDIF
        TOB%R(IPOIN) = MU* TOB%R(IPOIN)
!
      ENDDO
!
C      -----COMPUTES THE EXPLICIT EROSION FLUX-----
!
      IF(SEDCO) THEN
!
        IF(TASSE) THEN
!
          CALL ERODC(CONC,EPAI,FLUER,TOB%R,DENSI,
     &               MPART,DT,NPOIN2,NCOUCH)
!
        ELSE
!
          CALL ERODE(IVIDE,EPAI,HDEP,FLUER,TOB%R,DENSI,
     &               NPOIN2,NPFMAX,NPF,MPART,TOCE,
     &               CFDEP,RHOS,DT,GIBSON)
!
        ENDIF

      ELSE
!
          CALL ERODNC(CFDEP,WC,HDEP,FLUER,TOB,DT,
     &                NPOIN2,NPOIN3,KSPRATIO,AC,RHOS,RHO0,HN,
     &                GRAV,DMOY,CREF,CF)
!
      ENDIF
!
C      -----WRITES THE BOUNDARY CONDITIONS AT THE BOTTOM / SURFACE-----
C      -----                FOR THE SEDIMENT                      -----
!
      CALL FLUSED(ATABOF , BTABOF , ATABOS , BTABOS ,
     &            LITABF , LITABS , TA     , WC     ,
     &            X      , Y      , Z      , HN%R   ,
     &            GRADZFX, GRADZFY, GRADZSX, GRADZSY,
     &            TOB%R  , PDEPOT , FLUER  , TOCD   ,
     &            NPOIN3 , NPOIN2 , NPLAN  , KLOG   , SEDCO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C