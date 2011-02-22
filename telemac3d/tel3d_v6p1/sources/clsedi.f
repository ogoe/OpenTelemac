C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXPRESSES THE BOUNDARY CONDITIONS FOR THE SEDIMENT,
!>                AT THE BOTTOM AND SURFACE (FOR COHESIVE SEDIMENT OR NOT).

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
     &  AC     , KSPRATIO,ICR)
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
      INTEGER, INTENT(IN) :: NPOIN2,NPOIN3,KLOG,NPFMAX
      INTEGER, INTENT(IN) :: NCOUCH,ITURBV,NPLAN,ICR
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
C 
      DOUBLE PRECISION KSP,A,C,ZERO,HCLIP,MU
      INTEGER IPOIN,I
!
!-----------------------------------------------------------------------
!
      ZERO = 1.D-6
!
      DO IPOIN=1,NPOIN2
C       COMPUTES THE FLUID DENSITY
        DENSI(IPOIN) = (DELTAR(IPOIN)+1.D0)*RHO0
C       COMPUTES THE STRESS AT THE BOTTOM
        TOB%R(IPOIN) = DENSI(IPOIN)*UETCAR(IPOIN)
      ENDDO
!
      IF(ICR.EQ.1) THEN
!
        DO IPOIN=1,NPOIN2
C         CORRECTION FOR SKIN FRICTION (SEE TOB_SISYPHE)
          KSP=KSPRATIO *DMOY%R(IPOIN)
          IF(CF%R(IPOIN) > ZERO.AND.HN%R(IPOIN).GT.KSP) THEN
            HCLIP=MAX(HN%R(IPOIN),KSP)
            A = 2.5D0*LOG(12.D0*HCLIP/KSP)
            MU =2.D0/(A**2*CF%R(IPOIN))
          ELSE
            MU=0.D0
          ENDIF
          TOB%R(IPOIN) = MU* TOB%R(IPOIN)
        ENDDO
!
      ENDIF
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
