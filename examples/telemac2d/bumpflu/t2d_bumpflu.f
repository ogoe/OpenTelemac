!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.1         19/08/98  J-M HERVOUET TEL: 30 87 80 18
!
!***********************************************************************
!
!     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |                | -- |  
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TEMPS
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VITESSES : VITESSES NULLES
!
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DE H , LA HAUTEUR D'EAU
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!  ZONE A MODIFIER                                                      
      CALL EXACTE(H%R,U%R,ZF%R,X,NPOIN)
!  FIN DE LA ZONE A MODIFIER      
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TRACEUR
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P , C=TRAC0(ITRAC) )
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISATION DE LA VISCOSITE
!
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       ***************************
                        SUBROUTINE PRERES_TELEMAC2D
!                       ***************************
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
!                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |      LT        | -->| NUMERO D'ITERATION
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : TELMAC
!
!  SOUS-PROGRAMME APPELE : OV
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      LOGICAL IMP,LEO
!
      INTEGER LTT,N,IMAX
!
      DOUBLE PRECISION HHH,XMAX
!
      INTRINSIC MAX,SQRT
!
!-----------------------------------------------------------------------
!
! LOGIQUES POUR DECIDER DES SORTIES
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.PTINIG) LEO=.TRUE.
!
!     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
!
!
!=======================================================================
! CALCUL DE LA CELERITE (MISE DANS FU, VOIR LE BLOC VARSOR)
!=======================================================================
!
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DE LA SURFACE LIBRE (= H + ZF, MISE DANS FV)
!=======================================================================
!
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS( 'X=Y+Z   ' , FV , H , ZF , 0.D0 )
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE FROUDE
!=======================================================================
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        DO N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT SCALAIRE
!=======================================================================
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        DO N=1,NPOIN
          T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT X
!=======================================================================
!
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL OS( 'X=YZ    ' , T4 , H , U , HHH )
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT Y
!=======================================================================
!
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL OS( 'X=YZ    ' , T5 , H , V , HHH )
      ENDIF
!
!=======================================================================
! CALCUL DE LA VITESSE SCALAIRE
!=======================================================================
!
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , T6 , U , V , HHH )
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE COURANT
!=======================================================================
!
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
!                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF (LNG.EQ.1) WRITE(LU,78) XMAX
        IF (LNG.EQ.2) WRITE(LU,79) XMAX
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
! 
!=======================================================================  
! CALCUL DE LA VITESSE ET DE LA HAUTEUR EXACTE                            
!=======================================================================  
!                                                                         
      IF(((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))).AND.
     &   ((LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24)))) THEN
        CALL EXACTE(PRIVE%ADR(1)%P%R,PRIVE%ADR(2)%P%R,ZF%R,X,NPOIN)
      ENDIF
!                                                                         
!=======================================================================  
! CALCUL DE LA SURFACE LIBRE EXACTE                                       
!=======================================================================  
!                                                                         
      IF((LEO.AND.SORLEO(25)).OR.(IMP.AND.SORIMP(25))) THEN
        CALL OV( 'X=Y+Z   ' ,PRIVE%ADR(3)%P%R,
     &                       PRIVE%ADR(1)%P%R,ZF%R,0.D0,NPOIN)
      ENDIF
!
!=======================================================================  
! CALCUL DU NOMBRE DE FROUDE EXACT                                        
!=======================================================================  
!                                                                         
      IF((LEO.AND.SORLEO(26)).OR.(IMP.AND.SORIMP(26))) THEN
        DO N=1,NPOIN                                                   
          HHH = MAX(PRIVE%ADR(1)%P%R(N),1.D-8)
          PRIVE%ADR(4)%P%R(N)=SQRT(PRIVE%ADR(2)%P%R(N)**2/(HHH*GRAV))
        ENDDO                                                          
      ENDIF         
!
!=======================================================================
!
1000  CONTINUE
      RETURN
      END
!                       ***************************
                        SUBROUTINE NOMVAR_TELEMAC2D
!                       ***************************
!
     &(TEXTE,TEXTPR,MNEMO)
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
! FONCTION  :  FIXE LES NOMS DES VARIABLES DU CODE POUR LES FICHIERS
!              DE RESULTAT ET DE GEOMETRIE (TEXTE) ET POUR LE FICHIER
!              DE RESULTATS DU CALCUL PRECEDENT (TEXTPR)
!
!              EN GENERAL TEXTE ET TEXTPR SONT EGAUX SAUF SI ON FAIT
!              UNE SUITE A PARTIR D'UN AUTRE LOGICIEL.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE |
! |________________|____|______________________________________________|
! |   TEXTE        |<-- | NOM DES VARIABLES
! |   TEXTPR       |<-- | NOM DES VARIABLES DU CALCUL PRECEDENT
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : PREDON
!
! SOUS-PROGAMME APPELE : NEANT
!
!**********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      CHARACTER*32 TEXTE(26),TEXTPR(26)
      CHARACTER*8  MNEMO(26)
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.2) THEN
!
      TEXTE (1 ) = 'VELOCITY U      M/S             '
      TEXTE (2 ) = 'VELOCITY V      M/S             '
      TEXTE (3 ) = 'CELERITY        M/S             '
      TEXTE (4 ) = 'WATER DEPTH     M               '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'FROUDE NUMBER                   '
      TEXTE (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTE (9 ) = 'TRACER                          '
      TEXTE (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITY       M2/S            '
      TEXTE (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTE (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTE (15) = 'SCALAR VELOCITY M/S             '
      TEXTE (16) = 'WIND ALONG X    M/S             '
      TEXTE (17) = 'WIND ALONG Y    M/S             '
      TEXTE (18) = 'AIR PRESSURE    PASCAL          '
      TEXTE (19) = 'BOTTOM FRICTION                 '
      TEXTE (20) = 'DRIFT ALONG X   M               '
      TEXTE (21) = 'DRIFT ALONG Y   M               '
      TEXTE (22) = 'COURANT NUMBER                  '
      TEXTE (23) = 'EXACT DEPTH     M               '
      TEXTE (24) = 'EXACT VELOCITY  M/S             '
      TEXTE (25) = 'EXACT ELEVATION M               '       
      TEXTE (26) = 'EXACT FROUDE                    '
!
! TEXTPR IS USED FOR READING PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
! FROM ANOTHER CODE WITH DIFFERENT NAMES THAT YOU HAVE TO
! WRITE HERE.
!
      TEXTPR (1 ) = 'VELOCITY U      M/S             '
      TEXTPR (2 ) = 'VELOCITY V      M/S             '
      TEXTPR (3 ) = 'CELERITY        M/S             '
      TEXTPR (4 ) = 'WATER DEPTH     M               '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'FROUDE NUMBER                   '
      TEXTPR (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTPR (9 ) = 'TRACER                          '
      TEXTPR (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITY       M2/S            '
      TEXTPR (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTPR (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTPR (15) = 'SCALAR VELOCITY M/S             '
      TEXTPR (16) = 'WIND ALONG X    M/S             '
      TEXTPR (17) = 'WIND ALONG Y    M/S             '
      TEXTPR (18) = 'AIR PRESSURE    PASCAL          '
      TEXTPR (19) = 'BOTTOM FRICTION                 '
      TEXTPR (20) = 'DRIFT ALONG X   M               '
      TEXTPR (21) = 'DRIFT ALONG Y   M               '
      TEXTPR (22) = 'COURANT NUMBER                  '
      TEXTPR (23) = 'VARIABLE 23     UNIT   ??       '
      TEXTPR (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTPR (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTPR (26) = 'VARIABLE 26     UNIT   ??       '
!
!-----------------------------------------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
      TEXTE (1 ) = 'VITESSE U       M/S             '
      TEXTE (2 ) = 'VITESSE V       M/S             '
      TEXTE (3 ) = 'CELERITE        M/S             '
      TEXTE (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'FROUDE                          '
      TEXTE (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTE (9 ) = 'TRACEUR                         '
      TEXTE (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITE TURB. M2/S            '
      TEXTE (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTE (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTE (15) = 'VITESSE SCALAIREM/S             '
      TEXTE (16) = 'VENT X          M/S             '
      TEXTE (17) = 'VENT Y          M/S             '
      TEXTE (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTE (19) = 'FROTTEMENT                      '
      TEXTE (20) = 'DERIVE EN X     M               '
      TEXTE (21) = 'DERIVE EN Y     M               '
      TEXTE (22) = 'NBRE DE COURANT                 '
      TEXTE (23) = 'HAUTEUR EXACTE  M               '
      TEXTE (24) = 'VITESSE EXACTE  M/S             '
      TEXTE (25) = 'SURFACE EXACTE  M               '       
      TEXTE (26) = 'FROUDE EXACT                    '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
      TEXTPR (1 ) = 'VITESSE U       M/S             '
      TEXTPR (2 ) = 'VITESSE V       M/S             '
      TEXTPR (3 ) = 'CELERITE        M/S             '
      TEXTPR (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'FROUDE                          '
      TEXTPR (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTPR (9 ) = 'TRACEUR                         '
      TEXTPR (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITE TURB. M2/S            '
      TEXTPR (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTPR (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTPR (15) = 'VITESSE SCALAIREM/S             '
      TEXTPR (16) = 'VENT X          M/S             '
      TEXTPR (17) = 'VENT Y          M/S             '
      TEXTPR (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTPR (19) = 'FROTTEMENT                      '
      TEXTPR (20) = 'DERIVE EN X     M               '
      TEXTPR (21) = 'DERIVE EN Y     M               '
      TEXTPR (22) = 'NBRE DE COURANT                 '
      TEXTPR (23) = 'VARIABLE 23     UNITES ??       '
      TEXTPR (24) = 'VARIABLE 24     UNITES ??       '
      TEXTPR (25) = 'VARIABLE 25     UNITES ??       '
      TEXTPR (26) = 'VARIABLE 26     UNITES ??       '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIAS DES NOMS DE VARIABLES POUR LE FICHIER DES PARAMETRES
!
!     UVCHSBFQTKEDIJMXYPWAGLNORZ
!     VITESSE U
      MNEMO(1)   = 'U       '
!     VITESSE V
      MNEMO(2)   = 'V       '
!     CELERITE
      MNEMO(3)   = 'C       '
!     HAUTEUR D'EAU
      MNEMO(4)   = 'H       '
!     SURFACE LIBRE
      MNEMO(5)   = 'S       '
!     FOND
      MNEMO(6)   = 'B       '
!     FROUDE
      MNEMO(7)   = 'F       '
!     DEBIT SCALAIRE
      MNEMO(8)   = 'Q       '
!     TRACEUR
      MNEMO(9)   = 'T       '
!     ENERGIE TURBUL.
      MNEMO(10)   = 'K       '
!     DISSIPATION
      MNEMO(11)   = 'E       '
!     VISCOSITE TURB.
      MNEMO(12)   = 'D       '
!     DEBIT SUIVANT X
      MNEMO(13)   = 'I       '
!     DEBIT SUIVANT Y
      MNEMO(14)   = 'J       '
!     VITESSE SCALAIRE
      MNEMO(15)   = 'M       '
!     VENT X
      MNEMO(16)   = 'X       '
!     VENT Y
      MNEMO(17)   = 'Y       '
!     PRESSION ATMOS.
      MNEMO(18)   = 'P       '
!     FROTTEMENT
      MNEMO(19)   = 'W       '
!     DERIVE EN X
      MNEMO(20)   = 'A       '
!     DERIVE EN Y
      MNEMO(21)   = 'G       '
!     NBRE DE COURANT
      MNEMO(22)   = 'L       '
!     VARIABLE 23
      MNEMO(23)   = 'N       '
!     VARIABLE 24
      MNEMO(24)   = 'O       '
!     VARIABLE 25
      MNEMO(25)   = 'R       '
!     VARIABLE 26
      MNEMO(26)   = 'Z       '
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE 
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        ZF%R(I) = MAX(-0.2D0,-0.0246875D0*(X(I)-10.D0)**2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************                                 
                        SUBROUTINE EXACTE
!                       *****************                                 
!                                                                         
     &(H,U,ZF,X,NPOIN)
!                                                                         
!***********************************************************************  
! PROGICIEL : 'TELEMAC'       12/12/88    J-M HERVOUET                    
!                                                                         
!***********************************************************************  
!                                                                         
!      FONCTION:    SOLUTION EXACTE DE L'ECOULEMENT TRANSCRITIQUE         
!                   SUR UN BUMP.                                          
!                                                                         
!                   PAR CONVENTION, ZF=0. AU POINT CRITIQUE               
!                                                                         
!                   ATTENTION, IL NE S'AGIT ICI QUE DE LA SOLUTION        
!                   PERMANENTE, QUI EST TOUTEFOIS MISE DANS LE            
!                   FICHIER DE RESULTATS A TOUS LES PAS DE TEMPS.         
!                                                                         
!-----------------------------------------------------------------------  
!                             ARGUMENTS                                   
! .________________.____.______________________________________________.  
! |      NOM       |MODE|                   ROLE                       |  
! |________________|____|______________________________________________|  
! |     HN         |<-- |  HAUTEUR D'EAU.                              |  
! |     U          |<-- |  VITESSE U.                                     
! |     ZF         | -->|  COTE DU FOND.                                  
! |________________|____|______________________________________________|  
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
!**********************************************************************   
!                                                                         
      IMPLICIT NONE
!
      INTEGER I,NPOIN
!                                                                         
      DOUBLE PRECISION H(NPOIN),U(NPOIN),ZF(NPOIN),X(NPOIN)
      DOUBLE PRECISION Q,H0,A(4),HCRIT
!
      EXTERNAL FC1
      DOUBLE PRECISION FC1
!                                                                         
      COMMON/FORFC1/A
!                                                                         
      INTRINSIC MAX
!                                                                         
!-----------------------------------------------------------------------  
!                                                                         
! DEBIT ET HAUTEUR AU POINT CRITIQUE                                      
!                                                                         
      Q = 4.429446918D0
      HCRIT =  (Q**2/9.81D0 )**(1.D0/3.D0)
!     CHARGE DE L'ECOULEMENT
      H0 = 2.05D0
!                                                                         
! EQUATION A RESOUDRE : A(1)*H**3 + A(2)*H**2 + A(3)*H + A(4)             
!                                                                         
      A(1) = 1.D0
      A(3) = 0.D0
      A(4) = Q**2/2.D0/9.81D0
      DO I=1,NPOIN
!                                                                         
      A(2) = ZF(I)-H0
!                                                                         
!     ON PREND LA PLUS GRANDE SOLUTION REELLE                             
      H(I) = 2.01D0
      CALL ZBRENT(FC1,1.D-4,HCRIT,H(I),100)
!                                                                         
      ENDDO                                                            
!                                                                         
!-----------------------------------------------------------------------  
!                                                                         
      DO I=1,NPOIN                                                     
        U(I) = Q / MAX(H(I),1.D-8)                                      
      ENDDO                                                            
!                                                                         
!-----------------------------------------------------------------------  
!                                                                         
      RETURN
      END
!                       *****************************
                        DOUBLE PRECISION FUNCTION FC1
!                       *****************************
!                                                                         
     &(X)
!                                                                         
!***********************************************************************  
! PROGICIEL : TELEMAC        07/12/88    J-M HERVOUET (LNH) 30 71 80 18   
!                                                                         
!***********************************************************************  
!                                                                         
!  FONCTION  : CALCULE UN POLYNOME DU TROISIEME DEGRE                     
!                                                                         
!-----------------------------------------------------------------------  
!                             ARGUMENTS                                   
! .________________.____.______________________________________________   
! |      NOM       |MODE|                   ROLE                          
! |________________|____|______________________________________________   
! |   X            | -->| ARGUMENT DE LA FONCTION.                        
! |________________|____|______________________________________________   
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
!***********************************************************************  
!                                                                         
      IMPLICIT NONE 
!                                                                         
      DOUBLE PRECISION A(4),X
!                                                                         
      COMMON/FORFC1/A
!                                                                         
!-----------------------------------------------------------------------  
!                                                                         
      FC1 = A(1)*X**3 + A(2)*X**2 + A(3)*X + A(4)
!                                                                         
!-----------------------------------------------------------------------  
!                                                                         
      RETURN
      END
!                       *****************
                        SUBROUTINE ZBRENT
!                       *****************
!                                                                       
     &(FC1,EPS,X1,X2,ITMAX)
!                                                                        
!*********************************************************************** 
! BIEF VERSION 3.0           18/08/94    J-M HERVOUET (LNH) 30 87 80 18  
!                                                                        
!*********************************************************************** 
!                                                                        
!  FONCTION  :  SOLUTION D'UNE EQUATION DONT UN ZERO UNIQUE EST ENTRE    
!               LES POINTS X1 ET X2.                                     
!                                                                        
!----------------------------------------------------------------------- 
!                             ARGUMENTS                                  
! .________________.____.______________________________________________  
! |      NOM       |MODE|                   ROLE                         
! |________________|____|______________________________________________  
! |   FC1          | -->| FONCTION DONT ON CHERCHE LE ZERO               
! |                |    | DOIT ETRE DEFINIE EN DOUBLE PRECISION          
! |                |    | PAR AILLEURS.                                  
! |   EPS          | -->| PRECISION CHERCHEE.                            
! |   X1,X2        | -->| ENCADREMENT DE LA SOLUTION ENTREE              
! |                |<-->| X2 = SOLUTION EN SORTIE.                       
! |   ITMAX        | -->| NOMBRE MAXIMUM D'ITERATIONS.                   
! |________________|____|______________________________________________  
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)  
!----------------------------------------------------------------------- 
!                                                                        
!  FONCTION APPELEE : FC1                                                
!                                                                       
!***********************************************************************
!                                                                       
      IMPLICIT NONE                                                     
      INTEGER LNG,LU                                                    
      COMMON/INFO/LNG,LU                                                
!                                                                       
      DOUBLE PRECISION A,B,C,D,E,X1,X2,FA,FB,FC,EPS,EPS2,XM,S,P,Q,R     
!                                                                       
      INTEGER ITMAX,ITER                                                
!                                                                       
      DOUBLE PRECISION FC1                                              
      EXTERNAL FC1                                                      
!                                                                       
      INTRINSIC ABS,SIGN,MIN                                            
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
!  ON VERIFIE QU'ON ENCADRE BIEN LA SOLUTION :                          
!                                                                       
      A=X1                                                              
      B=X2                                                              
      FA=FC1(A)                                                         
      FB=FC1(B)                                                         
      IF(FB*FA.GT.0.D0) THEN                                            
        IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : FC1(X1)*FC1(X2) EST POSITIF'
        IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : ROOT MUST BE BRACKETED'
        CALL PLANTE(1)
        STOP
      ENDIF                                                             
!                                                                       
!  ITERATIONS :                                                         
!                                                                       
      FC=FB                                                             
      DO ITER=1,ITMAX                                                
        IF(FB*FC.GT.0.D0) THEN                                          
          C=A                                                           
          FC=FA                                                        
          D=B-A                                                        
          E=D                                                           
        ENDIF                                                           
        IF(ABS(FC).LT.ABS(FB)) THEN                                    
          A=B                                                          
          B=C                                                           
          C=A                                                           
          FA=FB                                                         
          FB=FC                                                        
          FC=FA                                                         
        ENDIF                                                           
        EPS2=0.5D0*EPS                                                 
        XM=0.5D0*(C-B)                                                 
        IF(ABS(XM).LE.EPS2.OR.FB.EQ.0.D0)THEN                          
          X2=B                                                        
          RETURN                                                      
        ENDIF                                                          
        IF(ABS(E).GE.EPS2.AND.ABS(FA).GT.ABS(FB)) THEN                
          S=FB/FA                                                      
          IF(A.EQ.C) THEN                                              
            P=2.D0*XM*S                                                
            Q=1.D0-S                                                   
          ELSE                                                        
            Q=FA/FC                                                   
            R=FB/FC                                                    
            P=S*(2.D0*XM*Q*(Q-R)-(B-A)*(R-1.D0))                       
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)                               
          ENDIF                                                        
          IF(P.GT.0.D0) Q=-Q                                           
          P=ABS(P)                                                     
          IF(2*P.LT.MIN(3.D0*XM*Q-ABS(EPS2*Q),ABS(E*Q))) THEN           
            E=D                                                         
            D=P/Q                                                       
          ELSE                                                          
            D=XM                                                        
            E=D                                                         
          ENDIF                                                         
        ELSE                                                            
          D=XM                                                          
          E=D                                                           
        ENDIF                                                           
        A=B                                                             
        FA=FB                                                           
        IF(ABS(D).GT.EPS2) THEN                                         
          B=B+D                                                         
        ELSE                                                            
          B=B+SIGN(EPS2,XM)                                             
        ENDIF                                                           
        FB=FC1(B)                                                       
      ENDDO                                                          
!                                                                       
      IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : MAXIMUM D''ITERATIONS ATTEINT'
      IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : EXCEEDING MAXIMUM ITERATIONS' 
      X2=B                                                              
!                                                                       
!-----------------------------------------------------------------------
!                                                                       
      RETURN
      END

