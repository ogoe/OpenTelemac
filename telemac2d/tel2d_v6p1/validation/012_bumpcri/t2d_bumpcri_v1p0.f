C                       *****************
                        SUBROUTINE CONDIN
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.1         19/08/98  J-M HERVOUET TEL: 30 87 80 18
C
C***********************************************************************
C
C     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |                | -- |  
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C***********************************************************************
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      INTEGER ITRAC 
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TEMPS
C
      AT = 0.D0
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DES VITESSES : VITESSES NULLES
C
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DE H , LA HAUTEUR D'EAU
C
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     *   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     *       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     *       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     *       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     *       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     *       CDTINI(1:07).EQ.'SPECIAL') THEN
C  ZONE A MODIFIER                                                      
      CALL EXACTE(H%R,U%R,ZF%R,X,NPOIN)                                        
C  FIN DE LA ZONE A MODIFIER      
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
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TRACEUR
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P , C=TRAC0(ITRAC) )
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
C INITIALISATION DE LA VISCOSITE
C
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       ***************************
                        SUBROUTINE PRERES_TELEMAC2D
C                       ***************************
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
C                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |      LT        | -->| NUMERO D'ITERATION
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : TELMAC
C
C  SOUS-PROGRAMME APPELE : OV
C
C***********************************************************************
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C     
      LOGICAL IMP,LEO
C
      INTEGER LTT,N,IMAX
C
      DOUBLE PRECISION HHH,XMAX
C
      INTRINSIC MAX,SQRT
C
C-----------------------------------------------------------------------
C
C LOGIQUES POUR DECIDER DES SORTIES
C
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.PTINIG) LEO=.TRUE.
C
C     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
C
C
C=======================================================================
C CALCUL DE LA CELERITE (MISE DANS FU, VOIR LE BLOC VARSOR)
C=======================================================================
C
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        DO 5 N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
5       CONTINUE
      ENDIF
C
C=======================================================================
C CALCUL DE LA SURFACE LIBRE (= H + ZF, MISE DANS FV)
C=======================================================================
C
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS( 'X=Y+Z   ' , FV , H , ZF , 0.D0 )
      ENDIF
C
C=======================================================================
C CALCUL DU NOMBRE DE FROUDE
C=======================================================================
C
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        DO 10 N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
10      CONTINUE
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT SCALAIRE
C=======================================================================
C
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        DO 30 N=1,NPOIN
         T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
30      CONTINUE
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT X
C=======================================================================
C
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL OS( 'X=YZ    ' , T4 , H , U , HHH )
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT Y
C=======================================================================
C
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL OS( 'X=YZ    ' , T5 , H , V , HHH )
      ENDIF
C
C=======================================================================
C CALCUL DE LA VITESSE SCALAIRE
C=======================================================================
C
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , T6 , U , V , HHH )
      ENDIF
C
C=======================================================================
C CALCUL DU NOMBRE DE COURANT
C=======================================================================
C
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
C                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF (LNG.EQ.1) WRITE(LU,78) XMAX
        IF (LNG.EQ.2) WRITE(LU,79) XMAX
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
C 
C=======================================================================  
C CALCUL DE LA VITESSE ET DE LA HAUTEUR EXACTE                            
C=======================================================================  
C                                                                         
      IF(((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))).AND.              
     *   ((LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24)))) THEN             
        CALL EXACTE(PRIVE%ADR(1)%P%R,PRIVE%ADR(2)%P%R,ZF%R,X,NPOIN)                                   
      ENDIF
C                                                                         
C=======================================================================  
C CALCUL DE LA SURFACE LIBRE EXACTE                                       
C=======================================================================  
C                                                                         
      IF((LEO.AND.SORLEO(25)).OR.(IMP.AND.SORIMP(25))) THEN               
        CALL OV( 'X=Y+Z   ' ,PRIVE%ADR(3)%P%R,
     *                       PRIVE%ADR(1)%P%R,ZF%R,0.D0,NPOIN)                
      ENDIF
C
C=======================================================================  
C CALCUL DU NOMBRE DE FROUDE EXACT                                        
C=======================================================================  
C                                                                         
      IF((LEO.AND.SORLEO(26)).OR.(IMP.AND.SORIMP(26))) THEN               
        DO 40 N=1,NPOIN                                                   
         HHH = MAX(PRIVE%ADR(1)%P%R(N),1.D-8)                                           
         PRIVE%ADR(4)%P%R(N)=SQRT(PRIVE%ADR(2)%P%R(N)**2/(HHH*GRAV))                        
40      CONTINUE                                                          
      ENDIF         
C
C=======================================================================
C
1000  CONTINUE
      RETURN
      END
C                       ***************************
                        SUBROUTINE NOMVAR_TELEMAC2D
C                       ***************************
C
     *(TEXTE,TEXTPR,MNEMO)
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C FONCTION  :  FIXE LES NOMS DES VARIABLES DU CODE POUR LES FICHIERS
C              DE RESULTAT ET DE GEOMETRIE (TEXTE) ET POUR LE FICHIER
C              DE RESULTATS DU CALCUL PRECEDENT (TEXTPR)
C
C              EN GENERAL TEXTE ET TEXTPR SONT EGAUX SAUF SI ON FAIT
C              UNE SUITE A PARTIR D'UN AUTRE LOGICIEL.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE |
C |________________|____|______________________________________________|
C |   TEXTE        |<-- | NOM DES VARIABLES
C |   TEXTPR       |<-- | NOM DES VARIABLES DU CALCUL PRECEDENT
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : PREDON
C
C SOUS-PROGAMME APPELE : NEANT
C
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      CHARACTER*32 TEXTE(26),TEXTPR(26)
      CHARACTER*8  MNEMO(26)
C
C-----------------------------------------------------------------------
C
C  ENGLISH
C
      IF(LNG.EQ.2) THEN
C
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
C
C TEXTPR IS USED FOR READING PREVIOUS COMPUTATION FILES.
C IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
C FROM ANOTHER CODE WITH DIFFERENT NAMES THAT YOU HAVE TO
C WRITE HERE.
C
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
C
C-----------------------------------------------------------------------
C
C  FRANCAIS OU AUTRE
C
      ELSE
C
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
C
C TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
C A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
C DE CALCUL A PARTIR D'UN AUTRE CODE.
C
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
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C   ALIAS DES NOMS DE VARIABLES POUR LE FICHIER DES PARAMETRES
C
C     UVCHSBFQTKEDIJMXYPWAGLNORZ
C     VITESSE U
      MNEMO(1)   = 'U       '
C     VITESSE V
      MNEMO(2)   = 'V       '
C     CELERITE
      MNEMO(3)   = 'C       '
C     HAUTEUR D'EAU
      MNEMO(4)   = 'H       '
C     SURFACE LIBRE
      MNEMO(5)   = 'S       '
C     FOND
      MNEMO(6)   = 'B       '
C     FROUDE
      MNEMO(7)   = 'F       '
C     DEBIT SCALAIRE
      MNEMO(8)   = 'Q       '
C     TRACEUR
      MNEMO(9)   = 'T       '
C     ENERGIE TURBUL.
      MNEMO(10)   = 'K       '
C     DISSIPATION
      MNEMO(11)   = 'E       '
C     VISCOSITE TURB.
      MNEMO(12)   = 'D       '
C     DEBIT SUIVANT X
      MNEMO(13)   = 'I       '
C     DEBIT SUIVANT Y
      MNEMO(14)   = 'J       '
C     VITESSE SCALAIRE
      MNEMO(15)   = 'M       '
C     VENT X
      MNEMO(16)   = 'X       '
C     VENT Y
      MNEMO(17)   = 'Y       '
C     PRESSION ATMOS.
      MNEMO(18)   = 'P       '
C     FROTTEMENT
      MNEMO(19)   = 'W       '
C     DERIVE EN X
      MNEMO(20)   = 'A       '
C     DERIVE EN Y
      MNEMO(21)   = 'G       '
C     NBRE DE COURANT
      MNEMO(22)   = 'L       '
C     VARIABLE 23
      MNEMO(23)   = 'N       '
C     VARIABLE 24
      MNEMO(24)   = 'O       '
C     VARIABLE 25
      MNEMO(25)   = 'R       '
C     VARIABLE 26
      MNEMO(26)   = 'Z       '
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      ZF        |<-->| FOND A MODIFIER.
C |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C |      A         |<-- | MATRICE
C |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
C |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
C |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
      INTEGER I
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
C
      IF(LISFON.GT.0) THEN
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      DO I=1,NPOIN
        ZF%R(I) = MAX(-0.2D0,-0.0246875D0*(X(I)-10.D0)**2)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END                                                                                                                               
C                       *****************                                 
                        SUBROUTINE EXACTE                                 
C                       *****************                                 
C                                                                         
     *(H,U,ZF,X,NPOIN)                                                    
C                                                                         
C***********************************************************************  
C PROGICIEL : 'TELEMAC'       12/12/88    J-M HERVOUET                    
C                                                                         
C***********************************************************************  
C                                                                         
C      FONCTION:    SOLUTION EXACTE DE L'ECOULEMENT TRANSCRITIQUE         
C                   SUR UN BUMP.                                          
C                                                                         
C                   PAR CONVENTION, ZF=0. AU POINT CRITIQUE               
C                                                                         
C                   ATTENTION, IL NE S'AGIT ICI QUE DE LA SOLUTION        
C                   PERMANENTE, QUI EST TOUTEFOIS MISE DANS LE            
C                   FICHIER DE RESULTATS A TOUS LES PAS DE TEMPS.         
C                                                                         
C-----------------------------------------------------------------------  
C                             ARGUMENTS                                   
C .________________.____.______________________________________________.  
C |      NOM       |MODE|                   ROLE                       |  
C |________________|____|______________________________________________|  
C |     HN         |<-- |  HAUTEUR D'EAU.                              |  
C |     U          |<-- |  VITESSE U.                                     
C |     ZF         | -->|  COTE DU FOND.                                  
C |________________|____|______________________________________________|  
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
C**********************************************************************   
C                                                                         
      IMPLICIT NONE
C
      INTEGER I,NPOIN
C                                                                        
      DOUBLE PRECISION H(NPOIN),U(NPOIN),ZF(NPOIN),X(NPOIN)
      DOUBLE PRECISION Q,H0,A(4)                                                    
C                                                                         
      EXTERNAL FC1                                                        
      DOUBLE PRECISION FC1                                                
C                                                                         
      COMMON/FORFC1/A                                                     
C
C-----------------------------------------------------------------------  
C
C DEBIT ET HAUTEUR AU POINT CRITIQUE                                      
C                                                                         
      Q = 0.3D0                                                           
      H0 = ( Q**2/9.81D0 )**(1.D0/3.D0)                                   
C                                                                         
C EQUATION A RESOUDRE : A(1)*H**3 + A(2)*H**2 + A(3)*H + A(4)             
C                                                                         
      A(1) = 1.D0                                                         
      A(3) = 0.D0                                                         
      A(4) = H0**3.D0/2.D0                                                
      DO 10 I=1,NPOIN                                                     
C                                                                         
      A(2) = ZF(I)-3.D0*H0/2.D0                                           
C                                                                         
      IF(X(I).GT.10.D0) THEN                                              
C        ON PREND LA PLUS PETITE SOLUTION REELLE                          
         H(I) = H0                                                        
         CALL ZBRENT(FC1,1.D-3,0.1D0,H(I),100)                            
      ELSEIF(X(I).LT.10.D0) THEN                                          
C        ON PREND LA PLUS GRANDE SOLUTION REELLE                          
         H(I) = 0.5D0                                                     
         CALL ZBRENT(FC1,1.D-3,H0,H(I),100)                               
      ELSE                                                                
C        POINT CRITIQUE                                                   
         H(I) = H0                                                        
        ENDIF                                                             
C                                                                         
10    CONTINUE                                                            
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
      DO 20 I=1,NPOIN                                                     
        U(I) = Q / MAX(H(I),1.D-8)                                        
20    CONTINUE                                                            
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
      RETURN                                                              
      END                                                                 
C                       *****************************                     
                        DOUBLE PRECISION FUNCTION FC1                     
C                       *****************************                     
C                                                                         
     *(X)                                                                 
C                                                                         
C***********************************************************************  
C PROGICIEL : TELEMAC        07/12/88    J-M HERVOUET (LNH) 30 71 80 18   
C                                                                         
C***********************************************************************  
C                                                                         
C  FONCTION  : CALCULE UN POLYNOME DU TROISIEME DEGRE                     
C                                                                         
C-----------------------------------------------------------------------  
C                             ARGUMENTS                                   
C .________________.____.______________________________________________   
C |      NOM       |MODE|                   ROLE                          
C |________________|____|______________________________________________   
C |   X            | -->| ARGUMENT DE LA FONCTION.                        
C |________________|____|______________________________________________   
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)   
C***********************************************************************  
C                                                                         
      IMPLICIT NONE                                                       
C                                                                         
      DOUBLE PRECISION A(4),X                                             
C                                                                         
      COMMON/FORFC1/A                                                     
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
      FC1 = A(1)*X**3 + A(2)*X**2 + A(3)*X + A(4)                         
C                                                                         
C-----------------------------------------------------------------------  
C                                                                         
      RETURN                                                              
      END                                                                 
C                       *****************                                
                        SUBROUTINE ZBRENT                                
C                       *****************                                
C                                                                       
     *(FC1,EPS,X1,X2,ITMAX)                                              
C                                                                        
C*********************************************************************** 
C BIEF VERSION 3.0           18/08/94    J-M HERVOUET (LNH) 30 87 80 18  
C                                                                        
C*********************************************************************** 
C                                                                        
C  FONCTION  :  SOLUTION D'UNE EQUATION DONT UN ZERO UNIQUE EST ENTRE    
C               LES POINTS X1 ET X2.                                     
C                                                                        
C----------------------------------------------------------------------- 
C                             ARGUMENTS                                  
C .________________.____.______________________________________________  
C |      NOM       |MODE|                   ROLE                         
C |________________|____|______________________________________________  
C |   FC1          | -->| FONCTION DONT ON CHERCHE LE ZERO               
C |                |    | DOIT ETRE DEFINIE EN DOUBLE PRECISION          
C |                |    | PAR AILLEURS.                                  
C |   EPS          | -->| PRECISION CHERCHEE.                            
C |   X1,X2        | -->| ENCADREMENT DE LA SOLUTION ENTREE              
C |                |<-->| X2 = SOLUTION EN SORTIE.                       
C |   ITMAX        | -->| NOMBRE MAXIMUM D'ITERATIONS.                   
C |________________|____|______________________________________________  
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)  
C----------------------------------------------------------------------- 
C                                                                        
C  FONCTION APPELEE : FC1                                                
C                                                                       
C***********************************************************************
C                                                                       
      IMPLICIT NONE                                                     
      INTEGER LNG,LU                                                    
      COMMON/INFO/LNG,LU                                                
C                                                                       
      DOUBLE PRECISION A,B,C,D,E,X1,X2,FA,FB,FC,EPS,EPS2,XM,S,P,Q,R     
C                                                                       
      INTEGER ITMAX,ITER                                                
C                                                                       
      DOUBLE PRECISION FC1                                              
      EXTERNAL FC1                                                      
C                                                                       
      INTRINSIC ABS,SIGN,MIN                                            
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  ON VERIFIE QU'ON ENCADRE BIEN LA SOLUTION :                          
C                                                                       
      A=X1                                                              
      B=X2                                                              
      FA=FC1(A)                                                         
      FB=FC1(B)                                                         
      IF(FB*FA.GT.0.D0) THEN                                            
       IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : FC1(X1)*FC1(X2) EST POSITIF' 
       IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : ROOT MUST BE BRACKETED'      
       STOP                                                             
      ENDIF                                                             
C                                                                       
C  ITERATIONS :                                                         
C                                                                       
      FC=FB                                                             
      DO 10 ITER=1,ITMAX                                                
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
10    CONTINUE                                                          
C                                                                       
      IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : MAXIMUM D''ITERATIONS ATTEINT'
      IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : EXCEEDING MAXIMUM ITERATIONS' 
      X2=B                                                              
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      RETURN
      END


