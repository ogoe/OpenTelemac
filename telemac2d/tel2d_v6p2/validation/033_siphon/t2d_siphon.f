C                       *****************
                        SUBROUTINE CONDIN
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.2         19/08/98  J-M HERVOUET TEL: 30 87 80 18
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
      INTEGER I,ITRAC 
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
      CALL OS( 'X=0     ' , X=U )
      CALL OS( 'X=0     ' , X=V )
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
C
C  DANS CHACUN DES DEUX BASSINS LA SURFACE LIBRE N'EST PAS PLATE
C  VOIR CORFON !!!!!!
C                                                     
        DO I = 1,NPOIN
          IF(X(I).LT.250.D0) THEN
            H%R(I)=4.D0
          ELSE
            H%R(I)=2.D0
          ENDIF
        ENDDO                                         
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
C
        DO I = 1,NPOIN
          IF(X(I).LT.250.D0) THEN
            T%ADR(ITRAC)%P%R(I)=TRAC0(ITRAC)
          ELSE
            T%ADR(ITRAC)%P%R(I)= 50.D0
          ENDIF
        ENDDO
C
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
C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.2          01/03/90    J-M HERVOUET
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
      INTEGER IPOIN
      DOUBLE PRECISION EIKON1,EIKON2
C
C-----------------------------------------------------------------------
C
C     LISSAGES EVENTUELS DU FOND
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
C     POUR ESSAI AVEC BANCS DECOUVRANTS
C
!     DO IPOIN=1,NPOIN
!       EIKON1=( (X(IPOIN)- 10.D0)**2 + (Y(IPOIN)- 10.D0)**2 ) /2500.D0
!       EIKON2=( (X(IPOIN)-510.D0)**2 + (Y(IPOIN)-210.D0)**2 ) /2500.D0
!       ZF%R(IPOIN) = 5.D0 * ( EXP(-EIKON1) + EXP(-EIKON2) )
!     ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
