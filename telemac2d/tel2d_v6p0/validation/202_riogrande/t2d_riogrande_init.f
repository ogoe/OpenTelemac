
c                       *****************
                        SUBROUTINE CONDIN
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.8      30/08/07  J-M HERVOUET TEL: 01 30 87 80 18
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
      INTEGER ITRAC,I 
      DOUBLE PRECISION WLev(NPOIN)
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TEMPS
C
      AT = 0.D0
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DES VITESSES : VITESSES 
C
      CALL OS( 'X=C     ' , U , U , U , 0.25D0/(0.6D0*1.1D0) )
      CALL OS( 'X=0     ' , X=V )
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DE H , LA HAUTEUR D'EAU
C
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     *   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=0     ' , X=H )
        CALL OS( 'X=X-Y   ' , X=H , Y=ZF )
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
        IF(LNG.EQ.1) WRITE(LU,10)                                       
        IF(LNG.EQ.2) WRITE(LU,11)                                       
10      FORMAT(1X,'CONDIN : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     *         ,/,'         VOUS DEVEZ MODIFIER CONDIN')                
11      FORMAT(1X,'CONDIN : WITH SPECIAL INITIAL CONDITIONS'            
     *         ,/,'         YOU HAVE TO MODIFY CONDIN')                 
        CALL PLANTE(1)                                                  
        STOP                                                            
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
C   INITIALISATION DES TRACEURS
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

      DO 2 I=1,NPOIN                                                            
         WLev(I) = 0.D0

          IF (Y(I) .GE. 8138.D0) THEN   
c       Station   <20             
               WLev(I)=0.000631*(Y(I)-8138)+10.97

         ELSEIF((Y(I).LE.8138.D0).AND.(Y(I).GE.7474.D0)) THEN  
c        20-40
         WLev(I)=0.000597*(Y(I)-7474)+10.58

         ELSEIF((Y(I).LE.7474).AND.(Y(I).GE.6922.D0)) THEN                 
c        40-60
         WLev(I)=0.000883*(Y(I)-6922)+10.09

         ELSEIF((Y(I).LE.6922).AND.(Y(I).GE.6378.D0)) THEN  
c        60-80
         WLev(I)=0.000672*(Y(I)-6378)+9.72 
        ELSEIF((Y(I).LE.6378).AND.(Y(I).GE.5766.D0)) THEN  
c        80-100
         WLev(I)=0.000697*(Y(I)-5766)+9.30
        ELSEIF((Y(I).LE.5766).AND.(Y(I).GE.5256.D0)) THEN  
c        100-120
         WLev(I)=0.000538*(Y(I)-5256)+9.02   
        ELSEIF((Y(I).LE.5256).AND.(Y(I).GE.4708.D0)) THEN  
c        120-140
         WLev(I)=0.000779*(Y(I)-4708)+8.60
         ELSEIF((Y(I).LE.4708).AND.(Y(I).GE.4064.D0)) THEN  
c        140-160
         WLev(I)=0.000521*(Y(I)-4064)+8.26  
         ELSEIF(Y(I).LE.4064) THEN  
c        140-160
         WLev(I)=0.000626*(Y(I)-3042)+7.92

         endif
         H%R(I)=Wlev(I)-ZF%R(I)
                                                                                                                      
2     CONTINUE

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
      DOUBLE PRECISION PI,L0,Li,Lo,Hdune,Lm
      INTEGER I
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  Bosse � t=0
C

c      Lo=1.5D0
      DO 2 I=1,NPOIN                                                            
         ZF%R(I) = 0.D0

          IF (Y(I) .GE. 8138.D0) THEN   
c         <20             
               ZF%R(I)=0.000904*(Y(I)-8138)+10.26
               print *,Y(I),ZF%R(I)
         ELSEIF((Y(I).LE.8138.D0).AND.(Y(I).GE.7474.D0)) THEN  
c        20-40
         ZF%R(I)=0.000491*(Y(I)-7474)+9.94

         ELSEIF((Y(I).LE.7474).AND.(Y(I).GE.6922.D0)) THEN                 
c        40-60
         ZF%R(I)=0.000392*(Y(I)-6922)+9.72

         ELSEIF((Y(I).LE.6922).AND.(Y(I).GE.6378.D0)) THEN  
c        60-80
         ZF%R(I)=0.000807*(Y(I)-6378)+9.28  
        ELSEIF((Y(I).LE.6378).AND.(Y(I).GE.5766.D0)) THEN  
c        80-100
         ZF%R(I)=0.00119*(Y(I)-5766)+8.55
        ELSEIF((Y(I).LE.5766).AND.(Y(I).GE.5256.D0)) THEN  
c        100-120
         ZF%R(I)=0.00061*(Y(I)-5256)+8.24   
        ELSEIF((Y(I).LE.5256).AND.(Y(I).GE.4708.D0)) THEN  
c        120-140
         ZF%R(I)=0.00059*(Y(I)-4708)+7.92 
         ELSEIF((Y(I).LE.4708).AND.(Y(I).GE.4064.D0)) THEN  
c        140-160
         ZF%R(I)=0.000364*(Y(I)-4064)+7.68  
         ELSEIF(Y(I).LE.4064) THEN  
c        140-160
         ZF%R(I)=0.000716*(Y(I)-3042)+6.95

         endif
                                                                                                                        
2     CONTINUE
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
      RETURN
      END  
