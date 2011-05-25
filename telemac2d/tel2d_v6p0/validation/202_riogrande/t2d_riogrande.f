C                       *****************
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
C     Initial water profile: linear according to the Y coordinate

      DO 2 I=1,NPOIN                                                            
         WLev(I) = 0.D0

          IF (Y(I) .GE. 8138.D0) THEN   
c         <20             
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
C     Bathymetric profile: linear according to the Y coordinate

      DO 2 I=1,NPOIN                                                            
         ZF%R(I) = 0.D0

          IF (Y(I) .GE. 8138.D0) THEN   
c         <20             
               ZF%R(I)=0.000904*(Y(I)-8138)+10.26
!              print *,Y(I),ZF%R(I)
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
C                       *****************
                        SUBROUTINE CORSTR
C                       *****************
C
C***********************************************************************
C  BIEF VERSION 5.0           01/10/96    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C
C      FONCTION: CALCUL DU COEFFICIENT DE FROTTEMENT SUR LE FOND
C                SI IL  EST VARIABLE EN ESPACE.
C
C      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
C      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
C |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
C |    ZF          | -->|  COTE DU FOND                                |
C |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
C |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
C |    MESH        | -->|  BLOC DES ENTIERS DU MAILLAGE.
c       KFROT      | -->|  LOI DE FROTTEMENT 
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : PREDAT
C

C
C**********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU 
C
C-----------------------------------------------------------------------
C
C     Local variables
C     masse volumique de l'eau et des sediments
      DOUBLE PRECISION  XMVE,XMVS
C     
C     diametre median des sediments d50
      DOUBLE PRECISION, POINTER :: ACLADM (:)
C     Variables pour la formule de van Rijn     
      DOUBLE PRECISION UC,AI
      DOUBLE PRECISION  ZERO
      DOUBLE PRECISION KS, KSCR,KSCD,KSCMR,UNORM   
      DOUBLE PRECISION MOB,FES,FFS
      DOUBLE PRECISION DSAND,DGRAVEL,DSILT,HVR
C      
      INTEGER I, KFDYN 
C     KFDYN =1 van Rijn, KFDYN=2 free for users    
C
C-----------------------------------------------------------------------
C   
      KFDYN=1
      DATA XMVE/1000.D0/
      DATA XMVS/2650.D0/   
      ACLADM => T1%R 
C
C-----------------------------------------------------------------------
C
      IF(KFDYN.EQ.1) THEN     
      DO I=1,NPOIN
C     
        ACLADM(I)=210.D-6
        UNORM =DSQRT(UN%R(I)**2 + VN%R(I)**2)
C       
        KS=0.d0
        HVR=MAX(HN%R(I),1.D-4)
C
C       Starting to compute the van Rijn rugosity Ks
C       van Rijn 2007
        ZERO=1.D-6 
        DSILT=0.000032D0   
        DGRAVEL=0.002D0
        DSAND=0.000062D0
C
C CALCULATION OF CURRENT-DOMINATED ROUGHNESS USING VAN RIJN (2007)
C
C Mobility number for current only
C
        AI  = ACLADM(I)*GRAV*(XMVS-XMVE)/XMVE             
        MOB = UNORM**2/AI
C
C RIPPLE ROUGHNESS
C
        IF(ACLADM(I).LE.0.25D0*DGRAVEL)THEN        
          FES=1.D0
        ELSE
          FES=SQRT(( 0.25D0*DGRAVEL/ACLADM(I) )**3)             
        ENDIF 
C 
        IF(ACLADM(I).LT.DSILT)THEN 
          KSCR=20.D0*DSILT
        ELSE
          AI= TANH(0.015D0*(MOB-150.D0))
          KSCR=FES*ACLADM(I)*(85.D0-65.D0*AI)
        ENDIF
C
C MEGARIPPLE ROUGHNESS
C
        IF(ACLADM(I).GE.1.5D0*DSAND)THEN
          FFS=1.D0
        ELSE
          FFS=ACLADM(I)/1.5D0/DSAND
        ENDIF
        IF(ACLADM(I).LE.DSILT) THEN
          KSCMR=0.D0            
        ELSE
          KSCMR=0.00002D0*FFS*HVR*(1.D0-EXP(-0.05D0*MOB))
          IF(MOB.GT.550.D0) THEN
            IF(ACLADM(I).GE.1.5D0*DSAND) THEN
              KSCMR=0.02D0
            ELSE
              KSCMR=200.D0*ACLADM(I)
            ENDIF
          ENDIF
        ENDIF
C       DUNE ROUGHNESS
        IF(ACLADM(I).LT.DSILT)THEN
          KSCD=0.D0
        ELSE
          AI=(1.D0-EXP(-0.02D0*MOB))*(600.D0-MOB)
          KSCD=0.00008D0*FFS*HVR* AI
        ENDIF
        IF(MOB.GT.600.D0) KSCD=0.D0
        IF(KSCD.GT.1.D0)  KSCD=1.D0
C        
C *** TOTAL ROUGHNESS FOR COMPUTATIONS IN TELEMAC2D **
C
        KS=SQRT(KSCR**2+KSCMR**2+KSCD**2)+3.D0*ACLADM(I)
C       End of the roughness computation    
C       Definition of the friction coefficient linked to the friction equation 
C       CF
C         
        IF(KFROT.EQ.2) THEN       
          CHESTR%R(I)=18.D0*LOG10(11.D0*HVR/KS)
        ELSEIF(KFROT.EQ.3) THEN
          CHESTR%R(I)=18.D0*LOG10(11.D0*HVR/KS)/HVR**(1.0/6.0)          
        ELSEIF(KFROT.EQ.4) THEN
          CHESTR%R(I)=HVR**(1.0/6.0)/18.D0*LOG10(11.D0*HVR/KS)
        ELSEIF(KFROT.EQ.5) THEN
          CHESTR%R(I)=KS
        ENDIF
C
      ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
!     IF(KFDYN.EQ.2) THEN
!     DO I=1,NPOIN
!     ENDDO
!     ENDIF
C
C-----------------------------------------------------------------------
C     
      RETURN  
      END 
