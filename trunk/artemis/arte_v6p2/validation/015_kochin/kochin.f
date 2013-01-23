C--------------------------------------------------------------------
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C--------------------------------------------------------------------

C                       ***************
                        SUBROUTINE BORH
C                       ***************
C
C***********************************************************************
C
C  ARTEMIS    VERSION 3.2 02/06/99   D. AELBRECHT (LNH) 01 30 87 74 12 
C
C  LINKED TO BIEF VERS. 5.0          J-M HERVOUET (LNH) 01 30 87 80 18
C
C***********************************************************************
C
C      FONCTION:    PREND EN COMPTE LES CONDITIONS AUX LIMITES
C                   DE L'UTILISATEUR
C                   ELLES SONT DONNEES PAR SEGMENT.
C
C      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   RP           |<-- |  COEFFICIENTS DE REFLEXION DES PAROIS        |
C |   TETAP        |<-- |  ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES |
C |                |    |  PAS SEULEMENT LES PAROIS, MAIS AUSSI LES    |
C |                |    |  LES FRONTIERES LIQUIDES                     |
C |                |    |  (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE |
C |                |    |   DANS LE SENS DIRECT)                       |
C |   ALFAP        |<-- |  DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE  |
C |                |    |  REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST |
C |                |    |  POSITIF, L'ONDE REFLECHIE EST EN RETARD)    |
C |   HB           |<-- |  HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES |
C |   TETAB        |<-- |  ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)   |
C |                |    |  (COMPTE PAR RAPPORT A L'AXE DES X DANS LE   |
C |                |    |   SENS DIRECT)                               |
C |    H           | -->|  HAUTEUR D'EAU                               |
C |    K           | -->|  NOMBRE D'ONDE                               |
C |    C,CG        | -->|  VITESSES DE PHASE ET DE GROUPE              |
C |    C           | -->|  CELERITE AU TEMPS N                         |
C |    ZF          | -->|  FOND                                        |
C |    X,Y         | -->|  COORDONNEES DES POINTS DU MAILLAGE          |
C |  TRA01,...,3   |<-->|  TABLEAUX DE TRAVAIL                         |
C | XSGBOR,YSGBOR  | -->|  NORMALES EXTERIEURES AUX SEGMENTS DE BORD   |
C |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
C |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
C |   KP1BOR       | -->|  NUMERO DU POINT FRONTIERE SUIVANT           |
C |   OMEGA        | -->|  PULSATION DE LA HOULE                       |
C |   PER          | -->|  PERIODE DE LA HOULE                         |
C |   TETAH        | -->|  ANGLE DE PROPAGATION DE LA HOULE            |
C |   GRAV         | -->|  GRAVITE                                     |
C |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
C |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
C |   KENT,KLOG    | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
C |   KSORT,KINC   |    |  LIMITES                                     |
C |                |    |  KENT  : ENTREE (VALEUR IMPOSEE)             |
C |                |    |  KLOG  : PAROI                               |
C |                |    |  KSORT : SORTIE                              |
C |                |    |  KINC  : ONDE INCIDENTE                      |
C |   PRIVE        | -->|  TABLEAU DE TRAVAIL (DIMENSION DANS PRINCI)  |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : ARTEMI
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C
      DOUBLE PRECISION PI,BID
      
CCP
      DOUBLE PRECISION Phi_Re    , Phi_Im
      DOUBLE PRECISION DDXPhi_Re , DDYPhi_Re , DDXPhi_Im , DDYPhi_Im

CCP      
      
C
C     ---------------------------------------- 
C     VOS NOUVELLES DECLARATIONS DE VARIABLES :
C     ---------------------------------------- 
C                                                                       
C JCB :                                                                       
      INTEGER I , IG   , JB
C JCB
C
C
      PARAMETER( PI = 3.1415926535897932384626433D0)
C
      INTRINSIC COS,SIN
C
C-----------------------------------------------------------------------
C
C CONDITIONS AUX LIMITES
C UN SEGMENT EST SOLIDE SI IL EST DE TYPE KLOG.
C UN SEGMENT EST ONDE INCIDENTE SI IL EST DE TYPE KINC.
C UN SEGMENT EST UNE ENTREE SI IL EST DE TYPE KENT.
C UN SEGMENT EST UNE SORTIE SI IL EST DE TYPE KSORT.
C
C TOUS LES ANGLES SONT EN DEGRES
C                         ------
C ---------------------------------------
C INITIALISATION DES VARIABLES PAR DEFAUT
C ---------------------------------------
      TETAB%R(:) = 0.D0
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 0.D0
      
      PRB%R(:)   =0.D0
      PIB%R(:)   =0.D0
      DDXPRB%R(:)=0.D0
      DDYPRB%R(:)=0.D0
      DDXPIB%R(:)=0.D0
      DDYPIB%R(:)=0.D0
      
      
      DO I=1,NPTFR
       JB=BOUNDARY_COLOUR%I(I)
      
C SORTIE
      IF(JB.GE.1.AND.JB.LE.72) THEN
         LIHBOR%I(I) = KSORT
         TETAP%R(I) = 0.D0
      ENDIF
      
C Potentiel incident
      IF(JB.GE.73.AND.JB.LE.144) THEN
         LIHBOR%I(I) = KPOT
         TETAP%R(I)  = 0.D0
         IG   = MESH%NBOR%I(I)
	 
	 CALL FAR_FIELD_POTENTIAL
     *( X(IG)        , Y(IG)         , K%R(IG) , Phi_Re   , Phi_Im, 
     * DDXPhi_Re , DDYPhi_Re , DDXPhi_Im , DDYPhi_Im)
         PRB%R(I)   = Phi_Re
         PIB%R(I)   = Phi_Im
	 DDXPRB%R(I)= DDXPhi_Re
         DDYPRB%R(I)= DDYPhi_Re
         DDXPIB%R(I)= DDXPhi_Im
         DDYPIB%R(I)= DDYPhi_Im	
      ENDIF
      
      ENDDO




C-----------------------------------------------------------------------
C                                                                       
      RETURN                                                            
      END                                                               
















C--------------------------------------------------------------------
C--------------------------- KOCHIN ---------------------------------
C--------------------------------------------------------------------


C     ******************************
      SUBROUTINE FAR_FIELD_POTENTIAL
C     ******************************

     *( X        , Y         , WNB       , Phi_Re   , Phi_Im, 
     * DDXPhi_Re , DDYPhi_Re , DDXPhi_Im , DDYPhi_Im)
C
      IMPLICIT NONE
C
C.....Variables transmises
C     """"""""""""""""""""
      DOUBLE PRECISION X     , Y     , WNB   , Phi_Re, Phi_Im
C
CCP
      DOUBLE PRECISION DDXPhi_Re , DDYPhi_Re , DDXPhi_Im , DDYPhi_Im
CCP
C.....Variables locales
C     """""""""""""""""
      INTEGER          IX    , II
      DOUBLE PRECISION PI    , R     , TETA  , XX    , ModZ  , ArgZ  ,
     *                 AUX1  , AUX2  , correc
      DOUBLE PRECISION ModZlu(38), ArgZlu(38)
C
C     A VIRER  !!!!!!!!!    A VIRER  !!!!!!!!
      DOUBLE PRECISION OMEGA , GRAVIT

CCP
      DOUBLE PRECISION PHTETA,DPHTETA,MODTETA,DMODTETA
      DOUBLE PRECISION ANG1,ANG2,AR,BR,RP12,RP32
      DOUBLE PRECISION DDTPHR,DDTPHI,DDRPHR,DDRPHI
      DOUBLE PRECISION DDTModZ(38),DDTArgZ(38)
CCP


      GRAVIT=9.81D0
      OMEGA=DSQRT(GRAVIT*WNB)

C
      DATA ModZlu   / 0.4275048D+01, 0.4296801D+01, 0.4362129D+01,
     * 0.4471456D+01, 0.4624897D+01, 0.4822242D+01, 0.5062084D+01,
     * 0.5342195D+01, 0.5659060D+01, 0.6008002D+01, 0.6383544D+01,
     * 0.6779524D+01, 0.7189541D+01, 0.7607111D+01, 0.8025802D+01,
     * 0.8439748D+01, 0.8843442D+01, 0.9232073D+01, 0.9601530D+01,
     * 0.9948439D+01, 0.1027029D+02, 0.1056528D+02, 0.1083235D+02,
     * 0.1107121D+02, 0.1128217D+02, 0.1146607D+02, 0.1162433D+02,
     * 0.1175860D+02, 0.1187084D+02, 0.1196319D+02, 0.1203774D+02,
     * 0.1209669D+02, 0.1214191D+02, 0.1217515D+02, 0.1219779D+02,
     * 0.1221109D+02, 0.1221539D+02, 0.1221539D+02/
      DATA ArgZlu   /-0.4036163D+02,-0.4009285D+02,-0.3930756D+02,
     *-0.3806484D+02,-0.3645247D+02,-0.3457508D+02,-0.3254024D+02,
     *-0.3044616D+02,-0.2837278D+02,-0.2638076D+02,-0.2451050D+02,
     *-0.2278505D+02,-0.2121416D+02,-0.1979829D+02,-0.1853172D+02,
     *-0.1740489D+02,-0.1640648D+02,-0.1552499D+02,-0.1474856D+02,
     *-0.1406632D+02,-0.1346808D+02,-0.1294480D+02,-0.1248813D+02,
     *-0.1209076D+02,-0.1174620D+02,-0.1144857D+02,-0.1119283D+02,
     *-0.1097438D+02,-0.1078925D+02,-0.1063398D+02,-0.1050551D+02,
     *-0.1040127D+02,-0.1031883D+02,-0.1025677D+02,-0.1021342D+02,
     *-0.1018777D+02,-0.1017922D+02,-0.1017922D+02/
C
C
      PI=4.0D0*DATAN(1.0D0)
      correc=-4.0D0*PI
C      correc=1.D0
C
C      WRITE(6,*) 'ON ENTRE CHEZ MICHEL'
C.....Calcul du rayon (R) et de l'angle (TETA, en degres sur [-180;180])
C.....à partir des coordonnees cartesiennes (X et Y) du point considere.
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      R=DSQRT(X*X+Y*Y)
      TETA=DATAN2(Y,X)*180.D0/PI
C      WRITE(6,*) 'TETA=',  X  ,  Y ,  TETA
    
      
  201 IF (TETA.LT.-180.D0) THEN
        TETA=TETA+360.D0
        GOTO 201
      ENDIF
  202 IF (TETA.GT.180.D0) THEN
        TETA=TETA-360.D0
        GOTO 202
      ENDIF
C
C.....Calcul du module (ModZ) et de l'angle de la fonction de Kochin 
C.....H(Teta) par interp. lineaire dans le vecteur des valeurs fournies.
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      XX=ABS(TETA)/5.0D0
      IX=INT(XX)+1
      XX=XX-INT(XX)
      ModZ=(1.D0-XX)*ModZlu(IX)+XX*ModZlu(IX+1)
      ArgZ=(1.D0-XX)*ArgZlu(IX)+XX*ArgZlu(IX+1)
      
C      WRITE(6,*) 'KOCHIN=',  ModZ  ,  ArgZ 

CCP On recupere les derivees (attention : fonctions en rad)
      DO II=1,38 
        IF (II.LE.37) THEN
         DDTModZ(II)=(ModZlu(II+1)-ModZlu(II))/(5.0D0*PI/180.0D0)
         DDTArgZ(II)=(ArgZlu(II+1)-ArgZlu(II))/5.0D0
        ELSE
         DDTModZ(II)=0.0D0
         DDTArgZ(II)=0.0D0
	ENDIF
      ENDDO
      
      DMODTETA=(1.D0-XX)*DDTModZ(IX)+XX*DDTModZ(IX+1)
      DPHTETA =(1.D0-XX)*DDTArgZ(IX)+XX*DDTArgZ(IX+1)
CCP


C
C.....Calcul du potentiel proprement dit (parties reelles et imaginaire)
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      AUX1=DSQRT(WNB/(2.D0*PI*R))*ModZ*correc
      AUX2 = WNB*R - PI/4.0D0 + ArgZ*PI/180.D0
      Phi_Re= AUX1*COS(AUX2)
      Phi_Im= AUX1*SIN(AUX2)
C      WRITE(6,*) 'X   Y   PR   PI=', X  ,  Y  ,  Phi_Re  , Phi_Im 

C

C      WRITE(6,*) 'ON SORT DE CHEZ MICHEL'
C      WRITE(6,*) 'ON ENTRE CHEZ CP'


CCP
C.....GRADIENT
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C -- DDR POTENTIEL
      ANG1=WNB*R - PI/4.0D0
      PHTETA =ArgZ*PI/180.D0
      MODTETA=ModZ
      RP12=DSQRT(R)
      RP32=RP12**3

      
      AR= (-1.D0/(2.D0*RP32))*COS(ANG1) - (WNB/RP12)*SIN(ANG1)
      BR= (-1.D0/(2.D0*RP32))*SIN(ANG1) + (WNB/RP12)*COS(ANG1)

      AUX1  =DSQRT(WNB/(2.D0*PI))*MODTETA*correc
      DDRPHR=AUX1*(AR*COS(PHTETA)-BR*SIN(PHTETA))
      DDRPHI=AUX1*(AR*SIN(PHTETA)+BR*COS(PHTETA))

C -- DDTETA POTENTIEL
      ANG2=WNB*R - PI/4.0D0 + PHTETA
      
      AR= DMODTETA*COS(ANG2)- MODTETA*DPHTETA*SIN(ANG2)
      BR= DMODTETA*SIN(ANG2)+ MODTETA*DPHTETA*COS(ANG2)

      AUX1=DSQRT(WNB/(2.D0*PI*R))*correc
      
      DDTPHR=AUX1*AR
      DDTPHI=AUX1*BR

C -- GRADIENT DANS LE REPERE ORTHO
      AUX2=TETA*PI/180.D0
      DDXPhi_Re=(DDRPHR*COS(AUX2)-(1.D0/R)*DDTPHR*SIN(AUX2))
      DDYPhi_Re=(DDRPHR*SIN(AUX2)+(1.D0/R)*DDTPHR*COS(AUX2))
      DDXPhi_Im=(DDRPHI*COS(AUX2)-(1.D0/R)*DDTPHI*SIN(AUX2))
      DDYPhi_Im=(DDRPHI*SIN(AUX2)+(1.D0/R)*DDTPHI*COS(AUX2))



CCP
C      WRITE(6,*) 'ON SORT DE CHEZ CP'


      RETURN
      END




C     ******************************
      SUBROUTINE SIMPLE_POTENTIAL
C     ******************************

     *( X        , Y         , WNB       , Phi_Re   , Phi_Im, 
     * DDXPhi_Re , DDYPhi_Re , DDXPhi_Im , DDYPhi_Im)
C
      IMPLICIT NONE
C
C.....Variables transmises
C     """"""""""""""""""""
      DOUBLE PRECISION X     , Y     , WNB   , Phi_Re, Phi_Im,H
C
CCP
      DOUBLE PRECISION DDXPhi_Re , DDYPhi_Re , DDXPhi_Im , DDYPhi_Im
CCP
C.....Variables locales
C     """""""""""""""""
      INTEGER          IX    , II
      DOUBLE PRECISION PI    , R     , TETA  , XX    , ModZ  , ArgZ  ,
     *                 AUX1  , AUX2  , correc
      DOUBLE PRECISION ModZlu(38), ArgZlu(38)
C
C     A VIRER  !!!!!!!!!    A VIRER  !!!!!!!!
      DOUBLE PRECISION OMEGA , GRAVIT

CCP
      DOUBLE PRECISION PHTETA,DPHTETA,MODTETA,DMODTETA
      DOUBLE PRECISION ANG1,ANG2,AR,BR,RP12,RP32
      DOUBLE PRECISION DDTPHR,DDTPHI,DDRPHR,DDRPHI
      DOUBLE PRECISION DDTModZ(38),DDTArgZ(38)
CCP

      H=1.0D0
      GRAVIT=9.81D0
      OMEGA=DSQRT(GRAVIT*WNB)
C
      PI=4.0D0*DATAN(1.0D0)
      correc=-4.0D0*PI
C      correc=1.D0
C
C      WRITE(6,*) 'ON ENTRE CHEZ MICHEL'
C.....Calcul du rayon (R) et de l'angle (TETA, en degres sur [-180;180])
C.....à partir des coordonnees cartesiennes (X et Y) du point considere.
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      R=DSQRT(X*X+Y*Y)
      TETA=DATAN2(Y,X)*180.D0/PI
C      WRITE(6,*) 'TETA=',  X  ,  Y ,  TETA
    
      
  201 IF (TETA.LT.-180.D0) THEN
        TETA=TETA+360.D0
        GOTO 201
      ENDIF
  202 IF (TETA.GT.180.D0) THEN
        TETA=TETA-360.D0
        GOTO 202
      ENDIF


      AUX1=H*GRAVIT/(2.0D0*OMEGA)
      AUX2=WNB*R
      
      Phi_Re=  AUX1*SIN(AUX2)
      Phi_Im= -AUX1*COS(AUX2)

CCP
C.....GRADIENT
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C -- DDR POTENTIEL

      AUX1  = AUX1*WNB
      
      DDRPHR=AUX1*COS(AUX2)
      DDRPHI=AUX1*SIN(AUX2)

C -- DDTETA POTENTIEL
      
      DDTPHR=0.0D0
      DDTPHI=0.0D0

C -- GRADIENT DANS LE REPERE ORTHO
      AUX2=TETA*PI/180.D0
      DDXPhi_Re=(DDRPHR*COS(AUX2)-(1.D0/R)*DDTPHR*SIN(AUX2))
      DDYPhi_Re=(DDRPHR*SIN(AUX2)+(1.D0/R)*DDTPHR*COS(AUX2))
      DDXPhi_Im=(DDRPHI*COS(AUX2)-(1.D0/R)*DDTPHI*SIN(AUX2))
      DDYPhi_Im=(DDRPHI*SIN(AUX2)+(1.D0/R)*DDTPHI*COS(AUX2))
CCP
C      WRITE(6,*) 'K=',WNB


      RETURN
      END

