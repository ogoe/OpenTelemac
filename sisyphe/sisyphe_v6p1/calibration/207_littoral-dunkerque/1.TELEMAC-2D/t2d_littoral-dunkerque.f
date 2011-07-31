C                       ***************
                        SUBROUTINE BORD
C                       ***************
C
     *(HBOR,UBOR,VBOR,TBOR,U,V,H,
     * ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     * XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     * NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     * NOMIMP)
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.9   27/03/08  J-M HERVOUET (LNHE) 01 30 87 80 18
C
C***********************************************************************
C
C      FONCTION:    MODIFIE LES TABLEAUX DE CONDITIONS AUX LIMITES
C                   DANS LE CAS OU ELLES SONT VARIABLES EN TEMPS.
C
C      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
C      SOIT DIRECTEMENT, SOIT PAR L'INTERMEDIAIRE DES FONCTIONS :
C
C                           Q , SL , TR , VIT
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   HBOR         |<-- |  HAUTEUR IMPOSEE.                            |
C |   UBOR         |<-- |  VITESSE U IMPOSEE.                          |
C |   VBOR         |<-- |  VITESSE V IMPOSEE.                          |
C |   TBOR         |<-- |  TRACEUR IMPOSE AU BORD                      |
C |    U,V         | -->|  COMPOSANTES DE LA VITESSE AU TEMPS N        |
C |    H           | -->|  HAUTEUR AU TEMPS N                          |
C |    ZF          | -->|  FOND                                        |
C |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
C |  TRA05,TRA06   | -->|  TABLEAUX DE TRAVAIL                         |
C |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
C | LIUBOR         | -->|  CONDITIONS AUX LIMITES SUR U 
C |   LITBOR       | -->|  CONDITIONS AUX LIMITES SUR LE TRACEUR       |
C |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
C |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
C |   TEMPS        | -->|  TEMPS                                       |
C |   DEBIT        |<-->|  TABLEAU DE DEBITS IMPOSES                   |
C |   NDEBIT       | -->|  NOMBRE DE FRONTIERES A DEBIT IMPOSE         |
C |   COTE         |<-->|  TABLEAU DE COTES DE LA SURFACE LIBRE IMPOSEES
C |   COTINI       | -->|  COTE INITIALE
C |   NCOTE        | -->|  NOMBRE DE FRONTIERES A COTE IMPOSEE         |
C |   VITES        |<-->|  TABLEAU DE COMPOSANTES NORMALES DE LA VITESSE
C |                |    |  IMPOSEES                                    |
C |   NVITES       | -->|  NOMBRE DE FRONTIERES A VITESSE IMPOSEE      |
C |   TRAC         | -->|  LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR  |
C |   TRACER       |<-->|  TABLEAU DE VALEURS DU TRACEUR IMPOSEES      |
C |   NTRACE       | -->|  NOMBRE DE FRONTIERES A TRACEUR IMPOSE       |
C |   NFRLIQ       | -->|  NOMBRE DE FRONTIERES LIQUIDES
C |   KENT,KENTU,  | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
C |                |    |  KENTU:U ET V IMPOSES                        |
C |   PROVEL       | -->|  OPTION POUR LES PROFILS DE VITESSE          |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : TELMAC
C
C SOUS-PROGRAMME APPELE : DEBIMP
C
C FONCTIONS APPELEES : Q , SL , TR , VIT
C
C***********************************************************************
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD => BORD
      USE DECLARATIONS_TELEMAC2D, ONLY : STA_DIS_CURVES,PTS_CURVES,QZ,
     *                                   FLUX_BOUNDARIES
      USE DECLARATIONS_TELEMAC2D, ONLY : DEBIT,COTE,
     *                                  T2D_FILES,T2DFO2
C
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR2) 
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN) :: EQUA
      CHARACTER(LEN=144), INTENT(IN) :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)  :: MASK,LITBOR 
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,MSK8,IFRLIQ,YADEB(300),IERR,ITRAC      
C
      DOUBLE PRECISION Z      
C
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
      INTRINSIC MAX
CC
CGB*****************************************************************
CGB       DECLARATION SUPPLEMENTAIRES : RELECTURE MAREE JMJ
CGB********************************************************
CGB
C
      DOUBLE PRECISION HM2,HS2,HM4,HN2,TM2,TS2,TM4,TN2,PI
      DOUBLE PRECISION UM2,US2,UM4,UN2,VM2,VS2,VM4,VN2,ALF,MODU,MODV
      DOUBLE PRECISION AHM2(174) , PHM2(174) , AHS2(174) , PHS2(174)
      DOUBLE PRECISION AHM4(174) , PHM4(174) , AHN2(174) , PHN2(174)
      DOUBLE PRECISION AUM2(174) , PUM2(174) , AUS2(174) , PUS2(174)
      DOUBLE PRECISION AUM4(174) , PUM4(174) , AUN2(174) , PUN2(174)
      DOUBLE PRECISION AVM2(174) , PVM2(174) , AVS2(174) , PVS2(174)
      DOUBLE PRECISION AVM4(174) , PVM4(174) , AVN2(174) , PVN2(174)
      INTEGER NPTFRL , IPTFRL
C
      DOUBLE PRECISION PROF(NPOIN)
C
      SAVE AHM2,PHM2,AHS2,PHS2,AHM4,PHM4,AHN2,PHN2
      SAVE AUM2,PUM2,AUS2,PUS2,AUM4,PUM4,AUN2,PUN2
      SAVE AVM2,PVM2,AVS2,PVS2,AVM4,PVM4,AVN2,PVN2
C
      DOUBLE PRECISION COEF, ANM
C
C****************************************************
C     FIN DES DECLARATIONS SUPPLEMENTAIRES
*******************************************************
CV modifs : 
      INTEGER NFO2
      NFO2 = T2D_FILES(T2DFO2)%LU
      
C-----------------------------------------------------------------------
C
      MSK8 = 8
C
C  INITIALISATION DE YADEB
C
      IF(NFRLIQ.GE.1) THEN
        DO 1 K=1,NFRLIQ
          YADEB(K)=0
1       CONTINUE
      ENDIF
C Début modif fichier 
C======================================================
C LECTURE DES DONNEES DE MAREES AU PREMIER PAS DE TEMPS
C======================================================
C
       PI=ACOS(-1.D0)
CJM    NB DE POINTS DE BORD DU MODELE 
       NPTFRL = 174
ccV cycle ME - VE       
          TM2 = 44714.D0
          TS2 = 43200.D0
          TN2 = 43200.D0
          TM4 = 22357.D0
C
       COEF = 1.D0
       ANM  = 0.D0
C
      IF (TEMPS.LT.200.D0) THEN
C
      REWIND (NFO2)
C
          DO 2 IPTFRL = 1,NPTFRL
             READ(NFO2,*)
             READ(NFO2,*) AHM2(IPTFRL),PHM2(IPTFRL),
     *                  AUM2(IPTFRL),PUM2(IPTFRL),
     *                  AVM2(IPTFRL),PVM2(IPTFRL)
             READ(NFO2,*) AHS2(IPTFRL),PHS2(IPTFRL),
     *                  AUS2(IPTFRL),PUS2(IPTFRL),
     *                  AVS2(IPTFRL),PVS2(IPTFRL)
             READ(NFO2,*) AHN2(IPTFRL),PHN2(IPTFRL),
     *                  AUN2(IPTFRL),PUN2(IPTFRL),
     *                  AVN2(IPTFRL),PVN2(IPTFRL)
             READ(NFO2,*) AHM4(IPTFRL),PHM4(IPTFRL),
     *                  AUM4(IPTFRL),PUM4(IPTFRL),
     *                  AVM4(IPTFRL),PVM4(IPTFRL)
C
cc tunning on boundary node node 738
             PHM2(IPTFRL) = (PHM2(IPTFRL)- 356.8D0) / 360.D0
             PUM2(IPTFRL) = (PUM2(IPTFRL)- 356.8D0) / 360.D0
             PVM2(IPTFRL) = (PVM2(IPTFRL)- 356.8D0) / 360.D0
C
             PHS2(IPTFRL) = (PHS2(IPTFRL)- 49.4D0) / 360.D0
             PUS2(IPTFRL) = (PUS2(IPTFRL)- 49.4D0) / 360.D0
             PVS2(IPTFRL) = (PVS2(IPTFRL)- 49.4D0) / 360.D0
C
             PHN2(IPTFRL) = (PHN2(IPTFRL)- 333.7D0) / 360.D0
             PUN2(IPTFRL) = (PUN2(IPTFRL)- 333.7D0) / 360.D0
             PVN2(IPTFRL) = (PVN2(IPTFRL)- 333.7D0) / 360.D0
C
             PHM4(IPTFRL) = (PHM4(IPTFRL)- 322.2D0) / 360.D0
             PUM4(IPTFRL) = (PUM4(IPTFRL)- 322.2D0) / 360.D0
             PVM4(IPTFRL) = (PVM4(IPTFRL)- 322.2D0) / 360.D0
C
2        CONTINUE
777      FORMAT(F9.3,F9.3,4F8.3)
       ENDIF

C======================================================
C CALCUL DE LA MAREE
C======================================================
C
CML   ON SIMULE UNE MAREE DE COEFF 120 
C     (toutes les ondes sont considerees)
C*********************************************************************
C
      IPTFRL = 1

      DO 51 K=1,NPTFR
C
        IF(LIHBOR(K).EQ.KENT) THEN

          PROF(K) = 
     *          AHM2(IPTFRL) * COS(2.D0*PI*(TEMPS/TM2-PHM2(IPTFRL)))
     *      +   AHS2(IPTFRL) * COS(2.D0*PI*(TEMPS/TS2-PHS2(IPTFRL)))
     *      +   AHN2(IPTFRL) * COS(2.D0*PI*(TEMPS/TN2-PHN2(IPTFRL)))
     *      +   AHM4(IPTFRL) * COS(2.D0*PI*(TEMPS/TM4-PHM4(IPTFRL)))  

           HBOR(K) = -ZF(NBOR(K)) + 0.53D0 + COEF*PROF(K) + ANM

       
        ENDIF
C
CML...On impose les vitesses
C                                                                    
        IF(LIUBOR(K).EQ.KENTU) THEN                        
C                                        
         UM2 = AUM2(IPTFRL) * COS(2.D0*PI*(TEMPS/TM2-PUM2(IPTFRL)))
         US2 = AUS2(IPTFRL) * COS(2.D0*PI*(TEMPS/TS2-PUS2(IPTFRL)))
         UN2 = AUN2(IPTFRL) * COS(2.D0*PI*(TEMPS/TN2-PUN2(IPTFRL)))
         UM4 = AUM4(IPTFRL) * COS(2.D0*PI*(TEMPS/TM4-PUM4(IPTFRL)))
         VM2 = AVM2(IPTFRL) * COS(2.D0*PI*(TEMPS/TM2-PVM2(IPTFRL)))
         VS2 = AVS2(IPTFRL) * COS(2.D0*PI*(TEMPS/TS2-PVS2(IPTFRL)))
         VN2 = AVN2(IPTFRL) * COS(2.D0*PI*(TEMPS/TN2-PVN2(IPTFRL)))
         VM4 = AVM4(IPTFRL) * COS(2.D0*PI*(TEMPS/TM4-PVM4(IPTFRL)))
         MODU = SQRT(COEF)* (UM2 + US2 + UN2 + UM4)
         MODV = SQRT(COEF)* (VM2 + VS2 + VN2 + VM4)
C     MODU = UM2 + US2 + UN2 + UM4
C     MODV = VM2 + VS2 + VN2 + VM4
         IF (TEMPS.LT.1800.D0) THEN
           MODU=MODU*(TEMPS/1800.D0)
           MODV=MODV*(TEMPS/1800.D0)
         ENDIF
C
         UBOR(K,1) = MODU
         VBOR(K,1) = MODV

         IPTFRL=IPTFRL+1
        ENDIF   
C 
51    CONTINUE  
C
C Fin modif
C
C  BOUCLE SUR TOUS LES POINTS FRONTIERE
C
      DO 5 K=1,NPTFR
C
C  COTE IMPOSEE AVEC VALEUR DONNEE DANS LE FICHIER CAS (NCOTE<>0)
C
      IF(LIHBOR(K).EQ.KENT) THEN
C
        IFRLIQ=NUMLIQ(K)
C
        IF(STA_DIS_CURVES(IFRLIQ).EQ.1) THEN
          Z = STA_DIS_CUR(IFRLIQ,FLUX_BOUNDARIES(IFRLIQ),
     *                    PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     *                    ZF(NBOR(K))+H%R(NBOR(K)))
          HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          H%R(NBOR(K))=HBOR(K)
        ELSEIF(NCOTE.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
          Z = SL(IFRLIQ,NBOR(K))
          HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          H%R(NBOR(K))=HBOR(K)
        ENDIF
C
      ENDIF
C
C  DEBIT IMPOSE : DIFFERENTES OPTIONS SUIVANT PROVEL
C                 ON UTILISE LES VALEURS DONNEES PAR L'UTILISATEUR
C                 COMME PROFIL DE VITESSE.
C                 UBOR(K,2) ET VBOR(K,2) SONT LES VALEURS DU
C                 FICHIER CONLIM CONSERVEES.
C
      IF(LIUBOR(K).EQ.KENT.AND.
     *  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
C         PROFIL NORMAL CONSTANT
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
C         PROFIL DONNE PAR L'UTILISATEUR
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
C         VITESSE NORMALE DONNEE DANS UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.4) THEN
C         PROFIL NORMAL EN RACINE DE H
          UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
        ENDIF
C       ON NE MET PAS DE VITESSE SI IL N'Y A PAS D'EAU.
        IF(H%R(NBOR(K)).LT.1.D-3) THEN
          UBOR(K,1) = 0.D0
          VBOR(K,1) = 0.D0
        ENDIF
C       U ET V INITIALISES AVEC LES BONNES VALEURS
        U%R(NBOR(K)) = UBOR(K,1)
        V%R(NBOR(K)) = VBOR(K,1)
        YADEB(NUMLIQ(K))=1
      ENDIF
C
C  VITESSE IMPOSEE : ON UTILISE LA DIRECTION SORTANTE NORMEE
C                    DONNEE PAR L'UTILISATEUR.
C
      IF(LIUBOR(K).EQ.KENTU.AND.
     *  (NVITES.NE.0.OR.NOMIMP(1:1).NE.' ')) THEN
C       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
          IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
            UBOR(K,1) = - XNEBOR(K) * VIT(NUMLIQ(K),NBOR(K))
            VBOR(K,1) = - YNEBOR(K) * VIT(NUMLIQ(K),NBOR(K))
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
            UBOR(K,1) = UBOR(K,2)
            VBOR(K,1) = VBOR(K,2)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
            UBOR(K,1) = - XNEBOR(K) * UBOR(K,2)
            VBOR(K,1) = - YNEBOR(K) * UBOR(K,2)
          ENDIF
        ENDIF
      ENDIF
C
C  TRACEUR IMPOSE
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
        IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT.AND.
     *    (NTRACE.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
C         LE CAS NUMLIQ(K)=0 CORRESPOND A UNE SINGULARITE DECLAREE
C         INITIALEMENT COMME UNE FRONTIERE SOLIDE ET POUR LAQUELLE
C         TBOR EST REMPLI DANS CLHUVT
          IF(NUMLIQ(K).GT.0) THEN
            Z = TR(NUMLIQ(K),ITRAC,NBOR(K),IERR)
            IF(IERR.EQ.0) TBOR%ADR(ITRAC)%P%R(K) = Z
          ENDIF
        ENDIF
        ENDDO
      ENDIF
C
5     CONTINUE
C
C  VITESSE QUADRATIQUE
C                   
      IF(U%ELM .EQ.13)THEN
        DO K=1,NPTFR
          IF(LIUBOR(K+NPTFR).EQ.KENT.AND.
     *  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        U%R(NBOR(K+NPTFR)) = (UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))/2.D0
        V%R(NBOR(K+NPTFR)) = (VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))/2.D0
          ENDIF
        ENDDO 
      ENDIF 
C
C  CAS DES DEBITS IMPOSES :
C
C  BOUCLE SUR LES FRONTIERES LIQUIDES
C
      IF(NFRLIQ.NE.0) THEN
C
      DO 10 IFRLIQ = 1 , NFRLIQ
C
      IF(NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
C
C         ON PREND LE MASQUE DES FRONTIERES LIQUIDES MSK8
C         EGAL AU MASQUE DES DEBITS IMPOSES SUR UNE FRONTIERE
C         A DEBIT IMPOSE. CECI PERMET DE CHANGER UNE FRONTIERE
C         DE VITESSE LIBRE A DEBIT IMPOSE AU NIVEAU DE BORD
C         MALGRE LE FAIT QUE LES MASQUES SONT FAITS DANS PROPIN
C         AVANT L'APPEL DE BORD
C
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
            CALL DEBIMP(Q(IFRLIQ),UBOR,VBOR,U,V,H,NUMLIQ,
     *                  IFRLIQ,TRA05,TRA06,
     *                  NPTFR,MASK%ADR(MSK8)%P%R,MESH,MESH%KP1BOR%I,
     *                  EQUA)
          ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
10    CONTINUE
C
      ENDIF
C
C VITESSES QUADRATIQUES
C
      IF(U%ELM.EQ.13) THEN
        DO K=1,NPTFR
          UBOR(K+NPTFR,1) =(UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))*0.5D0
          VBOR(K+NPTFR,1) =(VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))*0.5D0
        ENDDO 
      ENDIF  
C
C-----------------------------------------------------------------------
C
      RETURN
      END
