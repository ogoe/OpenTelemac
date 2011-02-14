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
C  TELEMAC 2D VERSION 5.0    24/04/97    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C      FONCTION:    MODIFIE LES TABLEAUX DE CONDITIONS AUX LIMITES
C                   DANS LE CAS OU ELLES SONT VARIABLES EN TEMPS.
C
C      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
C      SOIT DIRECTEMENT, SOIT PAR L'INTERMEDIAIRE DES FONCTIONS :
C
C             Q , SL , TR , VIT
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
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER K,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE,MSK1,NPTFR2
      INTEGER NPOIN,NFRLIQ,ITRAC,IERR
      INTEGER IFRLIQ
      INTEGER KENT,KENTU,NTRAC
      INTEGER NBOR(NPTFR)
      INTEGER LIHBOR(NPTFR) , LIUBOR(NPTFR)
      INTEGER PROVEL(100)
      INTEGER NUMLIQ(NPTFR)
C
      INTEGER YADEB(100)
C
      DOUBLE PRECISION HBOR(NPTFR),UBOR(NPTFR,2),VBOR(NPTFR,2) 
      DOUBLE PRECISION ZF(NPOIN) 
      DOUBLE PRECISION XNEBOR(NPTFR),YNEBOR(NPTFR)
C
      DOUBLE PRECISION TEMPS,Z
      DOUBLE PRECISION AT1, H1, PI
C
      CHARACTER(LEN=20) EQUA
      CHARACTER(LEN=144), INTENT(IN) :: NOMIMP
C
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ)  :: MASK,H,U,V,TRA05,TRA06,TBOR,LITBOR
C
      INTEGER P_IMAX
      DOUBLE PRECISION Q,SL,VIT,TR
      EXTERNAL         Q,SL,VIT,TR ,P_IMAX
      INTRINSIC MAX
C
C-----------------------------------------------------------------------
C
      MSK1 = 1
C
C  INITIALISATION DE YADEB
C
      IF(NFRLIQ.GE.1) THEN
        DO 1 K=1,NFRLIQ
          YADEB(K)=0
1       CONTINUE
      ENDIF
C
C  BOUCLE SUR TOUS LES POINTS FRONTIERE
C
      DO 5 K=1,NPTFR
C
C  COTE IMPOSEE AVEC VALEUR DONNEE DANS LE FICHIER CAS (NCOTE<>0)
C
      IF(LIHBOR(K).EQ.KENT.AND.NCOTE.NE.0) THEN
C
        IF(NCOTE.GE.NUMLIQ(K)) THEN
          PI=3.141592653589D0                                                  
          AT1=TEMPS/44700.D0                                                   
          H1 = 5.15D0+4.05D0*COS(PI*2.D0*AT1)  
          HBOR(K) = -ZF(NBOR(K)) + H1                                       
          HBOR(K) = MAX(0.D0,HBOR(K))                                       
C                                                                          
          H%R(NBOR(K))=HBOR(K)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) NUMLIQ(K)
100       FORMAT(1X,'BORD : COTES IMPOSEES EN NOMBRE INSUFFISANT',/,
     *           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     *           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,101) NUMLIQ(K)
101       FORMAT(1X,'BORD : MORE PRESCRIBED ELEVATIONS ARE REQUIRED',/,
     *           1X,'       IN THE PARAMETER FILE',/,
     *           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          STOP
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
C                 ON NE MET PAS DE VITESSE SI IL N'Y A PAS D'EAU.
C
C
      IF(LIUBOR(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
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
        ENDIF
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
      IF(LIUBOR(K).EQ.KENTU.AND.NVITES.NE.0) THEN
        IF(NVITES.GE.NUMLIQ(K)) THEN
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
        ELSE
          IF(LNG.EQ.1) WRITE(LU,200) NUMLIQ(K)
200       FORMAT(1X,'BORD : VITESSES IMPOSEES EN NOMBRE INSUFFISANT',/,
     *           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     *           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,201) NUMLIQ(K)
201       FORMAT(1X,'BORD : MORE PRESCRIBED VELOCITIES ARE REQUIRED',/,
     *           1X,'       IN THE PARAMETER FILE',/,
     *           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          STOP
        ENDIF
      ENDIF
C
C  TRACEUR IMPOSE
C
      IF(NTRAC.GT.0) THEN
      DO ITRAC=1,NTRAC
        IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT.AND.NTRACE.NE.0) THEN
        IF(NTRACE.GE.NUMLIQ(K)) THEN
C         LE CAS NUMLIQ(K)=0 CORRESPOND A UNE SINGULARITE DECLAREE
C         INITIALEMENT COMME UNE FRONTIERE SOLIDE ET POUR LAQUELLE
C         TBOR EST REMPLI DANS CLHUVT
          IF(NUMLIQ(K).NE.0) THEN
            TBOR%ADR(ITRAC)%P%R(K) = TR(NUMLIQ(K),ITRAC,NBOR(K),IERR)
          ENDIF
        ELSE
          IF(LNG.EQ.1) WRITE(LU,300) NUMLIQ(K)
300       FORMAT(1X,'BORD : VALEURS IMPOSEES DU TRACEUR',/,
     *           1X,'       EN NOMBRE INSUFFISANT',/,
     *           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     *           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,301) NUMLIQ(K)
301       FORMAT(1X,'BORD : MORE PRESCRIBED TRACER VALUES',/,
     *           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     *           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          STOP
        ENDIF
        ENDIF
      ENDDO
      ENDIF
C
5     CONTINUE
C
C  CAS DES DEBITS IMPOSES :
C
C  BOUCLE SUR LES FRONTIERES LIQUIDES
C
      IF(NFRLIQ.NE.0) THEN
C
      DO 10 IFRLIQ = 1 , NFRLIQ
C
      IF(NDEBIT.NE.0) THEN
C
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
            CALL DEBIMP(Q(IFRLIQ),UBOR,VBOR,U,V,H,NUMLIQ,
     *                  IFRLIQ,TRA05,TRA06,
     *                  NPTFR,MASK%ADR(MSK1)%P%R,MESH,MESH%KP1BOR%I,
     *                  EQUA)
          ENDIF
        ELSE
          IF(LNG.EQ.1) WRITE(LU,400) IFRLIQ
400       FORMAT(1X,'BORD : DEBITS IMPOSES',/,
     *           1X,'       EN NOMBRE INSUFFISANT',/,
     *           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     *           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) IFRLIQ
401       FORMAT(1X,'BORD : MORE PRESCRIBED FLOWRATES',/,
     *           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     *           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          STOP
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
C-----------------------------------------------------------------------
C
      RETURN
      END 

