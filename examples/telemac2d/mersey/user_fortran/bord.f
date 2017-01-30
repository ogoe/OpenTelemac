!                       ***************
                        SUBROUTINE BORD
!                       ***************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.0    24/04/97    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!      FONCTION:    MODIFIE LES TABLEAUX DE CONDITIONS AUX LIMITES
!                   DANS LE CAS OU ELLES SONT VARIABLES EN TEMPS.
!
!      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
!      SOIT DIRECTEMENT, SOIT PAR L'INTERMEDIAIRE DES FONCTIONS :
!
!             Q , SL , TR , VIT
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   HBOR         |<-- |  HAUTEUR IMPOSEE.                            |
! |   UBOR         |<-- |  VITESSE U IMPOSEE.                          |
! |   VBOR         |<-- |  VITESSE V IMPOSEE.                          |
! |   TBOR         |<-- |  TRACEUR IMPOSE AU BORD                      |
! |    U,V         | -->|  COMPOSANTES DE LA VITESSE AU TEMPS N        |
! |    H           | -->|  HAUTEUR AU TEMPS N                          |
! |    ZF          | -->|  FOND                                        |
! |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
! |  TRA05,TRA06   | -->|  TABLEAUX DE TRAVAIL                         |
! |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
! | LIUBOR         | -->|  CONDITIONS AUX LIMITES SUR U
! |   LITBOR       | -->|  CONDITIONS AUX LIMITES SUR LE TRACEUR       |
! |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
! |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
! |   TEMPS        | -->|  TEMPS                                       |
! |   DEBIT        |<-->|  TABLEAU DE DEBITS IMPOSES                   |
! |   NDEBIT       | -->|  NOMBRE DE FRONTIERES A DEBIT IMPOSE         |
! |   COTE         |<-->|  TABLEAU DE COTES DE LA SURFACE LIBRE IMPOSEES
! |   COTINI       | -->|  COTE INITIALE
! |   NCOTE        | -->|  NOMBRE DE FRONTIERES A COTE IMPOSEE         |
! |   VITES        |<-->|  TABLEAU DE COMPOSANTES NORMALES DE LA VITESSE
! |                |    |  IMPOSEES                                    |
! |   NVITES       | -->|  NOMBRE DE FRONTIERES A VITESSE IMPOSEE      |
! |   TRAC         | -->|  LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR  |
! |   TRACER       |<-->|  TABLEAU DE VALEURS DU TRACEUR IMPOSEES      |
! |   NTRACE       | -->|  NOMBRE DE FRONTIERES A TRACEUR IMPOSE       |
! |   NFRLIQ       | -->|  NOMBRE DE FRONTIERES LIQUIDES
! |   KENT,KENTU,  | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
! |                |    |  KENTU:U ET V IMPOSES                        |
! |   PROVEL       | -->|  OPTION POUR LES PROFILS DE VITESSE          |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : TELMAC
!
! SOUS-PROGRAMME APPELE : DEBIMP
!
! FONCTIONS APPELEES : Q , SL , TR , VIT
!
!***********************************************************************
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER K,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE,MSK1,NPTFR2
      INTEGER NPOIN,NFRLIQ,ITRAC,IERR
      INTEGER IFRLIQ
      INTEGER KENT,KENTU,NTRAC
      INTEGER NBOR(NPTFR)
      INTEGER LIHBOR(NPTFR) , LIUBOR(NPTFR)
      INTEGER PROVEL(*)
      INTEGER NUMLIQ(NPTFR)
!
      INTEGER YADEB(100)
!
      DOUBLE PRECISION HBOR(NPTFR),UBOR(NPTFR,2),VBOR(NPTFR,2)
      DOUBLE PRECISION ZF(NPOIN)
      DOUBLE PRECISION XNEBOR(NPTFR),YNEBOR(NPTFR)
!
      DOUBLE PRECISION TEMPS,Z
      DOUBLE PRECISION AT1, H1, PI
!
      CHARACTER(LEN=20) EQUA
      CHARACTER(LEN=144), INTENT(IN) :: NOMIMP
!
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ)  :: MASK,H,U,V,TRA05,TRA06,TBOR,LITBOR
!
      INTEGER P_IMAX
      DOUBLE PRECISION Q,SL,VIT,TR
      EXTERNAL         Q,SL,VIT,TR ,P_IMAX
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
      MSK1 = 1
!
!  INITIALISATION DE YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
!  BOUCLE SUR TOUS LES POINTS FRONTIERE
!
      DO K=1,NPTFR
!
!  COTE IMPOSEE AVEC VALEUR DONNEE DANS LE FICHIER CAS (NCOTE<>0)
!
      IF(LIHBOR(K).EQ.KENT.AND.NCOTE.NE.0) THEN
!
        IF(NCOTE.GE.NUMLIQ(K)) THEN
          PI=3.141592653589D0
          AT1=TEMPS/44700.D0
          H1 = 5.15D0+4.05D0*COS(PI*2.D0*AT1)
          HBOR(K) = -ZF(NBOR(K)) + H1
          HBOR(K) = MAX(0.D0,HBOR(K))
!
          H%R(NBOR(K))=HBOR(K)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) NUMLIQ(K)
100       FORMAT(1X,'BORD : COTES IMPOSEES EN NOMBRE INSUFFISANT',/,
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,101) NUMLIQ(K)
101       FORMAT(1X,'BORD : MORE PRESCRIBED ELEVATIONS ARE REQUIRED',/,
     &           1X,'       IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          STOP
        ENDIF
!
      ENDIF
!
!  DEBIT IMPOSE : DIFFERENTES OPTIONS SUIVANT PROVEL
!                 ON UTILISE LES VALEURS DONNEES PAR L'UTILISATEUR
!                 COMME PROFIL DE VITESSE.
!                 UBOR(K,2) ET VBOR(K,2) SONT LES VALEURS DU
!                 FICHIER CONLIM CONSERVEES.
!
!                 ON NE MET PAS DE VITESSE SI IL N'Y A PAS D'EAU.
!
!
      IF(LIUBOR(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
        IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
!         PROFIL NORMAL CONSTANT
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
!         PROFIL DONNE PAR L'UTILISATEUR
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
!         VITESSE NORMALE DONNEE DANS UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ENDIF
        IF(H%R(NBOR(K)).LT.1.D-3) THEN
          UBOR(K,1) = 0.D0
          VBOR(K,1) = 0.D0
        ENDIF
!       U ET V INITIALISES AVEC LES BONNES VALEURS
        U%R(NBOR(K)) = UBOR(K,1)
        V%R(NBOR(K)) = VBOR(K,1)
        YADEB(NUMLIQ(K))=1
      ENDIF
!
!  VITESSE IMPOSEE : ON UTILISE LA DIRECTION SORTANTE NORMEE
!                    DONNEE PAR L'UTILISATEUR.
!
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
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,201) NUMLIQ(K)
201       FORMAT(1X,'BORD : MORE PRESCRIBED VELOCITIES ARE REQUIRED',/,
     &           1X,'       IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          STOP
        ENDIF
      ENDIF
!
!  TRACEUR IMPOSE
!
      IF(NTRAC.GT.0) THEN
      DO ITRAC=1,NTRAC
        IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT.AND.NTRACE.NE.0) THEN
        IF(NTRACE.GE.NUMLIQ(K)) THEN
!         LE CAS NUMLIQ(K)=0 CORRESPOND A UNE SINGULARITE DECLAREE
!         INITIALEMENT COMME UNE FRONTIERE SOLIDE ET POUR LAQUELLE
!         TBOR EST REMPLI DANS CLHUVT
          IF(NUMLIQ(K).NE.0) THEN
            TBOR%ADR(ITRAC)%P%R(K) = TR(NUMLIQ(K),ITRAC,NBOR(K),IERR)
          ENDIF
        ELSE
          IF(LNG.EQ.1) WRITE(LU,300) NUMLIQ(K)
300       FORMAT(1X,'BORD : VALEURS IMPOSEES DU TRACEUR',/,
     &           1X,'       EN NOMBRE INSUFFISANT',/,
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,301) NUMLIQ(K)
301       FORMAT(1X,'BORD : MORE PRESCRIBED TRACER VALUES',/,
     &           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          STOP
        ENDIF
        ENDIF
      ENDDO
      ENDIF
!
      ENDDO
!
!  CAS DES DEBITS IMPOSES :
!
!  BOUCLE SUR LES FRONTIERES LIQUIDES
!
      IF(NFRLIQ.NE.0) THEN
!
      DO IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.NE.0) THEN
!
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
            CALL DEBIMP(Q(IFRLIQ),UBOR,VBOR,U,V,H,NUMLIQ,
     &                  IFRLIQ,TRA05,TRA06,
     &                  NPTFR,MASK%ADR(MSK1)%P%R,MESH,MESH%KP1BOR%I,
     &                  EQUA)
          ENDIF
        ELSE
          IF(LNG.EQ.1) WRITE(LU,400) IFRLIQ
400       FORMAT(1X,'BORD : DEBITS IMPOSES',/,
     &           1X,'       EN NOMBRE INSUFFISANT',/,
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) IFRLIQ
401       FORMAT(1X,'BORD : MORE PRESCRIBED FLOWRATES',/,
     &           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

