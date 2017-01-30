!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(H,V,YG,YD,X,T)
!
!***********************************************************************
! PROGICIEL : TELEMAC        07/12/88    F. LEPEINTRE (LNH) 30 71 80 18
!
!***********************************************************************
!
!     FONCTION  : SOLUTION EXACTE DE LA RUPTURE DE BARRAGE SUR
!                 FOND MOUILLE, DANS UN RECTANGLE.
!
!     ATTENTION AUX CONVENTIONS :    ******
!                                    *    *     ^
!                                    *    *     |
!                                    *    * JM  | X
!                                    *    *     |
!                                    *    *     |
!                                    *    *     |
!                                    ******     |
!                                      IM
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   H            |<-- | HAUTEUR D'EAU.
! |   V            |<-- | VITESSE EXACTE.
! |   YG           | -->| HAUTEUR D'EAU A GAUCHE DU BARRAGE.
! |   YD           | -->| HAUTEUR D'EAU A DROITE DU BARRAGE.
! |   X            | -->| COORDONNEES DES POINTS (BARRAGE EN 0.)
! |   T            | -->| TEMPS (DEPART A 0.)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!   SOUS-PROGRAMMES APPELES : DEGRE3, FONCTION FC1
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION T,G,YG,YD,CG,CD,FC1,H(21,81),X(21,81),V(21,81)
      DOUBLE PRECISION EPS,BG,BD,WR,URG,YRG,XC,XD,XE,XCOUR,CRG
!
      INTEGER IM,JM,ITMAX,NOEUD,I,J
!
      EXTERNAL FC1
      COMMON /ICOEC1/ CG,CD
!
!-----------------------------------------------------------------------
!
!     CALCUL DES VALEURS INITIALES DE LA SITUATION THEORIQUE
!     ------------------------------------------------------
!
      IM=21
      JM=81
!
      G=9.81D0
      CG = SQRT(G*YG)
      CD = SQRT(G*YD)
!
!     VITESSE URG ET TIRANT D EAU YRG A GAUCHE DU RESSAUT:
!
      EPS    = 0.D0
      BG     = CG
      BD     = CD
      ITMAX = 100
!
      CALL ZBRENT(FC1,EPS,BG,BD,ITMAX)
!
      CRG = BD
      WR  = (CRG/CD)*SQRT( (CRG**2+CD**2)/2.D0 )
      URG = WR-(CD/CRG)*SQRT( (CRG**2+CD**2)/2.D0  )
      YRG = CRG**2/G
!
!     ---------------------------
!     |VIII) VALEURS THEORIQUES |
!     ---------------------------
!
!
!     DANS LE PLAN (X,T)
!     ON APPELLE C L'INTERSECTION A T AVEC X = -CG*T
!                D L'INTERSECTION A T AVEC X = (URG - CRG)*T
!                E L'INTERSECTION A T AVEC X =  WR*T
!                                    WR:VITESSE DU RESSAUT
!          A ET B LES LIMITES DU DOMAINE D'ETUDE
!          A GAUCHE DE C , EAU IMMOBILE DE COTE YG
!          ENTRE C ET D , ONDE DE DETENTE
!          ENTRE D ET E, EAU A VITESSE URG DE COTE (CRG**2)/G
!          A DROITE DE E , EAU IMMOBILE DE COTE YD
!
!     ABSCISSES DES POINTS C , D ET E
      XC = -CG * T
      XD = (URG - CRG) * T
      XE = WR * T
!
      DO NOEUD=1,JM
!
        XCOUR = X(1,NOEUD)
!
        IF (XCOUR.LE.XC) THEN
!
!         EN XCOUR LE NIVEAU EST DE YG
          H(1,NOEUD)         = YG
          V(1,NOEUD)         = 0.
          CYCLE
        ENDIF
!
        IF (XCOUR.LE.XD) THEN
!
!         EN XCOUR POINT DE L'ONDE DE DETENTE
          H(1,NOEUD) =   ((2./3.*CG-(XCOUR/(3.* T)))**2)/G
          V(1,NOEUD) = 2.*(CG - SQRT(G*H(1,NOEUD)))
          CYCLE
        ENDIF
!
        IF (XCOUR.LE.XE) THEN
!
!         EN XCOUR TIRANT D'EAU EGAL TIRANT A GAUCHE DU RESSAUT
          H(1,NOEUD)= YRG
          V(1,NOEUD)= URG
          CYCLE
        ENDIF
!
!       EN XCOUR TIRANT D'EAU EGAL TIRANT A DROITE DU RESSAUT
        H(1,NOEUD)         = YD
        V(1,NOEUD)         = 0.D0
!
      ENDDO
!
      DO J = 1,JM
        DO I = 2,IM
          H(I,J) = H(1,J)
          V(I,J) = V(1,J)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

