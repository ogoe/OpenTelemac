!                       *****************
                        SUBROUTINE COUPEV
!                       *****************
!
     &(AT,Z,U,V,W,
     & SHP,IMSEG,X2DV,Y2DV,DISTOR,IKLES,INDIC,
     & ELEM,NC2DV,NPOIN2,NELEM2,NCOU,BINCOU,IM,JM,NVAR,
     & TITCAS,nva3,tab,textlu,N)
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : ECRIT POUR CHAQUE COUPE VERTICALES LES VARIABLES
!                      D'UN PAS DE TEMPS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   AT           ! -->! TEMPS CORRESPONDANT AU PAS TRAITE            !
! !   Z            ! -->! COTES DES NOEUDS                             !
! !   U,V,W        ! -->! COMPOSANTES 3D DE LA VITESSE                 !
! !   TA,TP        ! -->! CONCENTRATIONS DES TRACEURS                  !
! !   NUX,NUY,NUZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES VITESSES  !
! !   NAX,NAY,NAZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES TR.ACTIFS !
! !   NPX,NPY,NPZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES TR.PASSIFS!
! !   RI           ! -->! NOMBRE DE RICHARDSON                         !
! !   AK,EP        ! -->! VARIABLES DU MODELE K-EPSILON                !
! !   RHO          ! -->! ECARTS RELATIFS DE DENSITE                   !
! !   SHP          ! -->! COORDONNEES BARYCENTRIQUES DES PTS DE COUPE  !
! !   TAB1,2,3     !<-- ! TABLEAU DE TRAVAIL POUR PROJETER LES VAR.    !
! !   NSEG         ! -->! NOMBRE DE SEGMENTS CONSTITUANT CHAQUE COUPE  !
! !   IMSEG        ! -->! NOMBRE DE POINTS PAR SEGMENTS                !
! !   X2DV         ! -->! ABSCISSES DES SOMMETS DES COUPES VERTICALES  !
! !   Y2DV         ! -->! ORDONNEES DES SOMMETS DES COUPES VERTICALES  !
! !   DISTOR       ! -->! DISTORSION SUIVANT Z DE CHAQUE COUPE VERTICALE
! !   IKLES        ! -->! TABLE DE CONNECTIVITE                        !
! !   INDIC        ! -->! INDICATEUR DE LA NATURE DES POINTS           !
! !   ELEM         ! -->! NUMERO DES ELEMENTS CONTENANT LES PTS DE COUPE
! !   NC2DV        ! -->! NOMBRE DE COUPES VERTICALES                  !
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NELEM2       ! -->! NOMBRE D'ELEMENTS DU MAILLAGE 2D             !
! !   NCOU         ! -->! NUMERO DE CANAL - 1 DE LA PREMIERE COUPE     !
! !   BINCOU       ! -->! STANDARD DE BINAIRE POUR LES COUPES          !
! !   IM (LU)      ! -->! NOMBRE DE PTS DE COUPE SUIVANT L'HORIZONTALE !
! !   JM (=NPLAN)  ! -->! NOMBRE DE PTS DE COUPE SUIVANT LA VERTICALE  !
! !   NVAR         ! -->! NOMBRE DE VARIABLES ENREGISTREES             !
! !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
! !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !   TITCAS       ! -->! TITRE A PORTER SUR CHAQUE COUPE              !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : POSTEL3D
! SOUS-PROGRAMME APPELES : ECRDEB , ECRI2
!
!**********************************************************************
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPOIN2,NELEM2,NCOU,IM,JM,NC2DV,NVAR(1),NTRAC,NTRPA
      INTEGER , INTENT(IN) :: N
!
      DOUBLE PRECISION U(NPOIN2,JM),V(NPOIN2,JM),W(NPOIN2,JM)
      DOUBLE PRECISION Z(NPOIN2,JM)
      DOUBLE PRECISION TAB1(IM,JM),TAB2(IM,JM),TAB3(IM,JM)
      DOUBLE PRECISION X2DV(50,NC2DV),Y2DV(50,NC2DV),DISTOR(NC2DV)
      DOUBLE PRECISION LGDEB,LGSEG,ALFA,COST,SINT,A1,A2,A3,U1,V1
!      DOUBLE PRECISION , INTENT(INOUT) :: AT
      DOUBLE PRECISION ,INTENT(IN) ::AT
      DOUBLE PRECISION , INTENT(INOUT) :: SHP(IM,3,NC2DV)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TAB
!
      INTEGER IKLES(3,NELEM2),INDIC(IM,JM,NC2DV),ELEM(IM,NC2DV)
      INTEGER IMSEG(49,NC2DV)
      INTEGER NBV(2),IG(5),IB(10),IC,N1,N2,N3,I,J,K,CANAL,IBID(1),ISTAT
      INTEGER ISEG,IDSEG,IFSEG
      INTEGER NVA3
!
!
      LOGICAL FLAG
!
      CHARACTER*32 TEXTLU(100)
      CHARACTER*72 TITCAS
      CHARACTER*3  BINCOU
!
      CHARACTER(LEN=2) CB
      DOUBLE PRECISION XB(2)
!
!***********************************************************************
!
!  NOMBRE DE VARIABLES EN SORTIE :
!  (ON NE SORT PAS LES VITESSES SI ON N'A PAS LES 3 COMPOSANTES
!   ET ON NE SORT PAS LES VARIABLES QUI SERVENT A SUBIEF-3D)
!
! plus de z
      NBV(1) = NVA3-1
      NBV(2) = 0
!
!  DIMENSIONS DES GRILLES
!
!   IG(1), IG(2) : DIMENSIONS GRILLE 1.
      IG(1)=IM
      IG(2)=JM
!   IG(3), IG(4) : DIMENSIONS GRILLE 2.
      IG(3)=IM
      IG(4)=JM
!   IG(5) : DECALAGE DE LA GRILLE 2 PAR RAPPORT A LA GRILLE 1.
      IG(5)=1
!
!  LISTE DE FUTURS PARAMETRES DEJA PREVUS.(SEUL LES PREMIERS SERVENT)
!
      DO 1 I=1,10
         IB(I)=0
1     CONTINUE
!   ECRITURE ECLATEE DES RESULTATS (CONVENTION LEONARD)
      IB(2)=1
!
!-----------------------------------------------------------------------
!
!  POUR CHAQUE COUPE VERTICALE FAIRE :
!
!     CANCELLING WHAT HAS DONE OPEN_FILES
      CLOSE(NCOU)
!
      DO 2 IC = 1,NC2DV
!
         CANAL = NCOU + IC -1
!
!    OUVERTURE DU FICHIER + ENREGISTREMENT DES PREMIERS PARAMETRES
!    -------------------------------------------------------------
!
         CALL ECRDEB(CANAL,BINCOU,TITCAS,NBV,NTRAC,NTRPA,.FALSE.,TEXTLU,
     &               IC,N)
!
!    CALCUL DES AUTRES PARAMETRES DE L'ENTETE
!    ----------------------------------------
!
!    MAILLAGE LEONARD ASSOCIE A LA COUPE IC ET AU PAS DE TEMPS IT
!
         ISEG = 0
         IFSEG = 1
         LGDEB = 0.D0
         LGSEG = 0.D0
!
         DO 3 I = 1,IM
!
!    COORDONNEE HORIZONTALE SUIVANT LE PLAN DE COUPE (X)
!
            IF (I.GT.IFSEG.OR.I.EQ.1) THEN
               ISEG = ISEG + 1
               IDSEG = IFSEG
               IFSEG = IFSEG + IMSEG(ISEG,IC)
               LGDEB = LGDEB + LGSEG
               LGSEG = SQRT((X2DV(ISEG+1,IC)-X2DV(ISEG,IC))**2
     &                     +(Y2DV(ISEG+1,IC)-Y2DV(ISEG,IC))**2)
            ENDIF
!
            TAB1(I,1) = LGDEB + FLOAT(I-IDSEG)*LGSEG/FLOAT(IFSEG-IDSEG)
!
!    COORDONNEE VERTICALE (Y)
!
            DO 4 J = 1,JM
!
               TAB1(I,J) = TAB1(I,1)
               TAB2(I,J) = ( SHP(I,1,IC)*Z(IKLES(1,ELEM(I,IC)),J)
     &                     + SHP(I,2,IC)*Z(IKLES(2,ELEM(I,IC)),J)
     &                     + SHP(I,3,IC)*Z(IKLES(3,ELEM(I,IC)),J) )
     &                     * DISTOR(IC)
!
4           CONTINUE
3        CONTINUE
!
!    ENREGISTREMENT DES AUTRES PARAMETRES DE L'ENTETE
!    ------------------------------------------------
!
         CALL ECRI2(XB,IG,CB, 5,'I',CANAL,BINCOU,ISTAT)
         CALL ECRI2(XB,IB,CB,10,'I',CANAL,BINCOU,ISTAT)
         CALL ECRI2(TAB1,IBID,CB,IM*JM,'R4',CANAL,BINCOU,ISTAT)
         CALL ECRI2(TAB2,IBID,CB,IM*JM,'R4',CANAL,BINCOU,ISTAT)
         CALL ECRI2(XB,INDIC(1,1,IC),CB,IM*JM,'I',CANAL,BINCOU,ISTAT)
!
!-----------------------------------------------------------------------
!
!    SORTIE DES VARIABLES
!
         XB(1)=AT
         CALL ECRI2(XB,IBID,CB,1,'R4',CANAL,BINCOU,ISTAT)
!
!    3 COMPOSANTES DE LA VITESSE
!    ---------------------------
!
!
            ISEG = 1
            IFSEG = 1 + IMSEG(1,IC)
            ALFA = ATAN2(Y2DV(2,IC)-Y2DV(1,IC),X2DV(2,IC)-X2DV(1,IC))
            FLAG = .TRUE.
!
            DO 10 I = 1,IM
!
               IF (FLAG) COST = COS(ALFA)
               IF (FLAG) SINT = SIN(ALFA)
               FLAG = .FALSE.
!
               IF (I.EQ.IFSEG.AND.I.NE.IM) THEN
                  FLAG = .TRUE.
                  ISEG = ISEG + 1
                  IFSEG = IFSEG + IMSEG(ISEG,IC)
                  A1 = ALFA
                  ALFA = ATAN2(Y2DV(ISEG+1,IC)-Y2DV(ISEG,IC),
     &                         X2DV(ISEG+1,IC)-X2DV(ISEG,IC))
                  COST = COS(0.5D0*(ALFA+A1))
                  SINT = SIN(0.5D0*(ALFA+A1))
               ENDIF
!
               N1 = IKLES(1,ELEM(I,IC))
               N2 = IKLES(2,ELEM(I,IC))
               N3 = IKLES(3,ELEM(I,IC))
               A1 = SHP(I,1,IC)
               A2 = SHP(I,2,IC)
               A3 = SHP(I,3,IC)
!
               DO 11 J = 1,JM
!
                  U1 = A1*U(N1,J) + A2*U(N2,J) + A3*U(N3,J)
                  V1 = A1*V(N1,J) + A2*V(N2,J) + A3*V(N3,J)
!
!       COMPOSANTE TANGENTIELLE ET HORIZONTALE DE LA VITESSE (UT)
!
               TAB1(I,J) = COST*U1 + SINT*V1
!
!       COMPOSANTE VERTICALE DE LA VITESSE (W)
!
               TAB2(I,J) = (A1*W(N1,J)+A2*W(N2,J)+A3*W(N3,J))*DISTOR(IC)
!
!       COMPOSANTE NORMALE ET HORIZONTALE DE LA VITESSE (UN)
!
               TAB3(I,J) = -SINT*U1 + COST*V1
!
11             CONTINUE
10          CONTINUE
!
            CALL ECRI2(TAB1,IBID,CB,IM*JM,'R4',CANAL,BINCOU,ISTAT)
            CALL ECRI2(TAB2,IBID,CB,IM*JM,'R4',CANAL,BINCOU,ISTAT)
            CALL ECRI2(TAB3,IBID,CB,IM*JM,'R4',CANAL,BINCOU,ISTAT)
!
!
! autres variables
!
          IF (NBV(1).GT.3) THEN
            DO 30 K = 1,NBV(1)-3
               DO 31 J = 1,JM
                  DO 32 I = 1,IM
                     TAB1(I,J) = SHP(I,1,IC)
     &               *TAB%ADR(K)%P%R(IKLES(1,ELEM(I,IC))+(J-1)*NPOIN2)
     &                         + SHP(I,2,IC)
     &               *TAB%ADR(K)%P%R(IKLES(2,ELEM(I,IC))+(J-1)*NPOIN2)
     &                         + SHP(I,3,IC)
     &               *TAB%ADR(K)%P%R(IKLES(3,ELEM(I,IC))+(J-1)*NPOIN2)
32                CONTINUE
31             CONTINUE
               CALL ECRI2(TAB1,IBID,CB,IM*JM,'R4',CANAL,BINCOU,ISTAT)
30          CONTINUE
!
          ELSE
          ENDIF
!
      CLOSE(CANAL)
2     CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END