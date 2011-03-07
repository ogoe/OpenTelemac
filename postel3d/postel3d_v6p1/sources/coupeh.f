                        SUBROUTINE COUPEH
!                       *****************
!
     &(AT,Z,U,V,W,HREF,NPLREF,PLINF,NC2DH,NPOIN2,nplan,NCOU,BINCOU,
     & VAR,SHZ,NVA3,TAB)
!
!***********************************************************************
! POSTEL3D VERSION 6.0   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : ECRIT POUR CHAQUE COUPE HORIZONTALES LES VARIABLES
!                      D'UN PAS DE TEMPS
!
!     ATTENTION : LORSQUE LE PLAN DE COUPE SE SITUE EN DEHORS DU DOMAINE
!                 (EN DESSOUS DU FOND OU AU DESSUS DE LA SURFACE) :
!
!                 ON FIXE LES VITESSES HORIZONTALES A ZERO
!                 CE QUI EST BIEN ADAPTE POUR TRACER DES VECTEURS
!
!                 ON EXTRAPOLE LES AUTRES VARIABLES A PARTIR DE LEURS
!                 VALEURS AU PREMIER ETAGE SI EN DESSOUS DU FOND
!                         AU DERNIER ETAGE SI AU DESSUS DE LA SURFACE
!                 CE QUI EST BIEN ADAPTE POUR TRACER DES ISOCOURBES
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
! !   VAR          ! -->! TABLEAU DE TRAVAIL POUR PROJETER LES VARIABLES
! !   SHZ          ! -->! COORDONNEE BARYCENTRIQUE SUIVANT Z           !
! !   HREF         ! -->! DECALAGE PAR RAPPORT AU PLAN DE REFERENCE    !
! !   NPLREF       ! -->! PLAN DE REFERENCE                            !
! !   PLINF        ! -->! PLAN SUITE IMMEDIATEMENT SOUS LA COUPE       !
! !   NC2DH        ! -->! NOMBRE DE COUPES HORIZONTALES                !
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NCOU         ! -->! NUMERO DE CANAL - 1 DE LA PREMIERE COUPE     !
! !   BINCOU       ! -->! STANDARD DE BINAIRE POUR LES COUPES          !
! !   NPLAN        ! -->! NOMBRE DE PLANS                              !
! !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
! !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : POSTEL3D
! SOUS-PROGRAMME APPELES : ECRI2
!
!**********************************************************************
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NC2DH,NPOIN2,NCOU,NPLAN,IC,I,J,CANAL
!
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: W(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: Z(NPOIN2,NPLAN)
!
      DOUBLE PRECISION, INTENT(INOUT) :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: HREF(9)
      INTEGER , INTENT(INOUT) :: NPLREF(9)
      INTEGER , INTENT(INOUT) :: PLINF(NPOIN2)
!
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TAB
!
      DOUBLE PRECISION VAR(NPOIN2),SHZ(NPOIN2)
      INTEGER ISTAT
      INTEGER NVA3
      CHARACTER*3 BINCOU
!
      CHARACTER(LEN=2) CB
      DOUBLE PRECISION XB(2)
      INTEGER IB(2)
!
!***********************************************************************
!
!    POUR CHAQUE COUPE HORIZONTALE FAIRE :
!
      DO 1 IC = 1,NC2DH
!
         CANAL = NCOU + IC -1
         XB(1)=AT
         CALL ECRI2(XB,IB,CB,1,'R4',CANAL,BINCOU,ISTAT)
!
         DO 2 I = 1,NPOIN2
            VAR(I) = HREF(IC)
!            IF (NPLREF(IC).GE.1) VAR(I) = VAR(I) + Z(I,NPLREF(IC))
            IF (NPLREF(IC).GE.1) THEN
             VAR(I) = VAR(I) + Z(I,NPLREF(IC))
            ENDIF
            PLINF(I) = 1
2        CONTINUE
!
         IF (NPLAN.GE.3) THEN
            DO 3 J = 2,NPLAN-1
               DO 4 I = 1,NPOIN2
                  IF (Z(I,J).LE.VAR(I)) PLINF(I) = J
4              CONTINUE
3           CONTINUE
         ENDIF
!
!
         DO 5 I = 1,NPOIN2
!..01/2004
!  ATTENTION : Cas des bancs decouvrants (plans confondus)
            SHZ(I) = (          VAR(I)   -Z(I,PLINF(I)))
     &             / MAX((Z(I,PLINF(I)+1)-Z(I,PLINF(I))),1.D-6)
!..01/2004
5        CONTINUE
!
!-----------------------------------------------------------------------
!
!    INDICATEUR DU DOMAINE
!    ---------------------
         DO 8 I = 1,NPOIN2
            VAR(I) = MIN(SHZ(I),1.D0-SHZ(I)) + 1.D-6
8        CONTINUE
         CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
!
!
!    COMPOSANTE U DE LA VITESSE
!    --------------------------
            DO 10 I = 1,NPOIN2
               VAR(I) = 0.D0
               IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     &         VAR(I) = U(I,PLINF(I))*(1.-SHZ(I))+U(I,PLINF(I)+1)*SHZ(I)
10          CONTINUE
            CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
!
!
!    COMPOSANTE V DE LA VITESSE
!    --------------------------
            DO 20 I = 1,NPOIN2
               VAR(I) = 0.D0
               IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     &         VAR(I) = V(I,PLINF(I))*(1.-SHZ(I))+V(I,PLINF(I)+1)*SHZ(I)
20          CONTINUE
            CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
!
!    COMPOSANTE W DE LA VITESSE
!    --------------------------
            DO 30 I = 1,NPOIN2
               VAR(I) = W(I,PLINF(I))*(1.-SHZ(I))+W(I,PLINF(I)+1)*SHZ(I)
30          CONTINUE
            CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
!
!
         if (nva3.gt.4) then
         DO J=1,NVA3-4
            DO 40 I = 1,NPOIN2
               VAR(I) = 0.D0
               IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     &            VAR(I) =
     &          TAB%ADR(J)%P%R((PLINF(I)-1)*NPOIN2+I)*(1.-SHZ(I))
     &        + TAB%ADR(J)%P%R( PLINF(I)   *NPOIN2+I)*    SHZ(I)
40          CONTINUE
            CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
         ENDDO
         endif
!
!
1     CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END