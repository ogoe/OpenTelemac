                        SUBROUTINE COUPEH
C                       *****************
C
     *(AT,Z,U,V,W,HREF,NPLREF,PLINF,NC2DH,NPOIN2,nplan,NCOU,BINCOU,
     * VAR,SHZ,NVA3,TAB)
C
C***********************************************************************
C POSTEL3D VERSION 6.0   01/09/99   T. DENOT (LNH) 01 30 87 74 89
C FORTRAN90
C***********************************************************************
C
C     FONCTION  : ECRIT POUR CHAQUE COUPE HORIZONTALES LES VARIABLES
C                      D'UN PAS DE TEMPS
C
C     ATTENTION : LORSQUE LE PLAN DE COUPE SE SITUE EN DEHORS DU DOMAINE
C                 (EN DESSOUS DU FOND OU AU DESSUS DE LA SURFACE) :
C
C                 ON FIXE LES VITESSES HORIZONTALES A ZERO
C                 CE QUI EST BIEN ADAPTE POUR TRACER DES VECTEURS
C
C                 ON EXTRAPOLE LES AUTRES VARIABLES A PARTIR DE LEURS
C                 VALEURS AU PREMIER ETAGE SI EN DESSOUS DU FOND
C                         AU DERNIER ETAGE SI AU DESSUS DE LA SURFACE
C                 CE QUI EST BIEN ADAPTE POUR TRACER DES ISOCOURBES
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !   AT           ! -->! TEMPS CORRESPONDANT AU PAS TRAITE            !
C !   Z            ! -->! COTES DES NOEUDS                             !
C !   U,V,W        ! -->! COMPOSANTES 3D DE LA VITESSE                 !
C !   TA,TP        ! -->! CONCENTRATIONS DES TRACEURS                  !
C !   NUX,NUY,NUZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES VITESSES  !
C !   NAX,NAY,NAZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES TR.ACTIFS !
C !   NPX,NPY,NPZ  ! -->! COEFFICIENTS DE VISCOSITE POUR LES TR.PASSIFS!
C !   RI           ! -->! NOMBRE DE RICHARDSON                         !
C !   AK,EP        ! -->! VARIABLES DU MODELE K-EPSILON                !
C !   RHO          ! -->! ECARTS RELATIFS DE DENSITE                   !
C !   VAR          ! -->! TABLEAU DE TRAVAIL POUR PROJETER LES VARIABLES
C !   SHZ          ! -->! COORDONNEE BARYCENTRIQUE SUIVANT Z           !
C !   HREF         ! -->! DECALAGE PAR RAPPORT AU PLAN DE REFERENCE    !
C !   NPLREF       ! -->! PLAN DE REFERENCE                            !
C !   PLINF        ! -->! PLAN SUITE IMMEDIATEMENT SOUS LA COUPE       !
C !   NC2DH        ! -->! NOMBRE DE COUPES HORIZONTALES                !
C !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
C !   NCOU         ! -->! NUMERO DE CANAL - 1 DE LA PREMIERE COUPE     !
C !   BINCOU       ! -->! STANDARD DE BINAIRE POUR LES COUPES          !
C !   NPLAN        ! -->! NOMBRE DE PLANS                              !
C !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
C !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
C !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMME APPELE PAR : POSTEL3D 
C SOUS-PROGRAMME APPELES : ECRI2
C
C**********************************************************************
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NC2DH,NPOIN2,NCOU,NPLAN,IC,I,J,CANAL
C
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: W(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: Z(NPOIN2,NPLAN)
C
      DOUBLE PRECISION, INTENT(INOUT) :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: HREF(9)
      INTEGER , INTENT(INOUT) :: NPLREF(9)
      INTEGER , INTENT(INOUT) :: PLINF(NPOIN2)
C
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TAB
C
      DOUBLE PRECISION VAR(NPOIN2),SHZ(NPOIN2)
      INTEGER ISTAT
      INTEGER NVA3
      CHARACTER*3 BINCOU
C
      CHARACTER(LEN=2) CB
      DOUBLE PRECISION XB(2)
      INTEGER IB(2)
C
C***********************************************************************
C
C    POUR CHAQUE COUPE HORIZONTALE FAIRE :
C
      DO 1 IC = 1,NC2DH
C
         CANAL = NCOU + IC -1
         XB(1)=AT
         CALL ECRI2(XB,IB,CB,1,'R4',CANAL,BINCOU,ISTAT)
C
         DO 2 I = 1,NPOIN2
            VAR(I) = HREF(IC)
c            IF (NPLREF(IC).GE.1) VAR(I) = VAR(I) + Z(I,NPLREF(IC))
            IF (NPLREF(IC).GE.1) THEN
             VAR(I) = VAR(I) + Z(I,NPLREF(IC))
            ENDIF
            PLINF(I) = 1
2        CONTINUE
C
         IF (NPLAN.GE.3) THEN
            DO 3 J = 2,NPLAN-1
               DO 4 I = 1,NPOIN2
                  IF (Z(I,J).LE.VAR(I)) PLINF(I) = J
4              CONTINUE
3           CONTINUE
         ENDIF
c
C
         DO 5 I = 1,NPOIN2
C..01/2004
C  ATTENTION : Cas des bancs decouvrants (plans confondus)
            SHZ(I) = (          VAR(I)   -Z(I,PLINF(I)))
     *             / MAX((Z(I,PLINF(I)+1)-Z(I,PLINF(I))),1.D-6)
C..01/2004
5        CONTINUE
C
C-----------------------------------------------------------------------
C
C    INDICATEUR DU DOMAINE
C    ---------------------
         DO 8 I = 1,NPOIN2
            VAR(I) = MIN(SHZ(I),1.D0-SHZ(I)) + 1.D-6
8        CONTINUE
         CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
C
C
C    COMPOSANTE U DE LA VITESSE
C    --------------------------
            DO 10 I = 1,NPOIN2
               VAR(I) = 0.D0
               IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     *         VAR(I) = U(I,PLINF(I))*(1.-SHZ(I))+U(I,PLINF(I)+1)*SHZ(I)
10          CONTINUE
            CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
C
C
C    COMPOSANTE V DE LA VITESSE
C    --------------------------
            DO 20 I = 1,NPOIN2
               VAR(I) = 0.D0
               IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     *         VAR(I) = V(I,PLINF(I))*(1.-SHZ(I))+V(I,PLINF(I)+1)*SHZ(I)
20          CONTINUE
            CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)

C    COMPOSANTE W DE LA VITESSE
C    --------------------------
            DO 30 I = 1,NPOIN2
               VAR(I) = W(I,PLINF(I))*(1.-SHZ(I))+W(I,PLINF(I)+1)*SHZ(I)
30          CONTINUE
            CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
C
C
         if (nva3.gt.4) then
         DO J=1,NVA3-4
            DO 40 I = 1,NPOIN2
               VAR(I) = 0.D0
               IF (SHZ(I).GT.-1.D-6.AND.SHZ(I).LT.1.000001D0)
     *            VAR(I) = 
     *          TAB%ADR(J)%P%R((PLINF(I)-1)*NPOIN2+I)*(1.-SHZ(I))
     *        + TAB%ADR(J)%P%R( PLINF(I)   *NPOIN2+I)*    SHZ(I)
40          CONTINUE
            CALL ECRI2(VAR,IB,CB,NPOIN2,'R4',CANAL,BINCOU,ISTAT)
         ENDDO
         endif
C
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
