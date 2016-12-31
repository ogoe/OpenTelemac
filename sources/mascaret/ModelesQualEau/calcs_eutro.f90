!== Copyright (C) 2000-2016 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

SUBROUTINE CALCS_EUTRO( RNU , S , &
                        Nbsect , NBTRA , Nbsing , Singularite , &
                        Q , A , H , RH , ST , C , &
                        SA , T , TParph , TMeteo , DT , Erreur )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL
!
! VERSION : 8.1.1              EDF-CEREMA
!***********************************************************************
!
!  FONCTION : MODELE DE QUALITE D'EAU EUTRO
!  --------
!
! CE SOUS PROGRAMME CALCULE LES TERMES SOURCES IMPLICITES
! ET EXPLICITES, VOLUMIQUES ET SURFACIQUES,
! UTILISEES DANS L'EQUATION DE CONSERVATION DU TRACEUR
!
!       POUR UN PROBLEME D'OXYGENE DISSOUS EN RIVIERE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! ! NBTRA     ! E  ! M  ! NOMBRE DE TRACEURS                           !
! !  Q        ! TR ! D  ! DEBIT                                        !
! !  A        ! TR ! D  ! SECTION MOUILLEE                             !
! !  Z        ! TR ! D  ! HAUTEUR D EAU                                !
! !  RH       ! TR ! D  ! RAYON HYDRAULIQUE                            !
! !  ST       ! TR ! D  ! STRICKLER                                    !
! !  IM       ! E  ! M  ! NOMBRE DE SECTIONS DE CALCUL                 !
! !  C        ! TR ! D  ! CONCENTRATIONS                               !
! !  SVA      ! TR ! D  ! TERMES SOURCES VOLUMIQUE AJOUTES             !
! !  SSA      ! TR ! D  ! TERME SOURCE SURFACIQUE  AJOUTES             !
! !  T        !  R ! D  ! TEMPS                                        !
! !  DT       !  R ! D  ! PAS DE TEMPS                                 !
!  RESULTATS------------------------------------------------------------
! !  RNUV     ! TR ! D  ! TERMES SOURCES VOLUMIQUES IMPLICITES         !
! !  RNUS     ! TR ! D  ! TERME SOURCE SURFACIQUE IMPLICITES           !
! !  SV       ! TR ! D  ! TERMES SOURCES  EXPLICITES                   !
! !___________!____!____!______________________________________________!
!                               COMMON
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  NMSCAL   ! E  ! M  ! NOMBRE MAXIMUM DE SECTIONS DE CALCUL         !
! !  NMTRA    ! E  ! M  ! NOMBRE MAXIMUM DE TRACEURS                   !
! !___________!____!____!______________________________________________!
!                          VARIABLES INTERNES
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  U        ! TR ! D  ! VITESSE DE L'EAU                             !
! !  J        ! TR ! D  ! PENTE DE LA LIGNE DE CHARGE                  !
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20°C     !
! !  PS       ! R  !    ! PROFONDEUR DE SECCHI                         !
! !  KPE      ! R  !    ! COEF D EXTINCTION DU RAY SANS PHYTO          !
! !  BETA     ! R  !    ! COEF DE TURBIDITE VEGETALE                   !
! !  IK       ! R  !    ! PARAMETRE DE CALAGE DE LA FORMULE DE SMITH   !
! !  KP       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN PHOSPHATE    !
! !  KN       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN AZOTE        !
! !  ALPHA    ! R  !    ! COEF 1 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  ALPHA2   ! R  !    ! COEF 2 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  RP       ! R  !    ! TAUX DE RESP. DE LA BIOMASSE ALGALE A 20°C   !
! !  FP       ! R  !    ! PROP DE PHOSPHORE DANS LES CELLULES DU PHYTO !
! !  DTP      ! R  !    ! POURCENT DE PHOSPH DIRECT ASSIM DS PHY MORT  !
! !  K320     ! R  !    ! TAUX DE TRANSFORMATION DU POR EN PO4         !
! !  FN       ! R  !    ! PROP D AZOTE DANS LES CELLULES DU PHYTO      !
! !  DTN      ! R  !    ! POURCENT D AZOTE DIRECT ASSIM DS PHY MORT    !
! !  K620     ! R  !    ! TAUX DE TRANSFORMATION DU NOR EN NO3         !
! !  M1       ! R  !    ! COEF 1 DE MORTALITE ALGALE A 20°C            !
! !  M2       ! R  !    ! COEF 2 DE MORTALITE ALGALE A 20°C            !
! !  WLOR     ! R  !    ! VITESSE DE SEDIMENTATION DE LA CHARGE ORGANIQ!
! !  K120     ! R  !    ! CINETIQUE DE DEGRADATION DE LA CHARGE ORGANIQ!
! !  K520     ! R  !    ! CINETIQUE DE NITRIFICATION                   !
! !  F        ! R  !    ! QTTE D O2 PRODUITE PAR PHOTOSYNTHESE         !
! !  N        ! R  !    ! QTTE D O2 CONSOMMEE PAR NITRIFICATION        !
! !  BEN      ! R  !    ! DEMANDE BENTHIQUE A 20°C                     !
! !  K2       ! R  !    ! COEFFICIENT DE REAERATION                    !
! !  FORMK2   ! E  !    ! FORMULE DE CALCUL DE K2                      !
! !  CS       ! R  !    ! CONCENTRATION DE SATURATION EN OXYG DE L'EAU !
! !  FORMCS   ! E  !    ! FORMULE DE CALCUL DE CS                      !
! !  RS       ! R  !    ! COEFFICIENT DE REAERATION AUX SEUILS         !
! !  FORMRS   ! E  !    ! FORMULE DE CALCUL DE R                       !
! !  ARS      ! R  !    ! COEFFICIENT A DES FORMULES DE CALCUL DE R    !
! !  BRS      ! R  !    ! COEFFICIENT B DES FORMULES DE CALCUL DE R    !
! !  NBSEUI   ! E  !    ! NOMBRE DE SEUILS                             !
! !  XSEUI    ! TR !    ! ABSCISSES DES SEUILS                         !
! !  DZS      ! TR !    ! DELTA Z AUX SEUILS                           !
! !           !    !    !                                              !
! !  IF1      ! TR ! D  ! INDIC DE LECTURE DU FICHIER DES PARAMETRES   !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!***********************************************************************

   USE  M_PRECISION
   USE  M_PARAMETRE_C
   USE  M_CONSTANTES_TRACER_T
   USE  M_PARAMETRES_QUALITE_EAU_T
   USE  M_METEO_T
   USE  M_ERREUR_T
   USE  M_INTERPOLATION_S
   USE  M_SINGULARITE_T

   IMPLICIT NONE

   REAL(DOUBLE)       ,DIMENSION(:,:),intent(inout) :: RNU , S , SA
   REAL(DOUBLE)       ,DIMENSION(:)  ,intent(in   ) :: Q , A , H , ST , RH
   REAL(DOUBLE)       ,DIMENSION(:,:),intent(inout) :: C
   TYPE(Singularite_T),DIMENSION(:)  ,intent(in   ) :: Singularite
   INTEGER                                          :: Nbsect , NBTRA , nbsing
   REAL(DOUBLE)                                     :: T , DT
   !
   !  DECLARATION DES PARAMETRES PHYSIQUES
   !
   TYPE (PARAMETRES_QUALITE_EAU_T )         :: TPARPH
   TYPE (METEO_T)                           :: TMeteo
   TYPE (ERREUR_T)                          :: Erreur
   !
   ! VARIABLES LOCALES
   !
   REAL(DOUBLE), DIMENSION (Nbsect)       :: LNUT , IH , CP , DP , MP , KE
   REAL(DOUBLE), DIMENSION (Nbsect,nbtra) :: SS , SV
   REAL(DOUBLE), DIMENSION (Nbsect)       :: U , PJ
   REAL(DOUBLE), DIMENSION (Nbsing)       :: TRS , ARS , BRS
   REAL(DOUBLE), DIMENSION (Nbsect)       :: TK2 , RAY , RN
   INTEGER     , DIMENSION (Nbsing)       :: NUMSEUI
   !
   REAL(DOUBLE) WPOR , WNOR , CMAX , PS , KPE , BETA , IK , KP , KN , ALPHA1 , ALPHA2
   REAL(DOUBLE) RP , FP , DTP , K320 , FN , DTN , K620 , M1 , M2 , WLOR , K120 , K520
   REAL(DOUBLE) F , N , BEN , K2 , RS , CORR2 , CORR3 , CORR4 , DZ , TEMP , I0
   REAL(DOUBLE) CORR1T , CORR2T , CORR3T , CORR4T , CORRBEN , CS
   INTEGER I , J , K , KP1 , FORMK2 , FORMCS , FORMRS , NBSEUI
   INTEGER :: IF1 = 0
   !
   !  FONCTIONS
   !
   INTRINSIC DEXP, DLOG
   !
   SAVE CORR2 , CORR3 , CORR4
   SAVE WPOR , WNOR , CMAX , PS , KPE , BETA , IK , KP , KN , ALPHA1 , ALPHA2
   SAVE RP , FP , DTP , K320 , FN , DTN , K620 , M1 , M2 , WLOR , K120 , K520
   SAVE F, N , BEN , K2 , RS , NBSEUI
   SAVE FORMK2 , FORMCS , FORMRS , IF1
   !
   ! ------------------------------------------------------------------
   ! C1 : BIOMASSE PHYTOPLANCTONIQUE PHY  <<ug/l>> (micro-gramme/litre)
   ! C2 : PHOSPHORE MINERAL ASSIMILABLE PO4  <<mg/l>>
   ! C3 : PHOSPHORE MINERAL NON ASSIMILABLE POR  <<mg/l>>
   ! C4 : AZOTE MINERAL ASSIMILABLE NO3  <<mg/l>>
   ! C5 : AZOTE MINERAL NON ASSIMILABLE NOR  <<mg/l>>
   ! C6 : CHARGE AMMONIACALE                   NH4  <<mg/l>>
   ! C7 : CHARGE ORGANIQUE                     L    <<mg/l>>
   ! C8 : OXYGENE DISSOUS                      O2   <<mg/l>>
   ! ------------------------------------------------------------------

   ! -I- INITIALISATIONS
   !
   ! 1) AFFECTATIONS DES PARAMETRES PHYSIQUES
   !
   IF( IF1.EQ.0 ) THEN
   !
      WPOR   = TPARPH%ParQual_eau(1)
      WNOR   = TPARPH%ParQual_eau(2)
      CMAX   = TPARPH%ParQual_eau(3)
      PS     = TPARPH%ParQual_eau(4)
      KPE    = TPARPH%ParQual_eau(5)
      BETA   = TPARPH%ParQual_eau(6)
      IK     = TPARPH%ParQual_eau(7)
      KP     = TPARPH%ParQual_eau(8)
      KN     = TPARPH%ParQual_eau(9)
      ALPHA1 = TPARPH%ParQual_eau(10)
      ALPHA2 = TPARPH%ParQual_eau(11)
      RP     = TPARPH%ParQual_eau(12)
      FP     = TPARPH%ParQual_eau(13)
      DTP    = TPARPH%ParQual_eau(14)
      K320   = TPARPH%ParQual_eau(15)
      FN     = TPARPH%ParQual_eau(16)
      DTN    = TPARPH%ParQual_eau(17)
      K620   = TPARPH%ParQual_eau(18)
      M1     = TPARPH%ParQual_eau(19)
      M2     = TPARPH%ParQual_eau(20)
      WLOR   = TPARPH%ParQual_eau(21)
      K120   = TPARPH%ParQual_eau(22)
      K520   = TPARPH%ParQual_eau(23)
      F      = TPARPH%ParQual_eau(24)
      N      = TPARPH%ParQual_eau(25)
      BEN    = TPARPH%ParQual_eau(26)
      K2     = TPARPH%ParQual_eau(27)
      FORMK2 = INT(TPARPH%ParQual_eau(28))
      CS     = TPARPH%ParQual_eau(29)
      FORMCS = INT(TPARPH%ParQual_eau(30))
      RS     = TPARPH%ParQual_eau(31)
      FORMRS = INT(TPARPH%ParQual_eau(32))
      ! Nombre de seuils consideres dans le module O2
      ! (peut etre inferieur au nombre de seuils utilises pour l'hydraulique)
      NBSEUI = INT(TPARPH%ParQual_eau(33))
      IF1    = 1
      CORR2 = 1.050D0
      CORR3 = 1.047D0
      CORR4 = 1.025D0
   ENDIF

   DO I = 1 , NBSEUI
      ARS(I)     = TPARPH%ParQual_eau(33+I*3-2)
      BRS(I)     = TPARPH%ParQual_eau(33+I*3-1)
      ! Numero du seuil correspondant (numerotation du calcul hydraulique)
      NUMSEUI(I) = INT(TPARPH%ParQual_eau(33+I*3))
   ENDDO
   !
   !   Interpolation temporelle des donnees meteo
   !
   CALL INTERPOLATION_S( TEMP , T , 1 , TMeteo%Temps , TMeteo%Temp , size(TMeteo%Temps) , Erreur )
   CALL INTERPOLATION_S( I0  , T , 1 , TMeteo%Temps, TMeteo%I0 , size(TMeteo%Temps) , Erreur )
   !
   ! 2) CALCULS PRELIMINAIRES
   !
   CORR1T  = TEMP / 20.D0
   CORR2T  = CORR2**(TEMP-20.D0)
   CORR3T  = CORR3**(TEMP-20.D0)
   CORR4T  = CORR4**(TEMP-20.D0)
   CORRBEN = BEN * CORR2T
   !
   !   Calcul de Cs
   !
   IF( FORMCS == 1 ) THEN
      CS = 14.652D0 - 0.41022D0 * TEMP + 0.007991D0 * TEMP**2 - 7.7774D-5 * TEMP**3
   ELSEIF (FORMCS.EQ.2) THEN
      CS = 468.D0 / ( 31.6D0 + TEMP )
   ENDIF

   DO I = 1 , Nbsect
      !
      !   Calculs des donnees relatives au phytoplancton
      !
      IF( ABS(PS).GT.EPS15 ) THEN
         KE(I) = 1.7D0 / PS
      ELSE
         ! ke    = kpe + Beta*[PHY]
         KE(I) = KPE + BETA * C(I,1)
      ENDIF

      IH(I)  = I0 * DEXP( -KE(I) * H(I) )
      RAY(I) = 1.D0 / ( KE(I) * H(I) ) * DLOG( ( I0 + DSQRT( IK**2 + I0**2 ) ) / &
                      ( IH(I) + DSQRT( IK**2 + IH(I)**2 ) ) )

      ! Lnut  = min( [PO4] /(KP+[PO4] ),([NO3]+[NH4])/(KN+[NO3]+[NH4]) )
      LNUT(I) = DMIN1( C(I,2) / ( KP + C(I,2) ) , ( C(I,4) + C(I,6) ) / ( KN + C(I,4) + C(I,6) ) )
      CP(I)   = CMAX * RAY(I) * CORR1T * LNUT(I) * ALPHA1

      ! MP= m1 + m2*[PHY]  + alpha2
      MP(I) = M1 + M2 * C(I,1) + ALPHA2
      DP(I) = ( RP + MP(I) )*CORR2T

      ! Rn  = [NH4]/([NO3]+[NH4])
      IF( C(I,4) + C(I,6).GT.0.D0) THEN
         RN(I) = C(I,6) / ( C(I,4)+C(I,6) )
      ELSE
         RN(I) = 0.D0
      ENDIF
      !
      !   Calcul de K2
      !
      !   Pente de la ligne de charge
      PJ(I) = ( Q(I) / ( ST(I) * A(I) * RH(I)**( 2.D0 / 3.D0 ) ) )**2
      U(I) = Q(I) / A(I)

      IF( FORMK2 == 0 ) THEN
         TK2(I) = K2
      ELSEIF( FORMK2.EQ.1 ) THEN
         TK2(I) = 5.23D0 * U(I) * H(I)**(-1.67D0)
      ELSEIF( FORMK2.EQ.2 ) THEN
         TK2(I) = 5.33D0 * U(I)**0.67D0 * H(I)**( -1.85D0 )
      ELSEIF( FORMK2.EQ.3 ) THEN
         TK2(I) = ( 0.746D0 * U(I)**2.695D0 ) / ( H(I)**3.085D0 * PJ(I)**0.823D0 )
      ELSEIF( FORMK2.EQ.4 ) THEN
         TK2(I) = (3.90D0 * U(I)**0.5D0 ) /  H(I)**(1.5D0)
      ELSEIF( FORMK2.EQ.5 ) THEN
         IF( H(I).LE.0.6D0 ) THEN
            TK2(I) = 5.33D0 * U(I)**0.67D0 * H(I)**(-1.85D0)
         ELSEIF( H(I).LE.(12.D0 * U(I) - 6.6D0 ) ) THEN
            TK2(I) = ( 0.746D0 * U(I)**2.695D0 ) / ( H(I)**3.085D0 * PJ(I)**0.823D0 )
         ELSE
            TK2(I) = (3.90 * U(I)**0.5) /  H(I)**(1.5)
         ENDIF
      ENDIF
   ENDDO
   !
   ! Calcul de Rs
   !
   DO I = 1 , NBSEUI
      K = Singularite(NUMSEUI(I))%Section
      KP1 = K + 1
      DZ = H(K) - H(KP1)
      IF( FORMRS.EQ.0 ) THEN
         TRS(I) = RS
      ELSEIF( FORMRS.EQ.1 ) THEN
         TRS(I) = 1.D0 + 0.5D0 * ARS(I) * BRS(I) * DZ
      ELSEIF (FORMRS.EQ.2) THEN
         TRS(I) = 0.11D0 * ARS(I) * BRS(I) * (1.D0 + 0.046D0 * TEMP ) * DZ
      ELSEIF( FORMRS.EQ.3 ) THEN
         TRS(I) = 1.D0 + 0.69D0 * DZ * ( 1.D0 - 0.11D0 * DZ ) * ( 1.D0 + 0.046D0 * TEMP )
      ELSEIF (FORMRS.EQ.4) THEN
         TRS(I) = 1.D0 + 0.38D0 * ARS(I) * BRS(I) * DZ * ( 1.D0 - 0.11D0 * DZ ) *  ( 1.D0 + 0.046D0 * TEMP )
      ENDIF
      !
      !   Forcage de la concentration en O2 a l'aval des seuils
      !
      C(KP1,8) = (C(K,8)-CS)/TRS(I) + CS
   ENDDO
   !
   !----------------------------------------------------------------------
   ! -II- CALCUL DES TERMES SOURCES
   !
   DO I = 1 , nbsect
      !
      ! TRACEUR 1 : [PHY] Biomasse phytoplanctonique
      !
      SV(I,1) = ( CP(I) - DP(I) ) * C(I,1) /86400.D0
      SS(I,1) = 0.D0
      !
      ! TRACEUR 2 : [PO4] Phosphore mineral assimilable
      !
      SV(I,2) = ( FP * ( DTP * DP(I) - CP(I) ) * C(I,1) &
               + K320 * CORR2T * C(I,3) ) / 86400.D0
      SS(I,2) = 0.D0
      !
      ! TRACEUR 3 : [POR] Phosphore mineral non assimilable
      !
      SV(I,3) = ( FP * ( 1.D0 - DTP ) * DP(I) * C(I,1) &
               - K320 * CORR2T * C(I,3) ) / 86400.D0
      SS(I,3) = - WPOR * C(I,3)
      !
      ! TRACEUR 4 : [NO3] Azote mineral assimilable
      !
      SV(I,4) = ( - FN * ( 1.D0 - RN(I) ) * CP(I) * C(I,1) &
               + K520 * CORR2T * C(I,6) ) / 86400.D0
      SS(I,4) = 0.D0
      !
      ! TRACEUR 5 : [NOR] Azote mineral non assimilable
      !
      SV(I,5) = ( FN * ( 1.D0 - DTN ) * DP(I) * C(I,1) &
               - K620 * CORR2T * C(I,5) ) / 86400.D0
      SS(I,5) = - WNOR * C(I,5)
      !
      ! TRACEUR 6 : [NH4] Charge ammoniacale
      !
      SV(I,6) = ( FN * ( DTN * DP(I) - RN(I) * CP(I) ) * C(I,1) &
               + K620 * CORR2T * C(I,5) &
               - K520 * CORR2T * C(I,6) ) / 86400.D0
      SS(I,6) = 0.D0
      !
      ! TRACEUR 7 : [L] Charge organique
      !
      SV(I,7) = ( F * MP(I) * C(I,1) - K120 * CORR3T * C(I,7) ) / 86400.D0
      SS(I,7) = - WLOR * C(I,7)
      !
      ! TRACEUR 8 : [O2] Oxygene dissous
      !
      SV(I,8) = (  F * ( CP(I) - RP * CORR1T ) * C(I,1) &
               - N * K520 * CORR2T * C(I,6) &
               - K120 * CORR3T * C(I,7) &
               + TK2(I) * CORR4T * ( CS - C(I,8) ) ) / 86400.D0
      SS(I,8) = - CORRBEN / 86400.D0
   ENDDO
   !
   !----------------------------------------------------------------------
   ! -III- ASSEMBLAGE DES TERMES SOURCES
   !       (sources volumiques, surfaciques et ajoutees par l'utilisateur)
   !
   DO K = 1 , NBTRA
      DO I=1,nbsect
         S(I,K) = SV(I,K) + SS(I,K) / H(I) + SA(I,K)
         RNU(I,K) = 0.D0
      ENDDO
   ENDDO

   RETURN
END SUBROUTINE CALCS_EUTRO
