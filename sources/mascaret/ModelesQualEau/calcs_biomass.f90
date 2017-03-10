!== Copyright (C) 2000-2017 EDF-CEREMA ==
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

SUBROUTINE CALCS_BIOMASS( RNU , S , &
                          Nbsect , NBTRA , Nbsing , &
                          Q , A , H , RH , ST , C , &
                          SA , T , TParph , TMeteo , &
                          DT , Erreur )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL
!
! VERSION : 8.1.3              EDF-CEREMA
!***********************************************************************
!
!  FONCTION : MODELE DE QUALITE D'EAU BIOMASS
!  --------
!
!       CE SOUS PROGRAMME CALCULE LES TERMES SOURCES IMPLICITES
!       ET EXPLICITES, VOLUMIQUES ET SURFACIQUES,
!       UTILISEES DANS L'EQUATION DE CONSERVATION DU TRACEUR
!
!       POUR UN PROBLEME DE BIOMASSE PHYTOPLANCTONIQUE
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
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20° C  !
! !  PS       ! R  !    ! PROFONDEUR DE SECCHI                         !
! !  KPE      ! R  !    ! COEF D EXTINCTION DU RAY SANS PHYTO          !
! !  BETA     ! R  !    ! COEF DE TURBIDITE VEGETALE                   !
! !  IK       ! R  !    ! PARAMETRE DE CALAGE DE LA FORMULE DE SMITH   !
! !  KP       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN PHOSPHATE    !
! !  KN       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN AZOTE        !
! !  ALPHA    ! R  !    ! COEF 1 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  ALPHA2   ! R  !    ! COEF 2 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  RP       ! R  !    ! TAUX DE RESP. DE LA BIOMASSE ALGALE A 20° C  !
! !  FP       ! R  !    ! PROP DE PHOSPHORE DANS LES CELLULES DU PHYTO !
! !  DTP      ! R  !    ! POURCENT DE PHOSPH DIRECT ASSIM DS PHY MORT  !
! !  K1       ! R  !    ! TAUX DE TRANSFORMATION DU POR EN PO4         !
! !  FN       ! R  !    ! PROP D AZOTE DANS LES CELLULES DU PHYTO      !
! !  DTN      ! R  !    ! POURCENT D AZOTE DIRECT ASSIM DS PHY MORT    !
! !  K2       ! R  !    ! TAUX DE TRANSFORMATION DU NOR EN NO3         !
! !  M1       ! R  !    ! COEF 1 DE MORTALITE ALGALE A 20° C           !
! !  M2       ! R  !    ! COEF 2 DE MORTALITE ALGALE A 20° C           !
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

   IMPLICIT NONE

   REAL(DOUBLE), DIMENSION(:,:) ,intent(inout) :: RNU , S , SA
   REAL(DOUBLE), DIMENSION(:)   ,intent(in   ) :: Q , A , H , ST , RH
   REAL(DOUBLE), DIMENSION(:,:) ,intent(inout) :: C
   INTEGER                                     :: Nbsect , NBTRA , nbsing
   REAL(DOUBLE)                                :: T , DT
   !
   !  DECLARATION DES PARAMETRES PHYSIQUES
   !
   TYPE (PARAMETRES_QUALITE_EAU_T )         ::  TPARPH
   TYPE (METEO_T)                           ::  TMeteo
   TYPE (ERREUR_T)                          ::  Erreur
   !
   !  VARIABLES LOCALES
   !
   REAL(DOUBLE), DIMENSION (Nbsing)        :: TRS , ARS, BRS
   REAL(DOUBLE), DIMENSION (Nbsect)        :: TK2 , RAY
   REAL(DOUBLE), DIMENSION (Nbsect)        :: LNUT , IH , CP , DP , MP , KE
   REAL(DOUBLE), DIMENSION (Nbsect,nbtra)  :: SS , SV
   REAL(DOUBLE) :: WPOR , WNOR , CMAX , PS , KPE , BETA , RP
   REAL(DOUBLE) :: IK , KP , KN , ALPHA1 , ALPHA2 , RESP , FP , DTP , K1
   REAL(DOUBLE) :: FN , DTN , K2 , M1 , M2 , CORRT , TEMP , I0
   INTEGER I , J , K
   INTEGER :: IF1 = 0
   !
   !  FONCTIONS
   !
   INTRINSIC DEXP, DLOG
   !
   SAVE WPOR , WNOR , CMAX , PS , KPE , BETA , IK , KP , KN , ALPHA1 , ALPHA2
   SAVE RESP , FP , DTP , K1 , FN , DTN , K2 , M1 , M2 , IF1
   !
   ! --------------------------------------------------------------------
   ! C1 : BIOMASSE PHYTOPLANCTONIQUE PHY  << ug/l >> (micro-gramme/litre)
   ! C2 : PHOSPHORE MINERAL ASSIMILABLE PO4  << mg/l >>
   ! C3 : PHOSPHORE MINERAL NON ASSIMILABLE POR  << mg/l >>
   ! C4 : AZOTE MINERAL ASSIMILABLE NO3  << mg/l >>
   ! C5 : AZOTE MINERAL NON ASSIMILABLE NOR  << mg/l >>
   ! --------------------------------------------------------------------

   !
   ! -I- INITIALISATIONS
   !

   ! 1) AFFECTATIONS DES PARAMETRES PHYSIQUES 
   !
   IF(IF1.EQ.0) THEN
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
      K1     = TPARPH%ParQual_eau(15)
      FN     = TPARPH%ParQual_eau(16)
      DTN    = TPARPH%ParQual_eau(17)
      K2     = TPARPH%ParQual_eau(18)
      M1     = TPARPH%ParQual_eau(19)
      M2     = TPARPH%ParQual_eau(20)
      IF1    = 1
   ENDIF

   !
   !   Interpolation temporelle des donnees meteo
   !
   CALL INTERPOLATION_S( TEMP , T , 1 , TMeteo%Temps , TMeteo%Temp , size(TMeteo%Temps) , Erreur )
   CALL INTERPOLATION_S( I0 , T , 1 , TMeteo%Temps , TMeteo%I0 , size(TMeteo%Temps) , Erreur )

   !
   ! 2) CALCULS PRELIMINAIRES
   !
   CORRT = TEMP / 20.D0

   DO I = 1 , Nbsect
      IF( ABS(PS).GT.EPS15 ) THEN
         KE(I) = 1.7D0 / PS
      ELSE
         ! ke    = kpe + Beta*[PHY]
         KE(I) = KPE + BETA * C(I,1)
      ENDIF

      IH(I) = I0 * DEXP( -KE(I) * H(I) )
      RAY(I)= 1.D0 / ( KE(I) * H(I) ) * DLOG( ( I0 + DSQRT( IK**2 + I0**2 ) ) / &
                     ( IH(I) + DSQRT( IK**2 + IH(I)**2 ) ) )

      ! Lnut = min( [PO4] /(KP+[PO4] ) , [NO3] /(KN+[NO3] ) )
      LNUT(I) = DMIN1( C(I,2) / ( KP + C(I,2) ) , C(I,4) / ( KN + C(I,4) ) )

      ! MP= m1 + m2*[PHY]  + alpha2
      CP(I) = CMAX * RAY(I) * CORRT * LNUT(I) * ALPHA1
      MP(I) = M1 + M2 * C(I,1) + ALPHA2
      DP(I) = ( RP + MP(I) ) * CORRT
   ENDDO

   !
   !----------------------------------------------------------------------
   ! -II- CALCUL DES TERMES SOURCES
   !
   DO I = 1 , Nbsect
      !
      ! TRACEUR 1 : [PHY] Biomasse phytoplanctonique
      !
      SV(I,1) = ( CP(I) - DP(I) ) * C(I,1) / 86400.D0
      SS(I,1) = 0.D0
      !
      ! TRACEUR 2 : [PO4] Phosphore mineral assimilable
      !
      SV(I,2) = ( FP * ( DTP * DP(I) - CP(I) ) * C(I,1) &
               + K1 * CORRT * C(I,3) ) / 86400.D0
      SS(I,2) = 0.D0
      !
      ! TRACEUR 3 : [POR] Phosphore mineral non assimilable
      !
      SV(I,3) = ( FP * ( 1.D0 - DTP ) * DP(I) * C(I,1) &
               - K1 * CORRT * C(I,3) ) / 86400.D0
      SS(I,3) = - WPOR * C(I,3)
      !
      ! TRACEUR 4 : [NO3] Azote mineral assimilable
      !
      SV(I,4) = ( FN * ( DTN * DP(I) - CP(I) ) * C(I,1) &
               + K2 * CORRT * C(I,5) ) / 86400.D0
      SS(I,4) = 0.D0
      !
      ! TRACEUR 5 : [NOR] Azote mineral non assimilable
      !
      SV(I,5) = ( FN * (1.D0 - DTN ) * DP(I) * C(I,1) &
               - K2 * CORRT * C(I,5) ) / 86400.D0
      SS(I,5) = - WNOR * C(I,5)
   ENDDO

   !
   !----------------------------------------------------------------------
   ! -III- ASSEMBLAGE DES TERMES SOURCES
   !       (sources volumiques, surfaciques et ajoutees par l'utilisateur)
   !
   DO K = 1 , NBTRA
      DO I = 1 , nbsect
         S(I,K) = SV(I,K) + SS(I,K) / H(I) + SA(I,K)
         RNU(I,K) = 0.D0
      ENDDO
   ENDDO

   RETURN
END SUBROUTINE CALCS_BIOMASS
