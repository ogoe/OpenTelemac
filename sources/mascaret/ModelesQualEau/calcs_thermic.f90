!== Copyright (C) 2000-2015 EDF-CEREMA ==
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

SUBROUTINE CALCS_THERMIC( RNU , S , &
                          Nbsect , NBTRA , Nbsing , &
                          Q , A , H , RH , ST , C , &
                          SA , T , TParph , TMeteo ,  DT , Erreur )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL
!
! VERSION : 8.1.0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION : MODELE DE THERMIQUE EN RIVIERE
!  --------
!
!   CE SOUS PROGRAMME CALCULE LES TERMES SOURCES IMPLICITES
!   ET EXPLICITES, VOLUMIQUES ET SURFACIQUES,
!   UTILISEES DANS L'EQUATION DE CONSERVATION DU TRACEUR
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
! !  SV       ! TR ! D  ! TERMES SOURCES EXPLICITES                   !
! !          !
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
! !   RO      ! R  !    ! MASSE VOLUMIQUE DE L'EAU                     !
! !   CPE     ! R  !    ! CHALEUR SPECIFIQUE DE L'EAU                  !
! !   CPA     ! R  !    ! CHALEUR SPECIFIQUE DE L'AIR                  !
! !   COEF_A  ! R  !    ! COEF.A DE LA FORMULE D'AERATION A+UB         !
! !   COEF_B  ! R  !    ! COEF.B DE LA FORMULE D'AERATION A+UB         !
! !   COEF_K  ! R  !    ! COEF. REPR. DE LA COUVERTURE NUAGEUSE        !
! !   EMA     ! R  !    ! COEF. DE CALAGE DU RAYONNT ATMOSPH.          !
! !   EME     ! R  !    ! COEF. DE CALAGE DU RAYONNT DU PLAN D'EAU     !
! !           !    !    !                                              !
! !  IF1      ! TR ! D  ! INDIC DE LECTURE DU FICHIER DES PARAMETRES   !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!***********************************************************************

   USE M_PRECISION
   USE M_CONSTANTES_TRACER_T
   USE M_PARAMETRES_QUALITE_EAU_T
   USE M_METEO_T
   USE M_ERREUR_T
   USE M_INTERPOLATION_S

   IMPLICIT NONE

   REAL(DOUBLE) , DIMENSION(:,:) , intent(inout) :: RNU , S , SA
   REAL(DOUBLE) , DIMENSION(:)   , intent(in   ) :: Q , A , H , ST , RH
   REAL(DOUBLE) , DIMENSION(:,:) , intent(inout) :: C
   INTEGER                                       :: Nbsect , NBTRA , nbsing
   REAL(DOUBLE)                                  :: T , DT
   !
   !  DECLARATION DES PARAMETRES PHYSIQUES
   !
   TYPE (PARAMETRES_QUALITE_EAU_T) :: TPARPH
   TYPE (METEO_T)                  :: TMeteo
   TYPE (ERREUR_T)                 :: Erreur
   !
   ! VARIABLES LOCALES
   !
   REAL(DOUBLE) , DIMENSION (Nbsect)       :: CE , CV , RE , RA
   REAL(DOUBLE) , DIMENSION (Nbsect)       :: P_VAP_SAT , HA_SAT , L_VAP
   REAL(DOUBLE) , DIMENSION (Nbsect,nbtra) :: SS , SV
   REAL(DOUBLE) :: RO , CPE , CPA , COEF_A , COEF_B , COEF_K , EMA , EME
   REAL(DOUBLE) :: RS , ROA , HA , SIGMA
   REAL(DOUBLE) :: T_AIR , P_VAP , VIT_VENT , NEBULO , RAY3 , P_ATM
   INTEGER I
   INTEGER :: IF1 = 0
   !
   ! FONCTIONS
   !
   INTRINSIC DEXP
   !
   SAVE SIGMA , RO , CPE , CPA , COEF_A , COEF_B , COEF_K , EMA , EME , IF1
   !
   ! ----------------------------------------------------------------
   ! C1 : TEMPERATURE      TEMP <<°C>>
   ! ----------------------------------------------------------------
   ! -I- INITIALISATIONS
   !
   ! 1) AFFECTATIONS DES PARAMETRES PHYSIQUES
   !
   IF( IF1.EQ.0 ) THEN
      RO     = TPARPH%ParQual_eau(1)
      CPE    = TPARPH%ParQual_eau(2)
      CPA    = TPARPH%ParQual_eau(3)
      COEF_A = TPARPH%ParQual_eau(4)
      COEF_B = TPARPH%ParQual_eau(5)
      COEF_K = TPARPH%ParQual_eau(6)
      EMA    = TPARPH%ParQual_eau(7)
      EME    = TPARPH%ParQual_eau(8)
      IF1    = 1
   ENDIF
   !
   ! Interpolation temporelle des donnees meteo
   !
   CALL INTERPOLATION_S( T_AIR , T , 1 , TMeteo%Temps , TMeteo%T_air , size(TMeteo%Temps) , Erreur )
   CALL INTERPOLATION_S( P_VAP , T , 1 , TMeteo%Temps , TMeteo%P_Vap , size(TMeteo%Temps) , Erreur )
   CALL INTERPOLATION_S( VIT_VENT , T , 1 , TMeteo%Temps , TMeteo%Vit_vent , size(TMeteo%Temps) , Erreur )
   CALL INTERPOLATION_S( NEBULO , T , 1 , TMeteo%Temps , TMeteo%Nebulo , size(TMeteo%Temps) , Erreur )
   CALL INTERPOLATION_S( RAY3 , T , 1 , TMeteo%Temps , TMeteo%Ray3 , size(TMeteo%Temps) , Erreur )
   CALL INTERPOLATION_S( P_ATM , T , 1 , TMeteo%Temps , TMeteo%P_atm , size(TMeteo%Temps) , Erreur )
   IF( Erreur%Numero/=0 ) THEN
      return
   endif
   !
   ! 2) CALCULS PRELIMINAIRES
   !
   SIGMA = 5.67D-8
   !   Rayonnement solaire
   RS = RAY3
   !   Masse volumique de l'air
   ROA = 100.D0  * P_ATM / ( ( T_AIR + 273.15D0 ) * 287.D0 )
   !   Humidite specifique de l'air
   HA  = 0.622D0 * P_VAP / ( P_ATM  - 0.378D0 * P_VAP  )
   !
   DO I = 1 , nbsect
      !     Rayonnement emis par le plan d'eau
      RE(I) = EME * SIGMA * ( C(I,1) + 273.15D0 )**4
      !     Flux de chaleur de convection
      CV(I) = ROA * CPA * ( COEF_A + COEF_B * VIT_VENT ) * ( C(I,1) - T_AIR )
      !     Chaleur latente de vaporisation
      L_VAP(I) = 2500900.D0 - 2365.D0 * C(I,1)
      !     Pression partielle de vapeur d'eau a saturation
      P_VAP_SAT(I) = 6.11D0 * DEXP ( 17.27D0 * C(I,1) / ( C(I,1) + 237.3D0 ) )
      !     Humidite specifique de l'air a saturation
      HA_SAT(I) = 0.622D0 * P_VAP_SAT(I) / ( P_ATM - 0.378D0 * P_VAP_SAT(I) )
      !     Flux de chaleur d'evaporation
      CE(I) = L_VAP(I) * ROA * ( COEF_A + COEF_B * VIT_VENT ) * ( HA_SAT(I) - HA )
      !     Rayonnement atmospherique
      RA(I) = EMA * SIGMA * ( T_AIR + 273.15D0 )**4 * ( 1.D0 + COEF_K * ( NEBULO / 8.D0 )**2 )
      IF( HA_SAT(I).LT.HA ) THEN
         RA(I) = 1.8D0 * RA(I)
      ENDIF
   ENDDO
   !
   !----------------------------------------------------------------------
   ! -II- CALCUL ET ASSEMBLAGE DES TERMES SOURCES
   !      (sources volumiques, surfaciques et ajoutees par l'utilisateur)
   !
   DO I=1,nbsect
      SV(I,1) = 0.D0
      SS(I,1) = ( RS + RA(I) - RE(I) - CV(I) - CE(I) ) / ( RO * CPE )
      S(I,1)  = SV(I,1) + SS(I,1) / H(I) + SA(I,1)
      RNU(I,1) = 0.D0
   ENDDO

   RETURN

END SUBROUTINE CALCS_THERMIC
