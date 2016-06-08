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

SUBROUTINE CALCS_O2( RNU , S , &
                     Nbsect , NBTRA  , Nbsing , Singularite , &
                     Q , A , H , RH , ST , C , &
                     SA , T , TParph , DT )

!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - N.GOUTAL
!
! VERSION : 8.1.0              EDF-CEREMA
!***********************************************************************
!
!  FONCTION : MODELE DE QUALITE D'EAU OXYGENE DISSOUS
!  --------
!
! CE SOUS PROGRAMME CALCULE LES TERMES SOURCES IMPLICITES
! ET EXPLICITES, VOLUMIQUES ET SURFACIQUES,
! UTILISEES DANS L'EQUATION DE CONSERVATION DU TRACEUR
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
! !   U       ! TR ! D  ! VITESSE DE L'EAU                             !
! !   J       ! TR ! D  ! PENTE DE LA LIGNE DE CHARGE                  !
! !   K1      ! R  !    ! CONST DE CINET. DE DEGRAD. DE LA CHARGE ORG. !
! !   K4      ! R  !    ! CONST DE CINET. DE NITRIFICATION             !
! !   BEN     ! R  !    ! DEMANDE BENTHIQUE                            !
! !   RESP    ! R  !    ! RESPIRATION VEGETALE                         !
! !   PHOTO   ! R  !    ! PHOTOSYNTHESE                                !
! !   K2      ! R  !    ! COEFFICIENT DE REAERATION                    !
! !   FORMK2  ! E  !    ! FORMULE DE CALCUL DE K2                      !
! !   CS      ! R  !    ! CONC DE SATURATION EN OXYGENE DE L'EAU       !
! !   FORMCS  ! E  !    ! FORMULE DE CALCUL DE CS                      !
! !   CORECT  ! R  !    ! COEF DE CORRECTION DES FORMULES AVEC TEMP    !
! !   TEMP    ! R  !    ! TEMPERATURE                                  !
! !   RS      ! R  !    ! COEFFICIENT DE REAERATION AUX SEUILS         !
! !   FORMRS  ! E  !    ! FORMULE DE CALCUL DE RS                      !
! !   ARS     ! R  !    ! COEFFICIENT A DES FORMULES DE CALCUL DE R    !
! !   BRS     ! R  !    ! COEFFICIENT B DES FORMULES DE CALCUL DE R    !
! !   NBSEUI  ! E  !    ! NOMBRE DE SEUILS                             !
! !   XSEUI   ! TR !    ! ABSCISSES DES SEUILS                         !
! !   DZS     ! TR !    ! DELTA Z AUX SEUILS                           !
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
   USE M_SINGULARITE_T

   IMPLICIT NONE

   REAL(DOUBLE)       , DIMENSION(:,:)  ,intent(inout) :: RNU , S , SA
   REAL(DOUBLE)       , DIMENSION(:)    ,intent(in   ) :: Q , A , H , ST , RH
   REAL(DOUBLE)       , DIMENSION(:,:)  ,intent(inout) :: C
   TYPE(SINGULARITE_T), DIMENSION(:)    ,intent(in   ) :: Singularite
   INTEGER         :: Nbsect , NBTRA , nbsing
   REAL(DOUBLE)    :: T , DT
   !
   !  DECLARATION DES PARAMETRES PHYSIQUES
   !
   type (PARAMETRES_QUALITE_EAU_T ) ::  TPARPH
   !
   !  VARIABLES LOCALES
   !
   REAL(DOUBLE), DIMENSION (Nbsect)          :: U , PJ
   REAL(DOUBLE)                              :: TEMP, K1 , K2 , K4, BEN, RESP, PHOTO
   REAL(DOUBLE)                              :: CS  , RS , DZ , BENCORR
   REAL(DOUBLE), DIMENSION (Nbsing)          :: TRS , ARS, BRS
   REAL(DOUBLE), DIMENSION (Nbsect)          :: TK2
   REAL(DOUBLE), DIMENSION (Nbsect,nbtra)    :: SS  , SV
   REAL(DOUBLE)                              :: CORR1    , CORR2 , CORR1T, CORR2T
   INTEGER     , DIMENSION (Nbsing)          :: NUMSEUI
   INTEGER FORMK2 , FORMCS , FORMRS , NBSEUI
   INTEGER I , J , K , KP1
   INTEGER :: IF1 = 0
   !
   SAVE TEMP
   SAVE K1 , K2 , K4 , BEN , RESP , PHOTO , CS
   SAVE FORMK2, FORMCS , FORMRS
   SAVE CORR1 , CORR2 , NBSEUI , IF1
   !
   !----------------------------------------------------------------
   ! C1 : OXYGENE DISSOUS	  O2  << mgO2/l >>
   ! C2 : CHARGE ORGANIQUE   L   << mgO2/l >>
   ! C3 : CHARGE AMMONIACALE NH4 << mgO2/l >>
   !----------------------------------------------------------------
   !
   ! -I- INITIALISATIONS
   !
   ! 1) AFFECTATIONS DES PARAMETRES PHYSIQUES
   !
   IF( IF1.EQ.0 ) THEN
      K1     = TPARPH%ParQual_eau(1)
      K4     = TPARPH%ParQual_eau(2)
      BEN    = TPARPH%ParQual_eau(3)
      PHOTO  = TPARPH%ParQual_eau(4)
      RESP   = TPARPH%ParQual_eau(5)
      K2     = TPARPH%ParQual_eau(6)
      FORMK2 = INT(TPARPH%ParQual_eau(7))
      CS     = TPARPH%ParQual_eau(8)
      FORMCS = INT(TPARPH%ParQual_eau(9))
      TEMP   = TPARPH%ParQual_eau(10)
      RS     = TPARPH%ParQual_eau(11)
      FORMRS = INT(TPARPH%ParQual_eau(12))
      ! Nombre de seuils consideres dans le module O2
      ! (peut etre inferieur au nombre de seuils utilises pour l'hydraulique)
      NBSEUI = INT(TPARPH%ParQual_eau(13))
      CORR1 = 1.065D0
      CORR2 = 1.0241D0
      IF1   = 1
   ENDIF

   DO I = 1 , NBSEUI
      ARS(I)    = TPARPH%ParQual_eau(13+I*3-2)
      BRS(I)    = TPARPH%ParQual_eau(13+I*3-1)
      NUMSEUI(I)= INT(TPARPH%ParQual_eau(13+I*3))
   ENDDO
   !
   ! 2) CALCULS PRELIMINAIRES
   !
   CORR1T = CORR1**(TEMP-20.D0)
   CORR2T = CORR2**(TEMP-20.D0)
   !
   BENCORR = BEN * CORR1T
   !
   !   Calcul de CS
   !
   IF( FORMCS == 1 ) THEN
      CS = 14.652D0 - 0.41022D0 * TEMP + 0.007991D0 * TEMP**2 -  7.7774D-5 * TEMP**3
   ELSEIF( FORMCS.EQ.2 ) THEN
      CS = 468.D0 / ( 31.6D0 + TEMP )
   ENDIF
   !
   !   Calcul de K2
   !
   DO I = 1 , nbsect
      !
      !   Pente de la ligne de charge
      PJ(I) = ( Q(I) / ( ST(I) * A(I) * RH(I)**(2.D0/3.D0) ) )**2
      U(I)  = Q(I) / A(I)
      IF( FORMK2 == 0 ) THEN
         TK2(I) = K2
      ELSEIF( FORMK2.EQ.1 ) THEN
         TK2(I) = 5.23D0 * U(I) * H(I)**(-1.67D0)
      ELSEIF( FORMK2.EQ.2 ) THEN
         TK2(I) = 5.33D0 * U(I)**0.67D0 * H(I)**(-1.85D0)
      ELSEIF( FORMK2.EQ.3 ) THEN
         TK2(I) = ( 0.746D0 * U(I)**2.695D0 ) / ( H(I)**3.085D0 * PJ(I)**0.823D0 )
      ELSEIF( FORMK2.EQ.4 ) THEN
         TK2(I) = ( 3.90D0 * U(I)**0.5D0 ) /  H(I)**(1.5D0)
      ELSEIF( FORMK2.EQ.5 ) THEN
         IF( H(I).LE.0.6D0 ) THEN
            TK2(I) = 5.33D0 * U(I)**0.67D0 * H(I)**(-1.85D0)
         ELSEIF (H(I).LE.(12.*U(I)-6.6)) THEN
            TK2(I) = ( 0.746D0 * U(I)**2.695D0 ) / ( H(I)**3.085D0 * PJ(I)**0.823D0 )
         ELSE
            TK2(I) = (3.90D0 * U(I)**0.5D0 ) /  H(I)**(1.5D0)
         ENDIF
      ENDIF
   ENDDO
   !
   !   Calcul de RS
   !
   DO I = 1 , NBSEUI
      K   = Singularite(NUMSEUI(I))%Section
      KP1 = K + 1
      DZ  = H(K) - H(KP1)
      IF( FORMRS.EQ.0 ) THEN
         TRS(I) = RS
      ELSEIF( FORMRS.EQ.1 ) THEN
         TRS(I) = 1.D0 + 0.5D0 * ARS(I) * BRS(I) * DZ
      ELSEIF( FORMRS.EQ.2 ) THEN
         TRS(I) = 0.11D0 * ARS(I) * BRS(I) * ( 1.D0 + 0.046D0 * TEMP ) * DZ
      ELSEIF( FORMRS.EQ.3 ) THEN
         TRS(I) = 1.D0 + 0.69D0 * DZ * (1.D0 - 0.11D0 * DZ ) * ( 1.D0 + 0.046D0 * TEMP )
      ELSEIF (FORMRS.EQ.4) THEN
         TRS(I) = 1.D0 + 0.38D0 * ARS(I) * BRS(I) * DZ * ( 1.D0 - 0.11D0 * DZ ) * ( 1.D0 + 0.046D0 * TEMP )
      ENDIF
      !
      !   Forcage de la concentration en O2 a l'aval des seuils
      !
      C(KP1,1) = ( C(K,1) - CS ) / TRS(I) + CS
   ENDDO
   !
   !----------------------------------------------------------------------
   ! -II- CALCUL DES TERMES SOURCES
   !
   DO I = 1 , Nbsect
      !
      ! TRACEUR 1 : [O2] Oxygene dissous
      !
      SV(I,1) = ( TK2(I) * CORR2T * ( CS - C(I,1) ) - K1 * C(I,2) - K4 * C(I,3)        &
               + PHOTO - RESP )  / 86400.D0
      SS(I,1) = - BENCORR / 86400.D0
      !
      ! TRACEUR 2 : [L] Charge organique
      !
      SV(I,2) = - K1 * C(I,2) / 86400.D0
      SS(I,2) = 0.D0
      !
      ! TRACEUR 3 : [NH4] Charge ammoniacale
      !
      SV(I,3) = - K4 * C(I,3) / 86400.D0
      SS(I,3) = 0.D0
   ENDDO
   !
   !----------------------------------------------------------------------
   ! -III- ASSEMBLAGE DES TERMES SOURCES
   !       (sources volumiques, surfaciques et ajoutees par l'utilisateur)
   !
   DO K = 1 , NBTRA
      DO I = 1 , nbsect
         S(I,K)   = SV(I,K) + SS(I,K) / H(I) + SA(I,K)
         RNU(I,K) = 0.D0
      ENDDO
   ENDDO

   RETURN
END SUBROUTINE CALCS_O2
