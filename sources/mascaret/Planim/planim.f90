!== Copyright (C) 2000-2016 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

subroutine  PLANIM( &
                    ! RESULTATS
           ProfilPlan , & ! Profils planimetres
                    ! DONNEES
               Profil , & ! Caracteristiques des profils
                   F1 , & ! Fonction Impulsion
                    ! Parametres
      DebProgressifLM , & ! Debordement progressif lit majeur
      DebProgressifZS , & ! Debordement progressif zones de stockaage
      ImpressionPlani , & ! Impression du planimetrage
         UniteListing , & ! Unite logique listing
  FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
                          ! Code de retour
               Erreur  & ! Erreur
                      )

! *********************************************************************
! PROGICIEL : MASCARET        A. LEBOSSE
!                             P. CHERUBINI
!                             S. PERON
!                             S. MANDELKERN
!
! VERSION : 8.1.1                EDF-CEREMA
! *********************************************************************
!  FONCTION :
!  --------
!
!                    PLANIMETRAGE DES PROFILS
!
!-----------------------------------------------------------------------
!
!                         VARIABLES LOCALES
! _____________________________________________________________________
! ! DB1,DB2  ! R  !<-- ! LARGEUR AU MIROIR  ) PLANIMETRAGE
! ! DP1,DP2  ! R  !<-- ! PERIMETRE MOUILLE  )  1= MINEUR   2= MAJEUR
! ! DS1,DS2  ! R  !<-- ! SECTION MOUILEE    )
! ! DS2G     ! R  !<-- ! SECTION MOUILEE LIT MAJEUR GAUCHE
! ! DBS      ! R  !<-- ! LARGEUR AU MIROIR  ) PLANIMETRAGE
! ! DSS      ! R  !<-- ! SECTION MOUILEE    ) ZONE DE STOCKAGE
! ! DZREF    ! R  ! -->! COTE LA PLUS BASSE DU PROFIL
! ! DXP,DYP  ! R  ! -->! COORDONNEES DES POINTS DEFINISSANT LE PROFIL
! !__________!____!____!______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!
!   FICHIERS  ENTREE/SORTIE :	UniteListing  : IMPRESSION DES RESULTATS GLOBAUX
!   -------------------------
!
!   SOUS PROGRAMME APPELANT :    SUPERVISEUR
!   -------------------------
!   SOUS-PROGRAMMES APPELES :
!   -------------------------
!       - PAPY  : PLANIMETRAGE DES PROFILS ENTRES PAR POINTS
!       - PLADEB: CALCUL DES SECTIONS DES LITS MAJEURS GAUCHE ET DROIT
!                 OU DES ZONES DE STOCKAGE GAUCHE ET DROITE
!                 SOUS LA COTE DE DEBORDEMENT DES PROFILS ENTRES PAR PTS
!       - PLANMA: PLANIMETRAGE AUX SECTIONS POUR MASCARET
!***********************************************************************

   !============================= Declarations ===========================
   use M_PRECISION           ! Type DOUBLE
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_MESSAGE_C           ! Liste des messages d'erreur
   use M_CONSTANTES_CALCUL_C ! NOYAU_MASCARET
   use M_PROFIL_T         ! Definition du type PROFIL_T
   use M_PROFIL_PLAN_T    ! Definition du type PROFIL_PLAN_T
   use M_ERREUR_T         ! Definition du type ERREUR_T
   use M_TRAITER_ERREUR_I ! Traitement des erreurs
   use M_PAPY_I           ! Interface de sous-programme
   use M_PLADEB_I         ! Interface de sous-programme
   use M_PLANMA_I         ! Interface de sous-programme

   !.. Implicit Declarations ..
   implicit none

   !.. Formal Arguments ..
   type (PROFIL_PLAN_T)               , intent(  out) :: ProfilPlan
   type (PROFIL_T)     , dimension(:) , intent(inout) :: Profil
   logical                            , intent(in   ) :: DebProgressifLM
   logical                            , intent(in   ) :: DebProgressifZS
   logical                            , intent(in   ) :: FrottParoiVerticale
   logical                            , intent(in   ) :: ImpressionPlani
   integer                            , intent(in   ) :: UniteListing
   real(DOUBLE), dimension (:,:)      , pointer       ::  F1 ! Fonction impulsion
   type(ERREUR_T)                     , intent(inout) :: Erreur
   !.. Tableaux locaux ..
   real(DOUBLE)       , dimension(:) , allocatable    :: DXP,DYP ! Points geometriques
                                                                 ! du profil
   real(DOUBLE), dimension(Profil(1)%NbPas) :: DB1, DB2, DBS
   real(DOUBLE), dimension(Profil(1)%NbPas) :: DP1, DP2
   real(DOUBLE), dimension(Profil(1)%NbPas) :: DS1, DS2, DSS
   real(DOUBLE), dimension(Profil(1)%NbPas) :: DS2G
   real(DOUBLE), dimension(2) :: S2_sous_cote_debord ! Section majeur      sous la cote de debordement
   real(DOUBLE), dimension(2) :: SS_sous_cote_debord ! Section de stockage sous la cote de debordement
   integer     , dimension(2) :: tableau_temp
   integer     , dimension(2) :: lim_min   ! Points linites du lit mineur
   integer     , dimension(2) :: lim_maj   ! Points linites du lit majeur
   real(DOUBLE), dimension(2) :: zref_LM   ! Cote de reference
                                           ! lit majeur gauche et droit
   real(DOUBLE), dimension(2) :: zref_ZS   ! Cote de reference
                                           ! zone de stockage gauche et droite
   !.. Scalairs locaux ..
   character(30) :: nom_prof   ! Nom du profil
   integer       :: nb_point   ! Nombre de points par profil
   integer       :: nb_pas     ! Nombre de pas de planimetrage
   integer       :: ipoint     ! Compteur sur les points
   integer       :: ipas       ! Compteur sur les pas
   integer       :: iprof      ! Compteur sur les profils
   integer       :: irive      ! Compteur sur les rives (gauche et droite)
   real(DOUBLE) :: abs_abs ! Abscisse absolue des profils
   real(DOUBLE) :: abs_rel ! Abscisse relative des profils
   real(DOUBLE) :: DZREF
   real(DOUBLE) :: pas    ! Pas de planimetrage du profil I
   integer      :: retour ! Code de retour d'erreur des fonctions intrinseques
   integer      :: J
   ! character(132) :: arbredappel_old
   real(DOUBLE) :: cote

   !.. External Calls ..
   !.. Intrinsic Functions ..
   intrinsic DABS, DMIN1

   !============================ Instructions ==============================

   ! INITIALISATIONS
   ! ---------------
   Erreur%Numero = 0
   retour = 0
   ! arbredappel_old    = trim(Erreur%arbredappel)
   ! Erreur%arbredappel = trim(Erreur%arbredappel)//'=>PLANIM'
   if (ImpressionPlani) then
      write(UniteListing,9000)
   endif

   ! ALLOCATION DES TABLEAUX DE PLANIMETRAGE
   ! DE LA STRUCTURE PROFILPLAN A NB_PAS
   !----------------------------------------
   nb_pas = Profil(1)%NbPas

   if(.not.associated(ProfilPlan%B1)) allocate( ProfilPlan%B1(size( Profil ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR (Erreur, 'ProfilPlan%B1')
      return
   end if

   if(.not.associated(ProfilPlan%B2)) allocate( ProfilPlan%B2(size( Profil ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfilPlan%B2' )
      return
   end if

   if(.not.associated(ProfilPlan%BS)) allocate( ProfilPlan%BS(size( Profil ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfilPlan%BS' )
      return
   end if

   if(.not.associated(ProfilPlan%P1)) allocate( ProfilPlan%P1(size( Profil ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfilPlan%P1' )
      return
   end if

   if(.not.associated(ProfilPlan%P2)) allocate( ProfilPlan%P2(size( Profil ),nb_pas) , STAT = retour )
   if (retour /= 0) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfilPlan%P2' )
      return
   end if

   if(.not.associated(ProfilPlan%S1)) allocate( ProfilPlan%S1(size( Profil ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfilPlan%S1' )
      return
   end if

   if(.not.associated(ProfilPlan%S2)) allocate( ProfilPlan%S2(size( Profil ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfilPlan%S2' )
      return
   end if

   if(.not.associated(ProfilPlan%SS)) allocate(ProfilPlan%SS(size( Profil ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfilPlan%SS' )
      return
   end if

   if(.not.associated(ProfilPlan%S2G)) allocate(ProfilPlan%S2G(size( Profil ),nb_pas) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ProfilPlan%S2G' )
      return
   end if

   if(.not.associated(F1)) allocate(F1(size( Profil ) , nb_pas ) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'F1' )
      return
   end if

   ! PLANIMETRAGE
   ! ------------
   Boucle_profil : do iprof = 1 , size( Profil )
      nb_point = size( Profil(iprof)%X )
      pas      = Profil(iprof)%Pas
      nb_pas   = Profil(iprof)%NbPas
      nom_prof = Profil(iprof)%Nom
      abs_abs  = Profil(iprof)%AbsAbs
      abs_rel  = Profil(iprof)%AbsRel

      !----------------------------------------------------------
      ! Allocation des tableaux temporaires DXP et DYP a nb_point
      !----------------------------------------------------------
      allocate( DXP(nb_point) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft   = err_5
         Erreur%ft_c = err_5c
         call TRAITER_ERREUR( Erreur , 'DXP' )
         return
      end if

      allocate( DYP(nb_point) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft   = err_5
         Erreur%ft_c = err_5c
         call TRAITER_ERREUR( Erreur , 'DYP' )
         return
      end if

      !--------------------------------
      ! Affectation tableaux DXP et DYP
      !--------------------------------
      DXP(:) = Profil(iprof)%X(:)
      DYP(:) = Profil(iprof)%Y(:)

      !----------------------------------------
      ! Affectation tableaux lim_min et lim_maj
      !----------------------------------------
      lim_min(:) = Profil(iprof)%LimiteMin(:)
      lim_maj(:) = Profil(iprof)%LimiteMaj(:)

      ! CAS DU LIT MAJEUR
      !------------------
      if( lim_min(1) == 0 .or. lim_min(2) == 0 ) then
         Erreur%Numero = 202
         Erreur%ft     = err_202
         Erreur%ft_c   = err_202c
         call TRAITER_ERREUR( Erreur , 'MINEUR' , iprof )
         return
      end if

      if( DebProgressifLM ) then
         call PLADEB           ( &
         S2_sous_cote_debord   , &
         lim_min               , &
         lim_maj               , &
         iprof                 , &
         DXP                   , &
         DYP                   , &
         FrottParoiVerticale   , &
         ImpressionPlani       , &
         UniteListing          , &
         Erreur                  &
                          )

         if( Erreur%Numero /= 0 ) then
            return
         end if
      end if

      ! CAS DES ZONES DE STOCKAGE
      !--------------------------
      if( lim_maj(1) == 0 .or. lim_maj(2) == 0 ) then
         Erreur%Numero = 202
         Erreur%ft   = err_202
         Erreur%ft_c = err_202c
         call TRAITER_ERREUR( Erreur , 'MAJEUR' , iprof )
         return
      end if

      if( DebProgressifZS ) then
         tableau_temp(1) = 1
         tableau_temp(2) = nb_point

         call PLADEB           ( &
         SS_sous_cote_debord   , &
         lim_maj               , &
         tableau_temp          , &
         iprof                 , &
         DXP                   , &
         DYP                   , &
         FrottParoiVerticale   , &
         ImpressionPlani       , &
         UniteListing          , &
         Erreur                  &
                          )

         if( Erreur%Numero /= 0 ) then
            return
         end if
      end if

      ! RECHERCHE ET DEFINITION DU POINT BAS DU PROFIL
      DZREF = DYP(1)
      do ipoint = 2 , nb_point
         DZREF = DMIN1( DZREF , DYP(ipoint) )
      end do

      Profil(iprof)%Zref = DZREF

      ! RECHERCHE DU POINT BAS EN LIT MAJEUR GAUCHE
      zref_LM(1) = INFINI
      do ipoint = lim_maj(1), lim_min(1) - 1
         if( DYP(ipoint) <= zref_LM(1) ) then
            zref_LM(1) = DYP(ipoint)
         end if
      end do

      ! RECHERCHE DU POINT BAS EN LIT MAJEUR DROIT
      zref_LM(2) = INFINI
      do ipoint = lim_min(2) + 1 , lim_maj(2)
         if( DYP(ipoint) <= zref_LM(2) ) then
            zref_LM(2) = DYP(ipoint)
         end if
      end do

      ! RECHERCHE DU POINT BAS EN ZONE DE STOCKAGE GAUCHE
      zref_ZS(1) = INFINI
      do ipoint = 2 , lim_maj(1) - 1
         if (DYP(ipoint) <= zref_ZS(1)) then
            zref_ZS(1) = DYP(ipoint)
         end if
      end do

      ! RECHERCHE DU POINT BAS EN ZONE DE STOCKAGE DROITE
      zref_ZS(2) = INFINI
      do ipoint = lim_maj(2) + 1, nb_point - 1
         if( DYP(ipoint) <= zref_ZS(2) ) then
            zref_ZS(2) = DYP(ipoint)
         end if
      end do

      ! CALCUL DE LA LARGEUR AU MIROIR, DU PERIMETRE MOUILLE ET DE LA SECTION
      ! MOUILLEE DU LIT ACTIF
      ! ---------------------------------------------------------------------
      Boucle_pas : do ipas = 1 , nb_pas
         cote = DZREF + ( ipas - 1 ) * pas
         if( ipas == 1 ) then
            cote = DZREF + pas * EPS5
         end if
         do irive = 1 , 2
            if( DABS( cote - zref_LM(irive) ) < EPS2 ) then
               cote = zref_LM(irive) + pas * EPS5
            end if
            if( DABS( cote - zref_ZS(irive) ) < EPS2 ) then
               cote = zref_ZS(irive) + pas * EPS5
            end if
         end do

         call PAPY                         ( &
         DB1(ipas), DB2(ipas), DBS(ipas)   , &
         DP1(ipas), DP2(ipas)              , &
         DS1(ipas), DS2(ipas)              , &
         DSS(ipas), DS2G(ipas)             , &
         iprof                             , &
         cote                              , &
         lim_min, lim_maj                  , &
         DXP                               , &
         DYP                               , &
         S2_sous_cote_debord               , &
         SS_sous_cote_debord               , &
         DebProgressifLM                   , &
         DebProgressifZS                   , &
         FrottParoiVerticale               , &
         ImpressionPlani                   , &
         UniteListing                      , &
         Erreur                              &
                                      )

         if( Erreur%Numero /= 0 ) then
            return
         end if

         ! TEST SUR LES SECTIONS NEGATIVES ( PROBLEMES DE SAISIE DE DONNEES)
         if( DS1(ipas) <= W0 ) then
            Erreur%Numero = 205
            Erreur%ft     = err_205
            Erreur%ft_c   = err_205c
            call TRAITER_ERREUR( Erreur , 'MINEUR' , iprof )
            return
         end if

         if( DS2(ipas) <= W0 ) then
            Erreur%Numero = 205
            Erreur%ft     = err_205
            Erreur%ft_c   = err_205c
            call TRAITER_ERREUR( Erreur , 'MAJEUR' , iprof )
            return
         end if

         if( DSS(ipas) <= W0 ) then
            Erreur%Numero = 205
            Erreur%ft     = err_205
            Erreur%ft_c   = err_205c
            call TRAITER_ERREUR( Erreur , 'DE STOCKAGE' , iprof )
            return
         end if

         ! IMPRESSION DU PLANIMETRAGE
         if( ImpressionPlani ) then
            if( ipas == 1 ) then
               if( iprof == 1 ) then
                  write(UniteListing,'(''1'')')
               end if
               write (UniteListing,9500) iprof , nom_prof(1:12) , abs_rel , abs_abs , DZREF
               write (UniteListing,9510)
            end if
            write (UniteListing,9520)                  &
              cote, DB1(ipas), DB2(ipas),              &
              DP1(ipas), DP2(ipas),                    &
              DS1(ipas), DS2(ipas),DBS(ipas), DSS(ipas)
         end if
      end do Boucle_pas

      ProfilPlan%B1(iprof,:)  = DB1(:)
      ProfilPlan%B2(iprof,:)  = DB2(:)
      ProfilPlan%BS(iprof,:)  = DBS(:)
      ProfilPlan%P1(iprof,:)  = DP1(:)
      ProfilPlan%P2(iprof,:)  = DP2(:)
      ProfilPlan%S1(iprof,:)  = DS1(:)
      ProfilPlan%S2(iprof,:)  = DS2(:)
      ProfilPlan%SS(iprof,:)  = DSS(:)
      ProfilPlan%S2G(iprof,:) = DS2G(:)

      ! DE ALLOCATION DES TABLEAUX TEMPORAIRES
      !---------------------------------------
      deallocate( DXP, STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 6
         Erreur%ft     = err_6
         Erreur%ft_c   = err_6c
         call TRAITER_ERREUR( Erreur , 'DXP' )
         return
      end if

      deallocate(DYP, STAT = retour)
      if( retour /= 0 ) then
         Erreur%Numero = 6
         Erreur%ft     = err_6
         Erreur%ft_c   = err_6c
         call TRAITER_ERREUR( Erreur , 'DYP' )
         return
      end if
   end do Boucle_profil

   !
   ! Calcul des tableaux supplementaires pour la fonction impulsion
   !
   !      Boucle sur les profils
   do iprof = 1 , size( Profil )
      nb_point = size( Profil(iprof)%X )
           pas = Profil(iprof)%Pas
        nb_pas = Profil(iprof)%NbPas
   F1(iprof,1) = 0.D0
      do J = 2 , nb_pas
         F1(iprof,J) = F1(iprof,J-1) + &
                       ( ProfilPlan%B1(iprof,J-1) + ProfilPlan%B2(iprof,J-1)   &
                       + ProfilPlan%B1(iprof,J) + ProfilPlan%B2(iprof,J)) * ( J - 1 ) * pas * pas / 2.0D0
      enddo
   enddo

   ! Fin des traitements
   ! -------------------

   !  Erreur%arbredappel = arbredappel_old

   return

   ! ... Format Declarations ...

9000    format (/,'PLANIMETRAGE',/, &
               &  '------------',/)

9500	format (//,                                     &
        'Profil de donnee numero ',i4,', Nom : ',a12,/, &
        'Abscisse relative  =',f10.2,/,                 &
        'Abscisse de calcul =',f10.2,/,                 &
        'Cote basse (ZREF)  =',f10.2,/)

9510	format ( &
        5x,' Z ',6x,'DB1',7x,'DB2 ',6x,'DP1 ',6x,'DP2 ',7x,'DS1',8x,'DS2',9x, &
        'DBS',8x,'DSS',/)

9520	format (f9.2,4f10.2,4f11.2)

end subroutine PLANIM
