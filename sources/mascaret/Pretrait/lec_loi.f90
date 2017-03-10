!== Copyright (C) 2000-2017 EDF-CEREMA ==
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

subroutine LEC_LOI    ( &
     LoiHydrau        , & ! Tableau des lois hydrauliques
     FichierLoiHydrau , & ! Fichier des lois hydrauliques
     impression_hydrau, & ! Flag d'impression des lois
     UniteListing     , & ! Unite logique fichier listing
     CritereArret     , & ! Critere d'arret du calcul
     TempsMaximum     , & ! Temps maximum du calcul
     document         , & ! Pointeur vers document XML
     Erreur             & ! Erreur
                      )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            F. ZAOUI                      
!
! VERSION : 8.1.3              EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_FICHIER_T           ! UniteListing
   use M_LOI_T               ! Types LOI_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_LEC_HYDRAU_I        ! Interface de sous-programme
   use Fox_dom               ! parser XML Fortran
   
   implicit none
   ! Arguments
   type(LOI_T)    , dimension(:), pointer       :: LoiHydrau
   type(FICHIER_T)              , intent(inout) :: FichierLoiHydrau
   logical                      , intent(in   ) :: impression_hydrau
   integer                      , intent(in   ) :: UniteListing
   integer                      , intent(in   ) :: CritereArret
   real(DOUBLE)                 , intent(in   ) :: TempsMaximum
   type(Node), pointer, intent(in) :: document
   ! Variables locales
   integer :: nb_loi   ! nombre de lois
   integer :: nb_point ! nombre de points
   integer :: nb_point_z,nb_point_q ! nombre de points
   integer :: iloi     ! compteur sur les lois
   integer :: i        ! compteur sur les points
   integer :: j ,k       ! compteur sur les points
   integer :: retour   ! code de retour des fonctions intrinseques
   integer :: mode_entree_loi ! type d'entree clavier/fichier
   integer :: unite_temps     ! unite de temps des lois entres par clavier
   !character(132) :: !arbredappel_old
   type(Node), pointer :: champ1,champ2,champ3,champ4,champ5
   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_LOI'

   write(UniteListing,10000)

   ! Nombre de lois
   !---------------
   champ1 => item(getElementsByTagname(document, "parametresLoisHydrauliques"), 0)
   if(associated(champ1).eqv..false.) then
      print*,"Parse error => parametresLoisHydrauliques"
      call xerror(Erreur)
      return
   endif
   champ2 => item(getElementsByTagname(champ1, "nb"), 0)
   if(associated(champ2).eqv..false.) then
      print*,"Parse error => nb"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ2,nb_loi)
   if( nb_loi < 2 ) then
      Erreur%Numero = 305
      Erreur%ft     = err_305
      Erreur%ft_c   = err_305c
      call TRAITER_ERREUR( Erreur , 'Nombre de lois' , 'superieurs ou egaux a 2' )
      return
   end if

   if( impression_hydrau ) then
      write(UniteListing,10010) nb_loi
   endif

   ! Allocation des lois
   !--------------------
   if(.not.associated(LoiHydrau)) allocate( LoiHydrau(nb_loi) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR  (Erreur, 'LoiHydrau')
      return
   end if

   champ2 => item(getElementsByTagname(champ1, "lois"), 0)
   if(associated(champ2).eqv..false.) then
      print*,"Parse error => lois"
      call xerror(Erreur)
      return
   endif
   
   do iloi = 1 , nb_loi

      champ3 => item(getElementsByTagname(champ2, "structureParametresLoi"), iloi-1)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => structureParametresLoi"
         call xerror(Erreur)
         return
      endif
      champ4 => item(getElementsByTagname(champ3, "nom"), 0)
      if(associated(champ4).eqv..false.) then
         print*,"Parse error => nom"
         call xerror(Erreur)
         return
      endif
      LoiHydrau(iloi)%Nom  = getTextContent(champ4)
      
      champ4 => item(getElementsByTagname(champ3, "type"), 0)
      if(associated(champ4).eqv..false.) then
         print*,"Parse error => type"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ4,LoiHydrau(iloi)%Type)
      
      if( LoiHydrau(iloi)%Type < 1 .or. LoiHydrau(iloi)%Type > LOI_TYPE_NB_MAX ) then
         Erreur%Numero = 317
         Erreur%ft     = err_317
         Erreur%ft_c   = err_317c
         call TRAITER_ERREUR( Erreur , iloi )
         return
      end if

      champ4 => item(getElementsByTagname(champ3, "donnees"), 0)
      if(associated(champ4).eqv..false.) then
         print*,"Parse error => donnees"
         call xerror(Erreur)
         return
      endif
      champ5 => item(getElementsByTagname(champ4, "modeEntree"), 0)
      if(associated(champ5).eqv..false.) then
         print*,"Parse error => modeEntree"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ5,mode_entree_loi)
      
      if( mode_entree_loi /= SAISIE_PAR_FICHIER .and. mode_entree_loi /= SAISIE_PAR_CLAVIER ) then
         Erreur%Numero = 318
         Erreur%ft     = err_318
         Erreur%ft_c   = err_318c
         call TRAITER_ERREUR( Erreur , iloi )
         return
      end if

      if( impression_hydrau ) then
         write(UniteListing,10020) iloi, LoiHydrau(iloi)%Nom, LoiHydrau(iloi)%Type
      endif

      if( mode_entree_loi == SAISIE_PAR_FICHIER ) then

         champ5 => item(getElementsByTagname(champ4, "fichier"), 0)
         if(associated(champ5).eqv..false.) then
            print*,"Parse error => fichier"
            call xerror(Erreur)
            return
         endif
         FichierLoiHydrau%Nom = getTextContent(champ5) 
         if( impression_hydrau ) then
            write(UniteListing,10030) 'PAR FICHIER' , FichierLoiHydrau%Nom
         endif

         call LEC_HYDRAU          ( &
              LoiHydrau(iloi)     , &
              unite_temps         , &
              FichierLoiHydrau    , &
              impression_hydrau   , &
              UniteListing        , &
              Erreur                &
                                  )
         if( Erreur%Numero /= 0 ) then
            return
         endif

      endif ! de mode de saisie

      !---------------------------------------------------------
      ! Controles communs des lois quelquesoit le mode de saisie
      !---------------------------------------------------------
      ! Controles sur les courbes de tarage
      !------------------------------------
      if( LoiHydrau(iloi)%Type == LOI_TYPE_TARAGE_Z_Q .or. LoiHydrau(iloi)%Type == LOI_TYPE_TARAGE_Q_Z ) then

         nb_point = size(LoiHydrau(iloi)%Cote(:))
         ! Detection de points de rebroussements
         do i = 2 , nb_point
            if( ( LoiHydrau(iloi)%Cote(i) <= LoiHydrau(iloi)%Cote(i-1) ) .or. &
                ( LoiHydrau(iloi)%Debit(i) <= LoiHydrau(iloi)%Debit(i-1) ) ) then
               Erreur%Numero = 22
               Erreur%ft     = err_22
               Erreur%ft_c   = err_22c
               call TRAITER_ERREUR( Erreur , LoiHydrau(iloi)%Nom , i )
               return
            end if
         end do

      endif

      ! Controles sur les Lois temporelles
      !-----------------------------------
      if( LoiHydrau(iloi)%Type == LOI_TYPE_LIMNIGRAMME     .or.  &
          LoiHydrau(iloi)%Type == LOI_TYPE_HYDROGRAMME     .or.  &
          LoiHydrau(iloi)%Type == LOI_TYPE_LIMNHYDROGRAMME .or.  &
          LoiHydrau(iloi)%Type == LOI_TYPE_ZINF_ZSUP_T) then

         nb_point = size(LoiHydrau(iloi)%Temps(:))

         ! Detection de points de rebroussements
         do i = 2 , nb_point
            if( LoiHydrau(iloi)%Temps(i) <= &
                LoiHydrau(iloi)%Temps(i-1) ) then
               Erreur%Numero = 22
               Erreur%ft     = err_22
               Erreur%ft_c   = err_22c
               call TRAITER_ERREUR( Erreur , LoiHydrau(iloi)%Nom , i )
               return
            end if
         end do

         ! Passage du temps en secondes
         if( unite_temps /= LOI_UNITE_SECONDE ) then
            select case( unite_temps )
               case( LOI_UNITE_MINUTE )
                  do i = 1 , nb_point
                     LoiHydrau(iloi)%Temps(i) = LoiHydrau(iloi)%Temps(i) * 60._DOUBLE
                  end do
               case( LOI_UNITE_HEURE )
                  do i = 1 , nb_point
                     LoiHydrau(iloi)%Temps(i) = LoiHydrau(iloi)%Temps(i) * 3600._DOUBLE
                  end do
               case( LOI_UNITE_JOUR )
                  do i = 1 , nb_point
                     LoiHydrau(iloi)%Temps(i) = LoiHydrau(iloi)%Temps(i) * 86400._DOUBLE
                  end do

            end select

         endif  ! de unite de temps

         ! Coherence avec le nombre de pas de temps de la simulation
         if( CritereArret == TEMPS_MAXIMUM            .and. &
             LoiHydrau(iloi)%Temps(nb_point) < TempsMaximum) then
            Erreur%Numero = 303
            Erreur%ft     = err_303
            Erreur%ft_c   = err_303c
            call TRAITER_ERREUR( Erreur , iloi , trim(LoiHydrau(iloi)%Nom) )
            return
         end if

      endif    ! de lois temporelles

   end do

   !Erreur%Arbredappel = !arbredappel_old

   return

  10000 format (/,'LOIS HYDRAULIQUES',/, &
               &  '-----------------',/)
  10010 format ('Nombre de lois = ',i3)
  10020 format (/,'Loi ',i3,' : Nom = ',A,' Type =',i2)
  10030 format ('Mode d''entree      = ',A,' Nom du fichier = ',A)
  10040 format ('Mode d''entree      = ',A)
  10045 format ('Unite de temps     = ',A)
  10050 format ('Nombre de points   = ',i3)
  10060 format (A)
  10070 format (i5,2f12.3)
  10080 format (i5,3f12.3)
  10090 format (f12.3)

   contains
   
   subroutine xerror(Erreur)
       
       use M_MESSAGE_C
       use M_ERREUR_T            ! Type ERREUR_T
       
       type(ERREUR_T)                   , intent(inout) :: Erreur
       
       Erreur%Numero = 704
       Erreur%ft     = err_704
       Erreur%ft_c   = err_704c
       call TRAITER_ERREUR( Erreur )
       
       return
        
   end subroutine xerror      

end subroutine LEC_LOI
