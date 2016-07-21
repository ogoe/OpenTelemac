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

subroutine LEC_LOI_TRACER( &
               LoiTracer , & ! Tableau des lois tracer
                  nbtrac , & ! Nombre de traceurs
        FichierLoiTracer , & ! Fichier des lois tracer
              impression , & ! Flag d'impression des lois
            UniteListing , & ! Unite logique fichier listing
            TempsMaximum , & ! Temps maximum du calcul
                document , & ! Pointeur vers document XML                  
                  Erreur   & ! Erreur
                        )

!*****************************************************************************
! PROGICIEL : TRACER         M. LUCK
!                            F. ZAOUI                        
!
! VERSION : 8.1.1              EDF-CEREMA
!*****************************************************************************
!  Fonction : Lecture des lois temporelles d'evolution de concentration
!  --------    (pour CL et apports)
!
!  Sous-programme appelant : Pretrait_Traceur
!  -----------------------
!
!  Sous-programme appele : LecFicLoi_Tracer
!  ---------------------
!*************************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_FICHIER_T           ! UniteListing
   use M_LOI_TRACER_T        ! Types LOI_TRACER_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_MESSAGE_TRACER_C
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur
   use M_LEC_FIC_LOI_TRACER_I  ! Interface de sous-programme
   use Fox_dom               ! parser XML Fortran
   
   implicit none

   ! Arguments
   type(LOI_TRACER_T), dimension(:), pointer    :: LoiTracer
   type(FICHIER_T)              , intent(inout) :: FichierLoiTracer
   logical                      , intent(in   ) :: impression
   integer                      , intent(in   ) :: Nbtrac
   integer                      , intent(in   ) :: UniteListing
   real(DOUBLE)                 , intent(in   ) :: TempsMaximum
   type(Node), pointer, intent(in)                   :: document
   ! Variables locales
   integer :: nb_loi_tracer         ! nombre de lois Tracer
   integer :: nb_point              ! nombre de points
   integer :: nb_point_z,nb_point_q ! nombre de points
   integer :: iloi                  ! compteur sur les lois
   integer :: i                     ! compteur sur les points
   integer :: j ,k                  ! compteur sur les points
   integer :: retour                ! code de retour des fonctions intrinseques
   integer :: mode_entree_loi       ! type d'entree clavier/fichier
   integer :: unite_temps           ! unite de temps des lois entres par clavier
   character(132) :: arbredappel_old
   type(Node), pointer :: champ1,champ2,champ3,champ4,champ5
   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero      = 0
   retour             = 0
   arbredappel_old    = trim(Erreur%arbredappel)
   Erreur%arbredappel = trim(Erreur%arbredappel)//'=>LEC_LOI_TRACER'

   write(UniteListing,10000)

   ! Nombre de lois
   !---------------
   champ1 => item(getElementsByTagname(document, "parametresTraceur"), 0)
   if(associated(champ1).eqv..false.) then
      print*,"Parse error => parametresTraceur"
      call xerror(Erreur)
      return
   endif
   champ2 => item(getElementsByTagname(champ1, "parametresLoisTraceur"), 0)
   if(associated(champ2).eqv..false.) then
      print*,"Parse error => parametresLoisTraceur"
      call xerror(Erreur)
      return
   endif
   champ3 => item(getElementsByTagname(champ2, "nbLoisTracer"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => nbLoisTracer"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,nb_loi_tracer)
   if( impression ) then
      write(UniteListing,10010) nb_loi_tracer
   endif

   ! Allocation des lois
   !--------------------
   if(.not.associated(LoiTracer)) allocate( LoiTracer(nb_loi_tracer) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR  (Erreur, 'LoiTracer')
      return
   end if

   champ3 => item(getElementsByTagname(champ2, "loisTracer"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => loisTracer"
      call xerror(Erreur)
      return
   endif
   do iloi = 1 , nb_loi_tracer

      champ4 => item(getElementsByTagname(champ3, "structureSParametresLoiTraceur"), iloi-1)
      if(associated(champ4).eqv..false.) then
         print*,"Parse error => structureSParametresLoiTraceur"
         call xerror(Erreur)
         return
      endif
      champ5 => item(getElementsByTagname(champ4, "nom"), 0)
      if(associated(champ5).eqv..false.) then
         print*,"Parse error => nom"
         call xerror(Erreur)
         return
      endif
      LoiTracer(iloi)%Nom = getTextContent(champ5)
      champ5 => item(getElementsByTagname(champ4, "modeEntree"), 0)
      if(associated(champ5).eqv..false.) then
         print*,"Parse error => modeEntree"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ5,mode_entree_loi)
      
      if( mode_entree_loi /= SAISIE_PAR_FICHIER .and. &
          mode_entree_loi /= SAISIE_PAR_CLAVIER ) then
         Erreur%Numero = 510
         Erreur%ft     = err_510
         Erreur%ft_c   = err_510c
         call TRAITER_ERREUR( Erreur , 'la loi tracer' )
         return
      end if

      if( impression ) then
         write(UniteListing,10020) iloi , LoiTracer(iloi)%Nom
      endif

      if( mode_entree_loi == SAISIE_PAR_FICHIER ) then

         champ5 => item(getElementsByTagname(champ4, "fichier"), 0)
         if(associated(champ5).eqv..false.) then
            print*,"Parse error => fichier"
            call xerror(Erreur)
            return
         endif
         FichierLoiTracer%Nom = getTextContent(champ5)
         
         if( impression ) then
            write(UniteListing,10030) 'PAR FICHIER' , FichierLoiTracer%Nom
         endif

         call LEC_FIC_LOI_TRACER( &
               FichierLoiTracer , &	! Fic. contenant une evolution temporelle de conc
                LoiTracer(iloi) , & ! Concentration initiale des traceurs
                    unite_temps , & ! unite de temps des chroniques temporelles
                           iloi , & ! Numero de loi
                         nbtrac , & ! Nombre de traceurs
                         Erreur  )

         if( Erreur%Numero /= 0 ) then
            return
         endif

     endif ! de mode de saisie

      ! Passage du temps en secondes
      nb_point = size(LoiTracer(iloi)%Temps(:))

      if( unite_temps /= LOI_UNITE_SECONDE ) then

         select case (unite_temps)

            case( LOI_UNITE_MINUTE )
               do i = 1 , nb_point
                  LoiTracer(iloi)%Temps(i) = LoiTracer(iloi)%Temps(i) * 60.d0
               end do
            case( LOI_UNITE_HEURE )
               do i = 1 , nb_point
                  LoiTracer(iloi)%Temps(i) = LoiTracer(iloi)%Temps(i) * 3600.d0
               end do
            case( LOI_UNITE_JOUR )
               do i = 1 , nb_point
                  LoiTracer(iloi)%Temps(i) = LoiTracer(iloi)%Temps(i) * 86400.d0
               end do

         end select

      endif  ! de unite de temps

      ! Coherence avec le nombre de pas de temps de la simulation
      if( LoiTracer(iloi)%Temps(nb_point) < TempsMaximum ) then
         Erreur%Numero = 517
         Erreur%ft     = err_517
         Erreur%ft_c   = err_517c
         call TRAITER_ERREUR( Erreur , iloi , trim(LoiTracer(iloi)%Nom) )
         return
      end if

   end do

   !Erreur%Arbredappel = arbredappel_old

   return

   10000 format (/,'LOIS TRACER',/, &
                &  '-----------------',/)
   10010 format ('Nombre de lois = ',i3)
   10020 format (/,'Loi ',i3,' : Nom = ',A)
   10030 format ('Mode d''entree      = ',A,' Nom du fichier = ',A)
   10040 format ('Mode d''entree      = ',A)
   10045 format ('Unite de temps     = ',A)
   10050 format ('Nombre de points   = ',i3)
   10060 format (A)
   10070 format (i5,11f12.3)

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
   
end subroutine LEC_LOI_TRACER
