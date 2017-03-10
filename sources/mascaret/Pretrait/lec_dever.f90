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

subroutine LEC_DEVER( &
              Deversoir , & ! Tableau des lois hydrauliques
                Connect , & ! Table de connectivite du reseau
                      X , & ! Maillage
              LoiHydrau , & ! Lois hydrauliques
                 Profil , & ! Profils geometriques
            ProfDebBief , & ! Premiers profils des biefs
            ProfFinBief , & ! Derniers profils des biefs
      AbscRelExtDebBief , & ! Abscisse de l'extremite debut du bief
      AbscRelExtFinBief , & ! Abscisse de l'extremite debut du bief
           UniteListing , & ! Unite logique fichier listing
               document , & ! Pointeur vers document XML
                 Erreur & ! Erreur
                       )
! *********************************************************************
! PROGICIEL : MASCARET         A. LEBOSSE
!                              S. MANDELKERN
!                              F. ZAOUI                       
!
! VERSION : 8.1.3                EDF-CEREMA
! *********************************************************************

   !========================= Declarations ===========================
   use M_PRECISION
   use M_CONNECT_T           ! Type CONNECT_T
   use M_ERREUR_T            ! Type ERREUR_T
   use M_DEVERSOIR_T         ! Type DEVERSOIR_T
   use M_LOI_T               ! Types LOI_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_XINDIC_S            ! Calc de l'indice corresp a une absc
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use Fox_dom               ! parser XML Fortran
   
   implicit none
   ! Arguments
   type(DEVERSOIR_T) , dimension(:), pointer       :: Deversoir
   type(CONNECT_T)                 , intent(in   ) :: Connect
   real(DOUBLE)      , dimension(:), intent(in   ) :: X
   type(LOI_T)       , dimension(:), intent(in   ) :: LoiHydrau
   type(PROFIL_T)    , dimension(:), intent(in   ) :: Profil
   integer           , dimension(:), intent(in   ) :: ProfDebBief
   integer           , dimension(:), intent(in   ) :: ProfFinBief
   real(DOUBLE)      , dimension(:), intent(in   ) :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:), intent(in   ) :: AbscRelExtFinBief
   integer                         , intent(in   ) :: UniteListing
   type(Node), pointer, intent(in)                   :: document
   ! Traitement des erreurs
   type(ERREUR_T), intent(inout) :: Erreur
   ! Variables locales
   integer :: nb_deversoir ! nombre de deversoirs
   integer :: nb_point     ! nombre de points de la loi discretisee
   integer :: idev         ! compteur sur les deversoirs
   integer :: ipoint       ! compteur sur les points
   integer :: retour       ! code de retour des fonctions intrinseques
   integer :: num_branche  ! numero de branche du deversoir
   integer :: num_loi      ! numero de loi
   real(DOUBLE) :: abs_abs  ! abscisse absolue du deversoir
   real(DOUBLE) :: abs_fin  ! abscisse relative de fin du deversoir
   type(Node), pointer :: champ1,champ2,champ3,champ4
   integer, allocatable :: itab1(:),itab2(:),itab3(:)
   real(DOUBLE), allocatable :: rtab1(:),rtab2(:),rtab3(:),rtab4(:)
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_DEVER'
   if (UniteListing >0) write(UniteListing,11000)

   champ1 => item(getElementsByTagname(document, "parametresApportDeversoirs"), 0)
   if(associated(champ1).eqv..false.) then
      print*,"Parse error => parametresApportDeversoirs"
      call xerror(Erreur)
      return
   endif
   champ2 => item(getElementsByTagname(champ1, "deversLate"), 0)
   if(associated(champ2).eqv..false.) then
       nb_deversoir = 0
   else
       champ3 => item(getElementsByTagname(champ2, "nbDeversoirs"), 0)
       if(associated(champ3).eqv..false.) then
          print*,"Parse error => nbDeversoirs"
          call xerror(Erreur)
          return
       endif
       call extractDataContent(champ3,nb_deversoir)
       if (nb_deversoir < 0) then
          Erreur%Numero = 306
          Erreur%ft     = err_306
          Erreur%ft_c   = err_306c
          call TRAITER_ERREUR  (Erreur, nb_deversoir)
          return
       end if
   endif

   if (UniteListing >0) write(UniteListing,11010) nb_deversoir

   if( nb_deversoir > 0 ) then

      if(.not.associated(Deversoir)) allocate( Deversoir(nb_deversoir) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Deversoir' )
         return
      end if

      allocate( itab1(nb_deversoir) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab1' )
          return
      end if
      allocate( itab2(nb_deversoir) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab2' )
          return
      end if
      allocate( itab3(nb_deversoir) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab3' )
          return
      end if
      allocate( rtab1(nb_deversoir) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab1' )
          return
      end if
      allocate( rtab2(nb_deversoir) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab2' )
          return
      end if
      allocate( rtab3(nb_deversoir) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab3' )
          return
      end if
      allocate( rtab4(nb_deversoir) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'rtab4' )
          return
      end if
      
       champ3 => item(getElementsByTagname(champ2, "type"), 0)
       if(associated(champ3).eqv..false.) then
          print*,"Parse error => type"
          call xerror(Erreur)
          return
       endif
       call extractDataContent(champ3,itab1)
       champ3 => item(getElementsByTagname(champ2, "numBranche"), 0)
       if(associated(champ3).eqv..false.) then
          print*,"Parse error => numBranche"
          call xerror(Erreur)
          return
       endif
       call extractDataContent(champ3,itab2)
       champ3 => item(getElementsByTagname(champ2, "numLoi"), 0)
       if(associated(champ3).eqv..false.) then
          print*,"Parse error => numLoi"
          call xerror(Erreur)
          return
       endif
       call extractDataContent(champ3,itab3)
       champ3 => item(getElementsByTagname(champ2, "abscisse"), 0)
       if(associated(champ3).eqv..false.) then
          print*,"Parse error => abscisse"
          call xerror(Erreur)
          return
       endif
       call extractDataContent(champ3,rtab1)
       champ3 => item(getElementsByTagname(champ2, "longueur"), 0)
       if(associated(champ3).eqv..false.) then
          print*,"Parse error => longueur"
          call xerror(Erreur)
          return
       endif
       call extractDataContent(champ3,rtab2)
       champ3 => item(getElementsByTagname(champ2, "coteCrete"), 0)
       if(associated(champ3).eqv..false.) then
          print*,"Parse error => coteCrete"
          call xerror(Erreur)
          return
       endif
       call extractDataContent(champ3,rtab3)
       champ3 => item(getElementsByTagname(champ2, "coeffDebit"), 0)
       if(associated(champ3).eqv..false.) then
          print*,"Parse error => coeffDebit"
          call xerror(Erreur)
          return
       endif
       call extractDataContent(champ3,rtab4)
       
      
      do idev = 1 , nb_deversoir
          ! Initialisation des pointeurs pour l'API
          nullify(Deversoir(idev)%PtZ)
          nullify(Deversoir(idev)%PtQ)

         champ3 => item(getElementsByTagname(champ2, "noms"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => noms"
            call xerror(Erreur)
            return
         endif
         champ4 => item(getElementsByTagname(champ3, "string"), idev-1)
         if(associated(champ4).eqv..false.) then
            print*,"Parse error => string"
            call xerror(Erreur)
            return
         endif
         Deversoir(idev)%Nom  = getTextContent(champ4)
         
         Deversoir(idev)%Type = itab1(idev)
         if( Deversoir(idev)%Type /= DEVERSOIR_TYPE_CRETE_COEFF .and. Deversoir(idev)%Type /= DEVERSOIR_TYPE_LOI_Z_Q ) then
            Erreur%Numero = 340
            Erreur%ft     = err_340
            Erreur%ft_c   = err_340c
            call TRAITER_ERREUR  (Erreur, idev, Deversoir(idev)%Type)
            return
         end if

         if (UniteListing >0) write(UniteListing,11020) idev, Deversoir(idev)%Nom, Deversoir(idev)%Type

         Deversoir(idev)%NumBranche  = itab2(idev)
         num_branche                 = Deversoir(idev)%NumBranche
         if( num_branche < 1 .or. num_branche > size(Connect%OrigineBief) ) then
            Erreur%Numero = 332
            Erreur%ft     = err_332
            Erreur%ft_c   = err_332c
            call TRAITER_ERREUR( Erreur , 'deversoirs' , Deversoir(idev)%NumBranche , idev )
            return
         end if

         Deversoir(idev)%AbscisseRel = rtab1(idev)
         if( Deversoir(idev)%AbscisseRel < AbscRelExtDebBief(num_branche) .or. &
             Deversoir(idev)%AbscisseRel > AbscRelExtFinBief(num_branche)) then
            Erreur%Numero = 364
            Erreur%ft     = err_364
            Erreur%ft_c   = err_364c
            call TRAITER_ERREUR( Erreur , idev , Deversoir(idev)%AbscisseRel , num_branche )
            return
         endif

         Deversoir(idev)%Longueur = rtab2(idev)

         abs_fin = Deversoir(idev)%AbscisseRel + Deversoir(idev)%Longueur
         if( Deversoir(idev)%Longueur < 0._DOUBLE ) then
            Erreur%Numero = 342
            Erreur%ft     = err_342
            Erreur%ft_c   = err_342c
            call TRAITER_ERREUR( Erreur , idev , Deversoir(idev)%Longueur )
            return
         end if

         if( abs_fin < AbscRelExtDebBief(num_branche) .or. abs_fin > AbscRelExtFinBief(num_branche) ) then
            Erreur%Numero = 365
            Erreur%ft     = err_365
            Erreur%ft_c   = err_365c
            call TRAITER_ERREUR( Erreur , idev , abs_fin , num_branche )
            return
         endif

         if (UniteListing >0) write(UniteListing,11030) Deversoir(idev)%NumBranche , Deversoir(idev)%AbscisseRel , &
                                                        Deversoir(idev)%Longueur

         ! Calcul des sections amont et aval des apports
         !----------------------------------------------
         ! 1) Amont
         ! Calcul de l'abscisse absolue
         abs_abs = ABS_ABS_S           ( &
           num_branche                 , &
           Deversoir(idev)%AbscisseRel , &
           Profil                      , &
           ProfDebBief                 , &
           ProfFinBief                 , &
           Erreur                        &
                                    )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         ! Calcul de la section de calcul correspondante
         call XINDIC_S( Deversoir(idev)%SectionAm , abs_abs , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         ! 2) Aval
         ! Calcul de l'abscisse absolue
         abs_abs = ABS_ABS_S ( &
           num_branche       , &
           abs_fin           , &
           Profil            , &
           ProfDebBief       , &
           ProfFinBief       , &
           Erreur              &
                            )
         if( Erreur%Numero /= 0 ) then
            return
         end if

         ! Calcul de la section de calcul correspondante
         call XINDIC_S( Deversoir(idev)%SectionAv , abs_abs , X , Erreur )
         if( Erreur%Numero /= 0 ) then
            return
         endif

         Deversoir(idev)%CoteCrete = rtab3(idev)
         if( Deversoir(idev)%CoteCrete < 0._DOUBLE ) then
            Erreur%Numero = 343
            Erreur%ft     = err_343
            Erreur%ft_c   = err_343c
            call TRAITER_ERREUR( Erreur , idev , Deversoir(idev)%CoteCrete )
            return
         end if

         Deversoir(idev)%CoeffDebit = rtab4(idev)
         if( Deversoir(idev)%CoeffDebit < 0._DOUBLE .or. Deversoir(idev)%CoeffDebit >= 1._DOUBLE ) then
            Erreur%Numero = 344
            Erreur%ft     = err_344
            Erreur%ft_c   = err_344c
            call TRAITER_ERREUR( Erreur , idev , Deversoir(idev)%CoeffDebit )
            return
         end if

         if (UniteListing >0) write(UniteListing,11040) Deversoir(idev)%CoteCrete , Deversoir(idev)%CoeffDebit


         ! Calcul de la loi de deversement
         !--------------------------------
         if( Deversoir(idev)%Type == DEVERSOIR_TYPE_LOI_Z_Q ) then
            if (UniteListing >0) write(UniteListing,11050)
            Deversoir(idev)%NumeroLoi = itab3(idev)
            num_loi                   = Deversoir(idev)%NumeroLoi
            if( num_loi <= 0 .or. num_loi > size(LoiHydrau) ) then
               Erreur%Numero = 345
               Erreur%ft     = err_345
               Erreur%ft_c   = err_345c
               call TRAITER_ERREUR( Erreur , idev , num_loi , size(LoiHydrau) )
               return
            end if

            ! Controle de coherence Deversoir / type de loi
            if( LoiHydrau(num_loi)%Type /= LOI_TYPE_TARAGE_Z_Q ) then
               Erreur%Numero = 360
               Erreur%ft     = err_360
               Erreur%ft_c   = err_360c
               call TRAITER_ERREUR( Erreur , idev , num_loi , LoiHydrau(num_loi)%Type )
               return
            endif

            nb_point = size(LoiHydrau(num_loi)%Cote)

            if (UniteListing >0) write(UniteListing,11060) num_loi
            if (UniteListing >0) write(UniteListing,11070) nb_point

            allocate( Deversoir(idev)%PtZ(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR  (Erreur, 'Deversoir(idev)%PtZ')
               return
            end if

            allocate( Deversoir(idev)%PtQ(nb_point) , STAT = retour )
            if( retour /= 0 ) then
               Erreur%Numero = 5
               Erreur%ft     = err_5
               Erreur%ft_c   = err_5c
               call TRAITER_ERREUR( Erreur , 'Deversoir(idev)%PtQ' )
               return
            end if

            if (UniteListing >0) write(UniteListing,11080)

            Deversoir(idev)%PtQ(:) = LoiHydrau(num_loi)%Debit(:)
            Deversoir(idev)%PtZ(:) = LoiHydrau(num_loi)%Cote(:)

            do ipoint = 1 , nb_point
               if (UniteListing >0) write(UniteListing,11090) Deversoir(idev)%PtQ(ipoint) , Deversoir(idev)%PtZ(ipoint)
            end do
         endif
      end do    ! boucle sur les deversoirs

      deallocate(itab1)
      deallocate(itab2)
      deallocate(itab3)
      deallocate(rtab1)
      deallocate(rtab2)
      deallocate(rtab3)
      deallocate(rtab4)
      
   !--------------------
   ! Si pas de deversoir
   !--------------------
   else
      allocate( Deversoir(0) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'Deversoir' )
         return
      end if
   endif       ! de if nb_deversoir > 0

   !Erreur%Arbredappel = !arbredappel_old

   return

   ! Formats
   11000 format (/,'DEVERSOIRS',/, &
               &  '----------',/)
   11010 format ('Nombre de deversoirs : ',i3)
   11020 format (/,'Deversoir ',i3,' Nom : ',A,' Type : ',i3)
   11030 format (' Branche ',i3,' Abscisse = ',f12.3,' Longueur = ',f12.3)
   11040 format (' Cote de crete ',f12.3,' Coeff Debit = ',f12.3)
   11050 format (/,'Le deversoir est du type : LOI Z(Q)')
   11060 format ('Numero de la loi : ',i3)
   11070 format ('Nombre de points de la loi : ',i3)
   11080 format ('           Z           Q')
   11090 format (2f12.3)

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
   
end subroutine LEC_DEVER
