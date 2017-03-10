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

subroutine LEC_RESEAU    ( &
     NbBief              , & ! Nombre de biefs
     AbscRelExtDebBief   , & ! Abscisse rel de l'extremite debut du bief
     AbscRelExtFinBief   , & ! Abscisse rel de l'extremite debut du bief
     AbscAbsExtDebBief   , & ! Abscisse abs de l'extremite debut du bief
     AbscAbsExtFinBief   , & ! Abscisse abs de l'extremite debut du bief
     NbNoeud             , & ! Nombre de noeuds
     NbExtNoeud          , & ! Nombre d'extremite relie a chaque noeud
     ExtDebBief          , & ! Numero de l'extremite debut de chaque bief
     ExtFinBief          , & ! Numero de l'extremite fin de chaque bief
     ExtNoeud            , & ! Numero d'extremite lie a un noeud
     NbExtLibre          , & ! Nombre d'extremites libres
     NumExtLibre         , & ! Numero d'extremite libre
     Extremite           , & ! Extremites libres
     LoiHydrau           , & ! Lois hydrauliques
     ImpressionReseau    , & ! Flag d'impression du reseau
     UlLst               , & ! Unite logique fichier listing
     Profil              , & ! Profils geometriques
     ProfDebBief         , & ! Premiers profils des biefs
     ProfFinBief         , & ! Derniers profils des biefs
     Noyau               , & ! Noyau de calcul
     document            , & ! Pointeur vers document XML
     Erreur                & ! Erreur
                         )

! *********************************************************************
! PROGICIEL : MASCARET       S. MANDELKERN
!                            F. ZAOUI                         
!
! VERSION : 8.1.3              EDF-CEREMA
! *********************************************************************
!
!   FONCTION : Lecture du reseau
!   --------
!   SOUS PROGRAMMES APPELANTS :  PRETRAIT
!   ---------------------------
!   SOUS PROGRAMMES APPELES :    ---
!   -------------------------

   !========================= Declarations ===========================
   use M_PRECISION
   use M_ERREUR_T            ! Type ERREUR_T
   use M_EXTREMITE_T         ! Type EXTREMITE_T
   use M_LOI_T               ! Types LOI_T
   use M_PROFIL_T            ! Type  PROFIL_T
   use M_MESSAGE_C           ! Messages d'erreur
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_PARAMETRE_C         ! Parametres de calcul
   use M_TRAITER_ERREUR_I    ! Traitement de l'erreur
   use M_ABS_ABS_S           ! Calcul de l'abscisse absolue
   use Fox_dom               ! parser XML Fortran

   implicit none

   ! Arguments
   integer                           , intent(  out) :: NbBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscRelExtDebBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscRelExtFinBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscAbsExtDebBief
   real(DOUBLE)      , dimension(:)  , pointer       :: AbscAbsExtFinBief
   integer                           , intent(  out) :: NbNoeud
   integer           , dimension(:)  , pointer       :: NbExtNoeud
   integer           , dimension(:)  , pointer       :: ExtDebBief
   integer           , dimension(:)  , pointer       :: ExtFinBief
   integer           , dimension(:,:), pointer       :: ExtNoeud
   integer                           , intent(  out) :: NbExtLibre
   integer           , dimension(:)  , pointer       :: NumExtLibre
   type(EXTREMITE_T) , dimension(:)  , pointer       :: Extremite
   type(LOI_T)       , dimension(:)  , intent(in   ) :: LoiHydrau
   logical                           , intent(in   ) :: ImpressionReseau
   integer                           , intent(in   ) :: UlLst
   type(PROFIL_T)    , dimension(:)  , intent(in   ) :: Profil
   integer           , dimension(:)  , intent(in   ) :: ProfDebBief
   integer           , dimension(:)  , intent(in   ) :: ProfFinBief
   integer                           , intent(in   ) :: Noyau
   type(Node), pointer, intent(in)                   :: document
   type(ERREUR_T)                    , intent(inout) :: Erreur
   ! Variables locales
   integer :: num_loi
   integer :: dim
   integer :: ibief  ! compteur sur les biefs
   integer :: inoeud ! compteur sur les noeud
   integer :: iext   ! compteur sur les extremites
   integer :: i      ! compteur
   integer :: int    !
   integer :: retour ! code de retour des fonctions intrinseques
   logical :: test_loi ! test si au moins une extremite est reliee a une loi hydrau
   type(Node), pointer :: champ1,champ2,champ3,champ4,champ5
   !character(132) :: !arbredappel_old

   !========================= Instructions ===========================
   ! INITIALISATION
   ! --------------
   Erreur%Numero = 0
   retour        = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>LEC_RESEAU'

   if( ImpressionReseau ) then
      write(UlLst,10000)
   endif

   ! Nombre de branches
   !-------------------
   champ1 => item(getElementsByTagname(document, "parametresGeometrieReseau"), 0)
   if(associated(champ1).eqv..false.) then
      print*,"Parse error => parametresGeometrieReseau"
      call xerror(Erreur)
      return
   endif
   champ2 => item(getElementsByTagname(champ1, "listeBranches"), 0)
   if(associated(champ2).eqv..false.) then
      print*,"Parse error => listeBranches"
      call xerror(Erreur)
      return
   endif
   champ3 => item(getElementsByTagname(champ2, "nb"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => nb"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,NbBief)
   champ2 => item(getElementsByTagname(champ1, "listeNoeuds"), 0)
   if(associated(champ2).eqv..false.) then
      print*,"Parse error => listeNoeuds"
      call xerror(Erreur)
      return
   endif
   champ3 => item(getElementsByTagname(champ2, "nb"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => nb"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,NbNoeud)
   champ2 => item(getElementsByTagname(champ1, "extrLibres"), 0)
   if(associated(champ2).eqv..false.) then
      print*,"Parse error => extrLibres"
      call xerror(Erreur)
      return
   endif
   champ3 => item(getElementsByTagname(champ2, "nb"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => nb"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,NbExtLibre)
   
   if(.not.associated(ExtDebBief)) allocate( ExtDebBief(NbBief) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ExtDebBief' )
      return
   end if

   if(.not.associated(ExtFinBief)) allocate( ExtFinBief(NbBief) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ExtFinBief' )
      return
   end if

   if(.not.associated(AbscRelExtDebBief)) allocate( AbscRelExtDebBief(NbBief) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'AbscRelExtDebBief' )
      return
   end if

   if(.not.associated(AbscRelExtFinBief)) allocate( AbscRelExtFinBief(NbBief) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'AbscRelExtFinBief' )
      return
   end if

   if(.not.associated(AbscAbsExtDebBief)) allocate( AbscAbsExtDebBief(NbBief) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'AbscAbsExtDebBief' )
      return
   end if

   if(.not.associated(AbscAbsExtFinBief)) allocate( AbscAbsExtFinBief(NbBief) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'AbscAbsExtFinBief' )
      return
   end if

   if(.not.associated(ExtNoeud)) allocate( ExtNoeud(5,NbNoeud) , STAT = retour )
   if (retour /= 0) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ExtNoeud' )
      return
   end if

   ExtNoeud(:,:) = 0

   if(.not.associated(NbExtNoeud)) allocate( NbExtNoeud(NbNoeud) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'ExtNoeud' )
      return
   end if

   if(.not.associated(Extremite)) allocate( Extremite(NbExtLibre) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'Extremite' )
      return
   end if

   if(.not.associated(NumExtLibre)) allocate( NumExtLibre(NbExtLibre) , STAT = retour )
   if( retour /= 0 ) then
      Erreur%Numero = 5
      Erreur%ft     = err_5
      Erreur%ft_c   = err_5c
      call TRAITER_ERREUR( Erreur , 'NumExtLibre' )
      return
   end if

   if( NbBief <= 0 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de biefs' )
      return
   end if

   ! Si le nombre de biefs est different du nombre de biefs
   ! lu dans le fichier geometrie
   if( NbBief /= size(ProfDebBief) ) then
      Erreur%Numero = 309
      Erreur%ft     = err_309
      Erreur%ft_c   = err_309c
      call TRAITER_ERREUR( Erreur , NbBief , size(ProfDebBief) )
      return
   end if

   if( ImpressionReseau ) then
      write(UlLst,10010) NbBief
   endif

   champ2 => item(getElementsByTagname(champ1, "listeBranches"), 0)
   if(associated(champ2).eqv..false.) then
      print*,"Parse error => listeBranches"
      call xerror(Erreur)
      return
   endif
   ! Numero des extremites de debut des branches
   champ3 => item(getElementsByTagname(champ2, "numExtremDebut"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => numExtremDebut"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,ExtDebBief)
   ! Numeros des extremites de fin des branches
   champ3 => item(getElementsByTagname(champ2, "numExtremFin"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => numExtremFin"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,ExtFinBief)
   ! Abscisse des extremites de debut des branches
   champ3 => item(getElementsByTagname(champ2, "abscDebut"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => abscDebut"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,AbscRelExtDebBief)
   ! Abscisse des extremites de fin des branches
   champ3 => item(getElementsByTagname(champ2, "abscFin"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => abscFin"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,AbscRelExtFinBief)
   
   do ibief = 1 , NbBief

      if( AbscRelExtDebBief(ibief) < (Profil(ProfDebBief(ibief))%AbsRel-EPS4) &
          .or. &
          AbscRelExtDebBief(ibief) > (Profil(ProfFinBief(ibief))%AbsRel+EPS4)) then
         Erreur%Numero   = 335
         Erreur%ft       = err_335
         Erreur%ft_c     = err_335c
         call TRAITER_ERREUR( Erreur , 'debut' , ibief )
         return
      end if
      ! Abscisse des extremites de fin des branches
      if( AbscRelExtFinBief(ibief) < (Profil(ProfDebBief(ibief))%AbsRel-EPS4) &
           .or. &
          AbscRelExtFinBief(ibief) > (Profil(ProfFinBief(ibief))%AbsRel+EPS4)) then
         Erreur%Numero   = 335
         Erreur%ft       = err_335
         Erreur%ft_c     = err_335c
         call TRAITER_ERREUR( Erreur , 'fin' , ibief )
         return
      end if

      if( AbscRelExtFinBief(ibief) <= AbscRelExtDebBief(ibief) ) then
         Erreur%Numero = 313
         Erreur%ft     = err_313
         Erreur%ft_c   = err_313c
         call TRAITER_ERREUR( Erreur , ibief )
         return
      end if

      if( ImpressionReseau ) then
         write(UlLst,10020) ibief, ExtDebBief(ibief) , ExtFinBief(ibief)
         write(UlLst,10030) AbscRelExtDebBief(ibief) , AbscRelExtFinBief(ibief)
      endif

      !--------------------------------------
      ! Transformation de l'abscisse relative
      ! en abscisse absolue
      !--------------------------------------
      AbscAbsExtDebBief(ibief) = ABS_ABS_S ( &
       ibief                    , &
       AbscRelExtDebBief(ibief) , &
       Profil                   , &
       ProfDebBief              , &
       ProfFinBief              , &
       Erreur                     &
                                )
      if( Erreur%Numero /= 0 ) then
         return
      end if

      AbscAbsExtFinBief(ibief) = ABS_ABS_S ( &
       ibief                    , &
       AbscRelExtFinBief(ibief) , &
       Profil                   , &
       ProfDebBief              , &
       ProfFinBief              , &
       Erreur                     &
                                )
     if( Erreur%Numero /= 0 ) then
        return
     end if

   end do

   ! Nombre de noeuds
   !-----------------
   if( NbNoeud < 0 .and. NbBief > 1 ) then
      Erreur%Numero = 306
      Erreur%ft     = err_306
      Erreur%ft_c   = err_306c
      call TRAITER_ERREUR( Erreur , 'Nombre de noeuds' )
      return
   end if

   if( ImpressionReseau ) then
      write(UlLst,10040) NbNoeud
   endif

   if( NbNoeud > 0 ) then
      ! Numeros des extremites des noeuds
      champ2 => item(getElementsByTagname(champ1, "listeNoeuds"), 0)
      if(associated(champ2).eqv..false.) then
         print*,"Parse error => listeNoeuds"
         call xerror(Erreur)
         return
      endif
      champ3 => item(getElementsByTagname(champ2, "noeuds"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => noeuds"
         call xerror(Erreur)
         return
      endif
      do inoeud = 1 , NbNoeud
         NbExtNoeud(inoeud) = 5
         do i = 1 , 5
            champ4 => item(getElementsByTagname(champ3, "noeud"), inoeud-1)
            if(associated(champ4).eqv..false.) then
               print*,"Parse error => noeud"
               call xerror(Erreur)
               return
            endif
            champ5 => item(getElementsByTagname(champ4, "num"), 0)
            if(associated(champ5).eqv..false.) then
               print*,"Parse error => num"
               call xerror(Erreur)
               return
            endif
            call extractDataContent(champ5,ExtNoeud(:,inoeud))
            
            if( ExtNoeud(i,inoeud) < 0 ) then
               Erreur%Numero = 351
               Erreur%ft     = err_351
               Erreur%ft_c   = err_351c
               call TRAITER_ERREUR( Erreur , i , inoeud )
               return
            end if
            if( ExtNoeud(i,inoeud) == 0 ) then
               NbExtNoeud(inoeud) = i-1
               exit
            endif
         end do

         if( ImpressionReseau ) then
            write(UlLst,10050) inoeud , ( ExtNoeud(i,inoeud) , i = 1 , 5 )
         endif
      end do

      !
      !  controle sur l'ordre croissant des biefs
      !
      if( Noyau == 3 ) then
         do inoeud = 1 , NbNoeud
            do i = 1 , 3
               if( i-1 > 0 ) then
                  If( ExtNoeud (i,inoeud) < ExtNoeud(i-1,inoeud) ) then
                     if (ULLST >0) then
                        write (ULLST,*)  '==============================================================================='
                        write (ULLST,*) 'ATTENTION les extremites du noeud',Inoeud, 'ne sont pas rangees par ordre croissant'
                        Write (ULLST,*)  'Verifier la coherence de numerotation des affluents'
                        Write (ULLST,*)  '==============================================================================='
                     endif
                     !STOP
                     Erreur%Numero  = 2
                     Erreur%Message = 'Error : nodes are not sorted in increasing order (subroutine LEC_RESEAU)'
                     return
                  endif
                  if( i - 2 > 0 ) then
                     if( ExtNoeud(i-1,inoeud) < ExtNoeud(i-2,inoeud) ) then
                        if (ULLST >0) then
                           write (ULLST,*) 'ATTENTION les extremites du noeud',I, 'ne sont pas rangees par ordre croissant'
                           Write (ULLST,*)  'Verifier la coherence de numerotation des affluents'
                        end if
                        !STOP
                        Erreur%Numero  = 2
                        Erreur%Message = 'Error : nodes are not sorted in increasing order (subroutine LEC_RESEAU)'
                        return
                     endif
                  endif
               endif
            end do
            if( ImpressionReseau ) then
               write(UlLst,10050) inoeud , (ExtNoeud(i,inoeud),i=1,5)
            endif
         end do
      end if
   endif

   ! Nombre d'extremites libres
   if( NbExtLibre < 2 ) then
      Erreur%Numero = 305
      Erreur%ft   = err_305
      Erreur%ft_c = err_305c
      call TRAITER_ERREUR  (Erreur, 'Nombre d''extremites libre', 'superieurs ou egaux a 2')
      return
   end if

   if( ImpressionReseau ) then
      write(UlLst,10060) NbExtLibre
   endif

   ! Numeros des extremites libres
   champ2 => item(getElementsByTagname(champ1, "extrLibres"), 0)
   if(associated(champ2).eqv..false.) then
      print*,"Parse error => extrLibres"
      call xerror(Erreur)
      return
   endif
   champ3 => item(getElementsByTagname(champ2, "numExtrem"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => numExtrem"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,NumExtLibre)
   
   ! Types des extremites libres
   champ3 => item(getElementsByTagname(champ2, "typeCond"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => typeCond"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3,Extremite%Type)
   
   ! Numeros des lois des extremites libres ! ne sert pas pour Connect
   champ3 => item(getElementsByTagname(champ2, "numLoi"), 0)
   if(associated(champ3).eqv..false.) then
      print*,"Parse error => numLoi"
      call xerror(Erreur)
      return
   endif
   call extractDataContent(champ3, Extremite%NumeroLoi)
   
   do iext = 1 , NbExtLibre
      ! Numeros des extremites libres
      if( NumExtLibre(iext) < 0 ) then
         Erreur%Numero = 315
         Erreur%ft     = err_315
         Erreur%ft_c   = err_315c
         call TRAITER_ERREUR( Erreur , 'numeros d''extremite libre' , iext )
         return
      end if

      ! Noms des extremites libres
      champ3 => item(getElementsByTagname(champ2, "noms"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => noms"
         call xerror(Erreur)
         return
      endif
      champ4 => item(getElementsByTagname(champ3, "string"), iext-1)
      if(associated(champ4).eqv..false.) then
         print*,"Parse error => string"
         call xerror(Erreur)
         return
      endif
      Extremite(iext)%Nom = getTextContent(champ4)
      
      ! Types des extremites libres
      if( Extremite(iext)%Type < 1 .or. Extremite(iext)%Type > CONDITION_TYPE_NB_MAX ) then
         Erreur%Numero = 331
         Erreur%ft     = err_331
         Erreur%ft_c   = err_331c
         call TRAITER_ERREUR( Erreur , iext , CONDITION_TYPE_NB_MAX )
         return
      end if

      ! Controle des conditions aval en permanent
      if( Noyau == NOYAU_SARAP ) then
         do ibief = 1 , NbBief
            if( ExtFinBief(ibief) == NumExtLibre(iext) .and. &
                Extremite(iext)%Type /= CONDITION_TYPE_COTE_IMPOSE) then
               Erreur%Numero = 385
               Erreur%ft     = err_385
               Erreur%ft_c   = err_385c
               call TRAITER_ERREUR( Erreur , iext , "cote imposee" )
               return
            endif
            if( ExtDebBief(ibief) == NumExtLibre(iext) .and. &
                Extremite(iext)%Type /= CONDITION_TYPE_DEBIT_IMPOSE ) then
               Erreur%Numero = 385
               Erreur%ft     = err_385
               Erreur%ft_c   = err_385c
               call TRAITER_ERREUR( Erreur , iext , "debit imposee" )
               return
            endif
         end do
      endif

      ! Numeros des lois des extremites libres ! ne sert pas pour Connect
      num_loi = Extremite(iext)%NumeroLoi
      if( num_loi <= 0.and.Extremite(iext)%Type /= CONDITION_TYPE_SORTIE_LIBRE ) then
         Erreur%Numero = 315
         Erreur%ft     = err_315
         Erreur%ft_c   = err_315c
         call TRAITER_ERREUR( Erreur , 'numero des loi d''extremite libre' , iext )
         return
      end if

      ! Controle de coherence Type d'extremite / type de loi
      select case( Extremite(iext)%Type )

         case( CONDITION_TYPE_DEBIT_IMPOSE )

            if(LoiHydrau(Extremite(iext)%NumeroLoi)%Type /= LOI_TYPE_HYDROGRAMME .and. &
               LoiHydrau(Extremite(iext)%NumeroLoi)%Type /= LOI_TYPE_LIMNHYDROGRAMME) then
               Erreur%Numero = 358
               Erreur%ft   = err_358
               Erreur%ft_c = err_358c
               call TRAITER_ERREUR( Erreur , Extremite(iext)%NumeroLoi ,        &
                                    LoiHydrau(Extremite(iext)%NumeroLoi)%Type , &
                                    iext ,                                      &
                                    Extremite(iext)%Type )
               return
            endif

         case( CONDITION_TYPE_COTE_DEBIT_IMPOSES )

            if( LoiHydrau(Extremite(iext)%NumeroLoi)%Type /= LOI_TYPE_LIMNHYDROGRAMME ) then
               Erreur%Numero = 358
               Erreur%ft     = err_358
               Erreur%ft_c   = err_358c
               call TRAITER_ERREUR( Erreur, Extremite(iext)%NumeroLoi,              &
                                         LoiHydrau(Extremite(iext)%NumeroLoi)%Type, &
                                         iext,                                      &
                                         Extremite(iext)%Type )
               return
            endif

         case( CONDITION_TYPE_COTE_IMPOSE )

            if( LoiHydrau(Extremite(iext)%NumeroLoi)%Type /= LOI_TYPE_LIMNIGRAMME .and. &
                LoiHydrau(Extremite(iext)%NumeroLoi)%Type /= LOI_TYPE_LIMNHYDROGRAMME ) then
               Erreur%Numero = 358
               Erreur%ft     = err_358
               Erreur%ft_c   = err_358c
               call TRAITER_ERREUR( Erreur, Extremite(iext)%NumeroLoi,               &
                                          LoiHydrau(Extremite(iext)%NumeroLoi)%Type, &
                                          iext,                                      &
                                          Extremite(iext)%Type)
               return
            endif

         case( CONDITION_TYPE_COTE_DEBIT )

            if( LoiHydrau(Extremite(iext)%NumeroLoi)%Type /= LOI_TYPE_TARAGE_Z_Q ) then
               Erreur%Numero = 358
               Erreur%ft     = err_358
               Erreur%ft_c   = err_358c
               call TRAITER_ERREUR( Erreur, Extremite(iext)%NumeroLoi,              &
                                         LoiHydrau(Extremite(iext)%NumeroLoi)%Type, &
                                         iext,                                      &
                                         Extremite(iext)%Type )
               return
            endif

         case( CONDITION_TYPE_DEBIT_COTE )

            if( LoiHydrau(Extremite(iext)%NumeroLoi)%Type /= LOI_TYPE_TARAGE_Q_Z ) then
               Erreur%Numero = 358
               Erreur%ft     = err_358
               Erreur%ft_c   = err_358c
               call TRAITER_ERREUR( Erreur, Extremite(iext)%NumeroLoi,              &
                                         LoiHydrau(Extremite(iext)%NumeroLoi)%Type, &
                                         iext,                                      &
                                         Extremite(iext)%Type )
               return
            endif

          case( CONDITION_TYPE_ZAVAL_QAMONT )

             if( LoiHydrau(Extremite(iext)%NumeroLoi)%Type /= LOI_TYPE_TARAGE_Z_Q ) then
                Erreur%Numero = 358
                Erreur%ft   = err_358
                Erreur%ft_c = err_358c
                call TRAITER_ERREUR(Erreur, Extremite(iext)%NumeroLoi,              &
                                         LoiHydrau(Extremite(iext)%NumeroLoi)%Type, &
                                         iext,                                      &
                                         Extremite(iext)%Type)
                return
             endif

         case default  ! (CONDITION_TYPE_SORTIE_LIBRE, CONDITION_TYPE_NORMALE)

      end select

      !-----------------------------------
      ! Allocation des tableaux PtZ et PtQ
      !-----------------------------------
      if( Extremite(iext)%Type == CONDITION_TYPE_DEBIT_IMPOSE .or. &
          Extremite(iext)%Type == CONDITION_TYPE_SORTIE_LIBRE .or. &
          Extremite(iext)%Type == CONDITION_TYPE_COTE_IMPOSE.or.&
          Extremite(iext)%Type == CONDITION_TYPE_COTE_DEBIT_IMPOSES &
        ) then

         if(.not.associated(Extremite(iext)%PtZ)) allocate( Extremite(iext)%PtZ(1) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Extremite%PtZ' )
            return
         end if

         if(.not.associated(Extremite(iext)%PtQ)) allocate( Extremite(iext)%PtQ(1) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Extremite%PtQ' )
            return
         end if

      elseif( Extremite(iext)%Type == CONDITION_TYPE_COTE_DEBIT   .or.    &
              Extremite(iext)%Type == CONDITION_TYPE_ZAVAL_QAMONT .or.    &
              Extremite(iext)%Type == CONDITION_TYPE_DEBIT_COTE) then

         dim = size(LoiHydrau(num_loi)%Cote)

         if(.not.associated(Extremite(iext)%PtZ)) allocate( Extremite(iext)%PtZ(dim) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5c
            call TRAITER_ERREUR( Erreur , 'Extremite/PtZ' )
            return
         end if

         if(.not.associated(Extremite(iext)%PtQ)) allocate( Extremite(iext)%PtQ(dim) , STAT = retour )
         if( retour /= 0 ) then
            Erreur%Numero = 5
            Erreur%ft     = err_5
            Erreur%ft_c   = err_5
            call TRAITER_ERREUR( Erreur , 'Extremite/PtQ' )
            return
         end if

         ! Affectation dans le cas de lois constantes dans le temps
         !---------------------------------------------------------
         Extremite(iext)%PtZ(:) = LoiHydrau(num_loi)%Cote(:)
         Extremite(iext)%PtQ(:) = LoiHydrau(num_loi)%Debit(:)

      endif

      if( ImpressionReseau ) then
         write(UlLst,10070) iext , NumExtLibre(iext) , Extremite(iext)%Type , Extremite(iext)%NumeroLoi
      endif

   end do

   test_loi = .FALSE.

   do iext = 1 , NbExtLibre
      if( ( Extremite(iext)%NumeroLoi == 0)       &
         .and.(Extremite(iext)%Type /= CONDITION_TYPE_SORTIE_LIBRE)) then
         test_loi = .FALSE.
      else
         test_loi = .TRUE.
      end if
   end do

   if( test_loi .eqv. .FALSE. ) then
      Erreur%Numero = 386
      Erreur%ft     = err_386
      Erreur%ft_c   = err_386c
      call TRAITER_ERREUR( Erreur , 'extremites libres' )
      return
   end if

   !Erreur%arbredappel = !arbredappel_old
   return

   ! Formats
   10000 format (/,'RESEAU ET EXTREMITES LIBRES',/, &
                &  '---------------------------',/)
   10010 format ('Nombre de branches           : ',i3,/)
   10020 format ('Branche ',i3,' Numero   extremite debut= ',i12,' fin= ',i12)
   10030 format ('            Abscisse extremite debut= ',f12.3,' fin= ',f12.3)
   10040 format (/,'Nombre de noeuds           : ',i3,/)
   10050 format ('Noeud ',i3,' Lie les extremites ',5i3)
   10060 format (/,'Nombre d''extremites libres : ',i3,/)
   10070 format (i3,' Numero d''extremite = ',i3,' Type de condition = ',i3,' Numero de loi =',i3)

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
   
end subroutine LEC_RESEAU
