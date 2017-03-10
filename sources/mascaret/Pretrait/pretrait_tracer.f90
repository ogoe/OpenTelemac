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

subroutine PRETRAIT_Tracer( &
            FichierMotCle , & ! Fichier des mots-cles
                    Noyau , & ! Noyau de calcul hydraulique
            Type_maillage , & ! Choix du type de maillage longitudinal
                  Connect , & ! Table de connectivite
                   Apport , & ! Apports hydrauliques
                   Profil , & ! Profils geometriques
                        X , & ! Abscisses des sections de calcul
                Extremite , & ! Extremites libres
             TempsMaximum , & ! Temps maximum du calcul
                            ! Lecture des parametres de Tracer
             OptionTracer , & ! Choix d'un calcul avec TRACER
                 Ctraceur , & ! Concentrations en traceurs
                   Nbtrac , & ! Nombre de traceurs
                 ConsTrac , & ! Constantes pour TRACER
             FreqCouplage , & ! Frequence de couplage hydraulique/tracer
                            ! Conc init, CL, sources, lois tracer
           FichierConcIni , & ! Fichier des conc ini
                 Cond_Lim , & ! Conditions aux limites
           Sources_tracer , & ! Sources pour le traceur
                LoiTracer , & ! Lois Tracer (CL ou sources)
         FichierLoiTracer , & ! Fichier loi Tracer
                            ! Lecture des parametres de QE
          Modele_Qual_Eau , & ! Modele de QE
                   ParPhy , & ! Parametres de modele de QE
                    Meteo , & ! Donnees meteo
           Fichier_Parphy , & ! Fichier des parametres de QE
            Fichier_Meteo , & ! Fichier meteo
                            ! Impression des parametres et resultats
        FichierResuTracer , & ! Fichier resultats
         FormatResuTracer , & 
     FichierListingTracer , & ! Fichier listing
    ImpressionConcListing , & ! Logique pour les impressions
    ImpressionBilanTracer , & ! Logique pour les impressions
              PasStockage , & ! Pas de stockage  (hydraulique)
            PasImpression , & ! Pas d'impression (hydraulique)
                            ! Traitement des erreurs
                   Erreur )

!*****************************************************************************
! PROGICIEL : TRACER         S.MANDELKERN - M. LUCK
!                            F. ZAOUI                   
!
! VERSION : 8.1.3              EDF-CEREMA
!*****************************************************************************
!
!  FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES
!             POUR LE MODULE TRACER
! ----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELANT : - SUPER_TRACER
!
!*****************************************************************************

   !=========================== Declarations ================================
   use M_PRECISION                 ! Definition de la precision DOUBLE ou SIMPLE
   use M_CONSTANTES_CALCUL_C       ! Constantes num, phys et info
   use M_PARAMETRE_C
   use M_FICHIER_T
   use M_CONNECT_T                 ! Type CONNECT_T : connectivite du reseau
   use M_APPORT_T                  ! Definition du type APPORT_T
   use M_PROFIL_T                  ! Definition du type PROFIL_T
   use M_EXTREMITE_T               ! Definition du type EXTREMITE_T
   use M_TRAITER_ERREUR_I          ! Traitement de l'errreur
   use M_PARAMETRES_QUALITE_EAU_T  ! Donnees physiques du traceur 
   use M_METEO_T                   ! Donnees Meteo
   use M_SOURCE_TRACER_T           ! Sources de traceurs
   use M_LOI_TRACER_T              ! Lois tracer
   use M_CONSTANTES_TRACER_T
   use M_COND_LIM_TRACER_T         ! Conditions aux limites pour les traceurs
   use M_CONSTANTES_CALCUL_TRACER_C
   use M_LEC_CONC_INI_TRACER_I
   use M_LEC_LOI_TRACER_I
   use M_LEC_SOURCE_I
   use M_ERREUR_T                  ! Traitement des erreurs
   use M_MESSAGE_C
   use M_MESSAGE_TRACER_C
   use Fox_dom                 ! parser XML Fortran

   !.. Implicit Declarations .. 
   implicit none

   !.. Gestion des mots-cles .. 
   type(FICHIER_T), intent(inout) :: FichierMotCle
   !.. Variables d'entree (maillage et hydraulique) ..
   integer , intent(in   ) :: Noyau        ! Noyau de calcul hydraulique
   integer , intent(in   ) :: Type_maillage
   type(CONNECT_T)               , intent(in   ) :: Connect
   type(APPORT_T)   ,dimension(:), intent(in   ) :: Apport
   type(PROFIL_T)   ,dimension(:), pointer       :: Profil       ! Profils geometriques
   real(DOUBLE)     ,dimension(:), pointer       :: X            ! Maillage
   Type(EXTREMITE_T),dimension(:), pointer       :: Extremite
   real(DOUBLE)                  , intent(in   ) :: TempsMaximum
   !.. Traceurs .. 
   !
   logical                       , intent(  out) :: OptionTracer
   integer                                       :: Nbtrac
   integer                       , intent(  out) :: Modele_Qual_Eau
   integer                       , intent(  out) :: FreqCouplage
   real(DOUBLE)             , dimension(:,:), pointer :: Ctraceur
   type(CONSTANTES_TRACER_T), dimension(:)  , pointer :: ConsTrac
   type(COND_LIM_TRACER_T)  , dimension(:)  , pointer :: Cond_Lim
   type(SOURCE_TRACER_T)    , dimension(:)  , pointer :: Sources_tracer
   type(LOI_TRACER_T)       , dimension(:)  , pointer :: LoiTracer
   type(PARAMETRES_QUALITE_EAU_T)                     :: ParPhy
   type(METEO_T)                                      :: Meteo
   type(FICHIER_T)           , intent(inout)   :: FichierResuTracer
   type(FICHIER_T)           , intent(inout)   :: FichierListingTracer
   type(FICHIER_T)           , intent(inout)   :: FichierConcIni
   type(FICHIER_T)           , intent(inout)   :: FichierLoiTracer
   type(FICHIER_T)           , intent(inout)   :: Fichier_Meteo
   type(FICHIER_T)           , intent(inout)   :: Fichier_Parphy
   logical                   , intent(  out)   :: ImpressionConcListing
   logical                   , intent(  out)   :: ImpressionBilanTracer
   integer                   , intent(  out)   :: FormatResuTracer
   integer                   , intent(in   )   :: PasStockage
   integer                   , intent(in   )   :: PasImpression
   !.. Traitement des erreurs ..
   type(ERREUR_T)            , intent(inout)   :: Erreur
   ! VARIABLES LOCALES
   ! ----------------- 
   integer                                       :: post_processeur_tracer
   integer                       , parameter     :: POST_RUBENS  = 1
   integer                       , parameter     :: POST_OPTHYCA = 2
   logical  :: Presence_ConcIni
   logical  :: ImpressionConcIni, ImpressionLoiTracer
   character(132)     :: arbredappel_old
   integer  :: NbBief, NbExtLibre, ult
   integer  i, ib, k, retour

   ! FoX XML
   !--------
   type(Node), pointer :: document,element,champ1,champ2,champ3
   type(DOMconfiguration), pointer :: config
   type(DOMException) :: ex
   integer :: ios
   !
   logical, allocatable :: ltab1(:),ltab2(:)
   integer, allocatable :: itab1(:),itab2(:)
   
   !=========================================================================
   ! INITIALISATION
   !=========================================================================
   Erreur%Numero = 0
   !arbredappel_old = trim(Erreur%arbredappel)
   !Erreur%arbredappel = trim(Erreur%arbredappel)//'=>PRETRAIT_TRACER'

   ! FoX : initialisation
   !---------------------
   config => newDOMConfig()
   call setParameter(config, "xml-declaration", .false.)
   call setParameter(config, "validate-if-schema", .true.)
   document => parseFile(FichierMotCle%Nom,config,iostat=ios,ex=ex)
   if (inException(ex).or.ios.ne.0) then
       print*,"Parse error", getExceptionCode(ex)
       Erreur%Numero = 704
       Erreur%ft     = err_704
       Erreur%ft_c   = err_704c
       call TRAITER_ERREUR( Erreur )
       return
   endif
   element => getDocumentElement(document)
   !print *, 'element principal = ',getLocalName(element)        
           
   ! Couplage avec la qualite d'eau ? (implique 1 couplage Hydraulique / Convection Diffusion)
   champ1 => item(getElementsByTagname(document, "parametresTraceur"), 0)
   if(associated(champ1).eqv..false.) then
       OptionTracer = .false.
   else
     champ2 => item(getElementsByTagname(champ1, "presenceTraceurs"), 0)
     if(associated(champ1).eqv..false.) then
        print*,"Parse error => presenceTraceurs"
        call xerror(Erreur)
        return
     endif
     call extractDataContent(champ2,OptionTracer)
   endif
   PretraitTracer : if (OptionTracer) then
      !=======================================================================
      !           LECTURE DES DONNEES PROPRES A TRACER
      !=======================================================================
      !
      ! Fichier listing et de resultats
      ! -------------------------------
      champ2 => item(getElementsByTagname(champ1, "parametresImpressionTraceur"), 0)
      if(associated(champ2).eqv..false.) then
         print*,"Parse error => parametresImpressionTraceur"
         call xerror(Erreur)
         return
      endif
      champ3 => item(getElementsByTagname(champ2, "fichListTracer"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => fichListTracer"
         call xerror(Erreur)
         return
      endif
      FichierListingTracer%Nom = getTextContent(champ3)
      ult                      = FichierListingTracer%Unite
      champ3 => item(getElementsByTagname(champ2, "concentInit"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => concentInit"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,ImpressionConcIni)
      champ3 => item(getElementsByTagname(champ2, "loiTracer"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => loiTracer"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,ImpressionLoiTracer)
      champ3 => item(getElementsByTagname(champ2, "concentrations"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => concentrations"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,ImpressionConcListing)
      champ3 => item(getElementsByTagname(champ2, "bilanTracer"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => bilanTracer"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,ImpressionBilanTracer)
      
      open( unit = ult , file = FichierListingTracer%Nom , access = 'SEQUENTIAL' , &
            action = 'WRITE' , form = 'FORMATTED' , iostat = RETOUR , &
            position = 'rewind' , status = 'REPLACE' )
      if( RETOUR /= 0 ) then
         Erreur%Numero = 4
         Erreur%ft     = err_4
         Erreur%ft_c   = err_4c
         call TRAITER_ERREUR( Erreur , FichierListingTracer%Nom )
         return
      end if

      champ3 => item(getElementsByTagname(champ2, "fichResultTracer"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => fichResultTracer"
         call xerror(Erreur)
         return
      endif
      FichierResuTracer%Nom  = getTextContent(champ3)
      champ3 => item(getElementsByTagname(champ2, "formatFichResultat"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => formatFichResultat"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,post_processeur_tracer)

      if( ( post_processeur_tracer /= POST_RUBENS ).and.( post_processeur_tracer /= POST_OPTHYCA ) ) then
         Erreur%Numero = 512
         Erreur%ft     = err_512
         Erreur%ft_c   = err_512c
         call TRAITER_ERREUR( Erreur , post_processeur_tracer )
         return
      endif

      if( post_processeur_tracer == POST_RUBENS ) then
         FormatResuTracer = FORMAT_STO_NONPERMANENT
      else if( post_processeur_tracer == POST_OPTHYCA ) then
         FormatResuTracer = FORMAT_STO_OPTHYCA
      endif

      write(ult,10630)
      !
      ! Nombre de traceurs
      ! ------------------  
      champ2 => item(getElementsByTagname(champ1, "nbTraceur"), 0)
      if(associated(champ2).eqv..false.) then
         print*,"Parse error => nbTraceur"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ2,Nbtrac)
      write(ult,10640) Nbtrac
      if( NbTrac > 10 ) Then
         Erreur%Numero = 508
         Erreur%ft     = err_508
         Erreur%ft_c   = err_508c
         call TRAITER_ERREUR( Erreur , 'Nombre de traceurs' , '< ou egal a 10' )
         return
      endif
      !
      ! Modele de qualite d'eau
      ! -----------------------
      !... Type de modele
      champ2 => item(getElementsByTagname(champ1, "parametresNumeriquesQualiteEau"), 0)
      if(associated(champ2).eqv..false.) then
         print*,"Parse error => parametresNumeriquesQualiteEau"
         call xerror(Erreur)
         return
      endif
      champ3 => item(getElementsByTagname(champ2, "modeleQualiteEau"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => modeleQualiteEau"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,Modele_Qual_Eau)
      write(ult,10650) Modele_Qual_Eau
      if( Modele_Qual_Eau > 6 ) Then
         Erreur%Numero = 508
         Erreur%ft   = err_508
         Erreur%ft_c = err_508c
         call TRAITER_ERREUR  (Erreur, 'Modele de QE', '< ou egal a 6')
         return
      endif

      if( Modele_Qual_Eau.EQ.2 ) then  ! module O2
         Nbtrac = 3
      elseif( Modele_Qual_Eau.EQ.3 ) then  ! module BIOMASS
         Nbtrac = 5
      elseif( Modele_Qual_Eau.EQ.4 ) then  ! module EUTRO
         Nbtrac = 8
      elseif( Modele_Qual_Eau.EQ.5 ) then  ! module MICROPOL
         Nbtrac = 5
      elseif (Modele_Qual_Eau.EQ.6) then  ! module THERMIC
         Nbtrac = 1
      else
         Nbtrac = Nbtrac
      endif
      !
      !... Parametres physiques dependant du modele
      !
      if( Modele_Qual_Eau.NE.1 ) then
         champ3 => item(getElementsByTagname(champ2, "fichParamPhysiqueTracer"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => fichParamPhysiqueTracer"
            call xerror(Erreur)
            return
         endif
         Fichier_Parphy%nom = getTextContent(champ3)
         call LEC_PARPHY( Fichier_ParPhy , ParPhy , Erreur )
      endif
      !
      !... Donnees meteo (pour les modeles biomass, eutro et thermic)
      !
      if( ( ( Modele_Qual_Eau.EQ.3 )   &
         .OR.(Modele_Qual_Eau.EQ.4) ) &
         .OR.(Modele_Qual_Eau.EQ.6)   ) then
         champ3 => item(getElementsByTagname(champ2, "fichMeteoTracer"), 0)
         if(associated(champ3).eqv..false.) then
            Fichier_Meteo%nom = ""
         else    
            Fichier_Meteo%nom = getTextContent(champ3)
         endif
         call LEC_METEO( Meteo , Fichier_Meteo , Modele_Qual_Eau , Erreur )
      endif
      !
      ! Parametres pour la convection / diffusion
      ! -----------------------------------------
      allocate( Constrac(nbtrac) , STAT = retour )

      allocate( ltab1(nbtrac) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'ltab1' )
          return
      end if
      allocate( ltab2(nbtrac) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'ltab2' )
          return
      end if
      
      champ2 => item(getElementsByTagname(champ1, "parametresConvectionDiffusion"), 0)
      if(associated(champ2).eqv..false.) then
         print*,"Parse error => parametresConvectionDiffusion"
         call xerror(Erreur)
         return
      endif
      champ3 => item(getElementsByTagname(champ2, "convectionTraceurs"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => convectionTraceurs"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,ltab1)
      champ3 => item(getElementsByTagname(champ2, "diffusionTraceurs"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => diffusionTraceurs"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,ltab2)
      
      do ib = 1 , nbtrac
         Constrac(ib)%CONV             = ltab1(ib)
         champ3 => item(getElementsByTagname(champ2, "optionConvection"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => optionConvection"
            call xerror(Erreur)
            return
         endif
         call extractDataContent(champ3,ConsTrac(ib)%Scheconv)
         ConsTrac(ib)%DIFF             = ltab2(ib)
         champ3 => item(getElementsByTagname(champ2, "optionCalculDiffusion"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => optionCalculDiffusion"
            call xerror(Erreur)
            return
         endif
         call extractDataContent(champ3,ConsTrac(ib)%OptionCalculDisp)
         champ3 => item(getElementsByTagname(champ2, "coeffDiffusion1"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => coeffDiffusion1"
            call xerror(Erreur)
            return
         endif
         call extractDataContent(champ3,ConsTrac(ib)%CoefDiffu(1))
         champ3 => item(getElementsByTagname(champ2, "coeffDiffusion2"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => coeffDiffusion2"
            call xerror(Erreur)
            return
         endif
         call extractDataContent(champ3,ConsTrac(ib)%CoefDiffu(2))
         champ3 => item(getElementsByTagname(champ2, "ordreSchemaConvec"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => ordreSchemaConvec"
            call xerror(Erreur)
            return
         endif
         call extractDataContent(champ3,ConsTrac(ib)%OrdreVF)
         champ3 => item(getElementsByTagname(champ2, "paramW"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => paramW"
            call xerror(Erreur)
            return
         endif
         call extractDataContent(champ3,ConsTrac(ib)%ParamW)
         champ3 => item(getElementsByTagname(champ2, "LimitPente"), 0)
         if(associated(champ3).eqv..false.) then
            print*,"Parse error => LimitPente"
            call xerror(Erreur)
            return
         endif
         call extractDataContent(champ3,ConsTrac(ib)%LimiteurPente)
      end do

      if( ConsTrac(1)%Scheconv > 4 ) Then
         Erreur%Numero = 508
         Erreur%ft     = err_508
         Erreur%ft_c   = err_508c
         call TRAITER_ERREUR( Erreur , 'Schema de convection' , '< ou egal a 4' )
         return
      endif

      if( ConsTrac(1)%OrdreVF > 3 ) Then
         Erreur%Numero = 508
         Erreur%ft   = err_508
         Erreur%ft_c = err_508c
         call TRAITER_ERREUR  (Erreur, 'Ordre du schema VF', '< ou egal a 3')
         return
      endif

      if( ( ConsTrac(1)%ParamW < -1 ).or.( ConsTrac(1)%ParamW > 1 ) ) Then
         Erreur%Numero = 508
         Erreur%ft   = err_508
         Erreur%ft_c = err_508c
         call TRAITER_ERREUR  (Erreur, 'Parametre W', 'compris entre -1 et 1')
         return
      endif

      !   Cas particulier du modele MICROPOL :
      !   Traceurs relatifs aux sediments ni convectes ni diffuses
      if( Modele_Qual_Eau.EQ.5 ) then
         Constrac(2)%CONV             = .false.
         ConsTrac(2)%OptionCalculDisp = 0
         Constrac(5)%CONV             = .false.
         ConsTrac(5)%OptionCalculDisp = 0
      endif
      !
      ! Frequence de couplage entre hydraulique et TRACER
      ! -------------------------------------------------
      champ2 => item(getElementsByTagname(champ1, "parametresNumeriquesQualiteEau"), 0)
      if(associated(champ2).eqv..false.) then
         print*,"Parse error => parametresNumeriquesQualiteEau"
         call xerror(Erreur)
         return
      endif
      champ3 => item(getElementsByTagname(champ2, "frequenceCouplHydroTracer"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => frequenceCouplHydroTracer"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,FreqCouplage)

      if( MOD(PasStockage, FreqCouplage) > EPS8 ) then
         Erreur%Numero = 506
         Erreur%ft     = err_506
         Erreur%ft_c   = err_506c
         call TRAITER_ERREUR( Erreur , 'Pas de stockage' )
         return
      end if

      if( MOD(PasImpression, FreqCouplage) > EPS8 ) then
         Erreur%Numero = 506
         Erreur%ft     = err_506
         Erreur%ft_c   = err_506c
         call TRAITER_ERREUR( Erreur , 'Pas d impression' )
         return
      end if

      write(ult,10655) FreqCouplage

      ! Concentrations initiales 
      ! ------------------------
      ! presence de concentrations initiales
      !
      if( ImpressionConcIni ) write(ult,10660)
      champ2 => item(getElementsByTagname(champ1, "parametresConcentrationsInitialesTraceur"), 0)
      if(associated(champ2).eqv..false.) then
         print*,"Parse error => parametresConcentrationsInitialesTraceur"
         call xerror(Erreur)
         return
      endif
      champ3 => item(getElementsByTagname(champ2, "presenceConcInit"), 0)
      if(associated(champ3).eqv..false.) then
            print*,"Parse error => presenceConcInit"
            call xerror(Erreur)
            return
         endif
      call extractDataContent(champ3,Presence_ConcIni)
      if( ImpressionConcIni ) then
         if( Presence_ConcIni ) then
            write(ult,10690) 'OUI'
         else
            write(ult,10690) 'NON'
         endif
      endif

      if( Presence_ConcIni ) then
         call LEC_CONC_INI_TRACER( &
                             ult , &
               ImpressionConcIni , &
                  FichierConcIni , &
                               X , & ! Maillage
                   Type_maillage , & ! Mode de calcul du maillage
                        Ctraceur , & ! Concentrations initiales des traceurs
                          nbtrac , & ! nombre de traceurs
                         Connect , & ! Table de connectivite du reseau
                          Profil , & ! Profils geometriques
                        document , & ! Pointeur vers document XML
                          Erreur   ) ! Erreur
         if( Erreur%Numero /= 0 ) then
            return
         endif
      else
         !   s'il n'y a pas de conc. init.,
         !   il faut allouer et initialiser les conc. a 0
         Allocate( Ctraceur(size(X(:)),Nbtrac) , STAT = RETOUR )
         Do k = 1 , nbtrac
            Ctraceur(:,k) = W0    ! W0 = 0._DOUBLE
         end do
      endif ! de if Presence_ConcIni
      !
      ! Lecture des lois
      ! ----------------
      call LEC_LOI_TRACER( &
        LoiTracer           , & ! Tableau des lois tracer
        nbtrac              , & ! Nombre de traceurs
        FichierLoiTracer    , & ! Fichier des lois tracer
        ImpressionLoiTracer , & ! Flag d'impression des lois tracer
        ult                 , & ! Unite logique fichier listing
        TempsMaximum        , & ! Temps maximum du calcul
        document            , & ! Pointeur vers document XML        
        Erreur                )
      if( Erreur%Numero /= 0 ) then
         return
      endif
      !
      ! Sources de traceurs
      ! -------------------
      call LEC_SOURCE( &
      Sources_Tracer , & ! Tableau des sources tracer
              Apport , & ! Tableau des apports hydrauliques
             Connect , & ! Table de connectivite du reseau
                   X , & ! Maillage
           LoiTracer , & ! Lois Tracer
              Profil , & ! Profils geometriques
              nbtrac , & ! nb de traceurs
                 ult , & ! Unite listing
            document , & ! Pointeur vers document XML              
              Erreur   & ! Erreur
                      )
      if( Erreur%Numero /= 0 ) then
         return
      endif
      !
      ! Conditions aux limites
      ! ----------------------
      !
      NbExtLibre = size(Extremite)
      allocate( Cond_Lim(NbExtLibre) , STAT = RETOUR )

      allocate( itab1(NbExtLibre) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab1' )
          return
      end if
      allocate( itab2(NbExtLibre) , STAT = retour )
      if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'itab2' )
          return
      end if
      
      champ2 => item(getElementsByTagname(champ1, "parametresConditionsLimitesTraceur"), 0)
      if(associated(champ2).eqv..false.) then
         print*,"Parse error => parametresConditionsLimitesTraceur"
         call xerror(Erreur)
         return
      endif
      champ3 => item(getElementsByTagname(champ2, "typeCondLimTracer"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => typeCondLimTracer"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,itab1)
      champ3 => item(getElementsByTagname(champ2, "numLoiCondLimTracer"), 0)
      if(associated(champ3).eqv..false.) then
         print*,"Parse error => numLoiCondLimTracer"
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,itab2)
      
      do i = 1 , NbExtlibre

         Cond_Lim(i)%Type       = itab1(i) 
         Cond_Lim(i)%NumeroLoi  = itab2(i)

         if( Cond_Lim(i)%NumeroLoi <= 0 ) then
            Erreur%Numero = 585
            Erreur%ft     = err_585
            Erreur%ft_c   = err_585c
            call TRAITER_ERREUR( Erreur , i )
            return
         endif

         if( Cond_Lim(i)%NumeroLoi > size(LoiTracer) ) then
            Erreur%Numero = 586
            Erreur%ft   = err_586
            Erreur%ft_c = err_586c
            call TRAITER_ERREUR  (Erreur,i)
            return
         endif

      enddo

      deallocate(ltab1)
      deallocate(ltab2)
      deallocate(itab1)
      deallocate(itab2)
      
   endif PretraitTracer

   call destroy(document)
   call destroy(config)
   
   !Erreur%arbredappel = arbredappel_old

   return

   10630 format (/,'PARAMETRES GENERAUX DU CALCUL AVEC TRACER',/, &
               &  '------------------------------------------',/)
   10640 format ('Nombre de traceurs                           : ',I2)
   10650 format ('Modele de qualite d''eau choisi              : ',I2,/,&
               &' [ 1 : Aucun / 2 : O2       / 3 : Biomass    ',/,&
               &'   4 : Eutro / 5 : Micropol / 6 : Thermic  ] ')
   10655 format ('Frequence de couplage hydraulique / Tracer   : ',I3)
   10660 format (/,'CONDITIONS INITIALES TRACER',/, &
               &  '----------------------------',/)
   10690 format ('Presence de concentrations initiales        : ',A3)

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
   
end subroutine PRETRAIT_Tracer
