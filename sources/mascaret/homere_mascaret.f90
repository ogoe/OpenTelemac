!== Copyright (C) 2000-2015 EDF-CEREMA ==
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

program HOMERE_MASCARET
! *********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!                             J.-M. LACOMBE
!                             F. ZAOUI
!                             S. DELMAS
!                             C.COULET
!                             F. DEMANGEON
!
! VERSION : 8.1.0               EDF-CEREMA-ARTELIA
! *********************************************************************
! Fonction : Programme Chef d'Orchestre, Execution pas a pas d'un des
!            trois noyaux de calculs :
!            - SARAP pour le permanent (fluvial et torrentiel) DF
!            - REZO pour le transitoire (fluvial uniquement) DF
!            - MASCARET pour le transitoire (fluvial et torrentiel) VF
!
!            Couplage eventuel avec le transport de polluants passifs
! *********************************************************************

   !======================== Declarations =============================
   use M_PRECISION
   use M_BARRAGE_T              ! Type BARRAGE_T
   use M_LOI_T                  ! Type LOI_T
   use M_SECTION_T              ! Type SECTION_PLAN_T
   use M_SECTION_PLAN_T         ! Type SECTION_T
   use M_ZONE_SECHE_T           ! Type ZONE_SECHE_T
   use M_SAUVE_T                ! Type SAUVE_T
   use M_CASIER_T               ! Type CASIER_T
   use M_LIAISON_T              ! Type LIAISON_T
   use M_APPORT_PLUIE_T         ! Type APPORT_PLUIE_T
   use M_INDEX_VARIABLE_C       ! Index des variables
   use M_CONSTANTES_CALCUL_C    ! Constantes num, phys et info
   use M_MESSAGE_C              ! Messages d'erreur
   use M_PARAMETRE_C            ! EPS2,SEPS
   use M_INTERSECT_I            ! Modules d'interface de tous les sius-programmes
   use M_PLANIM_I
   use M_PLANMA_I
!TAPENADE--
   use M_MASCARET_I
   use M_POST_I
   use M_POST_IMP_I
   use M_POST_CASIER_I
   use M_POST_IMP_CASIER_I
   use M_STOCK_I
   use M_STOCK_REP_I
!--TAPENADE
   use M_PRETRAIT_I
   use M_QCL_I
   use M_REZO_I
   use M_SARAP_I
   use M_CLPLUIE_I
   use M_TRAITER_ERREUR_I       ! Traitement de l'errreur
   use M_TRAITER_ERREUR_CASIER_I
   use M_CQINJ_I
   use M_XINDIC_S               ! Calcul de l'indice correspondant a une abscisse
   use M_RHSBP_S
   use M_SHARE_VAR
!TAPENADE--
   use M_CONSTANTES_CALAGE_T    ! Constantes pour le calage automatique
   use M_CALAGE_N2QN1_I         ! Calage automatique
   use M_DIFF_Z_CF12_FWD_SARAP_VAR
   use M_DIFF_Z_CF12_BWD_SARAP_VAR
   !
   ! Tracer
   !-------
   use M_INDEX_VARIABLE_TRACER_C    ! Variables de sorties de TRACER 
   use M_PARAMETRES_QUALITE_EAU_T   ! Donnees physiques du modele de QE
   use M_METEO_T                    ! Donnees meteo
   use M_CONSTANTES_TRACER_T        ! Constantes liees au traceur (convection/diffusion)
   use M_LOI_TRACER_T
   use M_SOURCE_TRACER_T
   use M_COND_LIM_TRACER_T
   use M_TRACER_I
   use M_PRETRAIT_TRACER_I
   use M_QCL_TRACER_I
   use M_POST_IMP_TRACER_I
   use M_STOCK_TRACER_I
!--TAPENADE
   
   !.. Implicit Declarations ..
   implicit none

   !
   intrinsic cpu_time
   ! Parametres generaux
   !--------------------
   integer         :: VersionCode
   integer         :: Noyau
   type(FICHIER_T) :: FichierModele
   type(FICHIER_T) :: FichierMotCle
   logical         :: OptionCasier,OptionCalage
   logical         :: OndeSubm
   logical         :: CalculValidation
   integer         :: TypeValidation
   ! Modelisation physique
   integer      :: Regime
   logical      :: FrottParoiVerticale
   logical      :: PerteElargissementTrans
   logical      :: Boussinesq
   logical      :: NoConvection
   logical      :: DebProgressifLM
   logical      :: DebProgressifZS
   real(DOUBLE) :: FroudeLim
   Logical      :: FrottementImplicite
   Logical      :: Impli_Trans, Opt
   real(DOUBLE) :: DZArriveeFront
   logical      :: InterpLinCoeffFrott
   real(DOUBLE) :: HEPS
   ! Parametres temporels
   !---------------------
   real(DOUBLE)                                :: Temps1
   integer                                     :: PhaseSimulation
   integer                                     :: num_pas
   real(DOUBLE)                                :: DT              ! pas de temps
   real(DOUBLE)                                :: TempsInitial
   integer                                     :: CritereArret
   integer                                     :: NbPasTemps
   real(DOUBLE)                                :: TempsMaximum
   logical                                     :: PasTempsVariable
   real(DOUBLE)                                :: CourantObj
   real(DOUBLE)                                :: PasTempsOptimal
   real(DOUBLE)                                :: DTLEVY
   real(DOUBLE)                                :: Cote_max_controle
   integer                                     :: Section_controle
   ! Geometrie
   !----------
   type(FICHIER_T)                             :: FichierGeom
   integer                                     :: FormatGeom
   logical                                     :: PresenceZoneStockage
   logical                                     :: Prof_Abs
   ! Planimetrage
   !-------------
   real(DOUBLE)        , dimension(:), pointer :: DZ => null()
   type(SECTION_PLAN_T)                        :: SectionPlan
   real(DOUBLE)        , dimension(:), pointer :: DZD => null()
   real(DOUBLE)        , dimension(:), pointer :: XD => null()
   ! Maillage et Reseau
   !-------------------
   type(FICHIER_T)                           :: FichierMaillage
   type(FICHIER_T)                           :: FichierSauveMaillage
   integer                                   :: TypeMaillage
   ! Variables
   !----------
   ! Variables constantes
   real(DOUBLE)    , dimension(:), pointer :: ZInitial => null()
   real(DOUBLE)    , dimension(:), pointer :: RGC, RDC => null()
   integer         , dimension (:) , pointer :: IFIN => null()
   ! Variables communes a LIDO et MASCARET
   real(DOUBLE)    , dimension(:), pointer :: Q => null()      ! Debit total
   real(DOUBLE)    , dimension(:), pointer :: Q1 => null()     ! Debit mineur
   real(DOUBLE)    , dimension(:), pointer :: Q2 => null()     ! Debit majeur
   real(DOUBLE)    , dimension(:), pointer :: Z => null()      ! Cote
   real(DOUBLE)    , dimension(:), pointer :: S1 => null()     ! Section mineur
   real(DOUBLE)    , dimension(:), pointer :: S2 => null()     ! Section majeur
   real(DOUBLE)    , dimension(:), pointer :: Froude => null() ! Nombre de Froude
   real(DOUBLE)    , dimension(:), pointer :: BETA => null()   ! Coeff Beta
   real(DOUBLE)    , dimension(:), pointer :: YNODE => null()   ! hauteur d'eau (etat mascaret)
   real(DOUBLE)    , dimension(:), pointer :: UNODE => null()   ! vitesse (etat mascaret)
   real(DOUBLE)    , dimension(:), pointer :: CNODE => null()   ! celerite (etat mascaret)
   integer         , dimension(:), pointer :: JGNODE,JDNODE,IFIGE => null()
   real(DOUBLE)    , dimension(:,:), pointer :: FLUX => null()
   real(DOUBLE)    , dimension(:)  , pointer :: DebitFlux => null()
   real(DOUBLE)    , dimension(:,:,:), pointer :: W => null()   ! Etat 2D confluents
   real(DOUBLE)    , dimension(:,:),  pointer  :: AIRS => null() ! Etat 2D confluents
   ! Variables specifiques a LIDO
   real(DOUBLE)    , dimension(:), pointer :: B1 => null()   ! Largeur au miroir min
   real(DOUBLE)    , dimension(:), pointer :: B2 => null()   ! Largeur au miroir maj
   real(DOUBLE)    , dimension(:), pointer :: BS => null()   ! Largeur au miroir zones de stockage
   real(DOUBLE)    , dimension(:), pointer :: P1 => null()   ! Perimetre mouille min
   real(DOUBLE)    , dimension(:), pointer :: P2 => null()   ! Perimetre mouille maj
   real(DOUBLE)    , dimension(:), pointer :: RH1 => null()  ! Rayon hydrauliqu min
   real(DOUBLE)    , dimension(:), pointer :: RH2 => null()  ! Rayon hydrauliqu maj
   ! Variables specifiques a MASCARET
   real(DOUBLE)    , dimension(:), pointer :: XFRON => null() ! POSITION DU FRONT
   real(DOUBLE)    , dimension(:), pointer :: SVRAI => null() ! SECTION MOUILLEE ANALYTIQUE
   real(DOUBLE)    , dimension(:), pointer :: ZVRAI => null() ! COTE ANALYTIQUE
   real(DOUBLE)    , dimension(:), pointer :: QVRAI => null() ! DEBIT ANALYTIQUE
   real(DOUBLE)    , dimension(:), pointer :: ZINIT => null() ! Ligne initiale
   ! Variables complementaires
   real(DOUBLE)    , dimension(:), pointer :: V1 => null()    ! Vitesse majeur
   real(DOUBLE)    , dimension(:), pointer :: V2 => null()    ! Vitesse majeur
   real(DOUBLE)    , dimension(:), pointer :: Y => null()     ! Z - ZREF
   real(DOUBLE)    , dimension(:), pointer :: HMOY => null()  ! S/B
   real(DOUBLE)    , dimension(:), pointer :: SS => null()    ! Scetion stockage
   real(DOUBLE)    , dimension(:), pointer :: Q2G => null()   ! Debit maj gauche
   real(DOUBLE)    , dimension(:), pointer :: Q2D => null()   ! Debit maj droit
   real(DOUBLE)    , dimension(:), pointer :: VOL => null()   ! Volume du bief
   real(DOUBLE)    , dimension(:), pointer :: VOLS => null()  ! Volume zones de stockage
   real(DOUBLE)    , dimension(:), pointer :: CHARG => null() ! Charge
   real(DOUBLE)    , dimension(:), pointer :: ZMAX => null()  ! Cote max
   real(DOUBLE)    , dimension(:), pointer :: TZMAX => null() ! Temps du Z max
   real(DOUBLE)    , dimension(:), pointer :: VZMAX => null() ! Vitesse max
   real(DOUBLE)    , dimension(:), pointer :: ZMIN => null()  ! Cote min
   real(DOUBLE)    , dimension(:), pointer :: TZMIN => null() ! Temps du Z min
   real(DOUBLE)    , dimension(:), pointer :: V1MIN => null() ! Vitesse mineur min
   real(DOUBLE)    , dimension(:), pointer :: V1MAX => null() ! Vitesse mineur max
   real(DOUBLE)    , dimension(:), pointer :: BMAX => null()  ! Largeur au miroir max
   real(DOUBLE)    , dimension(:), pointer :: TOND => null()  ! temps d'arrivee onde
   real(DOUBLE)    , dimension(:), pointer :: QMAX => null()  ! Debit max
   real(DOUBLE)    , dimension(:), pointer :: TQMAX => null() ! Temps du debit max
   real(DOUBLE)    , dimension(:), pointer :: EMAX => null()  ! Energie max
   real(DOUBLE)    , dimension(:), pointer :: TAUF => null()  ! Contrainte de frottement
   ! Conditions initiales
   !---------------------
   logical                                   :: RepriseCalcul
   type(FICHIER_T)                           :: FichierRepriseEcr
   type(FICHIER_T)                           :: FichierRepriseLec
   type(FICHIER_T)                           :: FichierLigne
   ! Tracer
   ! -------------
   type(FICHIER_T)                           :: message
   !
   type(ZONE_SECHE_T), dimension(:), pointer :: ZoneSeche
   type(Sauve_T)                             :: Sauve
   ! Impressions - resultats
   !------------------------
   character(LEN=255)                      :: TitreCas
   character(len=32)                       :: messim
   logical                                 :: ImpressionPlani
   integer                                 :: PasStockage
   integer                                 :: PasImpression
   integer                                 :: PremierPasStocke
   type(FICHIER_T)                         :: FichierResultat
   integer                                 :: FormatResu
   type(FICHIER_T)                         :: FichierResultat2
   integer                                 :: FormatResu2
   logical, dimension(NB_TOT_VAR)          :: VarCalc
   logical, dimension(NB_TOT_VAR)          :: VarSto
   integer                                 :: OptionStockage
   integer, dimension(:)         , pointer :: SectionStockage => null()
   ! Lois hydrauliques
   !------------------
   type(LOI_T)    , dimension(:), pointer  :: LoiHydrau => null()
   type(FICHIER_T)                         :: FichierLoiHydrau
   ! Barrage - singularites
   !-----------------------
   type(BARRAGE_T)                            :: Barrage
   ! Pertes de charge singulieres
   !-----------------------------
   real(DOUBLE)       , dimension(:), pointer :: PCSing => null()
   ! Apports et Deversoirs
   !----------------------
   real(DOUBLE)       , dimension(:), pointer :: Qdeverse => null()
   real(DOUBLE)       , dimension(:), pointer :: Qinjec => null()
   ! Confluents
   !-----------
   ! Abaques pour le calcul des pertes de charge automatique aux confluences
   type(FICHIER_T)                             :: FichierAbaque
   !
   ! Etats
   !------
   real(DOUBLE)   , dimension(:), pointer    :: VolBiefActif => null()
   real(DOUBLE)   , dimension(:), pointer    :: VolBiefStockage => null()
   real(DOUBLE)   , dimension(:), pointer    :: QAmontPrec => null()
   real(DOUBLE)   , dimension(:), pointer    :: QAvalPrec => null()
   real(DOUBLE)                              :: TempsPrecedent
   ! Variables locales
   integer :: retour ! Code de retour d'erreur des fonctions intrinseques
   logical :: imp_avancement ! flag pour impression dans le fichier de controle
   real(DOUBLE)   , dimension(:), pointer    :: DPDZ1 => null()
   real(DOUBLE)   , dimension(:), pointer    :: DPDZ2 => null()
   real(DOUBLE)   , dimension(:), pointer    :: VOLSM => null()
   real (DOUBLE)                             :: DTImpression
   real (DOUBLE)                             :: POURC,XN1,XN2
   real(DOUBLE)                              :: avancement !arrondi de l'avancement du calcul
   integer :: phase_planim
   integer :: phase_intersect
   integer :: phase_planma
   integer :: phase_sarap
   integer :: phase_rezo
   integer :: phase_mascaret
   integer :: phase_qcl
   integer :: phase_post
   integer :: phase_post_imp
   integer :: phase_stock
   integer :: FormatResu1
   integer :: ul
   integer :: nb_pas
   integer :: i,j,l,m, nb_Bief,nbap
   real    :: T0,T1,T2
   type(FICHIER_T) :: FichierCas
   type(FICHIER_T) :: FichierControle
   ! Casiers
   ! -------
   type(CASIER_T), dimension(:), pointer :: Casier => null()
   type(LIAISON_T),dimension(:), pointer :: Liaison => null()
   type(APPORT_PLUIE_T), dimension(:), pointer :: ApportPluie => null()
   type(FICHIER_T) :: FichierListingCasier, FichierListingLiaison, &
                      FichierResultatCasier, FichierResultatLiaison, FichierGeomCasier
   real(DOUBLE) ::  abs_abs
   integer :: UniteListingCasier, iapport
   logical :: Impression
   integer :: phase_clpluie
   integer :: phase_post_casier
   integer :: phase_post_imp_casier
   integer :: phase_stock_casier
!TAPENADE--
   !
   ! Tracer
   ! ------
   integer :: Phase_Tracer, PhaseCouplageTracer
   integer :: npass
   logical :: OptionTracer
   integer :: nbsing
   integer                                :: Nbtrac , FreqCouplage
   real(DOUBLE)                           :: DT_trac  ! pas de temps Tracer
   real (DOUBLE) ,dimension(:,:), pointer :: Ctraceur => null() ! Concentration du traceur no i
   real (DOUBLE) ,dimension(:,:), pointer :: S => null()        ! Sources internes du traceur
   real (DOUBLE) ,dimension(:,:), pointer :: RNU => null()      ! terme source implicite
   real(DOUBLE)  , dimension(:), pointer  :: QT => null()       ! Debit total (Q1+Q2)
   real(DOUBLE)  , dimension(:), pointer  :: ST => null()       ! Surface mouillee totale (S1+S2)
   real(DOUBLE)  , dimension(:), pointer  :: BT => null()       ! Largeur totale (B1+B2)
   real(DOUBLE)  , dimension(:), pointer  :: QT_ANT => null()   ! Debit total au pas de temps anterieur
   real(DOUBLE)  , dimension(:), pointer  :: ST_ANT => null()   ! Surface mouillee totale au pas de temps anterieur
   real(DOUBLE)  , dimension(:), pointer  :: BT_ANT => null()   ! Largeur totale au pas de temps anterieur
   integer                                :: Modele_Qual_Eau
   real (DOUBLE) ,dimension(:)  , pointer :: CLAM , CLAV => null()
   Type(FICHIER_T)                        :: FichierListingTracer
   Type(FICHIER_T)                        :: FichierResuTracer
   Type(FICHIER_T)                        :: FichierConcInit
   Type(FICHIER_T)                        :: FichierLoiTracer
   Type(FICHIER_T)                        :: FichierParPhy
   Type(FICHIER_T)                        :: FichierMeteo
   Integer                                :: FormatResuTracer
   logical, dimension(NB_TOT_VAR)         :: VarStoTracer
   logical                                :: ImpressionConcListing , ImpressionBilanTracer
   Type (PARAMETRES_QUALITE_EAU_T)        :: ParPhy
   Type (METEO_T)                         :: Meteo
   Type (CONSTANTES_TRACER_T),dimension(:), pointer :: ConsTrac => null()
   Type (SOURCE_TRACER_T)    ,dimension(:), pointer :: Sources_Tracer => null()
   Type (COND_LIM_TRACER_T)  ,dimension(:), pointer :: CondLimTrac => null()
   Type (LOI_TRACER_T)       ,dimension(:), pointer :: LoiTracer => null()
   real (DOUBLE)           ,dimension(:)  , pointer :: NbCourant => null()
   real (DOUBLE)           ,dimension(:,:),pointer  :: MASS , FLUMAS , FLUENT => null()
   real (DOUBLE)           ,dimension(:,:), pointer :: FLUSOR , FLUSRC => null()
!--TAPENADE

   !======================== Instructions ========================
   FichierCas%Unite             = 10
   FichierModele%Unite          = 11
   FichierMotCle%Unite          = 12
   FichierGeom%Unite            = 14
   FichierMaillage%Unite        = 15
   FichierSauveMaillage%Unite   = 16
   FichierRepriseLec%Unite      = 17
   FichierRepriseEcr%Unite      = 18
   FichierLigne%Unite           = 19
   FichierResultat%Unite        = 20
   FichierResultat2%Unite       = 25
   FichierLoiHydrau%Unite       = 21
   FichierListing%Unite         = 22
   FichierAbaque%Unite          = 23
   FichierControle%Unite        = 24
   FichierListingCasier%Unite   = 33
   FichierListingLiaison%Unite  = 34
   FichierResultatCasier%Unite  = 35
   FichierResultatLiaison%Unite = 36
   FichierGeomCasier%Unite      = 37
!TAPENADE--
   FichierResultatCalage%Unite  = 41
   FichierResultatCalage1%Unite = 42
   !
   ! Tracer
   ! ------
   FichierListingTracer%Unite   = 43
   FichierResuTracer%Unite      = 44
   FichierConcInit%Unite        = 45
   FichierLoiTracer%Unite       = 46
   FichierParPhy%Unite          = 47
   FichierMeteo%Unite           = 48
   npass                        = 0
   VarStoTracer(:)              = .false.
!--TAPENADE
   ! common du canal listing
   ul_lst              = FichierListing%Unite
   UniteListing        = ul_lst
   UL_LST_CAS          = FichierListingCasier%Unite
   UniteListingCasier  = FichierListingCasier%Unite
   FichierCas%Nom      = 'FichierCas.txt'
   FichierAbaque%Nom   = 'Abaques.txt'
   FichierControle%Nom = 'Controle.txt'

   open(unit=FichierCas%Unite, file=FichierCas%Nom, access='SEQUENTIAL', &
        action='READ'           , form='FORMATTED'       , iostat=RETOUR, &
        position='rewind'       , status='OLD'     )
   if( RETOUR /= 0 ) then
      Erreur%Numero = 3
      Erreur%ft     = err_3
      Erreur%ft_c   = err_3c
      call TRAITER_ERREUR( Erreur , FichierCas%Nom )
      stop 1
   end if
   read(FichierCas%Unite,*) FichierMotCle%Nom

   avancement    = 0.d0
   Erreur%Numero = 0
   retour        = 0
   !Erreur%arbredappel = 'MAIN'
   Print *
   Print * ,'MASCARET v8.1.0 == Copyright (C) 2000-2015 EDF-CEREMA =='
   Print *
   Print * , 'Data File : ' // TRIM(FichierMotCle%Nom)
   Print *

   PhaseSimulation = PHASE_INITIALISATION

   ! SAISIE DES DONNEES POUR L'HYDRAULIQUE 
   !========================================
   call  PRETRAIT                                             (            &
     VersionCode, Noyau                                                  , &
     FichierModele, FichierMotCle                                        , &
     OptionCasier                                                        , &
     OndeSubm                                                            , &
     CalculValidation, TypeValidation                                    , &
     Regime, ModeleLit                                                   , &
     FrottParoiVerticale, PerteChargeConfluent                           , &
     DebProgressifLM, DebProgressifZS                                    , &
     DZArriveeFront                                                      , &
     FroudeLim, FrottementImplicite, Impli_Trans ,Opt                    , &
     PerteElargissementTrans , Boussinesq , NoConvection, CQMV           , &
     Prof_Abs , HEPS                                                     , &
     DT, TempsInitial, CritereArret, NbPasTemps, TempsMaximum            , &
     Section_controle, Cote_max_controle                                 , &
     PasTempsVariable, CourantObj                                        , &
     FichierGeom, FormatGeom, Profil, PresenceZoneStockage               , &
     X, IDT, XDT                                                         , &
     FichierMaillage, FichierSauveMaillage, TypeMaillage                 , &
     Connect                                                             , &
     Z, Q                                                                , &
     CF1, CF2, InterpLinCoeffFrott, LoiFrottement                        , &
     RepriseCalcul                                                       , &
     FichierRepriseEcr, FichierRepriseLec                                , &
     FichierLigne                                                        , &
     ZoneSeche                                                           , &
     TitreCas                                                            , &
     ImpressionPlani, ImpressionCalcul                                   , &
     PasStockage, PasImpression                                          , &
     PremierPasStocke                                                    , &
     FichierResultat, FormatResu, FichierResultat2, FormatResu2          , &
     FichierListing                                                      , &
     VarCalc, VarSto                                                     , &
     OptionStockage, SectionStockage                                     , &
     LoiHydrau, FichierLoiHydrau                                         , &
     Barrage, Singularite, PCSing                                        , &
     Apport, Deversoir                                                   , &
     Confluent, Extremite, Algorithme, Abaque, FichierAbaque             , &
     Casier                                                              , &  ! tableau des casiers
     Liaison                                                             , &  ! tableau des liaisons
     ApportPluie                                                         , &  ! tableau des apports de pluie
     FichierResultatCasier                                               , &  ! fichier des resultats des caracteristiques Casier
     FichierResultatLiaison                                              , &  ! fichier des resultats des caracteristiques Liaison
     FichierListingCasier                                                , &
     FichierListingLiaison                                               , &
     FichierGeomCasier                                                   , &
!TAPENADE--
     Calage_Frott,Calage_Crues, nb_zone_frottement,max_mes,OptionCalage , & !Donnees pour le calage automatique du Strickler
     Constantes_Calage , FichierResultatCalage , FichierResultatCalage1  , &
!--TAPENADE
     Erreur )

   if( Erreur%Numero /= 0 ) then
      write(*,321)
      Print * , Erreur%Message 
      stop 1
   endif

   messim = 'Simulation'
   if(OptionCalage.eqv..true.) messim = 'Automatic Calibration'


   !
   ! Impression de statistiques 
   !---------------------------
   call print_stat_masc(TitreCas,size(Connect%ORIGINEBIEF),size(Profil),size(Connect%NUMSECTIONEXTLIBRE),  &
                        size(Connect%NBBIEFCONFLUENCE),size(Apport),size(Deversoir),size(Casier),size(Liaison),  &
                        size(Singularite),size(X),Noyau,DT,CourantObj,TempsMaximum,NbPasTemps,CritereArret,  &
                        PastempsVariable,associated(Singularite),OptionCasier)
!TAPENADE--   
   !
   !  Tracer
   !  ------
   call  PRETRAIT_TRACER                                 ( &
     FichierMotCle                                       , & ! Fichier des mots-cles
     Noyau                                               , & ! Noyau de calcul hydraulique
     TypeMaillage                                        , & ! Choix du type de maillage longitudinal
     Connect                                             , & ! Table de connectivite
     Apport                                              , & ! Apports hydrauliques
     Profil                                              , & ! Profils geometriques
     X                                                   , & ! Abscisses des sections de calcul
     Extremite                                           , & ! Extremites libres
     TempsMaximum                                        , & ! Temps maximum du calcul
      ! Lecture des parametres de Tracer
     OptionTracer                                        , & ! Choix d'un calcul avec TRACER
     Ctraceur                                            , & ! Concentrations en traceurs
     Nbtrac                                              , & ! Nombre de traceurs
     ConsTrac                                            , & ! Constantes pour TRACER
     FreqCouplage                                        , & ! Frequence de couplage hydraulique/tracer
      ! Conc init, CL, sources, lois tracer
     FichierConcInit                                     , & ! Fichier des conc ini
     CondLimTrac                                         , & ! Conditions aux limites
     Sources_tracer                                      , & ! Sources pour le traceur
     LoiTracer                                           , & ! Lois Tracer (CL ou sources)
     FichierLoiTracer                                    , & ! Fichier loi Tracer
      ! Lecture des parametres de QE
     Modele_Qual_Eau                                     , & ! Modele de QE
     ParPhy                                              , & ! Parametres de modele de QE
     Meteo                                               , & ! Donnees meteo
     FichierParphy                                       , & ! Fichier des parametres de QE
     FichierMeteo                                        , & ! Fichier meteo
      ! Impression des parametres et resultats
     FichierResuTracer                                   , & ! Fichier resultats
     FormatResuTracer                                    , & 
     FichierListingTracer                                , & ! Fichier listing
     ImpressionConcListing                               , & ! Logique pour les impressions
     ImpressionBilanTracer                               , & ! Logique pour les impressions
     PasStockage                                         , & ! Pas de stockage  (hydraulique)
     PasImpression                                       , & ! Pas d'impression (hydraulique)
      ! Traitement des erreurs
     Erreur                                                )

   DT_Trac = DT * DBLE(FreqCouplage)

   if( Erreur%Numero /= 0 ) then 
       write(*,321)
       Print * , Erreur%Message 
       stop 1
   endif
!--TAPENADE

   ! ALLOCATIONS ET INITIALISATIONS
   !===============================

   nb_sect = size(X)
   nb_bief = size(Connect%OrigineBief)

   !-----------------------------------------------------
   ! Allocation des variables conmunes a LIDO et MASCARET
   !-----------------------------------------------------
   allocate( Q1(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('Q1')
   Q1(:) = W0    ! W0 = 0._DOUBLE
   
   allocate( Qdeverse(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('Qdeverse')
   Qdeverse(:) = W0    ! W0 = 0._DOUBLE

   allocate( Qinjec  (nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('Qinjec')
   Qinjec(:) = W0    ! W0 = 0._DOUBLE

   allocate( Q2(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('Q2')
   Q2(:) = W0    ! W0 = 0._DOUBLE

   allocate( S1(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('S1')
   S1(:) = W0    ! W0 = 0._DOUBLE

   allocate( S2(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('S2')
   S2(:) = W0    ! W0 = 0._DOUBLE

   allocate( BETA(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('BETA')
   BETA(:) = W0    ! W0 = 0._DOUBLE

   allocate( Froude(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('Froude')
   Froude(:) = W0    ! W0 = 0._DOUBLE

   allocate( FLUX(nb_sect,2) , STAT = retour )
   if( retour /= 0 ) call err_alloc('FLUX')
   Flux(:,:) = W0   ! W0 = 0._DOUBLE

   allocate( DebitFlux(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('DebitFlux')
   DebitFlux (:) = W0   ! W0 = 0._DOUBLE
 
   !--------------------------------------------
   ! Allocation des variables specifiques a LIDO
   !--------------------------------------------
   allocate( P1(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('P1')
   P1(:) = W0    ! W0 = 0._DOUBLE

   allocate( P2(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('P2')
   P2(:) = W0    ! W0 = 0._DOUBLE

   allocate( B1(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('B1')
   B1(:) = W0    ! W0 = 0._DOUBLE

   allocate( B2(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('B2')
   B2(:) = W0    ! W0 = 0._DOUBLE

   allocate( BS(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('BS')
   BS(:) = W0    ! W0 = 0._DOUBLE

   allocate( RH1(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('RH1')
   RH1(:) = W0    ! W0 = 0._DOUBLE

   allocate( RH2(nb_sect) , STAT = retour )
   if( retour /= 0 ) call err_alloc('RH2')
   RH2(:) = W0    ! W0 = 0._DOUBLE

   !------------------------------------------------
   ! Allocation des variables specifiques a MASCARET
   !------------------------------------------------
   if( Noyau == NOYAU_MASCARET ) then

      allocate( XFRON(nb_sect) , STAT = retour )
      if( retour /= 0 ) call err_alloc('XFRON')
      XFRON(:) = W0    ! W0 = 0._DOUBLE
      
      allocate( IFIN(nb_sect)  , STAT = retour )
      if( retour /= 0 ) call err_alloc('IFIN')

   endif

   allocate (SVRAI(nb_sect), STAT = retour)
   if( retour /= 0 ) call err_alloc('SVRAI')
   SVRAI(:) = W0    ! W0 = 0._DOUBLE
   
   allocate (QVRAI(nb_sect), STAT = retour)
   if( retour /= 0 ) call err_alloc('QVRAI')
   QVRAI(:) = W0    ! W0 = 0._DOUBLE
   
   allocate (ZVRAI(nb_sect), STAT = retour)
   if( retour /= 0 ) call err_alloc('ZVRAI')
   ZVRAI(:) = W0    ! W0 = 0._DOUBLE
   
   allocate (ZINIT(nb_sect), STAT = retour)
   if( retour /= 0 ) call err_alloc('ZINIT')
   ZINIT(:) = W0    ! W0 = 0._DOUBLE
   
   allocate (UNODE(nb_sect), STAT = retour)
   if( retour /= 0 ) call err_alloc('UNODE')
   UNODE(:) = W0    ! W0 = 0._DOUBLE
   
   allocate (CNODE(nb_sect), STAT = retour)
   if( retour /= 0 ) call err_alloc('CNODE')
   CNODE(:) = W0    ! W0 = 0._DOUBLE
   
   allocate (YNODE(nb_sect), STAT = retour)
   if( retour /= 0 ) call err_alloc('YNODE')
   YNODE(:) = W0    ! W0 = 0._DOUBLE
   
   allocate (JGNODE(nb_sect), STAT = RETOUR)
   if( retour /= 0 ) call err_alloc('JGNODE')
   
   allocate (JDNODE(nb_sect), STAT = RETOUR)
   if( retour /= 0 ) call err_alloc('JDNODE')
   
   allocate (IFIGE(nb_sect), STAT = RETOUR)
   if( retour /= 0 ) call err_alloc('IFIGE')
   
   allocate (W(3,12,size(Connect%OrigineBief)), STAT= retour)
   if( retour /= 0 ) call err_alloc('W')
   W(3,12,:) = W0    ! W0 = 0._DOUBLE
   
   allocate (AIRS(12,size(Connect%OrigineBief)), STAT= retour)
   if( retour /= 0 ) call err_alloc('AIRS')
   AIRS(12,:) = W0    ! W0 = 0._DOUBLE

   ! La ligne ci-dessous permet d'entrer le debit de la ligne d'eau
   ! initiale au premier pas de temps
   Q1(:) = Q(:)

!TAPENADE--
   !===================================================
   ! Allocation des variables specifiques a TRACER
   !===================================================
   !
   if( OptionTracer ) then

      allocate( QT(nb_sect) , STAT = retour )
      if( retour /= 0 ) call err_alloc('QT')
      QT(:) = W0    ! W0 = 0._DOUBLE

      allocate( ST(nb_sect) , STAT = retour )
      if( retour /= 0 ) call err_alloc('ST')
      ST(:) = W0    ! W0 = 0._DOUBLE

      allocate( BT(nb_sect) , STAT = retour )
      if( retour /= 0 ) call err_alloc('BT')
      BT(:) = W0    ! W0 = 0._DOUBLE

      allocate( QT_ANT(nb_sect) , STAT = retour )
      if( retour /= 0 ) call err_alloc('QT_ANT')
      QT_ANT(:) = W0    ! W0 = 0._DOUBLE

      allocate( ST_ANT(nb_sect) , STAT = retour )
      if( retour /= 0 ) call err_alloc('ST_ANT')
      ST_ANT(:) = W0    ! W0 = 0._DOUBLE

      allocate( BT_ANT(nb_sect) , STAT = retour )
      if( retour /= 0 ) call err_alloc('BT_ANT')
      BT_ANT(:) = W0    ! W0 = 0._DOUBLE

      allocate( NbCourant(nb_bief) , STAT = retour )
      if( retour /= 0 ) call err_alloc('NbCourant')
      NbCourant(:) = W0    ! W0 = 0._DOUBLE

      allocate( MASS(nb_bief,Nbtrac) , STAT = retour )
      if( retour /= 0 ) call err_alloc('MASS')
      MASS(:,:) = W0    ! W0 = 0._DOUBLE

      allocate( FLUMAS(nb_bief,Nbtrac) , STAT = retour )
      if( retour /= 0 ) call err_alloc('FLUMAS')
      FLUMAS(:,:) = W0    ! W0 = 0._DOUBLE

      allocate( FLUENT(nb_bief,Nbtrac) , STAT = retour )
      if( retour /= 0 ) call err_alloc('FLUENT')
      FLUENT(:,:) = W0    ! W0 = 0._DOUBLE

      allocate( FLUSOR(nb_bief,Nbtrac) , STAT = retour )
      if( retour /= 0 ) call err_alloc('FLUSOR')
      FLUSOR(:,:) = W0    ! W0 = 0._DOUBLE

      allocate( FLUSRC(nb_bief,Nbtrac) , STAT = retour )
      if( retour /= 0 ) call err_alloc('FLUSRC')
      FLUSRC(:,:) = W0    ! W0 = 0._DOUBLE

   endif

   if( OptionCalage ) then
      allocate( dcf1_fwd(size(cf1)) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'dcf1_fwd' )
         write(*,321)
         Print * , Erreur%Message 
         stop 1 
      end if
      dcf1_fwd(:) = W0
      allocate( dcf2_fwd(size(cf2)) , STAT = retour  )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'dcf2_fwd' )
         write(*,321)
         Print * , Erreur%Message 
         stop 1 
      end if
      dcf2_fwd(:) = W0
      allocate( diff_z_fwd(size(z)) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'diff_z_fwd' )
         write(*,321)
         Print * , Erreur%Message 
         stop 1 
      end if
      diff_z_fwd(:) = W0
      allocate( dcf1_bwd(size(cf1)) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'dcf1_bwd' )
         write(*,321)
         Print * , Erreur%Message 
         stop 1 
      end if
      dcf1_bwd(:) = W0
      allocate( dcf2_bwd(size(cf2)) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'dcf2_bwd' )
         write(*,321)
         Print * , Erreur%Message 
         stop 1 
      end if
      dcf2_bwd(:) = W0
      allocate( diff_z_bwd(size(z)) , STAT = retour )
      if( retour /= 0 ) then
         Erreur%Numero = 5
         Erreur%ft     = err_5
         Erreur%ft_c   = err_5c
         call TRAITER_ERREUR( Erreur , 'diff_z_bwd' )
         write(*,321)
         Print * , Erreur%Message 
         stop 1 
      end if
      diff_z_bwd(:) = W0
   endif
!--TAPENADE

   !==========================================================================
   !                                BOUCLE DE CALCUL
   !==========================================================================
   Temps          = TempsInitial
   TempsPrecedent = TempsInitial
   Temps1 = Temps
   CALL CPU_TIME (t0)
    Print *,'Start the '//trim(messim)//'...'

   if( RepriseCalcul ) then
      !============================
      ! ouverture du fichier a lire
      !============================
      ul = FichierRepriseLec%Unite
      open(unit=ul          , file=FichierRepriseLec%Nom, access='SEQUENTIAL', &
           action='READ'    , form='FORMATTED', iostat=RETOUR      , &
           position='rewind', status='OLD'    )
      if( RETOUR /= 0 ) then
         Erreur%Numero = 3
         Erreur%ft     = err_3
         Erreur%ft_c   = err_3c
         call TRAITER_ERREUR( Erreur , FichierRepriseLec%Nom )
         stop
      end if

      read (FichierRepriseLec%Unite,*) Temps
      read (FichierRepriseLec%Unite,*) (XFRON(I),I=1,size(Connect%OrigineBief))
   endif

   num_pas = 0

   do while( Erreur%Numero == 0 )

      if( PhaseSimulation == PHASE_INITIALISATION ) then

         phase_planim    = PHASE_CALCUL
         phase_intersect = PHASE_INITIALISATION
         if( Noyau == NOYAU_MASCARET ) then
            phase_planma = PHASE_CALCUL
         else
            phase_planma = PHASE_ARRET
         endif
         phase_qcl       = PHASE_INITIALISATION
         phase_sarap     = PHASE_INITIALISATION
         phase_rezo      = PHASE_INITIALISATION
         phase_mascaret  = PHASE_INITIALISATION
         phase_post      = PHASE_INITIALISATION
         phase_post_imp  = PHASE_INITIALISATION
         phase_stock     = PHASE_INITIALISATION

      elseif(PhaseSimulation == PHASE_CALCUL) then

         phase_planim    = PHASE_ARRET
         phase_intersect = PHASE_ARRET
         phase_planma    = PHASE_ARRET
         phase_qcl       = PHASE_CALCUL
         phase_sarap     = PHASE_CALCUL
         phase_rezo      = PHASE_CALCUL
         phase_mascaret  = PHASE_CALCUL
         phase_post      = PHASE_CALCUL
         DTImpression    = PasImpression * DT

         if( num_pas >= PremierPasStocke ) then
            if( MOD(num_pas, PasImpression) < EPS1 ) then
               phase_post_imp = PHASE_CALCUL
            else
               phase_post_imp = PHASE_ARRET
            endif
            if( MOD(num_pas , PasStockage) < EPS1 ) then
               phase_stock = PHASE_CALCUL
            else
               phase_stock = PHASE_ARRET
            endif
         else
            phase_stock    = PHASE_ARRET
            phase_post_imp = PHASE_ARRET
         endif

      elseif( PhaseSimulation == PHASE_TERMINAISON ) then

         if( Noyau == NOYAU_SARAP ) then
            phase_planim    = PHASE_ARRET
            phase_intersect = PHASE_ARRET
            phase_planma    = PHASE_ARRET
            phase_qcl       = PHASE_ARRET
            phase_sarap     = PHASE_ARRET
            phase_rezo      = PHASE_ARRET
            phase_mascaret  = PHASE_ARRET
            phase_post      = PHASE_ARRET
            phase_post_imp  = PHASE_ARRET
            phase_stock     = PHASE_ARRET
         else
            phase_planim    = PHASE_ARRET
            phase_intersect = PHASE_ARRET
            phase_planma    = PHASE_ARRET
            phase_qcl       = PHASE_ARRET
            phase_sarap     = PHASE_ARRET
            phase_rezo      = PHASE_ARRET
            phase_mascaret  = PHASE_ARRET
            phase_post      = PHASE_ARRET
            phase_post_imp  = PHASE_ARRET
            phase_stock     = PHASE_CALCUL
            FormatResu      = FORMAT_STO_PERMANENT
            FichierResultat = FichierListing
         endif

      endif

      if( OptionCasier ) then

         select case( PhaseSimulation )

            case( PHASE_INITIALISATION )

               phase_clpluie            = PHASE_INITIALISATION
               phase_post_casier        = PHASE_ARRET
               phase_post_imp_casier    = PHASE_INITIALISATION
               phase_stock_casier       = PHASE_CALCUL

            case( PHASE_CALCUL )

               phase_clpluie            = PHASE_CALCUL
               phase_post_casier        = PHASE_CALCUL
               phase_stock_casier       = PHASE_CALCUL
               if( num_pas >= PremierPasStocke ) then
                  if( MOD(num_pas, PasImpression) < EPS1 ) then
                     phase_post_imp_casier  = PHASE_CALCUL
                  else
                     phase_post_imp_casier  = PHASE_ARRET
                  endif
               end if

            case( PHASE_TERMINAISON )
               phase_clpluie          = PHASE_ARRET
               phase_post_casier      = PHASE_ARRET
               phase_post_imp_casier  = PHASE_ARRET
               phase_stock_casier     = PHASE_ARRET
               FichierResultatCasier  = FichierListingCasier
               FichierResultatLiaison = FichierListingLiaison

         end select

      else

         select case( PhaseSimulation )

            case( PHASE_INITIALISATION )

               phase_stock_casier       = PHASE_ARRET

            case( PHASE_CALCUL )

               phase_stock_casier       = PHASE_ARRET

            case( PHASE_TERMINAISON )

               phase_stock_casier  = PHASE_ARRET

         end select

         phase_post_casier = PHASE_ARRET

      end if
!TAPENADE--
      !  traitement des options de traceur
      !
      if( OptionTracer ) then

         select case( PhaseSimulation )

            case( PHASE_INITIALISATION )
               phase_tracer        = PHASE_INITIALISATION
               PhaseCouplageTracer = PHASE_INITIALISATION

            case( PHASE_CALCUL )
               if( mod(num_pas,FreqCouplage) < EPS8 ) then
                  phase_tracer = PHASE_CALCUL
               else
                  phase_tracer = PHASE_ARRET
               endif
               phaseCouplageTracer = PHASE_CALCUL

            case( PHASE_TERMINAISON )

               PhaseCouplageTracer = PHASE_ARRET
               phase_Tracer        = PHASE_ARRET

         end select

      endif
!--TAPENADE

      ! PLANIMETRAGE
      !=============
      if( phase_planim == PHASE_CALCUL ) then

         call  PLANIM             ( &
            ProfilPlan            , & ! Profils planimetres
            Profil                , & ! Caracteristiques des profils
            F1                    , & ! Fonction Impulsion
            DebProgressifLM       , & ! Debordement progressif lit majeur
            DebProgressifZS       , & ! Debordement progressif zones de stockaage
            ImpressionPlani       , & ! Impression du planimetrage
            FichierListing%Unite  , & ! Unite logique listing
            FrottParoiVerticale   , & ! Conservation du frottement sur les parois verticales
            Erreur                  ) ! Erreur
         if( Erreur%Numero /= 0 ) then
            write(*,321)
            Print * , Erreur%Message 
            stop 1 
         endif

      endif

      ! INTERPOLATIONS AUX SECTIONS DE CALCUL
      !======================================
      if( phase_intersect == PHASE_INITIALISATION .or. phase_intersect == PHASE_CALCUL) then

         call Intersect      ( &
           ZREF              , & ! Tableau des cotes de ref aux sections
           RGC               , & ! Cotes de la rive gauche         ''
           RDC               , & ! Cotes de la rive droite         ''
           CF1               , & ! Coefficient de frottement mineur
           CF2               , & ! Coefficient de frottement majeur
           Profil            , & ! Profils geometriques
           X                 , & ! Abscisses des sections de calcul
           IDT               , & ! Positionement des sections / profils
           XDT               , & ! Positionement des sections / profils
           Connect           , & ! Connectivite du reseau
           Extremite         , & ! Extremite libre
           TypeMaillage      , & ! Type de calcul du maillage
           ImpressionPlani   , & ! flag d'impression
           FichierListing%Unite, & !
        FormatGeom          , & ! Format du fichier geometrie utilise
        InterpLinCoeffFrott , & ! Flag d'interpolation lineaire des Strickler
        phase_intersect     , & ! Phase de la simulation
        Erreur                & ! Erreur
                      )
         if( Erreur%Numero /= 0 ) then
            write(*,321) 
            Print * , Erreur%Message 
            stop 1 
         endif
!
! PLANIMETRAGE DES VARIABLES PROPRES a MASCARET
! 
         if( phase_planma == PHASE_CALCUL ) then

            nb_pas = Profil(1)%NbPas

            call PLANMA         ( &
           SectionPlan          , & ! Section planimetrees
           Profil               , & ! Caracteritiques des profils
           ProfilPlan           , & ! Profils planimetrees
           nb_pas               , & ! Nombre de pas de planimetrage
           X                    , & ! Abscisse des sections de calcul
           DZ                   , & ! Caracteristiques des sections
           XD                   , & ! Abscisse des interfaces
           DZD                  , & ! Pas de planimetrage des interfaces
           XDT                  , & ! Position relative de la section/Profil
           IDT                  , & ! Profil de donnees amont de la section
           Connect              , & ! Connectivite du reseau
           CF1                  , & ! Strickler mineur
           CF2                  , & ! Strickler majeur
           PresenceZoneStockage , & ! Presence de zone de stockage
           LoiFrottement        , & ! Loi de frottement utilisee
           Erreur               )

            if( Erreur%Numero /= 0 ) then
               write(*,321)
               Print * , Erreur%Message 
               stop 1 
            endif

         endif

      endif

      ! CALCUL DES APPORTS
      !===================
      if( phase_qcl == PHASE_INITIALISATION .or. phase_qcl == PHASE_CALCUL ) then
         if( noyau == NOYAU_MASCARET .and. phase_qcl == PHASE_INITIALISATION ) then
            Temps1 = Temps + DT
         else
            Temps1 = Temps
         endif
         call QCL             ( &
              Apport          , & ! tableau des Apports
              Singularite     , & ! tableau des singularites
              Extremite       , & ! tableau des Extremites libres
              LoiHydrau       , & ! tableau des lois hydrauliques
              Temps1          , & ! Temps
              Num_pas         , & ! Numero du pas de temps
              Q1              , & ! Debits mineurs dans les sections de calcul
              Froude          , & ! Nombre de Froude
              Connect         , & ! Connectivite du reseau
              Noyau           , & ! Noyau de calcul utilise
              Erreur            ) ! Erreur

         CALL CPU_TIME( t1 )

         if( Erreur%Numero /= 0 ) then
            write(*,321)
            Print * , Erreur%Message 
            stop 1 
         endif
      endif
!TAPENADE--
   !================================================
   !
   ! Calage automatique du coefficient de Strickler
   !
   !================================================

   ! allocation des donnees sur un cas schematique
   if( OptionCalage ) then
      if( Phase_sarap == PHASE_INITIALISATION ) then
         !
         !  Boucle sur le nombre de crues de calage
         !
         nb_crue = Calage_crues%Nb_crue
         allocate( Calage_crues%Apport( nb_crue , nb_sect) , STAT = retour )
         if( retour /= 0 ) call err_alloc('Calage_crues')

         Do i = 1 , Calage_crues%Nb_crue
            !
            !  Conditions hydrauliques pour chaque crue de calage
            !
            nbap = Calage_crues%NbApports(i)
            !
            allocate( Apport_cal(Nbap) , STAT = retour )
            if( retour /= 0 ) call err_alloc('Apport_cal')

            Do j = 1 , NbAp
               Apport_cal(j)%Debit    = Calage_crues%Apport_X(i,j)
               Apport_cal(j)%Longueur = 0.D0
               abs_abs                = Calage_crues%Abscisse(i,j)
               call XINDIC_S( Apport_cal(j)%SectionAm , abs_abs , X , Erreur )
            enddo

            call CQINJ                 ( &
                 QInjec                , & ! Resultats
                 X, Z                  , & ! Donnees non modifiees
                 Apport_cal            , &
                 Deversoir             , &
                 Qdeverse              , &
                 Erreur                  ) ! Erreur

            do j = 1 , size (X)
               Calage_crues%Apport(i,j) = Qinjec(j)
            enddo

            deallocate(Apport_cal)

         enddo

         Impression = .true.
         
          call CALAGE_N2QN1              ( &
                           Z            , & ! Cote de la surface libre
                           Q1           , & ! Debit mineur
                           Q2           , & ! Debit majeur
                           Qinjec       , & ! Qinject
                           PCSing       , & ! Pertes de charge singulieres
                           Impression     & ! Flag d'impression
                                      ) 

         if( Erreur%Numero /= 0 ) then
            write(*,321)
            print * , Erreur%Message 
            stop 1 
         endif


      endif

   endif
!--TAPENADE

   if( OptionCasier ) then

      if( phase_clpluie == PHASE_CALCUL ) then

         call CLPLUIE (       &
                ApportPluie  ,& ! resultat, debit d apport du temps T-DT au temps T
                Temps, DT    ,& ! variable temps et pas de temps du calcul
                LoiHydrau    ,& ! hydrogramme de pluie
                Erreur         )! erreur

         if( Erreur%Numero /= 0 ) then
            write(*,321)
            Print * , Erreur%Message 
            stop 1 
         endif

      end if

   end if

   ! SLECTION DU NOYAU DE CALCUL
   !============================
   
      select case( Noyau )

         case( NOYAU_SARAP )

            if( phase_sarap == PHASE_CALCUL ) then

               call SARAP         ( &
               ! Donnees/Resultats
                     Z            , & ! Cote de la surface libre
                     Q1           , & ! Debit mineur
                     Q2           , & ! Debit majeur
                     P1           , & ! Perimetre mouille mineur
                     P2           , & ! Perimetre mouille majeur
                     B1           , & ! Largeur au miroir mineur
                     B2           , & ! Largeur au miroir majeur
                     BS           , & ! Largeur au miroir zone de stockage
                     RH1          , & ! Rayon hydraulique mineur
                     RH2          , & ! Rayon hydraulique majeur
                     S1           , & ! Section mouillee mineur
                     S2           , & ! Section mouillee majeur
                     Beta         , & ! Coefficient du modele Debord
                     Froude       , & ! Nombre de Froude
                     Extremite    , & ! Conditions aux limites
                     Apport       , & ! Apports
                     Qinjec       , & ! Qinjecte
                     Qdeverse     , & ! Qdeverse
                     Temps        , & ! Temps
                     Profil       , & ! Profils geometriques
                     ProfilPlan   , & ! Profils planimetrees
                     F1           , &
                     X            , & ! Maillage
                     CF1          , & ! Strickler mineur
                     CF2          , & ! Strickler majeur
                     ZREF         , & ! Cote de reference
                     XDT          , & ! Position section/profil amont
                     IDT          , & ! Numero du profil amont
                     Connect      , & ! Table de connectivite
                     Singularite  , & ! Singularites (seuils)
                     PCSing       , & ! Pertes de charge singulieres
                     Deversoir    , & ! Deversoirs
                     ModeleLit    , & ! Modelisation lit
                     Confluent    , & ! Caracteristiques des confluences
                     Abaque       , & ! Abaques des pertes de  charges aux confluences
                     Algorithme   , & ! Algorithme de parcours des biefs
                     ImpressionCalcul    , & ! Flag d'impression
                     FichierListing%Unite, & !
                     LoiFrottement       , & ! Type de lois de frottement utilisee
                     PerteChargeConfluent, & ! Flag de perte de charge auto aux confluents
                     CQMV                , & ! qmv debits d'apport
                     Erreur                & ! Erreur
                                )

               if( Erreur%Numero /= 0 ) then
                  write(*,321)
                  Print * , Erreur%Message  
                  stop 1 
               endif

            endif

         case( NOYAU_REZODT )

            if( phase_rezo == PHASE_INITIALISATION .or. phase_rezo == PHASE_CALCUL ) then

               call        REZO    ( &
                   Z               , & ! Cote de la surface libre
                   Q1, Q2          , & ! Debits mineur et majeur
                   P1, P2          , & ! Perimetres mouilles mineur et majeur
                   B1, B2, BS      , & ! Largeurs au miroir mineur, majeur et de stockage
                   RH1, RH2        , & ! Rayons hydrauliques mineur et majeur
                   S1, S2          , & ! Sections mouillee mineur et majeur
                   DTLEVY          , & ! Pas de temps optimal
                   Beta            , & ! Coefficient Beta de repartition des lits
                   Froude          , & ! Nombre de Froude
                   Extremite       , & ! Extremites libres
                   Apport          , & ! Debits d'apport
                   Qinjec          , & ! Debit injecte
                   Qdeverse        , & ! debit total deverse par un deversoir lateral ponctuel ou lineique
                   Temps           , & ! Temps
                   PhaseSimulation , & ! Phase de la simulation
                   Profil          , & ! Profils geometriques
                   ProfilPlan      , & ! Profils planimetrees
                   X               , & ! Maillage
                   CF1, CF2        , & ! Coefficients de frottement mineur et majeur
                   ZREF            , & ! Cote de reference
                   XDT             , & ! Position section/profil amont
                   IDT             , & ! Numero du profil amont d'une section
                   Connect         , & ! Table de connectivite
                   Singularite     , & ! Singularites
                   PCSing          , & ! Pertes de charges singulieres
                   Deversoir       , & ! Deversoirs
                   ModeleLit       , & ! Modele du lit
                   Confluent       , & ! Caracteristiques des confluences
                   Abaque          , & ! Abaques des pertes de  charges aux confluences
                   DTImpression    , & ! Pas de temps d'impression
                   ImpressionCalcul    , & ! Flag d'autorisation d'impression
                   FichierListing%Unite, & ! unite logique du fichier listing
                   LoiFrottement       , & ! Loi de frottement
                   PerteChargeConfluent, & !
                   TempsPrecedent   , & ! Temps precedent
                   TempsInitial     , & ! Temps de debut de simulation
                   num_pas          , & ! Numero du pas de temps
                   DPDZ1, DPDZ2     , & ! Derivee de P1 et de P2 / Z
                   OptionCasier     , & ! Flag de presence de casiers
                   Liaison          , & ! Caracteristiques des liaisons RIVIERE-CASIER et CASIER-CASIER
                   Casier           , & ! Caracteristiques des casiers
                   ApportPluie      , & ! Apport de pluie des casiers
                   NoConvection     , & ! Attenuation de la convection
                   CQMV             , & ! qmv debits d'apport
                   Erreur             & ! Erreur
                               )

               if( Erreur%Numero /= 0 ) then
                  write(*,321)
                  Print * , Erreur%Message  
                  stop 1 
               endif

            endif
!TAPENADE--
         case( NOYAU_MASCARET )

            if( phase_mascaret == PHASE_INITIALISATION .or. phase_mascaret == PHASE_CALCUL ) then
               call MASCARET            ( &
                    Z                   , & ! Cote
                    Q1, Q2              , & ! debits  mineur, majeur
                    S1, S2              , & ! section mineur, majeur
                    W   , AIRS          , & ! Etats du 2D pour les confluents
                    YNODE,UNODE,CNODE   , & ! Etats pour Mascaret
                    FLUX                , & ! Flux pour solveur de Roe
                    DebitFlux           , & ! Flux de masse
                 JGNODE, JDNODE ,IFIGE  , & ! Indices de planimetrage
                    BETA                , & ! Coefficient de repartition mineur/majeur
                    Froude              , & ! Nombre de Froude
                    XFRON               , & ! Abscisse du front d'onde
                    DT ,Temps ,num_pas  , & ! Pas de temps optimal,Temps
                    NbPasTemps          , & ! Nombre de pas de temps max
                    TempsMaximum        , & ! Temps maximun
                    Extremite, Apport   , & ! Extremites libres,Debits d'apports
                    Qinjec              , & ! Debit injecte
                    Qdeverse            , & ! Debit au deversoir
                    PhaseSimulation     , & !
                    Phase_post_imp      , &
                    DZ ,DZD             , &
                    XD                  , &
                    nb_pas              , &
                    SectionPlan         , &
                    X, CF1 , CF2, ZREF  , & ! Cote du fond
                    nb_sect             , & ! nombre de sections
                    ZoneSeche           , & ! Zone seche
                    Connect             , & ! Table de connectivite
                    Singularite         , & ! Singularites
                    Barrage             , & ! XBARP,ZBARP
                    PCSing              , & ! Pertes de charges singulieres
                    Deversoir           , & ! Deversoirs
                    Confluent           , & ! Confluents 2D
                    SVRAI, QVRAI, ZVRAI , & ! VALIDATION
                    ZINIT               , & ! COTE INITIALE
                    HEPS  , SEPS ,GPES  , & ! Hauteur et section minimale, acceleration pesant
                    OndeSubm            , &
       CalculValidation,TypeValidation  , & ! Indicateur et numero de validation
                    RepriseCalcul       , & ! Indicateur de reprise de calcul
                    FroudeLim           , & ! Indicateur de condition aux limites
                    FrottementImplicite , & ! Indicateur pour l'impliciation du frottement
                    Impli_Trans  ,  Opt , & ! Indicateur pour l'implicitation du solveur
                PerteElargissementTrans , & ! Perte de charge aux elargissements
                             Boussinesq , & ! Prise en compte de termes non hydrostatiques
                             CQMV       , & ! qmv debits d'apport
                PresenceZoneStockage    , & ! Indicateur de zones de stockage
                    PastempsVariable    , & ! Indicateur de pas de temps variable
                    CourantObj          , & ! Nombre de Courant limite
                    ImpressionCalcul    , & ! Flag d'impression
                    FichierListing%Unite, & ! Unite logique fichier listing
                    VOLS , Sauve , Erreur  ) ! apport

               if( Erreur%Numero /= 0 ) then
                  write(*,321)
                  Print * , Erreur%Message  
                  stop 1 
               endif

               if( OptionTracer ) then
                  ! Pour Tracer : re-calcul du rayon hydraulique
                  do j = 1,size(Connect%OrigineBief)

                     l = Connect%OrigineBief(j)
                     m = Connect%FinBief(j)

                     do I = l , m

                        call RHSBP_S         ( &
                             B1(I)           , &
                             B2(I)           , &
                             BS(I)           , &
                             P1(I)           , &
                             P2(I)           , &
                             S1(I)           , &
                             S2(I)           , &
                             RH1(I)          , &
                             RH2(I)          , &
                             I               , &
                             Z(I)            , &
                             ZREF(I)         , &
                             IDT             , &
                             XDT             , &
                             Profil          , &
                             ProfilPlan      , &
                             UniteListing    , &
                             Erreur            &
                                   )

                        if( Erreur%Numero /= 0 ) then
                           write(*,321)
                           Print * , Erreur%Message  
                           stop 1 
                        end if

                     enddo

                  enddo

               endif

           end if
!--TAPENADE
      end select
!TAPENADE--
   ! CALCUL DES VARIABLES COMPLEMENTAIRES
   !=====================================
   if( phase_post == PHASE_INITIALISATION .or. phase_post == PHASE_CALCUL ) then

      call       POST ( TAUF                      , &
                        Y           , HMOY        , &
                        Q2G         , Q2D         , &
                        VOL         , VOLS        , &
                        CHARG                     , &
                        SS                        , &
                        V1          , V2          , &
                        ZMAX        , TZMAX       , &
                        VZMAX                     , &
                        ZMIN        , TZMIN       , &
                        V1MIN       , V1MAX       , &
                        BMAX                      , &
                        TOND                      , &
                        QMAX        , TQMAX       , &
                        EMAX                      , &
                        Z                         , &
                        Q1          , Q2          , &
                        S1          , S2          , &
                        B1          , B2          , &
                        BS                        , &
                        P1          , P2          , &
                        RH1         , RH2         , &
                        BETA                      , &
                        Profil                    , &
                        ProfilPlan                , &
                        Temps, TempsInitial       , &
                        num_pas                   , &
                        X                         , &
                        ZREF                      , &
                        IDT         , XDT         , &
                        CF1                       , &
                        ZInitial                  , &
                        Noyau                     , &
                        PresenceZoneStockage      , &
                        DebProgressifLM           , &
                        DZArriveeFront            , &
                        PhaseSimulation           , &
                        VarCalc                   , &
                        LoiFrottement             , &
                        RepriseCalcul             , &
                        FichierRepriseLec         , &
                        Erreur                      &
                                          )

      if( Erreur%Numero /= 0 ) then
         write(*,321)
         Print * , Erreur%Message  
         stop 1 
      endif

   endif

   if( OptionCasier ) then
       if( phase_post_casier == PHASE_INITIALISATION .or. phase_post_casier == PHASE_CALCUL ) then

          call POST_CASIER ( &
                    Casier  ,&
                    Liaison ,&
                    TEMPS   )

       end if
   endif

   if( phase_post_imp == PHASE_INITIALISATION .or. phase_post_imp == PHASE_CALCUL ) then

      Q(:) = Q1(:) + Q2(:)

      call     POST_IMP ( &
        X, ZREF         , & ! Maillage et cotes de reference
        RGC, RDC        , & ! Rives gauche et droite
        CF1, CF2        , & ! Coeff de frottement mineur et majeur
     Z, Q, Q1, Q2       , & ! Cote debits mineur et majeur
         DebitFlux      , &  ! Flux de masse
        S1, S2          , & ! Sections mineur et majeur
        B1, B2, BS      , & ! Largeurs au miroir mineur, majeur et de stockage
        P1, P2          , & ! Perimetres mouillees mineur et majeur
        RH1, RH2        , & ! Rayons hydrauliques mineur et majeur
        Froude, Beta    , & ! Froude et BETA
        TAUF            , & ! Contrainte au fond
        Y, HMOY         , & ! Hauteur d'eau et hauteur d'eau moyenne
        Q2G, Q2D        , & ! Debits majeur droit et gauche
        VOL, VOLS       , & ! Volumes lit actif et zone de stockage
        CHARG           , & ! Charge
        SS, V1, V2      , & ! Vitesse mineur et majeur
   ZMAX, TZMAX  , VZMAX , & ! Cote max et temps associe
        ZMIN, TZMIN     , & ! Cote min et temps associe
        V1MIN, V1MAX    , & ! Vitesse mineur min et max
        BMAX            , & ! Largeur au miroir max
        TOND            , & ! Temps d'arrivee de l'onde
        QMAX, TQMAX     , & ! Debit max et temps associe
        EMAX            , & ! Energie maximale
        ZVRAI , QVRAI   , & ! Solutions analytiques
        Qdeverse        , & ! DEbit deverse
        Temps           , & ! Temps courant
        Apport          , & ! Debits d'Apports
        PasTempsOptimal , & ! Pas de temps optimal
        Connect         , & ! Connectivite du reseau
        ModeleLit       , & ! Modele du lit (Debord/Crugos)
        num_pas         , & ! Numero du pas de temps
        Noyau           , & ! Noyau de calcul utilise
        PhaseSimulation , & ! Phase Initialisation/Calcul
        ImpressionCalcul, & ! ImpressionCalcul
        Regime          , & ! Regime Permanent / Non Permanent
        VarCalc         , & ! Variables a imprimer
  FichierListing%Unite  , & ! Unite logique listing
     TempsPrecedent     , & ! Temps precedent
  VolBiefActif, VolBiefStockage, & ! Volumes actifs et de stockage
  QAmontPrec, QAvalPrec        , & ! Debits amont et aval des biefs
  Erreur                         & ! Erreur
                                 )

      if( Erreur%Numero /= 0 ) then
         write(*,321)
         Print * , Erreur%Message  
         stop 1 
      endif

   endif

   if( OptionCasier ) then
       if( phase_post_imp_casier == PHASE_INITIALISATION .or. phase_post_imp_casier == PHASE_CALCUL ) then

          call POST_IMP_CASIER(              &
                       Casier,               &
                       FichierListingCasier, &
                       Liaison,              &
                       FichierListingLiaison,&
                       TEMPS,                &
                       PhaseSimulation,      &
                       Erreur                 )

          if( Erreur%Numero /= 0 ) then
             write(*,321)
             Print * , Erreur%Message  
             stop 1 
          endif

       end if
   endif

   if( phase_stock == PHASE_INITIALISATION .or. phase_stock == PHASE_CALCUL ) then

      Q (:) = Q1(:)+ Q2(:)

      call STOCK       ( X               , &
                         ZREF            , &
                         RGC  , RDC      , &
                         CF1  , CF2      , &
                         Z               , &
                         Q               , &
                         Q1   , Q2       , &
                         DebitFlux       , &
                         S1   , S2       , &
                         B1   , B2       , &
                         BS              , &
                         P1   , P2       , &
                         RH1  , RH2      , &
                         Froude, BETA    , &
                         TAUF            , &
                         Y    , HMOY     , &
                         Q2G  , Q2D      , &
                         VOL  , VOLS     , &
                         CHARG           , &
                         SS              , &
                         V1   , V2       , &
                         ZMAX , TZMAX    , &
                         VZMAX           , &
                         ZMIN , TZMIN    , &
                         V1MIN, V1MAX    , &
                         BMAX            , &
                         TOND            , &
                         QMAX , TQMAX    , &
                         EMAX            , &
                         ZVRAI , QVRAI   , &
                         Qdeverse        , &
                         TEMPS           , &
                         Connect         , &
                         Casier          , &
                         Liaison         , &
                         FichierResultat , &
                         FichierResultat2, &
                   FichierResultatCasier , &
                   FichierResultatLiaison, &
                         OptionStockage  , &
                         FormatResu      , &
                         FormatResu2     , &
                         phase_stock     , &
                   phase_stock_casier    , &
                         num_pas         , &
                         VarSto          , &
                         SectionStockage , &
                         FichierMotCle   , &
                         Erreur          )
      if (Erreur%Numero /= 0) then
         write(*,321)
         Print * , Erreur%Message  
         stop 1
      endif

   endif

   !
   ! Couplage avec le traceur
   !
   if( OptionTracer ) then

      if( (Phase_tracer == PHASE_INITIALISATION ).OR.( Phase_tracer == PHASE_CALCUL ) ) then
         !
         !  CALCUL DES SOURCES INTERNES ET DE LA CONDITION LIMITE AMONT
         !
         call QCL_TRACER( &
            CondLimTrac , & ! Conditions aux limites Tracer
         Sources_Tracer , & ! Sources de traceurs ajoutees
              Extremite , & ! Extremite du reseau
              LoiTracer , & ! Lois Tracer pour CL et sources
                 Nbtrac , & ! Nombre de traceurs
                  Temps , & ! Temps courant
                 Erreur )
         if( Erreur%Numero /= 0 ) then
            write(*,321)
            Print * , Erreur%Message  
            stop 1
         endif
         !
         !  Equations de transport diffusion du traceur
         !
         QT(:) = Q1(:) + Q2(:)
         ST(:) = S1(:) + S2(:)
         BT(:) = B1(:) + B2(:)
         
         if( Temps.le.( TempsInitial + DT ) ) then
            QT_ANT(:) = QT(:)
            ST_ANT(:) = ST(:)
            BT_ANT(:) = BT(:)
         endif

         call TRACER(                  &
                            CTraceur , & ! Concentrations en traceurs
                        QT , ST , BT , & ! Debit, section mouillee et largeur totaux (pour Tracer HYP1FA)
            QT_ANT , ST_ANT , BT_ANT , & ! Debit, section mouillee et largeur totaux (pour Tracer VF)
                           RH1 , CF1 , & ! - lit mineur
                              QINJEC , & ! Debits d'apport
                                ZREF , & ! Cote du fond
                         CondLimTrac , & ! Conditions limites des traceurs
                      Sources_Tracer , & ! Sources ajoutees
                            ConsTrac , & ! Constantes lies au transport-diffusion
                     Modele_Qual_Eau , & ! Modele de qualite d'eau choisi
                              ParPhy , & ! Parametres lies au modele de QE
                               Meteo , & ! Donnees meteo
                                   X , & ! Abscisses des sections de calcul
                              Nbtrac , & ! Nombre de traceurs
                             nb_sect , & ! Dimension spatiale des tableaux 
                         SINGULARITE , & ! Singularites
                             Connect , & ! Table de connectivite
                             message , & !
                               Temps , & ! Temps
              MASS , FLUMAS , FLUENT , & ! Donnees du bilan de masse
                     FLUSOR , FLUSRC , & ! (masse et flux E-S)
                             DT_Trac , & ! Pas de temps Tracer
                        PHASE_TRACER , & ! Phase du calcul Tracer
          FichierListingTracer%Unite , & ! Unite du fichier listing Tracer 
               ImpressionBilanTracer , & ! Logique pour calcul du bilan
                           NbCourant , & ! Nombre de courant max
                              ERREUR )
         if( Erreur%Numero /= 0 ) then
            write(*,321)
            Print * , Erreur%Message  
            stop 1
         endif
         
         QT_ANT(:) = QT(:)
         ST_ANT(:) = ST(:)
         BT_ANT(:) = BT(:)
         
         if( phase_post_imp == PHASE_INITIALISATION .or. phase_post_imp == PHASE_CALCUL ) then

            call POST_IMP_TRACER( &
                              X , & ! Abscisse des sections de calcul
              CTraceur , Nbtrac , & ! Concentrations en traceur
                  MASS , FLUMAS , & ! Masse de traceur
                FLUENT , FLUSOR , & ! Flux de traceur
                         FLUSRC , &
                      NbCourant , & ! Nombre de courant max
                        Connect , & ! Dimension spatiale
           FichierListingTracer , &
          ImpressionConcListing , & ! Logique pour les impressions
          ImpressionBilanTracer , &
                          TEMPS , &
                 Phase_post_imp , &
                         Erreur )

               if( Erreur%Numero /= 0 ) then
                  write(*,321)
                  Print * , Erreur%Message  
                  stop 1 
               endif

         endif

         if( phase_stock == PHASE_INITIALISATION .or. phase_stock == PHASE_CALCUL ) then

            VarStoTracer(VARTR_X)    = .true.
            VarStoTracer(VARTR_ZREF) = .true.
            VarStoTracer(VARTR_Q)    = .true.
            VarStoTracer(VARTR_A)    = .true.

            do i = 1 , Nbtrac
               VarStoTracer(VARTR_CONC(i))=.true.
            enddo

            call STOCK_TRACER             &
                    ( X                 , &
                      ZREF              , &
                      QT , ST           , &
                      Ctraceur          , &
                      Nbtrac            , &
                      TEMPS             , &
                      Connect           , &
                      FichierResuTracer , &
                      OptionStockage    , &
                      FormatResuTracer  , &
                      phase_stock       , &
                      num_pas           , &
                      VarStoTracer      , &
                      SectionStockage   , &
                      FichierMotCle     , &
                      Erreur              )
            if( Erreur%Numero /= 0 ) then
               write(*,321)
               Print * , Erreur%Message  
               stop 1
            endif

         endif

      endif

   endif
!--TAPENADE
   ! en NP, j'ai besoin de faire NbPas sans compter le pas n0 0
   ! qui est necessaire a REZO
   ! En P, j'ai besoin de faire NbPas en commencant a 1

   if( Noyau == NOYAU_SARAP .and. PhaseSimulation == PHASE_INITIALISATION ) then
      Temps = TempsInitial
!TAPENADE--
      if( OptionTracer ) then
         Temps = Temps + DT
         TempsPrecedent = Temps
      endif
!--TAPENADE
   else
!TAPENADE--
      if( OptionTracer ) phase_sarap = phase_arret
!--TAPENADE
      TempsPrecedent = Temps
      Temps          = Temps + DT
   endif

   num_pas = num_pas + 1

   select case( PhaseSimulation )

      case( PHASE_INITIALISATION )

         PhaseSimulation = PHASE_CALCUL

      case( PHASE_TERMINAISON )

         ! stockage des variables pour la reprise de calculs au format LIDO permanent
!TAPENADE--
         If( Noyau == NOYAU_MASCARET ) then

            call STOCK_REP   ( X               , &
                               ZREF            , &
                               RGC  , RDC      , &
                               CF1  , CF2      , &
                               Z               , &
                               Q               , &
                               Q1   , Q2       , &
                               DebitFlux       , &
                               S1   , S2       , &
                               B1   , B2       , &
                               BS              , &
                               P1   , P2       , &
                               RH1  , RH2      , &
                               Froude, BETA    , &
                               TAUF            , &
                               Y    , HMOY     , &
                               Q2G  , Q2D      , &
                               VOL  , VOLS     , &
                               CHARG           , &
                               SS              , &
                               V1   , V2       , &
                               ZMAX , TZMAX    , &
                               VZMAX           , &
                               ZMIN , TZMIN    , &
                               V1MIN, V1MAX    , &
                               BMAX            , &
                               TOND            , &
                               QMAX , TQMAX    , &
                               EMAX            , &
                               ZVRAI , QVRAI   , &
                               XFRON           , &
                               Qdeverse        , &
                               TEMPS           , &
                               Connect         , &
                            FichierRepriseEcr  , &
                               VarSto          , &
                               SectionStockage , &
                               Erreur          )
            if( Erreur%Numero /= 0 ) then
               write(*,321)
               Print * , Erreur%Message  
               stop 1
            endif
         endif
!--TAPENADE
         exit

      case( PHASE_CALCUL )

         ! calcul de l'etat d'avancement du calcul pour impression dans le fichier de controle tous les 10%
         if( CritereArret == TEMPS_MAXIMUM ) then
            POURC = (Temps-TempsInitial) / (TempsMaximum-TempsInitial)
         else
            XN1   = num_pas
            XN2   = NbPasTemps
            POURC = XN1 / XN2
         endif

         imp_avancement = .false.
         if( POURC.GT.avancement ) then
            avancement     = avancement + 1.d-1
            imp_avancement = .true.
         endif

         if( imp_avancement ) then
            open(unit=FichierControle%Unite, file=FichierControle%Nom, access='SEQUENTIAL', &
                 action='WRITE'   , form='FORMATTED'  , iostat=RETOUR      , &
                 position='rewind', status='replace'      )
            if( RETOUR /= 0 ) then
               Erreur%Numero = 4
               Erreur%ft     = err_4
               Erreur%ft_c   = err_4c
               call TRAITER_ERREUR( Erreur , FichierControle%Nom )
               stop
            end if
            Write (FichierControle%Unite, *) 'POURCENTAGE DU CALCUL',avancement
            close(FichierControle%Unite)
         endif

         if( ( CritereArret == TEMPS_MAXIMUM .and. Temps > TempsMaximum ) .or. &
             ( CritereArret == NOMBRE_DE_PAS_TEMPS_MAXIMUM .and. num_pas > NbPasTemps) .or. &
             ( CritereArret == COTE_MAXIMALE_AU_POINT_DE_CONTROLE .and. &
             ( Z(Section_controle) > cote_max_controle .or. num_pas > NbPasTemps ) ) ) then

            PhaseSimulation = PHASE_TERMINAISON
            CALL CPU_TIME (t1)
            T1 = t1 - t0
            print *, trim(messim)//' ended successfully in ',T1,' s'

         endif

   end select

   end do     ! Fin boucle calcul

   !Desallocations
   deallocate(Q1)
   deallocate(Qdeverse)
   deallocate(Qinjec)
   deallocate(Q2)
   deallocate(S1)
   deallocate(S2)
   deallocate(BETA)
   deallocate(Froude)
   deallocate(FLUX)
   deallocate(DebitFlux)
   deallocate(P1)
   deallocate(P2)
   deallocate(B1)
   deallocate(B2)
   deallocate(BS)
   deallocate(RH1)
   deallocate(RH2)
   deallocate(SVRAI)
   deallocate(QVRAI)
   deallocate(ZVRAI)
   deallocate(ZINIT)
   deallocate(UNODE)
   deallocate(CNODE)
   deallocate(YNODE)
   deallocate(JGNODE)
   deallocate(JDNODE)
   deallocate(IFIGE)
   deallocate(W)
   deallocate(AIRS)
   if( Noyau == NOYAU_MASCARET ) then
      deallocate(XFRON)
      deallocate(IFIN)
   endif
!TAPENADE--
   if( OptionTracer ) then
       deallocate(QT)
       deallocate(ST)
       deallocate(BT)
       deallocate(QT_ANT)
       deallocate(ST_ANT)
       deallocate(BT_ANT)
       deallocate(NbCourant)
       deallocate(MASS)
       deallocate(FLUMAS)
       deallocate(FLUENT)
       deallocate(FLUSOR)
       deallocate(FLUSRC)
   endif
!--TAPENADE
   write(*,*) 
   321 format(/,"===========",/,"=> ERROR <=",/,"===========",/)

end program HOMERE_MASCARET
