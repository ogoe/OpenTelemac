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

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!                            F. ZAOUI
!
! VERSION : 8.1.3              EDF-CEREMA
! *********************************************************************
subroutine  PRETRAIT_INTERFACE                             ( &

  VersionCode, Noyau                                       , &
  FichierModele, FichierMotCle                             , &
  OptionCasier                                             , &
  OndeSubm                                                 , &
  CalculValidation, TypeValidation                         , &
  Regime, ModeleLit                                        , &
  FrottParoiVerticale, PerteChargeConfluent                , &
  DebProgressifLM, DebProgressifZS                         , &
  DZArriveeFront                                           , &
  FroudeLim, FrottementImplicite, Impli_Trans ,Opt         , &
  PerteElargissementTrans, Boussinesq , NoConvection, CQMV , &
  ProfAbs, HEPS                                            , &
  DT, TempsInitial, CritereArret, NbPasTemps, TempsMaximum , &
  PasTempsVariable, CourantObj                             , &
  FichierGeom, FormatGeom, Profil, PresenceZoneStockage    , &
  X, IDT, XDT                                              , &
  FichierMaillage, FichierSauveMaillage, TypeMaillage      , &
  Connect                                                  , &
  CF1, CF2, InterpLinCoeffFrott, LoiFrottement             , &
  RepriseCalcul                                            , &
  FichierRepriseEcr, FichierRepriseLec                     , &
  FichierLigne                                             , &
  ZoneSeche                                                , &
  ZoneFrot                                                 , &
  TitreCas                                                 , &
  ImpressionPlani, ImpressionCalcul                        , &
  PasStockage, PasImpression                               , &
  PremierPasStocke                                         , &
  FichierResultat, FormatResu, FichierResultat2, FormatResu2 ,&
  FichierListing                                           , &
  VarCalc, VarSto                                          , &
  OptionStockage, SectionStockage                          , &
  LoiHydrau, FichierLoiHydrau                              , &
  Barrage, Singularite, PCSing                             , &
  Apport, Deversoir                                        , &
  Confluent, Extremite, Algorithme, Abaque                 , &
  Casier,             &  ! tableau des casiers
  Liaison,            &  ! tableau des liaisons
  ApportPluie,        &  ! tableau des apports de pluie
  ProfDebBief, ProfFinBief, absc_rel_ext_deb_bief,  absc_rel_ext_fin_bief, &
  FichierResultatCasier,& ! fichier des resultats des caracteristiques Casier
  FichierResultatLiaison,& ! fichier des resultats des caracteristiques Liaison
  FichierListingCasier ,&
  FichierListingLiaison,&
  FichierGeomCasier,	&
  Erreur, &
  FichiersLois, Impression )

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE - S. MANDELKERN - N. GOUTAL
!
! VERSION : 8.1.3              EDF-CEREMA
! *********************************************************************
!  FONCTION : LECTURE DU FICHIER CAS PAR APPEL DU LOGICIEL DAMOCLES.
!----------------------------------------------------------------------
!
! SOUS-POGRAMME APPELANT : - IMPORT_MODELE_MASCARET
!
!
!**********************************************************************

   !=========================== Declarations ================================
use M_PRECISION

use M_APPORT_T            ! Type APPORT_T
use M_BARRAGE_T           ! Type BARRAGE_T
use M_CONFLUENT_T         ! Type CONFLUENT_T
use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
use M_DEVERSOIR_T         ! Type DEVERSOIR_T
use M_ERREUR_T            ! Type ERREUR_T
use M_EXTREMITE_T         ! Type EXTREMITE_T
use M_FICHIER_T           ! Type FICHIER_T
use M_LOI_T               ! Types LOI_T
use M_PROFIL_T            ! Type  PROFIL_T
use M_SINGULARITE_T       ! Type SINGULARITE_T
use M_ZONE_SECHE_T        ! Type ZONE_SECHE_T
use M_ZONE_FROT_T         ! Type Zone_Frot

use M_INDEX_VARIABLE_C    ! Index des variables
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_MESSAGE_C           ! Messages d'erreur
use M_PARAMETRE_C         ! EPS2

use M_ALGOP_I
use M_CALC_CONNECT_I
use M_CALC_MAILLAGE_I
use M_LEC_APPORT_I
use M_LEC_BARRAGE_I
use M_LEC_CONFLUENT_I
use M_LEC_DEVER_I
use M_LEC_FROTTEMENT_I
use M_LEC_GEOM_I          ! Interface de sous-programme
use M_LEC_LIGNE_I         ! Interface de sous-programme
use M_LEC_LOI_INTERFACE_I
use M_LEC_PCSING_I
use M_LEC_PLANIM_I
use M_LEC_RESEAU_I
use M_LEC_SING_I
use M_LEC_SORTIES_I
use M_LEC_STOCKAGE_I
use M_LEC_ZONE_SECHE_I

use M_TRAITER_ERREUR_I    ! Traitement de l'errreur

use M_XINDIC_S            ! Calc de l'indice corresp a une absc
use M_DATE_S              ! Calcul de la date et de l'heure
use M_ABS_ABS_S           ! Calcul de l'abscisse absolue

use M_CASIER_T       ! type Casier
use M_LIAISON_T      ! type Liaison
use M_APPORT_PLUIE_T ! type Apport de Pluie

use M_LEC_CASIER_I          ! interface du sous-programme Lec_Casier
use M_LEC_LIAISON_I         ! interface du sous-programme Lec_Liaison
use M_LEC_APPORT_PLUIE_I    ! interface du sous-programme Lec_Apport_Pluie
!use M_TRAITER_ERREUR_CASIER_I      ! traitements des erreurs

use M_CONSTANTES_CASIER_C   ! constantes de calcul propres a CASIER
!use M_MESSAGE_CASIER_C      ! messages d erreur propres a CASIER
use Fox_dom                 ! parser XML Fortran


!.. Implicit Declarations ..
  implicit none

! Parametres generaux

  integer        , intent(  out) :: VersionCode
  integer        , intent(  out) :: Noyau
  type(FICHIER_T), intent(inout) :: FichierModele
  type(FICHIER_T), intent(inout) :: FichierMotCle
  logical        , intent(  out) :: OptionCasier
  logical        , intent(  out) :: OndeSubm
  logical        , intent(  out) :: CalculValidation
  logical        , intent(  out) :: PerteChargeConfluent
  integer        , intent(  out) :: TypeValidation
  character(LEN=255)             :: nom_fortran
  character(LEN=255)             :: nom_bibli

! Modelisation physique

  integer     , intent(  out) :: Regime
  integer     , intent(  out) :: ModeleLit
  logical     , intent(  out) :: FrottParoiVerticale
  logical     , intent(  out) :: DebProgressifLM
  logical     , intent(  out) :: DebProgressifZS
  real(DOUBLE), intent(  out) :: FroudeLim
  Logical     , intent(  out) :: FrottementImplicite
  Logical     , intent(  out) :: PerteElargissementTrans
  Logical     , intent(  out) :: Boussinesq
  Logical     , intent(  out) :: NoConvection
  Integer     , intent(  out) :: CQMV
  Logical     , intent(  out) :: Impli_Trans,Opt
  real(DOUBLE), intent(  out) :: HEPS
  real(DOUBLE), intent(  out) :: DZArriveeFront
  logical     , intent(  out) :: InterpLinCoeffFrott

! Parametres temporels

  real(DOUBLE), intent(  out) :: DT
  real(DOUBLE), intent(  out) :: TempsInitial
  integer     , intent(  out) :: CritereArret
  integer     , intent(  out) :: NbPasTemps
  real(DOUBLE), intent(  out) :: TempsMaximum
  logical     , intent(  out) :: PasTempsVariable
  real(DOUBLE), intent(  out) :: CourantObj


! Geometrie

  type(FICHIER_T), intent(inout) :: FichierGeom
  integer        , intent(  out) :: FormatGeom

  type(PROFIL_T), dimension(:) , pointer       :: Profil      ! Profils geometriques
  integer                                      :: nb_bief_geom! Nombre de biefs du fichier geometrie
  logical                      , intent(  out) :: PresenceZoneStockage
  logical                      , intent(  out) :: ProfAbs   ! profils abscisse  Absolue

! Maillage et planimetrage

  real(DOUBLE)    , dimension(:), pointer     :: X

  integer         , dimension(:), pointer     :: IDT
  real(DOUBLE)    , dimension(:), pointer     :: XDT

  type(FICHIER_T), intent(inout)              :: FichierMaillage
  type(FICHIER_T), intent(inout)              :: FichierSauveMaillage
  integer                                     :: TypeMaillage

  integer                                     :: NbBief
  real(DOUBLE)      , dimension(:)  , pointer :: absc_abs_ext_deb_bief => null()
  real(DOUBLE)      , dimension(:)  , pointer :: absc_abs_ext_fin_bief => null()
  integer                                     :: NbNoeud
  integer           , dimension(:)  , pointer :: NbExtNoeud
  integer           , dimension(:)  , pointer :: ExtDebBief
  integer           , dimension(:)  , pointer :: ExtFinBief
  integer           , dimension(:,:), pointer :: ExtNoeud
  integer                                     :: NbExtLibre
  integer           , dimension(:)  , pointer :: NumExtLibre

! Reseau

  Type(EXTREMITE_T), dimension(:), pointer       :: Extremite
  type(CONNECT_T)                , intent(  out) :: Connect
  integer, dimension(:)          , pointer       :: Algorithme

! Variables principales

  real(DOUBLE)    , dimension(:), pointer        :: CF1
  real(DOUBLE)    , dimension(:), pointer        :: CF2
  integer                       , intent(  out)  :: LoiFrottement
! Conditions initiales

  logical                       , intent(  out)  :: RepriseCalcul
  type(FICHIER_T)               , intent(inout)  :: FichierRepriseEcr
  type(FICHIER_T)               , intent(inout)  :: FichierRepriseLec
  logical                                        :: presence_ligne_deau
  integer                                        :: type_entree_ligne
  type(FICHIER_T)               , intent(inout)  :: FichierLigne
  integer                                        :: format_ligne

  type(ZONE_SECHE_T), dimension(:), pointer      :: ZoneSeche
  type(ZONE_FROT_T) , dimension(:),pointer       :: ZoneFrot 

! Utilisation Cray

  logical                                        :: UtilisationCray

! Impressions - resultats

  character(LEN=255), intent(  out) :: TitreCas

  logical                                       :: impression_geo
  logical                       , intent(  out) :: ImpressionPlani
  logical                       , intent(  out) :: ImpressionCalcul
  logical                                       :: impression_reseau
  logical                                       :: impression_hydrau
  logical                                       :: impression_ligne

  integer                       , intent(  out) :: PasStockage
  integer                       , intent(  out) :: PasImpression
  integer                       , intent(  out) :: PremierPasStocke

  type(FICHIER_T)               , intent(inout) :: FichierResultat
  type(FICHIER_T)               , intent(inout) :: FichierResultat2

  integer                                       :: post_processeur
  integer                       , parameter     :: POST_RUBENS  = 1
  integer                       , parameter     :: POST_OPTHYCA = 2
  integer                       , parameter     :: POST_OPTRU   = 3

  integer                       , intent(  out) :: FormatResu
  integer                       , intent(  out) :: FormatResu2
  type(FICHIER_T)               , intent(inout) :: FichierListing
  integer                                       :: UniteListing
  real(DOUBLE)                                  :: ecart

  logical, dimension(NB_TOT_VAR), intent(  out) :: VarCalc
  logical, dimension(NB_TOT_VAR), intent(  out) :: VarSto

  integer                       , intent(  out) :: OptionStockage
  integer, dimension(:)         , pointer       :: SectionStockage


! Lois hydrauliques

  type(LOI_T)    , dimension(:) , pointer       :: LoiHydrau
  type(FICHIER_T)               , intent(inout) :: FichierLoiHydrau

! Barrage - singularites

  type(BARRAGE_T)                  , intent(  out) :: Barrage

  type(SINGULARITE_T), dimension(:), pointer       :: Singularite

! Pertes de charge singuliere

  real(DOUBLE)       , dimension(:), pointer       :: PCSing

! Apports et Deversoirs

  type(APPORT_T)     , dimension(:), pointer       :: Apport
  type(DEVERSOIR_T)  , dimension(:), pointer       :: Deversoir

! Confluents

  type(CONFLUENT_T)  , dimension(:), pointer       :: Confluent

! Abaques pour le calcul des pertes de charge automatique aux confluences

  real(DOUBLE)    , dimension(6,6,5) , intent(inout) :: Abaque

  type(CASIER_T),       dimension(:), pointer       :: Casier
  type(LIAISON_T),      dimension(:), pointer       :: Liaison
  type(APPORT_PLUIE_T), dimension(:), pointer       :: ApportPluie

  type(FICHIER_T),                    intent(inout) :: FichierResultatCasier
  type(FICHIER_T),                    intent(inout) :: FichierResultatLiaison
  type(FICHIER_T),                    intent(inout) :: FichierListingLiaison
  type(FICHIER_T),                    intent(inout) :: FichierListingCasier
  type(FICHIER_T),                    intent(inout) :: FichierGeomCasier

! Traitement des erreurs
!

  type(ERREUR_T)                   , intent(inout) :: Erreur

! Arguments specifiques pour l'interface
  type(FICHIER_T),  dimension(:), pointer       :: FichiersLois
  logical                       , intent(in   ) :: Impression
  integer     , dimension(:), pointer           :: ProfDebBief
  integer     , dimension(:), pointer           :: ProfFinBief
  real(DOUBLE), dimension(:), pointer           :: absc_rel_ext_deb_bief
  real(DOUBLE), dimension(:), pointer           :: absc_rel_ext_fin_bief

! SCALAIRES LOCAUX
! ----------------

  logical           :: impression_doc
  integer           :: iext            ! ompteur sur les extremites libres
  integer           :: iprof           ! Compteur sur les profils
  integer           :: iprof_inf       ! borne inf de boucle
  integer           :: isect           ! Compteur sur les sections
  integer           :: retour          ! code de retour des fonctions intrinseques
  integer           :: langue
  character(LEN=33) :: chaine_date     ! Chaine renvoyee par DATE_S
  integer           :: nb_site         ! Nombre de sites ou stocker
  integer           :: num_branche     ! Numero de branche de site ou stocker
  real(DOUBLE)      :: abscisse_rel    ! Abscisse relative du site ou stocker
  real(DOUBLE)      :: absc_abs        ! abscisse absolue correspondante
  integer           :: numero_max_loi  ! Numero de loi hydrau lu le + grand
  logical           :: sauvegarde_modele ! flag de sauvegarde du modele
  integer           :: ul              ! Numero d'unite d'un fichier
  integer             :: nb_casier, num_liaison, iliaison, icasier, ull, ulc
  integer				:: num_casier_origine, num_casier_fin, ilcc, nb_liaisonCC, nb_liaisonRC, jcasier
  integer, dimension(:,:), allocatable :: connect_casier

! Erreur

  !character(132)    :: !arbredappel_old
 
  ! FoX XML
  !--------
  type(Node), pointer :: document,element,champ1,champ2,champ3
  type(DOMconfiguration), pointer :: config
  type(DOMException) :: ex
  integer :: ios
  integer, allocatable      :: itab(:)
  real(double), allocatable :: rtab(:)
!========================== Instructions =============================

! INITIALISATION
! --------------
!   write(12,*)'DEbut Pretrait'
   Erreur%Numero = 0
   !arbredappel_old = trim(!Erreur%arbredappel)
   !Erreur%arbredappel = trim(!Erreur%arbredappel)//'=>PRETRAIT'


! DEFINITION DES POINTEURS
!
   nullify(CF1)

   ! FoX : initialisation
   !---------------------
   config => newDOMConfig()
   call setParameter(config, "xml-declaration", .false.)
   call setParameter(config, "validate-if-schema", .true.)
   document => parseFile(FichierMotCle%Nom,config,iostat=ios,ex=ex)
   if (inException(ex).or.ios.ne.0) then
       if( impression ) then
           print*,"Parse error", getExceptionCode(ex)
       endif
       Erreur%Numero = 704
       Erreur%ft     = err_704
       Erreur%ft_c   = err_704c
       call TRAITER_ERREUR( Erreur )
       return
   endif
   element => getDocumentElement(document)

!========================================================================
!                       LECTURE DES PARAMETRES GENERAUX
!========================================================================

! Ouverture du fichier listing
!-----------------------------
!  JML Nom donne par l'interface
!    FichierListing%Nom = MOTCAR(ADRESS(4,15))

    UniteListing = FichierListing%Unite

    if (Impression) then
        open(unit=UniteListing, file=FichierListing%Nom, access='SEQUENTIAL', &
             action='WRITE'           , form='FORMATTED'       , iostat=RETOUR      , &
             position='rewind'        , status='REPLACE'     )

        if (RETOUR /= 0) then
           Erreur%Numero = 4
           Erreur%ft   = err_4
           Erreur%ft_c = err_4c
           call TRAITER_ERREUR  (Erreur, FichierListing%Nom)
           return
        end if
    end if


! Version du code
!----------------

  champ1 => item(getElementsByTagName(document, "parametresGeneraux"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "versionCode"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,VersionCode)
  if (VersionCode > 3) then
    Erreur%Numero = 300
    Erreur%ft   = err_300
    Erreur%ft_c = err_300c
    call TRAITER_ERREUR  (Erreur)
    return
  end if


! Noyau SARAP/REZO/MASCARET
!--------------------------
  champ2 => item(getElementsByTagname(champ1, "code"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,Noyau)
  if (Noyau == NOYAU_SARAP) then
      Regime = REGIME_PERMANENT
  else if (Noyau == NOYAU_MASCARET .or. Noyau == NOYAU_REZODT) then
      Regime = REGIME_NON_PERMANENT
  else
      Erreur%Numero = 305
      Erreur%ft   = err_305
      Erreur%ft_c = err_305c
      call TRAITER_ERREUR  (Erreur, 'Noyau de calcul', '1, 2 et 3')
      return
  end if


! Fichier des mots cle
!---------------------

  ! affectation en tete de sous programme

! Fichier du dictionnaire
!------------------------

  ! affectation en tete de sous programme


! Fichier Fortran
!----------------

  champ2 => item(getElementsByTagname(champ1, "progPrincipal"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  nom_fortran = getTextContent(champ2)

! Nom du fichier des bibliotheques
!---------------------------------

  champ2 => item(getElementsByTagname(champ1, "bibliotheques"), 0)
  nom_bibli=' '
  if(associated(champ2).neqv..false.) then
     champ3 => item(getElementsByTagname(champ2, "bibliotheque"), 0)
     if(associated(champ3).eqv..false.) then
        call xerror(Erreur)
        return
     endif
     nom_bibli = getTextContent(champ3)
  endif

! Presence de Casiers (implique un couplage LIDO-CASIER)
! ------------------------------------------------------

  champ2 => item(getElementsByTagname(champ1, "presenceCasiers"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,OptionCasier)
   
! Perte de charge automatique due aux confluents
! ----------------------------------------------

  champ1 => item(getElementsByTagName(document, "parametresModelePhysique"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "perteChargeConf"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,PerteChargeConfluent)

! Perte de charge automatique due aux confluents
! ----------------------------------------------

   champ1 => item(getElementsByTagName(document, "parametresNumeriques"), 0)
   if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
   endif
   champ2 => item(getElementsByTagname(champ1, "perteChargeAutoElargissement"), 0)
   if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
   endif
   call extractDataContent(champ2,PerteElargissementTrans)

! Calcul d'une onde de submersion
!--------------------------------

  champ2 => item(getElementsByTagname(champ1, "calcOndeSubmersion"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,OndeSubm)
  if (OndeSubm .and. Noyau == NOYAU_SARAP) then
    Erreur%Numero = 301
    Erreur%ft   = err_301
    Erreur%ft_c = err_301c
    call TRAITER_ERREUR  (Erreur, 'Dam break flood wave', 'SARAP')
    return
  end if

  !
  ! Modelisation de type Boussinesq
  ! -------------------------------
   champ2 => item(getElementsByTagname(champ1, "termesNonHydrostatiques"), 0)
   if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
   call extractDataContent(champ2,Boussinesq)
   
  ! 
  ! Empechement du torrentiel pour REZO
  ! -----------------------------------
  champ2 => item(getElementsByTagname(champ1, "attenuationConvection"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,NoConvection)

  !
  ! Apport de debit dans la quantite de mvt 
  ! -------------------------------
  champ2 => item(getElementsByTagname(champ1, "apportDebit"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,cqmv)

! Calcul de validation
!---------------------

  champ1 => item(getElementsByTagName(document, "parametresGeneraux"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "validationCode"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,CalculValidation)

! Type de validation
!-------------------

  champ2 => item(getElementsByTagname(champ1, "typeValidation"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,TypeValidation)

!=======================
! MODELISATION PHYSIQUE
!=======================

! Modelisation du lit (Debord/Fond-Berge)
!----------------------------------------

  champ1 => item(getElementsByTagName(document, "parametresModelePhysique"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "compositionLits"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,ModeleLit)

  if (ModeleLit.lt.0.or.ModeleLit.gt.2) then
      Erreur%Numero = 305
      Erreur%ft   = err_305
      Erreur%ft_c = err_305c
      call TRAITER_ERREUR  (Erreur, 'Cross section layout', '0, 1 or 2')
      return
  end if

! Conservation du frottement sur les parois verticales
!-----------------------------------------------------

  champ2 => item(getElementsByTagname(champ1, "conservFrotVertical"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,FrottParoiVerticale)

! Debordement progressif dans le lit majeur
!------------------------------------------

  champ2 => item(getElementsByTagname(champ1, "debordement"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "litMajeur"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,DebProgressifLM)
  
! Debordement progressif dans les zones de stockage
!--------------------------------------------------

  champ3 => item(getElementsByTagname(champ2, "zoneStock"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,DebProgressifZS)

! Elevation de la cote signalant l'arrivee du front
!--------------------------------------------------

  champ2 => item(getElementsByTagname(champ1, "elevCoteArrivFront"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,DZArriveeFront)

! Froude Limite pour les conditions aux limites
!----------------------------------------------

  champ1 => item(getElementsByTagName(document, "parametresNumeriques"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "froudeLimCondLim"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,FroudeLim)

! Traitement du frottement
!-------------------------

  champ2 => item(getElementsByTagname(champ1, "traitImplicitFrot"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,FrottementImplicite)

! Implicitation du noyau transcritique
!-------------------------------------

  champ2 => item(getElementsByTagname(champ1, "implicitNoyauTrans"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,Impli_Trans)

! Optimisation du temps calcul (flux figes)
!------------------------------------------

  champ2 => item(getElementsByTagname(champ1, "optimisNoyauTrans"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,Opt)

! Hauteur d'eau minimale
! ----------------------

  champ2 => item(getElementsByTagname(champ1, "hauteurEauMini"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,HEPS)

  ! Interpolation lineaire du coefficient de Frottement
  ! par rapport aux profils
  !---------------------------------------------------

  champ1 => item(getElementsByTagName(document, "parametresModelePhysique"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "interpolLinStrickler"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,InterpLinCoeffFrott)

  ! Les profils sont definies en abscisse absolue
  ! sur le reseau
  !---------------------------------------------------
  champ1 => item(getElementsByTagName(document, "parametresGeometrieReseau"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "geometrie"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "profilsAbscAbsolu"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,ProfAbs)

!=============================================================
!                   PARAMETRES TEMPORELLES
!=============================================================

! Pas de temps
!-------------

  champ1 => item(getElementsByTagName(document, "parametresTemporels"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "pasTemps"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,DT)

  if (DT <= 0.) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Pas de temps')
    return
  end if

! Temps initial
!--------------

  champ2 => item(getElementsByTagname(champ1, "tempsInit"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,TempsInitial)

! Critere d'arret du calcul
!--------------------------

  champ2 => item(getElementsByTagname(champ1, "critereArret"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,CritereArret)
  if (CritereArret /= TEMPS_MAXIMUM .and. &
      CritereArret /= NOMBRE_DE_PAS_TEMPS_MAXIMUM .and. &
	  CritereArret /= COTE_MAXIMALE_AU_POINT_DE_CONTROLE) then
    Erreur%Numero = 305
    Erreur%ft   = err_305
    Erreur%ft_c = err_305c
    call TRAITER_ERREUR  (Erreur, 'Critere d''arret', '1, 2 et 3')
    return
  end if

! Nombre de pas de temps du calcul
!---------------------------------

  champ2 => item(getElementsByTagname(champ1, "nbPasTemps"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,NbPasTemps)

  if (CritereArret == NOMBRE_DE_PAS_TEMPS_MAXIMUM .and. &
      NbPasTemps <= 0) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Nombre de pas de temps')
    return
  end if

! Temps maximum du calcul
!------------------------

  champ2 => item(getElementsByTagname(champ1, "tempsMax"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,TempsMaximum)

  if (CritereArret == TEMPS_MAXIMUM .and. &
      TempsMaximum <= 0.) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Temps maximum de la simulation')
    return
  end if

! Pas de temps variable
!----------------------

  champ2 => item(getElementsByTagname(champ1, "pasTempsVar"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,PasTempsVariable)
   
  if (PasTempsVariable .and. Noyau /= NOYAU_MASCARET) then
    Erreur%Numero = 302
    Erreur%ft   = err_302
    Erreur%ft_c = err_302c
    call TRAITER_ERREUR  (Erreur, 'Pas de temps variable', 'MASCARET')
    return
  end if

! Nombre de Courant souhaite
!---------------------------

  champ2 => item(getElementsByTagname(champ1, "nbCourant"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,CourantObj)
  if (PasTempsVariable .and. CourantObj <= 0.) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Nombre de Courant souhaite')
    return
  end if


!==============================================================
!                       IMPRESSIONS - RESULTATS
!==============================================================

! Titre du calcul
!----------------

  champ1 => item(getElementsByTagName(document, "parametresImpressionResultats"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "titreCalcul"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  TitreCas = getTextContent(champ2)
! Impression de la geometrie
!---------------------------
  if (Impression) then
      champ2 => item(getElementsByTagname(champ1, "impression"), 0)
      if(associated(champ2).eqv..false.) then
         call xerror(Erreur)
         return
      endif
      champ3 => item(getElementsByTagname(champ2, "impressionGeometrie"), 0)
      if(associated(champ3).eqv..false.) then
         call xerror(Erreur)
         return
      endif
      call extractDataContent(champ3,impression_geo)
  else
     impression_geo = .FALSE.
  endif


! Impression du planimetrage
!---------------------------
  if (Impression) then
     champ3 => item(getElementsByTagname(champ2, "impressionPlanimetrage"), 0)
     if(associated(champ3).eqv..false.) then
        call xerror(Erreur)
        return
     endif
     call extractDataContent(champ3,ImpressionPlani)
  else
     ImpressionPlani = .FALSE.
  endif


! Impression du reseau
!---------------------
  if (Impression) then
     champ3 => item(getElementsByTagname(champ2, "impressionReseau"), 0)
     if(associated(champ3).eqv..false.) then
        call xerror(Erreur)
        return
     endif
     call extractDataContent(champ3,impression_reseau)
  else
     impression_reseau = .FALSE.
  endif

! Impression des lois hydrauliques
!---------------------------------

  if (Impression) then
    champ3 => item(getElementsByTagname(champ2, "impressionLoiHydraulique"), 0)
    if(associated(champ3).eqv..false.) then
      call xerror(Erreur)
      return
    endif
    call extractDataContent(champ3,impression_hydrau)
  else
     impression_hydrau = .FALSE.
  endif

! Impression de la ligne d'eau initiale
!--------------------------------------

  if (Impression) then
     champ3 => item(getElementsByTagname(champ2, "impressionligneEauInitiale"), 0)
     if(associated(champ3).eqv..false.) then
        call xerror(Erreur)
        return
     endif
     call extractDataContent(champ3,impression_ligne) 
  else
     impression_ligne = .FALSE.
  endif

! Impression en phase calcul
!---------------------------
  if (Impression) then
     champ3 => item(getElementsByTagname(champ2, "impressionCalcul"), 0)
     if(associated(champ3).eqv..false.) then
        call xerror(Erreur)
        return
     endif
     call extractDataContent(champ3,ImpressionCalcul)
  else
     ImpressionCalcul = .FALSE.
  endif


! Premier pas de temps a stocker
!-------------------------------

  champ2 => item(getElementsByTagname(champ1, "pasStockage"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "premPasTpsStock"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,PremierPasStocke)
  if (PremierPasStocke <= 0) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Premier pas de temps a stocker')
    return
  end if

! Pas de stockage
!----------------

  champ3 => item(getElementsByTagname(champ2, "pasStock"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,PasStockage)
  if (PasStockage <= 0) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Pas de stockage')
    return
  end if

! Pas d'impression
!-----------------

  champ3 => item(getElementsByTagname(champ2, "pasImpression"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,PasImpression)
  if (PasImpression <= 0) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Pas d''impression')
    return
  end if

! fichier des resultats
!----------------------
!  JML Noms donnes par l'interface
! FichierResultat%Nom = MOTCAR(ADRESS(4,13))
! FichierResultat2%Nom = MOTCAR(ADRESS(4,124))

! format du fichier des resultats
!--------------------------------

  champ2 => item(getElementsByTagname(champ1, "resultats"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "postProcesseur"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,post_processeur)
  if (post_processeur /= POST_RUBENS .and. post_processeur /= POST_OPTHYCA) then
      if (post_processeur /= POST_OPTRU) then
          Erreur%Numero = 312
          Erreur%ft   = err_312
          Erreur%ft_c = err_312c
          call TRAITER_ERREUR  (Erreur, post_processeur)
          return
	     endif
  end if

  if (post_processeur == POST_RUBENS) then
    if (Noyau == NOYAU_SARAP) then
      FormatResu = FORMAT_STO_PERMANENT
    else if (Noyau == NOYAU_REZODT .or. Noyau == NOYAU_MASCARET) then
      FormatResu = FORMAT_STO_NONPERMANENT
    endif
  else if (post_processeur == POST_OPTHYCA) then
     FormatResu = FORMAT_STO_OPTHYCA
  endif

  if (post_processeur == POST_OPTRU) then
    if (Noyau == NOYAU_SARAP) then
      FormatResu = FORMAT_STO_PERMANENT
    else if (Noyau == NOYAU_REZODT .or. Noyau == NOYAU_MASCARET) then
      FormatResu = FORMAT_STO_NONPERMANENT
      FormatResu2 = FORMAT_STO_OPTHYCA
    endif
  else
	   FormatResu2 = 0
  endif
! fichier listing
!----------------

  ! Affectation en tete de sous programme

! Ficher de reprise en ecriture
!------------------------------

  champ2 => item(getElementsByTagname(champ1, "fichReprise"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "fichRepriseEcr"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  FichierRepriseEcr%Nom  = getTextContent(champ3)
! Ecart entre branches
!---------------------

  champ2 => item(getElementsByTagname(champ1, "rubens"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "ecartInterBranch"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,ecart)
  if (ecart <= 0.) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Ecart entre branches')
    return
  end if


!============================================================
!                   LECTURE DE LA GEOMETRIE
!============================================================

! SM ? Faut il allouer ici Profil

! Nom du fichier geometrie
!-------------------------
!  JML Nom donne par l'interface
!  FichierGeom%Nom = MOTCAR(ADRESS(4,23))


! Format du fichier geometrie
!----------------------------

  champ1 => item(getElementsByTagName(document, "parametresGeometrieReseau"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "format"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ2,FormatGeom)
  if (FormatGeom /= FORMAT_GEOM_LIDOV2P0 .and.  &
        FormatGeom /= FORMAT_GEOM_LIDOV3P0) then
      Erreur%Numero = 305
      Erreur%ft   = err_305
      Erreur%ft_c = err_305c
      call TRAITER_ERREUR  (Erreur, 'Format du fichier geometrie', '1 et 2')
      return
  end if


! Lecture de la geometrie
!------------------------

  call LEC_GEOM                    ( &

        Profil                     , & ! Resultats
        nb_bief_geom               , &
        ProfDebBief                , &
        ProfFinBief                , &
        Ecart                      , & ! Donnes non modifiees
        FrottParoiVerticale        , &
        ProfAbs                    , &
        Noyau                      , &
        impression_geo             , &
        UniteListing               , &
        FichierGeom                , &
        FormatGeom                 , &
        Erreur                       )

  if (Erreur%numero /= 0) then
    return
  endif

!========================================================
!              LECTURE DES LOIS HYDRAULIQUES
!========================================================
! doit etre avant call LEC_RESEAU

  call LEC_LOI_INTERFACE  ( &

     LoiHydrau        , & ! Tableau des lois hydrauliques
     FichiersLois     , & ! Les Fichiers des lois hydrauliques
     impression_hydrau, & ! Flag d'impression des lois hydrauliques
     UniteListing     , & ! Unite logique fichier listing
     CritereArret     , & ! Criter d'arret du calcul
     TempsMaximum     , & ! Temps maximum du calcul
     document         , &  ! Pointeur vers document XML
     Erreur             & ! Erreur
                      )

     if (Erreur%Numero /= 0) then
       return
     endif

!========================================================
!       LECTURE DU RESEAU ET DES EXTREMITES LIBRES
!========================================================

     call LEC_RESEAU       ( &

     NbBief                , & ! Nombre de biefs
     absc_rel_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     absc_abs_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_abs_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     NbNoeud               , & ! Nombre de noeuds
     NbExtNoeud            , & ! Nombre d'extremite relie a chaque noeud
     ExtDebBief            , & ! Numero de l'extremite debut de chaque bief
     ExtFinBief            , & ! Numero de l'extremite fin de chaque bief
     ExtNoeud              , & ! Numero d'extremite lie a un noeud
     NbExtLibre            , & ! Nombre d'extremites libres
     NumExtLibre           , & ! Numero d'extremite libre
     Extremite             , & ! Extremites libres
     LoiHydrau             , & ! Lois hydrauliques
     impression_reseau     , & ! Flag d'impression du reseau
     UniteListing          , &
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     Noyau                 , & ! Noyau de calcul
     document              , &  ! Pointeur vers document XML
     Erreur                  & ! Erreur
                           )

     if (Erreur%Numero /= 0) then
       return
     endif

     numero_max_loi = 0

     do iext = 1, NbExtLibre
       numero_max_loi = max(numero_max_loi,Extremite(iext)%NumeroLoi)
     end do

     if(numero_max_loi > size(LoiHydrau(:))) then
       Erreur%Numero = 384
       Erreur%ft   = err_384
       Erreur%ft_c = err_384c
       call TRAITER_ERREUR  (Erreur, numero_max_loi, size(LoiHydrau(:)))
       return
     end if

!==============================================================
!                      CALCUL DU MAILLAGE
!==============================================================

     call CALC_MAILLAGE    ( &

     X                     , & ! Tableau des abscisses
     TypeMaillage          , & ! Type de calcul du maillage
     FichierMaillage       , & ! Fichier du maillage
     FichierSauveMaillage  , & ! Fichier de sauvegarde du maillage
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premier profil d'un bief
     ProfFinBief           , & ! Dernier profil d'un bief
     absc_rel_ext_deb_bief , & ! Abscisse rel de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse rel de l'extremite debut du bief
     impression_geo        , & ! Flag d'impression de la geometrie
     FichierListing%Unite          , & ! Unite logique fichier listing
     document              , & ! Pointeur vers document XML
     Erreur                  & ! Erreur
                           )


     if (Erreur%Numero /= 0) then
       return
     endif



!==============================================================
!                      LECTURE DU PLANIMETRAGE
!==============================================================

     call LEC_PLANIM     ( &

     Profil              , & ! Profils geometriques
     FichierListing%Unite, &
     document            , & ! Pointeur vers document XML
     Erreur                & ! Erreur
                         )

     if (Erreur%Numero /= 0) then
       return
     endif

!====================================================================
!                 LECTURE DES CONDITIONS INITIALES
!====================================================================

! Reprise de calcul
!------------------

  champ1 => item(getElementsByTagName(document, "parametresConditionsInitiales"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "repriseEtude"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "repriseCalcul"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,RepriseCalcul)
   
!========================================================
!                     CALCUL DE IDT et XDT
!========================================================
  retour = 0
  if(.not.associated(IDT)) allocate (IDT(size(X)), STAT = retour)
  if (retour /= 0) then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR  (Erreur, 'IDT')
    return
  end if

  if(.not.associated(XDT)) allocate (XDT(size(X)), STAT = retour)
  if (retour /= 0) then
    Erreur%Numero = 5
    Erreur%ft   = err_5
    Erreur%ft_c = err_5c
    call TRAITER_ERREUR  (Erreur, 'XDT')
    return
  end if

  iprof_inf = 1

  do isect =1,size(X(:))

    iprof = iprof_inf

    do while ((X(isect) <  (Profil(iprof)%AbsAbs   - EPS4)) .or.    &
              (X(isect) >= (Profil(iprof+1)%AbsAbs - EPS4)))

      if ((abs(X(isect)- Profil(size(Profil(:)))%AbsAbs)) <= 0.0001)  exit
      iprof = iprof + 1
    end do

	XDT(isect) = (X(isect) - Profil(iprof)%AbsAbs) /               &
                 (Profil(iprof + 1)%AbsAbs - Profil(iprof)%AbsAbs)
	IDT(isect) = iprof

    iprof_inf = iprof

  end do
!
!==============================================================
!               IMPRESSIONS - RESULTATS  annexe
!==============================================================

! Option de stockage
!-------------------

  champ1 => item(getElementsByTagName(document, "parametresImpressionResultats"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "stockage"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "option"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,OptionStockage)
  if (OptionStockage /= STOCKAGE_TOUTES_SECTION .and. &
      OptionStockage /= STOCKAGE_LISTE_SECTION) then
    Erreur%Numero = 305
    Erreur%ft   = err_305
    Erreur%ft_c = err_305c
    call TRAITER_ERREUR  (Erreur, 'Option de stockage','1 ou 2')
    return
  end if

  champ3 => item(getElementsByTagname(champ2, "nbSite"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,nb_site)
  if (OptionStockage == STOCKAGE_LISTE_SECTION .and. &
      nb_site <= 0) then
    Erreur%Numero = 306
    Erreur%ft   = err_306
    Erreur%ft_c = err_306c
    call TRAITER_ERREUR  (Erreur, 'Nombre de sites')
    return
  end if

  if (OptionStockage == STOCKAGE_LISTE_SECTION) then
    retour = 0
    if(.not.associated(SectionStockage)) allocate(SectionStockage(nb_site), STAT = retour)
    if (retour /= 0) then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR  (Erreur, 'SectionStockage')
      return
    end if

    allocate( itab(nb_site) , STAT = retour )
    if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'itab' )
       return
    end if
    allocate( rtab(nb_site) , STAT = retour )
    if( retour /= 0 ) then
       Erreur%Numero = 5
       Erreur%ft     = err_5
       Erreur%ft_c   = err_5c
       call TRAITER_ERREUR( Erreur , 'rtab' )
       return
    end if

    champ3 => item(getElementsByTagname(champ2, "branche"), 0)
    if(associated(champ3).eqv..false.) then
       call xerror(Erreur)
       return
    endif
    call extractDataContent(champ3,itab)
    champ3 => item(getElementsByTagname(champ2, "abscisse"), 0)
    if(associated(champ3).eqv..false.) then
       call xerror(Erreur)
       return
    endif
    call extractDataContent(champ3,rtab)
    
    do isect = 1,nb_site

      num_branche = itab(isect)
      if (num_branche < 1) then
        Erreur%Numero = 372
        Erreur%ft   = err_372
        Erreur%ft_c = err_372c
        call TRAITER_ERREUR  (Erreur, isect)
        return
      end if

      abscisse_rel = rtab(isect)
      if (Impression) then
          write(UniteListing,10436) isect, num_branche, abscisse_rel
      end if

      absc_abs = ABS_ABS_S    ( &
                 num_branche  , &
                 abscisse_rel , &
                 Profil       , &
                 ProfDebBief  , &
                 ProfFinBief  , &
                 Erreur         &
                              )
      if (Erreur%Numero /= 0) then
        return
      endif

      call XINDIC_S (SectionStockage(isect), &
                     absc_abs              , &
                     X                     , &
                     Erreur                )

      if (Erreur%Numero /= 0) then
        return
      endif

    end do

    deallocate(itab)
    deallocate(rtab)
    
  endif

!==============================================================
!                      LECTURE DES FROTTEMENTS
!==============================================================

! Lois de frottement
!-------------------

  champ1 => item(getElementsByTagName(document, "parametresCalage"), 0)
  if(associated(champ1).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ2 => item(getElementsByTagname(champ1, "frottement"), 0)
  if(associated(champ2).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  champ3 => item(getElementsByTagname(champ2, "loi"), 0)
  if(associated(champ3).eqv..false.) then
     call xerror(Erreur)
     return
  endif
  call extractDataContent(champ3,LoiFrottement)
  if (LoiFrottement < LOI_FROTTEMENT_STRICKLER .or. &
      LoiFrottement > LOI_FROTTEMENT_NB_MAX) then
    Erreur%Numero = 366
    Erreur%ft   = err_366
    Erreur%ft_c = err_366c
    call TRAITER_ERREUR  (Erreur, LoiFrottement)
    return
  end if

! Si on a lu un fichier geometrie au format 3.0 (c.a.d. qu'on n'a
! pas lu les coeff de frottement), on lit les coeff maintenant.
!---------------------------------------------------------------

     if  (FormatGeom == FORMAT_GEOM_LIDOV3P0) then

       call  LEC_FROTTEMENT  ( &

       CF1                   , &
       CF2                   , &
       X                     , &
       ZoneFrot              , & 
       XDT                   , &
       Profil                , &
       ProfDebBief           , &
       ProfFinBief           , &
       absc_rel_ext_deb_bief , &
       absc_rel_ext_fin_bief , &
       InterpLinCoeffFrott   , &
       UniteListing          , &
       document              , &
       Erreur                  & ! Erreur
                             )

       if (Erreur%Numero /= 0) then
         return
       endif

     endif

!========================================================
!       CALCUL DE LA TABLE DE CONNECTIVITE
!========================================================



     call CALC_CONNECT     ( &
     Connect               , & ! Table de connectivite
     NbBief                , & ! Nombre de biefs
     absc_abs_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_abs_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     X                     , & ! Abscisses de sections de calcul
     NbNoeud               , & ! Nombre de noeuds
     NbExtNoeud            , & ! Nombre d'extremite relie a chaque noeud
     ExtDebBief            , & ! Numero de l'extremite debut de chaque bief
     ExtFinBief            , & ! Numero de l'extremite fin de chaque bief
     ExtNoeud              , & ! Numero d'extremite lie a un noeud
     NbExtLibre            , & ! Nombre d'extremites libres
     NumExtLibre           , & ! Numero d'extremite libre
     Erreur                  & ! Erreur
                           )

     if (Erreur%Numero /= 0) then
       return
     endif


!==============================================================
!                  LECTURE DES ZONES DE STOCKAGE
!==============================================================


     if  (FormatGeom == FORMAT_GEOM_LIDOV3P0) then

       call LEC_STOCKAGE        ( &

            Profil              , & ! Profils geometriques
            PresenceZoneStockage, & ! Flag d'existence de zones de stockage
            FichierListing%Unite, & ! Unite logique fichier listing
            document            , & ! Pointeur vers document XML
            Erreur                & ! Erreur
                                )

       if (Erreur%Numero /= 0) then
         return
       endif

     endif

!==============================================================
!                    LECTURE DES ZONES SECHES
!==============================================================

     call LEC_ZONE_SECHE   ( &

     ZoneSeche             , & ! Zones seches
     Connect               , & ! Connectivite du reseau
     X                     , & ! Maillage
     Profil                , &
     ProfDebBief           , &
     ProfFinBief           , &
     absc_rel_ext_deb_bief , &
     absc_rel_ext_fin_bief , &
     FichierListing%Unite  , & ! Unite logique fichier listing
     document              , & ! Pointeur vers document XML
     Erreur                  & ! Erreur
                           )

     if (Erreur%Numero /= 0) then
       return
     endif

!==============================================================
!      CALCUL DE L'ALGORITHME DE PARCOURS DES BIEFS
!==============================================================)

    if (Noyau == NOYAU_SARAP) then

      call ALGOP                  ( &

               Algorithme         , &
               size(X)            , &
			   NbExtNoeud         , &
               Connect            , &
               impression_reseau  , &
               FichierListing%Unite, &
               Erreur             )


      if (Erreur%Numero /= 0) then
        return
      endif

    endif

!========================================================
!                      LECTURE DU BARRAGE
!========================================================

    if (OndeSubm) then

      call LEC_BARRAGE      ( &

      Barrage               , & ! Barrage
      Connect               , & ! Connectivite du reseau
      X                     , & ! Tableau du maillage
      Profil                , & ! Profils geometriques
      ProfDebBief           , & ! Premiers profils des biefs
      ProfFinBief           , & ! Derniers profils des biefs
      absc_rel_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
      absc_rel_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
      FichierListing%Unite  , & ! Unite logique fichier listing
      document              , & ! Pointeur vers document XML      
      Erreur                  & ! Erreur
                            )

      if (Erreur%Numero /= 0) then
        return
      endif

    endif


!=========================================================
!                   LECTURE DES SINGULARITES
!=========================================================

     call LEC_SING         ( &

     Singularite           , & ! Pertes de charges singulieres
     LoiHydrau             , & ! Loi hydraulique
     Connect               , & ! Connectivite du reseau
     X                     , & ! Maillage
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     absc_rel_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     Noyau                 , & ! Noyau de calcul utilise
     UniteListing          , &
     document              , & ! Pointeur vers document XML
     Erreur                  & ! Erreur
                           )

     if (Erreur%Numero /= 0) then
       return
     endif

!==============================================================
!            LECTURE DES PERTES DE CHARGE SINGULIERES
!==============================================================

     call LEC_PCSING       ( &

     PCSing                , & ! Pertes de charges singulieres
     X                     , & ! Tableau du maillage
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     absc_rel_ext_deb_bief , & ! Abscisse de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse de l'extremite debut du bief
     FichierListing%Unite  , &
     document              , & ! Pointeur vers document XML     
     Erreur                  & ! Erreur
                           )

     if (Erreur%Numero /= 0) then
       return
     endif


!==============================================================
!                      LECTURE DES DEBITS D'APPORTS
!==============================================================

call LEC_APPORT            ( &

     Apport                , & ! Tableau des lois hydrauliques
     Connect               , & ! Table de connectivite du reseau
     X                     , & ! Maillage
     LoiHydrau             , & ! Lois hydrauliques
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     absc_rel_ext_deb_bief , & ! Abscisse rel de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse rel de l'extremite debut du bief
     FichierListing%Unite  , & !
     document              , & ! Pointeur vers document XML
     Erreur                  & ! Erreur
                           )

     if (Erreur%Numero /= 0) then
       return
     endif

!==============================================================
!                      LECTURE DES DEVERSOIRS
!==============================================================

call LEC_DEVER             ( &

     Deversoir             , & ! Tableau des lois hydrauliques
     Connect               , & ! Table de connectivite du reseau
     X                     , & ! Maillage
     LoiHydrau             , & ! Lois hydrauliques
     Profil                , & ! Profils geometriques
     ProfDebBief           , & ! Premiers profils des biefs
     ProfFinBief           , & ! Derniers profils des biefs
     absc_rel_ext_deb_bief , & ! Abscisse rel de l'extremite debut du bief
     absc_rel_ext_fin_bief , & ! Abscisse rel de l'extremite debut du bief
     FichierListing%Unite  , &
     document              , & ! Pointeur vers document XML
     Erreur                  & ! Erreur
                           )

     if (Erreur%Numero /= 0) then
       return
     endif

!==============================================================
!                      LECTURE DES CONFLUENTS
!==============================================================


     call LEC_CONFLUENT  ( &

     Confluent           , &
     Connect             , &
     FichierListing%Unite , &
     Noyau               , &
     document            , & ! Pointeur vers document XML
     Erreur                & ! Erreur
                         )

     if (Erreur%Numero /= 0) then
       return
     endif


!==============================================================
!    LECTURE DES ABAQUES DES PERTES DE CHARGE AUX CONFLUENTS
!==============================================================
    call INIT_ABAQUE(Abaque)

!
!=============================================================
!               LECTURE DES VARIABLES A SORTIR
!=============================================================
     VarCalc(:) = .FALSE.
     call LEC_SORTIES    ( &
     VarSto              , &
     VarCalc             , &
     document            , & ! Pointeur vers document XML
     Erreur                & ! Erreur
                         )

     if (Erreur%numero /= 0) then
       return
     endif


  if (OptionCasier) then

!=======================================================================
!                       LECTURE DES FICHIERS CASIERS
!=======================================================================

    ulc = FichierListingCasier%Unite
    if (Impression) then
      open(unit=ulc, file=FichierListingCasier%Nom, access='SEQUENTIAL', &
           action='WRITE'           , form='FORMATTED'       , iostat=RETOUR      , &
           position='rewind'        , status='REPLACE'     )

      if (RETOUR /= 0) then
         Erreur%Numero = 4
         Erreur%ft   = err_4
         Erreur%ft_c = err_4c
         call TRAITER_ERREUR  (Erreur, FichierListingCasier%Nom)
         return
      end if
    endif


  !  JML Nom donne par l'interface
  !FichierListingLiaison%Nom = MOTCAR(ADRESS(4,203))

    ull = FichierListingLiaison%Unite

    if (Impression) then
      open(unit=ull, file=FichierListingLiaison%Nom, access='SEQUENTIAL', &
           action='WRITE'           , form='FORMATTED'       , iostat=RETOUR      , &
           position='rewind'        , status='REPLACE'     )

      if (RETOUR /= 0) then
         Erreur%Numero = 4
         Erreur%ft   = err_4
         Erreur%ft_c = err_4c
         call TRAITER_ERREUR  (Erreur, FichierListingLiaison%Nom)
         return
      end if
    endif
  !  JML Noms donnes par l'interface
  !FichierResultatCasier%Nom = MOTCAR(ADRESS(4,200))
  !FichierResultatLiaison%Nom = MOTCAR(ADRESS(4,202))
  !
  !FichierGeomCasier%Nom = MOTCAR(ADRESS(4,204))

  !========================================================================
  !                       LECTURE DE LA VARIABLE CASIER
  !========================================================================

    call LEC_CASIER                  (&
                  Casier,               &
                  FichierGeomCasier,    &
                  document,             & ! Pointeur vers document XML
                  Erreur    )

    if (Erreur%Numero /= 0) then
          return
    end if

    allocate (connect_casier(size(Casier),size(Casier)), STAT = retour)

    if (retour /= 0) then
              Erreur%Numero = 5
              Erreur%ft   = err_5
              Erreur%ft_c = err_5c
              call TRAITER_ERREUR  (Erreur, 'de la matrice de connection')
              return
    end if

    connect_casier(:,:) = 0

  !========================================================================
  !                       LECTURE DE LA VARIABLE LIAISON
  !========================================================================


    call LEC_LIAISON			   (&
                  Liaison,		    &
                  connect_casier,           &
                  X,			    &
                  Profil,		    &
                  ProfDebBief,              &
                  ProfFinBief,              &
                  document,                 &  ! Pointeur vers document XML    
                  Erreur				)

    if (Erreur%Numero /= 0) then
          return
    end if

  !==========================================================================
  !		CALCUL DE LA MATRICE DE CONNECTION CASIER - CASIER
  !=========================================================================


  ! corrige 18/02/05 - prise en compte complete des liaisons multiples casier-casier
  ! version v5p2 d'origine = 1 seule liaison prise en compte entre 2 memes casiers

    do icasier = 1, size(Casier)

      ! on compte le nombre de liaisons casier - casier relie a icasier
      nb_liaisonCC = 0
      do jcasier = 1, size(Casier)
        if (connect_casier(icasier,jcasier) == 1) then

          do iliaison = 1, size(Liaison)

            select case (Liaison(iliaison)%NatureLiaison)
              case (LIAISON_TYPE_CASIER_CASIER)
                num_casier_origine = Liaison(iliaison)%CaracCC%CasierOrigine
                num_casier_fin = Liaison(iliaison)%CaracCC%CasierFin
                if ((num_casier_origine == icasier) .or. (num_casier_fin == icasier)) then
                  if ((num_casier_origine == jcasier) .or. (num_casier_fin == jcasier)) then
                    nb_liaisonCC = nb_liaisonCC + 1
                  end if
                end if
            end select
          end do
        end if
      end do

      if(.not.associated(Casier(icasier)%LiaisonCC)) allocate (Casier(icasier)%LiaisonCC(nb_liaisonCC,2), STAT = retour)
      if (retour /= 0) then
              Erreur%Numero = 5
              Erreur%ft   = err_5
              Erreur%ft_c = err_5c
              call TRAITER_ERREUR  (Erreur, 'de definition des liaisons casier-casier')
              return
      end if

      ! on affecte les numeros de casiers et liaisons associes
      ilcc = 1
      do jcasier = 1, size(Casier)

        if (connect_casier(icasier,jcasier) == 1) then
          ! le casier "jcasier" est relie au casier "icasier"

          ! on cherche les numeros des liaisons qui relient ces deux casiers
          do iliaison = 1, size(Liaison)
            select case (Liaison(iliaison)%NatureLiaison)
              case (LIAISON_TYPE_CASIER_CASIER)
                num_casier_origine = Liaison(iliaison)%CaracCC%CasierOrigine
                num_casier_fin = Liaison(iliaison)%CaracCC%CasierFin
                if ((num_casier_origine == icasier) .or. (num_casier_fin == icasier)) then
                  if ((num_casier_origine == jcasier) .or. (num_casier_fin == jcasier)) then
                    Casier(icasier)%LiaisonCC(ilcc,1) = iliaison
                    Casier(icasier)%LiaisonCC(ilcc,2)= jcasier
                    ilcc = ilcc+1
                  end if
                end if
            end select
          end do

        end if

      end do

    end do

    !==========================================================================
    !           CALCUL DE LA MATRICE DE CONNECTION RIVIERE - CASIER
    !==========================================================================
    do icasier = 1 , size(Casier)
       nb_liaisonRC = 0
       
       do iliaison = 1, size(Liaison)
          if ( Liaison(iliaison)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
              if ( Liaison(iliaison)%CaracRC%NumCasier == icasier ) then
                  nb_liaisonRC =  nb_liaisonRC + 1  
              endif
          endif
       enddo
         
       if(.not.associated(Casier(icasier)%LiaisonRC)) allocate( Casier(icasier)%LiaisonRC(nb_liaisonRC,2) , STAT = retour )
       if( retour /= 0 ) then
          Erreur%Numero = 5
          Erreur%ft     = err_5
          Erreur%ft_c   = err_5c
          call TRAITER_ERREUR( Erreur , 'de definition des liaisons riviere-casier' )
          return
       end if
        
       ! Affectation des numeros de sections et liaisons associes
       ilcc = 1
       do iliaison = 1 , size(Liaison)
          if( Liaison(iliaison)%NatureLiaison == LIAISON_TYPE_RIVIERE_CASIER ) then
              if ( Liaison(iliaison)%CaracRC%NumCasier == icasier ) then
                  ! la liaison "iliaison" est reliee au casier "icasier"
                  Casier(icasier)%LiaisonRC(ilcc,1) = iliaison
                  Casier(icasier)%LiaisonRC(ilcc,2) = Liaison(iliaison)%CaracRC%Section
                  ilcc = ilcc + 1
              endif
          endif
       enddo
    end do
    
    deallocate(connect_casier)

  !========================================================================
  !                       LECTURE DE LA VARIABLE APPORTPLUIE
  !========================================================================
 !  write(12,*)'DEbut LEC_APPORT_PLUIE'


    call LEC_APPORT_PLUIE          (&
                   ApportPluie,       &
                   size(Casier),      &
                   LoiHydrau,         &
                   document,          & ! Pointeur vers document XML                   
                   ulc,&
                   Erreur)

    if (Erreur%Numero /= 0) then
          return
    end if

  end if

  if (Impression) then
      call DATE_S(chaine_date)

      if(VersionCode == 3) then
        write(UniteListing,10000) ' 8.1.3 ', chaine_date
      endif

      if(VersionCode == 2) then
        write(UniteListing,10000) ' 6.1 ', chaine_date
      endif

      if(VersionCode == 1) then
        write(UniteListing,10000) ' 5.1 ', chaine_date
      endif

      write(UniteListing,10005)

      write(UniteListing,10010) NOM_NOYAU(Noyau)
      write(UniteListing,10020) trim(FichierMotCle%Nom)
      write(UniteListing,10030) trim(nom_fortran)
      write(UniteListing,10035) trim(nom_bibli)

      if (OptionCasier) then
           write(UniteListing, 10044) 'OUI'
      else
           write(UniteListing, 10044) 'NON'
      endif

      if (PerteChargeConfluent) then
           write(UniteListing,10045) 'OUI'
      else
           write(UniteListing,10045) 'NON'
      endif

      if (PerteElargissementTrans) then
           write(UniteListing,10046) 'OUI'
      else
           write(UniteListing,10046) 'NON'
      endif

      if (OndeSubm) then
        write(UniteListing,10050)
      endif

      if (CalculValidation) then
        write(UniteListing,10060)
      endif

      if (CalculValidation) then
        write(UniteListing,10070) TypeValidation
      endif

      ! MODELISATION PHYSIQUE
      write(UniteListing,10075)

      if (ModeleLit == 1) then
        write(UniteListing,10080) 'Debord'
      else if(ModeleLit == 2) then
        write(UniteListing,10080) 'Fond-Berge'
      endif

      if(FrottParoiVerticale) then
        write(UniteListing,10090) 'OUI'
      else
        write(UniteListing,10090) 'NON'
      endif

      if(DebProgressifLM) then
        write(UniteListing,10100) 'OUI'
      else
        write(UniteListing,10100) 'NON'
      endif

      if(DebProgressifZS) then
        write(UniteListing,10110) 'OUI'
      else
        write(UniteListing,10110) 'NON'
      endif

      write(UniteListing,10115) DZArriveeFront
      write(UniteListing,10120) FroudeLim

      if (FrottementImplicite)  then
          write(UniteListing,10130) 'IMPLICITATION DU FROTTEMENT'
      else
          write(UniteListing,10130) 'EXPLICITATION DU FROTTEMENT'
      endif

      if (Impli_Trans)  then
          write(UniteListing,10136) 'IMPLICITATION DU SOLVEUR '
      else
          write(UniteListing,10136) 'EXPLICITATION DU SOLVEUR'
      endif

      if (opt)  then
          write(UniteListing,10136) 'OPTIMISATION DU TEMPS CALCUL (FLUX FIGES)'
      endif

      if(InterpLinCoeffFrott) then
        write(UniteListing,10135) 'OUI'
      else
        write(UniteListing,10135) 'NON'
      endif

      ! PARAMETRES TEMPORELLES
      write(UniteListing,10140)
      write(UniteListing,10150) DT
      write(UniteListing,10160) TempsInitial
      if (CritereArret == TEMPS_MAXIMUM) then
        write(UniteListing,10170) 'TEMPS MAXIMUM'
      elseif (CritereArret == NOMBRE_DE_PAS_TEMPS_MAXIMUM) then
        write(UniteListing,10170) 'NOMBRE DE PAS TEMPS MAXIMUM'
      elseif (CritereArret == COTE_MAXIMALE_AU_POINT_DE_CONTROLE) then
        write(UniteListing,10170) 'COTE MAXIMALE AU POINT DE CONTROLE'
      endif

      if (CritereArret == TEMPS_MAXIMUM) then
        write(UniteListing,10190) TempsMaximum
      elseif (CritereArret == NOMBRE_DE_PAS_TEMPS_MAXIMUM) then
        write(UniteListing,10180) NbPasTemps
      endif

      if (PasTempsVariable) then
        write(UniteListing,10200) 'OUI'
      else
        write(UniteListing,10200) 'NON'
      endif

      write(UniteListing,10210) CourantObj

      ! IMPRESSIONS - RESULTATS
      write(UniteListing,10300)
      write(UniteListing,10310) TitreCas

      if (impression_geo) then
        write(UniteListing,10320) 'OUI'
      else
        write(UniteListing,10320) 'NON'
      endif

      if (ImpressionPlani) then
        write(UniteListing,10330) 'OUI'
      else
        write(UniteListing,10330) 'NON'
      endif

      if (impression_reseau) then
        write(UniteListing,10340) 'OUI'
      else
        write(UniteListing,10340) 'NON'
      endif

      if (impression_hydrau) then
        write(UniteListing,10350) 'OUI'
      else
        write(UniteListing,10350) 'NON'
      endif

      if (impression_ligne) then
        write(UniteListing,10360) 'OUI'
      else
        write(UniteListing,10360) 'NON'
      endif

      if (ImpressionCalcul) then
        write(UniteListing,10365) 'OUI'
      else
        write(UniteListing,10365) 'NON'
      endif

      write(UniteListing,10370) PremierPasStocke
      write(UniteListing,10380) PasStockage
      write(UniteListing,10390) PasImpression
      write(UniteListing,10400) FichierResultat%Nom
      if (post_processeur == POST_RUBENS) then
        if (Noyau == NOYAU_SARAP) then
          write(UniteListing,10405) 'RUBENS', 'LIDOP'
        else if (Noyau == NOYAU_REZODT .or. Noyau == NOYAU_MASCARET) then
          write(UniteListing,10405) 'RUBENS', 'LIDONP'
        endif
      else if (post_processeur == POST_OPTHYCA) then
         write(UniteListing,10405) 'OPTHYCA', 'OPTHYCA'
      endif
      if (post_processeur == POST_OPTRU) then
        if (Noyau == NOYAU_SARAP) then
          write(UniteListing,10405) 'RUBENS', 'LIDOP'
        else if (Noyau == NOYAU_REZODT .or. Noyau == NOYAU_MASCARET) then
          write(UniteListing,10405) 'RUBENS', 'LIDONP'
          write(UniteListing,10405) 'OPTHYCA', 'OPTHYCA'
        endif
      endif

      write(UniteListing,10410) FichierListing%Nom
      write(UniteListing,10420) FichierRepriseEcr%Nom
      write(UniteListing,10430) ecart

      !  LECTURE DE LA GEOMETRIE
      write(UniteListing,10440)
      write(UniteListing,10450) FichierGeom%Nom

      if (FormatGeom == FORMAT_GEOM_LIDOV2P0) then
        write(UniteListing,10460) 'FORMAT LIDO 2.0'
      else if (FormatGeom == FORMAT_GEOM_LIDOV3P0) then
        write(UniteListing,10460) 'FORMAT LIDO 3.0'
      endif

      !  LECTURE DES CONDITIONS INITIALES
      write(UniteListing,10560)

      !  IMPRESSIONS - RESULTATS  annexe
      if (OptionStockage == STOCKAGE_TOUTES_SECTION) then
        write(UniteListing,10432) 'A toutes les sections'
      elseif (OptionStockage == STOCKAGE_LISTE_SECTION) then
        write(UniteListing,10432) 'A certaines sections'
      endif

      if (OptionStockage == STOCKAGE_LISTE_SECTION) then
        write(UniteListing,10434) nb_site
      endif
  end if

  !Erreur%arbredappel = !arbredappel_old
!     write(12,*)'Fin Pretrait'

   !call destroy(document)
   !call destroy(config)
  
  return

! ... Format Declarations ...

   10000 format (/18X,'*************************************'  &
                 /18X,'*        SYSTEME MASCARET           *'  &
                 /18X,'*         VERSION  ',A,'          *'     &
                 /18X,'* ',A33,' *'                          &
                 /18X,'*************************************'///)

  10005 format (/,'PARAMETRES GENERAUX',/, &
               &  '-------------------',/)
  10010 format ('Noyau de calcul utilise                         : ',A8)
  10020 format ('Nom du fichier des mots-cles                    : ',A)
  10025 format ('Nom du fichier du dictionnaire                  : ',A)
  10030 format ('Nom du fichier du programme principal           : ',A)
  10035 format ('Nom du fichier des bibliotheques                : ',A)
  10037 format ('Sauvegarde du modele                            : ',A)
  10040 format ('Nom du fichier de sauvegarde du modele          : ',A)
  10044 format ('Presence de casier                              : ',A)
  10045 format ('Calcul auto des pertes de charge aux confluents : ',A)
  10046 format ('Perte automatique aux elargissements transcritique : ',A)
  10050 format ('Calcul d''onde de submersion')
  10060 format ('Calcul de validation')
  10070 format ('Type de validation : ',i3)

  10075 format (/,'MODELISATION PHYSIQUE',/, &
               &  '---------------------',/)
  10080 format ('Modelisation du lit                               : ',A)
  10090 format ('Frottement sur les parois verticales              : ',A3)
  10100 format ('Debordement progressif dans le lit majeur         : ',A3)
  10110 format ('Debordement progressif dans les zones de stockage : ',A3)
  10115 format ('Elevation de cote signalant l''arrivee du front    : ',f12.3)
  10120 format ('Froude limite pour les conditions aux limites     : ',f12.3)
  10130 format ('Type de frottement                                : ',A)
  10135 format ('Interpolation lineaire des Coeff de frottement    : ',A)
  10136 format ('Implicitation du noyau transcritique              : ',A)

  10140 format (/,'PARAMETRES TEMPORELS',/, &
               &  '--------------------',/)
  10150 format ('Pas de temps                           : ',f12.3)
  10160 format ('Temps initial                          : ',f12.3)
  10170 format ('Critere d''arret du calcul              : ',A)
  10180 format ('Nombre de pas de temps du calcul       : ',i10)
  10190 format ('Duree maximale du calcul               : ',f12.1)
  10200 format ('Pas de temps variable                  : ',A3)
  10210 format ('Nombre de Courant objectif             : ',f12.3)

  10220 format (/,'CRAY',/, &
               &  '----',/)
  10230 format ('Utilisation du Cray                    : ',A3)

  10300 format (/,'IMPRESSION-RESULTATS',/, &
               &  '--------------------',/)
  10310 format ('Titre du calcul                        : ',A)
  10320 format ('Impression de la geometrie             : ',A3)
  10330 format ('Impression du planimetrage             : ',A3)
  10340 format ('Impression du reseau                   : ',A3)
  10350 format ('Impression des lois hydrauliques       : ',A3)
  10360 format ('Impression de la ligne d''eau initiale  : ',A3)
  10365 format ('Impression en phase calcul             : ',A3)
  10370 format (/,'Premier pas de temps a stocker         : ',i5)
  10380 format ('Pas de stockage                        : ',i5)
  10390 format ('Pas d''impression                       : ',i5)
  10400 format (/,'Fichier resultats                      : ',A)
  10405 format ('Post-processeur utilise                : ',A,/, &
                'Format du fichier Resultats            : ',A)
  10410 format ('Fichier listing                        : ',A)
  10420 format ('Fichier de reprise en ecriture         : ',A)
  10430 format (/,'Ecart entre branches                   : ',f12.3)
  10432 format (/,'Option de stockage                   : ',A)
  10434 format ('Nombre de sites                      : ',i3,/)
  10436 format ('Site n0 : ',i3,' Branche n0 : ',i3,' Abscisse : ',f12.3)

  10440 format (/,'GEOMETRIE',/, &
               &  '---------',/)
  10450 format ('Nom du fichier geometrie    : ',A)
  10460 format ('Format du fichier geometrie : ',A)

  10560 format (/,'CONDITIONS INITIALES',/, &
               &  '--------------------',/)
  10570 format ('Reprise de calcul                        : ',A3)
  10580 format ('Fichier de reprise en lecture            : ',A)

  10590 format (/,'Presence d''une ligne d''eau initiale     : ',A3)
  10600 format ('Mode d''entree de la ligne d''eau         : ',A)
  10610 format ('Nom du fichier de la ligne d''eau        : ',A)
  10620 format ('Format du fichier de la ligne d''eau     : ',A)



contains
!========================================================
!                 SOUS-PROGRAMMES UTILITAIRES
!========================================================


subroutine projeter     ( &
                     Y2 , &
                     Y1 , &
                     X2 , &
                     X1 )

!==================== Declarations ===========================

use M_PRECISION

implicit none

real(DOUBLE), dimension(:), intent(  out) :: Y2
real(DOUBLE), dimension(:), intent(in   ) :: Y1
real(DOUBLE), dimension(:), intent(in   ) :: X2
real(DOUBLE), dimension(:), intent(in   ) :: X1

real(DOUBLE) :: dx
real(DOUBLE) :: alpha
integer :: N1
integer :: N2

integer :: i ! Compteur
integer :: j ! Compteur

!==================== Instructions ===========================

N1 = size(X1)
N2 = size(X2)

boucle_1 : do i = 1, N2

  boucle_2 : do j = 1, N1
    if (X1(j) >= X2(i)) then
      exit boucle_2
    endif
  end do boucle_2

  dx = X1(j) - X1(j-1)
  alpha = (X2(i) - X1(j-1)) / dx

  Y2(i) = Y1(j-1) * (1-alpha) + Y1(j) * alpha

end do boucle_1

return

end subroutine projeter

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

end subroutine PRETRAIT_INTERFACE
