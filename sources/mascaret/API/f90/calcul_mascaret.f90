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
!
! VERSION : 8.1.3              EDF-CEREMA
! *********************************************************************
   !.................................................................................................................................
   ! Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant et l'etat precedent
   ! .................................................................................................................................
subroutine CALCUL_MASCARET(RetourErreur, Identifiant, TpsInitial, TpsFinal, PasTps, Impress)

   use M_ERREUR_T            ! Type ERREUR_T
   use M_LOI_T               ! Type LOI_T
   use M_MODELE_MASCARET_T   ! Type MODELE_MASCARET_T
   use M_ETAT_MASCARET_T     ! Type ETAT_MASCARET_T
   use M_APPORT_T            ! Type APPORT_T
   use M_BARRAGE_T           ! Type BARRAGE_T
   use M_CONFLUENT_T         ! Type CONFLUENT_T
   use M_CONNECT_T           ! Type CONNECT_T : connectivite du reseau
   use M_DEVERSOIR_T         ! Type DEVERSOIR_T
   use M_EXTREMITE_T         ! Type EXTREMITE_T
   use M_FICHIER_T           ! Type FICHIER_T
   use M_PROFIL_T            ! Type PROFIL_T
   use M_PROFIL_PLAN_T       ! Type PROFIL_PLAN_T
   use M_SECTION_T           ! Type SECTION_PLAN_T
   use M_SECTION_PLAN_T      ! Type SECTION_T
   use M_SINGULARITE_T       ! Type SINGULARITE_T
   use M_ZONE_SECHE_T        ! Type ZONE_SECHE_T
   use M_SAUVE_T             ! Type SAUVE_T
   use M_CASIER_T            ! Type CASIER_T
   use M_LIAISON_T           ! Type LIAISON_T
   use M_APPORT_PLUIE_T      ! Type APPORT_PLUIE_T
   use M_INDEX_VARIABLE_C    ! Index des variables
   use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
   use M_MESSAGE_C           ! Messages d'erreur
   use M_PARAMETRE_C         ! EPS2,SEPS
   use M_INTERSECT_I
   use M_APIMASCARET_STATIC
   use M_PLANIM_I
   use M_PLANMA_I
   use M_POST_I
   use M_POST_IMP_I
   use M_PRETRAIT_INTERFACE_I
   use M_QCL_I
   use M_REZO_INTERFACE_I
   use M_SARAP_I
   use M_CLPLUIE_I
   use M_POST_CASIER_I
   use M_POST_IMP_CASIER_I
   use M_STOCK_I
   use M_STOCK_REP_I
   use M_TRAITER_ERREUR_I    ! Traitement de l'errreur
   use M_TRAITER_ERREUR_CASIER_I
   use M_MASCARET_INTERFACE_I
   implicit none

   integer, intent(out)                        :: RetourErreur        ! different de 0 si erreur
   integer, intent(in )                        :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   real(8), intent(in )                        :: TpsInitial          ! Temps initial du calcul
   real(8), intent(in )                        :: TpsFinal            ! Temps final du calcul
   real(8), intent(in )                        :: PasTps              ! Pas de temps interne du calcul
   integer, intent(in )                        :: Impress             ! impression sur les fichiers listing

!.. Implicit Declarations ..

   !------------------------------------------------------------------------
   ! Variables  locales
   !------------------------------------------------------------------------
   integer :: retour ! Code de retour d'erreur des fonctions intrinseques
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
   integer :: phase_stock_casier
   integer :: phase_clpluie
   integer :: phase_post_casier
   integer :: phase_post_imp_casier
   integer :: phase_stock_couplage
   real(DOUBLE)  :: DTImpression    ! pas de temps d'impression
   real(DOUBLE)  :: Temps           ! Temps courant
   real(DOUBLE)  :: Temps1          ! Temps suivant
   type(FICHIER_T) :: FichierResultat
   type(FICHIER_T) :: FichierResultat2
   type(FICHIER_T) :: FichierListing
   type(FICHIER_T) :: FichierListingCasier, FichierListingLiaison, &
                      FichierResultatCasier, FichierResultatLiaison, FichierGeomCasier
   type(FICHIER_T) :: FichierRepriseLec, FichierRepriseEcr, FichierMotCle
   integer         :: FormatResu
   integer         :: FormatResu2
   logical         :: ImpressionCalcul
   real(DOUBLE)    :: DTLEVY
   real(DOUBLE)    , dimension(:), pointer :: SVRAI ! SECTION MOUILLEE ANALYTIQUE
   real(DOUBLE)    , dimension(:), pointer :: ZVRAI ! COTE ANALYTIQUE
   real(DOUBLE)    , dimension(:), pointer :: QVRAI ! DEBIT ANALYTIQUE
   real(DOUBLE)  :: TempsInitial
   real(DOUBLE)  :: TempsMaximum
   integer       :: nb_pas, nb_sect, NbPasTemps
   real(DOUBLE)    , dimension(:), pointer :: ZInitial
   real(DOUBLE)                            :: PasTempsOptimal
   real(DOUBLE)   , dimension(:), pointer  :: VolBiefActif
   real(DOUBLE)   , dimension(:), pointer  :: VolBiefStockage
   real(DOUBLE)   , dimension(:), pointer  :: QAmontPrec
   real(DOUBLE)   , dimension(:), pointer  :: QAvalPrec

 ! Variables complementaires

   real(DOUBLE)    , dimension(:), pointer :: HMOY  ! S/B
   real(DOUBLE)    , dimension(:), pointer :: Q2G   ! Debit maj gauche
   real(DOUBLE)    , dimension(:), pointer :: Q2D   ! Debit maj droit
   real(DOUBLE)    , dimension(:), pointer :: CHARG ! Charge
   real(DOUBLE)    , dimension(:), pointer :: ZMAX  ! Cote max
   real(DOUBLE)    , dimension(:), pointer :: TZMAX ! Temps du Z max
   real(DOUBLE)    , dimension(:), pointer :: VZMAX ! Vitesse max
   real(DOUBLE)    , dimension(:), pointer :: ZMIN  ! Cote min
   real(DOUBLE)    , dimension(:), pointer :: TZMIN ! Temps du Z min
   real(DOUBLE)    , dimension(:), pointer :: V1MIN ! Vitesse mineur min
   real(DOUBLE)    , dimension(:), pointer :: V1MAX ! Vitesse mineur max
   real(DOUBLE)    , dimension(:), pointer :: BMAX  ! Largeur au miroir max
   real(DOUBLE)    , dimension(:), pointer :: TOND  ! temps d'arrivee onde
   real(DOUBLE)    , dimension(:), pointer :: QMAX  ! Debit max
   real(DOUBLE)    , dimension(:), pointer :: TQMAX ! Temps du debit max
   real(DOUBLE)    , dimension(:), pointer :: EMAX  ! Energie max
   real(DOUBLE)    , dimension(:), pointer :: TAUF  ! Contrainte de frottement
   real(DOUBLE)  :: DiffTpsMax_TpsPrec ! difference (TempsMaximum - TempsPrecedent)
   integer :: nbCasier
   integer :: nbLiaison
   integer :: i
   logical :: Impression
   logical :: test_fin
   type(ERREUR_T)           :: Erreur
   type(MODELE_MASCARET_T)  :: Modele
   type(ETAT_MASCARET_T)    :: Etat
!
! Instructions
!
  RetourErreur = TEST_INIT_AND_ID(Identifiant, 'CALCUL_MASCARET')
  if (RetourErreur > 0 ) then
      RETURN
  end if

  if (Impress == 1) then
    Impression = .true.
  else
    Impression = .false.
  endif

  Modele = ptrTabMascaret(Identifiant)%ModeleMascaret
  Etat  = ptrTabMascaret(Identifiant)%EtatMascaret

  ImpressionCalcul = Modele%ImpressionCalcul

  Erreur%Numero = 0
  Erreur%arbredappel = 'CALCUL_MASCARET'

  ! common du canal listing
  if (Impression.or.ImpressionCalcul) then
    ul_lst = 22
    FichierListing = Modele%FichierListing
    FichierListing%Unite = ul_lst
    UL_LST_CAS = 33
    FichierListingCasier = Modele%FichierListingCasier
    FichierListingCasier%Unite = UL_LST_CAS
    
  else
    ul_lst     = -1
    FichierListing%Unite = -1
    FichierListing%Nom   = 'FichierListing'
    UL_LST_CAS = -1
    FichierListingCasier%Unite = -1
    FichierListingCasier%Nom   = 'FichierListingCasier'
    FichierListingLiaison%Nom   = 'FichierListingLiaison'
    FichierListingLiaison%Unite = -1
  endif

  if (Impression) then
    FichierResultat = Modele%FichierResultat
    FichierResultat2 = Modele%FichierResultat2
    FichierResultatCasier = Modele%FichierResuCasier
    FichierResultatLiaison = Modele%FichierResuLiaison
    FichierGeomCasier = Modele%FichierGeomCasier
  else
     FichierResultat%Nom   = 'FichierResultat'
     FichierResultat%Unite = -1

     FichierResultat2%Nom   = 'FichierResultat2'
     FichierResultat2%Unite = -1

     FichierResultatCasier%Nom   = 'FichierResultatCasier'
     FichierResultatCasier%Unite = -1

     FichierResultatLiaison%Nom   = 'FichierResultatLiaison'
     FichierResultatLiaison%Unite = -1

     FichierGeomCasier%Nom   = 'FichierGeomCasier'
     FichierGeomCasier%Unite = -1
  endif

  FormatResu = Modele%FormatResu
  FormatResu2 = Modele%FormatResu2

  if (Etat%phaseSimulation == PHASE_INITIALISATION) then
    test_fin = .false.
    if (PasTps > 0) then
       Etat%DT = PasTps
    else
       Etat%DT = Modele%DT
    endif
    Temps   = TpsInitial
    TempsInitial = TpsInitial
    Temps1 = TpsInitial
    TempsMaximum = TpsFinal
    NbPasTemps = ((TpsFinal - TpsInitial) / Etat%DT)+1
  end if
  if (Etat%phaseSimulation == PHASE_CALCUL) then
    TempsInitial = Etat%TempsPrecedent
    Etat%TempsPrecedent = TpsInitial
    if (PasTps > 0) then
       Etat%DT = PasTps
    endif
    Temps   = TpsInitial + Etat%DT
    Temps1 = TpsInitial + Etat%DT
    TempsMaximum = TpsFinal
    NbPasTemps = (TpsFinal - TpsInitial) / Etat%DT
  endif

  if (Impression) then
    open(unit=FichierListing%Unite          , file=FichierListing%Nom  , access='SEQUENTIAL', &
         action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
         position='append')

    if (RETOUR /= 0) then
      Erreur%Numero = 1
      Erreur%Message = 'Impossible d''ouvrir le fichier listing'
      RetourErreur = Erreur%Numero
      ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - Impossible d''ouvrir le fichier listing'
      return
    end if
  endif !endif (Impression)

  if (Modele%OptionCasier) then

    if (Impression) then
      open(unit=FichierListingCasier%Unite          , file=FichierListingCasier%Nom  , access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
           position='append')

      if (RETOUR /= 0) then
        Erreur%Numero = 1
        Erreur%Message = 'Impossible d''ouvrir le fichier listing casier'
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - Impossible d''ouvrir le fichier listing casier'
        return
      end if


      open(unit=FichierListingLiaison%Unite          , file=FichierListingLiaison%Nom  , access='SEQUENTIAL', &
           action='WRITE'   , form='FORMATTED', iostat=RETOUR      , &
           position='append' )

      if (RETOUR /= 0) then
        Erreur%Numero = 1
        Erreur%Message = 'Impossible d''ouvrir le fichier listing liaison casier'
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - Impossible d''ouvrir le fichier listing liaison casier'
        return
      end if
    endif !endif (Impression)

  end if !endif (Modele%OptionCasier)

  ! Effectuer le planimetrage quand la geometrie a ete modifie
  ! Appel aux subroutines PLANIM, Intersect et PLANMA (pour un noyau Mascaret)
  if (geometrieModifiee(Identifiant)) then
    geometrieModifiee(Identifiant) = .FALSE.
      ! PLANIMETRAGE
      !=============
    call  PLANIM                      ( &

                Modele%ProfilPlan       , & ! Profils planimetres
                Modele%Profils          , & ! Caracteristiques des profils
                Modele%F1               , & ! Fonction impulsion
                Modele%DebProgressifLM  , & ! Debordement progressif lit majeur
                Modele%DebProgressifZS  , & ! Debordement progressif zones de stockaage
                Impression              , & ! Impression du planimetrage
                FichierListing%Unite    , & ! Unite logique listing
                Modele%FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
                Erreur                  & ! Erreur
                                      )

    if (Erreur%Numero /= 0) then
      RetourErreur = Erreur%Numero
      ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - PLANIM - '//TRIM(Erreur%Message)
	  if (Impression) then
        rewind(FichierListing%Unite)
        close(FichierListing%Unite)
      endif
      return
    endif


    ! INTERPOLATIONS AUX SECTIONS DE CALCUL
    !======================================
    phase_intersect = PHASE_INITIALISATION

    call Intersect      ( &

          Modele%ZREF              , & ! Tableau des cotes de ref aux sections
          Modele%RGC               , & ! Cotes de la rive gauche         ''
          Modele%RDC               , & ! Cotes de la rive droite         ''
          Modele%CF1               , & ! Coefficient de frottement mineur
          Modele%CF2               , & ! Coefficient de frottement majeur
          Modele%Profils           , & ! Profils geometriques
          Modele%X                 , & ! Abscisses des sections de calcul
          Modele%IDT               , & ! Positionement des sections / profils
          Modele%XDT               , & ! Positionement des sections / profils
          Modele%Connect           , & ! Connectivite du reseau
          Modele%Extremites        , & ! Extremite libre
          Modele%TypeMaillage      , & ! Type de calcul du maillage
          Impression               , & ! flag d'impression
          FichierListing%Unite     , & !
          Modele%FormatGeom          , & ! Format du fichier geometrie utilise
          Modele%InterpolLinStrickler  , & ! Flag d'interpolation lineaire des Strickler
          phase_intersect     , & ! Phase de la simulation
          Erreur                & ! Erreur
                              )

    if (Erreur%Numero /= 0) then
      RetourErreur = Erreur%Numero
      ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - Intersect - '//TRIM(Erreur%Message)
      if (Impression) then
        rewind(FichierListing%Unite)
        close(FichierListing%Unite)
      endif
      return
    endif

    if (Modele%Noyau == NOYAU_MASCARET) then
      nb_pas = Modele%Profils(1)%NbPas
      call PLANMA          ( &
            Modele%SectionPlan          , & ! Section planimetrees
            Modele%Profils              , & ! Caracteritiques des profils
            Modele%ProfilPlan           , & ! Profils planimetrees
            nb_pas                      , & ! Nombre de pas de planimetrage
            Modele%X                    , & ! Abscisse des sections de calcul
            Modele%DZ                   , & ! Caracteristiques des sections
            Modele%XD                   , & ! Abscisse des interfaces
            Modele%DZD                  , & ! Pas de planimetrage des interfaces
            Modele%XDT                  , & ! Position relative de la section/Profil
            Modele%IDT                  , & ! Profil de donnees amont de la section
            Modele%Connect              , & ! Connectivite du reseau
            Modele%CF1                  , & ! Strickler mineur
            Modele%CF2                  , & ! Strickler majeur
            Modele%PresenceZoneStockage , & ! Presence de zone de stockage
            Modele%LoiFrottement        , & ! Loi de frottement utilisee
            Erreur               )
      if (Erreur%Numero /= 0) then
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - PLANMA - '//TRIM(Erreur%Message)
        if (Impression) then
          rewind(FichierListing%Unite)
          close(FichierListing%Unite)
        endif
        return
      endif
    endif

    ptrTabMascaret(Identifiant)%ModeleMascaret = Modele
  end if ! FIN if (geometrieModifiee(Identifiant))

  nb_pas = Modele%Profils(1)%NbPas
  nb_sect = size(Modele%X)

  if (Modele%OptionCasier) then
    nbCasier = size(Modele%Casiers)
    DO i=1, nbCasier
      Modele%Casiers(i)%Cote        = Etat%Casiers(i)%Cote
      Modele%Casiers(i)%Surface     = Etat%Casiers(i)%Surface
      Modele%Casiers(i)%Volume      = Etat%Casiers(i)%Volume
      Modele%Casiers(i)%VolumeIni   = Etat%Casiers(i)%VolumeIni
      Modele%Casiers(i)%Bilan       = Etat%Casiers(i)%Bilan
      Modele%Casiers(i)%BilanErreur = Etat%Casiers(i)%BilanErreur
      Modele%Casiers(i)%DzCas       = Etat%Casiers(i)%DzCas
      Modele%Casiers(i)%CoteMax     = Etat%Casiers(i)%CoteMax
      Modele%Casiers(i)%TempsMax    = Etat%Casiers(i)%TempsMax
    END DO

    nbLiaison = size(Modele%Liaisons)
    DO i=1, nbLiaison
      Modele%Liaisons(i)%DebitEchange    = Etat%Liaisons(i)%DebitEchange
      Modele%Liaisons(i)%DebitPrecedent  = Etat%Liaisons(i)%DebitPrecedent
      Modele%Liaisons(i)%DebitMax        = Etat%Liaisons(i)%DebitMax
      Modele%Liaisons(i)%TempsDebitMax   = Etat%Liaisons(i)%TempsDebitMax

      Modele%Liaisons(i)%VitesseEchange  = Etat%Liaisons(i)%VitesseEchange
      Modele%Liaisons(i)%VitesseMax      = Etat%Liaisons(i)%VitesseMax
      Modele%Liaisons(i)%TempsVitesseMax = Etat%Liaisons(i)%TempsVitesseMax

      Modele%Liaisons(i)%CaracCC%DQDZamont   = Etat%Liaisons(i)%DQDZamont
      Modele%Liaisons(i)%CaracCC%DQDZaval    = Etat%Liaisons(i)%DQDZaval
      Modele%Liaisons(i)%CaracRC%DQDZcasier  = Etat%Liaisons(i)%DQDZcasier
      Modele%Liaisons(i)%CaracRC%DQDZriviere = Etat%Liaisons(i)%DQDZriviere
    END DO

  end if !endif (Modele%OptionCasier)
  
  !==========================================================================
  !                                BOUCLE DE CALCUL
  !==========================================================================
  phase_post = PHASE_INITIALISATION
  do while (Erreur%Numero == 0)

    if (Etat%phaseSimulation == PHASE_INITIALISATION) then

      phase_planim    = PHASE_CALCUL
      phase_intersect = PHASE_INITIALISATION
      if (Modele%Noyau == NOYAU_MASCARET) then
        phase_planma  = PHASE_CALCUL
      else
        phase_planma  = PHASE_ARRET
      endif
      phase_qcl       = PHASE_INITIALISATION
      phase_sarap     = PHASE_INITIALISATION
      phase_rezo      = PHASE_INITIALISATION
      phase_mascaret  = PHASE_INITIALISATION
      phase_post      = PHASE_INITIALISATION
      phase_post_imp  = PHASE_INITIALISATION
      phase_stock     = PHASE_INITIALISATION

    elseif(Etat%phaseSimulation == PHASE_CALCUL) then

      phase_planim    = PHASE_ARRET
      phase_intersect = PHASE_ARRET
      phase_planma    = PHASE_ARRET
      phase_qcl       = PHASE_CALCUL
      phase_sarap     = PHASE_CALCUL
      phase_rezo      = PHASE_CALCUL
      phase_mascaret  = PHASE_CALCUL
      phase_post      = PHASE_CALCUL
      DTImpression    = Modele%PasImpression*Etat%DT

      if (Etat%numPasTps >= Modele%PremierPasStocke) then
        if (MOD(Etat%numPasTps, Modele%PasImpression) < EPS1 ) then
          phase_post_imp  = PHASE_CALCUL
        else
          phase_post_imp  = PHASE_ARRET
        endif
        if (MOD(Etat%numPasTps , Modele%PasStockage) < EPS1 ) then
          phase_stock     = PHASE_CALCUL
        else
          phase_stock     = PHASE_ARRET
        endif
      else
        phase_stock       = PHASE_ARRET
        phase_post_imp    = PHASE_ARRET
      endif

    elseif (Etat%phaseSimulation == PHASE_TERMINAISON) then

      if (Modele%Noyau == NOYAU_SARAP) then

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

      endif !endif (Modele%Noyau == NOYAU_SARAP)

    endif ! endif (Etat%phaseSimulation == PHASE_INITIALISATION)

    if (Modele%OptionCasier) then

      select case (Etat%phaseSimulation)

      case (PHASE_INITIALISATION)

        phase_clpluie = PHASE_INITIALISATION
        phase_post_casier = PHASE_ARRET
        phase_post_imp_casier = PHASE_INITIALISATION
        phase_stock_casier = PHASE_CALCUL
        phase_stock_couplage = PHASE_ARRET

      case (PHASE_CALCUL)

        phase_clpluie = PHASE_CALCUL
        phase_stock_couplage = PHASE_CALCUL
        phase_post_casier = PHASE_CALCUL
        phase_stock_casier = PHASE_CALCUL
        if (Etat%numPasTps >= Modele%PremierPasStocke) then
          if (MOD(Etat%numPasTps, Modele%PasImpression) < EPS1 ) then
             phase_post_imp_casier  = PHASE_CALCUL
          else
             phase_post_imp_casier  = PHASE_ARRET
          endif
        end if !ENDIF (Etat%numPasTps >= Modele%PremierPasStocke)


      case (PHASE_TERMINAISON)
        phase_clpluie = PHASE_ARRET
        phase_post_casier = PHASE_ARRET
        phase_post_imp_casier = PHASE_ARRET
        phase_stock_casier = PHASE_ARRET
        FichierResultatCasier = FichierListingCasier
        FichierResultatLiaison = FichierListingLiaison

      end select

    else ! NOT (Modele%OptionCasier)
      select case (Etat%phaseSimulation)

      case (PHASE_INITIALISATION)

        phase_stock_couplage = PHASE_ARRET
        phase_stock_casier = PHASE_ARRET

      case (PHASE_CALCUL)

        phase_stock_couplage = PHASE_ARRET
        phase_stock_casier = PHASE_ARRET

      case (PHASE_TERMINAISON)

        phase_stock_casier = PHASE_ARRET

      end select

      phase_post_casier = PHASE_ARRET


    end if ! endif (Modele%OptionCasier)

    ! CALCUL DES APPORTS
    !===================

    if (phase_qcl == PHASE_INITIALISATION .or. &
      phase_qcl == PHASE_CALCUL) then
      if (Modele%noyau == NOYAU_MASCARET .and. &
        phase_qcl == PHASE_INITIALISATION) then
        Temps1 = Temps +Etat%DT
      else
        Temps1 = Temps
      endif

      call QCL             ( &
           Modele%Apports          , & ! tableau des Apports
           Modele%Singularites     , & ! tableau des singularites
           Modele%Extremites       , & ! tableau des Extremites libres
           Modele%LoisHydrau       , & ! tableau des lois hydrauliques
           Temps1                  , & ! Temps
           Etat%numPasTps          , & ! Numero du pas de temps
           Etat%Q1                 , & ! Debits mineurs dans les sections de calcul
           Etat%Froude             , & ! Nombre de Froude
           Modele%Connect          , & ! Connectivite du reseau
           Modele%Noyau            , & ! Noyau de calcul utilise
           Erreur                    & ! Erreur
                           )

      if (Erreur%Numero /= 0) then
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - QCL - '//TRIM(Erreur%Message)
        if (Impression) then
          rewind(FichierListing%Unite)
          close(FichierListing%Unite)
        endif
        return
      endif ! endif (Erreur%Numero /= 0)

    endif !endif (phase_qcl == PHASE_INITIALISATION .or. phase_qcl == PHASE_CALCUL)

    if (Modele%OptionCasier) then

      if (phase_clpluie == PHASE_CALCUL) then
        call CLPLUIE (&

          Modele%ApportsPluie  ,& ! resultat, debit d apport du temps T-DT au temps T
          Temps, Etat%DT       ,& ! variable temps et pas de temps du calcul
          Modele%LoisHydrau    ,& ! hydrogramme de pluie
          Erreur                 )! erreur
            if (Erreur%Numero /= 0) then
             RetourErreur = Erreur%Numero
             ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - CLPLUIE - '//TRIM(Erreur%Message)
	            if (Impression) then
               rewind(FichierListingCasier%Unite)
               close(FichierListingCasier%Unite)
               rewind(FichierListingLiaison%Unite)
               close(FichierListingLiaison%Unite)
               rewind(FichierListing%Unite)
               close(FichierListing%Unite)
               endif
             return
            endif

           end if

      end if

      ! SELECTION DU NOYAU DE CALCUL
      !=============================
      select case (Modele%Noyau)

         case(NOYAU_SARAP)

             if (phase_sarap == PHASE_CALCUL) then

         call SARAP       ( &

        ! Donnees/Resultats
         Etat%Z            , & ! Cote de la surface libre
         Etat%Q1           , & ! Debit mineur
         Etat%Q2           , & ! Debit majeur
         Etat%P1           , & ! Perimetre mouille mineur
         Etat%P2           , & ! Perimetre mouille majeur
         Etat%B1           , & ! Largeur au miroir mineur
         Etat%B2           , & ! Largeur au miroir majeur
         Etat%BS           , & ! Largeur au miroir zone de stockage
         Etat%RH1          , & ! Rayon hydraulique mineur
         Etat%RH2          , & ! Rayon hydraulique majeur
         Etat%S1           , & ! Section mouillee mineur
         Etat%S2           , & ! Section mouillee majeur
         Etat%Beta         , & ! Coefficient du modele Debord
         Etat%Froude       , &  ! Nombre de Froude
         Modele%Extremites , & ! Conditions aux limites
         Modele%Apports    , & ! Apports
         Etat%Qinjec       , & ! Qinjecte
         Etat%Qdeverse     , & ! Qdeverse
         Temps        , & ! Temps
         Modele%Profils    , & ! Profils geometriques
         Modele%ProfilPlan , & ! Profils planimetrees
         Modele%F1         , & ! Fonction impulsion
         Modele%X          , & ! Maillage
         Modele%CF1          , & ! Strickler mineur
         Modele%CF2          , & ! Strickler majeur
         Modele%ZREF         , & ! Cote de reference
         Modele%XDT          , & ! Position section/profil amont
         Modele%IDT          , & ! Numero du profil amont
         Modele%Connect      , & ! Table de connectivite
         Modele%Singularites , & ! Singularites (seuils)
         Modele%PCSing       , & ! Pertes de charge singulieres
         Modele%Deversoirs   , & ! Deversoirs
         Modele%ModeleLit    , & ! Modelisation lit
         Modele%Confluents   , & ! Caracteristiques des confluences
         Modele%Abaque       , & ! Abaques des pertes de  charges aux confluences
         Modele%Algorithme   , & ! Algorithme de parcours des biefs
         ImpressionCalcul    , & ! Flag d'impression
         FichierListing%Unite, & !
         Modele%LoiFrottement       , & ! Type de lois de frottement utilisee
         Modele%PerteChargeConfluent, & ! Flag de perte de charge auto aux confluents
         Modele%CQMV          , & ! qmv des debits d'apports
         Erreur                & ! Erreur
              )

          if (Erreur%Numero /= 0) then
            RetourErreur = Erreur%Numero
            ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - SARAP - '//TRIM(Erreur%Message)
	           if (Impression) then
              rewind(FichierListing%Unite)
              close(FichierListing%Unite)
            endif
            return
          endif

          endif

        case(NOYAU_REZODT)

        if (phase_rezo == PHASE_INITIALISATION .or. &
         phase_rezo == PHASE_CALCUL) then

        call        REZO_INTERFACE ( &
         Etat%Z                    , & ! Cote de la surface libre
         Etat%Q1, Etat%Q2          , & ! Debits mineur et majeur
         Etat%P1, Etat%P2          , & ! Perimetres mouilles mineur et majeur
         Etat%B1, Etat%B2, Etat%BS      , & ! Largeurs au miroir mineur, majeur et de stockage
         Etat%RH1, Etat%RH2        , & ! Rayons hydrauliques mineur et majeur
         Etat%S1, Etat%S2          , & ! Sections mouillee mineur et majeur
         DTLEVY                    , & ! Pas de temps optimal
         Etat%Beta                 , & ! Coefficient Beta de repartition des lits
         Etat%Froude               , & ! Nombre de Froude
         Modele%Extremites         , & ! Extremites libres
         Modele%Apports            , & ! Debits d'apport
         Etat%Qinjec               , & ! Debit injecte
         Etat%Qdeverse             , & ! debit total deverse par un deversoir lateral ponctuel ou lineique
         Temps                     , & ! Temps
         Etat%phaseSimulation ,      & ! Phase de la simulation
         Modele%Profils            , & ! Profils geometriques
         Modele%ProfilPlan         , & ! Profils planimetrees
         Modele%X                  , & ! Maillage
         Modele%CF1, Modele%CF2    , & ! Coefficients de frottement mineur et majeur
         Modele%ZREF               , & ! Cote de reference
         Modele%XDT                , & ! Position section/profil amont
         Modele%IDT                , & ! Numero du profil amont d'une section
         Modele%Connect            , & ! Table de connectivite
         Modele%Singularites       , & ! Singularites
         Modele%PCSing             , & ! Pertes de charges singulieres
         Modele%Deversoirs         , & ! Deversoirs
         Modele%ModeleLit          , & ! Modele du lit
         Modele%Confluents         , & ! Caracteristiques des confluences
         Modele%Abaque             , & ! Abaques des pertes de  charges aux confluences
         DTImpression              , & ! Pas de temps d'impression
         ImpressionCalcul          , & ! Flag d'autorisation d'impression
         FichierListing%Unite      , & ! unite logique du fichier listing
         Modele%LoiFrottement       , & ! Loi de frottement
         Modele%PerteChargeConfluent, & !
         Etat%TempsPrecedent       , & ! Temps precedent
         TempsInitial              , & ! Temps de debut de simulation
         Etat%numPasTps            , & ! Numero du pas de temps
         Etat%DPDZ1, Etat%DPDZ2    , & ! Derivee de P1 et de P2 / Z
         Modele%OptionCasier       , & ! Flag de presence de casiers
         Modele%Liaisons           , & ! Caracteristiques des liaisons RIVIERE-CASIER et CASIER-CASIER
         Modele%Casiers            , & ! Caracteristiques des casiers
         Modele%ApportsPluie       , & ! Apport de pluie des casiers
         Etat%DTRezo               , & ! DT pour REZO
         Etat%MatriceRezo          , & ! Matrice REZO
         Modele%NoConvection       , & ! Attenuation des termes de debits d'apport
         Modele%CQMV               , & ! qmv debits d'apport
         Erreur                    & ! Erreur
                )

          if (Erreur%Numero /= 0) then
            RetourErreur = Erreur%Numero
            ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - REZO - '//TRIM(Erreur%Message)
	           if (Impression) then
              rewind(FichierListing%Unite)
              close(FichierListing%Unite)
            endif
            return
          endif

        endif

        case(NOYAU_MASCARET)

        if (phase_mascaret == PHASE_INITIALISATION .or. &
         phase_mascaret == PHASE_CALCUL) then

           if(.not.associated(SVRAI)) then 
               allocate (SVRAI(nb_sect), STAT = retour)
               if(retour.ne.0) then
                   RetourErreur = 1
                   ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - Impossible d allouer la variable SVRAI'
                   return
               endif
           endif
           if(.not.associated(QVRAI)) then 
               allocate (QVRAI(nb_sect), STAT = retour)
               if(retour.ne.0) then
                   RetourErreur = 1
                   ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - Impossible d allouer la variable QVRAI'
                   return
               endif
           endif
           if(.not.associated(ZVRAI)) then 
               allocate (ZVRAI(nb_sect), STAT = retour)
               if(retour.ne.0) then
                   RetourErreur = 1
                   ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - Impossible d allouer la variable ZVRAI'
                   return
               endif
           endif

           call MASCARET_INTERFACE  ( &
                  Etat%Z                   , & ! Cote
                  Etat%Q1, Etat%Q2              , & ! debits  mineur, majeur
                  Etat%S1, Etat%S2              , & ! section mineur, majeur
                  Etat%W , Etat%AIRS          , & ! Etats du 2D pour les confluents
                  Etat%YNODE,Etat%UNODE,Etat%CNODE   , & ! Etats pour Mascaret
                  Etat%FLUX,                 & ! Flux pour le solveur de Roe
                  Etat%DebitFlux,            & ! Flux de Masse
                  Etat%JGNODE,Etat%JDNODE,Etat%IFIGE,& ! indices de planimetrage
                  Etat%BETA                , & ! Coefficient de repartition mineur/majeur
                  Etat%Froude              , & ! Nombre de Froude
                  Etat%XFRON               , & ! Abscisse du front d'onde
                  Etat%DT ,Temps ,Etat%numPasTps  , & ! Pas de temps optimal,Temps
                  NbPasTemps          , & ! Nombre de pas de temps max
                  TempsMaximum        , & ! Temps maximun
                  Modele%Extremites, Modele%Apports   , & ! Extremites libres,Debits d'apports
                  Etat%Qinjec              , & ! Debit injecte
                  Etat%Qdeverse            , & ! Debit au deversoir
                  Etat%phaseSimulation     , &
                  Phase_post_imp      , &
                  Modele%DZ ,Modele%DZD             , &
                  Modele%XD                  , &
                  nb_pas              , &
                  Modele%SectionPlan         , &
                  Modele%X, Modele%CF1 , Modele%CF2, Modele%ZREF  , & ! Cote du fond
                  nb_sect                     , & ! nombre de sections
                  Modele%ZonesSeches          , & ! Zone seche
                  Modele%Connect              , & ! Table de connectivite
                  Modele%Singularites         , & ! Singularites
                  Modele%Barrage              , & ! XBARP,ZBARP
                  Modele%PCSing               , & ! Pertes de charges singulieres
                  Modele%Deversoirs           , & ! Deversoirs
                  Modele%Confluents           , & ! Confluents 2D
                  SVRAI, QVRAI, ZVRAI         , & ! VALIDATION
                  Etat%ZINIT                       , & ! COTE INITIALE
                  Modele%HEPS  , SEPS ,GPES    ,& ! Hauteur et section minimale, acceleration pesant
                  Modele%OndeSubm, &
                  Modele%CalculValidation,Modele%TypeValidation  ,   & ! Indicateur et numero de validation
                  Modele%RepriseCalcul       ,&  ! Indicateur de reprise de calcul
                  Modele%FroudeLim           , & ! Indicateur de condition aux limites
                  Modele%FrottementImplicite , & ! Indicateur pour l'impliciation du frottement
                  Modele%ImplicitTrans,Modele%Opt, & ! Indicateur pour l'implicitation du solveur
                  Modele%PerteElargissementTrans, & ! Perte de charge aux elargissements
                  Modele%Boussinesq          , & ! Prise en compte de termes non hydrostatiques
                  Modele%CQMV                , & ! qmv des debits d'apports
                  Modele%PresenceZoneStockage, & ! Indicateur de zones de stockage
                  Modele%PastempsVariable ,    & ! Indicateur de pas de temps variable
                  Modele%CourantObj          , & ! Nombre de Courant limite
                  ImpressionCalcul    , & ! Flag d'impression
                  FichierListing%Unite, & ! Unite logique fichier listing
                  Etat%VOLS , Etat%Sauve , &
                  Etat%NBARAD, Etat%IDEB , &
                  Etat%IFIN, Etat%ITEM0  , &
                  Erreur  ) ! apport

          if (Erreur%Numero /= 0) then
             RetourErreur = Erreur%Numero
             ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - MASCARET - '//TRIM(Erreur%Message)
	     if (Impression) then
                rewind(FichierListing%Unite)
                close(FichierListing%Unite)
             endif
            return
          endif

          if(associated(SVRAI)) deallocate(SVRAI)
          if(associated(QVRAI)) deallocate(QVRAI)
          if(associated(ZVRAI)) deallocate(ZVRAI)

        end if

        end select

      
! CALCUL DES VARIABLES COMPLEMENTAIRES
!=====================================
    if (phase_post == PHASE_INITIALISATION .or. &
        phase_post == PHASE_CALCUL) then
      if (Modele%VarCalc(VAR_Q2G)) then
        Modele%VarCalc(VAR_V2) = .TRUE.
      endif
      if (Modele%VarCalc(VAR_TZMAX)) then
        Modele%VarCalc(VAR_ZMAX) = .TRUE.
      endif
      if (Modele%VarCalc(VAR_VZMAX)) then
        Modele%VarCalc(VAR_ZMAX) = .TRUE.
      endif
      if (Modele%VarCalc(VAR_TQMAX)) then
        Modele%VarCalc(VAR_QMAX) = .TRUE.
      endif
      if (Modele%VarCalc(VAR_QMAX)) then
        Modele%VarCalc(VAR_TQMAX) = .TRUE.
      endif

      if (Impression) then
        call     POST ( TAUF                , &
                  Etat%Y      , HMOY        , &
                  Q2G         , Q2D         , &
                  Etat%VOL    , Etat%VOLS   , &
                  CHARG                     , &
                  Etat%SS                   , &
                  Etat%V1     , Etat%V2     , &
                  ZMAX        , TZMAX       , &
                  VZMAX                     , &
                  ZMIN        , TZMIN       , &
                  V1MIN       , V1MAX       , &
                  BMAX                      , &
                  TOND                      , &
                  QMAX        , TQMAX       , &
                  EMAX                      , &
                  Etat%Z                    , &
                  Etat%Q1     , Etat%Q2     , &
                  Etat%S1     , Etat%S2     , &
                  Etat%B1     , Etat%B2     , &
                  Etat%BS                   , &
                  Etat%P1     , Etat%P2     , &
                  Etat%RH1    , Etat%RH2    , &
                  Etat%BETA                 , &
                  Modele%Profils            , &
                  Modele%ProfilPlan         , &
                  Temps, TempsInitial       , &
                  Etat%numPasTps            , &
                  Modele%X                  , &
                  Modele%ZREF               , &
                 Modele%IDT  , Modele%XDT  , &
                  Modele%CF1                , &
                 ZInitial                  , &
                  Modele%Noyau                , &
                  Modele%PresenceZoneStockage , &
                  Modele%DebProgressifLM      , &
                  Modele%DZArriveeFront       , &
                  Etat%phaseSimulation        , &
                  Modele%VarCalc              , &
                  Modele%LoiFrottement        , &
                  Modele%RepriseCalcul        , &
                  FichierRepriseLec         , &
                  Erreur                      &
                                            )

        if (Erreur%Numero /= 0) then
          RetourErreur = Erreur%Numero
          ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - POST - '//TRIM(Erreur%Message)
          rewind(FichierListing%Unite)
          close(FichierListing%Unite)
          return
        endif ! endif (Erreur%Numero /= 0)

        if (Modele%OptionCasier) then
            if (phase_post_casier == PHASE_INITIALISATION .or. &
              phase_post_casier == PHASE_CALCUL) then

              call POST_CASIER (&
                     Modele%Casiers  ,&
                     Modele%Liaisons ,&
                     TEMPS   )

            end if ! (phase_post_casier == PHASE_INITIALISATION .or. phase_post_casier == PHASE_CALCUL)
        endif

        if (phase_post_imp == PHASE_INITIALISATION .or. &
            phase_post_imp == PHASE_CALCUL) then

          Etat%Q(:) = Etat%Q1(:) + Etat%Q2(:)

          call     POST_IMP ( &
                     Modele%X, Modele%ZREF         , & ! Maillage et cotes de reference
                     Modele%RGC, Modele%RDC        , & ! Rives gauche et droite
                     Modele%CF1, Modele%CF2        , & ! Coeff de frottement mineur et majeur
                     Etat%Z, Etat%Q, Etat%Q1, Etat%Q2 , & ! Cote debits mineur et majeur
                     Etat%DebitFlux            , & ! Flux de masse
                     Etat%S1, Etat%S2          , & ! Sections mineur et majeur
                     Etat%B1, Etat%B2, Etat%BS      , & ! Largeurs au miroir mineur, majeur et de stockage
                     Etat%P1, Etat%P2          , & ! Perimetres mouillees mineur et majeur
                     Etat%RH1, Etat%RH2        , & ! Rayons hydrauliques mineur et majeur
                     Etat%Froude, Etat%Beta    , & ! Froude et BETA
                     TAUF                 , & ! Contrainte au fond
                     Etat%Y, HMOY         , & ! Hauteur d'eau et hauteur d'eau moyenne
                     Q2G, Q2D             , & ! Debits majeur droit et gauche
                     Etat%VOL, Etat%VOLS  , & ! Volumes lit actif et zone de stockage
                     CHARG           , & ! Charge
                     Etat%SS, Etat%V1, Etat%V2, & ! Vitesse mineur et majeur
                     ZMAX, TZMAX  , VZMAX , & ! Cote max et temps associe
                     ZMIN, TZMIN     , & ! Cote min et temps associe
                     V1MIN, V1MAX    , & ! Vitesse mineur min et max
                     BMAX            , & ! Largeur au miroir max
                     TOND            , & ! Temps d'arrivee de l'onde
                     QMAX, TQMAX     , & ! Debit max et temps associe
                     EMAX            , & ! Energie maximale
                     ZVRAI , QVRAI   , & ! Solutions analytiques
                     Etat%Qdeverse   , & ! DEbit deverse
                     Temps           , & ! Temps courant
                     Modele%Apports  , & ! Debits d'Apports
                     PasTempsOptimal , & ! Pas de temps optimal
                     Modele%Connect         , & ! Connectivite du reseau
                     Modele%ModeleLit       , & ! Modele du lit (Debord/Crugos)
                     Etat%numPasTps         , & ! Numero du pas de temps
                     Modele%Noyau           , & ! Noyau de calcul utilise
                     Etat%phaseSimulation , & ! Phase Initialisation/Calcul
                     ImpressionCalcul , & ! ImpressionCalcul
                     Modele%Regime          , & ! Regime Permanent / Non Permanent
                     Modele%VarCalc         , & ! Variables a imprimer
                     FichierListing%Unite         , & ! Unite logique listing
                     Etat%TempsPrecedent               , & ! Temps precedent
                     VolBiefActif, VolBiefStockage, & ! Volumes actifs et de stockage
                     QAmontPrec, QAvalPrec        , & ! Debits amont et aval des biefs
                     Erreur                         & ! Erreur
          )

          if (Erreur%Numero /= 0) then
            RetourErreur = Erreur%Numero
            ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - POST_IMP - '//TRIM(Erreur%Message)
            rewind(FichierListing%Unite)
            close(FichierListing%Unite)
            return
          endif ! endif (Erreur%Numero /= 0)
        endif !if (phase_post_imp == PHASE_INITIALISATION .or. phase_post_imp == PHASE_CALCUL)
      endif  ! fin if (Impression)
    endif !endif (phase_post == PHASE_INITIALISATION .or. phase_post == PHASE_CALCUL)

    if (Modele%OptionCasier) then
        if (phase_post_imp_casier == PHASE_INITIALISATION .or. &
          phase_post_imp_casier == PHASE_CALCUL) then

          if (Impression) then
            call POST_IMP_CASIER(&
                       Modele%Casiers,               &
                       FichierListingCasier, &
                       Modele%Liaisons,              &
                       FichierListingLiaison,&
                       TEMPS,                &
                       Etat%phaseSimulation,      &
                       Erreur                 )

            if (Erreur%Numero /= 0) then
              RetourErreur = Erreur%Numero
              ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - POST_IMP_CASIER - '//TRIM(Erreur%Message)
              rewind(FichierListingCasier%Unite)
              close(FichierListingCasier%Unite)
              rewind(FichierListingLiaison%Unite)
              close(FichierListingLiaison%Unite)

              rewind(FichierListing%Unite)
              close(FichierListing%Unite)
              return
            endif ! endif (Erreur%Numero /= 0)
          endif !endif (Impression)

        end if ! endif (phase_post_imp_casier == PHASE_INITIALISATION .or. phase_post_imp_casier == PHASE_CALCUL)
    endif

    if (phase_stock == PHASE_INITIALISATION .or. &
      phase_stock == PHASE_CALCUL) then
      if (Impression) then
      
        Etat%Q(:) = Etat%Q1(:) + Etat%Q2(:)
        
        call STOCK       ( Modele%X               , &
                          Modele%ZREF            , &
                          Modele%RGC  , Modele%RDC      , &
                          Modele%CF1  , Modele%CF2      , &
                          Etat%Z                    , &
                          Etat%Q                    , &
                          Etat%Q1   , Etat%Q2       , &
                          Etat%DebitFlux            , &
                          Etat%S1   , Etat%S2       , &
                          Etat%B1   , Etat%B2       , &
                          Etat%BS                   , &
                          Etat%P1   , Etat%P2       , &
                          Etat%RH1  , Etat%RH2      , &
                          Etat%Froude, Etat%BETA    , &
                          TAUF                      , &
                          Etat%Y, HMOY              , &
                          Q2G  , Q2D                , &
                          Etat%VOL, Etat%VOLS       , &
                          CHARG                     , &
                          Etat%SS                   , &
                          Etat%V1 , Etat%V2         , &
                          ZMAX , TZMAX    , &
                          VZMAX           , &
                          ZMIN , TZMIN    , &
                          V1MIN, V1MAX    , &
                          BMAX            , &
                          TOND            , &
                          QMAX , TQMAX    , &
                          EMAX            , &
                          ZVRAI , QVRAI   , &
                          Etat%Qdeverse   , &
                          TEMPS           , &
                          Modele%Connect          , &
                          Modele%Casiers          , &
                          Modele%Liaisons         , &
                          FichierResultat , &
                          FichierResultat2 ,&
                          FichierResultatCasier, &
                          FichierResultatLiaison,&
                          Modele%OptionStockage  , &
                          Modele%formatResu      , &
                          Modele%FormatResu2     , &
                          phase_stock           , &
                          phase_stock_casier    , &
                          Etat%numPasTps        , &
                          Modele%VarSto          , &
                          Modele%SectionStockage , &
                          FichierMotCle   , &
                          Erreur          )

        if (Erreur%Numero /= 0) then
          RetourErreur = Erreur%Numero
          ptrMsgsErreurs(Identifiant) = 'CALCUL_MASCARET - STOCK - '//TRIM(Erreur%Message)
          rewind(FichierListing%Unite)
          close(FichierListing%Unite)
          return
        endif
      endif ! endif (Impression)
    endif ! endif (phase_stock == PHASE_INITIALISATION .or. phase_stock == PHASE_CALCUL)


! en NP, j'ai besoin de faire NbPas sans compter le pas n0 0
! qui est necessaire a REZO
! En P, j'ai besoin de faire NbPas en commencant a 1

    if (Modele%Noyau == NOYAU_SARAP .and. Etat%phaseSimulation == PHASE_INITIALISATION) then
      Temps = TempsInitial
    else
      Etat%TempsPrecedent = Temps
      Temps          = Temps + Etat%DT
    endif

    Etat%numPasTps = Etat%numPasTps + 1

    select case (Etat%phaseSimulation)

    case (PHASE_INITIALISATION)
	   if(Modele%Noyau == NOYAU_MASCARET .and. Etat%phaseSimulation == PHASE_INITIALISATION) then
         test_fin = .true.
      endif
      Etat%phaseSimulation = PHASE_CALCUL

    case (PHASE_TERMINAISON)
!
! stockage des variables pour la reprise de calculs au format LIDO permanent
! JML suppression du fichier de Reprise pour l'interface
!
! If (Modele%Noyau == NOYAU_MASCARET ) then
! call STOCK_REP   ( Modele%X               , &
!                   Modele%ZREF            , &
!                   Modele%RGC  , Modele%RDC      , &
!                   Modele%CF1  , Modele%CF2      , &
!                   Etat%Z               , &
!                   Etat%Q1   , Etat%Q2       , &
!                   Etat%S1   , Etat%S2       , &
!                   Etat%B1   , Etat%B2       , &
!                   Etat%BS              , &
!                   Etat%P1   , Etat%P2       , &
!                   Etat%RH1  , Etat%RH2      , &
!                   Etat%Froude, Etat%BETA    , &
!                   TAUF            , &
!                   Y    , HMOY     , &
!                   Q2G  , Q2D      , &
!                   VOL  , VOLS     , &
!                   CHARG           , &
!                   SS              , &
!                   V1   , V2       , &
!                   ZMAX , TZMAX    , &
!                   VZMAX           , &
!                   ZMIN , TZMIN    , &
!                   V1MIN, V1MAX    , &
!                   BMAX            , &
!                   TOND            , &
!                   QMAX , TQMAX    , &
!                   EMAX            , &
!                   ZVRAI , QVRAI   , &
!                   Etat%XFRON           , &
!				   Etat%Qdeverse        , &
!                   TEMPS           , &
!                   Modele%Connect         , &
!                   FichierRepriseEcr , &
!                   Modele%VarSto          , &
!                   Modele%SectionStockage , &
!                   Erreur          )
!   endif


    case (PHASE_CALCUL)
      test_fin = .true.
    end select

    if(test_fin.eqv..true.) then
       if (Temps.gt.TempsMaximum) then
          DiffTpsMax_TpsPrec = TempsMaximum - Etat%TempsPrecedent
          if ( DABS(DiffTpsMax_TpsPrec-Etat%DT).GT.EPS6.and.(Etat%TempsPrecedent < TempsMaximum)) then
             Temps = TempsMaximum
             Etat%DT = Temps - Etat%TempsPrecedent
          else
              Etat%Q(:) = Etat%Q1(:) + Etat%Q2(:)
              if (Modele%OptionCasier) then
	         if (Impression) then 
    	            rewind(FichierListingCasier%Unite)
    	            close(FichierListingCasier%Unite)
    	            rewind(FichierListingLiaison%Unite)
    	            close(FichierListingLiaison%Unite)
                 end if
                 DO i=1, nbCasier
                    Etat%Casiers(i)%Cote        = Modele%Casiers(i)%Cote
                    Etat%Casiers(i)%Surface     = Modele%Casiers(i)%Surface
                    Etat%Casiers(i)%Volume      = Modele%Casiers(i)%Volume
                    Etat%Casiers(i)%VolumeIni   = Modele%Casiers(i)%VolumeIni
                    Etat%Casiers(i)%Bilan       = Modele%Casiers(i)%Bilan
                    Etat%Casiers(i)%BilanErreur = Modele%Casiers(i)%BilanErreur
                    Etat%Casiers(i)%DzCas       = Modele%Casiers(i)%DzCas
                    Etat%Casiers(i)%CoteMax     = Modele%Casiers(i)%CoteMax
                    Etat%Casiers(i)%TempsMax    = Modele%Casiers(i)%TempsMax
		 END DO

                 DO i=1, nbLiaison
                    Etat%Liaisons(i)%DebitEchange    = Modele%Liaisons(i)%DebitEchange
                    Etat%Liaisons(i)%DebitPrecedent  = Modele%Liaisons(i)%DebitPrecedent
                    Etat%Liaisons(i)%DebitMax        = Modele%Liaisons(i)%DebitMax
                    Etat%Liaisons(i)%TempsDebitMax   = Modele%Liaisons(i)%TempsDebitMax
                    Etat%Liaisons(i)%VitesseEchange  = Modele%Liaisons(i)%VitesseEchange
                    Etat%Liaisons(i)%VitesseMax      = Modele%Liaisons(i)%VitesseMax
                    Etat%Liaisons(i)%TempsVitesseMax = Modele%Liaisons(i)%TempsVitesseMax
                    Etat%Liaisons(i)%DQDZamont   = Modele%Liaisons(i)%CaracCC%DQDZamont
                    Etat%Liaisons(i)%DQDZaval    = Modele%Liaisons(i)%CaracCC%DQDZaval
                    Etat%Liaisons(i)%DQDZcasier  = Modele%Liaisons(i)%CaracRC%DQDZcasier
                    Etat%Liaisons(i)%DQDZriviere = Modele%Liaisons(i)%CaracRC%DQDZriviere
		 END DO

              end if ! endif (Modele%OptionCasier)
              if (Impression) then
                 rewind(FichierListing%Unite)
                 close(FichierListing%Unite)
              endif
              ptrTabMascaret(Identifiant)%EtatMascaret = Etat
              return
           endif ! end if ( (DiffTpsMax_TpsPrec < Etat%DT).and.(Etat%TempsPrecedent < TempsMaximum))
       endif ! end if (Temps > TempsMaximum)
    endif !test final

  end do     ! Fin boucle calcul
  ptrTabMascaret(Identifiant)%EtatMascaret = Etat

end subroutine CALCUL_MASCARET
