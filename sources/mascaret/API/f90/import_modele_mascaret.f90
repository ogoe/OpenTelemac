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

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : 8.1.1              EDF-CEREMA
! *********************************************************************
   !..................................................................................................................................
   ! Importation d'un modele mascaret a partir des fichiers natifs de Mascaret
   ! .................................................................................................................................
    subroutine IMPORT_MODELE_MASCARET(RetourErreur, Identifiant, TabNomFichier, TypeNomFichier, Taille, Impress)
      use M_APIMASCARET_STATIC
      use M_CONSTANTES_CALCUL_C
      use M_PRETRAIT_INTERFACE_I
      use M_INTERSECT_I
      use M_PLANIM_I
      use M_PLANMA_I
      implicit none

      integer, intent(out)                         :: RetourErreur   ! different de 0 si erreur
      integer, intent(in )                         :: Identifiant    ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
      character(LEN=255), dimension(*), intent(in) :: TabNomFichier  ! Tableau des noms des fichiers natifs Mascaret a importer
      character(LEN=40),  dimension(*), intent(in) :: TypeNomFichier ! Tableau des type des fichiers natifs Mascaret a importer:
		                                                                   ! "casier", "geo", "loi", "cas","listing",
		                                                                   ! "res", "listing_casier", "listing_liaison", "res_casier", "res_liaison"
      integer, intent(in )                         :: Taille         ! Taille des 2 tableaux TabNomFichier et TypeNomFichier
      integer, intent(in )                         :: Impress        ! impression sur les fichiers listing (1-> Vrai 0-> Faux)

      !------------------------------------------------------------------------------------------------------------
      ! variables locales
      !------------------------------------------------------------------------------------------------------------
      Type(MODELE_MASCARET_T)  :: Modele

      ! fichiers en entree d'un modele hydraulique
      Type(FICHIER_T)                        :: FichierMotCle ! type "cas"  exemple Model.cas (obligatoire)
      Type(FICHIER_T)                        :: FichierGeom   ! type "geo"  exemple Model.geo (obligatoire)
      Type(FICHIER_T)                        :: FichierLoiHydrau
      Type(FICHIER_T), dimension(:), pointer :: FichiersLois  ! type "loi"  exemple Model.loi (pas obligatoire)
      Type(FICHIER_T)                        :: FichierLigne  ! type "lig"  exemple Model.lig (pas obligatoire)

      ! fichiers sorties d'un modele hydraulique
      Type(FICHIER_T)          :: FichierListing   ! type "listing"  exemple Model.listing (obligatoire)
      Type(FICHIER_T)          :: FichierResultat  ! type "res"      exemple Model.res     (obligatoire)
      Type(FICHIER_T)          :: FichierControle  ! type "?"        exemple controle.txt   (obligatoire)

      ! fichier en entre pour modele avec casiers (pas obligatoire)
      Type(FICHIER_T)          :: FichierGeomCasier      ! type "casier"           exemple Model.casier

      ! fichiers en sortie pour modele avec casiers (pas obligatoire)
      Type(FICHIER_T)          :: FichierListingLiaison  ! type "listing_liaison"  exemple Model.listing_liaison
      Type(FICHIER_T)          :: FichierListingCasier   ! type "listing_casier"   exemple Model.listing_casier
      Type(FICHIER_T)          :: FichierResultatLiaison ! type "res_liaison"      exemple Model.res_liaison
      Type(FICHIER_T)          :: FichierResultatCasier  ! type "res_casier"       exemple Model.res_casier

      ! fichiers jamais utilises dans le cadre d'une etude importe par l'interface
      Type(FICHIER_T)          :: FichierModele   ! sauvegarde du modele
      Type(FICHIER_T)          :: FichierMaillage ! fichier definissant un maillage
      Type(FICHIER_T)          :: FichierSauveMaillage ! fichier de sauvegarde du maillage
      Type(FICHIER_T)          :: FichierRepriseLec    ! fichier de reprise en lecture
      Type(FICHIER_T)          :: FichierRepriseEcr    ! fichier de reprise en ecriture
      Type(FICHIER_T)          :: FichierResultat2     ! deuxieme fichier resultat


      ! Impressions - resultats
      !------------------------

      character(LEN=255)                       :: TitreCas
      logical                                 :: Impression
      logical                                 :: ImpressionPlani
      integer                                 :: PasStockage
      integer                                 :: PasImpression
      integer                                 :: PremierPasStocke

      integer :: phase_intersect
      integer :: i, compteurLoi, retour
      integer :: nb_pas

      character(LEN=255)          :: nomFic
      character(LEN=40)           :: typeFic

      Type(ERREUR_T)              :: Erreur

      real(DOUBLE)    , dimension (:,:), pointer :: F1 ! Fonction impusion

      FichierModele%Nom        = ''
      FichierMaillage%Nom      = ''
      FichierSauveMaillage%Nom = ''
      FichierRepriseLec%Nom    = ''
      FichierRepriseEcr%Nom    = ''
      FichierResultat2%Nom     = ''

      FichierListingLiaison%Nom  = ''
      FichierListingCasier%Nom   = ''
      FichierResultatLiaison%Nom = ''
      FichierResultatCasier%Nom  = ''
      FichierGeomCasier%Nom      = ''

      RetourErreur = 0

      RetourErreur = TEST_INIT_AND_ID(Identifiant, 'IMPORT_MODELE_MASCARET')
      if (RetourErreur > 0 ) then
         RETURN
      end if


      if (Impress == 1) then
        Impression = .true.
      else
        Impression = .false.
      endif

      if (Impression) then
        FichierModele%Unite        = 11
        FichierSauveMaillage%Unite = 16
        FichierRepriseEcr%Unite    = 18
        FichierResultat%Unite      = 20
        FichierResultat2%Unite     = 25
        FichierListing%Unite       = 22
        FichierControle%Unite      = 24
        FichierListingCasier%Unite   = 33
        FichierListingLiaison%Unite  = 34
        FichierResultatCasier%Unite  = 35
        FichierResultatLiaison%Unite = 36
      else
        FichierModele%Unite        = -1
        FichierSauveMaillage%Unite = -1
        FichierRepriseEcr%Unite    = -1
        FichierResultat%Unite      = -1
        FichierResultat2%Unite     = -1
        FichierListing%Unite       = -1
        FichierControle%Unite      = -1
        FichierListingCasier%Unite   = -1
        FichierListingLiaison%Unite  = -1
        FichierResultatCasier%Unite  = -1
        FichierResultatLiaison%Unite = -1
      end if

      FichierMotCle%Unite        = 12
      FichierGeom%Unite          = 14
      FichierMaillage%Unite      = 15
      FichierRepriseLec%Unite    = 17
      FichierLigne%Unite         = 19
      FichierLoiHydrau%Nom       = ''
      FichierLoiHydrau%Unite     = 21
      FichierGeomCasier%Unite    = 37

      Modele%FichierGeomCasier%Unite = FichierGeomCasier%Unite
      Modele%FichierGeomCasier%Nom   = FichierGeomCasier%Nom
      Modele%FichierListing%Unite  = FichierListing%Unite
      Modele%FichierListing%Nom    = FichierListing%Nom
      Modele%FichierListingCasier%Unite = FichierListingCasier%Unite
      Modele%FichierListingCasier%Nom   = FichierListingCasier%Nom
      Modele%FichierListingLiaison%Unite = FichierListingLiaison%Unite
      Modele%FichierListingLiaison%Nom   = FichierListingLiaison%Nom
      Modele%FichierResuCasier%Unite = FichierResultatCasier%Unite
      Modele%FichierResuCasier%Nom   = FichierResultatCasier%Nom
      Modele%FichierResuLiaison%Unite = FichierResultatLiaison%Unite
      Modele%FichierResuLiaison%Nom   = FichierResultatLiaison%Nom
      Modele%FichierResultat%Unite = FichierResultat%Unite
      Modele%FichierResultat%Nom   = FichierResultat%Nom
      Modele%FichierResultat2%Unite = FichierResultat2%Unite
      Modele%FichierResultat2%Nom   = FichierResultat2%Nom

      ! common du canal listing
      ul_lst = FichierListing%Unite
      UL_LST_CAS = FichierListingCasier%Unite

      ! comptage de nombre de fichier de type loi
      compteurLoi = 0
      DO i=1, Taille
        typeFic = TRIM(TypeNomFichier(i))
        IF ( typeFic == 'loi') THEN
           compteurLoi = compteurLoi +1
        END IF
      END DO

      ! allocation de FichiersLois
      if(.not.associated(FichiersLois)) then
          allocate(FichiersLois(compteurLoi), STAT = retour)
          if (retour /= 0) then
            ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - Unable to allocate les fichiers des lois'
            RetourErreur = 2
            RETURN
          end if
      endif

      ! affection des noms des fichiers imposes par l'API
      compteurLoi = 0
      DO i=1, Taille
        nomFic  = TRIM(TabNomFichier(i))
        typeFic = TRIM(TypeNomFichier(i))

        IF ( typeFic == 'xcas') THEN
           FichierMotCle%Nom = nomFic
        END IF
        IF ( typeFic == 'geo') THEN
           FichierGeom%Nom   = nomFic
        END IF
        IF ( typeFic == 'loi') THEN
           compteurLoi = compteurLoi + 1
           FichiersLois(compteurLoi)%Nom = nomFic
           FichiersLois(compteurLoi)%Unite = 21
        END IF
        IF ( typeFic == 'casier') THEN
           FichierGeomCasier%Nom = nomFic
           Modele%FichierGeomCasier%Nom = nomFic
        END IF
        IF ( typeFic == 'listing') THEN
           FichierListing%Nom = nomFic
           Modele%FichierListing%Nom = nomFic
        END IF
        IF ( typeFic == 'listing_casier') THEN
           FichierListingCasier%Nom = nomFic
           Modele%FichierListingCasier%Nom = nomFic
        END IF
        IF ( typeFic == 'listing_liaison') THEN
           FichierListingLiaison%Nom = nomFic
           Modele%FichierListingLiaison%Nom = nomFic
        END IF
        IF ( typeFic == 'res') THEN
           FichierResultat%Nom = nomFic
           Modele%FichierResultat%Nom = nomFic
        END IF
        IF ( typeFic == 'res_casier') THEN
           FichierResultatCasier%Nom = nomFic
           Modele%FichierResuCasier%Nom = nomFic
        END IF
        IF ( typeFic == 'res_liaison') THEN
           FichierResultatLiaison%Nom = nomFic
           Modele%FichierResuLiaison%Nom = nomFic
        END IF
        IF ( typeFic == 'lig') THEN
           FichierLigne%Nom = nomFic
        END IF

      END DO

      Erreur%Numero = 0
      Erreur%arbredappel = 'IMPORTATION_MODELE'

!      write(12,*)'Avant nullify'
      nullify(Modele%Connect%OrigineBief)
      nullify(Modele%Connect%FinBief)
      nullify(Modele%Connect%NbBiefConfluence)
      nullify(Modele%Connect%NumBiefConfluence)
      nullify(Modele%Connect%NumSectionConfluence)
      nullify(Modele%Connect%NumBiefExtLibre)
      nullify(Modele%Connect%NumSectionExtLibre)
      nullify(Modele%Profils)
      nullify(Modele%SectionPlan%S)
      nullify(Modele%SectionPlan%S1)
      nullify(Modele%SectionPlan%S2)
      nullify(Modele%SectionPlan%SS)
      nullify(Modele%SectionPlan%CELER)
      nullify(Modele%SectionPlan%B)
      nullify(Modele%SectionPlan%INV)
      nullify(Modele%SectionPlan%INTE)
      nullify(Modele%SectionPlan%DYDX)
      nullify(Modele%SectionPlan%PRESS)
      nullify(Modele%SectionPlan%DEB)
      nullify(Modele%SectionPlan%DEB1)
      nullify(Modele%SectionPlan%DEB2)
      nullify(Modele%SectionPlan%SD)
      nullify(Modele%SectionPlan%SD1)
      nullify(Modele%SectionPlan%SD2)
      nullify(Modele%SectionPlan%PRESSD)
      nullify(Modele%SectionPlan%BD)
      nullify(Modele%SectionPlan%DEBD)
      nullify(Modele%ZonesSeches)
      nullify(Modele%LoisHydrau)
      nullify(Modele%Singularites)
      nullify(Modele%Deversoirs)
      nullify(Modele%Liaisons)
      nullify(Modele%Extremites)
      nullify(Modele%Casiers)
      nullify(Modele%Confluents)
      nullify(Modele%Apports)
      nullify(Modele%ApportsPluie)

      nullify(Modele%ProfilPlan%B1)
      nullify(Modele%ProfilPlan%B2)
      nullify(Modele%ProfilPlan%BS)
      nullify(Modele%ProfilPlan%P1)
      nullify(Modele%ProfilPlan%P2)
      nullify(Modele%ProfilPlan%S1)
      nullify(Modele%ProfilPlan%S2)
      nullify(Modele%ProfilPlan%S2G)
      nullify(Modele%ProfilPlan%SS)
      nullify(Modele%ProfilPlan%C)
      nullify(Modele%ProfilPlan%Deb1)
      nullify(Modele%ProfilPlan%Deb2)
      nullify(Modele%ProfilPlan%Pr)
      nullify(Modele%ProfilPlan%Inv)
      nullify(Modele%ProfilPlan%S1D)
      nullify(Modele%ProfilPlan%S2D)
      nullify(Modele%ProfilPlan%SSD)
      nullify(Modele%ProfilPlan%PrD)
      nullify(Modele%ProfilPlan%BD)
      nullify(Modele%ProfilPlan%DebD)

      nullify(Modele%ZREF)
      nullify(Modele%RDC)
      nullify(Modele%RGC)
      nullify(Modele%CF2)
      nullify(Modele%CF1)
      nullify(Modele%PCSing)
      nullify(Modele%XDT)
      nullify(Modele%X)
      nullify(Modele%SectionStockage)
      nullify(Modele%Algorithme)
      nullify(Modele%IDT)
      nullify(Modele%DZ)
      nullify(Modele%XD)
      nullify(Modele%DZD)
      nullify(Modele%ProfDebBief)
      nullify(Modele%ProfFinBief)
      nullify(Modele%absc_rel_ext_deb_bief)
      nullify(Modele%absc_rel_ext_fin_bief)
      nullify(Modele%F1)

      call  PRETRAIT_INTERFACE                                   ( &

        Modele%VersionCode, Modele%Noyau                         , &
        FichierModele, FichierMotCle                             , &
        Modele%OptionCasier                                      , &
        Modele%OndeSubm                                          , &
        Modele%CalculValidation, Modele%TypeValidation           , &
        Modele%Regime, Modele%ModeleLit                          , &
        Modele%FrottParoiVerticale, Modele%PerteChargeConfluent  , &
        Modele%DebProgressifLM, Modele%DebProgressifZS           , &
        Modele%DZArriveeFront                                    , &
        Modele%FroudeLim, Modele%FrottementImplicite             , &
        Modele%ImplicitTrans, Modele%Opt                         , &
        Modele%PerteElargissementTrans                           , &
        Modele%Boussinesq, Modele%NoConvection, Modele%CQMV      , &
        Modele%ProfAbs, Modele%HEPS                              , &
        Modele%DT, Modele%TempsInitial, Modele%CritereArret      , &
        Modele%NbPasTemps, Modele%TempsMaximum                   , &
        Modele%PasTempsVariable, Modele%CourantObj               , &
        FichierGeom, Modele%FormatGeom, Modele%Profils           , &
        Modele%PresenceZoneStockage                              , &
        Modele%X, Modele%IDT, Modele%XDT                         , &
        FichierMaillage, FichierSauveMaillage                    , &
        Modele%TypeMaillage                                      , &
        Modele%Connect                                           , &
        Modele%CF1, Modele%CF2, Modele%InterpolLinStrickler      , &
        Modele%LoiFrottement                                     , &
        Modele%RepriseCalcul                                     , &
        FichierRepriseEcr, FichierRepriseLec                     , &
        FichierLigne                                             , &
        Modele%ZonesSeches                                       , &
        Modele%TitreCas                                          , &
        ImpressionPlani, Modele%ImpressionCalcul                 , &
        Modele%PasStockage, Modele%PasImpression                 , &
        Modele%PremierPasStocke                                  , &
        FichierResultat, Modele%FormatResu, FichierResultat2     , &
        Modele%FormatResu2                                       , &
        FichierListing                                           , &
        Modele%VarCalc, Modele%VarSto                            , &
        Modele%OptionStockage, Modele%SectionStockage            , &
        Modele%LoisHydrau, FichierLoiHydrau                      , &
        Modele%Barrage, Modele%Singularites, Modele%PCSing       , &
        Modele%Apports, Modele%Deversoirs                        , &
        Modele%Confluents, Modele%Extremites, Modele%Algorithme  , &
        Modele%Abaque                                            , &
        Modele%Casiers,             &  ! tableau des casiers
        Modele%Liaisons,            &  ! tableau des liaisons
        Modele%ApportsPluie,        &  ! tableau des apports de pluie
        Modele%ProfDebBief, Modele%ProfFinBief, Modele%absc_rel_ext_deb_bief,  Modele%absc_rel_ext_fin_bief, &
        FichierResultatCasier,      &  ! fichier des resultats des caracteristiques Casier
        FichierResultatLiaison,     &  ! fichier des resultats des caracteristiques Liaison
        FichierListingCasier ,&
        FichierListingLiaison,&
        FichierGeomCasier,          &
        Erreur, &
        FichiersLois, Impression)

      if (Erreur%Numero /= 0) then
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - PRETRAIT_INTERFACE - '//Erreur%Message
	       if (Impression) then
          rewind(FichierListing%Unite)
          close(FichierListing%Unite)
        endif
        return
      endif

      ! PLANIMETRAGE
      !=============
      call  PLANIM                      ( &

                Modele%ProfilPlan       , & ! Profils planimetres
                Modele%Profils          , & ! Caracteristiques des profils
                Modele%F1               , & ! Fonction impulsion
                Modele%DebProgressifLM  , & ! Debordement progressif lit majeur
                Modele%DebProgressifZS  , & ! Debordement progressif zones de stockaage
                ImpressionPlani         , & ! Impression du planimetrage
                FichierListing%Unite    , & ! Unite logique listing
                Modele%FrottParoiVerticale , & ! Conservation du frottement sur les parois verticales
                Erreur                  & ! Erreur
                                    )
      if (Erreur%Numero /= 0) then
        RetourErreur = Erreur%Numero
        ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - PLANIM - '//Erreur%Message
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
          ImpressionPlani   , & ! flag d'impression
          FichierListing%Unite, & !
          Modele%FormatGeom          , & ! Format du fichier geometrie utilise
          Modele%InterpolLinStrickler  , & ! Flag d'interpolation lineaire des Strickler
          phase_intersect     , & ! Phase de la simulation
          Erreur                & ! Erreur
                              )

       if (Erreur%Numero /= 0) then
         RetourErreur = Erreur%Numero
         ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - Intersect - '//Erreur%Message
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
         ptrMsgsErreurs(Identifiant) = 'IMPORT_MODELE_MASCARET - PLANMA - '//Erreur%Message
         if (Impression) then
           rewind(FichierListing%Unite)
           close(FichierListing%Unite)
         endif
         return
       endif
     endif

     ptrTabMascaret(Identifiant)%ModeleMascaret = Modele

     if (Impression) then
       rewind(FichierListing%Unite)
       close(FichierListing%Unite)
       if (Modele%OptionCasier) then
           close(FichierListingCasier%Unite)
           close(FichierListingLiaison%Unite)
       endif
     endif

     if(associated(FichiersLois)) deallocate(FichiersLois)
     
     return
     
end subroutine IMPORT_MODELE_MASCARET
