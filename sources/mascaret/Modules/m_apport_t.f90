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

module M_APPORT_T
!***********************************************************************
! PROGICIEL : MASCARET        N. GOUTAL
!
! VERSION : 8.1.0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   TYPE APPORT_T
      sequence
      character(30) :: Nom             ! Nom de l'apport
      integer       :: NumBranche      ! Numero de la section debut
      real(DOUBLE)  :: AbscisseRel     ! Abscisse relative de l'apport
      real(DOUBLE)  :: Longueur        ! Longueur de l'apport
      integer       :: SectionAm       ! Numero de la section debut
      integer       :: SectionAv       ! Numero de la section fin
      integer       :: NumeroLoi       ! Numero de la loi associe a l'apport
      real(DOUBLE)  :: Debit           ! Valeur de l'apport (m3/s) ou (m3/s/m)
   END TYPE APPORT_T

   contains
    ! Retourne les noms des champs du type ainsi qu'une description
    subroutine GET_TAB_VAR_APPORT(i, tabNomVar, tabDescriptionVar)
      integer , intent(inout)                                  :: i                 ! indiceTableaux
      character(len= 40), dimension(*)                :: tabNomVar         ! Tableau des noms de variable du modele
      character(len=110), dimension(*)                :: tabDescriptionVar ! Tableau des description de variable du modele

        tabNomVar(i)         ="Model.Inflow.Name"
        tabDescriptionVar(i) ="Name of the inflow"
        i=i+1
        tabNomVar(i)         ="Model.Inflow.ReachNum"
        tabDescriptionVar(i) ="Number of the corresponding reach"
        i=i+1
        tabNomVar(i)         ="Model.Inflow.RelAbscissa"
        tabDescriptionVar(i) ="Relative abscissa of the inflow"
        i=i+1
        tabNomVar(i)         ="Model.Inflow.Length"
        tabDescriptionVar(i) ="Length of the inflow"
        i=i+1
        tabNomVar(i)         ="Model.Inflow.UpNode"
        tabDescriptionVar(i) ="Number of the upstream node"
        i=i+1
        tabNomVar(i)         ="Model.Inflow.DownNode"
        tabDescriptionVar(i) ="Number of the downstream node"
        i=i+1
        tabNomVar(i)         ="Model.Inflow.GraphNum"
        tabDescriptionVar(i) ="Number of the graph"
        i=i+1
        tabNomVar(i)         ="Model.Inflow.Discharge"
        tabDescriptionVar(i) ="Value of the inflow (m3/s or m3/s/m)"
        i=i+1

      return

    end subroutine GET_TAB_VAR_APPORT

	! Retourne une description du champ du type au niveau de static (independant de l'instance du modele ou de l'etat)
    function GET_TYPE_VAR_APPORT(NomVar, TypeVar, Categorie, Modifiable, dimVar, MessageErreur)
      implicit none

      integer                          :: GET_TYPE_VAR_APPORT    ! different de 0 si erreur
      character(LEN=40), intent(in)    :: NomVar                   ! Nom de la variable (notation point?)
      character(LEN=10), intent(out)   :: TypeVar                  ! "INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
      character(LEN=10), intent(out)   :: Categorie                ! "MODEL" ou "STATE"
      logical          , intent(out)   :: Modifiable               ! Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
      integer          , intent(out)   :: dimVar                   ! dimension (c'est ? dire le nombre d'indexe de 0 ? 3)
      character(LEN=256), intent(out)  :: MessageErreur            ! Message d'erreur

      GET_TYPE_VAR_APPORT = 0
      TypeVar               = ""
      Categorie             = "MODEL"
      Modifiable            = .FALSE.
      dimVar                = 0
      MessageErreur         = ""


      if ( NomVar == 'Model.Inflow.Name') then
          TypeVar = 'STRING'
          dimVar                = 0
       else if ( NomVar == 'Model.Inflow.ReachNum') then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( NomVar == 'Model.Inflow.RelAbscissa') then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( NomVar == 'Model.Inflow.Length') then
          TypeVar = 'DOUBLE'
          dimVar                = 0
       else if ( NomVar == 'Model.Inflow.UpNode') then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( NomVar == 'Model.Inflow.DownNode') then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( NomVar == 'Model.Inflow.GraphNum') then
          TypeVar = 'INT'
          dimVar                = 0
       else if ( NomVar == 'Model.Inflow.Discharge') then
          TypeVar = 'DOUBLE'
          dimVar                = 0
      else
        GET_TYPE_VAR_APPORT = 1
        TypeVar = "?"
        Categorie             = "MODEL"
        Modifiable            = .FALSE.
        dimVar                = -1
        MessageErreur         = "GET_TYPE_VAR_APPORT - Unknown variable name"
      end if


    end function GET_TYPE_VAR_APPORT

! .................................................................................................................................
! Permet d'acceder a la taille des valeurs des differents champs du type 
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_TAILLE_VAR_APPORT(Instance, NomVar, taille1, taille2, taille3, MessageErreur)
      implicit none
      integer                            :: GET_TAILLE_VAR_APPORT          ! different de 0 si erreur
      type(APPORT_T),         intent(in) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in) :: NomVar                         ! Nom de la variable du modele
      integer,                intent(out):: taille1                        ! valeur max du 1er indice
      integer,                intent(out):: taille2                        ! valeur max du 2e  indice
      integer,                intent(out):: taille3                        ! valeur max du 3e  indice
      character(LEN=256),     intent(out):: MessageErreur                  ! Message d'erreur

      GET_TAILLE_VAR_APPORT = 0
      taille1                = 0
      taille2                = 0
      taille3                = 0
      MessageErreur          = ""

      if ( NomVar == 'Model.Inflow.Name') then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( NomVar == 'Model.Inflow.ReachNum') then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( NomVar == 'Model.Inflow.RelAbscissa') then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( NomVar == 'Model.Inflow.Length') then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( NomVar == 'Model.Inflow.UpNode') then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( NomVar == 'Model.Inflow.DownNode') then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( NomVar == 'Model.Inflow.GraphNum') then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else if ( NomVar == 'Model.Inflow.Discharge') then
         taille1 = 0
         taille2 = 0
         taille3 = 0
      else
         GET_TAILLE_VAR_APPORT = 1
         taille1                = -1
         taille2                = -1
         taille3                = -1
         MessageErreur         = "GET_TAILLE_VAR_APPORT - Unknown variable name"
      end if
   end function GET_TAILLE_VAR_APPORT

! .................................................................................................................................
! Permet de modifier la taille les variables de type pointeurs fortran 
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_TAILLE_VAR_APPORT(Instance, NomVar, NewT1, NewT2, NewT3, MessageErreur)
      implicit none
      integer                               :: SET_TAILLE_VAR_APPORT          ! different de 0 si erreur
      type(APPORT_T),         intent(inout) :: Instance                       ! Instance du type derive dont on souhaite connaitre la taille des differents champs
      character(len= 40),     intent(in)    :: NomVar                         ! Nom de la variable du modele
      integer,                intent(in)    :: NewT1                          ! Nouvelle valeur max du 1er indice
      integer,                intent(in)    :: NewT2                          ! Nouvelle valeur max du 2e  indice
      integer,                intent(in)    :: NewT3                          ! Nouvelle valeur max du 3e  indice
      character(LEN=256),     intent(out)   :: MessageErreur                  ! Message d'erreur


      SET_TAILLE_VAR_APPORT = 0
      MessageErreur          = ""

   end function SET_TAILLE_VAR_APPORT

! .................................................................................................................................
! Accesseurs permettant d'acceder aux valeurs des differents champs du type 
!                     -- Generer automatiquement --
! .................................................................................................................................

   function GET_DOUBLE_APPORT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_DOUBLE_APPORT          ! different de 0 si erreur
      type(APPORT_T),         intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(out):: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_DOUBLE_APPORT = 0
      valeur                = -9999999.9999
      MessageErreur          = ""

      if ( NomVar == 'Model.Inflow.RelAbscissa') then
         valeur = Instance%AbscisseRel
      else if ( NomVar == 'Model.Inflow.Length') then
         valeur = Instance%Longueur
      else if ( NomVar == 'Model.Inflow.Discharge') then
         valeur = Instance%Debit
      else
         GET_DOUBLE_APPORT = 1
         valeur                = -9999999.9999
         MessageErreur         = "GET_DOUBLE_APPORT - Unknown variable name"
      end if
   end function GET_DOUBLE_APPORT


   function GET_INT_APPORT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_INT_APPORT             ! different de 0 si erreur
      type(APPORT_T),         intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(out):: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_INT_APPORT = 0
      valeur                = -9999
      MessageErreur          = ""

      if ( NomVar == 'Model.Inflow.ReachNum') then
         valeur = Instance%NumBranche
      else if ( NomVar == 'Model.Inflow.UpNode') then
         valeur = Instance%SectionAm
      else if ( NomVar == 'Model.Inflow.DownNode') then
         valeur = Instance%SectionAv
      else if ( NomVar == 'Model.Inflow.GraphNum') then
         valeur = Instance%NumeroLoi
      else
         GET_INT_APPORT = 1
         valeur                = -9999
         MessageErreur         = "GET_INT_APPORT - Unknown variable name"
      end if
   end function GET_INT_APPORT


   function GET_STRING_APPORT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: GET_STRING_APPORT          ! different de 0 si erreur
      type(APPORT_T),         intent(in) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(out):: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      GET_STRING_APPORT = 0
      valeur                = ""
      MessageErreur          = ""

      if ( NomVar == 'Model.Inflow.Name') then
         valeur = Instance%Nom
      else
         GET_STRING_APPORT = 1
         valeur                = ""
         MessageErreur         = "GET_STRING_APPORT - Unknown variable name"
      end if
   end function GET_STRING_APPORT



! .................................................................................................................................
! Mutateurs permettant de modifier les differents champs du type
!                     -- Generer automatiquement --
! .................................................................................................................................

   function SET_DOUBLE_APPORT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_DOUBLE_APPORT          ! different de 0 si erreur
      type(APPORT_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      real(DOUBLE),           intent(in) :: valeur                     ! valeur du real(DOUBLE) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_DOUBLE_APPORT = 0
      MessageErreur          = ""

      if ( NomVar == 'Model.Inflow.RelAbscissa') then
         Instance%AbscisseRel = valeur
      else if ( NomVar == 'Model.Inflow.Length') then
         Instance%Longueur = valeur
      else if ( NomVar == 'Model.Inflow.Discharge') then
         Instance%Debit = valeur
      else
         SET_DOUBLE_APPORT = 1
         MessageErreur         = "SET_DOUBLE_APPORT - Unknown variable name"
      end if
   end function SET_DOUBLE_APPORT


   function SET_INT_APPORT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_INT_APPORT             ! different de 0 si erreur
      type(APPORT_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      integer,                intent(in) :: valeur                     ! valeur du integer de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_INT_APPORT = 0
      MessageErreur          = ""

      if ( NomVar == 'Model.Inflow.ReachNum') then
         Instance%NumBranche = valeur
      else if ( NomVar == 'Model.Inflow.UpNode') then
         Instance%SectionAm = valeur
      else if ( NomVar == 'Model.Inflow.DownNode') then
         Instance%SectionAv = valeur
      else if ( NomVar == 'Model.Inflow.GraphNum') then
         Instance%NumeroLoi = valeur
      else
         SET_INT_APPORT = 1
         MessageErreur         = "SET_INT_APPORT - Unknown variable name"
      end if
   end function SET_INT_APPORT


   function SET_STRING_APPORT(Instance, NomVar, index1, index2, index3, valeur, MessageErreur)
      implicit none
      integer                            :: SET_STRING_APPORT          ! different de 0 si erreur
      type(APPORT_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite recuperer la valeur
      character(len= 40),     intent(in) :: NomVar                     ! Nom de la variable du modele
      integer,                intent(in) :: index1                     ! valeur du 1er indice
      integer,                intent(in) :: index2                     ! valeur du 2e  indice
      integer,                intent(in) :: index3                     ! valeur du 3e  indice
      character(LEN=256),     intent(in) :: valeur                     ! valeur du character(LEN=256) de l'instance pour les indexes specifies
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      SET_STRING_APPORT = 0
      MessageErreur          = ""

      if ( NomVar == 'Model.Inflow.Name') then
         Instance%Nom = valeur
      else
         SET_STRING_APPORT = 1
         MessageErreur         = "SET_STRING_APPORT - Unknown variable name"
      end if
   end function SET_STRING_APPORT



! .................................................................................................................................
! Desalloue tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function DESALLOUE_APPORT(Instance, MessageErreur)
      implicit none
      integer                            :: DESALLOUE_APPORT           ! different de 0 si erreur
      type(APPORT_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      integer                            :: i
      character(LEN=256)                 :: MessageErreurType
      DESALLOUE_APPORT = 0
      MessageErreur          = ""

   end function DESALLOUE_APPORT

! .................................................................................................................................
! Rend null tous les pointeurs et fait appel aux desalloues des membres
!                     -- Generer automatiquement --
! .................................................................................................................................

   function NULLIFIER_APPORT(Instance, MessageErreur)
      implicit none
      integer                            :: NULLIFIER_APPORT           ! different de 0 si erreur
      type(APPORT_T),         intent(inout) :: Instance                   ! Instance du type derive dont on souhaite desalloue
      character(LEN=256),     intent(out):: MessageErreur              ! Message d'erreur

      integer                            :: taille
      integer                            :: err
      integer                            :: i
      character(LEN=256)                 :: MessageErreurType
      NULLIFIER_APPORT = 0
      MessageErreur          = ""

   end function NULLIFIER_APPORT

end module M_APPORT_T
