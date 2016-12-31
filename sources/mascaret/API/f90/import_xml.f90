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
! PROGICIEL : MASCARET       B.MARCHAND (marchand@deltacad.fr)
!
! VERSION : 8.1.1              EDF-CEREMA
! *********************************************************************


!.................................................................................................................................
! Import d'un modele Mascaret en XML
! .................................................................................................................................
subroutine IMPORT_XML(RetourErreur, Identifiant, NomFichier, importModele)

   use M_APIMASCARET_STATIC
   use M_MASCARET_T
   use M_MODELE_MASCARET_T
   use FOX_SAX ! Le parser XML

   implicit none

   integer, intent(out)                        :: RetourErreur        ! different de 0 si erreur
   integer, intent(in )                        :: Identifiant         ! Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   character (len=255), intent(in )            :: NomFichier          ! Nom du fichier XML  contenant le Modele Mascaret
   logical, intent(in)                         :: importModele        ! si vrai import du modele, sinon import de l'etat

   type(xml_t) :: xparser
   character (len=255) :: baliseModeleEtat
   character (len=255) :: curVar="" ! Le nom courant de la variable. Si "", aucune variable en cours
   character (len=80) :: tpVar ! Le type de la variable
   integer :: idimVar ! La dimension de la variable
   logical :: mascaretFile ! Le fichier est bien un fichier mascaret
   integer :: taille1, taille2, taille3 ! Les tailles
   integer :: ind1, ind2, ind3 ! Les indice sur les tailles
   character (len=255) :: baliseText

   logical, parameter :: DEBUG=.false. ! Pour debugger

   mascaretFile=.false.

   if (importModele) then
       baliseModeleEtat="MASCARET_Model"
   else
       baliseModeleEtat="MASCARET_State"
   endif

   RetourErreur = TEST_INIT_AND_ID(Identifiant, 'IMPORT_XML')
   if (RetourErreur > 0 ) then
        RETURN
   end if


   call open_xml_file(xparser, NomFichier, iostat=RetourErreur)
   if (RetourErreur/=0) then
     ptrMsgsErreurs(Identifiant) = "IMPORT_XML - Impossible d'ouvrir "//NomFichier
     return
   endif

   call parse(xparser, &
              startElement_handler=START_ELEMENT_HANDLER, endElement_handler=END_ELEMENT_HANDLER, &
              characters_handler=CHARACTERS_HANDLER)

   call close_xml_t(xparser)
   geometrieModifiee(Identifiant) = .false.

contains

!.................................................................................................................................
! Debut d'une balise
! .................................................................................................................................
subroutine START_ELEMENT_HANDLER(uri, localname, name,attributes)
   use FOX_SAX

   implicit none

   character(len=*), intent(in)   :: uri
   character(len=*), intent(in)   :: localname
   character(len=*), intent(in)   :: name
   type(dictionary_t), intent(in) :: attributes

   integer :: i, icurdim, icurtaille
   character (len=*), parameter :: MODELE_NAME = 'MASCARET_Model', ETAT_NAME = 'MASCARET_State'
   character (len=80) :: curDim, curTaille
   character (len=5) :: dimVar

   baliseText=""

   ! Aucune balise n'a ete lue => Balise document
   if (.not.mascaretFile) then
     if ((name==MODELE_NAME .and. importModele) .or. &
         (name==ETAT_NAME .and. .not.importModele)) then
       mascaretFile=.true.
       return
     else
       if (importModele) then
         ptrMsgsErreurs(Identifiant) = "IMPORT_XML - Le fichier n'est pas de type modele Mascaret"
       else
         ptrMsgsErreurs(Identifiant) = "IMPORT_XML - Le fichier n'est pas de type etat Mascaret"
       endif
       RetourErreur = 2
       call stop_parser(xparser)
       return
     endif

   ! Pas de variable courante => On stocke la variable et ses attributs
   else if (curVar=="") then
     curVar=name
     tpVar=getValue(attributes,uri,"type")
     dimVar=getValue(attributes,uri,"dimension")
     read (dimVar,'(i5)') idimVar

     taille1=0
     taille2=0
     taille3=0
     ind1=0
     ind2=0
     ind3=0


     if (DEBUG) print *,'START_ELEMENT_HANDLER/curVar=',trim(curVar),' type=',trim(tpVar),' dimension=',trim(dimVar)

   ! Sous balises pour les dimensions > 1
   else if (name=="TABDOUBLE" .or. name=="TABINT" .or. name=="TABBOOL" .or. name=="TABSTRING" .or. &
            name=="DOUBLE" .or. name=="INT" .or. name=="BOOL" .or. name=="STRING") then
     curDim=getValue(attributes,uri,"dim")
     read (curDim,'(i5)') icurdim
     curTaille=getValue(attributes,uri,"taille")
     read (curTaille,'(i5)') icurtaille

     if (icurdim==idimVar) then
       taille1=icurtaille
       ind1=0
       ind2=0
       ind3=0

     else if (icurdim==idimVar-1) then
       taille2=icurtaille
       ind1=ind1+1
       ind2=0
       ind3=0

     else if (icurdim==idimVar-2) then
       taille3=icurtaille
       ind2=ind2+1
       ind3=0
     endif

     ! Si la dimension courante est 1, on redefinit systematiquement la taille de var
     if (icurtaille==0 .or. icurdim==1) then
       if (DEBUG) write (*,"('SET_TAILLE_VAR_MASCARET/curVar=',a,' ind1=',i0,' taille1=',i0,' taille2=',i0,' taille3=',i0)") &
         trim(curVar),ind1,taille1,taille2,taille3
       call SET_TAILLE_VAR_MASCARET(RetourErreur,Identifiant,curVar,ind1,taille1,taille2,taille3)
     endif

   endif

end subroutine START_ELEMENT_HANDLER

!.................................................................................................................................
! Affecte la valeur lue sur le fichier a la variable
! .................................................................................................................................
subroutine SET_VAR()
  implicit none

  integer :: ival
  real(8) :: dval
  logical :: bval

    if (tpVar=="TABDOUBLE" .or. tpVar=="DOUBLE") then
      read(baliseText,'(g160.3)') dval
      if (DEBUG) write (*,"('SET_DOUBLE_MASCARET/curVar=',a,' ind1=',i0,' ind2=',i0,' ind3=',i0,' val=',f0.0)") &
         trim(curVar),ind1,ind2,ind3,dval
      call SET_DOUBLE_MASCARET(RetourErreur,Identifiant,curVar,ind1,ind2,ind3,dval)

    else if (tpVar=="TABINT" .or. tpVar=="INT") then
      read(baliseText,'(i160)') ival
      if (DEBUG) write (*,"('SET_INT_MASCARET/curVar=',a,' ind1=',i0,' ind2=',i0,' ind3=',i0,' val=',i0)") &
         trim(curVar),ind1,ind2,ind3,ival
      call SET_INT_MASCARET(RetourErreur,Identifiant,curVar,ind1,ind2,ind3,ival)

    else if (tpVar=="TABSTRING" .or. tpVar=="STRING") then
      if (DEBUG) write (*,"('SET_STRING_MASCARET/curVar=',a,' ind1=',i0,' ind2=',i0,' ind3=',i0,' val=',a)") &
         trim(curVar),ind1,ind2,ind3,baliseText
      call SET_STRING_MASCARET(RetourErreur,Identifiant,curVar,ind1,ind2,ind3,baliseText)

    else if (tpVar=="TABBOOL" .or. tpVar=="BOOL") then
      if (baliseText=="FAUX") then
        bval=.false.
      else if (baliseText=="VRAI") then
        bval=.true.
      else
        ptrMsgsErreurs(Identifiant) = "IMPORT_XML - Variable "//trim(curVar)// &
          " de type BOOL, valeur "//trim(baliseText)//" invalide"
      endif
      if (DEBUG) write (*,"('SET_BOOL_MASCARET/curVar=',a,' ind1=',i0,' ind2=',i0,' ind3=',i0,' val=',a)") &
         trim(curVar),ind1,ind2,ind3,bval
      call SET_BOOL_MASCARET(RetourErreur,Identifiant,curVar,ind1,ind2,ind3,bval)

    else
      ptrMsgsErreurs(Identifiant) = "IMPORT_XML - Variable "//trim(curVar)// &
        " type invalide " // tpVar
      call stop_parser(xparser)
      RetourErreur = 2
    endif

end subroutine SET_VAR


!.................................................................................................................................
! Fermeture d'une balise
! .................................................................................................................................
subroutine END_ELEMENT_HANDLER(uri, localname, name)
  use FOX_SAX

  implicit none

  character(len=*), intent(in)   :: uri
  character(len=*), intent(in)   :: localname
  character(len=*), intent(in)   :: name

!  print *,'END_ELEMENT_HANDLER/BaliseText=',baliseText

  if (name==curVar) then

    if (idimVar==0) then
      call SET_VAR()
    endif

    if (DEBUG) print *,'END_ELEMENT_HANDLER/Fin Variable ',trim(curVar)
    curVar=""


  else if (name=="v") then

    if (idimVar==1) then
      ind1=ind1+1
    else if (idimVar==2) then
      ind2=ind2+1
    else
      ind3=ind3+1
    endif

    call SET_VAR()
  endif

end subroutine END_ELEMENT_HANDLER

!.................................................................................................................................
! Texte dans la balise
! .................................................................................................................................
subroutine CHARACTERS_HANDLER(str)
   use FOX_SAX

   implicit none

   character(len=*), intent(in) :: str

   baliseText=trim(baliseText)//str

end subroutine CHARACTERS_HANDLER

end subroutine IMPORT_XML



