//== Copyright (C) 2000-2013 EDF-CETMEF ==
//
//   This file is part of MASCARET.
//
//   MASCARET is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   MASCARET is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
//

// *********************************************************************
// PROGICIEL : MASCARET       J.-M. LACOMBE
//
// VERSION : 8.0.0              EDF-CETMEF
// *********************************************************************

//.................................................................................................................................
// Importation d'un modele mascaret a partir des fichiers natifs de Mascaret
// Arguments en entree :
//      id             : Identifiant de l'instance Mascaret retourne par "MASCARET_create"
//      TabNomFichier  : Tableau des noms des fichiers natifs Mascaret a importer (un nom de fichier ne doit pas depasser 255 caracteres)
//      TypeNomFichier : Tableau des type des fichiers natifs Mascaret a importer, valeurs acceptees :
//                       "dico", "casier", "geo", "loi", "cas","listing","damocle", "res", "listing_casier", "listing_liaison",
//                       "res_casier", "res_liaison"
//      Impression     : impression sur les fichiers listing (1-> Vrai 0-> Faux)
function [erreur]=MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression)
    tabNomFichierAdapt=adaptTabStringIntentIn(TabNomFichier, 255)
    tabTypeFichierAdapt=adaptTabStringIntentIn(TypeNomFichier, 40)
	taille = max(size(TabNomFichier))
    [erreur]=fort("import_modele_mascaret",id,2,"i",tabNomFichierAdapt,3,"c",tabTypeFichierAdapt,4,"c",taille,5,"i",impression,6,"i","out",[1,1],1,"i");
    return
endfunction