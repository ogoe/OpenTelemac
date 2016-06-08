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
//  Export d'un modele ou d'un état Mascaret dans un fichier XML
//  Arguments en entree :
//      id           : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//      NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 132 caracteres)
//      AvecDesc     : si vrai (valeur 1), ajoute la description de la variable
//      exportModele : si vrai (valeur 1), exportation du modele, sinon export de l'etat
function [erreur]=exportXmlMASCARET(id,nomFichier,AvecDesc,exportModele)
    tailleNomFichier=255
    nomFichierComplete=fillString(nomFichier,tailleNomFichier)
    [erreur]=fort("export_xml",id,2,"i",nomFichierComplete,3,"c",AvecDesc,4,"i",exportModele,5,"i","out",[1,1],1,"i");
    return
endfunction