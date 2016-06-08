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
//  Import d'un modele Mascaret depuis un fichier XML
//  Arguments en entree :
//      id           : Identifiant de l'instance Mascaret retourne par "createMASCARET"
//      NomFichier   : Nom du fichier XML contenant le Modele ou l'etat Mascaret (max 132 caracteres)
//      importModele : si vrai (valeur 1), import du modele, sinon import de l'etat
function [erreur]=importXmlMASCARET(id,nomFichier,importModele)
    tailleNomFichier=255
    nomFichierComplete=fillString(nomFichier,tailleNomFichier)
    [erreur]=fort("import_xml",id,2,"i",nomFichierComplete,3,"c",importModele,4,"i","out",[1,1],1,"i");
    return
endfunction