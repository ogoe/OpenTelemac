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
// Importation de l'etat Mascaret a partir du fichier natif contenant la ligne d'eau initiale
// Arguments en entree :
//      id          : Identifiant de l'instance Mascaret retourne par "createMASCARET"
//      NomFichier  : Nom du fichier natif contenant la ligne d'eau initiale (max 255 carateres)
//      Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
function [erreur]=initStateNameMASCARET(id,nomFichier,impression)
    tailleNomFichier=255
    nomFichierComplete=fillString(nomFichier,tailleNomFichier)
    [erreur]=fort("init_etat_mascaret",id,2,"i",nomFichierComplete,3,"c",impression,4,"i","out",[1,1],1,"i");
    return
endfunction