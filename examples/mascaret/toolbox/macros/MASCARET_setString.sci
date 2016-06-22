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
// Mutateur permettant de modifier une valeur d'une variable d'une instance du modele ou de l'etat
// Arguments en entree :
//      id          : Identifiant de l'instance Mascaret retourne par "MASCARET_create"
//      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par GetDescVarMascaret
//      index1      : valeur du 1er indice
//      index2      : valeur du 2e indice
//      index3      : valeur du 3e indice
//      valeur      : nouvelle valeur de la variable pour les indexes specifies (sous la forme d'une chaine de caractere)
function [erreur]=MASCARET_setString(id,nomVar,index1,index2,index3,valeur)
    tailleNomVar=40
    nomVarComplete=fillString(nomVar,tailleNomVar)
	tailleValeur = 256
	valeurComplete=fillString(valeur,tailleValeur)
    [erreur]=fort("set_string_mascaret",id,2,"i",nomVarComplete,3,"c",index1,4,"i",index2,5,"i",index3,6,"i",valeurComplete,7,"c","out",[1,1],1,"i");
    return
endfunction