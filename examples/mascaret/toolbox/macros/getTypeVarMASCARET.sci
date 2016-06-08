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

// .................................................................................................................................
// Recupere des informations sur une variable (identifie par NomVar) :
//   * le type (reel, entier, booleen, chaine de caractere),
//   * la categorie (modele ou etat)
//   * si la variable est modifiable dans l'API avec SetXxxxxMascaret
//   * le nombre d'indexe pour acceder a la variable
// Arguments en entree :
//      id          : Identifiant de l'instance Mascaret retourne par "createMASCARET"
//      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
// Arguments en sortie :
//      TypeVar     : valeurs possibles :"INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
//      Categorie   : valeurs possibles : "MODEL" ou "STATE"
//      Modifiable  : Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
//      dimVar      : dimension (c'est a dire le nombre d'indexe de 0 a 3)
function [erreur,typeVar,categorie,modifiable,dimVar]=getTypeVarMASCARET(id,nomVar)
    tailleNomVar=40;
    nomVarComplete=fillString(nomVar,tailleNomVar);
    [erreur,typeVarF,categorieF,modifiable,dimVar]=fort("get_type_var_mascaret",id,2,"i",nomVarComplete,3,"c","out",[1,1],1,"i",[10,1],4,"c",[10,1],5,"c",[1,1],6,"i",[1,1],7,"i");
	typeVar = stripblanks(typeVarF);
	categorie = stripblanks(categorieF);
    return
endfunction