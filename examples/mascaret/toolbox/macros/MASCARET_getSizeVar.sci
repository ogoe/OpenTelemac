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
// Accede a la taille maximum des indexes pour acceder a une variable
// Arguments en entree :
//      id          : Identifiant de l'instance Mascaret retourne par "MASCARET_create"
//      NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
//      index1      : valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
// Arguments en sortie :
//      taille1     : valeur max du 1er indice
//      taille2     : valeur max du 2e  indice
//      taille3     : valeur max du 3e  indice
function [erreur,taille1,taille2,taille3]=MASCARET_getSizeVar(id,nomVar, index1)
    tailleNomVar=40
    nomVarComplete=fillString(nomVar,tailleNomVar)
    [erreur,taille1,taille2,taille3]=fort("get_taille_var_mascaret",id,2,"i",nomVarComplete,3,"c",index1,4,"i","out",[1,1],1,"i",[1,1],5,"i",[1,1],6,"i",[1,1],7,"i");
    return
endfunction