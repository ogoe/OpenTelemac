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
// Recupere la liste des variables de Mascaret accompagnee d'une description
// Argument en entree :
//      id          : Identifiant de l'instance Mascaret retourne par "createMASCARET"
// Arguments en sortie :
//      TabVar      : Tableau des noms de variable du modele ou de l'etat
//      TabDesc     : Tableau des descriptions de variable du modele ou de l'etat
//      Taille      : Taille des tableaux des noms et des descriptions de variable
function [erreur,TabVar,TabDesc]=getDescVarMASCARET(id)
    [nbvar]=call("get_nb_var_mascaret","out",[1,1],1,"i");
    [erreur,tabVarBrute,tabDescBrute]=fort("get_desc_var_mascaret",id,2,"i",nbvar,5,"i","out",[1,1],1,"i",[40,nbvar],3,"c",[110,nbvar],4,"c");
    TabVar = tokens(tabVarBrute," ");
    indice=110:110:((110*nbvar)-1);
    TabDesc = stripblanks(strsplit(tabDescBrute,indice));
    return
endfunction