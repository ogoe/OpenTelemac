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
// Importation de l'etat Mascaret a partir de la ligne d'eau initiale (debit, cote) passee en argument
// Arguments en entree :
//      id          : Identifiant de l'instance Mascaret retourne par "createMASCARET"
//      Q           : Tableau des debits de la ligne d'eau initiale
//      Z           : Tableau des cotes de la ligne d'eau initiale
function [erreur]=initStateMASCARET(id,Q, Z)
    Taille = max(size(Q));
    [erreur]=fort("init_ligne_mascaret",id,2,"i",Q,3,"d",Z,4,"d",Taille,5,"i","out",[1,1],1,"i");
    return
endfunction
