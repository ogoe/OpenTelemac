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
// Recupere le nombre de condition limite dans le modele
// Argument en entree :
//        id   : Identifiant de l'instance Mascaret retourne par "createMASCARET"
// Arguments en sortie :
//        NbCL : le nombre de condition limite dans le modele
function [erreur,nbCl]=getNbBoundCondMASCARET(id)
    [erreur,nbCl]=fort("get_nb_condition_limite_mascaret",id,2,"i","out",[1,1],1,"i",[1,1],3,"i");
    return
endfunction