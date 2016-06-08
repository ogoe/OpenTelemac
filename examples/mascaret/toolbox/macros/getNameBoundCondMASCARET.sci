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
// Recupere le nom d'une condition limite du modele ainsi que le numero de la loi correspondante
// Arguments en entree :
//        id          : Identifiant de l'instance Mascaret retourne par "CreaetMascaret"
//        NumCL       : Le numero de la condition limite dont on veut connaitre le nom
// Arguments en sortie :
//        NomCL  : Le nom de la condition limite dans le modele
//        NumLoi : Numero de la Loi correspondant a la condition limite dans le modele
function [erreur,nomCL,numLoi]=getNameBoundCondMASCARET(id,numCL)
    [erreur,nomCLFortran,numLoi]=fort("get_nom_condition_limite_mascaret",id,2,"i",numCL,3,"i","out",[1,1],1,"i",[30,1],4,"c",[1,1],5,"i");
	nomCL = stripblanks(nomCLFortran)
    return
endfunction