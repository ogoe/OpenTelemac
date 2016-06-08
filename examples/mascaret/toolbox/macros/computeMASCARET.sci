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
// Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant et l'etat precedent
// Arguments en entree :
//      Id          : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//      TpsInitial  : Temps initiale du calcul
//      TpsFinal    : Temps final du calcul
//      PasTps      : Pas de temps interne du calcul, si négatif ou egale a zero, le pas de temps est recupere du modele
//      Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
function [erreur]=computeMASCARET(id,TpsInitial, TpsFinal, PasTps,impression)
    [erreur]=fort("calcul_mascaret",id,2,"i",TpsInitial,3,"d",TpsFinal,4,"d",PasTps,5,"d",impression,6,"i","out",[1,1],1,"i");
    return
endfunction