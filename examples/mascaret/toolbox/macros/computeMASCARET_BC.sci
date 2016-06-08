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
// Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant, les nouvelles contions limites et l'etat precedent
// Arguments en entree :
//       Id          : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
//       TpsInitial  : Temps initiale du calcul
//       TpsFinal    : Temps final du calcul
//       PasTps      : Pas de temps interne du calcul
//       TpsCl       : Vecteur temps des nouvelles conditions limites
//       TailleTpsCL : Nombre d'element du vecteur temps des nouvelles conditions limites
//       Cl1         : Matrice de la premiere composante des nouvelles conditions limites
//       Cl2         : Matrice de la deuxieme composante des nouvelles conditions limites
//       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
// .................................................................................................................................
function [erreur]=computeMASCARET_BC(id,TpsInitial, TpsFinal, PasTps,TpsCl, Cl1, Cl2, impression)
    TailleTpsCL = max(size(TpsCl));
    [erreur]=fort("calcul_mascaret_condition_limite",id,2,"i",TpsInitial,3,"d",TpsFinal,4,"d",PasTps,5,"d",TpsCl,6,"d",..
	              TailleTpsCL,7,"i",Cl1,8,"d",Cl2,9,"d",impression,10,"i","out",[1,1],1,"i");
    return
endfunction