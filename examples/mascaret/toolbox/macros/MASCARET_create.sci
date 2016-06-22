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
// Initialise les ressources associees a une instance de Mascaret (Modele et Etat)
// Retourne l'identifiant de l'instance
// Argument en sortie :
//        id : Identifiant de l'instance Mascaret utilise en argument des autres fonctions de l'API
function [e,id]=MASCARET_create()
   [e,id]=call("create_mascaret","out",[1,1],1,"i",[1,1],2,"i");
   return
endfunction
