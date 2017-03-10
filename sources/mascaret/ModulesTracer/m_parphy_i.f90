!== Copyright (C) 2000-2017 EDF-CEREMA ==
!
!   This file is part of MASCARET-TRACER.
!
!   MASCARET-TRACER is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET-TRACER is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET-TRACER.  If not, see <http://www.gnu.org/licenses/>
!

module M_PARPHY_I
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : 8.1.3              EDF-CEREMA
!***********************************************************************

   interface

   subroutine LEC_PARPHY( &
         FICHIER_PARPHY , & ! Fichier des parametres physiques
                 PARPHY , & ! Parmetres physiques
                 ERREUR )

   !========================= Declarations ===========================
   use M_PRECISION
   use M_PARAMETRES_QUALITE_EAU_T ! Type Parametres Qualite d'eau_T
   use M_FICHIER_T
   use M_ERREUR_T
   use M_TRAITER_ERREUR_I         ! Inteface generique de gestion des erreurs
   use M_MESSAGE_C                ! Definition des messages d'erreur

   implicit none

   ! Arguments
   type(PARAMETRES_QUALITE_EAU_T) :: ParPhy
   type (Fichier_T)               :: Fichier_Parphy
   type (ERREUR_T)                :: Erreur
   integer                        :: nb_parphy

   end subroutine LEC_PARPHY

   end interface

end module M_PARPHY_I
