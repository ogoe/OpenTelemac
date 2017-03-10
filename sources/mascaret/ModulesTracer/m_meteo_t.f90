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

module M_METEO_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : 8.1.3              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION
   ! Constantes reperant le  type de "loi"
   !--------------------------------------
   integer     , parameter :: METEO_UNITE_SECONDE = 1 !  Seconde "S"
   integer     , parameter :: METEO_UNITE_MINUTE  = 2 !  Minute  "M"
   integer     , parameter :: METEO_UNITE_HEURE   = 3 !  Heure   "H"
   integer     , parameter :: METEO_UNITE_JOUR    = 4 !  Jour    "J"
   integer     , parameter :: METEO_UNITE_NB_MAX  = 4

   type METEO_T
      sequence
      character(30)                         :: Nom
      real(DOUBLE), dimension(:)  , pointer :: Temps => null() 
      ! parametres meteo pour Eutro et Biomass
      real(DOUBLE), dimension(:)  , pointer :: Temp => null() 
      real(DOUBLE), dimension(:)  , pointer :: I0 => null() 
      ! parametres meteo pour Thermic
      real(DOUBLE), dimension(:)  , pointer :: T_Air => null() 
      real(DOUBLE), dimension(:)  , pointer :: P_Vap => null() 
      real(DOUBLE), dimension(:)  , pointer :: Vit_Vent => null() 
      real(DOUBLE), dimension(:)  , pointer :: Nebulo => null() 
      real(DOUBLE), dimension(:)  , pointer :: Ray3 => null() 
      real(DOUBLE), dimension(:)  , pointer :: P_atm => null() 
   end type METEO_T

end module M_METEO_T
