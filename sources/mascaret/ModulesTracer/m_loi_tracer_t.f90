!== Copyright (C) 2000-2015 EDF-CEREMA ==
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

Module M_LOI_TRACER_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : 8.1.0              EDF-CEREMA
!***********************************************************************

   use M_PRECISION

   implicit none

   integer, parameter :: LOI_UNITE_SECONDE = 1 !  Seconde "S"
   integer, parameter :: LOI_UNITE_MINUTE  = 2 !  Minute  "M"
   integer, parameter :: LOI_UNITE_HEURE   = 3 !  Heure   "H"
   integer, parameter :: LOI_UNITE_JOUR    = 4 !  Jour    "J"
   integer, parameter :: LOI_UNITE_NB_MAX  = 15

   TYPE LOI_TRACER_T
      sequence
      character(30)                         :: Nom        ! Nom de la loi
      real(DOUBLE), dimension(:), pointer   :: Temps => null()       ! Temps
      real(DOUBLE), dimension(:,:), pointer :: Conc => null()        ! Concentration en traceur
     ! (ou eventuellement flux volumique ou surfacique de traceur pour les sources)
   END TYPE LOI_TRACER_T

End Module M_LOI_TRACER_T
