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

module M_PARAMETRES_QUALITE_EAU_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : 8.1.3              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   TYPE PARAMETRES_QUALITE_EAU_T
      real(DOUBLE), dimension(:), pointer   :: ParQual_Eau => null() 
   end TYPE PARAMETRES_QUALITE_EAU_T

end module M_PARAMETRES_QUALITE_EAU_T
