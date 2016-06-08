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

module M_CONSTANTES_TRACER_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : 8.1.0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================
   use M_PRECISION

   TYPE CONSTANTES_TRACER_T
      logical                               :: Conv
      integer                               :: ScheConv
      integer                               :: OrdreVF
      real(DOUBLE)                          :: ParamW
      logical                               :: LimiteurPente 
      logical                               :: Diff
      integer                               :: OptionCalculDisp
      real(DOUBLE), dimension(2)            :: CoefDiffu
   end TYPE CONSTANTES_TRACER_T

end module M_CONSTANTES_TRACER_T
