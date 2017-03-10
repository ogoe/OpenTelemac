!== Copyright (C) 2000-2017 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

! *********************************************************************
! PROGICIEL : MASCARET       J.-M. LACOMBE
!
! VERSION : 8.1.3              EDF-CEREMA
! *********************************************************************
   !.................................................................................................................................
   ! Retourne la version courante de Mascaret
   ! .................................................................................................................................
    subroutine VERSION_MASCARET(Majeur, Mineur, Micro)
        implicit none
        integer, intent(out) :: Majeur  ! Numero de la version Majeur de Mascaret
        integer, intent(out) :: Mineur  ! Numero de la version Mineur de Mascaret
        integer, intent(out) :: Micro   ! Numero de la version Micro de Mascaret

        Majeur = 8
        Mineur = 1
        Micro  = 3

    end subroutine VERSION_MASCARET
