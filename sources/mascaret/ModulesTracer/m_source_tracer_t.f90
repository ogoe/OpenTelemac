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

module M_SOURCE_TRACER_T
!***********************************************************************
! PROGICIEL : TRACER         S.MANDELKERN
!
! VERSION : 8.1.0              EDF-CEREMA
!***********************************************************************

   !=========================== Declarations ==============================

   use M_PRECISION

   integer     , parameter :: SOURCE_TRACER_TYPE_VOLUMIQUE  = 1
   integer     , parameter :: SOURCE_TRACER_TYPE_SURFACIQUE = 2
   integer     , parameter :: SOURCE_TRACER_TYPE_FLUX_TEMP  = 3
   integer     , parameter :: SOURCE_TRACER_TYPE_NB_MAX     = 3

   TYPE SOURCE_TRACER_T
      sequence
      character(30) :: Nom             ! Nom de la source
      integer       :: Type            ! Type de la source
      integer       :: NumBranche      ! Numero de la section debut de la source
      real(DOUBLE)  :: AbscisseRel     ! Abscisse relative de la source
      real(DOUBLE)  :: Longueur        ! Longueur de la source
      integer       :: SectionAm       ! Numero de la section debut
      integer       :: SectionAv       ! Numero de la section fin
      integer       :: NumeroLoi     ! Numero de la loi associee
      real (DOUBLE),dimension(:),pointer :: Apport_source  => null() ! Apport par la source pour chaque traceur (conc, flux volum ou surf selon le cas)
      logical       :: SuperpositionApport ! Si superposition a un apport hydrau
      integer       :: NumeroApport        ! Numero de l'apport hydrau associe
   END TYPE SOURCE_TRACER_T

end module M_SOURCE_TRACER_T
