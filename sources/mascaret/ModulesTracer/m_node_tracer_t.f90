!== Copyright (C) 2000-2016 EDF-CEREMA ==
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

Module M_NODE_TRACER_T
!***********************************************************************
! PROGICIEL : MASCARET-TRACER     F. ZAOUI
!
! VERSION : 8.1.1                   EDF-CEREMA
!***********************************************************************

   integer     , dimension(:)     ,    pointer :: NB_CHILD => null()    ! POUR CHAQUE NOEUD, LE NOMBRE DE NOEUDS AVAL
   integer     , dimension(:)     ,    pointer :: NB_PARENT => null()   ! POUR CHAQUE NOEUD, LE NOMBRE DE NOEUDS AMONT
   integer     , dimension(:,:)   ,    pointer :: CHILD => null()   ! POUR CHAQUE NOEUD, LA LISTE DES NOEUDS AVAL
   integer     , dimension(:,:)   ,    pointer :: PARENT => null()   ! POUR CHAQUE NOEUD, LA LISTE DES NOEUDS AMONT


End Module M_NODE_TRACER_T
