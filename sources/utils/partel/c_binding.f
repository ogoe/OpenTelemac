!                    ****************
                     MODULE C_BINDING
!                    ****************
!
!***********************************************************************
! PARTEL
!***********************************************************************
!
!brief    call to the partionning software
!
!history   Y.AUDOUIN
!+
!+
!+         created
!
!
!
        use, intrinsic :: iso_c_binding
!
        implicit none
!
        interface
!
          ! Metis function
          SUBROUTINE C_MYMETIS_PARTMESHDUAL
     &        (NELEM, NPOIN, EPTR, EIND, VWGT,
     &         VSIZE, NCOMMONNODES, NPARTS, TPWGTS,
     &         OPTION, EDGECUT, EPART, NPART)
     &        BIND(C, NAME='METIS_PartMeshDual')

            USE,intrinsic :: ISO_C_BINDING
            implicit none
            type(c_ptr),value :: nelem
            type(c_ptr),value :: npoin
            integer(c_int) :: eptr(*)
            integer(c_int) :: eind(*)
            type(c_ptr), value :: vwgt
            type(c_ptr), value :: vsize
            type(c_ptr),value :: ncommonnodes
            type(c_ptr),value :: nparts
            type(c_ptr), value :: TPWGTS
            type(c_ptr), value :: option
            type(c_ptr),value :: edgecut
            integer(c_int) :: epart(*)
            integer(c_int) :: npart(*)
!
          END SUBROUTINE
        end interface
      contains
          SUBROUTINE MYMETIS_PARTMESHDUAL
     &        (NELEM, NPOIN, EPTR, EIND,
     &          NCOMMONNODES, NPARTS,
     &          EDGECUT, EPART, NPART)

            USE,intrinsic :: ISO_C_BINDING
            implicit none
            integer(c_int),target :: nelem
            integer(c_int),target :: npoin
            integer(c_int),target :: eptr(*)
            integer(c_int),target :: eind(*)
            integer(c_int),target :: ncommonnodes
            integer(c_int),target :: nparts
            integer(c_int),target :: edgecut
            integer(c_int),target :: epart(*)
            integer(c_int),target :: npart(*)
!
            call C_MYMETIS_PARTMESHDUAL
     &        (C_LOC(NELEM), C_LOC(NPOIN), EPTR,
     &         EIND, C_NULL_PTR,
     &         C_NULL_PTR, C_LOC(NCOMMONNODES), C_LOC(NPARTS),
     &         C_NULL_PTR,
     &         C_NULL_PTR, C_LOC(EDGECUT), EPART, NPART)
          END SUBROUTINE

      END MODULE C_BINDING
