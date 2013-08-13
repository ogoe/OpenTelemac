! -------------------------------------------------------------------------
! HeadOfPackageErrorModule ------------------------------------------------
!
!! saving of error messages of the "dredgesim"-package
!!
!
MODULE m_dredgesim_errors
  !
  ! ----------------------------------------------------------------------
  ! [A] base modules with frequently used methods
  ! ----------------------------------------------------------------------
  !
  ! [A.1] base module with type+methods "error handling"
  USE b_error, ONLY :         &
       ! routines / interfaces
       no_error,              &
       new_error, kill_error, &
       set_error_ierr, set_error_cerr, setup_error_act
  !
  ! ---------------------------------------------------------------------
  ! [B]  modules of the "dredgesim"-package
  ! ---------------------------------------------------------------------
  !
  ! [B.1] data module of the "dredgesim"-package 
  USE m_dredgesim_data, ONLY : all_errors ! 
  !
  IMPLICIT NONE
  PRIVATE
  !
  ! ---------------------------------------------------------------------
  ! [C] public declarations
  ! ---------------------------------------------------------------------
  !
  ! [C.1] interfaces
  !! intializing/allocating all error messages of the "dredgesim"-package
  INTERFACE init_dredgesim_errors
     MODULE PROCEDURE init_dredgesim_errors_d ! 
  END INTERFACE
  !! deintializing/deallocating all error messages of the "dredgesim"-package
  INTERFACE clear_dredgesim_errors
     MODULE PROCEDURE clear_dredgesim_errors_d ! 
  END INTERFACE
  !
  ! [C.2] list of public methods
  PUBLIC :: init_dredgesim_errors
  PUBLIC :: clear_dredgesim_errors
  !
  ! ---------------------------------------------------------------------
  ! [D] internal data types, internal data and methods
  ! ---------------------------------------------------------------------
  !
  !! name of the module
  CHARACTER (LEN=18) , PRIVATE, PARAMETER :: c_modname='m_dredgesim_errors' !
  !
CONTAINS
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+                                                                     +
  !+           PPPPPP   UUU  UUU  BBBBBB   LLL    III   CCCCC            +
  !+           PPP  PP  UUU  UUU  BBB  BB  LLL    III  CCC  CC           +
  !+           PPP  PP  UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPPPPP   UUU  UUU  BBBBBB   LLL    III  CCC               +
  !+           PPP      UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPP      UUU  UUU  BBB  BB  LLL    III  CCC               +
  !+           PPP       UUUUUU   BBB  BB  LLL    III  CCC  CC           +
  !+           PPP        UUUU    BBBBBB   LLLLLL III   CCCCC            +
  !+                                                                     +
  !+                                                                     +
  !+   MM     MM  EEEEEEE TTTTTTT  HHH  HH   OOOOO   DDDDDD    SSSSS     +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS   SS    +
  !+   MMMM MMMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS         +
  !+   MMM M MMM  EEEEEE    TTT    HHHHHHH  OOO  OO  DDD  DD   SSSSSS    +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SSS  SSS   +
  !+   MMM   MMM  EEEEEEE   TTT    HHH  HH   OOOOO   DDDDDD    SSSSSS    +
  !+                                                                     +
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! public methods or methods with access through PUBLIC interfaces
  !
  !! allocating/intializing all error messages of the "dredgesim"-package 
  !! subroutine does not throw error messages
  SUBROUTINE init_dredgesim_errors_d ( )
    !! variables
    INTEGER :: i, ic ! 
    !
    DO i=1,2
       !
       ic = 0
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 1 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: GENERAL\n'//&
               'package "p_dredgesim" ist not initialized\n'//&
               '--> execute INIT_dredgesim' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 2 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: GENERAL\n'//&
               'package "p_dredgesim" is already initialized\n'//&
               '--> execute CLEAR_dredgesim' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), 7500 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: PRINT-methods\n'//&
               'error when printing static data of "p_dredgesim_ui"\n'//&
               '--> check code / data in module "p_dredgesim_ui"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'one-dimensional CHARACTER-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'two-dimensional CHARACTER-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               'dimension 2 = "<idim2>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'one-dimensional INTEGER-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'two-dimensional INTEGER-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               'dimension 2 = "<idim2>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'one-dimensional REAL(Double)-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3210 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'two-dimensional REAL(Double)-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               'dimension 2 = "<idim2>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3220 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'three-dimensional REAL(Double)-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               'dimension 2 = "<idim2>"\n'//&
               'dimension 3 = "<idim3>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'one-dimensional FILE-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3400 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'one-dimensional LOGICAL-array\n'//&
               'name        = "<name>"\n'//&
               'dimension 1 = "<idim1>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3900 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'rank of an array does not fit requirements\n'//&
               'name                = "<name>"\n'//&
               'current rank        = <act>\n'//&
               'required rank       = <req>\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -3910 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private ALLOCATE-methods\n'//&
               'shape of two arrays do not conform\n'//&
               'name                = "<name>"\n'//&
               'current rank        = <actrank>\n'//&
               'required rank       = <reqrank>\n'//&
               'current shape       = <actshape>\n'//&
               'required shape      = <reqshape>\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -4000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private DEALLOCATE-methods\n'//&
               'CHARACTER-array\n'//&
               'name        = "<name>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -4100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private DEALLOCATE-methods\n'//&
               'INTEGER-array\n'//&
               'name        = "<name>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -4200 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private DEALLOCATE-methods\n'//&
               'REAL(Double)-array\n'//&
               'name        = "<name>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -4300 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private DEALLOCATE-methods\n'//&
               'FILE-array\n'//&
               'name        = "<name>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -4400 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private DEALLOCATE-methods\n'//&
               'LOGICAL-array\n'//&
               'name        = "<name>"\n'//&
               '--> check code / data in module "m_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -6001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private OK-methods (steering data)\n'//&
               'required file variant is not implemented\n'//&
               'req: file type      = "<FortranFileType>"\n'//&
               '     Fortran-FORM   = "<FortranFileForm>"\n'//&
               '     Fortran-ACCESS = "<FortranFileAccess>"\n'//&
               '     Fortran-DELIM  = "<FortranFileDelim>"\n'//&
               '--> implement required file variant in module "m_dredgesim_steer"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -7001 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private PRINT-methods (steering data)\n'//&
               'error in printing header before writing output of steering data\n'//&
               '--> check code in "m_dredgesim_steer"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -7002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private PRINT-methods (steering data)\n'//&
               'error in printing footer after writing output of steering data\n'//&
               '--> check code in "m_dredgesim_steer"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -7003 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private PRINT-methods (steering data)\n'//&
               'error in printing informationen of block and key name\n'//&
               'act. block number = <AktIblo>\n'//&
               'act. key number   = <AktIkey>\n'//&
               'act. block name   = <AktBloNam>\n'//&
               'act. key name     = <AktKeyNam>\n'//&
               '--> check code in "m_dredgesim_steer"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -7004 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private PRINT-methods (steering data)\n'//&
               'error in writing output of contents of keys\n'//&
               'act. block number = <AktIblo>\n'//&
               'act. key number   = <AktIkey>\n'//&
               'act. block name   = <AktBloNam>\n'//&
               'act. key name     = <AktKeyNam>\n'//&
               'act. key value    = <AktKeyValue>\n'//&
               '--> check code in "m_dredgesim_steer"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -7100 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private PRINT-methods (package data)\n'//&
               'error in writing hint that array is not allocated\n'//&
               'name of array = <name>\n'//&
               '--> check code / data in module "p_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -7110 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private PRINT-methods (package data)\n'//&
               'error in writing the SHAPE of an array\n'//&
               'name of array = <name>\n'//&
               '--> check code / data in module "p_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -7120 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private PRINT-methods (package data)\n'//&
               'error in writing selected data of an array\n'//&
               'name of array = <name>\n'//&
               '--> check code / data in module "p_dredgesim_data"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -8002 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private READ-Methods\n'//&
               'keyKeyzeile kann noch nicht gelesen werden !\n'//&
               'key word = <key_name> \n'//&
               'in block <blockname> \n'//&
               '--> extend code in module "m_dredgesim_steer"' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -40000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private CHECK-methods\n'//&
               'not all required pointer-arrays are allocated\n'//&
               'array "center_coord"           : ass = <ass1>\n'//&
               'array "poly_depth"             : ass = <ass2>\n'//&
               'array "poly_area"              : ass = <ass3>\n'//&
               'array "node_depth"             : ass = <ass4>\n'//&
               'array "edge_depth"             : ass = <ass5>\n'//&
               'array "cell_porosity"          : ass = <ass6>\n'//&
               'array "cell_sediment_fraction" : ass = <ass7>\n'//&
               '--> pointer in "ext_ds_ini_grid_import" needs to be defined correctly' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -40010 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private CHECK-methods\n'//&
               'size of the 1D-Pointer-array is not consistent to other arrays \n'//&
               'name         = <name>\n'//&
               'dim present  = <act1>\n'//&
               'dim required = <req1>\n'//&
               '--> pointer in "ext_ds_ini_grid_import" needs to be defined correctly' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -40020 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private CHECK-methods\n'//&
               'size of the 2D-Pointer-array is not consistent to other arrays \n'//&
               'name         = <name>\n'//&
               'dim present  = <act1>, <act2>\n'//&
               'dim required = <req1>, <req2>\n'//&
               '--> pointer in "ext_ds_ini_grid_import" needs to be defined correctly' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -40030 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private CHECK-methods\n'//&
               'the array "dredge_poly_index(:,:)" has no resonable data\n'//&
               'min. value = <min_act>, required = <min_req>\n'//&
               'max. value = <max_act>, required = <max_req>\n'//&
               'number of dredging spots = <dredge_act>, required = > 0\n'//&
               '--> data in "ext_ds_dredge_poly_index" needs to be defined correctly' )
       END IF
       ic = ic + 1
       IF ( i == 2 ) THEN
          IF ( no_error( ) ) CALL set_error_ierr ( all_errors(ic), -50000 )
          IF ( no_error( ) ) CALL set_error_cerr ( all_errors(ic), &
               'error category: private OUTPUT-methods\n'//&
               'at output of <print>\n'//&
               '--> check code / data' )
       END IF
       ! allocating arrays at first cycle (i==1)
       IF ( i == 1 ) THEN
          ALLOCATE ( all_errors( ic ) )
          CALL new_error( all_errors(:) )
       END IF
       !
    END DO
    !
  END SUBROUTINE init_dredgesim_errors_d
  !
  !! deallocating/deintializing all error messages of the "dredgesim"-package 
  !! subroutine does not throw error messages
  SUBROUTINE clear_dredgesim_errors_d ( )
    !
    CALL kill_error( all_errors(:) )
    DEALLOCATE ( all_errors )
    !
  END SUBROUTINE clear_dredgesim_errors_d
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+                                                                     +
  !+     PPPPPP   RRRRRR   III  VVV  VVV     AA    TTTTTTT  EEEEEEE      +
  !+     PPP  PP  RRR  RR  III  VVV  VVV    AAAA     TTT    EEE          +
  !+     PPP  PP  RRR  RR  III  VVV  VVV   AAA AA    TTT    EEE          +
  !+     PPPPPP   RRRRRR   III  VVV  VVV  AAA  AAA   TTT    EEEEEE       +
  !+     PPP      RRRRR    III  VVV  VVV  AAA  AAA   TTT    EEE          +
  !+     PPP      RRR RR   III   VVV VV   AAAAAAAA   TTT    EEE          +
  !+     PPP      RRR  RR  III    VVVV    AAA  AAA   TTT    EEE          +
  !+     PPP      RRR   RR III     VV     AAA  AAA   TTT    EEEEEEE      +
  !+                                                                     +
  !+                                                                     +
  !+   MM     MM  EEEEEEE TTTTTTT  HHH  HH   OOOOO   DDDDDD    SSSSS     +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS   SS    +
  !+   MMMM MMMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SS         +
  !+   MMM M MMM  EEEEEE    TTT    HHHHHHH  OOO  OO  DDD  DD   SSSSSS    +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD       SSS   +
  !+   MMM   MMM  EEE       TTT    HHH  HH  OOO  OO  DDD  DD  SSS  SSS   +
  !+   MMM   MMM  EEEEEEE   TTT    HHH  HH   OOOOO   DDDDDD    SSSSSS    +
  !+                                                                     +
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! local methods (PRIVATE)
  !
END MODULE m_dredgesim_errors
! TailOfApplikationErrorModule -------------------------------------------
