!                    **************************
                     MODULE INTERFACE_WAQTEL
!                    **************************
!
!
!***********************************************************************
! TELEMAC2D 6.2
!***********************************************************************
!
!
!-----------------------------------------------------------------------
!
!     DEFINITION OF INTERFACES
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECWAQPAR
     &(IFIC,NPOIN,NBRECH,OPTNBR,TDECBR,DURBR,ZFINBR,NUMPSD,MESH,
     & ZDECBR,NBNDBR,INDBR,ZCRBR)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER          , INTENT(IN)    :: IFIC
        INTEGER          , INTENT(IN)    :: NPOIN
        INTEGER          , INTENT(INOUT)    :: NBRECH
        TYPE(BIEF_OBJ), INTENT(INOUT) :: OPTNBR,TDECBR,DURBR,ZFINBR
        TYPE(BIEF_OBJ), INTENT(INOUT) :: ZDECBR
        TYPE(BIEF_OBJ), INTENT(INOUT) :: NUMPSD,NBNDBR,INDBR,ZCRBR
        TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS_BIOMASS
        IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS_EUTRO
        IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS_MICROPOL
        IMPLICIT NONE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS_O2
     & (NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,NTRAC,GRAV,HPROP,UN,VN,ZF,
     &  K2,FORMCS)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER          , INTENT(IN   ) :: FORMCS,FORMK2,NPOIN,NTRAC
        DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
        DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
        DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
        TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN
        TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,K2
        DOUBLE PRECISION, INTENT(IN) :: GRAV
        TYPE(BIEF_OBJ)   , INTENT(IN) :: HPROP
        TYPE(BIEF_OBJ)   , INTENT(IN) :: UN,VN,ZF
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS_THERMIC
     & (NPOIN,TN,TEXP,HPROP,PATMOS,IND_T,LISTIN)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER, INTENT(IN)             :: NPOIN
        TYPE(BIEF_OBJ), INTENT(IN)      :: TN
        TYPE(BIEF_OBJ), INTENT(INOUT)   :: TEXP
        TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP
        TYPE(BIEF_OBJ), INTENT(IN)      :: PATMOS
        INTEGER,        INTENT(IN)      :: IND_T
        LOGICAL,        INTENT(IN)      :: LISTIN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDON_WAQTEL
     & (FILE_DESC,PATH,NCAR,CODE)
        USE DECLARATIONS_WAQTEL
        IMPLICIT NONE
        CHARACTER(LEN=24), INTENT(IN)     :: CODE
        CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,MAXKEY)
        INTEGER, INTENT(IN)               :: NCAR
        CHARACTER(LEN=250), INTENT(IN)    :: PATH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOURCE_WAQ
     &(NPOIN,TEXP,TIMP,YASMI,TSCEXP,HPROP,TN,TETAT,
     & AT,DT,NTRAC,IND_T,GRAV,UN,VN,ZF,PATMOS,LISTIN,
     & WAQPROCESS)
        USE BIEF_DEF
        IMPLICIT NONE
        INTEGER          , INTENT(IN)    :: NPOIN
        INTEGER          , INTENT(IN)    :: NTRAC,WAQPROCESS
        LOGICAL          , INTENT(INOUT) :: YASMI(*)
        DOUBLE PRECISION , INTENT(IN)    :: AT,DT,TETAT
        TYPE(BIEF_OBJ)   , INTENT(IN)    :: TN
        TYPE(BIEF_OBJ)   , INTENT(IN)    :: HPROP
        TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TSCEXP,TEXP,TIMP
        INTEGER          , INTENT(IN)    :: IND_T
        DOUBLE PRECISION, INTENT(IN) :: GRAV
        TYPE(BIEF_OBJ)   , INTENT(IN) :: UN,VN,ZF
        TYPE(BIEF_OBJ), INTENT(IN)      :: PATMOS
        LOGICAL,        INTENT(IN)      :: LISTIN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NAMETRAC_WAQ
     &  (TEXTE,TEXTPR,NAMETRAC,NTRAC,IND_T,WAQPROCESS)
        IMPLICIT NONE
        CHARACTER(LEN=32), INTENT(INOUT) :: NAMETRAC(*) 
        INTEGER, INTENT(IN)              :: NTRAC,WAQPROCESS
        CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
        INTEGER, INTENT(INOUT)           :: IND_T
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE INCREASE_NTRAC
     &  (NTRAC,IND_T,WAQPROCESS)
        IMPLICIT NONE
        INTEGER, INTENT(INOUT)::  IND_T,NTRAC
        INTEGER, INTENT(IN)   ::  WAQPROCESS
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE POINT_WAQTEL
     &  (WAQPROCESS,MESH,IELM1,VENT,WINDX,WINDY)
        USE BIEF_DEF
        IMPLICIT NONE
!
        LOGICAL,         INTENT(IN   ) :: VENT
        INTEGER,         INTENT(IN   ) :: IELM1,WAQPROCESS
        TYPE(BIEF_OBJ ), INTENT(INOUT) :: WINDX,WINDY
        TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
        END SUBROUTINE
      END INTERFACE

!
!-----------------------------------------------------------------------
!
      END MODULE INTERFACE_WAQTEL
