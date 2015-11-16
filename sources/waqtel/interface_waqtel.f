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
        SUBROUTINE ALGAE_DEATH
     &(ALD,CMOR,TR,TRESP,GT,TOX,NPOIN)
      IMPLICIT NONE
      INTEGER         , INTENT(IN):: NPOIN
      DOUBLE PRECISION, INTENT(IN):: CMOR(2),TR(NPOIN),TOX,TRESP,GT
      DOUBLE PRECISION, INTENT(INOUT)::ALD(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ALGAE_GROWTH
     &(ALG,CMAX,RAY,GT,NUTR,TOX,NPOIN )
      IMPLICIT NONE
      INTEGER         , INTENT(IN):: NPOIN
      DOUBLE PRECISION, INTENT(IN):: CMAX,RAY(NPOIN),GT,NUTR(NPOIN),TOX
      DOUBLE PRECISION, INTENT(INOUT)::ALG(NPOIN)
        END SUBROUTINE
      END INTERFACE
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
     &     (NPOIN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,HPROP,T1,T2,DEBUG)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC,DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF,T1,T2
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS_EUTRO    
     &   (NPOIN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,
     &    HPROP,T1,T2,T3,U,V,DEBUG)
         USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC,DEBUG
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,U,V
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF,T1,T2,T3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS_MICROPOL (NPOIN,NTRAC,TN,TEXP,
     &                             HPROP,CF,UN,VN,T1,T2,T3,T4)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,T1,T2,T3,T4
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS_O2
     & (NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,NTRAC,
     &  GRAV,HPROP,UN,VN,ZF)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: FORMK2,NPOIN,NTRAC
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP,GRAV
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,UN,VN,ZF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP
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
        SUBROUTINE DEPOS_FX
     &  (SEDP,TAUB,TAUS,VITCHU,NPOIN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: TAUS,VITCHU
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TAUB
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: SEDP
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE EROSION_FX
     & (SEDERO,TAUB,SF,TAUR,ERO,ZZERO,NPOIN)
        USE BIEF_DEF 
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: TAUR,ERO,ZZERO
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TAUB,SF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: SEDERO
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
!      INTERFACE
!        SUBROUTINE NAMETRAC_WAQ
!     &  (NAMETRAC,WAQ,SECCURRENT,WAQPROCESS,NTRAC,IND_SEC,
!     &   MAXTRA,ICONVFT,VISCT)
!        USE BIEF_DEF
!        IMPLICIT NONE
!      INTEGER, INTENT(IN)             ::  WAQPROCESS,NTRAC,IND_SEC
!      INTEGER, INTENT(IN)             ::  MAXTRA
!      LOGICAL, INTENT(IN)             ::  WAQ,SECCURRENT
!      INTEGER, INTENT(INOUT)          ::  ICONVFT(MAXTRA)
!      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
!      TYPE(BIEF_OBJ)   , INTENT(INOUT)::  VISCT
!        END SUBROUTINE
!      END INTERFACE
      INTERFACE
        SUBROUTINE NAMETRAC_WAQ
     &  (TEXTE,TEXTPR,NAMETRAC,NTRAC,IND_T,WAQPROCESS,
     &   MAXTRA,ICONVFT,VISCT)
        USE BIEF_DEF
        IMPLICIT NONE
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*) 
      INTEGER, INTENT(IN)             ::  WAQPROCESS,NTRAC,MAXTRA
      INTEGER, INTENT(INOUT)          ::  IND_T
      CHARACTER(LEN=32), INTENT(INOUT)::  TEXTE(*),TEXTPR(*)
      INTEGER, INTENT(INOUT)          ::  ICONVFT(MAXTRA)
      TYPE(BIEF_OBJ)   , INTENT(INOUT)::  VISCT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NUTEFF
     &(LNUT,TRR,NPOIN,IPO4,INO3,KP,KN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: NPOIN,IPO4,INO3
      DOUBLE PRECISION, INTENT(IN)    :: KN,KP
      DOUBLE PRECISION, INTENT(INOUT) :: LNUT(NPOIN)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: TRR
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE INCREASE_NTRAC
     &  (NTRAC,IND_T,WAQPROCESS,ADDTR)
        IMPLICIT NONE
        INTEGER, INTENT(INOUT)::  IND_T,NTRAC,ADDTR
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
      INTERFACE
        SUBROUTINE RAY_EFFECT
     &(SECCHI,TRR,NPOIN,BETA,I0,IK,EFF,H)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: TRR(NPOIN),BETA,I0,IK,SECCHI
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: H
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: EFF
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE REAER
     &(FORMK2,K2,K22,NPOIN,UN,VN,H,EPS)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: FORMK2,NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: EPS,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: H,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: K2
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE REAER_WEIR
     &(FORMRS,H1,H2,ABRS,WATTEMP,EPS,O2SATU,TRUP,TN,
     & ADDTR,WAQPROCESS,IR,NTRAC)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: FORMRS,ADDTR,WAQPROCESS,IR
      INTEGER         , INTENT(IN)     :: NTRAC
      DOUBLE PRECISION, INTENT(IN)     :: EPS,H1,H2,ABRS(2),WATTEMP
      DOUBLE PRECISION, INTENT(IN)     :: TRUP,O2SATU
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SATUR_O2
     &(SATO2,FORMCS,WATTEMP,EPS)
        IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: FORMCS
      DOUBLE PRECISION, INTENT(IN)    :: WATTEMP,EPS
      DOUBLE PRECISION, INTENT(INOUT) :: SATO2
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE SOURCE_WAQ
     &(NPOIN,TEXP,TIMP,TN,NTRAC,WAQPROCESS,
     & RAYEFF,IND_T,HPROP,U,V,CF,T1,T2,T3,T4,PATMOS,LISTIN,GRAV,
     & ZF,DEBUG)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN)    :: NPOIN,WAQPROCESS,DEBUG
      INTEGER          , INTENT(IN)    :: NTRAC
      INTEGER       , INTENT(INOUT)    :: IND_T
      DOUBLE PRECISION, INTENT(IN )    :: GRAV
      LOGICAL       , INTENT(IN)       :: LISTIN
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: TN,HPROP,U,V,CF,PATMOS,ZF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,RAYEFF,T1,T2,T3,T4
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE TAUB_WAQTEL
     &(CF,DENSITY,TAUB,NPOIN,UN,VN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: DENSITY
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TAUB
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      END MODULE INTERFACE_WAQTEL
