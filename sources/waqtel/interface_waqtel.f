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
     &(ALD,MP,CMOR,TR,TRESP,GT,TOX,NPOIN)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER         , INTENT(IN):: NPOIN
      DOUBLE PRECISION, INTENT(IN):: CMOR(2),TR(NPOIN),TOX,TRESP
      DOUBLE PRECISION, INTENT(INOUT)::ALD(NPOIN),MP(NPOIN)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) ::GT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ALGAE_GROWTH
     &(ALG,CMAX,RAY,GT,NUTR,TOX,NPOIN )
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER         , INTENT(IN):: NPOIN
      DOUBLE PRECISION, INTENT(IN):: CMAX,RAY(NPOIN),NUTR(NPOIN),TOX
      DOUBLE PRECISION, INTENT(INOUT)::ALG(NPOIN)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) ::GT
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
        SUBROUTINE CALCS2D_BIOMASS
     &  (NPOIN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,HN,HPROP,T1,T2,T3,T4,T5,T6,
     &   DEBUG,MASSOU,DT,VOLU2D,YATEMP,IND_T)
        USE BIEF_DEF
        IMPLICIT NONE
      LOGICAL          , INTENT(IN   ) :: YATEMP
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC,DEBUG,IND_T
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP,DT
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,HN,VOLU2D
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF,T1,T2,T3,T4,T5,T6
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_BIOMASS
     &  (NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,ZPROP,
     &   T1,T2,T3,T4,T5,T6,DEBUG,YATEMP,IND_T)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3,NTRAC
      INTEGER          , INTENT(IN   ) :: DEBUG,IND_T,NPLAN
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
!
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_EUTRO
     &  (NPOIN,WATTEMP,TN,TEXP,TIMP,RAYEFF,NTRAC,HN,HPROP,T1,T2,T3,T4,
     &   T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,MASSOU,DT,VOLU2D,YATEMP,IND_T,
     &   UN,VN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC,DEBUG,IND_T
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP,DT
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,HN,VOLU2D,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TIMP,TEXP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T11,T12
!
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_EUTRO
     &  (NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,TIMP,RAYEFF,NTRAC,HPROP,
     &   ZPROP,T1,T21,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,YATEMP,
     &   IND_T,UN,VN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN3,NPOIN2,NPLAN
      INTEGER          , INTENT(IN   ) :: NTRAC,DEBUG,IND_T
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,ZPROP,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T21,T3,T4,T5,T6,T7,T8,T9
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T10,T11,T12
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_MICROPOL (NPOIN,NTRAC,TN,TEXP,TIMP,HN,
     &                             HPROP,CF,UN,VN,T1,T2,T3,T4,DT,VOLU2D,
     &                             MASSOU)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC
      DOUBLE PRECISION , INTENT(IN   ) :: DT
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,HN,CF,UN,VN,VOLU2D
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,T1,T2,T3,T4,TIMP
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_MICROPOL (NPOIN3,NPOIN2,NPLAN,NTRAC,TN,TEXP,
     &                           TIMP,ZPROP,CF,UN,VN,T1,T2,T3,T4,DEBUG)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN3,NPOIN2,NTRAC
      INTEGER          , INTENT(IN   ) :: NPLAN,DEBUG
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP,CF,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T1,T2,T3,T4
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_O2
     & (NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,TIMP,T1,T2,T3,T4,NTRAC,
     &  H,HPROP,UN,VN,MASSOU,DT,VOLU2D,YATEMP,IND_T,DEBUG)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: FORMK2,NPOIN,NTRAC,IND_T,DEBUG
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44,DT
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22,MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,H,HPROP,UN,VN,VOLU2D
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T1,T2,T3,T4
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_O2
     & (NPOIN3,NPOIN2,NPLAN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,TIMP,T31,T32,T21,T22,NTRAC,H,HPROP,ZPROP,
     &  UN,VN,YATEMP,IND_T)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3,NPLAN
      INTEGER          , INTENT(IN   ) :: FORMK2,NTRAC,IND_T
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,H,HPROP,UN,VN,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T31,T32,T21,T22
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS2D_THERMIC
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
        SUBROUTINE CALCS3D_THERMICS
     & (NPOIN2,NPOIN3,IND_T,IND_S,TA,ATABOS,BTABOS,PATMOS,ATMOSEXCH,
     &  WIND,LISTIN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3
      INTEGER, INTENT(IN)             :: IND_T,IND_S,ATMOSEXCH
      TYPE(BIEF_OBJ), INTENT(IN)      :: TA,WIND
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: ATABOS,BTABOS
      TYPE(BIEF_OBJ), INTENT(IN)      :: PATMOS
      LOGICAL,        INTENT(IN)      :: LISTIN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCS3D_THERMICV(NPOIN2,NPOIN3,NPLAN,Z,IND_T,IND_S,
     &                            TA,TEXP,TIMP,LONGIT,LATIT,LISTIN,AT,
     &                            MARDAT,MARTIM)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NPLAN
      INTEGER, INTENT(IN)             :: IND_T,IND_S
      INTEGER, INTENT(IN)             :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN3),LATIT,LONGIT,AT
      TYPE(BIEF_OBJ), INTENT(IN)      :: TA
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TEXP,TIMP
      LOGICAL,        INTENT(IN)      :: LISTIN
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DEPOS_FX
     &  (SEDP,TAUB,CSUS,TAUS,VITCHU,NPOIN)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: TAUS,VITCHU
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TAUB,CSUS
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
        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
        CHARACTER(LEN=24), INTENT(IN)     :: CODE
        CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
        INTEGER, INTENT(IN)               :: NCAR
        CHARACTER(LEN=250), INTENT(IN)    :: PATH
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NAMETRAC_WAQ
     &  (TEXTE,TEXTPR,NAMETRAC,NTRAC,IND_T,
     &   MAXTRA,ICONVFT,VISCT,DIMM)
        USE BIEF_DEF
        IMPLICIT NONE
      CHARACTER(LEN=32), INTENT(INOUT)::  NAMETRAC(*)
      INTEGER, INTENT(IN)             ::  NTRAC,MAXTRA,DIMM
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
     &  (WAQPROCESS,MESH,IELM1,VENT,WINDX,WINDY,ATMOS,PATMOS,
     &   MESH3D,IELM3)
        USE BIEF_DEF
        IMPLICIT NONE
!
        LOGICAL,         INTENT(IN   ) :: VENT,ATMOS
        INTEGER,         INTENT(IN   ) :: IELM1,WAQPROCESS
        TYPE(BIEF_OBJ ), INTENT(INOUT) :: WINDX,WINDY
        TYPE(BIEF_OBJ ), INTENT(INOUT) :: PATMOS
        TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
        TYPE(BIEF_MESH), INTENT(INOUT),OPTIONAL :: MESH3D
        INTEGER,         INTENT(IN   ),OPTIONAL :: IELM3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RAY_EFFECT
     &(SECCHI,TRR,NPOIN,MEXT,I0,IK,KPE,EFF,H,T1,T2)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)    :: NPOIN,MEXT
      DOUBLE PRECISION, INTENT(IN)    :: I0,IK,SECCHI,KPE
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: H,TRR
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: EFF,T1,T2
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE REAER
     &(FORMK2,K2,K22,NPOIN,NPLAN,UN,VN,H,EPS)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER         , INTENT(IN)     :: FORMK2,NPOIN,NPLAN
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
     &(NPOIN3,NPOIN2,TEXP,TIMP,TN,NTRAC,WAQPROCESS,RAYEFF,IND_T,IND_S,H,
     & HPROP,U,V,CF,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     & T21,T22,T23,PATMOS,LISTIN,GRAV,ZF,
     & DEBUG,MASSOU,DT,DIMM,
     & VOLU2D,NPLAN,LATIT,LONGIT,AT,MARDAT,MARTIM,ZPROP)
        USE BIEF_DEF
        IMPLICIT NONE
      INTEGER          , INTENT(IN)    :: NPOIN3,WAQPROCESS,DEBUG,DIMM
      INTEGER          , INTENT(IN)    :: NTRAC,NPLAN,NPOIN2
      INTEGER          , INTENT(IN)    :: MARDAT(3),MARTIM(3)
      INTEGER       , INTENT(INOUT)    :: IND_T,IND_S
      DOUBLE PRECISION, INTENT(IN )    :: GRAV,DT,LATIT,LONGIT,AT
      LOGICAL       , INTENT(IN)       :: LISTIN
      DOUBLE PRECISION,INTENT(INOUT)   :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: TN,H,HPROP,U,V,CF,PATMOS,ZF
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: VOLU2D,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,RAYEFF,T1,T2,T3,T4
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T5,T6,T7,T8,T9,T10,T11,T12
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T21,T22,T23
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
      INTERFACE
        SUBROUTINE YASMI_WAQ
     &  (NTRAC,YASMI)
        IMPLICIT NONE
      INTEGER          , INTENT(IN   )::  NTRAC
      LOGICAL          , INTENT(INOUT)::  YASMI(*)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      END MODULE INTERFACE_WAQTEL
