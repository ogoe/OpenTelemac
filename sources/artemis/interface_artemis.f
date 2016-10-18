!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                  INTERFACES FOR ARTEMIS SUBROUTINES
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
!#######################################################################
!
      MODULE INTERFACE_ARTEMIS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
!-----------------------------------------------------------------------
!
!     DEFINES INTERFACES
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE BERKHO(LT)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: LT
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCFW
     &(I,H,C,CG,K,HMU,NPOIN,OMEGA,GRAV,VISCO,
     & DIAM90,DIAM50,MVSED,MVEAU,FORMFR,
     & REGIDO,RICOEF,ENTREG,ENTRUG,FFW)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER I, NPOIN, FORMFR, REGIDO
      DOUBLE PRECISION C(NPOIN),CG(NPOIN),K(NPOIN)
      DOUBLE PRECISION HMU(NPOIN),H(NPOIN),GRAV,OMEGA
      DOUBLE PRECISION VISCO, DIAM90, DIAM50,MVSED
      DOUBLE PRECISION MVEAU, RICOEF,FFW
      LOGICAL ENTREG,ENTRUG
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALCQB
     &(Q1,Q2,Q3)
      IMPLICIT NONE
      DOUBLE PRECISION Q1,Q2,Q3
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
!      INTERFACE
!        SUBROUTINE CALDIR()
!       USE BIEF_DEF
!       IMPLICIT NONE
!        END SUBROUTINE
!      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CALTETAP(TETA,XNEBOR,YNEBOR,XSGBOR,YSGBOR,ADIR,NPTFR)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER                      :: NPTFR
      DOUBLE PRECISION, INTENT(IN) :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR,2),YNEBOR(NPTFR,2)
      DOUBLE PRECISION TETA(NPTFR),ADIR(NPTFR)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE CNTPRE
     &(DAM,NPOIN,IPRECO,IPREC2)
      IMPLICIT NONE
      INTEGER NPOIN,IPRECO,IPREC2
      DOUBLE PRECISION DAM(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DIRALE
     &(DALE,EXPOS,TETAH,TETMIN,TETMAX,
     & NDALE,TRA01,NPOIN,PRIVE,NPRIV)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER NDALE,NPOIN,NPRIV
      DOUBLE PRECISION DALE(NDALE),TRA01(NPOIN)
      DOUBLE PRECISION EXPOS,TETAH,TETMIN,TETMAX
      TYPE(BIEF_OBJ) :: PRIVE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE DISMOY
     &(NPOIN,NELEM,X,Y,IKLE,K,LISHHO)
      IMPLICIT NONE
      INTEGER NPOIN,NELEM,LISHHO
      INTEGER IKLE(NELEM,*)
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),K(NPOIN)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE ENTART
     &(ITITRE,X,LT,NBR,NBRTOT,ALEMON,ALEMUL,BALAYE)
      IMPLICIT NONE
      INTEGER ITITRE,LT,NBR,NBRTOT
      DOUBLE PRECISION X
      LOGICAL ALEMON,ALEMUL,BALAYE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION FCTE1(KH)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: KH
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION FCTE2(KH)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: KH
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE FWSPEC
     &(FW,FWCOEF,X,Y,NPOIN,PRIVE,ZF)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER NPOIN
      DOUBLE PRECISION FW(NPOIN),X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION ZF(NPOIN),FWCOEF
      TYPE(BIEF_OBJ) :: PRIVE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE LECDON_ARTEMIS
     &(FILE_DESC,PATH,NCAR,CODE)
        USE DECLARATIONS_SPECIAL
        IMPLICIT NONE
!
!
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
!
        END SUBROUTINE
      END INTERFACE

!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE NOMVAR_ARTEMIS
     &(TEXTE,TEXTPR,MNEMO)
      IMPLICIT NONE
      CHARACTER(LEN=32) TEXTE(26),TEXTPR(26)
      CHARACTER(LEN=8)  MNEMO(26)
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PENTCO(II)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: II
        END SUBROUTINE
      END INTERFACE
!
!
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE PERALE
     &(PALE,GAMMA,PERPIC,NPALE,TRA01,
     & NPOIN,PRIVE,NPRIV,PMIN,PMAX)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER NPALE,NPOIN,NPRIV
      DOUBLE PRECISION PALE(NPALE),TRA01(NPOIN)
      DOUBLE PRECISION PERPIC,GAMMA,PMIN,PMAX
      TYPE(BIEF_OBJ) :: PRIVE
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RADIA1(LISHHO)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER LISHHO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE RADIA2(LISHHO)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER LISHHO
        END SUBROUTINE
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION SPD(TETA)
      IMPLICIT NONE
      DOUBLE PRECISION TETA
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION SPE(F)
      IMPLICIT NONE
      DOUBLE PRECISION F
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        DOUBLE PRECISION FUNCTION STWC(F,DIR)
        IMPLICIT NONE
        DOUBLE PRECISION F,DIR
        END FUNCTION
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      INTERFACE
        SUBROUTINE UTIMP
     &(PHIR,PHII,C,CG,K,X,Y,ZF,H,
     & HHO,U0,V0,PHAS,S,TRA01,TRA02,TRA03,TRA04,INCI,
     & GRAV,PER,OMEGA,IKLE,NBOR,KP1BOR,
     & NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN,PRIVE)
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN
      INTEGER IKLE(NELMAX,3),NBOR(NPTFR),KP1BOR(NPTFR)
      DOUBLE PRECISION PHIR(NPOIN),PHII(NPOIN)
      DOUBLE PRECISION C(NPOIN),CG(NPOIN),K(NPOIN)
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION H(NPOIN),HHO(NPOIN),U0(NPOIN),V0(NPOIN)
      DOUBLE PRECISION INCI(NPOIN)
      DOUBLE PRECISION PHAS(NPOIN),S(NPOIN)
      DOUBLE PRECISION TRA01(NPOIN),TRA02(NPOIN)
      DOUBLE PRECISION TRA03(NPOIN),TRA04(NPOIN)
      DOUBLE PRECISION GRAV,PER,OMEGA
      TYPE(BIEF_OBJ) :: PRIVE
        END SUBROUTINE
      END INTERFACE
!
!=======================================================================
!
      END MODULE INTERFACE_ARTEMIS
!
!#######################################################################
!
