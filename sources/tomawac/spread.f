!                    *****************
                     SUBROUTINE SPREAD
!                    *****************
!
     &( DIRSPR, F     , COSTET, SINTET, NPLAN , FREQ  , DFREQ , NF    ,
     &  NPOIN2, TAILF , COSMOY, SINMOY, VARIAN, TAUXC , TAUXS , TAUXE )
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    COMPUTES THE MEAN DIRECTIONAL SPREAD (=DIRECTIONAL
!+                WIDTH) S IN DEGREES.
!
!history  M. BENOIT
!+        28/12/95
!+        V1P1
!+   CREATED
!
!history  M. BENOIT
!+        05/07/96
!+        V1P2
!+   MODIFIED
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  G.MATTAROLO (EDF - LNHE)
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COSMOY         |<--| WORK TABLE
!| COSTET         |<--| WORK TABLE
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| DIRSPR         |<--| MEAN DIRECTIONAL SPREAD
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SINMOY         |<--| WORK TABLE
!| SINTET         |-->| SINE OF TETA ANGLE
!| TAILF          |-->| SPECTRUM QUEUE FACTOR
!| TAUXC          |<--| WORK TABLE
!| TAUXE          |<--| WORK TABLE
!| TAUXS          |<--| WORK TABLE
!| VARIAN         |<--| WORK TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRADEG
!
      USE INTERFACE_TOMAWAC, EX_SPREAD => SPREAD
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)    :: NF    , NPLAN , NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: TAILF
      DOUBLE PRECISION, INTENT(IN)    :: COSTET(NPLAN) , SINTET(NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF), DFREQ(NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: SINMOY(NPOIN2), COSMOY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: VARIAN(NPOIN2), TAUXE (NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TAUXS (NPOIN2), TAUXC (NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DIRSPR(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION AUXC  , AUXS  , DFDTET, DTETAR, AUXI
      DOUBLE PRECISION SEUIL , COEFT
!
!
      SEUIL=1.D-20
      DTETAR=DEUPI/DBLE(NPLAN)
!
      DO IP=1,NPOIN2
        COSMOY(IP)=0.D0
        SINMOY(IP)=0.D0
        VARIAN(IP)=0.D0
      ENDDO ! IP
!
!-----C-------------------------------------------------------C
!-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
!-----C-------------------------------------------------------C
      DO JF=1,NF
!
        DFDTET=DFREQ(JF)*DTETAR
!
        DO IP=1,NPOIN2
          TAUXC(IP)=0.D0
          TAUXS(IP)=0.D0
          TAUXE(IP)=0.D0
        ENDDO ! IP
!
        DO JP=1,NPLAN
          AUXC=COSTET(JP)*DFDTET
          AUXS=SINTET(JP)*DFDTET
          DO IP=1,NPOIN2
            TAUXC(IP)=TAUXC(IP)+F(IP,JP,JF)*AUXC
            TAUXS(IP)=TAUXS(IP)+F(IP,JP,JF)*AUXS
            TAUXE(IP)=TAUXE(IP)+F(IP,JP,JF)*DFDTET
          ENDDO ! IP
        ENDDO ! JP
!
        DO IP=1,NPOIN2
          COSMOY(IP)=COSMOY(IP)+TAUXC(IP)
          SINMOY(IP)=SINMOY(IP)+TAUXS(IP)
          VARIAN(IP)=VARIAN(IP)+TAUXE(IP)
        ENDDO ! IP
!
      ENDDO ! JF
!
!-----C-------------------------------------------------------------C
!-----C  TAKES INTO ACCOUNT THE HIGH FREQUENCY PART (OPTIONAL)      C
!-----C-------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        COEFT=FREQ(NF)/((TAILF-1.D0)*DFREQ(NF))
        DO IP=1,NPOIN2
          COSMOY(IP)=COSMOY(IP)+TAUXC(IP)*COEFT
          SINMOY(IP)=SINMOY(IP)+TAUXS(IP)*COEFT
          VARIAN(IP)=VARIAN(IP)+TAUXE(IP)*COEFT
        ENDDO ! IP
      ENDIF
!
!-----C-------------------------------------------------------------C
!-----C  COMPUTES THE DIRECTIONAL WIDTH                             C
!-----C-------------------------------------------------------------C
      DO IP=1,NPOIN2
        IF (VARIAN(IP).GT.SEUIL) THEN
          AUXS=SINMOY(IP)/VARIAN(IP)
          AUXC=COSMOY(IP)/VARIAN(IP)
          AUXI=MIN(SQRT(AUXS*AUXS+AUXC*AUXC),1.D0)
          DIRSPR(IP)=SQRT(2.D0*(1.D0-AUXI))*GRADEG
        ELSE
          DIRSPR(IP)=SEUIL
        ENDIF
      ENDDO ! IP
!
      RETURN
      END
