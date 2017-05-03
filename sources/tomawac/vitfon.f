!                    *****************
                     SUBROUTINE VITFON
!                    *****************
!
     &(UWBM,F, XK , DEPTH , DFREQ , NF    , NPOIN2, NPLAN )
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
!***********************************************************************
!
!brief    COMPUTES THE MAXIMUM ORBITAL VELOCITY NEAR THE BOTTOM
!+               (AVERAGE VELOCITY ON THE SPECTRUM).
!
!history  M. BENOIT
!+        05/07/96
!+        V1P2
!+
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
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETA           |<->| WORK TABLE
!| DEPTH          |-->| WATER DEPTH
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| UWBM           |<--| MAXIMUM ORBITAL VELOCITY NEAR THE BOTTOM
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT
!
      USE INTERFACE_TOMAWAC, EX_VITFON => VITFON
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NPLAN,NPOIN2
      DOUBLE PRECISION, INTENT(INOUT) :: UWBM(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2),DFREQ(NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION DTETAR, DEUKD , COEF , BETAA
!
      INTRINSIC SQRT,SINH,MIN
!
!-----------------------------------------------------------------------
!
      DTETAR=DEUPI/FLOAT(NPLAN)
!
      DO IP = 1,NPOIN2
        UWBM(IP) = 0.D0
!
!     SUMS UP THE DISCRETISED PART OF THE SPECTRUM
!
        DO JF = 1,NF
           COEF=2.D0*GRAVIT*DFREQ(JF)*DTETAR
           DEUKD = MIN(2.D0*DEPTH(IP)*XK(IP,JF),7.D2)
           BETAA = COEF*XK(IP,JF)/SINH(DEUKD)
           DO JP = 1,NPLAN
              UWBM(IP) = UWBM(IP) + F(IP,JP,JF)*BETAA
           ENDDO
        ENDDO
!
        UWBM(IP) = SQRT(UWBM(IP))
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
