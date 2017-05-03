!                    *****************
                     SUBROUTINE FREPIC
!                    *****************
!
     &( FPIC  , F     , FREQ  , NF    , NPLAN , NPOIN2 )
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE PEAK FREQUENCY FOR ALL THE NODES IN THE
!+                2D MESH. THIS PEAK FREQUENCY IS DEFINED AS THE
!+                DISCRETISED FREQUENCY FOR WHICH E(F) IS GREATEST.
!
!history  P. THELLIER; M. BENOIT
!+        09/02/95
!+        V1P0
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
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| E              |<->| WORK TABLE
!| EMAX           |<->| WORK TABLE
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FPIC           |<--| PEAK FREQUENCIES
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_FREPIC => FREPIC
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER,INTENT(IN)             :: NF    , NPLAN , NPOIN2
      DOUBLE PRECISION,INTENT(IN)    :: F(NPOIN2,NPLAN,NF), FREQ(NF)
      DOUBLE PRECISION,INTENT(INOUT) :: FPIC(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION E, EMAX
!
      DO IP = 1,NPOIN2
        FPIC(IP) = 1.D-20
        EMAX = 0.D0
!
!.....LOOP OVER DISCRETISED FREQUENCIES
!     """""""""""""""""""""""""""""""""""""""""""""
        DO JF = 1,NF
!
!.......INTEGRATES WRT DIRECTIONS TO GET E(F)
!       """""""""""""""""""""""""""""""""""""""""""""""""
           E = 0.D0
           DO JP = 1,NPLAN
              E = E + F(IP,JP,JF)
           ENDDO                ! JP
!
!.......KEEPS THE MAXIMUM VALUE FOR E(F) AND ASSOCIATED FREQUENCY
!       """""""""""""""""""""""""""""""""""""""""""""""""""""
           IF (E.GT.EMAX) THEN
              EMAX = E
              FPIC(IP) = FREQ(JF)
           ENDIF
        ENDDO                   ! JF
      ENDDO                     ! IP
!
!
      RETURN
      END
