!                    *****************
                     SUBROUTINE QMOUT1
!                    *****************
!
     &( TSTOT , TSDER , F     , XK    , ENRJ  , FREQ  , FMOY  , XKMOY ,
     &  PROINF, CMOUT1, CMOUT2, GRAVIT, NF    , NPLAN , NPOIN2, TAUX1 ,
     &  BETA  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE WHITECAPPING
!+                SOURCE TERM BASED ON KOMEN ET AL. (1984).
!
!note     CMOUT1 (USED IN WAM-CYCLE 4) EQUALS 4.5.
!note   CMOUT2 (USED IN WAM-CYCLE 4) EQUALS 0.5.
!
!reference  KOMEN G.J., HASSELMANN S., HASSELMANN K. (1984) :
!+                     "ON THE EXISTENCE OF A FULLY DEVELOPED WINDSEA
!+                      SPECTRUM". JPO, VOL 14, PP 1271-1285.
!
!history  P. THELLIER; M. BENOIT (EDF/DER/LNH)
!+        06/04/95
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
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETA           |<--| WORK TABLE
!| CMOUT1         |-->| WHITE CAPPING DISSIPATION COEFFICIENT
!| CMOUT2         |-->| WHITE CAPPING WEIGHTING COEFFICIENT
!| ENRJ           |-->| SPECTRUM VARIANCE
!| F              |-->| DIRECTIONAL SPECTRUM
!| FMOY           |-->| MEAN SPECTRAL FRQUENCY FMOY
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| TAUX1          |<--| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKMOY          |-->| AVERAGE WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION CMOUT1, CMOUT2, GRAVIT
      DOUBLE PRECISION XKMOY(NPOIN2), ENRJ(NPOIN2) ,  BETA(NPOIN2)
      DOUBLE PRECISION      FREQ(NF), FMOY(NPOIN2) , TAUX1(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF),    XK(NPOIN2,NF)
      LOGICAL PROINF
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION AUX   , DEUPI , C1    , C2
!
!
      DEUPI = 6.283185307D0
      C1 = - CMOUT1*DEUPI**9.D0/GRAVIT**4.D0
      C2 = - CMOUT1*DEUPI
!
      IF (PROINF) THEN
!     ---------------- INFINITE WATER DEPTH (USES F).
!
!.......WORKING ARRAY (THIS TERM ONLY DEPENDS ON THE POINT IN SPACE)
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          TAUX1(IP) = C1 * ENRJ(IP)**2.D0 * FMOY(IP)**9.D0
        ENDDO
!
!.......LOOP OVER DISCRETISED FREQUENCIES
!       """""""""""""""""""""""""""""""""""""""""""
        DO JF=1,NF
!
!.........COMPUTES THE BETA COEFFICIENT : QMOUT1 = BETA * F
!         """"""""""""""""""""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            AUX = (FREQ(JF)/FMOY(IP))**2.D0
            BETA(IP)=TAUX1(IP)*((1.D0-CMOUT2)*AUX+CMOUT2*AUX**2.D0)
          ENDDO
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETA(IP)*F(IP,JP,JF)
              TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETA(IP)
            ENDDO
          ENDDO
        ENDDO
!
      ELSE
!     ---------------- FINITE WATER DEPTH (USES K).
!
!.......WORKING ARRAY (THIS TERM ONLY DEPENDS ON THE POINT IN SPACE)
!       """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          TAUX1(IP) = C2 * ENRJ(IP)**2.D0 * FMOY(IP) * XKMOY(IP)**4.D0
        ENDDO
!
!.......LOOP OVER THE DISCRETISED FREQUENCIES
!       """""""""""""""""""""""""""""""""""""""""""
        DO JF=1,NF
!
!.........COMPUTES THE BETA COEFFICIENT : QMOUT1 = BETA * F
!         """"""""""""""""""""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            AUX = XK(IP,JF) / XKMOY(IP)
            BETA(IP)=TAUX1(IP)*((1.D0-CMOUT2)*AUX+CMOUT2*AUX**2.D0)
          ENDDO
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)+BETA(IP)*F(IP,JP,JF)
              TSDER(IP,JP,JF) = TSDER(IP,JP,JF)+BETA(IP)
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
      RETURN
      END
