!                    *****************
                     SUBROUTINE QWIND2
!                    *****************
!
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , TETA  , ROAIR , ROEAU , GRAVIT, NF    , NPLAN , NPOIN2,
     &  CIMPLI, CPHAS , USN   , USO   , BETAN , BETAO )
!
!***********************************************************************
! TOMAWAC   V6P1                                   27/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE WAVE GENERATION
!+               (BY WIND) SOURCE TERM BASED ON SNYDER ET AL. (1981),
!+                MODIFIED BY KOMEN ET AL. (1984) TO MAKE USE OF THE
!+                FRICTION VELOCITY U* INSTEAD OF THE U5 VELOCITY
!+               (MEASURED 5 METERS ABOVE) FOR THE WIND.
!+
!+            THIS GENERATION THEORY IS IDENTICAL TO THAT IN WAM-CYCLE 3.
!
!reference  SNYDER ET AL. (1981) :
!+                     "ARRAY MEASUREMENTS OF ATMOSPHERIC PRESSURE
!+                      FLUCTUATIONS ABOVE SURFACE GRAVITY WAVES".
!+                      JOURNAL OF FLUID MECH., VOL 102., PP 1-59.
!reference KOMEN ET AL.  (1984) :
!+                     "ON THE EXISTENCE OF A FULLY DEVELOPED
!+                      WINDSEA SPECTRUM". JPO, VOL 14, PP 1271-1285.
!
!history  M. BENOIT (EDF/DER/LNH)
!+        26/03/96
!+        V1P1
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
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETAN          |<--| WORK TABLE
!| BETAO          |<--| WORK TABLE
!| CIMPLI         |-->| IMPLICITATION COEFFICIENT FOR SOURCE TERMS
!| CPHAS          |<--| WORK TABLE
!| F              |-->| DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| ROAIR          |-->| AIR DENSITY
!| ROEAU          |-->| WATER DENSITY
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |-->| WIND DIRECTION AT TIME N+1
!| TWOLD          |-->| WIND DIRECTION AT TIME N
!| USN            |<--| WORK TABLE
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USO            |<--| WORK TABLE
!| USOLD          |-->| FRICTION VELOCITY AT TIME N
!| XK             |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION GRAVIT, ROAIR , ROEAU , CIMPLI
      DOUBLE PRECISION  FREQ(NF)    , TETA(NPLAN)
      DOUBLE PRECISION TWOLD(NPOIN2), TWNEW(NPOIN2), USNEW(NPOIN2)
      DOUBLE PRECISION BETAO(NPOIN2), BETAN(NPOIN2), USOLD(NPOIN2)
      DOUBLE PRECISION CPHAS(NPOIN2),   USO(NPOIN2),   USN(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF),    XK(NPOIN2,NF)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION DEUPI , C1    , DIREC , CONST , DIMPLI
!
!
      DEUPI = 2.D0* 3.14159265358978D0
      C1 = 0.25D0 * (ROAIR/ROEAU) * DEUPI
      DIMPLI=1.0D0-CIMPLI
!
!.....LOOP ON THE DISCRETISED DIRECTIONS
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NPLAN
!
!.......COMPUTES (1ST PASS) THE DIRECTIONAL DEPENDENCES
!       """"""""""""""""""""""""""""""""""""""""""""""
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          USO(IP)=USOLD(IP)*COS(DIREC-TWOLD(IP))
          USN(IP)=USNEW(IP)*COS(DIREC-TWNEW(IP))
        ENDDO
!
!.......LOOP ON THE DISCRETISED FREQUENCIES
!       """"""""""""""""""""""""""""""""""""""""""""
        DO JF=1,NF
          CONST=C1*FREQ(JF)
!
!.........COMPUTES THE PROPORTIONALITY FACTORS BETA
!         """"""""""""""""""""""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            CPHAS(IP) = DEUPI * FREQ(JF) / XK(IP,JF)
            BETAO(IP)=MAX(28.D0*USO(IP)/CPHAS(IP)-1.D0,0.D0)*CONST
            BETAN(IP)=MAX(28.D0*USN(IP)/CPHAS(IP)-1.D0,0.D0)*CONST
          ENDDO
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     &             + (DIMPLI*BETAO(IP)+CIMPLI*BETAN(IP)) * F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF) + BETAN(IP)
          ENDDO
!
        ENDDO
!
      ENDDO
!
      RETURN
      END
