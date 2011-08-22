!                    *****************
                     SUBROUTINE QWIND1
!                    *****************
!
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , Z0OLD , Z0NEW , TETA  , ROAIR , ROEAU , BETAM , XKAPPA,
     &  DECAL , GRAVIT, NF    , NPLAN , NPOIN2, CIMPLI, TOLD  , TNEW  ,
     &  CPHAS , USN   , USO   , OMNEW , OMOLD , BETAN , BETAO )
!
!***********************************************************************
! TOMAWAC   V6P1                                   27/06/2011
!***********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE WAVE GENERATION
!+               (BY WIND) SOURCE TERM BASED ON JANSSEN (1989,1991).
!
!reference  JANSSEN P.A.E.M (1989) :
!+                     "WIND-INDUCED STRESS AND THE DRAG OF AIR
!+                      FLOW OVER SEA WAVES". JPO, VOL 19, PP 745-754.
!reference JANSSEN P.A.E.M (1991) :
!+                     "QUASI-LINEAR THEORY OF WIND-WAVE GENERATION
!+                      APPLIED TO WAVE FORECASTING". JPO, VOL 21, PP 1631-1642.
!
!history  P. THELLIER; M. BENOIT (EDF/DER/LNH)
!+        11/04/95
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
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETAM          |-->| WIND GENERATION COEFFICIENT
!| BETAN          |<--| WORK TABLE
!| BETAO          |<--| WORK TABLE
!| CIMPLI         |-->| IMPLICITATION COEFFICIENT FOR SOURCE TERMS
!| CPHAS          |<--| WORK TABLE
!| DECAL          |-->| SHIFT GROWING CURVE DUE TO WIND
!| F              |-->| DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| OMNEW          |<--| WORK TABLE
!| OMOLD          |<--| WORK TABLE
!| ROAIR          |-->| AIR DENSITY
!| ROEAU          |-->| WATER DENSITY
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TNEW           |<--| WORK TABLE
!| TOLD           |<--| WORK TABLE
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |-->| WIND DIRECTION AT TIME N+1
!| TWOLD          |-->| WIND DIRECTION AT TIME N
!| USN            |<--| WORK TABLE
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USO            |<--| WORK TABLE
!| USOLD          |-->| FRICTION VELOCITY AT TIME N
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| XKAPPA         |-->| VON KARMAN CONSTANT
!| Z0NEW          |-->| SURFACE ROUGHNESS LENGTH AT TIME N+1
!| Z0OLD          |-->| SURFACE ROUGHNESS LENGTH AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION GRAVIT, ROAIR , ROEAU , BETAM , XKAPPA, DECAL ,
     &                 CIMPLI
      DOUBLE PRECISION  FREQ(NF)    , TETA(NPLAN)  , USOLD(NPOIN2)
      DOUBLE PRECISION TWOLD(NPOIN2), TWNEW(NPOIN2), USNEW(NPOIN2)
      DOUBLE PRECISION Z0OLD(NPOIN2), Z0NEW(NPOIN2), BETAN(NPOIN2)
      DOUBLE PRECISION CPHAS(NPOIN2),   USN(NPOIN2), OMOLD(NPOIN2)
      DOUBLE PRECISION OMNEW(NPOIN2),   USO(NPOIN2), BETAO(NPOIN2)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION     F(NPOIN2,NPLAN,NF),    XK(NPOIN2,NF)
      DOUBLE PRECISION TOLD(NPOIN2,NPLAN)    ,  TNEW(NPOIN2,NPLAN)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION DEUPI , C1 , DIREC , CONST , DIMPLI
      DOUBLE PRECISION XX    , ZLOGMU
!
!
      DEUPI = 2.D0* 3.14159265358978D0
      C1 = DEUPI * (ROAIR/ROEAU) * (BETAM/XKAPPA**2.D0)
      DIMPLI=1.0D0-CIMPLI
!
!.....COMPUTES (1ST PASS) THE DIRECTIONAL DEPENDENCES
!     """"""""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NPLAN
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          TOLD(IP,JP)=COS(DIREC-TWOLD(IP))
          TNEW(IP,JP)=COS(DIREC-TWNEW(IP))
        ENDDO
      ENDDO
!
!.....LOOP ON THE DISCRETISED FREQUENCIES
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JF=1,NF
        CONST=C1*FREQ(JF)
!
!.......COMPUTES THE PHASE VELOCITY
!       """""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          CPHAS(IP) = DEUPI * FREQ(JF) / XK(IP,JF)
        ENDDO
!
!.......COMPUTES (1ST PASS) THE FREQUENCIES (OMEGA AND UETOILE/CPHASE)
!       """""""""""""""""""""""""""""""""""""""""""""""""
        DO IP=1,NPOIN2
          OMOLD(IP) = GRAVIT * Z0OLD(IP) / CPHAS(IP)**2.D0
          OMNEW(IP) = GRAVIT * Z0NEW(IP) / CPHAS(IP)**2.D0
          USO(IP) = (USOLD(IP) / CPHAS(IP)) + DECAL
          USN(IP) = (USNEW(IP) / CPHAS(IP)) + DECAL
        ENDDO
!
!.......LOOP ON THE DISCRETISED DIRECTIONS
!       """"""""""""""""""""""""""""""""""""""""""""
        DO JP=1,NPLAN
!
          DO IP=1,NPOIN2
            BETAO(IP)=0.D0
            BETAN(IP)=0.D0
          ENDDO
!
!.........COMPUTES THE SOURCE TERM
!         """"""""""""""""""""""
          DO IP=1,NPOIN2
            IF (TOLD(IP,JP).GT.0.01D0) THEN
              XX = USO(IP) * TOLD(IP,JP)
              ZLOGMU = DLOG(OMOLD(IP)) + XKAPPA/XX
              IF (ZLOGMU.LT.0.D0) THEN
                BETAO(IP) = CONST*OMOLD(IP)*EXP(XKAPPA/XX)*
     &                          ZLOGMU**4.D0*XX**2.D0
              ENDIF
            ENDIF
          ENDDO
          DO IP=1,NPOIN2
            IF (TNEW(IP,JP).GT.0.01D0) THEN
              XX = USN(IP) * TNEW(IP,JP)
              ZLOGMU = DLOG(OMNEW(IP)) + XKAPPA/XX
              IF (ZLOGMU.LT.0.D0) THEN
                BETAN(IP) = CONST*OMNEW(IP)*EXP(XKAPPA/XX)*
     &                          ZLOGMU**4.D0*XX**2.D0
              ENDIF
            ENDIF
          ENDDO
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     &              + (DIMPLI*BETAO(IP)+CIMPLI*BETAN(IP)) * F(IP,JP,JF)
            TSDER(IP,JP,JF) = TSDER(IP,JP,JF) + BETAN(IP)
          ENDDO
!
        ENDDO
      ENDDO
!
      RETURN
      END
