!                    *****************
                     SUBROUTINE STRESS
!                    *****************
!
     &( TAUWAV, TSTOT , F     , USNEW , TWNEW , Z0NEW , FREQ  , DFREQ ,
     &  TETA  , SINTET, COSTET, ROAIR , ROEAU , XKAPPA, BETAM , DECAL ,
     &  GRAVIT, NPOIN2, NPLAN , NF    , XTAUW , YTAUW , TAUHF )
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    COMPUTES THE WAVE STRESSES FOR ALL THE NODES
!+                IN THE MESH.
!
!note     THIS SUBROUTINE COMPUTES TAUHF DIRECTLY FROM USTAR AND
!+          ALFA. (THIS WAS DONE VIA A CALL TO 'TAUWHF' PREVIOUSLY).
!
!history  M. BENOIT (EDF/DER/LNH)
!+        03/05/95
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
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETAM          |-->| WIND GENERATION COEFFICIENT
!| COSTET         |-->| COSINE OF TETA ANGLE
!| DECAL          |-->| SHIFT GROWING CURVE DUE TO WIND
!| DFREQ          |-->| FREQUENCY STEPS BETWEEN DISCRETIZED FREQUENCIES
!| F              |-->| DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GRAVIT         |-->| GRAVITY ACCELERATION
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| ROAIR          |-->| AIR DENSITY
!| ROEAU          |-->| WATER DENSITY
!| SINTET         |-->| SINE OF TETA ANGLE
!| TAUHF          |<->| WORK TABLE
!| TAUWAV         |<--| STRESS DUE TO THE WAVES
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TSTOT          |-->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |-->| WIND DIRECTION
!| USNEW          |-->| FRICTION VELOCITY
!| XKAPPA         |-->| VON KARMAN CONSTANT
!| XTAUW          |<->| WORK TABLE
!| YTAUW          |<->| WORK TABLE
!| Z0NEW          |-->| SURFACE ROUGHNESS LENGTH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NPOIN2, NPLAN , NF
      DOUBLE PRECISION ROAIR , ROEAU , XKAPPA , BETAM , DECAL , GRAVIT
      DOUBLE PRECISION TAUWAV(NPOIN2), USNEW(NPOIN2), TWNEW(NPOIN2)
      DOUBLE PRECISION  Z0NEW(NPOIN2), FREQ(NF) , DFREQ(NF)
      DOUBLE PRECISION TSTOT(NPOIN2,NPLAN,NF), F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION TETA(NPLAN), SINTET(NPLAN), COSTET(NPLAN)
      DOUBLE PRECISION XTAUW(NPOIN2) , YTAUW(NPOIN2), TAUHF(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP    , JP    , JF    , JTOT  , J
      DOUBLE PRECISION DEUPI , DTETAR, FRMAX , COEF1 , COEF2 , TTAUHF
      DOUBLE PRECISION USTAR , ALFA  , COSTMP, C1    , C2    , DIREC
      DOUBLE PRECISION Y     , OMEGA , CM    , ZX    , ZARG  , ZMU
      DOUBLE PRECISION ZLOG  , ZBETA , UST   , Z0    , UST2  , ALF2
      DOUBLE PRECISION CONST1, OMEGAM, X0    , YC    , DELY  , AUX
!
!
      DEUPI = 6.283185307D0
      DTETAR= DEUPI/DBLE(NPLAN)
      FRMAX = FREQ(NF)
      COEF1 = DTETAR*DEUPI**4*FRMAX**5/GRAVIT**2
      COEF2 = DEUPI*ROEAU/ROAIR*DTETAR
!
      DO 12 IP=1,NPOIN2
        XTAUW(IP)=0.D0
        YTAUW(IP)=0.D0
   12 CONTINUE
!
!.....INTEGRATES THE SOURCE TERM OVER FREQUENCIES AND DIRECTIONS
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      DO 110 JF=1,NF
        AUX=COEF2*FREQ(JF)*DFREQ(JF)
        DO 120 JP=1,NPLAN
          C1=AUX*SINTET(JP)
          C2=AUX*COSTET(JP)
!
          DO 100 IP=1,NPOIN2
            XTAUW(IP)=XTAUW(IP)+TSTOT(IP,JP,JF)*C1
            YTAUW(IP)=YTAUW(IP)+TSTOT(IP,JP,JF)*C2
  100     CONTINUE
  120   CONTINUE
  110 CONTINUE
!
!.....COMPUTES THE PARAMETERISED HIGH FREQUENCY PART
!     """""""""""""""""""""""""""""""""""""""""""""""""""
      DO 170 IP=1,NPOIN2
        TAUHF(IP)=0.D0
  170 CONTINUE
!
      DO 200 JP=1,NPLAN
        DIREC=TETA(JP)
        DO 171 IP=1,NPOIN2
          COSTMP=MAX(COS(DIREC-TWNEW(IP)),0.D0)
          TAUHF(IP)=TAUHF(IP)+F(IP,JP,NF)*COSTMP**3
  171   CONTINUE
  200 CONTINUE
!
      JTOT  = 50
      CONST1= BETAM/XKAPPA**2
      OMEGAM= 6.283185307D0*FRMAX
      X0    = 0.05D0
!
      DO 173 IP=1,NPOIN2
        USTAR=USNEW(IP)
        ALFA =Z0NEW(IP)*GRAVIT/USTAR**2
!
!----------------------------------------------OLD SUBROUTINE TAUWHF
!C      CALL TAUWHF
!C   *( TAUHF , USTAR , ALFA  , BETAM , XKAPPA , DECAL , FRMAX , GRAVIT)
!----------------------------------------------
!
!MB.....LIMITING FACTORS TO REPRODUCE WAM4 (SUBROUTINE TAUHF OF PREPROC)
!.......(THIS LIMITATION IS NOT JUSTIFIED A PRIORI. IT IS AN ARTEFACT
!.......OF TAUHF BEING DISCRETISED ON A GRID)
        UST2  = MIN(USTAR,5.D0)
        ALF2  = MIN(ALFA,0.11D0)
!MB.....END OF LIMITATION
!
        UST   = MAX(UST2,0.000001D0)
        Z0    = ALF2*UST**2/GRAVIT
!
        YC    = MAX(OMEGAM,X0*GRAVIT/UST)*SQRT(Z0/GRAVIT)
        DELY  = MAX((1.D0-YC)/FLOAT(JTOT),0.D0)
        TTAUHF = 0.D0
        DO 102 J=1,JTOT
          Y     = YC+DBLE(J-1)*DELY
          OMEGA = Y*SQRT(GRAVIT/Z0)
          CM    = GRAVIT/OMEGA
          ZX    = UST/CM +DECAL
          ZARG  = MIN(XKAPPA/ZX,20.D0)
          ZMU   = MIN(GRAVIT*Z0/CM**2*DEXP(ZARG),1.D0)
          ZLOG  = MIN(DLOG(ZMU),0.D0)
          ZBETA = CONST1*ZMU*ZLOG**4
          TTAUHF= TTAUHF+ZBETA/Y*DELY
  102   CONTINUE
!----------------------------------------------ANCIENNE SUB TAUWHF
!
        TAUHF(IP) = TTAUHF*COEF1*USTAR**2*TAUHF(IP)
  173 CONTINUE
!
!.....TAKES THE PARAMETERISED HIGH FREQUENCY PART INTO ACCOUNT
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
!
      DO 190 IP=1,NPOIN2
        XTAUW(IP) = XTAUW(IP) + TAUHF(IP)*SIN(TWNEW(IP))
        YTAUW(IP) = YTAUW(IP) + TAUHF(IP)*COS(TWNEW(IP))
        TAUWAV(IP)= SQRT(XTAUW(IP)**2+YTAUW(IP)**2)
  190 CONTINUE
!
      RETURN
      END
