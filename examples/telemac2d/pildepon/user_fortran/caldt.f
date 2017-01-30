!                      ****************
                       SUBROUTINE CALDT
!                      ****************
!
     &(NS,G,H,U,V,DTHAUT,DT,AT,TMAX,CFL,ICIN,DTVARI,LISTIN)
!
!***********************************************************************
!TELEMAC-2D VERSION 7.0                                 30/06/13
!***********************************************************************
!
!brief  COMPUTES THE TIME STEP UNDER CFL CONDITION
!
!history  INRIA FOR KINETIC SCHEMES
!+
!+        V5P8
!+
!
!history  R. ATA (EDF-LNHE) DT FOR REMAINING SCHEMES
!+        15/03/2010
!+        V6P1
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!+
!
!history  R. ATA (EDF-LNHE)
!+        15/01/2013
!+        V6P3
!+   introduce fixed time step
!+   handle very specific cases
!+   parallelization
!
!history  R. ATA (EDF-LNHE) INTRODUCE FIXED TIME STEP
!+        30/06/2013
!+        V6P3
!+  clean and remove unused variables
!
!history  R. ATA (EDF-LNHE) INTRODUCE FIXED TIME STEP
!+        11/01/2016
!+        V7P2
!+  adjust time step to graphical outputs
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NS             |-->| TOTAL NUMBER OF NODES
!| G              |-->| GRAVITY
!| H              |-->| WATER DEPTHS
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| DTHAUT         |-->| CHARACTERISTIC LENTH FOR CFL (DX)
!| DT             |<--| TIME STEP
!| AT             |---| CURRENT TIME
!| TMAX           |---| MAX SIMULATION TIME
!| CFL            |-->| CFL
!| ICIN           |-->| WHICH SCHEME (SEE LIST BELOW)
!| DTVARI         |-->| LOGICAL: VARIABLE TIME STEP
!| LISTIN         |-->| LOGICAL: OUTPUT LISTING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY:NCSIZE
      USE DECLARATIONS_TELEMAC2D,ONLY:DTINI,LEOPRD
      USE INTERFACE_TELEMAC2D, EX_CALDT => CALDT
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,ICIN
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(IN) :: H(NS),U(NS),V(NS),DTHAUT(NS)
      DOUBLE PRECISION, INTENT(IN) :: G,CFL,AT,TMAX
      LOGICAL, INTENT(IN) :: DTVARI,LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS
      LOGICAL DEJA
      DOUBLE PRECISION RA3,EPSL,SIGMAX,UA2,UA3,UNORM,DTT
      DOUBLE PRECISION RESTE, GPRDTIME
      DOUBLE PRECISION P_DMIN
      EXTERNAL P_DMIN
      INTRINSIC MIN,CEILING
!
!-----------------------------------------------------------------------
!
!   +++++++++++++++++++++++++
!     VARIABLE TIME STEP
      IF(DTVARI)THEN
!   +++++++++++++++++++++++++
      DEJA=.FALSE.
      DT = 1.E+12
      EPSL = 0.01D0
!
      IF(ICIN.EQ.1) THEN
!       KINETIC SCHEME
!
        RA3 = SQRT(1.5D0*G)
        DO IS=1,NS
          IF(H(IS).LT.0.D0.AND.LISTIN.AND..NOT.DEJA)THEN
            WRITE(LU,*) 'CALDT WARNING : NEGATIVE WATER DEPTH'
            WRITE(LU,*) ' SEE NODE:',IS,' FOR EXAMPLE'
            DEJA = .TRUE.
          ELSE
            SIGMAX = ABS(H(IS))
            UA2    = U(IS)
            UA3    = V(IS)
            UNORM=SQRT(UA2*UA2 + UA3*UA3)
            SIGMAX= MAX(EPSL, RA3*SQRT(SIGMAX) +UNORM )
            DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
          ENDIF
        ENDDO
!
      ELSEIF(ICIN.EQ.0.OR.ICIN.EQ.2.OR.ICIN.EQ.3.OR.
     &       ICIN.EQ.4.OR.ICIN.EQ.5) THEN
!     SCHEMES OF ROE, ZOKAGOA, TCHAMEN, HLLC AND WAF
!
        DO IS=1,NS
          IF(H(IS).LT.0.D0.AND.LISTIN.AND..NOT.DEJA)THEN
            WRITE(LU,*) 'CALDT WARNING : NEGATIVE WATER DEPTH'
            WRITE(LU,*) ' SEE NODE:',IS,' FOR EXAMPLE'
            DEJA = .TRUE.
          ELSE
            UA2    = U(IS)
            UA3    = V(IS)
            UNORM=SQRT(UA2*UA2 + UA3*UA3)
            SIGMAX= MAX(EPSL,SQRT(G*ABS(H(IS)))+UNORM)
!           DTHAUT=|Ci|/Sum(Lij)
            DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
          ENDIF
        ENDDO
!
      ELSE
        IF(LNG.EQ.1) WRITE(LU,4010) ICIN
        IF(LNG.EQ.2) WRITE(LU,4020) ICIN
4010    FORMAT(1X,'CALDT: ERREUR DANS LE CHOIX DE ICIN : ',1I6)
4020    FORMAT(1X,'CALDT: ERROR IN THE CHOICE OF ICIN: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      ENDIF
!
      IF(DTVARI) THEN
!
        IF(TMAX.LT.DT)DT=TMAX !REALLY CRAZY CASES
        DTT=TMAX-AT
        IF(CFL.GE.1.D0) DT=0.9D0*DT/CFL
        IF(DTT.LE.DT.AND.DTT.GT.0.D0)  DT=DTT !LAST TIME STEP
        IF(AT.GT.TMAX) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'CALDT: MAUVAIS CHOIX DE PARAMETRES DE TEMPS '
            WRITE(LU,*)'TEMPS ET TEMPS MAX',AT,TMAX
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)'CALDT: BAD TIME PARAMETERS'
            WRITE(LU,*)'TIME AND TMAX',AT,TMAX
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
!
!       DT NOT VARIABLE
!
        IF(LISTIN.AND.LNG.EQ.1) THEN
          WRITE(LU,*) 'ATTENTION: PAS DE TEMPS FIXE ET CFL NON FOURNI.!'
          WRITE(LU,*) 'PAS DE TEMPS PEUT NE PAS VERIFIER LE CFL : ',DT
        ELSEIF(LISTIN.AND.LNG.EQ.2) THEN
          WRITE(LU,*) 'WARNING: FIXED TIME-STEP AND CFL NOT GIVEN!...! '
          WRITE(LU,*) 'TIME-STEP MAY NOT SATISFY CFL CONDITION: ',DT
        ENDIF
!
      ENDIF
!
!*************************************************************************
!      GRAPHIC OUTPUTS LIKE ASKED BY USER: DT ADATPATION
!*************************************************************************
        GPRDTIME=0.1D0
        IF(GPRDTIME.LT.1.E-12)THEN
          WRITE(LU,*) 'CALDT: PROBLEM WITH PARAMETERS: DTINI,LEOPRD',
     &                 DTINI,LEOPRD
          CALL PLANTE(1)
          STOP
!       CASE WHERE TIME STEP IS BIGGER THEN GRAPHIC OUTPUT (GPRDTIME)
        ELSEIF(GPRDTIME.LT.DT)THEN
          DT=DTINI
          IF(LISTIN)THEN
            WRITE(LU,*) 'WARNING: GRAPHICAL OUTPUT NOT OPTIMIZED: '
            WRITE(LU,*) '   - INITIAL TIME STEP TOO SMALL '
            WRITE(LU,*) '   - AND/OR PERIOD OF GRAPHIC OUTPUT TOO SMALL'
            WRITE(LU,*) 'THIS COULD REDUCE COMPUTATION TIME-STEP'
            WRITE(LU,*) 'AND INCREASE CPU TIME'
          ENDIF
        ENDIF
!       GRAPHIC OUTPUT
        IS=CEILING(AT/GPRDTIME)
        RESTE=IS*GPRDTIME-AT
        IF(RESTE.GT.1.E-13)DT=MIN(RESTE,DT)
!
!       FOR PARALLELISM
!
        IF(NCSIZE.GT.1) DT=P_DMIN(DT)
!
!
!*************************************************************************
!     ERROR TREATMENT AND LISTING OUTPUTS
!*************************************************************************
!
!     CASE DT <=0
!
      IF(DT.LE.1.E-12) THEN
        IF(LISTIN.AND.LNG.EQ.1) THEN
          WRITE(LU,*) 'PAS DE TEMPS NEGATIF OU NUL: ',DT
          WRITE(LU,*) 'PROBABLEMENT A CAUSE D UNE HAUTEUR'
          WRITE(LU,*) 'D EAU NULLE PARTOUT ...'
        ENDIF
        IF(LISTIN.AND.LNG.EQ.2) THEN
          WRITE(LU,*) 'NEGATIVE (OR NIL) TIME-STEP: ',DT
          WRITE(LU,*) 'PROBABLY DUE TO NIL WATER'
          WRITE(LU,*) 'DEPTH EVERYWHERE IN THE DOMAIN ...'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,*) 'PAS DE TEMPS : ',DT
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,*) 'TIME-STEP: ',DT
      IF(CFL.GE.1.D0) THEN
        IF(LISTIN.AND.LNG.EQ.1) THEN
          WRITE(LU,*) 'ATTENTION CFL NON FOURNI OU > 1 !...!'
          WRITE(LU,*) 'PAS DE TEMPS (AVEC CFL = 0.9) : ',DT
        ELSEIF(LISTIN.AND.LNG.EQ.2) THEN
          WRITE(LU,*) 'WARNING: CFL NOT GIVEN OR >1 !...! '
          WRITE(LU,*) 'TIME-STEP (WITH CFL = 0.9): ',DT
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

