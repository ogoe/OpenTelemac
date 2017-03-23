!                     ***************************
                      SUBROUTINE CALCS3D_THERMICS
!                     ***************************
!
     & (NPOIN2,NPOIN3,IND_T,IND_S,TA,ATABOS,BTABOS,PATMOS,ATMOSEXCH,
     &  WIND,LISTIN)
!
!***********************************************************************
! WAQTEL   V7P2
!***********************************************************************
!
!brief   COMPUTES BOUNDARY CONDITIONS FOR WAQ THERMIC PROCESS
!        COUPLED WITH T3D
!
!history  R. ATA
!+        21/02/2016
!+        V7P2
!+  Creation from old BORD3D (V7P0 and V7P1)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DT             |-->| TIME STEP
!| DIMM           |-->| 2D OR 3D
!| IND_T          |-->| INDEX OF THE TEMPERATURE IN THE TRACER TABLE
!| LISTIN         |-->| LOGICAL FOR LISTING
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| NTRAC          |-->| NUMBER OF TRACERS
!| PATMOS         |-->| ATMOSPHERIC PRESSURE
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| VOLU2D         |-->| BASES AREA (NON ASSEMBLED)
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY:C_ATMOS,HREL,TAIR,NEBU,CP_EAU,RO0
      USE EXCHANGE_WITH_ATMOSPHERE
      USE INTERFACE_WAQTEL, EX_CALCS3D_THERMICS => CALCS3D_THERMICS
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN2,NPOIN3
      INTEGER, INTENT(IN)           :: IND_T,IND_S,ATMOSEXCH
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA,WIND
      TYPE(BIEF_OBJ), INTENT(INOUT) :: ATABOS,BTABOS
      TYPE(BIEF_OBJ), INTENT(IN)    :: PATMOS
      LOGICAL,        INTENT(IN)    :: LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  LOCAL VARIABLES
!
      INTEGER          IPOIN2
      DOUBLE PRECISION TREEL,SAL,RO,LAMB
      DOUBLE PRECISION FACT,WW,WW2,A
      DOUBLE PRECISION RAY_ATM,RAY_EAU,FLUX_EVAP
      DOUBLE PRECISION FLUX_SENS,DEBEVAP
!
! ----------------------------------------------------------------
!
!     INITIALISATION
      CALL OS( 'X=0     ' ,X=ATABOS%ADR(IND_T)%P)
      CALL OS( 'X=0     ' ,X=BTABOS%ADR(IND_T)%P)
      IF(ATMOSEXCH.EQ.0) RETURN
      IF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
!
        FACT=LOG(1.D4)/LOG(5.D4)
        DO IPOIN2=1,NPOIN2
          TREEL=TA%ADR(IND_T)%P%R(NPOIN3-NPOIN2+IPOIN2)
          IF (IND_S.EQ.0) THEN
            SAL = 0.D0
          ELSE
            SAL = TA%ADR(IND_S)%P%R(NPOIN3-NPOIN2+IPOIN2)
          ENDIF
          RO = RO0*(1.D0-(7.D0*(TREEL-4.D0)**2-750.D0*SAL)*1.D-6)
          LAMB=RO*CP_EAU
!
          WW = SQRT(WIND%ADR(1)%P%R(IPOIN2)*WIND%ADR(1)%P%R(IPOIN2)
     &       + WIND%ADR(2)%P%R(IPOIN2)*WIND%ADR(2)%P%R(IPOIN2))
!         LOG LAW FOR WIND AT 2 METERS
!          WW2 = WW * LOG(2.D0/0.0002D0)/LOG(10.D0/0.0002D0)
!         WRITTEN BELOW AS:
          WW2 = WW * FACT
!         ALTERNATIVE LAW FOR WIND AT 2 METERS
!          WW2 = 0.6D0*WW
          IF(ATMOSEXCH.EQ.1) THEN
            A=(4.48D0+0.049D0*TREEL)+2021.5D0*C_ATMOS*(1.D0+WW)*
     &        (1.12D0+0.018D0*TREEL+0.00158D0*TREEL**2)/LAMB
            ATABOS%ADR(IND_T)%P%R(IPOIN2)=-A
            BTABOS%ADR(IND_T)%P%R(IPOIN2)= A*TAIR%R(IPOIN2)
          ELSEIF(ATMOSEXCH.EQ.2) THEN
!
!     SENSIBLE HEAT FLUXES
!
            CALL EVAPO(TREEL,TAIR%R(IPOIN2),WW2,PATMOS%R(IPOIN2),HREL,
     &                 RO,FLUX_EVAP,FLUX_SENS,DEBEVAP,C_ATMOS)
!
!     LONGWAVE HEAT FLUXES
!
            CALL SHORTRAD(TREEL,TAIR%R(IPOIN2),NEBU,HREL,RAY_ATM,
     &                    RAY_EAU)
!
!     BOUNDARY CONDITION FOR TEMPERATURE AT SURFACE
!
            ATABOS%ADR(IND_T)%P%R(IPOIN2) = 0.D0
            BTABOS%ADR(IND_T)%P%R(IPOIN2) = (RAY_ATM-RAY_EAU-FLUX_EVAP
     &                                      -FLUX_SENS)/LAMB
          ENDIF
        ENDDO
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) "CALCS3D_THERMICS: MODELE D'ECHANGE AVEC "
          WRITE(LU,*) "        L'ATMOSPHERE NON ENCORE PROGRAMME"
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) "CALCS3D_THERMICS: MODELE EXCHANGE WITH  "
          WRITE(LU,*) "        THE ATMOSPHERE NOT IMPLEMENTED YET"
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
