!                     ***************************
                      SUBROUTINE CALCS3D_THERMICV
!                     ***************************
!
     & (NPOIN3,NPOIN2,NPLAN,Z,IND_T,IND_S,TA,TEXP,TIMP,LONGIT,
     &  LATIT,LISTIN,AT,MARDAT,MARTIM)
!
!***********************************************************************
! WAQTEL   V7P2
!***********************************************************************
!
!brief   COMPUTES SOURCE TERMS FOR  WAQ THERMIC PROCESS COUPLED WITH T3D
!
!history  R. ATA
!+        21/03/2016
!+        V7P2
!+  Creation from an example in old SOURCE_TRAC (V7P0 and V7P1)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DT             |-->| TIME STEP
!| IND_T          |-->| INDEX OF THE TEMPERATURE IN THE TRACER TABLE
!| LISTIN         |-->| LOGICAL FOR LISTING
!| LONGIT         |-->| LONGITUTE OF ORIGIN POINT
!| LATIT          |-->| LATITUDE OF ORIGIN POINT
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| NPLAN          |-->| NUMBER OF VERTICAL PLANES
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
!| YASMI          |-->| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!| Z              |-->| Z COORDINATES FOR NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY:NEBU,ZSD,CP_EAU,RO0
      USE EXCHANGE_WITH_ATMOSPHERE
      USE INTERFACE_WAQTEL, EX_CALCS3D_THERMICV => CALCS3D_THERMICV
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN2,NPOIN3,NPLAN
      INTEGER, INTENT(IN)           :: IND_T,IND_S
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(IN)  :: Z(NPOIN3),LATIT,LONGIT,AT
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TEXP,TIMP
      LOGICAL,        INTENT(IN)    :: LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  LOCAL VARIABLES
!
      INTEGER          I,J,IPLAN
      DOUBLE PRECISION TREEL,SAL,RO,LAMB,RAY_SOL,KD
!
      INTRINSIC EXP
!
!----------------------------------------------------------------------
!
!       EXAMPLE OF PENETRATION OF THE SOLAR RADIATION
!
!       SOURCE IN TEMPERATURE NOT EQUAL TO ZERO

        TEXP%ADR(IND_T)%P%TYPR='Q'
!
!       INCIDENT SOLAR RADIATION
!
        CALL SOLRAD(RAY_SOL,NEBU,MARDAT,MARTIM,AT,LATIT,LONGIT)
!
!       FORMULA FOR TURBID WATER WITH SECCHI LENGTH
!
        KD  = 1.7D0/ZSD ! 83% OF THE INCIDENT ENERGY IS ABSORBED
        SAL = 0.D0
        DO I=1,NPOIN2
          DO IPLAN=1,NPLAN
            J = I + (IPLAN-1)*NPOIN2
            TREEL=TA%ADR(IND_T)%P%R(NPOIN3-NPOIN2+I)
            IF (IND_S.NE.0) THEN
              SAL = TA%ADR(IND_S)%P%R(NPOIN3-NPOIN2+I)
            ENDIF
            RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)**2-750.D0*SAL)*1.D-6)
            LAMB=RO*CP_EAU
            TEXP%ADR(IND_T)%P%R(J) =
     &       KD*EXP(KD*(Z(J)-Z(I+(NPLAN-1)*NPOIN2)))*RAY_SOL/LAMB
!
!           EXAMPLE OF FORMULA FOR TURBID WATER
!           ALL CONSTANTS MAY BE TUNED
!           0.22D0 = 1.D0-0.78D0
!           TEXP%ADR(IND_T)%P%R(J) =
!           ( 0.78D0*0.66D0 *EXP(0.66D0* (Z(J)-Z(I+(NPLAN-1)*NPOIN2)))
!     &      +0.22D0*0.125D0*EXP(0.125D0*(Z(J)-Z(I+(NPLAN-1)*NPOIN2))))
!     &     *RAY_SOL/LAMB
!
!           EXAMPLE OF FORMULA FOR CLEAR WATER
!           ALL CONSTANTS MAY BE TUNED
!           0.42D0 = 1.D0-0.58D0
!           TEXP%ADR(IND_T)%P%R(J) =
!           ( 0.58D0/0.35D0*EXP((Z(J)-Z(I+(NPLAN-1)*NPOIN2))/0.35D0)
!     &      +0.42D0/23.D0 *EXP((Z(J)-Z(I+(NPLAN-1)*NPOIN2))/23.D0 ))
!     &     *RAY_SOL/LAMB
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
