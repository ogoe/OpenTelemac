!                    **********************
                      SUBROUTINE CALCS3D_O2
!                    **********************
     & (NPOIN3,NPOIN2,NPLAN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,TIMP,T31,T32,T21,T22,NTRAC,HN,HPROP,ZPROP,
     &  UN,VN,YATEMP,IND_T)
!
!
!***********************************************************************
! TELEMAC2D   V7P2                                        21/02/2016
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR O2 WAQ PROCESS FOR 3D CASE
!
!history  R. ATA
!+        21/02/2016
!+        V7P2
!+       CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEMBEN        |-->| BENTIC DEMAND
!| DT            |-->| TIME STEP
!| FORMK2        |-->| FORMULA FOR COMPUTINK K2
!| HN            |-->| DEPTH AT TIME N
!| HPROP         |-->| PROPAGATION DEPTH
!| IND_T         |-->| INDEX OF TEMPERATURE IN TRACER TABLE
!| K1            |-->| CONST. OF DEGRADATION KINETIC OF ORGANIC LOAD
!| K22           |<--| CONST. OF REAERATION
!| K44           |-->| CONST. OF NITRIFICATION KINETIC 
!| MASSOU        |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN         |-->| NUMBER OF NODES IN THE MESH
!| NTRAC         |-->| TOTAL NUMBER OF TRACERS
!| O2SATU        |<--| O2 SATURATION DENSITY OF WATER (CS) 
!| PHOTO         |-->| PHOTOSYNTHESIS
!| WATTEMP       |-->| TEMPERATURE OF WATER (DEG CELSIUS)
!| T1,T2         |-->| WORKING ARRAYS
!| TEXP          |<--| EXPLICIT SOURCE TERM.
!| TIMP          |<--| IMPLICIT SOURCE TERM.
!| TN            |-->| TRACERS AT TIME N
!| UN,VN         |-->| VELOCITY COMPONENTS AT TIME N
!| YATEMP        |-->| IF YES TEMPERATURE IS VARIABLE
!| ZPROP         |-->| PROPAGATION ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY: FORMCS,K2,SECTODAY
      USE INTERFACE_PARALLEL
      USE INTERFACE_WAQTEL, EX_CALCS3D_O2 => CALCS3D_O2
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3,NPLAN
      INTEGER          , INTENT(IN   ) :: FORMK2,NTRAC,IND_T
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HN,HPROP,UN,VN,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T31,T32,T21,T22
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      INTEGER                     :: I,J,RANKTR1,RANKTR2,RANKTR3
      INTEGER         , PARAMETER :: ADDTR = 3
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: BENCORR,PMOINR,CONV_K1,CONV_K44
      DOUBLE PRECISION            :: POWER
      INTRINSIC MAX
!
!     PRELIMINARY COMPUTATIONS
!
      RANKTR1 = NTRAC-ADDTR+1
      RANKTR2 = RANKTR1+1
      RANKTR3 = NTRAC
!     INITIALIZE
      CALL OS('X=0     ',X=TEXP%ADR(RANKTR1)%P)
      CALL OS('X=0     ',X=TIMP%ADR(RANKTR2)%P) 
      CALL OS('X=0     ',X=TIMP%ADR(RANKTR3)%P) 
!
      PMOINR  = (PHOTO-RESP)*SECTODAY
!     CONVERT DAYS TO SECONDS
      CONV_K44= K44   *SECTODAY
      CONV_K1 = K1    *SECTODAY
      BENCORR = DEMBEN*SECTODAY
      POWER   = WATTEMP-20.D0
      DO I=1,NPOIN3
        IF(YATEMP) POWER=TN%ADR(IND_T)%P%R(I)-20.D0
!       CORR2T AND BENCOR STOCKED HERE IN T31,T32
        T31%R(I)=CORR2**POWER
        T32%R(I)=BENCORR*(CORR1**POWER)
      ENDDO
!
!     COMPUTE CS (O2SATU, STOCKED IN T21) 
!
      IF(.NOT.YATEMP)THEN
        CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
        CALL OS('X=C     ',X=T21,C=O2SATU       )
      ELSE
        DO I=1,NPOIN2
          J=(NPLAN-1)*NPOIN2+I
          CALL SATUR_O2(T21%R(J),FORMCS,TN%ADR(IND_T)%P%R(J),EPS)
        ENDDO
      ENDIF
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN2,NPLAN,UN,VN,HPROP,EPS)
!     CONVERT DAY TO SEC
      CALL OS('X=CX    ',X=K2,C=SECTODAY)
!
!     COMPUTE RS (DONE IN DIFSOU)
!
!----------------------------------------------------------------------
!     LET'S NOW COMPUTE SOURCE TERMS
!
!     FIRST TRACER O2 (RANK NTRAC-ADDTR+1) 
!     warning: here there are lots of choices that can be changed 
!              by the user: 
!              1- reareration is considered for only water surface,
!              2- bentic demand concerns only the bottom or all the water
!              column (here the second choice is considered)
!              these choices and many others, have to be decided by WAQ
!              experts
!
!     EXPLICIT PART 
!     =============
!
!     SURFACE SOURCES
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(RANKTR1)%P%R(J)= T31%R(J)*K2%R(I)*
     &          MAX((T21%R(I)-TN%ADR(RANKTR1)%P%R(J)),0.D0)-
!               O2 DENSITY CAN NOT BE GREATER THAN O2SATU 
     &          T32%R(I)/MAX(EPS,ZPROP%R(J))
      ENDDO
!
!     ADD PHOTOSYNTHESIS - RESPIRATION
!
      CALL OS('X=X+C   ',X=TEXP%ADR(RANKTR1)%P,C=PMOINR)
!
!     THE REMAINING TERMS 
!
      CALL OS('X=X+CY  ',X=TEXP%ADR(RANKTR1)%P,Y=TN%ADR(RANKTR2)%P,
     &         C=-CONV_K1 )
      CALL OS('X=X+CY  ',X=TEXP%ADR(RANKTR1)%P,Y=TN%ADR(NTRAC  )%P,
     &         C=-CONV_K44)
!     IMPLICIT PART: 
!     ==============
!     SOFAR, ALL TERMS ARE EXPLICIT FOR O2. CHANGE   
!     IF THERE ARE DISAPPOINTING RESULTS
!
!     SECOND TRACER [L] ORGANIC LOAD
!
      CALL OS('X=C     ',X=TIMP%ADR(RANKTR2)%P,C=CONV_K1)
!
!     THIRD TRACER [NH4]
!
      CALL OS('X=C     ',X=TIMP%ADR(RANKTR3)%P,C=CONV_K44)
!    
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS (TO CHECK)
!
!     ACTIVATE BIEF_OBJ FOR FURTHER CALCULATIONS
      TEXP%ADR(RANKTR1)%P%TYPR='Q'
      TIMP%ADR(RANKTR2)%P%TYPR='Q'
      TIMP%ADR(NTRAC  )%P%TYPR='Q'
!-----------------------------------------------------------------------
!
      RETURN
      END
