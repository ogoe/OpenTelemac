!                    **********************
                      SUBROUTINE CALCS2D_O2
!                    **********************
     & (NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,K22,
     &  PHOTO,RESP,TN,TEXP,TIMP,T1,T2,T3,T4,NTRAC,HN,HPROP,UN,VN,
     &  MASSOU,DT,VOLU2D,YATEMP,IND_T,DEBUG)
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                        21/09/2014
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR O2 WAQ PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBUG         |-->| FOR DEBUGGING
!| DEMBEN        |-->| BENTIC DEMAND
!| DT            |-->| TIME STEP
!| FORMK2        |-->| FORMULA FOR COMPUTINK K2
!| K1            |-->| CONST. OF DEGRADATION KINETIC OF ORGANIC LOAD
!| HN            |-->| DEPTH AT TIME N
!| HPROP         |-->| PROPAGATION DEPTH
!| IND_T         |-->| INDEX OF TEMPERATURE IN TRACER TABLE
!| K22           |<--| CONST. OF REAERATION
!| K44           |-->| CONST. OF NITRIFICATION KINETIC
!| MASSOU        |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN         |-->| NUMBER OF NODES IN THE MESH
!| NTRAC         |-->| TOTAL NUMBER OF TRACERS
!| O2SATU        |<--| O2 SATURATION DENSITY OF WATER (CS)
!| PHOTO         |-->| PHOTOSYNTHESIS
!| WATTEMP       |-->| TEMPERATURE OF WATER (DEG CELSIUS)
!| T1,T2,T3,T4   |-->| WORKING ARRAY
!| TEXP          |<--| EXPLICIT SOURCE TERM.
!| TIMP          |<--| IMPLICIT SOURCE TERM.
!| TN            |-->| TRACERS AT TIME N
!| UN,VN         |-->| VELOCITY COMPONENTS AT TIME N
!| VOLU2D        |-->| BASES AREA (NON ASSEMBLED)
!| YATEMP        |-->| IF TEMPERATURE IS VARIABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_WAQTEL,ONLY: FORMCS,K2,SECTODAY
      USE INTERFACE_PARALLEL
      USE INTERFACE_WAQTEL, EX_CALCS2D_O2 => CALCS2D_O2
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER          , INTENT(IN   ) :: FORMK2,NPOIN,NTRAC,IND_T,DEBUG
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: DEMBEN,WATTEMP
      DOUBLE PRECISION , INTENT(IN   ) :: PHOTO,RESP,K1,K44,DT
      DOUBLE PRECISION , INTENT(INOUT) :: O2SATU,K22,MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HN,HPROP,UN,VN,VOLU2D
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,T1,T2,T3,T4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      INTEGER                     :: I,RANKTR1,RANKTR2,RANKTR3
      INTEGER         , PARAMETER :: ADDTR = 3
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: BENCORR,PMOINR,CONV_K1,CONV_K44
      DOUBLE PRECISION            :: POWER
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
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
      DO I=1,NPOIN
        IF(YATEMP)POWER=TN%ADR(IND_T)%P%R(I)-20.D0
!       CORR2T AND BENCOR STOCKED HERE IN T3,T4
        T3%R(I)=CORR2**POWER
        T4%R(I)=BENCORR*(CORR1**POWER)
      ENDDO
!
!     COMPUTE CS (O2SATU, STOCKED IN T2)
!
      IF(.NOT.YATEMP)THEN
        CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
        CALL OS('X=C     ',X=T2,C=O2SATU       )
      ELSE
        DO I=1,NPOIN
          CALL SATUR_O2(T2%R(I),FORMCS,TN%ADR(IND_T)%P%R(I),EPS)
        ENDDO
      ENDIF
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN,1,UN,VN,HPROP,EPS)
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
      DO I=1,NPOIN
        TEXP%ADR(RANKTR1)%P%R(I)= T3%R(I)*K2%R(I)*
     &          MAX((T2%R(I)-TN%ADR(RANKTR1)%P%R(I)),0.D0)
!               O2 DENSITY CAN NOT BE GREATER THAN O2SATU
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

      DO I=1,NPOIN
        TEXP%ADR(RANKTR1)%P%R(I)= TEXP%ADR(RANKTR1)%P%R(I)-
     &                            T4%R(I)/MAX(EPS,HPROP%R(I))
      ENDDO
      IF(DEBUG.GT.0)WRITE(LU,*)'IN O2, STEP1:',TEXP%ADR(RANKTR1)%P%R(1)

!     IMPLICIT PART:
!     ==============
!     SOFAR, ALL TERMS ARE EXPLICIT FOR O2. CHANGE
!     IF THERE ARE DISAPPOINTING RESULTS
!
!     SECOND TRACER [L] ORGANIC LOAD
!
      CALL OS('X=CY    ',X=TIMP%ADR(RANKTR2)%P,Y=HPROP,C=-CONV_K1)
      IF(DEBUG.GT.0)WRITE(LU,*)'IN O2, STEP2:',TIMP%ADR(RANKTR2)%P%R(1)
!
!     THIRD TRACER [NH4]
!
      CALL OS('X=CY    ',X=TIMP%ADR(RANKTR3)%P,Y=HPROP,C=-CONV_K44)
      IF(DEBUG.GT.0)WRITE(LU,*)'IN O2, STEP3:',TIMP%ADR(RANKTR3)%P%R(1)
!
!     CONVERT DAYS TO SECONDS: NOW DONE IN REAER
!
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS
!                   (IMPLICIT PART IS ADDED IN CVDFTR)
!
      MASSOU(RANKTR1) = 0.D0
      DO I=1,NPOIN
        MASSOU(RANKTR1) = MASSOU(RANKTR1)
     &                  + HN%R(I)*TEXP%ADR(RANKTR1)%P%R(I)*VOLU2D%R(I)
      ENDDO
      MASSOU(RANKTR1)=MASSOU(RANKTR1)*DT
      IF(NCSIZE.GT.1) MASSOU(RANKTR1)=P_DSUM(MASSOU(RANKTR1))
!
!-----------------------------------------------------------------------
!
      RETURN
      END
