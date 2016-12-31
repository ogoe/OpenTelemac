!                       *****************
                        SUBROUTINE QWINDL
!                       *****************
!
     &( TSTOT , FREQ  , USOLD , USNEW , TWOLD , TWNEW , TETA  ,
     &  NF    , NPLAN , NPOIN2, CIMPLI, USN   , USO   , FPMO  , FPMN )
!
!**********************************************************************
! TOMAWAC   V6P3                                   27/06/2011
!**********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE LINEAR WAVE GENERATION
!+               (BY WIND) SOURCE TERM BASED ON CAVALERI AND
!+                MALANOTTE-RIZZOLI (1981)
!
!reference   CAVALERI L. & P. MALANOTTE-RIZZOLI, 1981 :
!+                 "WIND WAVE PREDICTION IN SHALLOW WATER : THEORY AND
!+                  APPLICATIONS". J. GEOPHYS. RES., 86(C5),10,961-975
!
!reference   TOLMAN  (1992) : EFFECT OF NUMERICS ON THE PHYSICS IN
!+                A THIRD-GENERATION WIND-WAVE MODEL, JPO, VOL 22,
!+                PP 1095-1111.
!
!history  E. GAGNAIRE-RENOU (EDF/LNHE)
!+        09/2010
!+        V6P0
!+
!
!history  G.MATTAROLO (EDF - LNHE)
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF/LNHE)
!+        23/12/2012
!+        V6P3
!+   A first optimisation.
!
!history  J-M HERVOUET (EDF/LNHE)
!+        09/07/2013
!+        V6P3
!+   (1.D0/1.D-90)**4 triggers an overflow, 20 put instead of 90.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CIMPLI         |-->| IMPLICITATION COEFFICIENT FOR SOURCE TERMS
!| FPMN           |<->| WORK TABLE
!| FPMO           |<->| WORK TABLE
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |<->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| TWNEW          |-->| WIND DIRECTION AT TIME N+1
!| TWOLD          |-->| WIND DIRECTION AT TIME N
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USOLD          |-->| FRICTION VELOCITY AT TIME N
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| USN            |<--| WORK TABLE
!| USO            |<--| WORK TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  APPELS :    - PROGRAMME(S) APPELANT  : SEMIMP
!  ********    - PROGRAMME(S) APPELE(S) :    -
!**********************************************************************
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT
!
      USE INTERFACE_TOMAWAC, EX_QWINDL => QWINDL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NPLAN,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: CIMPLI
      DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF),TETA(NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: FPMO(NPOIN2),FPMN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TWOLD(NPOIN2),TWNEW(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: USO(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: USN(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NPLAN,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,JF,IP
      DOUBLE PRECISION C1,C2,DIREC,ALPHAN,ALPHAO,SURFREQ4
!
      INTRINSIC MAX,COS,EXP
!
!-----------------------------------------------------------------------
!
      C1 = 1.5D-3/GRAVIT**2
      C2 = GRAVIT/(DEUPI*28.D0)
!
!     ARRAYS DEPENDING ONLY ON POINTS
!
      DO IP=1,NPOIN2
        FPMO(IP)=(C2/MAX(USOLD(IP),1.D-20))**4
        FPMN(IP)=(C2/MAX(USNEW(IP),1.D-20))**4
      ENDDO
!
!     ARRAYS DEPENDING ONLY ON POINTS AND DIRECTIONS
!     COULD BE OPTIMISED MORE BY DECOMPOSING THE COS...
!
      DO JP=1,NPLAN
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          USO(IP,JP)=C1*(MAX(USOLD(IP)*COS(DIREC-TWOLD(IP)),0.D0))**4
          USN(IP,JP)=C1*(MAX(USNEW(IP)*COS(DIREC-TWNEW(IP)),0.D0))**4
        ENDDO
      ENDDO
!
!     LOOP ON THE DISCRETISED FREQUENCIES
!
      DO JF=1,NF
        SURFREQ4=1.D0/FREQ(JF)**4
        DO JP=1,NPLAN
          DO IP=1,NPOIN2
            ALPHAO=USO(IP,JP)*EXP( -FPMO(IP)*SURFREQ4 )
            ALPHAN=USN(IP,JP)*EXP( -FPMN(IP)*SURFREQ4 )
!           TAKES THE SOURCE TERM INTO ACCOUNT
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     &                      + (ALPHAO + CIMPLI*(ALPHAN-ALPHAO))
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
