!                       *****************
                        SUBROUTINE QWINDL
!                       *****************
!
     &( TSTOT , FREQ  , USOLD , USNEW , TWOLD , TWNEW , TETA  , GRAVIT,
     &  NF    , NPLAN , NPOIN2, CIMPLI, USN   , USO   , ALPHAN, ALPHAO, 
     &  FPMO  , FPMN )
!
!**********************************************************************
! TOMAWAC   V6P1                                   27/06/2011
!**********************************************************************
!
!brief    COMPUTES THE CONTRIBUTION OF THE LINEAR WAVE GENERATION
!+               (BY WIND) SOURCE TERM BASED ON CAVALERI AND
!+                MALANOTTE-RIZZOLI (1981)
!
!reference   CAVALERI L. & P. MALANOTTE-RIZZOLI, 1981 :
!+                 "WIND WAVE PREDICTION IN SHALLOW WATER : THEORY AND
!+                  APPLICATIONS". J. GEOPHYS. RES., 86(C5),10,961-975
!reference   TOLMAN  (1992) : EFFECT OF NUMERICS ON THE PHYSICS IN 
!+                A THIRD-GENERATION WIND-WAVE MODEL, JPO, VOL 22,
!+                PP 1095-1111.
!
!history  E. GAGNAIRE-RENOU (EDF/LNHE)
!+        09/2010
!+        V6P0
!
!history  G.MATTAROLO (EDF - LNHE)
!+        27/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALPHAN         |<->| WORK TABLE
!| ALPHAO         |<->| WORK TABLE
!| CIMPLI         |-->| IMPLICITATION COEFFICIENT FOR SOURCE TERMS
!| FPMN           |<->| WORK TABLE
!| FPMO           |<->| WORK TABLE
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GRAVIT         |-->| GRAVITY ACCELERATION
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
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER     NF ,  NPLAN         , NPOIN2
      DOUBLE PRECISION  GRAVIT        , CIMPLI
      DOUBLE PRECISION  FREQ(NF)      , TETA(NPLAN)
      DOUBLE PRECISION  FPMO(NPOIN2)  , FPMN(NPOIN2)
      DOUBLE PRECISION  TWOLD(NPOIN2) , TWNEW(NPOIN2) , USNEW(NPOIN2)
      DOUBLE PRECISION  ALPHAO(NPOIN2), ALPHAN(NPOIN2), USOLD(NPOIN2)
      DOUBLE PRECISION  USO(NPOIN2)   , USN(NPOIN2)
      DOUBLE PRECISION  TSTOT(NPOIN2,NPLAN,NF)  
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF   , IP
      DOUBLE PRECISION  C1  , DIREC , CONSTO , CONSTN ,DEUPI , DIMPLI
!
!
      C1 = 1.5D-3 * GRAVIT**(-2) 
      DEUPI = 2.D0* 3.14159265358978D0
      DIMPLI=1.0D0-CIMPLI
!
!.....LOOP ON THE DISCRETISED DIRECTIONS
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NPLAN
!
!.......PRECALCULATION OF THE DIRECTIONALS DEPENDENCES
!       """"""""""""""""""""""""""""""""""""""""""""""
        DIREC=TETA(JP)
        DO IP=1,NPOIN2
          USO(IP)=USOLD(IP)*COS(DIREC-TWOLD(IP))
          USN(IP)=USNEW(IP)*COS(DIREC-TWNEW(IP))
          FPMO(IP)=GRAVIT/(DEUPI*28*(USOLD(IP)+1.D-90))
          FPMN(IP)=GRAVIT/(DEUPI*28*(USNEW(IP)+1.D-90))
        ENDDO
!
!.....LOOP ON THE DISCRETISED FREQUENCIES
!     """"""""""""""""""""""""""""""""""""""""""""
        DO JF=1,NF
!
!.........COMPUTES THE PROPORTIONALITY COEFFICIENTS BETA.
!         """"""""""""""""""""""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            CONSTO=C1*EXP(-(FREQ(JF)/FPMO(IP))**(-4))
            CONSTN=C1*EXP(-(FREQ(JF)/FPMN(IP))**(-4))
            ALPHAO(IP)=CONSTO*(MAX(USO(IP),0.D0))**4
            ALPHAN(IP)=CONSTN*(MAX(USN(IP),0.D0))**4
          ENDDO
!
!.........TAKES THE SOURCE TERM INTO ACCOUNT
!         """"""""""""""""""""""""""""""""
          DO IP=1,NPOIN2
            TSTOT(IP,JP,JF) = TSTOT(IP,JP,JF)
     *                    + (DIMPLI*ALPHAO(IP) + CIMPLI*ALPHAN(IP))
          ENDDO
!
        ENDDO
!
      ENDDO
!
      RETURN
      END
