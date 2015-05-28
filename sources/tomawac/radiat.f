!                    *****************
                     SUBROUTINE RADIAT
!                    *****************
!
     &( FX    , FY    , SXX   , SXY   , SYY   , XK1   , FS    , CG1   ,
     &  DEPTH1, CGSUC1, DSXXDX, DSXYDX, DSXYDY, DSYYDY )
!
!***********************************************************************
! TOMAWAC   V7P0                                
!***********************************************************************
!
!brief    COMPUTES THE RADIATION STRESSES AND DRIVING FORCES
!+                FOR THE GENERATION OF WAVE-INDUCED CURRENTS.
!+
!+           (SEE NOTES FOR METHODOLOGY)
!code
!+  THE RESULT OF THIS COMPUTATION IS GIVEN AS :
!+       FI = - 1/D D( SIJ )/D( XJ )    UNIT : M/S**2
!
!note     COMPUTATION ACCORDING TO THE "THEORICAL" FORMULATION, WITH
!+          COMPUTATION OF THE TERMS IN THE TENSOR OF THE RADIATION
!+          STRESSES, AND THEN THEIR GRADIENTS IN SPACE.
!
!history  M. BENOIT (EDF/DER/LNH)
!+        13/12/95
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
!history  J-M HERVOUET (EDF - LNHE)
!+        08/01/2014
!+        V7P0
!+   CALL PARCOM suppressed by using new argument ASSPAR in VECTOR
!
!history  C. VILLARET (HR-WALLINGFORD)
!+        15/09/2014
!+        V7P0
!+   Cancellation of radiation stresses below a given depth hmin.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CG1            |-->| DISCRETIZED GROUP VELOCITY
!| CGSUC1         |<--| WORK TABLE
!| DEPTH1         |-->| WATER DEPTH
!| DSXXDX         |<->| WORK TABLE
!| DSXYDX         |<->| WORK TABLE
!| DSXYDY         |<->| WORK TABLE
!| DSYYDY         |<->| WORK TABLE
!| FS             |-->| DIRECTIONAL SPECTRUM
!| FX             |<--| DRIVING FORCE ALONG X
!| FY             |<--| DRIVING FORCE ALONG Y
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SXX            |<--| RADIATION STRESS ALONG XX
!| SXY            |<--| RADIATION STRESS ALONG XY
!| SYY            |<--| RADIATION STRESS ALONG YY
!| XK1            |-->| DISCRETIZED WAVE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: FS(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CG1(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH1(NPOIN2),XK1(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: SXX(NPOIN2),SXY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: SYY(NPOIN2),CGSUC1(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: FX(NPOIN2),FY(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DSXXDX(NPOIN2),DSXYDX(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DSXYDY(NPOIN2),DSYYDY(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!    
      INTEGER JP,JF,IP
      DOUBLE PRECISION COEF,COEF2,COCO,SISI,SICO,OMEGA,DTETAR,C
! 
!     CV: MINIMUM WATER DEPTH FOR TIDAL FLATS TREATMENT
!     NEW KEYWORD IS NEEDED
!
      DOUBLE PRECISION HMIN
!
!-----------------------------------------------------------------------
!
      HMIN=0.1D0     
!
      DTETAR=DEUPI/NPLAN
!
      DO IP=1,NPOIN2
        SXX(IP) = 0.D0
        SXY(IP) = 0.D0
        SYY(IP) = 0.D0
      ENDDO
!
!     COMPUTES THE WORKING ARRAY N = CG/C
!    
      DO JF = 1,NF
        OMEGA=DEUPI*FREQ(JF)
        DO IP=1,NPOIN2
          CGSUC1(IP,JF)=CG1(IP,JF)*XK1(IP,JF)/OMEGA
        ENDDO
      ENDDO
!
!     COMPUTES THE RADIATION STRESSES INTEGRATED OVER THE SPECTRUM
!     SUMS UP THE DISCRETISED PART OF THE SPECTRUM
!     
      DO JP=1,NPLAN
        COCO=COSTET(JP)**2
        SISI=SINTET(JP)**2
        SICO=SINTET(JP)*COSTET(JP)       
        DO JF=1,NF
          COEF=GRAVIT*DFREQ(JF)*DTETAR
          DO IP=1,NPOIN2
            COEF2=COEF*FS(IP,JP,JF)
            SXX(IP)=SXX(IP)+(CGSUC1(IP,JF)*(1.D0+SISI)-0.5D0)*COEF2
            SXY(IP)=SXY(IP)+(CGSUC1(IP,JF)*SICO             )*COEF2
            SYY(IP)=SYY(IP)+(CGSUC1(IP,JF)*(1.D0+COCO)-0.5D0)*COEF2
          ENDDO
        ENDDO
      ENDDO
!
!     COMPUTES THE GRADIENTS IN SPACE OF THE RADIATION STRESSES
! 
!
!     INVERSE OF INTEGRALS OF TEST FUNCTIONS
!
      CALL VECTOR(ST0,'=','MASBAS          ',IELM2,1.D0,
     &            ST0,ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,
     &            ASSPAR=.TRUE.)
      CALL OS('X=1/Y   ',X=ST0,Y=ST0)
!    
!     DERIVATIVE IN X
!
      CALL OV('X=Y     ',T4,SXX,T3,C,NPOIN2)
      CALL VECTOR
     & (ST1,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL OV('X=Y     ',T4,SXY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST2,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL OV('X=YZ    ',DSXXDX,T1,ST0%R,C,NPOIN2)
      CALL OV('X=YZ    ',DSXYDX,T2,ST0%R,C,NPOIN2)
!
!     DERIVATIVE IN Y
!
      CALL OV('X=Y     ',T4,SYY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST1,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL OV('X=Y     ',T4,SXY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST2,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,ST3,ASSPAR=.TRUE.)
!
      CALL OV('X=YZ    ',DSYYDY,T1,ST0%R,C,NPOIN2)
      CALL OV('X=YZ    ',DSXYDY,T2,ST0%R,C,NPOIN2)
!
!     COMPUTES THE DRIVING FORCES FOR WAVE-INDUCED CURRENTS
!     
      DO IP=1,NPOIN2
        IF(DEPTH1(IP).GE.HMIN) THEN
          FX(IP)= - (DSXXDX(IP)+DSXYDY(IP))/DEPTH1(IP)
          FY(IP)= - (DSXYDX(IP)+DSYYDY(IP))/DEPTH1(IP)
        ELSE
          FX(IP)=0.D0
          FY(IP)=0.D0
        ENDIF 
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

