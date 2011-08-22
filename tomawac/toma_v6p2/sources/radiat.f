!                    *****************
                     SUBROUTINE RADIAT
!                    *****************
!
     &( FX    , FY    , SXX   , SXY   , SYY   , XK1   , FS    , CG1   ,
     &  DEPTH1, CGSUC1, DSXXDX, DSXYDX, DSXYDY, DSYYDY, NPOIN2_DIM    )
!
!***********************************************************************
! TOMAWAC   V6P1                                   27/06/2011
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
!| NPOIN2_DIM     |-->| NUMBER OF POINTS IN 2D MESH
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
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      DOUBLE PRECISION DEUPI
!
      INTEGER NPOIN2_DIM
      DOUBLE PRECISION FS(NPOIN2_DIM,NPLAN,NF),CG1(NPOIN2_DIM,NF)
      DOUBLE PRECISION DEPTH1(NPOIN2_DIM), CGSUC1(NPOIN2_DIM,NF)
      DOUBLE PRECISION XK1(NPOIN2_DIM,NF)
      DOUBLE PRECISION DSXXDX(NPOIN2_DIM),DSXYDX(NPOIN2_DIM),
     &                 FX(NPOIN2_DIM)
      DOUBLE PRECISION DSXYDY(NPOIN2_DIM),DSYYDY(NPOIN2_DIM),
     &                 FY(NPOIN2_DIM)
      DOUBLE PRECISION SXX(NPOIN2_DIM),SXY(NPOIN2_DIM),SYY(NPOIN2_DIM)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION COEF  , COCO  , SISI  , SICO  , OMEGA , ROGER
      DOUBLE PRECISION DTETAR, C
!
!
      DEUPI=6.283185307D0
      ROGER=ROEAU*GRAVIT
      DTETAR=DEUPI/DBLE(NPLAN)
!
      DO IP=1,NPOIN2
        SXX(IP) = 0.D0
        SXY(IP) = 0.D0
        SYY(IP) = 0.D0
      ENDDO
!
!.....COMPUTES THE WORKING ARRAY N = CG/C
!     """"""""""""""""""""""""""""""""""""""""""""
      DO JF = 1,NF
        OMEGA=DEUPI*FREQ(JF)
        DO IP=1,NPOIN2
          CGSUC1(IP,JF)=CG1(IP,JF)*XK1(IP,JF)/OMEGA
        ENDDO
      ENDDO
!
!.....COMPUTES THE RADIATION STRESSES INTEGRATED OVER THE SPECTRUM
!.....(SUMS UP THE DISCRETISED PART OF THE SPECTRUM)
!     """""""""""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NPLAN
        COCO=COSTET(JP)*COSTET(JP)
        SICO=SINTET(JP)*COSTET(JP)
        SISI=SINTET(JP)*SINTET(JP)
        DO JF=1,NF
          COEF=GRAVIT*DFREQ(JF)*DTETAR
          DO IP=1,NPOIN2
            SXX(IP)=SXX(IP)
     &             +(CGSUC1(IP,JF)*(1.D0+SISI)-0.5D0)*FS(IP,JP,JF)*COEF
            SXY(IP)=SXY(IP)
     &             +(CGSUC1(IP,JF)*SICO)*FS(IP,JP,JF)*COEF
            SYY(IP)=SYY(IP)
     &             +(CGSUC1(IP,JF)*(1.D0+COCO)-0.5D0)*FS(IP,JP,JF)*COEF
          ENDDO
        ENDDO
      ENDDO
!
!.....COMPUTES THE GRADIENTS IN SPACE OF THE RADIATION STRESSES
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
!.....NO MASKING
      DO IP=1,NELEM2
        W1(IP)=1.D0
      ENDDO
!
!.....DERIVATIVE IN X
      CALL OV('X=Y     ',T4,SXX,T3,C,NPOIN2)
      CALL VECTOR
     & (ST1,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
!
      CALL OV('X=Y     ',T4,SXY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST2,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
!
      CALL VECTOR
     & (ST3,'=','GRADF          X',IELM2,1.D0,MESH%X,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
!
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST2,2,MESH)
          CALL PARCOM(ST3,2,MESH)
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      CALL OV('X=Y/Z   ',DSXXDX,T1,T3,C,NPOIN2)
      CALL OV('X=Y/Z   ',DSXYDX,T2,T3,C,NPOIN2)
!
!.....DERIVATIVE IN Y
      CALL OV('X=Y     ',T4,SYY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST1,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
!
      CALL OV('X=Y     ',T4,SXY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST2,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
!
      CALL VECTOR
     & (ST3,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
!
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST2,2,MESH)
          CALL PARCOM(ST3,2,MESH)
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      CALL OV('X=Y/Z   ',DSYYDY,T1,T3,C,NPOIN2)
      CALL OV('X=Y/Z   ',DSXYDY,T2,T3,C,NPOIN2)
!
!
!.....COMPUTES THE DRIVING FORCES FOR WAVE-INDUCED CURRENTS
!     """""""""""""""""""""""""""""""""""""""""""""""""""""
      DO IP=1,NPOIN2
        FX(IP)= - (DSXXDX(IP)+DSXYDY(IP))/DEPTH1(IP)
        FY(IP)= - (DSXYDX(IP)+DSYYDY(IP))/DEPTH1(IP)
      ENDDO
!
      RETURN
      END
