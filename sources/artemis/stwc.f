!                    ********************************
                     DOUBLE PRECISION FUNCTION STWC
!                    ********************************
!
     &(F,DIR)
!
!***********************************************************************
! ARTEMIS   V7P0                                 07/2014
!***********************************************************************
!
!brief    COMPUTES THE ENERGY DENSITY BASED ON A TOMAWAC SPECTRUM.
!
!history  C. PEYRARD (LNHE)
!+        07/2014
!+        V7P0
!+   Interpolation of arbitrary spectrum taken from TOMAWAC
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F             |-->| FREQUENCY FOR WHICH ENERGY DENSITY IS CALCULATED (Hz)
!| DIR           |-->| DIRECTION FOR WHICH ENERGY DENSITY IS CALCULATED (°)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!      USE INTERFACE_ARTEMIS, EX_SPE => SPE
      USE DECLARATIONS_ARTEMIS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      DOUBLE PRECISION F,DIR,F1,F2,D1,D2,DIR1,DIR2,EPS
      DOUBLE PRECISION SPT1F1,SPT1F2,SPT2F1,SPT2F2,SPTF1,SPTF2

      INTEGER          IFF,INDF,INDD,ID,IFF1,ID1
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
! TOMAWAC SPECTRUM is known at frequencies (f1<f2) and directions (t1<t2)
!  INTERPOLATION :
!  Sp(f,t) =  Sp(f1,t) + (f-f1)*(Sp(f2,t)-Sp(f1,t))/(f2-f1)
!   with
!   Sp(f1,t)=  Sp(f1,t1) + (t-t1)*(Sp(f1,t2)-Sp(f1,t1))/(t2-t1)
!   Sp(f2,t)=  Sp(f2,t1) + (t-t1)*(Sp(f2,t2)-Sp(f2,t1))/(t2-t1)

      EPS=1E-5
!     FIND FI and F2
!     --------------
      INDF=0
      F1=0D0
      F2=0D0
      DO IFF=1,NFTWC-1
        IF((FREQTWC(IFF)-EPS.LE.F).AND.(FREQTWC(IFF+1)+EPS.GE.F))THEN
          F1=FREQTWC(IFF)
          F2=FREQTWC(IFF+1)
          IFF1=IFF
          INDF=1
        ENDIF
      ENDDO
!
!     FIND DI and D2
!     --------------
      IF (DIR.LE.DIRTWC(1)) THEN
        DIR=DIR+360D0
      ENDIF
      INDD=0
      D1=0D0
      D2=0D0
      DO ID=1,NDTWC
        IF ((DIRTWC(ID).LE.DIR).AND.(DIRTWC(ID+1).GE.DIR)) THEN
          D1=DIRTWC(ID)
          D2=DIRTWC(ID+1)
          ID1=ID
          INDD=1
        ENDIF
      ENDDO
!
!
! ----------------------------------------------------------------------
! SPECTRUM COMPUTATION
! --------------------
      IF ((INDD*INDF).EQ.0) THEN
        STWC = 0D0
        WRITE(LU,*) '--------------------ALARM---------------------'
        WRITE(LU,*) 'ROUTINE STWC : YOU ASK FOR A PERIOD/DIRECTION '
        WRITE(LU,*) 'OUT OF THE TOMAWAC SPECTRUM                   '
        WRITE(LU,*) 'INDD,INDF=',INDD,INDF,F,DIR
        WRITE(LU,*) 'FMIN , FMAX=',FREQTWC(1),FREQTWC(NFTWC)
      ELSE
!     Sp(t1,f1), Sp(t1,f2), Sp(t2,f1), Sp(t2,f2)
        SPT1F1=SPETWC(ID1,IFF1)
        SPT1F2=SPETWC(ID1,IFF1+1)
        SPT2F1=SPETWC(ID1+1,IFF1)
        SPT2F2=SPETWC(ID1+1,IFF1+1)
!
        SPTF1= SPT1F1 + (DIR-D1)*(SPT2F1-SPT1F1)/(D2-D1)
        SPTF2= SPT1F2 + (DIR-D1)*(SPT2F2-SPT1F2)/(D2-D1)
        STWC = SPTF1  + (F  -F1)*(SPTF2 - SPTF2)/(F2-F1)
      ENDIF
!-----------------------------------------------------------------------
!
      RETURN
      END
