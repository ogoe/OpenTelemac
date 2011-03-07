!                    **********************
                     SUBROUTINE TCHAM_SMALL
!                    **********************
!
     *(HI,HJ,ETAI,ETAJ,UI,UJ,VI,VJ,G,FLX)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: G,HI,HJ,ETAI,ETAJ,UI,UJ
      DOUBLE PRECISION, INTENT(IN)    :: VI,VJ
      DOUBLE PRECISION, INTENT(INOUT) :: FLX(3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!***********************************************************************
      INTEGER IVAR
!
      DOUBLE PRECISION ZFI,ZFJ
!
      INTEGER CHOICE_D
      DOUBLE PRECISION ALPHA,EPS,FLUIJ_1
      DOUBLE PRECISION U_IJ,D_IJ,C_IJ,C_I,C_J
      DOUBLE PRECISION  FLUIJ_2,FLUIJ_3
!-----------------------------------------------------------------------
!**************************************************************
! VOIR SON EFFET
         ALPHA=1.D0
         CHOICE_D=2
         EPS=1.E-6
!**************************************************************
! INITIALISATION DE FLX
         DO IVAR=1,3
           FLX(IVAR) = 0.D0
         ENDDO
!
!-----------------------------------------------------------------------
! BATHYMETRIES
         ZFI   =    ETAI-HI
         ZFJ   =    ETAJ-HJ
!
! WET/DRY TREATMENT
!
        CALL WETDRY(ETAI,ZFI,HI,UI,VI,ETAJ,ZFJ,HJ,UJ,VJ,EPS)
!
1234   CONTINUE
!
! LET'S COMPUTE D_IJ
!
      IF(CHOICE_D.EQ.1)THEN
! ZOKAGOA'S CHOICE
        U_IJ=0.5D0*(UI+UJ)
        C_IJ=SQRT(0.5*G*(HI+HJ))
        D_IJ=ALPHA*MAX(ABS(U_IJ-C_IJ),MAX(ABS(U_IJ),ABS(U_IJ+C_IJ)))
        ELSEIF(CHOICE_D.EQ.2)THEN
! TORO'S CHOICE
        C_I=SQRT(G*HI)
        C_J=SQRT(G*HJ)
        D_IJ=MAX(ABS(UI)+C_I,ABS(UJ)+C_J)
      ENDIF
5000   CONTINUE
!
! CENTERED FLUX COMPUTATION
!
! TCHAMEN FLUX
           FLUIJ_1=0.5D0*(HI*UI+HJ*UJ)
           FLUIJ_2=0.5D0*(HI*(UI*UI)+HJ*(UJ*UJ)+G*HI*(ETAI+ETAJ))
           FLUIJ_3=0.5D0*(HI*UI*VI+HJ*UJ*VJ)
!
! UPWIND ADDING
!
          FLUIJ_1=FLUIJ_1-0.5D0*D_IJ*(ETAJ-ETAI)
          FLUIJ_2=FLUIJ_2-0.5D0*D_IJ*(HJ*UJ-HI*UI)
          FLUIJ_3=FLUIJ_3-0.5D0*D_IJ*(HJ*VJ-HI*VI)
!
! FINAL FLUX
!
         FLX(1) =  FLUIJ_1
         FLX(2) =  FLUIJ_2
         FLX(3) =  FLUIJ_3
!
5500   CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END