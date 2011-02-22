C                       **********************
                        SUBROUTINE TCHAM_SMALL
C                       **********************

     *(HI,HJ,ETAI,ETAJ,UI,UJ,VI,VJ,G,FLX)
C
      USE BIEF
C  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)    :: G,HI,HJ,ETAI,ETAJ,UI,UJ
      DOUBLE PRECISION, INTENT(IN)    :: VI,VJ
      DOUBLE PRECISION, INTENT(INOUT) :: FLX(3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C***********************************************************************
      INTEGER IVAR     
C
      DOUBLE PRECISION ZFI,ZFJ
c
      INTEGER CHOICE_D
      DOUBLE PRECISION ALPHA,EPS,FLUIJ_1
      DOUBLE PRECISION U_IJ,D_IJ,C_IJ,C_I,C_J
      DOUBLE PRECISION  FLUIJ_2,FLUIJ_3

C-----------------------------------------------------------------------
C**************************************************************
C VOIR SON EFFET
         ALPHA=1.D0
         CHOICE_D=2
         EPS=1.E-6
C**************************************************************
C INITIALISATION DE FLX      
         DO IVAR=1,3
           FLX(IVAR) = 0.D0
         ENDDO
C
C-----------------------------------------------------------------------
C BATHYMETRIES
         ZFI   =    ETAI-HI
         ZFJ   =    ETAJ-HJ
C
C WET/DRY TREATMENT
C
        CALL WETDRY(ETAI,ZFI,HI,UI,VI,ETAJ,ZFJ,HJ,UJ,VJ,EPS)
C
1234   CONTINUE
C
C LET'S COMPUTE D_IJ
C
      IF(CHOICE_D.EQ.1)THEN
C ZOKAGOA'S CHOICE

        U_IJ=0.5D0*(UI+UJ)
        C_IJ=SQRT(0.5*G*(HI+HJ))
        D_IJ=ALPHA*MAX(ABS(U_IJ-C_IJ),MAX(ABS(U_IJ),ABS(U_IJ+C_IJ)))

        ELSEIF(CHOICE_D.EQ.2)THEN
C TORO'S CHOICE
        C_I=SQRT(G*HI)
        C_J=SQRT(G*HJ)
        D_IJ=MAX(ABS(UI)+C_I,ABS(UJ)+C_J)
      ENDIF

5000   CONTINUE
C
C CENTERED FLUX COMPUTATION
C
C TCHAMEN FLUX
           FLUIJ_1=0.5D0*(HI*UI+HJ*UJ)
           FLUIJ_2=0.5D0*(HI*(UI*UI)+HJ*(UJ*UJ)+G*HI*(ETAI+ETAJ))
           FLUIJ_3=0.5D0*(HI*UI*VI+HJ*UJ*VJ)
C
C UPWIND ADDING
C 
          FLUIJ_1=FLUIJ_1-0.5D0*D_IJ*(ETAJ-ETAI)
          FLUIJ_2=FLUIJ_2-0.5D0*D_IJ*(HJ*UJ-HI*UI)
          FLUIJ_3=FLUIJ_3-0.5D0*D_IJ*(HJ*VJ-HI*VI)
C
C FINAL FLUX 
C
         FLX(1) =  FLUIJ_1
         FLX(2) =  FLUIJ_2 
         FLX(3) =  FLUIJ_3 
C
5500   CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
