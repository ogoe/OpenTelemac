!                    ***********************
                     SUBROUTINE FLUX_EF_VF_3
!                    ***********************
!
     &(PHIEL,NELEM,ELTSEG,ORISEG,FXMATPAR,NSEG,IKLE,IOPT,NPOIN,
     & FN,FI_I,SU,HDFDT,TETA,YAFLULIM,FLULIM)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    Equivalent of FLUX_EF_VF, but only for PSI scheme, and the 
!+        result is given in terms of contribution per point, not fluxes
!+        between points, and takes a derivative in time into account.
!
!history  J-M HERVOUET & SARA PAVAN (EDF LAB, LNHE)
!+        04/08/2015
!+        V7P1
!+   First version. The N and PSI versions are not different here.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |-->| GIVES THE SEGMENTS IN A TRIANGLE
!| FI_I           |<--| CONTRIBUTIONS TO POINTS
!| FLULIM         |-->| LIMITATION OF FLUXES
!| FN             |-->| TRACER AT TIME T(N)
!| FXMARPAR       |-->| FLUXES ASSEMBLED IN //
!| HDFDT          |-->| DEPTH*DH/DT
!| IKLE           |-->| CONNECTIVITY TABLE
!| IOPT           |-->| OPTION : 2=N  3=PSI
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| ORISEG         |-->| GIVES THE ORIENTATION OF SEGMENTS IN A TRIANGLE
!| PHIEL          |-->| PER ELEMENT, FLUXES LEAVING POINTS
!| SU             |-->| SURFACE OF TRIANGLES
!| TETA           |-->| LOCAL IMPLICITATION
!| YAFLULIM       |-->| IF YES, A LIMITATION OF FLUXES IS GIVEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FLUX_EF_VF_3 => FLUX_EF_VF_3
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NPOIN,IOPT,NSEG
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: ORISEG(NELEM,3)
      LOGICAL, INTENT(IN)             :: YAFLULIM
      DOUBLE PRECISION, INTENT(IN)    :: PHIEL(NELEM,3),TETA(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMATPAR(NSEG),FLULIM(NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: FI_I(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SU(NELEM),HDFDT(NPOIN) 
      TYPE(BIEF_OBJ), INTENT(IN)      :: FN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I,ISEG1,ISEG2,ISEG3
      DOUBLE PRECISION K1,K2,K3,FN1,FN2,FN3
      DOUBLE PRECISION BETA1,BETA2,BETA3,BETA1PSI,BETA2PSI,BETA3PSI
      DOUBLE PRECISION FINCORR1,FINCORR2,FINCORR3
      DOUBLE PRECISION BETA1FI,BETA2FI,BETA3FI
      DOUBLE PRECISION PHITCOR,SUMAX,COEF
!     PSI FLUXES WITH FN
      DOUBLE PRECISION FP21,FP32,FP13,FP12,FP23,FP31,MIN12,MIN23,MIN13
!
      INTRINSIC MIN,MAX,ABS
!
      DOUBLE PRECISION EPSPHI
      DATA EPSPHI/1.D-12/
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        FI_I(I)=0.D0
      ENDDO
!
!     CHECKING THE PRESENCE OF FLULIM
!
      IF(.NOT.YAFLULIM) THEN
!
!       AS WHEN YAFLULIM=.TRUE., BUT WITH FLULIM ASSUMED TO BE 1
!
        DO IELEM = 1,NELEM
!
          K1 = -PHIEL(IELEM,1)
          K2 = -PHIEL(IELEM,2)
          K3 = -PHIEL(IELEM,3)
!
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!
          ISEG1=ELTSEG(IELEM,1)
          ISEG2=ELTSEG(IELEM,2)
          ISEG3=ELTSEG(IELEM,3)
!
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK
!
          FP12=MAX(MIN(K1,-K2),0.D0)
          FP23=MAX(MIN(K2,-K3),0.D0)
          FP31=MAX(MIN(K3,-K1),0.D0)
          FP21=MAX(MIN(K2,-K1),0.D0)
          FP32=MAX(MIN(K3,-K2),0.D0)
          FP13=MAX(MIN(K1,-K3),0.D0)
!
!         CORRECTING THE FLUXES WHEN THEIR SIGN IS NOT THE SAME
!         AS THE ASSEMBLED VALUE
!
!         SEGMENT 1
!
          IF(ORISEG(IELEM,1).EQ.1) THEN
            IF(FXMATPAR(ISEG1)*FP12.GT.0.D0) THEN
              FP12=0.D0
            ELSEIF(ABS(FP12).GT.ABS(FXMATPAR(ISEG1))) THEN
              FP12=-FXMATPAR(ISEG1)
            ENDIF
            IF(FXMATPAR(ISEG1)*FP21.LT.0.D0) THEN
              FP21=0.D0
            ELSEIF(ABS(FP21).GT.ABS(FXMATPAR(ISEG1))) THEN
              FP21=FXMATPAR(ISEG1)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG1)*FP12.LT.0.D0) THEN
              FP12=0.D0
            ELSEIF(ABS(FP12).GT.ABS(FXMATPAR(ISEG1))) THEN
              FP12=FXMATPAR(ISEG1)
            ENDIF
            IF(FXMATPAR(ISEG1)*FP21.GT.0.D0) THEN
              FP21=0.D0
            ELSEIF(ABS(FP21).GT.ABS(FXMATPAR(ISEG1))) THEN
              FP21=-FXMATPAR(ISEG1)
            ENDIF
          ENDIF
!
!         SEGMENT 2
!
          IF(ORISEG(IELEM,2).EQ.1) THEN
            IF(FXMATPAR(ISEG2)*FP23.GT.0.D0) THEN
              FP23=0.D0
            ELSEIF(ABS(FP23).GT.ABS(FXMATPAR(ISEG2))) THEN
              FP23=-FXMATPAR(ISEG2)
            ENDIF
            IF(FXMATPAR(ISEG2)*FP32.LT.0.D0) THEN
              FP32=0.D0
            ELSEIF(ABS(FP32).GT.ABS(FXMATPAR(ISEG2))) THEN
              FP32=FXMATPAR(ISEG2)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG2)*FP23.LT.0.D0) THEN
              FP23=0.D0
            ELSEIF(ABS(FP23).GT.ABS(FXMATPAR(ISEG2))) THEN
              FP23=FXMATPAR(ISEG2)
            ENDIF
            IF(FXMATPAR(ISEG2)*FP32.GT.0.D0) THEN
              FP32=0.D0
            ELSEIF(ABS(FP32).GT.ABS(FXMATPAR(ISEG2))) THEN
              FP32=-FXMATPAR(ISEG2)
            ENDIF
          ENDIF
!
!         SEGMENT 3
!
          IF(ORISEG(IELEM,3).EQ.1) THEN
            IF(FXMATPAR(ISEG3)*FP31.GT.0.D0) THEN
              FP31=0.D0
            ELSEIF(ABS(FP31).GT.ABS(FXMATPAR(ISEG3))) THEN
              FP31=-FXMATPAR(ISEG3)
            ENDIF
            IF(FXMATPAR(ISEG3)*FP13.LT.0.D0) THEN
              FP13=0.D0
            ELSEIF(ABS(FP13).GT.ABS(FXMATPAR(ISEG3))) THEN
              FP13=FXMATPAR(ISEG3)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG3)*FP31.LT.0.D0) THEN
              FP31=0.D0
            ELSEIF(ABS(FP31).GT.ABS(FXMATPAR(ISEG3))) THEN
              FP31=FXMATPAR(ISEG3)
            ENDIF
            IF(FXMATPAR(ISEG3)*FP13.GT.0.D0) THEN
              FP13=0.D0
            ELSEIF(ABS(FP13).GT.ABS(FXMATPAR(ISEG3))) THEN
              FP13=-FXMATPAR(ISEG3)
            ENDIF
          ENDIF
!
!         END OF CORRECTION OF FLUXES
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
!
!         MINIMUM OF THE 1-TETA
!
          MIN12=MIN(1.D0-TETA(I1),1.D0-TETA(I2))
          MIN13=MIN(1.D0-TETA(I1),1.D0-TETA(I3))
          MIN23=MIN(1.D0-TETA(I2),1.D0-TETA(I3))
!
!         PART OF CONTRIBUTIONS THAT WILL NOT BE LIMITED
!
          BETA1FI=
     &     FP12*(FN1*(1.D0-TETA(I1)-MIN12)-FN2*(1.D0-TETA(I2)-MIN12))
     &    +FP13*(FN1*(1.D0-TETA(I1)-MIN13)-FN3*(1.D0-TETA(I3)-MIN13))
          BETA2FI=
     &     FP21*(FN2*(1.D0-TETA(I2)-MIN12)-FN1*(1.D0-TETA(I1)-MIN12))
     &    +FP23*(FN2*(1.D0-TETA(I2)-MIN23)-FN3*(1.D0-TETA(I3)-MIN23))
          BETA3FI=
     &     FP31*(FN3*(1.D0-TETA(I3)-MIN13)-FN1*(1.D0-TETA(I1)-MIN13))
     &    +FP32*(FN3*(1.D0-TETA(I3)-MIN23)-FN2*(1.D0-TETA(I2)-MIN23))
!
          FI_I(I1)=FI_I(I1)+BETA1FI 
          FI_I(I2)=FI_I(I2)+BETA2FI
          FI_I(I3)=FI_I(I3)+BETA3FI
!       
!         NOW PART OF CONTRIBUTIONS THAT WILL BE LIMITED
!
          COEF=SU(IELEM)/3.D0
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!         HDFDT MUST BE ((1.D0-TETA)*H+TETA*HN)*(FSTAR-F)/DDT
!
          BETA1FI=FP12*(FN1-FN2)*MIN12+FP13*(FN1-FN3)*MIN13
          BETA2FI=FP21*(FN2-FN1)*MIN12+FP23*(FN2-FN3)*MIN23
          BETA3FI=FP31*(FN3-FN1)*MIN13+FP32*(FN3-FN2)*MIN23
!
          FINCORR1=HDFDT(I1)*COEF+BETA1FI
          FINCORR2=HDFDT(I2)*COEF+BETA2FI
          FINCORR3=HDFDT(I3)*COEF+BETA3FI
!
!         PHITCOR IS THE NEW TOTAL RESIDUAL,
!
          PHITCOR=FINCORR1+FINCORR2+FINCORR3
!
          IF(ABS(PHITCOR).GT.EPSPHI) THEN
!           NO BOUNDARY WITH FLUX IN THIS TRIANGLE
            BETA1=FINCORR1/PHITCOR
            BETA2=FINCORR2/PHITCOR
            BETA3=FINCORR3/PHITCOR
            SUMAX=MAX(0.D0,BETA1)+MAX(0.D0,BETA2)+MAX(0.D0,BETA3)
            IF(SUMAX.GT.1.D-20) THEN
              BETA1PSI=MAX(0.D0,BETA1)/SUMAX
              BETA2PSI=MAX(0.D0,BETA2)/SUMAX
              BETA3PSI=MAX(0.D0,BETA3)/SUMAX
              FI_I(I1)=FI_I(I1)+BETA1PSI*PHITCOR 
              FI_I(I2)=FI_I(I2)+BETA2PSI*PHITCOR
              FI_I(I3)=FI_I(I3)+BETA3PSI*PHITCOR
            ENDIF
          ELSE
            FI_I(I1)=FI_I(I1)+FINCORR1 
            FI_I(I2)=FI_I(I2)+FINCORR2
            FI_I(I3)=FI_I(I3)+FINCORR3
          ENDIF      
!
        ENDDO
!
      ELSE
!
        DO IELEM = 1,NELEM
!
          K1 = -PHIEL(IELEM,1)
          K2 = -PHIEL(IELEM,2)
          K3 = -PHIEL(IELEM,3)
!
          I1=IKLE(IELEM,1)
          I2=IKLE(IELEM,2)
          I3=IKLE(IELEM,3)
!
          ISEG1=ELTSEG(IELEM,1)
          ISEG2=ELTSEG(IELEM,2)
          ISEG3=ELTSEG(IELEM,3)
!
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK
!
          FP12=MAX(MIN(K1,-K2),0.D0)*FLULIM(ISEG1)
          FP23=MAX(MIN(K2,-K3),0.D0)*FLULIM(ISEG2)
          FP31=MAX(MIN(K3,-K1),0.D0)*FLULIM(ISEG3)
          FP21=MAX(MIN(K2,-K1),0.D0)*FLULIM(ISEG1)
          FP32=MAX(MIN(K3,-K2),0.D0)*FLULIM(ISEG2)
          FP13=MAX(MIN(K1,-K3),0.D0)*FLULIM(ISEG3)
!
!         CORRECTING THE FLUXES WHEN THEIR SIGN IS NOT THE SAME
!         AS THE ASSEMBLED VALUE
!
!         SEGMENT 1
!
          IF(ORISEG(IELEM,1).EQ.1) THEN
            IF(FXMATPAR(ISEG1)*FP12.GT.0.D0) THEN
              FP12=0.D0
            ELSEIF(ABS(FP12).GT.ABS(FXMATPAR(ISEG1))) THEN
              FP12=-FXMATPAR(ISEG1)
            ENDIF
            IF(FXMATPAR(ISEG1)*FP21.LT.0.D0) THEN
              FP21=0.D0
            ELSEIF(ABS(FP21).GT.ABS(FXMATPAR(ISEG1))) THEN
              FP21=FXMATPAR(ISEG1)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG1)*FP12.LT.0.D0) THEN
              FP12=0.D0
            ELSEIF(ABS(FP12).GT.ABS(FXMATPAR(ISEG1))) THEN
              FP12=FXMATPAR(ISEG1)
            ENDIF
            IF(FXMATPAR(ISEG1)*FP21.GT.0.D0) THEN
              FP21=0.D0
            ELSEIF(ABS(FP21).GT.ABS(FXMATPAR(ISEG1))) THEN
              FP21=-FXMATPAR(ISEG1)
            ENDIF
          ENDIF
!
!         SEGMENT 2
!
          IF(ORISEG(IELEM,2).EQ.1) THEN
            IF(FXMATPAR(ISEG2)*FP23.GT.0.D0) THEN
              FP23=0.D0
            ELSEIF(ABS(FP23).GT.ABS(FXMATPAR(ISEG2))) THEN
              FP23=-FXMATPAR(ISEG2)
            ENDIF
            IF(FXMATPAR(ISEG2)*FP32.LT.0.D0) THEN
              FP32=0.D0
            ELSEIF(ABS(FP32).GT.ABS(FXMATPAR(ISEG2))) THEN
              FP32=FXMATPAR(ISEG2)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG2)*FP23.LT.0.D0) THEN
              FP23=0.D0
            ELSEIF(ABS(FP23).GT.ABS(FXMATPAR(ISEG2))) THEN
              FP23=FXMATPAR(ISEG2)
            ENDIF
            IF(FXMATPAR(ISEG2)*FP32.GT.0.D0) THEN
              FP32=0.D0
            ELSEIF(ABS(FP32).GT.ABS(FXMATPAR(ISEG2))) THEN
              FP32=-FXMATPAR(ISEG2)
            ENDIF
          ENDIF
!
!         SEGMENT 3
!
          IF(ORISEG(IELEM,3).EQ.1) THEN
            IF(FXMATPAR(ISEG3)*FP31.GT.0.D0) THEN
              FP31=0.D0
            ELSEIF(ABS(FP31).GT.ABS(FXMATPAR(ISEG3))) THEN
              FP31=-FXMATPAR(ISEG3)
            ENDIF
            IF(FXMATPAR(ISEG3)*FP13.LT.0.D0) THEN
              FP13=0.D0
            ELSEIF(ABS(FP13).GT.ABS(FXMATPAR(ISEG3))) THEN
              FP13=FXMATPAR(ISEG3)
            ENDIF
          ELSE
            IF(FXMATPAR(ISEG3)*FP31.LT.0.D0) THEN
              FP31=0.D0
            ELSEIF(ABS(FP31).GT.ABS(FXMATPAR(ISEG3))) THEN
              FP31=FXMATPAR(ISEG3)
            ENDIF
            IF(FXMATPAR(ISEG3)*FP13.GT.0.D0) THEN
              FP13=0.D0
            ELSEIF(ABS(FP13).GT.ABS(FXMATPAR(ISEG3))) THEN
              FP13=-FXMATPAR(ISEG3)
            ENDIF
          ENDIF
!
!         END OF CORRECTION OF FLUXES
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
!
!         MINIMUM OF THE 1-TETA
!
          MIN12=MIN(1.D0-TETA(I1),1.D0-TETA(I2))
          MIN13=MIN(1.D0-TETA(I1),1.D0-TETA(I3))
          MIN23=MIN(1.D0-TETA(I2),1.D0-TETA(I3))
!
!         PART OF CONTRIBUTIONS THAT WILL NOT BE LIMITED
!
          BETA1FI=
     &     FP12*(FN1*(1.D0-TETA(I1)-MIN12)-FN2*(1.D0-TETA(I2)-MIN12))
     &    +FP13*(FN1*(1.D0-TETA(I1)-MIN13)-FN3*(1.D0-TETA(I3)-MIN13))
          BETA2FI=
     &     FP21*(FN2*(1.D0-TETA(I2)-MIN12)-FN1*(1.D0-TETA(I1)-MIN12))
     &    +FP23*(FN2*(1.D0-TETA(I2)-MIN23)-FN3*(1.D0-TETA(I3)-MIN23))
          BETA3FI=
     &     FP31*(FN3*(1.D0-TETA(I3)-MIN13)-FN1*(1.D0-TETA(I1)-MIN13))
     &    +FP32*(FN3*(1.D0-TETA(I3)-MIN23)-FN2*(1.D0-TETA(I2)-MIN23))
!
          FI_I(I1)=FI_I(I1)+BETA1FI 
          FI_I(I2)=FI_I(I2)+BETA2FI
          FI_I(I3)=FI_I(I3)+BETA3FI
!       
!         NOW PART OF CONTRIBUTIONS THAT WILL BE LIMITED
!
          COEF=SU(IELEM)/3.D0
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!         HDFDT MUST BE ((1.D0-TETA)*H+TETA*HN)*(FSTAR-F)/DDT
!
          BETA1FI=FP12*(FN1-FN2)*MIN12+FP13*(FN1-FN3)*MIN13
          BETA2FI=FP21*(FN2-FN1)*MIN12+FP23*(FN2-FN3)*MIN23
          BETA3FI=FP31*(FN3-FN1)*MIN13+FP32*(FN3-FN2)*MIN23
!
          FINCORR1=HDFDT(I1)*COEF+BETA1FI
          FINCORR2=HDFDT(I2)*COEF+BETA2FI
          FINCORR3=HDFDT(I3)*COEF+BETA3FI
!
!         PHITCOR IS THE NEW TOTAL RESIDUAL,
!
          PHITCOR=FINCORR1+FINCORR2+FINCORR3
!
          IF(ABS(PHITCOR).GT.EPSPHI) THEN
!           NO BOUNDARY WITH FLUX IN THIS TRIANGLE
            BETA1=FINCORR1/PHITCOR
            BETA2=FINCORR2/PHITCOR
            BETA3=FINCORR3/PHITCOR
            SUMAX=MAX(0.D0,BETA1)+MAX(0.D0,BETA2)+MAX(0.D0,BETA3)
            IF(SUMAX.GT.1.D-20) THEN
              BETA1PSI=MAX(0.D0,BETA1)/SUMAX
              BETA2PSI=MAX(0.D0,BETA2)/SUMAX
              BETA3PSI=MAX(0.D0,BETA3)/SUMAX
              FI_I(I1)=FI_I(I1)+BETA1PSI*PHITCOR 
              FI_I(I2)=FI_I(I2)+BETA2PSI*PHITCOR
              FI_I(I3)=FI_I(I3)+BETA3PSI*PHITCOR
            ENDIF
          ELSE
            FI_I(I1)=FI_I(I1)+FINCORR1 
            FI_I(I2)=FI_I(I2)+FINCORR2
            FI_I(I3)=FI_I(I3)+FINCORR3
          ENDIF      
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

