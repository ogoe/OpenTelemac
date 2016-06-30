!                    ***********************
                     SUBROUTINE FLUX_EF_VF_2
!                    ***********************
!
     &(PHIEL,NELEM,IKLE,IOPT,NPOIN,FN,FI_I,FSTAR,HN,H,SU,DDT,TETA,DFDT)
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
!+        29/04/2014
!+        V7P0
!+   First version, after Mario Ricchiuto's (INRIA Bordeaux) theory.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLOW           |<--| FLUXES PER SEGMENTS
!| FN             |-->| OPTIONAL ARGUMENT FOR PSI SCHEME
!| IKLE           |-->| CONNECTIVITY TABLE
!| IOPT           |-->| OPTION FOR THE CONSTANT PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| PHIEL          |-->| PER ELEMENT, FLUXES LEAVING POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FLUX_EF_VF_2 => FLUX_EF_VF_2
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IOPT,NELEM
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: PHIEL(NELEM,3)
      TYPE(BIEF_OBJ), INTENT(IN)      :: FN
      DOUBLE PRECISION, INTENT(INOUT) :: FI_I(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FSTAR(NPOIN),H(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SU(NELEM),DFDT(NPOIN),DDT,TETA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I
      DOUBLE PRECISION K1,K2,K3
      DOUBLE PRECISION FN1,FN2,FN3,FS1,FS2,FS3
      DOUBLE PRECISION BETA1,BETA2,BETA3,BETA1PSI,BETA2PSI,BETA3PSI
      DOUBLE PRECISION FINCORR1,FINCORR2,FINCORR3
      DOUBLE PRECISION FI,BETA1FI,BETA2FI,BETA3FI
      DOUBLE PRECISION PHITCOR,SUMAX,H1,H2,H3,COEF
!     PSI FLUXES WITH FN (FP..) AND FSTAR (FS..)
      DOUBLE PRECISION FP21,FP32,FP13,FP12,FP23,FP31
      DOUBLE PRECISION FS21,FS32,FS13,FS12,FS23,FS31
!
      INTRINSIC MIN,MAX,ABS
!
      DOUBLE PRECISION EPSPHI
      DATA EPSPHI/1.D-12/
!
!-----------------------------------------------------------------------
!
      IF(IOPT.EQ.2) THEN
!
!-----------------------------------------------------------------------
!
!       N-SCHEME, CORRECTOR
!
        DO I=1,NPOIN
          FI_I(I)=0.D0
        ENDDO
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
!         STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!         FIJ HERE LIKE LAMBDA(I,J) IN BOOK
!
          FP12=MAX(MIN(K1,-K2),0.D0)
          FP23=MAX(MIN(K2,-K3),0.D0)
          FP31=MAX(MIN(K3,-K1),0.D0)
          FP21=MAX(MIN(K2,-K1),0.D0)
          FP32=MAX(MIN(K3,-K2),0.D0)
          FP13=MAX(MIN(K1,-K3),0.D0)
!         NOT VERY USEFUL HERE AS THEY WILL NOT BE MODIFIED...
          FS12=FP12
          FS23=FP23
          FS31=FP31
          FS21=FP21
          FS32=FP32
          FS13=FP13
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
          FS1=FSTAR(I1)
          FS2=FSTAR(I2)
          FS3=FSTAR(I3)
!
!         COMPUTE THE NEW RESIDUAL AND NEW DISTRIBUTION
!
          COEF=SU(IELEM)/3.D0
          H1=((1.D0-TETA)*H(I1)+TETA*HN(I1))*COEF
          H2=((1.D0-TETA)*H(I2)+TETA*HN(I2))*COEF
          H3=((1.D0-TETA)*H(I3)+TETA*HN(I3))*COEF
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!
          FINCORR1=H1*DFDT(I1)
     &            +(1.D0-TETA)*(FP12*(FN1-FN2)+FP13*(FN1-FN3))
     &            +      TETA *(FS12*(FS1-FS2)+FS13*(FS1-FS3))
          FINCORR2=H2*DFDT(I2)
     &            +(1.D0-TETA)*(FP21*(FN2-FN1)+FP23*(FN2-FN3))
     &            +      TETA *(FS21*(FS2-FS1)+FS23*(FS2-FS3))
          FINCORR3=H3*DFDT(I3)
     &            +(1.D0-TETA)*(FP31*(FN3-FN1)+FP32*(FN3-FN2))
     &            +      TETA *(FS31*(FS3-FS1)+FS32*(FS3-FS2))
!
!         PHITCOR IS THE NEW TOTAL RESIDUAL,
!
          PHITCOR=FINCORR1+FINCORR2+FINCORR3
!
          IF(ABS(PHITCOR).GT.EPSPHI) THEN
!           NO BOUNDARY WITH FLUX IN THIS TRIANGLE
!           THE BETA OF N SCHEME
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
          ENDIF
!
        ENDDO
!
      ELSEIF(IOPT.EQ.3) THEN
!
!-----------------------------------------------------------------------
!
!       PSI-SCHEME, CORRECTOR
!
        DO I=1,NPOIN
          FI_I(I)=0.D0
        ENDDO
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
          FS12=FP12
          FS23=FP23
          FS31=FP31
          FS21=FP21
          FS32=FP32
          FS13=FP13
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
          FS1=FSTAR(I1)
          FS2=FSTAR(I2)
          FS3=FSTAR(I3)
!
!         NOT IN MARIO RICCHIUTO'S THEORY !!!! BUT SLIGHTLY BETTER......
!
!         FLUXES MODIFIED ACCORDING TO CLASSICAL PSI SCHEME AND FN
!
          BETA1FI=FP12*(FN1-FN2)+FP13*(FN1-FN3)
          BETA2FI=FP21*(FN2-FN1)+FP23*(FN2-FN3)
          BETA3FI=FP31*(FN3-FN1)+FP32*(FN3-FN2)
          FI=BETA1FI+BETA2FI+BETA3FI
          IF(FI.GT.0.D0) THEN
            IF(BETA1FI.GT.FI) THEN
              FP12=FP12*FI/BETA1FI
              FP13=FP13*FI/BETA1FI
            ELSEIF(BETA1FI.LT.0.D0) THEN
              FP12=0.D0
              FP13=0.D0
            ENDIF
            IF(BETA2FI.GT.FI) THEN
              FP21=FP21*FI/BETA2FI
              FP23=FP23*FI/BETA2FI
            ELSEIF(BETA2FI.LT.0.D0) THEN
              FP21=0.D0
              FP23=0.D0
            ENDIF
            IF(BETA3FI.GT.FI) THEN
              FP31=FP31*FI/BETA3FI
              FP32=FP32*FI/BETA3FI
            ELSEIF(BETA3FI.LT.0.D0) THEN
              FP31=0.D0
              FP32=0.D0
            ENDIF
          ELSEIF(FI.LT.0.D0) THEN
            IF(BETA1FI.LT.FI) THEN
              FP12=FP12*FI/BETA1FI
              FP13=FP13*FI/BETA1FI
            ELSEIF(BETA1FI.GT.0.D0) THEN
              FP12=0.D0
              FP13=0.D0
            ENDIF
            IF(BETA2FI.LT.FI) THEN
              FP21=FP21*FI/BETA2FI
              FP23=FP23*FI/BETA2FI
            ELSEIF(BETA2FI.GT.0.D0) THEN
              FP21=0.D0
              FP23=0.D0
            ENDIF
            IF(BETA3FI.LT.FI) THEN
              FP31=FP31*FI/BETA3FI
              FP32=FP32*FI/BETA3FI
            ELSEIF(BETA3FI.GT.0.D0) THEN
              FP31=0.D0
              FP32=0.D0
            ENDIF
          ELSE
            FP12=0.D0
            FP23=0.D0
            FP31=0.D0
            FP21=0.D0
            FP32=0.D0
            FP13=0.D0
          ENDIF
!
!         FLUXES MODIFIED ACCORDING TO CLASSICAL PSI SCHEME AND FSTAR
!
          BETA1FI=FS12*(FS1-FS2)+FS13*(FS1-FS3)
          BETA2FI=FS21*(FS2-FS1)+FS23*(FS2-FS3)
          BETA3FI=FS31*(FS3-FS1)+FS32*(FS3-FS2)
          FI=BETA1FI+BETA2FI+BETA3FI
          IF(FI.GT.0.D0) THEN
            IF(BETA1FI.GT.FI) THEN
              FS12=FS12*FI/BETA1FI
              FS13=FS13*FI/BETA1FI
            ELSEIF(BETA1FI.LT.0.D0) THEN
              FS12=0.D0
              FS13=0.D0
            ENDIF
            IF(BETA2FI.GT.FI) THEN
              FS21=FS21*FI/BETA2FI
              FS23=FS23*FI/BETA2FI
            ELSEIF(BETA2FI.LT.0.D0) THEN
              FS21=0.D0
              FS23=0.D0
            ENDIF
            IF(BETA3FI.GT.FI) THEN
              FS31=FS31*FI/BETA3FI
              FS32=FS32*FI/BETA3FI
            ELSEIF(BETA3FI.LT.0.D0) THEN
              FS31=0.D0
              FS32=0.D0
            ENDIF
          ELSEIF(FI.LT.0.D0) THEN
            IF(BETA1FI.LT.FI) THEN
              FS12=FS12*FI/BETA1FI
              FS13=FS13*FI/BETA1FI
            ELSEIF(BETA1FI.GT.0.D0) THEN
              FS12=0.D0
              FS13=0.D0
            ENDIF
            IF(BETA2FI.LT.FI) THEN
              FS21=FS21*FI/BETA2FI
              FS23=FS23*FI/BETA2FI
            ELSEIF(BETA2FI.GT.0.D0) THEN
              FS21=0.D0
              FS23=0.D0
            ENDIF
            IF(BETA3FI.LT.FI) THEN
              FS31=FS31*FI/BETA3FI
              FS32=FS32*FI/BETA3FI
            ELSEIF(BETA3FI.GT.0.D0) THEN
              FS31=0.D0
              FS32=0.D0
            ENDIF
          ELSE
            FS12=0.D0
            FS23=0.D0
            FS31=0.D0
            FS21=0.D0
            FS32=0.D0
            FS13=0.D0
          ENDIF
!
!         END OF "NOT IN MARIO RICCHIUTO'S THEORY" !!!!!!
!
!         COMPUTE THE NEW RESIDUAL AND NEW DISTRIBUTION
!
          COEF=SU(IELEM)/3.D0
          H1=((1.D0-TETA)*H(I1)+TETA*HN(I1))*COEF
          H2=((1.D0-TETA)*H(I2)+TETA*HN(I2))*COEF
          H3=((1.D0-TETA)*H(I3)+TETA*HN(I3))*COEF
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!
          FINCORR1=H1*DFDT(I1)
     &            +(1.D0-TETA)*(FP12*(FN1-FN2)+FP13*(FN1-FN3))
     &            +      TETA *(FS12*(FS1-FS2)+FS13*(FS1-FS3))
          FINCORR2=H2*DFDT(I2)
     &            +(1.D0-TETA)*(FP21*(FN2-FN1)+FP23*(FN2-FN3))
     &            +      TETA *(FS21*(FS2-FS1)+FS23*(FS2-FS3))
          FINCORR3=H3*DFDT(I3)
     &            +(1.D0-TETA)*(FP31*(FN3-FN1)+FP32*(FN3-FN2))
     &            +      TETA *(FS31*(FS3-FS1)+FS32*(FS3-FS2))
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
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FLUX_EF_VF_2 :'
          WRITE(LU,*) 'OPTION INCONNUE : ',IOPT
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'FLUX_EF_VF_2:'
          WRITE(LU,*) 'UNKNOWN OPTION: ',IOPT
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

