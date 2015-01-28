!                    ***********************
                     SUBROUTINE FLUX_EF_VF_2
!                    ***********************
!
     &(PHIEL,NELEM,IKLE,IOPT,NPOIN,FN,FI_I,FSTAR,H,SU,DDT)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IOPT,NELEM
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: PHIEL(NELEM,3)
      TYPE(BIEF_OBJ), INTENT(IN)      :: FN
      DOUBLE PRECISION, INTENT(INOUT) :: FI_I(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FSTAR(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SU(NELEM),DDT      
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I
      DOUBLE PRECISION K1,K2,K3,F12,F23,F31
      DOUBLE PRECISION FN1,FN2,FN3,F21,F32,F13
      DOUBLE PRECISION BETA1,BETA2,BETA3,BETA1PSI,BETA2PSI,BETA3PSI
      DOUBLE PRECISION FINCORR1,FINCORR2,FINCORR3
      DOUBLE PRECISION FI,BETA1FI,BETA2FI,BETA3FI
      DOUBLE PRECISION PHITCOR,SUMAX,H1,H2,H3,COEF 
!
      INTRINSIC MIN,MAX,ABS
!
!-----------------------------------------------------------------------
!
      IF(IOPT.EQ.3) THEN
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
          F12=MAX(MIN(K1,-K2),0.D0)
          F23=MAX(MIN(K2,-K3),0.D0)
          F31=MAX(MIN(K3,-K1),0.D0)
          F21=MAX(MIN(K2,-K1),0.D0)
          F32=MAX(MIN(K3,-K2),0.D0)
          F13=MAX(MIN(K1,-K3),0.D0)
!
          FN1=FN%R(I1)
          FN2=FN%R(I2)
          FN3=FN%R(I3)
!
!         NOT IN MARIO RICCHIUTO'S THEORY !!!!
!
!         FLUXES MODIFIED ACCORDING TO CLASSICAL PSI SCHEME
!
          BETA1FI=F12*(FN1-FN2)+F13*(FN1-FN3)
          BETA2FI=F21*(FN2-FN1)+F23*(FN2-FN3)
          BETA3FI=F31*(FN3-FN1)+F32*(FN3-FN2)
          FI=BETA1FI+BETA2FI+BETA3FI
          IF(FI.GT.0.D0) THEN
            IF(BETA1FI.GT.FI) THEN
              F12=F12*FI/BETA1FI
              F13=F13*FI/BETA1FI
            ELSEIF(BETA1FI.LT.0.D0) THEN
              F12=0.D0
              F13=0.D0
            ENDIF
            IF(BETA2FI.GT.FI) THEN
              F21=F21*FI/BETA2FI
              F23=F23*FI/BETA2FI
            ELSEIF(BETA2FI.LT.0.D0) THEN
              F21=0.D0
              F23=0.D0
            ENDIF
            IF(BETA3FI.GT.FI) THEN
              F31=F31*FI/BETA3FI
              F32=F32*FI/BETA3FI
            ELSEIF(BETA3FI.LT.0.D0) THEN
              F31=0.D0
              F32=0.D0
            ENDIF
          ELSEIF(FI.LT.0.D0) THEN
            IF(BETA1FI.LT.FI) THEN
              F12=F12*FI/BETA1FI
              F13=F13*FI/BETA1FI
            ELSEIF(BETA1FI.GT.0.D0) THEN
              F12=0.D0
              F13=0.D0
            ENDIF
            IF(BETA2FI.LT.FI) THEN
              F21=F21*FI/BETA2FI
              F23=F23*FI/BETA2FI
            ELSEIF(BETA2FI.GT.0.D0) THEN
              F21=0.D0
              F23=0.D0
            ENDIF
            IF(BETA3FI.LT.FI) THEN
              F31=F31*FI/BETA3FI
              F32=F32*FI/BETA3FI
            ELSEIF(BETA3FI.GT.0.D0) THEN
              F31=0.D0
              F32=0.D0
            ENDIF
          ELSE
            F12=0.D0
            F23=0.D0
            F31=0.D0
            F21=0.D0
            F32=0.D0
            F13=0.D0
          ENDIF
!
!         END OF "NOT IN MARIO RICCHIUTO'S THEORY" !!!!!!
!         
!         COMPUTE THE NEW RESIDUAL AND NEW DISTRIBUTION
!
          COEF=SU(IELEM)/(DDT*3.D0)
          H1=H(I1)*COEF
          H2=H(I2)*COEF
          H3=H(I3)*COEF
!
!         AS CLASSICAL N SCHEME, BUT WITH DERIVATIVE IN TIME ADDED
!
          FINCORR1=H1*(FSTAR(I1)-FN1)+F12*(FN1-FN2)+F13*(FN1-FN3)
          FINCORR2=H2*(FSTAR(I2)-FN2)+F21*(FN2-FN1)+F23*(FN2-FN3)
          FINCORR3=H3*(FSTAR(I3)-FN3)+F31*(FN3-FN1)+F32*(FN3-FN2)
!
!         PHITCOR IS THE NEW TOTAL RESIDUAL,
!
          PHITCOR=FINCORR1+FINCORR2+FINCORR3
!
          IF(ABS(PHITCOR).GT.1.D-25) THEN
            BETA1=FINCORR1/PHITCOR
            BETA2=FINCORR2/PHITCOR
            BETA3=FINCORR3/PHITCOR
            SUMAX=MAX(0.D0,BETA1)+MAX(0.D0,BETA2)+MAX(0.D0,BETA3)
            IF(SUMAX.NE.0.D0) THEN
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
