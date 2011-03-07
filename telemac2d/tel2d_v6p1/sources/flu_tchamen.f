!                    **********************
                     SUBROUTINE FLU_TCHAMEN
!                    **********************
!
     *(H1,H2,ETA1,ETA2,U1,U2,V1,V2,
     * XNN,YNN,FLX,G,EPS)
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
      DOUBLE PRECISION, INTENT(IN)    :: G,H1,H2,ETA1,ETA2,U1,U2
      DOUBLE PRECISION, INTENT(IN)    :: V1,V2,XNN,YNN,EPS
      DOUBLE PRECISION, INTENT(INOUT) :: FLX(3)
!***********************************************************************
!
      INTEGER NSG,J,IVAR,IS,K,ILIM,ERR
!
      DOUBLE PRECISION VNX,VNY,VNL,ZF1,ZF2
      DOUBLE PRECISION FLUIJ_20
      DOUBLE PRECISION SIGMAX,UNORM
!
      INTEGER CHOICE_D
      DOUBLE PRECISION ALPHA,FLUIJ_1
      DOUBLE PRECISION UI,UJ,VI,VJ
      DOUBLE PRECISION U_IJ,D_IJ,C_IJ,C_I,C_J,UI0,UJ0
      DOUBLE PRECISION  FLUIJ_2,FLUIJ_3
!-----------------------------------------------------------------------
!**************************************************************
! VOIR SON EFFET
         ALPHA=1.D0
         CHOICE_D=2
!**************************************************************
! INITIALISATION DE FLX
         DO IVAR=1,3
           FLX(IVAR) = 0.D0
         ENDDO
!
!-----------------------------------------------------------------------
! BATHYMETRIES
         ZF1   =    ETA1-H1
         ZF2   =    ETA2-H2
! VELOCITIES
         UI=U1
         VI=V1
         UJ=U2
         VJ=V2
!
! ROTATION
!
         UI0 = UI
         UI  = XNN*UI0+YNN*VI
         VI  =-YNN*UI0+XNN*VI
!
         UJ0 = UJ
         UJ  = XNN*UJ0+YNN*VJ
         VJ  =-YNN*UJ0+XNN*VJ
!
! WET/DRY TREATMENT
!
        CALL WETDRY(ETA1,ZF1,H1,UI,VI,ETA2,ZF2,H2,UJ,VJ,EPS)
!
1234   CONTINUE
!
! LET'S COMPUTE D_IJ
!
        IF(CHOICE_D.EQ.1)THEN
! ZOKAGOA'S CHOICE
        U_IJ=0.5D0*(UI+UJ)
        C_IJ=SQRT(0.5*G*(H1+H2))
        D_IJ=ALPHA*MAX(ABS(U_IJ-C_IJ),MAX(ABS(U_IJ),ABS(U_IJ+C_IJ)))
        ELSEIF(CHOICE_D.EQ.2)THEN
! TORO'S CHOICE
        C_I=SQRT(G*H1)
        C_J=SQRT(G*H2)
        D_IJ=MAX(ABS(UI)+C_I,ABS(UJ)+C_J)
        ELSE
!
! ERROR MESSAGE
!
        IF(LNG.EQ.1) WRITE(LU,4010) ERR
        IF(LNG.EQ.2) WRITE(LU,4020) ERR
4010    FORMAT(1X,'FLU_AZZ : ERREUR DANS LE CHOIX DE L UPWIND : ',/,1X,
     &         'CODE D''ERREUR : ',1I6)
4020    FORMAT(1X,'FLU_AZZ: ERROR IN THE UPWIND CHOICE: ',/,1X,
     &        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
       ENDIF
5000   CONTINUE
!
! CENTERED FLUX COMPUTATION
!
! TCHAMEN FLUX
           FLUIJ_1=0.5D0*(H1*UI+H2*UJ)
           FLUIJ_2=0.5D0*(H1*(UI*UI) + H2*(UJ*UJ) +
     &              G*H1*(ETA1+ETA2))
           FLUIJ_3=0.5D0*(H1*UI*VI+H2*UJ*VJ)
!
! UPWIND ADDING
!
          FLUIJ_1=FLUIJ_1-0.5D0*D_IJ*(ETA2-ETA1)
          FLUIJ_2=FLUIJ_2-0.5D0*D_IJ*(H2*UJ-H1*UI)
          FLUIJ_3=FLUIJ_3-0.5D0*D_IJ*(H2*VJ-H1*VI)
!
! INVERSE ROTATION
!
         FLUIJ_20 = FLUIJ_2
         FLUIJ_2  = XNN*FLUIJ_20-YNN*FLUIJ_3
         FLUIJ_3  = YNN*FLUIJ_20+XNN*FLUIJ_3
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