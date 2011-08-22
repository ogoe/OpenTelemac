!                    *****************
                     SUBROUTINE FLUCIN
!                    *****************
!
     &(NS,NSEG,NUBO,G,X,Y,CFL,DT,UA,ZF,VNOCL,CE,NORDRE,CMI,JMI,
     & DJX,DJY,DX,DY,BETA,DSZ0,AIRS,AIRST,HC,FLUXTEMP,NPTFR,NBOR,
     & XNEBOR,YNEBOR,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FLUXES FOR THE INTERNAL INTERFACES.
!
!history  INRIA
!+
!+        V5P4
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRS           |-->| CELL'S AREA
!| AIRST          |-->| AREA OF SUB-TRIANGLES IN THE CELLS
!| BETA           |-->| EXTRAPOLATION COEFFICIENT FOR ORDRE 2
!| CE             |<->| FLUX INCREMENTS AT INTERNAL INTERFACES
!| CFL            |-->| CFL NUMBER
!| CMI            |-->| COORDINATES OF MIDDLE POINTS OF INTERFACES
!| DJX,DJY        |-->| GRADIENTS PER TRIANGLE
!| DSZ0           |-->| VARIATIONS Z (BATHY) FOR ORDRE 2
!| DT             |<->| TIME STEP (CAN CHANGE IF ORDRE 2)
!| DX,DY          |-->| GRADIENTS PER NODE
!| FLUXTEMP       |<--| MASS FLUX OF TRACER
!| G              |-->| GRAVITY
!| HC             |<--| REBUILT H (ORDRE 2)
!| JMI            |-->| NUMBER OF THE TRIANGLE IN WHICH IS LOCATED
!|                |   | THE MIDDLE POINT OF THE INTERFACE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY NODES
!| NORDRE         |-->| ORDRE OF THE SCHEME
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NS             |-->| TOTAL NUMER OF POINTS IN THE MESH
!| NSEG           |-->| NUMBER OF EDGES IN THE MESH
!| NTRAC          |-->| NUMBER OF TRACERS 
!| NUBO           |-->| GLOBAL NUMBERS OF THE NODES FORMING THE EDGE
!| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!| VNOCL          |-->| NORMAL VECTOR TO THE INTERFACE
!|                |   | (2 FIRST COMPONENTS) AND
!|                |   | LENGTH OF THE SEGMENT (3RD COMPONENT)
!| X,Y            |-->| COORDINATES IF THE NODES
!| XNEBOR,YNEBOR  |-->| NORMAL VECTOR TO BOUNDARY NODES 
!| ZF             |-->| BATHYMETRY
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
      INTEGER, INTENT(IN) :: NS,NSEG,NPTFR,NORDRE,NTRAC
      INTEGER, INTENT(IN) :: NBOR(*),NUBO(2,NSEG),JMI(*)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(*),YNEBOR(*),X(NS),Y(NS)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NS),VNOCL(3,NSEG),AIRS(*)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFL,UA(3,NS),AIRST(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: DSZ0(2,*),CMI(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA,DT,CE(NS,3),HC(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: DJX(3,*),DJY(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: DX(3,*),DY(3,*)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUXTEMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     AUTOMATIC EX ARRAYS!!!!!!
!
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: DSH(:,:),DSU(:,:)
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: DSV(:,:)
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: DSP(:),DSM(:),DSZ(:,:)
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: CORR(:),DTLL(:)
!
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
!-----------------------------------------------------------------------
!
      INTEGER NSG,NUBO1,NUBO2,J,IVAR,IS,K,ILIM,ERR,ITRAC
!
      DOUBLE PRECISION VNX,VNY,VNL,RA2,RA3,ALP,ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION UAS11,UAS12,UAS21,UAS22,UAS31,UAS32,AMDS
      DOUBLE PRECISION GRADI(3),GRADJ(3),GRADIJ(3),GRADJI(3)
      DOUBLE PRECISION GRADI2,GRADIJ2,GRADJ2,GRADJI2
      DOUBLE PRECISION AIX,AIY,AJX,AJY,HI,HI0,HIJ,CIJ,UAS210,UAS220
      DOUBLE PRECISION HJ,HJ0,HJI,CJI,DZIJ,DZJI
      DOUBLE PRECISION EXT1,EXT2,FLU11,FLU21,FLU31,FLU12,FLU22
      DOUBLE PRECISION FLU210,A01,A11,A21,A02,A12,A22
      DOUBLE PRECISION HGZI,HGZJ,HDXZ1,HDYZ1,HDXZ2,HDYZ2
      DOUBLE PRECISION SIGMAX,DTL,UNORM,DSZ1,DSZ2,AUX,AUX1
!
      DOUBLE PRECISION EXLIM
      EXTERNAL         EXLIM
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA) THEN
        ALLOCATE(DSH(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSU(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSV(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSP(NS)    ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSM(NS)    ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSZ(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(CORR(NS)   ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DTLL(NS)   ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        GO TO 1002
1001    CONTINUE
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'FLUCIN : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &         'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'FLUCIN: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
1002    CONTINUE
        DEJA=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
      RA2 = SQRT(2.D0)
      RA3 = SQRT(1.5D0*G)
      ALP = 0.5D0/RA3
!
      IF(NORDRE.EQ.1) GOTO 12
!
!  2ND ORDER RECONSTRUCTION
!  ************************
!
      DO IS=1,NS
        DSP(IS)=0.D0
        DSM(IS)=0.D0
        DTLL(IS) =1.D6
      ENDDO
!
      DO NSG=1,NSEG
!
         J         = JMI(NSG)
!
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
!
         ZF1   =    ZF(NUBO1)
         ZF2   =    ZF(NUBO2)
         DSZ(1,NSG) = DSZ0(1,NSG)
         DSZ(2,NSG) = DSZ0(2,NSG)
!
         HI0=UA(1,NUBO1)
         HJ0=UA(1,NUBO2)
!
!   FOR AN EDGE BEING RECOVERED, ONE WILL REMAIN 1ST ORDER
!
         IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &  .OR. 2.*ABS(DSZ(1,NSG)).GE.HI0
     &  .OR. 2.*ABS(DSZ(1,NSG)).GE.HJ0
     &  .OR. 2.*ABS(DSZ(2,NSG)).GE.HI0
     &  .OR. 2.*ABS(DSZ(2,NSG)).GE.HJ0)  THEN
         DSH(1,NSG) =0.D0
         DSH(2,NSG) =0.D0
         DSU(1,NSG) =0.D0
         DSU(2,NSG) =0.D0
         DSV(1,NSG) =0.D0
         DSV(2,NSG) =0.D0
         DSZ(1,NSG) =0.D0
         DSZ(2,NSG) =0.D0
        ELSE
!
!     NORMALISED UNIT VNOCL, RNN LENGTH OF LIJ
!
         XNN       = VNOCL(1,NSG)
         YNN       = VNOCL(2,NSG)
         RNN       = VNOCL(3,NSG)
!
         AIX       = CMI(1,NSG)-X(NUBO1)
         AIY       = CMI(2,NSG)-Y(NUBO1)
         AJX       = CMI(1,NSG)-X(NUBO2)
         AJY       = CMI(2,NSG)-Y(NUBO2)
!
       DO IVAR=1,3
!
         GRADI(IVAR)  = AIX*DX(IVAR,NUBO1) + AIY*DY(IVAR,NUBO1)
!
         GRADJ(IVAR)  = AJX*DX(IVAR,NUBO2) + AJY*DY(IVAR,NUBO2)
!
         GRADIJ(IVAR)  = AIX*DJX(IVAR,J) + AIY*DJY(IVAR,J)
!
         GRADJI(IVAR)  = AJX*DJX(IVAR,J) + AJY*DJY(IVAR,J)
!
       ENDDO
!
! ROTATION OF THE GRADIENTS
!
!
         GRADI2    = GRADI(2)
         GRADI(2)  = XNN*GRADI2+YNN*GRADI(3)
         GRADI(3)  =-YNN*GRADI2+XNN*GRADI(3)
!
         GRADIJ2    = GRADIJ(2)
         GRADIJ(2)  = XNN*GRADIJ2+YNN*GRADIJ(3)
         GRADIJ(3)  =-YNN*GRADIJ2+XNN*GRADIJ(3)
!
         GRADJ2    = GRADJ(2)
         GRADJ(2)  = XNN*GRADJ2+YNN*GRADJ(3)
         GRADJ(3)  =-YNN*GRADJ2+XNN*GRADJ(3)
!
         GRADJI2    = GRADJI(2)
         GRADJI(2)  = XNN*GRADJI2+YNN*GRADJI(3)
         GRADJI(3)  =-YNN*GRADJI2+XNN*GRADJI(3)
!
!
!    EXTRAPOLATES THE GRADIENTS AND USES OF SLOPE LIMITERS
!
!
!   ONE REBUILDS H+Z, DSH = VARIATION OF H+Z
!
         ILIM=1
         BETA=1.D0
!
         DSH(1,NSG) = EXLIM(ILIM,BETA,GRADI(1),GRADIJ(1))
         DSH(2,NSG) = EXLIM(ILIM,BETA,GRADJ(1),GRADJI(1))
         IF(DSH(1,NSG).GE.0.D0) THEN
         DSP(NUBO1) = DSP(NUBO1) + AIRST(1,NSG)*DSH(1,NSG)
         ELSE
         DSM(NUBO1) = DSM(NUBO1) - AIRST(1,NSG)*DSH(1,NSG)
         ENDIF
         IF(DSH(2,NSG).GE.0.D0) THEN
         DSP(NUBO2) = DSP(NUBO2) + AIRST(2,NSG)*DSH(2,NSG)
         ELSE
         DSM(NUBO2) = DSM(NUBO2) - AIRST(2,NSG)*DSH(2,NSG)
         ENDIF
!
         ILIM=2
         BETA=0.3333D0
!
         DSU(1,NSG) = EXLIM(ILIM,BETA,GRADI(2),GRADIJ(2))
         DSU(2,NSG) = EXLIM(ILIM,BETA,GRADJ(2),GRADJI(2))
!
         DSV(1,NSG) = EXLIM(ILIM,BETA,GRADI(3),GRADIJ(3))
         DSV(2,NSG) = EXLIM(ILIM,BETA,GRADJ(3),GRADJI(3))
!
       ENDIF
       ENDDO
!
!  ONE CALCULATES THE CORRECTIONS TO ENSURE THE CONSERVATION OF H
!
      DO IS=1,NS
        CORR(IS) =  DSM(IS) - DSP(IS)
        AMDS =MAX(DSP(IS),DSM(IS))
        IF(AMDS.GT.0.D0) THEN
          CORR(IS) = CORR(IS)/AMDS
        ENDIF
      ENDDO
 12       CONTINUE
!
!     LOOP ON GLOBAL LIST OF EDGES
!    ******************************
!
      DO 500 NSG=1,NSEG
!
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
!
         ZF1   =    ZF(NUBO1)
         ZF2   =    ZF(NUBO2)
         DSZ1=0.D0
         DSZ2=0.D0
!
         XNN       = VNOCL(1,NSG)
         YNN       = VNOCL(2,NSG)
         RNN       = VNOCL(3,NSG)
!
         UAS11     = UA(1,NUBO1)
         UAS12     = UA(1,NUBO2)
         UAS21     = UA(2,NUBO1)
         UAS22     = UA(2,NUBO2)
         UAS31     = UA(3,NUBO1)
         UAS32     = UA(3,NUBO2)
!
         HI0=UAS11
         HJ0=UAS12
!
! ROTATION
!
         UAS210 = UAS21
         UAS21  = XNN*UAS210+YNN*UAS31
         UAS31  =-YNN*UAS210+XNN*UAS31
!
         UAS220 = UAS22
         UAS22  = XNN*UAS220+YNN*UAS32
         UAS32  =-YNN*UAS220+XNN*UAS32
!
         IF(NORDRE.EQ.1)  GOTO 1234
!
!    REBUILDING FOR 2ND ORDER
!    ***************************
!
!   FOR AN EDGE BEING RECOVERED, ONE REMAINS 1ST ORDER
!
         IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &  .OR. 2.*ABS(DSZ(1,NSG)).GE.HI0
     &  .OR. 2.*ABS(DSZ(1,NSG)).GE.HJ0
     &  .OR. 2.*ABS(DSZ(2,NSG)).GE.HI0
     &  .OR. 2.*ABS(DSZ(2,NSG)).GE.HJ0)  GOTO 1234
!
!
         UAS11     = UAS11  + DSH(1,NSG)  -  DSZ(1,NSG) +
     & MIN(0.D0,CORR(NUBO1))*MAX(0.D0,DSH(1,NSG))+
     & MAX(0.D0,CORR(NUBO1))*MAX(0.D0,-DSH(1,NSG))
!
         UAS21     = UAS21 + DSU(1,NSG)
         UAS31     = UAS31 + DSV(1,NSG)
         DSZ1 = DSZ(1,NSG)
         ZF1 = ZF1 + DSZ1
!
         UAS12     = UAS12  + DSH(2,NSG)  -  DSZ(2,NSG) +
     & MIN(0.D0,CORR(NUBO2))*MAX(0.D0,DSH(2,NSG))+
     & MAX(0.D0,CORR(NUBO2))*MAX(0.D0,-DSH(2,NSG))
!
         UAS22     = UAS22 + DSU(2,NSG)
         UAS32     = UAS32 + DSV(2,NSG)
         DSZ2 = DSZ(2,NSG)
         ZF2 = ZF2 + DSZ2
!
!
       IF(UAS11.LE.0.D0) THEN
         UAS11 =0.D0
         UAS21 =0.D0
         UAS31 =0.D0
       ENDIF
       IF(UAS12.LE.0.D0)  THEN
          UAS12 =0.D0
          UAS22 =0.D0
          UAS32 =0.D0
       ENDIF
!
!
!    LIMITATION OF THE TIME STEP
!    ***************************
!
!
            SIGMAX=MAX( 1.D-2, RA3* SQRT(UAS11) + ABS(UAS21) )
               DTL    = CFL*AIRST(1,NSG)/(RNN*SIGMAX)
               DTLL(NUBO1) = MIN (DTL,DTLL(NUBO1))
               DT          = MIN(DT, DTL)
!
            SIGMAX=MAX( 1.D-2, RA3* SQRT(UAS12) + ABS(UAS22) )
               DTL    = CFL*AIRST(2,NSG)/(RNN*SIGMAX)
               DTLL(NUBO2) = MIN (DTL,DTLL(NUBO2))
               DT          = MIN(DT, DTL)
!
1234   CONTINUE
!
!
         HI   = UAS11
         HC(1,NSG) = UAS11
!
         DZIJ = MAX(0.D0,ZF2-ZF1)
         HIJ  = MAX(0.D0, HI- DZIJ)
!
!
           IF(HIJ.LE.0.D0) THEN
             CIJ=0.D0
             FLU11=0.D0
             FLU21=0.D0
!
           ELSE
             CIJ  =SQRT(HIJ)
             EXT1 = MIN(RA3,MAX(-RA3,-UAS21/CIJ))
!
         A01  = ALP*(RA3-EXT1)
         A11  = ALP*(RA3**2-EXT1**2)/2.D0
         A21  = ALP*(RA3**3-EXT1**3)/3.D0
!
         FLU11= HIJ*(UAS21*A01+CIJ*A11)
         FLU21= UAS21*(FLU11+CIJ*HIJ*A11) +A21*HIJ*HIJ
!
        ENDIF
!
!
         HJ   = UAS12
         HC(2,NSG) = UAS12
!
         DZJI = MAX(0.D0,ZF1-ZF2)
         HJI  = MAX(0.D0, HJ- DZJI)
!
            IF(HJI.LE.0.D0) THEN
              CJI=0.D0
              FLU12=0.D0
              FLU22=0.D0
!
            ELSE
              CJI  =SQRT(HJI)
              EXT2 = MIN(RA3,MAX(-RA3,-UAS22/CJI))
!
              A02  = ALP*(RA3+EXT2)
              A12  = ALP*(EXT2**2-RA3**2)/2.D0
              A22  = ALP*(RA3**3+EXT2**3)/3.D0
!
              FLU12= HJI*(UAS22*A02+CJI*A12)
              FLU22= UAS22*(FLU12+CJI*HJI*A12) +A22*HJI*HJI
            ENDIF
!
!
         HGZI =0.5D0*RNN*(HIJ+HI)*(HIJ-HI)
         HGZJ =0.5D0*RNN*(HJI+HJ)*(HJI-HJ)
!
        IF(NORDRE.EQ.2) THEN
          HGZI = HGZI - 0.5D0*RNN*(HI0+HI)*DSZ1
          HGZJ = HGZJ - 0.5D0*RNN*(HJ0+HJ)*DSZ2
        ENDIF
!
        FLU11=(FLU11+FLU12)*RNN
        FLU21=(FLU21+FLU22)*RNN
!
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            FLUXTEMP%ADR(ITRAC)%P%R(NSG)=FLU11
          ENDDO
        ENDIF
!
        IF(FLU11.GE.0.D0) THEN
          FLU31 =  UAS31 * FLU11
        ELSE
          FLU31 =  UAS32 * FLU11
        ENDIF
!
! OPPOSITE ROTATION
!
         FLU210 = FLU21
         FLU21  = XNN*FLU210-YNN*FLU31
         FLU31  = YNN*FLU210+XNN*FLU31
!
!       TERM DUE TO THE BOTTOM GRADIENT
!
         HDXZ1  = G*XNN*HGZI
         HDYZ1  = G*YNN*HGZI
!
         HDXZ2  = G*XNN*HGZJ
         HDYZ2  = G*YNN*HGZJ
!
         CE(NUBO1,1) = CE(NUBO1,1) - FLU11
         CE(NUBO1,2) = CE(NUBO1,2) - FLU21 + HDXZ1
         CE(NUBO1,3) = CE(NUBO1,3) - FLU31 + HDYZ1
!
         CE(NUBO2,1) = CE(NUBO2,1) + FLU11
         CE(NUBO2,2) = CE(NUBO2,2) + FLU21 - HDXZ2
         CE(NUBO2,3) = CE(NUBO2,3) + FLU31 - HDYZ2
!
500   CONTINUE
!
      IF(NORDRE.EQ.2) THEN
!
!       LIMITATION OF THE TIME STEP FOR THE BOUNDARY NODES
!
        DO K=1,NPTFR
          IS=NBOR(K)
          VNX=XNEBOR(K+NPTFR)
          VNY=YNEBOR(K+NPTFR)
          VNL=SQRT(VNX**2+VNY**2)
          SIGMAX= SQRT(UA(1,IS))
          UNORM=SQRT(UA(2,IS)*UA(2,IS) + UA(3,IS)*UA(3,IS))
          SIGMAX=MAX( 1.D-2, RA3*SIGMAX +UNORM )
          DTL    = CFL*AIRS(IS)/(VNL*SIGMAX)
          AUX = DTL/DTLL(IS)
          AUX1=AUX/(1.D0+AUX)
          DT =MIN(DT, AUX1*DTLL(IS))
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
