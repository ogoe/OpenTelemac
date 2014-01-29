!                    *****************
                     SUBROUTINE FLUCIN
!                    *****************
!
     &(NS,NELEM,NSEG,NUBO,G,X,Y,CFL,DT,UA,ZF,VNOCL,CE,NORDRE,CMI,JMI,
     & DJX,DJY,DX,DY,BETA,DSZ0,AIRS,AIRST,HC,FLUXTEMP,NPTFR,NBOR,
     & XNEBOR,YNEBOR,NTRAC,ELTSEG,IFABOR,MESH)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/07/2013
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
!history  R.ATA
!+        21/07/2013
!+        V6P3
!+   Adaptation for new data structure of finite volumes
!+   clean and optimize 
!+   parallelism
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
      USE DECLARATIONS_TELEMAC2D , ONLY:DEBUG
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,NSEG,NPTFR,NORDRE,NTRAC,NELEM
      INTEGER, INTENT(IN) :: NBOR(*),NUBO(2,NSEG),JMI(*)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(*),YNEBOR(*),X(NS),Y(NS)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NS),VNOCL(3,NSEG),AIRS(*)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFL,UA(3,NS),AIRST(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: DSZ0(2,*),CMI(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA,DT,CE(NS,3),HC(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: DJX(3,*),DJY(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: DX(3,*),DY(3,*)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUXTEMP
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3) 
      TYPE(BIEF_MESH),INTENT(INOUT)   :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     AUTOMATIC EX ARRAYS!!!!!!
!
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: DSH(:,:),DSU(:,:)
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: DSV(:,:)
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: DSP(:),DSM(:),DSZ(:,:)
      DOUBLE PRECISION, ALLOCATABLE,SAVE :: CORR(:),DTLL(:)
! ra29/042013      DOUBLE PRECISION GRADI(3),GRADJ(3),GRADIJ(3),GRADJI(3)
      DOUBLE PRECISION,ALLOCATABLE,SAVE  :: GRADI(:,:),GRADJ(:,:)
      DOUBLE PRECISION,ALLOCATABLE,SAVE  :: GRADIJ(:,:),GRADJI(:,:)
!end ra
!
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
!-----------------------------------------------------------------------
!
      INTEGER NSG,NUBO1,NUBO2,J,IVAR,IS,K,ILIM,ERR,ITRAC,I,IEL
!
      DOUBLE PRECISION VNX,VNY,VNL,RA2,RA3,ALP,ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION UAS11,UAS12,UAS21,UAS22,UAS31,UAS32,AMDS
      DOUBLE PRECISION GRADI2,GRADIJ2,GRADJ2,GRADJI2,DEMI
      DOUBLE PRECISION AIX,AIY,AJX,AJY,HI,HI0,HIJ,CIJ,UAS210,UAS220
      DOUBLE PRECISION HJ,HJ0,HJI,CJI,DZIJ,DZJI
      DOUBLE PRECISION EXT1,EXT2,FLU11,FLU21,FLU31,FLU12,FLU22
      DOUBLE PRECISION FLU210,A01,A11,A21,A02,A12,A22
      DOUBLE PRECISION HGZI,HGZJ,HDXZ1,HDYZ1,HDXZ2,HDYZ2
      DOUBLE PRECISION SIGMAX,DTL,UNORM,DSZ1,DSZ2,AUX,AUX1
      DOUBLE PRECISION PROD_SCAL
      LOGICAL, ALLOCATABLE ::   YESNO(:)
!
      DOUBLE PRECISION EXLIM,P_DMIN
      EXTERNAL         EXLIM,P_DMIN
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
        ALLOCATE(GRADI(3,NSEG),GRADJ(3,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(GRADIJ(3,NSEG),GRADJI(3,NSEG),STAT=ERR)
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
      DEMI = 0.5D0
!
!-----------------------------------------------------------------------
!
      RA2 = SQRT(2.D0)
      RA3 = SQRT(1.5D0*G)
      ALP = 0.5D0/RA3
      ALLOCATE(YESNO(NSEG),STAT=ERR)
      IF(ERR.NE.0)THEN
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
        CALL PLANTE(1)
        STOP
      ENDIF
!
! INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
      IF(NORDRE.EQ.1) GOTO 12
!
!  2ND ORDER RECONSTRUCTION
!  ************************
!
!    INITIALIZATION    
      DTLL(:)=(/(1.E10,I=1,NS)/)
      DSP (:)=(/(0.D0,I=1,NS)/)
      DSM (:)=(/(0.D0,I=1,NS)/)
      DSH(1,:)=(/(0.D0,I=1,NSEG)/)
      DSH(2,:)=(/(0.D0,I=1,NSEG)/)
      DSU(1,:)=(/(0.D0,I=1,NSEG)/)
      DSU(2,:)=(/(0.D0,I=1,NSEG)/)
      DSV(1,:)=(/(0.D0,I=1,NSEG)/)
      DSV(2,:)=(/(0.D0,I=1,NSEG)/)
      DSZ(1,:)=(/(0.D0,I=1,NSEG)/)
      DSZ(2,:)=(/(0.D0,I=1,NSEG)/)
!GIVES ERROR WITH INTEL COMPILER IF WRITTEN LIKE THIS
!       CALL OV( 'X=C     ' ,DSH(1,1:NSEG),DSM ,DSM ,0.D0,NSEG)
!
!    INITIALIZATION  OF GRADIENTS 
      GRADI(1,:)=(/(0.D0,I=1,NSEG)/)
      GRADI(2,:)=(/(0.D0,I=1,NSEG)/)
      GRADI(3,:)=(/(0.D0,I=1,NSEG)/)
!     
      GRADJ(1,:)=(/(0.D0,I=1,NSEG)/)
      GRADJ(2,:)=(/(0.D0,I=1,NSEG)/)
      GRADJ(3,:)=(/(0.D0,I=1,NSEG)/)
!     
      GRADIJ(1,:)=(/(0.D0,I=1,NSEG)/)
      GRADIJ(2,:)=(/(0.D0,I=1,NSEG)/)
      GRADIJ(3,:)=(/(0.D0,I=1,NSEG)/)
!     
      GRADJI(1,:)=(/(0.D0,I=1,NSEG)/)
      GRADJI(2,:)=(/(0.D0,I=1,NSEG)/)
      GRADJI(3,:)=(/(0.D0,I=1,NSEG)/)
!
      DO IEL=1, NELEM 
       DO I = 1,3
        IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
         NSG = ELTSEG(IEL,I)
!     RECUPERATE JMI
         J   = JMI(NSG) ! THIS THE TRIANGLE IN WHICH IS LOCATED CMI
         IF(NCSIZE.GT.1.AND.J.EQ.0)CYCLE  ! THAT MEANS CMI IS NOT LOCATED IN TRIANGLE J
!
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
         NUBO1 = NUBO(1,NSG)
         NUBO2 = NUBO(2,NSG)
         PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &               (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
         IF(PROD_SCAL.LT.0.D0)THEN
           NUBO1 = NUBO(2,NSG)
           NUBO2 = NUBO(1,NSG)
         ENDIF
!
         ZF1        = ZF(NUBO1)
         ZF2        = ZF(NUBO2)
         IF(PROD_SCAL.LT.0.D0)THEN
           DSZ(1,NSG) = DSZ0(2,NSG)
           DSZ(2,NSG) = DSZ0(1,NSG)
         ELSE
           DSZ(1,NSG) = DSZ0(1,NSG)
           DSZ(2,NSG) = DSZ0(2,NSG)
         ENDIF
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
!ra02/05/2013 FOR OPTIMIZATION
          CYCLE
!           DSH(1,NSG) =0.D0
!           DSH(2,NSG) =0.D0
!           DSU(1,NSG) =0.D0
!           DSU(2,NSG) =0.D0
!           DSV(1,NSG) =0.D0
!           DSV(2,NSG) =0.D0
!           DSZ(1,NSG) =0.D0
!           DSZ(2,NSG) =0.D0
         ELSE
!
!     NORMALIZED UNIT NORMAL (VNOCL), RNN LENGTH OF LIJ
!
          XNN = VNOCL(1,NSG)
          YNN = VNOCL(2,NSG)
          RNN = VNOCL(3,NSG)
!
          AIX = CMI(1,NSG)-X(NUBO1) ! THESE ARE COORDINATES OF
          AIY = CMI(2,NSG)-Y(NUBO1) !  VECTOR PM (EQ 5.1)
          AJX = CMI(1,NSG)-X(NUBO2) ! P: NUBO1 OR NUBO2
          AJY = CMI(2,NSG)-Y(NUBO2) ! M: CMI(NSG)
!
          DO IVAR=1,3
            GRADI(IVAR,NSG) = AIX*DX(IVAR,NUBO1)+AIY*DY(IVAR,NUBO1)!NODE GRADIENT (PM.GRADZ)
            GRADJ(IVAR,NSG) = AJX*DX(IVAR,NUBO2)+AJY*DY(IVAR,NUBO2)!eq 5.1 of audusse paper)
!
            GRADIJ(IVAR,NSG) = AIX*DJX(IVAR,J) + AIY*DJY(IVAR,J)
            GRADJI(IVAR,NSG) = AJX*DJX(IVAR,J) + AJY*DJY(IVAR,J)
          ENDDO
! ROTATION OF THE GRADIENTS
!
          GRADI2       = GRADI(2,NSG)
          GRADI(2,NSG) = XNN*GRADI2+YNN*GRADI(3,NSG)
          GRADI(3,NSG) =-YNN*GRADI2+XNN*GRADI(3,NSG)
!
          GRADIJ2      = GRADIJ(2,NSG)
          GRADIJ(2,NSG)= XNN*GRADIJ2+YNN*GRADIJ(3,NSG)
          GRADIJ(3,NSG)=-YNN*GRADIJ2+XNN*GRADIJ(3,NSG)
!
          GRADJ2       = GRADJ(2,NSG)
          GRADJ(2,NSG) = XNN*GRADJ2+YNN*GRADJ(3,NSG)
          GRADJ(3,NSG) =-YNN*GRADJ2+XNN*GRADJ(3,NSG)
!
          GRADJI2      = GRADJI(2,NSG)
          GRADJI(2,NSG)= XNN*GRADJI2+YNN*GRADJI(3,NSG)
          GRADJI(3,NSG)=-YNN*GRADJI2+XNN*GRADJI(3,NSG)
!
         ENDIF
         YESNO(NSG)=.TRUE. 
        ENDIF 
       ENDDO
      ENDDO
      IF(NCSIZE.GT.1)THEN      ! NPON,NPLAN,ICOM,IAN , HERE ICOM=1 VALUE WITH MAX | |
        CALL PARCOM2_SEG(GRADI(1,1:NSEG),
     &                   GRADI(2,1:NSEG),
     &                   GRADI(3,1:NSEG),
     &              NSEG,1,2,3,MESH,1,11)
        CALL PARCOM2_SEG(GRADJ(1,1:NSEG),
     &                   GRADJ(2,1:NSEG),
     &                   GRADJ(3,1:NSEG),
     &              NSEG,1,2,3,MESH,1,11)
        CALL PARCOM2_SEG(GRADIJ(1,1:NSEG),
     &                   GRADIJ(2,1:NSEG),
     &                   GRADIJ(3,1:NSEG),
     &              NSEG,1,2,3,MESH,1,11)
        CALL PARCOM2_SEG(GRADJI(1,1:NSEG),
     &                   GRADJI(2,1:NSEG),
     &                   GRADJI(3,1:NSEG),
     &              NSEG,1,2,3,MESH,1,11)
      ENDIF
!
!    EXTRAPOLATES THE GRADIENTS AND USES OF SLOPE LIMITERS
!
!
! INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
      DO IEL=1, NELEM 
       DO I = 1,3
        IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
         NSG = ELTSEG(IEL,I)
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
         NUBO1 = NUBO(1,NSG)
         NUBO2 = NUBO(2,NSG)
         PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &               (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
         IF(PROD_SCAL.LT.0.D0)THEN
           NUBO1 = NUBO(2,NSG)
           NUBO2 = NUBO(1,NSG)
         ENDIF
!
         ZF1        = ZF(NUBO1)
         ZF2        = ZF(NUBO2)
         IF(PROD_SCAL.LT.0.D0)THEN
           DSZ(1,NSG) = DSZ0(2,NSG)
           DSZ(2,NSG) = DSZ0(1,NSG)
         ELSE
           DSZ(1,NSG) = DSZ0(1,NSG)
           DSZ(2,NSG) = DSZ0(2,NSG)
         ENDIF
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
!ra02/05/2013 FOR APTIMIZATION
          CYCLE 
         ELSE
!
!   ONE REBUILDS H+Z, DSH = VARIATION OF H+Z
!
          ILIM=1
          BETA=1.D0
!
          DSH(1,NSG) = EXLIM(ILIM,BETA,GRADI(1,NSG),GRADIJ(1,NSG))
          DSH(2,NSG) = EXLIM(ILIM,BETA,GRADJ(1,NSG),GRADJI(1,NSG))
          !   FOR PARALLELILSM
          IF(NCSIZE.GT.1.AND.IFABOR(IEL,I).EQ.-2)THEN ! THIS IS AN INTERFACE EDGE
           IF(DSH(1,NSG).GE.0.D0) THEN
            DSP(NUBO1) = DSP(NUBO1)+DEMI*AIRST(1,NSG)*DSH(1,NSG) ! WE CONSIDER ONLY
           ELSE                                                  ! 0.5 AIRST 
            DSM(NUBO1) = DSM(NUBO1)-DEMI*AIRST(1,NSG)*DSH(1,NSG) ! PARCOM2 WILL ADD 
           ENDIF                                                 ! CONTRIBUTIONS
           IF(DSH(2,NSG).GE.0.D0) THEN
            DSP(NUBO2) = DSP(NUBO2)+DEMI*AIRST(2,NSG)*DSH(2,NSG)
           ELSE
            DSM(NUBO2) = DSM(NUBO2)-DEMI*AIRST(2,NSG)*DSH(2,NSG)
           ENDIF 
          ELSE ! NO PARALLELILSM OR NO INTERFACE EDGE
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
          ENDIF
!
          ILIM=2
          BETA=0.3333D0 ! THESE ARE CHOICES OF INRIA 1/3 FOR 
                        ! VELOCITIES AND 1 FOR H
!
          DSU(1,NSG) = EXLIM(ILIM,BETA,GRADI(2,NSG),GRADIJ(2,NSG))
          DSU(2,NSG) = EXLIM(ILIM,BETA,GRADJ(2,NSG),GRADJI(2,NSG))
!
          DSV(1,NSG) = EXLIM(ILIM,BETA,GRADI(3,NSG),GRADIJ(3,NSG))
          DSV(2,NSG) = EXLIM(ILIM,BETA,GRADJ(3,NSG),GRADJI(3,NSG))
!
         ENDIF
         YESNO(NSG)=.TRUE. 
        ENDIF 
       ENDDO
      ENDDO
      !  FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN     
        CALL PARCOM2(DSP,DSM,DSM,NS,1,2,2,MESH)
      ENDIF
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
! IF ORDER 2 REINITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
! FIRST ORDER
 12       CONTINUE
!
!     LOOP ON GLOBAL LIST OF EDGES
!    ******************************
!
       DO IEL=1, NELEM 
        DO I = 1,3
         IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
          NSG = ELTSEG(IEL,I)
!
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
          NUBO1 = NUBO(1,NSG)
          NUBO2 = NUBO(2,NSG)
          PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
          IF(PROD_SCAL.LT.0.D0)THEN
           NUBO1 = NUBO(2,NSG)
           NUBO2 = NUBO(1,NSG)
          ENDIF
!
          ZF1  = ZF(NUBO1)
          ZF2  = ZF(NUBO2)
          DSZ1 = 0.D0
          DSZ2 = 0.D0
!
          XNN  = VNOCL(1,NSG)
          YNN  = VNOCL(2,NSG)
          RNN  = VNOCL(3,NSG)
!
          UAS11 = UA(1,NUBO1)
          UAS12 = UA(1,NUBO2)
          UAS21 = UA(2,NUBO1)
          UAS22 = UA(2,NUBO2)
          UAS31 = UA(3,NUBO1)
          UAS32 = UA(3,NUBO2)
!
          HI0 = UAS11
          HJ0 = UAS12
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
     &   .OR. 2.*ABS(DSZ(1,NSG)).GE.HI0
     &   .OR. 2.*ABS(DSZ(1,NSG)).GE.HJ0
     &   .OR. 2.*ABS(DSZ(2,NSG)).GE.HI0
     &   .OR. 2.*ABS(DSZ(2,NSG)).GE.HJ0)  GOTO 1234
!
!
          UAS11  = UAS11  + DSH(1,NSG)  -  DSZ(1,NSG) +
     &      MIN(0.D0,CORR(NUBO1))*MAX(0.D0,DSH(1,NSG))+
     &      MAX(0.D0,CORR(NUBO1))*MAX(0.D0,-DSH(1,NSG))
!
          UAS21  = UAS21 + DSU(1,NSG)
          UAS31  = UAS31 + DSV(1,NSG)
          DSZ1   = DSZ(1,NSG)
          ZF1    = ZF1 + DSZ1
!
          UAS12  = UAS12  + DSH(2,NSG)  -  DSZ(2,NSG) +
     &      MIN(0.D0,CORR(NUBO2))*MAX(0.D0,DSH(2,NSG))+
     &      MAX(0.D0,CORR(NUBO2))*MAX(0.D0,-DSH(2,NSG))
!
          UAS22  = UAS22 + DSU(2,NSG)
          UAS32  = UAS32 + DSV(2,NSG)
          DSZ2   = DSZ(2,NSG)
          ZF2    = ZF2 + DSZ2
!
!
          IF(UAS11.LE.0.D0) THEN
            UAS11 = 0.D0
            UAS21 = 0.D0
            UAS31 = 0.D0
          ENDIF
          IF(UAS12.LE.0.D0)  THEN
            UAS12 = 0.D0
            UAS22 = 0.D0
            UAS32 = 0.D0
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
! PARALLEL: TAKE MIN DT OF ALL SUBDOMAINS
!          IF(NCSIZE.GT.1)DT = P_DMIN(DT) !WILL BE PLACED AT THE END (SEE BELOW) 
!
1234   CONTINUE
!
!
!   MAIN FLUX COMPUTATAION 
!
          HI        = UAS11
          HC(1,NSG) = UAS11
!
          DZIJ = MAX(0.D0,ZF2-ZF1)
          HIJ  = MAX(0.D0, HI- DZIJ)
!
!
          IF(HIJ.LE.0.D0) THEN
            CIJ  = 0.D0
            FLU11= 0.D0
            FLU21= 0.D0
          ELSE
            CIJ  = SQRT(HIJ)
            EXT1 = MIN(RA3,MAX(-RA3,-UAS21/CIJ))
!
            A01  = ALP*(RA3-EXT1)
            A11  = ALP*(RA3**2-EXT1**2)/2.D0
            A21  = ALP*(RA3**3-EXT1**3)/3.D0
!
            FLU11= HIJ*(UAS21*A01+CIJ*A11)
            FLU21= UAS21*(FLU11+CIJ*HIJ*A11) +A21*HIJ*HIJ
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
            CJI   = 0.D0
            FLU12 = 0.D0
            FLU22 = 0.D0
          ELSE
            CJI  = SQRT(HJI)
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
          HGZI = 0.5D0*RNN*(HIJ+HI)*(HIJ-HI)
          HGZJ = 0.5D0*RNN*(HJI+HJ)*(HJI-HJ)
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
!FOR PARALLELISM
!
         IF(NCSIZE.GT.1)THEN
          IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
            FLU11= DEMI*FLU11
            FLU21= DEMI*FLU21
            FLU31= DEMI*FLU31
! 
            HDXZ1 = DEMI*HDXZ1
            HDYZ1 = DEMI*HDYZ1
            HDXZ2 = DEMI*HDXZ2
            HDYZ2 = DEMI*HDYZ2
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                FLUXTEMP%ADR(ITRAC)%P%R(NSG)=DEMI*FLU11
              ENDDO
            ENDIF
          ENDIF
         ENDIF
!
!***********************************************************
!       FLUX INCREMENT
!***********************************************************
!
          CE(NUBO1,1) = CE(NUBO1,1) - FLU11
          CE(NUBO1,2) = CE(NUBO1,2) - FLU21 + HDXZ1
          CE(NUBO1,3) = CE(NUBO1,3) - FLU31 + HDYZ1
!
          CE(NUBO2,1) = CE(NUBO2,1) + FLU11
          CE(NUBO2,2) = CE(NUBO2,2) + FLU21 - HDXZ2
          CE(NUBO2,3) = CE(NUBO2,3) + FLU31 - HDYZ2
!
          YESNO(NSG)=.TRUE. 
         ENDIF
        ENDDO
      ENDDO
!
!       LIMITATION OF THE TIME STEP FOR THE BOUNDARY NODES
!
      IF(NORDRE.EQ.2) THEN
        IF(NPTFR.GT.0)THEN  ! USEFUL FOR PARALLEL CASE
          DO K=1,NPTFR
            IS = NBOR(K)
            VNX= XNEBOR(K+NPTFR)
            VNY= YNEBOR(K+NPTFR)
            VNL= SQRT(VNX**2+VNY**2)
            SIGMAX= SQRT(UA(1,IS))
            UNORM=SQRT(UA(2,IS)*UA(2,IS) + UA(3,IS)*UA(3,IS))
            SIGMAX=MAX( 1.D-2, RA3*SIGMAX +UNORM )
            DTL   = CFL*AIRS(IS)/(VNL*SIGMAX)
            AUX   = DTL/DTLL(IS)
            AUX1  =AUX/(1.D0+AUX)
            DT    =MIN(DT, AUX1*DTLL(IS))
          ENDDO
        ENDIF
!       FOR PARALLELISME
        IF(NCSIZE.GT.1) DT=P_DMIN(DT)
      ENDIF
! 
      DEALLOCATE(YESNO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
