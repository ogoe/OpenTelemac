!                    ******************
                     SUBROUTINE FLUCINT
!                    ******************
!
     &(NS,NSEG,DIMT,NUBO,G,X,Y,UA,TN,ZF,VNOCL,CE,
     & NORDRE,CMI,JMI,DJX,DJY,DX,DY,DJXT,DJYT,DXT,DYT,EPSWL)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES EDGE-WISE ADVECTIVE FLUXES USING THE
!+                KINETIC BOLTZMANN FLUX.
!
!history  INRIA
!+
!+        V5P3
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
!| CE             |<->| FLUX INCREMENTS AT INTERNAL INTERFACES
!| CMI            |-->| COORDINATES OF MIDDLE PONTS OF EDGES
!| DIMT           |-->| DIMENSION OF TN
!| DJX            |-->| GRADIENTS PER TRIANGLE
!| DJXT           |-->| WORKING TABLES FOR TRACER
!| DJY            |-->| GRADIENTS PER TRIANGLE
!| DJYT           |---| WORKING TABLES FOR TRACER
!| DX             |-->| GRADIENTS PER NODE
!| DXT            |-->| WORKING TABLES FOR TRACER
!| DY             |-->| GRADIENTS PER NODE
!| DYT            |-->| WORKING TABLES FOR TRACER
!| EPSWL          |-->| THRESHOLD DEPTH
!| G              |-->| GRAVITY
!| JMI            |-->| NUMBER OF THE TRIANGLE IN WHICH IS LOCATED
!|                |   | THE MIDDLE POINT OF THE INTERFACE
!| NORDRE         |-->| ORDER OF THE SCHEME
!| NS             |-->| TOTAL NUMBER OF NODES IN THE MESH
!| NSEG           |-->| TOTAL NUMBER OF SGMENTS IN THE MESH
!| NUBO           |-->| GLOBAL NUMBERS OF THE NODES FORMING THE EDGE
!| TN             |-->| CURRENT TIME
!| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!| VNOCL          |-->| NORMAL VECTOR TO THE INTERFACE
!|                |   | (2 FIRST COMPONENTS) AND
!|                |   | LENGTH OF THE SEGMENT (3RD COMPONENT)
!| X              |-->| X COORDINATES OF THE NODES
!| Y              |-->| Y COORDINATES OF THE NODES
!| ZF             |-->| BATHYMETRY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,NSEG,DIMT,NORDRE
      INTEGER, INTENT(IN) :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN) :: G,X(NS),Y(NS),VNOCL(3,NSEG),ZF(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3)
      DOUBLE PRECISION, INTENT(IN)    :: UA(3,NS),TN(DIMT),CMI(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: JMI(*),EPSWL
      DOUBLE PRECISION, INTENT(IN)    :: DJX(3,*),DJY(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: DX(3,*),DY(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: DJXT(*),DJYT(*),DXT(*),DYT(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSG,NUBO1,NUBO2,J,IVAR
!
      DOUBLE PRECISION E2,RA2,RA3,ALP,ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION UAS11,UAS12,UAS21,UAS22,UAS31,UAS32
      DOUBLE PRECISION GRADI(4),GRADJ(4),GRADIJ(4),GRADJI(4),GRIJ,GRJI
      DOUBLE PRECISION GRI,GRJ,AUX1,AUX2,GRI2,GRIJ2,GRJ2,GRJI2
      DOUBLE PRECISION AIX,AIY,AJX,AJY,HI,HI0,HIJ,CIJ,ETAI,UAS110,UAS120
      DOUBLE PRECISION HJ,HJ0,HJI,CJI,ETAJ,EXT1,EXT2,FLU11,FLU12,FLU41
      DOUBLE PRECISION A01,A11,A02,A12,UAS41,UAS42
      DOUBLE PRECISION UAS410,UAS420,BETA,BETA1
!
!-----------------------------------------------------------------------
!
      RA2  = SQRT(2.D0)
      RA3  = SQRT(1.5D0*G)
      ALP  = 0.5D0/RA3
!
      BETA = 0.333D0
      BETA1 = 1.D0+BETA
      E2    = 1.E-6
!
!     LOOP ON GLOBAL LIST OF EDGES
!
      DO NSG=1,NSEG
        J         = INT(JMI(NSG))
!
        NUBO1     = NUBO(1,NSG)
        NUBO2     = NUBO(2,NSG)
!
        ZF1   =    ZF(NUBO1)
        ZF2   =    ZF(NUBO2)
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
        UAS41     = TN(NUBO1)
        UAS42     = TN(NUBO2)
!
        HI0=UAS11*UAS11
        HJ0=UAS12*UAS12
!
! ROTATION
!
        UAS21  = XNN*UAS21+YNN*UAS31
!
        UAS22  = XNN*UAS22+YNN*UAS32
!
!
        IF(NORDRE.EQ.1)  GOTO 1234
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
        GRADI(4)  = AIX*DXT(NUBO1) + AIY*DYT(NUBO1)
!
        GRADJ(4)  = AJX*DXT(NUBO2) + AJY*DYT(NUBO2)
!
        GRADIJ(4)  = AIX*DJXT(J) + AIY*DJYT(J)
!
        GRADJI(4)  = AJX*DJXT(J) + AJY*DJYT(J)
!
! GRADIENTS ROTATION
!
!
        GRADI(2)  = XNN*GRADI(2)+YNN*GRADI(3)
!
        GRADIJ(2)  = XNN*GRADIJ(2)+YNN*GRADIJ(3)
!
        GRADJ(2)  = XNN*GRADJ(2)+YNN*GRADJ(3)
!
        GRADJI(2)  = XNN*GRADJI(2)+YNN*GRADJI(3)
!
        DO IVAR=1,4
          IF(IVAR.NE.3) THEN
!
            GRIJ = GRADIJ(IVAR)
            GRJI = GRADJI(IVAR)
!
            GRI = BETA1*GRADI(IVAR) - BETA*GRIJ
            GRJ = BETA1*GRADJ(IVAR) - BETA*GRJI
!
            AUX1 = 0.5*(1.0+  SIGN(1.0D0, GRI*GRIJ))
            AUX2 = 0.5*(1.0 + SIGN(1.0D0, GRJ*GRJI))
!
!    VAN ALBADA
!
            GRI2  = GRI*GRI    + E2
            GRIJ2 = GRIJ*GRIJ  + E2
            GRJ2  = GRJ*GRJ    + E2
            GRJI2 = GRJI*GRJI  + E2
!
            GRADI(IVAR)  = AUX1*
     &         (GRI2  *GRIJ  + GRIJ2  *GRI )/(GRI2 + GRIJ2)
!
            GRADJ(IVAR)  = AUX2*
     &         (GRJ2  *GRJI + GRJI2 *GRJ )/(GRJ2 + GRJI2)
!
          ENDIF
        ENDDO
!
!
        UAS110 =   UAS11
        UAS11 = MIN(MAX(UAS110/RA2,UAS11 + GRADI(1)),RA2*UAS110)
        UAS21     = UAS21 + GRADI(2)
!
        UAS120 =   UAS12
        UAS12 = MIN(MAX(UAS120/RA2,UAS12 + GRADJ(1)),RA2*UAS120)
        UAS22     = UAS22 + GRADJ(2)
!
        IF(UAS11.LE.0.D0) THEN
          UAS11 =0.D0
          UAS21 =0.D0
        ENDIF
        IF(UAS12.LE.0.D0)  THEN
           UAS12 =0.D0
           UAS22 =0.D0
        ENDIF
!
!
1234    CONTINUE
!
        HI   = UAS11*UAS11
!
!   ETAI = FREE SURFACE ELEVATION
!   IF HI < EPSWL KEEP FLAT BED
!
        ETAI = HI0+ZF1
        IF(HI0.LE.EPSWL) ETAI = MIN(ETAI,HJ0+ZF2)
!
!       HIJ= ETAI-MIN(ETAI,MAX(ZF1,ZF2))
!
        IF(ZF1.GE.ZF2) THEN
          HIJ=HI0
        ELSE
          HIJ=MAX(0.D0,ETAI-ZF2)
        ENDIF
!
        IF(HI.LE.0.D0) THEN
          CIJ=0.D0
          FLU11=0.D0
        ELSE
!
          IF(HIJ.LE.0.D0) THEN
            CIJ=0.D0
!
            IF(UAS21.GE.0.D0) THEN
              EXT1=-RA3
            ELSE
              EXT1=RA3
            ENDIF
!
          ELSE
            CIJ  =HIJ*SQRT(HIJ)/HI
            EXT1 = MIN(RA3,MAX(-RA3,-UAS21/CIJ))
          ENDIF
!
        A01  = ALP*(RA3-EXT1)
        A11  = ALP*(RA3**2-EXT1**2)/2.D0
!
        FLU11= HI*(UAS21*A01+CIJ*A11)
!
        ENDIF
!
!
        HJ   = UAS12 *UAS12
!
        ETAJ = HJ0+ZF2
        IF(HJ0.LE.EPSWL) ETAJ = MIN(ETAJ,HI0+ZF1)
!
!       HJI= ETAJ-MIN(ETAJ,MAX(ZF1,ZF2))
        IF(ZF2.GE.ZF1) THEN
          HJI=HJ0
        ELSE
          HJI=MAX(0.D0,ETAJ-ZF1)
        ENDIF
!
        IF(HJ.LE.0.D0) THEN
          CJI=0.D0
          FLU12=0.D0
        ELSE
!
          IF(HJI.LE.0.D0) THEN
            CJI=0.D0
!
            IF(UAS22.GE.0.D0) THEN
              EXT2=-RA3
            ELSE
              EXT2=RA3
            ENDIF
!
          ELSE
            CJI  =HJI*SQRT(HJI)/HJ
            EXT2 = MIN(RA3,MAX(-RA3,-UAS22/CJI))
          ENDIF
!
          A02  = ALP*(RA3+EXT2)
          A12  = ALP*(EXT2**2-RA3**2)/2.D0
!
          FLU12= HJ*(UAS22*A02+CJI*A12)
        ENDIF
!
!
        FLU11=(FLU11 + FLU12)*RNN
!
        IF(FLU11.GE.0.D0) THEN
          IF(NORDRE.GE.2) THEN
            UAS410 = UAS41
            UAS41 = MIN(MAX(0.5D0*UAS410,UAS41 + GRADI(4)),2.D0*UAS410)
          ENDIF
          FLU41 =  UAS41 * FLU11
        ELSE
          IF(NORDRE.GE.2) THEN
            UAS420 = UAS42
            UAS42 = MIN(MAX(0.5D0*UAS420,UAS42 + GRADJ(4)),2.D0*UAS420)
          ENDIF
          FLU41 =  UAS42 * FLU11
        ENDIF
!
        CE(NUBO1,1) = CE(NUBO1,1) - FLU41
        CE(NUBO2,1) = CE(NUBO2,1) + FLU41
!
      ENDDO ! NSG
!
!-----------------------------------------------------------------------
!
      RETURN
      END
