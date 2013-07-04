!                       **************
                        SUBROUTINE CDL
!                       **************
!
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,G,HBOR,
     & UBOR,VBOR,UA,CE,FLUENT,FLUSORT,FLBOR,
     & DTHAUT,DT,CFL,FLUHBTEMP,NTRAC)
!
!***********************************************************************
! TELEMAC 2D VERSION 6.1                                          INRIA
!***********************************************************************
!
!brief  COMPUTATION OF THE CONVECTIVE FLUXES AT BOUNDARIES
!
!    UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!
!history  INRIA 
!+
!+        V5P8
!+
!
!history  R. ATA (EDF-LNHE) BALANCE OF WATER
!+        15/03/2010
!+        V6P1
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|  NS            |-->|  TOTAL NUMNER OF NODES
!|  NPTFR         |-->|  TOTAL NUMBER OF BOUNDARY NODES
!|  NBOR          |-->|  GLOBAL NUMBERS OF BOUNDARY POINTS
!|  LIMPRO        |-->|  TYPES OF BOUNDARY CONDITION
!|  XNEBOR        |-->|  UNIT OUTWARD NORMAL COMPONENT AT BOUNDARY POINTS
!|  YNEBOR        |-->|  UNIT OUTWARD NORMAL COMPONENT AT BOUNDARY POINTS
!|  KDIR          |-->|  CONVENTION FOR DIRICHLET POINTS
!|  KNEU          |-->|  CONVENTION FOR NEUMANN POINTS
!|  G             |-->|  GRAVITY CONSTANT
!|  HBOR          |-->|  IMPOSED VALUES FOR H
!|  UBOR          |-->|  IMPOSED VALUES FOR U
!|  VBOR          |-->|  IMPOSED VALUES FOR V
!|  UA            |-->|  UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!|  CE            |<->|  FLUX 
!|  FLUENT        |<--|  ENTERING MASS FLUX
!|  FLUSORT       |<--|  EXITING MASS FLUX
!|  DTHAUT        |-->|  CHARACTERISTIC LENGTH (DX) FOR CFL
!|  DT            |<->|  TIME STEP
!|  CFL           |-->|  CFL NUMBER
!|  FLUHBTEMP     |<--|  BOUNDARY FLUX FOR THE TRACER
!|  TRAC          |-->|  LOGICAL: TO INDICATE THE PRESENCE OF A TRACER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF 
      IMPLICIT NONE
!
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NS,NPTFR,KDIR,KNEU,NTRAC
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),UA(3,NS),DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFL
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUHBTEMP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NIT,ITRAC      
!
      DOUBLE PRECISION RA3,RA32,RA33, ALP,ALP2,ALP3,SG,SQ2      
      DOUBLE PRECISION VNX,VNY,VNX1,VNY1,VNL,H,U,V,RUN
      DOUBLE PRECISION FLUH,FLUU,FLUV,AUX,FLUTMP,RH,HRH,UNN,VNN
      DOUBLE PRECISION FHPLUS,FUPLUS,FHMOINS,FUMOINS 
      DOUBLE PRECISION A,A1,A2,A3,ALPHA0,ALPHA1,ALPHA2,C,VP1,VP2 ,VP3
      DOUBLE PRECISION HG ,RHG,HRHG,UG,VG,DEST,RVG,CA1,AM
      DOUBLE PRECISION UIN,VIN,HUIN,HVIN,SIGMAX,DTL,UNORM 
!
      SQ2   = SQRT(2.D0)
      SG    = SQRT(G)
      RA3   = SQRT(1.5D0*G)
      RA32  = RA3**2
      RA33  = RA3*RA32
      ALP   = 0.5D0/RA3
      ALP2  = 0.5D0 *ALP
      ALP3  = ALP/3.D0
!
      FLUENT=0.D0
      FLUSORT=0.D0
!
      DO K=1,NPTFR
       IS=NBOR(K)
       VNX1=XNEBOR(K)
       VNY1=YNEBOR(K)
       VNX=XNEBOR(K+NPTFR)
       VNY=YNEBOR(K+NPTFR)
       VNL=SQRT(VNX**2+VNY**2)
!
       H   = UA(1,IS)
       RH  = SQRT(H)
       U   = UA(2,IS)
       V   = UA(3,IS)
!
!        SOLID WALLS
!        **************

!     PERFECT SLIPPING CONDITION 
!     **************************
!
       IF(LIMPRO(K,1).EQ.KNEU) THEN
!
         AUX=0.5D0*G*H**2
         FLUH = 0.D0
         FLUU = AUX*VNX
         FLUV = AUX*VNY
       ELSE
!
!       LIQUID BOUNDARY
!       *******************
!
!    CALCULATION OF F+(H,U,V)
!
       HRH = RH * H
!
       IF(H.LE.0.D0) THEN
         U=0.D0
         V=0.D0
         UNN=0.D0
         VNN=0.D0
         FHPLUS = 0.D0
         FUPLUS = 0.D0
       ELSE
         UNN= +VNX1*U+VNY1*V
         VNN= -VNY1*U+VNX1*V
!
         A=MIN(RA3,MAX(-RA3,-UNN/RH))
         A2 =A * A
         A3 =A2 * A
         ALPHA0=ALP*(RA3-A)
         ALPHA1=ALP2*(RA32-A2)
         ALPHA2=ALP3*(RA33-A3)
!
         FHPLUS = H*UNN*ALPHA0 + HRH*ALPHA1
         FUPLUS = UNN*(FHPLUS+HRH*ALPHA1) + H*H*ALPHA2
       ENDIF
!
!
!    CALCULATION OF FICTIVE STATE (HG,UG,VG)
!
!
!    H GIVEN 
!    ########
!
        IF(LIMPRO(K,1).EQ.KDIR) THEN
!
         C   = SG*RH
         VP1 = UNN
         VP2 = VP1  + C
         VP3 = VP1  - C
!
         HG     =HBOR(K)
         RHG    =SQRT (HG)
         HRHG   =RHG*HG
!
        IF (VP2*VP3.LE.0.D0.OR. VP1.LE.0.D0) THEN
!
         IF(HG.EQ.0.D0) THEN
           UG=0.D0
           VG=0.D0
           FHMOINS = 0.D0
           FUMOINS = 0.D0
           SIGMAX=1.D-2
         ELSE
!
!   FLUVIAL REGIME
!   --------------
!
          IF (VP2*VP3.LE.0.D0) THEN
!
           UG=UNN+2.D0*SG*(RH-RHG)
           VG=VNN
!
!   TORRENTIAL REGIME 
!   -----------------
!
          ELSE
!
!  IMPOSED FLUX
!  -----------
          IF(LIMPRO(K,2).EQ.KDIR) THEN
!
            UIN = UBOR(K)
            VIN = VBOR(K)
            HUIN = H*UIN
            HVIN = H*VIN
!
            DEST=HUIN*VNX1+HVIN*VNY1
            RVG =-HUIN*VNY1+HVIN*VNX1
!
            A1 = DEST-FHPLUS
            CA1= SQ2*A1/(SG*HG*RHG)
            CALL ZEROPHI(-1.D0,AM,NIT,CA1)
!
            UG= AM*SG*RHG
            VG=RVG/HG
!
          ELSE  
!
!  ONE DATUM IS MISSING, WE SUPPOSE "THE LAKE AT REST"

            UG= 0.D0
            VG= 0.D0
!
          ENDIF
!
           ENDIF
!
         GOTO 220
      ENDIF
      GOTO 200
!
! THE OUTFLOW IS TORRENTIAL SO WE HAVE NO NEED FOR THE GIVEN H
!
       ELSE
       GOTO 100
!
      ENDIF
!
!
!    GIVEN VELOCITY 
!    ################
!
        ELSE IF(LIMPRO(K,2).EQ.KDIR) THEN
!
        UIN = UBOR(K)
        VIN = VBOR(K)
        HUIN = H*UIN
        HVIN = H*VIN
!
         DEST=HUIN*VNX1+HVIN*VNY1
         RVG =-HUIN*VNY1+HVIN*VNX1
!    WARNING: SIGN CHANGE / INRIA REPORT
            A1 = -DEST+FHPLUS
            A2 = -UNN - 2.D0*SG*RH
!
         IF (A1.LE.0.D0) THEN
!
!    FH- =-A1 CANNOT BE SATISFIED
!
         FHMOINS = 0.D0
         FUMOINS = 0.D0
         VG=0.D0
        SIGMAX=1.E-2
           ELSE
           CA1= 1.D0/(G*SQ2*A1)**(1.D0/3.D0)
           CALL ZEROPSI(-0.5D0,AM,NIT,CA1,A2)
!
           RHG =A2/(SG*(AM-2.D0))
           HG= RHG * RHG
           HRHG= RHG * HG
!
         IF (HG.EQ.0.D0) THEN
         UG=0.D0
         VG=0.D0
         FHMOINS = 0.D0
         FUMOINS = 0.D0
               SIGMAX=1.D-2
         ELSE
            UG=-AM*A2/(AM-2.D0)
            VG=RVG/HG
        GOTO 220
      ENDIF
      ENDIF
        GOTO 200
!
! NO CONDITION 
!
        ELSE
!
        GOTO 100
!
        ENDIF
       GOTO 1000
!
!
!   CALCULATION OF F-(HG,UG,VG)
!
 220   CONTINUE
!
         A=MIN(RA3,MAX(-RA3,-UG/RHG))
         A2 =A * A
         A3 =A2 * A
         ALPHA0=ALP*(A+RA3)
         ALPHA1=ALP2*(A2-RA32)
         ALPHA2=ALP3*(A3+RA33)
!
         FHMOINS = HG*UG*ALPHA0 + HRHG*ALPHA1
         FUMOINS = UG*(FHMOINS + HRHG*ALPHA1) 
     &  + HG*HG*ALPHA2
!
            SIGMAX= RHG
            UNORM=SQRT(UG *UG + VG*VG)
            SIGMAX=MAX( 1.D-2, RA3 *SIGMAX +UNORM )
!
!    CALCUL DES FLUX ET ROTATION INVERSE
!
 200   CONTINUE
         FLUH=(FHPLUS +FHMOINS)*VNL
         FLUU=(FUPLUS +FUMOINS)*VNL
!
         IF (FLUH.GE.0.D0) THEN 
         FLUV= VNN*FLUH 
         ELSE
         FLUV= VG*FLUH 
         ENDIF
!
      FLUTMP=FLUU
      FLUU = +VNX1*FLUTMP-VNY1*FLUV
      FLUV = +VNY1*FLUTMP+VNX1*FLUV
!
!      CORRECTION OF THE TIME STEP
!
       DTL = CFL*DTHAUT(IS)/SIGMAX
       DT  = MIN(DT, DTL)
!
       GOTO 1000
100    CONTINUE
       RUN     = H*UNN
!
       FLUH =  RUN* VNL
       FLUU =  (U *RUN + 0.5D0*G*H**2* VNX)*VNL
       FLUV =  (V *RUN + 0.5D0*G*H**2* VNY)*VNL
!
 1000  CONTINUE
       ENDIF
!
       IF(LIMPRO(K,1).EQ.KDIR)  FLUSORT = FLUSORT + FLUH
       IF(LIMPRO(K,2).EQ.KDIR)  FLUENT = FLUENT +FLUH

!RA
       FLBOR%R(K)=FLUH       
!
       CE(IS,1)  = CE(IS,1) - FLUH
       CE(IS,2)  = CE(IS,2) - FLUU
       CE(IS,3)  = CE(IS,3) - FLUV
!
       IF(NTRAC.GT.0) THEN
         DO ITRAC=1,NTRAC
           FLUHBTEMP%ADR(ITRAC)%P%R(K)=FLUH
         ENDDO
       ENDIF
! 
       ENDDO
!
!-----------------------------------------------------------------------
!
       RETURN
       END
