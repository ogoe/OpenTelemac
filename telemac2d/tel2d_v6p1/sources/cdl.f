!                    **************
                     SUBROUTINE CDL
!                    **************
!
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,G,HBOR,
     & UBOR,VBOR,UA,CE,FLUENT,FLUSORT,FLBOR,
     & DTHAUT,DT,CFL,FLUHBTEMP,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SAINT VENANT-KINETIC.
!+
!+            COMPUTES THE ADVECTIVE FLUXES AT BOUNDARIES.
!+                UA(1,IS) = H;  UA(2,IS)=U;  UA(3,IS)=V.
!
!history  INRIA
!+        
!+        V5P8
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
!| CE             |<->| FLUX
!| CFL            |-->| NOMBRE DE CFL
!| DT             |<->| PAS DE TEMPS
!| DTHAUT         |-->| UTILISE POUR CONDITION CFL
!| FLBOR          |<--| FLUX MASSE ENTREE ET SORTIE
!| FLUENT,FLUSORT |<--| FLUX MASSE ENTREE ET SORTIE
!| FLUHBTEMP      |---| 
!| G              |-->| CONSTANTE DE GRAVITE
!| HBOR           |-->| VALEURS IMPOSEES DE H
!| KDIR           |-->| CONVENTION POUR LES POINTS DIRICHLET
!| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
!| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
!| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
!| NTRAC          |---| 
!| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!| UBOR           |-->| VALEURS IMPOSEES DE U
!| VBOR           |-->| VALEURS IMPOSEES DE V
!| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NPTFR,KDIR,KNEU,NTRAC
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),UA(3,NS),DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFL
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: CE(3,NS),FLUENT,FLUSORT
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
!         SOLID WALLS
!         **************
!
!      SLIP CONDITIONS
!      ***********************
!
       IF(LIMPRO(K,1).EQ.KNEU) THEN
!
         AUX=0.5D0*G*H**2
         FLUH = 0.D0
         FLUU = AUX*VNX
         FLUV = AUX*VNY
       ELSE
!
!        LIQUID BOUNDARIES
!        *******************
!
!     CALCULATES F+(H,U,V)
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
!     CALCULATES THE FICTITIOUS STATE (HG,UG,VG)
!     ----------------------------------
!
!     H PROVIDED
!     ########
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
!    FLUVIAL REGIME
!    --------------
!
          IF (VP2*VP3.LE.0.D0) THEN
!
           UG=UNN+2.D0*SG*(RH-RHG)
           VG=VNN
!
!    TORRENTIAL REGIME
!    -----------------
!
          ELSE
!
!   FLUX IMPOSED
!   -----------
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
!   DATA MISSING, ONE SUPPOSES STILL WATER
!
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
!  THE DOWNSTREAM IS IN FACT TORRENTIAL, ONE CANNOT MAINTAIN
!   THE CONDITION OF H IMPOSED
!
       ELSE
       GOTO 100
!
      ENDIF
!
!
!     VELOCITY IMPOSED
!     ################
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
!     ATTENTION: MODIFICATION OF THE SIGN / INRIA REPORT
            A1 = -DEST+FHPLUS
            A2 = -UNN - 2.D0*SG*RH
!
         IF (A1.LE.0.D0) THEN
!
!     FH- =-A1 CANNOT BE SATISFIED
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
!   NO CONDITION
!
        ELSE
!
        GOTO 100
!
        ENDIF
       GOTO 1000
!
!
!     CALCULATES F-(HG,UG,VG)
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
!     CALCULATES FLUXES AND OPPOSITE ROTATION
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
!       CORRECTION OF THE TIME STEP
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
!
! RA
       FLBOR%R(K)=FLUH
!
       CE(1,IS)  = CE(1,IS) - FLUH
       CE(2,IS)  = CE(2,IS) - FLUU
       CE(3,IS)  = CE(3,IS) - FLUV
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