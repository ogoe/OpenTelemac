!                    ***************
                     SUBROUTINE BUSE
!                    ***************
!
     &(RELAXB,NBUSE,ENTBUS,SORBUS,GRAV,
     & H,ZF,DBUS,LRGBUS,HAUBUS,CLPBUS,
     & ALTBUS,CSBUS,CEBUS,ANGBUS,LBUS,
     & NTRAC,T,TBUS,UBUS,VBUS,U,V,ENTET)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   23/05/2012
!***********************************************************************
!
!brief    TREATS TUBES (OR BRIDGES) IN CHARGE OR WITH FREE SURFACE.
!
!history  C. COULET (ARTELIA)
!+        23/05/2012
!+     First version.
!+
!
!history  U.H.Merkel
!+        17/07/2012
!+        V6P2
!+     Adaption to NAG.
!
!history  J-M HERVOUET (LNHE)
!+        27/07/2012
!+        V6P2 
!+     Correction in parallel.
!
!history  C. COULET (ARTELIA)
!+        23/04/2013
!+        V6P3
!+     Correction of a bug.
!+     Sometimes SECT=0 and DBUS<>0 due to relaxation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALTBUS         |-->| ELEVATIONS OF TUBES
!| ANGBUS         |-->| ANGLE OF TUBES WITH AXIS OX.
!| CEBUS          |-->| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW
!| CLPBUS         |-->| INTEGER FLAG FOR FLOW DIRECTION (VALVE)
!|                |   | 0 - BOTH DIRECTIONS
!|                |   | 1 - ONLY FROM ENTRY TO EXIT
!|                |   | 2 - ONLY FROM EXIT TO ENTRY
!|                |   | 3 - NO FLOW
!| CSBUS          |-->| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW
!| DBUS           |-->| DISCHARGE OF TUBES.
!| ENTET          |-->| IF YES, PRINTING INFORMATION ON LISTING
!| ENTBUS         |-->| INDICES OF ENTRY OF TUBES IN GLOBAL NUMBERING
!| GRAV           |-->| GRAVITY
!| H              |-->| DEPTH
!| LBUS           |-->| LINEAR HEAD LOSS OF TUBE
!| LBUS           |-->| LINEAR HEAD LOSS OF TUBE
!| NBUSE          |-->| NUMBER OF TUBES
!| NTRAC          |-->| NUMBER OF TRACERS
!| RELAXB         |-->| RELAXATION COEFFICIENT
!| SORBUS         |-->| INDICES OF TUBES EXITS IN GLOBAL NUMBERING
!| T              |-->| BLOCK OF TRACERS
!| TBUS           |<->| VALUES OF TRACERS AT TUBES EXTREMITY
!| U              |-->| X-COMPONENT OF VELOCITY
!| UBUS           |<->| VELOCITY U AT TUBES EXTREMITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| VBUS           |<->| VELOCITY V AT TUBES EXTREMITY
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : V2DPAR,DT,SECBUS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: NBUSE,NTRAC
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE),SORBUS(NBUSE)
      LOGICAL          , INTENT(IN)    :: ENTET
      DOUBLE PRECISION , INTENT(IN)    :: RELAXB,GRAV
      DOUBLE PRECISION , INTENT(INOUT) :: UBUS(NBUSE,2),VBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(INOUT) :: DBUS(NBUSE)
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TBUS
      DOUBLE PRECISION , INTENT(IN)    :: ANGBUS(NBUSE,2),LBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: CEBUS(NBUSE,2),CSBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(IN)    :: ALTBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(IN)    :: LRGBUS(NBUSE),HAUBUS(NBUSE)
      INTEGER          , INTENT(IN)    :: CLPBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: H(*),ZF(*),U(*),V(*)
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I1,I2,ITRAC
!
      DOUBLE PRECISION L,LARG,HAUT
      DOUBLE PRECISION S1,S2,CE1,CE2,CS1,CS2,Q,QMAX1,QMAX2
      DOUBLE PRECISION RD1,RD2
!
      INTRINSIC SQRT,COS,SIN
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
! LOOP OVER THE TUBES
!
      DO N=1,NBUSE
!
!     IDENTIFIES ENTRY / EXIT NODES
!
!     NUMBER OF THE POINTS
      I1=ENTBUS(N)
      I2=SORBUS(N)
!
!     LOADS, TAKEN AS FREE SURFACE ELEVATION
!
      IF(I1.GT.0) THEN
        S1=H(I1)+ZF(I1)
        QMAX1=0.9D0*H(I1)*V2DPAR%R(I1)/DT
      ELSE
        S1=0.D0
        QMAX1=0.D0
      ENDIF
      IF(I2.GT.0) THEN
        S2=H(I2)+ZF(I2)
        QMAX2=0.9D0*H(I2)*V2DPAR%R(I2)/DT
      ELSE
        S2=0.D0
        QMAX2=0.D0
      ENDIF
!     CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN
      IF(NCSIZE.GT.1) THEN
        S1=P_DMAX(S1)+P_DMIN(S1)
        S2=P_DMAX(S2)+P_DMIN(S2)
        QMAX1=P_DMAX(QMAX1)+P_DMIN(QMAX1)
        QMAX2=P_DMAX(QMAX2)+P_DMIN(QMAX2)
      ENDIF
!
!     COEFFICIENTS FOR COMPUTATION OF PRESSURE LOSS
!
      CE1=CEBUS(N,1)
      CE2=CEBUS(N,2)
      CS1=CSBUS(N,1)
      CS2=CSBUS(N,2)
      L  =LBUS(N)
      RD1 =ALTBUS(N,1)
      RD2 =ALTBUS(N,2)
      LARG=LRGBUS(N)
      HAUT=HAUBUS(N)
!
!     COMPUTES THE FLOW ACCORDING TO DELTAH
!     IF THE LINEAR PRESSURE LOSS IS NEGLIGIBLE, COULD HAVE DIFFERENT
!     ENTRY / EXIT SECTIONS
!
      IF(S1.GE.S2) THEN
!
        IF(S1.GT.RD1.AND.S1.GT.RD2) THEN
!
          IF(S1.LT.(RD1+HAUT).AND.S1.LT.(RD2+HAUT)) THEN
!           FREE SURFACE FLOW WHICH FOLLOW A WEIR LAW
            IF(S2.GT.(0.666666667D0*(S1-RD2)+RD2)) THEN
              Q = LARG * SQRT( 2.D0*GRAV*(S1-S2)/(CE1+L+CS2) )*(S2-RD2)
              SECBUS%R(N) = (S2-RD2) * LARG
            ELSE
              Q = LARG * SQRT(2.D0*GRAV) * (S1-RD1)**1.5D0 * 0.385D0
              SECBUS%R(N) = (S1-RD1) * LARG
            ENDIF
          ELSE
!           PRESSURE FLOW --> ORIFICE LAW
            SECBUS%R(N) = LARG * HAUT
            IF(S1.GE.(RD1+HAUT)) THEN
              Q = SECBUS%R(N) * SQRT( 2.D0*GRAV*(S1-S2)/(CE1+L) )
            ELSE
              Q = SECBUS%R(N) * SQRT( 2.D0*GRAV*(S1-S2)/(L+CS2+CE1) )
            ENDIF
          ENDIF
        ELSE
          Q=0.D0
        ENDIF
!
      ELSE
!
        IF(S2.GT.RD1.AND.S2.GT.RD2) THEN
!
          IF(S2.LT.(RD1+HAUT).AND.S2.LT.(RD2+HAUT)) THEN
!           FREE SURFACE FLOW WHICH FOLLOW A WEIR LAW
            IF(S1.GT.(0.6667*(S2-RD1)+RD1)) THEN
              Q=-LARG*SQRT(2.D0*GRAV*(S2-S1)/(CE2+L+CS1))*(S1-RD1)
              SECBUS%R(N) = (S1-RD1) * LARG
            ELSE
              Q = - LARG * SQRT( 2.D0*GRAV ) * SQRT((S2-RD2)**3) * 0.385
              SECBUS%R(N) = (S2-RD2) * LARG
            ENDIF
          ELSE
!           PRESSURE FLOW --> ORIFICE LAW
            SECBUS%R(N) = LARG * HAUT
            IF(S2.GE.(RD2+HAUT)) THEN
              Q = - SECBUS%R(N) * SQRT( 2.D0*GRAV*(S2-S1)/(CE2+L) )
            ELSE
              Q = - SECBUS%R(N) * SQRT( 2.D0*GRAV*(S2-S1)/(L+CS1+CE2) )
            ENDIF
          ENDIF
        ELSE
          Q=0.D0
        ENDIF
      ENDIF
!
!     NOTHING HAPPENS IF THE LOADS AT THE 2 ENDS ARE LOWER THAN
!     THE ELEVATION OF THE NOZZLES
!
      IF(S1.LT.RD1.AND.S2.LT.RD2) Q=0.D0
!
!     SLUICE VALVE TREATMENT
!
      IF ((CLPBUS(N).EQ.1.AND.S2.GT.S1).OR.
     &    (CLPBUS(N).EQ.2.AND.S1.GT.S2).OR.
     &    (CLPBUS(N).EQ.3))
     &    Q = 0.D0
!
!     FILLS OUT DBUS(N) USING RELAXATION
!
      DBUS(N)= RELAXB * Q + (1.D0-RELAXB) * DBUS(N)
!
!     LIMITATION WITH AVAILABLE WATER
!
      IF(DBUS(N).GT.0.D0) THEN
        DBUS(N)=MIN(QMAX1,DBUS(N))
      ELSE
        DBUS(N)=MAX(-QMAX2,DBUS(N))
      ENDIF
!
      IF(ENTET.AND.ABS(DBUS(N)).GT.1.D-4) THEN
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'BUSE ',N,' DEBIT DE ',DBUS(N),' M3/S'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TUBE ',N,' DISCHARGE OF ',DBUS(N),' M3/S'
        ENDIF
        WRITE(LU,*) ' '
      ENDIF
!
!  TREATS THE VELOCITIES AT THE SOURCES
!  SAME APPROACH FOR VELOCITY AND TRACER
!
      IF(DBUS(N).GT.0.D0) THEN
        UBUS(N,2) = ( DBUS(N)/SECBUS%R(N) ) * COS(ANGBUS(N,2))
        VBUS(N,2) = ( DBUS(N)/SECBUS%R(N) ) * SIN(ANGBUS(N,2))
        IF(I1.GT.0) THEN
          UBUS(N,1) = U(I1)
          VBUS(N,1) = V(I1)
        ELSE
          UBUS(N,1) = 0.D0
          VBUS(N,1) = 0.D0
        ENDIF
        IF(NCSIZE.GT.1) THEN
          UBUS(N,1)=P_DMAX(UBUS(N,1))+P_DMIN(UBUS(N,1))
          VBUS(N,1)=P_DMAX(VBUS(N,1))+P_DMIN(VBUS(N,1))
        ENDIF
      ELSE
        UBUS(N,1) = ( DBUS(N)/SECBUS%R(N) ) * COS(ANGBUS(N,1))
        VBUS(N,1) = ( DBUS(N)/SECBUS%R(N) ) * SIN(ANGBUS(N,1))
        IF(I2.GT.0) THEN
          UBUS(N,2) = U(I2)
          VBUS(N,2) = V(I2)
        ELSE
          UBUS(N,2) = 0.D0
          VBUS(N,2) = 0.D0
        ENDIF
        IF(NCSIZE.GT.1) THEN
          UBUS(N,2)=P_DMAX(UBUS(N,2))+P_DMIN(UBUS(N,2))
          VBUS(N,2)=P_DMAX(VBUS(N,2))+P_DMIN(VBUS(N,2))
        ENDIF
      ENDIF
!
!     TREATS THE TRACER :
!     NOTA : NBUSE + N <==> N,2
!                    N <==> N,1
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          IF(DBUS(N).GE.0.D0) THEN ! I1 --> I2
!           CASE DBUS(N)=0.D0 NOT CLEAR, BUT A VALUE HAS TO BE
!           GIVEN HERE, LEST IT IS USED AFTER 
            IF(I1.GT.0) THEN
              TBUS%ADR(ITRAC)%P%R(NBUSE+N)=T%ADR(ITRAC)%P%R(I1)
              TBUS%ADR(ITRAC)%P%R(N)      =T%ADR(ITRAC)%P%R(I1)
            ELSE
              TBUS%ADR(ITRAC)%P%R(NBUSE+N)=0.D0
              TBUS%ADR(ITRAC)%P%R(N)      =0.D0
            ENDIF
          ELSE ! I2 --> I1
            IF(I2.GT.0) THEN
              TBUS%ADR(ITRAC)%P%R(N)      =T%ADR(ITRAC)%P%R(I2)
              TBUS%ADR(ITRAC)%P%R(NBUSE+N)=T%ADR(ITRAC)%P%R(I2)
            ELSE
              TBUS%ADR(ITRAC)%P%R(N)      =0.D0
              TBUS%ADR(ITRAC)%P%R(NBUSE+N)=0.D0
            ENDIF
          ENDIF
          IF(NCSIZE.GT.1) THEN
            TBUS%ADR(ITRAC)%P%R(NBUSE+N)=
     &        P_DMAX(TBUS%ADR(ITRAC)%P%R(NBUSE+N))
     &       +P_DMIN(TBUS%ADR(ITRAC)%P%R(NBUSE+N))
            TBUS%ADR(ITRAC)%P%R(N)      =
     &        P_DMAX(TBUS%ADR(ITRAC)%P%R(N))
     &       +P_DMIN(TBUS%ADR(ITRAC)%P%R(N))
          ENDIF
        ENDDO
      ENDIF
!
!  END OF THE LOOP OVER THE TUBES
!
      ENDDO ! N
!
!-----------------------------------------------------------------------
!
      RETURN
      END
