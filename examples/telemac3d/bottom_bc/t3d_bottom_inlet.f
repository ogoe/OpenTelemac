!                    *****************
                     SUBROUTINE CONDIM
!                    *****************
!
!
!***********************************************************************
! TELEMAC3D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES VELOCITY, DEPTH AND TRACERS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET(LNH)
!+        11/12/2000
!+        V5P1
!+   TELEMAC 3D VERSION 5.1
!
!history
!+        20/04/2007
!+
!+   ADDED INITIALISATION OF DPWAVE
!
!history
!+        23/01/2009
!+
!+   ADDED CHECK OF ZSTAR
!
!history
!+        16/03/2010
!+
!+   NEW OPTIONS FOR BUILDING THE MESH IN CONDIM, SEE BELOW
!
!history  J-M HERVOUET(LNHE)
!+        05/05/2010
!+        V6P0
!+   SUPPRESSED INITIALISATION OF DPWAVE
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
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        C.-T. PHAM (LNHE)
!+        19/07/2012
!+        V6P2
!+   Addition of the TPXO tidal model by calling CONDI_TPXO
!+   (the TPXO model being coded in module TPXO)
!
!history  C.-T. PHAM (LNHE), M.S.TURNBULL (HRW)
!+        02/11/2012
!+        V6P3
!+   Correction of bugs when initialising velocity with TPXO
!+   or when sea levels are referenced with respect to Chart Datum (CD)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CONDIM => CONDIM
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE TPXO
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
! TO HAVE A LINEAR OR QUADRATIC GROWTH OF THE PLANES
      INTEGER IPCENTER
      DOUBLE PRECISION A,B,C
!
!***********************************************************************
!
!     ORIGIN OF TIME
!
      IF(.NOT.SUIT2) AT  = 0.D0
!
!     INITIALISES H, THE WATER DEPTH
!
      IF(.NOT.SUIT2) THEN
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=COTINI)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=HAUTIN)
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
        CALL OS('X=-Y    ',X=H,Y=ZF)
        CALL CONDI_TPXO(NPOIN2,MESH2D%NPTFR,MESH2D%NBOR%I,
     &                  X2%R,Y2%R,H%R,U2D%R,V2D%R,
     &                  LIHBOR%I,LIUBOL%I,KENT,KENTU,
     &                  GEOSYST,NUMZONE,LATIT,LONGIT,
     &                  T3D_FILES,T3DBB1,T3DBB2,
     &                  MARDAT,MARTIM,INTMICON,MSL)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!     USER INPUT :
!     PROGRAM HERE SPECIAL INITIAL CONDITIONS ON DEPTH
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIM : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &      ,/,1X,'         VOUS DEVEZ MODIFIER CONDIM')
11      FORMAT(1X,'CONDIM : WITH SPECIAL INITIAL CONDITIONS'
     &      ,/,1X,'         YOU HAVE TO MODIFY CONDIM')
        CALL PLANTE(1)
        STOP
!     END OF SPECIAL INITIAL CONDITIONS
!     END OF USER INPUT
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIM : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR LUE DANS LE FICHIER BINAIRE 1'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
!
!     CLIPS H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
!
      CALL OS ('X=Y     ',X=HN,Y=H)
!
!-----------------------------------------------------------------------
!
!     DATA TO BUILD VERTICAL COORDINATES IN CALCOT
!
!     TRANSF IS KEYWORD "MESH TRANSFORMATION"
!     IF TRANSF = 0, SUBROUTINE CALCOT MUST BE IMPLEMENTED BY THE USER
!
!     AN EQUIVALENT OF TRANSF MUST BE GIVEN FOR EVERY PLANE:
!
!     POSSIBLE VALUES OF TRANSF_PLANE :
!
!     1 : SIGMA TRANSFORMATION WITH EVENLY SPACED PLANES
!     2 : SIGMA TRANSFORMATION WITH PROPORTIONS GIVEN IN ZSTAR
!     3 : PRESCRIBED ELEVATION GIVEN IN ZPLANE
!
!     STANDARD BELOW IS: EVENLY SPACED PLANES, NO OTHER DATA REQUIRED
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=2
      ENDDO
      IPCENTER = INT(REAL(NPLAN)/2.0+0.5)
      ZSTAR%R(1)=0.D0
      ZSTAR%R(IPCENTER)=0.5D0
      ZSTAR%R(NPLAN)=1.D0
      !QUADRATIC PROFILE
      ! BOTTOM HALF
      A=(0.5-0.D0)/REAL(IPCENTER-1)/REAL(IPCENTER-1)
      B=-2.D0*A
      C=0.D0+A
      DO IPLAN = 2,IPCENTER-1
        ZSTAR%R(IPLAN)=A*REAL(IPLAN)**2+B*REAL(IPLAN)+C
      ENDDO
      ! TOP HALF
      A=(0.5-1.0)/REAL(NPLAN-IPCENTER)/REAL(IPCENTER-NPLAN)
      B=2.D0*REAL(NPLAN)*A
      C=1.D0-REAL(NPLAN)**2*A
      DO IPLAN = IPCENTER+1,NPLAN-1
        ZSTAR%R(IPLAN)=-A*REAL(IPLAN)**2+B*REAL(IPLAN)+C
      ENDDO
!
!***********************************************************************
!
!     COMPUTES ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISES VELOCITIES
!
      IF(SUIT2) THEN
        DO I=1,NPLAN
          DO J=1,NPOIN2
           U%R((I-1)*NPOIN2+J)=U2D%R(J)
           V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
        DO I=1,NPLAN
          DO J=1,NPOIN2
            U%R((I-1)*NPOIN2+J)=U2D%R(J)
            V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSE
        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISES THE K-EPSILON MODEL (OPTIONAL)
!   WHEN DONE: AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3) THEN
!
!       HERE INITIALISES K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISES THE PRESSURE FIELDS TO 0.0
!
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END



!                    *****************
                     SUBROUTINE LIMI3D
!                    *****************
!
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES TYPES OF 3D BOUNDARY CONDITIONS.
!+
!+            SETS THE VALUE OF SOME COEFFICIENTS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)
!+
!+        V6P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN2, IPLAN, IPTFR, IPTFR3, ITRAC
!
!***********************************************************************
!
!     BOUNDARY CONDITIONS ON VELOCITIES
!     *********************************
!
!     BOTTOM
!     ======
!
!     DEFAULT: IMPERMEABILITY AND LOG LAW (SEE ALSO BORD3D)
!
      IF(BC_BOTTOM.EQ.1) THEN
!
        DO IPOIN2 = 1,NPOIN2
          LIUBOF%I(IPOIN2) = KLOG
          LIVBOF%I(IPOIN2) = KLOG
          LIWBOF%I(IPOIN2) = KLOG
!         USEFUL ? SHOULD NOT BE USED ANYWAY
          UBORF%R(IPOIN2)  = 0.D0
          VBORF%R(IPOIN2)  = 0.D0
          WBORF%R(IPOIN2)  = 0.D0
          IF(SQRT((X(IPOIN2)-2000.D0)**2+(Y(IPOIN2)-2000.D0)**2)
     &       .LE.50.D0)THEN
            !KENT = 5; I.E. IMPOSED FLOW RATE
            !KENTU = 6; I.E. IMPOSED VELOCITY
            LIUBOF%I(IPOIN2) = 5 ! AT THE MOMENT KENT DOES NOT WORK
            LIVBOF%I(IPOIN2) = 5 ! AT THE MOMENT KENT DOES NOT WORK
            LIWBOF%I(IPOIN2) = 5 ! AT THE MOMENT KENT DOES NOT WORK
            NLIQBED%I(IPOIN2) = 1
            PRINT*, '========================'
            PRINT*, 'FOR POINT ',IPOIN2
            PRINT*, 'BEDFLO',BEDFLO(1),' NBEDFLO',NBEDFLO
          ENDIF
        ENDDO
!
      ELSEIF(BC_BOTTOM.EQ.2) THEN
!
        DO IPOIN2 = 1,NPOIN2
          LIUBOF%I(IPOIN2) = KADH
          LIVBOF%I(IPOIN2) = KADH
          LIWBOF%I(IPOIN2) = KADH
!         USEFUL ? KADH SAYS IT IS 0.D0
          UBORF%R(IPOIN2)  = 0.D0
          VBORF%R(IPOIN2)  = 0.D0
          WBORF%R(IPOIN2)  = 0.D0
        ENDDO
!
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LIMI3D : FAUSSE CONDITION A LA LIMTE AU FOND'
          WRITE(LU,*) '         VALEUR ',BC_BOTTOM,' INCONNUE'
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LIMI3D : BAD BOUNDARY CONDITION ON THE BOTTOM'
          WRITE(LU,*) '         VALUE ',BC_BOTTOM,' UNKNOWN'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COEFFICIENTS SET TO 0 BY MEANS OF THEIR COMPONENT TYPR
!     NO DIFFUSION FLUX THROUGH BOTTOM
!
      AUBORF%TYPR='0'
      AVBORF%TYPR='0'
      BUBORF%TYPR='0'
      BVBORF%TYPR='0'
      IF(NONHYD) THEN
        AWBORF%TYPR='0'
        BWBORF%TYPR='0'
      ENDIF
!
!     LATERAL BOUNDARIES
!     ==================
!
!     DEFAULT: 2D CONDITIONS DUPLICATED ON THE VERTICAL
!              FREE FOR W
!              NO FRICTION
!
      DO IPLAN = 2,NPLAN
        DO IPTFR = 1,NPTFR2
          IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
          LIUBOL%I(IPTFR3) = LIUBOL%I(IPTFR)
          LIVBOL%I(IPTFR3) = LIVBOL%I(IPTFR)
          UBORL%R(IPTFR3)  = UBORL%R(IPTFR)
          VBORL%R(IPTFR3)  = VBORL%R(IPTFR)
          AUBORL%R(IPTFR3) = AUBORL%R(IPTFR)
        ENDDO
      ENDDO
!
!     IDEA OF OPTIMISATION (BEWARE PARALLELISM)
!
!     IF(DOTS(AUBORL,AUBORL).GT.1.D-8) THEN
!       AUBORL%TYPR='Q'
!     ELSE
!       AUBORL%TYPR='0'
!     ENDIF
!
      DO IPTFR3 = 1,NPTFR3
!                           KSORT: W FREE ON LATERAL BOUNDARIES
        LIWBOL%I(IPTFR3)        = KSORT
!       VALUES SAVED IN SECOND DIMENSION BECAUSE ADVECTION
!       SCHEMES MAY CHANGE THE VALUES
        LIUBOL%I(IPTFR3+NPTFR3) = LIUBOL%I(IPTFR3)
        LIVBOL%I(IPTFR3+NPTFR3) = LIVBOL%I(IPTFR3)
        LIWBOL%I(IPTFR3+NPTFR3) = LIWBOL%I(IPTFR3)
        WBORL%R(IPTFR3)  = 0.D0
!       BUBORL%R(IPTFR3) = 0.D0
!       BVBORL%R(IPTFR3) = 0.D0
      ENDDO
      BUBORL%TYPR='0'
      BVBORL%TYPR='0'
!
      IF(NONHYD) THEN
!       DO IPTFR3 = 1,NPTFR3
!         AWBORL%R(IPTFR3) = 0.D0
!         BWBORL%R(IPTFR3) = 0.D0
!       ENDDO
        AWBORL%TYPR='0'
        BWBORL%TYPR='0'
      ENDIF
!
!     FREE SURFACE
!     ============
!
!     DEFAULT: IMPERMEABILITY AND NO FRICTION (SEE ALSO BORD3D)
!
      DO IPOIN2 = 1,NPOIN2
        LIUBOS%I(IPOIN2) = KLOG
        LIVBOS%I(IPOIN2) = KLOG
        LIWBOS%I(IPOIN2) = KLOG
        UBORS%R(IPOIN2)  = 0.D0
        VBORS%R(IPOIN2)  = 0.D0
        WBORS%R(IPOIN2)  = 0.D0
!       AUBORS%R(IPOIN2) = 0.D0
!       BUBORS%R(IPOIN2) = 0.D0
!       BVBORS%R(IPOIN2) = 0.D0
      ENDDO
      AUBORS%TYPR='0'
      BUBORS%TYPR='0'
      AVBORS%TYPR='0'
      BVBORS%TYPR='0'
!
      IF(NONHYD) THEN
!       DO IPOIN2 = 1,NPOIN2
!         AWBORS%R(IPOIN2) = 0.D0
!         BWBORS%R(IPOIN2) = 0.D0
!       ENDDO
        AWBORS%TYPR='0'
        BWBORS%TYPR='0'
      ENDIF
!
!     **************
!     TRACERS BC'S
!     **************
!
      IF (NTRAC.NE.0) THEN
        DO ITRAC = 1,NTRAC
!
!     BOTTOM
!     ======
!
!     DEFAULT: NEUMANN BC'S
!
          DO IPOIN2 = 1,NPOIN2
            LITABF%ADR(ITRAC)%P%I(IPOIN2) = KLOG
            TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           ATABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           BTABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
          ENDDO
          ATABOF%ADR(ITRAC)%P%TYPR='0'
          BTABOF%ADR(ITRAC)%P%TYPR='0'
!
!     SIDES
!     =====
!
!     DEFAULT: NEUMANN BC'S
!
!           WHAT HAS BEEN READ IN THE BOUNDARY CONDITIONS FILE
!           FOR 1 TRACER IS DUPLICATED ON THE VERTICAL AND FOR
!           ALL TRACERS
!
          DO IPLAN = 1,NPLAN
            DO IPTFR = 1,NPTFR2
              IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
              LITABL%ADR(ITRAC)%P%I(IPTFR3) = LITABL%ADR(1)%P%I(IPTFR)
!             SAVING ON SECOND DIMENSION BECAUSE ADVECTION SCHEMES
!             MAY CHANGE THIS VALUE
              LITABL%ADR(ITRAC)%P%I(IPTFR3+NPTFR3) =
     &                                        LITABL%ADR(1)%P%I(IPTFR)
              TABORL%ADR(ITRAC)%P%R(IPTFR3) = TABORL%ADR(1)%P%R(IPTFR)
!             ATABOL%ADR(ITRAC)%P%R(IPTFR3) = ATABOL%ADR(1)%P%R(IPTFR)
!             BTABOL%ADR(ITRAC)%P%R(IPTFR3) = BTABOL%ADR(1)%P%R(IPTFR)
            ENDDO
          ENDDO
          ATABOL%ADR(ITRAC)%P%TYPR='0'
          BTABOL%ADR(ITRAC)%P%TYPR='0'
!
!     FREE SURFACE
!     =============
!
!     DEFAULT: NEUMANN BC'S
!
          DO IPOIN2 = 1,NPOIN2
             LITABS%ADR(ITRAC)%P%I(IPOIN2) = KLOG
             TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!            ATABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!            BTABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
          ENDDO
          ATABOS%ADR(ITRAC)%P%TYPR='0'
          BTABOS%ADR(ITRAC)%P%TYPR='0'
!
        ENDDO
      ENDIF
!
!     SOLID BOUNDARIES FOR K AND EPSILON
!     **********************************
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
!
!     BOTTOM
!     ======
!
!     DEFAULT : NO GRADIENT
!
        DO IPOIN2 = 1,NPOIN2
          AKBORF%R(IPOIN2) = 0.D0
          BKBORF%R(IPOIN2) = 0.D0
          AEBORF%R(IPOIN2) = 0.D0
          BEBORF%R(IPOIN2) = 0.D0
        ENDDO
        AKBORF%TYPR = '0'
        BKBORF%TYPR = '0'
        AEBORF%TYPR = '0'
        BEBORF%TYPR = '0'
!
!     SIDES
!     =====
!
!     DEFAULT : NO GRADIENT
!
        DO IPTFR3 = 1,NPTFR3
          AKBORL%R(IPTFR3) = 0.D0
          BKBORL%R(IPTFR3) = 0.D0
          AEBORL%R(IPTFR3) = 0.D0
          BEBORL%R(IPTFR3) = 0.D0
        ENDDO
!
!     FREE SURFACE
!     ============
!
!     DEFAULT : NO GRADIENT
!
        DO IPOIN2 = 1,NPOIN2
           AKBORS%R(IPOIN2) = 0.D0
           BKBORS%R(IPOIN2) = 0.D0
           AEBORS%R(IPOIN2) = 0.D0
           BEBORS%R(IPOIN2) = 0.D0
        ENDDO
!
      ENDIF
!
!
!     FRICTION COEFFICIENTS
!     *********************
!
!     DEFAULT: VALUE GIVEN IN STEERING FILE
!
      CALL OV('X=C     ',RUGOL%R,RUGOL%R,RUGOL%R,RUGOL0,NPTFR2*NPLAN)
!
!======================================================================
! DEFAULT BOUNDARY CONDITION TYPES AND VALUES FOR THE
! PRESSURE POISSON EQUATION
!======================================================================
!
      IF(NONHYD) THEN
!
!-----------------------------------------------------------------------
!
! DEFAULT TYPES AND VALUES FOR THE PRESSURE BOUNDARY CONDITIONS
! BOTTOM AND FREE SURFACE
!
!       AT ALL LATERAL BOUNDARIES AND BOTTOM DP/DN = 0;
!       DIRICHLET = 0 AT THE SURFACE; DIRICHLET CONDITIONS SET TO 0 ALL OVER
!       (CORRECT AT THE SURFACE ONLY)
!
!       BOTTOM AND SURFACE
!       CHECK KLOG BOTTOM
!
        DO IPOIN2=1,NPOIN2
          LIPBOF%I(IPOIN2) = KLOG
          LIPBOS%I(IPOIN2) = KENT
          PBORF%R(IPOIN2)  = 0.D0
          PBORS%R(IPOIN2)  = 0.D0
        ENDDO
!
!       LATERAL SURFACES: ALL TREATED AS NEUMANN
!
        DO IPTFR3=1,NPTFR3
          LIPBOL%I(IPTFR3) = KLOG
          PBORL%R(IPTFR3)  = 0.D0
        ENDDO
!
!       LATERAL SURFACES: DIRICHLET ON ENTRANCES, NEUMANN ELSEWHERE
!
!       DO IPTFR3=1,NPTFR3
!         PBORL%R(IPTFR3)  = 0.D0
!         IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
!    *       LIUBOL%I(IPTFR3).EQ.KENTU) THEN
!           LIPBOL%I(IPTFR3) = KENT
!         ELSE
!           LIPBOL%I(IPTFR3) = KLOG
!         ENDIF
!       ENDDO
!
      ENDIF
!
!======================================================================
!
      RETURN
      END
