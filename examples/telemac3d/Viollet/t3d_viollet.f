!
!  CHANGES VS SOURCE FILES:
!  IN KEPINI: SPECIFIC INITIAL CONDITIONS FOR K AND EPSILON
!  IN CSTKEP: DEFAULT VALUEES HAVE BEEN CHANGED: OPTPROD=1 AND KMIN=1.D-16
!  IN KEPCL3: SPECIFIC LATERAL BOUNDARY CONDITIONS FOR K AND EPSILON
!  IN KEPICL:
!  IN CONDIM: SPECIFIC INITIAL CONDITION FOR VELOCITY AND TRACER
!  IN BORD3D: EXCHANGE WITH ATMOSPHERE
!  IN UTIMP: CALL TO WRITE_NRFO
!
!                    *****************
                     SUBROUTINE KEPINI
!                    *****************
!
     &(AK,EP,U,V,Z,ZF,NPOIN2,NPLAN,DNUVIH,DNUVIV,KARMAN,CMU,KMIN,EMIN)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES K AND EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  V. BOYER UMIST
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
!| AK             |<->| TURBULENT ENERGY
!| CMU            |-->| CONSTANT FOR MODELE K-EPSILON MODEL
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EMIN           |-->| MINIMUM VALUE FOR EPSILON WHEN CLIPPING
!| EP             |<->| TURBULENT DISSIPATION
!| KARMAN         |-->| KARMAN CONSTANT
!| KMIN           |-->| MINIMUM VALUE FOR K WHEN CLIPPING
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| U              |-->| COMPONENT OF VELOCITY
!| V              |-->| COMPONENT OF VELOCITY
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN2,NPLAN
      DOUBLE PRECISION, INTENT(INOUT):: AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT):: EP(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: U(NPOIN2,NPLAN), V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: KARMAN, DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN)   :: CMU
      DOUBLE PRECISION, INTENT(IN)   :: KMIN, EMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,IPLAN
!
      INTRINSIC LOG, SQRT, MAX
!
      DOUBLE PRECISION, PARAMETER :: FICTIFEPS = 2.D0
!
! BEGIN OF PART SPECIFIC TO THIS CASE
      DOUBLE PRECISION DIST,DISTFOND,HAUT
! END OF PART SPECIFIC TO THIS CASE
!
!-----------------------------------------------------------------------
!
!     A THEORY BY VINCENT BOYER MODIFIED BY MARTIN FERRAND
!
! BEGIN OF PART SPECIFIC TO THIS CASE
!EGR UNCOMMENTED
      DO IPOIN2 = 1,NPOIN2
!EGR MODIF SUPPL MF
        DIST = (Z(IPOIN2,2)-ZF(IPOIN2))/FICTIFEPS
        HAUT = MAX(Z(IPOIN2,NPLAN)-ZF(IPOIN2),1.D-7)
!EGR END MODIF SUPPL
        DO IPLAN = 1,NPLAN
!
!         ARBITRARY COMPUTATION OF K EXPRESSED AS A PERCENTAGE OF SPEED
!
          AK(IPOIN2,IPLAN) = 1.D-3*U(IPOIN2,IPLAN)**2
          AK(IPOIN2,IPLAN) = MAX(AK(IPOIN2,IPLAN),KMIN)
!
!         COMPUTATION OF EPSILON
!
!         EP INITIALISED ACCORDING TO UETOIL**3/KAPPA/Y
!         WHERE UETOIL IS CALCULATED FROM THE VALUE OF K AT THE WALL
!
!         IF(IPLAN.EQ.1) THEN
!           DIST = (Z(IPOIN2,2)-ZF(IPOIN2))/FICTIFEPS
!         ELSE
!           DIST = Z(IPOIN2,IPLAN)-ZF(IPOIN2)
!         ENDIF
!         EP(IPOIN2,IPLAN)=CMU**0.75*SQRT(AK(IPOIN2,1)**3)/KARMAN/DIST
!EGR MODIF SUPPL MF
!         UETOIL**3/KAPPA/Z/SQRT(1-Z/H)

          IF(IPLAN.EQ.1) THEN
            DISTFOND = (Z(IPOIN2,2)-ZF(IPOIN2))/FICTIFEPS
          ELSE
            DISTFOND = Z(IPOIN2,IPLAN)-ZF(IPOIN2)
          ENDIF
          EP(IPOIN2,IPLAN)=CMU**0.75*SQRT(AK(IPOIN2,1)**3)/KARMAN/
     &                     DISTFOND/SQRT(1.D0-(DISTFOND-DIST)/HAUT)
!EGR END MODIF SUPPL
          EP(IPOIN2,IPLAN)=MAX(EP(IPOIN2,IPLAN),EMIN)
        ENDDO
      ENDDO
!EGR END MODIF
!
!-----------------------------------------------------------------------
!
!     HERE: NO INITIAL TURBULENCE
!
!EGR COMMENTED
!      DO IPOIN2 = 1,NPOIN2
!        DO IPLAN = 1,NPLAN
!          AK(IPOIN2,IPLAN) = KMIN
!          EP(IPOIN2,IPLAN) = EMIN
!        ENDDO
!      ENDDO
!EGR END MODIF
! END OF PART SPECIFIC TO THIS CASE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE CSTKEP
!                    *****************
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    SETS CONSTANTS OF K-EPSILON AND K-OMEGA MODELS.
!
!history  VINCENT BOYER
!+        01/02/01
!+
!+
!
!history  OLIVER GOETHEL
!+        18/03/04
!+
!+
!
!history  J-M HERVOUET(LNH)
!+        14/12/09
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        164/12/2012
!+        V6P3
!+   New parameters for monitoring k-epsilon, all arguments suppressed.
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALPHA          |<->| K-OMEGA CONSTANT
!| BETA           |<->| K-OMEGA CONSTANT
!| BETAS          |<->| K-OMEGA CONSTANT
!| C1             |<->| K-EPSILON CONSTANT
!| C2             |<->| K-EPSILON CONSTANT
!| CMU            |<->| K-EPSILON CONSTANT
!| EMAX           |<->| EPSILON MAXIMUM
!| EMIN           |<->| EPSILON MINIMUM
!| ITURBV         |-->| TURBULENCE MODEL (3:K-EPSILON 7:K-OMEGA)
!| KARMAN         |<->| VON KARMAN CONSTANT
!| KMAX           |<->| K MAXIMUM
!| KMIN           |<->| K MINIMUM
!| OMSTAR         |<->| K-OMEGA CONSTANT
!| PRANDTL        |<->| PRANDTL NUMBER
!| SCHMIT         |<->| SCHMIT NUMBER
!| SIGMAE         |<->| K-EPSILON OR K-OMEGA CONSTANT
!| SIGMAK         |<->| K-EPSILON OR K-OMEGA CONSTANT
!| VIRT           |<->| VIRTUAL ORIGIN FOR EPSILON
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
                       USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D, ONLY : CMU,C1,C2,SIGMAK,SIGMAE,
     &                                   VIRT,SCHMIT,KMIN,KMAX,
     &                                   EMIN,EMAX,ALPHA,
     &                                   BETA,BETAS,OMSTAR,ITURBV,
     &                                   CLIPK,CLIPE,WSIK,YAP,
     &                                   PERNORM2,PERPROD,RIMIN,RIMAX,
     &                                   OPTPROD,LIMKF,LIMEF,LIMKS,LIMES
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INTEGER,          INTENT(IN )   :: ITURBV
!     DOUBLE PRECISION, INTENT(INOUT) :: KMIN,KMAX,EMIN,EMAX
!     DOUBLE PRECISION, INTENT(INOUT) :: KARMAN,CMU,C1,C2,SIGMAK,SIGMAE
!     DOUBLE PRECISION, INTENT(INOUT) :: VIRT,PRANDTL,SCHMIT
!     DOUBLE PRECISION, INTENT(INOUT) :: ALPHA,BETA,BETAS,OMSTAR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!
!=======================================================================
!
!     ALL MODELS
!
!=======================================================================
!
!     VON KARMAN CONSTANT
!
!     UP TO VERSION 6.0, 0.41  FROM NOW ON : 0.40
!     FROM 7.0 USE KEYWORDS
!      KARMAN = 0.40D0
!
!     SCHMIDT NUMBER (not used)
!
!      SCHMIT = 1.D0
!
!     PRANDTL NUMBER (BETWEEN 0.8 AND 0.9 FOR TEMPERATURE)
!
!###>TBE - Prandtl number should be 1 by default for sediment
!cc      PRANDTL = 0.71D0
!      PRANDTL = 1.D0
!###<TBE
!
!     K-EPSILON OR K-OMEGA MODEL
!
      IF(ITURBV.EQ.3) THEN
        SIGMAK = 1.D0
        SIGMAE = 1.3D0
      ELSEIF(ITURBV.EQ.7) THEN
        SIGMAK = 2.D0
        SIGMAE = 2.D0
      ENDIF
!
!=======================================================================
!
!     K-EPSILON MODEL
!
!=======================================================================
!
      CMU    = 0.09D0
      C1     = 1.44D0
      C2     = 1.92D0
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS AT BOTTOM AND FREE SURFACE
!
!-----------------------------------------------------------------------
!
!     K : K
!     E : EPSILON
!     F : BOTTOM
!     S : FREE SURFACE
!     1 : NEUMANN
!     2 : DIRICHLET (VALUES COMPUTED IN KEPCL3)
!
      LIMKF = 1
      LIMEF = 1
      LIMKS = 1
! BEGIN OF PART SPECIFIC TO THIS CASE
!      LIMES = 1
      LIMES = 2
! END OF PART SPECIFIC TO THIS CASE
!
!-----------------------------------------------------------------------
!
!     PARAMETERS USED IN SUBROUTINE SOUKEP
!
!-----------------------------------------------------------------------
!
!     LIMITATION OF K AND EPSILON WITH PHYSICAL CRITERIA
!
      CLIPK    = .TRUE.
      CLIPE    = .TRUE.
      PERNORM2 = 0.5D0
      PERPROD  = 0.1D0
!
!     MIN AND MAX OF RICHARDSON NUMBER
!
      RIMIN=0.D0
      RIMAX=100.D0
!
!     OPTION FOR PRODUCTION
!     1: LINEAR (NOT STANDARD)
!     2: QUADRATIC (STANDARD)
!
! BEGIN OF PART SPECIFIC TO THIS CASE
!      OPTPROD=2
      OPTPROD=1
! END OF PART SPECIFIC TO THIS CASE
!
!     WIND STRESS IN K
!
      WSIK=.TRUE.
!
!     YAP CORRECTION
!
      YAP=.FALSE.
!
!=======================================================================
!
!     K-OMEGA MODEL
!
!=======================================================================
!
      ALPHA  = 5.D0/9.D0
      BETA   = 3.D0/40.D0
      BETAS  = 0.09D0
!
!     TO COMPUTE THE FREE SURFACE VALUE OF OMEGA
!
      OMSTAR  = 100.D0
!
!     VIRTUAL ORIGIN FOR EPSILON
!
      VIRT = 0.07D0
!
!     MINIMA AND MAXIMA FOR CLIPPING
!
      IF(ITURBV.EQ.3) THEN
! BEGIN OF PART SPECIFIC TO THIS CASE
!        KMIN = 1.D-10
        KMIN = 1.D-16
! END OF PART SPECIFIC TO THIS CASE
        EMIN = 1.D-16
        KMAX = 1.D4
        EMAX = 1.D10
      ELSEIF(ITURBV.EQ.7) THEN
        KMIN = 1.D-8
        EMIN = 1.D-3
        KMAX = 1.D-1
        EMAX = 1.D4
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE KEPCL3
!                    *****************
!
     &(KBORF,EBORF,LIKBOF,LIEBOF,LIUBOF,
     & KBORL,EBORL,LIKBOL,LIEBOL,LIUBOL,RUGOL,
     & KBORS,EBORS,LIKBOS,LIEBOS,LIUBOS,
     & DISBOR,AK,U,V,H,Z,NBOR,NPOIN2,NPLAN,NPTFR,
     & DNUVIH,DNUVIV,KARMAN,CMU,LISRUF,LISRUL,
     & VIRT,KMIN,KMAX,EMIN,EMAX,KENT,KENTU,KSORT,KADH,KLOG,
     & UETCAR,UETCAL,FICTIF)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES KBOR, EBOR AND AUBOR WHEN THE TURBULENCE
!+                MODEL IS K-EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)     ; V. BOYER UMIST
!+        04/01/2010
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |-->| TURBULENT ENERGY
!| CMU            |-->| CONSTANT FOR MODELE K-EPSILON MODEL
!| DISBOR         |-->| DISTANCE TO BOUNDARY OF POINTS CLOSE TO BOUNDARY
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EBORF          |<->| EPSILON ON BOTTOM
!| EBORL          |<->| EPSILON ON LATERAL SOLID BOUNDARIES
!| EBORS          |<->| EPSILON AT SURFACE
!| EMAX           |-->| MAXIMUM VALUE FOR EPSILON WHEN CLIPPING
!| EMIN           |-->| MINIMUM VALUE FOR EPSILON WHEN CLIPPING
!| H              |-->| WATER DEPTH AT TIME N
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KARMAN         |-->| KARMAN CONSTANT
!| KBORF          |<->| K ON BOTTOM
!| KBORL          |<->| K ON LATERAL SOLID BOUNDARIES
!| KBORS          |<->| K AT SURFACE
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KLOG           |-->| CONVENTION FOR LOGARITHMIC WALL
!| KMAX           |-->| MAXIMUM VALUE FOR K WHEN CLIPPING
!| KMIN           |-->| MINIMUM VALUE FOR K WHEN CLIPPING
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIEBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE BOTTOM
!| LIEBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON ON THE LATERAL WALLS
!| LIEBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE SURFACE
!| LIKBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE BOTTOM
!| LIKBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON K ON THE LATERAL WALLS
!| LIKBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE SURFACE
!| LISRUF         |-->| TURBULENCE MODEL FOR BOTTOM
!|                |   | 1: SMOOTH  2: ROUGH  3: ROUGH (CHEZY)
!| LISRUL         |-->| TURBULENCE MODEL FOR SOLID BOUNDARIES
!|                |   | 1: SMOOTH  2: ROUGH  3: ROUGH (CHEZY)
!| LIUBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON U AT THE BOTTOM
!| LIUBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON U ON THE LATERAL WALLS
!| LIUBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON U AT THE SURFACE
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!| RUGOL          |-->| NOT USED
!| U              |-->| X COMPONENT OF VELOCITY AT TIME N
!| UETCAL         |-->| (UETUTA*UTANG(IPTFR))**2: IN COMMENT
!| UETCAR         |-->| USTAR**2
!| V              |-->| Y COMPONENT OF VELOCITY AT TIME N
!| VIRT           |-->| VIRTUAL ORIGIN FOR EPSILON (TELEMAC 3D): COMMENT
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY: IPBOT,AEBORF,BEBORF,SIGMAE,RUGOF
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2,KENTU
      INTEGER, INTENT(IN) :: LISRUF,LISRUL,KENT,KSORT,KADH,KLOG
!
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR,NPLAN)
      INTEGER, INTENT(INOUT) :: LIUBOF(NPOIN2), LIUBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIUBOL(NPTFR,NPLAN)
!
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN), V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: Z(NPOIN2,NPLAN), AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: H(NPOIN2)      , UETCAR(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: KBORF(NPOIN2), KBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: KBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORF(NPOIN2), EBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: UETCAL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN)    :: DISBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: RUGOL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN) :: VIRT, DNUVIH, DNUVIV, KARMAN
      DOUBLE PRECISION, INTENT(IN) :: CMU
      DOUBLE PRECISION, INTENT(IN) :: KMIN, KMAX, EMIN, EMAX
      DOUBLE PRECISION, INTENT(IN) :: FICTIF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN2,IP,IBOT
!
      DOUBLE PRECISION ESURF,HAUT,Z0,SSQCMU,DIST,PROPNU,DISTFOND
!
      DOUBLE PRECISION, PARAMETER :: NIVTURB = 0.005D0
      DOUBLE PRECISION, PARAMETER :: TESTREICH = 1.D-4
      INTEGER, PARAMETER :: MAXITEREICH = 30
! BEGIN OF PART SPECIFIC TO THIS CASE
      DOUBLE PRECISION, PARAMETER :: FICTIFUET = 2.D0
      DOUBLE PRECISION D_SURF
! END OF PART SPECIFIC TO THIS CASE
!
      INTRINSIC SQRT,MAX,LOG
!
!-----------------------------------------------------------------------
!
      SSQCMU = 1.D0 /SQRT(CMU)
      PROPNU = (2*DNUVIH + DNUVIV) /3.D0
!
!=======================================================================
!     BOTTOM
!=======================================================================
!
!     THIS IS DONE IN LIMI3D
!     AEBORF%TYPR='0'
!     BEBORF%TYPR='0'
!     AKBORF%TYPR='0'
!     BKBORF%TYPR='0'
!
      DO IPOIN2=1,NPOIN2
!
        IF(IPBOT%I(IPOIN2).EQ.0) THEN
!         NORMAL CASE
          DIST =(Z(IPOIN2,2)-Z(IPOIN2,1))/FICTIF
          IF(LIEBOF(IPOIN2).EQ.KENT) THEN
            EBORF(IPOIN2)=MAX(UETCAR(IPOIN2)*SQRT(UETCAR(IPOIN2))
     &                                             /(KARMAN*DIST),EMIN)
          ENDIF
          IF(LIKBOF(IPOIN2).EQ.KENT) THEN
            KBORF(IPOIN2) = MAX(SSQCMU*UETCAR(IPOIN2),KMIN)
          ENDIF
        ELSE
!         RISK OF SMASHED PLANES OR TIDAL FLATS
          IPLAN=IPBOT%I(IPOIN2)+1
          IF(IPLAN.EQ.NPLAN) THEN
!           CASE OF TIDAL FLATS
            IF(LIEBOF(IPOIN2).EQ.KENT) THEN
              EBORF(IPOIN2)=EMIN
            ENDIF
!           IN THIS CASE KBORF COMPUTED ABOVE MAY YIELD
!           ABNORMAL VALUES OF VISCOSITY
            IF(LIKBOF(IPOIN2).EQ.KENT) KBORF(IPOIN2)=KMIN
          ELSE
!           CASE OF SMASHED PLANES : DIST COMPUTED ON FIRST FREE LAYER
            DIST =(Z(IPOIN2,IPLAN+1)-Z(IPOIN2,IPLAN))/FICTIF
            IF(LIEBOF(IPOIN2).EQ.KENT) THEN
              EBORF(IPOIN2)=MAX(UETCAR(IPOIN2)*
     &                         SQRT(UETCAR(IPOIN2))/(KARMAN*DIST),EMIN)
            ENDIF
            IF(LIKBOF(IPOIN2).EQ.KENT) THEN
              KBORF(IPOIN2) = MAX(SSQCMU*UETCAR(IPOIN2),KMIN)
            ENDIF
          ENDIF
        ENDIF
!
      ENDDO
!
!=======================================================================
!     FREE SURFACE
!=======================================================================
!
      DO IPOIN2=1,NPOIN2
!
!       DIRICHLET ON EPSILON
!       ---------------------
!
        IF(LIEBOS(IPOIN2).EQ.KENT) THEN
!
!         NEZU & NAKAGAWA: TURBULENCE IN OPEN CHANNEL FLOWS
!
!         SEE KEPICL: THIS VALUE IS NOT USED IF CONDITION
!                     IS NOT SET TO KENT ON EPSILON
          HAUT = MAX(H(IPOIN2),1.D-7)
          ESURF = SQRT(AK(IPOIN2,NPLAN)**3) / (0.18D0*HAUT)
!
!         5.9
!         ESURF = CMU**0.75D0 / KARMAN
!    &          * AK(IPOIN2,NPLAN)**1.5D0 / (VIRT*HAUT)
          EBORS(IPOIN2) = MAX(ESURF,EMIN)
!
        ENDIF
!
      ENDDO
!
!=======================================================================
!     LATERAL BOUNDARIES
!=======================================================================
!
      DO IPTFR=1,NPTFR
!
        IPOIN2 = NBOR(IPTFR)
        DIST   = DISBOR(IPTFR) / FICTIF
        HAUT   = MAX(H(IPOIN2),1.D-7)
!
        Z0=RUGOF%R(IPOIN2)/30.D0
!
        DO IPLAN=1,NPLAN
!
! BEGIN OF PART SPECIFIC TO THIS CASE
!         IP=MAX(IPLAN,2)
!         IBOT=MIN(IPBOT%I(IPOIN2)+1,NPLAN-1)
!         DISTANCE TO BOTTOM (WILL BE 0 WITH TIDAL FLATS)
!         DISTFOND = (Z(IPOIN2,IP)-Z(IPOIN2,IBOT))
!
          IF(IPLAN.EQ.1.OR.IPLAN.EQ.(NPLAN+1)/2) THEN
            DISTFOND =  (Z(IPOIN2,2)-Z(IPOIN2,1)) / FICTIFUET
          ELSEIF(IPLAN.GT.((NPLAN+1)/2)) THEN
!EGR DISTANCE BETWEEN 2 LAYERS
            DISTFOND =  Z(IPOIN2,IPLAN)-Z(IPOIN2,(NPLAN+1)/2)
!EGR POURQUOI EST-CE QU ON DIVISE PAR LA HAUTEUR DE LA COUCHE DU HAUT ?
            D_SURF = (Z(IPOIN2,NPLAN)-Z(IPOIN2,IPLAN))/
     &               (Z(IPOIN2,NPLAN)-Z(IPOIN2,(NPLAN+1)/2))
          ELSE
!EGR MINIMUM DISTANCE BETWEEN BOTTOM AND UPPER LAYER
            DISTFOND = MIN(Z(IPOIN2,(NPLAN+1)/2)-Z(IPOIN2,IPLAN),
     &                     Z(IPOIN2,IPLAN)-Z(IPOIN2,1))
          ENDIF
! END OF PART SPECIFIC TO THIS CASE
!
!         DIRICHLET ON K
!         ---------------
!
          IF(LIKBOL(IPTFR,IPLAN).EQ.KENT) THEN
!         ------------------------------------
!
!           ************************************
            IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN
!           ************************************
!
!          THEORY BY VINCENT BOYER (SEE ALSO KEPINI)
!
!          KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U(IPOIN2,IPLAN)**2,KMIN)
!
! BEGIN OF PART SPECIFIC TO THIS CASE
              IF(IPLAN.GT.(NPLAN+1)/2) THEN
                KBORL(IPTFR,IPLAN) = MAX(NIVTURB*
     &                                  U(IPOIN2,(NPLAN+1)/2)**2*D_SURF,
     &                                   KMIN)
              ELSE
                KBORL(IPTFR,IPLAN) = MAX(NIVTURB*U(IPOIN2,1)**2,KMIN)
              ENDIF
!
!          NO TURBULENCE
!
!           KBORL(IPTFR,IPLAN) = KMIN
!           CV  HANS AND BURCHARD CL FOR K
!            KBORL(IPTFR,IPLAN)=UETCAR(IPOIN2)
!     &                        *(1.D0-DISTFOND/HAUT)/SQRT(CMU)
! END OF PART SPECIFIC TO THIS CASE
!
!            ****************************************
            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &             LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!           ****************************************
!
!             WALL
!
!             KBORL(IPTFR,IPLAN)=MAX(SSQCMU*UETCAL(IPTFR,IPLAN),KMIN)
              KBORL(IPTFR,IPLAN)=KMIN
!
!           ****
            ELSE
!           ****
!
              IF (LNG.EQ.1) WRITE(LU,111) IPTFR,LIUBOL(IPTFR,IPLAN)
              IF (LNG.EQ.2) WRITE(LU,112) IPTFR,LIUBOL(IPTFR,IPLAN)
              CALL PLANTE(1)
              STOP
!
!           *****
            ENDIF
!           *****
!
          ENDIF
!         -----
!
!         DIRICHLET ON EPSILON
!         ---------------------
!
          IF(LIEBOL(IPTFR,IPLAN).EQ.KENT) THEN
!         ------------------------------------
!
!           ************************************
            IF(LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KENTU.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KSORT     ) THEN
!           ************************************
!
!              COMING IN THE DOMAIN: TURBULENCE DUE TO THE
!              BOTTOM AS IN KEPINI; COMPUTES EBORL ACCORDING
!              TO KBORL AT THE BOTTOM
! BEGIN OF PART SPECIFIC TO THIS CASE
              EBORL(IPTFR,IPLAN)=CMU**0.75*SQRT(KBORL(IPTFR,1)**3)
     &                          /KARMAN/MAX(DISTFOND,1.D-6)
              EBORL(IPTFR,IPLAN)= MAX(EBORL(IPTFR,IPLAN),EMIN)
!               EBORL(IPTFR,IPLAN)=EMIN
!             Hans et Burchard
!             CV ...
!              EBORL(IPTFR,IPLAN)=SQRT(UETCAR(IPOIN2))**3
!     &                          *(1.D0-DISTFOND/HAUT)
!     &                          /KARMAN/MAX(DISTFOND,Z0)
! END OF PART SPECIFIC TO THIS CASE
!
!           ****************************************
            ELSEIF(LIUBOL(IPTFR,IPLAN).EQ.KLOG .OR.
     &             LIUBOL(IPTFR,IPLAN).EQ.KADH) THEN
!           ****************************************
!
!             WALL
!
!             EBORL(IPTFR,IPLAN) =
!    &        MAX(UETCAL(IPTFR,IPLAN)*SQRT(UETCAL(IPTFR,IPLAN))/
!    &        (KARMAN*DIST*FICTIFUET/FICTIFEPS),EMIN)
              EBORL(IPTFR,IPLAN)=EMIN
!
!           ****
            ELSE
!           ****
!
!             OTHER
!
              IF (LNG.EQ.1) WRITE(LU,121) IPTFR,LIUBOL(IPTFR,IPLAN)
              IF (LNG.EQ.2) WRITE(LU,122) IPTFR,LIUBOL(IPTFR,IPLAN)
              CALL PLANTE(1)
              STOP
!
!           *****
            ENDIF
!           *****
!
          ENDIF
!          -----
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
111   FORMAT(' KEPCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR KBOR : LIUBOR =',I6)
112   FORMAT(' KEPCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR KBOR : LIUBOR =',I6)
121   FORMAT(' KEPCL3 : POINT DE BORD',I6,
     &       ' - CAS NON PREVU POUR EBOR : LIUBOR =',I6)
122   FORMAT(' KEPCL3 : BOUNDARY NODE',I6,
     &       ' - UNEXPECTED CONDITION FOR EBOR : LIUBOR =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE KEPICL
!                    *****************
!
     & (LIKBOF,LIEBOF,LIUBOF,LIKBOL,LIEBOL,LIUBOL,LIKBOS,LIEBOS,LIUBOS,
     &  NPTFR,NPLAN,NPOIN2,KENT,KSORT,KADH,KLOG,KENTU)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION
!+                SOURCE TERM STEP OF THE K-EPSILON MODEL.
!+
!+            CASE.
!
!warning  LIKBOR AND LIEBOR ARE BUILT FROM LIUBOR
!
!history  L. VAN HAREN (LNH)
!+        25/11/97
!+        V6P0
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KLOG           |-->| CONVENTION FOR LOGARITHMIC SOLID BOUNDARY
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIEBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE BOTTOM
!| LIEBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON ON THE LATERAL WALLS
!| LIEBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON EPSILON AT THE SURFACE
!| LIKBOF         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE BOTTOM
!| LIKBOL         |<->| TYPE OF BOUNDARY CONDITIONS ON K ON THE LATERAL WALLS
!| LIKBOS         |<->| TYPE OF BOUNDARY CONDITIONS ON K AT THE SURFACE
!| LIUBOF         |-->| TYPE OF BOUNDARY CONDITIONS ON U AT THE BOTTOM
!| LIUBOL         |-->| TYPE OF BOUNDARY CONDITIONS ON U ON THE LATERAL WALLS
!| LIUBOS         |-->| TYPE OF BOUNDARY CONDITIONS ON U AT THE SURFACE
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY : LIMKF,LIMEF,LIMKS,LIMES
      USE DECLARATIONS_SPECIAL
!
      USE BIEF
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN)    :: KENT, KSORT, KADH, KLOG, KENTU
      INTEGER, INTENT(IN)    :: LIUBOF(NPOIN2), LIUBOS(NPOIN2)
      INTEGER, INTENT(IN)    :: LIUBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR*NPLAN*2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPTFR, IPOIN2,NPTFR3
!
!-----------------------------------------------------------------------
!
!     LATERAL BOUNDARIES :
!
!-----------------------------------------------------------------------
!
      NPTFR3=NPLAN*NPTFR
!
      DO IPTFR=1,NPTFR3
        IF(LIUBOL(IPTFR).EQ.KENT) THEN
          LIKBOL(IPTFR) = KENT
          LIEBOL(IPTFR) = KENT
        ELSE
          LIKBOL(IPTFR) = KSORT
          LIEBOL(IPTFR) = KSORT
        ENDIF
!       SAVING VALUES IN THE SECOND DIMENSION (SEE POINT_TELEMAC3D)
        LIKBOL(IPTFR+NPTFR3) = KSORT
        LIEBOL(IPTFR+NPTFR3) = KSORT
      ENDDO
!
!-----------------------------------------------------------------------
!
!     BOTTOM
!
!-----------------------------------------------------------------------
!
      IF(LIMKF.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LIKBOF(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIUBOF(IPOIN2).EQ.KSORT) THEN
            LIKBOF(IPOIN2) = KSORT
          ELSE
            LIKBOF(IPOIN2) = KENT
          ENDIF
        ENDDO
      ENDIF
!
      IF(LIMEF.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LIEBOF(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIUBOF(IPOIN2).EQ.KSORT) THEN
            LIEBOF(IPOIN2) = KSORT
          ELSE
! BEGIN OF PART SPECIFIC TO THIS CASE
!           LIEBOF(IPOIN2) = KENT
            LIEBOF(IPOIN2) = KSORT
! END OF PART SPECIFIC TO THIS CASE
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FREE SURFACE
!
!-----------------------------------------------------------------------
!
      IF(LIMKS.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LIKBOS(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          LIKBOS(IPOIN2) = KSORT
        ENDDO
      ENDIF
!
      IF(LIMES.EQ.2) THEN
        DO IPOIN2 = 1,NPOIN2
          LIEBOS(IPOIN2) = KENT
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          LIEBOS(IPOIN2) = KSORT
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
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
!history  C.-T. PHAM (LNHE)
!+        03/09/2015
!+        V7P1
!+   Change in the number of arguments when calling CONDI_TPXO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CONDIM => CONDIM
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE DECLARATIONS_SPECIAL
      USE TPXO
!
      IMPLICIT NONE
! BEGIN OF PART SPECIFIC TO THIS CASE
      INTEGER ITRAC,NFO1
      DOUBLE PRECISION TEMP0,TEMP1,FROUD
! END OF PART SPECIFIC TO THIS CASE
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
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
     &                  MARDAT,MARTIM,INTMICON,MSL,
     &                  TIDALTYPE,BOUNDARY_COLOUR,ICALHWG)
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
        TRANSF_PLANE%I(IPLAN)=1
      ENDDO
!
!     OTHER EXAMPLES:
!
!     EXAMPLE 1: ALL PLANES WITH PRESCRIBED ELEVATION
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=3
!     ENDDO
!     ZPLANE%R(2)=-7.D0
!     ZPLANE%R(3)=-4.D0
!     ...
!     ZPLANE%R(NPLAN-1)=-0.05D0
!
!
!     EXAMPLE 2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     ZSTAR%R(1)=0.D0
!     ZSTAR%R(2)=0.02D0
!     ZSTAR%R(3)=0.1D0
!     ...
!     ZSTAR%R(NPLAN-1)=0.95D0
!     ZSTAR%R(NPLAN)=1.D0
!
!
!     EXAMPLE 3: ONE PLANE (NUMBER 4) WITH PRESCRIBED ELEVATION
!                AND SIGMA ELSEWHERE
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=1
!     ENDDO
!     TRANSF_PLANE%I(4)=3
!     ZPLANE%R(4)=-3.D0
!
!
!     EXAMPLE 4: ONE PLANE WITH PRESCRIBED ELEVATION
!                AND 2 SIGMA TRANSFORMATIONS, WITH NPLAN=7
!                SIGMA TRANSFORMATIONS ARE MEANT BETWEEN
!                BOTTOM, FIXED ELEVATION PLANES AND FREE SURFACE
!                THE VALUES OF ZSTAR ARE LOCAL FOR EVERY
!                SIGMA TRANSFORMATION: 0. FOR LOWER FIXED PLANE
!                                      1. FOR UPPER FIXED PLANE
!
!     DO IPLAN = 1,7
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     TRANSF_PLANE%I(4)=3
!     ZPLANE%R(4)=3.D0
!     ZSTAR%R(2)=0.2D0
!     ZSTAR%R(3)=0.8D0
!     ZSTAR%R(5)=0.1D0
!     ZSTAR%R(6)=0.9D0
!
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
! BEGIN OF PART SPECIFIC TO THIS CASE
!        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=C     ' , X=U, C=0.05D0 )
! END OF PART SPECIFIC TO THIS CASE
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
! BEGIN OF PART SPECIFIC TO THIS CASE
!      IF(NTRAC.GT.0) THEN
!        DO I=1,NTRAC
!          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
!        ENDDO
!      ENDIF
      NFO1 = T3D_FILES(T3DFO1)%LU
      REWIND NFO1
      READ(NFO1,*) FROUD
      TEMP0 = 20.D0
      IF(ABS(FROUD-0.9D0).LT.1.D-5) THEN
        TEMP1 = 25.3485028D0
      ELSEIF (ABS(FROUD-1.6D0).LT.1.D-5) THEN
        TEMP1 = 21.8663052D0
      ELSEIF (ABS(FROUD-5.0D0).LT.1.D-5) THEN
        TEMP1 = 20.2009931D0
      ELSE
        TEMP1 = 4.D0+SQRT((TEMP0-4.D0)**2
     &                    +0.0333D0**2/(9.81D0*7.D-6*0.1D0*FROUD**2))
      ENDIF

      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              J=NPOIN2*(IPLAN-1)+I
              IF(IPLAN.GT.(NPLAN+1)/2) THEN
                TA%ADR(ITRAC)%P%R(J) = TEMP1
              ELSE
                TA%ADR(ITRAC)%P%R(J) = TEMP0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
! END OF PART SPECIFIC TO THIS CASE
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
                     SUBROUTINE BORD3D
!                    *****************
!
     &(TIME,LT,ENTET,NPTFR2_DIM,NFRLIQ)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    SPECIFIC BOUNDARY CONDITIONS.
!
!note     1) FOR PRESCRIBED BOUNDARIES OF POINT BEING BOTH LATERAL
!+            AND BOTTOM : USE LATERAL ARRAYS.
!+
!+     2) FOR TYPES OF BOUNDARY CONDITIONS : USE SUBROUTINE LIMI3D.
!+
!+     3) SEDIMENT IS THE LAST TRACER.
!
!warning  MAY BE MODIFIED BY THE USER
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
!history  J.-M. HERVOUET (LNHE)
!+        19/09/2011
!+        V6P2
!+   Call to DEBIMP3D replaced by CALL DEBIMP_3D (new arguments)
!
!history  J.-M. HERVOUET (LNHE)
!+        11/03/2013
!+        V6P3
!+   Test IFRLIQ.NE.0 line 210.
!
!history  C. VILLARET & T. BENSON (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   Case IPROF.EQ.3 added to test IPROF.EQ.2.
!
!history  A. GINEAU, N. DURAND, N. LORRAIN, C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Adding the heat balance of exchange with atmosphere
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS
!|                |   | CONSERVATION.
!| LT             |-->| CURRENT TIME STEP NUMBER
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPTFR2_DIM     |-->| NPTFR2? NOT USED
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, EX_NFRLIQ=>NFRLIQ
      USE DECLARATIONS_WAQTEL, ONLY: TAIR,HREL,NEBU,RO0,CP_EAU,
     &                               ATMOSEXCH,WAQPROCESS
      USE INTERFACE_TELEMAC3D, EX_BORD3D => BORD3D
      USE DECLARATIONS_SPECIAL
      USE EXCHANGE_WITH_ATMOSPHERE
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     TIME AND ENTET ARE AT AND INFOGR (NOW IN DECLARATIONS_TELEMAC3D)
      DOUBLE PRECISION, INTENT(IN)    :: TIME
      INTEGER         , INTENT(IN)    :: LT
      LOGICAL         , INTENT(IN)    :: ENTET
      INTEGER         , INTENT(IN)    :: NPTFR2_DIM
      INTEGER         , INTENT(IN)    :: NFRLIQ
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,NP,IBORD,IVIT,ICOT,IDEB,IFRLIQ,IPROF,K,N
      INTEGER IPTFR,ITRAC,IPLAN,I3D
      LOGICAL YAZMIN
      DOUBLE PRECISION ROEAU,ROAIR,VITV,PROFZ,WINDRELX,WINDRELY
!
! BEGIN OF PART SPECIFIC TO THIS CASE
      DOUBLE PRECISION DZ,Z_MF,UET1,UET2,H_MF,S_MF1,S_MF2,XI_S,U_MF1,
     &                 U_MF2
      DOUBLE PRECISION FROUD,TEMP0,TEMP1
      INTEGER NFO1
! END OF PART SPECIFIC TO THIS CASE
!
      DOUBLE PRECISION P_DMIN,P_DSUM
      INTEGER  P_IMAX
      EXTERNAL P_IMAX,P_DMIN,P_DSUM
      DOUBLE PRECISION STA_DIS_CUR
      EXTERNAL STA_DIS_CUR
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION ZMIN(MAXFRO)
!
      INTEGER YADEB(MAXFRO),MSK1,IJK
!
!     NORMALS TO THE BED
      DOUBLE PRECISION XNB,YNB,ZNB
!
!     SIMPLE CASES FOR LATERAL BOUNDARIES ARE TREATED AUTOMATICALLY:
!
!     - PRESCRIBED DEPTH     (5 4 4)
!     - PRESCRIBED VELOCITY  (  6 6)
!     - PRESCRIBED DISCHARGE (  5 5)
!
!     CORRESPONDING KEYWORDS ARE:
!
!     'PRESCRIBED ELEVATIONS' OR 'COTES IMPOSEES'
!     'PRESCRIBED VELOCITIES' OR 'VITESSES IMPOSEES'
!     'PRESCRIBED FLOWRATES' OR 'DEBITS IMPOSES'
!
!     THE IMPLEMENTATION OF AUTOMATIC CASES MAY BE CANCELLED
!     PROVIDED THAT THE RELEVANT ARRAYS ARE FILLED
!
!
!***********************************************************************
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!              AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!=======================================================================
!
!     SECURES NO SLIP BOUNDARY CONDITIONS
!
      IF(LT.EQ.1) THEN
!
!     VELOCITIES
!
      DO IPTFR = 1,NPTFR2
        IPOIN2 = NBOR2%I(IPTFR)
        DO IPLAN = 1,NPLAN
          IBORD = (IPLAN-1)*NPTFR2 + IPTFR
          IF(LIUBOL%I(IBORD).EQ.KADH) UBORL%R(IBORD) = 0.D0
          IF(LIVBOL%I(IBORD).EQ.KADH) VBORL%R(IBORD) = 0.D0
          IF(LIWBOL%I(IBORD).EQ.KADH) WBORL%R(IBORD) = 0.D0
        ENDDO
      ENDDO
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIUBOF%I(IPOIN2).EQ.KADH) UBORF%R(IPOIN2) = 0.D0
        IF(LIVBOF%I(IPOIN2).EQ.KADH) VBORF%R(IPOIN2) = 0.D0
        IF(LIWBOF%I(IPOIN2).EQ.KADH) WBORF%R(IPOIN2) = 0.D0
        IF(LIUBOS%I(IPOIN2).EQ.KADH) UBORS%R(IPOIN2) = 0.D0
        IF(LIVBOS%I(IPOIN2).EQ.KADH) VBORS%R(IPOIN2) = 0.D0
        IF(LIWBOS%I(IPOIN2).EQ.KADH) WBORS%R(IPOIN2) = 0.D0
      ENDDO
!
!     IMPORTANT OPTION:
!     VERTICAL VELOCITIES ARE SET AS HORIZONTAL VELOCITIES
!     THIS IS AN OPTION, OTHERWISE LIWBOL=KSORT (SEE LIMI3D)
!
!     DO IPTFR = 1,NPTFR2
!       IPOIN2 = NBOR2%I(IPTFR)
!       DO IPLAN = 1,NPLAN
!         IBORD = (IPLAN-1)*NPTFR2 + IPTFR
!         LIWBOL%I(IBORD)= LIUBOL%I(IBORD)
!         IF(LIWBOL%I(IBORD).EQ.KENT) WBORL%R(IBORD) = 0.D0
!       ENDDO
!     ENDDO
!
!     TRACERS
!
!     IF(NTRAC.NE.0) THEN
!
!       DO ITRAC = 1,NTRAC
!
!         DO IPTFR = 1,NPTFR2
!           IPOIN2 = NBOR2%I(IPTFR)
!           LITABF%ADR(ITRAC)%P%I(IPOIN2) = KSORT (DOES NOT WORK WITH SEDIMENT)
!           LITABS%ADR(ITRAC)%P%I(IPOIN2) = KSORT
!           DO IPLAN = 1,NPLAN
!             IBORD = (IPLAN-1)*NPTFR2 + IPTFR
!             IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KADH)
!    &           TABORL%ADR(ITRAC)%P%R(IBORD) = 0.D0
!           ENDDO
!         ENDDO
!
!         DO IPOIN2 = 1,NPOIN2
!           IF(LITABF%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
!    &                       TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           IF(LITABS%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
!    &                       TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!         ENDDO
!
!       ENDDO
!
!     ENDIF
!
      ENDIF
!
!=======================================================================
!  FOR ALL TIMESTEPS
!=======================================================================
!
!     IF VELOCITY PROFILE OPTION 5: MINIMUM ELEVATION OF EVERY BOUNDARY
!
      YAZMIN=.FALSE.
      DO IFRLIQ=1,NFRLIQ
        ZMIN(IFRLIQ)=1.D99
        IF(PROFVEL(IFRLIQ).EQ.5) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR2
          IFRLIQ=NUMLIQ%I(K)
          IPOIN2=NBOR2%I(K)
          IF(IFRLIQ.NE.0) THEN
            ZMIN(IFRLIQ)=MIN(ZMIN(IFRLIQ),ZF%R(IPOIN2)+H%R(IPOIN2))
          ENDIF
        ENDDO
        IF(NCSIZE.GT.1) THEN
          DO IFRLIQ=1,NFRLIQ
            ZMIN(IFRLIQ)=P_DMIN(ZMIN(IFRLIQ))
          ENDDO
        ENDIF
      ENDIF
!
!     INITIALISES YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
      IDEB=0
      ICOT=0
      IVIT=0
!
!     LOOP ON ALL 2D BOUNDARY POINTS
!
      DO K=1,NPTFR2
!
!     PRESCRIBED ELEVATION GIVEN IN STEERING FILE (NCOTE<>0)
!     -------------------------------------------------------
!
      IF(LIHBOR%I(K).EQ.KENT.AND.NCOTE.NE.0) THEN
!
        IPOIN2 = NBOR2%I(K)
        ICOT=NUMLIQ%I(K)
        IF(STA_DIS_CURVES(ICOT).EQ.1) THEN
          HBOR%R(K) = STA_DIS_CUR(ICOT,FLUX_BOUNDARIES(ICOT),
     &                            PTS_CURVES(ICOT),QZ,NFRLIQ,
     &                            ZF%R(IPOIN2)+H%R(IPOIN2))
     &                - ZF%R(IPOIN2)
          HBOR%R(K) = MAX(0.D0,HBOR%R(K))
        ELSEIF(NCOTE.GE.NUMLIQ%I(K)) THEN
          N=IPOIN2
          IF(NCSIZE.GT.1) N=MESH2D%KNOLG%I(N)
          HBOR%R(K) = SL3(ICOT,AT,N,INFOGR)-ZF%R(IPOIN2)
          HBOR%R(K) = MAX(0.D0,HBOR%R(K))
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) NUMLIQ%I(K)
100       FORMAT(1X,'BORD3D : COTES IMPOSEES EN NOMBRE INSUFFISANT',/,
     &           1X,'         DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'         IL EN FAUT AU MOINS : ',1I6,/,
     &           1X,'         AUTRE POSSIBILITE :',/,
     &           1X,'         FICHIER DES COURBES DE TARAGE MANQUANT')
          IF(LNG.EQ.2) WRITE(LU,101) NUMLIQ%I(K)
101       FORMAT(1X,'BORD3D: MORE PRESCRIBED ELEVATIONS ARE REQUIRED',/,
     &           1X,'        IN THE PARAMETER FILE',/,
     &           1X,'        AT LEAST ',1I6,' MUST BE GIVEN',/,
     &           1X,'        OTHER POSSIBILITY:',/,
     &           1X,'        STAGE-DISCHARGE CURVES FILE MISSING')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
      ENDDO
!
!     PRESCRIBED DISCHARGE GIVEN IN STEERING FILE (NDEBIT<>0)
!     --------------------------------------------------------
!
      DO K=1,NPTFR2
!
!     A VELOCITY PROFILE IS SET HERE AND WILL BE CORRECTED LATER
!     TO GET THE CORRECT DISCHARGE (CALL TO DEBIMP3D)
!
      IF(LIUBOL%I(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
!
!
! BEGIN OF PART SPECIFIC TO THIS CASE
!     INITIALISATION FOR PROFILE SPECIFICATION
        S_MF1=0.D0
        S_MF2=0.D0
! END OF PART SPECIFIC TO THIS CASE
        IPOIN2 = NBOR2%I(K)
        DO NP=1,NPLAN
          IJK=(NP-1)*NPTFR2+K
          I3D=(NP-1)*NPOIN2+IPOIN2
          IFRLIQ=NUMLIQ%I(K)
          IF(PROFVEL(IFRLIQ).EQ.2) THEN
!           GIVEN BY USER IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = VBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.3) THEN
!           NORMAL AND NORM GIVEN BY UBOR IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = -XNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = -YNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.4) THEN
!           NORMAL AND PROPORTIONAL TO SQRT(H)
            UBORL%R(IJK)=-XNEBOR2%R(K) * SQRT(MAX(H%R(IPOIN2),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) * SQRT(MAX(H%R(IPOIN2),0.D0))
          ELSEIF(PROFVEL(IFRLIQ).EQ.5) THEN
!           NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
!           DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
            UBORL%R(IJK)=-XNEBOR2%R(K) *
     &                   SQRT(MAX(ZMIN(IFRLIQ)-ZF%R(IPOIN2),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) *
     &                   SQRT(MAX(ZMIN(IFRLIQ)-ZF%R(IPOIN2),0.D0))
          ELSE
!           NORMAL AND NORM 1
            UBORL%R(IJK)=-XNEBOR2%R(K)
            VBORL%R(IJK)=-YNEBOR2%R(K)
          ENDIF
!         NO VELOCITY IF NO WATER
          IF(H%R(IPOIN2).LT.1.D-4) THEN
            UBORL%R(IJK) = 0.D0
            VBORL%R(IJK) = 0.D0
          ENDIF
!         CASE OF A VERTICAL PROFILE
! BEGIN OF PART SPECIFIC TO THIS CASE
!     LOG PROFILE PRESCRIBED
          VERPROVEL(IFRLIQ)=2
          IF(VERPROVEL(IFRLIQ).NE.1) THEN
!     STEP PROFILE
!            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
!     &                       AT,LT,NP,INFOGR,VERPROVEL(IFRLIQ))
            H_MF=0.1D0
            DZ=2.D0*H_MF/DBLE(NPLAN-1.D0)

            U_MF1=1.D0/30.D0
            U_MF2=2.D0/30.D0

            XI_S=0.0001D0

            IF(NP.GT.(NPLAN+1)/2) THEN
              UBORL%R(IJK) = U_MF1
              VBORL%R(IJK) = 0.D0

              Z_MF=DBLE(NP-(NPLAN+1)/2)*DZ
              S_MF1=S_MF1+LOG(Z_MF/XI_S)+8.5D0
            ELSE
              UBORL%R(IJK) = U_MF2
              VBORL%R(IJK) = 0.D0
              Z_MF=DBLE(NP-1.D0)*DZ
              IF(NP.NE.1.AND.NP.NE.(NPLAN+1)/2) THEN
                S_MF2 = S_MF2+LOG(MIN(Z_MF,H_MF-Z_MF)/XI_S)+8.5D0
              ELSE
                UBORL%R(IJK)=0.D0
                S_MF2 = S_MF2+LOG(DZ*0.1D0/XI_S)+8.5D0
              ENDIF
            ENDIF
          ENDIF
        ENDDO
!
!     FIXING UET1 AND UET2
!
        UET1=0.41D0*DBLE((NPLAN-1)/2)*U_MF1/S_MF1
        UET2=0.41D0*DBLE((NPLAN-1)/2)*U_MF2/S_MF2
!
        DO NP=1,NPLAN
          IJK=(NP-1)*NPTFR2+K
          IF(NP.GT.((NPLAN+1)/2)) THEN
            Z_MF=DBLE(NP-(NPLAN+1)/2)*DZ
            UBORL%R(IJK) = UET1/0.41D0*(LOG(Z_MF/XI_S)+8.5D0)
!               WRITE(LU,*)'NP =',NP,'U =',UBORL%R(IJK)
          ELSEIF(NP.NE.1.AND.NP.NE.(NPLAN+1)/2) THEN
            Z_MF=DBLE(NP-1.D0)*DZ
            UBORL%R(IJK) = UET2/0.41D0*(LOG(MIN(Z_MF,H_MF-Z_MF)/XI_S)
     &                                 +8.5D0)
          ELSE
            UBORL%R(IJK) = UET2/0.41D0*(LOG(DZ*0.1D0/XI_S)+8.5D0)
          ENDIF
!
!          IF(VERPROVEL(IFRLIQ).NE.1) THEN
!            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
!     &                       AT,LT,NP,INFOGR,VERPROVEL(IFRLIQ))
!            UBORL%R(IJK) = UBORL%R(IJK)*PROFZ
!            VBORL%R(IJK) = VBORL%R(IJK)*PROFZ
! END OF PART SPECIFIC TO THIS CASE
!         U AND V INITIALISED WITH PRESCRIBED VALUES (FOR DEBIMP3D)
!         WILL BE CHANGED AGAIN AFTER DEBIMP3D
          U%R(I3D)=UBORL%R(IJK)
          V%R(I3D)=VBORL%R(IJK)
        ENDDO
!
        YADEB(NUMLIQ%I(K))=1
!
      ENDIF
!
      ENDDO
!
!     PRESCRIBED VELOCITY GIVEN IN STEERING FILE (NVIT<>0)
!     -----------------------------------------------------
!
      DO K=1,NPTFR2
!
!     THIS VELOCITY IS CONSIDERED NORMAL TO THE BOUNDARY
!
      IF(LIUBOL%I(K).EQ.KENTU.AND.NVIT.NE.0) THEN
        IVIT=NUMLIQ%I(K)
        IF(NVIT.GE.IVIT) THEN
          DO NP=1,NPLAN
            IBORD = (NP-1)*NPTFR2+K
            IF(NCSIZE.GT.1) THEN
              N=MESH2D%KNOLG%I(NBOR2%I(K))+(NP-1)*NPOIN2
            ELSE
              N=NBOR3%I(IBORD)
            ENDIF
            UBORL%R(IBORD)=-MESH2D%XNEBOR%R(K)*VIT3(IVIT,AT,N,INFOGR)
            VBORL%R(IBORD)=-MESH2D%YNEBOR%R(K)*VIT3(IVIT,AT,N,INFOGR)
            WBORL%R(IBORD)=0.D0
          ENDDO
        ELSE
          IF(LNG.EQ.1) WRITE(LU,200) NUMLIQ%I(K)
200       FORMAT(1X,'BORD3D : VITESSES IMPOSEES EN NOMBRE INSUFFISANT',
     &           /,1X,'       DANS LE FICHIER DES PARAMETRES',
     &           /,1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,201) NUMLIQ%I(K)
201       FORMAT(1X,'BORD3D : MORE PRESCRIBED VELOCITIES ARE REQUIRED',
     &           /,1X,'       IN THE PARAMETER FILE',
     &           /,1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      ENDDO
!
!     PRESCRIBED TRACER GIVEN IN STEERING FILE,
!     BUT POSSIBLE OVERWRITING IF LIQUID BOUNDARY FILE IS GIVEN
!     SEE FUNCTION TR3
!     -------------------------------------------------------
!
! BEGIN OF PART SPECIFIC TO THIS CASE
! EGR MODIF BD MODIF TO AVOID TO ENTER IN princi.f, TEMP0 =
! TEMPERATURE OF COFLOW DENENDS ON FROUD
! TAKES THE VALUE DEFINED IN THE FILE FROUD.TXT
      NFO1 = T3D_FILES(T3DFO1)%LU
      REWIND NFO1
      READ(NFO1,*) FROUD
      TEMP0 = 20.D0
      IF(ABS(FROUD-0.9D0).LT.1.D-5) THEN
        TEMP1 = 25.3485028D0
      ELSEIF (ABS(FROUD-1.6D0).LT.1.D-5) THEN
        TEMP1 = 21.8663052D0
      ELSEIF (ABS(FROUD-5.0D0).LT.1.D-5) THEN
        TEMP1 = 20.2009931D0
      ELSE
        TEMP1 = 4.D0+SQRT((TEMP0-4.D0)**2
     &                    +0.0333D0**2/(9.81D0*7.D-6*0.1D0*FROUD**2))
      ENDIF
! END OF PART SPECIFIC TO THIS CASE
!
!
      IF(NTRAC.GT.0.AND.NTRACER.GT.0) THEN
        DO ITRAC=1,NTRAC
        DO K=1,NPTFR2
        DO NP=1,NPLAN
          IBORD = (NP-1)*NPTFR2+K
          IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KENT) THEN
            IFRLIQ=NUMLIQ%I(K)
            IF(IFRLIQ.EQ.0) THEN
              IF(LNG.EQ.1) WRITE(LU,298) IBORD
298           FORMAT(1X,'BORD3D : VALEURS IMPOSEES DU TRACEUR',/,
     &               1X,'         SUR PAROI SOLIDE',/,
     &               1X,'         AU POINT DE BORD ',1I6)
              IF(LNG.EQ.2) WRITE(LU,299) IBORD
299           FORMAT(1X,'BORD3D: PRESCRIBED TRACER VALUE',/,
     &               1X,'        ON A SOLID BOUNDARY',/,
     &               1X,'        AT BOUNDARY POINT ',1I6)
              CALL PLANTE(1)
              STOP
            ENDIF
            IF(NTRACER.GE.IFRLIQ*NTRAC) THEN
              IF(NCSIZE.GT.1) THEN
                N=MESH2D%KNOLG%I(NBOR2%I(K))+(NP-1)*NPOIN2
              ELSE
                N=NBOR3%I(IBORD)
              ENDIF
              TABORL%ADR(ITRAC)%P%R(IBORD)=
     &                                   TR3(IFRLIQ,ITRAC,N,AT,INFOGR)
            ELSE
              IF(LNG.EQ.1) WRITE(LU,300) NUMLIQ%I(K)*NTRAC
300           FORMAT(1X,'BORD3D : VALEURS IMPOSEES DU TRACEUR',/,
     &               1X,'         EN NOMBRE INSUFFISANT',/,
     &               1X,'         DANS LE FICHIER DES PARAMETRES',/,
     &               1X,'         IL EN FAUT AU MOINS : ',1I6)
              IF(LNG.EQ.2) WRITE(LU,301) NUMLIQ%I(K)
301           FORMAT(1X,'BORD3D: MORE PRESCRIBED TRACER VALUES',/,
     &               1X,'        ARE REQUIRED IN THE PARAMETER FILE',/,
     &               1X,'        AT LEAST ',1I6,' MUST BE GIVEN')
              CALL PLANTE(1)
              STOP
            ENDIF
!           CASE OF A PROFILE ON THE VERTICAL
            IPROF=VERPROTRA(ITRAC+(IFRLIQ-1)*NTRAC)
            IF(IPROF.NE.1) THEN
              PROFZ=TRA_PROF_Z(IFRLIQ,NBOR2%I(K),AT,LT,NP,
     &                         INFOGR,IPROF,ITRAC)
              IF(IPROF.EQ.2.OR.IPROF.EQ.0) THEN
!               Rouse concentrations profiles (IPROF=2) or values given by user (IPROF=0)
                TABORL%ADR(ITRAC)%P%R(IBORD)=PROFZ
              ELSEIF(IPROF.EQ.3) THEN
!               Normalised concentrations profiles (IPROF=3)
                TABORL%ADR(ITRAC)%P%R(IBORD)=
     &          TABORL%ADR(ITRAC)%P%R(IBORD)*PROFZ
              ELSE
                WRITE(LU,*) 'BORD3D : IPROF=',IPROF
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'OPTION INCONNUE POUR LES'
                  WRITE(LU,*) 'PROFILS DES TRACEURS SUR LA VERTICALE'
                ENDIF
                IF(LNG.EQ.2) THEN
                  WRITE(LU,*) 'UNKNOWN OPTION FOR THE'
                  WRITE(LU,*) 'TRACERS VERTICAL PROFILES'
                ENDIF
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDIF
! BEGIN OF PART SPECIFIC TO THIS CASE
!    LINEAR STRATIFICATION AT THE ENTRANCE
            IF(NP.GT.(NPLAN+1)/2.AND.ITRAC.EQ.1) THEN
              TABORL%ADR(ITRAC)%P%R(IBORD) = TEMP1
            ENDIF
! END OF PART SPECIFIC TO THIS CASE
          ENDIF
!
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
      IF(NBEDFLO.GT.0) THEN
!
!       PRESCRIBED FLOWRATES ON THE BED GIVEN BY THE USER
!       -------------------------------------------------
!
        CALL VECTOR(T2_01,'=','MASBAS          ',IELM2H,1.D0,
     &              WBORF,WBORF,WBORF,WBORF,WBORF,WBORF,MESH2D,
     &              .FALSE.,MASKEL)
!
!       FIND THE AREA OF EACH BOUNDARY
        DO IFRLIQ=1,NBEDFLO
          BEDQAREA(IFRLIQ) = 0.D0
        ENDDO
!
        DO K=1,NPOIN2
          IF(LIWBOF%I(K).EQ.KENT) THEN
            IFRLIQ=NLIQBED%I(K)
            IF(IFRLIQ.GT.0) THEN
              BEDQAREA(IFRLIQ) = BEDQAREA(IFRLIQ) + T2_01%R(K)
            ENDIF
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) THEN
          DO IFRLIQ = 1 , NBEDFLO
            BEDQAREA(IFRLIQ)=P_DSUM(BEDQAREA(IFRLIQ))
          ENDDO
        ENDIF
!
        DO IFRLIQ = 1 , NBEDFLO
          IF(BEDQAREA(IFRLIQ).LE.0.D0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'BORD3D : FRONTIERE DU FOND ',IFRLIQ
              WRITE(LU,*) '         AVEC SURFACE EGALE A : ',
     &                              BEDQAREA(IFRLIQ)
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'BORD3D: BOUNDARY ON THE BOTTOM: ',IFRLIQ
              WRITE(LU,*) '        WITH AREA EQUAL TO : ',
     &                             BEDQAREA(IFRLIQ)
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
      IF(TIDALTYPE.GE.1) CALL TIDAL_MODEL_T3D()
!
!-----------------------------------------------------------------------
!
!     PRESCRIBED DISCHARGES: FINAL TREATMENT OF VELOCITIES
!     ----------------------------------------------------
!
!     LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
      DO IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.NE.0) THEN
!
        MSK1=1
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
           IF(YADEB(IFRLIQ).EQ.1) THEN
           CALL DEBIMP_3D(Q3(IFRLIQ,AT,INFOGR),
     &                    UBORL%R,VBORL%R,WBORL%R,
     &                    U,V,NUMLIQ%I,NUMLIQ_ELM%I,IFRLIQ,T3_02,
     &                    NPTFR2,NETAGE,MASK_3D%ADR(MSK1)%P,
     &                    MESH3D,EQUA,IELM2V,SVIDE,MASKTR,
     &                    MESH3D%NELEB)
           ENDIF
          ELSE
          IF(LNG.EQ.1) WRITE(LU,400) IFRLIQ
400       FORMAT(1X,'BORD3D : DEBITS IMPOSES',/,
     &           1X,'       EN NOMBRE INSUFFISANT',/,
     &           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) IFRLIQ
401       FORMAT(1X,'BORD3D : MORE PRESCRIBED FLOWRATES',/,
     &           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     &           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      ENDDO ! IFRLIQ
      ENDIF
!
!     RESETS BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
!     AND OTHER SUBROUTINES BEFORE THE NEXT BOUNDARY CONDITIONS TREATMENT)
!
      DO K=1,NPTFR2
        IF(LIUBOL%I(K).EQ.KENT) THEN
          DO NP=1,NPLAN
            IJK=(NP-1)*NPTFR2+K
            U%R((NP-1)*NPOIN2+NBOR2%I(K))=UBORL%R(IJK)
            V%R((NP-1)*NPOIN2+NBOR2%I(K))=VBORL%R(IJK)
          ENDDO
        ENDIF
      ENDDO
!
!     EXAMPLE OF PRESCRIBED VERTICAL VELOCITIES AT ENTRANCES
!     VELOCITIES TANGENT TO BOTTOM AND FREE SURFACE
!
!     DO K=1,NPTFR2
!       IF(LIWBOL%I(K).EQ.KENT.OR.LIWBOL%I(K).EQ.KENTU) THEN
!         DO NP=1,NPLAN
!             IJK=(NP-1)*NPTFR2+K
!             I2D=NBOR2%I(K)
!             I3D=(NP-1)*NPOIN2+I2D
!             WBORL DEDUCED FROM FREE SURFACE AND BOTTOM
!             TETA=(Z(I3D)-Z(I2D))/
!    *        MAX(1.D-3,Z((NPLAN-1)*NPOIN2+I2D)-Z(I2D))
!             GX=        TETA *GRADZN%ADR(1)%P%R(I2D)
!    *            +(1.D0-TETA)*GRADZF%ADR(1)%P%R(I2D)
!             GY=        TETA *GRADZN%ADR(2)%P%R(I2D)
!    *            +(1.D0-TETA)*GRADZF%ADR(2)%P%R(I2D)
!             WBORL%R(IJK)=UBORL%R(IJK)*GX+VBORL%R(IJK)*GY
!         ENDDO
!       ENDIF
!     ENDDO
!
!     PRESCRIBED FLOWRATES ON THE BED: FINAL TREATMENT
!     --------------------------------------------------------
!
      IF(NBEDFLO.GT.0) THEN
!
        DO K=1,NPOIN2
!
!         CORRECT THE VELOCITY PROFILES BY DIVIDING THE FLOW RATE WITH
!         THE CROSS-SECTIONAL AREA OVER WHICH IT WILL BE IMPOSED
!
          IF(LIWBOF%I(K).EQ.KENT) THEN
            IFRLIQ=NLIQBED%I(K)
            IF(IFRLIQ.GT.0) THEN
!             GRADZF IS THE GRADIENT OF THE BED, I.E. OUTWARD NORMAL
!             THE Z COMPONENT IS ASSUMED TO BE ALWAYS NEGATIVE
              XNB=GRADZF%ADR(1)%P%R(K)
              YNB=GRADZF%ADR(2)%P%R(K)
              ZNB=-SQRT(1.D0-XNB**2-YNB**2)
!             NO OUTFLOW IF NO WATER
              IF(H%R(K).LT.1.D-4.AND.BEDFLO(IFRLIQ).LE.0.D0) THEN
                UBORF%R(K)=0.D0
                VBORF%R(K)=0.D0
                WBORF%R(K)=0.D0
              ELSE
                UBORF%R(K)=-XNB*BEDFLO(IFRLIQ)/BEDQAREA(IFRLIQ)
                VBORF%R(K)=-YNB*BEDFLO(IFRLIQ)/BEDQAREA(IFRLIQ)
                WBORF%R(K)=-ZNB*BEDFLO(IFRLIQ)/BEDQAREA(IFRLIQ)
              ENDIF
            ENDIF
          ENDIF
!
        ENDDO ! NPOIN2
!
      ENDIF ! IF(NBEDFLO.GT.0)
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!           END OF AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                               WIND
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
      IF(VENT) THEN
        ROEAU = 1000.D0
        ROAIR = 1.3D0
        DO IPOIN2 = 1,NPOIN2
!         RELATIVE WIND
          WINDRELX=WIND%ADR(1)%P%R(IPOIN2)-U%R(NPOIN3-NPOIN2+IPOIN2)
          WINDRELY=WIND%ADR(2)%P%R(IPOIN2)-V%R(NPOIN3-NPOIN2+IPOIN2)
          VITV=SQRT(WINDRELX**2+WINDRELY**2)
!         A MORE ACCURATE TREATMENT
!         IF(VITV.LE.5.D0) THEN
!           FAIR = ROAIR/ROEAU*0.565D-3
!         ELSEIF (VITV.LE.19.22D0) THEN
!           FAIR = ROAIR/ROEAU*(-0.12D0+0.137D0*VITV)*1.D-3
!         ELSE
!           FAIR = ROAIR/ROEAU*2.513D-3
!         ENDIF
!         BEWARE : BUBORS IS VISCVI*DU/DN, NOT DU/DN
          IF(H%R(IPOIN2).GT.HWIND) THEN
!           EXPLICIT PART
            BUBORS%R(IPOIN2) =  FAIR*VITV*WIND%ADR(1)%P%R(IPOIN2)
            BVBORS%R(IPOIN2) =  FAIR*VITV*WIND%ADR(2)%P%R(IPOIN2)
!           IMPLICIT PART
            AUBORS%R(IPOIN2) = -FAIR*VITV
            AVBORS%R(IPOIN2) = -FAIR*VITV
          ELSE
            BUBORS%R(IPOIN2) = 0.D0
            BVBORS%R(IPOIN2) = 0.D0
            AUBORS%R(IPOIN2) = 0.D0
            AVBORS%R(IPOIN2) = 0.D0
          ENDIF
        ENDDO
      ENDIF
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                         END OF WIND TREATMENT
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                     HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!                 LINES BELOW ARE AN EXAMPLE
!                                    =======
!    TO BE GIVEN :
!
!    TAIR  = AIR TEMPERATURE WHICH MAY VARY WITH TIME
!    SAL   = SALINITY WHICH MAY VARY WITH TIME
!
!     EXCHANGE WITH ATMOSPHERE CORRESPONDS NOW TO WAQPROCESS=5
      IF(INCLUS(COUPLING,'WAQTEL').AND.WAQPROCESS.EQ.5)THEN
!       IMPORTANT:
!       STATES THAT ATABOS AND BTABOS ARE NOT ZERO (SEE LIMI3D AND DIFF3D)
!       OTHERWISE THEY WILL NOT BE CONSIDERED
        ATABOS%ADR(IND_T)%P%TYPR='Q'
        BTABOS%ADR(IND_T)%P%TYPR='Q'
!
        CALL CALCS3D_THERMICS(NPOIN2,NPOIN3,IND_T,IND_S,TA,ATABOS,
     &                        BTABOS,PATMOS,ATMOSEXCH,WIND,LISTIN)
      ENDIF
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                 END OF HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
! BEGIN OF PART SPECIFIC TO THIS CASE
!     NEUMANN FOR EPS AT THE BOTTOM
      IF(ITURBV.EQ.3.OR.ITURBH.EQ.3) THEN
        DO IPOIN2=1,NPOIN2
          DZ=MESH3D%Z%R(IPOIN2+NPOIN2)-MESH3D%Z%R(IPOIN2)
          BEBORF%R(IPOIN2)=0.D0
     &           +4.D0*UETCAR%R(IPOIN2)**1.5D0/KARMAN/DZ**2
     &           *VISCVI%ADR(3)%P%R(IPOIN2)
          BEBORF%TYPR='Q'
        ENDDO
      ENDIF
! END OF PART SPECIFIC TO THIS CASE
!
!-----------------------------------------------------------------------
!
!     OPTIMISATION:
!
!     EXPLICIT STRESSES WILL NOT BE TREATED IF SAID TO BE 0
!
!     EXPLICIT STRESSES SET TO 0 ON VELOCITIES (UNLESS PROGRAMMED
!                                               IN THIS SUBROUTINE):
!
      BUBORF%TYPR='0'
      BUBORL%TYPR='0'
      BVBORF%TYPR='0'
      BVBORL%TYPR='0'
      BWBORF%TYPR='0'
      BWBORL%TYPR='0'
      BWBORS%TYPR='0'
!
!     CASE OF WIND (SEE ABOVE)
!
      IF(VENT) THEN
        BUBORS%TYPR='Q'
        BVBORS%TYPR='Q'
        AUBORS%TYPR='Q'
        AVBORS%TYPR='Q'
      ELSE
        BUBORS%TYPR='0'
        BVBORS%TYPR='0'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ****************
                     SUBROUTINE UTIMP
!                    ****************
!
     & (LT,TIME,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRINTS OUT SPECIFIC RESULTS
!+               (SEE THE LIST OF VARIABLES IN DECLARATIONS_TELEMAC3D).
!+
!+            FOR BIEF_OBJ STRUCTURES, THE DOUBLE PRECISION ARRAY
!+                IS IN COMPONENT R, E.G. U%R FOR THE VELOCITY.
!
!history  C LE NORMANT(LNH)    ; F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P2
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| GRADEBL        |-->| NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
!| GRAPRDL        |-->| GRAPHIC PRINTOUT PERIOD
!| LISDEBL        |-->| NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
!| LISPRDL        |-->| LISTING PRINTOUT PERIOD
!| LT             |-->| ITERATION NUMBER
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER, INTENT(IN) :: LT,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC MOD

!
!***********************************************************************
! USER OUTPUT ACCORDING TO THE VALUES: TO BE IMPLEMENTED
!
!
! BEGIN OF PART SPECIFIC TO THIS CASE
      IF(LT.EQ.NIT) THEN
        CALL WRITE_NRFO(T3D_FILES(T3DRFO)%LU,VARSO3,TEXT3,NPOIN3,
     &                  NPOIN2,NPLAN,MAXVA3,X,Y)
      ENDIF
! END OF PART SPECIFIC TO THIS CASE
!
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP
!                    *********************
                     SUBROUTINE WRITE_NRFO
!                    *********************
!
     &(NRFO,VARSOR,TEXT,NPOIN3,NPOIN2,NPLAN,MAXTAB,X,Y)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!
!brief    WRITES IN A FORMATTED FILE THE VELOCITIES U AND THE SCALAR
!         AT 3 POINTS OF THE SPACE
!         THIS WRITING IS NECESSARY TO DRAW COMPARISON CURVES
!         WITH PIERRE-LOUIS VIOLLET S EXPERIENCES
!
!history  B. DELHOM (INCKA PREST)
!+        15/10/2009
!+        V6P0
!+   Creation IN VERSION 5.9
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAXTAB         |-->| MAXIMUM NUMBER OF VARIABLES
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| NRFO           |-->| LOGICAL UNIT FOR FORMATTED RESULT FILE
!| VARSOR         |<->| RESULTS OF THE COMPUTATION
!| TEXT           |-->| NAMES FO VARIABLES OF THE COMPUTATION
!| X              |-->| ABSCISSA COORDONATES
!| Y              |-->| ORDINATE COORDONATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NRFO,NPOIN3,NPOIN2,NPLAN,MAXTAB
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN3), Y(NPOIN3)
!
      CHARACTER(LEN=32), INTENT(IN) :: TEXT(MAXTAB)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARSOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVAR,I,IPOIN2,IPLAN,INUM(3),I0,IT,IU
      DOUBLE PRECISION EPS,Z_RED,TEMP(NPLAN),U0(NPLAN),T_MIN,T_MAX,U1,
     &                 DTEMP
      DOUBLE PRECISION VITU1(NPLAN),VITU2(NPLAN),VITU3(NPLAN),
     &                 TEMP1(NPLAN),TEMP2(NPLAN),TEMP3(NPLAN)
!
      CHARACTER(LEN=9) TEXT1
!
      DATA EPS / 1.D-6 /
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
      I0 = 0
      INUM(1) = 0
      INUM(2) = 0
      INUM(3) = 0
!
      DO IPLAN=1,NPLAN
        U0(IPLAN)   = 0.D0
        TEMP(IPLAN) = 0.D0
        VITU1(IPLAN) = 0.D0
        VITU2(IPLAN) = 0.D0
        VITU3(IPLAN) = 0.D0
        TEMP1(IPLAN) = 0.D0
        TEMP2(IPLAN) = 0.D0
        TEMP3(IPLAN) = 0.D0
      ENDDO
!
      DO IPOIN2=1,NPOIN2
        IF ((ABS(X(IPOIN2)-0.D0) .LT.EPS).AND.
     &      (ABS(Y(IPOIN2)-0.5D0).LT.EPS)) I0 = IPOIN2
        IF ((ABS(X(IPOIN2)-1.D0) .LT.EPS).AND.
     &      (ABS(Y(IPOIN2)-0.5D0).LT.EPS)) INUM(1) = IPOIN2
        IF ((ABS(X(IPOIN2)-3.D0) .LT.EPS).AND.
     &      (ABS(Y(IPOIN2)-0.5D0).LT.EPS)) INUM(2) = IPOIN2
        IF ((ABS(X(IPOIN2)-10.D0).LT.EPS).AND.
     &      (ABS(Y(IPOIN2)-0.5D0).LT.EPS)) INUM(3) = IPOIN2
      ENDDO

      DO IVAR=1,MAXTAB
        TEXT1 = TEXT(IVAR)
        IF (TEXT1=='VITESSE U') IU = IVAR
        IF (TEXT1=='TEMPERATU') IT = IVAR
      ENDDO

      IF(I0.NE.0) THEN
        DO IPLAN=1,NPLAN
          U0(IPLAN)   = VARSOR%ADR(IU)%P%R(I0+(IPLAN-1)*NPOIN2)
          TEMP(IPLAN) = VARSOR%ADR(IT)%P%R(I0+(IPLAN-1)*NPOIN2)
        ENDDO
      ENDIF
      T_MIN = MINVAL(TEMP)
      T_MAX = MAXVAL(TEMP)
!
      IF(NCSIZE.GT.1) THEN
        T_MIN = P_DMAX(T_MIN) + P_DMIN(T_MIN)
        T_MAX = P_DMAX(T_MAX) + P_DMIN(T_MAX)
      ENDIF
!
      DTEMP = T_MAX-T_MIN
!      U1    = MAXVAL(U0)/2.D0
      U1    = 1.D0/30.D0
!
      IF(INUM(1).NE.0) THEN
        DO IPLAN=1,NPLAN
          VITU1(IPLAN) = VARSOR%ADR(IU)%P%R(INUM(1)+(IPLAN-1)*NPOIN2)/U1
          TEMP1(IPLAN) = ( VARSOR%ADR(IT)%P%R(INUM(1)+(IPLAN-1)*NPOIN2)
     &                    -T_MIN)/DTEMP
        ENDDO
      ENDIF
!
      IF(INUM(2).NE.0) THEN
        DO IPLAN=1,NPLAN
          VITU2(IPLAN) = VARSOR%ADR(IU)%P%R(INUM(2)+(IPLAN-1)*NPOIN2)/U1
          TEMP2(IPLAN) = ( VARSOR%ADR(IT)%P%R(INUM(2)+(IPLAN-1)*NPOIN2)
     &                    -T_MIN)/DTEMP
        ENDDO
      ENDIF
!
      IF(INUM(3).NE.0) THEN
        DO IPLAN=1,NPLAN
          VITU3(IPLAN) = VARSOR%ADR(IU)%P%R(INUM(3)+(IPLAN-1)*NPOIN2)/U1
          TEMP3(IPLAN) = ( VARSOR%ADR(IT)%P%R(INUM(3)+(IPLAN-1)*NPOIN2)
     &                    -T_MIN)/DTEMP
        ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        DO IPLAN=1,NPLAN
          VITU1(IPLAN) = P_DMAX(VITU1(IPLAN)) +  P_DMIN(VITU1(IPLAN))
          VITU2(IPLAN) = P_DMAX(VITU2(IPLAN)) +  P_DMIN(VITU2(IPLAN))
          VITU3(IPLAN) = P_DMAX(VITU3(IPLAN)) +  P_DMIN(VITU3(IPLAN))
          TEMP1(IPLAN) = P_DMAX(TEMP1(IPLAN)) +  P_DMIN(TEMP1(IPLAN))
          TEMP2(IPLAN) = P_DMAX(TEMP2(IPLAN)) +  P_DMIN(TEMP2(IPLAN))
          TEMP3(IPLAN) = P_DMAX(TEMP3(IPLAN)) +  P_DMIN(TEMP3(IPLAN))
        ENDDO
      ENDIF
!
      DO IPLAN=1,NPLAN
        Z_RED = (IPLAN-1)
        Z_RED = 2.D0*DBLE(IPLAN-1)/DBLE(NPLAN-1)
        WRITE(NRFO,1001)Z_RED,
     &                   VITU1(IPLAN),VITU2(IPLAN),VITU3(IPLAN),
     &                   TEMP1(IPLAN),TEMP2(IPLAN),TEMP3(IPLAN)
      ENDDO


 1001 FORMAT(F7.4,F7.4,F7.4,F7.4,F7.4,F7.4,F7.4)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
