!                    *****************
                     SUBROUTINE FLUROE
!                    *****************
!
     &(W,FLUSCE,NUBO,VNOIN,WINF,FLUX,FLUSORT,FLUENT,
     & NELEM,NSEG,NPTFR,NPOIN,X,Y,AIRS,ZF,EPS,DDMIN,G,
     & XNEBOR,YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,FLBOR)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES FLUXES OF ROE TYPE (INTERNAL AND BOUNDARY FLUXES)
!
!history  N.GOUTAL
!+        19/08/1994
!+        V5P2
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
!| AIRS           |-->| CELL AREA
!| DDMIN          |<--| MINIMUM DISTANCE 
!| EPS            |-->| TOLERANCE
!| FLUENT         |<--| MASS FLUX MASSE INLET 
!| FLUSCE         |-->| SOURCE FLUXES
!| FLUSORT        |<--| MASS FLUX MASSE OUTLET 
!| FLUX           |<--| ROE FLUX
!| G              |-->| GRAVITY
!| KDDL           |-->| CONVENTION FOR FREE POINTS (BC)
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINTS
!| KFROT          |-->| BED FRICTION LAW 
!| KNEU           |-->| CONVENTION NEUMANN POINTS
!| LIMPRO         |-->| TYPES OF BOUNDARY CONDITION!
!| NBOR           |-->| NUMER OF BORD NODES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPTFR          |-->| TOTAL NUMBER OF BOUNDARY NODES
!| NREJET         |-->| NUMBER OF SOURCE/SINK
!| NSEG           |-->| NUMBER OF EDGES
!| NUBO           |-->| GLOBAL INDICES OF EDGE EXTREMITIES
!| VNOIN          |-->| NORMAL TO THE INTERFACE
!|                |   | (2 FIRS COMPOSANTES) AND 
!|                |   | SEGMENT LENGTH (3RD COMPONENT)
!| W              |<->| WORKING TABLE
!| WINF           |-->| PRESCRIBED VALUES AT THE INLET AND OUTLET
!| X,Y            |-->| COORDINATES FOR MESH NODES
!| XNEBOR,YNEBOR  |-->| NORMAL TO BOUNDARY POINTS
!| ZF             |-->| BED TOPOGRAPHY (BATHYMETRY)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FLUROE => FLUROE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NELEM,NSEG,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN) :: NUBO(2,*),LIMPRO(NPTFR,6),NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: AIRS(NPOIN),ZF(NPOIN),VNOIN(3,*)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: WINF(3,NPTFR),G,EPS
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),DDMIN
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSCE(3,NPOIN),FLUSORT,FLUENT
! RA
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IEL,ISEGIN,IEL1,IEL2,INDIC,K,KPRINT,IELM(2)
!
      DOUBLE PRECISION DIJ,FLULOC(3),INFLOW,OUTFLOW
      DOUBLE PRECISION D1,HI,UI,VI,HJ,VJ,UJ,XN,YN,RNORM,CT,PRI,USN
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
!------
! 1. INITIALISATIONS
!------
!
      D1 = 0.3D0
!
      FLULOC(1) = 0.D0
      FLULOC(2) = 0.D0
      FLULOC(3) = 0.D0
!
      DO 20  IEL =  1 , NPOIN
        FLUX(IEL,1) = 0.D0
        FLUX(IEL,2) = 0.D0
        FLUX(IEL,3) = 0.D0
        FLUSCE(1,IEL) = 0.D0
        FLUSCE(2,IEL) = 0.D0
        FLUSCE(3,IEL) = 0.D0
20    CONTINUE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!------
! 2. COMPUTE INTERNAL FLUXES
!------
!
!  LOOP OVER INTERNAL SEGMENTS
!       ----------------------------------
!
        DDMIN = 10000.D0
!
        DO 100 ISEGIN = 1 , NSEG
!
         IEL1 = NUBO(1,ISEGIN)
         IEL2 = NUBO(2,ISEGIN)
         KPRINT =0
       INDIC =0
!
!
!   --->    INTERMEDIATE COMPUTATIONS
!           ------------------------------
!
       HI = W(1,IEL1)
!
       IF(HI.GT.EPS) THEN
         UI = W(2,IEL1) / HI
         VI = W(3,IEL1) / HI
       ELSE
         INDIC = INDIC + 1
         UI = 0.D0
         VI = 0.D0
         HI = 0.D0
       ENDIF
!
       HJ = W(1,IEL2)
!
       IF(HJ.GT.EPS) THEN
         UJ = W(2,IEL2) / HJ
         VJ = W(3,IEL2) / HJ
       ELSE
         INDIC = INDIC + 1
         UJ = 0.D0
         VJ = 0.D0
         HJ = 0.D0
       ENDIF
!
       DIJ = SQRT((X(IEL1)-Y(IEL2))**2+(Y(IEL1)-Y(IEL2))**2)
       IF(DIJ.LE.DDMIN) THEN
         DDMIN=DIJ
         IELM(1)=IEL1
         IELM(2)=IEL2
       ENDIF
       RNORM = VNOIN(3,ISEGIN)
!
       IF(INDIC.LT.2) THEN
         XN = VNOIN (1,ISEGIN)
         YN = VNOIN (2,ISEGIN)
!
       CALL FLUXE(HJ,UJ,VJ,HI,UI,VI,XN,YN,RNORM,G,FLULOC)
!
       CALL FLUSRC(IEL1,IEL2,ISEGIN,VNOIN,W,FLUSCE,X,Y,AIRS,NPOIN,
     &              NSEG,ZF,EPS,G)
!
          ELSE
        FLULOC(1)= 0.D0
        FLULOC(2)= 0.D0
        FLULOC(3)= 0.D0
        FLUSCE(1,IEL1)=0.D0
        FLUSCE(2,IEL1)=0.D0
        FLUSCE(3,IEL1)=0.D0
        FLUSCE(1,IEL2)=0.D0
        FLUSCE(2,IEL2)=0.D0
        FLUSCE(3,IEL2)=0.D0
!
         ENDIF
!
         FLUX(IEL1,1)=FLUX(IEL1,1)+FLULOC(1)*RNORM+FLUSCE(1,IEL1)
         FLUX(IEL1,2)=FLUX(IEL1,2)+FLULOC(2)*RNORM+FLUSCE(2,IEL1)
         FLUX(IEL1,3)=FLUX(IEL1,3)+FLULOC(3)*RNORM+FLUSCE(3,IEL1)
         FLUX(IEL2,1)=FLUX(IEL2,1)-FLULOC(1)*RNORM+FLUSCE(1,IEL2)
         FLUX(IEL2,2)=FLUX(IEL2,2)-FLULOC(2)*RNORM+FLUSCE(2,IEL2)
         FLUX(IEL2,3)=FLUX(IEL2,3)-FLULOC(3)*RNORM+FLUSCE(3,IEL2)
!
 100     CONTINUE
!
!
!------
! 3. COMPUTE FLUXES AT THE BOUNDARIES
!------
!
!        START AT ZERO
!
         FLUSORT = 0.D0
         FLUENT  = 0.D0
!
! ====>   INLET FLUX
!
      DO 200 K = 1 , NPTFR
!
      IEL = NBOR(K)
      INDIC = 0
!
      IF(LIMPRO(K,1).EQ.KDIR.OR.LIMPRO(K,1).EQ.KDDL) THEN
!
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!   --->    INTERMEDIATE COMPUTATIONS
!           -------------------------------
!
!     SI H IMPOSEE
      IF(LIMPRO(K,1).EQ.KDIR) THEN
       HJ = WINF(1,K)
       IF(HJ.GT.EPS) THEN
         UJ = WINF(2,K) / HJ
         VJ = WINF(3,K) / HJ
       ELSE
         HJ = 0.D0
         UJ = 0.D0
         VJ = 0.D0
         INDIC = INDIC+1
       ENDIF
       HI = W(1,IEL)
       IF(HI.GT.EPS) THEN
         UI = W(2,IEL) / HI
         VI = W(3,IEL) / HI
       ELSE
         HI = 0.D0
         UI = 0.D0
         VI = 0.D0
         INDIC = INDIC+1
       ENDIF
       XN = XNEBOR(K)
       YN = YNEBOR(K)
       RNORM = SQRT( XNEBOR(K+NPTFR)**2 + YNEBOR(K+NPTFR)**2 )
!
       IF(INDIC.LT.2) THEN
       CALL FLUXE(HJ,UJ,VJ,HI,UI,VI,XN,YN,RNORM,G,FLULOC)
       OUTFLOW = FLULOC(1)*RNORM
       FLUSORT = FLUSORT + OUTFLOW
       FLBOR%R(K)=OUTFLOW
       ELSE
        FLULOC(1)= 0.D0
        FLULOC(2)= 0.D0
        FLULOC(3)= 0.D0
        FLBOR%R(K)=0.0D0
       ENDIF
!
! RA: LIMPRO .NE. KDIR
       ELSE
       HI = WINF(1,K)
       IF(HI.GT.EPS) THEN
         UI = WINF(2,K) / HI
         VI = WINF(3,K) / HI
       ELSE
         HI = 0.D0
         UI = 0.D0
         VI = 0.D0
         INDIC = INDIC+1
       ENDIF
       HJ = W(1,IEL)
       IF(HJ.GT.EPS) THEN
         UJ = W(2,IEL) / HJ
         VJ = W(3,IEL) / HJ
       ELSE
         HJ = 0.D0
         UJ = 0.D0
         VJ = 0.D0
         INDIC = INDIC+1
       ENDIF
       XN = XNEBOR(K)
       YN = YNEBOR(K)
       RNORM = SQRT( XNEBOR(K+NPTFR)**2 + YNEBOR(K+NPTFR)**2 )
!
       IF(INDIC.LT.2) THEN
!
!
       CALL FLUXE(HI,UI,VI,HJ,UJ,VJ,XN,YN,RNORM,G,FLULOC)
!
! RA
         INFLOW = FLULOC(1)*RNORM
         FLUENT = FLUENT + INFLOW
!RA
         FLBOR%R(K)=INFLOW
!
          ELSE
        FLULOC(1)= 0.D0
        FLULOC(2)= 0.D0
        FLULOC(3)= 0.D0
        FLBOR%R(K)=0.0D0
!
         ENDIF
         ENDIF
!
         FLUX(IEL,1)=FLUX(IEL,1)+FLULOC(1)*RNORM
         FLUX(IEL,2)=FLUX(IEL,2)+FLULOC(2)*RNORM
         FLUX(IEL,3)=FLUX(IEL,3)+FLULOC(3)*RNORM
!
!------
! 5. FLUX AT THE BOUNDARY
!------
!
       ELSEIF(LIMPRO(K,1).EQ.KNEU) THEN
!
!
        IF(W(1,IEL).GT.0.) THEN
          PRI =G*( W(1,IEL)*W(1,IEL))/2.D0
          USN = (W(2,IEL)*XNEBOR(K)+W(3,IEL)*YNEBOR(K))/W(1,IEL)
          CT = SQRT(G*W(1,IEL))
!
          IF((USN+2.D0*CT).LE.0.D0) THEN
!GODUNOV
             PRI =0.D0
!
          ELSEIF(((USN+2.D0*CT).GE.0.D0) .AND. (USN.LE.0.D0)) THEN
!GODUNOV
             PRI = PRI * (1.D0 +  USN / 2.D0 / CT)
     &                 * (1.D0 +  USN / 2.D0 / CT)
     &                 * (1.D0 +  USN / 2.D0 / CT)
     &                 * (1.D0 +  USN / 2.D0 / CT)
!
          ELSEIF(USN.GE.0.D0) THEN
!VFROENC
             PRI = PRI*(1.D0 + 2.D0 * USN / CT)
!
          ENDIF
!
          FLUX(IEL,2) = FLUX(IEL,2) + XNEBOR(K+NPTFR) * PRI
          FLUX(IEL,3) = FLUX(IEL,3) + YNEBOR(K+NPTFR) * PRI
!
      ENDIF
!
!
!CCCCCCCCCCCCC
!
       ENDIF
200   CONTINUE
!
!-----------------------------------------------------------------------
!
 1000 FORMAT(' LE SEGMENT ',I2,' NE CORRESPOND PAS A UNE ENTREE ')
 1010 FORMAT(' LE SEGMENT ',I2,' NE CORRESPOND PAS A UNE SORTIE ')
 1001 FORMAT(' VALEUR DE < U , N > : ',E12.5)
!
      RETURN
      END
