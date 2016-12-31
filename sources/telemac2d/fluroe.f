!                    *****************
                     SUBROUTINE FLUROE
!                    *****************
!
     &(W,FLUSCE,NUBO,VNOIN,WINF,FLUX,FLUSORT,FLUENT,
     & NELEM,NSEG,NPTFR,NPOIN,X,Y,AIRS,ZF,EPS,DDMIN,G,
     & XNEBOR,YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,FLBOR,
     & ELTSEG,IFABOR,MESH)
!
!***********************************************************************
! TELEMAC2D   V6P3                                          21/06/2013
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
!history  R.ATA (EDF-LNHE)
!+        21/06/2013
!+        V6P3
!+   Clean and remove of unused variables
!+   Adaptation for new data structure
!+   Parallelization
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NELEM,NSEG,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN) :: NUBO(2,*),LIMPRO(NPTFR,6),NBOR(NPTFR)
      INTEGER, INTENT(IN) :: ELTSEG(NELEM,3)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: AIRS(NPOIN),ZF(NPOIN),VNOIN(3,*)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: WINF(3,NPTFR),G,EPS
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(NPOIN,3),DDMIN
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSCE(3,NPOIN),FLUSORT,FLUENT
! RA
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: FLBOR
      INTEGER, INTENT(IN)            :: IFABOR(NELEM,3)
      TYPE(BIEF_MESH),INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NB,NSG,NUBO1,NUBO2,INDIC,K,I,IEL,IER
!
      DOUBLE PRECISION DIJ,FLULOC(3),INFLOW,OUTFLOW,PROD_SCAL,DEMI
      DOUBLE PRECISION HI,UI,VI,HJ,VJ,UJ,XN,YN,RNORM,CT,PRI,USN
      LOGICAL, ALLOCATABLE ::  YESNO(:)
!
      INTRINSIC SQRT
!-----------------------------------------------------------------------
!------
! 1. INITIALISATIONS
!
      DEMI=0.5D0
      FLULOC(1) = 0.D0
      FLULOC(2) = 0.D0
      FLULOC(3) = 0.D0
!
      ALLOCATE(YESNO(NSEG),STAT=IER)
      IF(IER.NE.0)THEN
        IF(LNG.EQ.1)WRITE(LU,*)'FLUX_TCH: ERREUR D''ALLOCATION'
        IF(LNG.EQ.2)WRITE(LU,*)'FLUX_TCH: ALLOCATION ERROR '
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO  NB =  1 , NPOIN
        FLUX(NB,1) = 0.D0
        FLUX(NB,2) = 0.D0
        FLUX(NB,3) = 0.D0
        FLUSCE(1,NB) = 0.D0
        FLUSCE(2,NB) = 0.D0
        FLUSCE(3,NB) = 0.D0
      ENDDO
!    INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
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
      DO IEL=1, NELEM
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!
!           RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!           WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOIN(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOIN(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
            INDIC =0
!
!   --->    INTERMEDIATE COMPUTATIONS
!
            HI = W(1,NUBO1)
            IF(HI.GT.EPS) THEN
              UI = W(2,NUBO1) / HI
              VI = W(3,NUBO1) / HI
            ELSE
              INDIC = INDIC + 1
              UI = 0.D0
              VI = 0.D0
              HI = 0.D0
            ENDIF
!
            HJ = W(1,NUBO2)
            IF(HJ.GT.EPS) THEN
              UJ = W(2,NUBO2) / HJ
              VJ = W(3,NUBO2) / HJ
            ELSE
              INDIC = INDIC + 1
              UJ = 0.D0
              VJ = 0.D0
              HJ = 0.D0
            ENDIF
!
            DIJ = SQRT((X(NUBO1)-Y(NUBO2))**2+(Y(NUBO1)-Y(NUBO2))**2)
            IF(DIJ.LE.DDMIN) THEN
              DDMIN=DIJ
            ENDIF
            RNORM = VNOIN(3,NSG)
!
            IF(INDIC.LT.2) THEN
              XN = VNOIN (1,NSG)
              YN = VNOIN (2,NSG)
!
              CALL FLUXE(HJ,UJ,VJ,HI,UI,VI,XN,YN,RNORM,G,FLULOC)
              CALL FLUSRC(NUBO1,NUBO2,NSG,VNOIN,W,FLUSCE,X,Y,AIRS,NPOIN,
     &                 NSEG,ZF,EPS,G)
            ELSE
              FLULOC(1)= 0.D0
              FLULOC(2)= 0.D0
              FLULOC(3)= 0.D0
              FLUSCE(1,NUBO1)=0.D0
              FLUSCE(2,NUBO1)=0.D0
              FLUSCE(3,NUBO1)=0.D0
              FLUSCE(1,NUBO2)=0.D0
              FLUSCE(2,NUBO2)=0.D0
              FLUSCE(3,NUBO2)=0.D0
            ENDIF
!
!FOR PARALLELISM
!
            IF(NCSIZE.GT.1)THEN
              IF(IFABOR(IEL,I).EQ.-2)THEN !THIS IS INTERFACE EDGE
                ! DEMI=DEMI*SIGN(1.0D0,PROD_SCAL)
                FLULOC(1)= DEMI*FLULOC(1)
                FLULOC(2)= DEMI*FLULOC(2)
                FLULOC(3)= DEMI*FLULOC(3)
!
                FLUSCE(1,NUBO1)= DEMI*FLUSCE(1,NUBO1)
                FLUSCE(2,NUBO1)= DEMI*FLUSCE(2,NUBO1)
                FLUSCE(3,NUBO1)= DEMI*FLUSCE(3,NUBO1)
                FLUSCE(1,NUBO2)= DEMI*FLUSCE(1,NUBO2)
                FLUSCE(2,NUBO2)= DEMI*FLUSCE(2,NUBO2)
                FLUSCE(3,NUBO2)= DEMI*FLUSCE(3,NUBO2)
              ENDIF
            ENDIF
!
            FLUX(NUBO1,1)=FLUX(NUBO1,1)+FLULOC(1)*RNORM+FLUSCE(1,NUBO1)
            FLUX(NUBO1,2)=FLUX(NUBO1,2)+FLULOC(2)*RNORM+FLUSCE(2,NUBO1)
            FLUX(NUBO1,3)=FLUX(NUBO1,3)+FLULOC(3)*RNORM+FLUSCE(3,NUBO1)
!
            FLUX(NUBO2,1)=FLUX(NUBO2,1)-FLULOC(1)*RNORM+FLUSCE(1,NUBO2)
            FLUX(NUBO2,2)=FLUX(NUBO2,2)-FLULOC(2)*RNORM+FLUSCE(2,NUBO2)
            FLUX(NUBO2,3)=FLUX(NUBO2,3)-FLULOC(3)*RNORM+FLUSCE(3,NUBO2)
!
            YESNO(NSG)=.TRUE.
          ENDIF

        ENDDO
      ENDDO

!     FOR PARALLESM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(FLUX(:,1),FLUX(:,2),FLUX(:,3),NPOIN,1,2,3,MESH)
      ENDIF
!
! 3. COMPUTE FLUXES AT THE BOUNDARIES
!     START AT ZERO
      FLUSORT = 0.D0
      FLUENT  = 0.D0
!
! ====>   INLET FLUX
!
      DO K = 1 , NPTFR
!
        NB = NBOR(K)
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
!         IF H IMPOSED
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
            HI = W(1,NB)
            IF(HI.GT.EPS) THEN
              UI = W(2,NB) / HI
              VI = W(3,NB) / HI
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
            HJ = W(1,NB)
            IF(HJ.GT.EPS) THEN
             UJ = W(2,NB) / HJ
             VJ = W(3,NB) / HJ
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
             CALL FLUXE(HI,UI,VI,HJ,UJ,VJ,XN,YN,RNORM,G,FLULOC)
! RA
             INFLOW = FLULOC(1)*RNORM
             FLUENT = FLUENT + INFLOW
             FLBOR%R(K)=INFLOW
!
            ELSE
             FLULOC(1)= 0.D0
             FLULOC(2)= 0.D0
             FLULOC(3)= 0.D0
             FLBOR%R(K)=0.0D0
            ENDIF
          ENDIF
!
          FLUX(NB,1)=FLUX(NB,1)+FLULOC(1)*RNORM
          FLUX(NB,2)=FLUX(NB,2)+FLULOC(2)*RNORM
          FLUX(NB,3)=FLUX(NB,3)+FLULOC(3)*RNORM
!
!------
! 5. FLUX AT THE BOUNDARY
!------
!
        ELSEIF(LIMPRO(K,1).EQ.KNEU) THEN
!
!
          IF(W(1,NB).GT.0.) THEN
            PRI =G*( W(1,NB)*W(1,NB))/2.D0
            USN = (W(2,NB)*XNEBOR(K)+W(3,NB)*YNEBOR(K))/W(1,NB)
            CT = SQRT(G*W(1,NB))
!
            IF((USN+2.D0*CT).LE.0.D0) THEN
              PRI =0.D0
            ELSEIF(((USN+2.D0*CT).GE.0.D0) .AND. (USN.LE.0.D0)) THEN
              PRI = PRI * (1.D0 +  USN / 2.D0 / CT)
     &                  * (1.D0 +  USN / 2.D0 / CT)
     &                  * (1.D0 +  USN / 2.D0 / CT)
     &                  * (1.D0 +  USN / 2.D0 / CT)
!
            ELSEIF(USN.GE.0.D0) THEN
              PRI = PRI*(1.D0 + 2.D0 * USN / CT)
            ENDIF
!
            FLUX(NB,2) = FLUX(NB,2) + XNEBOR(K+NPTFR) * PRI
            FLUX(NB,3) = FLUX(NB,3) + YNEBOR(K+NPTFR) * PRI
          ENDIF
!
        ENDIF
      ENDDO ! K
!
      DEALLOCATE(YESNO)
!-----------------------------------------------------------------------
!
      RETURN
      END
