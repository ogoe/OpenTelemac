!                    *****************
                     SUBROUTINE CLSING
!                    *****************
!
     &(NWEIRS,NPSING,NPSMAX,NUMDIG,X,Y,ZF,CHESTR,NKFROT,KARMAN,
     & ZDIG,PHIDIG,NBOR,H,T,NTRAC,IOPTAN,UNORM,
     & UBOR,VBOR,TBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR,GRAV)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MANAGES THE COMPUTATION OF DISCHARGES AND
!+                DETERMINES BOUNDARY CONDITIONS.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        23/11/2005
!+        V5P6
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
!| CHESTR         |-->| FRICTION COEFFICIENT
!| GRAV           |-->| GRAVITY
!| H              |-->| WATER DEPTH
!| IOPTAN         |-->| OPTION FOR TANGENTIAL VELOCITIES.
!| KARMAN         |-->| VON KARMAN CONSTANT.
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NKFROT         |-->| FRICTION LAW, PER POINT
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NPSMAX         |-->| MAXIMUM NUMBER OF POINTS FOR ONE SIDE OF A
!|                |   | SINGULARITY.
!| NTRAC          |-->| NUMBER OF TRACERS
!| NUMDIG         |-->| NUMDIG(K,I,NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF SIDE K OF WEIR I.
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| PHIDIG         |-->| DISCHARGE COEFFICIENT OF THE WEIR
!| T              |-->| BLOCK OF TRACERS
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| UNORM          |-->| NORM OF VELOCITY 
!| X              |-->| ABSCISSAE OF NODES
!| Y              |-->| ORDINATES OF NODES
!| ZDIG           |-->| ELEVATIONS OF POINTS OF WEIRS 
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_CLSING => CLSING
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NWEIRS,NPSMAX,IOPTAN
      INTEGER, INTENT(IN) :: NKFROT(*),NBOR(*)
      INTEGER, INTENT(IN) :: NPSING(NWEIRS),NUMDIG(2,NWEIRS,NPSMAX)
      INTEGER, INTENT(INOUT) :: LIUBOR(*),LIVBOR(*),LIHBOR(*)
      INTEGER, INTENT(IN) :: NTRAC
      DOUBLE PRECISION, INTENT(IN) :: PHIDIG(NWEIRS,NPSMAX)
      DOUBLE PRECISION, INTENT(IN) :: ZDIG(NWEIRS,NPSMAX),H(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),ZF(*),CHESTR(*)
      DOUBLE PRECISION, INTENT(IN) :: KARMAN,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UNORM(*)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR,LITBOR
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N,IA,IB,NA,NB
!
      DOUBLE PRECISION HMIN,PHI,QAB,YAA,YBB,YDEN,YS
!
!-----------------------------------------------------------------------
!
      HMIN=1.D-3
!
!     COMPUTES UNIT DISCHARGES
!
      DO 10 N=1,NWEIRS
      DO 20 I=1,NPSING(N)
        IA=NUMDIG(1,N,I)
        IB=NUMDIG(2,N,I)
        NA=NBOR(IA)
        NB=NBOR(IB)
        YAA=H(NA)+ZF(NA)
        YBB=H(NB)+ZF(NB)
        YS=ZDIG(N,I)
        PHI=PHIDIG(N,I)
!
        IF(YAA.GT.YBB) THEN
!         CASE WHERE A IS UPSTREAM
          YDEN=YS/3.D0+2.D0*YAA/3.D0
          IF(YBB.LT.YDEN) THEN
            CALL LOIDEN(YAA,YS,PHI,QAB,GRAV)
          ELSE
            CALL LOINOY(YAA,YBB,YS,PHI,QAB,GRAV)
          ENDIF
        ELSE
!         CASE WHERE B IS UPSTREAM
          YDEN=YS/3.D0+2.D0*YBB/3.D0
          IF(YAA.LT.YDEN) THEN
            CALL LOIDEN(YBB,YS,PHI,QAB,GRAV)
          ELSE
            CALL LOINOY(YBB,YAA,YS,PHI,QAB,GRAV)
          ENDIF
          QAB=-QAB
        ENDIF
!
! COMPUTES THE NORMAL DISCHARGE
!
        IF(H(NA).LE.HMIN) THEN
          UNORM(IA)=0.D0
        ELSE
          UNORM(IA)=-QAB/H(NA)
        ENDIF
!
        IF(H(NB).LE.HMIN) THEN
          UNORM(IB)=0.D0
        ELSE
          UNORM(IB)=-QAB/H(NB)
        ENDIF
!
20    CONTINUE
10    CONTINUE
!
!     DETERMINES THE NUMERICAL VALUE
!     OF THE BOUNDARY CONDITIONS:
!
      CALL CLHUVT(NWEIRS,NPSING,NPSMAX,NUMDIG,ZDIG,X,Y,ZF,
     &            IOPTAN,UNORM,CHESTR,NKFROT,KARMAN,T,NTRAC,H,
     &            UBOR,VBOR,TBOR,NBOR,LIHBOR,LIUBOR,LIVBOR,LITBOR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
