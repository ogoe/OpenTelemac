!                    *****************
                     SUBROUTINE CLTRAC
!                    *****************
!
     &(NWEIRS,NPSING,NPSMAX,NUMDIG,ZF,ZDIG,H,T,NBOR,LITBOR,TBOR,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MANAGES THE BOUNDARY CONDITIONS FOR TRACER.
!+                FOR WEIRS.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        03/10/1996
!+        V5P2
!+   MODIFIED
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
!+        09/08/2011
!+        V6P2
!+   Adaptation for parallelism
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| H              |-->| WATER DEPTH
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NPSMAX         |-->| MAXIMUM NUMBER OF POINTS FOR ONE SIDE OF A
!|                |   | SINGULARITY.
!| NTRAC          |-->| NUMBER OF TRACERS
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NUMDIG         |-->| NUMDIG(K,I,NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF SIDE K OF WEIR I.
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| T              |-->| BLOCK OF TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ZDIG           |-->| ELEVATIONS OF POINTS OF WEIRS 
!| ZF             |-->| BOTTOM TOPOGRAPHY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NWEIRS,NPSMAX,NTRAC
      INTEGER, INTENT(IN) :: NPSING(NWEIRS),NUMDIG(2,NWEIRS,NPSMAX)
      INTEGER, INTENT(IN) :: NBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: ZDIG(NWEIRS,NPSMAX)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(*),H(*)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LITBOR,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N,N1,N2,ITRAC
!
      DOUBLE PRECISION Z1,Z2,T1,T2
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NTRAC
!
        DO 10 N=1,NWEIRS
        DO 20 I=1,NPSING(N)
!
          IF(NUMDIG(1,N,I).GT.0) THEN
            N1=NBOR(NUMDIG(1,N,I))
            Z1=H(N1)+ZF(N1)
            T1=T%ADR(ITRAC)%P%R(N1)
          ELSE
            Z1=0.D0
            T1=0.D0
          ENDIF
          IF(NUMDIG(2,N,I).GT.0) THEN
            N2=NBOR(NUMDIG(2,N,I))
            Z2=H(N2)+ZF(N2)
            T2=T%ADR(ITRAC)%P%R(N2)
          ELSE
            Z2=0.D0
            T2=0.D0
          ENDIF
!
          IF(NCSIZE.GT.1) THEN
            Z1=P_DMAX(MAX(Z1,0.D0))-P_DMIN(MAX(-Z1,0.D0))
            Z2=P_DMAX(MAX(Z2,0.D0))-P_DMIN(MAX(-Z2,0.D0))
            T1=P_DMAX(MAX(T1,0.D0))-P_DMIN(MAX(-T1,0.D0))
            T2=P_DMAX(MAX(T2,0.D0))-P_DMIN(MAX(-T2,0.D0))
          ENDIF
!
!         POINT 1
!
          IF(NUMDIG(1,N,I).GT.0) THEN
            IF(Z1.GT.Z2.AND.Z1.GT.ZDIG(N,I)) THEN
              TBOR%ADR(ITRAC)%P%R(NUMDIG(1,N,I))=T1
              LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=4
            ELSEIF(Z2.GE.Z1.AND.Z2.GT.ZDIG(N,I)) THEN
              TBOR%ADR(ITRAC)%P%R(NUMDIG(1,N,I))=T2
              LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=5
            ELSE
              LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=2
            ENDIF
          ENDIF
!
!         POINT 2
!
          IF(NUMDIG(2,N,I).GT.0) THEN
            IF(Z1.GT.Z2.AND.Z1.GT.ZDIG(N,I)) THEN
              TBOR%ADR(ITRAC)%P%R(NUMDIG(2,N,I))=T1
              LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=5
            ELSEIF(Z2.GE.Z1.AND.Z2.GT.ZDIG(N,I)) THEN
              TBOR%ADR(ITRAC)%P%R(NUMDIG(2,N,I))=T2
              LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=4
            ELSE
              LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=2
            ENDIF
          ENDIF
!
20      CONTINUE
10      CONTINUE
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
