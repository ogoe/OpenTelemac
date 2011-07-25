!                    *****************
                     SUBROUTINE CLTRAC
!                    *****************
!
     &(NWEIRS,NPSING,NPSMAX,NUMDIG,ZF,ZDIG,H,T,NBOR,LITBOR,TBOR,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
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
      DOUBLE PRECISION Z1,Z2
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NTRAC
!
        DO 10 N=1,NWEIRS
        DO 20 I=1,NPSING(N)
!
          N1=NBOR(NUMDIG(1,N,I))
          N2=NBOR(NUMDIG(2,N,I))
          Z1=H(N1)+ZF(N1)
          Z2=H(N2)+ZF(N2)
          IF(Z1.GT.Z2.AND.Z1.GT.ZDIG(N,I)) THEN
            TBOR%ADR(ITRAC)%P%R(NUMDIG(1,N,I))=T%ADR(ITRAC)%P%R(N1)
            TBOR%ADR(ITRAC)%P%R(NUMDIG(2,N,I))=T%ADR(ITRAC)%P%R(N1)
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=4
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=5
          ELSEIF(Z2.GE.Z1.AND.Z2.GT.ZDIG(N,I)) THEN
            TBOR%ADR(ITRAC)%P%R(NUMDIG(1,N,I))=T%ADR(ITRAC)%P%R(N2)
            TBOR%ADR(ITRAC)%P%R(NUMDIG(2,N,I))=T%ADR(ITRAC)%P%R(N2)
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=5
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=4
          ELSE
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=2
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=2
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
