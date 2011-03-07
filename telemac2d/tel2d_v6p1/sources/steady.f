!                    *****************
                     SUBROUTINE STEADY
!                    *****************
!
     &(H1,H2,NPH,U1,U2,NPU,V1,V2,NPV,NTRAC,T1,T2,NPT,CRIPER,ARRET)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CHECKS IF A STEADY STATE IS REACHED.
!
!warning  ARRET IS NOT INITIALISED
!
!history  J-M HERVOUET (LNHE)
!+        05/09/2007
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
!| ARRET          |<--| LOGIQUE MIS A TRUE SI PERMANENT ATTEINT
!| CRIPER         |-->| CRITERES D'ARRET
!|                |   | DANS L'ORDRE SUIVANT : H , U , V , T
!| H1,H2,NPH      |-->| HAUTEURS A COMPARER ET NOMBRE DE POINTS
!| NPV            |---|
!| NTRAC          |---|
!| T1,T2,NPT      |-->| TRACEURS A COMPARER ET NOMBRE DE POINTS
!| U1,U2,NPU      |-->| VITESSES A COMPARER ET NOMBRE DE POINTS
!| V1,V2,NPU      |-->| VITESSES A COMPARER ET NOMBRE DE POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NPH,NPU,NPV,NPT,NTRAC
      LOGICAL, INTENT(INOUT)       :: ARRET
      DOUBLE PRECISION, INTENT(IN) :: H1(NPH),H2(NPH),U1(NPU),U2(NPU)
      DOUBLE PRECISION, INTENT(IN) :: V1(NPV),V2(NPV)
      DOUBLE PRECISION, INTENT(IN) :: CRIPER(3)
      TYPE(BIEF_OBJ)  , INTENT(IN) :: T1,T2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ITRAC
!
!-----------------------------------------------------------------------
!
!  CHECKS THE WATER DEPTH
!
      DO 10 I = 1 , NPH
        IF(ABS(H1(I)-H2(I)).GT.CRIPER(1)) GO TO 1000
10    CONTINUE
!
!-----------------------------------------------------------------------
!
!  CHECKS U
!
      DO 20 I = 1 , NPU
        IF(ABS(U1(I)-U2(I)).GT.CRIPER(2)) GO TO 1000
20    CONTINUE
!
!-----------------------------------------------------------------------
!
!  CHECKS V
!
      DO 30 I = 1 , NPV
        IF(ABS(V1(I)-V2(I)).GT.CRIPER(2)) GO TO 1000
30    CONTINUE
!
!-----------------------------------------------------------------------
!
!  CHECKS THE TRACER
!
      IF(NTRAC.GT.0) THEN
!
      DO ITRAC=1,NTRAC
        DO I = 1 , NPT
          IF(ABS(T1%ADR(ITRAC)%P%R(I)
     &          -T2%ADR(ITRAC)%P%R(I)).GT.CRIPER(3)) GO TO 1000
        ENDDO
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      ARRET=.TRUE.
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,200)
100   FORMAT(/,1X,'ETAT PERMANENT ATTEINT')
200   FORMAT(/,1X,'THE STEADY STATE HAS BEEN REACHED')
!
!-----------------------------------------------------------------------
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END