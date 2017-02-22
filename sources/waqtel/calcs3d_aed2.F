!                     ***********************
                      SUBROUTINE CALCS3D_AED2
!                     ***********************
     &(NPOIN3,NPOIN2,NPLAN,TEXP,NTRAC)
!
!
!***********************************************************************
! WAQTEL      V7P3
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS WHEN COUPLING WITH AED2 FOR 3D CASE
!
!history  M JODEAU
!+        18/05/2016
!+        V7P3
!+       Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FLUXAED2      |<--| EXPLICIT SOURCE TERM CALCULATED BY AED2
!| NPOIN2        |-->| NUMBER OF NODES IN THE 2D MESH
!| NPOIN3        |-->| NUMBER OF NODES IN THE 3D MESH
!| NTRAC         |-->| TOTAL NUMBER OF TRACERS
!| TEXP          |<--| EXPLICIT SOURCE TERM.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
#if defined (HAVE_AED2)
      USE T3D_AED2
#endif
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER       , INTENT(IN)    :: NPOIN3,NPOIN2,NPLAN
      INTEGER       , INTENT(IN)    :: NTRAC
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TEXP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K
      DOUBLE PRECISION, DIMENSION(NPOIN2,NTRAC,NPLAN) :: FLUXAED2
!
      INTRINSIC MAX
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined (HAVE_AED2)
! INITIALIZATION OF SOURCE TERMS
      DO J=1,NTRAC
        CALL OS('X=0     ',X=TEXP%ADR(J)%P)
      ENDDO
!
! COMPUTATION OF THE SOURCE TERMS BY AED2
!
      CALL DO_AED2_MODELS(NPOIN3,NPOIN2,FLUXAED2)
!
      DO J=3,NTRAC
        TEXP%ADR(J)%P%TYPR='Q'
        DO I=1,NPOIN2
          DO K=1,NPLAN
            TEXP%ADR(J)%P%R(I+(K-1)*NPOIN2) = FLUXAED2(I,J-2,K)
          ENDDO
        ENDDO
      ENDDO
#endif
!
!-----------------------------------------------------------------------
!
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS (TO CHECK)
!
!     ACTIVATE BIEF_OBJ FOR FURTHER CALCULATIONS
!      TEXP%ADR(RANKTR1)%P%TYPR='Q'
!      TIMP%ADR(RANKTR2)%P%TYPR='Q'
!      TIMP%ADR(NTRAC  )%P%TYPR='Q'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
