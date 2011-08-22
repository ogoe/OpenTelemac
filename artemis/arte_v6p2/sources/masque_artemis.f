!                    *************************
                     SUBROUTINE MASQUE_ARTEMIS
!                    *************************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FILLS THE ARRAYS MASK1, MASK2, MASK3, MASK4.
!+
!+      MASK1: CORRESPONDS TO INCIDENT WAVES (KINC)
!+      MASK2: CORRESPONDS TO FREE EXIT (KSORT)
!+      MASK3: CORRESPONDS TO SOLID BOUNDARY (KLOG)
!+      MASK4: CORRESPONDS TO IMPOSED WAVES (KENT)
!
!history  D. AELBRECHT (LNH)
!+        06/07/1999
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!
      INTEGER IK
!
!-----------------------------------------------------------------------
!
! MASKS
!
!     INITIALISES (SETS TO 0) ALL MASKING VECTORS
!
      IF (NCSIZE .GT. 1) THEN
         MASK1T=0.0
         MASK2T=0.0
         MASK3T=0.0
         MASK4T=0.0
         DO 5 IK=1,NPTFR_TOT
            IF (LIHBORT(IK).EQ.KLOG) THEN
               MASK3T(IK) = 1.D0
            ELSEIF (LIHBORT(KP1BOR_TOT(IK)).NE.KLOG) THEN
               IF (LIHBORT(IK).EQ.KINC) THEN
                  MASK1T(IK) = 1.D0
               ENDIF
               IF (LIHBORT(IK).EQ.KSORT) THEN
                  MASK2T(IK) = 1.D0
               ENDIF
               IF (LIHBORT(IK).EQ.KENT) THEN
                  MASK4T(IK) = 1.D0
               ENDIF
            ELSE
               MASK3T(IK) = 1.D0
            ENDIF
 5       CONTINUE
!
!-----------------------------------------------------------------------
!
         CALL GLOBAL_TO_LOCAL_BOUND(MASK1T,MASK1,MESH%NPTFR,NPTFR_TOT)
         CALL GLOBAL_TO_LOCAL_BOUND(MASK2T,MASK2,MESH%NPTFR,NPTFR_TOT)
         CALL GLOBAL_TO_LOCAL_BOUND(MASK3T,MASK3,MESH%NPTFR,NPTFR_TOT)
         CALL GLOBAL_TO_LOCAL_BOUND(MASK4T,MASK4,MESH%NPTFR,NPTFR_TOT)
!         DEALLOCATE(MASK1T)
!         DEALLOCATE(MASK2T)
!         DEALLOCATE(MASK3T)
!         DEALLOCATE(MASK4T)
      ELSE
! MASKS
!
!     INITIALISES (SETS TO 0) ALL MASKING VECTORS
!
      CALL OS( 'X=C     ' , MASK1 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK2 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK3 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK4 , SBID , SBID , 0.D0 )
!
      DO IK=1,NPTFR
         LIHBOR%I(IK)=LIHBORT(IK)
      END DO
      DO 6 IK=1,NPTFR
!
         IF (LIHBOR%I(IK).EQ.KLOG) THEN
            MASK3%R(IK) = 1.D0
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(IK)).NE.KLOG) THEN
            IF (LIHBOR%I(IK).EQ.KINC) THEN
               MASK1%R(IK) = 1.D0
            ENDIF
            IF (LIHBOR%I(IK).EQ.KSORT) THEN
               MASK2%R(IK) = 1.D0
            ENDIF
            IF (LIHBOR%I(IK).EQ.KENT) THEN
               MASK4%R(IK) = 1.D0
            ENDIF
         ELSE
            MASK3%R(IK) = 1.D0
         ENDIF
 6    CONTINUE
      ENDIF
        RETURN
        CONTAINS
        SUBROUTINE GLOBAL_TO_LOCAL_BOUND(TAB1,OBJ,NPTFR,NPTFR_TOT)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(INOUT)  :: TAB1(:)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: OBJ
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER, INTENT(IN) :: NPTFR_TOT
      INTEGER :: I,J
      OBJ%R=0.0
      DO I=1,NPTFR_TOT
         DO J=1,MESH%NPTFR
            IF (NBOR_TOT(I) .EQ. MESH%KNOLG%I(MESH%NBOR%I(J))) THEN
               OBJ%R(J)=TAB1(I)
            END IF
         END DO
      END DO
      OBJ%DIM1=NPTFR
      RETURN
      END SUBROUTINE
      END
