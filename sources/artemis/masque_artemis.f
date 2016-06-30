!                    *************************
                     SUBROUTINE MASQUE_ARTEMIS
!                    *************************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FILLS THE ARRAYS MASK1, MASK2, MASK3, MASK4, MASK5
!+
!+      MASK1: CORRESPONDS TO INCIDENT WAVES (KINC)
!+      MASK2: CORRESPONDS TO FREE EXIT (KSORT)
!+      MASK3: CORRESPONDS TO SOLID BOUNDARY (KLOG)
!+      MASK4: CORRESPONDS TO IMPOSED WAVES (KENT)
!+      MASK5: CORRESPONDS TO INCIDENT POTENTIAL (KPOT)
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      INTEGER IK
!
!-----------------------------------------------------------------------
!
!
!     INITIALISES (SETS TO 0) ALL MASKING VECTORS
!
      CALL OS( 'X=C     ' , MASK1 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK2 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK3 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK4 , SBID , SBID , 0.D0 )
      CALL OS( 'X=C     ' , MASK5 , SBID , SBID , 0.D0 )
!
      DO IK=1,NPTFR
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
          IF (LIHBOR%I(IK).EQ.KPOT) THEN
            MASK5%R(IK) = 1.D0
          ENDIF
        ELSE
          MASK3%R(IK) = 1.D0
        ENDIF
      ENDDO ! IK
!
      RETURN
      END

