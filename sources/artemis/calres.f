!                    *****************
                     SUBROUTINE CALRES
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE WAVE HEIGHT AND PHASE, SPEEDS
!+                AND THE FREE SURFACE ELEVATION.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
!+        V5P1
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
      INTEGER I
!
      DOUBLE PRECISION PI
      DOUBLE PRECISION ZERO, BID
!
      INTRINSIC SQRT, ATAN2, MOD, ABS, COS, SIN, ATAN
!
!-----------------------------------------------------------------------
!
      PARAMETER (ZERO = 1.D-10)
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
      PI = 4.D0 * ATAN( 1.D0 )
!      PARAMETER (PI = 3.1415926535897932384626433D0)
!      PARAMETER (RADDEG = 57.29577951D0)
!< SEB @ HRW
!
!=======================================================================
! WAVE HEIGHT HHO <=> Hm0
!=======================================================================
!
      CALL OS( 'X=N(Y,Z)', T1, PHIR, PHII , BID             )
      IF (COURANT) THEN
!     WE USE WR (RELATIVE PULSATION)
        CALL OS( 'X=CY    ', X=T2   ,Y=WR, C=2.D0/GRAV)
        CALL OS( 'X=YZ    ', X=HHO  ,Y=T1, Z=T2 )
      ELSE
!     WE USE OMEGA
        CALL OS( 'X=CY    ', X=HHO  ,Y=T1, C=2.D0*OMEGA/GRAV)
      ENDIF
!
!=======================================================================
! PHASE OF THE POTENTIAL (IN RADIAN)
!=======================================================================
!
      DO I=1,NPOIN
        IF (T1%R(I).LT.ZERO) THEN
          PHAS%R(I) = 0.D0
        ELSE
          PHAS%R(I) = ATAN2( PHII%R(I),PHIR%R(I) )
        ENDIF
      ENDDO
!
!=======================================================================
! FREE SURFACE ELEVATION
!=======================================================================
      IF (COURANT) THEN
        DO I=1,NPOIN
          S%R(I) = -WR%R(I)/GRAV*PHII%R(I) + H%R(I) + ZF%R(I)
        ENDDO
      ELSE
        DO I=1,NPOIN
          S%R(I) = -OMEGA/GRAV*PHII%R(I) + H%R(I) + ZF%R(I)
        ENDDO
      ENDIF
!
!=======================================================================
! WAVE INIDENCE USING SPEEDS AT THE SURFACE (AT T=0 AND T=OMEGA/4)
!=======================================================================
      CALL CALDIR()
!=======================================================================
!    NOMBRES D INTERET POUR LE COURANT, ATTENTION IL FAUT DECLARER 4 VARIABLES
!                                                 PRIVEES DANS LE .cas
!      IF (COURANT) THEN
!      ON IMPRIME LE COURANT ET LE VECTEUR D ONDE
!       DO I=1,NPOIN
!        PRIVE%ADR(1)%P%R(I) = UC%R(I)
!        PRIVE%ADR(2)%P%R(I) = VC%R(I)
!        PRIVE%ADR(3)%P%R(I) = T5%R(I)
!        PRIVE%ADR(4)%P%R(I) = T6%R(I)
!       ENDDO
!      ENDIF

!=======================================================================
!
      RETURN
      END SUBROUTINE
