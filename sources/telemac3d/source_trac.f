!                    **********************
                     SUBROUTINE SOURCE_TRAC
!                    **********************
!
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.
!
!history  CDG/SOGREAH
!+        **/06/2001
!+
!+   TRACER SOURCES
!
!history  J-M HERVOUET (LNHE)
!+        21/10/2004
!+        V5P5
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
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------
!
      INTEGER ITRAC
!
!----------------------------------------------------------------------
!
!     SETS SOURCE TERMS TO ZERO
!
      IF(NTRAC.GE.1) THEN
!
!        CALL OS ( 'X=C     ' , X=S0TA , C=0.D0 )
!        CALL OS ( 'X=C     ' , X=S1TA , C=0.D0 )
!
!        SOURCE TERMS SIMPLY MARKED
!
!        BEWARE, PUT Q INSTEAD OF 0 IN TYPR IF NOT NIL
!
         DO ITRAC=1,NTRAC
           S0TA%ADR(ITRAC)%P%TYPR='0'
           S1TA%ADR(ITRAC)%P%TYPR='0'
         ENDDO
!
!        EXAMPLE OF RADIOACTIVE DECAY E**(-KT) ON FIRST TRACER, HERE C=K
!
!        S1TA%ADR(1)%P%TYPR='Q'
!        CALL OS('X=C     ',S1TA%ADR(1)%P,C=1.D0)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
