!                    *******************
                     SUBROUTINE CELERITE
!                    *******************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE REFERENCE CELERITY (C0)
!+                WHICH IS USED IN THE CASE OF INCIDENTAL WAVE.
!+
!+                HERE C0 = SQRT (G H INITIAL),
!+                BUT C0 IS A BOUNDARY ARRAY.
!
!history  J-M HERVOUET (LNH)
!+        17/08/1994
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      CALL OSBD( 'X=CY    ' , C0 , H  , H , GRAV , MESH )
      CALL OS  ( 'X=+(Y,C)' , C0 , C0 , H , 0.D0        )
      CALL OS  ( 'X=SQR(Y)' , C0 , C0 , H , 0.D0        )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
