!                    *****************
                     SUBROUTINE QSFORM
!                    *****************
!
     &(U2D, V2D, TOB, HN, XMVE, TETAP, MU, NPOIN, DM,
     & DENS, GRAV, DSTAR, AC, QSC, QSS)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    ALLOWS THE USER TO CODE THEIR OWN BEDLOAD TRANSPORT
!+                FORMULATION, BEST SUITED TO THEIR APPLICATION.
!
!warning  USER SUBROUTINE; SAND TRANSPORT FORMULA MUST BE CODED BY THE USER
!
!history  F. HUVELIN
!+        **/11/2003
!+        V5P4
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
!history  P. Tassi
!+        22/05/2012
!+        V6P2
!+   Arguments added
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_QSFORM => QSFORM
!     USE DECLARATIONS_SISYPHE
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,TOB,HN,TETAP,MU
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, DM, DENS, GRAV, DSTAR, AC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          :: I
      DOUBLE PRECISION :: C1, C2, T
      DOUBLE PRECISION, PARAMETER :: ACOEFF = 0.004D0 ! Sediment transport param (m^2s^-1)
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     GRASS (1981) TYPE
!
      DO I = 1, NPOIN

        QSC%R(I) = ACOEFF * U2D%R(I) * (U2D%R(I)**2+V2D%R(I)**2)   ! Grass (1981) type bedload (total load)
        QSS%R(I) = 0.D0                                            ! Zero suspended load

      END DO
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END

