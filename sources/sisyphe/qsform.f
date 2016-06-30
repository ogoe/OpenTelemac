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
!     INTEGER          :: I
!     DOUBLE PRECISION :: C1, C2, T
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!     EXAMPLE BY VAN RIJN
!
!     C1 = DENS * GRAV * DM
!     C2 = 0.053D0 * SQRT(DM**3*DENS*GRAV) * DSTAR**(-0.3D0)
!
!     DO I = 1, NPOIN
!
!       TRANSPORT STAGE PARAMETER
!
!       IF(TETAP%R(I) .LE. AC) THEN
!         T = 0.D0
!       ELSE
!         T = (TETAP%R(I)-AC)/MAX(AC,1.D-06)
!       ENDIF
!
!       BEDLOAD TRANSPORT RATE
!
!       QSC%R(I) = C2 * T**2.1D0
!       QSS%R(I) = 0.D0
!
!     ENDDO
!
!  FOLLOWING LINES NEED TO BE COMMENTED OUT
!
      IF(LNG.EQ.1) WRITE(LU,52)
      IF(LNG.EQ.2) WRITE(LU,53)
!
52    FORMAT(/,1X,' STOP :',/
     &     ,1X,' LE TAUX DE TRANSPORT DOIT ETRE CALCULE DANS QSFORM')
53    FORMAT(/,1X,'SISYPHE IS STOPPED : ',/
     &      ,1X,' SAND TRANSPORT MUST BE CALCULATED IN QSFORM')
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
