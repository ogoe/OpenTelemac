!                    *****************
                     SUBROUTINE SOURCE
!                    *****************
!
     & (S0U,S0V,S0W,S1U,S1V,S1W,
     &  UN3,VN3,WSN3,WN3,
     &  VOLU,VOLUN,T3,NPOIN3,NTRAC,LT,AT,DT,PRIVE,NONHYD,
     &  NPOIN2,NSCE,ISCE,KSCE,QSCE,USCE,VSCE,MAXSCE)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
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
!+        29/08/2008
!+        V5P6
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
!| AT             |-->| TIME
!| DT             |-->| TIME STEP
!| ISCE           |---| 
!| KSCE           |---| 
!| LT             |-->| ITERATION NUMBER
!| MAXSCE         |---| 
!| NONHYD         |---| 
!| NPOIN2         |---| 
!| NPOIN3         |-->| NUMBER OF POINTS IN THE MESH
!| NSCE           |---| 
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRIVE          |-->| BLOCK OF ARRAYS FOR USER
!| QSCE           |---| 
!| S0U,S0V        |<--| EXPLICIT SOURCE TERMS ON VELOCITIES U AND V
!| S0W            |---| 
!| S1U,S1V        |<--| IMPLICIT SOURCE TERMS ON VELOCITIES U AND V
!| S1W            |---| 
!| T3             |---| 
!| UN3,VN3,WSN3   |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| USCE           |---| 
!| VOLU           |---| 
!| VOLUN          |---| 
!| VSCE           |---| 
!| WN3            |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN3, NTRAC, LT, MAXSCE
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: UN3, VN3, WSN3, WN3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: S0U, S0V, S1U, S1V, S0W, S1W
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T3
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU, VOLUN,PRIVE
!
      DOUBLE PRECISION, INTENT(IN)  :: AT,DT
      LOGICAL, INTENT(IN)           :: NONHYD
!
      INTEGER, INTENT(IN)           :: NPOIN2
      INTEGER, INTENT(IN)           ::           NSCE
      INTEGER, INTENT(IN)           ::           ISCE(NSCE)
      INTEGER, INTENT(IN)           ::           KSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  QSCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  USCE(NSCE)
      DOUBLE PRECISION, INTENT(IN)           ::  VSCE(NSCE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC, IS
!
!-----------------------------------------------------------------------
!
!     BEWARE : BE SURE TO DO S0U = S0U + YOUR SOURCE TERMS
!              BECAUSE S0U HAS ALREADY BEEN INITIALISED IN TRISOU
!
!
!     INITIALISES OTHER SOURCE TERMS
!
      S1U%TYPR='0'
      S1V%TYPR='0'
      IF(NONHYD) THEN
        S0W%TYPR='0'
        S1W%TYPR='0'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END