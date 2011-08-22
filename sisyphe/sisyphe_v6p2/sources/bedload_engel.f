!                    **********************************
                     SUBROUTINE BEDLOAD_ENGEL ! (_IMP_)
!                    **********************************
!
     &  (TOB, CF, DENS, GRAV, DM, XMVE, TETA, QSC)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    ENGELUND-HANSEN BEDLOAD TRANSPORT FORMULATION.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+        V5P1
!+
!
!history  C.VILLARET
!+        **/10/2003
!+        V5P4
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| QSC            |<->| BED LOAD TRANSPORT
!| TETA           |<->| DIMENSIONLESS BED SHEAR STRESS 
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| XMVE           |-->| FLUID DENSITY 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_ENGEL => BEDLOAD_ENGEL
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, CF
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, XMVE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA ! WORK ARRAY T1
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      DOUBLE PRECISION :: CENGEL, C1
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ************************** !
      ! I - TOTAL STRESS ADIM      ! (_IMP_)
      ! ************************** !
      C1 = 1.D0/(DENS*XMVE*GRAV*DM)
      CALL OS('X=CY    ', X=TETA, Y=TOB , C=C1)
      CALL OS('X=Y**C  ', X=TETA, Y=TETA, C=5.D0/2.D0)
!
      ! *************************** !
      ! II - BEDLOAD TRANSPORT      ! (_IMP_)
      ! *************************** !
      CENGEL = 0.1D0*SQRT(DENS*GRAV*DM**3)
      CALL OS('X=+(Y,C)', X=QSC , Y=CF  , C=1.D-06)
      CALL OS('X=1/Y   ', X=QSC , Y=QSC)
      CALL OS('X=CXY   ', X=QSC , Y=TETA, C=CENGEL)
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_ENGEL
