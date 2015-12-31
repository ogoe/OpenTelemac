!                    *****************
                     SUBROUTINE PHSTAT
!                    *****************
!
     &   (PH, DELTAR, Z, TRA01, TRA02, RHO0, GRAV,
     &    NPOIN3, NPOIN2, NPLAN, PRIVE )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE HYDROSTATIC PRESSURE FIELD PH [PA]
!+                THROUGH INTEGRATION BASED ON TRAPEZIUM RULE IN VERTICAL.
!+
!+            THIS IS NEEDED FOR APPLICATIONS WHERE THE GLOBAL
!+                PRESSURE OUTPUT IS REQUIRED.
!code
!+                                               S
!+ PH(Z) = G * RHO0 * (S-Z) + G * RHO0 * INTEGRAL  DELTAR DZ
!+                                               Z
!+ WHERE: DELTAR = (RHO-RHO0)/RHO0
!
!history  JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!+        **/04/99
!+        V5P1
!+   FORTRAN95 VERSION
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
!| DELTAR         |-->| RELATIVE DENSITY DELTAR = (RHO-RHO0)/RHO0
!| GRAV           |-->| GRAVITY ACCELERATION
!| NPLAN          |-->| NUMBER OF MESH PLANES
!| NPOIN2         |-->| NUMBER OF 2D-POINTS
!| NPOIN3         |-->| NUMBER OF 3D-POINTS
!| PH             |<->| HYDROSTATIC PRESSURE
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| RHO0           |-->| WATER DENSITY AT REFERENCE CONCENTRATION
!| TRA01          |<->| WORK FIELDS
!| TRA02          |<->| WORK FIELDS
!| Z              |-->| Z-COORDINATES OF NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3, NPOIN2, NPLAN
      DOUBLE PRECISION, INTENT(INOUT) :: PH(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: DELTAR(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION,INTENT(INOUT)  :: TRA01(NPOIN3), TRA02(NPOIN3)
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: PRIVE
      DOUBLE PRECISION, INTENT(IN)    :: RHO0, GRAV
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K1,K2
      DOUBLE PRECISION C
!
!----------------------------------------------------------------------
! COMPUTES USING THE DENSITY VARIABLE PART
!
!                            S
! PH(Z) = G * RHO0 * INTEGRAL  DELTAR DZ
!                            Z
!
      CALL OV('X=Y     ', TRA02, DELTAR, DELTAR, C, NPOIN3)
      K1 = (NPLAN-1) * NPOIN2 + 1
      CALL OV('X=C     ', TRA01(K1), TRA01, TRA01, 0.0D0, NPOIN2)
!
      DO I=NPLAN-1,1,-1
        K2 = K1-NPOIN2
        CALL OV('X=Y-Z   ', TRA01,      Z(K1),     Z(K2), C, NPOIN2)
        CALL OV('X=X+Y   ', TRA02(K1),  TRA02(K2), Z,     C, NPOIN2)
        CALL OV('X=XY    ', TRA01,      TRA02(K1), Z,     C, NPOIN2)
        CALL OV('X=Y+Z   ', TRA01(K2),  TRA01, TRA01(K1), C, NPOIN2)
        K1 = K2
      END DO
      CALL OV('X=CY    ', PH , TRA01, TRA01, 0.5D0*GRAV*RHO0, NPOIN3)
!
! COMPUTES THE CONSTANT DENSITY PART
! PH(Z) = PH(Z) + G * RHO0 * (S-Z)
!
      K1 = (NPLAN-1) * NPOIN2 + 1
      CALL OV('X=Y-Z   ', TRA01,  Z(K1), Z(1), C, NPOIN2)
!
      DO I=1,NPLAN
        K2 = (I-1)*NPOIN2+1
        CALL OV('X=Y-Z   ', TRA01, Z(K1), Z(K2),  C, NPOIN2)
        CALL OV('X=Y+CZ  ', PH(K2), PH(K2), TRA01, GRAV*RHO0, NPOIN2)
      END DO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE PHSTAT
