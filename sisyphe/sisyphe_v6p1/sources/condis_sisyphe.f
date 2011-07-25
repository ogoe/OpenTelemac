!                    *************************
                     SUBROUTINE CONDIS_SISYPHE
!                    *************************
!
     &(CONSTFLOW)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief
!
!history  BUI MINH DUC
!+        **/08/2003
!+
!+   DEVELOPED THE SUBROUTINE
!
!history  F. HUVELIN
!+        **/02/2004
!+        V5P4
!+
!
!history  B. MINH DUC; F. HUVELIN
!+
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
!| CONSTFLOW      |<->|LOGICAL, CONSTANT FLOW DISCHARGE OR NOT 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      !
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(INOUT) :: CONSTFLOW
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, EXTERNAL      :: P_ISUM
!
!3/ LOCAL VARIABLES
!--------------------
!
      INTEGER          :: NZFMAX, I
      DOUBLE PRECISION :: ZFMAX, C
!
!=======================================================================!
!=======================================================================!
!                               PROGRAM                                 !
!=======================================================================!
!=======================================================================!
!
      NZFMAX = 0
!
      IF(CONSTFLOW) THEN
         CALL OS('X=X+Y   ', ECPL, E, S, C)
!
         DO I=1,NPOIN
            ZFMAX = ABS(ECPL%R(I)) - CRIT_CFD*HCPL%R(I)
            IF (ZFMAX.GT.1.D-8) NZFMAX=NZFMAX+1
         ENDDO
!
         IF (NCSIZE.GT.1) THEN
            NZFMAX=P_ISUM(NZFMAX)
            CALL PARCOM(ECPL,2,MESH)
         ENDIF
!
         IF (NZFMAX.GE.1) CONSTFLOW = .FALSE.
      ENDIF
!
      IF(.NOT.CONSTFLOW) THEN
         CALL OS('X=C     ', ECPL,  S, S, 0.D0)
         CALL OS('X=Y     ', HCPL, HN, S,    C)
!
         IF (NCSIZE.GT.1) THEN
            CALL PARCOM(ECPL,2,MESH)
            CALL PARCOM(HCPL,2,MESH)
         ENDIF
      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
