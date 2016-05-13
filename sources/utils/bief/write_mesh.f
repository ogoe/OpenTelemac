!                    *********************
                     SUBROUTINE WRITE_MESH
!                    *********************
!
     &(FFORMAT,NFILE,MESH,NPLAN,DATE,TIME)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    WRITES THE MESH, DESCRIBED BY THE BIEF_MESH STRUCTURE
!+        INTO THE FILE. BIEF_MESH STRUCTURE CONTAINS INFORMATIONS
!+        ABOUT CONNECTIVITY, COORDINATES, BOUNDARY NODES. OTHER
!+        INFORMATIONS NEEDED : THE DATE AND TIME INFORMATION, AND
!+        THE ORIGIN OF THE COORDINATE SYSTEM (X_ORIG,Y_ORIG).
!
!history  R NEBAUER (LNHE)
!+        25/11/08
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
!history  U.H.Merkel
!+        21/07/2012
!+        V6P2
!+   Changed to work with NAG
!
!history  Y AUDOUIN
!+        21/05/2015
!+        V7P0
!+   Adapt code to work with the hermes module
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        26/06/2015
!+        V7P1
!+   Deallocate of IPOBO must always be done.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FILE FORMAT
!| NFILE          |-->| LOGICAL UNIT OF FILE
!| MESH           |-->| MESH STRUCTURE
!| NPLAN          |-->| NUMBER OF PLANES (3D)
!| DATE           |-->| 3 INTEGERS (YEAR, MONTH, DAY)
!| TIME           |-->| 3 INTEGERS (HOUR, MINUTE, SECOND)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_WRITE_MESH => WRITE_MESH
      USE INTERFACE_HERMES
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8) ,     INTENT(IN) :: FFORMAT
      INTEGER          ,     INTENT(IN) :: NFILE,NPLAN
      TYPE(BIEF_MESH),       INTENT(IN) :: MESH
      INTEGER, DIMENSION(3), INTENT(IN) :: DATE
      INTEGER, DIMENSION(3), INTENT(IN) :: TIME
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: IERR, I, NDP
      INTEGER, ALLOCATABLE :: IPOBO(:)
!
!-----------------------------------------------------------------------
!
!     BUILDING IPOBO ONLY IN SERIAL RUN
      IF(NCSIZE.LE.1) THEN
        ALLOCATE(IPOBO(MESH%NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'IPOBO')
        DO I=1,MESH%NPOIN
          IPOBO(I) = 0
        END DO
        DO I=1,MESH%NPTFR
          IPOBO(MESH%NBOR%I(I)) = I
        END DO
      ELSE
        ! In case if nptir = 0 because then ipobo is written instead of knolg
        ALLOCATE(IPOBO(MESH%NPOIN),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'IPOBO')
        DO I=1,MESH%NPOIN
          IPOBO(I) = MESH%KNOLG%I(I)
        END DO
      ENDIF
!
      CALL CHECK_ALLOCATE(IERR,'IPOBO')
!
      NDP = MESH%NDS(MESH%TYPELM+1,3)
      IF(MESH%DIM1.EQ.3) THEN
        CALL SET_MESH(FFORMAT,NFILE,MESH%DIM1,MESH%TYPELM,NDP,
     &                MESH%NPTFR,NPTIR,MESH%NELEM,MESH%NPOIN,
     &                MESH%IKLE%I,IPOBO,MESH%KNOLG%I,MESH%X%R,MESH%Y%R,
     &                NPLAN,DATE,TIME,IERR,Z=MESH%Z%R)
        CALL CHECK_CALL(IERR,'WRITE_MESH:SET_MESH')
      ELSE
        CALL SET_MESH(FFORMAT,NFILE,MESH%DIM1,MESH%TYPELM,NDP,
     &                MESH%NPTFR,NPTIR,MESH%NELEM,MESH%NPOIN,
     &                MESH%IKLE%I,IPOBO,MESH%KNOLG%I,MESH%X%R,MESH%Y%R,
     &                NPLAN,DATE,TIME,IERR)
        CALL CHECK_CALL(IERR,'WRITE_MESH:SET_MESH')
      ENDIF
!
      DEALLOCATE(IPOBO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
