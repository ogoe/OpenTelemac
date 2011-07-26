!                    *****************************
                     SUBROUTINE BUILD_GLOBAL_BOUND
!                    *****************************
!
     &(KNOLG,NPOIN,NPOIN_TOT,X,Y,K,C,CG,XT,YT,KT,CTT,CGT)
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    REBUILDS THE BOUNDARY OF THE MESH (GLOBAL NODES).
!
!warning  TO BE USED ONLY IN PARALLEL MODE
!
!history  C. DENIS (SINETICS)
!+        16/01/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| LOCAL PHASE VELOCITIES
!| CG             |-->| LOCAL GROUP VELOCITIES
!| CGT            |<--| GLOBAL GROUP VELOCITIES
!| CTT            |<--| GLOBAL PHASE VELOCITIES
!| K              |-->| LOCAL WAVE NUMBER
!| KNOLG          |<--| GLOBAL NUMBER OF A LOCAL POINT IN PARALLEL
!| KT             |<--| GLOBAL WAVE NUMBER
!| NPOIN          |-->| NUMBER OF LOCAL POINTS
!| NPOIN_TOT      |<--| NUMBER OF GLOBAL POINTS
!| X              |-->| LOCAL X VECTOR
!| XT             |<--| GLOBAL X VECTOR
!| Y              |-->| LOCAL X VECTOR
!| YT             |<--| GLOBAL Y VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INCLUDE 'mpif.h'
!
      INTEGER, INTENT(IN) :: NPOIN_TOT
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN), DIMENSION(NPOIN) :: KNOLG
      DOUBLE PRECISION, INTENT(IN), DIMENSION(NPOIN)  :: X, Y, K, C,CG
      DOUBLE PRECISION , INTENT(OUT), DIMENSION(NPOIN_TOT) :: XT,YT,KT,
     &     CTT,CGT
      INTEGER :: I
      INTEGER :: IER
      INTEGER, ALLOCATABLE :: TEMP1(:)
      INTEGER, ALLOCATABLE :: TEMP2(:)
      DOUBLE PRECISION, ALLOCATABLE :: TEMP3(:)
      DOUBLE PRECISION :: TMP
      ALLOCATE(TEMP3(NPOIN_TOT))
      YT(:)=0.D0
      XT(:)=0.D0
      CTT(:)=0.D0
      CGT(:)=0.D0
      KT(:)=0.D0
!     XT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=X(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,XT,NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(XT .EQ. -HUGE(TMP))
         XT=0.0
      END WHERE
!     YT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=Y(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,YT, NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(YT .EQ. -HUGE(TMP))
         YT=0.0
      END WHERE
!     CT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=C(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,CTT, NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(CTT .EQ. -HUGE(TMP))
         CTT=0.0
      END WHERE
!     CGT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=CG(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,CGT, NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(CGT .EQ.-HUGE(TMP))
         CGT=0.0
      END WHERE
!     KT MERGING
      TEMP3(:)=-HUGE(TMP)
      DO I=1,NPOIN
         TEMP3(KNOLG(I))=K(I)
      END DO
      CALL MPI_ALLREDUCE(TEMP3,KT, NPOIN_TOT,MPI_REAL8,MPI_MAX,
     &     MPI_COMM_WORLD,IER)
      WHERE(KT .EQ. -HUGE(TMP))
         KT=0.0
      END WHERE
      DEALLOCATE(TEMP3)
      RETURN
      END
