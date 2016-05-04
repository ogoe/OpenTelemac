!                      **************************
                       SUBROUTINE SOLVE_MUMPS_PAR
!                      **************************
!
     &(NPOIN,NSEGB,GLOSEG,MAXSEG,DA,XA,XINC,RHS,INFOGR,TYPEXT,KNOLG,
     & NPOIN_TOT,IPID)
!
!***********************************************************************
!  BIEF VERSION 7.1
!***********************************************************************
!
!brief    PARALLEL DIRECT SYSTEM SOLUTION
!+
!
!history  C. DENIS (SINETICS)
!+        26/02/2010
!+        V6P0
!+     First version
!
!history  J-M HERVOUET (LNHE)
!+        23/11/2015
!+        V7P1
!+   Correction in the case of non symmetric matrix.
!+   Decommenting the deallocation of allocatable arrays.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF POINTS IN THE PROCESSOR.
!| ...            |-->| ...
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     PARALLEL SOLVING USING MUMPS
      USE DECLARATIONS_PARALLEL
!
      IMPLICIT NONE
!     structures MPI et MUMPS
#if defined (HAVE_MUMPS)
      INCLUDE 'dmumps_struc.h'
#endif
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NSEGB,MAXSEG,NPOIN_TOT
      INTEGER, INTENT(IN)             :: IPID,GLOSEG(MAXSEG,2)
      LOGICAL, INTENT(IN)             :: INFOGR
      DOUBLE PRECISION, INTENT(INOUT) :: XA(*),RHS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XINC(NPOIN),DA(NPOIN)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPEXT
      INTEGER, INTENT(IN)             :: KNOLG(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      COMMON/INFO/LNG,LU
      INTEGER LNG,LU
#if defined (HAVE_MUMPS)
!
      DOUBLE PRECISION TIME_IN_SECONDS2
      EXTERNAL         TIME_IN_SECONDS2
      DOUBLE PRECISION T1,T2,T3,T4,T5
      TYPE (DMUMPS_STRUC) MUMPS_PAR
!     INDIRECT ARRAYS
      DOUBLE PRECISION ,ALLOCATABLE :: TEMP1(:),TEMP2(:),TEMP3(:)
      INTEGER TAILLE
      INTEGER I,J,K,ERR,NBELEM,IERR,II,NC,IER
!
!     MUMPS INITIALISATION
      MUMPS_PAR%COMM = MPI_COMM_WORLD
      MUMPS_PAR%JOB = -1
      IF(TYPEXT.EQ.'S') THEN
        MUMPS_PAR%SYM = 2
      ELSE
        MUMPS_PAR%SYM = 0
      ENDIF
      MUMPS_PAR%PAR = 1
      CALL DMUMPS(MUMPS_PAR)
!     NO INFORMATION COMING FROM MUMPS ARE PRINTED
!     TO BE MODIFIED TO INTRODUCE A DEBGUGGING MODE
      MUMPS_PAR%ICNTL(3)=-1
      MUMPS_PAR%ICNTL(4)=0
!     THE MATRIX IS DISTRIBUTED OVER PROCESSORS AND PROVIDED BY THE USER
      MUMPS_PAR%ICNTL(18)=3
!     THE MATRIX IS ASSEMBLED
      MUMPS_PAR%ICNTL(5)=0
!     AUTOMATIC SCALING OF THE MATRIX
      MUMPS_PAR%ICNTL(6)=7
!     AUTOMATIC REORDERING OF THE MATRIX
      MUMPS_PAR%ICNTL(7)=7
      MUMPS_PAR%ICNTL(11)=1
!
      MUMPS_PAR%ICNTL(8)=77
      MUMPS_PAR%ICNTL(11)=77
      MUMPS_PAR%ICNTL(10)=10
      MUMPS_PAR%ICNTL(14)=50
      MUMPS_PAR%ICNTL(13)=40
!
!     SIZE OF THE MATRIX
      MUMPS_PAR%N = 2*NPOIN_TOT
      IF(TYPEXT.EQ.'S') THEN
        MUMPS_PAR%NZ_LOC = NPOIN+NSEGB
      ELSE
        MUMPS_PAR%NZ_LOC = NPOIN+2*NSEGB
      ENDIF
      IF (.NOT. ASSOCIATED(MUMPS_PAR%IRN_LOC)) THEN
        ALLOCATE(TEMP1(MUMPS_PAR%N),STAT=ERR)
        IF(ERR.NE.0) GOTO 100
        ALLOCATE(TEMP2(MUMPS_PAR%N),STAT=ERR)
        IF(ERR.NE.0) GOTO 100
        ALLOCATE(TEMP3(NPOIN),STAT=ERR)
        IF(ERR.NE.0) GOTO 100
        ALLOCATE(MUMPS_PAR%IRN_LOC(MUMPS_PAR%NZ_LOC),STAT=ERR)
        IF(ERR.NE.0) GOTO 100
        ALLOCATE(MUMPS_PAR%JCN_LOC(MUMPS_PAR%NZ_LOC),STAT=ERR)
        IF(ERR.NE.0) GOTO 100
        ALLOCATE(MUMPS_PAR%A_LOC(MUMPS_PAR%NZ_LOC),STAT=ERR)
        IF(ERR.NE.0) GOTO 100
        ALLOCATE(MUMPS_PAR%RHS(MUMPS_PAR%N),STAT=ERR)
        IF(ERR.NE.0) GOTO 100
        GOTO 101
 100    CONTINUE
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
        CALL PLANTE(1)
        STOP
 101    CONTINUE
      END IF
      TEMP1(:)=0.D0
      TEMP2(:)=0.D0
      TEMP3(:)=0.D0
      MUMPS_PAR%IRN_LOC(:)=0
      MUMPS_PAR%JCN_LOC(:)=0
      MUMPS_PAR%A_LOC(:)=0.0
      MUMPS_PAR%RHS(:)=0.0
      DO K = 1,NPOIN
        IF (K  .LE.  NPOIN/2) THEN
          MUMPS_PAR%IRN_LOC(K) = KNOLG(K)
          MUMPS_PAR%JCN_LOC(K) = KNOLG(K)
          TEMP1(KNOLG(K))=RHS(K)
          TEMP3(K)=KNOLG(K)
        ELSE
          MUMPS_PAR%IRN_LOC(K) = KNOLG(K-NPOIN/2) + MUMPS_PAR%N/2
          MUMPS_PAR%JCN_LOC(K) = KNOLG(K-NPOIN/2) + MUMPS_PAR%N/2
          TEMP1(KNOLG(K-NPOIN/2)+ MUMPS_PAR%N/2)=RHS(K)
          TEMP3(K)=KNOLG(K-NPOIN/2)+ MUMPS_PAR%N/2
        END IF
        MUMPS_PAR%A_LOC(K) = DA(K)
      ENDDO
!     GLOBAL REDUCTION OF RHS VECTOR
      CALL MPI_ALLREDUCE(TEMP1,TEMP2,MUMPS_PAR%N,MPI_DOUBLE_PRECISION,
     &     MPI_SUM,
     &     MPI_COMM_WORLD,IER)
      DO I=1,MUMPS_PAR%N
        MUMPS_PAR%RHS(I)=TEMP2(I)
      END DO
!
      NBELEM = NPOIN
      IF(TYPEXT.EQ.'S') THEN
        DO K = 1,NSEGB
          I = TEMP3(GLOSEG(K,1))
          J = TEMP3(GLOSEG(K,2))
          NBELEM = NBELEM + 1
          IF(I.LT.J) THEN
            MUMPS_PAR%IRN_LOC(NBELEM) = I
            MUMPS_PAR%JCN_LOC(NBELEM) = J
          ELSE
            MUMPS_PAR%IRN_LOC(NBELEM) = J
            MUMPS_PAR%JCN_LOC(NBELEM) = I
          ENDIF
          MUMPS_PAR%A_LOC(NBELEM) = XA(K)
        ENDDO
      ELSE
        DO K = 1,NSEGB
          I = TEMP3(GLOSEG(K,1))
          J = TEMP3(GLOSEG(K,2))
          NBELEM = NBELEM + 1
          MUMPS_PAR%IRN_LOC(NBELEM) = I
          MUMPS_PAR%JCN_LOC(NBELEM) = J
          MUMPS_PAR%A_LOC(NBELEM) = XA(K)
        ENDDO
        DO K = 1,NSEGB
          I = TEMP3(GLOSEG(K,2))
          J = TEMP3(GLOSEG(K,1))
          NBELEM = NBELEM + 1
          MUMPS_PAR%IRN_LOC(NBELEM) = I
          MUMPS_PAR%JCN_LOC(NBELEM) = J
          MUMPS_PAR%A_LOC(NBELEM) = XA(K+NSEGB)
        ENDDO
      ENDIF
!
!     -----------
!     RESOLUTION
!     -----------
!
      MUMPS_PAR%JOB = 6
      CALL DMUMPS(MUMPS_PAR)
      TEMP1(:)=0.0
      IF(MUMPS_PAR%MYID.EQ. 0 ) THEN
!       An error occured...
        IF(MUMPS_PAR%INFO(1).LT.0) THEN
          IF(LNG.EQ.1) WRITE(LU,1001) MUMPS_PAR%INFO(1)
          IF(LNG.EQ.2) WRITE(LU,2001) MUMPS_PAR%INFO(1)
          CALL PLANTE(1)
          STOP
        ENDIF
        DO K = 1,MUMPS_PAR%N
          TEMP1(K)=MUMPS_PAR%RHS(K)
        END DO
      END IF
      TEMP2(:)=0.0
!
      CALL MPI_BCAST(TEMP1,MUMPS_PAR%N,MPI_DOUBLE_PRECISION,
     &               0,MPI_COMM_WORLD,IER)
      DO  K = 1,NPOIN
        I=TEMP3(K)
        XINC(K)=TEMP1(I)
      ENDDO
!
!     DEALLOCATION
!
      DEALLOCATE(TEMP1)
      DEALLOCATE(TEMP2)
      DEALLOCATE(TEMP3)
      DEALLOCATE(MUMPS_PAR%IRN_LOC)
      DEALLOCATE(MUMPS_PAR%JCN_LOC)
      DEALLOCATE(MUMPS_PAR%A_LOC)
      DEALLOCATE(MUMPS_PAR%RHS)
!
!     Killing the current instance
!
      MUMPS_PAR%JOB = -2
      CALL DMUMPS(MUMPS_PAR)
      RETURN
1000  FORMAT(1X,'SOLVE_MUMPS : ERREUR A L''ALLOCATION DE MEMOIRE : '
     &,/,1X,'CODE D''ERREUR : ',1I6)
1001  FORMAT(1X,'SOLVE_MUMPS : ERREUR LORS DE LA RESOLUTION : '
     &,/,1X,'CODE D''ERREUR INFO(1) : ',1I6)
2000  FORMAT(1X,'SOLVE_MUMPS: ERROR DURING ALLOCATION OF MEMORY: '
     &,/,1X,'ERROR CODE: ',1I6)
2001  FORMAT(1X,'SOLVE_MUMPS: ERROR DURING SOLVE: '
     &     ,/,1X,'ERROR CODE INFO(1): ',1I6)
#else
      IF(LNG.EQ.1) WRITE(LU,2018)
      IF(LNG.EQ.2) WRITE(LU,2019)
2018  FORMAT(1X,'MUMPS_PAR NON INSTALLE SUR CE SYSTEME,',/,1X,
     &     'CHOISIR UNE AUTRE METHODE',///)
2019  FORMAT(1X,'MUMPS_PAR NOT INSTALLED IN THIS SYSTEM',/,1X,
     &     'CHOOSE OTHER METHOD ',///)
      CALL PLANTE(1)
      STOP
!
#endif
!
      END

