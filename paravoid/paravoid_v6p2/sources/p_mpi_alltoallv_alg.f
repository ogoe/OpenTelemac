!                    ******************************
                     SUBROUTINE P_MPI_ALLTOALLV_ALG
!                    ******************************
!
     &(I1,I2,I3,I4,I5,I6,I7,I8,I9,I10)
!
!***********************************************************************
! PARAVOID   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    CALLS FUNCTION MPI_ALLTOALLV with ALG_TYPE.
!
!warning  EMPTY SHELL IN SCALAR MODE FOR PARALLEL COMPATIBILITY
!
!history  C. DENIS (SINETICS)
!+        27/10/2009
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I1             |-->| SEND BUFFER  
!| I2             |-->| SPECIFIES THE NUMBER OF ELEMENTS TO SEND TO EACH
!|                |   | PROCESSOR 
!| I3             |-->| DISPLACEMENT ARRAY FOR THE SEND BUFFER 
!| I4             |-->| DATA TYPE OF SEND BUFFER ELEMENTS
!| I5             |-->| RECEIVE BUFFER
!| I6             |-->| SPECIFIES THE MAXIMUM NUMBER OF ELEMENTS THAT 
!|                |   | CAN BE RECEIVED FROM EACH PROCESSOR
!| I7             |-->| DISPLACEMENT ARRAY FOR THE RECEIVE BUFFER 
!| I8             |-->| DATA TYPE OF RECEIVE BUFFER ELEMENTS
!| I9             |-->| COMMUNICATOR 
!| I10            |-->| ERROR VALUE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      TYPE ALG_TYPE 
        SEQUENCE   ! NECESSARY TO DEFINE MPI TYPE ALG_CHAR
        INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD) 
        INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO  
        INTEGER :: IGLOB  ! THE GLOBAL NUMBER OF THE PARTICLES 
        INTEGER :: FLAG  ! USED TO ALIGN FIELDS
        DOUBLE PRECISION :: VX,VY,VZ  ! THE (X,Y,Z) PARTICLE VELOCITY  
        DOUBLE PRECISION :: UX,UY,UZ  ! THE (X,Y,Z) FLUID VELOCITY  
        DOUBLE PRECISION :: UX_AV,UY_AV,UZ_AV  ! THE (X,Y,Z) AVERAGE FLUID VELOCITY  
        DOUBLE PRECISION :: K_AV,EPS_AV  ! THE VALUES OF K AND EPS  
        DOUBLE PRECISION :: H_FLU  ! THE WATER DEPTH AT POSITION OF VELOCITY 
      END TYPE ALG_TYPE 
      INTEGER, INTENT(IN)  :: I2(*),I3(*),I4,I6(*),I7(*),I8,I9
      INTEGER, INTENT(OUT) :: I10
      TYPE(ALG_TYPE), INTENT(IN)  :: I1(*)
      TYPE(ALG_TYPE), INTENT(OUT) :: I5(*)
!
!-----------------------------------------------------------------------
!
      I5(1)%MYPID=0
      I10=0
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE  P_MPI_ALLTOALLV_ALG  VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_MPI_ALLTOALLV_ALG VOID'
!
!-----------------------------------------------------------------------
!
      STOP
      END
