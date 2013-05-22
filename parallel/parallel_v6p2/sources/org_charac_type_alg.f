!                    ******************************
                     SUBROUTINE ORG_CHARAC_TYPE_ALG 
!                    ****************************** 
! 
     &(ALG_CHAR)                      
!
!***********************************************************************
! PARALLEL   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    RETURNS THE MPI TYPE FOR TYPE ALG_CHAR 
!
!history  A. JOLY
!+        22/05/2013
!+        V6P3
!+   First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALG_CHAR       |<-->| MPI TYPE FOR TYPE CHARAC_TYPE - ALG_CHAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!           
      IMPLICIT NONE 
      INCLUDE 'mpif.h' 
!
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      INTEGER, INTENT(INOUT) :: ALG_CHAR 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     ARRAY OF DISPLACEMENTS BETWEEN BASIC COMPONENTS, HERE INITIALISED ONLY 
!
      INTEGER (KIND=MPI_ADDRESS_KIND), DIMENSION(17) :: CH_DELTA
! 
!     ARRAY OF BLOCKLENGTHS OF TYPE COMPONENTS, BASKET INITIALISED TO 1 
!
      INTEGER, DIMENSION(17) :: CH_BLENGTH  
!     ARRAY OF COMPONENT TYPES IN TERMS OF THE MPI COMMUNICATION  
      INTEGER, DIMENSION(17) :: CH_TYPES 
      INTEGER IER,I      
      INTEGER (KIND=MPI_ADDRESS_KIND) :: EXTENT,ILB,IUB,INTEX 
!     
!----------------------------------------------------------------------
!
!     THE CODE BELOW HERE MUST BE CONSISTENT WITH THIS TYPE:
!
!     TYPE ALG_TYPE 
!       SEQUENCE   ! NECESSARY TO DEFINE MPI TYPE
!       INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD) 
!       INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO  
!       INTEGER :: IGLOB  ! THE GLOBAL NUMBER OF THE PARTICLES 
!       INTEGER :: FLAG  ! USED TO ALIGN FIELDS
!       DOUBLE PRECISION :: VX,VY,VZ  ! THE (X,Y,Z) PARTICLE VELOCITY  
!       DOUBLE PRECISION :: UX,UY,UZ  ! THE (X,Y,Z) FLUID VELOCITY  
!       DOUBLE PRECISION :: UX_AV,UY_AV,UZ_AV  ! THE (X,Y,Z) AVERAGE FLUID VELOCITY  
!       DOUBLE PRECISION :: K_AV,EPS_AV  ! THE VALUES OF K AND EPS  
!       DOUBLE PRECISION :: H_FLU  ! THE WATER DEPTH AT POSITION OF VELOCITY 
!     END TYPE ALG_TYPE 
!     
      CH_BLENGTH=(/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)  
      CH_DELTA=  (/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/) 
!
!     INTEGERS IN THE STRUCTURE
!
      CALL P_MPI_TYPE_GET_EXTENT(MPI_INTEGER,ILB,INTEX,IER) 
      CH_DELTA(1)=ILB
!     5 IS THE FIRST DOUBLE PRECISION THAT COMES AFTER AN INTEGER 
      DO I=2,5
        CH_DELTA(I)=CH_DELTA(I-1)+INTEX 
      ENDDO 
!
!     DOUBLE PRECISION IN THE STRUCTURE
!
      CALL P_MPI_TYPE_GET_EXTENT(MPI_DOUBLE_PRECISION,ILB,INTEX,IER) 
!     THE NINE REMAINING DOUBLE PRECISION (INCLUDING BASKET)     
      DO I=6,16
         CH_DELTA(I)=CH_DELTA(I-1)+INTEX 
      ENDDO 
!     ADDRESS AFTER THE THE LAST DOUBLE PRECISION
      CH_DELTA(17)=CH_DELTA(16)+INTEX      
!     
      CH_TYPES(1) =MPI_INTEGER          !MYPID
      CH_TYPES(2) =MPI_INTEGER          !NEPID
      CH_TYPES(3) =MPI_INTEGER          !IGLOB
      CH_TYPES(4) =MPI_INTEGER          !FLAG
      CH_TYPES(5) =MPI_DOUBLE_PRECISION !VX
      CH_TYPES(6) =MPI_DOUBLE_PRECISION !VY
      CH_TYPES(7) =MPI_DOUBLE_PRECISION !VZ
      CH_TYPES(8) =MPI_DOUBLE_PRECISION !UX
      CH_TYPES(9) =MPI_DOUBLE_PRECISION !UY
      CH_TYPES(10)=MPI_DOUBLE_PRECISION !UZ
      CH_TYPES(11)=MPI_DOUBLE_PRECISION !UX_AV
      CH_TYPES(12)=MPI_DOUBLE_PRECISION !UY_AV
      CH_TYPES(13)=MPI_DOUBLE_PRECISION !UZ_AV
      CH_TYPES(14)=MPI_DOUBLE_PRECISION !K_AV
      CH_TYPES(15)=MPI_DOUBLE_PRECISION !EPS_AV
      CH_TYPES(16)=MPI_DOUBLE_PRECISION !H_FLU
      CH_TYPES(17)=MPI_UB               !THE TYPE UPPER BOUND MARKER           
      CALL P_MPI_TYPE_CREATE_STRUCT(17,CH_BLENGTH,CH_DELTA,CH_TYPES, 
     &                              ALG_CHAR,IER) 
      CALL P_MPI_TYPE_COMMIT(ALG_CHAR,IER) 
      CALL P_MPI_TYPE_GET_EXTENT(ALG_CHAR,ILB,EXTENT,IER) 
      IUB=ILB+EXTENT 
!      
      IF(ILB.NE.CH_DELTA(1).OR.IUB.NE.CH_DELTA(17)) THEN 
        WRITE(LU,*) ' PARALLEL::ORG_CHARAC_TYPE_ALG:' 
        WRITE(LU,*) ' MEMORY PROBLEM WITH THIS COMPILER: ' 
        WRITE(LU,*) ' ILB=',ILB,' NOT EQUAL TO CH_DELTA(1)=', 
     &        CH_DELTA(1) 
        WRITE(LU,*) ' OR' 
        WRITE(LU,*) ' IUB=',IUB,' NOT EQUAL TO CH_DELTA(17)=', 
     &        CH_DELTA(17) 
        CALL PLANTE(1) 
        STOP 
      ENDIF 
!     
!----------------------------------------------------------------------
!     
      RETURN  
      END SUBROUTINE ORG_CHARAC_TYPE_ALG
