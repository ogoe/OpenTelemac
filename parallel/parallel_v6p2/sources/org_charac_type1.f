!                    ***************************
                     SUBROUTINE ORG_CHARAC_TYPE1 
!                    *************************** 
! 
     &(NOMB,TRACE,CHARACTERISTIC)                      
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MPI TYPE FOR TYPE CHARAC_TYPE - CHARACTERISTICS /
!
!history  C. DENIS
!+        01/07/2011
!+        V6P1
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
!| NOMB           |<---| NUMBER OF VARIABLES 
!| TRACE          |<---| IF .TRUE. TRACE EXECUTION
!| CHARACTERISTIC |--->| DATATYPE FOR CHARACTERISTIC 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!           
      IMPLICIT NONE 
      INCLUDE 'mpif.h' 
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      INTEGER, INTENT(IN)    :: NOMB 
      INTEGER, INTENT(INOUT) :: CHARACTERISTIC 
      LOGICAL, INTENT(IN)    ::TRACE 
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, PARAMETER :: MAX_BASKET_SIZE=10 
!
!     NOTE JMH: CHARAC_TYPE IS NOT USED HERE !!!  CH NOT USED
!
      TYPE CHARAC_TYPE 
        SEQUENCE                  
!       BUT SEEMS USELESS (HENCE TRICK BELOW WITH VOID)  
        INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD) 
        INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO  
        INTEGER :: INE   ! THE LOCAL 2D ELEMENT NR THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION    
        INTEGER :: KNE   ! THE LOCAL LEVEL THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION    
        INTEGER :: IOR   ! THE POSITION OF THE TRAJECTORY -HEAD- IN MYPID [THE 2D/3D NODE OF ORIGIN] 
        INTEGER :: ISP,NSP ! NUMBERS OF RUNGE-KUTTA PASSED AS COLLECTED AND TO FOLLOW AT ALL 
        INTEGER :: VOID  ! TRICK FOR ALIGNMENT 
        DOUBLE PRECISION :: XP,YP,ZP                ! THE (X,Y,Z)-POSITION NOW  
        DOUBLE PRECISION :: BASKET(MAX_BASKET_SIZE) ! VARIABLES INTERPOLATED AT THE FOOT   
      END TYPE CHARAC_TYPE 
!
!     ARRAY OF DISPLACEMENTS BETWEEN BASIC COMPONENTS, HERE INITIALISED ONLY 
!
      INTEGER (KIND=MPI_ADDRESS_KIND), DIMENSION(12) ::  
     &       CH_DELTA=  (/0,0,0,0,0,0,0,0,0,0,0,0/) 
! 
!     ARRAY OF BLOCKLENGTHS OF TYPE COMPONENTS, BASKET INITIALISED TO 1 
!
      INTEGER, DIMENSION(12) :: CH_BLENGTH=(/1,1,1,1,1,1,1,1,1,1,1,1/)  
!     ARRAY OF COMPONENT TYPES IN TERMS OF THE MPI COMMUNICATION  
      INTEGER, DIMENSION(12) :: CH_TYPES 
      INTEGER INTEX, IBASE, IER       
      INTEGER (KIND=MPI_ADDRESS_KIND) :: EXTENT,ILB,IUB 
      TYPE(CHARAC_TYPE) :: CH 
!          
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
      INTEGER I 
!
!     NOTE JMH : P_MPI_ADDRESS 2 AND 3 ARE IN PARALLEL LIBRARY 
!                THEY ALL CALL MPI_ADDRESS BUT WITH DIFFERENT 
!                DATA TYPES (THIS IS TO ENABLE COMPILING BY NAG)            
!      
      CH_BLENGTH=(/1,1,1,1,1,1,1,1,1,1,1,1/)  
      CH_DELTA=  (/0,0,0,0,0,0,0,0,0,0,0,0/) 
      CALL P_MPI_TYPE_GET_EXTENT(MPI_INTEGER,ILB,INTEX,IER) 
      CH_DELTA(1)=0 
      DO I=2,7 
         CH_DELTA(I)=CH_DELTA(I-1)+INTEX 
      ENDDO 
      CALL P_MPI_TYPE_GET_EXTENT(MPI_DOUBLE_PRECISION,ILB,INTEX,IER) 
!     IF YOU KNOW A COMPILER WITH REAL8 NOT OF A SIZE 8, WARN US !! 
!     INTEX=8 
!     BEWARE TRICK: INTEGER NUMBER 7 IS TREATED HERE AS A REAL8 BECAUSE 
!                       WE HAVE ADDED INTEGER VOID BEHIND. IF WE DO NOT ADD 
!     VOID, SOME COMPILERS WILL ADD A GAP IN THE DATA 
!     
      DO I=8,11 
         CH_DELTA(I)=CH_DELTA(I-1)+INTEX 
      ENDDO 
      CH_DELTA(12)=CH_DELTA(11)+INTEX*MAX_BASKET_SIZE         
!     
      IF(NOMB.GT.0.AND.NOMB.LE.MAX_BASKET_SIZE) THEN  
         CH_BLENGTH(11) = NOMB  ! CH%BASKET RANGE APPLIED FOR COMMUNICATION   
      ELSE 
         WRITE(LU,*) ' @STREAMLINE::ORG_CHARAC_TYPE::', 
     &        ' NOMB NOT IN RANGE [1..MAX_BASKET_SIZE]' 
         WRITE(LU,*) ' MAX_BASKET_SIZE, NOMB: ',MAX_BASKET_SIZE,NOMB 
         CALL PLANTE(1) 
         STOP  
      ENDIF 
      CH_TYPES(1)=MPI_INTEGER 
      CH_TYPES(2)=MPI_INTEGER 
      CH_TYPES(3)=MPI_INTEGER 
      CH_TYPES(4)=MPI_INTEGER 
      CH_TYPES(5)=MPI_INTEGER 
      CH_TYPES(6)=MPI_INTEGER 
      CH_TYPES(7)=MPI_INTEGER 
      CH_TYPES(8)=MPI_DOUBLE_PRECISION 
      CH_TYPES(9)=MPI_DOUBLE_PRECISION 
      CH_TYPES(10)=MPI_DOUBLE_PRECISION 
      CH_TYPES(11)=MPI_DOUBLE_PRECISION 
      CH_TYPES(12)=MPI_UB       ! THE TYPE UPPER BOUND MARKER           
      CALL P_MPI_TYPE_CREATE_STRUCT(12,CH_BLENGTH,CH_DELTA,CH_TYPES, 
     &                              CHARACTERISTIC,IER) 
      CALL P_MPI_TYPE_COMMIT(CHARACTERISTIC,IER) 
      CALL P_MPI_TYPE_GET_EXTENT(CHARACTERISTIC,ILB,EXTENT,IER) 
      IUB=ILB+EXTENT 
!      
!     DEPRECATED MPI-1 ROUTINES REMPLACED BY P_MPI_TYPE_GET_EXTENT 
!     CALL P_MPI_TYPE_LB(CHARACTERISTIC,ILB,IER) 
!     CALL P_MPI_TYPE_UB(CHARACTERISTIC,IUB,IER)
! 
      IF(ILB.NE.CH_DELTA(1).OR.IUB.NE.CH_DELTA(12)) THEN 
        WRITE(LU,*) ' @STREAMLINE::ORG_CHARAC_TYPE:' 
        WRITE(LU,*) ' MEMORY PROBLEM WITH THIS COMPILER: ' 
        WRITE(LU,*) ' ILB=',ILB,' NOT EQUAL TO CH_DELTA(1)=', 
     &        CH_DELTA(1) 
        WRITE(LU,*) ' OR' 
        WRITE(LU,*) ' IUB=',IUB,' NOT EQUAL TO CH_DELTA(12)=', 
     &        CH_DELTA(12) 
        CALL PLANTE(1) 
        STOP 
      ENDIF 
      IF(TRACE) THEN 
         WRITE(LU,*) ' @STREAMLINE::ORG_CHARAC_TYPE:' 
         WRITE(LU,*) ' MAX_BASKET_SIZE: ', MAX_BASKET_SIZE 
!        WRITE(LU,*) ' SIZE(CH%BASKET): ',SIZE(CH%BASKET) 
         WRITE(LU,*) ' CH_DELTA: ',CH_DELTA 
         WRITE(LU,*) ' CH_BLENGTH: ',CH_BLENGTH 
         WRITE(LU,*) ' CH_TYPES: ',CH_TYPES 
         WRITE(LU,*) ' CHARACTERISTIC: ',CHARACTERISTIC 
         WRITE(LU,*) ' ILB, IUB: ',ILB, IUB 
      ENDIF 
      IF (TRACE) WRITE(LU,*) ' -> LEAVING ORG_CHARAC_TYPE' 
!     
!----------------------------------------------------------------------
!     
      RETURN  
      END SUBROUTINE ORG_CHARAC_TYPE1 
 
 
