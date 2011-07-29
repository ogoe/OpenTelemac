!                    ******************************
                     SUBROUTINE P_ORG_FONCTION_TYPE
!                    ****************************** 
! 
     &(NOMB,TRACE,FONCTION)                      
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MPI TYPE FOR TYPE CHARAC_TYPE - CHARACTERISTICS /
!+        USED BY TOMAWAC ONLY
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
! 
      INTEGER, PARAMETER :: MAX_BASKET_SIZE=10 
      INTEGER, INTENT(INOUT) :: NOMB 
      INTEGER, INTENT(INOUT)  :: FONCTION
      LOGICAL, INTENT(IN) ::TRACE 
      TYPE FONCTION_TYPE
          INTEGER :: MYPID          ! PARTITION OF THE TRACEBACK ORIGIN (HEAD)
          INTEGER :: NEPID          ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO
          INTEGER :: INE   ! THE LOCAL 2D ELEMENT NR THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION
          INTEGER :: KNE   ! THE LOCAL LEVEL THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION
          INTEGER :: IOR   ! THE POSITION OF THE TRAJECTORY -HEAD- IN MYPID [THE 2D/3D NODE OF ORIGIN]
          INTEGER :: ISP,NSP ! NUMBERS OF RUNGE-KUTTA PASSED AS COLLECTED AND TO FOLLOW AT ALL
          DOUBLE PRECISION :: XP,YP,ZP                ! THE (X,Y,Z)-POSITION NOW
          DOUBLE PRECISION :: SHP1,SHP2,SHP3,SHZ
          DOUBLE PRECISION :: BP
          DOUBLE PRECISION :: F(6) ! FUNCTION VALUES AT THE 6 POINT OF THE PRISM
      END TYPE FONCTION_TYPE
!           
      INTEGER (KIND=MPI_ADDRESS_KIND)  :: INTEX,ILB,IUB 
      TYPE(FONCTION_TYPE) :: CH 
                                ! ARRAY OF BLOCKLENGTHS OF TYPE COMPONENTS, NOTE THE BASKET INITIALISED TO 1
      INTEGER, DIMENSION(17) :: FC_BLENGTH=
     &     (/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/)
                                ! ARRAY OF DISPLACEMENTS BETWEEN BASIC COMPONENTS, HERE INITIALISED ONLY
      INTEGER, DIMENSION(17) :: FC_DELTA=
     &     (/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
                                ! ARRAY OF COMPONENT TYPES IN TERMS OF THE MPI COMMUNICATION
      INTEGER, DIMENSION(17) :: FC_TYPES
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
      INTEGER I,IER,IBASE 
      CALL P_MPI_ADDRESS (CH%MYPID,  FC_DELTA(1),  IER)
      CALL P_MPI_ADDRESS (CH%NEPID,  FC_DELTA(2),  IER)
      CALL P_MPI_ADDRESS (CH%INE,    FC_DELTA(3),  IER)
      CALL P_MPI_ADDRESS (CH%KNE,    FC_DELTA(4),  IER)
      CALL P_MPI_ADDRESS (CH%IOR,    FC_DELTA(5),  IER)
      CALL P_MPI_ADDRESS (CH%ISP,    FC_DELTA(6),  IER)
      CALL P_MPI_ADDRESS (CH%NSP,    FC_DELTA(7),  IER)
      CALL P_MPI_ADDRESS2(CH%XP,     FC_DELTA(8),  IER)
      CALL P_MPI_ADDRESS2(CH%YP,     FC_DELTA(9),  IER)
      CALL P_MPI_ADDRESS2(CH%ZP,     FC_DELTA(10), IER)
      CALL P_MPI_ADDRESS2(CH%SHP1,   FC_DELTA(11), IER)
      CALL P_MPI_ADDRESS2(CH%SHP2,   FC_DELTA(12), IER)
      CALL P_MPI_ADDRESS2(CH%SHP3,   FC_DELTA(13), IER)
      CALL P_MPI_ADDRESS2(CH%SHZ,    FC_DELTA(14), IER)
      CALL P_MPI_ADDRESS2(CH%BP,     FC_DELTA(15), IER)
      CALL P_MPI_ADDRESS3(CH%F,      FC_DELTA(16), IER)
!
      CALL P_MPI_TYPE_GET_EXTENT(MPI_REAL8,ILB,INTEX,IER)
                                ! MARKING THE END OF THE TYPE
      FC_DELTA(17) = FC_DELTA(16) + 6*INTEX ! MPI_UB POSITION
      IBASE = FC_DELTA(1)
      FC_DELTA = FC_DELTA - IBASE ! RELATIVE ADDRESSES
      IF (NOMB>0.AND.NOMB<=6) THEN
         FC_BLENGTH(16) = NOMB  ! CH%BASKET RANGE APPLIED FOR COMMUNICATION
      ELSE
         WRITE(LU,*) ' @STREAMLINE::ORG_CHARAC_TYPE::',
     &        ' NOMB NOT IN RANGE [1..MAX_BASKET_SIZE]'
         WRITE(LU,*) ' MAX_BASKET_SIZE, NOMB: ',6
         CALL PLANTE(1)
         STOP
      ENDIF
      FC_TYPES(1)=MPI_INTEGER
      FC_TYPES(2)=MPI_INTEGER
      FC_TYPES(3)=MPI_INTEGER
      FC_TYPES(4)=MPI_INTEGER
      FC_TYPES(5)=MPI_INTEGER
      FC_TYPES(6)=MPI_INTEGER
      FC_TYPES(7)=MPI_INTEGER
      FC_TYPES(8)=MPI_REAL8
      FC_TYPES(9)=MPI_REAL8
      FC_TYPES(10)=MPI_REAL8
      FC_TYPES(11)=MPI_REAL8
      FC_TYPES(12)=MPI_REAL8
      FC_TYPES(13)=MPI_REAL8
      FC_TYPES(14)=MPI_REAL8
      FC_TYPES(15)=MPI_REAL8
      FC_TYPES(16)=MPI_REAL8
      FC_TYPES(17)=MPI_UB       ! THE TYPE UPPER BOUND MARKER
      CALL P_MPI_TYPE_CREATE_STRUCT(17,FC_BLENGTH,FC_DELTA,FC_TYPES,
     &                         FONCTION,IER)
      CALL P_MPI_TYPE_COMMIT(FONCTION,IER)
      CALL P_MPI_TYPE_GET_EXTENT(FONCTION,ILB,INTEX,IER)
      IUB=INTEX+ILB
      IF (TRACE) THEN
         WRITE(LU,*) ' @STREAMLINE::ORG_FONCTION_TYPE:'
         WRITE(LU,*) ' MAX_BASKET_SIZE: ', 6
         WRITE(LU,*) ' SIZE(CH%BASKET): ',SIZE(CH%F)
         WRITE(LU,*) ' CH_DELTA: ',FC_DELTA
         WRITE(LU,*) ' CH_BLENGTH: ',FC_BLENGTH
         WRITE(LU,*) ' CH_TYPES: ',FC_TYPES
         WRITE(LU,*) ' COMMITING MPI_TYPE_STRUCT: ', FONCTION
         WRITE(LU,*) ' MPI_TYPE_LB, MPI_TYPE_UB: ',ILB, IUB
      ENDIF
      IF (TRACE) WRITE(LU,*) ' -> LEAVING ORG_FONCTION_TYPE'      
!     
!----------------------------------------------------------------------
!     
      RETURN  
      END 
 
 
