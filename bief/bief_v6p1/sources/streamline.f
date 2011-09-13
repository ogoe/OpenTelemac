!                    *****************
                     MODULE STREAMLINE
!                    *****************
!
!
!***********************************************************************
! BIEF 6.1
!***********************************************************************
!
!brief    MODULE FOR PARALLEL CHARACTERISTICS FOR TELEMAC2D (OR TELEMAC3D)
!
!note     JAJ PINXIT :: JACEK.JANKOWSKI@BAW.DE :: ENJOY!
!+          JAJ STARTED WELL FRI JUL  4 12:26:57 CEST 2008
!+          JAJ RUNNING WELL FRI JUL 11 16:20:21 CEST 2008
!+          JAJ RUNNING EVEN BETTER WED JUL 16 18:25:35 CEST 2008
!+          JAJ PRIVATISED MON JUL 21 10:41:04 CEST 2008
!+          JAJ OPTIMISED WED JUL 30 15:40:45 CEST 2008
!
!warning  FORTRAN-77 CODE FORMATTING FOR TELEMAC SYSTEM!
!+          (ESPECIALLY IMPORTANT FOR COMMANDS TAKING MORE THAN ONE LINE)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE   
      PRIVATE  
      PUBLIC :: SCARACT     ! CALLED IN (MODIFIED) CHARAC OF TELEMAC2D FROM BIEF 
! 
!     MAX_BASKET_SIZE IS THE NUMBER OF ADVECTED VARIABLES IN ONE PROCEDURE CALL  
!     (CAPITULATED DUE TO MPI_TYPE-S FIELDS -> STATIC)  
! 
      INTEGER, PARAMETER :: MAX_BASKET_SIZE=10 ! LARGE 
! 
!     SEE CALL GET_MPI_PARAMETERS IN SCARACT         
      INTEGER MPI_INTEGER,MPI_REAL8,MPI_UB,MPI_COMM_WORLD,MPI_SUCCESS 
!  
!     THE TYPE FOR CHARACTERISTICS - LOST TRACEBACKS 
!     DESCRIBES A TRACEBACK LEAVING A PARTITION TO ANOTHER ONE   
!     FOR 2D WE USE 3D -> KNE AND ZP ARE OBSOLETE THEN 
! 
        TYPE CHARAC_TYPE 
          SEQUENCE   ! BUT SEEMS USELESS (HENCE TRICK BELOW WITH VOID)  
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
 
        ! ARRAY OF BLOCKLENGTHS OF TYPE COMPONENTS, NOTE THE BASKET INITIALISED TO 1 
        INTEGER, DIMENSION(12) :: CH_BLENGTH=(/1,1,1,1,1,1,1,1,1,1,1,1/)  
 
        ! ARRAY OF DISPLACEMENTS BETWEEN BASIC COMPONENTS, HERE INITIALISED ONLY  
        INTEGER, DIMENSION(12) :: CH_DELTA=  (/0,0,0,0,0,0,0,0,0,0,0,0/) 
 
        ! ARRAY OF COMPONENT TYPES IN TERMS OF THE MPI COMMUNICATION  
        INTEGER, DIMENSION(12) :: CH_TYPES 
 
        ! THE CORRESPONDING MPI TYPE  
        INTEGER :: CHARACTERISTIC 
 
        ! STRUCTURES FOR ALL-TO-ALL COMMUNICATION / SEND AND RECEIVE WITH COUNTERS 
        ! HEAP/SEND/RECVCOUNTS : COUNT THE NUMBER OF LOST TRACEBACKS PARTITION-WISE 
        ! S/RDISPLS : DISPLACEMENTS IN PARTITION-WISE SORTED SEND/RECVCHARS  
        ! HEAPCHAR : FOR SAVING INITIALLY LOST CHARACTERISTICS AND COLLECTING  
        !            THE IMPLANTED TRACEBACKS LOCALISED IN MY PARTITION  
        ! WHILE COLLECTING IS DONE IN HEAPCHARS, MOST ACTIVE OPERATIONS IN RECVCHAR 
        ! SENDCHAR REQUIRED DUE TO THE SPECIFIC SORTING FOR MPI_ALLTOALLV (OPTIMISE?)  
 
        TYPE (CHARAC_TYPE), ALLOCATABLE, DIMENSION(:), SAVE ::  
     &                                 HEAPCHAR, SENDCHAR, RECVCHAR 
        INTEGER, ALLOCATABLE, DIMENSION(:), SAVE :: SENDCOUNTS, SDISPLS 
        INTEGER, ALLOCATABLE, DIMENSION(:), SAVE :: RECVCOUNTS, RDISPLS 
        INTEGER, ALLOCATABLE, DIMENSION(:), SAVE :: HEAPCOUNTS 
 
        ! WORK FIELD FOR COUNTING OCCURANCES PRO RANK / SORTING SENDCHAR  
        INTEGER, ALLOCATABLE, DIMENSION(:), SAVE :: ICHA 
 
        ! IF SET TO TRUE, EVERY DETAILED DEBUGGING IS SWITCHED ON  
        LOGICAL :: TRACE=.FALSE. 
 
      CONTAINS  
 
  !--------------------------------------------------------------------- 
  !   <<<<<<<<<<<<<<<<<< CHARACTERISTICS: PRIVATE >>>>>>>>>>>>>>>>>> 
  !--------------------------------------------------------------------- 
        
        SUBROUTINE DEORG_CHARAC_TYPE 
          INTEGER IER 
          CALL P_MPI_TYPE_FREE(CHARACTERISTIC,IER) 
          RETURN 
        END SUBROUTINE DEORG_CHARAC_TYPE 
 
  !--------------------------------------------------------------------- 
  ! GET/SET ON DIMENSIONS AND COUNTERS  
  !--------------------------------------------------------------------- 
         
        INTEGER FUNCTION GET_MAX_BASKET_SIZE() 
          GET_MAX_BASKET_SIZE = MAX_BASKET_SIZE 
        END FUNCTION GET_MAX_BASKET_SIZE 
 
  !--------------------------------------------------------------------- 
  ! ALLOCATE ALL STATIC FIELDS FOR ALL-TO-ALL COMMUNICATION 
  ! PREPARE THE MPI_TYPE FOR LOST CHARACTERISTICS / TRACEBACKS  
  !--------------------------------------------------------------------- 
 
        SUBROUTINE ORGANISE_CHARS(NPARAM,NOMB,NCHDIM) ! WATCH OUT 
          USE BIEF_DEF, ONLY: NCSIZE 
          IMPLICIT NONE 
          INTEGER, INTENT(IN)  :: NPARAM,NOMB 
          INTEGER, INTENT(OUT) :: NCHDIM 
          INTEGER I 
          IF (.NOT.ALLOCATED(HEAPCOUNTS)) ALLOCATE(HEAPCOUNTS(NCSIZE)) 
          IF (.NOT.ALLOCATED(SENDCOUNTS)) ALLOCATE(SENDCOUNTS(NCSIZE)) 
          IF (.NOT.ALLOCATED(RECVCOUNTS)) ALLOCATE(RECVCOUNTS(NCSIZE)) 
          IF (.NOT.ALLOCATED(SDISPLS))    ALLOCATE(SDISPLS(NCSIZE)) 
          IF (.NOT.ALLOCATED(RDISPLS))    ALLOCATE(RDISPLS(NCSIZE)) 
          IF (.NOT.ALLOCATED(ICHA))       ALLOCATE(ICHA(NCSIZE)) 
          HEAPCOUNTS=0 
          SENDCOUNTS=0 
          RECVCOUNTS=0 
          SDISPLS=0 
          RDISPLS=0 
          ICHA=0 
          ! 
          NCHDIM=NPARAM 
          IF (.NOT.ALLOCATED(SENDCHAR)) ALLOCATE(SENDCHAR(NCHDIM)) 
          IF (.NOT.ALLOCATED(RECVCHAR)) ALLOCATE(RECVCHAR(NCHDIM)) 
          IF (.NOT.ALLOCATED(HEAPCHAR)) ALLOCATE(HEAPCHAR(NCHDIM)) 
          CALL ORG_CHARAC_TYPE1(NOMB,TRACE,CHARACTERISTIC) ! COMMIT THE CHARACTERISTICS TYPE FOR COMM. 
          RETURN 
        END SUBROUTINE ORGANISE_CHARS 
 
  !--------------------------------------------------------------------- 
  ! FOR COLLECTING CHARACTERISTICS LEAVING INITIALLY A GIVEN PARTITION  
  ! TO BE CALLED IN MODIFIED CHAR11 OR CHAR41 SIMILAR TO THE ORIGINAL 
  ! BIEF SUBROUTINES / NOTE THE COUNTER NCHARA/HEAPCHAR USAGE  
  !--------------------------------------------------------------------- 
 
        SUBROUTINE COLLECT_CHAR(MYPID,IOR,MYII,IFACE,KNE, 
     &                          ISP,NSP,XP,YP,ZP,IFAPAR, 
     &                          NCHDIM,NCHARA) 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER,  INTENT(IN) :: MYPID,IOR,MYII,IFACE,KNE 
          INTEGER,  INTENT(IN) :: ISP,NSP,NCHDIM 
          INTEGER,  INTENT(IN) :: IFAPAR(6,*) 
          INTEGER,  INTENT(INOUT) :: NCHARA 
          DOUBLE PRECISION, INTENT(IN) :: XP,YP,ZP 
          INTEGER :: NEPID,II,III 
          ! 
          IF(NCHARA==0) HEAPCOUNTS=0 
          NEPID=IFAPAR(IFACE  ,MYII) 
          II   =IFAPAR(IFACE+3,MYII)  
          NCHARA=NCHARA+1  
          IF(NCHARA>NCHDIM) THEN ! PROBABLY EXAGGERATED  
            WRITE (LU,*) 'NCHARA=',NCHARA,' NCHDIM=',NCHDIM  
            WRITE (LU,*) 'COLLECT_CHAR::NCHARA>NCHDIM, INCREASE NCHDIM'  
            CALL PLANTE(1) 
            STOP 
          ENDIF 
          HEAPCHAR(NCHARA)%MYPID=MYPID ! THE ORIGIN PID  
          HEAPCHAR(NCHARA)%NEPID=NEPID ! THE NEXT PID  
          HEAPCHAR(NCHARA)%INE=II      ! ELEMENT THERE  
          HEAPCHAR(NCHARA)%KNE=KNE     ! LEVEL THERE   
          HEAPCHAR(NCHARA)%IOR=IOR     ! THE ORIGIN 2D OR 3D NODE  
          HEAPCHAR(NCHARA)%ISP=ISP     ! R-K STEP AS COLLECTED 
          HEAPCHAR(NCHARA)%NSP=NSP     ! R-K STEPS TO BE DONE  
          HEAPCHAR(NCHARA)%XP=XP       ! X-POSITION  
          HEAPCHAR(NCHARA)%YP=YP       ! Y-POSITION  
          HEAPCHAR(NCHARA)%ZP=ZP       ! Z-POSITION 
!         TAGGING THE BASKET FOR DEBUGGING 
          DO III=1,10 
            HEAPCHAR(NCHARA)%BASKET(III)=1000.D0*III+NCHARA 
          ENDDO 
          ! 
          HEAPCOUNTS(NEPID+1)=HEAPCOUNTS(NEPID+1)+1 
          ! 
          RETURN 
        END SUBROUTINE COLLECT_CHAR 
 
  !--------------------------------------------------------------------- 
  ! THE NUMBER OF INITALLY COLLECTED LOST TRACEBACKS IS DIMINISHED  
  ! AFTER APPLYING THE JMH'S ALGORITHM - I.E. MARK ALL -INITIAL-  
  ! TRACEBACKS WHICH HAVE BEEN SUCCESFULLY COMPLETED AND LOCALISED  
  ! IN THE NEIGHBOUR PARTITION - THEY DO NOT HAVE TO BE TREATED 
  ! THIS IS VIRTUALLY THE SAME PROCEDURE AS IN BIEF CARACT 
  !--------------------------------------------------------------------- 
 
        SUBROUTINE WIPE_HEAPED_CHAR(RTEST,NPOIN,DOIT,NSEND,NLOSTCHAR, 
     *                              NCHDIM,NCHARA) 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN)     :: NPOIN,NCHDIM 
          INTEGER, INTENT(OUT)    :: NSEND,NLOSTCHAR 
          INTEGER, INTENT(INOUT)  :: NCHARA 
          DOUBLE PRECISION, INTENT(IN) :: RTEST(NPOIN) 
          LOGICAL, INTENT(IN) :: DOIT  
          INTEGER :: I 
          IF(NCHARA>NCHDIM) THEN  
            WRITE (LU,*) ' @STREAMLINE::WIPE_HEAPED_CHAR::NPOIN>NCHDIM'  
            CALL PLANTE(1) 
            STOP  
          ENDIF 
          NSEND=NCHARA 
          IF(DOIT) THEN  
            IF (TRACE) WRITE (LU,*) ' -> APPLYING JMH-ALGORITHM ' 
            DO I=1,NCHARA  
              IF(RTEST(HEAPCHAR(I)%IOR).GT.0.5D0) THEN 
                NSEND=NSEND-1 
                HEAPCOUNTS ( HEAPCHAR(I)%NEPID+1 ) =  
     &                 HEAPCOUNTS( HEAPCHAR(I)%NEPID+1 ) - 1  
                HEAPCHAR(I)%NEPID=-1 ! THIS IS THE MARKER FOR WIPING  
              ENDIF  
            END DO  
          ELSE 
            IF (TRACE) WRITE (LU,*) ' -> JMH-ALGORITHM -NOT- APPLIED' 
          ENDIF 
          NLOSTCHAR=NSEND ! SAVE THE NUMBER OF MY REALLY LOST CHARS  
          IF (TRACE) WRITE (LU,'(A,A,4(1X,I6))')  
     &           ' @STREAMLINE::WIPE_HEAPED_CHAR:: ', 
     &           'NSEND, NLOSTCHAR, NCHARA, SUM(HEAPCOUNTS): ', 
     &            NSEND, NLOSTCHAR, NCHARA, SUM(HEAPCOUNTS) 
          RETURN 
        END SUBROUTINE WIPE_HEAPED_CHAR 
 
  !--------------------------------------------------------------------- 
  ! RE-INITIALISE THE STRUCTURE AFTER COMPLETING ALL ACTIONS  
  !--------------------------------------------------------------------- 
       
        SUBROUTINE RE_INITIALISE_CHARS(NSEND,NLOSTCHAR,NLOSTAGAIN,NARRV) 
          IMPLICIT NONE 
          INTEGER, INTENT(OUT) :: NSEND,NLOSTCHAR,NLOSTAGAIN,NARRV 
          NLOSTCHAR=0 
          NLOSTAGAIN=0 
          NARRV=0 
          NSEND=0 
!         MAYBE NOT MANDATORY 
          IF (ALLOCATED(HEAPCOUNTS)) HEAPCOUNTS=0 
          IF (ALLOCATED(SENDCOUNTS)) SENDCOUNTS=0 
          IF (ALLOCATED(RECVCOUNTS)) RECVCOUNTS=0 
          IF (ALLOCATED(SDISPLS))    SDISPLS=0    ! NOT NECESSARY? 
          IF (ALLOCATED(RDISPLS))    RDISPLS=0    ! NOT NECESSARY?  
          IF (ALLOCATED(ICHA))      ICHA=0      ! NOT NECESSARY?  
        END SUBROUTINE RE_INITIALISE_CHARS  
 
  !--------------------------------------------------------------------- 
  ! PREPARE THE INITIAL SEND OF THE LOST CHARACTERISTICS 
  ! THE FIELDS ARE PREPARED ACCORDING THE MPI_ALLTOALL(V) REQUIREMENTS 
  !--------------------------------------------------------------------- 
 
        SUBROUTINE PREP_INITIAL_SEND(NSEND,NLOSTCHAR,NCHARA) 
          USE BIEF_DEF, ONLY : NCSIZE 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN)    :: NSEND 
          INTEGER, INTENT(OUT)   :: NLOSTCHAR 
          INTEGER, INTENT(INOUT) :: NCHARA 
          INTEGER I,N 
          IF (NCHARA==0) RETURN  
          SENDCOUNTS=HEAPCOUNTS 
          SDISPLS(1) = 0 ! CONTIGUOUS DATA 
          DO I=2,NCSIZE 
            SDISPLS(I) = SDISPLS(I-1)+SENDCOUNTS(I-1) 
          END DO 
          ICHA=SENDCOUNTS ! A RUNNING COUNTER PARTITION-WISE 
          DO I=1,NCHARA 
            ! HEAPCHAR(I)%NEPID+1 - THE PARTITION WE SEND TO / OR -1  
            IF(HEAPCHAR(I)%NEPID>=0) THEN              
              N=HEAPCHAR(I)%NEPID+1  
              SENDCHAR(SDISPLS(N)+ICHA(N))=HEAPCHAR(I) 
              ICHA(N)=ICHA(N)-1 
            ENDIF  
          ENDDO  
          NLOSTCHAR = NSEND 
          HEAPCOUNTS=0  
          NCHARA=0 
          RETURN  
        END SUBROUTINE PREP_INITIAL_SEND 
 
  !--------------------------------------------------------------------- 
  ! COLLECT IMPLANTED TRACEBACKS WHICH ARE COMPLETED/LOCALISED  
  ! ON A HEAP, SETTING BY THE WAY ALSO THE NUMBER OF THE LOST-AGAIN  
  ! TRACEBACKS ACCORDINGLY TO THE PARTITION THEY SHOULD BE SEND TO  
  ! THE "LOCALISED" MARK IS SET IN ADD_CHAR11/41 
  !--------------------------------------------------------------------- 
 
        SUBROUTINE HEAP_FOUND(NLOSTAGAIN,NARRV,NCHARA)  
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(OUT)    :: NLOSTAGAIN 
          INTEGER, INTENT(IN)     :: NARRV 
          INTEGER, INTENT(INOUT)  :: NCHARA 
          INTEGER I  
          SENDCOUNTS=0 
          ! DO NOT ZEROIZE NCHARA, HEAPCOUNTS / ADDING FROM GENERATIONS! 
          ! COUNTER PARTITION-WISE, ALSO MY-OWN 
          DO I=1,NARRV  
            IF(RECVCHAR(I)%NEPID==-1) THEN ! A LOCALISED TRACEBACK 
              NCHARA=NCHARA+1 
              HEAPCHAR(NCHARA) = RECVCHAR(I) ! ALREADY INTERPOLATED?  
              HEAPCOUNTS(HEAPCHAR(NCHARA)%MYPID+1) =  
     &             HEAPCOUNTS(HEAPCHAR(NCHARA)%MYPID+1)+1  
            ELSE ! A LOST-AGAIN CHARACTERISTIC / TO BE SORTED LATER  
              SENDCOUNTS(RECVCHAR(I)%NEPID+1) =  
     &           SENDCOUNTS(RECVCHAR(I)%NEPID+1)+1 
            ENDIF 
          END DO  
          NLOSTAGAIN=SUM(SENDCOUNTS) 
          IF (TRACE) WRITE(LU,'(2(A,I7))')  
     &       ' @STREAMLINE::HEAP_FOUND:: HEAPED: ',NCHARA, 
     &                 ' LOST-AGAIN: ',NLOSTAGAIN 
          RETURN  
        END SUBROUTINE HEAP_FOUND 
 
  !--------------------------------------------------------------------- 
  ! PREPARE LOST-AGAIN TRACEBACKS FOR THE NEXT COMMUNICATION 
  ! FILL IN THE STRUCTURE FOR THE ALL-TO-ALL COMMUNICATION  
  ! NOTE THAT SENDCOUNTS ARE SET IN HEAP_FOUND 
  ! (OPTIMISE(?): HEAP-FOUND AND PREP_LOST_AGAIN CAN BE JOINED) 
  !--------------------------------------------------------------------- 
 
        SUBROUTINE PREP_LOST_AGAIN(NOMB,NSEND,NLOSTAGAIN,NARRV) 
          USE BIEF_DEF, ONLY : NCSIZE 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER,INTENT(IN)   :: NOMB,NLOSTAGAIN,NARRV 
          INTEGER, INTENT(OUT) :: NSEND 
          INTEGER I,J,K,N 
          SDISPLS(1) = 0 ! CONTIGUOUS DATA MARKER 
          DO I=2,NCSIZE 
            SDISPLS(I) = SDISPLS(I-1)+SENDCOUNTS(I-1) 
          END DO 
          ICHA=0 
          NSEND=0 
          DO I=1,NARRV 
            N=RECVCHAR(I)%NEPID 
            IF (N/=-1) THEN  ! A LOST-AGAIN TRACEBACK 
              ICHA(N+1)=ICHA(N+1)+1 
              NSEND=NSEND+1 
              SENDCHAR(SDISPLS(N+1)+ICHA(N+1)) = RECVCHAR(I)  
            ENDIF  
          END DO  
          IF (TRACE.AND.NARRV>0) THEN ! DEBUGGING  
            WRITE (LU,*) ' @STREAMLINE::PREP_LOST_AGAIN:' 
            WRITE (LU,*) ' NSEND, NLOSTAGAIN, SUM(SENDCOUNTS): ', 
     &           NSEND, NLOSTAGAIN, SUM(SENDCOUNTS) 
            WRITE (LU,*) ' => SENDCHAR SORTED ACCORDING TO PROCESSORS'  
            DO N=1,NCSIZE 
              IF (SENDCOUNTS(N)>0) WRITE (LU,*)  
     &            'CHARS TO BE SENT TO PROCESSOR: ',N-1 
              DO I=1,SENDCOUNTS(N) 
                !WRITE(LU,*) ' NUMBER: ',SDISPLS(N)+I 
                !WRITE(LU,*) ' SENDCHAR(ICHA): ',SENDCHAR(SDISPLS(N)+I) 
                J=SDISPLS(N)+I 
                WRITE(LU,'(1X,I3,A,2(1X,I3),3(1X,I6),2(1X,I3), 
     &                     13(1X,1PG13.6))')  
     &   J,':', SENDCHAR(J)%MYPID, SENDCHAR(J)%NEPID, SENDCHAR(J)%INE,  
     &          SENDCHAR(J)%KNE,   SENDCHAR(J)%IOR,    
     &          SENDCHAR(J)%ISP,   SENDCHAR(J)%NSP, 
     &          SENDCHAR(J)%XP,    SENDCHAR(J)%YP,    SENDCHAR(J)%ZP, 
     &       (  SENDCHAR(J)%BASKET(K), K=1,NOMB) 
              END DO  
            END DO  
          ENDIF 
          RETURN  
        END SUBROUTINE PREP_LOST_AGAIN 
 
  !--------------------------------------------------------------------- 
  ! MOVE THE HEAP OF IMPLANTED AND COMPLETED TRACEBACKS (I.E. COMPLETED  
  ! IN MY PARTITION) TO DATA STRUCTURES FOR ALL-TO-ALL COMMUNICATION  
  ! SENDCHAR IS FILLED ACCORDING TO THE MPI_ALLTOALLV REQUIREMENTS  
  ! ALL-TO-ALL PATTERN INCLUDE MY OWN LOST TRACEBACKS THAT CAME BACK 
  !--------------------------------------------------------------------- 
 
        SUBROUTINE PREP_SENDBACK(NOMB,NCHARA) 
          USE BIEF_DEF, ONLY: IPID, NCSIZE 
          IMPLICIT NONE  
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: NOMB 
          INTEGER, INTENT(INOUT) :: NCHARA 
          INTEGER  :: I,J,K,N 
          IF (NCHARA==0) RETURN ! UHM. 
          SENDCOUNTS=HEAPCOUNTS 
          IF(TRACE) THEN 
            WRITE(LU,*) ' @STREAMLINE::PREP_SENDBACK:: NCHARA: ',NCHARA 
            WRITE(LU,*) ' SUM(HEAPCOUNTS): ',SUM(HEAPCOUNTS) 
            WRITE(LU,*) ' SENDCOUNTS: ',SENDCOUNTS 
          ENDIF 
          SDISPLS(1) = 0 ! CONTIGUOUS DATA 
          DO I=2,NCSIZE 
            SDISPLS(I) = SDISPLS(I-1)+SENDCOUNTS(I-1) 
          ENDDO 
          ICHA=0 
          DO I=1,NCHARA     
            ! MYPID+1 - IS THE -ORIGIN- PARTITION  
            N=HEAPCHAR(I)%MYPID+1  
            ICHA(N)=ICHA(N)+1 
            SENDCHAR(SDISPLS(N)+ICHA(N))=HEAPCHAR(I) 
            ! SIGN IN THE SENDBACK ORIGIN FOR DEBUGGING PURPOSES   
            SENDCHAR(SDISPLS(N)+ICHA(N))%NEPID=IPID 
          ENDDO  
          IF(TRACE.AND.NCHARA>0) THEN ! DEBUGGING PRINTOUTS  
            WRITE (LU,*) ' @STREAMLINE::PREP_SENDBACK:' 
            WRITE (LU,*) ' IPID: ',IPID 
            DO N=1,NCSIZE 
              IF (SENDCOUNTS(N)>0) WRITE (LU,*)  
     &          ' CHARS TO BE SENT BACK TO PROCESSOR: ',N-1 
              DO I=1,SENDCOUNTS(N) 
                !WRITE(LU,*) ' NUMBER: ',SDISPLS(N)+I 
                !WRITE(LU,*) ' SENDCHAR(ICHA):',SENDCHAR(SDISPLS(N)+I) 
                J=SDISPLS(N)+I 
                WRITE(LU,'(1X,I3,A,2(1X,I3),3(1X,I6),2(1X,I3), 
     &                     13(1X,1PG13.6))')  
     &  J, ':', SENDCHAR(J)%MYPID, SENDCHAR(J)%NEPID, SENDCHAR(J)%INE,  
     &          SENDCHAR(J)%KNE,   SENDCHAR(J)%IOR,    
     &          SENDCHAR(J)%ISP,   SENDCHAR(J)%NSP, 
     &          SENDCHAR(J)%XP,    SENDCHAR(J)%YP,    SENDCHAR(J)%ZP, 
     &       (  SENDCHAR(J)%BASKET(K), K=1,NOMB) 
              ENDDO  
            ENDDO  
          ENDIF 
          HEAPCOUNTS=0 
          NCHARA=0 
          RETURN  
        END SUBROUTINE PREP_SENDBACK 
 
  !--------------------------------------------------------------------- 
  ! THE GLOBAL COMMUNICATION OF LOST CHARACTERISTICS - ALL-TO-ALL 
  ! (THIS IS THE HEART OF ALL THINGS / THE GLOBAL COMMUNICATION)  
  ! THE DATA IS SENT AND (NOTE!) RECEIVED -SORTED- ACCORDING TO THE  
  ! MPI_ALLTOALL(V) SPECIFICATION IN A CONTIGUOUS FIELDS  
  ! DATA FOR A GIVEN PROCESSOR/PARTITION IN FIELD SECTIONS DESCRIBED BY  
  ! DISPLACEMENTS SDISPLS AND RDISPLS 
  !--------------------------------------------------------------------- 
 
        SUBROUTINE GLOB_CHAR_COMM () 
          USE BIEF_DEF, ONLY : NCSIZE 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER :: I,IER 
          CALL P_MPI_ALLTOALL(SENDCOUNTS,1,MPI_INTEGER,  
     &          RECVCOUNTS,1,MPI_INTEGER,  
     &          MPI_COMM_WORLD,IER) 
          IF (IER/=MPI_SUCCESS) THEN 
            WRITE(LU,*)  
     &       ' @STREAMLINE::GLOB_CHAR_COMM::MPI_ALLTOALL ERROR: ',IER 
            CALL PLANTE(1)  
          ENDIF     
          RDISPLS(1) = 0 ! SAVE THE RECEIVED DATA CONTIGUOUSLY  
          DO I=2,NCSIZE 
            RDISPLS(I) = RDISPLS(I-1)+RECVCOUNTS(I-1) 
          END DO 
          CALL P_MPI_ALLTOALLV  
     &      (SENDCHAR,SENDCOUNTS,SDISPLS,CHARACTERISTIC, 
     &       RECVCHAR,RECVCOUNTS,RDISPLS,CHARACTERISTIC, 
     &       MPI_COMM_WORLD,IER) 
          IF (IER/=MPI_SUCCESS) THEN 
            WRITE(LU,*)  
     &       ' @STREAMLINE::GLOB_CHAR_COMM::MPI_ALLTOALLV ERROR: ',IER 
            CALL PLANTE(1) 
            STOP  
          ENDIF     
          RETURN 
        END SUBROUTINE GLOB_CHAR_COMM 
 
  !--------------------------------------------------------------------- 
  ! TELEMAC3D PRISMS, INTERPOLATION OF RECVCHAR  
  ! ELT,ETA AND SHP,SHZ MUST BE CORRECTLY PROVIDED VIA ADD_CHAR11   
  !   -> MATCHED TO THE RANGE 1:NRANGE (NO CHECKING - FOR SPEED!!!)  
  !   N IS THE POSITION FORESEEN FOR A GIVEN VARIABLE VAL IN THE BASKET 
  !--------------------------------------------------------------------- 
 
        SUBROUTINE INTERP_RECVCHAR_41 
     &      (VAL,N,IKLE,ELT,ETA,SHP,SHZ,NELEM,NPOIN2,NPLAN,NRANGE)  
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: N,NELEM,NPOIN2,NPLAN,NRANGE 
          INTEGER, INTENT(IN) :: IKLE(NELEM,3)  
          INTEGER, INTENT(IN) :: ELT(NRANGE), ETA(NRANGE)  
          DOUBLE PRECISION, INTENT(IN) :: SHP(3,NRANGE), SHZ(NRANGE)  
          DOUBLE PRECISION, INTENT(IN) :: VAL(NPOIN2,NPLAN) 
          INTEGER I 
          ! 
          DO I=1,NRANGE 
            IF (RECVCHAR(I)%NEPID==-1) THEN ! LOCALISED 
              RECVCHAR(I)%BASKET(N) = 
     &          VAL(IKLE(ELT(I),1),ETA(I))   * SHP(1,I) * (1.D0-SHZ(I)) 
     &        + VAL(IKLE(ELT(I),2),ETA(I))   * SHP(2,I) * (1.D0-SHZ(I)) 
     &        + VAL(IKLE(ELT(I),3),ETA(I))   * SHP(3,I) * (1.D0-SHZ(I)) 
     &        + VAL(IKLE(ELT(I),1),ETA(I)+1) * SHP(1,I) * SHZ(I) 
     &        + VAL(IKLE(ELT(I),2),ETA(I)+1) * SHP(2,I) * SHZ(I) 
     &        + VAL(IKLE(ELT(I),3),ETA(I)+1) * SHP(3,I) * SHZ(I) 
!           THIS IS JUST TO AVOID A BUG ON HP COMPILER 
            ELSEIF(RECVCHAR(I)%NEPID.LT.-1) THEN 
              WRITE(LU,*) 'STREAMLINE  INTERP_RECVCHAR_11' 
              WRITE(LU,*) 'NEPID OUT OF RANGE:',RECVCHAR(I)%NEPID 
              WRITE(LU,*) 'FOR I=',I 
              CALL PLANTE(1) 
              STOP 
!           END OF THIS IS JUST TO AVOID A BUG ON HP COMPILER 
            ENDIF 
          END DO  
          ! 
          RETURN  
        END SUBROUTINE INTERP_RECVCHAR_41 
         
  !--------------------------------------------------------------------- 
  ! TELEMAC2D TRIANGLES, INTERPOLATION OF RECVCHAR 
  ! ELT AND SHP MUST BE CORRECTLY PROVIDED VIA ADD_CHAR11   
  !   -> MATCHED TO THE RANGE 1:NRANGE (NO CHECKING - FOR SPEED!!!)  
  !   N IS THE POSITION FORESEEN FOR A GIVEN VARIABLE VAL IN THE BASKET 
  !--------------------------------------------------------------------- 
 
        SUBROUTINE INTERP_RECVCHAR_11  
     &      (VAL,N,IKLE,ELT,SHP,NELEM,NPOIN,NRANGE,IELM) 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: N,NELEM,NPOIN,NRANGE,IELM 
          INTEGER, INTENT(IN) :: IKLE(NELEM,*)  
          INTEGER, INTENT(IN) :: ELT(NRANGE) 
          DOUBLE PRECISION, INTENT(IN) :: SHP(3,NRANGE) 
          DOUBLE PRECISION, INTENT(IN) :: VAL(NPOIN) 
          INTEGER I 
          ! 
          IF(IELM.EQ.11.OR.IELM.EQ.12) THEN 
          DO I=1,NRANGE 
            IF(RECVCHAR(I)%NEPID==-1) THEN ! LOCALISED 
              RECVCHAR(I)%BASKET(N) =  
     &                       VAL(IKLE(ELT(I),1)) * SHP(1,I) 
     &                     + VAL(IKLE(ELT(I),2)) * SHP(2,I) 
     &                     + VAL(IKLE(ELT(I),3)) * SHP(3,I) 
!           THIS IS JUST TO AVOID A BUG ON HP COMPILER 
            ELSEIF(RECVCHAR(I)%NEPID.LT.-1) THEN 
              WRITE(LU,*) 'STREAMLINE  INTERP_RECVCHAR_11' 
              WRITE(LU,*) 'NEPID OUT OF RANGE:',RECVCHAR(I)%NEPID 
              WRITE(LU,*) 'FOR I=',I 
              CALL PLANTE(1) 
              STOP 
!           END OF THIS IS JUST TO AVOID A BUG ON HP COMPILER 
            ENDIF 
          ENDDO 
          ELSEIF(IELM.EQ.13) THEN 
          DO I=1,NRANGE 
            IF(RECVCHAR(I)%NEPID==-1) THEN ! LOCALISED      
              RECVCHAR(I)%BASKET(N) =  
     *      VAL(IKLE(ELT(I),1)) * (2.D0*SHP(1,I)-1.D0)* SHP(1,I) 
     *     +VAL(IKLE(ELT(I),2)) * (2.D0*SHP(2,I)-1.D0)* SHP(2,I) 
     *     +VAL(IKLE(ELT(I),3)) * (2.D0*SHP(3,I)-1.D0)* SHP(3,I) 
     *     +VAL(IKLE(ELT(I),4)) * 4.D0 * SHP(1,I)*SHP(2,I) 
     *     +VAL(IKLE(ELT(I),5)) * 4.D0 * SHP(2,I)*SHP(3,I) 
     *     +VAL(IKLE(ELT(I),6)) * 4.D0 * SHP(3,I)*SHP(1,I) 
!           THIS IS JUST TO AVOID A BUG ON HP COMPILER 
            ELSEIF(RECVCHAR(I)%NEPID.LT.-1) THEN 
              WRITE(LU,*) 'STREAMLINE  INTERP_RECVCHAR_11' 
              WRITE(LU,*) 'NEPID OUT OF RANGE:',RECVCHAR(I)%NEPID 
              WRITE(LU,*) 'FOR I=',I 
              CALL PLANTE(1) 
              STOP 
!           END OF THIS IS JUST TO AVOID A BUG ON HP COMPILER 
            ENDIF 
          ENDDO 
          ELSE 
            WRITE(LU,*) 'WRONG IELM IN INTERP_RECVCHAR_11:',IELM 
            CALL PLANTE(1) 
            STOP 
          ENDIF     
          ! 
          RETURN  
        END SUBROUTINE INTERP_RECVCHAR_11 
 
  !--------------------------------------------------------------------- 
  ! INTRODUCE RECEIVED VALUES IN THE BASKET BACK IN THE TELEMAC  
  ! STRUCTURES, N BEING THE POSITION IN THE BASKET FOR A GIVEN VARIABLE  
  !--------------------------------------------------------------------- 
 
        SUBROUTINE INTRODUCE_RECVCHAR(VAL,N,NPOIN,NOMB,NARRV)  
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: N,NPOIN,NOMB,NARRV 
          DOUBLE PRECISION, INTENT(INOUT) :: VAL(NPOIN) 
          INTEGER I  
          IF (N>NOMB.OR.N>MAX_BASKET_SIZE) THEN  
            WRITE(LU,*)  
     &       ' @STREAMLINE::INTRODUCE_RECVCHAR ::', 
     &       ' N>NOMB.OR.N>MAX_BASKET_SIZE,',  
     &       ' N,NOMB,MAX_BASKET_SIZE : ',N,NOMB,MAX_BASKET_SIZE 
            CALL PLANTE(1) 
            STOP  
          ENDIF  
          DO I=1,NARRV 
            VAL(RECVCHAR(I)%IOR)=RECVCHAR(I)%BASKET(N) 
          ENDDO  
          RETURN  
        END SUBROUTINE INTRODUCE_RECVCHAR 
 
  !--------------------------------------------------------------------- 
  ! PRINTOUTS FOR DEBUGGING / PRINTING CHARS / NOTICE COUNTERS! 
  !--------------------------------------------------------------------- 
 
        ! PRINTS SENDCHAR STRUCTURE TO LU  
 
        SUBROUTINE PRINT_HEAPCHAR(MESSAGE,NOMB,NCHARA)  
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: NOMB,NCHARA 
          CHARACTER(LEN=*), INTENT(IN) :: MESSAGE 
          INTEGER I,J 
          WRITE(LU,*) TRIM(MESSAGE)  
          IF (.NOT.ALLOCATED(SENDCHAR)) RETURN 
          IF (SIZE(HEAPCHAR)==0) RETURN 
          WRITE (LU,*) 'NCHARA: ',NCHARA 
!          WRITE (LU,*) 'SIZE(HEAPCHAR(1)%BASKET): ', 
!     &                  SIZE(HEAPCHAR(1)%BASKET) 
          WRITE (LU,*)  
     &       'NR: MYPID, NEPID, INE, KNE, ', 
     &       'IOR, ISP, NSP, XP, YP, ZP, BASKET(:)' 
          !DO I=1,SIZE(HEAPCHAR) 
          DO I=1,NCHARA 
            WRITE(LU,'(1X,I3,A,2(1X,I3),3(1X,I6),2(1X,I3), 
     &                 13(1X,1PG13.6))')  
     &      I, ':', HEAPCHAR(I)%MYPID, HEAPCHAR(I)%NEPID,  
     &         HEAPCHAR(I)%INE,   HEAPCHAR(I)%KNE,  
     &         HEAPCHAR(I)%IOR,    
     &         HEAPCHAR(I)%ISP,   HEAPCHAR(I)%NSP, 
     &         HEAPCHAR(I)%XP,    HEAPCHAR(I)%YP,    HEAPCHAR(I)%ZP, 
     &       ( HEAPCHAR(I)%BASKET(J), J=1,NOMB) 
!     &       ( HEAPCHAR(I)%BASKET(J), J=1,SIZE(HEAPCHAR(I)%BASKET)) 
          END DO  
        END SUBROUTINE PRINT_HEAPCHAR 
 
        ! PRINTS SENDCHAR STRUCTURE TO LU 
 
        SUBROUTINE PRINT_SENDCHAR(MESSAGE,NOMB,NSEND) 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: NOMB,NSEND 
          CHARACTER(LEN=*), INTENT(IN) :: MESSAGE 
          INTEGER I,J 
          WRITE(LU,*) TRIM(MESSAGE)  
          !IF (.NOT.ASSOCIATED(SENDCHAR)) RETURN 
          IF (.NOT.ALLOCATED(SENDCHAR)) RETURN 
          IF (SIZE(SENDCHAR)==0) RETURN 
          WRITE (LU,*) 'NSEND: ',NSEND  
!          WRITE (LU,*) 'SIZE(SENDCHAR(1)%BASKET): ', 
!     &                  SIZE(SENDCHAR(1)%BASKET) 
          WRITE (LU,*)  
     &       'NR: MYPID, NEPID, INE, KNE, ', 
     &       'IOR, ISP, NSP, XP, YP, ZP, BASKET(:)' 
          !DO I=1,SIZE(SENDCHAR) 
          DO I=1,NSEND 
            WRITE(LU,'(1X,I3,A,2(1X,I3),3(1X,I6),2(1X,I3), 
     &                 13(1X,1PG13.6))')  
     &      I, ':', SENDCHAR(I)%MYPID, SENDCHAR(I)%NEPID,  
     &         SENDCHAR(I)%INE,   SENDCHAR(I)%KNE,  
     &         SENDCHAR(I)%IOR,    
     &         SENDCHAR(I)%ISP,   SENDCHAR(I)%NSP, 
     &         SENDCHAR(I)%XP,    SENDCHAR(I)%YP,    SENDCHAR(I)%ZP, 
     &       ( SENDCHAR(I)%BASKET(J), J=1,NOMB) 
!     &       ( SENDCHAR(I)%BASKET(J), J=1,SIZE(SENDCHAR(I)%BASKET)) 
          END DO  
        END SUBROUTINE PRINT_SENDCHAR 
         
        ! PRINTS RECVCHAR STRUCTURE TO LU  
 
        SUBROUTINE PRINT_RECVCHAR(MESSAGE,NOMB,NARRV)  
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: NOMB,NARRV 
          CHARACTER(LEN=*), INTENT(IN) :: MESSAGE 
          INTEGER I,J 
          WRITE(LU,*) TRIM(MESSAGE)  
          !WRITE(LU,*) ' ALLOCATED(RECVCHAR): ', ALLOCATED(RECVCHAR)  
          !WRITE(LU,*) ' SIZE(RECVCHAR): ', SIZE(RECVCHAR) 
          !IF (.NOT.ASSOCIATED(RECVCHAR)) RETURN 
          IF (.NOT.ALLOCATED(RECVCHAR)) RETURN 
          IF (SIZE(RECVCHAR)==0) RETURN  
          WRITE (LU,*) 'NARRV: ',NARRV  
          WRITE (LU,*)  
     &       'NR: MYPID, NEPID, INE, KNE, ', 
     &       'IOR, ISP, NSP, XP, YP, ZP, BASKET(:)' 
          !DO I=1,SIZE(RECVCHAR) 
          DO I=1,NARRV 
            WRITE(LU,'(1X,I3,A,2(1X,I3),3(1X,I6),2(1X,I3), 
     &                 13(1X,1PG13.6))')  
     &      I, ':', RECVCHAR(I)%MYPID, RECVCHAR(I)%NEPID,  
     &         RECVCHAR(I)%INE,   RECVCHAR(I)%KNE,  
     &         RECVCHAR(I)%IOR,    
     &         RECVCHAR(I)%ISP,   RECVCHAR(I)%NSP, 
     &         RECVCHAR(I)%XP,    RECVCHAR(I)%YP,    RECVCHAR(I)%ZP, 
     &       ( RECVCHAR(I)%BASKET(J), J=1,NOMB) 
          END DO  
        END SUBROUTINE PRINT_RECVCHAR 
 
  !///////////////////////////////////////////////////////////////////// 
  ! 
  !   TELEMAC ROUTINES MODIFIED FOR THE PURPOSE OF  
  !           PARALLEL STREAMLINE TRACKING - MOSTLY FROM BIEF  
  ! 
  !///////////////////////////////////////////////////////////////////// 
 
  !--------------------------------------------------------------------- 
  ! CHAR41 MODIFIED FOR INITIAL COLLECTING OF THE LOST CHARACTERISTICS  
  ! I.E. THE ONES CROSSING INTERFACE PARTITIONS IN THE PARALLEL CASE 3D 
  ! IFAPAR  :: DELIVERS LOCAL ELEMENT NUMBER AND THE PARTITION NR THERE   
  !            WHEN CROSSING THE INTERFACE VIA A HALO ELEMENT FACE   
  !--------------------------------------------------------------------- 
  ! JAJ MODIFIED FRI JUL 18 10:11:11 CEST 2008 
 
!                       ****************** 
                        SUBROUTINE SCHAR41 
!                       ****************** 
! 
     &( U , V , W , DT , NRK , X , Y , ZSTAR , Z , IKLE2 , IBOR , 
     &  XPLOT , YPLOT , ZPLOT , DX , DY , DZ , SHP , SHZ , ELT , ETA , 
     &  NSP   , NPLOT , NPOIN2 , NELEM2 , NPLAN , SURDET , 
     &  SENS  , TEST  , IFAPAR, NCHDIM,NCHARA) 
! 
!*********************************************************************** 
! BIEF VERSION 5.9           28/04/93     J-M JANIN (LNH) 30 87 72 84 
!                        12/10/05     J-M HERVOUET (LNHE) 01 30 87 80 18 
! 
! 08/11/04 : ADAPTATION A LA TRANSFORMEE SIGMA GENERALISEE 
! 12/10/05 : BUG CORRIGE, VOIR VARIABLE IELE QUI ETAIT AVANT IEL 
!            ET EFFACAIT UN AUTRE IEL. 
!             
! 
!*********************************************************************** 
! 
!  FONCTION : 
! 
!     REMONTEE OU DESCENTE 
!     DES COURBES CARACTERISTIQUES 
!     SUR DES PRISMES DE TELEMAC-3D 
!     DANS L'INTERVALLE DE TEMPS DT 
!     AVEC UNE DISCRETISATION ELEMENTS FINIS 
! 
! 
!  DISCRETISATION : 
! 
!     LE DOMAINE EST APPROCHE PAR UNE DISCRETISATION ELEMENTS FINIS 
!     UNE APPROXIMATION LOCALE EST DEFINIE POUR LE VECTEUR VITESSE : 
!     LA VALEUR EN UN POINT D'UN ELEMENT NE DEPEND QUE DES VALEURS 
!     AUX NOEUDS DE CET ELEMENT 
! 
! 
!  RESTRICTIONS ET HYPOTHESES : 
! 
!     LE CHAMP CONVECTEUR U EST SUPPOSE INDEPENDANT DU TEMPS 
! 
!----------------------------------------------------------------------- 
!                             ARGUMENTS 
! .________________.____.______________________________________________. 
! |      NOM       |MODE|                   ROLE                       | 
! |________________|____|______________________________________________| 
! |    U,V,W       | -->| COMPONENTS OF ADVECTION VELOCITY 
! |                |    | BUT W IS W* x DELTAZ (STEMS FROM TRIDW2) 
! |    DT          | -->| PAS DE TEMPS.                                | 
! |    NRK         | -->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.           | 
! |    X,Y,ZSTAR   | -->| COORDONNEES DES POINTS DU MAILLAGE.          | 
! |    Z           | -->| COTE DANS LE MAILLAGE REEL                   | 
! |    IKLE2       | -->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE    | 
! |                |    | ET GLOBALE DU MAILLAGE 2D.                   | 
! |    IBOR        | -->| NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE 
! |                |    | AVEC L'ELEMENT .  SI IFABOR<0 OU NUL         | 
! |                |    | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE   | 
! |  X..,Y..,ZPLOT |<-->| POSITIONS SUCCESSIVES DES DERIVANTS.         | 
! |    DX,DY,DZ    | -- | STOCKAGE DES SOUS-PAS .                      | 
! |    SHP         |<-->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES    | 
! |                |    | COURBES CARACTERISTIQUES.                    | 
! |    SHZ         |<-->| COORDONNEES BARYCENTRIQUES SUIVANT Z DES     | 
! |                |    | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.     | 
! |    ELT         |<-->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE  | 
! |                |    | NOEUD.                                       | 
! |    ETA         |<-->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.| 
! |    NSP         | -- | NOMBRE DE SOUS-PAS DE RUNGE KUTTA.           | 
! |    NPLOT       | -->| NOMBRE DE DERIVANTS.                         | 
! |    NPOIN2      | -->| NOMBRE DE POINTS DU MAILLAGE 2D.             | 
! |    NELEM2      | -->| NOMBRE D'ELEMENTS DU MAILLAGE 2D.            | 
! |    NPLAN       | -->| NOMBRE DE PLANS.                             | 
! |    SURDET      | -->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM. 
! |    SENS        | -->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.   | 
! |    ISO         | -- | STOCKAGE BINAIRE DE LA FACE DE SORTIE.       | 
! |________________|____|______________________________________________| 
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE) 
!----------------------------------------------------------------------- 
!     - APPELE PAR : CARACT 
!     - PROGRAMMES APPELES : NEANT 
! 
!*********************************************************************** 
! 
      USE BIEF 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER         , INTENT(IN)    :: SENS,NPLAN,NCHDIM 
      INTEGER         , INTENT(IN)    :: NPOIN2,NELEM2,NPLOT,NRK 
      INTEGER         , INTENT(IN)    :: IKLE2(NELEM2,3) 
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NSP(NPLOT),NCHARA 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN) 
      DOUBLE PRECISION, INTENT(IN)    :: W(NPOIN2,NPLAN),SURDET(NELEM2) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: ZPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT),SHZ(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2),DT 
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT),TEST(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: DZ(NPLOT) 
      INTEGER         , INTENT(IN)    :: IBOR(NELEM2,5,NPLAN-1) 
      INTEGER         , INTENT(INOUT) :: ETA(NPLOT) 
      INTEGER         , INTENT(IN)    :: IFAPAR(6,*) 
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER IELE,ISO 
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,IET2,ISOH,ISOV,IFA,ISUI(3) 
! 
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,ZP,DENOM 
      DOUBLE PRECISION DELTAZ,EPSDZ,PAS2 
! 
      INTRINSIC ABS , INT , MAX , SQRT 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA EPSILO / -1.D-6 / 
      DATA EPSDZ /1.D-4/ 
! 
!----------------------------------------------------------------------- 
!     NUMBER OF RUNGE-KUTTA STEPS FOR EVERY POINT 
!----------------------------------------------------------------------- 
! 
      DO IPLOT = 1 , NPLOT 
! 
         IEL = ELT(IPLOT) 
         IET = ETA(IPLOT) 
! 
         I1 = IKLE2(IEL,1) 
         I2 = IKLE2(IEL,2) 
         I3 = IKLE2(IEL,3) 
! 
         DXP = U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + U(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &       + U(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &       + U(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) 
! 
         DYP = V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + V(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &       + V(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &       + V(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) 
! 
         NSP(IPLOT)=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL)))) 
! 
      ENDDO 
! 
!----------------------------------------------------------------------- 
!  POUR TOUT PAS DE R-K REPETER 
!----------------------------------------------------------------------- 
! 
      DO IPLOT=1,NPLOT 
! 
      PAS = SENS * DT / NSP(IPLOT) 
! 
      DO ISP = 1 , NSP(IPLOT) 
! 
!----------------------------------------------------------------------- 
!  LOCALISATION DU POINT D'ARRIVEE DE TOUTES LES CARACTERISTIQUES 
!----------------------------------------------------------------------- 
! 
        ISO = 0 
        PAS2 = PAS 
        IEL = ELT(IPLOT) 
        IET = ETA(IPLOT) 
! 
        I1 = IKLE2(IEL,1) 
        I2 = IKLE2(IEL,2) 
        I3 = IKLE2(IEL,3) 
! 
        DX(IPLOT) = ( U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &              + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &              + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &              + U(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &              + U(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &              + U(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) * PAS 
! 
        DY(IPLOT) = ( V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &              + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &              + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &              + V(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &              + V(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &              + V(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) * PAS 
! 
        DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT) 
     &          + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT) 
     &          + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT) 
! 
        IF(DELTAZ.GT.EPSDZ) THEN 
!         DIVISION BY DELTAZ IS DUE TO THE FACT THAT W IS  
!         W* MULTIPLIED BY DELTAZ (IT STEMS FROM TRIDW2 IN TELEMAC3D) 
          DZ(IPLOT) = ( W(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                + W(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                + W(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                + W(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &                + W(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &                + W(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) )   
     &                * PAS * (ZSTAR(IET+1)-ZSTAR(IET)) / DELTAZ 
        ELSE 
          DZ(IPLOT) = 0.D0 
        ENDIF 
! 
        XP = XPLOT(IPLOT) + DX(IPLOT) 
        YP = YPLOT(IPLOT) + DY(IPLOT) 
        ZP = ZPLOT(IPLOT) + DZ(IPLOT) 
! 
        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                 -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                 -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                 -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
        SHZ(IPLOT) = (ZP-ZSTAR(IET)) / (ZSTAR(IET+1)-ZSTAR(IET)) 
! 
        XPLOT(IPLOT) = XP 
        YPLOT(IPLOT) = YP 
        ZPLOT(IPLOT) = ZP 
! 
        IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
        IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
        IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
        IF(SHZ(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,0) 
        IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
! 
!----------------------------------------------------------------------- 
!  TRAITEMENT PARTICULIER POUR LES CARACTERISTIQUES SORTIES 
!  DE L'ELEMENT DE DEPART 
!----------------------------------------------------------------------- 
! 
50      CONTINUE 
! 
        IF(ISO.NE.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QU'ON EST SORTI DE L'ELEMENT 
!----------------------------------------------------------------------- 
! 
          ISOH = IAND(ISO,28) 
          ISOV = IAND(ISO, 3) 
          IEL = ELT(IPLOT)           
          IET = ETA(IPLOT) 
          XP = XPLOT(IPLOT) 
          YP = YPLOT(IPLOT) 
          ZP = ZPLOT(IPLOT) 
! 
          IF(ISOH.NE.0) THEN 
! 
            IF(ISOH.EQ.4) THEN 
              IFA = 2 
            ELSEIF(ISOH.EQ.8) THEN 
              IFA = 3 
            ELSEIF(ISOH.EQ.16) THEN 
              IFA = 1 
            ELSEIF(ISOH.EQ.12) THEN 
              IFA = 2 
              IF(DX(IPLOT)*(Y(IKLE2(IEL,3))-YP).LT. 
     &           DY(IPLOT)*(X(IKLE2(IEL,3))-XP)) IFA = 3 
            ELSEIF(ISOH.EQ.24) THEN 
              IFA = 3 
              IF(DX(IPLOT)*(Y(IKLE2(IEL,1))-YP).LT. 
     &           DY(IPLOT)*(X(IKLE2(IEL,1))-XP)) IFA = 1 
            ELSE 
              IFA = 1 
              IF(DX(IPLOT)*(Y(IKLE2(IEL,2))-YP).LT. 
     &           DY(IPLOT)*(X(IKLE2(IEL,2))-XP)) IFA = 2 
            ENDIF 
! 
            IF(ISOV.GT.0) THEN 
              IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN 
                A1 = (ZP-ZSTAR(IET+ISOV-1)) / DZ(IPLOT) 
              ELSE 
                A1 = 0.D0 
              ENDIF 
              I1 = IKLE2(IEL,IFA) 
              I2 = IKLE2(IEL,ISUI(IFA)) 
              IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT. 
     &            (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOV+3 
            ENDIF 
! 
          ELSE 
! 
            IFA = ISOV + 3 
! 
          ENDIF 
! 
          IEL = IBOR(IEL,IFA,IET) 
! 
          IF(IFA.LE.3) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE DU PRISME EST UNE FACE QUADRAN. 
!  ================================================================= 
!----------------------------------------------------------------------- 
! 
            IF(IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST INTERNE AU DOMAINE 
!  ON SE RELOCALISE DANS L'ELEMENT ADJACENT 
!----------------------------------------------------------------------- 
! 
              I1 = IKLE2(IEL,1) 
              I2 = IKLE2(IEL,2) 
              I3 = IKLE2(IEL,3) 
              ELT(IPLOT) = IEL 
              SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                       -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
              SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                       -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
              SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                       -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
              ISO = ISOV 
! 
              IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
              IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
              IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
              GOTO 50 
! 
            ENDIF 
! 
!----------------------------------------------------------------------- 
! HERE WE TEST PASSING TO THE NEIGHBOUR SUBDOMAIN AND COLLECT DATA 
!----------------------------------------------------------------------- 
! 
!           THIS CAN ONLY HAPPEN IN PARALLELISM 
            IF(IEL==-2) THEN ! INTERFACE CROSSING 
              CALL COLLECT_CHAR  
     &            (IPID, IPLOT, ELT(IPLOT), IFA, ETA(IPLOT), ISP,  
     &             NSP(IPLOT), XPLOT(IPLOT),YPLOT(IPLOT),ZPLOT(IPLOT),  
     &             IFAPAR,NCHDIM,NCHARA)  
!             CAN ONLY HAPPEN IN PARALLEL 
              TEST(IPLOT) = 0.D0        
! 
! ALTHOUGH A LOST TRACEBACK DETECTED AND SAVED HERE, ALLOW THE  
! FURTHER TREATMENT AS IF NOTHING HAPPENED IN ORDER TO APPLY  
! THE JMH ALGORITHM WITH "TEST" FIELD OF MARKERS  
! 
            ENDIF  
! 
!----------------------------------------------------------------------- 
! FURTHER ON, THE SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
!----------------------------------------------------------------------- 
! 
            DXP = DX(IPLOT) 
            DYP = DY(IPLOT) 
            I1  = IKLE2(ELT(IPLOT),IFA) 
            I2  = IKLE2(ELT(IPLOT),ISUI(IFA)) 
            DX1 = X(I2) - X(I1) 
            DY1 = Y(I2) - Y(I1) 
! 
            IF(IEL.EQ.-1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE SOLIDE 
!  ON PROJETTE LE RELICAT SUR LA FRONTIERE PUIS SE RELOCALISE 
!----------------------------------------------------------------------- 
! 
              A1 = (DXP*DX1+DYP*DY1) / (DX1**2+DY1**2) 
              DX(IPLOT) = A1 * DX1 
              DY(IPLOT) = A1 * DY1 
! 
              A1=((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2) 
              SHP(          IFA  ,IPLOT) = 1.D0 - A1 
              SHP(     ISUI(IFA) ,IPLOT) = A1 
              SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0 
              XPLOT(IPLOT) = X(I1) + A1 * DX1 
              YPLOT(IPLOT) = Y(I1) + A1 * DY1 
! 
              ISO = ISOV 
! 
              IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
              IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
              IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
              GOTO 50 
! 
            ENDIF 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE LIQUIDE 
!  ON ARRETE LA REMONTEE DES CARACTERISTIQUE (SIGNE DE ELT) 
! 
!     OU QUE 
! 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE INTERFACE DE SOUS-DOMAINES 
!  POINT D'INTERFACE QUI SERA TRAITE PAR LE SOUS-DOMAINE VOISIN 
!  ON SE CONTENTE DE METTRE ICI TEST A ZERO 
!----------------------------------------------------------------------- 
! 
            DENOM = DXP*DY1-DYP*DX1 
            IF(ABS(DENOM).GT.1.D-8) THEN 
              A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM 
            ELSE 
              A1 = 0.D0 
            ENDIF 
            IF(A1.GT.1.D0) A1 = 1.D0 
            IF(A1.LT.0.D0) A1 = 0.D0 
            SHP(          IFA  ,IPLOT) = 1.D0 - A1 
            SHP(     ISUI(IFA) ,IPLOT) = A1 
            SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0 
            XPLOT(IPLOT) = X(I1) + A1 * DX1 
            YPLOT(IPLOT) = Y(I1) + A1 * DY1 
            IF(ABS(DXP).GT.ABS(DYP)) THEN 
              A1 = (XP-XPLOT(IPLOT))/DXP 
            ELSE 
              A1 = (YP-YPLOT(IPLOT))/DYP 
            ENDIF 
            ZPLOT(IPLOT) = ZP - A1*DZ(IPLOT) 
            SHZ(IPLOT) = (ZPLOT(IPLOT)-ZSTAR(IET)) 
     &                 / (ZSTAR(IET+1)-ZSTAR(IET)) 
            ELT(IPLOT) = - SENS * ELT(IPLOT) 
!           NSP(IPLOT) = ISP  (REPLACED BY EXIT) 
            EXIT 
! 
          ELSE 
! 
!----------------------------------------------------------------------- 
!  CAS OU IFA = 4 OU 5  
!  LA, ON SAIT QUE LA FACE DE SORTIE DU PRISME EST UNE FACE TRIANGULAIRE 
!  ===================================================================== 
!----------------------------------------------------------------------- 
! 
            IFA = IFA - 4 
!           HENCE IFA NOW EQUALS 0 OR 1 
! 
            IF(IEL.EQ.1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST INTERNE AU DOMAINE 
!  ET ON N'A PAS BESOIN DE RECALCULER LES VITESSES 
!  ON SE RELOCALISE DANS L'ELEMENT ADJACENT 
!----------------------------------------------------------------------- 
! 
              ETA(IPLOT) = IET + IFA + IFA - 1 
              SHZ(IPLOT) = (ZP-ZSTAR(ETA(IPLOT))) 
     &                   / (ZSTAR(ETA(IPLOT)+1)-ZSTAR(ETA(IPLOT))) 
! 
              ISO = ISOH 
! 
              IF(SHZ(IPLOT).LT.     EPSILO) ISO=IBSET(ISO,0) 
              IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
! 
              GOTO 50 
! 
            ENDIF 
! 
            IF(IEL.EQ.-1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE SOLIDE 
!  ON PROJETTE LE RELIQUAT SUR LA FRONTIERE PUIS ON SE RELOCALISE 
!----------------------------------------------------------------------- 
! 
              ZPLOT(IPLOT) = ZSTAR(IET+IFA) 
              DZ   (IPLOT) = 0.D0 
              SHZ  (IPLOT) = IFA 
! 
              ISO = ISOH 
              IF(ISOH.NE.0) GOTO 50 
! 
            ELSE 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE LIQUIDE (CAS 0) 
!      ON ARRETE ALORS LA REMONTEE DES CARACTERISTIQUES (SIGNE DE ELT) 
!  OU, QUE L'ON VIENT DE TRAVERSER UN PLAN AVEC RECALCUL DES VITESSES 
!  DEMANDE (CAS 2) 
!----------------------------------------------------------------------- 
! 
              IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN 
                A1 = (ZP-ZSTAR(IET+IFA)) / DZ(IPLOT) 
              ELSE 
                A1 = 0.D0 
              ENDIF 
              XP = XP - A1*DX(IPLOT) 
              YP = YP - A1*DY(IPLOT) 
              ZP = ZSTAR(IET+IFA) 
              IELE = ELT(IPLOT) 
              I1 = IKLE2(IELE,1) 
              I2 = IKLE2(IELE,2) 
              I3 = IKLE2(IELE,3) 
! 
              SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                       -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IELE) 
              SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                       -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IELE) 
              SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                       -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IELE) 
! 
              IF(IEL.EQ.2) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE SE SITUE SUR LE PLAN OU ON DEMANDE 
!  UN RECALCUL DES VITESSES 
!----------------------------------------------------------------------- 
! 
!               IF IFA = 1 EXIT THROUGH THE TOP 
!               IF IFA = 0 EXIT THROUGH THE BOTTOM 
!               THEN NEW IET IS  IET+1 IF IFA = 1 
!                            AND IET-1 IF IFA = 0 
!               THIS IS SUMMARISED BY IET=IET+2*IFA-1 
! 
!               RECOMPUTED VELOCITIES MUST BE TAKEN AT IET2=IET+IFA 
!               I.E. BOTTOM IF EXIT THROUGH THE BOTTOM 
!               AND TOP IF EXIT THROUGH THE TOP 
! 
                IET2 = IET + IFA 
                IET  = IET + IFA + IFA - 1 
                PAS2 = PAS2 * A1 
! 
                DX(IPLOT) = ( U(I1,IET2)*SHP(1,IPLOT) 
     &                      + U(I2,IET2)*SHP(2,IPLOT) 
     &                      + U(I3,IET2)*SHP(3,IPLOT) ) * PAS2 
! 
                DY(IPLOT) = ( V(I1,IET2)*SHP(1,IPLOT) 
     &                      + V(I2,IET2)*SHP(2,IPLOT) 
     &                      + V(I3,IET2)*SHP(3,IPLOT) ) * PAS2 
! 
                DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT) 
     &                  + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT) 
     &                  + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT) 
! 
                IF(DELTAZ.GT.EPSDZ) THEN 
                  DZ(IPLOT) = ( W(I1,IET2)*SHP(1,IPLOT) 
     &                        + W(I2,IET2)*SHP(2,IPLOT) 
     &                        + W(I3,IET2)*SHP(3,IPLOT) ) * PAS2 
     &                        * (ZSTAR(IET+1)-ZSTAR(IET)) / DELTAZ 
                ELSE 
                  DZ(IPLOT) = 0.D0 
                ENDIF 
! 
                XP = XP + DX(IPLOT) 
                YP = YP + DY(IPLOT) 
                ZP = ZP + DZ(IPLOT) 
! 
                SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                       -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IELE) 
                SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                       -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IELE) 
                SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                       -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IELE) 
                SHZ(IPLOT)=(ZP-ZSTAR(IET))/(ZSTAR(IET+1)-ZSTAR(IET)) 
! 
                XPLOT(IPLOT) = XP 
                YPLOT(IPLOT) = YP 
                ZPLOT(IPLOT) = ZP 
                ETA(IPLOT) = IET 
! 
                ISO = 0 
! 
                IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
                IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
                IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
                IF(SHZ(IPLOT).LT.     EPSILO) ISO=IBSET(ISO,0) 
                IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
! 
                GOTO 50 
! 
              ENDIF 
! 
              XPLOT(IPLOT) = XP 
              YPLOT(IPLOT) = YP 
              ZPLOT(IPLOT) = ZP 
              SHZ  (IPLOT) = IFA 
              ELT  (IPLOT) = - SENS * ELT(IPLOT) 
              NSP  (IPLOT) = ISP 
! 
            ENDIF 
! 
          ENDIF 
! 
!       IF(ISO.NE.0) 
        ENDIF 
! 
        ENDDO 
      ENDDO 
! 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END SUBROUTINE SCHAR41 
 
! 4141414141414141414141414141414141414141414141414141414141414141414141 
 
!----------------------------------------------------------------------- 
! 3D STREAMLINE TRACKING FOR ADDITIONAL CHARACTERISTICS ARRIVED FROM  
! NEIGHBOUR PARTITIONS - THERE'S NPLOT=NARRV OF THEM  
! NOTE CHANGES IN THE INTERFACE COMPARED TO SCHAR11  
! ISPDONE :: NUMBER OF ALREADY DONE R-K STEPS BY A TRACEBACK 
! IFAPAR  :: DELIVERS LOCAL ELEMENT NUMBER AND THE PARTITION NR THERE   
!            WHEN CROSSING THE INTERFACE VIA A HALO ELEMENT FACE    
!----------------------------------------------------------------------- 
! JAJ PINXIT BASED ON CHAR11 FRI JUL 18 14:30:18 CEST 2008 
! 
!                       ********************* 
                        SUBROUTINE ADD_CHAR41 
!                       ********************* 
! 
     & ( U , V , W , DT , NRK , X , Y , ZSTAR , Z , IKLE2 , IBOR , 
     &   XPLOT , YPLOT , ZPLOT , DX , DY , DZ , SHP , SHZ , ELT , ETA, 
     &   NSP , ISPDONE, NPLOT , NPOIN2 , NELEM2 , NPLAN ,   
     &   SURDET , SENS , TEST, IFAPAR, NOMB,NARRV) 
 
      USE BIEF 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER         , INTENT(IN)    :: SENS,NPLAN,NOMB,NARRV 
      INTEGER         , INTENT(IN)    :: NPOIN2,NELEM2,NPLOT,NRK 
      INTEGER         , INTENT(IN)    :: IKLE2(NELEM2,3) 
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NSP(NPLOT) 
      INTEGER         , INTENT(INOUT) :: ISPDONE(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN) 
      DOUBLE PRECISION, INTENT(IN)    :: W(NPOIN2,NPLAN),SURDET(NELEM2) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: ZPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT),SHZ(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2),DT 
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT),TEST(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: DZ(NPLOT) 
      INTEGER         , INTENT(IN)    :: IBOR(NELEM2,5,NPLAN-1) 
      INTEGER         , INTENT(INOUT) :: ETA(NPLOT) 
      INTEGER, INTENT(IN)             :: IFAPAR(6,*) 
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER IELE,ISO 
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,IET2,ISOH,ISOV,IFA,ISUI(3) 
      INTEGER IPROC,ILOC 
! 
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,ZP,DENOM 
      DOUBLE PRECISION DELTAZ,EPSDZ,PAS2 
! 
      INTRINSIC ABS 
 
      INTEGER P_IMAX 
      EXTERNAL P_IMAX 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA EPSILO / -1.D-6 / 
      DATA EPSDZ /1.D-4/ 
! 
!*********************************************************************** 
!  DEBUG PRINTOUTS  
! 
      IF (NCSIZE<=1) THEN  
        WRITE(LU,*) 'CALLING ADD_CHAR41 IN A SERIAL RUN.' 
        CALL PLANTE(1) 
        STOP  
      ENDIF 
      IF (TRACE) CALL PRINT_RECVCHAR  
     &                 (' ===> RECVCHAR INSIDE ADD_CHAR41',NOMB,NARRV) 
! 
!----------------------------------------------------------------------- 
! FILL ELT,NSP,XPLOT,YPLOT, COMPUTE VALID SHP FUNCTIONS, RANGE 1..NPLOT 
! IMPORTANT: THE COMPUTED SHP(IPLOT) APPLIED LATER ON  
! IN THE INTERPOLATION!... 
! 
      DO IPLOT=1,NPLOT 
        XPLOT(IPLOT)   = RECVCHAR(IPLOT)%XP  
        YPLOT(IPLOT)   = RECVCHAR(IPLOT)%YP  
        ZPLOT(IPLOT)   = RECVCHAR(IPLOT)%ZP  
        ELT(IPLOT)     = RECVCHAR(IPLOT)%INE 
        ETA(IPLOT)     = RECVCHAR(IPLOT)%KNE 
        NSP(IPLOT)     = RECVCHAR(IPLOT)%NSP ! R-K STEPS TO BE FULLFILLED 
        ISPDONE(IPLOT) = RECVCHAR(IPLOT)%ISP ! R-K STEPS ALREADY DONE  
        IEL = ELT(IPLOT) 
        IET = ETA(IPLOT)  
        XP  = XPLOT(IPLOT) 
        YP  = YPLOT(IPLOT)  
        ZP  = ZPLOT(IPLOT)  
        I1 = IKLE2(IEL,1) 
        I2 = IKLE2(IEL,2) 
        I3 = IKLE2(IEL,3) 
        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                 -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                 -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                 -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
        SHZ(IPLOT) = (ZP-ZSTAR(IET)) / (ZSTAR(IET+1)-ZSTAR(IET)) 
!       ASSUME ALL ARE LOCALISED, IT WILL BE SET OTHERWISE IF LOST-AGAIN  
        RECVCHAR(IPLOT)%NEPID=-1 
! 
! 
! IF SOME OF THE SHP FUNCTIONS ARE NEGATIVE, WE ARE IN A WRONG ELEMENT 
! (XP,YP) PROBABLY DEEPER IN THE SUBDOMAIN THAN THE HALO CELL GIVEN IN "INE" 
! 
        DO WHILE(ANY(SHP(:,IPLOT)<EPSILO).OR.SHZ(IPLOT).LT.EPSILO.OR. 
     &                                       SHZ(IPLOT).GT.1.D0-EPSILO) 
          ISO=0 
          IF(SHP(1,IPLOT).LT.   EPSILO) ISO=IBSET(ISO,2) 
          IF(SHP(2,IPLOT).LT.   EPSILO) ISO=IBSET(ISO,3) 
          IF(SHP(3,IPLOT).LT.   EPSILO) ISO=IBSET(ISO,4) 
          IF(SHZ(IPLOT).LT.     EPSILO) ISO=IBSET(ISO,0) 
          IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
               ISOH = IAND(ISO,28) 
               ISOV = IAND(ISO, 3) 
               IEL = ELT(IPLOT) 
               IET = ETA(IPLOT) 
               XP = XPLOT(IPLOT) 
               YP = YPLOT(IPLOT) 
               ZP = ZPLOT(IPLOT) 
! 
               IF (ISOH.NE.0) THEN 
! 
                  IF (ISOH.EQ.4) THEN 
                     IFA = 2 
                  ELSEIF (ISOH.EQ.8) THEN 
                     IFA = 3 
                  ELSEIF (ISOH.EQ.16) THEN 
                     IFA = 1 
                  ELSEIF (ISOH.EQ.12) THEN 
                     IFA = 2 
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,3))-YP).LT. 
     &                   DY(IPLOT)*(X(IKLE2(IEL,3))-XP)) IFA = 3 
                  ELSEIF (ISOH.EQ.24) THEN 
                     IFA = 3 
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,1))-YP).LT. 
     &                   DY(IPLOT)*(X(IKLE2(IEL,1))-XP)) IFA = 1 
                  ELSE 
                     IFA = 1 
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,2))-YP).LT. 
     &                   DY(IPLOT)*(X(IKLE2(IEL,2))-XP)) IFA = 2 
                  ENDIF 
! 
                  IF (ISOV.GT.0) THEN 
                     IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN 
                       A1 = (ZP-ZSTAR(IET+ISOV-1)) / DZ(IPLOT) 
                     ELSE 
                       A1 = 0.D0 
                     ENDIF 
                     I1 = IKLE2(IEL,IFA) 
                     I2 = IKLE2(IEL,ISUI(IFA)) 
                     IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT. 
     &                 (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOV+3 
                  ENDIF 
! 
               ELSE 
! 
                  IFA = ISOV + 3 
! 
               ENDIF 
! 
               IEL = IBOR(IEL,IFA,IET) 
! 
               IF (IFA.LE.3) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE DU PRISME EST UNE FACE QUADRAN. 
!  ================================================================= 
!----------------------------------------------------------------------- 
! 
                  IF (IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST INTERNE AU DOMAINE 
!  ON SE RELOCALISE DANS L'ELEMENT ADJACENT 
!----------------------------------------------------------------------- 
! 
                     I1 = IKLE2(IEL,1) 
                     I2 = IKLE2(IEL,2) 
                     I3 = IKLE2(IEL,3) 
! 
                     ELT(IPLOT) = IEL 
                     SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
                     SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
                     SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
                     ISO = ISOV 
! 
                     IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
                     IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
                     IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
                     CYCLE 
! 
                  ENDIF 
! 
!----------------------------------------------------------------------- 
! HERE WE TEST PASSING TO THE NEIGHBOUR SUBDOMAIN AND COLLECT DATA 
!----------------------------------------------------------------------- 
! 
               IF(IEL==-2) THEN  ! A LOST-AGAIN TRACEBACK DETECTED  
! 
                 IPROC=IFAPAR(IFA,ELT(IPLOT)) 
                 ILOC=IFAPAR(IFA+3,ELT(IPLOT))   
                 RECVCHAR(IPLOT)%NEPID=IPROC 
                 RECVCHAR(IPLOT)%INE=ILOC  
                 RECVCHAR(IPLOT)%KNE=ETA(IPLOT) 
                 EXIT  
! 
               ENDIF  
! 
!----------------------------------------------------------------------- 
! FURTHER ON, THE SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
!----------------------------------------------------------------------- 
! 
                  DXP = DX(IPLOT) 
                  DYP = DY(IPLOT) 
                  I1  = IKLE2(ELT(IPLOT),IFA) 
                  I2  = IKLE2(ELT(IPLOT),ISUI(IFA)) 
                  DX1 = X(I2) - X(I1) 
                  DY1 = Y(I2) - Y(I1) 
! 
                  IF(IEL.EQ.-1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE SOLIDE 
!  ON PROJETTE LE RELICAT SUR LA FRONTIERE PUIS SE RELOCALISE 
!----------------------------------------------------------------------- 
! 
                     A1 = (DXP*DX1+DYP*DY1) / (DX1**2+DY1**2) 
                     DX(IPLOT) = A1 * DX1 
                     DY(IPLOT) = A1 * DY1 
! 
                     A1=((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2) 
                     SHP(          IFA  ,IPLOT) = 1.D0 - A1 
                     SHP(     ISUI(IFA) ,IPLOT) = A1 
                     SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0 
                     XPLOT(IPLOT) = X(I1) + A1 * DX1 
                     YPLOT(IPLOT) = Y(I1) + A1 * DY1 
! 
                     ISO = ISOV 
! 
                     IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
                     IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
                     IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
                     CYCLE 
! 
                  ENDIF 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE LIQUIDE 
!  ON ARRETE LA REMONTEE DES CARACTERISTIQUE (SIGNE DE ELT) 
! 
!     OU QUE 
! 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE INTERFACE DE SOUS-DOMAINES 
!  POINT D'INTERFACE QUI SERA TRAITE PAR LE SOUS-DOMAINE VOISIN 
!  ON SE CONTENTE DE METTRE ICI TEST A ZERO 
!----------------------------------------------------------------------- 
! 
!>>>> 
!                 A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1)))/(DXP*DY1-DYP*DX1) 
                  DENOM = DXP*DY1-DYP*DX1 
                  IF(ABS(DENOM).GT.1.D-8) THEN 
                     A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM 
                  ELSE 
                     A1 = 0.D0 
                  ENDIF 
!<<<< 
                  IF (A1.GT.1.D0) A1 = 1.D0 
                  IF (A1.LT.0.D0) A1 = 0.D0 
                  SHP(          IFA  ,IPLOT) = 1.D0 - A1 
                  SHP(     ISUI(IFA) ,IPLOT) = A1 
                  SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0 
                  XPLOT(IPLOT) = X(I1) + A1 * DX1 
                  YPLOT(IPLOT) = Y(I1) + A1 * DY1 
                  IF(ABS(DXP).GT.ABS(DYP)) THEN 
                     A1 = (XP-XPLOT(IPLOT))/DXP 
                  ELSE 
                     A1 = (YP-YPLOT(IPLOT))/DYP 
                  ENDIF 
                  ZPLOT(IPLOT) = ZP - A1*DZ(IPLOT) 
                  SHZ(IPLOT) = (ZPLOT(IPLOT)-ZSTAR(IET)) 
     &                       / (ZSTAR(IET+1)-ZSTAR(IET)) 
                  ELT(IPLOT) = - SENS * ELT(IPLOT) 
! 
!                 CAN ONLY HAPPEN IN PARALLEL  ACTUALLY, NOT REQUIRED 
                  IF(IEL.EQ.-2) TEST(IPLOT) = 0.D0 
                  ! A FUSE 
                  IF(IEL==-2) WRITE(LU,*) ' *** SHIT IPLOT: ',IPLOT 
! 
               ELSE 
! 
!----------------------------------------------------------------------- 
!  CAS OU IFA = 4 OU 5  
!  LA, ON SAIT QUE LA FACE DE SORTIE DU PRISME EST UNE FACE TRIANGULAIRE 
!  ===================================================================== 
!----------------------------------------------------------------------- 
! 
                  IFA = IFA - 4 
!                 HENCE IFA NOW EQUALS 0 OR 1 
! 
                  IF (IEL.EQ.1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST INTERNE AU DOMAINE 
!  ET ON N'A PAS BESOIN DE RECALCULER LES VITESSES 
!  ON SE RELOCALISE DANS L'ELEMENT ADJACENT 
!----------------------------------------------------------------------- 
! 
                     ETA(IPLOT) = IET + IFA + IFA - 1 
                     SHZ(IPLOT) = (ZP-ZSTAR(ETA(IPLOT))) 
     &                   / (ZSTAR(ETA(IPLOT)+1)-ZSTAR(ETA(IPLOT))) 
! 
                     ISO = ISOH 
! 
                     IF(SHZ(IPLOT).LT.     EPSILO) ISO=IBSET(ISO,0) 
                     IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
! 
                     CYCLE 
! 
                  ENDIF 
! 
                  IF(IEL.EQ.-1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE SOLIDE 
!  ON PROJETTE LE RELIQUAT SUR LA FRONTIERE PUIS ON SE RELOCALISE 
!----------------------------------------------------------------------- 
! 
                     ZPLOT(IPLOT) = ZSTAR(IET+IFA) 
                     DZ   (IPLOT) = 0.D0 
                     SHZ  (IPLOT) = IFA 
! 
                     ISO = ISOH 
                     IF(ISOH.NE.0) CYCLE 
! 
                  ELSE 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE LIQUIDE (CAS 0) 
!      ON ARRETE ALORS LA REMONTEE DES CARACTERISTIQUES (SIGNE DE ELT) 
!  OU, QUE L'ON VIENT DE TRAVERSER UN PLAN AVEC RECALCUL DES VITESSES 
!  DEMANDE (CAS 2) 
!----------------------------------------------------------------------- 
! 
                     IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN 
                       A1 = (ZP-ZSTAR(IET+IFA)) / DZ(IPLOT) 
                     ELSE 
                       A1 = 0.D0 
                     ENDIF 
                     XP = XP - A1*DX(IPLOT) 
                     YP = YP - A1*DY(IPLOT) 
                     ZP = ZSTAR(IET+IFA) 
                     IELE = ELT(IPLOT) 
                     I1 = IKLE2(IELE,1) 
                     I2 = IKLE2(IELE,2) 
                     I3 = IKLE2(IELE,3) 
! 
                     SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IELE) 
                     SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IELE) 
                     SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IELE) 
! 
                     IF(IEL.EQ.2) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE SE SITUE SUR LE PLAN OU ON DEMANDE 
!  UN RECALCUL DES VITESSES 
!----------------------------------------------------------------------- 
! 
!                       IF IFA = 1 EXIT THROUGH THE TOP 
!                       IF IFA = 0 EXIT THROUGH THE BOTTOM 
!                       THEN NEW IET IS  IET+1 IF IFA = 1 
!                                    AND IET-1 IF IFA = 0 
!                       THIS IS SUMMARISED BY IET=IET+2*IFA-1 
! 
!                       RECOMPUTED VELOCITIES MUST BE TAKEN AT IET2=IET+IFA 
!                       I.E. BOTTOM IF EXIT THROUGH THE BOTTOM 
!                           AND TOP IF EXIT THROUGH THE TOP 
! 
                        IET2 = IET + IFA 
                        IET  = IET + IFA + IFA - 1 
!                       NOTE JMH : IT SHOULD BE HERE 
!                       PAS2=PAS2*A1, BUT NO IDEA OF THE REMAINING TIME 
!                       PAS2 AT THIS LEVEL 
                        PAS = SENS * A1 * DT / NSP(IPLOT) 
! 
                        DX(IPLOT) = ( U(I1,IET2)*SHP(1,IPLOT) 
     &                              + U(I2,IET2)*SHP(2,IPLOT) 
     &                              + U(I3,IET2)*SHP(3,IPLOT) ) * PAS 
! 
                        DY(IPLOT) = ( V(I1,IET2)*SHP(1,IPLOT) 
     &                              + V(I2,IET2)*SHP(2,IPLOT) 
     &                              + V(I3,IET2)*SHP(3,IPLOT) ) * PAS 
! 
                        DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT) 
     &                          + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT) 
     &                          + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT) 
! 
                        IF(DELTAZ.GT.EPSDZ) THEN 
                          DZ(IPLOT) = ( W(I1,IET2)*SHP(1,IPLOT) 
     &                                + W(I2,IET2)*SHP(2,IPLOT) 
     &                                + W(I3,IET2)*SHP(3,IPLOT) )*PAS 
     &                              * (ZSTAR(IET+1)-ZSTAR(IET)) / DELTAZ 
                        ELSE 
                          DZ(IPLOT) = 0.D0 
                        ENDIF 
! 
                        XP = XP + DX(IPLOT) 
                        YP = YP + DY(IPLOT) 
                        ZP = ZP + DZ(IPLOT) 
! 
                        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IELE) 
                        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IELE) 
                        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IELE) 
                        SHZ(IPLOT)=(ZP-ZSTAR(IET))/ 
     &                                         (ZSTAR(IET+1)-ZSTAR(IET)) 
! 
                        XPLOT(IPLOT) = XP 
                        YPLOT(IPLOT) = YP 
                        ZPLOT(IPLOT) = ZP 
                        ETA(IPLOT) = IET 
! 
                        ISO = 0 
! 
               IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
               IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
               IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
               IF(SHZ(IPLOT).LT.     EPSILO) ISO=IBSET(ISO,0) 
               IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
! 
                        CYCLE 
! 
                     ENDIF 
! 
                     XPLOT(IPLOT) = XP 
                     YPLOT(IPLOT) = YP 
                     ZPLOT(IPLOT) = ZP 
                     SHZ  (IPLOT) = IFA 
                     ELT  (IPLOT) = - SENS * ELT(IPLOT) 
                     EXIT 
! 
                  ENDIF 
! 
               ENDIF 
        ENDDO 
! 
      ENDDO  
! 
!----------------------------------------------------------------------- 
!  POUR TOUT PAS DE R-K REPETER 
!----------------------------------------------------------------------- 
! 
      DO IPLOT=1,NPLOT 
! 
      PAS = SENS * DT / NSP(IPLOT) 
! 
      DO ISP =1,NSP(IPLOT) 
! 
!----------------------------------------------------------------------- 
!  LOCALISATION DU POINT D'ARRIVEE DE TOUTES LES CARACTERISTIQUES 
!----------------------------------------------------------------------- 
! 
             ISO = 0  
             PAS2=PAS 
! 
             IF ( RECVCHAR(IPLOT)%NEPID==-1 .AND.  
     &              ISP>ISPDONE(IPLOT) ) THEN 
! 
               IEL = ELT(IPLOT) 
               IET = ETA(IPLOT) 
! 
               I1 = IKLE2(IEL,1) 
               I2 = IKLE2(IEL,2) 
               I3 = IKLE2(IEL,3) 
 
! 
               DX(IPLOT) = ( U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + U(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &                     + U(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &                     + U(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) * PAS 
! 
               DY(IPLOT) = ( V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + V(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &                     + V(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &                     + V(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) * PAS 
! 
               DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT) 
     &                 + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT) 
     &                 + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT) 
! 
               IF(DELTAZ.GT.EPSDZ) THEN 
               DZ(IPLOT) = ( W(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + W(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + W(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &                     + W(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &                     + W(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &                     + W(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) 
     &                     * PAS * (ZSTAR(IET+1)-ZSTAR(IET)) / DELTAZ 
               ELSE 
                 DZ(IPLOT) = 0.D0 
               ENDIF 
! 
               XP = XPLOT(IPLOT) + DX(IPLOT) 
               YP = YPLOT(IPLOT) + DY(IPLOT) 
               ZP = ZPLOT(IPLOT) + DZ(IPLOT) 
! 
               SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
               SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
               SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
               SHZ(IPLOT) = (ZP-ZSTAR(IET)) / (ZSTAR(IET+1)-ZSTAR(IET)) 
! 
               XPLOT(IPLOT) = XP 
               YPLOT(IPLOT) = YP 
               ZPLOT(IPLOT) = ZP 
! 
               IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
               IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
               IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
               IF(SHZ(IPLOT).LT.     EPSILO) ISO=IBSET(ISO,0) 
               IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
! 
           ! CONTINUOUS SETTING OF THE REACHED POSITION FOR IPLOT  
           ! AND THE NUMBER OF STEPS DONE ALREADY  
 
               RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
               RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
               RECVCHAR(IPLOT)%ZP=ZPLOT(IPLOT) 
               RECVCHAR(IPLOT)%INE=ELT(IPLOT) 
               RECVCHAR(IPLOT)%ISP=ISP 
 
            ENDIF 
! 
!----------------------------------------------------------------------- 
!  TRAITEMENT PARTICULIER POUR LES CARACTERISTIQUES SORTIES 
!  DE L'ELEMENT DE DEPART 
!----------------------------------------------------------------------- 
! 
50          CONTINUE 
! 
            IF (RECVCHAR(IPLOT)%NEPID==-1.AND.ISO.NE.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QU'ON EST SORTI DE L'ELEMENT 
!----------------------------------------------------------------------- 
! 
               ISOH = IAND(ISO,28) 
               ISOV = IAND(ISO, 3) 
               IEL = ELT(IPLOT) 
               IET = ETA(IPLOT) 
               XP = XPLOT(IPLOT) 
               YP = YPLOT(IPLOT) 
               ZP = ZPLOT(IPLOT) 
! 
               IF (ISOH.NE.0) THEN 
! 
                  IF (ISOH.EQ.4) THEN 
                     IFA = 2 
                  ELSEIF (ISOH.EQ.8) THEN 
                     IFA = 3 
                  ELSEIF (ISOH.EQ.16) THEN 
                     IFA = 1 
                  ELSEIF (ISOH.EQ.12) THEN 
                     IFA = 2 
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,3))-YP).LT. 
     &                   DY(IPLOT)*(X(IKLE2(IEL,3))-XP)) IFA = 3 
                  ELSEIF (ISOH.EQ.24) THEN 
                     IFA = 3 
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,1))-YP).LT. 
     &                   DY(IPLOT)*(X(IKLE2(IEL,1))-XP)) IFA = 1 
                  ELSE 
                     IFA = 1 
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,2))-YP).LT. 
     &                   DY(IPLOT)*(X(IKLE2(IEL,2))-XP)) IFA = 2 
                  ENDIF 
! 
                  IF (ISOV.GT.0) THEN 
                     IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN 
                       A1 = (ZP-ZSTAR(IET+ISOV-1)) / DZ(IPLOT) 
                     ELSE 
                       A1 = 0.D0 
                     ENDIF 
                     I1 = IKLE2(IEL,IFA) 
                     I2 = IKLE2(IEL,ISUI(IFA)) 
                     IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT. 
     &                 (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOV+3 
                  ENDIF 
! 
               ELSE 
! 
                  IFA = ISOV + 3 
! 
               ENDIF 
! 
               IEL = IBOR(IEL,IFA,IET) 
! 
               IF (IFA.LE.3) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE DU PRISME EST UNE FACE QUADRAN. 
!  ================================================================= 
!----------------------------------------------------------------------- 
! 
                  IF (IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST INTERNE AU DOMAINE 
!  ON SE RELOCALISE DANS L'ELEMENT ADJACENT 
!----------------------------------------------------------------------- 
! 
                     I1 = IKLE2(IEL,1) 
                     I2 = IKLE2(IEL,2) 
                     I3 = IKLE2(IEL,3) 
! 
                     ELT(IPLOT) = IEL 
                     SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
                     SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
                     SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
                     ISO = ISOV 
! 
                     IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
                     IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
                     IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
                     GOTO 50 
! 
                  ENDIF 
! 
!----------------------------------------------------------------------- 
! HERE WE TEST PASSING TO THE NEIGHBOUR SUBDOMAIN AND COLLECT DATA 
!----------------------------------------------------------------------- 
! 
               IF(IEL==-2) THEN  ! A LOST-AGAIN TRACEBACK DETECTED  
! 
                 IPROC=IFAPAR(IFA,ELT(IPLOT)) 
                 ILOC=IFAPAR(IFA+3,ELT(IPLOT))   
                 RECVCHAR(IPLOT)%NEPID=IPROC 
                 RECVCHAR(IPLOT)%INE=ILOC  
                 RECVCHAR(IPLOT)%KNE=ETA(IPLOT) 
! 
                 EXIT ! LOOP ON NSP 
! 
               ENDIF  
! 
!----------------------------------------------------------------------- 
! FURTHER ON, THE SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
!----------------------------------------------------------------------- 
! 
                  DXP = DX(IPLOT) 
                  DYP = DY(IPLOT) 
                  I1  = IKLE2(ELT(IPLOT),IFA) 
                  I2  = IKLE2(ELT(IPLOT),ISUI(IFA)) 
                  DX1 = X(I2) - X(I1) 
                  DY1 = Y(I2) - Y(I1) 
! 
                  IF(IEL.EQ.-1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE SOLIDE 
!  ON PROJETTE LE RELICAT SUR LA FRONTIERE PUIS SE RELOCALISE 
!----------------------------------------------------------------------- 
! 
                     A1 = (DXP*DX1+DYP*DY1) / (DX1**2+DY1**2) 
                     DX(IPLOT) = A1 * DX1 
                     DY(IPLOT) = A1 * DY1 
! 
                     A1=((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2) 
                     SHP(          IFA  ,IPLOT) = 1.D0 - A1 
                     SHP(     ISUI(IFA) ,IPLOT) = A1 
                     SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0 
                     XPLOT(IPLOT) = X(I1) + A1 * DX1 
                     YPLOT(IPLOT) = Y(I1) + A1 * DY1 
! 
                     ISO = ISOV 
! 
                     IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
                     IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
                     IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
                     GOTO 50 
! 
                  ENDIF 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE LIQUIDE 
!  ON ARRETE LA REMONTEE DES CARACTERISTIQUE (SIGNE DE ELT) 
! 
!     OU QUE 
! 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE INTERFACE DE SOUS-DOMAINES 
!  POINT D'INTERFACE QUI SERA TRAITE PAR LE SOUS-DOMAINE VOISIN 
!  ON SE CONTENTE DE METTRE ICI TEST A ZERO 
!----------------------------------------------------------------------- 
! 
!>>>> 
!                 A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1)))/(DXP*DY1-DYP*DX1) 
                  DENOM = DXP*DY1-DYP*DX1 
                  IF(ABS(DENOM).GT.1.D-8) THEN 
                     A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM 
                  ELSE 
                     A1 = 0.D0 
                  ENDIF 
!<<<< 
                  IF (A1.GT.1.D0) A1 = 1.D0 
                  IF (A1.LT.0.D0) A1 = 0.D0 
                  SHP(          IFA  ,IPLOT) = 1.D0 - A1 
                  SHP(     ISUI(IFA) ,IPLOT) = A1 
                  SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0 
                  XPLOT(IPLOT) = X(I1) + A1 * DX1 
                  YPLOT(IPLOT) = Y(I1) + A1 * DY1 
                  IF(ABS(DXP).GT.ABS(DYP)) THEN 
                    A1 = (XP-XPLOT(IPLOT))/DXP 
                  ELSE 
                    A1 = (YP-YPLOT(IPLOT))/DYP 
                  ENDIF 
                  ZPLOT(IPLOT) = ZP - A1*DZ(IPLOT) 
                  SHZ(IPLOT) = (ZPLOT(IPLOT)-ZSTAR(IET)) 
     &                       / (ZSTAR(IET+1)-ZSTAR(IET)) 
                  ELT(IPLOT) = - SENS * ELT(IPLOT) 
                  NSP(IPLOT) = ISP 
! 
!                 CAN ONLY HAPPEN IN PARALLEL  ACTUALLY, NOT REQUIRED 
                  IF(IEL.EQ.-2) TEST(IPLOT) = 0.D0 
                  ! A FUSE 
                  IF(IEL==-2) WRITE(LU,*) ' *** SHIT IPLOT: ',IPLOT 
! 
               ELSE 
! 
!----------------------------------------------------------------------- 
!  CAS OU IFA = 4 OU 5  
!  LA, ON SAIT QUE LA FACE DE SORTIE DU PRISME EST UNE FACE TRIANGULAIRE 
!  ===================================================================== 
!----------------------------------------------------------------------- 
! 
                  IFA = IFA - 4 
!                 HENCE IFA NOW EQUALS 0 OR 1 
! 
                  IF (IEL.EQ.1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST INTERNE AU DOMAINE 
!  ET ON N'A PAS BESOIN DE RECALCULER LES VITESSES 
!  ON SE RELOCALISE DANS L'ELEMENT ADJACENT 
!----------------------------------------------------------------------- 
! 
                     ETA(IPLOT) = IET + IFA + IFA - 1 
                     SHZ(IPLOT) = (ZP-ZSTAR(ETA(IPLOT))) 
     &                   / (ZSTAR(ETA(IPLOT)+1)-ZSTAR(ETA(IPLOT))) 
! 
                     ISO = ISOH 
! 
                     IF(SHZ(IPLOT).LT.     EPSILO) ISO=IBSET(ISO,0) 
                     IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
! 
                     GOTO 50 
! 
                  ENDIF 
! 
                  IF(IEL.EQ.-1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE SOLIDE 
!  ON PROJETTE LE RELIQUAT SUR LA FRONTIERE PUIS ON SE RELOCALISE 
!----------------------------------------------------------------------- 
! 
                     ZPLOT(IPLOT) = ZSTAR(IET+IFA) 
                     DZ   (IPLOT) = 0.D0 
                     SHZ  (IPLOT) = IFA 
! 
                     ISO = ISOH 
                     IF(ISOH.NE.0) GOTO 50 
! 
                  ELSE 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE LIQUIDE (CAS 0) 
!      ON ARRETE ALORS LA REMONTEE DES CARACTERISTIQUES (SIGNE DE ELT) 
!  OU, QUE L'ON VIENT DE TRAVERSER UN PLAN AVEC RECALCUL DES VITESSES 
!  DEMANDE (CAS 2) 
!----------------------------------------------------------------------- 
! 
                     IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN 
                       A1 = (ZP-ZSTAR(IET+IFA)) / DZ(IPLOT) 
                     ELSE 
                       A1 = 0.D0 
                     ENDIF 
                     XP = XP - A1*DX(IPLOT) 
                     YP = YP - A1*DY(IPLOT) 
                     ZP = ZSTAR(IET+IFA) 
                     IELE = ELT(IPLOT) 
                     I1 = IKLE2(IELE,1) 
                     I2 = IKLE2(IELE,2) 
                     I3 = IKLE2(IELE,3) 
! 
                     SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IELE) 
                     SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IELE) 
                     SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IELE) 
! 
                     IF(IEL.EQ.2) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE SE SITUE SUR LE PLAN OU ON DEMANDE 
!  UN RECALCUL DES VITESSES 
!----------------------------------------------------------------------- 
! 
!                       IF IFA = 1 EXIT THROUGH THE TOP 
!                       IF IFA = 0 EXIT THROUGH THE BOTTOM 
!                       THEN NEW IET IS  IET+1 IF IFA = 1 
!                                    AND IET-1 IF IFA = 0 
!                       THIS IS SUMMARISED BY IET=IET+2*IFA-1 
! 
!                       RECOMPUTED VELOCITIES MUST BE TAKEN AT IET2=IET+IFA 
!                       I.E. BOTTOM IF EXIT THROUGH THE BOTTOM 
!                           AND TOP IF EXIT THROUGH THE TOP 
! 
                        IET2 = IET + IFA 
                        IET  = IET + IFA + IFA - 1 
                        PAS2=A1*PAS 
! 
                        DX(IPLOT) = ( U(I1,IET2)*SHP(1,IPLOT) 
     &                              + U(I2,IET2)*SHP(2,IPLOT) 
     &                              + U(I3,IET2)*SHP(3,IPLOT) ) * PAS2 
! 
                        DY(IPLOT) = ( V(I1,IET2)*SHP(1,IPLOT) 
     &                              + V(I2,IET2)*SHP(2,IPLOT) 
     &                              + V(I3,IET2)*SHP(3,IPLOT) ) * PAS2 
! 
                        DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT) 
     &                          + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT) 
     &                          + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT) 
! 
                        IF(DELTAZ.GT.EPSDZ) THEN 
                        DZ(IPLOT) = ( W(I1,IET2)*SHP(1,IPLOT) 
     &                              + W(I2,IET2)*SHP(2,IPLOT) 
     &                              + W(I3,IET2)*SHP(3,IPLOT) ) * PAS2 
     &                              * (ZSTAR(IET+1)-ZSTAR(IET)) / DELTAZ 
                        ELSE 
                          DZ(IPLOT) = 0.D0 
                        ENDIF 
! 
                        XP = XP + DX(IPLOT) 
                        YP = YP + DY(IPLOT) 
                        ZP = ZP + DZ(IPLOT) 
! 
                        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IELE) 
                        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IELE) 
                        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IELE) 
                        SHZ(IPLOT)=(ZP-ZSTAR(IET))/ 
     &                                         (ZSTAR(IET+1)-ZSTAR(IET)) 
! 
                        XPLOT(IPLOT) = XP 
                        YPLOT(IPLOT) = YP 
                        ZPLOT(IPLOT) = ZP 
                        ETA(IPLOT) = IET 
! 
                        ISO = 0 
! 
               IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2) 
               IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3) 
               IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4) 
! 
               IF(SHZ(IPLOT).LT.     EPSILO) ISO=IBSET(ISO,0) 
               IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
! 
                        GOTO 50 
! 
                     ENDIF 
! 
                     XPLOT(IPLOT) = XP 
                     YPLOT(IPLOT) = YP 
                     ZPLOT(IPLOT) = ZP 
                     SHZ  (IPLOT) = IFA 
                     ELT  (IPLOT) = - SENS * ELT(IPLOT) 
!                    NEXT LINE NOT NECESSARY ? 
                     NSP  (IPLOT) = ISP 
                     EXIT 
! 
                  ENDIF 
! 
               ENDIF 
! 
            ENDIF 
! 
! 
        ENDDO 
      ENDDO 
! 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END SUBROUTINE ADD_CHAR41 
 
!----------------------------------------------------------------------- 
! CHAR11 MODIFIED FOR INITIAL COLLECTING OF THE LOST CHARACTERISTICS  
! I.E. THE ONES CROSSING INTERFACE PARTITIONS IN THE PARALLEL CASE 
! IFAPAR  :: DELIVERS LOCAL ELEMENT NUMBER AND THE PARTITION NR THERE   
!            WHEN CROSSING THE INTERFACE VIA A HALO ELEMENT FACE   
!----------------------------------------------------------------------- 
! JAJ MODIFIED WED JUL 16 18:24:08 CEST 2008 
! 
  
!                       ****************** 
                        SUBROUTINE SCHAR11 
!                       ****************** 
! 
     & ( U , V , DT , NRK , X , Y , IKLE , IFABOR , 
     &   XPLOT , YPLOT , DX , DY , SHP , ELT , NSP , 
     &   NPLOT , NPOIN , NELEM , NELMAX , SURDET , SENS , TEST,  
     &   IFAPAR, MESH,NCHDIM,NCHARA) 
! 
!*********************************************************************** 
! BIEF VERSION 5.9           24/04/97    J-M JANIN (LNH) 30 87 72 84 
! 
!*********************************************************************** 
! 
!  FONCTION : 
! 
!     REMONTEE OU DESCENTE 
!     DES COURBES CARACTERISTIQUES 
!     SUR DES QUADRILATERES P1 
!     DANS L'INTERVALLE DE TEMPS DT 
!     AVEC UNE DISCRETISATION ELEMENTS FINIS 
! 
! 
!  DISCRETISATION : 
! 
!     LE DOMAINE EST APPROCHE PAR UNE DISCRETISATION ELEMENTS FINIS 
!     UNE APPROXIMATION LOCALE EST DEFINIE POUR LE VECTEUR VITESSE : 
!     LA VALEUR EN UN POINT D'UN ELEMENT NE DEPEND QUE DES VALEURS 
!     AUX NOEUDS DE CET ELEMENT 
! 
! 
!  RESTRICTIONS ET HYPOTHESES : 
! 
!     LE CHAMP CONVECTEUR U EST SUPPOSE INDEPENDANT DU TEMPS 
!     LE DERIVANT EST SUPPOSE PONCTUEL DONC NON DISPERSIF 
! 
!----------------------------------------------------------------------- 
!                             ARGUMENTS 
! .________________.____.______________________________________________. 
! |      NOM       |MODE|                   ROLE                       | 
! |________________|____|______________________________________________| 
! |    U,V         | -->| COMPOSANTE DE LA VITESSE DU CONVECTEUR       | 
! |    DT          | -->| PAS DE TEMPS.                                | 
! |    NRK         | -->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.           | 
! |    X,Y         | -->| COORDONNEES DES POINTS DU MAILLAGE.          | 
! |    IKLE        | -->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE    | 
! |                |    | ET GLOBALE.                                  | 
! |    IFABOR      | -->| NUMEROS DES ELEMENTS AYANT UNE FACE COMMUNE  | 
! |                |    | AVEC L'ELEMENT .  SI IFABOR<0 OU NUL         | 
! |                |    | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE   | 
! |  XPLOT,YPLOT   |<-->| POSITIONS SUCCESSIVES DES DERIVANTS.         | 
! |    DX,DY       | -- | STOCKAGE DES SOUS-PAS . | 
! |    SHP         |<-->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES    | 
! |                |    | COURBES CARACTERISTIQUES.                    | 
! |    ELT         |<-->| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES  | 
! |                |    | CARACTERISTIQUES.                            | 
! |    NSP         | -- | NOMBRE DE SOUS-PAS DE RUNGE KUTTA.           | 
! |    NPLOT       | -->| NOMBRE DE DERIVANTS.                         | 
! |    NPOIN       | -->| NOMBRE DE POINTS DU MAILLAGE.                | 
! |    NELEM       | -->| NOMBRE D'ELEMENTS.                           | 
! |    NELMAX      | -->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D| 
! |    SURDET      | -->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM. 
! |    SENS        | -->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.   | 
! |________________|____|______________________________________________| 
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE) 
!----------------------------------------------------------------------- 
!     - APPELE PAR : CARACT , DERIVE , DERLAG 
!     - PROGRAMMES APPELES : NEANT 
! 
!*********************************************************************** 
! 
      USE BIEF !, EX_CHAR11 => CHAR11 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER         , INTENT(IN)    :: SENS,NCHDIM 
      INTEGER         , INTENT(IN)    :: NPOIN,NELEM,NELMAX,NPLOT,NRK 
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,3),IFABOR(NELMAX,3) 
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NCHARA 
      INTEGER         , INTENT(OUT)   :: NSP(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),SURDET(NELEM) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: DT 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT),TEST(NPLOT) 
      INTEGER, INTENT(IN)             :: IFAPAR(6,*) 
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH 
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
!      
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,ISO,IFA,ISUI(3),ISUI2(3) 
! 
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,DENOM 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA ISUI2  / 3 , 1 , 2 / 
      DATA EPSILO / -1.D-6 / 
! 
      INTRINSIC INT,MAX,MIN,SQRT 
! 
!----------------------------------------------------------------------- 
!  POUR TOUT PAS DE R-K REPETER 
!----------------------------------------------------------------------- 
! 
      DO IPLOT=1,NPLOT 
! 
      IEL = ELT(IPLOT) 
      I1 = IKLE(IEL,1) 
      I2 = IKLE(IEL,2) 
      I3 = IKLE(IEL,3) 
      DXP = U(I1)*SHP(1,IPLOT)+U(I2)*SHP(2,IPLOT)+U(I3)*SHP(3,IPLOT) 
      DYP = V(I1)*SHP(1,IPLOT)+V(I2)*SHP(2,IPLOT)+V(I3)*SHP(3,IPLOT) 
      NSP(IPLOT)=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL)))) 
      PAS = SENS * DT / NSP(IPLOT) 
! 
      DO ISP=1,NSP(IPLOT) 
! 
!----------------------------------------------------------------------- 
!  LOCALISATION DU POINT D'ARRIVEE DE TOUTES LES CARACTERISTIQUES 
!----------------------------------------------------------------------- 
! 
         IEL = ELT(IPLOT) 
         I1 = IKLE(IEL,1) 
         I2 = IKLE(IEL,2) 
         I3 = IKLE(IEL,3) 
! 
         DX(IPLOT) = ( U(I1)*SHP(1,IPLOT) 
     &               + U(I2)*SHP(2,IPLOT) 
     &               + U(I3)*SHP(3,IPLOT) ) * PAS 
         DY(IPLOT) = ( V(I1)*SHP(1,IPLOT) 
     &               + V(I2)*SHP(2,IPLOT) 
     &               + V(I3)*SHP(3,IPLOT) ) * PAS 
         XP = XPLOT(IPLOT) + DX(IPLOT) 
         YP = YPLOT(IPLOT) + DY(IPLOT) 
! 
         SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                  -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
         SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                  -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
         SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                  -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
! 
         XPLOT(IPLOT) = XP 
         YPLOT(IPLOT) = YP 
! 
!----------------------------------------------------------------------- 
!  TRAITEMENT PARTICULIER POUR LES CARACTERISTIQUES SORTIES 
!  DE L'ELEMENT DE DEPART 
!----------------------------------------------------------------------- 
! 
50       CONTINUE 
! 
            ISO = 0 
            IF (SHP(1,IPLOT).LT.EPSILO) ISO = 1 
            IF (SHP(2,IPLOT).LT.EPSILO) ISO = ISO + 2 
            IF (SHP(3,IPLOT).LT.EPSILO) ISO = ISO + 4 
! 
            IF (ISO.NE.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QU'ON EST SORTI DE L'ELEMENT 
!----------------------------------------------------------------------- 
! 
               IEL = ELT(IPLOT) 
               XP = XPLOT(IPLOT) 
               YP = YPLOT(IPLOT) 
! 
               IF     (ISO.EQ.1) THEN 
                  IFA = 2 
               ELSEIF (ISO.EQ.2) THEN 
                  IFA = 3 
               ELSEIF (ISO.EQ.4) THEN 
                  IFA = 1 
               ELSEIF (ISO.EQ.3) THEN 
                  IFA = 2 
                  IF (DX(IPLOT)*(Y(IKLE(IEL,3))-YP).LT. 
     &                DY(IPLOT)*(X(IKLE(IEL,3))-XP)) IFA = 3 
               ELSEIF (ISO.EQ.6) THEN 
                  IFA = 3 
                  IF (DX(IPLOT)*(Y(IKLE(IEL,1))-YP).LT. 
     &                DY(IPLOT)*(X(IKLE(IEL,1))-XP)) IFA = 1 
               ELSE 
                  IFA = 1 
                  IF (DX(IPLOT)*(Y(IKLE(IEL,2))-YP).LT. 
     &                DY(IPLOT)*(X(IKLE(IEL,2))-XP)) IFA = 2 
               ENDIF 
! 
               IEL = IFABOR(IEL,IFA) 
! 
               IF (IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST INTERNE AU DOMAINE 
!  ON SE RELOCALISE DANS L'ELEMENT ADJACENT 
!----------------------------------------------------------------------- 
! 
                  I1 = IKLE(IEL,1) 
                  I2 = IKLE(IEL,2) 
                  I3 = IKLE(IEL,3) 
! 
                  ELT(IPLOT) = IEL 
                  SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
                  SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
                  SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
                  GOTO 50 
! 
               ENDIF 
! 
!----------------------------------------------------------------------- 
! FURTHER ON, THE SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
!----------------------------------------------------------------------- 
! 
               DXP = DX(IPLOT) 
               DYP = DY(IPLOT) 
               I1  = IKLE(ELT(IPLOT),IFA) 
               I2  = IKLE(ELT(IPLOT),ISUI(IFA)) 
               DX1 = X(I2) - X(I1) 
               DY1 = Y(I2) - Y(I1) 
! 
               IF(IEL.EQ.-1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE SOLIDE 
!  ON PROJETTE LE RELIQUAT SUR LA FRONTIERE PUIS ON SE RELOCALISE 
!----------------------------------------------------------------------- 
! 
                  A1 = (DXP*DX1 + DYP*DY1) / (DX1**2 + DY1**2) 
                  DX(IPLOT) = A1 * DX1 
                  DY(IPLOT) = A1 * DY1 
! 
                  A1 = ((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2) 
                  SHP(      IFA ,IPLOT) = 1.D0 - A1 
                  SHP( ISUI(IFA),IPLOT) = A1 
                  SHP(ISUI2(IFA),IPLOT) = 0.D0 
                  XPLOT(IPLOT) = X(I1) + A1 * DX1 
                  YPLOT(IPLOT) = Y(I1) + A1 * DY1 
! 
                  GOTO 50 
! 
               ENDIF 
! 
!----------------------------------------------------------------------- 
! HERE WE DETECT PASSING TO THE NEIGHBOUR SUBDOMAIN AND COLLECT DATA 
!----------------------------------------------------------------------- 
! 
               IF(IEL.EQ.-2) THEN ! INTERFACE CROSSING    
                 CALL COLLECT_CHAR  
     &             ( IPID,IPLOT,ELT(IPLOT),IFA,0,ISP,  
     &               NSP(IPLOT),XPLOT(IPLOT),YPLOT(IPLOT),0.D0,  
     &               IFAPAR,NCHDIM,NCHARA ) 
                 TEST(IPLOT)=0.D0  
! 
! ALTHOUGH A LOST TRACEBACK DETECTED AND SAVED HERE, ALLOW THE  
! FURTHER TREATMENT AS IF NOTHING HAPPENED IN ORDER TO APPLY  
! THE JMH ALGORITHM WITH "TEST" FIELD OF MARKERS  
! 
               ENDIF  
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE LIQUIDE 
!  ON ARRETE LA REMONTEE DES CARACTERISTIQUE (SIGNE DE ELT) 
! 
!     OU QUE 
! 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE INTERFACE DE SOUS-DOMAINES 
!  POINT D'INTERFACE QUI SERA TRAITE PAR LE SOUS-DOMAINE VOISIN 
!  ON SE CONTENTE DE METTRE ICI TEST A ZERO 
!----------------------------------------------------------------------- 
! 
               DENOM = DXP*DY1-DYP*DX1 
               IF(DENOM.NE.0.D0) THEN 
                 A1  = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM 
               ELSE 
                 A1  = 0.D0 
               ENDIF 
               A1 = MAX(MIN(A1,1.D0),0.D0) 
               SHP(      IFA ,IPLOT) = 1.D0 - A1 
               SHP( ISUI(IFA),IPLOT) = A1 
               SHP(ISUI2(IFA),IPLOT) = 0.D0 
               XPLOT(IPLOT) = X(I1) + A1 * DX1 
               YPLOT(IPLOT) = Y(I1) + A1 * DY1 
               ELT(IPLOT) = - SENS * ELT(IPLOT) 
               EXIT 
! 
            ENDIF 
! 
      ENDDO 
      ENDDO 
! 
!----------------------------------------------------------------------- 
!     
      RETURN 
      END SUBROUTINE SCHAR11 
 
! 1111111111111111111111111111111111111111111111111111111111111111111111 
! 
!----------------------------------------------------------------------- 
! STREAMLINE TRACKING FOR ADDITIONAL CHARACTERISTICS ARRIVED FROM  
! NEIGHBOUR PARTITIONS - THERE'S NPLOT=NARRV OF THEM  
! NOTE CHANGES IN THE INTERFACE COMPARED TO SCHAR11  
! ISPDONE :: NUMBER OF ALREADY DONE R-K STEPS BY A TRACEBACK 
! IFAPAR  :: DELIVERS LOCAL ELEMENT NUMBER AND THE PARTITION NR THERE   
!            WHEN CROSSING THE INTERFACE VIA A HALO ELEMENT FACE   
!  
! NOTE THAT SAVED SHAPE FUNCTIONS ARE APPLIED FURTHER FOR INTERPOLATION  
!----------------------------------------------------------------------- 
! JAJ PINXIT BASED ON CHAR11 WED JUL 16 18:24:08 CEST 2008 
! 
!                       ********************* 
                        SUBROUTINE ADD_CHAR11 
!                       ********************* 
! 
     & ( U , V , DT , NRK , X , Y , IKLE , IFABOR , 
     &   XPLOT , YPLOT , DX , DY , SHP , ELT , NSP , ISPDONE, 
     &   NPLOT , NPOIN , NELEM , NELMAX , SURDET , SENS , TEST, 
     &   IFAPAR, NOMB,NARRV) 
! 
      USE BIEF 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER         , INTENT(IN)    :: SENS,NOMB,NARRV 
      INTEGER         , INTENT(IN)    :: NPOIN,NELEM,NELMAX,NPLOT,NRK 
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,3),IFABOR(NELMAX,3) 
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NSP(NPLOT) 
      INTEGER         , INTENT(INOUT) :: ISPDONE(NPLOT) 
!                                          * : NPOIN OR LARGER (QUADRATIC...) 
      DOUBLE PRECISION, INTENT(IN)    :: U(*),V(*),SURDET(NELEM) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: DT 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT),TEST(NPLOT) 
      INTEGER, INTENT(IN) :: IFAPAR(6,*) 
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
!      
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,ISO,IFA,ISUI(3),ISUI2(3) 
      INTEGER IPROC,ILOC 
! 
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,DENOM 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA ISUI2  / 3 , 1 , 2 / 
      DATA EPSILO / -1.D-6 / 
! 
      INTRINSIC MAX,MIN 
! 
!*********************************************************************** 
!  DEBUG PRINTOUTS  
! 
      IF (TRACE) WRITE(LU,*) ' -> ENTERING ADD_CHAR11 ' 
      IF (NCSIZE<=1) THEN  
        WRITE(LU,*) 'CALLING ADD_CHAR11 IN A SERIAL RUN.' 
        CALL PLANTE(1) 
        STOP  
      ENDIF 
! 
      IF (TRACE) CALL PRINT_RECVCHAR 
     &                 (' ===> RECVCHAR INSIDE ADD_CHAR11',NOMB,NARRV) 
! 
!----------------------------------------------------------------------- 
! FILL ELT,NSP,XPLOT,YPLOT, COMPUTE VALID SHP FUNCTIONS, RANGE 1..NPLOT 
! IMPORTANT: THE COMPUTED SHP(IPLOT) APPLIED LATER ON  
! IN THE INTERPOLATION!... 
! 
      DO IPLOT=1,NPLOT 
        XPLOT(IPLOT) = RECVCHAR(IPLOT)%XP  
        YPLOT(IPLOT) = RECVCHAR(IPLOT)%YP   
        ELT(IPLOT)   = RECVCHAR(IPLOT)%INE 
        NSP(IPLOT)   = RECVCHAR(IPLOT)%NSP   ! R-K STEPS TO BE FULLFILLED 
        ISPDONE(IPLOT) = RECVCHAR(IPLOT)%ISP ! R-K STEPS ALREADY DONE  
        IEL = ELT(IPLOT) 
        XP  = XPLOT(IPLOT) 
        YP  = YPLOT(IPLOT)  
        I1 = IKLE(IEL,1) 
        I2 = IKLE(IEL,2) 
        I3 = IKLE(IEL,3) 
        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                 -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                 -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                 -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
! ASSUME ALL ARE LOCALISED, IT WILL BE SET OTHERWISE IF LOST-AGAIN 
! 
        RECVCHAR(IPLOT)%NEPID=-1  
! 
! IF SOME OF THE SHP FUNCTIONS ARE NEGATIVE, WE ARE IN A WRONG ELEMENT 
! (XP,YP) PROBABLY DEEPER IN THE SUBDOMAIN THAN THE HALO CELL GIVEN IN "INE" 
! 
        DO WHILE ( ANY(SHP(:,IPLOT)<EPSILO) ) 
          ISO=0 
          IF (SHP(1,IPLOT) < EPSILO) ISO = 1 
          IF (SHP(2,IPLOT) < EPSILO) ISO = ISO + 2 
          IF (SHP(3,IPLOT) < EPSILO) ISO = ISO + 4 
          IF (ISO.EQ.1) THEN 
            IFA = 2 
          ELSEIF (ISO.EQ.2) THEN 
            IFA = 3 
          ELSEIF (ISO.EQ.4) THEN 
            IFA = 1 
          ELSEIF (ISO.EQ.3) THEN 
            IFA = 2 
!                            I3 ??? 
            IF (DX(IPLOT)*(Y(IKLE(IEL,3))-YP).LT. 
     &          DY(IPLOT)*(X(IKLE(IEL,3))-XP)) IFA = 3 
          ELSEIF (ISO.EQ.6) THEN 
            IFA = 3 
            IF (DX(IPLOT)*(Y(IKLE(IEL,1))-YP).LT. 
     &          DY(IPLOT)*(X(IKLE(IEL,1))-XP)) IFA = 1 
          ELSE 
            IFA = 1 
            IF (DX(IPLOT)*(Y(IKLE(IEL,2))-YP).LT. 
     &          DY(IPLOT)*(X(IKLE(IEL,2))-XP)) IFA = 2 
          ENDIF 
          IEL = IFABOR(IEL,IFA)  
          IF (IEL>0) THEN ! INSIDE THE DOMAIN, MOVE TO ELEMENT IEL, BUT DO NOT STOP CHECKING SHP 
            I1 = IKLE(IEL,1) 
            I2 = IKLE(IEL,2) 
            I3 = IKLE(IEL,3) 
            ELT(IPLOT) = IEL 
            SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                     -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
            SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                     -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
            SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                     -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
            CYCLE 
          ENDIF  
          IF (IEL==-2) THEN  ! A LOST-AGAIN TRACEBACK DETECTED, ALREADY HERE! 
            ! SET THE IMPLANTING PARAMETERS  
            IPROC=IFAPAR(IFA  ,ELT(IPLOT)) 
            ILOC =IFAPAR(IFA+3,ELT(IPLOT))  
            RECVCHAR(IPLOT)%NEPID=IPROC !ANOTHER ONE AS IPID, MEANS ALSO NOT LOCALISED 
            RECVCHAR(IPLOT)%INE=ILOC 
            EXIT 
          ENDIF  
          ! LIQUID AND SOLID BOUNDARIES REMAIN            
          DXP = DX(IPLOT) 
          DYP = DY(IPLOT) 
          I1  = IKLE(ELT(IPLOT),IFA) 
          I2  = IKLE(ELT(IPLOT),ISUI(IFA)) 
          DX1 = X(I2) - X(I1) 
          DY1 = Y(I2) - Y(I1) 
          IF (IEL==-1) THEN ! SOLID BOUNDARY 
            A1 = (DXP*DX1 + DYP*DY1) / (DX1**2 + DY1**2) 
            DX(IPLOT) = A1 * DX1 
            DY(IPLOT) = A1 * DY1 
            A1 = ((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2) 
            SHP(      IFA ,IPLOT) = 1.D0 - A1 
            SHP( ISUI(IFA),IPLOT) = A1 
            SHP(ISUI2(IFA),IPLOT) = 0.D0 
            XPLOT(IPLOT) = X(I1) + A1 * DX1 
            YPLOT(IPLOT) = Y(I1) + A1 * DY1 
            RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) ! NEW POSITION 
            RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) ! IN THE OLD ELEMENT 
            XP=XPLOT(IPLOT) ! NEW POSITION 
            YP=YPLOT(IPLOT) ! IN THE OLD ELEMENT 
            IEL=ELT(IPLOT) 
            CYCLE ! ELT REMAINS THE SAME  
          ENDIF           
          ! NOW ONLY LIQUID BOUNDARY REMAINS 
          DENOM = DXP*DY1-DYP*DX1 
          IF (DENOM /= 0.D0) THEN 
            A1  = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM 
          ELSE 
            A1  = 0.D0 
          ENDIF 
          A1 = MAX(MIN(A1,1.D0),0.D0) 
          SHP(      IFA ,IPLOT) = 1.D0 - A1 
          SHP( ISUI(IFA),IPLOT) = A1 
          SHP(ISUI2(IFA),IPLOT) = 0.D0 
          XPLOT(IPLOT) = X(I1) + A1 * DX1 
          YPLOT(IPLOT) = Y(I1) + A1 * DY1 
          ELT(IPLOT) = - SENS * ELT(IPLOT)         ! ??? 
          ISPDONE(IPLOT) = NSP(IPLOT)+1   ! THIS WILL FORBID ENTERING FURTHER LOOPS   
          RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) ! NEW POSITION 
          RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) ! IN THE OLD ELEMENT 
          RECVCHAR(IPLOT)%ISP=NSP(IPLOT)  ! TRICKY  
          EXIT ! BUT DO NOT DO ANYTHING ELSE  
        ENDDO 
      ENDDO  
! 
!----------------------------------------------------------------------- 
!  POUR TOUT PAS DE R-K REPETER 
!----------------------------------------------------------------------- 
!  
         DO IPLOT=1,NPLOT         
         DO ISP = 1, NSP(IPLOT)  
! 
!----------------------------------------------------------------------- 
!  LOCALISATION DU POINT D'ARRIVEE DE TOUTES LES CARACTERISTIQUES 
!----------------------------------------------------------------------- 
! 
             IF (RECVCHAR(IPLOT)%NEPID==-1.AND.  
     &           ISP>ISPDONE(IPLOT) ) THEN 
! 
               IEL = ELT(IPLOT) 
               I1 = IKLE(IEL,1) 
               I2 = IKLE(IEL,2) 
               I3 = IKLE(IEL,3) 
               PAS = SENS * DT / NSP(IPLOT) 
! 
               DX(IPLOT) = ( U(I1)*SHP(1,IPLOT) 
     &                     + U(I2)*SHP(2,IPLOT) 
     &                     + U(I3)*SHP(3,IPLOT) ) * PAS 
               DY(IPLOT) = ( V(I1)*SHP(1,IPLOT) 
     &                     + V(I2)*SHP(2,IPLOT) 
     &                     + V(I3)*SHP(3,IPLOT) ) * PAS 
! 
               XP = XPLOT(IPLOT) + DX(IPLOT) 
               YP = YPLOT(IPLOT) + DY(IPLOT) 
! 
               SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
               SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
               SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
! 
               XPLOT(IPLOT) = XP 
               YPLOT(IPLOT) = YP 
! 
           ! CONTINUOUS SETTING OF THE REACHED POSITION FOR IPLOT  
           ! AND THE NUMBER OF STEPS DONE ALREADY  
 
               RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
               RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
               RECVCHAR(IPLOT)%INE=ELT(IPLOT) 
               RECVCHAR(IPLOT)%ISP=ISP 
! 
!----------------------------------------------------------------------- 
!  TRAITEMENT PARTICULIER POUR LES CARACTERISTIQUES SORTIES 
!  DE L'ELEMENT DE DEPART 
!----------------------------------------------------------------------- 
! 
50             CONTINUE 
! 
               ISO = 0 
               IF (SHP(1,IPLOT).LT.EPSILO) ISO = 1 
               IF (SHP(2,IPLOT).LT.EPSILO) ISO = ISO + 2 
               IF (SHP(3,IPLOT).LT.EPSILO) ISO = ISO + 4 
! 
               IF (ISO.NE.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QU'ON EST SORTI DE L'ELEMENT 
!----------------------------------------------------------------------- 
! 
                 IEL = ELT(IPLOT) 
                 XP = XPLOT(IPLOT) 
                 YP = YPLOT(IPLOT) 
! 
                 IF (ISO.EQ.1) THEN 
                   IFA = 2 
                 ELSEIF (ISO.EQ.2) THEN 
                   IFA = 3 
                 ELSEIF (ISO.EQ.4) THEN 
                   IFA = 1 
                 ELSEIF (ISO.EQ.3) THEN 
                   IFA = 2 
                   IF (DX(IPLOT)*(Y(IKLE(IEL,3))-YP).LT. 
     &                 DY(IPLOT)*(X(IKLE(IEL,3))-XP)) IFA = 3 
                 ELSEIF (ISO.EQ.6) THEN 
                   IFA = 3 
                   IF (DX(IPLOT)*(Y(IKLE(IEL,1))-YP).LT. 
     &                 DY(IPLOT)*(X(IKLE(IEL,1))-XP)) IFA = 1 
                 ELSE 
                   IFA = 1 
                   IF (DX(IPLOT)*(Y(IKLE(IEL,2))-YP).LT. 
     &                DY(IPLOT)*(X(IKLE(IEL,2))-XP)) IFA = 2 
               ENDIF 
! 
               IEL = IFABOR(IEL,IFA) 
! 
               IF (IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST INTERNE AU DOMAINE 
!  ON SE RELOCALISE DANS L'ELEMENT ADJACENT 
!----------------------------------------------------------------------- 
! 
                 I1 = IKLE(IEL,1) 
                 I2 = IKLE(IEL,2) 
                 I3 = IKLE(IEL,3) 
! 
                 ELT(IPLOT) = IEL 
                 SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                          -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
                 SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                          -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
                 SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                          -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
                 GOTO 50 
! 
               ENDIF 
! 
!----------------------------------------------------------------------- 
! HERE WE TEST PASSING TO THE NEIGHBOUR SUBDOMAIN AND COLLECT DATA 
!----------------------------------------------------------------------- 
 
               IF (IEL==-2) THEN  ! A LOST-AGAIN TRACEBACK DETECTED  
 
                 ! SET THE IMPLANTING PARAMETERS  
                 IPROC=IFAPAR(IFA  ,ELT(IPLOT)) 
                 ILOC =IFAPAR(IFA+3,ELT(IPLOT))   
                 RECVCHAR(IPLOT)%NEPID=IPROC  ! ANOTHER ONE AS IPID 
                 RECVCHAR(IPLOT)%INE=ILOC  
                 EXIT ! LOOP ON NSP 
 
               ENDIF  
! 
!----------------------------------------------------------------------- 
! FURTHER ON, THE SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
!----------------------------------------------------------------------- 
! 
               DXP = DX(IPLOT) 
               DYP = DY(IPLOT) 
               I1  = IKLE(ELT(IPLOT),IFA) 
               I2  = IKLE(ELT(IPLOT),ISUI(IFA)) 
               DX1 = X(I2) - X(I1) 
               DY1 = Y(I2) - Y(I1) 
! 
               IF(IEL.EQ.-1) THEN 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE SOLIDE 
!  ON PROJETTE LE RELIQUAT SUR LA FRONTIERE PUIS ON SE RELOCALISE 
!----------------------------------------------------------------------- 
! 
                 A1 = (DXP*DX1 + DYP*DY1) / (DX1**2 + DY1**2) 
                 DX(IPLOT) = A1 * DX1 
                 DY(IPLOT) = A1 * DY1 
! 
                 A1 = ((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2) 
                 SHP(      IFA ,IPLOT) = 1.D0 - A1 
                 SHP( ISUI(IFA),IPLOT) = A1 
                 SHP(ISUI2(IFA),IPLOT) = 0.D0 
                 XPLOT(IPLOT) = X(I1) + A1 * DX1 
                 YPLOT(IPLOT) = Y(I1) + A1 * DY1 
! 
                 GOTO 50 
! 
               ENDIF 
! 
!----------------------------------------------------------------------- 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE FRONTIERE LIQUIDE 
!  ON ARRETE LA REMONTEE DES CARACTERISTIQUE (SIGNE DE ELT) 
! 
!     OU QUE 
! 
!  LA, ON SAIT QUE LA FACE DE SORTIE EST UNE INTERFACE DE SOUS-DOMAINES 
!  POINT D'INTERFACE QUI SERA TRAITE PAR LE SOUS-DOMAINE VOISIN 
! 
!----------------------------------------------------------------------- 
! 
               DENOM = DXP*DY1-DYP*DX1 
               IF(DENOM.NE.0.D0) THEN 
                 A1  = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM 
               ELSE 
                 A1  = 0.D0 
               ENDIF 
               A1 = MAX(MIN(A1,1.D0),0.D0) 
               SHP(      IFA ,IPLOT) = 1.D0 - A1 
               SHP( ISUI(IFA),IPLOT) = A1 
               SHP(ISUI2(IFA),IPLOT) = 0.D0 
               XPLOT(IPLOT) = X(I1) + A1 * DX1 
               YPLOT(IPLOT) = Y(I1) + A1 * DY1 
               ELT(IPLOT) = - SENS * ELT(IPLOT) 
               NSP(IPLOT) = ISP 
               EXIT 
! 
            ENDIF 
! 
         ENDIF 
! 
      ENDDO 
      ENDDO 
! 
!----------------------------------------------------------------------- 
!    
      RETURN 
      END SUBROUTINE ADD_CHAR11 
 
 
  !--------------------------------------------------------------------- 
  !   <<<<<<<<<<<<<<<<<< CHARACTERISTICS: PUBLIC >>>>>>>>>>>>>>>>>> 
  !--------------------------------------------------------------------- 
 
!----------------------------------------------------------------------- 
! BIEF'S CARACT MODIFIED FOR PARALLEL STREAMLINE TRACKING 
! IN THEORY THIS IS THE ONLY PUBLIC SUBROUTINE IN THIS MODULE 
! NOTE: IN 3D, NPOIN->NPOIN3, IMPORTANT!  
!----------------------------------------------------------------------- 
! JAJ MODIFIED WED JUL 16 18:24:08 CEST 2008 
! 
!                       ****************** 
                        SUBROUTINE SCARACT 
!                       ****************** 
! 
     & ( U , UTILD , UCONV , VCONV , WCONV , X , Y , ZSTAR , 
     &   XCONV , YCONV , ZCONV , DX , DY , DZ , Z , SHP , SHZ , SURDET , 
     &   DT , IKLE , IFABOR , ELT , ETA , ITRAV1, ITRAV2, IELM , 
     &   IELMU , NELEM , NELMAX , NOMB , NPOIN , NPOIN2 , NDP , NPLAN ,  
     &   LV , MSK , MASKEL , MESH , FAC , TEST , STEST , INITLOC,  
     &   QUAD,NPLOT,DOIT,DOCOM) 
! 
!*********************************************************************** 
! BIEF VERSION 6.1           24/04/97    J-M JANIN (LNH) 30 87 72 84 
! 
!*********************************************************************** 
! 
!     FONCTION: 
! 
!     RESOUT LES EQUATIONS DE CONVECTION PAR LA METHODE DES 
!     CARACTERISTIQUES, POUR UN ENSEMBLE DE FONCTIONS. 
! 
!     ATTENTION LA COMPATIBILITE AVEC 3.0 N'EST PLUS ASSUREE A CAUSE 
!     DE L'APPEL A PARCOM 
! 
!     EN REVANCHE U ET UTILD PEUVENT MAINTENANT ETRE DES VECTEURS, 
!     DANS CE CAS NOMB SERA CONSIDERE EGAL A 1 
! 
!----------------------------------------------------------------------- 
!                             ARGUMENTS 
! .________________.____.______________________________________________. 
! |      NOM       |MODE|                   ROLE                       | 
! |________________|____|______________________________________________| 
! |   U            | -->| VARIABLES A L'ETAPE N .                      | 
! |   UTILD        |<-- | VARIABLES APRES LA CONVECTION .              | 
! |   UCONV,VCONV..| -->| COMPOSANTES DES VITESSES DU CONVECTEUR.      | 
! |   X,Y,ZSTAR    | -->| COORDONNEES DU MAILLAGE .                    | 
! |   XCONV,YCONV..| -- | COORDONNEES AU PIED DES CARACTERISTIQUES.    | 
! |   DX,DY,DZ     | -- | STOCKAGE DES SOUS-PAS .                      | 
! |   Z            | -->| COTE DANS LE MAILLAGE REEL (POUR TEL3D) .    | 
! |   SHP          | -- | COORDONNEES BARYCENTRIQUES 2D AU PIED DES    | 
! |                |    | COURBES CARACTERISTIQUES.                    | 
! |   SHZ          | -- | COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED | 
! |                |    | DES COURBES CARACTERISTIQUES (POUR TEL3D)    | 
! |   SURDET       | -->| 1/DETERMINANT POUR LES ELEMENTS 2D.          | 
! |   DT           | -->| PAS DE TEMPS                                 | 
! |   IKLE         | -->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D.  | 
! |   IFABOR       | -->| NUMEROS DES ELEMENTS VOISINS (ATTENTION, POUR| 
! |                |    | TEL3D, IFABOR EST LE TABLEAU IBOR DE MITRID).| 
! |   ELT          | -- | NUMEROS DES ELEMENTS 2D AU PIED DES COURBES  | 
! |                |    | CARACTERISTIQUES.                            | 
! |   ETA          | -- | NUMEROS DES ETAGES AU PIED DES COURBES       | 
! |                |    | CARACTERISTIQUES (POUR TEL3D).               | 
! |   ITRAV1       | -- | TABLEAU DE TRAVAIL ENTIER.                   | 
! |   ITRAV2       | -- | TABLEAU DE TRAVAIL ENTIER.                   | 
! |   IELM         | -->| TYPE D'ELEMENT : 11 : TRIANGLE P1            | 
! |                |    |                  21 : QUADRANGLE P1          | 
! |                |    |                  41 : PRISME DE TEL3D        | 
! |   NELEM        | -->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D. | 
! |   NELMAX       | -->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D| 
! |   NOMB         | -->| NOMBRE DE VARIABLES A CONVECTER.             | 
! |   NPOIN        | -->| NOMBRE TOTAL DE POINTS DU MAILLAGE.          | 
! |   NPOIN2       | -->| NOMBRE DE POINTS DU MAILLAGE 2D (POUR TEL3D).| 
! |   NDP          | -->| NOMBRE DE POINTS PAR ELEMENT 2D.             | 
! |   NPLAN        | -->| NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).       | 
! |   LV           | -->| LONGUEUR DU VECTEUR POUR LA VECTORISATION.   | 
! |   MSK          | -->| SI OUI, PRESENCE D'ELEMENTS MASQUES.         | 
! |   MASKEL       | -->| TABLEAU DE MASQUAGE DES ELEMENTS             | 
! |                |    |  =1. : NORMAL   =0. : ELEMENT MASQUE.        | 
! |   DOIT         | -->| SWITCH ON/OFF JMH METHOD 
! |   DOCOM        | -->| IF YES, CALL PARCOM ON RESULTS
! |________________|____|______________________________________________| 
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
! 
!----------------------------------------------------------------------- 
! 
! APPELE PAR : TELMAC , MITRID 
! 
! SOUS-PROGRAMMES APPELES : CHAR11 , CHAR41 , 
!                           GTSH11 , GTSH41 
! 
!*********************************************************************** 
! 
      USE BIEF 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER, INTENT(IN)             :: NELEM,NELMAX,NPOIN,NPOIN2,NPLOT 
      INTEGER, INTENT(IN)             :: NOMB,NDP,NPLAN,IELM,IELMU,LV 
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U 
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: UTILD 
      DOUBLE PRECISION, INTENT(INOUT) :: XCONV(*),YCONV(*) 
      DOUBLE PRECISION, INTENT(IN)    :: UCONV(NPOIN) 
      DOUBLE PRECISION, INTENT(IN)    :: VCONV(NPOIN) 
      DOUBLE PRECISION, INTENT(IN)    :: WCONV(NPOIN) 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPLOT,NPLAN),ZSTAR(NPLAN) 
      DOUBLE PRECISION, INTENT(INOUT) :: ZCONV(NPOIN2,NPLAN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPOIN),DY(NPLOT),DZ(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(NDP,NPLOT),SHZ(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX) 
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM) 
      DOUBLE PRECISION, INTENT(IN)    :: DT 
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,NDP) 
      INTEGER, INTENT(IN)             :: IFABOR(NELMAX,*) 
      INTEGER, INTENT(INOUT)          :: ELT(NPLOT),ETA(NPLOT) 
      INTEGER, INTENT(INOUT)          :: ITRAV1(NPOIN),ITRAV2(NPOIN) 
      LOGICAL, INTENT(IN)             :: MSK,INITLOC,DOIT,DOCOM
      DOUBLE PRECISION, INTENT(INOUT) :: TEST(NPOIN2) 
      DOUBLE PRECISION, INTENT(IN)    :: FAC(NPOIN) 
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH 
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: STEST 
      LOGICAL, INTENT(IN) ::  QUAD	 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER NRK,I,II,IPOIN,ISTOP 
      DOUBLE PRECISION C 
! 
!----------------------------------------------------------------------- 
! 
      LOGICAL :: INIT=.TRUE. 
!
! PRE-NUMBER OF INITIALLY COLLECTED LOST CHARACTERISTICS 
! WHICH IS DIMINISHED LATER BY APPLYING JMH'S PROCEDURE  
! (THE TRACEBACKS INITIALLY COMPLETED IN OTHER PARTITIONS ARE NOT TREATED) 
! LATER ON, IT COUNTS THE IMPLANTED TRACEBACKS LOCALISED IN MY PARTITION 
!
      INTEGER NCHARA,NLOSTCHAR,NARRV,IGEN,NSEND,NLOSTAGAIN 
      INTEGER NPOINT,NPOINT2  
      INTEGER  P_IMAX, P_ISUM 
      EXTERNAL P_IMAX, P_ISUM 
      INTEGER LAST_NOMB 
!     STATIC DIMENSION FOR HEAPCHAR, SENDCHAR, RECVCHAR (SORRY, STATIC)  
      INTEGER NCHDIM 
! 
      SAVE 
! 
!----------------------------------------------------------------------- 
! 
      
      IF(INIT) THEN ! CHECK THINGS ONCE AND FOREVER  
! 
!       SEE IN LIBRARY PARALLEL OR PARAVOID (AND INCLUDE 'mpif.h' OR NOT) 
! 
        CALL GET_MPI_PARAMETERS(MPI_INTEGER,MPI_REAL8,MPI_UB, 
     *                          MPI_COMM_WORLD,MPI_SUCCESS) 
! 
        INIT=.FALSE. 
        LAST_NOMB=NOMB 
! 
        IF(TRACE) WRITE(LU,*)  
     &   '===> BEWARE: APPLYING SCARACT: CARACT IN THE PARALLEL VERSION' 
! 
        IF(IELM/=11.AND.IELM/=41) THEN 
          WRITE(LU,*) ' @STREAMLINE::SCARACT:: ', 
     &      'PARALLEL CHARACTERISTICS NOT IMPLEMENTED FOR ', 
     &      'IELM: ',IELM 
          CALL PLANTE(1) 
          STOP  
        ENDIF  
! 
        WRITE(LU,*) 
     &      'STREAMLINE: USING PARALLEL VERSION OF CHARACTERISTICS 6.1' 
! 
!       NOW THE VERY NECESSARY INITIALISATION PROCEDURES  
! 
        IF(NCSIZE>1) CALL ORGANISE_CHARS(NPOIN,NOMB,NCHDIM) 
! 
      ENDIF 
! 
!     CASE OF A CALL FROM DIFFERENT PROGRAMMES WITH DIFFERENT NOMB 
!     JAJ + JMH 26/08/2008 
! 
      IF(NCSIZE.GT.1) THEN 
        IF(NOMB.NE.LAST_NOMB) THEN 
          ! DESTROY THE CHARACTERISTICS TYPE FOR COMM. 
          CALL DEORG_CHARAC_TYPE()  
          ! SET DATA STRUCTURES ACCORDINGLY  
          CALL ORGANISE_CHARS(NPOIN,NOMB,NCHDIM)  
        ENDIF 
!    
!       INITIALISING NCHARA (NUMBER OF LOST CHARACTERISTICS) 
        NCHARA=0 
! 
      ENDIF 
!  
!*********************************************************************** 
! NOMBRE DE SOUS-PAS DE RUNGE-KUTTA PAR ELEMENT TRAVERSE 
! 
      NRK = 3 
!  
!----------------------------------------------------------------------- 
! 
      IF(IELM==11) THEN  
C 
C----------------------------------------------------------------------- 
C 
        IF(.NOT.QUAD) THEN 
! 
!    TRIANGLES P1 
!    ============ 
! 
!      REMPLISSAGE DES SHP ET DES ELT OPTIMISE 
! 
         NPOINT  = NPOIN 
         NPOINT2 = NPOIN 
         NPOINT  = NPLOT 
! 
!      APPEL DU SOUS-PROGRAMME DE REMONTEE DES COURBES CARACTERISTIQUES 
!      
         TEST=1.D0 
 
         CALL SCHAR11( UCONV , VCONV , DT , NRK , X , Y , IKLE, IFABOR, 
     &                 XCONV,YCONV,DX,DY,SHP,ELT,ITRAV1, 
     &                 NPOINT, NPOINT2, NELEM , NELMAX , SURDET , -1 , 
     &                 TEST, MESH%IFAPAR%I, MESH,NCHDIM,NCHARA) 
! 
        ELSEIF(QUAD) THEN 
C              
!          CALL CHGDIS(XCONV,IELM,13,MESH) 
!          CALL CHGDIS(YCONV,IELM,13,MESH)  
C 
C         TRIANGLES P2 POUR L'UNE DES VARIABLES CONVECTEE 
C         =============================================== 
C 
C         REMPLISSAGE DES SHP ET DES ELT OPTIMISE 
C 
          NPOINT  = NPOIN+MESH%NSEG  
          NPOINT2 = NPOIN+MESH%NSEG  
C 
C         CAS NON PREVU D'UN TRACEUR QUADRATIQUE ET D'UNE VITESSE LINEAIRE 
C       
          IF(IELMU.NE.13)THEN 
            IF(LNG.EQ.1) WRITE(LU,21)  
            IF(LNG.EQ.2) WRITE(LU,22)  
            CALL PLANTE(1) 
            STOP 
          ENDIF 
 
C 
          IF(NCSIZE>1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOINT) 
          CALL SCHAR11( UCONV   , VCONV , DT    , NRK , X , Y , 
     *                  IKLE    , IFABOR  , 
     *                  XCONV , YCONV , DX , DY ,SHP,ELT ,ITRAV1, 
     *                  NPOINT  , NPOINT2 , NELEM , 
     *                  NELMAX  , SURDET  , -1    , TEST, 
     *                  MESH%IFAPAR%I, MESH ,NCHDIM,NCHARA) 
        ENDIF 
! 
!----------------------------------------------------------------------- 
! 
      ELSEIF (IELM==41) THEN 
! 
!    PRISMES DE TELEMAC-3D 
!    ===================== 
! 
         NPOINT  = NPOIN 
         NPOINT2 = NPOIN2 
 
! 
!      APPEL DU SOUS-PROGRAMME DE REMONTEE DES COURBES CARATERISTIQUES 
! 
         IF(NCSIZE>1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOINT) 
         CALL SCHAR41(UCONV,VCONV,WCONV,DT,NRK,X,Y,ZSTAR, 
     &                Z,IKLE,IFABOR,XCONV,YCONV,ZCONV,DX, 
     &                DY,DZ,SHP,SHZ,ELT,ETA,ITRAV1,NPOINT, 
     &                NPOINT2,NELEM,NPLAN,SURDET,-1, 
     &                TEST,MESH%IFAPAR%I,NCHDIM,NCHARA) 
! 
!----------------------------------------------------------------------- 
! 
      ELSE ! ERROR 
! 
        IF(LNG.EQ.1) WRITE(LU,11) IELM 
        IF(LNG.EQ.2) WRITE(LU,12) IELM 
        CALL PLANTE(1) 
        STOP 
! 
      ENDIF 
! 
!----------------------------------------------------------------------- 
!JAJ //// THIS PART OF THE ALGORITHM REMAINS, IT IS VERY PRACTICAL  
!    TO DETECT THESE LOST TRACEBACKS WITH THEIR HEADS  
!    AT INTERFACE POINTS, WHICH ARE COMPLETED IN THE NEIGHBOUR  
!    PARTITION / NOTE, THE ALGORITHM WOULD WORK WITHOUT IT AS WELL 
!    -> PERFORMANCE (DIS)ADVANTAGES UNKNOWN 
!    -> SEE: WIPE_HEAPED_CHAR 
!    -> FURTHER REFERRED TO AS "JMH'S METHOD" 
! 
!  PROVISOIRE 
!  TEST = NOMBRE DE SOUS-DOMAINES QUI ONT TRAITE UN POINT 
! 
!  AJUSTEMENT DES SHP EN FONCTION DE CE QUE L'ON A TROUVE 
!  DANS LES AUTRES SOUS-DOMAINES (DONNE PAR TEST) 
! 
      IF(NCSIZE.GT.1) THEN 
! 
        DO IPOIN = 1,NPOINT 
!         CONTRIBUTION OF THIS SUB-DOMAIN CANCELLED 
!         THE RESULT WITH MAXIMUM ABSOLUTE VALUE WILL BE KEPT 
!         HENCE IT WILL NOT BE THIS ONE 
          IF(TEST(IPOIN).LT.0.5D0) THEN 
            SHP(1,IPOIN) = 0.D0 
            SHP(2,IPOIN) = 0.D0 
            SHP(3,IPOIN) = 0.D0 
          ENDIF 
        ENDDO 
! 
!       ADDING TEST OF NEIGHBOURING SUB-DOMAINS 
!       WILL BE USED ALSO BY WIPE_CHAR... 
!       THIS IS NOT RELEVANT IF THE NPLOT POINTS ARE NOT LINKED TO MESH
!       OR WITH A DIFFERENT NUMBERING (WAITING FOR VERSION 6.2...)
        IF(DOCOM) CALL PARCOM(STEST,2,MESH) 
! 
        IF(TRACE) THEN  
          DO IPOIN = 1,NPOINT 
            IF(TEST(IPOIN).LT.0.5D0) THEN 
!             HERE CALL ADD_CHAR.. WILL BE USEFUL   
              IF(LNG==1) WRITE(LU,13) IPOIN  
              IF(LNG==2) WRITE(LU,14) IPOIN 
            ENDIF 
          ENDDO 
        ENDIF  
! 
      ENDIF 
! 
!----------------------------------------------------------------------- 
! 
!     INTERPOLATION AU PIED DES CARACTERISTIQUES SI DEMANDE 
! 
      IF(NOMB.GT.0) THEN 
! 
        IF(U%TYPE==2.AND.UTILD%TYPE==2) THEN 
! 
!         U ET UTILD VECTEURS (NOMB VAUT ALORS 1) 
!         
          IF(U%ELM.EQ.13) THEN 
!           INTERPOLATION DES VITESSES POUR UNE VARIABLE QUADRATIQUE   
            CALL INTERP(U%R,UTILD%R,SHP,NDP,SHZ,ETA,ELT, 
     *                  U%DIM1,U%DIM1,NPLAN,13,IKLE,NELMAX)          
          ELSE  
!           INTERPOLATION DES VITESSES DANS LES AUTRES CAS 
            CALL INTERP(U%R,UTILD%R,SHP,NDP,SHZ,ETA,ELT, 
     *                  NPOINT,NPOINT2,NPLAN,IELM,IKLE,NELMAX) 
          ENDIF 
          IF(NCSIZE.GT.1.AND.DOCOM) THEN 
!           IF A POINT HAS BEEN TREATED SEVERAL TIMES 
!           THE RESULT WITH MAXIMUM ABSOLUTE VALUE IS TAKEN 
            CALL PARCOM(UTILD,1,MESH)  
          ENDIF              
! 
        ELSEIF(U%TYPE==4.AND.UTILD%TYPE==4) THEN 
! 
!     U ET UTILD BLOCS DE VECTEURS 
! 
          IF(U%N.LT.NOMB.OR.UTILD%N.LT.NOMB) THEN 
            IF(LNG.EQ.1) WRITE(LU,15) U%N,UTILD%N 
            IF(LNG.EQ.2) WRITE(LU,16) U%N,UTILD%N 
            CALL PLANTE(1) 
            STOP 
          ENDIF 
! 
          DO I=1,NOMB 
!          
          IF(U%ADR(I)%P%ELM.EQ.13) THEN 
!           INTERPOLATION DES VITESSES POUR UNE VARIABLE QUADRATIQUE  
            CALL INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ, 
     *                  ETA,ELT,U%ADR(I)%P%DIM1,U%ADR(I)%P%DIM1, 
     *                  NPLAN,13,IKLE,NELMAX)          
          ELSE  
!           INTERPOLATION DES VITESSES DANS LES AUTRES CAS 
            CALL INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ, 
     *                  ETA,ELT,NPOINT,NPOINT2,NPLAN, 
     *                  IELM,IKLE,NELMAX) 
          ENDIF 
          IF(NCSIZE.GT.1.AND.DOCOM) THEN 
!           IF A POINT HAS BEEN TREATED SEVERAL TIMES 
!           THE RESULT WITH MAXIMUM ABSOLUTE VALUE IS TAKEN 
            CALL PARCOM(UTILD%ADR(I)%P,1,MESH) 
          ENDIF 
! 
          ENDDO 
! 
        ELSE 
! 
          IF(LNG.EQ.1) WRITE(LU,17) U%TYPE,UTILD%TYPE 
          IF(LNG.EQ.2) WRITE(LU,18) U%TYPE,UTILD%TYPE 
          CALL PLANTE(1) 
          STOP 
! 
        ENDIF 
! 
      ENDIF 
! 
!----------------------------------------------------------------------- 
! THE TREATMENT OF LOST TRACEBACKS ALONG THE CHARACTERISTIC CURVES 
!----------------------------------------------------------------------- 
! THIS IS THE MAIN PROCEDURE LOOP OVER THE GENERATIONS OF LOST  
! AND LOST-AGAIN TRACEBACKS REPEATED UNTIL ALL LOST TRACEBACKS  
! ARE COMPLETED (I.E. THEIR FEET LOCALISED) SO THAT  
! THEY CAN BE SENT BACK TO THEIR ORIGIN PARTITIONS AND INTRODUCED  
! AT THEIR HEAD POSITIONS IN THE APPROPRIATE FIELDS 
!----------------------------------------------------------------------- 
! 
      IF(NCSIZE.GT.1) THEN  
 
        ! TAKE ACCOUNT TO THE JMH METHOD -> WIPE OUT FROM THE LIST   
        ! THE TRACEBACKS COMPLETED CORRECTLY BY THE NEIGHBOURS  
        ! THE NUMBER GETS REDUCED FROM NCHARA TO NLOSTCHAR 
       
        CALL WIPE_HEAPED_CHAR(TEST,NPOINT,DOIT,NSEND,NLOSTCHAR,NCHDIM, 
     &                        NCHARA)  
 
        IF(TRACE) CALL PRINT_HEAPCHAR  
     &   (' ===> HEAPCHAR COLLECTED FOR THE TREATMENT ::',NOMB,NCHARA) ! DEBUG 
         
        IF(P_IMAX(NLOSTCHAR)>0) THEN ! THERE ARE -REALLY- LOST TRACEBACKS SOMEWHERE 
 
          ! PREPARE INITIAL SENDING OF COLLECTED LOST TRACEBACKS 
          CALL PREP_INITIAL_SEND(NSEND,NLOSTCHAR,NCHARA)  
 
          IF (TRACE) CALL PRINT_SENDCHAR  
     &     (' ===> SENDCHAR PREPARED BY TESTING ::',NOMB,NSEND) ! DEBUG 
 
          IGEN=0 ! NUMBER OF GENERATIONS (I.E. INTERFACE CROSSINGS ON THE WAY) 
          DO 
            IGEN=IGEN+1   
 
            ! GET THE ARRIVING TRACEBACKS VIA ALL-TO-ALL COMMUNICATION 
            CALL GLOB_CHAR_COMM()    
 
            ! COMPUTE THE NUMBER OF ARRIVED ONES (NECESSARY)  
            NARRV = SUM(RECVCOUNTS)  
            ISTOP=0 
            IF(NARRV.GT.NCHDIM) THEN 
              ISTOP=1 
              WRITE(LU,*) 'NARRV=',NARRV,' NCHDIM=',NCHDIM 
            ENDIF 
            ISTOP=P_ISUM(ISTOP) 
            IF(ISTOP.GT.0) THEN 
              WRITE (LU,*) 'TOO MANY LOST TRACEBACKS IN ',ISTOP, 
     *                     ' PROCESSORS' 
              CALL PLANTE(1) 
              STOP 
            ENDIF 
             
            IF (TRACE) CALL PRINT_RECVCHAR  
     &             (' ===> RECVCHAR RECEIVED IS ::',NOMB,NARRV) ! DEBUG 
 
            ! CALL TRACKING WITH ARRIVED TRACEBACKS ... 
            ! WE RE-USE THE PREVIOUSLY USED FIELDS FOR ADDITIONAL TRACEBACKS 
            ! COMPUTING SHAPE FUNCTIONS BY THE WAY / 2D OR 3D  
 
            IF(IELM.EQ.11) CALL ADD_CHAR11 
     &             (UCONV, VCONV, DT, NRK, X, Y, IKLE, IFABOR, 
     &              XCONV,YCONV,DX,DY,SHP,ELT,ITRAV1,ITRAV2, 
     &              NARRV, NPOIN, NELEM, NELMAX, SURDET, -1, TEST,  
     &              MESH%IFAPAR%I, NOMB,NARRV)  
 
            IF(IELM.EQ.41) CALL ADD_CHAR41 
     &             (UCONV, VCONV, WCONV, DT, NRK, X, Y, ZSTAR, Z, 
     &              IKLE, IFABOR, XCONV, YCONV, ZCONV, DX, DY, DZ, 
     &              SHP, SHZ, ELT, ETA, ITRAV1, ITRAV2, NARRV, 
     &              NPOIN2, NELEM, NPLAN, SURDET, -1, 
     &              TEST, MESH%IFAPAR%I, NOMB,NARRV) 
 
            IF (TRACE) CALL PRINT_RECVCHAR  
     &          (' ===> RECVCHAR AFTER ADDITIONAL TRACKING ::', 
     &           NOMB,NARRV) ! DEBUG 
 
            ! INTERPOLATE THE -LOCATED- TRACEBACKS -> SOME OF RANGE 1:NARRV 
            ! APPLYING THE JUST VALID ELT, SHP, ETC. JUST SAVED FROM  
            ! ADD_CHAR11 OR ADD_CHAR41 COMPUTED FOR JUST THIS RANGE 
            ! 
            ! NOTICE: THIS IS THE ONLY REASON TO DO IT NOW -   
            ! WHEN THE ELT,SHP COULD BE SAVED, THIS COULD HAVE BEEN DONE  
            ! AFTER THE GENERATION LOOP WORKING ON HEAPCHAR -  
            ! WHICH WOULD HAVE BEEN MORE PERFORMANT, BECAUSE IN RECVCHAR  
            ! WE HAVE TO CHECK FOR EACH TRACEBACK IF IT IS LOCATED OR NOT   
            ! 
            ! NOTICE: UTILD==RECVCHAR%BASKET(:)  
 
          IF(NARRV.GT.0) THEN 
            IF(U%TYPE==2) THEN 
              IF(IELM.EQ.11) THEN 
                CALL INTERP_RECVCHAR_11 
     &            (U%R,1,IKLE,ELT,SHP,NELMAX,U%DIM1,NARRV,U%ELM) 
              ELSEIF(IELM.EQ.41) THEN 
                CALL INTERP_RECVCHAR_41 
     &         (U%R,1,IKLE,ELT,ETA,SHP,SHZ,NELMAX,NPOIN2,NPLAN,NARRV) 
              ELSE 
                WRITE(LU,*) 'WRONG IELM IN SCARACT:',IELM 
                CALL PLANTE(1) 
                STOP 
              ENDIF 
            ELSEIF(U%TYPE==4) THEN 
              IF(IELM.EQ.11) THEN  
                DO I=1,NOMB 
                  CALL INTERP_RECVCHAR_11 
     &          (U%ADR(I)%P%R,I,IKLE,ELT,SHP,NELMAX, 
     &           U%ADR(I)%P%DIM1,NARRV,U%ADR(I)%P%ELM) 
                ENDDO  
              ELSEIF(IELM.EQ.41) THEN  
                DO I=1,NOMB  
                  CALL INTERP_RECVCHAR_41 
     &              (U%ADR(I)%P%R,I,IKLE,ELT,ETA,SHP,SHZ, 
     &                 NELMAX,NPOIN2,NPLAN,NARRV) 
                ENDDO 
              ELSE  
                WRITE(LU,*) 'WRONG IELM IN SCARACT:',IELM 
                CALL PLANTE(1) 
                STOP 
              ENDIF 
            ENDIF 
          ENDIF 
!             
            IF(TRACE) CALL PRINT_RECVCHAR  
     &                 (' ===> RECVCHAR AFTER INTERPOLATE ::', 
     &                  NOMB,NARRV) ! DEBUG      
            CALL HEAP_FOUND(NLOSTAGAIN,NARRV,NCHARA)  
            IF (TRACE) CALL PRINT_HEAPCHAR  
     &                 (' ===> HEAPCHAR COLLECTED ::',NOMB,NCHARA) ! DEBUG 
            IF(TRACE) WRITE (LU,'(A,I2,3(A,I7))')  
     &                  ' #GENERATION: ',IGEN,  
     &                 ', #MY-LOST: ',NLOSTCHAR, 
     &                 ', #ARRIVED: ',NARRV, 
     &                 ', #LOST-AGAIN: ',NLOSTAGAIN  
            IF(TRACE) WRITE (LU,'(A,1X,I9)')  
     &        ' #LOST-AGAIN SUM EVERYWHERE: ', P_ISUM(NLOSTAGAIN) 
 
            IF(P_ISUM(NLOSTAGAIN)>0) THEN ! THERE ARE LOST-AGAINS SOMEWHERE   
              CALL PREP_LOST_AGAIN(NOMB,NSEND,NLOSTAGAIN,NARRV) ! PREPARE SENDING LOST-AGAINS  
            ELSE  
              EXIT ! NO LOST-AGAIN TRACEBACKS ANYWHERE, LEAVE THESE ITERATIONS 
            ENDIF 
! 
            IF(IGEN>99) THEN  ! A SECURITY FUSE / TO BE TESTED IN EXTREME TEST CASES  
              WRITE(LU,*) '@STREAMLINE::SCARACT: ', 
     &          'THE NUMBER OF TRACEBACK INTERFACE CROSSINGS IGEN > 99'  
              CALL PLANTE(1)  
              STOP 
            ENDIF 
 
          ENDDO ! ON TRACEBACKS GENERATIONS (IGEN) / ALL COLLECTED  
 
          ! PREPARE SENDING OF THE HEAPED LOCALISED TRACEBACKS 
          CALL PREP_SENDBACK(NOMB,NCHARA) 
          ! THE FINAL SEND/RECV OF THE LOST TRACEBACKS BACK VIA ALLTOALL COMM. 
          CALL GLOB_CHAR_COMM() 
          NARRV = SUM(RECVCOUNTS) ! FROM RECVCOUNTS / THE FINALLY ARRIVED ONES  
 
          IF(TRACE) CALL PRINT_RECVCHAR  
     &               (' ===> RECVCHAR FINALLY RECEIVED IS ::', 
     &                NOMB,NARRV) ! DEBUG 
 
          IF(NARRV/=NLOSTCHAR) THEN ! THE MOST SERIOUS PROBLEM WE CAN HAVE  
            WRITE (LU,*) ' @STREAMLINE::SCARACT ::',  
     &                   ' THE NUMBER OF INITALLY LOST TRACEBACKS', 
     &                   ' /= THE NUMBER OF FINALLY ARRIVED ONES ' 
            WRITE (LU,*) '             NLOSTCHAR, NARRV: ',  
     &                                 NLOSTCHAR, NARRV 
            CALL PLANTE(1) 
            STOP  
          ENDIF   
 
          ! INTRODUCE THE VALUES FROM THE RECEIVED TRACEBACK BASKETS  
          ! NOTE THAT IN 3D, NPOIN->NPOIN3 / WE DO NOT DISTINGUISH 11 AND 41 
 
          IF (NARRV>0) THEN  
            IF(UTILD%TYPE==2) THEN 
              CALL INTRODUCE_RECVCHAR(UTILD%R,1,UTILD%DIM1,NOMB,NARRV)  
            ELSEIF(UTILD%TYPE==4) THEN 
              DO I=1,NOMB 
                CALL INTRODUCE_RECVCHAR(UTILD%ADR(I)%P%R,I, 
     &                                  UTILD%ADR(I)%P%DIM1, 
     &                                  NOMB,NARRV)  
              ENDDO 
            ELSE  
              WRITE (LU,*)  
     &          ' @STREAMLINE::SCARACT :: UTILD%TYPE: ',UTILD%TYPE 
              CALL PLANTE(1) 
              STOP  
            ENDIF  
          ENDIF  
        ENDIF 
        CALL RE_INITIALISE_CHARS(NSEND,NLOSTCHAR,NLOSTAGAIN,NARRV) ! DEALLOCATING 
! 
!       SOME POINTS ARE TREATED SEVERAL TIMES BY DIFFERENT PROCESSORS 
!       AND GET SLIGHTLY DIFFERENT VALUES. THE LARGEST ONE IS TAKEN 
! 
        IF(DOCOM) THEN
        IF(NOMB.GT.0) THEN 
          IF(UTILD%TYPE==2) THEN 
            CALL PARCOM(UTILD,1,MESH)               
          ELSEIF(UTILD%TYPE==4) THEN 
          DO I=1,NOMB          
            CALL PARCOM(UTILD%ADR(I)%P,1,MESH) 
          ENDDO 
          ENDIF 
        ENDIF
        ENDIF 
! 
      ENDIF  
! 
      LAST_NOMB=NOMB 
! 
!----------------------------------------------------------------------- 
! 
11    FORMAT(1X,'STREAMLINE::SCARACT:: TYPE D''ELEMENT INCONNU : ',I6) 
12    FORMAT(1X,'STREAMLINE::SCARACT:: UNKNOWN TYPE OF ELEMENT : ',I6) 
! 
13    FORMAT(1X,'STREAMLINE::SCARACT::', 
     &          ' (PARALLELE) REMONTEE INCOMPLETE POUR : ',I6) 
14    FORMAT(1X,'STREAMLINE::SCARACT::', 
     &          ' (PARALLEL) INCOMPLETE PATH LINE FOR : ',I6) 
! 
15    FORMAT(1X,'STREAMLINE::SCARACT::', 
     &          ' MAUVAIS BLOC DES VARIABLES : ',2I6) 
16    FORMAT(1X,'STREAMLINE::SCARACT::', 
     &          ' WRONG BLOCK OF VARIABLES : ',2I6) 
! 
17    FORMAT(1X,'STREAMLINE::SCARACT:: TYPE D''OBJET INCONNU : ',2I6) 
18    FORMAT(1X,'STREAMLINE::SCARACT:: UNKNOWN TYPE OF OBJECT : ',2I6) 
19    FORMAT(1X,'SCARACT : PARALLELISME NON PREVU EN QUADRATIQUE') 
20    FORMAT(1X,'SCARACT: PARALLELISM NOT TREATED WITH QUADRATIC') 
21    FORMAT(1X,'SCARACT : VITESSES LINEAIRES ET TRACEUR QUADRATIQUE') 
22    FORMAT(1X,'SCARACT: LINEAR VELOCITY AND QUADRATIC TRACER') 
! 
!----------------------------------------------------------------------- 
! 
      RETURN  
      END SUBROUTINE SCARACT 
! 
      END MODULE STREAMLINE  
