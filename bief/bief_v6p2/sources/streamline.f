!                    *****************
                     MODULE STREAMLINE
!                    *****************
!
!
!***********************************************************************
! BIEF 6.3
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
!history  J-M HERVOUET (LNHE)
!+        31/07/2012
!+        V6P2
!+   Use of new array ELTCAR.
!+   Dimensions in SCARACT reviewed (previous confusion between
!+   NPOIN and NPLOT). INTENT(OUT) changed into INTENT(INOUT) in
!+   subroutine organise_char. NPOINT=NPLOT replaces NPOINT=NPOIN
!+   before the call to SCHAR41. ADD_CHAR11 and ADD_CHAR41 deleted.
!+   SCHAR11 and SCHAR41 simplified. Arguments removed in SCARACT.
!+   SCHAR12 and SCHAR13 added. DX,DY,DZ added to CHARAC_TYPE.
!+   More data saved when touching a solid boundary: XPLOT, YPLOT, DX, DY
!+   All this ensures strict equality of scalar and parallel runs !!!!
!+   However in 3D the vertical velocity is computed in Telemac-3D and
!+   has truncation errors, this will trigger differences.
!+   Now the sub-domain at the foot of the characteristic is returned
!
!history  J-M HERVOUET (LNHE)
!+        08/01/2013
!+        V6P3
!+   Advection subroutines from Tomawac re-implemented here
!+   See SCHAR41_PER and SCHAR41_PER_4D
!+   A posteriori interpolation now possible in all cases.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     USE INTERFACE_PARALLEL
      IMPLICIT NONE   
      PRIVATE
! 
!     CALLED BY CHARAC (BIEF). CHARAC CALLED BY TELEMAC-2D AND 3D AND SISYPHE
!     CALLED BY THOMPS (TELEMAC-2D)
!     CALLED BY DERIVE (BIEF, USED BY TELEMAC-2D) 
!     CALLED BY DERLAG (BIEF, USED BY TELEMAC-2D) 
!     CALLED BY DERI3D (TELEMAC-3D)
! 
      PUBLIC :: SCARACT,POST_INTERP 
! 
!     MAX_BASKET_SIZE IS THE NUMBER OF ADVECTED VARIABLES IN ONE PROCEDURE CALL   
! 
      INTEGER, PARAMETER :: MAX_BASKET_SIZE=10 ! LARGE 
! 
!     SEE CALL GET_MPI_PARAMETERS IN SCARACT 
!        
      INTEGER MPI_INTEGER,MPI_REAL8,MPI_UB,MPI_COMM_WORLD,MPI_SUCCESS
      INTEGER LAST_NOMB,LAST_NPLOT  
!  
!     THE TYPE FOR CHARACTERISTICS - LOST TRACEBACKS 
!     DESCRIBES A TRACEBACK LEAVING A PARTITION TO ANOTHER ONE   
!     FOR 2D WE USE 3D -> KNE AND ZP ARE OBSOLETE THEN 
!
!     SEE ORG_CHARAC_TYPE1 IN LIBRARY PARALLEL
!     IT MUST BE THE SAME LOCAL TYPE
! 
      TYPE CHARAC_TYPE 
        SEQUENCE   ! BUT SEEMS USELESS   
        INTEGER :: MYPID ! PARTITION OF THE TRACEBACK ORIGIN (HEAD) 
        INTEGER :: NEPID ! THE NEIGHBOUR PARTITION THE TRACEBACK ENTERS TO  
        INTEGER :: INE   ! THE LOCAL 2D ELEMENT NR THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION    
        INTEGER :: KNE   ! THE LOCAL LEVEL THE TRACEBACK ENTERS IN THE NEIGBOUR PARTITION    
        INTEGER :: IOR   ! THE POSITION OF THE TRAJECTORY -HEAD- IN MYPID [THE 2D/3D NODE OF ORIGIN] 
        INTEGER :: ISP   ! CURRENT RUNGE-KUTTA STEPS PASSED AS COLLECTED 
        INTEGER :: NSP   ! TOTAL RUNGE-KUTTA STEPS 
        INTEGER :: IFR   ! FREQUENCY
        DOUBLE PRECISION :: XP,YP,ZP,FP  ! THE (X,Y,Z,F)-POSITION 
        DOUBLE PRECISION :: DX,DY,DZ,DF  ! THE CORRESPONDING DISPLACEMENTS
        DOUBLE PRECISION :: BASKET(MAX_BASKET_SIZE) ! VARIABLES INTERPOLATED AT THE FOOT   
      END TYPE CHARAC_TYPE 
! 
!     THE CORRESPONDING MPI TYPE
!  
      INTEGER :: CHARACTERISTIC 
! 
!     STRUCTURES FOR ALL-TO-ALL COMMUNICATION / SEND AND RECEIVE WITH COUNTERS 
!     HEAP/SEND/RECVCOUNTS : COUNT THE NUMBER OF LOST TRACEBACKS PARTITION-WISE 
!     S/RDISPLS : DISPLACEMENTS IN PARTITION-WISE SORTED SEND/RECVCHARS  
!     HEAPCHAR : FOR SAVING INITIALLY LOST CHARACTERISTICS AND COLLECTING  
!                THE IMPLANTED TRACEBACKS LOCALISED IN MY PARTITION  
!     WHILE COLLECTING IS DONE IN HEAPCHARS, MOST ACTIVE OPERATIONS IN RECVCHAR 
!     SENDCHAR REQUIRED DUE TO THE SPECIFIC SORTING FOR MPI_ALLTOALLV (OPTIMISE?)  
! 
      TYPE (CHARAC_TYPE),ALLOCATABLE,DIMENSION(:),SAVE::HEAPCHAR 
      TYPE (CHARAC_TYPE),ALLOCATABLE,DIMENSION(:),SAVE::SENDCHAR 
      TYPE (CHARAC_TYPE),ALLOCATABLE,DIMENSION(:),SAVE::RECVCHAR 
      INTEGER, ALLOCATABLE, DIMENSION(:), SAVE::SENDCOUNTS,SDISPLS 
      INTEGER, ALLOCATABLE, DIMENSION(:), SAVE::RECVCOUNTS,RDISPLS 
      INTEGER, ALLOCATABLE, DIMENSION(:), SAVE::HEAPCOUNTS 
! 
!     WORK FIELD FOR COUNTING OCCURANCES PRO RANK / SORTING SENDCHAR
!  
      INTEGER, ALLOCATABLE, DIMENSION(:), SAVE :: ICHA 
! 
!     IF SET TO TRUE, EVERY DETAILED DEBUGGING IS SWITCHED ON
!  
      LOGICAL :: TRACE=.FALSE. 
! 
!     TO OPTIMISE PERIODICITY
!
      INTEGER, PARAMETER :: NPLANMAX=200
      INTEGER ETA1(NPLANMAX)
      DATA ETA1/002,003,004,005,006,007,008,009,010,011,
     &          012,013,014,015,016,017,018,019,020,021,
     &          022,023,024,025,026,027,028,029,030,031,
     &          032,033,034,035,036,037,038,039,040,041,
     &          042,043,044,045,046,047,048,049,050,051,
     &          052,053,054,055,056,057,058,059,060,061,
     &          062,063,064,065,066,067,068,069,070,071,
     &          072,073,074,075,076,077,078,079,080,081,
     &          082,083,084,085,086,087,088,089,090,091,
     &          092,093,094,095,096,097,098,099,100,101,
     &          102,103,104,105,106,107,108,109,110,111,
     &          112,113,114,115,116,117,118,119,120,121,
     &          122,123,124,125,126,127,128,129,130,131,
     &          132,133,134,135,136,137,138,139,140,141,
     &          142,143,144,145,146,147,148,149,150,151,
     &          152,153,154,155,156,157,158,159,160,161,
     &          162,163,164,165,166,167,168,169,170,171,
     &          172,173,174,175,176,177,178,179,180,181,
     &          182,183,184,185,186,187,188,189,190,191,
     &          192,193,194,195,196,197,198,199,200,201/
!  
      CONTAINS  
! 
!--------------------------------------------------------------------- 
!   <<<<<<<<<<<<<<<<<< CHARACTERISTICS: PRIVATE >>>>>>>>>>>>>>>>>> 
!--------------------------------------------------------------------- 
!        
      SUBROUTINE DEORG_CHARAC_TYPE 
        INTEGER IER 
        CALL P_MPI_TYPE_FREE(CHARACTERISTIC,IER) 
        RETURN 
      END SUBROUTINE DEORG_CHARAC_TYPE 
! 
!--------------------------------------------------------------------- 
!     GET/SET ON DIMENSIONS AND COUNTERS  
!--------------------------------------------------------------------- 
!         
      INTEGER FUNCTION GET_MAX_BASKET_SIZE() 
        GET_MAX_BASKET_SIZE = MAX_BASKET_SIZE 
      END FUNCTION GET_MAX_BASKET_SIZE 
! 
!--------------------------------------------------------------------- 
! ALLOCATE ALL STATIC FIELDS FOR ALL-TO-ALL COMMUNICATION 
! PREPARE THE MPI_TYPE FOR LOST CHARACTERISTICS / TRACEBACKS  
!--------------------------------------------------------------------- 
! 
      SUBROUTINE ORGANISE_CHARS(NPARAM,NOMB,NCHDIM,LAST_NCHDIM) ! WATCH OUT 
          USE BIEF_DEF, ONLY: NCSIZE 
          IMPLICIT NONE 
          INTEGER, INTENT(IN)    :: NPARAM,NOMB 
          INTEGER, INTENT(INOUT) :: NCHDIM,LAST_NCHDIM 
          INTEGER ISIZE 
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
          IF(LAST_NCHDIM.EQ.0) THEN
!           CASE OF A FIRST CALL (THINK THAT NCHDIM MAY BE 0)
            ISIZE=MAX(NCHDIM,1)
            ALLOCATE(SENDCHAR(ISIZE)) 
            ALLOCATE(RECVCHAR(ISIZE)) 
            ALLOCATE(HEAPCHAR(ISIZE))
!           LAST_NCHDIM WILL NEVER BE 0 AGAIN 
            LAST_NCHDIM=ISIZE
          ELSEIF(NCHDIM.GT.LAST_NCHDIM) THEN
!           NEW CALL WITH LARGER NCHDIM, WE HAVE TO REALLOCATE
            DEALLOCATE(SENDCHAR)
            DEALLOCATE(RECVCHAR)
            DEALLOCATE(HEAPCHAR) 
            ALLOCATE(SENDCHAR(NCHDIM)) 
            ALLOCATE(RECVCHAR(NCHDIM)) 
            ALLOCATE(HEAPCHAR(NCHDIM)) 
            LAST_NCHDIM=NCHDIM
          ELSE
!           NEW CALL BUT DIMENSIONS OK, NOTHING TO DO
          ENDIF        
          CALL ORG_CHARAC_TYPE1(NOMB,TRACE,CHARACTERISTIC) ! COMMIT THE CHARACTERISTICS TYPE FOR COMM
          RETURN 
        END SUBROUTINE ORGANISE_CHARS 
! 
!--------------------------------------------------------------------- 
! FOR COLLECTING CHARACTERISTICS LEAVING INITIALLY A GIVEN PARTITION  
! TO BE CALLED IN MODIFIED CHAR11 OR CHAR41 SIMILAR TO THE ORIGINAL 
! BIEF SUBROUTINES / NOTE THE COUNTER NCHARA/HEAPCHAR USAGE  
!--------------------------------------------------------------------- 
! 
        SUBROUTINE COLLECT_CHAR(MYPID,IOR,MYII,IFACE,KNE,IFR, 
     &                          ISP,NSP,XP,YP,ZP,FP,DX,DY,DZ,DF,
     &                          IFAPAR,NCHDIM,NCHARA) 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER,  INTENT(IN) :: MYPID,IOR,MYII,IFACE,KNE,IFR 
          INTEGER,  INTENT(IN) :: ISP,NSP,NCHDIM 
          INTEGER,  INTENT(IN) :: IFAPAR(6,*) 
          INTEGER,  INTENT(INOUT) :: NCHARA 
          DOUBLE PRECISION, INTENT(IN) :: XP,YP,ZP,FP,DX,DY,DZ,DF 
          INTEGER :: NEPID,II 
          ! 
          IF(NCHARA.EQ.0) HEAPCOUNTS=0 
          NEPID=IFAPAR(IFACE  ,MYII) 
          II   =IFAPAR(IFACE+3,MYII)  
          NCHARA=NCHARA+1  
          IF(NCHARA.GT.NCHDIM) THEN ! PROBABLY EXAGGERATED  
            WRITE (LU,*) 'NCHARA=',NCHARA,' NCHDIM=',NCHDIM  
            WRITE (LU,*) 'COLLECT_CHAR::NCHARA>NCHDIM, INCREASE NCHDIM' 
            WRITE (LU,*) 'MYPID=',MYPID  
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
          HEAPCHAR(NCHARA)%IFR=IFR     ! FREQUENCY THERE   
          HEAPCHAR(NCHARA)%XP=XP       ! X-POSITION  
          HEAPCHAR(NCHARA)%YP=YP       ! Y-POSITION  
          HEAPCHAR(NCHARA)%ZP=ZP       ! Z-POSITION
          HEAPCHAR(NCHARA)%FP=FP       ! F-POSITION
          HEAPCHAR(NCHARA)%DX=DX       ! X-DISPLACEMENT  
          HEAPCHAR(NCHARA)%DY=DY       ! Y-DISPLACEMENT   
          HEAPCHAR(NCHARA)%DZ=DZ       ! Z-DISPLACEMENT 
          HEAPCHAR(NCHARA)%DF=DF       ! F-DISPLACEMENT  
! 
          HEAPCOUNTS(NEPID+1)=HEAPCOUNTS(NEPID+1)+1 
! 
          RETURN 
        END SUBROUTINE COLLECT_CHAR 
! 
!--------------------------------------------------------------------- 
! RE-INITIALISE THE STRUCTURE AFTER COMPLETING ALL ACTIONS  
!--------------------------------------------------------------------- 
!       
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
          IF (ALLOCATED(ICHA))      ICHA=0        ! NOT NECESSARY?  
        END SUBROUTINE RE_INITIALISE_CHARS  
! 
!--------------------------------------------------------------------- 
! PREPARE THE INITIAL SEND OF THE LOST CHARACTERISTICS 
! THE FIELDS ARE PREPARED ACCORDING THE MPI_ALLTOALL(V) REQUIREMENTS 
!--------------------------------------------------------------------- 
! 
        SUBROUTINE PREP_INITIAL_SEND(NSEND,NLOSTCHAR,NCHARA) 
          USE BIEF_DEF, ONLY : NCSIZE 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN)    :: NSEND 
          INTEGER, INTENT(OUT)   :: NLOSTCHAR 
          INTEGER, INTENT(INOUT) :: NCHARA 
          INTEGER I,N 
          IF(NCHARA.EQ.0) RETURN  
          SENDCOUNTS=HEAPCOUNTS 
          SDISPLS(1) = 0 ! CONTIGUOUS DATA 
          DO I=2,NCSIZE 
            SDISPLS(I) = SDISPLS(I-1)+SENDCOUNTS(I-1) 
          END DO 
          ICHA=SENDCOUNTS ! A RUNNING COUNTER PARTITION-WISE 
          DO I=1,NCHARA 
!           HEAPCHAR(I)%NEPID+1 - THE PARTITION WE SEND TO / OR -1  
            IF(HEAPCHAR(I)%NEPID.GE.0) THEN              
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
! 
!--------------------------------------------------------------------- 
! COLLECT IMPLANTED TRACEBACKS WHICH ARE COMPLETED/LOCALISED  
! ON A HEAP, SETTING BY THE WAY ALSO THE NUMBER OF THE LOST-AGAIN  
! TRACEBACKS ACCORDINGLY TO THE PARTITION THEY SHOULD BE SEND TO  
! THE "LOCALISED" MARK IS SET IN ADD_CHAR11/41 
!--------------------------------------------------------------------- 
! 
        SUBROUTINE HEAP_FOUND(NLOSTAGAIN,NARRV,NCHARA)  
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(OUT)    :: NLOSTAGAIN 
          INTEGER, INTENT(IN)     :: NARRV 
          INTEGER, INTENT(INOUT)  :: NCHARA 
          INTEGER I  
          SENDCOUNTS=0 
!         DO NOT ZEROIZE NCHARA, HEAPCOUNTS / ADDING FROM GENERATIONS! 
!         COUNTER PARTITION-WISE, ALSO MY-OWN 
          DO I=1,NARRV  
            IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! A LOCALISED TRACEBACK 
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
          RETURN  
        END SUBROUTINE HEAP_FOUND 
! 
!--------------------------------------------------------------------- 
! PREPARE LOST-AGAIN TRACEBACKS FOR THE NEXT COMMUNICATION 
! FILL IN THE STRUCTURE FOR THE ALL-TO-ALL COMMUNICATION  
! NOTE THAT SENDCOUNTS ARE SET IN HEAP_FOUND 
! (OPTIMISE(?): HEAP-FOUND AND PREP_LOST_AGAIN CAN BE JOINED) 
!--------------------------------------------------------------------- 
! 
        SUBROUTINE PREP_LOST_AGAIN(NOMB,NSEND,NLOSTAGAIN,NARRV) 
          USE BIEF_DEF, ONLY : NCSIZE 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER,INTENT(IN)   :: NOMB,NLOSTAGAIN,NARRV 
          INTEGER, INTENT(OUT) :: NSEND 
          INTEGER I,N 
          SDISPLS(1) = 0 ! CONTIGUOUS DATA MARKER 
          DO I=2,NCSIZE 
            SDISPLS(I) = SDISPLS(I-1)+SENDCOUNTS(I-1) 
          ENDDO 
          ICHA=0 
          NSEND=0 
          DO I=1,NARRV 
            N=RECVCHAR(I)%NEPID 
            IF (N.NE.-1) THEN  ! A LOST-AGAIN TRACEBACK 
              ICHA(N+1)=ICHA(N+1)+1 
              NSEND=NSEND+1 
              SENDCHAR(SDISPLS(N+1)+ICHA(N+1)) = RECVCHAR(I)  
            ENDIF  
          ENDDO  
          RETURN  
        END SUBROUTINE PREP_LOST_AGAIN 
! 
!--------------------------------------------------------------------- 
! MOVE THE HEAP OF IMPLANTED AND COMPLETED TRACEBACKS (I.E. COMPLETED  
! IN MY PARTITION) TO DATA STRUCTURES FOR ALL-TO-ALL COMMUNICATION  
! SENDCHAR IS FILLED ACCORDING TO THE MPI_ALLTOALLV REQUIREMENTS  
! ALL-TO-ALL PATTERN INCLUDE MY OWN LOST TRACEBACKS THAT CAME BACK 
!--------------------------------------------------------------------- 
! 
        SUBROUTINE PREP_SENDBACK(NOMB,NCHARA) 
          USE BIEF_DEF, ONLY: IPID, NCSIZE 
          IMPLICIT NONE  
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: NOMB 
          INTEGER, INTENT(INOUT) :: NCHARA 
          INTEGER  :: I,N 
          IF (NCHARA.EQ.0) RETURN ! UHM. 
          SENDCOUNTS=HEAPCOUNTS 
          SDISPLS(1) = 0 ! CONTIGUOUS DATA 
          DO I=2,NCSIZE 
            SDISPLS(I) = SDISPLS(I-1)+SENDCOUNTS(I-1) 
          ENDDO 
          ICHA=0 
          DO I=1,NCHARA     
!           MYPID+1 - IS THE -ORIGIN- PARTITION  
            N=HEAPCHAR(I)%MYPID+1  
            ICHA(N)=ICHA(N)+1 
            SENDCHAR(SDISPLS(N)+ICHA(N))=HEAPCHAR(I) 
!           SIGN IN THE SENDBACK ORIGIN FOR DEBUGGING PURPOSES   
            SENDCHAR(SDISPLS(N)+ICHA(N))%NEPID=IPID 
          ENDDO  
          HEAPCOUNTS=0 
          NCHARA=0 
          RETURN  
        END SUBROUTINE PREP_SENDBACK 
! 
!--------------------------------------------------------------------- 
! THE GLOBAL COMMUNICATION OF LOST CHARACTERISTICS - ALL-TO-ALL 
! (THIS IS THE HEART OF ALL THINGS / THE GLOBAL COMMUNICATION)  
! THE DATA IS SENT AND (NOTE!) RECEIVED -SORTED- ACCORDING TO THE  
! MPI_ALLTOALL(V) SPECIFICATION IN A CONTIGUOUS FIELDS  
! DATA FOR A GIVEN PROCESSOR/PARTITION IN FIELD SECTIONS DESCRIBED BY  
! DISPLACEMENTS SDISPLS AND RDISPLS 
!--------------------------------------------------------------------- 
! 
        SUBROUTINE GLOB_CHAR_COMM() 
          USE BIEF_DEF, ONLY : NCSIZE 
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER :: I,IER 
          CALL P_MPI_ALLTOALL(SENDCOUNTS,1,MPI_INTEGER,  
     &                        RECVCOUNTS,1,MPI_INTEGER,  
     &                        MPI_COMM_WORLD,IER) 
          IF(IER.NE.MPI_SUCCESS) THEN 
            WRITE(LU,*)  
     &       ' @STREAMLINE::GLOB_CHAR_COMM::MPI_ALLTOALL ERROR: ',IER 
            CALL PLANTE(1) 
            STOP 
          ENDIF     
          RDISPLS(1) = 0 ! SAVE THE RECEIVED DATA CONTIGUOUSLY  
          DO I=2,NCSIZE 
            RDISPLS(I) = RDISPLS(I-1)+RECVCOUNTS(I-1) 
          END DO 
          CALL P_MPI_ALLTOALLV
     &      (SENDCHAR,SENDCOUNTS,SDISPLS,CHARACTERISTIC, 
     &       RECVCHAR,RECVCOUNTS,RDISPLS,CHARACTERISTIC, 
     &       MPI_COMM_WORLD,IER) 
          IF(IER.NE.MPI_SUCCESS) THEN 
            WRITE(LU,*)  
     &       ' @STREAMLINE::GLOB_CHAR_COMM::MPI_ALLTOALLV ERROR: ',IER 
            CALL PLANTE(1) 
            STOP  
          ENDIF     
          RETURN 
        END SUBROUTINE GLOB_CHAR_COMM 
! 
!--------------------------------------------------------------------- 
! TELEMAC3D PRISMS, INTERPOLATION OF RECVCHAR  
! ELT,ETA AND SHP,SHZ MUST BE CORRECTLY PROVIDED VIA ADD_CHAR11   
!   -> MATCHED TO THE RANGE 1:NRANGE (NO CHECKING - FOR SPEED!!!)  
!   N IS THE POSITION FORESEEN FOR A GIVEN VARIABLE VAL IN THE BASKET 
!--------------------------------------------------------------------- 
! 
        SUBROUTINE INTERP_RECVCHAR_41 
     &    (VAL,N,IKLE,ELT,ETA,FRE,SHP,SHZ,SHF,NELEM,NPOIN2,
     &     NPLAN,NRANGE,POST,NOMB,PERIO,YA4D) 
          IMPLICIT NONE     
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: N,NELEM,NPOIN2,NPLAN,NRANGE,NOMB 
          INTEGER, INTENT(IN) :: IKLE(NELEM,3)  
          INTEGER, INTENT(IN) :: ELT(NRANGE),ETA(NRANGE) 
          INTEGER, INTENT(IN) :: FRE(NRANGE)  
          DOUBLE PRECISION, INTENT(IN) :: SHP(3,NRANGE),SHZ(NRANGE) 
          DOUBLE PRECISION, INTENT(IN) :: SHF(NRANGE) 
          DOUBLE PRECISION, INTENT(IN) :: VAL(NPOIN2,NPLAN,*)
          LOGICAL, INTENT(IN) :: POST,PERIO,YA4D 
          INTEGER I,ETAP1,I1,I2,I3,IFR
          DOUBLE PRECISION UMSHZ,UMSHF 
! 
!         INTERPOLATION
!
          IF(NOMB.GT.0) THEN
            IF(PERIO) THEN
              ETA1(NPLAN)=1
              IF(YA4D) THEN
                DO I=1,NRANGE 
                  IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED
                    I1=IKLE(ELT(I),1)
                    I2=IKLE(ELT(I),2)
                    I3=IKLE(ELT(I),3)
                    UMSHZ=1.D0-SHZ(I)
                    UMSHF=1.D0-SHF(I)
                    ETAP1=ETA1(ETA(I))
                    IFR=FRE(I)
                    RECVCHAR(I)%BASKET(N) = UMSHF *
     &        ((VAL(I1,ETA(I),IFR  ) * SHP(1,I)
     &        + VAL(I2,ETA(I),IFR  ) * SHP(2,I)
     &        + VAL(I3,ETA(I),IFR  ) * SHP(3,I)) * UMSHZ
     &       +( VAL(I1,ETAP1 ,IFR  ) * SHP(1,I)
     &        + VAL(I2,ETAP1 ,IFR  ) * SHP(2,I)
     &        + VAL(I3,ETAP1 ,IFR  ) * SHP(3,I)) * SHZ(I) )
     &                 + SHF(I) *
     &        ((VAL(I1,ETA(I),IFR+1) * SHP(1,I)
     &        + VAL(I2,ETA(I),IFR+1) * SHP(2,I)
     &        + VAL(I3,ETA(I),IFR+1) * SHP(3,I)) * UMSHZ
     &       +( VAL(I1,ETAP1 ,IFR+1) * SHP(1,I)
     &        + VAL(I2,ETAP1 ,IFR+1) * SHP(2,I)
     &        + VAL(I3,ETAP1 ,IFR+1) * SHP(3,I)) * SHZ(I) )
!                 THIS IS JUST TO AVOID A BUG ON HP COMPILER 
                  ELSEIF(RECVCHAR(I)%NEPID.LT.-1) THEN 
                    WRITE(LU,*) 'STREAMLINE INTERP_RECVCHAR_11' 
                    WRITE(LU,*) 'NEPID OUT OF RANGE:',RECVCHAR(I)%NEPID 
                    WRITE(LU,*) 'FOR I=',I 
                    CALL PLANTE(1) 
                    STOP 
!                 END OF THIS IS JUST TO AVOID A BUG ON HP COMPILER 
                  ENDIF 
                ENDDO
              ELSE
                DO I=1,NRANGE 
                  IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED
                    ETAP1=ETA1(ETA(I))
                    RECVCHAR(I)%BASKET(N) = 
     &          (VAL(IKLE(ELT(I),1),ETA(I),1)*SHP(1,I) 
     &          +VAL(IKLE(ELT(I),2),ETA(I),1)*SHP(2,I) 
     &          +VAL(IKLE(ELT(I),3),ETA(I),1)*SHP(3,I))*(1.D0-SHZ(I)) 
     &         +(VAL(IKLE(ELT(I),1),ETAP1 ,1)*SHP(1,I) 
     &          +VAL(IKLE(ELT(I),2),ETAP1 ,1)*SHP(2,I)
     &          +VAL(IKLE(ELT(I),3),ETAP1 ,1)*SHP(3,I))*SHZ(I) 
!                 THIS IS JUST TO AVOID A BUG ON HP COMPILER 
                  ELSEIF(RECVCHAR(I)%NEPID.LT.-1) THEN 
                    WRITE(LU,*) 'STREAMLINE INTERP_RECVCHAR_11' 
                    WRITE(LU,*) 'NEPID OUT OF RANGE:',RECVCHAR(I)%NEPID 
                    WRITE(LU,*) 'FOR I=',I 
                    CALL PLANTE(1) 
                    STOP 
!                 END OF THIS IS JUST TO AVOID A BUG ON HP COMPILER 
                  ENDIF 
                ENDDO
              ENDIF
!             RESTORING THE ORIGINAL ETA1
              ETA1(NPLAN)=NPLAN+1
            ELSE
              DO I=1,NRANGE 
              IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED 
                RECVCHAR(I)%BASKET(N) = 
     &          ( VAL(IKLE(ELT(I),1),ETA(I),1)  *SHP(1,I) 
     &          + VAL(IKLE(ELT(I),2),ETA(I),1)  *SHP(2,I)
     &          + VAL(IKLE(ELT(I),3),ETA(I),1)  *SHP(3,I))*(1.D0-SHZ(I)) 
     &         +( VAL(IKLE(ELT(I),1),ETA(I)+1,1)*SHP(1,I) 
     &          + VAL(IKLE(ELT(I),2),ETA(I)+1,1)*SHP(2,I) 
     &          + VAL(IKLE(ELT(I),3),ETA(I)+1,1)*SHP(3,I))*SHZ(I) 
!             THIS IS JUST TO AVOID A BUG ON HP COMPILER 
              ELSEIF(RECVCHAR(I)%NEPID.LT.-1) THEN 
                WRITE(LU,*) 'STREAMLINE  INTERP_RECVCHAR_11' 
                WRITE(LU,*) 'NEPID OUT OF RANGE:',RECVCHAR(I)%NEPID 
                WRITE(LU,*) 'FOR I=',I 
                CALL PLANTE(1) 
                STOP 
!             END OF THIS IS JUST TO AVOID A BUG ON HP COMPILER 
              ENDIF 
              ENDDO
            ENDIF
          ENDIF
!
!         SAVING INTERPOLATION DATA
!
          IF(POST) THEN
            IF(YA4D) THEN
              DO I=1,NRANGE 
                IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED 
                  RECVCHAR(I)%XP=SHP(1,I)
                  RECVCHAR(I)%YP=SHP(2,I)
                  RECVCHAR(I)%ZP=SHP(3,I)
                  RECVCHAR(I)%DX=SHZ(I)
                  RECVCHAR(I)%DY=SHF(I)
                  RECVCHAR(I)%INE=ELT(I)
                  RECVCHAR(I)%KNE=ETA(I) 
                  RECVCHAR(I)%IFR=FRE(I) 
                ENDIF 
              ENDDO
            ELSE
              DO I=1,NRANGE 
                IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED 
                  RECVCHAR(I)%XP=SHP(1,I)
                  RECVCHAR(I)%YP=SHP(2,I)
                  RECVCHAR(I)%ZP=SHP(3,I)
                  RECVCHAR(I)%DX=SHZ(I)
                  RECVCHAR(I)%INE=ELT(I)
                  RECVCHAR(I)%KNE=ETA(I) 
                ENDIF 
              ENDDO
            ENDIF
          ENDIF  
! 
          RETURN  
        END SUBROUTINE INTERP_RECVCHAR_41 
!         
!--------------------------------------------------------------------- 
! TELEMAC2D TRIANGLES, INTERPOLATION OF RECVCHAR 
! ELT AND SHP MUST BE CORRECTLY PROVIDED VIA SCHAR11   
!   -> MATCHED TO THE RANGE 1:NRANGE (NO CHECKING - FOR SPEED!!!)  
!   N IS THE POSITION FORESEEN FOR A GIVEN VARIABLE VAL IN THE BASKET 
!--------------------------------------------------------------------- 
! 
        SUBROUTINE INTERP_RECVCHAR_11  
     &      (VAL,N,IKLE,ELT,SHP,NELEM,NPOIN,NRANGE,IELM,POST,NOMB)
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN) :: N,NELEM,NPOIN,NRANGE,IELM,NOMB 
          INTEGER, INTENT(IN) :: IKLE(NELEM,*)  
          INTEGER, INTENT(IN) :: ELT(NRANGE) 
          DOUBLE PRECISION, INTENT(IN) :: SHP(3,NRANGE) 
          DOUBLE PRECISION, INTENT(IN) :: VAL(NPOIN)
          LOGICAL, INTENT(IN) :: POST
!
          DOUBLE PRECISION SHP11,SHP12,SHP14
          DOUBLE PRECISION SHP22,SHP23,SHP24
          DOUBLE PRECISION SHP33,SHP31,SHP34
!
!         SHOULD BE SAME EPSILO THAN SCHAR11 AND INTERP
          DOUBLE PRECISION EPSILO
          DATA EPSILO / 1.D-6 /  
          INTEGER I 
!
          IF(NOMB.GT.0) THEN
          IF(IELM.EQ.11) THEN 
            DO I=1,NRANGE 
              IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED 
                RECVCHAR(I)%BASKET(N) =  
     &                         VAL(IKLE(ELT(I),1)) * SHP(1,I) 
     &                       + VAL(IKLE(ELT(I),2)) * SHP(2,I) 
     &                       + VAL(IKLE(ELT(I),3)) * SHP(3,I) 
!             THIS IS JUST TO AVOID A BUG ON HP COMPILER 
              ELSEIF(RECVCHAR(I)%NEPID.LT.-1) THEN 
                WRITE(LU,*) 'STREAMLINE INTERP_RECVCHAR_11' 
                WRITE(LU,*) 'NEPID OUT OF RANGE:',RECVCHAR(I)%NEPID 
                WRITE(LU,*) 'FOR I=',I 
                CALL PLANTE(1) 
                STOP 
!             END OF THIS IS JUST TO AVOID A BUG ON HP COMPILER 
              ENDIF 
            ENDDO 
          ELSEIF(IELM.EQ.12) THEN
            DO I=1,NRANGE 
              IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED 
                SHP11=SHP(1,I)-SHP(3,I)
                SHP12=SHP(2,I)-SHP(3,I)
                SHP14=3.D0*SHP(3,I)
                SHP22=SHP(2,I)-SHP(1,I)
                SHP23=SHP(3,I)-SHP(1,I)
                SHP24=3.D0*SHP(1,I)
                SHP33=SHP(3,I)-SHP(2,I)
                SHP31=SHP(1,I)-SHP(2,I)
                SHP34=3.D0*SHP(2,I)
!               SEE INTERP
                IF(     SHP11.GT.    -2.D0*EPSILO .AND. 
     &                  SHP11.LT.1.D0+4.D0*EPSILO .AND.
     &                  SHP12.GT.    -2.D0*EPSILO .AND. 
     &                  SHP12.LT.1.D0+4.D0*EPSILO .AND.
     &                  SHP14.LT.1.D0+4.D0*EPSILO ) THEN
                        RECVCHAR(I)%BASKET(N) = 
     &                      VAL(IKLE(ELT(I),1)) * SHP11
     &                    + VAL(IKLE(ELT(I),2)) * SHP12
     &                    + VAL(IKLE(ELT(I),4)) * SHP14
                ELSEIF( SHP22.GT.    -2.D0*EPSILO .AND. 
     &                  SHP22.LT.1.D0+4.D0*EPSILO .AND.
     &                  SHP23.GT.    -2.D0*EPSILO .AND. 
     &                  SHP23.LT.1.D0+4.D0*EPSILO .AND.
     &                  SHP24.LT.1.D0+4.D0*EPSILO ) THEN
                        RECVCHAR(I)%BASKET(N) = 
     &                      VAL(IKLE(ELT(I),2)) * SHP22
     &                    + VAL(IKLE(ELT(I),3)) * SHP23
     &                    + VAL(IKLE(ELT(I),4)) * SHP24
                ELSEIF( SHP33.GT.    -2.D0*EPSILO .AND. 
     &                  SHP33.LT.1.D0+4.D0*EPSILO .AND.
     &                  SHP31.GT.    -2.D0*EPSILO .AND. 
     &                  SHP31.LT.1.D0+4.D0*EPSILO .AND.
     &                  SHP34.LT.1.D0+4.D0*EPSILO ) THEN
                        RECVCHAR(I)%BASKET(N) = 
     &                      VAL(IKLE(ELT(I),3)) * SHP33
     &                    + VAL(IKLE(ELT(I),1)) * SHP31
     &                    + VAL(IKLE(ELT(I),4)) * SHP34
                ELSE
                  WRITE(LU,*) 'I=',I,' NOT LOCATED, ELT=',ELT(I)
                  CALL PLANTE(1)
                  STOP
                ENDIF
!             THIS IS JUST TO AVOID A BUG ON HP COMPILER 
              ELSEIF(RECVCHAR(I)%NEPID.LT.-1) THEN 
                WRITE(LU,*) 'STREAMLINE  INTERP_RECVCHAR_11' 
                WRITE(LU,*) 'NEPID OUT OF RANGE:',RECVCHAR(I)%NEPID 
                WRITE(LU,*) 'FOR I=',I 
                CALL PLANTE(1) 
                STOP 
!             END OF THIS IS JUST TO AVOID A BUG ON HP COMPILER 
              ENDIF 
            ENDDO  
          ELSEIF(IELM.EQ.13) THEN 
          DO I=1,NRANGE 
            IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED      
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
          ENDIF 
!
          IF(POST) THEN
            IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.13) THEN 
              DO I=1,NRANGE 
                IF(RECVCHAR(I)%NEPID.EQ.-1) THEN ! LOCALISED 
                  RECVCHAR(I)%XP=SHP(1,I)
                  RECVCHAR(I)%YP=SHP(2,I)
                  RECVCHAR(I)%ZP=SHP(3,I)
                  RECVCHAR(I)%INE=ELT(I)
                ENDIF 
              ENDDO
            ELSE 
              WRITE(LU,*) 'WRONG IELM IN INTERP_RECVCHAR_11:',IELM 
              CALL PLANTE(1) 
              STOP 
            ENDIF
          ENDIF           
! 
          RETURN  
        END SUBROUTINE INTERP_RECVCHAR_11 
! 
!--------------------------------------------------------------------- 
! INTRODUCE RECEIVED VALUES IN THE BASKET BACK IN THE TELEMAC  
! STRUCTURES, N BEING THE POSITION IN THE BASKET FOR A GIVEN VARIABLE  
!--------------------------------------------------------------------- 
! 
        SUBROUTINE INTRODUCE_RECVCHAR(VAL,NOMB,NARRV,IELM,
     &                                SHP,SHZ,SHF,ELT,ETA,FRE,
     &                                POST,YA4D)
          USE BIEF  
          IMPLICIT NONE 
          INTEGER LNG,LU 
          COMMON/INFO/LNG,LU 
          INTEGER, INTENT(IN)    :: NOMB,NARRV,IELM
          INTEGER, INTENT(INOUT) :: ELT(*),ETA(*),FRE(*)
          DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,*),SHZ(*)
          DOUBLE PRECISION, INTENT(INOUT) :: SHF(*)
!         IF YES, SAVE INTERPOLATION DATA
          LOGICAL, INTENT(IN) :: POST,YA4D
          TYPE(BIEF_OBJ), INTENT(INOUT) :: VAL 
          INTEGER I,N,IPOIN  
          IF(NOMB.GT.MAX_BASKET_SIZE) THEN  
            WRITE(LU,*) 'STREAMLINE::INTRODUCE_RECVCHAR' 
            WRITE(LU,*) 'NOMB>MAX_BASKET_SIZE'  
            WRITE(LU,*) 'NOMB,MAX_BASKET_SIZE=',NOMB,MAX_BASKET_SIZE 
            CALL PLANTE(1) 
            STOP  
          ENDIF
!
!         SAVING INTERPOLATION DATA, DEPENDING ON IELM
!
          IF(POST) THEN
            IF(IELM.EQ.11) THEN
              DO I=1,NARRV 
                IPOIN=RECVCHAR(I)%IOR
                SHP(1,IPOIN)=RECVCHAR(I)%XP
                SHP(2,IPOIN)=RECVCHAR(I)%YP
                SHP(3,IPOIN)=RECVCHAR(I)%ZP
                ELT(IPOIN)  =RECVCHAR(I)%INE             
              ENDDO
            ELSEIF(IELM.EQ.41) THEN
              IF(YA4D) THEN
                DO I=1,NARRV
                  IPOIN=RECVCHAR(I)%IOR 
                  SHP(1,IPOIN)=RECVCHAR(I)%XP
                  SHP(2,IPOIN)=RECVCHAR(I)%YP
                  SHP(3,IPOIN)=RECVCHAR(I)%ZP
                  SHZ(IPOIN)  =RECVCHAR(I)%DX
                  SHF(IPOIN)  =RECVCHAR(I)%DY
                  ELT(IPOIN)  =RECVCHAR(I)%INE
                  ETA(IPOIN)  =RECVCHAR(I)%KNE 
                  FRE(IPOIN)  =RECVCHAR(I)%IFR 
                ENDDO
              ELSE
                DO I=1,NARRV
                  IPOIN=RECVCHAR(I)%IOR 
                  SHP(1,IPOIN)=RECVCHAR(I)%XP
                  SHP(2,IPOIN)=RECVCHAR(I)%YP
                  SHP(3,IPOIN)=RECVCHAR(I)%ZP
                  SHZ(IPOIN)  =RECVCHAR(I)%DX
                  ELT(IPOIN)  =RECVCHAR(I)%INE
                  ETA(IPOIN)  =RECVCHAR(I)%KNE 
                ENDDO
              ENDIF
            ELSE
              WRITE(LU,*)'STREAMLINE::INTRODUCE_RECVCHAR'
              WRITE(LU,*)'UNEXPECTED IELM=',IELM 
              CALL PLANTE(1) 
              STOP  
            ENDIF
          ENDIF 
!
!         NOW THE INTERPOLATION, DEPENDING ON TYPE
! 
          IF(NOMB.GT.0) THEN          
            IF(VAL%TYPE.EQ.2) THEN 
              DO I=1,NARRV 
                VAL%R(RECVCHAR(I)%IOR)=RECVCHAR(I)%BASKET(1) 
              ENDDO
            ELSEIF(VAL%TYPE.EQ.4) THEN      
              DO I=1,NARRV
                DO N=1,NOMB 
                  VAL%ADR(N)%P%R(RECVCHAR(I)%IOR)=
     &            RECVCHAR(I)%BASKET(N) 
                ENDDO
              ENDDO
            ELSE
              WRITE(LU,*)'STREAMLINE::INTRODUCE_RECVCHAR'
              WRITE(LU,*)'UNEXPECTED VAL%TYPE=',VAL%TYPE 
              CALL PLANTE(1) 
              STOP  
            ENDIF
          ENDIF 
          RETURN  
        END SUBROUTINE INTRODUCE_RECVCHAR 
 
!///////////////////////////////////////////////////////////////////// 
! 
!            TELEMAC ROUTINES MODIFIED FOR THE PURPOSE OF  
!           PARALLEL STREAMLINE TRACKING - MOSTLY FROM BIEF  
! 
!///////////////////////////////////////////////////////////////////// 
 
!                       ****************** 
                        SUBROUTINE SCHAR41 
!                       ****************** 
! 
     &( U , V , W , DT , NRK , X , Y , ZSTAR , Z , IKLE2 , IBOR , 
     &  XPLOT , YPLOT , ZPLOT , DX , DY , DZ , SHP , SHZ , ELT , ETA , 
     &  NPLOT , NPOIN2 , NELEM2 , NPLAN , SURDET , 
     &  SENS  , IFAPAR, NCHDIM,NCHARA,ADD) 
! 
!*********************************************************************** 
! BIEF VERSION 6.2           28/04/93     J-M JANIN (LNH) 30 87 72 84 
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
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NCHARA 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN) 
      DOUBLE PRECISION, INTENT(IN)    :: W(NPOIN2,NPLAN),SURDET(NELEM2) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: ZPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT),SHZ(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2),DT 
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: DZ(NPLOT) 
      INTEGER         , INTENT(IN)    :: IBOR(NELEM2,5,NPLAN-1) 
      INTEGER         , INTENT(INOUT) :: ETA(NPLOT) 
      INTEGER         , INTENT(IN)    :: IFAPAR(6,*) 
      LOGICAL, INTENT(IN)             :: ADD 
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER IELE,ISO,ISPDONE,NSP 
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,IET2,ISOH,ISOV,IFA,ISUI(3) 
! 
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,ZP,DENOM 
      DOUBLE PRECISION DELTAZ,EPSDZ,PAS2 
! 
      INTRINSIC ABS , INT , MAX , SQRT 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA EPSILO / -1.D-6 / 
      DATA EPSDZ  /1.D-4/ 
! 
!----------------------------------------------------------------------- 
!     FOR EVERY POINT 
!----------------------------------------------------------------------- 
! 
      DO IPLOT = 1 , NPLOT 
!  
        IF(ADD) THEN
!
        XPLOT(IPLOT)   = RECVCHAR(IPLOT)%XP  
        YPLOT(IPLOT)   = RECVCHAR(IPLOT)%YP  
        ZPLOT(IPLOT)   = RECVCHAR(IPLOT)%ZP 
        DX(IPLOT)      = RECVCHAR(IPLOT)%DX  
        DY(IPLOT)      = RECVCHAR(IPLOT)%DY 
        DZ(IPLOT)      = RECVCHAR(IPLOT)%DZ  
        ELT(IPLOT)     = RECVCHAR(IPLOT)%INE 
        ETA(IPLOT)     = RECVCHAR(IPLOT)%KNE 
        NSP            = RECVCHAR(IPLOT)%NSP ! R-K STEPS TO BE FULLFILLED 
        ISPDONE        = RECVCHAR(IPLOT)%ISP ! R-K STEPS ALREADY DONE  
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
        ELSE
!
         IEL = ELT(IPLOT) 
!        POINTS WITH IEL=0 ARE TREATED SO THAT THE FINAL
!        INTERPOLATION GIVES 0.,
!        AND WE SKIP TO NEXT POINT IPLOT (CYCLE)
         IF(IEL.EQ.0) THEN
           ELT(IPLOT)=1
           ETA(IPLOT)=1
           SHP(1,IPLOT)=0.D0
           SHP(2,IPLOT)=0.D0
           SHP(3,IPLOT)=0.D0
           SHZ(IPLOT)=0.D0
           CYCLE   
         ENDIF
         IET = ETA(IPLOT) 
         I1 = IKLE2(IEL,1) 
         I2 = IKLE2(IEL,2) 
         I3 = IKLE2(IEL,3)  
         DXP =( U(I1,IET  )*SHP(1,IPLOT) 
     &        + U(I2,IET  )*SHP(2,IPLOT)
     &        + U(I3,IET  )*SHP(3,IPLOT) )*(1.D0-SHZ(IPLOT)) 
     &       +( U(I1,IET+1)*SHP(1,IPLOT) 
     &        + U(I2,IET+1)*SHP(2,IPLOT) 
     &        + U(I3,IET+1)*SHP(3,IPLOT) )*SHZ(IPLOT)  
         DYP =( V(I1,IET  )*SHP(1,IPLOT) 
     &        + V(I2,IET  )*SHP(2,IPLOT) 
     &        + V(I3,IET  )*SHP(3,IPLOT) )*(1.D0-SHZ(IPLOT)) 
     &       +( V(I1,IET+1)*SHP(1,IPLOT)
     &        + V(I2,IET+1)*SHP(2,IPLOT) 
     &        + V(I3,IET+1)*SHP(3,IPLOT) )*SHZ(IPLOT) 
         NSP=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL)))) 
         ISPDONE=1
!
        ENDIF
! 
        PAS = SENS * DT / NSP
! 
!       LOOP ON RUNGE-KUTTA SUB-STEPS
!
!       COMPILER MUST DO NOTHING IF ISPDONE>NSP
!       IN MODE "ADD", ISP = ISPDONE HAS NOT BEEN FULLY DONE
!       IT IS RESTARTED HERE
!
      DO ISP = ISPDONE,NSP 
! 
!----------------------------------------------------------------------- 
!       LOCALISING THE ARRIVAL POINT
!----------------------------------------------------------------------- 
!  
        PAS2 = PAS 
!
!       IN MODE "ADD" ITERATIONS ALREADY DONE ARE SKIPPED AND
!                     CHARACTERISTICS GONE IN ANOTHER SUB-DOMAIN SKIPPED                   
!
        IF(ADD) THEN
          IF(ISP.EQ.ISPDONE) GO TO 50
          IF(RECVCHAR(IPLOT)%NEPID.NE.-1) CYCLE 
        ENDIF
!
        IEL = ELT(IPLOT) 
        IET = ETA(IPLOT)  
        I1 = IKLE2(IEL,1) 
        I2 = IKLE2(IEL,2) 
        I3 = IKLE2(IEL,3) 
! 
        DX(IPLOT) = ((U(I1,IET  )*SHP(1,IPLOT) 
     &              + U(I2,IET  )*SHP(2,IPLOT) 
     &              + U(I3,IET  )*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT)) 
     &              +(U(I1,IET+1)*SHP(1,IPLOT) 
     &              + U(I2,IET+1)*SHP(2,IPLOT) 
     &              + U(I3,IET+1)*SHP(3,IPLOT))*SHZ(IPLOT) ) * PAS 
! 
        DY(IPLOT) = ((V(I1,IET  )*SHP(1,IPLOT)
     &              + V(I2,IET  )*SHP(2,IPLOT) 
     &              + V(I3,IET  )*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT)) 
     &              +(V(I1,IET+1)*SHP(1,IPLOT) 
     &              + V(I2,IET+1)*SHP(2,IPLOT) 
     &              + V(I3,IET+1)*SHP(3,IPLOT))*SHZ(IPLOT) ) * PAS 
! 
        DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT) 
     &          + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT) 
     &          + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT) 
! 
        IF(DELTAZ.GT.EPSDZ) THEN 
!         DIVISION BY DELTAZ IS DUE TO THE FACT THAT W IS  
!         W* MULTIPLIED BY DELTAZ (IT STEMS FROM TRIDW2 IN TELEMAC3D) 
          DZ(IPLOT) = ((W(I1,IET  )*SHP(1,IPLOT) 
     &                + W(I2,IET  )*SHP(2,IPLOT) 
     &                + W(I3,IET  )*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT)) 
     &                +(W(I1,IET+1)*SHP(1,IPLOT) 
     &                + W(I2,IET+1)*SHP(2,IPLOT)
     &                + W(I3,IET+1)*SHP(3,IPLOT))*SHZ(IPLOT) )   
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
        IF(ADD) THEN
!         CONTINUOUS SETTING OF THE REACHED POSITION FOR IPLOT  
!         AND THE NUMBER OF STEPS DONE ALREADY
          RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
          RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
          RECVCHAR(IPLOT)%ZP=ZPLOT(IPLOT) 
          RECVCHAR(IPLOT)%DX=DX(IPLOT) 
          RECVCHAR(IPLOT)%DY=DY(IPLOT)
          RECVCHAR(IPLOT)%DZ=DZ(IPLOT)
          RECVCHAR(IPLOT)%INE=ELT(IPLOT) 
          RECVCHAR(IPLOT)%ISP=ISP 
        ENDIF
!
!----------------------------------------------------------------------- 
!       TEST: IS THE PATHLINE WENT OUT THE ORIGINAL ELEMENT
!----------------------------------------------------------------------- 
! 
50      CONTINUE 
! 
        ISO = 0
        IF(SHP(1,IPLOT).LT.EPSILO)      ISO=IBSET(ISO,2) 
        IF(SHP(2,IPLOT).LT.EPSILO)      ISO=IBSET(ISO,3) 
        IF(SHP(3,IPLOT).LT.EPSILO)      ISO=IBSET(ISO,4) 
        IF(SHZ(IPLOT)  .LT.EPSILO)      ISO=IBSET(ISO,0) 
        IF(SHZ(IPLOT)  .GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
!
        IF(ISO.NE.0) THEN
! 
!----------------------------------------------------------------------- 
!         HERE WE ARE OUT OF THE ELEMENT
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
!           HERE WE ARRIVE IN ANOTHER ELEMENT THROUGH A QUADRANGULAR FACE
!----------------------------------------------------------------------- 
! 
            IF(IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!             RELOCALISING IN ADJACENT ELEMENT
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
              GOTO 50 
! 
            ENDIF 
! 
!----------------------------------------------------------------------- 
!           HERE WE PASS TO A NEIGHBOUR SUBDOMAIN AND COLLECT DATA
!----------------------------------------------------------------------- 
! 
          IF(IEL.EQ.-2) THEN
            IF(.NOT.ADD) THEN
!             INTERFACE CROSSING 
              CALL COLLECT_CHAR  
     &            (IPID,IPLOT,ELT(IPLOT),IFA,ETA(IPLOT),0,ISP,  
     &             NSP,XPLOT(IPLOT),YPLOT(IPLOT),
     &             ZPLOT(IPLOT),0.D0,  
     &             DX(IPLOT),DY(IPLOT),DZ(IPLOT),0.D0,  
     &             IFAPAR,NCHDIM,NCHARA) 
            ELSE 
!             A LOST-AGAIN TRACEBACK DETECTED   
!             PROCESSOR NUMBER   
              RECVCHAR(IPLOT)%NEPID=IFAPAR(IFA,ELT(IPLOT))
              RECVCHAR(IPLOT)%INE=IFAPAR(IFA+3,ELT(IPLOT))  
              RECVCHAR(IPLOT)%KNE=ETA(IPLOT)       
            ENDIF
!           EXITING LOOP ON ISP
            EXIT
          ENDIF  
! 
!----------------------------------------------------------------------- 
!           SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
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
!             HERE SOLID BOUNDARY, VELOCITY IS PROJECTED ON THE BOUNDARY
!             AND WE GO ON
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
              IF(ADD) THEN 
                RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
                RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
                RECVCHAR(IPLOT)%ZP=ZPLOT(IPLOT) 
                RECVCHAR(IPLOT)%DX=DX(IPLOT) 
                RECVCHAR(IPLOT)%DY=DY(IPLOT)
                RECVCHAR(IPLOT)%DZ=DZ(IPLOT)
              ENDIF
! 
              GOTO 50 
! 
            ELSEIF(IEL.EQ.0) THEN 
! 
!----------------------------------------------------------------------- 
!           HERE WE HAVE A LIQUID BOUNDARY, THE CHARACTERISTIC IS STOPPED
!----------------------------------------------------------------------- 
! 
            DENOM = DXP*DY1-DYP*DX1 
            IF(ABS(DENOM).GT.1.D-12) THEN 
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
!           THIS IS A MARKER FOR PARTICLES EXITING A DOMAIN
!           SENS=-1 FOR BACKWARD CHARACTERISTICS
            ELT(IPLOT) = - SENS * ELT(IPLOT) 
!           EXITING LOOP ON ISP
            EXIT
!
            ELSE
!
              WRITE(LU,*) 'UNEXPECTED CASE IN SCHAR41'
              WRITE(LU,*) 'IEL=',IEL
              CALL PLANTE(1)
              STOP
!
            ENDIF
! 
          ELSE 
! 
!----------------------------------------------------------------------- 
!  CASE IFA = 4 OR 5  
!  HERE WE EXIT THROUGH TOP OR BOTTOM OF THE PRISM
!----------------------------------------------------------------------- 
! 
            IFA = IFA - 4 
!           HENCE IFA NOW EQUALS 0 OR 1 
! 
            IF(IEL.EQ.1) THEN 
! 
!----------------------------------------------------------------------- 
!           NO NEED TO RECOMPUTE THE VELOCITIES,
!           RELOCALISING IN NEW ELEMENT
!----------------------------------------------------------------------- 
! 
              ETA(IPLOT) = IET + IFA + IFA - 1 
              SHZ(IPLOT) = (ZP-ZSTAR(ETA(IPLOT))) 
     &                   / (ZSTAR(ETA(IPLOT)+1)-ZSTAR(ETA(IPLOT))) 
!
              IF(ADD) THEN 
                RECVCHAR(IPLOT)%KNE=ETA(IPLOT) 
              ENDIF 
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
!              
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
!             THIS IS A MARKER FOR PARTICLES EXITING A DOMAIN
!             SENS=-1 FOR BACKWARD CHARACTERISTICS
              ELT(IPLOT) = - SENS * ELT(IPLOT)
!             EXITING LOOP ON ISP 
              EXIT
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
!                       ********************** 
                        SUBROUTINE SCHAR41_PER 
!                       ********************** 
! 
     &( U , V , W , DT , NRK , X , Y , ZSTAR , Z , IKLE2 , IBOR , 
     &  XPLOT , YPLOT , ZPLOT , DX , DY , DZ , SHP , SHZ , ELT , ETA , 
     &  NPLOT , NPOIN2 , NELEM2 , NPLAN , SURDET , 
     &  SENS  , IFAPAR, NCHDIM,NCHARA,ADD) 
! 
!*********************************************************************** 
! BIEF VERSION 6.3           28/04/93     J-M JANIN (LNH) 30 87 72 84 
!                        12/10/05     J-M HERVOUET (LNHE) 01 30 87 80 18 
! 
!brief    LIKE SCHAR41 BUT WITH PERIODICITY ON THE VERTICAL
!         DIFFERENCES MAEKED WITH "PERIODICITY"
!
!
!history  J-M HERVOUET (LNHE)
!+        31/07/2012
!+        V6P3
!+      First version (differences taken in Tomawac)             
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
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NCHARA 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN) 
      DOUBLE PRECISION, INTENT(IN)    :: W(NPOIN2,NPLAN),SURDET(NELEM2) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: ZPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT),SHZ(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2),DT 
!                                                              PERIODICITY                                                              
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN+1) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: DZ(NPLOT) 
      INTEGER         , INTENT(IN)    :: IBOR(NELEM2,5,NPLAN-1) 
      INTEGER         , INTENT(INOUT) :: ETA(NPLOT) 
      INTEGER         , INTENT(IN)    :: IFAPAR(6,*) 
      LOGICAL, INTENT(IN)             :: ADD 
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER IELE,ISO,ISPDONE,NSP,NSPMAX 
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,IET2,ISOH,ISOV,IFA,ISUI(3) 
      INTEGER IETP1 
! 
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,DZP,XP,YP,ZP,DENOM 
      DOUBLE PRECISION EPSDZ,PAS2 
!
      INTEGER  P_IMAX 
      EXTERNAL P_IMAX
! 
      INTRINSIC ABS , INT , MAX , SQRT 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA EPSILO / -1.D-6 / 
      DATA EPSDZ  /1.D-4/ 
! 
      NSPMAX=1
      ETA1(NPLAN)=1
!
!----------------------------------------------------------------------- 
!     FOR EVERY POINT 
!----------------------------------------------------------------------- 
! 
      DO IPLOT = 1 , NPLOT 
!  
        IF(ADD) THEN
!
        XPLOT(IPLOT)   = RECVCHAR(IPLOT)%XP  
        YPLOT(IPLOT)   = RECVCHAR(IPLOT)%YP  
        ZPLOT(IPLOT)   = RECVCHAR(IPLOT)%ZP 
        DX(IPLOT)      = RECVCHAR(IPLOT)%DX  
        DY(IPLOT)      = RECVCHAR(IPLOT)%DY 
        DZ(IPLOT)      = RECVCHAR(IPLOT)%DZ  
        ELT(IPLOT)     = RECVCHAR(IPLOT)%INE 
        ETA(IPLOT)     = RECVCHAR(IPLOT)%KNE 
        NSP            = RECVCHAR(IPLOT)%NSP ! R-K STEPS TO BE FULLFILLED 
        ISPDONE        = RECVCHAR(IPLOT)%ISP ! R-K STEPS ALREADY DONE  
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
        ELSE
!
         IEL = ELT(IPLOT) 
!        POINTS WITH IEL=0 ARE TREATED SO THAT THE FINAL
!        INTERPOLATION GIVES 0.,
!        AND WE SKIP TO NEXT POINT IPLOT (CYCLE)
         IF(IEL.EQ.0) THEN
           ELT(IPLOT)=1
           ETA(IPLOT)=1
           SHP(1,IPLOT)=0.D0
           SHP(2,IPLOT)=0.D0
           SHP(3,IPLOT)=0.D0
           SHZ(IPLOT)=0.D0
           CYCLE   
         ENDIF
         IET = ETA(IPLOT) 
         I1 = IKLE2(IEL,1) 
         I2 = IKLE2(IEL,2) 
         I3 = IKLE2(IEL,3) 
!        HERE IET+1 IS ALWAYS < NPLAN+1 (SEE GTSH41) 
         DXP = U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + U(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &       + U(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &       + U(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT)  
         DYP = V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     &       + V(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT) 
     &       + V(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT) 
     &       + V(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) 
         NSP=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL)))) 
!!       PERIODICITY
         DZP = W(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &       + W(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &       + W(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT)
!
         NSP=MAX(NSP,INT(NRK*DT*ABS(DZP/(ZSTAR(IET+1)-ZSTAR(IET)))))
         NSPMAX = MAX(NSPMAX,NSP)
!!       END PERIODICITY
!
         ISPDONE=1
!
        ENDIF
! 
        PAS = SENS * DT / NSP
! 
!       LOOP ON RUNGE-KUTTA SUB-STEPS
!
!       COMPILER MUST DO NOTHING IF ISPDONE>NSP
!       IN MODE "ADD", ISP = ISPDONE HAS NOT BEEN FULLY DONE
!       IT IS RESTARTED HERE
!
      DO ISP = ISPDONE,NSP 
! 
!----------------------------------------------------------------------- 
!       LOCALISING THE ARRIVAL POINT
!----------------------------------------------------------------------- 
!  
        PAS2 = PAS 
!
!       IN MODE "ADD" ITERATIONS ALREADY DONE ARE SKIPPED AND
!                     CHARACTERISTICS GONE IN ANOTHER SUB-DOMAIN SKIPPED                   
!
        IF(ADD) THEN
          IF(ISP.EQ.ISPDONE) GO TO 50
          IF(RECVCHAR(IPLOT)%NEPID.NE.-1) CYCLE 
        ENDIF
!
        IEL = ELT(IPLOT) 
        IET = ETA(IPLOT) 
!!      PERIODICITY IETP1 REPLACES IET+1 (BUT NOT ALWAYS)
        IETP1=ETA1(IET)
!
        I1 = IKLE2(IEL,1) 
        I2 = IKLE2(IEL,2) 
        I3 = IKLE2(IEL,3) 
! 
        DX(IPLOT) = ((U(I1,IET  )*SHP(1,IPLOT)
     &              + U(I2,IET  )*SHP(2,IPLOT) 
     &              + U(I3,IET  )*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT)) 
     &              +(U(I1,IETP1)*SHP(1,IPLOT) 
     &              + U(I2,IETP1)*SHP(2,IPLOT) 
     &              + U(I3,IETP1)*SHP(3,IPLOT))*SHZ(IPLOT) )*PAS 
! 
        DY(IPLOT) = ((V(I1,IET  )*SHP(1,IPLOT) 
     &              + V(I2,IET  )*SHP(2,IPLOT) 
     &              + V(I3,IET  )*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT)) 
     &              +(V(I1,IETP1)*SHP(1,IPLOT) 
     &              + V(I2,IETP1)*SHP(2,IPLOT) 
     &              + V(I3,IETP1)*SHP(3,IPLOT))*SHZ(IPLOT) )*PAS 
!! PERIODICITY, NO DELTA, Z NEVER USED
!       DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT) 
!    &          + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT) 
!    &          + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT) 
! 
!       IF(DELTAZ.GT.EPSDZ) THEN 
!         DIVISION BY DELTAZ IS DUE TO THE FACT THAT W IS  
!         W* MULTIPLIED BY DELTAZ (IT STEMS FROM TRIDW2 IN TELEMAC3D) 
          DZ(IPLOT) = ((W(I1,IET  )*SHP(1,IPLOT)
     &                + W(I2,IET  )*SHP(2,IPLOT) 
     &                + W(I3,IET  )*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT)) 
     &                +(W(I1,IETP1)*SHP(1,IPLOT)
     &                + W(I2,IETP1)*SHP(2,IPLOT) 
     &                + W(I3,IETP1)*SHP(3,IPLOT))*SHZ(IPLOT))
!! PERIODICITY, PAS DE DELTAZ 
!    &                * PAS * (ZSTAR(IET+1)-ZSTAR(IET)) / DELTAZ   
     &                * PAS 
!       ELSE 
!         DZ(IPLOT) = 0.D0 
!       ENDIF 
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
        IF(ADD) THEN
!         CONTINUOUS SETTING OF THE REACHED POSITION FOR IPLOT  
!         AND THE NUMBER OF STEPS DONE ALREADY
          RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
          RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
          RECVCHAR(IPLOT)%ZP=ZPLOT(IPLOT) 
          RECVCHAR(IPLOT)%DX=DX(IPLOT) 
          RECVCHAR(IPLOT)%DY=DY(IPLOT)
          RECVCHAR(IPLOT)%DZ=DZ(IPLOT)
          RECVCHAR(IPLOT)%INE=ELT(IPLOT) 
          RECVCHAR(IPLOT)%ISP=ISP 
        ENDIF
!
!----------------------------------------------------------------------- 
!       TEST: IS THE PATHLINE WENT OUT THE ORIGINAL ELEMENT
!----------------------------------------------------------------------- 
! 
50      CONTINUE 
! 
        ISO = 0
        IF(SHP(1,IPLOT).LT.EPSILO)      ISO=IBSET(ISO,2) 
        IF(SHP(2,IPLOT).LT.EPSILO)      ISO=IBSET(ISO,3) 
        IF(SHP(3,IPLOT).LT.EPSILO)      ISO=IBSET(ISO,4) 
        IF(SHZ(IPLOT)  .LT.EPSILO)      ISO=IBSET(ISO,0) 
        IF(SHZ(IPLOT)  .GT.1.D0-EPSILO) ISO=IBSET(ISO,1) 
!
        IF(ISO.NE.0) THEN
! 
!----------------------------------------------------------------------- 
!         HERE WE ARE OUT OF THE ELEMENT
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
!         PERIODICITY (ALL VALUES ARE THE SAME SO FAR ON THE VERTICAL
!                      BUT WITH PERIODICITY WE MAY HAVE IET=NPLAN AND
!                      IT IS NOT POSSIBLE IN IFABOR).
!         IEL = IBOR(IEL,IFA,IET) 
          IEL = IBOR(IEL,IFA,1)
!         END PERIODICITY
! 
          IF(IFA.LE.3) THEN 
! 
!----------------------------------------------------------------------- 
!           HERE WE ARRIVE IN ANOTHER ELEMENT THROUGH A QUADRANGULAR FACE
!----------------------------------------------------------------------- 
! 
            IF(IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!             RELOCALISING IN ADJACENT ELEMENT
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
              GOTO 50 
! 
            ENDIF 
! 
!----------------------------------------------------------------------- 
!           HERE WE PASS TO A NEIGHBOUR SUBDOMAIN AND COLLECT DATA
!----------------------------------------------------------------------- 
! 
            IF(IEL.EQ.-2) THEN
              IF(.NOT.ADD) THEN
!               INTERFACE CROSSING 
                CALL COLLECT_CHAR  
     &            (IPID,IPLOT,ELT(IPLOT),IFA,ETA(IPLOT),0,ISP,  
     &             NSP,XPLOT(IPLOT),YPLOT(IPLOT),
     &             ZPLOT(IPLOT),0.D0, 
     &             DX(IPLOT),DY(IPLOT),DZ(IPLOT),0.D0,  
     &             IFAPAR,NCHDIM,NCHARA) 
              ELSE 
!               A LOST-AGAIN TRACEBACK DETECTED   
!               PROCESSOR NUMBER   
                RECVCHAR(IPLOT)%NEPID=IFAPAR(IFA,ELT(IPLOT))
                RECVCHAR(IPLOT)%INE=IFAPAR(IFA+3,ELT(IPLOT))  
                RECVCHAR(IPLOT)%KNE=ETA(IPLOT)       
              ENDIF
!             EXITING LOOP ON ISP
              EXIT
            ENDIF  
! 
!----------------------------------------------------------------------- 
!           SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
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
!             HERE SOLID BOUNDARY, VELOCITY IS PROJECTED ON THE BOUNDARY
!             AND WE GO ON
!----------------------------------------------------------------------- 
! 
!  TOMAWAC DIFFERENCES
!
!  STANDARD IMPLEMENTATION
!
!!            A1 = (DXP*DX1+DYP*DY1) / (DX1**2+DY1**2) 
!!            DX(IPLOT) = A1 * DX1 
!!            DY(IPLOT) = A1 * DY1 
! 
!!            A1=((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2) 
!!            SHP(          IFA  ,IPLOT) = 1.D0 - A1 
!!            SHP(     ISUI(IFA) ,IPLOT) = A1 
!!            SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0 
!!            XPLOT(IPLOT) = X(I1) + A1 * DX1 
!!            YPLOT(IPLOT) = Y(I1) + A1 * DY1 
!!            IF(ADD) THEN 
!!              RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
!!              RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
!!              RECVCHAR(IPLOT)%ZP=ZPLOT(IPLOT) 
!!              RECVCHAR(IPLOT)%DX=DX(IPLOT) 
!!              RECVCHAR(IPLOT)%DY=DY(IPLOT)
!!              RECVCHAR(IPLOT)%DZ=DZ(IPLOT)
!!            ENDIF
! 
!!            GOTO 50 
!
!  TOMAWAC IMPLEMENTATION
!
              SHP(1,IPLOT) = 0.D0
              SHP(2,IPLOT) = 0.D0
              SHP(3,IPLOT) = 0.D0
              ELT(IPLOT) = - SENS * ELT(IPLOT)
              EXIT  ! LOOP ON ISP
! 
            ELSEIF(IEL.EQ.0) THEN 
! 
!----------------------------------------------------------------------- 
!           HERE WE HAVE A LIQUID BOUNDARY, THE CHARACTERISTIC IS STOPPED
!----------------------------------------------------------------------- 
! 
            DENOM = DXP*DY1-DYP*DX1 
            IF(ABS(DENOM).GT.1.D-12) THEN 
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
!           THIS IS A MARKER FOR PARTICLES EXITING A DOMAIN
!           SENS=-1 FOR BACKWARD CHARACTERISTICS
            ELT(IPLOT) = - SENS * ELT(IPLOT) 
!           EXITING LOOP ON ISP
            EXIT
!
            ELSE
!
              WRITE(LU,*) 'UNEXPECTED CASE IN SCHAR41'
              WRITE(LU,*) 'IEL=',IEL
              CALL PLANTE(1)
              STOP
!
            ENDIF
! 
          ELSE 
! 
!----------------------------------------------------------------------- 
!  CASE IFA = 4 OR 5  
!  HERE WE EXIT THROUGH TOP OR BOTTOM OF THE PRISM
!----------------------------------------------------------------------- 
! 
            IFA = IFA - 4 
!           HENCE IFA NOW EQUALS 0 OR 1 
! 
            IF(IEL.EQ.1) THEN 
! 
!----------------------------------------------------------------------- 
!           NO NEED TO RECOMPUTE THE VELOCITIES,
!           RELOCALISING IN NEW ELEMENT
!----------------------------------------------------------------------- 
! 
              ETA(IPLOT) = IET + IFA + IFA - 1 
!! PERIODICITY (THIS CAN NEVER HAPPEN WITHOUT PERIODICITY)
              IF(ETA(IPLOT).EQ.NPLAN+1) THEN
                ETA(IPLOT)=1
                ZP=ZP-ZSTAR(NPLAN+1)
                ZPLOT(IPLOT)=ZP
              ENDIF
              IF(ETA(IPLOT).EQ.0) THEN
                ETA(IPLOT) = NPLAN
                ZP=ZP+ZSTAR(NPLAN+1)
                ZPLOT(IPLOT)=ZP
              ENDIF
!! END OF PERIODICITY
              SHZ(IPLOT) = (ZP-ZSTAR(ETA(IPLOT))) 
     &                   / (ZSTAR(ETA(IPLOT)+1)-ZSTAR(ETA(IPLOT))) 
!
              IF(ADD) THEN 
                RECVCHAR(IPLOT)%KNE=ETA(IPLOT)
                RECVCHAR(IPLOT)%ZP=ZP
              ENDIF 
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
!              
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
!               DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT) 
!    &                  + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT) 
!    &                  + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT) 
! 
!               IF(DELTAZ.GT.EPSDZ) THEN 
                  DZ(IPLOT) = ( W(I1,IET2)*SHP(1,IPLOT) 
     &                        + W(I2,IET2)*SHP(2,IPLOT) 
     &                        + W(I3,IET2)*SHP(3,IPLOT) ) * PAS2 
!    &                        * (ZSTAR(IET+1)-ZSTAR(IET)) / DELTAZ 
!               ELSE 
!                 DZ(IPLOT) = 0.D0 
!               ENDIF 
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
!             THIS IS A MARKER FOR PARTICLES EXITING A DOMAIN
!             SENS=-1 FOR BACKWARD CHARACTERISTICS
              ELT(IPLOT) = - SENS * ELT(IPLOT)
!             EXITING LOOP ON ISP 
              EXIT
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
      IF(.NOT.ADD) THEN
        IF(NCSIZE.GT.1) NSPMAX=P_IMAX(NSPMAX) 
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'NOMBRE MAX DE SOUS PAS :',NSPMAX
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'NUMBER OF SUB-ITERATIONS :',NSPMAX
        ENDIF
      ENDIF
!
!     RESTORING ORIGINAL ETA1
!
      ETA1(NPLAN)=NPLAN+1
! 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END SUBROUTINE SCHAR41_PER
!                       ************************* 
                        SUBROUTINE SCHAR41_PER_4D 
!                       ************************* 
! 
     &( U , V , W , F , DT , NRK , X , Y , ZSTAR , FREQ,Z ,IKLE2 ,IBOR , 
     &  XPLOT , YPLOT , ZPLOT  , FPLOT,DX , DY , DZ , DF,
     &  SHP   , SHZ   , SHF    , ELT    , ETA , 
     &  FRE   , NPLOT , NPOIN2 , NELEM2 , NPLAN , NF,SURDET , 
     &  SENS  , IFAPAR, NCHDIM ,NCHARA,ADD) 
! 
!*********************************************************************** 
! BIEF VERSION 6.3           28/04/93     J-M JANIN (LNH) 30 87 72 84 
!                        12/10/05     J-M HERVOUET (LNHE) 01 30 87 80 18 
! 
!brief    LIKE SCHAR41 BUT WITH PERIODICITY ON THE VERTICAL
!         DIFFERENCES MAEKED WITH "PERIODICITY"
!
!
!history  J-M HERVOUET (LNHE)
!+        12/07/2012
!+        V6P3
!+      First version (differences with schar41 taken in Tomawac)             
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
! |                |    | AVEC L'ELEMENT .  SI IBOR<0 OU NUL           | 
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
      INTEGER         , INTENT(IN)    :: SENS,NPLAN,NCHDIM,NF 
      INTEGER         , INTENT(IN)    :: NPOIN2,NELEM2,NPLOT,NRK 
      INTEGER         , INTENT(IN)    :: IKLE2(NELEM2,3) 
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NCHARA 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN,NF) 
      DOUBLE PRECISION, INTENT(IN)    :: V(NPOIN2,NPLAN,NF) 
      DOUBLE PRECISION, INTENT(IN)    :: W(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NPLAN,NF) 
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM2) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: ZPLOT(NPLOT),FPLOT(NPLOT)  
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT),SHZ(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHF(NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2),DT 
!                                                              PERIODICITY                                                              
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN+1) 
      DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: DZ(NPLOT),DF(NPLOT) 
      INTEGER         , INTENT(IN)    :: IBOR(NELEM2,5,NPLAN-1) 
      INTEGER         , INTENT(INOUT) :: ETA(NPLOT),FRE(NPLOT) 
      INTEGER         , INTENT(IN)    :: IFAPAR(6,*) 
      LOGICAL, INTENT(IN)             :: ADD 
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER ISO,ISPDONE,NSP,NSPMAX 
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,ISOH,ISOV,ISOF,ISOT
      INTEGER IETP1,IFA,ISUI(3),IFR
! 
      DOUBLE PRECISION PAS,EPSILO,A1,A2,DX1,DY1,DXP,DYP,DZP,XP,YP,ZP,FP 
      DOUBLE PRECISION DELTAZ,EPSDZ,PAS2,DFP,DENOM 
!
      INTEGER  P_IMAX 
      EXTERNAL P_IMAX
!  
      INTRINSIC ABS , INT , MAX , SQRT 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA EPSILO / -1.D-6 / 
      DATA EPSDZ  /1.D-4/ 
! 
      NSPMAX=1  
      ETA1(NPLAN)=1
! 
!----------------------------------------------------------------------- 
!     FOR EVERY POINT 
!----------------------------------------------------------------------- 
! 
      DO IPLOT = 1 , NPLOT 
!
        IF(ADD) THEN
!
        XPLOT(IPLOT)   = RECVCHAR(IPLOT)%XP  
        YPLOT(IPLOT)   = RECVCHAR(IPLOT)%YP  
        ZPLOT(IPLOT)   = RECVCHAR(IPLOT)%ZP 
        FPLOT(IPLOT)   = RECVCHAR(IPLOT)%FP 
        DX(IPLOT)      = RECVCHAR(IPLOT)%DX  
        DY(IPLOT)      = RECVCHAR(IPLOT)%DY 
        DZ(IPLOT)      = RECVCHAR(IPLOT)%DZ 
        DF(IPLOT)      = RECVCHAR(IPLOT)%DF 
        ELT(IPLOT)     = RECVCHAR(IPLOT)%INE 
        ETA(IPLOT)     = RECVCHAR(IPLOT)%KNE 
        FRE(IPLOT)     = RECVCHAR(IPLOT)%IFR
        NSP            = RECVCHAR(IPLOT)%NSP ! R-K STEPS TO BE FULLFILLED 
        ISPDONE        = RECVCHAR(IPLOT)%ISP ! R-K STEPS ALREADY DONE 
        IEL = ELT(IPLOT) 
        IET = ETA(IPLOT) 
        IFR = FRE(IPLOT) 
        XP  = XPLOT(IPLOT) 
        YP  = YPLOT(IPLOT)  
        ZP  = ZPLOT(IPLOT) 
        FP  = FPLOT(IPLOT)   
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
        SHF(IPLOT) = (FP-FREQ(IFR)) / (FREQ(IFR+1)-FREQ(IFR))
!       ASSUME ALL ARE LOCALISED, IT WILL BE SET OTHERWISE IF LOST-AGAIN 
        RECVCHAR(IPLOT)%NEPID=-1 
!
        ELSE
!
         IEL = ELT(IPLOT) 
!        POINTS WITH IEL=0 ARE TREATED SO THAT THE FINAL
!        INTERPOLATION GIVES 0.,
!        AND WE SKIP TO NEXT POINT IPLOT (CYCLE)
         IF(IEL.EQ.0) THEN
           ELT(IPLOT)=1
           ETA(IPLOT)=1
           FRE(IPLOT)=1
           SHP(1,IPLOT)=0.D0
           SHP(2,IPLOT)=0.D0
           SHP(3,IPLOT)=0.D0
           SHZ(IPLOT)=0.D0
           SHF(IPLOT)=0.D0
           CYCLE   
         ENDIF
         IET = ETA(IPLOT) 
         IFR = FRE(IPLOT)      
         I1 = IKLE2(IEL,1) 
         I2 = IKLE2(IEL,2) 
         I3 = IKLE2(IEL,3) 
!        HERE IET+1 IS ALWAYS < NPLAN+1 (SEE GTSH41) 
         DXP =(1.D0-SHF(IPLOT))*
     &          ((U(I1,IET  ,IFR)*SHP(1,IPLOT)
     &          + U(I2,IET  ,IFR)*SHP(2,IPLOT)
     &          + U(I3,IET  ,IFR)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &          +(U(I1,IET+1,IFR)*SHP(1,IPLOT)
     &          + U(I2,IET+1,IFR)*SHP(2,IPLOT)
     &          + U(I3,IET+1,IFR)*SHP(3,IPLOT))*SHZ(IPLOT))
     &        + SHF(IPLOT)*
     &          ((U(I1,IET  ,IFR+1)*SHP(1,IPLOT)
     &          + U(I2,IET  ,IFR+1)*SHP(2,IPLOT)
     &          + U(I3,IET  ,IFR+1)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &          +(U(I1,IET+1,IFR+1)*SHP(1,IPLOT)
     &          + U(I2,IET+1,IFR+1)*SHP(2,IPLOT)
     &          + U(I3,IET+1,IFR+1)*SHP(3,IPLOT))*SHZ(IPLOT))
!
         DYP =(1.D0-SHF(IPLOT))*
     &          ((V(I1,IET  ,IFR)*SHP(1,IPLOT)
     &          + V(I2,IET  ,IFR)*SHP(2,IPLOT)
     &          + V(I3,IET  ,IFR)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &          +(V(I1,IET+1,IFR)*SHP(1,IPLOT)
     &          + V(I2,IET+1,IFR)*SHP(2,IPLOT)
     &          + V(I3,IET+1,IFR)*SHP(3,IPLOT))*SHZ(IPLOT))
     &        + SHF(IPLOT)*
     &          ((V(I1,IET  ,IFR+1)*SHP(1,IPLOT)
     &          + V(I2,IET  ,IFR+1)*SHP(2,IPLOT)
     &          + V(I3,IET  ,IFR+1)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &          +(V(I1,IET+1,IFR+1)*SHP(1,IPLOT)
     &          + V(I2,IET+1,IFR+1)*SHP(2,IPLOT)
     &          + V(I3,IET+1,IFR+1)*SHP(3,IPLOT))*SHZ(IPLOT))
!
         DZP =(1.D0-SHF(IPLOT))*
     &          ((W(I1,IET  ,IFR)*SHP(1,IPLOT)
     &          + W(I2,IET  ,IFR)*SHP(2,IPLOT)
     &          + W(I3,IET  ,IFR)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &          +(W(I1,IET+1,IFR)*SHP(1,IPLOT)
     &          + W(I2,IET+1,IFR)*SHP(2,IPLOT)
     &          + W(I3,IET+1,IFR)*SHP(3,IPLOT))*SHZ(IPLOT))
     &        + SHF(IPLOT)*
     &          ((W(I1,IET  ,IFR+1)*SHP(1,IPLOT)
     &          + W(I2,IET  ,IFR+1)*SHP(2,IPLOT)
     &          + W(I3,IET  ,IFR+1)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &          +(W(I1,IET+1,IFR+1)*SHP(1,IPLOT)
     &          + W(I2,IET+1,IFR+1)*SHP(2,IPLOT)
     &          + W(I3,IET+1,IFR+1)*SHP(3,IPLOT))*SHZ(IPLOT))
!
         DFP =(1.D0-SHF(IPLOT))*
     &          ((F(I1,IET  ,IFR)*SHP(1,IPLOT)
     &          + F(I2,IET  ,IFR)*SHP(2,IPLOT)
     &          + F(I3,IET  ,IFR)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &          +(F(I1,IET+1,IFR)*SHP(1,IPLOT)
     &          + F(I2,IET+1,IFR)*SHP(2,IPLOT)
     &          + F(I3,IET+1,IFR)*SHP(3,IPLOT))*SHZ(IPLOT))
     &        + SHF(IPLOT)*
     &          ((F(I1,IET  ,IFR+1)*SHP(1,IPLOT)
     &          + F(I2,IET  ,IFR+1)*SHP(2,IPLOT)
     &          + F(I3,IET  ,IFR+1)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &          +(F(I1,IET+1,IFR+1)*SHP(1,IPLOT)
     &          + F(I2,IET+1,IFR+1)*SHP(2,IPLOT)
     &          + F(I3,IET+1,IFR+1)*SHP(3,IPLOT))*SHZ(IPLOT))
!
         NSP=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL))))
         NSP=MAX(NSP,INT(NRK*DT*ABS(DZP/(ZSTAR(IET+1)-ZSTAR(IET)))))
         NSP=MAX(NSP,INT(NRK*DT*ABS(DFP/(FREQ(IFR)-FREQ(IFR+1)))))
         NSPMAX=MAX(NSP,NSPMAX)
!!       END PERIODICITY
!
         ISPDONE=1
!
        ENDIF
! 
        PAS = SENS * DT / NSP
! 
!       LOOP ON RUNGE-KUTTA SUB-STEPS
!
!       COMPILER MUST DO NOTHING IF ISPDONE>NSP
!       IN MODE "ADD", ISP = ISPDONE HAS NOT BEEN FULLY DONE
!       IT IS RESTARTED HERE
!
      DO ISP = ISPDONE,NSP 
! 
!----------------------------------------------------------------------- 
!       LOCALISING THE ARRIVAL POINT
!----------------------------------------------------------------------- 
!  
        PAS2 = PAS 
!
!       IN MODE "ADD" ITERATIONS ALREADY DONE ARE SKIPPED AND
!                     CHARACTERISTICS GONE IN ANOTHER SUB-DOMAIN SKIPPED                   
!
        IF(ADD) THEN
          IF(ISP.EQ.ISPDONE) GO TO 50
          IF(RECVCHAR(IPLOT)%NEPID.NE.-1) CYCLE 
        ENDIF
!
        IEL = ELT(IPLOT) 
        IET = ETA(IPLOT) 
        IFR = FRE(IPLOT)
!!      PERIODICITY IETP1 REPLACES IET+1 (BUT NOT ALWAYS)
        IETP1=ETA1(IET)
        I1 = IKLE2(IEL,1) 
        I2 = IKLE2(IEL,2) 
        I3 = IKLE2(IEL,3) 
!
         DX(IPLOT) = ( (1.D0-SHF(IPLOT))*
     &      ((U(I1,IET  ,IFR)*SHP(1,IPLOT)
     &      + U(I2,IET  ,IFR)*SHP(2,IPLOT)
     &      + U(I3,IET  ,IFR)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &      +(U(I1,IETP1,IFR)*SHP(1,IPLOT)
     &      + U(I2,IETP1,IFR)*SHP(2,IPLOT)
     &      + U(I3,IETP1,IFR)*SHP(3,IPLOT))*SHZ(IPLOT))
     &        + SHF(IPLOT)*
     &      ((U(I1,IET  ,IFR+1)*SHP(1,IPLOT)
     &      + U(I2,IET  ,IFR+1)*SHP(2,IPLOT)
     &      + U(I3,IET  ,IFR+1)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &      +(U(I1,IETP1,IFR+1)*SHP(1,IPLOT)
     &      + U(I2,IETP1,IFR+1)*SHP(2,IPLOT)
     &      + U(I3,IETP1,IFR+1)*SHP(3,IPLOT))*SHZ(IPLOT)))*PAS
!
         DY(IPLOT) = ( (1.D0-SHF(IPLOT))*
     &      ((V(I1,IET  ,IFR)*SHP(1,IPLOT)
     &      + V(I2,IET  ,IFR)*SHP(2,IPLOT)
     &      + V(I3,IET  ,IFR)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &      +(V(I1,IETP1,IFR)*SHP(1,IPLOT)
     &      + V(I2,IETP1,IFR)*SHP(2,IPLOT)
     &      + V(I3,IETP1,IFR)*SHP(3,IPLOT))*SHZ(IPLOT))
     &        + SHF(IPLOT)*
     &      ((V(I1,IET  ,IFR+1)*SHP(1,IPLOT)
     &      + V(I2,IET  ,IFR+1)*SHP(2,IPLOT)
     &      + V(I3,IET  ,IFR+1)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &      +(V(I1,IETP1,IFR+1)*SHP(1,IPLOT)
     &      + V(I2,IETP1,IFR+1)*SHP(2,IPLOT)
     &      + V(I3,IETP1,IFR+1)*SHP(3,IPLOT))*SHZ(IPLOT)))*PAS
!
         DZ(IPLOT) = ( (1.D0-SHF(IPLOT))*
     &      ((W(I1,IET  ,IFR)*SHP(1,IPLOT)
     &      + W(I2,IET  ,IFR)*SHP(2,IPLOT)
     &      + W(I3,IET  ,IFR)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &      +(W(I1,IETP1,IFR)*SHP(1,IPLOT)
     &      + W(I2,IETP1,IFR)*SHP(2,IPLOT)
     &      + W(I3,IETP1,IFR)*SHP(3,IPLOT))*SHZ(IPLOT))
     &        + SHF(IPLOT)*
     &      ((W(I1,IET  ,IFR+1)*SHP(1,IPLOT)
     &      + W(I2,IET  ,IFR+1)*SHP(2,IPLOT)
     &      + W(I3,IET  ,IFR+1)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &      +(W(I1,IETP1,IFR+1)*SHP(1,IPLOT)
     &      + W(I2,IETP1,IFR+1)*SHP(2,IPLOT)
     &      + W(I3,IETP1,IFR+1)*SHP(3,IPLOT))*SHZ(IPLOT)))*PAS
!
         DF(IPLOT) = ( (1.D0-SHF(IPLOT))*
     &      ((F(I1,IET  ,IFR)*SHP(1,IPLOT)
     &      + F(I2,IET  ,IFR)*SHP(2,IPLOT)
     &      + F(I3,IET  ,IFR)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &      +(F(I1,IETP1,IFR)*SHP(1,IPLOT)
     &      + F(I2,IETP1,IFR)*SHP(2,IPLOT)
     &      + F(I3,IETP1,IFR)*SHP(3,IPLOT))*SHZ(IPLOT))
     &        + SHF(IPLOT)*
     &      ((F(I1,IET  ,IFR+1)*SHP(1,IPLOT)
     &      + F(I2,IET  ,IFR+1)*SHP(2,IPLOT)
     &      + F(I3,IET  ,IFR+1)*SHP(3,IPLOT))*(1.D0-SHZ(IPLOT))
     &      +(F(I1,IETP1,IFR+1)*SHP(1,IPLOT)
     &      + F(I2,IETP1,IFR+1)*SHP(2,IPLOT)
     &      + F(I3,IETP1,IFR+1)*SHP(3,IPLOT))*SHZ(IPLOT)) )*PAS
! 
        XP = XPLOT(IPLOT) + DX(IPLOT) 
        YP = YPLOT(IPLOT) + DY(IPLOT) 
        ZP = ZPLOT(IPLOT) + DZ(IPLOT) 
        FP = FPLOT(IPLOT) + DF(IPLOT)
! 
        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                 -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                 -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                 -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
        SHZ(IPLOT) = (ZP-ZSTAR(IET)) / (ZSTAR(IET+1)-ZSTAR(IET))        
        SHF(IPLOT) = (FP-FREQ(IFR)) / (FREQ(IFR+1)-FREQ(IFR))   
! 
        XPLOT(IPLOT) = XP 
        YPLOT(IPLOT) = YP 
        ZPLOT(IPLOT) = ZP 
        FPLOT(IPLOT) = FP
!
        IF(ADD) THEN
!         CONTINUOUS SETTING OF THE REACHED POSITION FOR IPLOT  
!         AND THE NUMBER OF STEPS DONE ALREADY
          RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
          RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
          RECVCHAR(IPLOT)%ZP=ZPLOT(IPLOT) 
          RECVCHAR(IPLOT)%FP=FPLOT(IPLOT)
          RECVCHAR(IPLOT)%DX=DX(IPLOT) 
          RECVCHAR(IPLOT)%DY=DY(IPLOT)
          RECVCHAR(IPLOT)%DZ=DZ(IPLOT)
          RECVCHAR(IPLOT)%DF=DF(IPLOT)
          RECVCHAR(IPLOT)%INE=ELT(IPLOT) 
          RECVCHAR(IPLOT)%ISP=ISP 
        ENDIF
!
!----------------------------------------------------------------------- 
!       TEST: IF THE PATHLINE WENT OUT THE ORIGINAL ELEMENT
!----------------------------------------------------------------------- 
! 
50      CONTINUE 
! 
        ISO = 0
        IF(SHP(1,IPLOT).LT.EPSILO)    ISO=IBSET(ISO,4)
        IF(SHP(2,IPLOT).LT.EPSILO)    ISO=IBSET(ISO,5)
        IF(SHP(3,IPLOT).LT.EPSILO)    ISO=IBSET(ISO,6)
        IF(SHZ(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,0)
        IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1)
        IF(SHF(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,2)
        IF(SHF(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,3)
!   
        IF(ISO.NE.0) THEN
!
!-----------------------------------------------------------------------
!         HERE: WE LEFT THE ELEMENT
!-----------------------------------------------------------------------
!
          ISOT = IAND(ISO, 3)
          ISOF = IAND(ISO,12)/4
          ISOV = IAND(ISO,15)
          ISOH = IAND(ISO,112)
          IEL  = ELT(IPLOT)
          IET  = ETA(IPLOT)
          IFR  = FRE(IPLOT)
          XP   = XPLOT(IPLOT)
          YP   = YPLOT(IPLOT)
          ZP   = ZPLOT(IPLOT)
          FP   = FPLOT(IPLOT)
!       
          IF(ISOH.NE.0) THEN
!
            IF(ISOH.EQ.16) THEN
              IFA = 2
            ELSEIF(ISOH.EQ.32) THEN
              IFA = 3
            ELSEIF(ISOH.EQ.64) THEN
              IFA = 1
            ELSEIF(ISOH.EQ.48) THEN
              IFA = 2
              IF(DX(IPLOT)*(Y(IKLE2(IEL,3))-YP).LT.
     &           DY(IPLOT)*(X(IKLE2(IEL,3))-XP)) IFA = 3
            ELSEIF(ISOH.EQ.96) THEN
              IFA = 3
              IF(DX(IPLOT)*(Y(IKLE2(IEL,1))-YP).LT.
     &           DY(IPLOT)*(X(IKLE2(IEL,1))-XP)) IFA = 1
            ELSE
!             CASE ISOH=80
              IFA = 1
              IF(DX(IPLOT)*(Y(IKLE2(IEL,2))-YP).LT.
     &           DY(IPLOT)*(X(IKLE2(IEL,2))-XP)) IFA = 2  
            ENDIF
!          
            IF(ISOV.GT.0) THEN
              I1 = IKLE2(IEL,IFA)
              I2 = IKLE2(IEL,ISUI(IFA))
              IF(ISOF.GT.0) THEN
      	        IF(ISOT.GT.0) THEN
      	          A1=(FP- FREQ(IFR+ISOF-1))/DF(IPLOT)
      	          A2=(ZP-ZSTAR(IET+ISOT-1))/DZ(IPLOT)
      	          IF(A1.LT.A2) THEN
                    IF((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &                 (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) THEN
                      IFA=ISOF+5
                    ENDIF
      	          ELSE
                    IF((X(I2)-X(I1))*(YP-A2*DY(IPLOT)-Y(I1)).GT.
     &             (Y(I2)-Y(I1))*(XP-A2*DX(IPLOT)-X(I1))) THEN
                      IFA=ISOT+3
                    ENDIF
      		  ENDIF
      	        ELSE
                  A1 = (FP-FREQ(IFR+ISOF-1)) / DF(IPLOT)
                  IF((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &               (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) THEN
                    IFA=ISOF+5
                  ENDIF
      	        ENDIF
      	      ELSE
                A1 = (ZP-ZSTAR(IET+ISOT-1)) / DZ(IPLOT)
                IF((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &             (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) THEN
                  IFA=ISOT+3
                ENDIF
      	      ENDIF
            ENDIF
!
          ELSEIF(ISOT.GT.0) THEN
      	    IFA = ISOT + 3
            IF(ISOF.GT.0) THEN
      	      A1=(FP-FREQ(IFR+ISOF-1))/DF(IPLOT)
      	      A2=(ZP-ZSTAR(IET+ISOT-1))/DZ(IPLOT)
      	      IF(A1.LT.A2) IFA = ISOF + 5
      	    ENDIF
          ELSE
      	    IFA = ISOF + 5
          ENDIF        
!
          IF(IFA.LE.3) THEN
!
!         IEL = IBOR(IEL,IFA,IET)
          IEL = IBOR(IEL,IFA,1)  
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE OF THE PRISM IS A RECTANGULAR FACE
!  =================================================================
!-----------------------------------------------------------------------
!
            IF(IEL.GT.0) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS AN INTERIOR FACE
!  MOVES TO THE ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
              I1 = IKLE2(IEL,1)
              I2 = IKLE2(IEL,2)
              I3 = IKLE2(IEL,3)
!
              ELT(IPLOT) = IEL
              SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                       -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL)
              SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                       -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL)
              SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                       -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)
!
              GOTO 50
!
            ENDIF
! 
!----------------------------------------------------------------------- 
!           HERE WE PASS TO A NEIGHBOUR SUBDOMAIN AND COLLECT DATA
!----------------------------------------------------------------------- 
! 
            IF(IEL.EQ.-2) THEN
              IF(.NOT.ADD) THEN
!               INTERFACE CROSSING 
                CALL COLLECT_CHAR  
     &            (IPID,IPLOT,ELT(IPLOT),IFA,ETA(IPLOT),FRE(IPLOT),ISP,  
     &             NSP,XPLOT(IPLOT),YPLOT(IPLOT),
     &             ZPLOT(IPLOT),FPLOT(IPLOT),  
     &             DX(IPLOT),DY(IPLOT),DZ(IPLOT),DF(IPLOT),  
     &             IFAPAR,NCHDIM,NCHARA) 
              ELSE 
!               A LOST-AGAIN TRACEBACK DETECTED   
!               PROCESSOR NUMBER   
                RECVCHAR(IPLOT)%NEPID=IFAPAR(IFA,ELT(IPLOT))
                RECVCHAR(IPLOT)%INE=IFAPAR(IFA+3,ELT(IPLOT))  
                RECVCHAR(IPLOT)%KNE=ETA(IPLOT) 
                RECVCHAR(IPLOT)%IFR=FRE(IPLOT)      
              ENDIF
!             EXITING LOOP ON ISP
              EXIT
            ENDIF  
! 
!----------------------------------------------------------------------- 
!           EXIT THROUGH TOP OR BOTTOM, OR LIQUID OR SOLID BOUNDARIES
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
!  HERE: THE EXIT FACE IS A SOLID BOUNDARY
!  SETS SHP TO 0, END OF TRACING BACK (PROVISIONAL !!!!!!)
!-----------------------------------------------------------------------
!
              SHP(1,IPLOT) = 0.D0
              SHP(2,IPLOT) = 0.D0
              SHP(3,IPLOT) = 0.D0
              ELT(IPLOT) = - SENS * ELT(IPLOT)
              EXIT   ! LOOP ON ISP
!
            ENDIF
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS A LIQUID BOUNDARY
!  ENDS TRACING BACK (SIGN OF ELT)
!-----------------------------------------------------------------------
!
            DENOM = DXP*DY1-DYP*DX1 
            IF(ABS(DENOM).GT.1.D-12) THEN 
              A1  = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM 
            ELSE 
              A1  = 0.D0 
            ENDIF 
            IF(A1.GT.1.D0) A1 = 1.D0
            IF(A1.LT.0.D0) A1 = 0.D0        
            SHP(IFA            ,IPLOT) = 1.D0 - A1 
            SHP(ISUI(IFA)      ,IPLOT) = A1 
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
            FPLOT(IPLOT) = FP - A1*DF(IPLOT)
            SHF(IPLOT) = (FPLOT(IPLOT)-FREQ(IFR))
     &                       / (FREQ(IFR+1)-FREQ(IFR))
            ELT(IPLOT) = - SENS * ELT(IPLOT)
!           END OF TRACING BACK          
            EXIT   ! LOOP ON ISP
!
          ELSEIF(IFA.LE.5) THEN
!
!         IEL = IBOR(IEL,IFA,IET)
          IEL = IBOR(IEL,IFA,1)  
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE OF THE PRISM IS A TRIANGULAR FACE IN Z
!  =====================================================================
!-----------------------------------------------------------------------
!
            IFA = IFA - 4
!
            IF(IEL.EQ.1) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS AN INTERIOR FACE
!  MOVES TO THE ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
              ETA(IPLOT) = IET + IFA + IFA - 1
      	      IF(ETA(IPLOT).EQ.NPLAN+1) THEN
      		ETA(IPLOT)=1
      		ZP=ZP-ZSTAR(NPLAN+1)
      		ZPLOT(IPLOT)=ZP
              ENDIF
      	      IF(ETA(IPLOT).EQ.0) THEN
      		ETA(IPLOT) = NPLAN
      		ZP=ZP+ZSTAR(NPLAN+1)
      		ZPLOT(IPLOT)=ZP
              ENDIF
              SHZ(IPLOT) = (ZP-ZSTAR(ETA(IPLOT)))
     &                   / (ZSTAR(ETA(IPLOT)+1)-ZSTAR(ETA(IPLOT)))
!
              IF(ADD) THEN 
                RECVCHAR(IPLOT)%KNE=ETA(IPLOT) 
                RECVCHAR(IPLOT)%ZP=ZP
              ENDIF
!
              GO TO 50
!
            ELSE
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'PROBLEME DANS SCHAR41_PER_YA4D',IEL,IPLOT
              ELSE
                WRITE(LU,*) 'PROBLEM IN SCHAR41_PER_YA4D',IEL,IPLOT
              ENDIF
              CALL PLANTE(1)
              STOP
            ENDIF
!
          ELSE
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE OF THE PRISM IS A TRIANGULAR FACE FREQ
!  =====================================================================
!-----------------------------------------------------------------------
!
!           IBOR IS NOT REALLY BUILT FOR IFA = 6 OR 7 BUT IS ALWAYS 1.
!           IEL = IBOR(IEL,IFA,1)  
            IEL=1
            IFA = IFA - 6
!
            IF(IFA.EQ.1.AND.IFR.EQ.NF-1) IEL=-1
            IF(IFA.EQ.0.AND.IFR.EQ.1)    IEL=-1
            IF(IEL.EQ.1) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS AN INTERIOR FACE
!  MOVES TO THE ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
              FRE(IPLOT) = IFR + IFA + IFA - 1
              SHF(IPLOT) = (FP-FREQ(FRE(IPLOT)))
     &                  / (FREQ(FRE(IPLOT)+1)-FREQ(FRE(IPLOT)))
!
              IF(ADD) THEN 
                RECVCHAR(IPLOT)%IFR=FRE(IPLOT) 
              ENDIF
!
              GOTO 50
!
            ELSE
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS THE MIN OR MAX FREQUENCY
!  PROJECTS THE REMAINING PART ON THE BOUNDARY AND CONTINUES
!-----------------------------------------------------------------------
!
              FPLOT(IPLOT)=FREQ(IFR+IFA)
      	      DF(IPLOT)=0.D0
      	      SHF(IPLOT)=IFA
      	      ISO = ISOH +ISOT
      	      IF(ISO.NE.0) THEN
      	        GO TO 50
      	      ENDIF
!
            ENDIF
!
          ENDIF
!
!       IF(ISO.NE.0) THEN
        ENDIF
! 
        ENDDO 
      ENDDO
!
      IF(.NOT.ADD) THEN
        IF(NCSIZE.GT.1) NSPMAX=P_IMAX(NSPMAX) 
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'NOMBRE MAX DE SOUS PAS :',NSPMAX
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'NUMBER OF SUB-ITERATIONS :',NSPMAX
        ENDIF
      ENDIF
!
!     RESTORING ORIGINAL ETA1
!
      ETA1(NPLAN)=NPLAN+1 
! 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END SUBROUTINE SCHAR41_PER_4D 
! 
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
     &(U,V,DT,NRK,X,Y,IKLE,IFABOR,XPLOT,YPLOT,DX,DY,SHP,ELT, 
     & NPLOT,NPOIN,NELEM,NELMAX,SURDET,SENS, 
     & IFAPAR,MESH,NCHDIM,NCHARA,ADD) 
! 
!*********************************************************************** 
! BIEF VERSION 6.2           24/04/97    J-M JANIN (LNH) 30 87 72 84 
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
! |    DT          | -->| PAS DE TEMPS.                                 
! |    NRK         | -->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.            
! |    X,Y         | -->| COORDONNEES DES POINTS DU MAILLAGE.           
! |    IKLE        | -->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE     
! |                |    | ET GLOBALE.                                   
! |    IFABOR      | -->| NUMEROS DES ELEMENTS AYANT UNE FACE COMMUNE   
! |                |    | AVEC L'ELEMENT .  SI IFABOR<0 OU NUL          
! |                |    | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE    
! |  XPLOT,YPLOT   |<-->| POSITIONS SUCCESSIVES DES DERIVANTS.          
! |    DX,DY       | -- | STOCKAGE DES SOUS-PAS . | 
! |    SHP         |<-->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES     
! |                |    | COURBES CARACTERISTIQUES.                     
! |    ELT         |<-->| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES   
! |                |    | CARACTERISTIQUES.                                        
! |    NPLOT       | -->| NOMBRE DE DERIVANTS.                          
! |    NPOIN       | -->| NOMBRE DE POINTS DU MAILLAGE.                 
! |    NELEM       | -->| NOMBRE D'ELEMENTS.                            
! |    NELMAX      | -->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D 
! |    SURDET      | -->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM. 
! |    SENS        | -->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.    
! |________________|____|______________________________________________| 
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE) 
!----------------------------------------------------------------------- 
!     - APPELE PAR : CARACT , DERIVE , DERLAG 
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
      INTEGER         , INTENT(IN)    :: SENS,NCHDIM 
      INTEGER         , INTENT(IN)    :: NPOIN,NELEM,NELMAX,NPLOT,NRK 
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,3),IFABOR(NELMAX,3) 
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NCHARA 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),SURDET(NELEM) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: DT 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT) 
      INTEGER, INTENT(IN)             :: IFAPAR(6,*) 
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL, INTENT(IN)             :: ADD
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
!      
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,ISO,IFA,ISUI(3),ISUI2(3),ISPDONE
      INTEGER IPROC,ILOC,NSP
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,DENOM 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA ISUI2  / 3 , 1 , 2 / 
      DATA EPSILO / -1.D-6 / 
! 
      INTRINSIC INT,MAX,MIN,SQRT 
!
!----------------------------------------------------------------------- 
!     FOR EVERY POINT 
!----------------------------------------------------------------------- 
! 
      DO IPLOT=1,NPLOT 
! 
        IF(ADD) THEN
!
          XPLOT(IPLOT)   = RECVCHAR(IPLOT)%XP  
          YPLOT(IPLOT)   = RECVCHAR(IPLOT)%YP 
          DX(IPLOT)      = RECVCHAR(IPLOT)%DX  
          DY(IPLOT)      = RECVCHAR(IPLOT)%DY   
          ELT(IPLOT)     = RECVCHAR(IPLOT)%INE 
          NSP            = RECVCHAR(IPLOT)%NSP ! R-K STEPS TO BE FULLFILLED 
          ISPDONE        = RECVCHAR(IPLOT)%ISP ! R-K STEPS ALREADY DONE  
          IEL = ELT(IPLOT) 
          XP  = XPLOT(IPLOT) 
          YP  = YPLOT(IPLOT)  
          I1 = IKLE(IEL,1) 
          I2 = IKLE(IEL,2) 
          I3 = IKLE(IEL,3) 
          SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
          SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
          SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)      
!         ASSUME TO BE LOCALISED IT WILL BE SET OTHERWISE IF LOST-AGAIN  
          RECVCHAR(IPLOT)%NEPID=-1 
!     
        ELSE 
          IEL = ELT(IPLOT) 
!         POINTS WITH IEL=0 ARE TREATED SO THAT THE FINAL INTERPOLATION
!         GIVES 0., AND WE SKIP TO NEXT POINT IPLOT (CYCLE) 
!         THIS WILL NOT INTERFERE WITH ELT(IPLOT)=0 GIVEN ON LIQUID BOUNDARIES
!         BY ARRAY IFABOR, THAT MAY HAPPEN LATER    
          IF(IEL.EQ.0) THEN
            ELT(IPLOT)=1
            SHP(1,IPLOT)=0.D0
            SHP(2,IPLOT)=0.D0
            SHP(3,IPLOT)=0.D0
            CYCLE      
          ENDIF
          I1 = IKLE(IEL,1) 
          I2 = IKLE(IEL,2) 
          I3 = IKLE(IEL,3) 
          DXP = U(I1)*SHP(1,IPLOT)+U(I2)*SHP(2,IPLOT) 
     &                            +U(I3)*SHP(3,IPLOT) 
          DYP = V(I1)*SHP(1,IPLOT)+V(I2)*SHP(2,IPLOT) 
     &                            +V(I3)*SHP(3,IPLOT) 
          NSP=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL))))   
          ISPDONE=1
        ENDIF
!
        PAS = SENS * DT / NSP 
! 
!       LOOP ON RUNGE-KUTTA SUB-STEPS
!
!       COMPILER MUST DO NOTHING IF ISPDONE>NSP
!       IN MODE "ADD", ISP = ISPDONE HAS NOT BEEN FULLY DONE
!       IT IS RESTARTED HERE
!
        DO ISP=ISPDONE,NSP 
!
!----------------------------------------------------------------------- 
!       LOCALISING THE ARRIVAL POINT
!----------------------------------------------------------------------- 
! 
!       IN MODE "ADD" ITERATIONS ALREADY DONE ARE SKIPPED AND
!                     CHARACTERISTICS GONE IN ANOTHER SUB-DOMAIN SKIPPED                   
!
        IF(ADD) THEN
          IF(ISP.EQ.ISPDONE) GO TO 50
          IF(RECVCHAR(IPLOT)%NEPID.NE.-1) CYCLE 
        ENDIF
!                       
        IEL = ELT(IPLOT) 
        I1 = IKLE(IEL,1) 
        I2 = IKLE(IEL,2) 
        I3 = IKLE(IEL,3) 
! 
        DX(IPLOT) = ( U(I1)*SHP(1,IPLOT) 
     &              + U(I2)*SHP(2,IPLOT) 
     &              + U(I3)*SHP(3,IPLOT) ) * PAS 
        DY(IPLOT) = ( V(I1)*SHP(1,IPLOT) 
     &              + V(I2)*SHP(2,IPLOT) 
     &              + V(I3)*SHP(3,IPLOT) ) * PAS 
        XP = XPLOT(IPLOT) + DX(IPLOT) 
        YP = YPLOT(IPLOT) + DY(IPLOT) 
! 
        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                 -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                 -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                 -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
! 
        XPLOT(IPLOT) = XP 
        YPLOT(IPLOT) = YP 
!
        IF(ADD) THEN
!         CONTINUOUS SETTING OF THE REACHED POSITION FOR IPLOT  
!         AND THE NUMBER OF STEPS DONE ALREADY   
          RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
          RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
          RECVCHAR(IPLOT)%DX=DX(IPLOT) 
          RECVCHAR(IPLOT)%DY=DY(IPLOT)
          RECVCHAR(IPLOT)%INE=ELT(IPLOT) 
          RECVCHAR(IPLOT)%ISP=ISP
        ENDIF 
! 
!----------------------------------------------------------------------- 
!       TEST: IS THE PATHLINE WENT OUT THE ORIGINAL ELEMENT
!----------------------------------------------------------------------- 
! 
50      CONTINUE 
! 
        ISO = 0 
        IF(SHP(1,IPLOT).LT.EPSILO) ISO = 1 
        IF(SHP(2,IPLOT).LT.EPSILO) ISO = ISO + 2 
        IF(SHP(3,IPLOT).LT.EPSILO) ISO = ISO + 4 
! 
        IF(ISO.NE.0) THEN 
! 
!----------------------------------------------------------------------- 
!         HERE WE ARE OUT OF THE ELEMENT
!----------------------------------------------------------------------- 
! 
          IEL = ELT(IPLOT) 
          XP = XPLOT(IPLOT) 
          YP = YPLOT(IPLOT) 
!
!         THE 3 LINES FORMING THE TRIANGLE CUT THE PLANE INTO 7
!         ZONES, NUMBERED FROM 0 (INSIDE THE TRIANGLE) TO 6
!         ISO IS THE NUMBER. FOR ISO =1,2,4, THERE IS NO AMBIGUITY
!         AS TO THE EDGE CROSSED. FOR ISO = 3, IT CAN BE EDGE 2
!         OR 3, FOR ISO = 5 IT CAN BE EDGE 1 OR 2, FOR ISO = 6 IT
!         CAN BE EDGE 1 OR 3.
!         FOR CASES 3, 5 AND 6, AN INNER PRODUCT SHOWS IF THE DIRECTION
!         OF THE DISPLACEMENT (DX,DY) IS ON THE RIGHT OR ON THE LEFT
!         OF THE INTERSECTION BETWEEN THE TWO EDGES, SO IT GIVES
!         THE REAL EDGE THAT HAS BEEN CROSSED
!
          IF(ISO.EQ.1) THEN 
            IFA = 2 
          ELSEIF (ISO.EQ.2) THEN 
            IFA = 3 
          ELSEIF (ISO.EQ.4) THEN 
            IFA = 1 
          ELSEIF (ISO.EQ.3) THEN 
            IFA = 2 
            IF(DX(IPLOT)*(Y(IKLE(IEL,3))-YP).LT. 
     &         DY(IPLOT)*(X(IKLE(IEL,3))-XP)) IFA = 3 
          ELSEIF (ISO.EQ.6) THEN 
            IFA = 3 
            IF (DX(IPLOT)*(Y(IKLE(IEL,1))-YP).LT. 
     &          DY(IPLOT)*(X(IKLE(IEL,1))-XP)) IFA = 1 
          ELSE
!           HERE CASE ISO=5 
            IFA = 1 
            IF(DX(IPLOT)*(Y(IKLE(IEL,2))-YP).LT. 
     &         DY(IPLOT)*(X(IKLE(IEL,2))-XP)) IFA = 2 
          ENDIF 
! 
          IEL = IFABOR(IEL,IFA) 
! 
          IF(IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!           HERE WE ARRIVE IN ANOTHER ELEMENT
!----------------------------------------------------------------------- 
! 
            I1 = IKLE(IEL,1) 
            I2 = IKLE(IEL,2) 
            I3 = IKLE(IEL,3) 
! 
            ELT(IPLOT) = IEL 
            SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                     -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
            SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                     -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
            SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                     -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
            GOTO 50 
! 
          ENDIF
!
!----------------------------------------------------------------------- 
!         HERE WE PASS TO NEIGHBOUR SUBDOMAIN AND COLLECT DATA 
!----------------------------------------------------------------------- 
! 
          IF(IEL.EQ.-2) THEN   
            IF(ADD) THEN  
!             A LOST-AGAIN TRACEBACK DETECTED, ALREADY HERE    
!             SET THE IMPLANTING PARAMETERS  
              IPROC=IFAPAR(IFA  ,ELT(IPLOT)) 
              ILOC =IFAPAR(IFA+3,ELT(IPLOT))
!             ANOTHER ONE AS IPID, MEANS ALSO NOT LOCALISED  
              RECVCHAR(IPLOT)%NEPID=IPROC  
              RECVCHAR(IPLOT)%INE=ILOC 
            ELSE
              CALL COLLECT_CHAR(IPID,IPLOT,ELT(IPLOT),IFA,0,0,ISP,NSP,
     &                          XPLOT(IPLOT),YPLOT(IPLOT),0.D0,0.D0,
     &                          DX(IPLOT),DY(IPLOT),0.D0,0.D0,
     &                          IFAPAR,NCHDIM,NCHARA)                
            ENDIF
!           EXITING LOOP ON ISP 
            EXIT   
          ENDIF 
! 
!----------------------------------------------------------------------- 
!         SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
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
!           HERE SOLID BOUNDARY, VELOCITY IS PROJECTED ON THE BOUNDARY
!           AND WE GO ON
!----------------------------------------------------------------------- 
! 
!           HERE A1 IS THE PARAMETRIC COORDINATE OF THE PROJECTED
!           DISPLACEMENT ON SEGMENT I1----I2
!
            A1 = (DXP*DX1 + DYP*DY1) / (DX1**2 + DY1**2) 
!
!           THE TOTAL DISPLACEMENT IS PROJECTED HERE, NOT THE REMAINING
!           PART, BUT ONLY THE DIRECTION WILL BE USED
            DX(IPLOT) = A1 * DX1 
            DY(IPLOT) = A1 * DY1 
! 
!           NOW A1 IS THE PARAMETRIC COORDINATE ON SEGMENT I1----I2
!           OF THE POSITION OF THE ARRIVAL POINT, I.E. INTERSECTION
!           + REMAINING DISPLACEMENT PROJECTED ON THE SEGMENT
!           ITS VALUE MAY BE OUTSIDE THE RANGE (0,1). THE VALUE OF A1
!           SIMPLIFIES INTO THE FOLLOWING FORMULA, BECAUSE IT IS
!           SIMPPLY VECTOR I1----P PROJECTED ON SEGMENT I1----I2
!
            A1 = ((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2) 
            SHP(      IFA ,IPLOT) = 1.D0 - A1 
            SHP( ISUI(IFA),IPLOT) = A1 
            SHP(ISUI2(IFA),IPLOT) = 0.D0 
            XPLOT(IPLOT) = X(I1) + A1 * DX1 
            YPLOT(IPLOT) = Y(I1) + A1 * DY1
            IF(ADD) THEN 
              RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
              RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
              RECVCHAR(IPLOT)%DX=DX(IPLOT) 
              RECVCHAR(IPLOT)%DY=DY(IPLOT)
            ENDIF  
! 
            GOTO 50 
! 
          ELSEIF(IEL.EQ.0) THEN 
! 
!------------------------------------------------------------------------ 
!           HERE WE HAVE A LIQUID BOUNDARY, THE CHARACTERISTIC IS STOPPED
!------------------------------------------------------------------------ 
! 
            DENOM = DXP*DY1-DYP*DX1 
            IF(ABS(DENOM).GT.1.D-12) THEN 
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
!           THIS IS A MARKER FOR PARTICLES EXITING A DOMAIN
!           SENS=-1 FOR BACKWARD CHARACTERISTICS
            ELT(IPLOT) = - SENS * ELT(IPLOT)          
            EXIT
!
          ELSE
!
            WRITE(LU,*) 'UNEXPECTED CASE IN SCHAR11'
            WRITE(LU,*) 'IEL=',IEL
            CALL PLANTE(1)
            STOP
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
      END SUBROUTINE SCHAR11
!                       ****************** 
                        SUBROUTINE SCHAR12 
!                       ****************** 
! 
     &(U,V,DT,NRK,X,Y,IKLE,IFABOR,XPLOT,YPLOT,DX,DY,SHP,ELT, 
     & NPLOT,NPOIN,NELEM,NELMAX,SURDET,SENS, 
     & IFAPAR,MESH,NCHDIM,NCHARA,ADD) 
! 
!*********************************************************************** 
! BIEF VERSION 6.2           24/04/97    J-M JANIN (LNH) 30 87 72 84 
! 
!*********************************************************************** 
! 
!  FONCTION : THIS IS A MERE COPY OF SCHAR11, EXCEPT THE INTERPOLATION
!             OF VELOCITY WHICH IS HERE CONSIDERED QUASI-BUBBLE
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
! |    NPLOT       | -->| NOMBRE DE DERIVANTS.                         | 
! |    NPOIN       | -->| NOMBRE DE POINTS DU MAILLAGE.                | 
! |    NELEM       | -->| NOMBRE D'ELEMENTS.                           | 
! |    NELMAX      | -->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D| 
! |    SURDET      | -->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM. 
! |    SENS        | -->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.   | 
! |________________|____|______________________________________________| 
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE) 
!----------------------------------------------------------------------- 
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
      INTEGER         , INTENT(IN)    :: SENS,NCHDIM 
      INTEGER         , INTENT(IN)    :: NPOIN,NELEM,NELMAX,NPLOT,NRK 
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,6),IFABOR(NELMAX,3) 
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NCHARA 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),SURDET(NELEM) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: DT 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT) 
      INTEGER, INTENT(IN)             :: IFAPAR(6,*) 
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL, INTENT(IN)             :: ADD 
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
!      
      INTEGER IPLOT,ISP,I1,I2,I3,I4,I5,I6,IEL,ISO,IFA,ISUI(3),ISUI2(3)
      INTEGER IPROC,ILOC,ISPDONE,NSP
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,DENOM
      DOUBLE PRECISION SHP11,SHP12,SHP14
      DOUBLE PRECISION SHP22,SHP23,SHP24
      DOUBLE PRECISION SHP33,SHP31,SHP34 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA ISUI2  / 3 , 1 , 2 / 
      DATA EPSILO / -1.D-6 / 
! 
      INTRINSIC INT,MAX,MIN,SQRT 
!
!----------------------------------------------------------------------- 
!     FOR EVERY POINT 
!----------------------------------------------------------------------- 
! 
      DO IPLOT=1,NPLOT 
! 
        IF(ADD) THEN
!
          XPLOT(IPLOT)   = RECVCHAR(IPLOT)%XP  
          YPLOT(IPLOT)   = RECVCHAR(IPLOT)%YP  
          DX(IPLOT)      = RECVCHAR(IPLOT)%DX  
          DY(IPLOT)      = RECVCHAR(IPLOT)%DY    
          ELT(IPLOT)     = RECVCHAR(IPLOT)%INE 
          NSP            = RECVCHAR(IPLOT)%NSP ! R-K STEPS TO BE FULLFILLED 
          ISPDONE        = RECVCHAR(IPLOT)%ISP ! R-K STEPS ALREADY DONE  
          IEL = ELT(IPLOT) 
          XP  = XPLOT(IPLOT) 
          YP  = YPLOT(IPLOT)  
          I1 = IKLE(IEL,1) 
          I2 = IKLE(IEL,2) 
          I3 = IKLE(IEL,3) 
          SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
          SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
          SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)      
!         ASSUME TO BE LOCALISED IT WILL BE SET OTHERWISE IF LOST-AGAIN  
          RECVCHAR(IPLOT)%NEPID=-1 
!     
        ELSE 
          IEL = ELT(IPLOT) 
!         POINTS WITH IEL=0 ARE TREATED SO THAT THE FINAL INTERPOLATION
!         GIVES 0., AND WE SKIP TO NEXT POINT IPLOT (CYCLE) 
!         THIS WILL NOT INTERFERE WITH ELT(IPLOT)=0 GIVEN ON LIQUID BOUNDARIES
!         BY ARRAY IFABOR, THAT MAY HAPPEN LATER    
          IF(IEL.EQ.0) THEN
            ELT(IPLOT)=1
            SHP(1,IPLOT)=0.D0
            SHP(2,IPLOT)=0.D0
            SHP(3,IPLOT)=0.D0
            CYCLE      
          ENDIF
          SHP11=SHP(1,IPLOT)-SHP(3,IPLOT)
          SHP12=SHP(2,IPLOT)-SHP(3,IPLOT)
          SHP14=3.D0*SHP(3,IPLOT)
          SHP22=SHP(2,IPLOT)-SHP(1,IPLOT)
          SHP23=SHP(3,IPLOT)-SHP(1,IPLOT)
          SHP24=3.D0*SHP(1,IPLOT)
          SHP33=SHP(3,IPLOT)-SHP(2,IPLOT)
          SHP31=SHP(1,IPLOT)-SHP(2,IPLOT)
          SHP34=3.D0*SHP(2,IPLOT)
          IF(     SHP11.GT.     2.D0*EPSILO .AND. 
     &            SHP11.LT.1.D0-4.D0*EPSILO .AND.
     &            SHP12.GT.     2.D0*EPSILO .AND. 
     &            SHP12.LT.1.D0-4.D0*EPSILO .AND.
     &            SHP14.LT.1.D0-4.D0*EPSILO ) THEN
            DXP = U(IKLE(IEL,1)) * SHP11
     &          + U(IKLE(IEL,2)) * SHP12
     &          + U(IKLE(IEL,4)) * SHP14
            DYP = V(IKLE(IEL,1)) * SHP11
     &          + V(IKLE(IEL,2)) * SHP12
     &          + V(IKLE(IEL,4)) * SHP14
          ELSEIF( SHP22.GT.     2.D0*EPSILO .AND. 
     &            SHP22.LT.1.D0-4.D0*EPSILO .AND.
     &            SHP23.GT.     2.D0*EPSILO .AND. 
     &            SHP23.LT.1.D0-4.D0*EPSILO .AND.
     &            SHP24.LT.1.D0-4.D0*EPSILO ) THEN
            DXP = U(IKLE(IEL,2)) * SHP22
     &          + U(IKLE(IEL,3)) * SHP23
     &          + U(IKLE(IEL,4)) * SHP24
            DYP = V(IKLE(IEL,2)) * SHP22
     &          + V(IKLE(IEL,3)) * SHP23
     &          + V(IKLE(IEL,4)) * SHP24
          ELSEIF( SHP33.GT.     2.D0*EPSILO .AND. 
     &            SHP33.LT.1.D0-4.D0*EPSILO .AND.
     &            SHP31.GT.     2.D0*EPSILO .AND. 
     &            SHP31.LT.1.D0-4.D0*EPSILO .AND.
     &            SHP34.LT.1.D0-4.D0*EPSILO ) THEN
            DXP = U(IKLE(IEL,3)) * SHP33
     &          + U(IKLE(IEL,1)) * SHP31
     &          + U(IKLE(IEL,4)) * SHP34
            DYP = V(IKLE(IEL,3)) * SHP33
     &          + V(IKLE(IEL,1)) * SHP31
     &          + V(IKLE(IEL,4)) * SHP34
          ELSE
            WRITE(LU,*) 'SCHAR12: POINT ',IPLOT
            WRITE(LU,*) '         NOT IN ELEMENT ',IEL
            WRITE(LU,*) 'SHP(1,IPLOT)=',SHP(1,IPLOT)
            WRITE(LU,*) 'SHP(2,IPLOT)=',SHP(2,IPLOT)
            WRITE(LU,*) 'SHP(3,IPLOT)=',SHP(3,IPLOT)
            WRITE(LU,*) 'EPSILO=',EPSILO,' IPID=',IPID,' CASE 1'
            WRITE(LU,*) 'SHP11=',SHP11
            WRITE(LU,*) 'SHP12=',SHP12
            WRITE(LU,*) 'SHP14=',SHP14
            WRITE(LU,*) 'SHP22=',SHP22
            WRITE(LU,*) 'SHP23=',SHP23
            WRITE(LU,*) 'SHP24=',SHP24
            WRITE(LU,*) 'SHP33=',SHP33
            WRITE(LU,*) 'SHP31=',SHP31
            WRITE(LU,*) 'SHP34=',SHP34
            CALL PLANTE(1)
            STOP
          ENDIF
          NSP=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL))))   
          ISPDONE=1
        ENDIF
!
        PAS = SENS*DT/NSP 
! 
!       LOOP ON RUNGE-KUTTA SUB-STEPS
!
!       COMPILER MUST DO NOTHING IF ISPDONE>NSP
!       IN MODE "ADD", ISP = ISPDONE HAS NOT BEEN FULLY DONE
!       IT IS RESTARTED HERE
!
        DO ISP=ISPDONE,NSP 
!
!----------------------------------------------------------------------- 
!       LOCALISING THE ARRIVAL POINT
!----------------------------------------------------------------------- 
! 
!       IN MODE "ADD" ITERATIONS ALREADY DONE ARE SKIPPED AND
!                     CHARACTERISTICS GONE IN ANOTHER SUB-DOMAIN SKIPPED                   
!
        IF(ADD) THEN
          IF(ISP.EQ.ISPDONE) GO TO 50
          IF(RECVCHAR(IPLOT)%NEPID.NE.-1) CYCLE 
        ENDIF
!                       
        IEL = ELT(IPLOT) 
        I1 = IKLE(IEL,1) 
        I2 = IKLE(IEL,2) 
        I3 = IKLE(IEL,3) 
!
        SHP11=SHP(1,IPLOT)-SHP(3,IPLOT)
        SHP12=SHP(2,IPLOT)-SHP(3,IPLOT)
        SHP14=3.D0*SHP(3,IPLOT)
        SHP22=SHP(2,IPLOT)-SHP(1,IPLOT)
        SHP23=SHP(3,IPLOT)-SHP(1,IPLOT)
        SHP24=3.D0*SHP(1,IPLOT)
        SHP33=SHP(3,IPLOT)-SHP(2,IPLOT)
        SHP31=SHP(1,IPLOT)-SHP(2,IPLOT)
        SHP34=3.D0*SHP(2,IPLOT)
        IF(     SHP11.GT.     2.D0*EPSILO .AND. 
     &          SHP11.LT.1.D0-4.D0*EPSILO .AND.
     &          SHP12.GT.     2.D0*EPSILO .AND. 
     &          SHP12.LT.1.D0-4.D0*EPSILO .AND.
     &          SHP14.LT.1.D0-4.D0*EPSILO ) THEN
          DX(IPLOT) = ( U(IKLE(IEL,1)) * SHP11
     &                + U(IKLE(IEL,2)) * SHP12
     &                + U(IKLE(IEL,4)) * SHP14 ) * PAS
          DY(IPLOT) = ( V(IKLE(IEL,1)) * SHP11
     &                + V(IKLE(IEL,2)) * SHP12
     &                + V(IKLE(IEL,4)) * SHP14 ) * PAS
        ELSEIF( SHP22.GT.     2.D0*EPSILO .AND. 
     &          SHP22.LT.1.D0-4.D0*EPSILO .AND.
     &          SHP23.GT.     2.D0*EPSILO .AND. 
     &          SHP23.LT.1.D0-4.D0*EPSILO .AND.
     &          SHP24.LT.1.D0-4.D0*EPSILO ) THEN
          DX(IPLOT) = ( U(IKLE(IEL,2)) * SHP22
     &                + U(IKLE(IEL,3)) * SHP23
     &                + U(IKLE(IEL,4)) * SHP24 ) * PAS
          DY(IPLOT) = ( V(IKLE(IEL,2)) * SHP22
     &                + V(IKLE(IEL,3)) * SHP23
     &                + V(IKLE(IEL,4)) * SHP24 ) * PAS
        ELSEIF( SHP33.GT.     2.D0*EPSILO .AND. 
     &          SHP33.LT.1.D0-4.D0*EPSILO .AND.
     &          SHP31.GT.     2.D0*EPSILO .AND. 
     &          SHP31.LT.1.D0-4.D0*EPSILO .AND.
     &          SHP34.LT.1.D0-4.D0*EPSILO ) THEN
          DX(IPLOT) = ( U(IKLE(IEL,3)) * SHP33
     &                + U(IKLE(IEL,1)) * SHP31
     &                + U(IKLE(IEL,4)) * SHP34 ) * PAS
          DY(IPLOT) = ( V(IKLE(IEL,3)) * SHP33
     &                + V(IKLE(IEL,1)) * SHP31
     &                + V(IKLE(IEL,4)) * SHP34 ) * PAS
        ELSE
          WRITE(LU,*) 'SCHAR12: POINT ',IPLOT
          WRITE(LU,*) '         NOT IN ELEMENT ',ELT(IPLOT)
          WRITE(LU,*) 'SHP(1,IPLOT)=',SHP(1,IPLOT)
          WRITE(LU,*) 'SHP(2,IPLOT)=',SHP(2,IPLOT)
          WRITE(LU,*) 'SHP(3,IPLOT)=',SHP(3,IPLOT)
          WRITE(LU,*) 'EPSILO=',EPSILO,' IPID=',IPID,' CASE 2'
          CALL PLANTE(1)
          STOP
        ENDIF
        XP = XPLOT(IPLOT) + DX(IPLOT) 
        YP = YPLOT(IPLOT) + DY(IPLOT) 
! 
        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                 -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                 -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                 -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
! 
        XPLOT(IPLOT) = XP 
        YPLOT(IPLOT) = YP 
!
        IF(ADD) THEN
!         CONTINUOUS SETTING OF THE REACHED POSITION FOR IPLOT  
!         AND THE NUMBER OF STEPS DONE ALREADY   
          RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
          RECVCHAR(IPLOT)%YP=YPLOT(IPLOT)
          RECVCHAR(IPLOT)%DX=DX(IPLOT) 
          RECVCHAR(IPLOT)%DY=DY(IPLOT)  
          RECVCHAR(IPLOT)%INE=ELT(IPLOT) 
          RECVCHAR(IPLOT)%ISP=ISP
        ENDIF 
! 
!----------------------------------------------------------------------- 
!       TEST: IS THE PATHLINE WENT OUT THE ORIGINAL ELEMENT
!----------------------------------------------------------------------- 
! 
50      CONTINUE 
! 
        ISO = 0 
        IF(SHP(1,IPLOT).LT.EPSILO) ISO = 1 
        IF(SHP(2,IPLOT).LT.EPSILO) ISO = ISO + 2 
        IF(SHP(3,IPLOT).LT.EPSILO) ISO = ISO + 4 
! 
        IF(ISO.NE.0) THEN 
! 
!----------------------------------------------------------------------- 
!         HERE WE ARE OUT OF THE ELEMENT
!----------------------------------------------------------------------- 
! 
          IEL = ELT(IPLOT) 
          XP = XPLOT(IPLOT) 
          YP = YPLOT(IPLOT) 
! 
          IF(ISO.EQ.1) THEN 
            IFA = 2 
          ELSEIF (ISO.EQ.2) THEN 
            IFA = 3 
          ELSEIF (ISO.EQ.4) THEN 
            IFA = 1 
          ELSEIF (ISO.EQ.3) THEN 
            IFA = 2 
            IF(DX(IPLOT)*(Y(IKLE(IEL,3))-YP).LT. 
     &                DY(IPLOT)*(X(IKLE(IEL,3))-XP)) IFA = 3 
            ELSEIF (ISO.EQ.6) THEN 
              IFA = 3 
              IF (DX(IPLOT)*(Y(IKLE(IEL,1))-YP).LT. 
     &            DY(IPLOT)*(X(IKLE(IEL,1))-XP)) IFA = 1 
            ELSE 
              IFA = 1 
              IF(DX(IPLOT)*(Y(IKLE(IEL,2))-YP).LT. 
     &           DY(IPLOT)*(X(IKLE(IEL,2))-XP)) IFA = 2 
            ENDIF 
! 
            IEL = IFABOR(IEL,IFA) 
! 
            IF(IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!             HERE WE ARRIVE IN ANOTHER ELEMENT
!----------------------------------------------------------------------- 
! 
              I1 = IKLE(IEL,1) 
              I2 = IKLE(IEL,2) 
              I3 = IKLE(IEL,3) 
! 
              ELT(IPLOT) = IEL 
              SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                       -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
              SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                       -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
              SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                       -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
              GOTO 50 
! 
            ENDIF
!
!----------------------------------------------------------------------- 
!           HERE WE PASS TO NEIGHBOUR SUBDOMAIN AND COLLECT DATA 
!----------------------------------------------------------------------- 
! 
            IF(IEL.EQ.-2) THEN
              IF(ADD) THEN  
!               A LOST-AGAIN TRACEBACK DETECTED, ALREADY HERE    
!               SET THE IMPLANTING PARAMETERS  
                IPROC=IFAPAR(IFA  ,ELT(IPLOT)) 
                ILOC =IFAPAR(IFA+3,ELT(IPLOT))
!               ANOTHER ONE AS IPID, MEANS ALSO NOT LOCALISED  
                RECVCHAR(IPLOT)%NEPID=IPROC  
                RECVCHAR(IPLOT)%INE=ILOC 
              ELSE
                CALL COLLECT_CHAR(IPID,IPLOT,ELT(IPLOT),IFA,0,0,ISP,NSP,
     &                            XPLOT(IPLOT),YPLOT(IPLOT),0.D0,0.D0,
     &                            DX(IPLOT),DY(IPLOT),0.D0,0.D0,
     &                            IFAPAR,NCHDIM,NCHARA)                
              ENDIF
!             EXITING LOOP ON ISP 
              EXIT   
            ENDIF 
! 
!----------------------------------------------------------------------- 
!           SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
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
!             HERE SOLID BOUNDARY, VELOCITY IS PROJECTED ON THE BOUNDARY
!             AND WE GO ON
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
              IF(ADD) THEN 
                RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
                RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
                RECVCHAR(IPLOT)%DX=DX(IPLOT) 
                RECVCHAR(IPLOT)%DY=DY(IPLOT)
              ENDIF   
! 
              GOTO 50 
! 
            ELSEIF(IEL.EQ.0) THEN 
! 
!----------------------------------------------------------------------- 
!             HERE WE HAVE A LIQUID BOUNDARY, THE CHARACTERISTIC IS STOPPED
!----------------------------------------------------------------------- 
! 
              DENOM = DXP*DY1-DYP*DX1 
              IF(ABS(DENOM).GT.1.D-12) THEN 
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
!             THIS IS A MARKER FOR PARTICLES EXITING A DOMAIN
!             SENS=-1 FOR BACKWARD CHARACTERISTICS
              ELT(IPLOT) = - SENS * ELT(IPLOT)
              EXIT
!
            ELSE
!
              WRITE(LU,*) 'UNEXPECTED CASE IN SCHAR12'
              WRITE(LU,*) 'IEL=',IEL
              CALL PLANTE(1)
              STOP
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
      END SUBROUTINE SCHAR12 
!
!                       ****************** 
                        SUBROUTINE SCHAR13 
!                       ****************** 
! 
     &(U,V,DT,NRK,X,Y,IKLE,IFABOR,XPLOT,YPLOT,DX,DY,SHP,ELT, 
     & NPLOT,NPOIN,NELEM,NELMAX,SURDET,SENS, 
     & IFAPAR,MESH,NCHDIM,NCHARA,ADD) 
! 
!*********************************************************************** 
! BIEF VERSION 6.2           24/04/97    J-M JANIN (LNH) 30 87 72 84 
! 
!*********************************************************************** 
! 
!  FONCTION : THIS IS A MERE COPY OF SCHAR11, EXCEPT THE INTERPOLATION
!             OF VELOCITY WHICH IS HERE CONSIDERED QUADRATIC
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
! |    NPLOT       | -->| NOMBRE DE DERIVANTS.                         | 
! |    NPOIN       | -->| NOMBRE DE POINTS DU MAILLAGE.                | 
! |    NELEM       | -->| NOMBRE D'ELEMENTS.                           | 
! |    NELMAX      | -->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D| 
! |    SURDET      | -->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM. 
! |    SENS        | -->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.   | 
! |________________|____|______________________________________________| 
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE) 
!----------------------------------------------------------------------- 
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
      INTEGER         , INTENT(IN)    :: SENS,NCHDIM 
      INTEGER         , INTENT(IN)    :: NPOIN,NELEM,NELMAX,NPLOT,NRK 
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,6),IFABOR(NELMAX,3) 
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NCHARA 
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),SURDET(NELEM) 
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT) 
      DOUBLE PRECISION, INTENT(IN)    :: DT 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT) 
      INTEGER, INTENT(IN)             :: IFAPAR(6,*) 
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL, INTENT(IN)             :: ADD
!  
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
!      
      INTEGER IPLOT,ISP,I1,I2,I3,I4,I5,I6,IEL,ISO,IFA,ISUI(3),ISUI2(3)
      INTEGER IPROC,ILOC,ISPDONE,NSP
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,DENOM 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA ISUI2  / 3 , 1 , 2 / 
      DATA EPSILO / -1.D-6 / 
! 
      INTRINSIC INT,MAX,MIN,SQRT 
!
!----------------------------------------------------------------------- 
!     FOR EVERY POINT 
!----------------------------------------------------------------------- 
! 
      DO IPLOT=1,NPLOT 
! 
        IF(ADD) THEN
!
          XPLOT(IPLOT)   = RECVCHAR(IPLOT)%XP  
          YPLOT(IPLOT)   = RECVCHAR(IPLOT)%YP 
          DX(IPLOT)      = RECVCHAR(IPLOT)%DX  
          DY(IPLOT)      = RECVCHAR(IPLOT)%DY   
          ELT(IPLOT)     = RECVCHAR(IPLOT)%INE 
          NSP            = RECVCHAR(IPLOT)%NSP ! R-K STEPS TO BE FULLFILLED 
          ISPDONE        = RECVCHAR(IPLOT)%ISP ! R-K STEPS ALREADY DONE  
          IEL = ELT(IPLOT) 
          XP  = XPLOT(IPLOT) 
          YP  = YPLOT(IPLOT)  
          I1 = IKLE(IEL,1) 
          I2 = IKLE(IEL,2) 
          I3 = IKLE(IEL,3) 
          SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
          SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
          SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)      
!         ASSUME TO BE LOCALISED IT WILL BE SET OTHERWISE IF LOST-AGAIN  
          RECVCHAR(IPLOT)%NEPID=-1 
!     
        ELSE 
          IEL = ELT(IPLOT) 
!         POINTS WITH IEL=0 ARE TREATED SO THAT THE FINAL INTERPOLATION
!         GIVES 0., AND WE SKIP TO NEXT POINT IPLOT (CYCLE) 
!         THIS WILL NOT INTERFERE WITH ELT(IPLOT)=0 GIVEN ON LIQUID BOUNDARIES
!         BY ARRAY IFABOR, THAT MAY HAPPEN LATER    
          IF(IEL.EQ.0) THEN
            ELT(IPLOT)=1
            SHP(1,IPLOT)=0.D0
            SHP(2,IPLOT)=0.D0
            SHP(3,IPLOT)=0.D0
            CYCLE      
          ENDIF
          I1 = IKLE(IEL,1) 
          I2 = IKLE(IEL,2) 
          I3 = IKLE(IEL,3) 
          I4 = IKLE(IEL,4)
          I5 = IKLE(IEL,5)
          I6 = IKLE(IEL,6)
          DXP = U(I1)*(2.D0*SHP(1,IPLOT)-1.D0)*SHP(1,IPLOT)
     &        + U(I2)*(2.D0*SHP(2,IPLOT)-1.D0)*SHP(2,IPLOT)
     &        + U(I3)*(2.D0*SHP(3,IPLOT)-1.D0)*SHP(3,IPLOT)
     &        + U(I4)*4.D0*SHP(1,IPLOT)*SHP(2,IPLOT)
     &        + U(I5)*4.D0*SHP(2,IPLOT)*SHP(3,IPLOT)
     &        + U(I6)*4.D0*SHP(3,IPLOT)*SHP(1,IPLOT)
          DYP = V(I1)*(2.D0*SHP(1,IPLOT)-1.D0)*SHP(1,IPLOT)
     &        + V(I2)*(2.D0*SHP(2,IPLOT)-1.D0)*SHP(2,IPLOT)
     &        + V(I3)*(2.D0*SHP(3,IPLOT)-1.D0)*SHP(3,IPLOT)
     &        + V(I4)*4.D0*SHP(1,IPLOT)*SHP(2,IPLOT)
     &        + V(I5)*4.D0*SHP(2,IPLOT)*SHP(3,IPLOT)
     &        + V(I6)*4.D0*SHP(3,IPLOT)*SHP(1,IPLOT)
          NSP=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL))))   
          ISPDONE=1
        ENDIF
!
        PAS = SENS*DT/NSP 
! 
!       LOOP ON RUNGE-KUTTA SUB-STEPS
!
!       COMPILER MUST DO NOTHING IF ISPDONE>NSP
!       IN MODE "ADD", ISP = ISPDONE HAS NOT BEEN FULLY DONE
!       IT IS RESTARTED HERE
!
        DO ISP=ISPDONE,NSP 
!
!----------------------------------------------------------------------- 
!       LOCALISING THE ARRIVAL POINT
!----------------------------------------------------------------------- 
! 
!       IN MODE "ADD" ITERATIONS ALREADY DONE ARE SKIPPED AND
!                     CHARACTERISTICS GONE IN ANOTHER SUB-DOMAIN SKIPPED                   
!
        IF(ADD) THEN
          IF(ISP.EQ.ISPDONE) GO TO 50
          IF(RECVCHAR(IPLOT)%NEPID.NE.-1) CYCLE 
        ENDIF
!                       
        IEL = ELT(IPLOT) 
        I1 = IKLE(IEL,1) 
        I2 = IKLE(IEL,2) 
        I3 = IKLE(IEL,3) 
        I4 = IKLE(IEL,4)
        I5 = IKLE(IEL,5)
        I6 = IKLE(IEL,6)
        DX(IPLOT) = ( U(I1)*(2.D0*SHP(1,IPLOT)-1.D0)*SHP(1,IPLOT)
     &              + U(I2)*(2.D0*SHP(2,IPLOT)-1.D0)*SHP(2,IPLOT)
     &              + U(I3)*(2.D0*SHP(3,IPLOT)-1.D0)*SHP(3,IPLOT)
     &              + U(I4)*4.D0*SHP(1,IPLOT)*SHP(2,IPLOT)
     &              + U(I5)*4.D0*SHP(2,IPLOT)*SHP(3,IPLOT)
     &              + U(I6)*4.D0*SHP(3,IPLOT)*SHP(1,IPLOT)) * PAS
        DY(IPLOT) = ( V(I1)*(2.D0*SHP(1,IPLOT)-1.D0)*SHP(1,IPLOT)
     &              + V(I2)*(2.D0*SHP(2,IPLOT)-1.D0)*SHP(2,IPLOT)
     &              + V(I3)*(2.D0*SHP(3,IPLOT)-1.D0)*SHP(3,IPLOT)
     &              + V(I4)*4.D0*SHP(1,IPLOT)*SHP(2,IPLOT)
     &              + V(I5)*4.D0*SHP(2,IPLOT)*SHP(3,IPLOT)
     &              + V(I6)*4.D0*SHP(3,IPLOT)*SHP(1,IPLOT)) * PAS
        XP = XPLOT(IPLOT) + DX(IPLOT) 
        YP = YPLOT(IPLOT) + DY(IPLOT) 
! 
        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                 -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                 -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                 -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
! 
        XPLOT(IPLOT) = XP 
        YPLOT(IPLOT) = YP 
!
        IF(ADD) THEN
!         CONTINUOUS SETTING OF THE REACHED POSITION FOR IPLOT  
!         AND THE NUMBER OF STEPS DONE ALREADY   
          RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
          RECVCHAR(IPLOT)%YP=YPLOT(IPLOT)
          RECVCHAR(IPLOT)%DX=DX(IPLOT) 
          RECVCHAR(IPLOT)%DY=DY(IPLOT)  
          RECVCHAR(IPLOT)%INE=ELT(IPLOT) 
          RECVCHAR(IPLOT)%ISP=ISP
        ENDIF 
! 
!----------------------------------------------------------------------- 
!       TEST: IS THE PATHLINE WENT OUT THE ORIGINAL ELEMENT
!----------------------------------------------------------------------- 
! 
50      CONTINUE 
! 
        ISO = 0 
        IF(SHP(1,IPLOT).LT.EPSILO) ISO = 1 
        IF(SHP(2,IPLOT).LT.EPSILO) ISO = ISO + 2 
        IF(SHP(3,IPLOT).LT.EPSILO) ISO = ISO + 4 
! 
        IF(ISO.NE.0) THEN 
! 
!----------------------------------------------------------------------- 
!         HERE WE ARE OUT OF THE ELEMENT
!----------------------------------------------------------------------- 
! 
          IEL = ELT(IPLOT) 
          XP = XPLOT(IPLOT) 
          YP = YPLOT(IPLOT) 
! 
          IF(ISO.EQ.1) THEN 
            IFA = 2 
          ELSEIF (ISO.EQ.2) THEN 
            IFA = 3 
          ELSEIF (ISO.EQ.4) THEN 
            IFA = 1 
          ELSEIF (ISO.EQ.3) THEN 
            IFA = 2 
            IF(DX(IPLOT)*(Y(IKLE(IEL,3))-YP).LT. 
     &                DY(IPLOT)*(X(IKLE(IEL,3))-XP)) IFA = 3 
            ELSEIF (ISO.EQ.6) THEN 
              IFA = 3 
              IF (DX(IPLOT)*(Y(IKLE(IEL,1))-YP).LT. 
     &            DY(IPLOT)*(X(IKLE(IEL,1))-XP)) IFA = 1 
            ELSE 
              IFA = 1 
              IF(DX(IPLOT)*(Y(IKLE(IEL,2))-YP).LT. 
     &           DY(IPLOT)*(X(IKLE(IEL,2))-XP)) IFA = 2 
            ENDIF 
! 
            IEL = IFABOR(IEL,IFA) 
! 
            IF(IEL.GT.0) THEN 
! 
!----------------------------------------------------------------------- 
!             HERE WE ARRIVE IN ANOTHER ELEMENT
!----------------------------------------------------------------------- 
! 
              I1 = IKLE(IEL,1) 
              I2 = IKLE(IEL,2) 
              I3 = IKLE(IEL,3) 
! 
              ELT(IPLOT) = IEL 
              SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                       -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
              SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                       -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
              SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                       -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
              GOTO 50 
! 
            ENDIF
!
!----------------------------------------------------------------------- 
!           HERE WE PASS TO NEIGHBOUR SUBDOMAIN AND COLLECT DATA 
!----------------------------------------------------------------------- 
! 
            IF(IEL.EQ.-2) THEN
              IF(ADD) THEN  
!               A LOST-AGAIN TRACEBACK DETECTED, ALREADY HERE    
!               SET THE IMPLANTING PARAMETERS  
                IPROC=IFAPAR(IFA  ,ELT(IPLOT)) 
                ILOC =IFAPAR(IFA+3,ELT(IPLOT))
!               ANOTHER ONE AS IPID, MEANS ALSO NOT LOCALISED  
                RECVCHAR(IPLOT)%NEPID=IPROC  
                RECVCHAR(IPLOT)%INE=ILOC 
              ELSE
                CALL COLLECT_CHAR(IPID,IPLOT,ELT(IPLOT),IFA,0,0,ISP,NSP,
     &                            XPLOT(IPLOT),YPLOT(IPLOT),0.D0,0.D0,
     &                            DX(IPLOT),DY(IPLOT),0.D0,0.D0,
     &                            IFAPAR,NCHDIM,NCHARA)                
              ENDIF
!             EXITING LOOP ON ISP 
              EXIT   
            ENDIF 
! 
!----------------------------------------------------------------------- 
!           SPECIAL TREATMENT FOR SOLID OR LIQUID BOUNDARIES  
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
!             HERE SOLID BOUNDARY, VELOCITY IS PROJECTED ON THE BOUNDARY
!             AND WE GO ON
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
              IF(ADD) THEN 
                RECVCHAR(IPLOT)%XP=XPLOT(IPLOT) 
                RECVCHAR(IPLOT)%YP=YPLOT(IPLOT) 
                RECVCHAR(IPLOT)%DX=DX(IPLOT) 
                RECVCHAR(IPLOT)%DY=DY(IPLOT)
              ENDIF   
! 
              GOTO 50 
! 
            ELSEIF(IEL.EQ.0) THEN 
! 
!----------------------------------------------------------------------- 
!             HERE WE HAVE A LIQUID BOUNDARY, THE CHARACTERISTIC IS STOPPED
!----------------------------------------------------------------------- 
! 
              DENOM = DXP*DY1-DYP*DX1 
              IF(ABS(DENOM).GT.1.D-12) THEN 
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
!             THIS IS A MARKER FOR PARTICLES EXITING A DOMAIN
!             SENS=-1 FOR BACKWARD CHARACTERISTICS
              ELT(IPLOT) = - SENS * ELT(IPLOT)
              EXIT
!
            ELSE
!
              WRITE(LU,*) 'UNEXPECTED CASE IN SCHAR13'
              WRITE(LU,*) 'IEL=',IEL
              CALL PLANTE(1)
              STOP
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
      END SUBROUTINE SCHAR13 
! 
!--------------------------------------------------------------------- 
!   <<<<<<<<<<<<<<<<<< CHARACTERISTICS: PUBLIC >>>>>>>>>>>>>>>>>> 
!--------------------------------------------------------------------- 
! 
!----------------------------------------------------------------------- 
! BIEF'S CARACT MODIFIED FOR PARALLEL STREAMLINE TRACKING 
! THIS IS THE ONLY PUBLIC SUBROUTINE IN THIS MODULE 
! NOTE: IN 3D, NPOIN->NPOIN3, IMPORTANT!  
!----------------------------------------------------------------------- 
! 
!                       ****************** 
                        SUBROUTINE SCARACT 
!                       ****************** 
! 
     &(U,UTILD,UCONV,VCONV,WCONV,FRCONV,X,Y,ZSTAR,FREQ, 
     & XCONV,YCONV,ZCONV,FCONV,DX,DY,DZ,DF,Z,SHP,SHZ,SHF,SURDET, 
     & DT , IKLE , IFABOR , ELT , ETA , FRE, ELTBUF, ISUB, IELM , 
     & IELMU,NELEM,NELMAX,NOMB,NPOIN,NPOIN2,NDP,NPLAN,NF,  
     & MESH ,NPLOT,DIM1U, SENS,SHPBUF,SHZBUF,SHFBUF,FREBUF,SIZEBUF,
     & POST,PERIO,YA4D) 
! 
!*********************************************************************** 
! BIEF VERSION 6.3           24/04/97    J-M JANIN (LNH) 30 87 72 84 
! 
!*********************************************************************** 
!
!brief    Computes characteristic pathlines and interpolates functions
!+        at their foot.
!
!history  J. JANKOWSKI (BAW)
!+        01/01/2008
!+        V5P8
!+
!
!history  C. DENIS & J-M HERVOUET (EDF)
!+        20/06/2012
!+        V6P2
!+        Calls to Schar12 and Schar13 added.
! 
!----------------------------------------------------------------------- 
!                             ARGUMENTS 
! .________________.____.______________________________________________. 
! |      NOM       |MODE|                   ROLE                       | 
! |________________|____|______________________________________________| 
! |   U            | -->| VARIABLES A L'ETAPE N .                        
! |   UTILD        |<-- | VARIABLES APRES LA CONVECTION .                
! |   UCONV,VCONV..| -->| COMPOSANTES DES VITESSES DU CONVECTEUR. 
! |   FRCONV       | -->| 4TH COMPONENT OF VELOCITY (4D) 
! |   FRE          |<-->| LAYER IN 4TH DIMENSION  
! |   FREBUF       |<-->| INTEGER WORK ARRAY   
! |   FREQ         | -->| 4TH COMPONENT OF SPACE (4D)         
! |   X,Y          | -->| COORDINATES OF MESH                    
! |   XCONV,YCONV..|<-->| COORDINATES AT THE FOOT OF CHARACTERISTICS     
! |   DX,DY,DZ,DF  |<-->| STOCKAGE DES SOUS-PAS .                        
! |   Z            | -->| COTE DANS LE MAILLAGE REEL (POUR TEL3D) .      
! |   SHP          |<-->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES      
! |                |    | COURBES CARACTERISTIQUES. 
! |   SHPBUF       |<-->| WORK ARRAY USED AS TEMPORARY SHP 
! |   SHF          |<-->| BARYCENTRIC COORDINATES IN FREQUENCY (4D)                 
! |   SHZ          |<-->| COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED   
! |                |    | DES COURBES CARACTERISTIQUES (POUR TEL3D)
! |   SHZBUF       |<-->| WORK ARRAY USED AS TEMPORARY SHZ                 
! |   SURDET       | -->| 1/DETERMINANT POUR LES ELEMENTS 2D.            
! |   DT           | -->| PAS DE TEMPS                                   
! |   IKLE         | -->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D.    
! |   IFABOR       | -->| NUMEROS DES ELEMENTS VOISINS (ATTENTION, POUR  
! |                |    | TEL3D, IFABOR EST LE TABLEAU IBOR DE MITRID).  
! |   ELT          |<-->| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES    
! |                |    | CARACTERISTIQUES.                              
! |   ETA          |<-->| NUMEROS DES ETAGES AU PIED DES COURBES         
! |                |    | CARACTERISTIQUES (POUR TEL3D).                 
! |   ELTBUF       |<-->| INTEGER WORK ARRAY                     
! |   ISUB         |<-- | IN SCALAR MODE: NOT USED
! |                |    | IN PARALLEL: RETURNS THE SUB-DOMAIN WHERE IS
! |                |    | THE FOOT OF THE CHARACTERISTIC                     
! |   IELM         | -->| TYPE D'ELEMENT : 11 : TRIANGLE P1              
! |                |    |                  21 : QUADRANGLE P1            
! |                |    |                  41 : PRISME DE TEL3D          
! |   NELEM        | -->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D.  
! |   NELMAX       | -->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D 
! |   NF           | -->| NUMBER OF FREQUENCIES (4D CASE)   
! |   NOMB         | -->| NOMBRE DE VARIABLES A CONVECTER.               
! |   NPOIN        | -->| NOMBRE TOTAL DE POINTS DU MAILLAGE.            
! |   NPOIN2       | -->| NOMBRE DE POINTS DU MAILLAGE 2D (POUR TEL3D).  
! |   NDP          | -->| NOMBRE DE POINTS PAR ELEMENT 2D.               
! |   NPLAN        | -->| NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).           
! |   DIM1U        | -->| FIRST DIMENSIONS OF VARIABLES, THAT WILL BE
! |                |    | CONSIDERED IN SUBROUTINE INTERP
! |   SENS         | -->| -1: UPSTREAM CHARACTERISTICS
! |                |    | +1: DOWNSTREAM (FOR PARTICLE TRACKING)
! |   POST         | -->| IF YES, DATA KEPT FOR A POSTERIORI INTERPOLATION
! |   PERIO        | -->| IF YES, PERIODICITY ALONG THE VERTICAL
! |   ZSTAR        | -->| COORDINATES OF TRANSFORMED MESH
! |                |    | (CONSTANT IN SPACE) 
! |   YA4D         | -->| IF YES, THERE IS A FOURTH DIMENSION (FREQUENCY) 
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
! 
!----------------------------------------------------------------------- 
! 
! APPELE PAR : CHARAC, THOMPS, DERIVE
! 
! SOUS-PROGRAMMES APPELES : SCHAR11 , SCHAR41 
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
      INTEGER, INTENT(IN)             :: NOMB,NDP,NPLAN,IELM,IELMU,NF
      INTEGER, INTENT(IN)             :: DIM1U,SENS,SIZEBUF
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U 
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: UTILD 
      DOUBLE PRECISION, INTENT(INOUT) :: XCONV(*),YCONV(*),FCONV(*) 
      DOUBLE PRECISION, INTENT(IN)    :: UCONV(NPOIN),VCONV(NPOIN) 
      DOUBLE PRECISION, INTENT(IN)    :: WCONV(NPOIN),FRCONV(NPOIN) 
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2) 
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN2) 
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF) 
      DOUBLE PRECISION, INTENT(INOUT) :: ZCONV(NPOIN2,NPLAN) 
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: DZ(NPLOT),DF(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(NDP,NPLOT),SHZ(NPLOT) 
      DOUBLE PRECISION, INTENT(INOUT) :: SHF(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPBUF(NDP,SIZEBUF)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZBUF(SIZEBUF)
      DOUBLE PRECISION, INTENT(INOUT) :: SHFBUF(SIZEBUF)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM) 
      DOUBLE PRECISION, INTENT(IN)    :: DT 
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,NDP) 
      INTEGER, INTENT(IN)             :: IFABOR(NELMAX,*) 
      INTEGER, INTENT(INOUT)          :: ELT(NPLOT),ETA(NPLOT)
      INTEGER, INTENT(INOUT)          :: FRE(NPLOT)   
      INTEGER, INTENT(INOUT)          :: ELTBUF(NPLOT),FREBUF(NPLOT)
      INTEGER, TARGET,  INTENT(INOUT) :: ISUB(NPLOT)  
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      LOGICAL, INTENT(IN)             :: POST,PERIO,YA4D 	 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER NRK,I,ISTOP 
      INTEGER, POINTER, DIMENSION(:)  :: ETABUF
! 
!----------------------------------------------------------------------- 
!      
      LOGICAL :: INIT=.TRUE. 
!
! PRE-NUMBER OF INITIALLY COLLECTED LOST CHARACTERISTICS  
! LATER ON, IT COUNTS THE IMPLANTED TRACEBACKS LOCALISED IN MY PARTITION 
!
      INTEGER NCHARA,NLOSTCHAR,NARRV,IGEN,NSEND,NLOSTAGAIN 
      INTEGER  P_IMAX, P_ISUM 
      EXTERNAL P_IMAX, P_ISUM 
!     STATIC DIMENSION FOR HEAPCHAR, SENDCHAR, RECVCHAR (SORRY, STATIC)  
      INTEGER NCHDIM 
! 
      SAVE
!
      ETABUF => ISUB 
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
        LAST_NPLOT=0
! 
        IF(IELM.NE.11.AND.IELM.NE.41) THEN 
          WRITE(LU,*) 'STREAMLINE::SCARACT:: ', 
     &      'PARALLEL CHARACTERISTICS NOT IMPLEMENTED FOR ', 
     &      'IELM: ',IELM 
          CALL PLANTE(1) 
          STOP  
        ENDIF  
! 
        WRITE(LU,*) 'USING STREAMLINE VERSION 6.3 FOR CHARACTERISTICS' 
! 
!       NOW THE VERY NECESSARY INITIALISATION PROCEDURES  
! 
        IF(NCSIZE.GT.1) THEN       
          CALL ORGANISE_CHARS(NPLOT,NOMB,NCHDIM,LAST_NPLOT)         
        ENDIF
! 
      ENDIF 
! 
!     CASE OF A CALL FROM DIFFERENT PROGRAMMES WITH DIFFERENT NOMB 
!     JAJ + JMH 26/08/2008 
! 
      IF(NCSIZE.GT.1) THEN     
        IF(NOMB.NE.LAST_NOMB.OR.NPLOT.GT.LAST_NPLOT) THEN 
!         DESTROY THE CHARACTERISTICS TYPE FOR COMM. 
          CALL DEORG_CHARAC_TYPE()  
!         SET DATA STRUCTURES ACCORDINGLY        
          CALL ORGANISE_CHARS(NPLOT,NOMB,NCHDIM,LAST_NPLOT)         
          LAST_NOMB=NOMB  
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
!    TRIANGLES P1 
!    ============ 
! 
      IF(IELM.EQ.11) THEN  
! 
!----------------------------------------------------------------------- 
! 
!       APPEL DU SOUS-PROGRAMME DE REMONTEE DES COURBES CARACTERISTIQUES 
! 
        IF(IELMU.EQ.11) THEN
!    
          CALL SCHAR11(UCONV,VCONV,DT,NRK,X,Y,IKLE,IFABOR, 
     &                 XCONV,YCONV,DX,DY,SHP,ELT,
     &                 NPLOT,DIM1U,NELEM,NELMAX,SURDET,SENS, 
     &                 MESH%IFAPAR%I,MESH,NCHDIM,NCHARA,.FALSE.)  
!
        ELSEIF(IELMU.EQ.12) THEN
!    
          CALL SCHAR12(UCONV,VCONV,DT,NRK,X,Y,IKLE,IFABOR, 
     &                 XCONV,YCONV,DX,DY,SHP,ELT, 
     &                 NPLOT,DIM1U,NELEM,NELMAX,SURDET,SENS, 
     &                 MESH%IFAPAR%I,MESH,NCHDIM,NCHARA,.FALSE.)  
!
        ELSEIF(IELMU.EQ.13) THEN
!     
          CALL SCHAR13(UCONV,VCONV,DT,NRK,X,Y,IKLE,IFABOR, 
     &                 XCONV,YCONV,DX,DY,SHP,ELT, 
     &                 NPLOT,DIM1U,NELEM,NELMAX,SURDET,SENS, 
     &                 MESH%IFAPAR%I,MESH,NCHDIM,NCHARA,.FALSE.)
!
        ELSE
          WRITE(LU,*) 'WRONG DISCRETISATION OF VELOCITY:',IELMU
          CALL PLANTE(1)
          STOP
        ENDIF 
! 
!----------------------------------------------------------------------- 
! 
      ELSEIF(IELM.EQ.41) THEN 
! 
!    PRISMES DE TELEMAC-3D 
!    ===================== 
!  
!      APPEL DU SOUS-PROGRAMME DE REMONTEE DES COURBES CARATERISTIQUES 
!
         IF(PERIO) THEN
           IF(YA4D) THEN
             CALL SCHAR41_PER_4D(UCONV,VCONV,WCONV,FRCONV,DT,NRK,X,Y,
     &                           ZSTAR,FREQ,Z,IKLE,IFABOR,
     &                           XCONV,YCONV,ZCONV,FCONV,DX, 
     &                           DY,DZ,DF,SHP,SHZ,SHF,ELT,ETA,FRE,NPLOT, 
     &                           DIM1U,NELEM,NPLAN,NF,SURDET,SENS, 
     &                           MESH%IFAPAR%I,NCHDIM,NCHARA,.FALSE.)
           ELSE
             CALL SCHAR41_PER(UCONV,VCONV,WCONV,DT,NRK,X,Y,ZSTAR, 
     &                        Z,IKLE,IFABOR,XCONV,YCONV,ZCONV,DX, 
     &                        DY,DZ,SHP,SHZ,ELT,ETA,NPLOT, 
     &                        DIM1U,NELEM,NPLAN,SURDET,SENS, 
     &                        MESH%IFAPAR%I,NCHDIM,NCHARA,.FALSE.)
           ENDIF
         ELSE 
           CALL SCHAR41(UCONV,VCONV,WCONV,DT,NRK,X,Y,ZSTAR, 
     &                  Z,IKLE,IFABOR,XCONV,YCONV,ZCONV,DX, 
     &                  DY,DZ,SHP,SHZ,ELT,ETA,NPLOT, 
     &                  DIM1U,NELEM,NPLAN,SURDET,SENS, 
     &                  MESH%IFAPAR%I,NCHDIM,NCHARA,.FALSE.)
         ENDIF 
! 
!----------------------------------------------------------------------- 
! 
      ELSE 
! 
        IF(LNG.EQ.1) WRITE(LU,11) IELM 
        IF(LNG.EQ.2) WRITE(LU,12) IELM 
        CALL PLANTE(1) 
        STOP 
! 
      ENDIF 
! 
!----------------------------------------------------------------------- 
!
!     INTERPOLATION (IF ANY VARIABLE TO INTERPOLATE)
!
!     IN PARALLEL IT WILL BE DONE HERE FOR ALL POINTS, EVEN THOSE
!     WITH LOST CHARACTERISTICS (HENCE CONTROLS IN INTERP MUST NOT
!     BE DONE) 
! 
      IF(NOMB.GT.0) THEN 
! 
        IF(U%TYPE.EQ.2.AND.UTILD%TYPE.EQ.2) THEN 
! 
!         U ET UTILD VECTEURS (NOMB VAUT ALORS 1) 
!         
          IF(U%ELM.EQ.13.OR.U%ELM.EQ.12) THEN 
!           INTERPOLATION DES VITESSES POUR UNE VARIABLE QUADRATIQUE
!                                               OU QUASI-BULLE
            CALL BIEF_INTERP(U%R,UTILD%R,SHP,NDP,SHZ,ETA,SHF,FRE,ELT, 
     &                       U%DIM1,DIM1U,NPLAN,U%ELM,IKLE,NELMAX,
     &                       PERIO,YA4D)        
          ELSE  
!           INTERPOLATION DES VITESSES DANS LES AUTRES CAS 
            CALL BIEF_INTERP(U%R,UTILD%R,SHP,NDP,SHZ,ETA,SHF,FRE,ELT, 
     &                       NPLOT,DIM1U,NPLAN,IELM,IKLE,NELMAX,
     &                       PERIO,YA4D) 
          ENDIF            
! 
        ELSEIF(U%TYPE.EQ.4.AND.UTILD%TYPE.EQ.4) THEN 
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
          IF(U%ADR(I)%P%ELM.EQ.13.OR.U%ADR(I)%P%ELM.EQ.12) THEN 
!           INTERPOLATION DES VARIABLES POUR UNE VARIABLE QUADRATIQUE  
            CALL BIEF_INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,
     &                       SHP,NDP,SHZ,ETA,SHF,FRE,ELT,
     &                       U%ADR(I)%P%DIM1,DIM1U, 
     &                       NPLAN,U%ADR(I)%P%ELM,IKLE,NELMAX,
     &                       PERIO,YA4D)          
          ELSE  
!           INTERPOLATION DES VARIABLES DANS LES AUTRES CAS 
            CALL BIEF_INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ, 
     &                       ETA,SHF,FRE,ELT,NPLOT,DIM1U,NPLAN, 
     &                       IELM,IKLE,NELMAX,PERIO,YA4D)
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
   
        IF(NCHARA>NCHDIM) THEN  
          WRITE (LU,*) ' @STREAMLINE::NCHARA>NCHDIM'  
          CALL PLANTE(1) 
          STOP  
        ENDIF 
        NSEND=NCHARA 
        NLOSTCHAR=NSEND     
!         
        IF(P_IMAX(NSEND).GT.0) THEN ! THERE ARE LOST TRACEBACKS SOMEWHERE 
! 
!         PREPARE INITIAL SENDING OF COLLECTED LOST TRACEBACKS 
          CALL PREP_INITIAL_SEND(NSEND,NLOSTCHAR,NCHARA)  
! 
          IGEN=0 ! NUMBER OF GENERATIONS (I.E. INTERFACE CROSSINGS ON THE WAY) 
          DO 
            IGEN=IGEN+1 
!
!           GET THE ARRIVING TRACEBACKS VIA ALL-TO-ALL COMMUNICATION
! 
            CALL GLOB_CHAR_COMM()    
! 
!           COMPUTE THE NUMBER OF ARRIVED ONES 
!  
            NARRV = SUM(RECVCOUNTS)  
            ISTOP=0 
            IF(NARRV.GT.NCHDIM) THEN 
              ISTOP=1 
              WRITE(LU,*) 'NARRV=',NARRV
              WRITE(LU,*) 'NCHDIM=',NCHDIM,' IS TOO SMALL' 
            ENDIF
            IF(NARRV.GT.SIZEBUF) THEN 
              ISTOP=1 
              WRITE(LU,*) 'NARRV=',NARRV
              WRITE(LU,*) 'SIZEBUF=',SIZEBUF,' IS TOO SMALL' 
            ENDIF  
            ISTOP=P_ISUM(ISTOP) 
            IF(ISTOP.GT.0) THEN 
              WRITE (LU,*) 'TOO MANY LOST TRACEBACKS IN ',ISTOP, 
     *                     ' PROCESSORS' 
              CALL PLANTE(1) 
              STOP 
            ENDIF 
! 
!           CALL TRACKING WITH ARRIVED TRACEBACKS ... 
!           WE RE-USE THE PREVIOUSLY USED FIELDS FOR ADDITIONAL TRACEBACKS 
!           COMPUTING SHAPE FUNCTIONS BY THE WAY / 2D OR 3D  
! 
            IF(IELM.EQ.11) THEN
!
              IF(IELMU.EQ.11) THEN
!
                CALL SCHAR11(UCONV,VCONV,DT,NRK,X,Y,IKLE,IFABOR, 
     &                       XCONV,YCONV,DX,DY,SHPBUF,ELTBUF, 
     &                       NARRV,DIM1U,NELEM,NELMAX,SURDET,SENS, 
     &                       MESH%IFAPAR%I,MESH,NCHDIM,NARRV,
     &                       .TRUE.)
!
              ELSEIF(IELMU.EQ.12) THEN
!
                CALL SCHAR12(UCONV,VCONV,DT,NRK,X,Y,IKLE,IFABOR, 
     &                       XCONV,YCONV,DX,DY,SHPBUF,ELTBUF, 
     &                       NARRV,DIM1U,NELEM,NELMAX,SURDET,SENS, 
     &                       MESH%IFAPAR%I,MESH,NCHDIM,NARRV,
     &                       .TRUE.)
!
!
              ELSEIF(IELMU.EQ.13) THEN
!
                CALL SCHAR13(UCONV,VCONV,DT,NRK,X,Y,IKLE,IFABOR, 
     &                       XCONV,YCONV,DX,DY,SHPBUF,ELTBUF, 
     &                       NARRV,DIM1U,NELEM,NELMAX,SURDET,SENS, 
     &                       MESH%IFAPAR%I,MESH,NCHDIM,NARRV,
     &                       .TRUE.)
!
              ELSE
                WRITE(LU,*) 'WRONG DISCRETISATION OF VELOCITY:',IELMU
                CALL PLANTE(1)
                STOP
              ENDIF
! 
            ELSEIF(IELM.EQ.41) THEN
!
              IF(PERIO) THEN
                IF(YA4D) THEN
                  CALL SCHAR41_PER_4D(UCONV,VCONV,WCONV,FRCONV,DT,NRK,
     &                                X,Y,ZSTAR,FREQ,Z,IKLE,IFABOR,
     &                                XCONV,YCONV,ZCONV,FCONV,
     &                                DX,DY,DZ,DF,
     &                                SHPBUF,SHZBUF,SHFBUF,
     &                                ELTBUF,ETABUF,FREBUF, 
     &                                NARRV,DIM1U,NELEM,
     &                                NPLAN,NF,SURDET,SENS, 
     &                                MESH%IFAPAR%I,NCHDIM,NARRV,.TRUE.)
                ELSE
                  CALL SCHAR41_PER(UCONV,VCONV,WCONV,DT,NRK,X,Y,ZSTAR, 
     &                             Z,IKLE,IFABOR,XCONV,YCONV,ZCONV,DX, 
     &                             DY,DZ,SHPBUF,SHZBUF,ELTBUF,ETABUF, 
     &                             NARRV,DIM1U,NELEM,NPLAN,SURDET,SENS, 
     &                             MESH%IFAPAR%I,NCHDIM,NARRV,.TRUE.)
                ENDIF
              ELSE
                CALL SCHAR41(UCONV,VCONV,WCONV,DT,NRK,X,Y,ZSTAR, 
     &                       Z,IKLE,IFABOR,XCONV,YCONV,ZCONV,DX, 
     &                       DY,DZ,SHPBUF,SHZBUF,ELTBUF,ETABUF,NARRV, 
     &                       DIM1U,NELEM,NPLAN,SURDET,SENS, 
     &                       MESH%IFAPAR%I,NCHDIM,NARRV,.TRUE.)
              ENDIF
!
            ENDIF  
! 
!           INTERPOLATE THE -LOCATED- TRACEBACKS -> SOME OF RANGE 1:NARRV 
!           APPLYING THE JUST VALID ELT, SHP, ETC. JUST SAVED FROM  
!           SCHAR11 OR SCHAR41 CALLED FOR JUST THIS RANGE 
!           NOTICE: THIS IS THE ONLY REASON TO DO IT NOW -   
!           WHEN THE ELT,SHP COULD BE SAVED, THIS COULD HAVE BEEN DONE  
!           AFTER THE GENERATION LOOP WORKING ON HEAPCHAR -  
!           WHICH WOULD HAVE BEEN MORE PERFORMANT, BECAUSE IN RECVCHAR  
!           WE HAVE TO CHECK FOR EACH TRACEBACK IF IT IS LOCATED OR NOT   
! 
!           NOTICE: UTILD==RECVCHAR%BASKET(:)  
! 
          IF(NARRV.GT.0) THEN 
            IF(U%TYPE.EQ.2) THEN 
              IF(IELM.EQ.11) THEN 
                CALL INTERP_RECVCHAR_11 
     &            (U%R,1,IKLE,ELTBUF,SHPBUF,NELMAX,U%DIM1,NARRV,U%ELM,
     &             POST,NOMB) 
              ELSEIF(IELM.EQ.41) THEN 
                CALL INTERP_RECVCHAR_41 
     &         (U%R,1,IKLE,ELTBUF,ETABUF,FREBUF,SHPBUF,SHZBUF,SHFBUF,
     &          NELMAX,NPOIN2,NPLAN,NARRV,POST,NOMB,PERIO,YA4D) 
              ELSE 
                WRITE(LU,*) 'WRONG IELM IN SCARACT:',IELM 
                CALL PLANTE(1) 
                STOP 
              ENDIF 
            ELSEIF(U%TYPE.EQ.4) THEN 
              IF(IELM.EQ.11) THEN  
                DO I=1,NOMB 
                  CALL INTERP_RECVCHAR_11 
     &          (U%ADR(I)%P%R,I,IKLE,ELTBUF,SHPBUF,NELMAX, 
     &           U%ADR(I)%P%DIM1,NARRV,U%ADR(I)%P%ELM,POST,NOMB) 
                ENDDO  
              ELSEIF(IELM.EQ.41) THEN  
                DO I=1,NOMB  
                  CALL INTERP_RECVCHAR_41 
     &              (U%ADR(I)%P%R,I,IKLE,ELTBUF,ETABUF,FREBUF,
     &               SHPBUF,SHZBUF,SHFBUF,NELMAX,NPOIN2,NPLAN,NARRV,
     &               POST,NOMB,PERIO,YA4D) 
                ENDDO 
              ELSE  
                WRITE(LU,*) 'WRONG IELM IN SCARACT:',IELM 
                CALL PLANTE(1) 
                STOP 
              ENDIF 
            ENDIF 
          ENDIF 
!                 
          CALL HEAP_FOUND(NLOSTAGAIN,NARRV,NCHARA)  
!
          IF(P_ISUM(NLOSTAGAIN).GT.0) THEN ! THERE ARE LOST-AGAINS SOMEWHERE   
            CALL PREP_LOST_AGAIN(NOMB,NSEND,NLOSTAGAIN,NARRV) ! PREPARE SENDING LOST-AGAINS  
          ELSE  
            EXIT ! NO LOST-AGAIN TRACEBACKS ANYWHERE, LEAVE THESE ITERATIONS 
          ENDIF 
! 
          IF(IGEN.GT.99) THEN  ! A SECURITY FUSE 
            WRITE(LU,*) '@STREAMLINE::SCARACT: ', 
     &          'THE NUMBER OF TRACEBACK INTERFACE CROSSINGS IGEN > 99'  
            CALL PLANTE(1)  
            STOP 
          ENDIF 
! 
          ENDDO ! ON TRACEBACKS GENERATIONS (IGEN) / ALL COLLECTED  
! 
!         PREPARE SENDING OF THE HEAPED LOCALISED TRACEBACKS 
!
          CALL PREP_SENDBACK(NOMB,NCHARA) 
!         THE FINAL SEND/RECV OF THE LOST TRACEBACKS BACK VIA ALLTOALL COMM. 
          CALL GLOB_CHAR_COMM() 
          NARRV = SUM(RECVCOUNTS) ! FROM RECVCOUNTS / THE FINALLY ARRIVED ONES  
! 
          IF(NARRV.NE.NLOSTCHAR) THEN ! THE MOST SERIOUS PROBLEM WE CAN HAVE  
            WRITE (LU,*) ' @STREAMLINE::SCARACT ::',  
     &                   ' THE NUMBER OF INITALLY LOST TRACEBACKS', 
     &                   ' /= THE NUMBER OF FINALLY ARRIVED ONES ' 
            WRITE (LU,*) '             NLOSTCHAR, NARRV: ',  
     &                                 NLOSTCHAR, NARRV 
            CALL PLANTE(1) 
            STOP  
          ENDIF   
! 
!         INTRODUCE THE VALUES FROM THE RECEIVED TRACEBACK BASKETS  
!
!         ISUB : SUB-DOMAIN WHERE IS THE FOOT OF THE CHARACTERISTIC
!                IT IS HERE INITIALISED AS THE STARTING SUB-DOMAIN
!                AND IS THEN MODIFIED IN CASE OF REMOTE INTERPOLATION
          IF(POST) THEN
            DO I=1,NPLOT
              ISUB(I)=IPID
            ENDDO
          ENDIF 
          IF(NARRV.GT.0) THEN
            CALL INTRODUCE_RECVCHAR(UTILD,NOMB,NARRV,IELM,
     &                              SHP,SHZ,SHF,ELT,ETA,FRE,POST,YA4D)
            IF(POST) THEN
              DO I=1,NARRV 
                ISUB(RECVCHAR(I)%IOR)=RECVCHAR(I)%NEPID  
              ENDDO
            ENDIF
          ENDIF  
!
        ELSEIF(POST) THEN
!         IN THIS CASE ALL INTERPOLATIONS ARE LOCAL
          DO I=1,NPLOT
            ISUB(I)=IPID
          ENDDO
        ENDIF ! P_IMAX(NSEND).GT.0 
        CALL RE_INITIALISE_CHARS(NSEND,NLOSTCHAR,NLOSTAGAIN,NARRV) ! DEALLOCATING 
!    
      ENDIF ! NCSIZE.GT.1 
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
! 
!----------------------------------------------------------------------- 
! 
      RETURN  
      END SUBROUTINE SCARACT 
!                    **********************
                     SUBROUTINE BIEF_INTERP
!                    **********************
!
     &( U , UTILD , SHP , NDP , SHZ , ETA , SHF , FRE , ELT , NP , 
     &  NPOIN2 , NPLAN , IELM , IKLE , NELMAX , PERIO , YA4D )
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    INTERPOLATES THE VALUES OF A FUNCTION AT SOME OF THE
!+                MESH NODES ACCORDING TO THE BARYCENTRIC COORDINATES
!+                OF THE POINTS AND THE VALUES AT THE NODES OF THE
!+                FUNCTION.
!
!warning  DOES NOT WORK IF THE PROVIDED BARYCENTRIC COORDINATES
!+            DO NOT CORRESPOND TO THE ELEMENT OF THE FUNCTION
!warning  ELEMENTS OTHER THAN 11, 21 AND 41 ARE NOT IMPLEMENTED
!
!history  J-M JANIN (LNH)
!+        28/04/93
!+        V5P1
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
!history  J-M HERVOUET (LNHE)
!+        20/06/2012
!+        V6P2
!+   Adding Quasi-bubble interpolation
!
!history  J-M HERVOUET (LNHE)
!+        16/10/2012
!+        V6P3
!+   Adding interpolation with periodicity and 4D.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELT            |-->| 2D ELEMENT AT THE FOOT OF CHARACTERISTIC LINES.
!| ETA            |-->| LAYER NUMBER AT THE FOOT OF CHARACTERISTIC LINES.
!| IELM           |-->| TYPE OF ELEMENT.
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT FOR U.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NP             |-->| NOMBER OF POINTS TO BE INTERPOLATED.
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| SHP            |-->| 2D BARYCENTRIC COORDINATES AT THE FOOT OF
!|                |   | CHARACTERISTIC LINES.
!| SHZ            |-->| BARYCENTRIC COORDINATES ALONG Z AT THE FOOT OF
!|                |   | CHARACTERISTIC LINES (FOR TELEMAC-3D)
!| U              |-->| VALUES AT NODES FOR INTERPOLATION.
!| UTILD          |<--| INTERPOLATED VALUES.
!| YA4D           |-->| IF YES, 4 DIMENSIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NP,NELMAX,NPLAN,NPOIN2,NDP,IELM
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),ELT(NP),ETA(NP),FRE(NP)
!
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN,*)
      DOUBLE PRECISION, INTENT(IN)    :: SHP(NDP,NP),SHZ(NP),SHF(NP)
      DOUBLE PRECISION, INTENT(INOUT) :: UTILD(NP)
      LOGICAL, INTENT(IN)             :: PERIO,YA4D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,ETAP1,I1,I2,I3,IFR
      DOUBLE PRECISION SHP11,SHP12,SHP14
      DOUBLE PRECISION SHP22,SHP23,SHP24
      DOUBLE PRECISION SHP33,SHP31,SHP34,UMSHZ,UMSHF
!
!     SHOULD BE SAME EPSILO THAN SCHAR11
      DOUBLE PRECISION EPSILO
      DATA EPSILO / 1.D-6 /
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11) THEN
!
!    P1 TRIANGLES
!    ============
!
      DO IP = 1 , NP
         UTILD(IP) = U(IKLE(ELT(IP),1),1,1) * SHP(1,IP)
     &             + U(IKLE(ELT(IP),2),1,1) * SHP(2,IP)
     &             + U(IKLE(ELT(IP),3),1,1) * SHP(3,IP)
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELM.EQ.12) THEN
!
!    QUASI-BUBBLE TRIANGLES
!    ======================
!
      DO IP = 1 , NP
!
!        BARYCENTRIC COORDINATES OF SUB-TRIANGLES AS A FUNCTION OF
!        BARYCENTRIC COORDINATES OF THE ORIGINAL TRIANGLE
!        (A NICE GEOMETRY PROBLEM...)
!
         SHP11=SHP(1,IP)-SHP(3,IP)
         SHP12=SHP(2,IP)-SHP(3,IP)
         SHP14=3.D0*SHP(3,IP)
         SHP22=SHP(2,IP)-SHP(1,IP)
         SHP23=SHP(3,IP)-SHP(1,IP)
         SHP24=3.D0*SHP(1,IP)
         SHP33=SHP(3,IP)-SHP(2,IP)
         SHP31=SHP(1,IP)-SHP(2,IP)
         SHP34=3.D0*SHP(2,IP)
!
!        FINDING IN WHICH SUB-TRIANGLE WE ARE
!
!        IF(     SHP11.GT.0.D0 .AND. SHP11.LT.1.D0 .AND.
!    &           SHP12.GT.0.D0 .AND. SHP12.LT.1.D0 .AND.
!    &           SHP14.GT.0.D0 .AND. SHP14.LT.1.D0 ) THEN
!          UTILD(IP) = U(IKLE(ELT(IP),1),1,1) * SHP11
!    &               + U(IKLE(ELT(IP),2),1,1) * SHP12
!    &               + U(IKLE(ELT(IP),4),1,1) * SHP14
!        ELSEIF( SHP22.GT.0.D0 .AND. SHP22.LT.1.D0 .AND.
!    &           SHP23.GT.0.D0 .AND. SHP23.LT.1.D0 .AND.
!    &           SHP24.GT.0.D0 .AND. SHP24.LT.1.D0 ) THEN
!          UTILD(IP) = U(IKLE(ELT(IP),2),1,1) * SHP22
!    &               + U(IKLE(ELT(IP),3),1,1) * SHP23
!    &               + U(IKLE(ELT(IP),4),1,1) * SHP24
!        ELSEIF( SHP33.GT.0.D0 .AND. SHP33.LT.1.D0 .AND.
!    &           SHP31.GT.0.D0 .AND. SHP31.LT.1.D0 .AND.
!    &           SHP34.GT.0.D0 .AND. SHP34.LT.1.D0 ) THEN
!          UTILD(IP) = U(IKLE(ELT(IP),3),1,1) * SHP33
!    &               + U(IKLE(ELT(IP),1),1,1) * SHP31
!    &               + U(IKLE(ELT(IP),4),1,1) * SHP34
!
!        OPTIMISED VERSION WITH TRUNCATION ERRORS
!        SHP14, SHP24 AND SHP34 POSITIVITY ALREADY ENSURED
!
         IF(     SHP11.GT.    -2.D0*EPSILO .AND. 
     &           SHP11.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP12.GT.    -2.D0*EPSILO .AND. 
     &           SHP12.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP14.LT.1.D0+4.D0*EPSILO ) THEN
           UTILD(IP) = U(IKLE(ELT(IP),1),1,1) * SHP11
     &               + U(IKLE(ELT(IP),2),1,1) * SHP12
     &               + U(IKLE(ELT(IP),4),1,1) * SHP14
         ELSEIF( SHP22.GT.    -2.D0*EPSILO .AND. 
     &           SHP22.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP23.GT.    -2.D0*EPSILO .AND. 
     &           SHP23.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP24.LT.1.D0+4.D0*EPSILO ) THEN
           UTILD(IP) = U(IKLE(ELT(IP),2),1,1) * SHP22
     &               + U(IKLE(ELT(IP),3),1,1) * SHP23
     &               + U(IKLE(ELT(IP),4),1,1) * SHP24
         ELSEIF( SHP33.GT.    -2.D0*EPSILO .AND. 
     &           SHP33.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP31.GT.    -2.D0*EPSILO .AND. 
     &           SHP31.LT.1.D0+4.D0*EPSILO .AND.
     &           SHP34.LT.1.D0+4.D0*EPSILO ) THEN
           UTILD(IP) = U(IKLE(ELT(IP),3),1,1) * SHP33
     &               + U(IKLE(ELT(IP),1),1,1) * SHP31
     &               + U(IKLE(ELT(IP),4),1,1) * SHP34
!
!        THE FOLLOWING CASE MAY HAPPEN IN PARALLEL
!        BECAUSE EVEN LOST CHARACTERISTICS ARE INTERPOLATED
!        AT GENERATION 0
!
         ELSEIF(NCSIZE.EQ.0) THEN
           WRITE(LU,*) 'INTERP: POINT ',IP,' NOT IN ELEMENT ',ELT(IP)
           WRITE(LU,*) 'SHP(1,IP)=',SHP(1,IP)
           WRITE(LU,*) 'SHP(2,IP)=',SHP(2,IP)
           WRITE(LU,*) 'SHP(3,IP)=',SHP(3,IP)
           WRITE(LU,*) 'EPSILO=',EPSILO,' IPID=',IPID
           CALL PLANTE(1)
           STOP
         ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELM.EQ.13) THEN
!
!    P2 TRIANGLES
!    ============
!
      DO IP = 1 , NP
         UTILD(IP) = U(IKLE(ELT(IP),1),1,1) *
     &               (2.D0*SHP(1,IP)-1.D0)* SHP(1,IP)
     &             + U(IKLE(ELT(IP),2),1,1) *
     &               (2.D0*SHP(2,IP)-1.D0)* SHP(2,IP)
     &             + U(IKLE(ELT(IP),3),1,1) *
     &               (2.D0*SHP(3,IP)-1.D0)* SHP(3,IP)
     &             + U(IKLE(ELT(IP),4),1,1) * 4.D0 * SHP(1,IP)*SHP(2,IP)
     &             + U(IKLE(ELT(IP),5),1,1) * 4.D0 * SHP(2,IP)*SHP(3,IP)
     &             + U(IKLE(ELT(IP),6),1,1) * 4.D0 * SHP(3,IP)*SHP(1,IP)
      ENDDO
!
!------------------------------------------------------------------------
!
      ELSEIF(IELM.EQ.41) THEN
!
      IF(PERIO) ETA1(NPLAN)=1
!
!    TELEMAC-3D PRISMS
!    =====================
!
      IF(PERIO) THEN 
        IF(YA4D) THEN
          DO IP = 1 , NP
            I1=IKLE(ELT(IP),1)
            I2=IKLE(ELT(IP),2)
            I3=IKLE(ELT(IP),3)
            UMSHZ=1.D0-SHZ(IP)
            UMSHF=1.D0-SHF(IP)
            ETAP1=ETA1(ETA(IP))
            IFR=FRE(IP)
            UTILD(IP) =   UMSHF *
     &        ((U(I1,ETA(IP),IFR  ) * SHP(1,IP)
     &        + U(I2,ETA(IP),IFR  ) * SHP(2,IP)
     &        + U(I3,ETA(IP),IFR  ) * SHP(3,IP)) * UMSHZ
     &       +( U(I1,ETAP1  ,IFR  ) * SHP(1,IP)
     &        + U(I2,ETAP1  ,IFR  ) * SHP(2,IP)
     &        + U(I3,ETAP1  ,IFR  ) * SHP(3,IP)) * SHZ(IP) )
     &                 + SHF(IP) *
     &        ((U(I1,ETA(IP),IFR+1) * SHP(1,IP)
     &        + U(I2,ETA(IP),IFR+1) * SHP(2,IP)
     &        + U(I3,ETA(IP),IFR+1) * SHP(3,IP)) * UMSHZ
     &       +( U(I1,ETAP1  ,IFR+1) * SHP(1,IP)
     &        + U(I2,ETAP1  ,IFR+1) * SHP(2,IP)
     &        + U(I3,ETAP1  ,IFR+1) * SHP(3,IP)) * SHZ(IP) )
          ENDDO 
        ELSE     
          DO IP = 1 , NP
            ETAP1=ETA1(ETA(IP))
            UTILD(IP) =
     &       (U(IKLE(ELT(IP),1),ETA(IP),1)*SHP(1,IP) 
     &       +U(IKLE(ELT(IP),2),ETA(IP),1)*SHP(2,IP) 
     &       +U(IKLE(ELT(IP),3),ETA(IP),1)*SHP(3,IP))*(1.D0-SHZ(IP))
     &     + (U(IKLE(ELT(IP),1),ETAP1,1)  *SHP(1,IP) 
     &       +U(IKLE(ELT(IP),2),ETAP1,1)  *SHP(2,IP) 
     &       +U(IKLE(ELT(IP),3),ETAP1,1)  *SHP(3,IP))*SHZ(IP)
          ENDDO
        ENDIF
      ELSE
        DO IP = 1 , NP
          UTILD(IP) =
     &    ( U(IKLE(ELT(IP),1),ETA(IP),1)   * SHP(1,IP) 
     &    + U(IKLE(ELT(IP),2),ETA(IP),1)   * SHP(2,IP)
     &    + U(IKLE(ELT(IP),3),ETA(IP),1)   * SHP(3,IP) ) *(1.D0-SHZ(IP))
     &  + ( U(IKLE(ELT(IP),1),ETA(IP)+1,1) * SHP(1,IP) 
     &    + U(IKLE(ELT(IP),2),ETA(IP)+1,1) * SHP(2,IP) 
     &    + U(IKLE(ELT(IP),3),ETA(IP)+1,1) * SHP(3,IP) ) * SHZ(IP)
        ENDDO
      ENDIF
!
!     RESTORING THE ORIGINAL ETA1
      IF(PERIO) ETA1(NPLAN)=NPLAN+1
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
11      FORMAT(1X,'BIEF_INTERP : TYPE D''ELEMENT INCONNU : ',I6)
12      FORMAT(1X,'BIEF_INTERP : UNKNOWN TYPE OF ELEMENT : ',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE BIEF_INTERP
!                       ********************** 
                        SUBROUTINE POST_INTERP
!                       ********************** 
! 
     &(U,UTILD,SHP,SHZ,SHF,IKLE,NELMAX,NOMB,NPOIN2,
     & ELT,ETA,FRE,ISUB,NDP,NPLAN,IELM,IELMU,NPLOT,DIM1U,
     & WSHP,WSHZ,WSHF,WELT,WETA,WFRE,SIZEBUF,PERIO,YA4D)
! 
!*********************************************************************** 
! BIEF VERSION 6.3           24/04/97    J-M HERVOUET (LNHE) 
! 
!*********************************************************************** 
!
!brief    A posteriori interpolation at the foot of characteristics.
!
!warning  See explanations on IKLE
!
!history  J-M HERVOUET (EDF-LNHE)
!+        02/10/2012
!+        V6P3
!+     First version
! 
!----------------------------------------------------------------------- 
!                             ARGUMENTS 
! .________________.____.______________________________________________. 
! |      NOM       |MODE|                   ROLE                       | 
! |________________|____|______________________________________________| 
! |   U            | -->| VARIABLES A L'ETAPE N .                        
! |   UTILD        |<-- | VARIABLES APRES LA CONVECTION .                
! |   SHP          |<-- | COORDONNEES BARYCENTRIQUES 2D AU PIED DES      
! |                |    | COURBES CARACTERISTIQUES.                     
! |   SHZ          | -->| COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED   
! |                |    | DES COURBES CARACTERISTIQUES (POUR TEL3D)      
! |   ELT          | -->| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES    
! |                |    | CARACTERISTIQUES.                              
! |   ETA          | -->| NUMEROS DES ETAGES AU PIED DES COURBES         
! |                |    | CARACTERISTIQUES (POUR TEL3D).                       
! |   ISUB         | -->| IN SCALAR MODE: NOT USED
! |                |    | IN PARALLEL: RETURNS THE SUB-DOMAIN WHERE IS
! |                |    | THE FOOT OF THE CHARACTERISTIC                     
! |   IELM         | -->| TYPE D'ELEMENT : 11 : TRIANGLE P1              
! |                |    |                  21 : QUADRANGLE P1            
! |                |    |                  41 : PRISME DE TEL3D 
! |   IELMU        | -->| TYPE OF ELEMENT FOR U.   
! |   IKLE         | -->| CONNECTIVITY TABLE IN 2D
! |                |    | IN PARALLEL MUST BE EXTENDED TO THE LARGER
! |                |    | NUMBER OF ELEMENTS OF ALL SUB-DOMAINS
! |                |    | SEE SUBROUTINE BUILD_IKLE_EXT                   
! |   NELMAX       | -->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D  
! |   NOMB         | -->| NOMBRE DE VARIABLES A CONVECTER.               
! |   NDP          | -->| NOMBRE DE POINTS PAR ELEMENT 2D.               
! |   NPLAN        | -->| NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).
! |   NPOIN2       | -->| NUMBER OF POINTS IN 2D          
! |   DIM1U        | -->| FIRST DIMENSIONS OF VARIABLES, THAT WILL BE
! |                |    | CONSIDERED IN SUBROUTINE INTERP
! |   PERIO        | -->| IF YES, PERIODIC CONDITIONS ON THE VERTICAL
! |   YA4D         | -->| IF YES, 4 DIMENSIONS
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
! 
!----------------------------------------------------------------------- 
! 
! APPELE PAR : 
! 
! SOUS-PROGRAMMES APPELES : 
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
      INTEGER, INTENT(IN)             :: NELMAX,NPLOT,NOMB,NDP,NPLAN
      INTEGER, INTENT(IN)             :: IELM,IELMU,DIM1U,NPOIN2,SIZEBUF
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,NDP) 
      INTEGER, INTENT(INOUT)          :: ELT(NPLOT),ETA(NPLOT)
      INTEGER, INTENT(INOUT)          :: FRE(NPLOT)
      INTEGER, INTENT(IN)             :: ISUB(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(NDP,NPLOT),SHZ(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHF(NPLOT)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U 
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: UTILD 
      DOUBLE PRECISION, INTENT(INOUT) :: WSHP(3,SIZEBUF),WSHZ(SIZEBUF)
      DOUBLE PRECISION, INTENT(INOUT) :: WSHF(SIZEBUF)
      INTEGER, INTENT(INOUT)          :: WELT(SIZEBUF),WETA(SIZEBUF)
      INTEGER, INTENT(INOUT)          :: WFRE(SIZEBUF)
      LOGICAL, INTENT(IN)             :: PERIO,YA4D  	 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER I,ISTOP,IPLOT 
! 
!----------------------------------------------------------------------- 
! 
      INTEGER NCHARA,NLOSTCHAR,NARRV,NSEND,NLOSTAGAIN 
      INTEGER  P_IMAX, P_ISUM 
      EXTERNAL P_IMAX, P_ISUM 
!     STATIC DIMENSION FOR HEAPCHAR, SENDCHAR, RECVCHAR  
      INTEGER NCHDIM  
! 
      SAVE 
! 
!----------------------------------------------------------------------- 
!     
      IF(NOMB.EQ.0) RETURN 
!
      IF(IELM.NE.11.AND.IELM.NE.41) THEN 
        WRITE(LU,*) 'STREAMLINE::POST_INTERP:: ', 
     &              'NOT IMPLEMENTED FOR ', 
     &              'IELM: ',IELM 
        CALL PLANTE(1) 
        STOP  
      ENDIF  
! 
      NCHDIM=LAST_NPLOT
! 
!     ORGANISE_CHARS MUST HAVE BEEN CALLED BEFORE IN A CALL TO SCARACT
!     BUT WE MAY HAVE THE CASE OF A CALL FROM DIFFERENT PROGRAMMES WITH
!     A DIFFERENT NOMB OR DIFFERENT NPLOT 
! 
      IF(NCSIZE.GT.1) THEN     
        IF(NOMB.NE.LAST_NOMB.OR.NPLOT.GT.LAST_NPLOT) THEN 
!         DESTROY THE CHARACTERISTICS TYPE FOR COMM. 
          CALL DEORG_CHARAC_TYPE()  
!         SET DATA STRUCTURES ACCORDINGLY  
          CALL ORGANISE_CHARS(NPLOT,NOMB,NCHDIM,LAST_NPLOT)         
          LAST_NOMB=NOMB  
        ENDIF 
!    
!       INITIALISING NCHARA (NUMBER OF LOST CHARACTERISTICS) 
        NCHARA=0
        HEAPCOUNTS=0  
! 
      ENDIF 
!
!----------------------------------------------------------------------- 
!
!     INTERPOLATION (IF ANY VARIABLE TO INTERPOLATE)
!
!     IN PARALLEL IT IS DONE HERE FOR ALL POINTS.
!     3 TYPES OF POINTS ARE TREATED HERE:
!
!     1) "NORMAL POINTS": ELTCAR.NE.0 AND ISUB=IPID
!     2) POINTS WITH ELTCAR=0, THEIR SHP ARE ALL 0.D0 SO THAT THE 
!        INTERPOLATION GIVES 0. THE FINAL PARCOM WILL RETRIEVE THE CORRECT
!        VALUE FROM OTHER PROCESSORS.
!     3) POINTS WITH ISUB.NE.IPID, THAT WILL BE INTERPOLATED IN ANOTHER
!        SUB-DOMAIN. THE INTERPOLATION HERE WILL GIVE A FALSE VALUE AND
!        MAY EVEN HAVE A VALUE OF ELT THAT IS BEYOND THE LOCAL MAXIMUM
!        OF ELEMENTS. HENCE THE NEED OF AN EXTENDED IKLE. THE CORRECT VALUE
!        IS FOUND AFTER IN THE PARALLEL PART.
! 
      IF(U%TYPE.EQ.2.AND.UTILD%TYPE.EQ.2) THEN 
! 
!       U ET UTILD VECTEURS (NOMB VAUT ALORS 1) 
!         
        IF(U%ELM.EQ.13.OR.U%ELM.EQ.12) THEN 
!         INTERPOLATION DES VITESSES POUR UNE VARIABLE QUADRATIQUE
!                                               OU QUASI-BULLE
          CALL BIEF_INTERP(U%R,UTILD%R,SHP,NDP,SHZ,ETA,SHF,FRE,ELT, 
     &                     U%DIM1,DIM1U,NPLAN,U%ELM,IKLE,NELMAX,
     &                     PERIO,YA4D)        
        ELSE  
!         INTERPOLATION DES VITESSES DANS LES AUTRES CAS
          CALL BIEF_INTERP(U%R,UTILD%R,SHP,NDP,SHZ,ETA,SHF,FRE,ELT, 
     &                     NPLOT,DIM1U,NPLAN,IELM,IKLE,NELMAX,
     &                     PERIO,YA4D)
        ENDIF            
! 
      ELSEIF(U%TYPE.EQ.4.AND.UTILD%TYPE.EQ.4) THEN 
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
        IF(U%ADR(I)%P%ELM.EQ.13.OR.U%ADR(I)%P%ELM.EQ.12) THEN 
!         INTERPOLATION DES VARIABLES POUR UNE VARIABLE QUADRATIQUE  
          CALL BIEF_INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ, 
     &                     ETA,SHF,FRE,ELT,U%ADR(I)%P%DIM1,DIM1U, 
     &                     NPLAN,U%ADR(I)%P%ELM,IKLE,NELMAX,PERIO,YA4D)          
        ELSE  
!         INTERPOLATION DES VARIABLES DANS LES AUTRES CAS
          CALL BIEF_INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ, 
     &                     ETA,SHF,FRE,ELT,NPLOT,DIM1U,NPLAN, 
     &                     IELM,IKLE,NELMAX,PERIO,YA4D)
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
!     NOW THE PARALLEL PART
!
      IF(NCSIZE.GT.1) THEN  
!
!       INTERPOLATION FUNCTIONS, AND POSITIONS (ELEMENT, LAYER)
!       ARE SENT BACK TO ORIGINAL SUB-DOMAINS WHERE THEYHAVE BEEN DONE.
!
        IF(YA4D) THEN
          DO IPLOT=1,NPLOT
          IF(ISUB(IPLOT).NE.IPID) THEN
            NCHARA=NCHARA+1 
            IF(NCHARA.GT.NCHDIM.OR.NCHARA.GT.SIZEBUF) THEN  
              WRITE (LU,*) 'NCHARA=',NCHARA,' NCHDIM=',NCHDIM 
              WRITE (LU,*) 'SIZEBUF=',SIZEBUF  
              WRITE (LU,*) 'POST_INTERP::NCHARA>NCHDIM, INCREASE NCHDIM' 
              WRITE (LU,*) 'IPID=',IPID  
              CALL PLANTE(1) 
              STOP 
            ENDIF             
            HEAPCHAR(NCHARA)%MYPID=IPID        ! THE ORIGIN PID  
            HEAPCHAR(NCHARA)%NEPID=ISUB(IPLOT) ! THE NEXT PID  
            HEAPCHAR(NCHARA)%INE=ELT(IPLOT)    ! ELEMENT THERE  
            HEAPCHAR(NCHARA)%KNE=ETA(IPLOT)    ! LEVEL THERE 
            HEAPCHAR(NCHARA)%IFR=FRE(IPLOT)    ! FREQUENCY THERE     
            HEAPCHAR(NCHARA)%IOR=IPLOT         ! THE ORIGIN 2D OR 3D NODE  
!           HEAPCHAR(NCHARA)%ISP=0             ! R-K STEP AS COLLECTED (NOT USED) 
!           HEAPCHAR(NCHARA)%NSP=0             ! R-K STEPS TO BE DONE  (NOT USED) 
            HEAPCHAR(NCHARA)%XP=SHP(1,IPLOT)   ! X-POSITION (HERE SHP1)  
            HEAPCHAR(NCHARA)%YP=SHP(2,IPLOT)   ! Y-POSITION (HERE SHP2)  
            HEAPCHAR(NCHARA)%ZP=SHP(3,IPLOT)   ! Z-POSITION (HERE SHP3)
            HEAPCHAR(NCHARA)%DX=SHZ(IPLOT)     ! X-DISPLACEMENT (HERE SHZ)  
            HEAPCHAR(NCHARA)%DY=SHF(IPLOT)     ! Y-DISPLACEMENT (HERE SHF)   
!           HEAPCHAR(NCHARA)%DZ=0.D0           ! Z-DISPLACEMENT (NOT USED)   
!
            HEAPCOUNTS(ISUB(IPLOT)+1)=HEAPCOUNTS(ISUB(IPLOT)+1)+1
          ENDIF
          ENDDO
        ELSE
          DO IPLOT=1,NPLOT
          IF(ISUB(IPLOT).NE.IPID) THEN
            NCHARA=NCHARA+1 
            IF(NCHARA.GT.NCHDIM.OR.NCHARA.GT.SIZEBUF) THEN  
              WRITE (LU,*) 'NCHARA=',NCHARA,' NCHDIM=',NCHDIM 
              WRITE (LU,*) 'SIZEBUF=',SIZEBUF  
              WRITE (LU,*) 'POST_INTERP::NCHARA>NCHDIM, INCREASE NCHDIM' 
              WRITE (LU,*) 'IPID=',IPID  
              CALL PLANTE(1) 
              STOP 
            ENDIF             
            HEAPCHAR(NCHARA)%MYPID=IPID        ! THE ORIGIN PID  
            HEAPCHAR(NCHARA)%NEPID=ISUB(IPLOT) ! THE NEXT PID  
            HEAPCHAR(NCHARA)%INE=ELT(IPLOT)    ! ELEMENT THERE  
            HEAPCHAR(NCHARA)%KNE=ETA(IPLOT)    ! LEVEL THERE 
!           HEAPCHAR(NCHARA)%IFR=0             ! NO FREQUENCY THERE     
            HEAPCHAR(NCHARA)%IOR=IPLOT         ! THE ORIGIN 2D OR 3D NODE  
!           HEAPCHAR(NCHARA)%ISP=0             ! R-K STEP AS COLLECTED (NOT USED) 
!           HEAPCHAR(NCHARA)%NSP=0             ! R-K STEPS TO BE DONE  (NOT USED) 
            HEAPCHAR(NCHARA)%XP=SHP(1,IPLOT)   ! X-POSITION (HERE SHP1)  
            HEAPCHAR(NCHARA)%YP=SHP(2,IPLOT)   ! Y-POSITION (HERE SHP2)  
            HEAPCHAR(NCHARA)%ZP=SHP(3,IPLOT)   ! Z-POSITION (HERE SHP3)
            HEAPCHAR(NCHARA)%DX=SHZ(IPLOT)     ! X-DISPLACEMENT (HERE SHZ)  
            HEAPCHAR(NCHARA)%DY=SHF(IPLOT)     ! NO SHF HERE   
!           HEAPCHAR(NCHARA)%DZ=0.D0           ! Z-DISPLACEMENT (NOT USED)   
!
            HEAPCOUNTS(ISUB(IPLOT)+1)=HEAPCOUNTS(ISUB(IPLOT)+1)+1
          ENDIF
          ENDDO
        ENDIF     
!    
        NSEND=NCHARA 
        NLOSTCHAR=NSEND    
!         
        IF(P_IMAX(NSEND).GT.0) THEN ! THERE ARE LOST TRACEBACKS SOMEWHERE 
! 
!         PREPARE INITIAL SENDING OF COLLECTED LOST TRACEBACKS
!         BASICALLY HEAPCHAR IS COPIED TO SENDCHAR...
! 
          CALL PREP_INITIAL_SEND(NSEND,NLOSTCHAR,NCHARA) 
! 
!         NOW THE TRANSMISSION VIA ALL-TO-ALL COMMUNICATION
!         RECVCHAR WILL BE FILLED
! 
          CALL GLOB_CHAR_COMM()     
! 
!         COMPUTE THE NUMBER OF SET OF DATA ARRIVED 
!  
          NARRV = SUM(RECVCOUNTS)       
          ISTOP=0 
          IF(NARRV.GT.NCHDIM) THEN 
            ISTOP=1 
            WRITE(LU,*) 'NARRV=',NARRV,' NCHDIM=',NCHDIM 
          ENDIF
          IF(NARRV.GT.SIZEBUF) THEN 
            ISTOP=1 
            WRITE(LU,*) 'SIZEBUF=',SIZEBUF,' NCHDIM=',NCHDIM 
          ENDIF  
          ISTOP=P_ISUM(ISTOP) 
          IF(ISTOP.GT.0) THEN 
            WRITE (LU,*) 'TOO MANY LOST TRACEBACKS IN ',ISTOP, 
     *                   ' PROCESSORS' 
            CALL PLANTE(1) 
            STOP 
          ENDIF 
!
!         RETRIEVING SHP, SHZ, ELT, ETA IN RECVCHAR
!
          IF(NARRV.GT.0) THEN
            IF(YA4D) THEN
              DO I=1,NARRV
                WELT(I)  =RECVCHAR(I)%INE
                WETA(I)  =RECVCHAR(I)%KNE
                WFRE(I)  =RECVCHAR(I)%IFR
                WSHP(1,I)=RECVCHAR(I)%XP
                WSHP(2,I)=RECVCHAR(I)%YP
                WSHP(3,I)=RECVCHAR(I)%ZP
                WSHZ(I)  =RECVCHAR(I)%DX
                WSHF(I)  =RECVCHAR(I)%DY
!               TO TRIGGER INTERPOLATION (SEE IN INTERP_RECVCHAR...)
                RECVCHAR(I)%NEPID=-1
              ENDDO
            ELSE  
              DO I=1,NARRV
                WELT(I)  =RECVCHAR(I)%INE
                WETA(I)  =RECVCHAR(I)%KNE
                WSHP(1,I)=RECVCHAR(I)%XP
                WSHP(2,I)=RECVCHAR(I)%YP
                WSHP(3,I)=RECVCHAR(I)%ZP
                WSHZ(I)  =RECVCHAR(I)%DX
!               TO TRIGGER INTERPOLATION (SEE IN INTERP_RECVCHAR...)
                RECVCHAR(I)%NEPID=-1
              ENDDO
            ENDIF
          ENDIF
! 
!         LOCAL INTERPOLATION BEFORE SENDING BACK THE RESULT 
!         PARADOXICALLY HERE PARAMETER POST IS SET TO .FALSE.
!         BECAUSE SHP, ETC. ARE ALREADY SAVED ONCE FOR ALL CALLS 
! 
          IF(NARRV.GT.0) THEN 
            IF(U%TYPE.EQ.2) THEN 
              IF(IELM.EQ.11) THEN 
                CALL INTERP_RECVCHAR_11 
     &         (U%R,1,IKLE,WELT,WSHP,NELMAX,U%DIM1,NARRV,U%ELM,
     &          .FALSE.,NOMB) 
              ELSEIF(IELM.EQ.41) THEN 
                CALL INTERP_RECVCHAR_41 
     &         (U%R,1,IKLE,WELT,WETA,WFRE,WSHP,WSHZ,WSHF,NELMAX,
     &          NPOIN2,NPLAN,NARRV,.FALSE.,NOMB,PERIO,YA4D)
              ELSE 
                WRITE(LU,*) 'WRONG IELM IN POST_INTERP:',IELM 
                CALL PLANTE(1) 
                STOP 
              ENDIF 
            ELSEIF(U%TYPE.EQ.4) THEN 
              IF(IELM.EQ.11) THEN  
                DO I=1,NOMB 
                  CALL INTERP_RECVCHAR_11 
     &          (U%ADR(I)%P%R,I,IKLE,WELT,WSHP,NELMAX, 
     &           U%ADR(I)%P%DIM1,NARRV,U%ADR(I)%P%ELM,.FALSE.,NOMB) 
                ENDDO  
              ELSEIF(IELM.EQ.41) THEN  
                DO I=1,NOMB  
                  CALL INTERP_RECVCHAR_41 
     &             (U%ADR(I)%P%R,I,IKLE,WELT,
     &              WETA,WFRE,WSHP,WSHZ,WSHF, 
     &              NELMAX,NPOIN2,NPLAN,NARRV,
     &              .FALSE.,NOMB,PERIO,YA4D) 
                ENDDO 
              ELSE  
                WRITE(LU,*) 'WRONG IELM IN POST_INTERP:',IELM 
                CALL PLANTE(1) 
                STOP 
              ENDIF 
            ENDIF 
          ENDIF 
! 
          NCHARA = NARRV
          IF(NARRV.GT.0) THEN
            DO I=1,NARRV  
              HEAPCHAR(I) = RECVCHAR(I) ! ALREADY INTERPOLATED?  
              HEAPCOUNTS(HEAPCHAR(I)%MYPID+1) =  
     &        HEAPCOUNTS(HEAPCHAR(I)%MYPID+1)+1  
            ENDDO
          ENDIF  
!
!         PREPARE SENDING OF THE HEAPED LOCALISED TRACEBACKS 
!
          CALL PREP_SENDBACK(NOMB,NCHARA)
!
!         THE FINAL SEND/RECV OF INTERPOLATED VALUES VIA ALLTOALL COMM.
!
          CALL GLOB_CHAR_COMM() 
!
!         THE DATA RECEIVED HERE MUST BE IN SAME NUMBER THAT THE ONE 
!         INITIALLY SENT    
!
          NARRV = SUM(RECVCOUNTS) ! FROM RECVCOUNTS / THE FINALLY ARRIVED ONES            
! 
          IF(NARRV.NE.NLOSTCHAR) THEN ! THE MOST SERIOUS PROBLEM WE CAN HAVE  
            WRITE (LU,*) ' @STREAMLINE::POST_INTERP ::',  
     &                   ' THE NUMBER OF INITALLY LOST TRACEBACKS', 
     &                   ' /= THE NUMBER OF FINALLY ARRIVED ONES ' 
            WRITE (LU,*) '             NLOSTCHAR, NARRV: ',  
     &                                 NLOSTCHAR, NARRV 
            CALL PLANTE(1) 
            STOP  
          ENDIF   
! 
!         INTRODUCE THE VALUES FROM THE RECEIVED TRACEBACK BASKETS  
! 
          IF(NARRV.GT.0) THEN
            CALL INTRODUCE_RECVCHAR(UTILD,NOMB,NARRV,IELM,
     &                              SHP,SHZ,SHF,ELT,ETA,FRE,
!                                   NO NEED TO SAVE THEM
!                                   THEY ARE ALREADY SAVED
     &                              .FALSE.,YA4D) 
!
          ENDIF 
! 
        ENDIF 
        CALL RE_INITIALISE_CHARS(NSEND,NLOSTCHAR,NLOSTAGAIN,NARRV) ! DEALLOCATING
!
      ENDIF 
!
      LAST_NOMB=NOMB 
! 
!----------------------------------------------------------------------- 
! 
15    FORMAT(1X,'STREAMLINE::POST_INTERP::', 
     &          ' MAUVAIS BLOC DES VARIABLES : ',2I6) 
16    FORMAT(1X,'STREAMLINE::POST_INTERP::', 
     &          ' WRONG BLOCK OF VARIABLES : ',2I6) 
! 
17    FORMAT(1X,'STREAMLINE::POST_INTERP:: TYPE D''OBJET INCONNU : ',
     &       2I6) 
18    FORMAT(1X,'STREAMLINE::POST_INTERP:: UNKNOWN TYPE OF OBJECT : ',
     &       2I6) 
! 
!----------------------------------------------------------------------- 
! 
      RETURN  
      END SUBROUTINE POST_INTERP
!
      END MODULE STREAMLINE
