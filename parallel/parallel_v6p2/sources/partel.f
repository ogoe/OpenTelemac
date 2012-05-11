!                    **************
                     PROGRAM PARTEL
!                    **************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    PREPROCESSING STEP BEFORE A PARALLEL COMPUTATION
!
!history   R. KOPMANN (BAW)
!+
!+
!+         FIRST  VERSION JANUARY-MARCH 2000
!
!history   JAJ
!+      12/12/2000
!+      SECOND VERSION PINXIT
!+     PARTITIONING OF GEOMETRY AND 2D RESULT FILES POSSIBLE

!history   JAJ
!+      22/02/2002
!+      THIRD VERSION
!+     ERRORS IN BC VALUES IN DECOMPOSED BC FILES REMOVED
!+     ERRONEOUS TREATMENT OF ISLANDS DEBUGGED
!
!history   J-M HERVOUET ; JAJ
!+      17/04/2002
!+     FOURTH VERSION
!+     PARTITIONING FOR 3D RESULT FILES DONE BY JMH
!+     INCLUDING BOTH PARTITIONING METHODS AND BEAUTIFYING BY JAJ
!
!history  J-M HERVOUET
!+     21/01/2003
!+     FIFTH VERSION
!+     CORRECTED A WRONG DIMENSION OF THE ARRAY CUT, AN ERROR
!+     OCCURING BY A LARGER NUMBER OF PROCESSORS
!
!history  JAJ; MATTHIEU GONZALES DE LINARES
!+        27/01/2003
!+        SIXTH VERSION
!+    CORRECTED A WRONG DIMENSION OF THE ARRAY ALLVAR
!
!history  J-M HERVOUET
!+       12/03/2003
!+      SEVENTH VERSION
!+      ALGORITHM CHANGED : A SEGMENT IS IN A SUBDOMAIN IF IT BELONGS
!+      TO AN ELEMENT IN THE SUBDOMAIN NOT IF THE 2 POINTS OF THE
!+      SEGMENT BELONG TO THE SUBDOMAIN.
!+       SPECIFIC ELEBD INCLUDED, ALL REFERENCE TO MPI OR BIEF REMOVED
!
!history  J-M HERVOUET
!+        01/09/2003
!+      EIGHTH VERSION
!+      UBOR AND VBOR INVERTED LINE 613 WHEN READING THE CLI FILE.
!
!history   C. MOULINEC, P. VEZOLLE, O. BOITEAU
!+
!+
!+    OTHER MODIFICATIONS PERFORMED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!     MAXIMUM GEOMETRICAL MULTIPLICITY OF A NODE (VARIABLE AUSSI
!     PRESENTE DANS LA BIEF, NE PAS CHANGER L'UNE SANS L'AUTRE)
      INTEGER, PARAMETER :: NBMAXNSHARE =  10
!     MAXIMUM NUMBER OF HALO, IN THE PARALLEL VERSION THE NUMBER OF HALO WILL BE DIRECTLY COMPUTED
      INTEGER, PARAMETER :: NBMAXHALO=100000
!
      INTEGER, PARAMETER :: MAXNPROC = 100000 ! MAX PARTITION NUMBER [00000..99999]
      INTEGER, PARAMETER :: MAXLENSOFT = 144 ! SOFT MAX FILE NAME LENGTH
      INTEGER, PARAMETER :: MAXLENHARD = 250 ! HARD MAX FILE NAME LENGTH
      INTEGER, PARAMETER :: MAXADDCH = 10 ! MAX ADDED SUFFIX LENGTH
      INTEGER, PARAMETER :: MAXVAR = 100  ! MAX NUMBER OF VARIABLES
      INTEGER, PARAMETER :: MAXALLVARLENGTH = 3200 ! MAXVAR*32 FOR ALLVAR
!
      INTEGER PMETHOD
      INTEGER NVAR, NREC, NPLAN, NPTFR, NPTIR, NPTFRMAX
      INTEGER NELEM, NPOIN, NDP, NELEM2, NPOIN2, NDUM
      INTEGER IB(10)
!
      INTEGER, ALLOCATABLE :: IKLES(:), IKLES_P(:)
      INTEGER, ALLOCATABLE :: IKLES3D(:),IKLES3D_P(:,:,:)
      INTEGER, ALLOCATABLE :: IRAND(:), IRAND_P(:)
      INTEGER, ALLOCATABLE :: LIHBOR(:), LIUBOR(:), LIVBOR(:)
      INTEGER, ALLOCATABLE :: LITBOR(:)
      INTEGER, ALLOCATABLE :: NPOIN_P(:), NELEM_P(:), NPTFR_P(:)
      INTEGER, ALLOCATABLE :: NBOR(:), NBOR_P(:), NPTIR_P(:)
      INTEGER, ALLOCATABLE :: NUMLIQ(:), NUMSOL(:)
      INTEGER, ALLOCATABLE :: KNOLG(:,:), KNOGL(:,:),CHECK(:)
      INTEGER, ALLOCATABLE :: ELELG(:,:), ELEGL(:)
      INTEGER, ALLOCATABLE :: CUT(:), CUT_P(:,:), SORT(:)
      INTEGER, ALLOCATABLE :: PART_P(:,:), PART(:)
!
      REAL, ALLOCATABLE    :: F(:,:), F_P(:,:,:)
      REAL, ALLOCATABLE    :: HBOR(:) 
      REAL, ALLOCATABLE    :: UBOR(:), VBOR(:), AUBOR(:)
      REAL, ALLOCATABLE    :: TBOR(:), ATBOR(:), BTBOR(:)
!
      REAL TIMES, TIMED
!
      INTEGER :: NINP=10, NCLI=11, NMET=12,NINPFORMAT=52
      INTEGER :: NEPART=15, NNPART=16, NOUT=17, NCLM=18
      INTEGER TIME(3),DATE(3),TIME_TMP(3), DATE_TMP(3)
!
      CHARACTER(LEN=80)  :: TITLE
      CHARACTER(LEN=32)  :: VARI, VARIABLE(MAXVAR)
      CHARACTER(LEN=MAXALLVARLENGTH) :: ALLVAR 
      CHARACTER(LEN=MAXLENHARD)  :: NAMEINP, NAMECLI, NAMEOUT, NAMECLM
      CHARACTER(LEN=MAXLENHARD)  :: NAMEMET,NAMEEPART,NAMENPART
      CHARACTER(LEN=MAXLENHARD)  :: NAMENINPFORMAT,NAMEOUTFORMA
      CHARACTER(LEN=5)   :: CHCH  
      CHARACTER(LEN=12)  :: FMT4
!
      INTEGER MAX_NELEM_P, MIN_NELEM_P
      INTEGER  MAX_NPOIN_P,MAX_N_NEIGH
      INTEGER I, J, K, L , M, N, P, ERR, ISO, IDUM
      INTEGER ISTOP, ISTART, ISEG, II, ILOOP
      INTEGER I_LEN, I_S, I_SP, I_LENCLI, I_LENINP
      INTEGER IELEM_P, IPOIN_P, IPTFR_P,JJ
!
      REAL XSEG, YSEG, BAL, RDUM
      DOUBLE PRECISION AREA, X1, X2, X3, Y1, Y2, Y3
      LOGICAL IS, WRT, TIMECOUNT
!
! METISOLOGY
!
      INTEGER NPARTS, ETYPE, NUMFLAG, EDGECUT
      INTEGER, ALLOCATABLE :: EPART(:), NPART(:)
      CHARACTER(LEN=10) FMT1, FMT2, FMT3
!
! FOR CALLING FRONT2
!
      INTEGER, PARAMETER :: MAXFRO = 300   ! MAX NUMBER OF BOUNDARIES
      INTEGER NFRLIQ, NFRSOL, DEBLIQ(MAXFRO), FINLIQ(MAXFRO)
      INTEGER DEBSOL(MAXFRO), FINSOL(MAXFRO)
      INTEGER, ALLOCATABLE :: DEJAVU(:), KP1BOR(:,:)
!
! FOR CALLING BIEF MESH SUBROUTINES (TO BE OPTIMISED SOON):
      INTEGER, ALLOCATABLE :: IFABOR(:,:), IFANUM(:,:), NELBOR(:)
      INTEGER, ALLOCATABLE :: NULONE(:,:)
      INTEGER, ALLOCATABLE :: IKLE(:,:), IKLBOR(:,:), ISEGF(:)
      INTEGER, ALLOCATABLE :: IT1(:), IT2(:), IT3(:)

      INTEGER NPOIN_TOT
      REAL TMP
      INTEGER LNG,LU,LI
      COMMON /INFO/ LNG,LU
!
! TIME MEASURING 
!
      INTEGER  TDEB, TFIN, TDEBP, TFINP, TEMPS, PARSEC
      INTEGER  TDEB_GLOB, TFIN_GLOB
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
!
! EXTENS FUNCTION
!
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS   
!
!----------------------------------------------------------------------
!
!JAJ NEW FOR PARALLEL CHARACTERISTICS ////
! HALO ELEMENTS: THESE ADJACENT TO THE INTERFACE EDGES HAVING 
! NEIGHBOURS BEHIND A BOUNDARY 
!
      ! THE ELEMENTAL GLOBAL->LOCAL NUMBERING TRANSLATION TABLE 
      ! THIS IS ELEGL SAVED FROM ALL PARTITIONS FOR FURTHER USE
      INTEGER, ALLOCATABLE :: GELEGL(:,:)
!
      ! THE HALO ELEMENTS NEIGHBOURHOOD DESCRIPTION FOR A HALO CELL 
      INTEGER, ALLOCATABLE :: IFAPAR(:,:,:)
!
      ! THE NUMBER OF HALO CELLS PRO PARTITION 
      INTEGER, ALLOCATABLE :: NHALO(:) 
!
      ! WORK VARIABLES 
      INTEGER IFALOC(3)
      LOGICAL FOUND
      INTEGER NDP_2D,NDP_3D
      INTEGER EF,POS
      INTEGER, ALLOCATABLE :: NBRE_EF(:),NBRE_EF_LOC(:),EF_I(:)
      INTEGER, ALLOCATABLE :: TAB_TMP(:),EF_II(:)
      LOGICAL TROUVE,HALO
      INTEGER NOEUD,NBRE_NOEUD_INTERNE
      INTEGER NBRE_NOEUD_INTERF
      INTEGER FRONTIERE,NBRE_EF_I
      LOGICAL INTERFACE      

! #### FOR SECTIONS 

      TYPE CHAIN_TYPE
        INTEGER :: NPAIR(2)
        DOUBLE PRECISION :: XYBEG(2), XYEND(2)
        CHARACTER(LEN=24) :: DESCR
        INTEGER :: NSEG
        INTEGER, POINTER :: LISTE(:,:) 
      END TYPE 
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
      INTEGER, PARAMETER :: NSEMAX=500 ! MAX NUMBER OF SEGMENTS IN A SECTION 
      INTEGER, ALLOCATABLE :: LISTE(:,:), ANPBEG(:),ANPEND(:) 
      INTEGER :: NSEC, IHOWSEC, ISEC, IELEM, IM(1), IN(1), NPBEG, NPEND 
      INTEGER :: NCP, PT, I1,I2,I3, ARR,DEP, ILPREC,ILBEST,ELBEST,IGBEST 
      DOUBLE PRECISION :: XA, YA, DISTB, DISTE, DMINB, DMINE
      DOUBLE PRECISION :: DIST1, DIST2, DIST3, DIST
      CHARACTER(LEN=MAXLENHARD) :: NAMESEC
      LOGICAL :: WITH_SECTIONS=.FALSE.

!
!----------------------------------------------------------------------
!
      NDP_2D=3
      NDP_3D=6

      CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
      TIMECOUNT = .TRUE.
      IF (PARSEC==0) TIMECOUNT = .FALSE.  ! COUNT_RATE == 0 : NO CLOCK
      IF (TIMECOUNT) TDEB = TEMPS
!
      LNG=2 ! JE NE PARLE FRANCAIS, JE SUIS BARBARIEN
      LU=6  ! FORTRAN STANDARD OUPUT CHANNEL
      LI=5  ! FORTRAN STANDARD INPUT CHANNEL
       

!----------------------------------------------------------------------
! NAMES OF THE INPUT FILE TO EVENTUALLY GUIDE TO PARES3D
! IF PARALLEL COMPUTATION WITH ESTEL3D 
!
!
!=>FABS
!      DO 
!        READ(LI,'(A)')NAMEINP
!        IF (NAMEINP /= ' ') EXITABOUT:
!      ENDDO
!      IF (NAMEINP(1:3)=='ES3') THEN
! PARTEL ADAPTED TO ESTEL3D CODE
!        CALL PARES3D(NAMEINP,LI)
! BACK TO THE END OF PARTELABOUT:
!        GOTO 299
!      ELSE
! CONTINUE WITH TELEMAC CODES
!        REWIND LI
!      ENDIF 
!<=FABS
!
!----------------------------------------------------------------------
! INTRODUCE YOURSELF
!
      WRITE(LU,*) ' '
      WRITE(LU,*) '+-------------------------------------------------+'
      WRITE(LU,*) '  PARTEL: TELEMAC SELAFIN METISOLOGIC PARTITIONER'
      WRITE(LU,*) '                                                   '          
      WRITE(LU,*) '  REBEKKA KOPMANN & JACEK A. JANKOWSKI (BAW)'
      WRITE(LU,*) '                 JEAN-MICHEL HERVOUET (LNHE)'
      WRITE(LU,*) '                 CHRISTOPHE DENIS     (SINETICS) '
      WRITE(LU,*) '  PARTEL (C) COPYRIGHT 2000-2002 '
      WRITE(LU,*) '  BUNDESANSTALT FUER WASSERBAU, KARLSRUHE'
      WRITE(LU,*) ' '
      WRITE(LU,*) '  METIS 4.0.1 (C) COPYRIGHT 1998 '
      WRITE(LU,*) '  REGENTS OF THE UNIVERSITY OF MINNESOTA '
      WRITE(LU,*) ' '
      WRITE(LU,*) '  BIEF 5.9 (C) COPYRIGHT 2008 EDF'
      WRITE(LU,*) '+-------------------------------------------------+'
      WRITE(LU,*) ' '
!JAJ ////
      WRITE(LU,*) '  => THIS IS A PRELIMINARY DEVELOPMENT VERSION '
      WRITE(LU,*) '     DATED:  TUE JAN 27 11:11:20 CET 2009'
      WRITE(LU,*) ' '
      WRITE(LU,*) '  MAXIMUM NUMBER OF PARTITIONS: ',MAXNPROC
      WRITE(LU,*) ' '
      WRITE(LU,*) '+--------------------------------------------------+'
      WRITE(LU,*) ' '
!
!----------------------------------------------------------------------
! NAMES OF THE INPUT FILES:
!
!###> SEB@HRW
      INQUIRE(FILE='partel.par',EXIST=FOUND)
      IF( FOUND ) OPEN(UNIT=72,FILE='partel.par')
!###< SEB@HRW

      DO 
        IF( FOUND ) THEN                      !###> SEB@HRW
           READ(72,*) NAMEINP                 !###> SEB@HRW
        ELSE                                  !###> SEB@HRW
           WRITE(LU, ADVANCE='NO', FMT=
     &         '(/,'' SELAFIN INPUT NAME <INPUT_NAME>: '')')
           READ(LI,'(A)') NAMEINP
        ENDIF                                 !###> SEB@HRW
        IF (NAMEINP.EQ.' ') THEN
          WRITE (LU,'('' NO FILENAME'')') 
        ELSE
!=>FABS
          IF (NAMEINP(1:3)=='ES3') THEN
! PARTEL ADAPTED TO ESTEL3D CODE
            CALL PARES3D(NAMEINP,LI)
            GOTO 299
          ELSE
!<=FABS
! CONTINUE WITH TELEMAC CODES
            WRITE(LU,*) 'INPUT: ',NAMEINP
            EXIT 
!=>FABS
          ENDIF
!<=FABS
        END IF  
      END DO

      INQUIRE (FILE=NAMEINP,EXIST=IS)
      IF (.NOT.IS) THEN 
        WRITE (LU,'('' FILE DOES NOT EXIST: '',A30)') NAMEINP
        CALL PLANTE2(-1)
        STOP
      END IF  
!
      DO
        IF( FOUND ) THEN                      !###> SEB@HRW
           READ(72,*) NAMECLI                 !###> SEB@HRW
        ELSE                                  !###> SEB@HRW
           WRITE(LU, ADVANCE='NO', FMT=
     &           '(/,'' BOUNDARY CONDITIONS FILE NAME : '')')
           READ(LI,'(A)') NAMECLI
        ENDIF                                 !###> SEB@HRW
        IF (NAMECLI.EQ.' ') THEN
          WRITE (LU,'('' NO FILENAME'')') 
        ELSE
          WRITE(LU,*) 'INPUT: ',NAMECLI
          EXIT
        END IF
      END DO
!  
      INQUIRE (FILE=NAMECLI,EXIST=IS)
      IF (.NOT.IS) THEN 
        WRITE (LU,'('' FILE DOES NOT EXIST: '',A30)') NAMECLI
        CALL PLANTE2(-1)
        STOP
      END IF  
!
      DO 
        IF( FOUND ) THEN                      !###> SEB@HRW
           READ(72,*) NPARTS                  !###> SEB@HRW
        ELSE                                  !###> SEB@HRW
           WRITE(LU, ADVANCE='NO',FMT=
     &    '(/,'' NUMBER OF PARTITIONS <NPARTS> [2 -'',I6,'']: '')') 
     &        MAXNPROC
           READ(LI,*) NPARTS
        ENDIF                                 !###> SEB@HRW
        IF ( (NPARTS > MAXNPROC) .OR. (NPARTS < 2) ) THEN
          WRITE(LU,
     &    '('' NUMBER OF PARTITIONS MUST BE IN [2 -'',I6,'']'')') 
     &      MAXNPROC
        ELSE
          WRITE(LU,'('' INPUT: '',I4)') NPARTS
          EXIT
        END IF 
      END DO
!
      WRITE(LU,FMT='(/,'' PARTITIONING OPTIONS: '')')
!      WRITE(LU,*) '  1: DUAL  GRAPH', 
!     & ' (EACH ELEMENT OF THE MESH BECOMES A VERTEX OF THE GRAPH)'
!      WRITE(LU,*) '  2: NODAL GRAPH', 
!     & ' (EACH NODE OF THE MESH BECOMES A VERTEX OF THE GRAPH)'

      DO 
        IF( FOUND ) THEN                      !###> SEB@HRW
           READ(72,*) PMETHOD                 !###> SEB@HRW
        ELSE                                  !###> SEB@HRW
           WRITE(LU, ADVANCE='NO',FMT=
     &    '(/,'' PARTITIONING METHOD <PMETHOD> [1 OR 2]: '')') 
           READ(LI,*) PMETHOD
        ENDIF                                 !###> SEB@HRW
        IF ( (PMETHOD > 2) .OR. (PMETHOD < 1) ) THEN
          WRITE(LU,
     &    '('' PARTITIONING METHOD MUST BE 1 OR 2'')') 
        ELSE
          WRITE(LU,'('' INPUT: '',I3)') PMETHOD
          EXIT
        END IF 
      END DO
!
! #### THE SECTIONS FILE NAME 

      DO
        IF( FOUND ) THEN                      !###> SEB@HRW
           READ(72,*) I                       !###> SEB@HRW
        ELSE                                  !###> SEB@HRW
           WRITE(LU, ADVANCE='NO',FMT=
     &    '(/,'' WITH SECTIONS? [1:YES 0:NO]: '')') 
           READ(LI,*) I
        ENDIF                                 !###> SEB@HRW
        IF ( I<0 .OR. I>1 ) THEN
          WRITE(LU,
     &    '('' PLEASE ANSWER 1:YES OR 0:NO '')') 
        ELSE
          WRITE(LU,'('' INPUT: '',I4)') I
          EXIT
        END IF 
      END DO
      IF (I==1) WITH_SECTIONS=.TRUE.

!###> SEB@HRW
      IF( FOUND ) CLOSE(72)
!###< SEB@HRW


      IF (WITH_SECTIONS) THEN 
        DO
          WRITE(LU, ADVANCE='NO', FMT=
     &      '(/,'' CONTROL SECTIONS FILE NAME (OR RETURN) : '')')
          READ(LI,'(A)') NAMESEC
          IF (NAMESEC.EQ.' ') THEN
            WRITE (LU,'('' NO FILENAME '')') 
          ELSE
            WRITE(LU,*) 'INPUT: ',NAMESEC
            EXIT
          ENDIF
        END DO
!  
        INQUIRE (FILE=NAMESEC,EXIST=IS)
        IF (.NOT.IS) THEN
          WRITE (LU,'('' FILE DOES NOT EXIST: '',A30)') NAMESEC
          CALL PLANTE2(-1)
          STOP
        ENDIF  
      ENDIF
!
! FIND THE INPUT FILE CORE NAME LENGTH
!
      I_S  = LEN(NAMEINP)
      I_SP = I_S + 1
      DO I=1,I_S
        IF (NAMEINP(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
      I_LENINP = I_LEN
!
      IF (I_LENINP > MAXLENSOFT) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ATTENTION:'
        WRITE(LU,*) 'THE NAME OF THE INPUT FILE:'
        WRITE(LU,*) NAMEINP
        WRITE(LU,*) 'IS LONGER THAN ',MAXLENSOFT,' CHARACTERS' 
        WRITE(LU,*) 'WHICH IS THE LONGEST APPLICABLE NAME FOR TELEMAC '
        WRITE(LU,*) 'INPUT AND OUTPUT FILES. STOPPED. '
        CALL PLANTE2(-1)
        STOP
      ENDIF
!
      NAMEMET = NAMEINP(1:I_LENINP)//'.MET'
!
      OPEN(NINP,FILE=NAMEINP,STATUS='OLD',FORM='UNFORMATTED')
      REWIND NINP
!
!----------------------------------------------------------------------
! START READING THE GEOMETRY OR RESULT FILE
!
!
      READ (NINP) TITLE
      READ (NINP) I, J
      NVAR = I + J 
    
      ALLVAR(1:41) = 'X-COORDINATE----M---,Y-COORDINATE----M---'
      ISTART = 42
!
      WRITE (LU,*) 'VARIABLES ARE: '
      DO I=1,NVAR
        READ(NINP) VARI       
        VARIABLE(I) = VARI     
        DO J=1,32
          IF(VARI(J:J).EQ.' ') VARI(J:J) = '-'
        END DO
        ISTOP = ISTART+20
        IF (ISTOP.GT.MAXALLVARLENGTH) THEN
          WRITE(LU,*) 'VARIABLE NAMES TOO LONG FOR STRING ALLVAR'
          WRITE(LU,*) 'STOPPED.'
          CALL PLANTE2(-1)
          STOP
        ENDIF
        ALLVAR(ISTART:ISTART) = ','
        ALLVAR(ISTART+1:ISTOP) = VARI
        ISTART=ISTOP+1
      ENDDO 
!
! READ THE REST OF THE SELAFIN FILE
! 10 INTEGERS, THE FIRST IS THE NUMBER OF RECORDS (TIMESTEPS)
!
      READ(NINP) (IB(I), I=1,10)
      IF (IB(8).NE.0.OR.IB(9).NE.0) THEN
        WRITE(LU,*) 'THIS IS A PARTIAL OUTPUT FILE'
        WRITE(LU,*) 'MAYBE MEET GRETEL BEFORE...'
      ENDIF 
      NREC  = IB(1)
      NPLAN = IB(7) 
      IF(IB(10).EQ.1) THEN 
        READ(NINP) DATE(1),DATE(2),DATE(3),TIME(1),TIME(2),TIME(3)        
      ENDIF 
!
      READ(NINP) NELEM,NPOIN,NDP,NDUM
      NPOIN_TOT=NPOIN
      IF(NPLAN.GT.1) THEN 
        WRITE(LU,*) ' '
        WRITE(LU,*) '3D MESH DETECTED.' 
        NPOIN2 = NPOIN/NPLAN
        NELEM2 = NELEM/(NPLAN-1)
        WRITE(LU,*) 'NDP NODES PER ELEMENT:             ',NDP
        WRITE(LU,*) 'NPLAN NUMBER OF MESH LEVELS:       ',NPLAN
        WRITE(LU,*) 'NPOIN2 NUMBER OF 2D MESH NODES:    ',NPOIN2
        WRITE(LU,*) 'NPOIN NUMBER OF 3D MESH NODES:     ',NPOIN
        WRITE(LU,*) 'NELEM2 NUMBER OF 2D MESH ELEMENTS: ',NELEM2
        WRITE(LU,*) 'NELEM NUMBER OF 3D MESH ELEMENTS:  ',NELEM
        IF (MOD(NPOIN,NPLAN).NE.0) THEN 
          WRITE (LU,*) 'BUT NPOIN2 /= NPOIN3/NPLAN!'
          CALL PLANTE2(-1)
          STOP   
        ENDIF
        IF (MOD(NELEM,(NPLAN-1)).NE.0) THEN 
          WRITE (LU,*) 'BUT NELEM2 /= NELEM3/NPLAN!'
          CALL PLANTE2(-1)
          STOP
        ENDIF
        WRITE(LU,*) ' '
      ELSE
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ONE-LEVEL MESH.'
        WRITE(LU,*) 'NDP NODES PER ELEMENT:         ',NDP
        WRITE(LU,*) 'NPOIN NUMBER OF MESH NODES:    ',NPOIN
        WRITE(LU,*) 'NELEM NUMBER OF MESH ELEMENTS: ',NELEM
        WRITE(LU,*) ' '
        NPOIN2 = NPOIN
        NELEM2 = NELEM
      ENDIF
!
      IF (NDP.EQ.3) THEN  
        WRITE(LU,*) 'THE INPUT FILE ASSUMED TO BE 2D SELAFIN'
      ELSEIF (NDP.EQ.6) THEN
        WRITE(LU,*) 'THE INPUT FILE ASSUMED TO BE 3D SELAFIN'
      ELSE   
        WRITE(LU,*) 'THE ELEMENTS ARE NEITHER TRIANGLES NOR PRISMS!'
        WRITE(LU,*) 'NDP = ',NDP
        CALL PLANTE2(-1)
        STOP
      ENDIF
!
! NOW LET US ALLOCATE 
!
      ALLOCATE (IKLES(NELEM2*3),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IKLES')
      IF(NPLAN.GT.1) THEN
        ALLOCATE (IKLES3D(NELEM*NDP),STAT=ERR)
        IF (ERR.NE.0) CALL ALLOER (LU, 'IKLES3D')
      ENDIF 
      ALLOCATE (IRAND(NPOIN),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IRAND')
!     NVAR+2 : FIRST TWO FUNCTIONS ARE X AND Y
!     NPOIN IS 3D HERE IN 3D
      ALLOCATE (F(NPOIN,NVAR+2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'F')
!
! CONNECTIVITY TABLE:
!
      IF(NPLAN.LE.1) THEN
        READ(NINP) ((IKLES((K-1)*NDP+J),J=1,NDP),K=1,NELEM)        
      ELSE
        READ(NINP) ((IKLES3D((K-1)*NDP+J),J=1,NDP),K=1,NELEM)
!       BUILDING IKLES
        DO J=1,3
          DO K=1,NELEM2
            IKLES((K-1)*3+J)=IKLES3D((K-1)*6+J)
          ENDDO
        ENDDO
      ENDIF
!
! BOUNDARY NODES INDICATIONS
!
      READ(NINP) (IRAND(J),J=1,NPOIN)
! COMPUTATION OF NPTFR DONE LATER WITH THE BOUNDARY CONDITIONS FILE
! (MODIFICATION BY J-M HERVOUET ON 10/04/02)
! IRAND IS NOT ALWAYS CORRECT AND MAY LEAD TO ERRORS
!
! NUMBER OF BOUNDARY POINTS IN 2D MESH
!      NPTFR = 0
!      DO J=1,NPOIN2
!        IF(IRAND(J).NE.0) NPTFR = NPTFR+1
!      END DO 
!      WRITE (LU,*) ' '
!      WRITE (LU,*) 'NPTFR NUMBER OF BOUNDARY NODES IN 2D MESH',NPTFR
!      WRITE (LU,*) ' '
!
! X-, Y-COORDINATES
!
      READ(NINP) (F(J,1),J=1,NPOIN)
      READ(NINP) (F(J,2),J=1,NPOIN)
!     
! NOW THE LOOP OVER ALL RECORDS (TIMESTEPS) - FOR AN INITIAL 
! CONDITIONS FILE AUTOMATICALLY THE LAST TIME STEP VALUES ARE 
! TAKEN (!)
!
      ILOOP = 0
      DO 
!
! READ THE TIME STEP
!
        READ(NINP, END=111, ERR=300) TIMES
        ILOOP = ILOOP + 1
!
        TIMED = TIMES/3600
        WRITE(LU,*) 'TIMESTEP: ',TIMES,'S = ',TIMED,'H'
!
! READ THE TIME VARIABLES; NO 1 AND 2 ARE X,Y
!
        DO K=3,NVAR+2
!          WRITE(LU,*) 'NOW READING VARIABLE',K-2
          READ(NINP, END=300, ERR=300) (F(J,K), J=1,NPOIN)
!          WRITE(LU,*) 'READING VARIABLE',K-2,' SUCCESSFUL'
        END DO
      END DO
 111  CLOSE (NINP)
 !     WRITE(LU,*) ' '
       WRITE(LU,*) 'THERE HAS BEEN ',ILOOP,' TIME-DEPENDENT RECORDINGS'
 !     WRITE(LU,*) 'ONLY THE LAST ONE TAKEN INTO CONSIDERATION'
 !     WRITE(LU,*) ' '
!
!-----------------------------------------------------------------------
! ...CHECK IF THE AREA OF THE ELEMENTS ARE NEGATIVE...
! ... AREA = 0.5*ABS(X1*Y2 - Y1*X2 + Y1*X3 - X1*Y3 + X2*Y3 - Y2*X3)
! NOTICE: AREA AND X1, Y1, X2, Y2, X3, Y3 MUST BE DOUBLE PRECISION
!
!        DO J=1,NELEM
!          X1 = F(IKLES((J-1)*3+1),1)
!          Y1 = F(IKLES((J-1)*3+1),2)
!          X2 = F(IKLES((J-1)*3+2),1)
!          Y2 = F(IKLES((J-1)*3+2),2)
!          X3 = F(IKLES((J-1)*3+3),1)
!          Y3 = F(IKLES((J-1)*3+3),2)
!          AREA = X1*Y2-Y1*X2+Y1*X3-X1*Y3+X2*Y3-Y2*X3
!          IF ( AREA < 0.0 ) THEN
!            WRITE(LU,*) 'GLOBAL DOMAIN'
!            WRITE(LU,*) 'DETERMINANT OF ELEMENT',J,' IS NEGATIVE'
!            WRITE(LU,*) '(LOCAL NODE ORIENTATION IS CLOCKWISE!)'
!            WRITE(LU,*) 'DET-VALUE: ',AREA
!            WRITE(LU,*) 'NODE NR 1, X1,Y1: ',IKLES((J-1)*3+1),X1,Y1
!            WRITE(LU,*) 'NODE NR 2, X2,Y2: ',IKLES((J-1)*3+2),X2,Y2
!            WRITE(LU,*) 'NODE NR 3, X3,Y3: ',IKLES((J-1)*3+3),X3,Y3
!            CALL PLANTE2(-1)
!            STOP
!          ENDIF
!        END DO
!
!----------------------------------------------------------------------
! READ THE BOUNDARY CONDITIONS FILE
!
!      WRITE(LU,*) ' '
!      WRITE(LU,*) '--------------------------'
!      WRITE(LU,*) '  BC FILE: ',NAMECLI
!      WRITE(LU,*) '--------------------------'
!      WRITE(LU,*) ' '
!
! BUT ALLOCATE FIRST
!
!     NPTFRMAX MUST BE GREATER OR EQUAL TO NPFTR
!
      NPTFRMAX = NPOIN2
!
!     IT COULD BE THAT (TO BE PROVEN, BUT NILES MAY BE LARGE)
!     NPTFRMAX = 2*NPOIN2-NELEM2-2+2*NILES
!
      ALLOCATE (LIHBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'LIHBOR')
      ALLOCATE (LIUBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'LIUBOR')
      ALLOCATE (LIVBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'LIVBOR')
      ALLOCATE (HBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'HBOR')
      ALLOCATE (UBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'UBOR')
      ALLOCATE (VBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'VBOR')
      ALLOCATE (AUBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'AUBOR')
      ALLOCATE (TBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'TBOR')
      ALLOCATE (ATBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'ATBOR')
      ALLOCATE (BTBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'BTBOR')
      ALLOCATE (LITBOR(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'LITBOR')
      ALLOCATE (NBOR(2*NPTFRMAX),STAT=ERR)  ! FOR FRONT2
      IF (ERR.NE.0) CALL ALLOER (LU, 'NBOR')
      ALLOCATE (NUMLIQ(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NUMLIQ')
      ALLOCATE (NUMSOL(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NUMSOL')
      ALLOCATE (CHECK(NPTFRMAX),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'CHECK')
!
! CORE NAME LENGTH
!
      I_S  = LEN(NAMECLI)
      I_SP = I_S + 1
      DO I=1,I_S
         IF (NAMECLI(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
      I_LENCLI = I_LEN
!
      IF (I_LENINP > MAXLENSOFT) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ATTENTION:'
        WRITE(LU,*) 'THE NAME OF THE BOUNDARY CONDITIONS FILE:'
        WRITE(LU,*) NAMECLI
        WRITE(LU,*) 'IS LONGER THAN ',MAXLENSOFT,' CHARACTERS' 
        WRITE(LU,*) 'WHICH IS THE LONGEST APPLICABLE NAME FOR TELEMAC '
        WRITE(LU,*) 'INPUT AND OUTPUT FILES. STOPPED. '
        CALL PLANTE2(-1)
        STOP
      ENDIF
!
      OPEN(NCLI,FILE=NAMECLI,STATUS='OLD',FORM='FORMATTED')
      REWIND NCLI
!
!     READING BOUNDARY FILE AND COUNTING BOUNDARY POINTS
!
      K=1
 900  CONTINUE
      READ(NCLI,*,END=901,ERR=901) LIHBOR(K),LIUBOR(K),
     &                             LIVBOR(K),
     &             HBOR(K),UBOR(K),VBOR(K),AUBOR(K),LITBOR(K),
     &             TBOR(K),ATBOR(K),BTBOR(K),NBOR(K),CHECK(K)
!
!     NOW CHECK IS THE BOUNDARY NODE COLOUR
!     IF(CHECK(K).NE.K) THEN
!       WRITE(LU,*) 'ERROR IN BOUNDARY CONDITIONS FILE AT LINE ',K
!       CALL PLANTE2(-1)
!       STOP
!     ENDIF
      K=K+1
      GOTO 900
 901  CONTINUE
      NPTFR = K-1
!      WRITE (LU,*) ' '
!      WRITE (LU,*) 'NUMBER OF BOUNDARY NODES IN 2D MESH: ',NPTFR
!      WRITE (LU,*) ' '
      CLOSE(NCLI)
!
!----------------------------------------------------------------------
! NUMBERING OF OPEN BOUNDARIES 
! NUMBERING OF LIQUID BOUNDARY, IF 0 = SOLID
! OPN: NUMBER OF OPEN BOUNDARY
! IN ORDER TO DO IT IN THE SAME WAY AS TELEMAC DOES, 
! IT IS BEST TO CALL FRONT2 HERE
!
! FOR CALLING BIEF MESH SUBROUTINES
! CAN BE OPTIMISED / USES A LOT OF MEMORY 
! THE ONLY REASON IS TO OBTAIN KP1BOR AND NUMLIQ
!
      ALLOCATE (DEJAVU(NPTFR),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'DEJAVU')
      ALLOCATE (KP1BOR(NPTFR,2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'KP1BOR')
!JAJ----------V ////
!     CHANGED NELEM TO NELEM2, NDP TO 3 HUH! 
!     CAUSING ERRORS WHEN 3D RESTART/REFERENCE FILES ARE PARTITIONED
!     AND BC FILE IS WRITTEN AGAIN (WHAT FOR, ACTUALLY???) 
!     CAUSE: CALLING VOISIN WITH NELEM2 BUT IFABOR(NELEM=NELEM3,NDP=6)
      ALLOCATE (IFABOR(NELEM2,3),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IFABOR')
      ALLOCATE (IFANUM(NELEM2,3),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IFANUM')
      ALLOCATE (IKLBOR(NPTFR,2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IKLBOR')
      ALLOCATE (NELBOR(NPTFR),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NELBOR')
      ALLOCATE (NULONE(NPTFR,2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NULONE')
      ALLOCATE (ISEGF(NPTFR),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'ISEGF')
      ALLOCATE (IKLE(NELEM2,3),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IKLE')
      ALLOCATE (IT1(NPOIN),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IT1')
      ALLOCATE (IT2(NPOIN),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IT2')
      ALLOCATE (IT3(NPOIN),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'IT3')
!
! TRANSFORM IKLES--> IKLE FOR 2D ROUTINES  (AN OLD TELEMAC DISEASE) 
!
      DO I = 1,3
        DO J  = 1,NELEM2
          IKLE(J,I) = IKLES((J-1)*3+I)
        ENDDO
      ENDDO
!
      CALL VOISIN_PARTEL(IFABOR, NELEM2, NELEM2, 11, IKLE, NELEM2,
     &                   NPOIN2, IT1, IT2)
!
!      WRITE(LU,'(/,'' CALLING ELEBD'')')
!
      CALL ELEBD_PARTEL (NELBOR, NULONE, KP1BOR, IFABOR, NBOR, IKLE, 
     &                   NELEM2, IKLBOR, NELEM2, NELEM2, NPTFRMAX,
     &                   NPOIN2, NPTFR, 11, LIHBOR, 2, IFANUM,
     &                   1, ISEGF, IT1, IT2, IT3,NPOIN_TOT )
!
!      WRITE(LU,'(/,'' BOUNDARY TYPE NUMBERING USING FRONT2'')')
!      
      IF (NAMEINP(1:3)== 'ART') THEN
         OPEN(UNIT=89,FILE='FRONT_GLOB.DAT')
         WRITE(89,*) NPOIN_TOT
         WRITE(89,*) NPTFR
         DO K=1,NPTFR
            WRITE(89,*) NBOR(K)
         END DO 
         DO K=1,NPTFR
            WRITE(89,*) KP1BOR(K,1)
         END DO
         DO K=1,NPTFR
            WRITE(89,*) KP1BOR(K,2)
         END DO 
!         CALL FLUSH(89)
         CLOSE(89)
      END IF
      CALL FRONT2_PARTEL (NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,
     &             LIHBOR,LIUBOR,F(1:NPOIN2,1),F(1:NPOIN2,2),
     &             NBOR,KP1BOR(1:NPTFR,1),DEJAVU,NPOIN2,NPTFR,
     &             2,.TRUE.,NUMLIQ,NUMSOL,NPTFRMAX)  
!
      DEALLOCATE (DEJAVU)
!JAJ //// IFABOR APPLIED LATER FOR FINDING HALO CELL NEIGHBOURHOODS 
!!!!      DEALLOCATE (IFABOR)
      DEALLOCATE (IFANUM)
      DEALLOCATE (IKLBOR)
!     DEALLOCATE (NELBOR)
      DEALLOCATE (NULONE)
      DEALLOCATE (ISEGF)
!      DEALLOCATE (IKLE) !JAJ #### WE NEED IT FOR SECTIONS 
      DEALLOCATE (IT1)
      DEALLOCATE (IT2)
      DEALLOCATE (IT3)
!    COMMENTED BY CD 

!----------------------------------------------------------------------
! OPEN AND REWRITE METIS SOFTWARE INPUT FILES
! NOT NECESSARY IF VISUALISATION OR MANUAL DECOMPOSITIONS 
! ARE NOT REQUIRED
!
C$$$      WRITE(LU,*) ' '
C$$$      WRITE(LU,*) '---------------------------'
C$$$      WRITE(LU,*) ' METIS & PMVIS INPUT FILES '
C$$$      WRITE(LU,*) '---------------------------'
C$$$      WRITE(LU,*) ' '
C$$$!
C$$$      OPEN(NMET,FILE=NAMEMET,STATUS='UNKNOWN',FORM='FORMATTED')
C$$$      REWIND NMET
C$$$      WRITE(LU,*) 'INPUT FILE FOR PARTITIONING: ', NAMEMET
C$$$!
C$$$! THE FIRST LINE IS NOT NECESSARY IN THE LATEST VERSION
C$$$! WE WRITE THE FILES USING C CONVENTION
C$$$!
C$$$! HERE THE IKLE 2D IS WRITTEN, EVEN IN 3D (HENCE NDP CONSIDERED TO BE 3)
C$$$!
C$$$      WRITE(NMET,*) NELEM2,'1'
C$$$      DO K=1,NELEM2
C$$$        WRITE(NMET,'(3(I7,1X))') (IKLES((K-1)*3+J)-1, J=1,3)
C$$$      END DO
C$$$      CLOSE(NMET)
C$$$!
! WRITE THE NODE COORDINATES FOR VISUALISATION 
! A CHECK FIRST...
!
C$$$      WRITE (LU,'(/,'' AS COORDINATES FOR VISUALISATION TAKEN: '')')
C$$$      WRITE (*,'(1X,A20)') ALLVAR(1:20)
C$$$      WRITE (*,'(1X,A20)') ALLVAR(22:41)
C$$$      WRITE (*,'(1X,A20)') ALLVAR(43:62)
!
!======================================================================
! PARTITIONING
!
!

      !======================================================================
! STEP 2 : PARTITIONING THE MESH 
!
! OTHER PARTITIONING METHODS SHOULD BE USED (SCOTCH FOR EXAMPLE)
!     ALL PROCESSORS PERFORM THIS TASK TO AVOID COMMUNICATION
!     THE USE OF PARMETIS OR PTSCOTCH COULD BE USED FOR LARGER MESHES
!     IF THERE WILL BE SOME MEMORY ALLOCATION PROBLEM 
!======================================================================      
    
      ALLOCATE (EPART(NELEM2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'EPART')
      ALLOCATE (NPART(NPOIN2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NPART')
!
      IF (NDP==3.OR.NDP==6) THEN 
         ETYPE = 1
      ELSE
         WRITE(LU,*) 'METIS: IMPLEMENTED FOR TRIANGLES OR PRISMS ONLY'
         CALL PLANTE2(-1)
         STOP
      ENDIF 
      
! WE ONLY USE METIS_PARTMESHDUAL AS ONLY THE FINITE ELEMENTS PARTITION
!     IS RELEVANT HERE.   
!     
!     IMPORTANT: WE USE FORTRAN-LIKE FIELD ELEMENTS NUMBERING 1...N
!     IN C VERSION, 0...N-1 NUMBERING IS APPLIED!!!
!     
      NUMFLAG = 1
!
      WRITE(LU,*) 'USING ONLY METIS_PARTMESHDUAL SUBROUTINE'
      
      WRITE(LU,*) ' THE MESH PARTITIONING STEP METIS STARTS'
      IF (TIMECOUNT) THEN 
         CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
         TDEBP = TEMPS
      ENDIF
      CALL METIS_PARTMESHDUAL 
     &     (NELEM2, NPOIN2, IKLES, ETYPE, NUMFLAG, 
     &     NPARTS, EDGECUT, EPART, NPART)
     
      WRITE(LU,*) ' THE MESH PARTITIONING STEP HAS FINISHED'
      IF (TIMECOUNT) THEN
        CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
        TFINP = TEMPS
        WRITE(LU,*) ' RUNTIME OF METIS ',
     &            (1.0*(TFINP-TDEBP))/(1.0*PARSEC),' SECONDS'
      ENDIF

      
!======================================================================
! STEP 3 : ALLOCATE THE GLOBAL  ARRAYS NOT DEPENDING OF THE PARTITION
!     
!======================================================================   
 
!      WRITE(LU,*) 'HERE '  
!     KNOGL(I) =>  GLOBAL LABEL OF THE LOCAL POINT I 
      ALLOCATE (KNOGL(NPOIN2,NPARTS),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'KNOGL')
      KNOGL(:,:)=0
      
!     NBRE_EF(I) => NUMBER OF FINITE ELEMENT CONTAINING I
!     I IS A GLOBAL LABEL 
      ALLOCATE (NBRE_EF(NPOIN2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NBRE_EF')
      
      IF(NPLAN.EQ.0) THEN
         ALLOCATE (F_P(NPOIN2,NVAR+2,NPARTS),STAT=ERR)
      ELSE
         ALLOCATE (F_P(NPOIN2,NVAR+2,NPARTS),STAT=ERR)
      ENDIF
      IF (ERR.NE.0) CALL ALLOER (LU, 'F_P')
      
      ALLOCATE (PART_P(NPOIN2,0:NBMAXNSHARE),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'PART_P')
      PART_P(:,:)=0
      
      ALLOCATE (CUT_P(NPOIN2,NPARTS),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'CUT_P')
      
      ALLOCATE (GELEGL(NELEM2,NPARTS),STAT=ERR) 
      IF (ERR.NE.0) CALL ALLOER (LU, 'GELEGL')
      
      ALLOCATE (SORT(NPOIN2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'CUT_P')
      
      ALLOCATE (CUT(NPOIN2),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'CUT_P')
      
      ALLOCATE (NELEM_P(NPARTS),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NELEM_P')
       
      ALLOCATE (NPOIN_P(NPARTS),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NPOIN_P')
      
      ALLOCATE (NPTFR_P(NPARTS),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NPTFR_P')
      
      ALLOCATE (NPTIR_P(NPARTS),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NPTIR_P')
      
      ALLOCATE (NHALO(NPARTS),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NHALO')

      ALLOCATE(TAB_TMP( NBMAXNSHARE),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'TAB_TMP')
      
      ALLOCATE(IFAPAR(NPARTS,7,NBMAXHALO),STAT=ERR)
         IF (ERR.NE.0) CALL ALLOER (LU, 'IFAPAR')
         IFAPAR(:,:,:)=0

!======================================================================
! STEP 4 : COMPUTE THE NUMBER OF FINITE ELEMENTS AND POINTS
!     BELONGING TO SUBMESH I
!
!======================================================================   

      
!     FIRSTLY, ALL MPI PROCESSES  WORK ON THE WHOLE MESH
!     ----------------------------------------------      
!   
!     LOOP OVER THE FINITE ELEMENT OF THE MESH 
!     TO COMPUTE THE NUMBER OF FINITE ELEMENTS CONTAINING EACH POINT NOEUD
         IF (NAMEINP(1:3) == 'ART') THEN     
            DO EF=1,NELEM2
               DO K=1,NDP_2D
                  NOEUD=IKLES((EF-1)*3+K)
                  IF (IRAND(NOEUD) .NE. 0) THEN
                     EPART(EF)=1
                  END IF
               END DO 
            END DO
         END IF
         
         NBRE_EF(:)=0
      DO EF=1,NELEM2
         DO K=1,NDP_2D
            NOEUD=IKLES((EF-1)*3+K)
            NBRE_EF(NOEUD)=NBRE_EF(NOEUD)+1
         END DO
      END DO 
      DO I=1,NPARTS
         
     

!     LOOP OVER THE FINITE ELEMENT OF THE MESH TO COMPUTE 
!     THE NUMBER OF THE FINITE ELEMENT AND POINTS BELONGING 
!     TO SUBMESH I
   
         NELEM_P(I)=0
         NPOIN_P(I)=0
         DO EF=1,NELEM2
            IF (EPART(EF) .EQ. I) THEN
               NELEM_P(I)=NELEM_P(I)+1
               DO K=1,NDP_2D
                  NOEUD=IKLES((EF-1)*3+K)
                  IF (KNOGL(NOEUD,I) .EQ. 0) THEN
                     NPOIN_P(I)=NPOIN_P(I)+1
                     KNOGL(NOEUD,I)=NPOIN_P(I)
                  END IF
               END DO 
            END IF
         END DO
      END DO  
    
!======================================================================
!     STEP 4 : ALLOCATION OF LOCAL ARRAYS NEEDED BY MPI PROCESSUS ID
!              WORKING ON SUBMESH ID+1
!======================================================================   
 !     WRITE(LU,*) 'AFTER THE FIRST LOOP'
      MAX_NELEM_P=MAXVAL(NELEM_P)
      MAX_NPOIN_P=MAXVAL(NPOIN_P)


!     ELEGL(E) => GLOBAL LABEL OF THE FINITE ELEMENT E
!     E IS THE LOCAL LABEL ON SUBMESH I 
      ALLOCATE (ELELG(MAX_NELEM_P,NPARTS),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'ELELG')
      ELELG(:,:)=0
!     KNOLG(I) => GLOBAL LABEL OF THE POINT I
!     I IS THE LOCAL LABEL ON SUBDOMAIN I
      IF(NPLAN.EQ.0) THEN
         ALLOCATE (KNOLG(MAX_NELEM_P,NPARTS),STAT=ERR)
      ELSE
         ALLOCATE (KNOLG(MAX_NPOIN_P*NPLAN,NPARTS),STAT=ERR)
      ENDIF
      IF (ERR.NE.0) CALL ALLOER (LU, 'KNOLG')
      KNOLG(:,:)=0
!     NBRE_EF_LOC(I) : NUMBER OF FINITE ELEMENTS CONTAINING THE POINT I
!                      ON SUBMESH I  
!     I IS THE LOCAL LABEL ON SUBMESH I
      ALLOCATE (NBRE_EF_LOC(MAX_NELEM_P),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'NBRE_EF_LOC')

!     EF_I(E) IS THE GLOBAL LABEL OF THE INTERFACE FINITE ELEMENT NUMBER E
      ALLOCATE (EF_I(MAX_NELEM_P),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'EF_I')
!     EF_II(E) IS THE LOCAL LABEL OF THE INTERFACE FINITE ELEMENT NUMBER E
      ALLOCATE (EF_II(MAX_NELEM_P),STAT=ERR)
      IF (ERR.NE.0) CALL ALLOER (LU, 'EF_II')

      
!
!======================================================================
!     STEP 5 : INITIALISATION  OF LOCAL ARRAYS 
!                  (GELELG AND ELELG, NBRE_EF_LOC)
!              
!======================================================================
!   
      DO I=1,NPARTS
         NELEM_P(I)=0
         DO EF=1,NELEM2
            IF (EPART(EF) .EQ. I) THEN
               NELEM_P(I)=NELEM_P(I)+1
               ELELG(NELEM_P(I),I)=EF
               GELEGL(EF,I)=NELEM_P(I)
            END IF
         END DO
         DO J=1,NPOIN_P(I)
            NBRE_EF_LOC(J)=0
         END DO
!        
!======================================================================
!     STEP 5 : COMPUTE THE NUMBER OF BOUNDARY AND INTERFACE POINTS
!              INITIALISATION OF NBRE_EF_LOC AND F_P 
!======================================================================   
!         
         NPOIN_P(I)=0
         NPTFR_P(I)=0
         NBRE_NOEUD_INTERNE=0
         NBRE_NOEUD_INTERF=0
         
         DO J=1,NELEM_P(I)
            EF=ELELG(J,I)
            DO K=1,3
               NOEUD=IKLES((EF-1)*3+K)
               NBRE_EF_LOC(KNOGL(NOEUD,I))=
     &              NBRE_EF_LOC(KNOGL(NOEUD,I))+1 
               IF (NBRE_EF_LOC(KNOGL(NOEUD,I)) .EQ. 1) THEN
!     THE POINT NOEUD IS ENCOUNTERED FOR THE FIRST TIME 
                  NPOIN_P(I)=NPOIN_P(I)+1    
!     IS NOEUD A BOUNDARY POINT ?     
                  IF (IRAND(NOEUD) .NE. 0) THEN
                     NPTFR_P(I)= NPTFR_P(I)+1
                  END IF
!     MODIFICATION OF   KNOGL ET F_P
                  KNOLG(NPOIN_P(I),I)=NOEUD
                  DO L=1,NVAR+2
                     F_P(NPOIN_P(I),L,I)=F(NOEUD,L)
                  END DO
               END IF
!    
!     NOEUD IS A INTERNAL POINT IF ALL FINITE ELEMENTS
!     CONTAINING IT BELONGS TO THE SAME SUBMESH
               IF (NBRE_EF_LOC(KNOGL(NOEUD,I)) .EQ. NBRE_EF(NOEUD)) THEN 
                  NBRE_NOEUD_INTERNE=NBRE_NOEUD_INTERNE+1
               END IF
            END DO
         END DO
        
         NBRE_NOEUD_INTERF=NPOIN_P(I)-NBRE_NOEUD_INTERNE
         NPTIR_P(I)=0 
!     NOMBRE DE NOEUD INTERFACE DU SDI
         NBRE_EF_I=0            ! NOMBRE D'ELEMENTS FINIS INTERFACES DU SDI
         DO J=1,NELEM_P(I)      ! ON PARCOURS A NOUVEAU LES ELEMENTS FINIS DU SDI
            INTERFACE=.FALSE.
            EF=ELELG(J,I)
            DO K=1,NDP_2D
               NOEUD=IKLES((EF-1)*3+K)
               IF (ABS(NBRE_EF_LOC(KNOGL(NOEUD,I))) .NE. NBRE_EF(NOEUD))
     &          THEN
                  INTERFACE=.TRUE.
               END IF
               IF (NBRE_EF_LOC(KNOGL(NOEUD,I)) .NE.  NBRE_EF(NOEUD).AND. 
     &              NBRE_EF_LOC(KNOGL(NOEUD,I)) .GT. 0) THEN
!     NOEUD EST INTERFACE CAR IL RESTE DES ELEMENTS FINIS HORS DE SDI QUI LE CONTIENT
                  INTERFACE=.TRUE.
                  NPTIR_P(I)=NPTIR_P(I)+1
                  CUT_P(NPTIR_P(I),I)=NOEUD
                   PART_P(NOEUD,0)=PART_P(NOEUD,0)+1
                   POS=PART_P(NOEUD,0)
                   IF (POS > NBMAXNSHARE-1) THEN
                     WRITE(LU,*)  'ERROR : AN INTERFACE NODE BELONGS TO 
     &                     MORE THAN NBMAXNSHARE-1 SUBDOMAINS'
                      CALL PLANTE2(-1)
                      STOP 
                   ENDIF
                   PART_P(NOEUD,POS)=I
                   NBRE_EF_LOC(KNOGL(NOEUD,I))=
     &                  -1*NBRE_EF_LOC(KNOGL(NOEUD,I))
               END IF
            END DO 
            IF(INTERFACE) THEN 
              NBRE_EF_I=NBRE_EF_I+1 ! L'ELEMENT FINI EST DONC AUSSI INTERFACE
              EF_I(NBRE_EF_I)=EF
              EF_II(NBRE_EF_I)=J
            ENDIF
         END DO
!         
! FIRST LOOP TO COMPUTE THE NUMBER OF HALO TO ALLOCATE IFAPAR 
!        
!     FILLING OF  IFAPAR
         NHALO(I)=0
         DO J=1,NBRE_EF_I       ! ON PARCOURS JUSTE LES ELEMENTS FINIS INTERFACES POUR                             ! DETERMINER DES HALO
            EF=EF_I(J)
            HALO=.FALSE.
            IFALOC(:)=IFABOR(EF,:)
            WHERE (IFALOC .GT. 0) 
               IFALOC=EPART(IFALOC)
            END WHERE
            HALO=ANY(IFALOC .GT. 0 .AND. IFALOC .NE. I)
            IF(HALO) THEN
               NHALO(I)=NHALO(I)+1
               IF(NHALO(I) > NBMAXHALO) THEN
                 WRITE(LU,*)  'ERROR : NBMAXHALO TOO SMALL'
                 CALL PLANTE2(-1)
                 STOP 
               ENDIF
               IFAPAR(I,1,NHALO(I))=EF_II(J)
               IFAPAR(I,2:4,NHALO(I))=IFALOC(:)
               IFAPAR(I,5:7,NHALO(I))=IFABOR(EF_I(J),:)
            ENDIF
         ENDDO 
      ENDDO
!         
      MAX_N_NEIGH=MAXVAL(PART_P(:,0))
      IF ( MAX_N_NEIGH > NBMAXNSHARE-1 ) THEN 
         WRITE(LU,*) 'SERIOUS WARNING: ' 
         WRITE(LU,*) 
     &        'AN INTERFACE NODE BELONGS TO ',
     &        'MORE THAN NBMAXNSHARE-1 SUBDOMAINS'
         WRITE(LU,*) 'TELEMAC MAY PROTEST!'
      END IF 
      IF (MAX_N_NEIGH > MAXNPROC) THEN
         WRITE (LU,*) 'THERE IS A NODE WHICH BELONGS TO MORE THAN ',
     &        MAXNPROC,' PROCESSORS, HOW COME?'
         CALL PLANTE2(-1)
          STOP 
       ENDIF
       IF (MAX_N_NEIGH < NBMAXNSHARE-1) MAX_N_NEIGH = NBMAXNSHARE-1
       
      DO I=1,NPARTS
        
!-----------------------------------------------------------------------
! THE CORE NAMES FOR THE OUTPUT BC FILES ACCORDING TO THE NUMBER OF PARTS
!
      NAMECLM = NAMECLI    ! CORE NAME LENGTH IS I_LENCLI
      NAMEOUT = NAMEINP    ! CORE NAME LENGTH IS I_LENINP
!
!----------------------------------------------------------------------
!     WORK ON THE BOUNDARIES WRITING THE BC FILES SIMULTANEOUSLY...
!     
        NAMECLM(I_LENCLI+1:I_LENCLI+11) = EXTENS(NPARTS-1,I-1)
        OPEN(NCLM,FILE=NAMECLM,STATUS='UNKNOWN',FORM='FORMATTED')
        REWIND(NCLM)
!     
! FILE OPENED, NOW WORK ON BOUNDARIES 
! -----------------------------------
!
! WHEN THE BOUNDARY NODE BELONGS TO THIS SUBDOMAIN IT WILL BE TAKEN
! J IS THE RUNNING BOUNDARY NODE NUMBER
!
!        NPTIR = 0
         J = 0
!
         DO K=1,NPTFR
!
! BOUNDARY NODES BELONGING TO THIS PARTITION
!
            IF ( KNOGL(NBOR(K),I) /= 0) THEN
               J = J + 1
!               NBOR_P(J) = NBOR(K)
               ISEG = 0
               XSEG = 0.0
               YSEG = 0.0
!     
!     IF THE ORIGINAL (GLOBAL) BOUNDARY LEADS FURTHER INTO 
!     ANOTHER PARTITION THEN ISEG IS SET NOT EQUAL TO ZERO
!     THE NEXT NODE ALONG THE GLOBAL BOUNDARY HAS IPTFR = M
!     (BUT CHECK THE CASE THE CIRCLE CLOSES)
!     
               M = KP1BOR(K,1)
!
! NBOR_P CANNOT BE USED, IT IS NOT FULLY FILLED WITH DATA
!
               ISO = 0
!     MODIF JMH ON 10/03/2003 : CHECKING IF THE ADJACENT ELEMENT IS NOT IN THE
!     SUB-DOMAIN
               IF (EPART(NELBOR(K)).NE.I) THEN
!     THIS WAS A TEST : IF NEXT BOUNDARY POINT NOT IN THE SUBDOMAIN
!     BUT IT CAN BE IN WHEREAS THE SEGMENT IS NOT.
!     IF ( KNOGL(NBOR(M)) == 0 ) THEN
!                  WRITE(LU,*) 
!     &                 'GLOBAL BOUNDARY LEAVES @NODE (#G,#L): ',
!     &                 NBOR(K), KNOGL(NBOR(K),I),
!     &                 ' --> (#G) ', NBOR(M)
!     
                  ISEG = NBOR(M)
                  XSEG = F(ISEG,1)
                  YSEG = F(ISEG,2)
C                  NPTIR = NPTIR + 1
C                  CUT(NPTIR) = IRAND_P(KNOGL(NBOR(K)))
                  ISO = ISO + 1
            ENDIF
!     
            M = KP1BOR(K,2)
!     
!     MODIF JMH ON 10/03/2003 : SAME AS ABOVE, BUT PREVIOUS SEGMENT ,THUS M, NOT K
            IF (EPART(NELBOR(M)).NE.I) THEN
!     IF ( KNOGL(NBOR(M) ) == 0 ) THEN
!               WRITE(LU,*) 
!     &              'GLOBAL BOUNDARY ENTERS @NODE (#G,#L): ',
!     &              NBOR(K), KNOGL(NBOR(K),I),
!     &              ' <-- (#G) ', NBOR(M)
!     
               ISEG = -NBOR(M)
               XSEG = F(-ISEG,1)
               YSEG = F(-ISEG,2)
               ISO = ISO + 1
C               NPTIR = NPTIR + 1
C               CUT(NPTIR) = IRAND_P(KNOGL(NBOR(K)))
            ENDIF
!     
!     WHEN BOTH NEIGHBOURS BOUNDARY NODES BELONG TO ANOTHER PARTITION
!     
            IF (ISO == 2) THEN
               ISEG = -9999
               ISO = 0
               WRITE(LU,*) 'ISOLATED BOUNDARY POINT', 
     &              NBOR(K), KNOGL(NBOR(K),I)
            ENDIF
!     
!            NBOR_P(J) = IRAND_P(KNOGL(NBOR(K)))
!     
!     WRITE A LINE OF THE FIRST (CLASSICAL) PART OF THE BOUNDARY FILE
! CONCERNING THE NODE WHICH HAS BEEN RESEARCHED
!
            WRITE (NCLM,4000) 
     &           LIHBOR(K), LIUBOR(K), LIVBOR(K),
     &           HBOR(K), UBOR(K), VBOR(K), 
     &            AUBOR(K), LITBOR(K), TBOR(K), ATBOR(K), BTBOR(K),
!     JMH 16/06/2008: INITIAL LINE NUMBER OR COLOUR
     &           NBOR(K),CHECK(K), ISEG, XSEG, YSEG, NUMLIQ(K)
!     &            NBOR(K),    J   , ISEG, XSEG, YSEG, NUMLIQ(K)
     
!     19/10/2007 ER+JMH SUR RECOMMANDATION CHARLES MOULINEC
!     MAIS XSEG ET YSEG NE SONT PLUS UTILISES
 4000       FORMAT (1X,I2,1X,2(I1,1X),3(F24.12,1X),1X,
     &           F24.12,3X,I1,1X,3(F24.12,1X),1I9,1X,1I9,
     &           1X,I10,1X,2(F27.15,1X),I6)
         ENDIF
!     
      END DO
      
      FMT4='(I7)'
      WRITE (NCLM,*) NPTIR_P(I)
       IF (MAX_N_NEIGH < NBMAXNSHARE-1) MAX_N_NEIGH = NBMAXNSHARE-1
       FMT4='(   (I7,1X))'
       WRITE (FMT4(2:4),'(I3)') MAX_N_NEIGH+1
  
       ! SORTING NODE NUMBERS TO SORT(J) SO THAT CUT_P(SORT(J)) IS ORDERED 
! CUT IS OVERWRITTEN NOW
!
         DO J=1,NPTIR_P(I)
           CUT(J)=CUT_P(J,I)
         END DO
!
! IF A NODE HAS BEEN ALREADY FOUND AS MIN, CUT(NODE) GETS 0
!
         DO J=1,NPTIR_P(I)
           IDUM = NPOIN2+1  ! LARGEST POSSIBLE NODE NUMBER + 1
           K=0
 401       CONTINUE
           K = K + 1
           IF ( CUT(K) /= 0 .AND. CUT_P(K,I) < IDUM ) THEN
             SORT(J) = K
             IDUM = CUT_P(K,I)
           ENDIF
           IF ( K < NPTIR_P(I) ) THEN
             GOTO 401
           ELSE
             CUT(SORT(J)) = 0
           ENDIF
         END DO
!
         DO J=1,NPTIR_P(I)
            TAB_TMP=0
            L=0
            DO K=1,MAX_N_NEIGH
              
               IF (PART_P(CUT_P(SORT(J),I),K) .NE. I .AND. 
     &        PART_P(CUT_P(SORT(J),I),K) .NE. 0) THEN
                  L=L+1
               TAB_TMP(L)=PART_P(CUT_P(SORT(J),I),K)
            END IF
         END DO 
         WRITE(NCLM,FMT=FMT4) CUT_P(SORT(J),I),
     &                  (TAB_TMP(K)-1, K=1,MAX_N_NEIGH)
         END DO
                                 !     
         DO J=1,NHALO(I)
            DO M=0,2
               IF (IFAPAR(I,2+M,J)>0) THEN
                  IFAPAR(I,5+M,J)=GELEGL(IFAPAR(I,5+M,J),
     &                 IFAPAR(I,2+M,J))
               END IF
            END DO
         END DO
          DO J=1,NHALO(I)
           DO M=0,2
              IF (IFAPAR(I,2+M,J)>0) THEN
                 IFAPAR(I,2+M,J)=IFAPAR(I,2+M,J)-1
              END IF
           END DO
        END DO
      
!
      WRITE(NCLM,'(I9)') NHALO(I)
      DO K=1,NHALO(I)
         WRITE (NCLM,'(7(I9,1X))') IFAPAR(I,:,K) 
      END DO 
     
      CLOSE(NCLM)
      END DO 

      DEALLOCATE(IFAPAR)
      DEALLOCATE(PART_P)
      DEALLOCATE(LIHBOR)
      DEALLOCATE(LIUBOR)
      DEALLOCATE(LIVBOR)
      DEALLOCATE(HBOR)
      DEALLOCATE(UBOR)
      DEALLOCATE(VBOR)
      DEALLOCATE(AUBOR)
      DEALLOCATE(LITBOR)
      DEALLOCATE(TBOR)
      DEALLOCATE(ATBOR)
      DEALLOCATE(BTBOR)
      DEALLOCATE(NBOR)
      DEALLOCATE(NUMLIQ)
      DEALLOCATE(TAB_TMP)
      DEALLOCATE(NUMSOL)
      DEALLOCATE(CHECK)
      DEALLOCATE(GELEGL)
      DEALLOCATE(CUT)
      DEALLOCATE(CUT_P)
      DEALLOCATE(SORT)
     

      IF (ERR.NE.0) CALL ALLOER (LU, 'F_P')
      ALLOCATE(IKLES_P(MAX_NELEM_P*3),STAT=ERR)
      IF(NPLAN.GT.1) THEN
        ALLOCATE(IKLES3D_P(6,MAX_NELEM_P,NPLAN-1),STAT=ERR)
      ENDIF
      IF (ERR.NE.0) CALL ALLOER (LU, 'IKLES3D_P')

      
      DO I=1,NPARTS
         WRITE(LU,*) 'ON TRAITE LE SOUS-DOMAINE', I 
!     ***************************************************************
!     WRITING GEOMETRY FILES FOR ALL PARTS/PROCESSORS
!
      NAMEOUT(I_LENINP+1:I_LENINP+11) = EXTENS(NPARTS-1,I-1)
!      WRITE(LU,*) 'WRITING GEOMETRY FILE: ',NAMEOUT      
      OPEN(NOUT,FILE=NAMEOUT,FORM='UNFORMATTED',STATUS='UNKNOWN')      
      REWIND(NOUT)
!     
!     TITLE, THE NUMBER OF VARIABLES
!     
      WRITE(NOUT) TITLE
      WRITE(NOUT) NVAR,0
      DO K=1,NVAR
         WRITE(NOUT) VARIABLE(K)
      END DO
!     
!     10 INTEGERS...
! 1.  IS THE NUMBER OF RECORDINGS IN FILES
! 8.  IS THE NUMBER OF BOUNDARY POINTS (NPTFR_P)
! 9.  IS THE NUMBER OF INTERFACE POINTS (NPTIR_P)
! 10. IS 0 WHEN NO DATE PASSED; 1 IF A DATE/TIME RECORD FOLLOWS
!
!       IB(7) = NPLAN   (ALREADY DONE)
        IB(8) = NPTFR_P(I)
        IB(9) = NPTIR_P(I)
        WRITE(NOUT) (IB(K), K=1,10)
        IF (IB(10).EQ.1) THEN 
           WRITE(NOUT) DATE(1), DATE(2), DATE(3), 
     &                TIME(1), TIME(2), TIME(3)           
        ENDIF 

        IF(NPLAN.LE.1) THEN
          WRITE(NOUT) NELEM_P(I), NPOIN_P(I), NDP, NDUM
        ELSE
          WRITE(NOUT) NELEM_P(I)*(NPLAN-1),
     &          NPOIN_P(I)*NPLAN, NDP, NDUM
        ENDIF
!     
        DO J=1,NELEM_P(I)
           EF=ELELG(J,I)
           DO K=1,3
              IKLES_P((J-1)*3+K) = KNOGL(IKLES((EF-1)*3+K),I)
           END DO
        END DO
        IF(NPLAN > 1) THEN
           DO K = 1,NPLAN-1
              DO J = 1,NELEM_P(I)       
                IKLES3D_P(1,J,K) = IKLES_P(1+(J-1)*3) + (K-1)*NPOIN_P(I)
                IKLES3D_P(2,J,K) = IKLES_P(2+(J-1)*3) + (K-1)*NPOIN_P(I)
                IKLES3D_P(3,J,K) = IKLES_P(3+(J-1)*3) + (K-1)*NPOIN_P(I)
                IKLES3D_P(4,J,K) = IKLES_P(1+(J-1)*3) +  K   *NPOIN_P(I)
                IKLES3D_P(5,J,K) = IKLES_P(2+(J-1)*3) +  K   *NPOIN_P(I)
                IKLES3D_P(6,J,K) = IKLES_P(3+(J-1)*3) +  K   *NPOIN_P(I)
              ENDDO
           ENDDO
        ENDIF
!
        IF(NPLAN.EQ.0) THEN
           WRITE(NOUT) 
     &          ((IKLES_P((J-1)*3+K),K=1,3),J=1,NELEM_P(I))
        ELSE           
           WRITE(NOUT)
     &         (((IKLES3D_P(L,J,K),L=1,6),J=1,NELEM_P(I)),K=1,NPLAN-1)
        ENDIF
!     
! INSTEAD OF IRAND, KNOLG IS WRITTEN !!!
! I.E. THE TABLE PROCESSOR-LOCAL -> PROCESSOR-GLOBAL NODE NUMBERS
!
        IF(NPLAN.EQ.0) THEN
          WRITE(NOUT) (KNOLG(J,I), J=1,NPOIN_P(I))
        ELSE
!         BEYOND NPOIN_P(I) : DUMMY VALUES IN KNOLG, NEVER USED
          WRITE(NOUT) (KNOLG(J,I), J=1,NPOIN_P(I)*NPLAN)
        ENDIF
!
! NODE COORDINATES X AND Y
!
        IF(NPLAN.EQ.0) THEN
          WRITE(NOUT) (F_P(J,1,I),J=1,NPOIN_P(I))
          WRITE(NOUT) (F_P(J,2,I),J=1,NPOIN_P(I))
        ELSE                      
          WRITE(NOUT) ((F(KNOLG(J,I)+(L-1)*NPOIN2,1),J=1,NPOIN_P(I)), 
     &          L=1,NPLAN)  
          WRITE(NOUT) ((F(KNOLG(J,I)+(L-1)*NPOIN2,2),J=1,NPOIN_P(I)), 
     &          L=1,NPLAN)  
        ENDIF
!
! TIME STAMP (SECONDS) 
!
CD
CD   -------------------------------------------------------------------
CD   MODIFICATION TO PUT ALL THE RECORDINGS IN PARALLEL 
CD   GEO FILE 08/06/2011
CD   -------------------------------------------------------------------
CD     FIRST STEP : CLOSE/REOPEN/REWIND THE FILE AND READ ALL THE RECORDINGS UNTIL
CD     THOSE CONCERNING THE TIME-DEPENDENT  VARIABLES
CD
        CLOSE(NINP)
        OPEN(NINP,FILE=NAMEINP,STATUS='OLD',FORM='UNFORMATTED')
        REWIND(NINP)
        READ (NINP) TITLE
        READ (NINP) II, JJ
        NVAR = II + JJ 
        DO II=1,NVAR
          READ(NINP) VARI
        ENDDO 
        READ (NINP) (II, JJ=1,10)
        IF(II.EQ.1) THEN 
          READ(NINP) DATE_TMP(1), DATE_TMP(2), DATE_TMP(3), 
     &               TIME_TMP(1), TIME_TMP(2), TIME_TMP(3)
        ENDIF 
        READ(NINP) II,II,II,II
        READ(NINP) ((II,JJ=1,NDP),K=1,NELEM)
        READ(NINP) (II,JJ=1,NPOIN)
        READ(NINP) (TMP,JJ=1,NPOIN)
        READ(NINP) (TMP,JJ=1,NPOIN)
CD      SECOND STEP 
CD      EACH RECORDING IS READ AND ONLY THE LOCAL VARIABLES ARE STORED 
CD      INTO THE PARALLEL GEO FILE        
        DO 
          READ(NINP, END=1111, ERR=300) TIMES
          WRITE(NOUT) TIMES
          DO K=3,NVAR+2
            READ(NINP, END=300, ERR=300) (F(J,K), J=1,NPOIN)
!
!           CORRECTION JMH 05/09/2011 : F_P IS NOT DIMENSIONED
!           FOR 3D AND IS NOT USED IN 3D
            IF(NPLAN.EQ.0) THEN
              DO JJ=1,NPOIN
                IF(KNOGL(JJ,I).GT.0) THEN
CD                IF KNOGL(JJ,I) > 0 THE VARIABLE HAVING GLOBAL NUMBER 
CD                JJ BELONGS TO THE SUBDOMAIN I AND ITS LOCAL NUMBER IS
CD                KNOGL(JJ,I) 
                  F_P(KNOGL(JJ,I),K,I)=F(JJ,K)
                ENDIF
              ENDDO
            ENDIF 
          ENDDO
          DO K=3,NVAR+2
            IF(NPLAN.EQ.0) THEN
              WRITE(NOUT) (F_P(J,K,I),J=1,NPOIN_P(I))
            ELSE
              WRITE(NOUT) ((F(KNOLG(J,I)+(L-1)*NPOIN2,K),
     &                      J=1,NPOIN_P(I)),L=1,NPLAN) 
            ENDIF
          ENDDO
        ENDDO
 1111   CONTINUE   
        CLOSE (NINP)
        CLOSE (NOUT)    
      ENDDO
CD   -------------------------------------------------------------------
CD   END OF THE MODIFICATION TO PUT ALL THE
CD   RECORDINGS IN PARALLEL GEO FILE 08/06/2011
CD   -------------------------------------------------------------------

! CD I HAVE COMMENTED THIS ... AVOIDING MULTIPLES FILES MAKING BUG ON SGI
!
!======================================================================
! WRITING EPART AND NPART 
!
C$$$      CHCH='00000'
C$$$      IF (NPARTS<10) THEN
C$$$        WRITE (CHCH(5:5),'(I1)') NPARTS
C$$$        NAMEEPART=NAMEINP(1:I_LENINP) // '.EPART.' // CHCH(5:5)
C$$$        NAMENPART=NAMEINP(1:I_LENINP) // '.NPART.' // CHCH(5:5)
C$$$      ELSEIF (NPARTS<100) THEN
C$$$        WRITE (CHCH(4:5),'(I2)') NPARTS
C$$$        NAMEEPART=NAMEINP(1:I_LENINP) // '.EPART.' // CHCH(4:5)
C$$$        NAMENPART=NAMEINP(1:I_LENINP) // '.NPART.' // CHCH(4:5)
C$$$      ELSEIF (NPARTS<1000) THEN
C$$$        WRITE (CHCH(3:5),'(I3)') NPARTS
C$$$        NAMEEPART=NAMEINP(1:I_LENINP) // '.EPART.' // CHCH(3:5)
C$$$        NAMENPART=NAMEINP(1:I_LENINP) // '.NPART.' // CHCH(3:5)
C$$$      ELSEIF (NPARTS<10000) THEN
C$$$        WRITE (CHCH(2:5),'(I4)') NPARTS
C$$$        NAMEEPART=NAMEINP(1:I_LENINP) // '.EPART.' // CHCH(2:5)
C$$$        NAMENPART=NAMEINP(1:I_LENINP) // '.NPART.' // CHCH(2:5)
C$$$      ELSE 
C$$$        WRITE (CHCH(1:5),'(I5)') NPARTS
C$$$        NAMEEPART=NAMEINP(1:I_LENINP) // '.EPART.' // CHCH(1:5)
C$$$        NAMENPART=NAMEINP(1:I_LENINP) // '.NPART.' // CHCH(1:5)
C$$$      ENDIF
C$$$!
C$$$      WRITE(LU,*) ' '
C$$$      WRITE(LU,*) '------------------'
C$$$      WRITE(LU,*) ' PARTITION FILES  '
C$$$      WRITE(LU,*) '------------------'
C$$$      WRITE(LU,*) ' '
C$$$!
C$$$      OPEN(NEPART,FILE=NAMEEPART,STATUS='UNKNOWN',FORM='FORMATTED')
C$$$      REWIND NEPART
C$$$      WRITE(LU,*) 'ELEMENT PARTITION FILE: ', NAMEEPART
C$$$!
C$$$      OPEN(NNPART,FILE=NAMENPART,STATUS='UNKNOWN',FORM='FORMATTED')
C$$$      REWIND NNPART
C$$$      WRITE(LU,*) 'NODE PARTITION FILE: ', NAMENPART
C$$$!
C$$$! OUTPUT ABSOLUTELY THE SAME AS FROM PARTDNMESH (A C-PROGRAM)
C$$$! THAT'S WHY 1 SUBSTRACTED AND THE FORMATS
C$$$!
C$$$      FMT1 = '(I1)'
C$$$      FMT2 = '(I2)'
C$$$      FMT3 = '(I3)'
C$$$!
C$$$      DO J=1,NELEM2
C$$$         K = EPART(J) - 1
C$$$         IF (K<10) THEN
C$$$           WRITE (NEPART,FMT=FMT1) K
C$$$         ELSEIF (K<100) THEN
C$$$           WRITE (NEPART,FMT=FMT2) K
C$$$         ELSE 
C$$$           WRITE (NEPART,FMT=FMT3) K
C$$$         ENDIF
C$$$      END DO
C$$$      CLOSE(NEPART)
C$$$!
C$$$      DO J=1,NPOIN2
C$$$         K = NPART(J) - 1
C$$$         IF (K<10) THEN
C$$$           WRITE (NNPART,FMT=FMT1) K
C$$$         ELSEIF (K<100) THEN
C$$$           WRITE (NNPART,FMT=FMT2) K
C$$$         ELSE
C$$$           WRITE (NNPART,FMT=FMT3) K
C$$$         ENDIF
C$$$      END DO
C$$$      CLOSE(NNPART)

!
! //// JAJ: LA FINITA COMMEDIA FOR PARALLEL CHARACTERISTICS, BYE! 
!----------------------------------------------------------------------
! !JAJ #### DEAL WITH SECTIONS 
!
      IF (NPLAN/=0) WITH_SECTIONS=.FALSE.
      IF (WITH_SECTIONS) THEN ! PRESENTLY, FOR TELEMAC2D, EV. SISYPHE 

      WRITE(LU,*) 'DEALING WITH SECTIONS'
      OPEN (NINP,FILE=TRIM(NAMESEC),FORM='FORMATTED',STATUS='OLD') 
      READ (NINP,*) ! COMMENT LINE
      READ (NINP,*) NSEC, IHOWSEC
      IF (.NOT.ALLOCATED(CHAIN)) ALLOCATE (CHAIN(NSEC))
      IF (IHOWSEC<0) THEN 
        DO ISEC=1,NSEC
          READ (NINP,*) CHAIN(ISEC)%DESCR
          READ (NINP,*) CHAIN(ISEC)%NPAIR(:)
          CHAIN(ISEC)%XYBEG(:)= (/F(CHAIN(ISEC)%NPAIR(1),1),
     &                            F(CHAIN(ISEC)%NPAIR(1),2)/)
          CHAIN(ISEC)%XYEND(:)= (/F(CHAIN(ISEC)%NPAIR(2),1),
     &                            F(CHAIN(ISEC)%NPAIR(2),2)/)
        END DO 
      ELSE
        DO ISEC=1,NSEC
          READ (NINP,*) CHAIN(ISEC)%DESCR
          READ (NINP,*) CHAIN(ISEC)%XYBEG(:), CHAIN(ISEC)%XYEND(:)
          CHAIN(ISEC)%NPAIR(:)=0
        END DO 
      ENDIF
!      CLOSE(NINP) 
 
      ! IF TERMINAL POINTS GIVEN BY COORDINATES, FIND NEAREST NODES FIRST

      WRITE(LU,*) 'NPOIN:',NPOIN
      IF (IHOWSEC>=0) THEN 
        DO ISEC=1,NSEC
          XA=F(1,1) 
          YA=F(1,2)
          DMINB = (CHAIN(ISEC)%XYBEG(1)-XA)**2 
     &          + (CHAIN(ISEC)%XYBEG(2)-YA)**2 
          DMINE = (CHAIN(ISEC)%XYEND(1)-XA)**2 
     &          + (CHAIN(ISEC)%XYEND(2)-YA)**2 
          CHAIN(ISEC)%NPAIR(1)=1
          CHAIN(ISEC)%NPAIR(2)=1
          DO I=2,NPOIN ! COMPUTATIONALLY INTENSIVE 
            XA=F(I,1)
            YA=F(I,2)
            DISTB = (CHAIN(ISEC)%XYBEG(1)-XA)**2 
     &            + (CHAIN(ISEC)%XYBEG(2)-YA)**2 
            DISTE = (CHAIN(ISEC)%XYEND(1)-XA)**2 
     &            + (CHAIN(ISEC)%XYEND(2)-YA)**2 
            IF ( DISTB < DMINB ) THEN 
              CHAIN(ISEC)%NPAIR(1)=I
              DMINB=DISTB
            ENDIF
            IF ( DISTE < DMINE ) THEN 
              CHAIN(ISEC)%NPAIR(2)=I
              DMINE=DISTE 
            ENDIF 
          END DO
          WRITE(LU,'(A,3(1X,I9))') 
     &          ' -> SECTION, TERMINAL NODES: ', 
     &          ISEC, CHAIN(ISEC)%NPAIR(:)
        END DO  
      ELSE
        DO ISEC=1,NSEC
          WRITE(LU,'(A,1X,I9,4(1X,1PG13.6))') 
     &          ' -> SECTION, TERMINAL COORDINATES: ', ISEC, 
     &          CHAIN(ISEC)%XYBEG, CHAIN(ISEC)%XYEND
        END DO 
      ENDIF 

      WRITE(LU,*) 'NSEC,IHOWSEC: ',NSEC,IHOWSEC
      WRITE(LU,*) 'ANTICIPATED SECTIONS SUMMARY:'
      DO ISEC=1,NSEC
        WRITE(LU,*) CHAIN(ISEC)%DESCR
        WRITE(LU,*) CHAIN(ISEC)%XYBEG(:), CHAIN(ISEC)%XYEND(:)
        WRITE(LU,*) CHAIN(ISEC)%NPAIR(:)
      END DO  

! NOW FOLLOW THE FLUSEC SUBROUTINE IN BIEF TO FIND SECTIONS 
! IN THE GLOBAL MESH -> FILL THE FIELD LISTE

      NCP = 2*NSEC
      ALLOCATE(LISTE(NSEMAX,2),STAT=ERR) ! WORKHORSE 
      IF (ERR.NE.0) CALL ALLOER (LU, 'LISTE')

      DO ISEC =1,NSEC

        DEP = CHAIN(ISEC)%NPAIR(1) 
        ARR = CHAIN(ISEC)%NPAIR(2)

        PT = DEP
        ISEG = 0
        DIST=(F(DEP,1)-F(ARR,1))**2+(F(DEP,2)-F(ARR,2))**2

 1010   CONTINUE ! A JUMP POINT 

        DO IELEM =1,NELEM
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          IF (PT.EQ.I1.OR.PT.EQ.I2.OR.PT.EQ.I3) THEN
            DIST1 = (F(I1,1)-F(ARR,1))**2 + (F(I1,2)-F(ARR,2))**2
            DIST2 = (F(I2,1)-F(ARR,1))**2 + (F(I2,2)-F(ARR,2))**2
            DIST3 = (F(I3,1)-F(ARR,1))**2 + (F(I3,2)-F(ARR,2))**2
            IF (DIST1.LT.DIST) THEN
              DIST = DIST1
              ELBEST = IELEM
              IGBEST = I1
              ILBEST = 1
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
            IF (DIST2.LT.DIST) THEN 
              DIST = DIST2
              ELBEST = IELEM
              IGBEST = I2
              ILBEST = 2
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
            IF(DIST3.LT.DIST) THEN
              DIST = DIST3
              ELBEST = IELEM
              IGBEST = I3
              ILBEST = 3
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
          ENDIF

        END DO ! OVER ELEMENTS 

        IF (IGBEST.EQ.PT) THEN
          WRITE(LU,*)'FLUSEC : ALGORITHM FAILED'
          CALL PLANTE2(-1)
          STOP
        ELSE
          PT = IGBEST
          ISEG = ISEG + 1
          IF (ISEG.GT.NSEMAX) THEN
            WRITE(LU,*) 'TOO MANY SEGMENTS IN A   '
            WRITE(LU,*) 'SECTION. INCREASE  NSEMAX'
            CALL PLANTE2(-1)
            STOP
          ENDIF
          LISTE(ISEG,1) = IKLE(ELBEST,ILPREC)
          LISTE(ISEG,2) = IKLE(ELBEST,ILBEST)
          IF (IGBEST.NE.ARR) GOTO 1010
        ENDIF
        CHAIN(ISEC)%NSEG = ISEG
        ALLOCATE (CHAIN(ISEC)%LISTE(CHAIN(ISEC)%NSEG,3), STAT=ERR)
        IF (ERR/=0) CALL ALLOER (LU, 'CHAIN(ISEC)%LISTE') 
        DO ISEG=1,CHAIN(ISEC)%NSEG
          CHAIN(ISEC)%LISTE(ISEG,1)=LISTE(ISEG,1) 
          CHAIN(ISEC)%LISTE(ISEG,2)=LISTE(ISEG,2) 
          CHAIN(ISEC)%LISTE(ISEG,3)=-1 ! INITIALISE... FOR DEVEL 
        END DO 
      END DO ! OVER SECTIONS 
      DEALLOCATE (LISTE) 

! NOW ONE CAN INDICATE THE PARTITIONS THE SECTIONS GO THROUGH
! PROCEED SEGMENT-WISE, USINF 2D KNOLG / KNOGL

!      DO I=1,NPOIN
!        WRITE(LU,*) I,KNOGL(I,:) 
!      END DO 

      ALLOCATE (ANPBEG(NBMAXNSHARE), STAT=ERR)
      IF (ERR/=0) CALL ALLOER (LU, 'ANPBEG') 
      ALLOCATE (ANPEND(NBMAXNSHARE), STAT=ERR) 
      IF (ERR/=0) CALL ALLOER (LU, 'ANPEND') 

      DO ISEC=1,NSEC 
        DO ISEG=1,CHAIN(ISEC)%NSEG 

          NPBEG=COUNT( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),:)>0 )
          NPEND=COUNT( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),:)>0 )          

          IF (NPBEG>NBMAXNSHARE .OR. NPEND>NBMAXNSHARE) THEN 
            WRITE(LU,*) 'NPBEG OR NPEND: ',NPBEG,NPEND
            WRITE(LU,*) 'ARE LARGER THAN NBMAXNSHARE: ',NBMAXNSHARE
            CALL PLANTE2(-1) 
            STOP
          ENDIF 

          ! THE NICE AND USUAL CASE WHEN BOTH SEGMENT ENDS 
          ! BELONG TO ONE SUBDOMAIN - ONLY ONE POSITION IN KNOGL 
          IF ( NPBEG==1 .AND. NPEND==1) THEN  
             IM(:) = MAXLOC ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),:) ) 
             IN(:) = MAXLOC ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),:) )
             IF (IM(1)==IN(1)) THEN  
               CHAIN(ISEC)%LISTE(ISEG,3)=IM(1) 
             ELSE ! THEY BELONG TO DIFFERENT SUBDOMAINS? HOW COME?
               WRITE(LU,*) 'IMPOSSIBLE CASE (1) BY SECTIONS???'
               CALL PLANTE2(-1)
               STOP
             ENDIF 
          ! AT LEAST ONE OF THE TERMINAL NODES IS ON THE INTERFACE
          ! TAKE THE LARGEST COMMON PARTITION NUMBER THEY BOTH BELONG TO
          ELSE 
            IF (NPBEG==1 .AND. NPEND>1) THEN ! THE SEGMENT'S END TOUCHES THE INTERFACE 
              IM(:) = MAXLOC ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),:) )
              IF ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),IM(1))>0 ) THEN  
                CHAIN(ISEC)%LISTE(ISEG,3) = IM(1) 
              ELSE 
                WRITE(LU,*) 'IMPOSSIBLE CASE (2) BY SECTIONS???'
                CALL PLANTE2(-1)
                STOP
              ENDIF 
            ELSE IF (NPBEG>1 .AND. NPEND==1) THEN ! THE SEGMENT'S BEG. TOUCHES THE INTERFACE
              IN(:) = MAXLOC ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),:) )
              IF ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),IN(1))>0 ) THEN  
                CHAIN(ISEC)%LISTE(ISEG,3) = IN(1) 
              ELSE 
                WRITE(LU,*) 'IMPOSSIBLE CASE (3) BY SECTIONS???'
                CALL PLANTE2(-1)
                STOP
              ENDIF 
            ELSE ! I.E. (NPBEG>1 .AND. NPEND>1) - LIES JUST ON THE INTERFACE OR "A SHORTCUT" 
              ANPBEG=0
              ANPEND=0 
              I=0 
              DO N=1,NPARTS
                IF ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),N)>0 ) THEN
                  I=I+1
                  ANPBEG(I)=N 
                ENDIF  
              END DO 
              IF (I/=NPBEG) WRITE(LU,*) 'OH! I/=NPBEG'
              I=0 
              DO N=1,NPARTS
                IF ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),N)>0 ) THEN
                  I=I+1
                  ANPEND(I)=N 
                ENDIF  
              END DO 
              IF (I/=NPEND) WRITE(LU,*) 'OH! I/=NPEND'

              WRITE(LU,*) 'ANPBEG: ',ANPBEG
              WRITE(LU,*) 'ANPEND: ',ANPEND

              FOUND=.FALSE.
              DO I=NPBEG,1,-1
                DO J=NPEND,1,-1
                  IF (ANPBEG(I)==ANPEND(J)) THEN 
                     CHAIN(ISEC)%LISTE(ISEG,3) = ANPBEG(I)
                    FOUND=.TRUE.
                    EXIT
                  ENDIF 
                END DO 
                IF (FOUND) EXIT 
              END DO 
              IF (.NOT.FOUND) THEN 
                WRITE(LU,*) 'BY SECTION WITH NODES: ',
     &            CHAIN(ISEC)%LISTE(ISEG,1),CHAIN(ISEC)%LISTE(ISEG,2)
                WRITE(LU,*) 'IMPOSSIBLE CASE (4) BY SECTIONS???'
                CALL PLANTE2(-1)
                STOP
              ENDIF 

            ENDIF
          ENDIF 

        END DO 
      END DO 

      DEALLOCATE (ANPBEG,ANPEND) 

! DEVEL PRINTOUT 

!      WRITE(LU,*) 'SUMMARY OF SECTION CHAINS PARTITIONING'
!      DO ISEC=1,NSEC
!        WRITE(LU,*) 'ISEC, NSEG: ',ISEC,CHAIN(ISEC)%NSEG
!        WRITE(LU,*) 'DESCR: ',TRIM(CHAIN(ISEC)%DESCR) 
!        DO ISEG=1,CHAIN(ISEC)%NSEG
!          WRITE(LU,*) CHAIN(ISEC)%LISTE(ISEG,1), 
!     &                CHAIN(ISEC)%LISTE(ISEG,2),
!     &                CHAIN(ISEC)%LISTE(ISEG,3)
!        END DO 
!      END DO 

! WRITE FILES 

      DO N=1,NPARTS
        NAMEOUT=TRIM(NAMESEC)//EXTENS(NPARTS-1,N-1)

        WRITE(LU,*) 'WRITING: ', TRIM(NAMEOUT) 

        OPEN (NOUT,FILE=TRIM(NAMEOUT),FORM='FORMATTED',STATUS='UNKNOWN')
        REWIND(NOUT) 
        WRITE(NOUT,*) '# SECTIONS PARTITIONED FOR ',EXTENS(NPARTS-1,N-1)
        WRITE(NOUT,*) NSEC, 1
        DO ISEC=1,NSEC
          WRITE(NOUT,*) TRIM(CHAIN(ISEC)%DESCR)
          I=COUNT(CHAIN(ISEC)%LISTE(:,3)==N) 
          WRITE(NOUT,*) I
          DO ISEG=1,CHAIN(ISEC)%NSEG
            IF (CHAIN(ISEC)%LISTE(ISEG,3)==N) THEN 
              WRITE(NOUT,*) 
     &          KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),N),
     &          KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),N)
            ENDIF
          END DO 
        END DO
        CLOSE(NOUT) 
      END DO 

      WRITE(LU,*) 'FINISHED DEALING WITH SECTIONS'
      ENDIF ! NPLAN==0
!
!----------------------------------------------------------------------
!
!     NOTE BY J-M HERVOUET : DEALLOCATE CAUSES ERRORS ON HP
!     (POSSIBLE REMAINING BUG ?)
!     NOTE BY JAJ: DEALLOCATE(HP) ,^)
!
       DEALLOCATE (IKLE) ! #### MOVED FROM FAR ABOVE 
       DEALLOCATE(NPART)
       DEALLOCATE(EPART)
       DEALLOCATE(NPOIN_P)
       DEALLOCATE(NELEM_P)
       DEALLOCATE(NPTFR_P)
       DEALLOCATE(NPTIR_P)
!
       DEALLOCATE(IKLES)
      IF(NPLAN.GT.1) THEN
         DEALLOCATE(IKLES3D)
         DEALLOCATE(IKLES3D_P)
      ENDIF
      DEALLOCATE(IKLES_P)
      DEALLOCATE(IRAND)
!      DEALLOCATE(F)
!      DEALLOCATE(F_P)

      DEALLOCATE(KNOLG)
      DEALLOCATE(KNOGL)
      DEALLOCATE(ELELG)
      DEALLOCATE(KP1BOR)

!
!----------------------------------------------------------------------      
!
 299  IF (TIMECOUNT) THEN 
        CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
        TFIN = TEMPS
        WRITE(LU,*) 'OVERALL TIMING: ',
     &    (1.0*(TFIN-TDEB))/(1.0*PARSEC),' SECONDS'
        WRITE(LU,*) ' '
      ENDIF
      WRITE(LU,*) '+---- PARTEL: NORMAL TERMINATION ----+'
      WRITE(LU,*) ' '
!
      GO TO 999

 300  WRITE(LU,*) 'ERROR BY READING. '
      CALL PLANTE2(-1)

 999  STOP  
      END PROGRAM PARTEL


      SUBROUTINE ALLOER (N, CHFILE)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      CHARACTER*(*), INTENT(IN) :: CHFILE
      WRITE(N,*) 'ERROR BY ALLOCATION OF ',CHFILE
      CALL PLANTE2(-1)
      STOP
      END SUBROUTINE ALLOER
      
      SUBROUTINE ALLOER2(N,CHFILE)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      CHARACTER*(*), INTENT(IN) :: CHFILE
      WRITE(N,*) TRIM(CHFILE)
      CALL PLANTE2(-1)
      STOP
      END SUBROUTINE ALLOER2


      SUBROUTINE PLANTE2 (IVAL)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: IVAL
      INTEGER ICODE
!     STANDARD F90 :  STOP [n] WHERE N IS A STRING OF NOT MORE
!     THAN FIVE DIGITS OR IS A CHARACTER CONSTANT.
!     HOWEVER, CODE IS NOT ALWAYS SENT TO STDERR
!     (COMPILER DEPENDENT, NAG DOESN'T FOR INSTANCE)
!     ICODE MIGHT BE USED IN A POSSIBLE SYSTEM DEPENDENT EXIT PROCEDURE
!     EXAMPLE : STOP 1 ; STOP '    1'
      IF(IVAL.LT.0) THEN
        ICODE = 0      ! JUST ASSUMED FOR NON-ERROR STOP
      ELSEIF(IVAL.EQ.0.OR.IVAL.EQ.1) THEN
        ICODE = 2      ! EXIT IVAL 0 OR 1 INDICATING A "CONTROLLED" ERROR
        STOP 2
      ELSE
        ICODE = 1     ! SOMETHING ELSE? BUT AN ERROR!
        STOP 1
      ENDIF
      WRITE(LU,*) 'RETURNING EXIT CODE: ', ICODE
      STOP 0   !WHICH IS USUALLY EQUIVALENT TO CALL EXIT(0)

!     JMH 30/09/2011 WHAT IS THIS (NAG COMPILER DOES NOT KNOW)
!     CALL EXIT(ICODE)
      END SUBROUTINE PLANTE2
C                       *********************************
                        CHARACTER(LEN=11) FUNCTION EXTENS
C                       *********************************
C
     *(N,IPID)
C
C***********************************************************************
C  PARA       VERSION 4.0         08/01/97        J-M HERVOUET (LNH)
C
C***********************************************************************
C
C      FONCTIONS: EXTENSION DES FICHIERS SUR CHAQUE PROCESSEUR.
C      ==========
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________|
C |     N          | -->| NOMBRE DE PROCESSEURS MOINS UN = NCSIZE-1
C |     IPID       | -->| NUMERO DU PROCESSEUR
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR :
C
C SOUS-PROGRAMMES APPELES : NEANT
C
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER IPID,N
C
C-----------------------------------------------------------------------
C
      IF(N.GT.0) THEN
C
        EXTENS='00000-00000'
C
        IF(N.LT.10) THEN
          WRITE(EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(EXTENS(01:05),'(I5)') N
        ENDIF
C
        IF(IPID.LT.10) THEN
          WRITE(EXTENS(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(EXTENS(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(EXTENS(09:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(EXTENS(08:11),'(I4)') IPID
        ELSE
          WRITE(EXTENS(07:11),'(I5)') IPID
        ENDIF
C
      ELSE
C
        EXTENS='       '
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       ************************
                        SUBROUTINE FRONT2_PARTEL
C                       ************************
C
     *(NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,LIHBOR,LIUBOR,
     * X,Y,NBOR,KP1BOR,DEJAVU,NPOIN,NPTFR,KLOG,LISTIN,NUMLIQ,NUMSOL,
     * NPTFRMAX)
C
C***********************************************************************
C BIEF VERSION 5.5           04/05/04    J-M HERVOUET  01 30 87 80 18
C***********************************************************************
C
C  FONCTION  : REPERAGE, NUMEROTATION DES FRONTIERES LIQUIDES ET SOLIDES
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   NFRLIQ       |<-- | NOMBRE DE FRONTIERES LIQUIDES
C |   NFRSOL       |<-- | NOMBRE DE FRONTIERES SOLIDES
C |   DEBLIQ       |<-- | DEBUTS DES FRONTIERES LIQUIDES
C |   FINLIQ       |<-- | FINS DES FRONTIERES LIQUIDES
C |   DEBSOL       |<-- | DEBUTS DES FRONTIERES SOLIDES
C |   FINSOL       |<-- | FINS DES FRONTIERES SOLIDES
C |   LIHBOR       | -->| CONDITIONS AUX LIMITES SUR H
C |   X , Y        | -->| COORDONNEES DU MAILLAGE.
C |   NBOR         | -->| NUMEROS GLOBAUX DES POINTS DE BORD
C |   KP1BOR       | -->| NUMEROS DES EXTREMITES DES SEGMENTS DE BORD
C |                |    | DANS LA NUMEROTATION DES POINTS DE BORD
C |   DEJAVU       | -- | TABLEAU DE TRAVAIL
C |   NPOIN        | -->| NOMBRE DE POINTS DU MAILLAGE
C |   NPTFR        | -->| NOMBRE DE POINTS FRONTIERE
C |   KLOG         | -->| LIHBOR(K)=KLOG : FRONTIERE SOLIDE
C |   LISTIN       | -->| IMPRESSIONS SUR LISTING (OU NON)
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C  PRECAUTIONS D'EMPLOI : LES FRONTIERES SOLIDES SONT REPEREES PAR LE
C                         FAIT QUE LIHBOR(K) = KLOG POUR UN POINT DE
C                         BORD DE NUMERO K.
C                         UN SEGMENT COMPRIS ENTRE UN POINT LIQUIDE ET
C                         UN POINT SOLIDE EST SOLIDE.
C
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,KLOG,NPTFRMAX
      INTEGER, INTENT(OUT) :: NFRLIQ,NFRSOL
C                                    *=MAXFRO (300 DANS TELEMAC-2D)
      INTEGER, INTENT(OUT) :: DEBLIQ(*),FINLIQ(*),DEBSOL(*),FINSOL(*)
CCCCCCMOULINEC BEGIN
      INTEGER , INTENT(IN) :: LIHBOR(NPTFRMAX),LIUBOR(NPTFRMAX)
C      INTEGER , INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR)
CCCCCCMOULINEC END
      REAL, INTENT(IN) :: X(NPOIN) , Y(NPOIN)
CCCCCCMOULINEC BEGIN
      INTEGER, INTENT(IN) :: NBOR(2*NPTFRMAX),KP1BOR(NPTFR)
C      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR)
CCCCCCMOULINEC END
      INTEGER, INTENT(OUT) :: DEJAVU(NPTFR)
      LOGICAL, INTENT(IN) :: LISTIN
CCCCCCMOULINEC BEGIN
      INTEGER, INTENT(OUT) :: NUMLIQ(NPTFRMAX)
      INTEGER, INTENT(OUT) :: NUMSOL(NPTFRMAX)
C      INTEGER, INTENT(OUT) :: NUMLIQ(NPTFR)
C      INTEGER, INTENT(OUT) :: NUMSOL(NPTFR)
CCCCCCMOULINEC END
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      INTEGER K,KPREV,IDEP,SOL1,LIQ1,L1,L2,L3,NILE
C
      LOGICAL SOLF,LIQF,SOLD,LIQD
C
      REAL MINNS,MAXNS,EPS,YMIN,NS
C
      INTRINSIC ABS
C
C-----------------------------------------------------------------------
C
C  INITIALISATIONS
C
C  DEJAVU : MARQUE D'UN 1 LES POINTS DEJA TRAITES
C  NILE   : NOMBRE D'ILES
C
      DO 10 K=1,NPTFR
        DEJAVU(K) = 0
        NUMLIQ(K) = 0
        NUMSOL(K) = 0
10    CONTINUE
C
      NILE = 0
      IDEP = 1
      NFRLIQ = 0
      NFRSOL = 0
C
C-----------------------------------------------------------------------
C
C  ON REVIENDRA A L'ETIQUETTE 20 S'IL Y A AU MOINS UNE ILE
C
20    CONTINUE
C
C  RECHERCHE DU POINT LE PLUS SUD-OUEST (IL PEUT Y EN AVOIR PLUSIEURS)
C
      MINNS = X(NBOR(IDEP)) + Y(NBOR(IDEP))
      MAXNS = MINNS
      YMIN  = Y(NBOR(IDEP))
C
      DO 30 K = 1 , NPTFR
      IF(DEJAVU(K).EQ.0) THEN
        NS = X(NBOR(K)) + Y(NBOR(K))
        IF(NS.LT.MINNS) THEN
         IDEP = K
         MINNS = NS
         YMIN = Y(NBOR(K))
        ENDIF
        IF(NS.GT.MAXNS) MAXNS = NS
      ENDIF
30    CONTINUE
C
      EPS = (MAXNS-MINNS) * 1.D-4
C
C  CHOIX DU POINT LE PLUS SUD PARMI LES CANDIDATS SUD-OUEST
C
      DO 40 K = 1 , NPTFR
      IF(DEJAVU(K).EQ.0) THEN
        NS = X(NBOR(K)) + Y(NBOR(K))
        IF(ABS(MINNS-NS).LT.EPS) THEN
          IF(Y(NBOR(K)).LT.YMIN) THEN
           IDEP = K
           YMIN = Y(NBOR(K))
          ENDIF
        ENDIF
      ENDIF
40    CONTINUE
C
C-----------------------------------------------------------------------
C
C  NUMEROTATION ET REPERAGE DES FRONTIERES DU CONTOUR COMMENCANT
C  AU POINT IDEP.
C
C  SOLD = .TRUE. : LA FRONTIERE AU DEPART DE IDEP EST SOLIDE
C  LIQD = .TRUE. : LA FRONTIERE AU DEPART DE IDEP EST LIQUIDE
C  SOLF = .TRUE. : LA FRONTIERE AU RETOUR A IDEP EST SOLIDE
C  LIQF = .TRUE. : LA FRONTIERE AU RETOUR A IDEP EST LIQUIDE
C  LIQ1 : NUMERO DE LA PREMIERE FRONTIERE LIQUIDE DU CONTOUR
C  SOL1 : NUMERO DE LA PREMIERE FRONTIERE SOLIDE DU CONTOUR
C
      K = IDEP
C
      SOL1 = 0
      LIQ1 = 0
      LIQF = .FALSE.
      SOLF = .FALSE.
C
C NATURE DU PREMIER SEGMENT
C
C     LOI DE DOMINANCE DU SOLIDE SUR LE LIQUIDE
      IF(LIHBOR(K).EQ.KLOG.OR.LIHBOR(KP1BOR(K)).EQ.KLOG) THEN
C       LE PREMIER SEGMENT EST SOLIDE
        NFRSOL = NFRSOL + 1
        SOL1 = NFRSOL
        SOLD = .TRUE.
        LIQD = .FALSE.
      ELSE
C       LE PREMIER SEGMENT EST LIQUIDE
        NFRLIQ = NFRLIQ + 1
        LIQ1 = NFRLIQ
        LIQD = .TRUE.
        SOLD = .FALSE.
      ENDIF
C
      DEJAVU(K) = 1
      KPREV = K
      K = KP1BOR(K)
C
50    CONTINUE
C
C RECHERCHE DES POINTS DE TRANSITION A PARTIR DU POINT SUIVANT IDEB
C
C ON CHERCHE AUSSI LES CAS DE POINTS ISOLES POUR DETECTER LES ERREURS
C DANS LES DONNEES.
C
      L1 = LIHBOR(KPREV)
      L2 = LIHBOR(K)
      L3 = LIHBOR(KP1BOR(K))
C
      IF(L1.EQ.KLOG.AND.L2.NE.KLOG.AND.L3.NE.KLOG) THEN
C     TRANSITION SOLIDE-LIQUIDE AU POINT K
        NFRLIQ = NFRLIQ + 1
        FINSOL(NFRSOL) = K
        DEBLIQ(NFRLIQ) = K
        LIQF = .TRUE.
        SOLF = .FALSE.
      ELSEIF(L1.NE.KLOG.AND.L2.NE.KLOG.AND.L3.EQ.KLOG) THEN
C     TRANSITION LIQUIDE-SOLIDE AU POINT K
        NFRSOL = NFRSOL + 1
        FINLIQ(NFRLIQ) = K
        DEBSOL(NFRSOL) = K
        LIQF = .FALSE.
        SOLF = .TRUE.
      ELSEIF(L1.NE.KLOG.AND.L2.NE.KLOG.AND.L3.NE.KLOG) THEN
C     RECHERCHE DES TRANSITIONS LIQUIDE-LIQUIDE AU POINT K
        IF(L2.NE.L3.OR.LIUBOR(K).NE.LIUBOR(KP1BOR(K))) THEN
          FINLIQ(NFRLIQ) = K
          NFRLIQ = NFRLIQ + 1
          DEBLIQ(NFRLIQ) = KP1BOR(K)
        ENDIF
      ELSEIF(L1.EQ.KLOG.AND.L2.NE.KLOG.AND.L3.EQ.KLOG) THEN
C     ERREUR DANS LES DONNEES
        IF(LNG.EQ.1) WRITE(LU,102) K
        IF(LNG.EQ.2) WRITE(LU,103) K
        CALL PLANTE2(-1)
        STOP
      ELSEIF(L1.NE.KLOG.AND.L2.EQ.KLOG.AND.L3.NE.KLOG) THEN
C     ERREUR DANS LES DONNEES
        IF(LNG.EQ.1) WRITE(LU,104) K
        IF(LNG.EQ.2) WRITE(LU,105) K
        CALL PLANTE2(-1)
        STOP
      ENDIF
C
      DEJAVU(K) = 1
      KPREV = K
      K = KP1BOR(K)
      IF(K.NE.IDEP) GO TO 50
C
C  CAS D'UN CHANGEMENT DE FRONTIERE AU POINT DE DEPART IDEP
C
      IF(SOLF) THEN
C       LA DERNIERE FRONTIERE ETAIT SOLIDE
        IF(SOLD) THEN
C         LA PREMIERE FRONTIERE ETAIT SOLIDE
          DEBSOL(SOL1) = DEBSOL(NFRSOL)
          NFRSOL = NFRSOL - 1
        ELSEIF(LIQD) THEN
C         LA PREMIERE FRONTIERE ETAIT LIQUIDE
          DEBLIQ(LIQ1) = IDEP
          FINSOL(NFRSOL) = IDEP
        ENDIF
C
      ELSEIF(LIQF) THEN
C       LA DERNIERE FRONTIERE DU CONTOUR ETAIT LIQUIDE
        IF(LIQD) THEN
C         LA PREMIERE FRONTIERE DU CONTOUR ETAIT LIQUIDE
          DEBLIQ(LIQ1) = DEBLIQ(NFRLIQ)
          NFRLIQ = NFRLIQ - 1
        ELSEIF(SOLD) THEN
C         LA PREMIERE FRONTIERE DU CONTOUR ETAIT SOLIDE
          DEBSOL(SOL1) = IDEP
          FINLIQ(NFRLIQ) = IDEP
        ENDIF
C
      ELSE
C     CAS OU TOUT LE CONTOUR EST DU MEME TYPE
        IF(SOL1.NE.0) THEN
          DEBSOL(SOL1) = IDEP
          FINSOL(SOL1) = IDEP
        ELSEIF(LIQ1.NE.0) THEN
          DEBLIQ(LIQ1) = IDEP
          FINLIQ(LIQ1) = IDEP
        ELSE
          IF(LISTIN.AND.LNG.EQ.1) THEN
           WRITE(LU,'(1X,A)') 'CAS IMPOSSIBLE DANS FRONT2'
          ENDIF
          IF(LISTIN.AND.LNG.EQ.2) THEN
           WRITE(LU,'(1X,A)') 'IMPOSSIBLE CASE IN FRONT2'
          ENDIF
          CALL PLANTE2(-1)
          STOP
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C  ON REGARDE S'IL RESTE DES CONTOURS :
C
      DO 60 K = 1 , NPTFR
        IF(DEJAVU(K).EQ.0) THEN
          IDEP = K
          NILE = NILE + 1
          GO TO 20
        ENDIF
60    CONTINUE
C
C-----------------------------------------------------------------------
C
      DO 79 K=1,NPTFR
        NUMLIQ(K)=0
79    CONTINUE
C
C  IMPRESSION DES RESULTATS ET CALCUL DE NUMLIQ
C
      IF(NILE.NE.0.AND.LISTIN.AND.LNG.EQ.1) WRITE(LU,69) NILE
      IF(NILE.NE.0.AND.LISTIN.AND.LNG.EQ.2) WRITE(LU,169) NILE
C
      IF(NFRLIQ.NE.0) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,70) NFRLIQ
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,170) NFRLIQ

        DO 80 K = 1, NFRLIQ
C
C  MARQUAGE DES NUMEROS DES FRONTIERES LIQUIDES
C
          L1=DEBLIQ(K)
          NUMLIQ(L1)=K
707       L1=KP1BOR(L1)
          NUMLIQ(L1)=K
          IF(L1.NE.FINLIQ(K)) GO TO 707
C
C  FIN DU MARQUAGE
C
          IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,90)
     *                            K,DEBLIQ(K),NBOR(DEBLIQ(K)),
     *                            X(NBOR(DEBLIQ(K))),Y(NBOR(DEBLIQ(K))),
     *                            FINLIQ(K),NBOR(FINLIQ(K)),
     *                            X(NBOR(FINLIQ(K))),Y(NBOR(FINLIQ(K)))
          IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,190)
     *                            K,DEBLIQ(K),NBOR(DEBLIQ(K)),
     *                            X(NBOR(DEBLIQ(K))),Y(NBOR(DEBLIQ(K))),
     *                            FINLIQ(K),NBOR(FINLIQ(K)),
     *                            X(NBOR(FINLIQ(K))),Y(NBOR(FINLIQ(K)))
80      CONTINUE
      ENDIF
C
      IF(NFRSOL.NE.0) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,100) NFRSOL
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,101) NFRSOL

        DO 110 K = 1, NFRSOL
!
!  MARKING SOLID BOUNDARIES (WHY NOT?)
!  THEY GET NEXT BOUNDARY NUMBERS 
!
          L1=DEBSOL(K)
          NUMSOL(L1)=K+NFRLIQ
708       L1=KP1BOR(L1)
          NUMSOL(L1)=K+NFRLIQ
          IF(L1.NE.FINSOL(K)) GO TO 708
!
!  END OD FMARKING
!
          IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,90)
     *                            K,DEBSOL(K),NBOR(DEBSOL(K)),
     *                            X(NBOR(DEBSOL(K))),Y(NBOR(DEBSOL(K))),
     *                            FINSOL(K),NBOR(FINSOL(K)),
     *                            X(NBOR(FINSOL(K))),Y(NBOR(FINSOL(K)))
          IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,190)
     *                            K,DEBSOL(K),NBOR(DEBSOL(K)),
     *                            X(NBOR(DEBSOL(K))),Y(NBOR(DEBSOL(K))),
     *                            FINSOL(K),NBOR(FINSOL(K)),
     *                            X(NBOR(FINSOL(K))),Y(NBOR(FINSOL(K)))
110     CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C
C  FORMATS
C
69    FORMAT(/,1X,'IL Y A ',1I3,' ILE(S) DANS LE DOMAINE')
169   FORMAT(/,1X,'THERE IS ',1I3,' ISLAND(S) IN THE DOMAIN')
70    FORMAT(/,1X,'IL Y A ',1I3,' FRONTIERE(S) LIQUIDE(S) :')
170   FORMAT(/,1X,'THERE IS ',1I3,' LIQUID BOUNDARIES:')
100   FORMAT(/,1X,'IL Y A ',1I3,' FRONTIERE(S) SOLIDE(S) :')
101   FORMAT(/,1X,'THERE IS ',1I3,' SOLID BOUNDARIES:')
102   FORMAT(/,1X,'FRONT2 : ERREUR AU POINT DE BORD ',1I5,
     *       /,1X,'         POINT LIQUIDE ENTRE DEUX POINTS SOLIDES')
103   FORMAT(/,1X,'FRONT2 : ERROR AT BOUNDARY POINT ',1I5,
     *       /,1X,'         LIQUID POINT BETWEEN TWO SOLID POINTS')
104   FORMAT(/,1X,'FRONT2 : ERREUR AU POINT DE BORD ',1I5,
     *       /,1X,'         POINT SOLIDE ENTRE DEUX POINTS LIQUIDES')
105   FORMAT(/,1X,'FRONT2 : ERROR AT BOUNDARY POINT ',1I5,
     *       /,1X,'         SOLID POINT BETWEEN TWO LIQUID POINTS')
90    FORMAT(/,1X,'FRONTIERE ',1I3,' : ',/,1X,
     *            ' DEBUT AU POINT DE BORD ',1I4,
     *            ' , DE NUMERO GLOBAL ',1I6,/,1X,
     *            ' ET DE COORDONNEES : ',G16.7,3X,G16.7,
     *       /,1X,' FIN AU POINT DE BORD ',1I4,
     *            ' , DE NUMERO GLOBAL ',1I6,/,1X,
     *            ' ET DE COORDONNEES : ',G16.7,3X,G16.7)
190   FORMAT(/,1X,'BOUNDARY ',1I3,' : ',/,1X,
     *            ' BEGINS AT BOUNDARY POINT: ',1I4,
     *            ' , WITH GLOBAL NUMBER: ',1I6,/,1X,
     *            ' AND COORDINATES: ',G16.7,3X,G16.7,
     *       /,1X,' ENDS AT BOUNDARY POINT: ',1I4,
     *            ' , WITH GLOBAL NUMBER: ',1I6,/,1X,
     *            ' AND COORDINATES: ',G16.7,3X,G16.7)
C
C-----------------------------------------------------------------------
C
      IF(NILE.GT.300.OR.NFRSOL.GT.300.OR.NFRLIQ.GT.300) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FRONT2 : DEPASSEMENT DE TABLEAUX'
          WRITE(LU,*) '         AUGMENTER MAXFRO DANS LE CODE APPELANT' 
          WRITE(LU,*) '         A LA VALEUR ',MAX(NILE,NFRSOL,NFRLIQ)             
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'FRONT2: SIZE OF ARRAYS EXCEEDED'
          WRITE(LU,*) '        INCREASE MAXFRO IN THE CALLING PROGRAM' 
          WRITE(LU,*) '        UP TO THE VALUE ',MAX(NILE,NFRSOL,NFRLIQ)             
        ENDIF
        CALL PLANTE2(-1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END 
C                       ************************
                        SUBROUTINE VOISIN_PARTEL
C                       ************************
C
     *(IFABOR,NELEM,NELMAX,IELM,IKLE,SIZIKL,NPOIN,IADR,NVOIS)
C
C***********************************************************************
C BIEF VERSION 5.9         16/06/2008    J-M HERVOUET (LNHE) 30 87 80 18
C
C***********************************************************************
C
C    FONCTION : CONSTRUCTION DU TABLEAU IFABOR, OU IFABOR(IELEM,IFACE)
C               EST LE NUMERO GLOBAL DU VOISIN DE LA FACE IFACE DE
C               L'ELEMENT IELEM SI CE VOISIN EXISTE ET 0 SI LA FACE EST
C               SUR LA FRONTIERE DU DOMAINE.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    IFABOR      |<-- | TABLEAU DES VOISINS DES FACES.
C |    NELEM       | -->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C |    NELMAX      | -->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
C |                |    | (CAS DES MAILLAGES ADAPTATIFS)
C |    IELM        | -->| 11: TRIANGLES
C |                |    | 21: QUADRILATERES
C |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT
C |    NPOIN       | -->| NOMBRE TOTAL DE POINTS DU DOMAINE
C |________________|____|_______________________________________________
C  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT DANS TELEMAC 2D : PREDAT
C
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER IR1,IR2,IR3,IR4,SIZIKL
C
      INTEGER NELEM,NELMAX,IELM,IDIMAT,NPOIN,I,J,ERR,NDP
      INTEGER NFACE,KEL,I1,I2,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER IFABOR(NELMAX,*),IKLE(SIZIKL,*),NVOIS(NPOIN),IADR(NPOIN)
C
      INTEGER SOMFAC(2,4,2)
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
C
C  TABLEAUX DE TRAVAIL ALLOUES DYNAMIQUEMENT
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: MAT1,MAT2,MAT3
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.21) THEN
C       QUADRILATERES
        NFACE = 4
        NDP = 4
        KEL = 2
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.41) THEN
C       TRIANGLES
        NFACE = 3
        NDP = 3
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,98) IELM
        IF(LNG.EQ.2) WRITE(LU,99) IELM
98      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE2(-1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C     IDIMAT EST UNE MAJORATION DE LA SOMME DES NOMBRES DE VOISINS DE
C     TOUS LES POINTS.
C    
      IDIMAT = 2*NDP*NELEM
C
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'VOISIN : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     *            'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'VOISIN: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     *            'ERROR CODE: ',1I6)
      ENDIF
C
C-----------------------------------------------------------------------
C
C  CALCUL DU TABLEAU NVOIS POUR CHAQUE POINT
C  ATTENTION : NVOIS N'EST PAS LE NOMBRE DE VOISINS MAIS PERMET DE
C              RESERVER ASSEZ DE PLACE DANS LES TABLEAUX MAT1,2,3.
C
      DO 10 I=1,NPOIN
        NVOIS(I) = 0
10    CONTINUE
C
      DO 20 IFACE = 1,NFACE
        DO 30 IELEM=1,NELEM
          I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
          I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
30      CONTINUE
20    CONTINUE
C
C-----------------------------------------------------------------------
C
C  CALCUL DES ADRESSES DE CHAQUE POINT DANS UNE STRUCTURE DE TYPE
C  MATRICE COMPACTE
C
      IADR(1) = 1
      DO 50 I= 2,NPOIN
        IADR(I) = IADR(I-1) + NVOIS(I-1)
50    CONTINUE
C
      IMAX = IADR(NPOIN) + NVOIS(NPOIN) - 1
      IF(IMAX.GT.IDIMAT) THEN
        IF(LNG.EQ.1) WRITE(LU,51) IDIMAT,IMAX
        IF(LNG.EQ.2) WRITE(LU,52) IDIMAT,IMAX
51      FORMAT(1X,'VOISIN: TAILLE DE MAT1,2,3 (',1I6,') INSUFFISANTE',/,
     *         1X,'IL FAUT AU MOINS : ',1I6)
52      FORMAT(1X,'VOISIN: SIZE OF MAT1,2,3 (',1I6,') TOO SHORT',/,
     *         1X,'MINIMUM SIZE: ',1I6)
        CALL PLANTE2(-1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INITIALISATION A ZERO DE LA MATRICE COMPACTE
C
      DO 53 I=1,IMAX
        MAT1(I) = 0
53    CONTINUE
C
C-----------------------------------------------------------------------
C
C  BOUCLE SUR LES FACES DE CHAQUE ELEMENT :
C
      DO 60 IFACE = 1 , NFACE
      DO 70 IELEM = 1 , NELEM
C
      IFABOR(IELEM,IFACE) = -1
C
C        NUMEROS GLOBAUX DES POINTS DE LA FACE :
C
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
C
C        NUMEROS GLOBAUX ORDONNES :
C
         M1 = MIN0(I1,I2)
         M2 = MAX0(I1,I2)
C
         DO 80 IV = 1,NVOIS(M1)
C
           IF(MAT1(IADR(M1)+IV-1).EQ.0) THEN
              MAT1(IADR(M1)+IV-1)=M2
              MAT2(IADR(M1)+IV-1)=IELEM
              MAT3(IADR(M1)+IV-1)=IFACE
              GO TO 81
           ELSEIF(MAT1(IADR(M1)+IV-1).EQ.M2) THEN
              IELEM2 = MAT2(IADR(M1)+IV-1)
              IFACE2 = MAT3(IADR(M1)+IV-1)
              IFABOR(IELEM,IFACE) = IELEM2
              IFABOR(IELEM2,IFACE2) = IELEM
              GO TO 81
           ENDIF
C
80       CONTINUE
C
         IF(LNG.EQ.1) WRITE(LU,82)
         IF(LNG.EQ.2) WRITE(LU,83)
82       FORMAT(1X,'VOISIN : ERREUR DANS LE MAILLAGE       ',/,1X,
     *             '         PEUT-ETRE DES POINTS CONFONDUS')
83       FORMAT(1X,'VOISIN : ERROR IN THE MESH             ',/,1X,
     *             '         MAYBE SUPERIMPOSED POINTS     ')
         CALL PLANTE2(-1)
         STOP
C
81       CONTINUE
C
70    CONTINUE
60    CONTINUE
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       ***********************
                        SUBROUTINE ELEBD_PARTEL
C                       ***********************
C
C
C      TAKEN FROM BIEF AND ADAPTED : USE BIEF REMOVED
C                                    CALL PLANTE REMOVED
C                                    ALL ACTIONS UNDER IF(NCSIZE.GT.1) REMOVED
C
C
     *(NELBOR,NULONE,KP1BOR,IFABOR,NBOR,IKLE,SIZIKL,
     * IKLBOR,NELEM,NELMAX,NPTFRMAX,
     * NPOIN,NPTFR,IELM,LIHBOR,KLOG,IFANUM,OPTASS,ISEG,T1,T2,T3,
     *     NPOIN_TOT)
C
C***********************************************************************
C BIEF VERSION 5.3           23/08/99    J-M HERVOUET (LNH) 30 87 80 18
C COPYRIGHT 1999
C***********************************************************************
C
C    PRISMES DECOUPES EN TETRAEDRES
C
C    FONCTION : 1) CONSTRUCTION DES TABLEAUX NELBOR ET NULONE
C               2) CONSTRUCTION DU TABLEAU KP1BOR
C               3) DISTINCTION DANS LE TABLEAU IFABOR ENTRE
C                  LES FACES DE BORD SOLIDES ET LES FACES LIQUIDES
C               4) COMPLEMENT DE NBOR. 
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    NELBOR      |<-- | NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT|
C |    NULONE      |<-- | NUMERO LOCAL D'UN POINT DE BORD DANS         |
C |                |    | L'ELEMENT ADJACENT DONNE PAR NELBOR          |
C |    KP1BOR      |<-- | NUMERO DU POINT SUIVANT LE POINT DE BORD K.  |
C |    IFABOR      | -->| TABLEAU DES VOISINS DES FACES.
C |    NBOR        | -->| NUMERO GLOBAL DU POINT DE BORD K.
C |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
C |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
C |    T1,2,3      | -->| TABLEAUX DE TRAVAIL ENTIERS.
C |    NPOIN       | -->| NOMBRE TOTAL DE POINTS DU DOMAINE.
C |    NPTFR       | -->| NOMBRE DE POINTS FRONTIERES.
C |    IELM        | -->| TYPE D'ELEMENT.
C |                |    | 11 : TRIANGLES.
C |                |    | 21 : QUADRILATERES.
C |    LIHBOR      | -->| TYPES DE CONDITIONS AUX LIMITES SUR H
C |    KLOG        | -->| CONVENTION POUR LA CONDITION LIMITE DE PAROI
C |    MXPTVS      | -->| NOMBRE MAXIMUM DE VOISINS D'UN POINT
C |    MXELVS      | -->| NOMBRE MAXIMUM D'ELEMENTS AUTOUR D'UN POINT
C |________________|____|______________________________________________|
C  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : 
C
C SOUS-PROGRAMME APPELE : NEANT
C
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: KLOG,NELMAX,NELEM,SIZIKL
      INTEGER, INTENT(IN)    :: NPOIN,NPTFR,IELM,OPTASS,NPTFRMAX
      INTEGER, INTENT(OUT)   :: NELBOR(NPTFR),NULONE(NPTFR,2)
      INTEGER, INTENT(OUT)   :: KP1BOR(NPTFR,2)
      INTEGER, INTENT(INOUT) :: NBOR(2*NPTFRMAX)
      INTEGER, INTENT(INOUT) :: IFABOR(NELMAX,*)
      INTEGER, INTENT(IN)    :: IKLE(SIZIKL,*)
      INTEGER, INTENT(IN)    :: LIHBOR(NPTFRMAX)
      INTEGER, INTENT(OUT)   :: IKLBOR(NPTFR,2)
      INTEGER, INTENT(INOUT) :: IFANUM(NELMAX,*)
      INTEGER, INTENT(IN)    :: ISEG(NPTFR)
      INTEGER, INTENT(OUT)   :: T1(NPOIN),T2(NPOIN),T3(NPOIN) 
      INTEGER, INTENT(IN)    :: NPOIN_TOT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,NFACE,NPT,KEL,IPOIN
      INTEGER K,IFACE,I1,I2,N1,N2,IPT,IEL,I,K1,K2
C
      INTEGER SOMFAC(2,4,2)
C
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
C       TRIANGLES
        NFACE = 3
        NPT = 3
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,900) IELM
        IF(LNG.EQ.2) WRITE(LU,901) IELM
900     FORMAT(1X,'ELEBD : IELM=',1I6,' TYPE D''ELEMENT INCONNU')
901     FORMAT(1X,'ELEBD: IELM=',1I6,' UNKNOWN TYPE OF ELEMENT')
        CALL PLANTE2(-1)
        STOP
      ENDIF
C
C  INITIALISATION DE T1,2,3 A ZERO
C
      DO IPOIN=1,NPOIN
        T1(IPOIN) = 0
        T2(IPOIN) = 0
        T3(IPOIN) = 0
      ENDDO
C
C  ON STOCKE K DANS TRAV(*,3) A L'ADRESSE NBOR(K)
C  CE QUI PERMET DE PASSER DE NUMERO GLOBAL A NUMERO DE BORD
C
      DO K = 1, NPTFR
         T3(NBOR(K)) = K
      ENDDO
C
C  BOUCLE SUR TOUTES LES FACES DE TOUS LES ELEMENTS :
C
      DO 20 IFACE = 1 , NFACE
      DO 10 IELEM = 1 , NELEM
C
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
C
C      C'EST UNE VRAIE FACE DE BORD (LES FACES INTERNES EN PARALLELISME
C                                    SONT MARQUEES AVEC DES -2).
C      NUMEROS GLOBAUX DES POINTS DE LA FACE :
C
       I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
       I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
C
C      ON STOCKE DANS T1 ET T2 A L'ADRESSE I1 : I2 ET IELEM
C
       T1(I1) = I2
       T2(I1) = IELEM
C
C      UNE FACE LIQUIDE EST RECONNUE AVEC LA CONDITION LIMITE SUR H
C
C      07/02/03 IF(NPTFR...  COURTESY OLIVER GOETHEL, HANNOVER UNIVERSITY
       IF(NPTFR.GT.0) THEN
       IF(LIHBOR(T3(I1)).NE.KLOG.AND.LIHBOR(T3(I2)).NE.KLOG) THEN
C        FACE LIQUIDE : IFABOR=0  FACE SOLIDE : IFABOR=-1
         IFABOR(IELEM,IFACE)=0
       ENDIF
       ENDIF
C
      ENDIF
C
10    CONTINUE
20    CONTINUE
C
C  BOUCLE SUR TOUS LES POINTS:
C
C     07/02/03 IF(NPTFR...  CORRECTION BY OLIVER GOETHELS, HANNOVER
      IF(NPTFR.GT.0) THEN
      DO I = 1 , NPOIN
         IF(T1(I).NE.0) THEN
C          POINT SUIVANT
           KP1BOR(T3(I),1)=T3(T1(I))
C          POINT PRECEDENT
           KP1BOR(T3(T1(I)),2)=T3(I)
           NELBOR(T3(I))=T2(I)
         ENDIF
      ENDDO
      ENDIF
C
C CALCUL DU TABLEAU NULONE
C
      DO 50 K1=1,NPTFR
C
      K2=KP1BOR(K1,1)
      IEL = NELBOR(K1)
      N1  = NBOR(K1)
      N2  = NBOR(K2)
C
      I1 = 0
      I2 = 0
C
      DO 60 IPT=1,NPT
C
        IF(IKLE(IEL,IPT).EQ.N1) THEN
          NULONE(K1,1) = IPT
          I1 = 1
        ENDIF
        IF(IKLE(IEL,IPT).EQ.N2) THEN
          NULONE(K1,2) = IPT
          I2 = 1
        ENDIF
C
60    CONTINUE
C
      IF(I1.EQ.0.OR.I2.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,810) IEL
        IF(LNG.EQ.2) WRITE(LU,811) IEL
810     FORMAT(1X,'ELEBD: ERREUR DE NUMEROTATION DANS L''ELEMENT:',I6,/,
     *         1X,'       CAUSE POSSIBLE :                       '   ,/,
     *         1X,'       LE FICHIER DES CONDITIONS AUX LIMITES NE'  ,/,
     *         1X,'       CORRESPOND PAS AU FICHIER DE GEOMETRIE  ')
811     FORMAT(1X,'ELEBD: ERROR OF NUMBERING IN THE ELEMENT:',I6,
     *         1X,'       POSSIBLE REASON:                       '   ,/,
     *         1X,'       THE BOUNDARY CONDITION FILE IS NOT      '  ,/,
     *         1X,'       RELEVANT TO THE GEOMETRY FILE           ')
        CALL PLANTE2(-1)
        STOP
      ENDIF
C
50    CONTINUE
C
C  COMPLEMENT DU TABLEAU NBOR
C
C -----
C FD : BAW ONLY ?
C
C      OPEN(UNIT=89,FILE='FRONT_GLOB.DAT')
C      WRITE(89,*) NPOIN_TOT
C      WRITE(89,*) NPTFR
C      DO K=1,NPTFR
C         WRITE(89,*) NBOR(K)
C      END DO 
C -----
C
      DO 80 K=1,NPTFR
C
        NBOR(K+NPTFR) = NBOR(KP1BOR(K,1))
C
        IKLBOR(K,1) = K
        IKLBOR(K,2) = KP1BOR(K,1)
C
80    CONTINUE

      

C
C -----
C      DO K=1,NPTFR
C         WRITE(89,*) KP1BOR(K,1)
C      END DO
C        DO K=1,NPTFR
C         WRITE(89,*) KP1BOR(K,2)
C      END DO 
C ------
C
C ------
C      CALL FLUSH(89)
C      CLOSE(89)
C ------
C-----------------------------------------------------------------------
C
      RETURN
      END
      
C                       ******************
!
! 29/01/2009:

C                       ******************
                        SUBROUTINE PARES3D
C                       ******************

     *(NAMEINP,LI)
C
C**********************************************************************
C  12/11/2009 CHRISTOPHE DENIS SINETICS/I23 
C  NEW VERSION TO DECREASE THE PARES3D COMPUTING TIME BY IMPROVING 
C
C   - THE TETRA-TRIA CONNECTION
C   - THE POSTPROCESSING 
C
C COMMENTS ON THIS NEW VERSION ->  CD 
C *********************************************************************
C***********************************************************************
C PARTEL VERSION 5.6        08/06/06   O.BOITEAU/F.DECUNG(SINETICS/LNHE)
C PARTEL VERSION 5.8        02/07/07   F.DECUNG(LNHE)
C VERSION DE DEVELOPPEMENT POUR PRISE EN COMPTE PB DECOUPAGE
C F.DECUNG/O.BOITEAU (JANV 2008)
C COPYRIGHT 2006
C***********************************************************************
C
C    CONSTRUCTIONS DES FICHIERS POUR ALIMENTER LE FLOT DE DONNEES
C    PARALLELE LORS D'UN CALCUL ESTEL3D PARALLELE EN ECOULEMENT
C 
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    NAMEINP     | -->| NOM DU FICHIER DE GEOMETRIE ESTEL3D
C |    LI          | -->| UNITE LOGIQUE D'ECRITURE POUR MONITORING 
C |________________|____|______________________________________________|
C  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR :  PARTEL
C
C SOUS-PROGRAMME APPELE :
C        ALLOER, ALLOER2 (GESTION MSGS)
C        METIS_PARTMESHDUAL (FROM METIS LIBRARY)
C***********************************************************************
C
      IMPLICIT NONE
!     INTEGER, PARAMETER :: MAXNPROC = 1000  ! MAX PARTITION NUMBER [000..999]
      INTEGER, PARAMETER :: MAXNPROC = 100000 ! MAX PARTITION NUMBER [00000..99999]
      INTEGER, PARAMETER :: MAXLENHARD = 250 ! HARD MAX FILE NAME LENGTH
      INTEGER, PARAMETER :: MAXLENSOFT = 144 ! SOFT MAX FILE NAME LENGTH
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
! PARAMETRES D'APPEL DE LA ROUTINE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: NAMEINP
      INTEGER,                   INTENT(IN) :: LI
! VARIABLES LOCALES
      CHARACTER(LEN=MAXLENHARD) :: NAMELOG,NAMEINP2,NAMELOG2
      INTEGER :: NINP=10,NLOG=11,NINP2=12,NLOG2=13
      INTEGER :: NPARTS,I_S,I_SP,I,I_LEN,I_LENINP,IERR,J,K,COMPT,
     &           N,NUMTET,NUMTRI,NUMTRIG,I_LENLOG,L,NI,NF,NT,IBID,IDD,
     &           COMPT1,COMPT2,COMPT3,NBTRIIDD,IBID1,M,NI1,NF1,COLOR1,
     &           COLOR2,PR1,PR2,IDDBIS,IDDTERCE,NBTETJ,IDDNT,NIT,NFT,MT,
     &           NUMTRIB,NUMTETB,IBIDC,NBRETOUCHE
      LOGICAL :: IS,ISBIS,ISTERCE,LINTER
      CHARACTER(LEN=300) :: TEXTERROR  ! TEXTE MSG D'ERREUR
      CHARACTER(LEN=8)   :: STR8       ! TEXTE MSG D'ERREUR
      CHARACTER(LEN=300) :: STR26      ! TEXTE MSG D'ERREUR
      CHARACTER(LEN=80)  :: TITRE      ! MESH TITLE IN THE FILE
      CHARACTER(LEN=2)   :: MOINS1     ! "-1"
      CHARACTER(LEN=4)   :: BLANC      ! WHITE SPACE

      ! ADDITION JP RENAUD 15/02/2007
      CHARACTER(LEN=200) :: LINE       ! ONE LINE, 200 CHARACTERS MAXADDCH
      INTEGER            :: POS        ! POSITION OF A CHARACTER IN THE LINE
      INTEGER            :: IOS        ! STATUS INTEGER
      ! END ADDITION JP RENAUD
      CHARACTER(LEN=72) :: THEFORMAT
      
      CHARACTER(LEN=80), ALLOCATABLE :: LOGFAMILY(:)  ! LOG INFORMATIONS
      INTEGER            :: NSEC       ! TYPE OF THE SECTION READ
      INTEGER, PARAMETER :: NSEC1=151  ! MESH TITLE SECTION ID 
      INTEGER, PARAMETER :: NSEC2=2411 ! NODES COORDINATES SECTION ID
      INTEGER, PARAMETER :: NSEC3=2412 ! CONNECTIVITY SECTION ID
      INTEGER, PARAMETER :: NSEC4=2435 ! POUR CLORE PROPREMENT LA LECTURE
                                       ! DU UNV DANS ESTEL3D      
      LOGICAL            :: READ_SEC1  ! FLAG FOR READING SECTION 1
      LOGICAL            :: READ_SEC2  ! FLAG FOR READING SECTION 2
      LOGICAL            :: READ_SEC3  ! FLAG FOR READING SECTION 3
      INTEGER            :: NELEMTOTAL ! TOTAL NUMBER OF UNV ELEMENTS
      INTEGER            :: NPOINT     ! TOTAL NUMBER OF NODES 
      INTEGER            :: NBFAMILY   ! TOTAL NUMBER OF FAMILY
      INTEGER            :: NELIN      ! TOTAL NUMBER OF INNER TRIANGLES
      INTEGER            :: SIZE_FLUX  !  TOTAL NUMBER OF INNER SURFACES
      INTEGER, DIMENSION(:), ALLOCATABLE :: VECTNB  ! VECTEUR AUX POUR NACHB
!
      DOUBLE PRECISION, ALLOCATABLE :: X1(:),Y1(:),Z1(:) ! COORD NODES
      INTEGER,          ALLOCATABLE :: NCOLOR(:) ! NODES' COLOUR
      INTEGER,          ALLOCATABLE :: ECOLOR(:) ! ELEMENTS' COLOUR
      INTEGER            :: ELEM       ! TYPE OF THE ELEMENT 
      INTEGER            :: IKLE1,IKLE2,IKLE3,IKLE4,IKLEB   ! NODES
      INTEGER, DIMENSION(:), ALLOCATABLE :: IKLESTET ! CONNECTIVITE EN
                   ! RENUMEROTATION GLOBAL DE LA BIEF POUR LES TETRAEDRES
      INTEGER, DIMENSION(:), ALLOCATABLE :: IKLESTRI ! CONNECTIVITE EN
                   ! RENUMEROTATION GLOBAL DE LA BIEF POUR LES TRIANGLES
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLESTRIN ! CONNECTIVITE EN
                   ! RENUMEROTATION GLOBAL DE LA BIEF POUR LES TRIANGLES
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLEIN ! COPIE AJUSTEE DE IKLESTRIN
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TYPELEM ! TYPE D'ELT
      INTEGER            :: NBTET,NBTRI  ! NBRE DE TETRA, TRIANGLE BORD
      INTEGER, DIMENSION(:), ALLOCATABLE :: TETTRI, TETTRI2 ! JOINTURE
                                               !  TETRA/TRIANGLE DE BORD
      INTEGER            ::  EDGECUT ! VAR. AUXILIAIRE POUR METIS
      INTEGER, DIMENSION(:), ALLOCATABLE :: EPART ! NUMERO DE PARTITION
                                                  ! PAR ELEMENT
      INTEGER, DIMENSION(:), ALLOCATABLE :: NPART ! NUMERO DE PARTITION
                                                  ! PAR NOEUDS
      INTEGER, DIMENSION(:), ALLOCATABLE :: CONVTRI,CONVTET ! CONVERTISSEUR 
         ! NUMERO LOCAL TRIA/TETRA NUMERO GLOBAL; INVERSE DE TYPELEM(:,2)
      INTEGER            ::  TDEB,TFIN,TEMPS,PARSEC  ! RUNTIME
      CHARACTER(LEN=11), EXTERNAL :: EXTENS ! EXTENSION DES NOMS FICHIER
      INTEGER, DIMENSION(:), ALLOCATABLE :: NPOINTSD, NELEMSD ! NBRE
                            ! DE POINTS ET D'ELEMENTS PAR SOUS-DOMAINE
      INTEGER, DIMENSION(:), ALLOCATABLE :: NPOINTISD  ! NBRE
                            ! DE POINTS D'INTERFACE PAR SOUS-DOMAINE
                   ! VECTEURS LIES AUX CONNECTIVITEES NODALES INVERSES
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODES1,NODES2,NODES3,NODES4
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODES1T,NODES2T,NODES3T
      INTEGER, DIMENSION(:), ALLOCATABLE :: TRIUNV ! BUFFER POUR ECRIRE
                 ! DANS LES .UNV, D'ABORD LES TETRAS PUIS LES TRIA
! POUR TRAITEMENT DES DIRICHLETS CONFONDUS AVEC L'INTERFACE
      INTEGER  :: NBCOLOR ! NBRE DE COULEUR DE MAILLES EXTERNES
      INTEGER, DIMENSION(:), ALLOCATABLE :: PRIORITY
      INTEGER, DIMENSION(:), ALLOCATABLE :: NCOLOR2
! POUR TRAITEMENT DES DIRICHLETS SUR LES NOEUDS DE TETRA
      LOGICAL, DIMENSION(:,:), ALLOCATABLE :: TETCOLOR
      LOGICAL, DIMENSION(:), ALLOCATABLE :: DEJA_TROUVE
! INDISPENSABLE POUR PARALLELISME TELEMAC
      INTEGER, DIMENSION(:), ALLOCATABLE :: KNOLG 
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NACHB
      LOGICAL :: NACHBLOG
!     MAXIMUM GEOMETRICAL MULTIPLICITY OF A NODE (VARIABLE AUSSI
!     PRESENTE DANS LA BIEF, NE PAS CHANGER L'UNE SANS L'AUTRE)
      INTEGER, PARAMETER :: NBMAXNSHARE =  10
! CETTE VARIABLE EST LIEE A LA PRECEDENTE ET DIMENSIONNE DIFFERENTS
! VECTEURS
! NOTE SIZE OF NACHB WILL BE HERE 2 MORE THAN IN BIEF, BUT THE EXTRA 2 ARE
! LOCAL WORK ARRAYS
      INTEGER :: NBSDOMVOIS = NBMAXNSHARE + 2
!
      INTEGER, PARAMETER :: MAX_SIZE_FLUX = 200
! NUMBER OF INNER SURFACE (SAME AS SIZE_FLUX AT THE END)
      INTEGER, DIMENSION(MAX_SIZE_FLUX) :: SIZE_FLUXIN
! VECTEUR POUR PROFILING
      INTEGER  TEMPS_SC(20)
!
!F.D
      INTEGER, DIMENSION(:  ), ALLOCATABLE  :: TEMPO,GLOB_2_LOC
      INTEGER, DIMENSION(:,:), ALLOCATABLE  :: IKLES,IKLE,IFABOR
      INTEGER, DIMENSION(:,:), ALLOCATABLE  :: NULONE,IKLBOR
      INTEGER                               :: N1,N2,N3,IKL,IFACE
      INTEGER                               :: NSOLS,NSOLS_OLD
      INTEGER                               :: IFACEBIS,IELEMBIS
      INTEGER                               :: IELEM,IPTFR,IELEB,IPOIN
      LOGICAL, DIMENSION(:), ALLOCATABLE    :: FACE_CHECK
      INTEGER, PARAMETER                    :: NCOL = 256
      INTEGER, DIMENSION(NCOL  )            :: COLOR_PRIO
      INTEGER                               :: PRIO_NEW,NPTFR
      INTEGER, DIMENSION(:), ALLOCATABLE    :: NBOR2,NBOR
      INTEGER, DIMENSION(:), ALLOCATABLE    :: NELBOR,IPOBO
CD******************************************************    ADDED BY CHRISTOPHE DENIS
      INTEGER, DIMENSION(:), ALLOCATABLE     :: NELEM_P
C     SIZE NPARTS, NELEM_P(I) IS THE NUMBER OF FINITE ELEMENTS ASSIGNED TO SUBDOMAIN I
      INTEGER, DIMENSION(:), ALLOCATABLE     :: NPOIN_P
C     SIZE NPARTS, NPOIN_P(I) IS THE NUMBER OF NODES  ASSIGNED TO SUBDOMAIN I
      INTEGER :: NODE
C     ONE NODE ...
      INTEGER ::  POS_NODE 
C     POSITION OF ONE ONE NODE
      INTEGER :: MAX_NELEM_P
C     MAXIMUM NUMBER OF FINITE ELEMENTS ASSIGNED AMONG SUBDOMAINS
      INTEGER :: MAX_NPOIN_P
C     MAXIMUM NUMBER OF NODES ASSIGNED AMONG SUBDOMAINS
      INTEGER :: MAX_TRIA
C     MAXIMUM NUMBER OF TRIANGLE SHARING A NODE 
      INTEGER :: THE_TRI
C     ONE TRIANGLE 
      INTEGER :: JJ
C     INDEX COUNTER
      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMBER_TRIA
C     MAXIMUM NUMBER OF TRIANGLE SHARING A SAME NODE  
      INTEGER, DIMENSION(:,:), ALLOCATABLE  :: ELEGL
C     SIZE MAX_NELEM_P,NPARTS, ELEGL(J,I) IS THE GLOBAL NUMBER OF LOCAL FINITE ELEMENT J IN SUBDOMAIN I
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NODEGL
C     SIZE MAX_NPOIN_P,NPARTS, NODEGL(J,I) IS THE GLOBAL NUMBER OF LOCAL NODE J IN SUBDOMAIN I
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODELG
C     SIZE NPOINT, NODELG(I)=J, J IS THE LOCAL NUMBER OF GLOBAL NODE I ON ONE SUBDOMAIN
      INTEGER,  DIMENSION(:,:), ALLOCATABLE :: TRI_REF
C     SIZE NPOINT*MAX_TRIA
CD********************************************************     
      INTEGER SOMFAC(3,4)
      DATA SOMFAC / 1,2,3 , 4,1,2 , 2,3,4 , 3,4,1  /


!-----------------------------------------------------------------------
! 1. PREAMBULE
!---------------
      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(1),COUNT_RATE=PARSEC)
      ALLOCATE (VECTNB(NBSDOMVOIS-3))
      WRITE(LU,*)' '
      WRITE(LU,*)'+-------------------------------------------------+'
      WRITE(LU,*)'  PARTEL: TELEMAC ESTEL3D PARTITIONER'
      WRITE(LU,*)'+-------------------------------------------------+'
      WRITE(LU,*)' READING UNV AND LOG FILES'
! NAMES OF THE INPUT FILES:
      IF (NAMEINP.EQ.' ') THEN
        GOTO 149 
      ELSE
        WRITE(LU,89)NAMEINP
      ENDIF
      INQUIRE (FILE=NAMEINP,EXIST=IS)
      IF (.NOT.IS) GOTO 140
      DO
        READ(LI,'(A)')NAMELOG
        IF (NAMELOG.EQ.' ') THEN
          GOTO 150 
        ELSE
          WRITE(LU,90)NAMELOG
          EXIT
        ENDIF
      END DO  
      INQUIRE(FILE=NAMELOG,EXIST=IS)
      IF (.NOT.IS) GOTO 141   
      DO 
        READ(LI,*)NPARTS
        IF ( (NPARTS > MAXNPROC) .OR. (NPARTS < 2) ) THEN
          WRITE(LU,
     &    '('' NUMBER OF PARTITIONS MUST BE IN [2 -'',I9,'']'')') 
     &      MAXNPROC
        ELSE
          WRITE(LU,91)NPARTS
          EXIT
        ENDIF 
      ENDDO
      
      
! FIND THE INPUT FILES CORE NAME LENGTH
      I_S  = LEN(NAMEINP)
      I_SP = I_S + 1
      DO I=1,I_S
        IF (NAMEINP(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
      I_LENINP = I_LEN
      IF (I_LENINP > MAXLENSOFT) GOTO 144
!
      I_S  = LEN(NAMELOG)
      I_SP = I_S + 1
      DO I=1,I_S
        IF (NAMELOG(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
      I_LENLOG = I_LEN
      IF (I_LENLOG > MAXLENSOFT) GOTO 145
!
      OPEN(NINP,FILE=NAMEINP,STATUS='OLD',FORM='FORMATTED',ERR=131)
      REWIND(NINP)
      OPEN(NLOG,FILE=NAMELOG,STATUS='OLD',FORM='FORMATTED',ERR=130)
      REWIND(NLOG)
   
!----------------------------------------------------------------------
! 2A. LECTURE DU FICHIER .LOG
!---------------
      READ(NLOG,51,ERR=110,END=120)NPOINT
      READ(NLOG,52,ERR=110,END=120)NELEMTOTAL
      READ(NLOG,53,ERR=110,END=120)NBFAMILY
      NBFAMILY=NBFAMILY+1            ! POUR TITRE DU BLOC
      ALLOCATE(LOGFAMILY(NBFAMILY),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' LOGFAMILY')
      DO I=1,NBFAMILY
        READ(NLOG,50,ERR=111,END=120)LOGFAMILY(I) 
      ENDDO
      NBCOLOR=0

!      READ(NLOG,531,ERR=110,END=120)NBCOLOR

      READ(UNIT=NLOG, FMT='(A200)', IOSTAT=IOS) LINE
      IF (IOS .NE. 0) THEN
         !         '!----------------------------------!'
         TEXTERROR='! PROBLEM WITH THE NUMBER OF COLOR !'
         CALL ALLOER2(LU,TEXTERROR)
         CALL PLANTE2(-1)
      ENDIF
      POS = INDEX(LINE,':') + 1
      READ(UNIT=LINE(POS:), FMT=*, IOSTAT=IOS) NBCOLOR
      IF (IOS .NE. 0) THEN
         !         '!-------------------------------!'
         TEXTERROR='! PROBLEM WITH THE NUMBER COLOR !'
      ENDIF

      ALLOCATE(PRIORITY(NBCOLOR),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' PRIORITY')
      WRITE(LU,92) NPOINT
      WRITE(LU,93) NELEMTOTAL
      WRITE(LU,94) NBCOLOR
      IF (NBCOLOR.EQ.0) THEN
        WRITE(LU,*) 'VOUS AVEZ OUBLIE DE REMPLIR LE FICHIER LOG...'
        CALL PLANTE2(-1)
        STOP
      ENDIF

      ! MODIFICATION JP RENAUD 15/02/2007
      ! SOME TEXT HAS BEEN ADDED BEFORE THE LIOST OF PRIORITIES.
      ! READ A 200 CHARACTER LINE, FIND THE ':' AND THEN
      ! READ THE VALUES AFTER THE ':'
      READ(UNIT=NLOG, FMT='(A200)', IOSTAT=IOS) LINE
      IF (IOS .NE. 0) THEN
        !         '!------------------------------------------!'
        TEXTERROR='! PROBLEM WITH THE PRIORITY OF COLOR NODES !'
        CALL ALLOER2(LU,TEXTERROR)
        CALL PLANTE2(-1)
      ENDIF
      POS = INDEX(LINE,':') + 1
      READ(UNIT=LINE(POS:), FMT=*, IOSTAT=IOS) (PRIORITY(J),J=1,NBCOLOR)
      IF (IOS .NE. 0) THEN
        !         '!------------------------------------------!'
        TEXTERROR='! PROBLEM WITH THE PRIORITY OF COLOR NODES !'
        CALL ALLOER2(LU,TEXTERROR)
        CALL PLANTE2(-1)
      ENDIF
      ! END MODIFICATION JP RENAUD
      WRITE(LU,*) (PRIORITY(J),J=1,NBCOLOR)
      CLOSE(NLOG)
!
! 2B. ALLOCATIONS MEMOIRES ASSOCIEES
!--------------- 

CD    ****************************** ALLOCATION MEMORY ADDED BY CD
      ALLOCATE(NELEM_P(NPARTS),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,'NELEM_P')
      ALLOCATE(NPOIN_P(NPARTS),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,'NPOIN_P')
      ALLOCATE(NODELG(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,'NODELG')
CD    *******************************      
      
      ALLOCATE(X1(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' X1')
      ALLOCATE(Y1(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' Y1')
      ALLOCATE(Z1(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' Z1')
      ALLOCATE(NCOLOR(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NCOLOR')
      ALLOCATE(NCOLOR2(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NCOLOR2')
      ALLOCATE(ECOLOR(NELEMTOTAL),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' ECOLOR')
      ALLOCATE(IKLESTET(4*NELEMTOTAL),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' IKLESTET')
      ALLOCATE(IKLESTRI(3*NELEMTOTAL),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' IKLESTRI')
      ALLOCATE(IKLESTRIN(NELEMTOTAL,4),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' IKLESTRIN')
      ALLOCATE(TYPELEM(NELEMTOTAL,2),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' TYPELEM')
      ALLOCATE(CONVTRI(NELEMTOTAL),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' CONVTRI')
      ALLOCATE(CONVTET(NELEMTOTAL),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' CONVTET')
      ALLOCATE(NPOINTSD(NPARTS),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NPOINTSD')
      ALLOCATE(NELEMSD(NPARTS),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NELEMSD')
      ALLOCATE(NPOINTISD(NPARTS),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NPOINTISD')

!F.D
      ALLOCATE(NBOR2(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NBOR2')
      ALLOCATE(TEMPO(2*NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' TEMPO')
      ALLOCATE(FACE_CHECK(NBFAMILY),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' FACE_CHECK')      
      ALLOCATE(GLOB_2_LOC(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' GLOB_2_LOC')
      ALLOCATE(IKLES(NELEMTOTAL,4),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' IKLES')


      READ_SEC1 = .TRUE.
      READ_SEC2 = .TRUE.
      READ_SEC3 = .TRUE.

      DO WHILE ( READ_SEC1 .OR. READ_SEC2 .OR. READ_SEC3 )

          MOINS1 = '  '
          BLANC  = '1111'
          DO WHILE (MOINS1/='-1' .OR. BLANC/='    ')
              READ(NINP,2000, ERR=1100, END=1200) BLANC, MOINS1
          END DO

 2000     FORMAT(A4,A2)

          NSEC = -1

          DO WHILE (NSEC == -1)
              READ(NINP,*, ERR=1100, END=1200) NSEC
          END DO

 2100     FORMAT(I10)

          SELECT CASE (NSEC)

          CASE ( NSEC1 )

              READ_SEC1 = .FALSE.

              READ(NINP,25,ERR=1100, END=1200) TITRE

 25           FORMAT(A80)

          CASE ( NSEC2 )

              READ_SEC2 = .FALSE.
              NCOLOR(:) = -1
              TEMPO(:)  =  0

              DO IELEM = 1, NPOINT
                 READ(NINP,*,ERR=1100,END=1200) N, N1, N2, NCOLOR(IELEM)
          READ(NINP,*,ERR=1100,END=1200) X1(IELEM), Y1(IELEM), Z1(IELEM)
                 TEMPO(N) = IELEM
              END DO

          CASE (NSEC3 )

             READ_SEC3 = .FALSE.
                         
             NBTET         = 0  ! NUMBER OF TETRA ELEMENTS TO 0
             NBTRI         = 0  ! NUMBER OF BORDER ELEMENTS TO 0
             NPTFR         = 0  ! NUMBER OF BORDER NODES TO 0.
             NELIN         = 0  ! NUMBER OF INNER SURFACES TO 0.
             SIZE_FLUX     = 0  ! NUMBER OF USER SURFACES TO 0.
             NBOR2(:)      = 0  ! LOCAL TO GLOBAL NUMBERING
             GLOB_2_LOC(:) = 0  ! GLOBAL TO LOCAL NUMBERING

!OB'S STUFF
             ECOLOR(:)    = -1
             IKLESTET(:)  = -1
             IKLESTRI(:)  = -1
             TYPELEM(:,:) = -1
             CONVTRI(:)   = -1
             CONVTET(:)   = -1
!
             IKLESTRIN(:,:) = -1
                       
             FACE_CHECK(:) = .FALSE.
             !
             COLOR_PRIO(:)  = 0
             SIZE_FLUXIN(:) = 0
             !
             DO K = 1, NBCOLOR
                COLOR_PRIO(PRIORITY(K)) = K
             END DO

             DO IELEM = 1, NELEMTOTAL

            READ(NINP,*,ERR=1100,END=1200) NSEC,ELEM,N1,N2,NSOLS,N3
            
                IF (NSEC == -1) EXIT

                SELECT CASE ( ELEM )

                CASE ( 111 )

                   NBTET        = NBTET + 1

                   ECOLOR(IELEM) = NSOLS

               READ(NINP,*, ERR=1100, END=1200) IKLE1,IKLE2,IKLE3,IKLE4

                   IKLES(IELEM, 1) = TEMPO(IKLE1)
                   IKLES(IELEM, 2) = TEMPO(IKLE2)
                   IKLES(IELEM, 3) = TEMPO(IKLE3)
                   IKLES(IELEM, 4) = TEMPO(IKLE4)

!OB'S STUFF
                   N=4*(NBTET-1)+1                       
                   IKLESTET(N)=IKLE1    ! VECTEUR DE CONNECTIVITE
                   IKLESTET(N+1)=IKLE2
                   IKLESTET(N+2)=IKLE3
                   IKLESTET(N+3)=IKLE4
                   TYPELEM(IELEM,1)=ELEM    ! POUR TYPER LES ELTS
                   TYPELEM(IELEM,2)=NBTET   ! POUR CONVERSION NUM ELT> NUM TETRA
                   CONVTET(NBTET)=IELEM     ! L'INVERSE
                   
                CASE ( 91 )
             
                   IF (NSOLS.GT.0.AND.NSOLS.LT.100) THEN

                      IF ( NSOLS > NCOL ) THEN
                         WRITE(LU,*) 'COLOR ID POUR SURFACES EXTERNES ', 
     &                        ' TROP GRAND. LA LIMITE EST : ',NCOL
                      END IF

                      PRIO_NEW = COLOR_PRIO(NSOLS)

                      IF ( PRIO_NEW .EQ. 0 ) THEN
                         WRITE(LU,*) ' NUMERO DE FACE NON DECLARE',
     &                        'DANS LE TABLEAU UTILISATEUR LOGFAMILY ',      
     &                        'VOIR LE FICHIER DES PARAMETRES '
                         CALL PLANTE2(1)
                      END IF
                     
                      FACE_CHECK(PRIO_NEW) = .TRUE.

                      NBTRI = NBTRI + 1

                      ECOLOR(IELEM) = NSOLS

                     READ(NINP,*, ERR=1100, END=1200) IKLE1, IKLE2,IKLE3
                     !
                     PRIO_NEW = SIZE_FLUXIN(NSOLS)
                     !
                     IF (PRIO_NEW.EQ.0) THEN 
                        SIZE_FLUX = SIZE_FLUX + 1
                        SIZE_FLUXIN(NSOLS) = 1
                     ENDIF

                      IKLES(IELEM, 1) = TEMPO(IKLE1)
                      IKLES(IELEM, 2) = TEMPO(IKLE2)
                      IKLES(IELEM, 3) = TEMPO(IKLE3)

!OB'S STUFF
                      N=3*(NBTRI-1)+1
                      IKLESTRI(N)=IKLE1
                      IKLESTRI(N+1)=IKLE2
                      IKLESTRI(N+2)=IKLE3
                      TYPELEM(IELEM,1)=ELEM    ! IDEM QUE POUR TETRA
                      TYPELEM(IELEM,2)=NBTRI
                      CONVTRI(NBTRI)=IELEM

                      DO J=1,3
                                               
                         IKL = IKLES(IELEM,J)
                      
                         IPTFR = GLOB_2_LOC(IKL)

                         IF ( IPTFR .EQ. 0 ) THEN
                            
                            NPTFR           = NPTFR+1
                            NBOR2(NPTFR)    = IKL
                            GLOB_2_LOC(IKL) = NPTFR        
                            IPTFR           = NPTFR

                         END IF                                                        
                         
                    ENDDO  ! LOOP OVER THE NODES OF THE ELEMENT

                 ELSE IF (NSOLS.GT.100) THEN
                    !
                    ! USER-DEFINED SURFACE FOR FLUXES COMPUTATION
                    !                      
                    ! NELIN IS THE COUNTER FOR THE INTERNAL ELEMENTS.
                    ! ACTUALLY, WE ARE READING THE NEXT INTERNAL ELEMENT.

                    ! NSOLS_OLD IS USED FOR SAVING USE OF A NEW VARIABLE
                    NSOLS_OLD = NSOLS
                    !
                    ! PRIO_NEW IS USED FOR SAVING USE OF A NEW VARIABLE
                    PRIO_NEW = SIZE_FLUXIN(NSOLS_OLD)
                    !
                    IF (PRIO_NEW.EQ.0) THEN
                       SIZE_FLUX = SIZE_FLUX + 1
                       SIZE_FLUXIN(NSOLS_OLD) = 1
                    ENDIF
                    !
                    NELIN = NELIN + 1
                    !
                    READ(NINP,*, ERR=1100, END=1200) IKLE1, IKLE2,IKLE3
                    !
                         IKLESTRIN(NELIN,1) = NSOLS
                         IKLESTRIN(NELIN,2) = TEMPO(IKLE1)
                         IKLESTRIN(NELIN,3) = TEMPO(IKLE2)
                         IKLESTRIN(NELIN,4) = TEMPO(IKLE3)
                    !
                 ELSE           ! THIS IS AN INNER SURFACE, JUST READ THE LINE.

                    READ(NINP,*, ERR=1100, END=1200) IKLE1, IKLE2,IKLE3
                    
                 END IF
                                  
              CASE DEFAULT      ! THIS IS AN UNKNOWN ELEMENT.
                 
                 WRITE(LU,*) 'ELEMENT INCONNU DANS LE MAILLAGE'
                 
              END SELECT        ! THE TYPE OF THE MESH ELEMENT
              
           END DO               ! LOOP OVER ELEMENTS TO READ.

           DO K=1,NBCOLOR
              IF ( .NOT. FACE_CHECK(K)) THEN
                 WRITE(LU,*) ' LA COULEUR DE FACE ',LOGFAMILY(K),
     &                ' N''APPARAIT PAS DANS LE MAILLAGE.'
              END IF
           END DO
           
!-----------------------------------------------------------------------

      END SELECT                ! TYPE OF THE SECTION
      
      END DO                    ! WHILE LOOP OVER SECTIONS TO READ

!------------------------------------------------------- FIN VERSION F.D

! CORRECTION DU NOMBRE D'ELEMENTS TOTAL CAR CELUI DANS LE .LOG EST
! COMPORTE DES ELEMENTS NON PRIS EN COMPTE DANS UNE ETUDE ESTEL
      NELEMTOTAL=NBTET+NBTRI

      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(2),COUNT_RATE=PARSEC)
      WRITE(LU,*)' TEMPS DE LECTURE FICHIERS LOG & UNV',
     &           (1.0*(TEMPS_SC(2)-TEMPS_SC(1)))/(1.0*PARSEC),' SECONDS'
!----------------------------------------------------------------------
! 3A. CONSTRUCTION DE TETTRI/TETTRI2: CORRESPONDANCE TETRA > TRIA
!---------------

      ALLOCATE(NELBOR(NBTRI),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NELBOR')
      ALLOCATE(NULONE(NBTRI,3),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NULONE')
      ALLOCATE(IKLBOR(NBTRI,3),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' IKLBOR')
      ALLOCATE(IKLE(NBTET,4),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' IKLE')
      ALLOCATE(IFABOR(NBTET,4),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' IFABOR')
!
!------------------------------------------------------- DEBUT VERSION O. BOITEAU
! RECHERCHE DE LA CORRESPONDANCE TETRAEDRE <--> TRIANGLES DE BORDS
!      ALLOCATE(TETTRI(4*NBTET),STAT=IERR)
!      IF (IERR.NE.0) CALL ALLOER(LU,' TETTRI')
!      ALLOCATE(TETTRI2(NBTET),STAT=IERR)
!      IF (IERR.NE.0) CALL ALLOER(LU,' TETTRI2')
!      TETTRI(:)=-1
!      TETTRI2(:)=0
!      DO I=1,NBTRI
!        N=3*(I-1)+1
!        IKLE1=IKLESTRI(N)
!        IKLE2=IKLESTRI(N+1)
!        IKLE3=IKLESTRI(N+2)
!        DO J=1,NBTET
!          COMPT=0
!          DO K=1,4
!            IKLEB=IKLESTET(4*(J-1)+K)
!            IF (IKLEB.EQ.IKLE1) COMPT=COMPT+1
!            IF (IKLEB.EQ.IKLE2) COMPT=COMPT+10
!            IF (IKLEB.EQ.IKLE3) COMPT=COMPT+100 
!          ENDDO ! BOUCLE SUR LES NOEUDS DU TETRAEDRE J
!          IF (COMPT.EQ.111) THEN   ! TETRAEDRE J ASSOCIE AU TRIANGLE I
!            NI=TETTRI2(J)
!            IF (NI==4) THEN   ! TETRA LIE A PLUS DE 4 TRIA, EXIT             
!              GOTO 153
!            ELSE              ! PROCHAIN EMPLACEMENT DE LIBRE
!              M=4*(J-1)+NI+1  ! DANS TETTRI
!              TETTRI2(J)=NI+1
!              TETTRI(M)=I     ! EN NUMEROTATION LOCALE
!            ENDIF
!            EXIT
!          ENDIF
!          IF (J.EQ.NBTET) GOTO 143  ! ERREUR CAR TRIANGLE SOLITAIRE
!        ENDDO  ! SUR LA BOUCLE EN TETRAEDRES
!      ENDDO ! SUR LA BOUCLE EN TRIANGLES DE BORDS
!------------------------------------------------------- FIN VERSION O. BOITEAU

!------------------------------------------------------- DEBUT VERSION F.D
      ALLOCATE(TETTRI(4*NBTET),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' TETTRI')
      ALLOCATE(TETTRI2(NBTET),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' TETTRI2')
!
      TETTRI (:) =-1
      TETTRI2(:) =0
!      
      DO IELEM = 1, NBTET
         DO I = 1,4
            IKLE(IELEM,I ) = IKLES (IELEM, I)
         END DO
      END DO
!
      DEALLOCATE(IKLES)
!
      ALLOCATE(IKLEIN(NELIN,4),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' IKLEIN')
!
      DO IELEM = 1, NELIN
         DO I = 1,4
            IKLEIN(IELEM,I ) = IKLESTRIN (IELEM, I)
         END DO
      END DO
!      
      DEALLOCATE(IKLESTRIN)
!
      WRITE(LU,*) 'FIN DE LA COPIE DE LA CONNECTIVITE INITIALE'
!      
      ALLOCATE(NBOR(NPTFR),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NBOR')
!
      DO IELEM = 1, NPTFR
            NBOR(IELEM) = NBOR2(IELEM)
      END DO
!
      DEALLOCATE(NBOR2)
!
      WRITE(LU,*) 'VOISIN31'

      CALL VOISIN31_PARTEL (NBTET, NBTET,NBTET,
     &  NPOINT,NPTFR,IKLE,IFABOR,NBOR)

      WRITE(LU,*) 'FIN DE VOISIN31'
      
      ALLOCATE(IPOBO(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,'IPOBO')

      CALL ELEBD31_PARTEL( NELBOR, NULONE, IKLBOR,    
     &              IFABOR, NBOR, IKLE,         
     &              NBTET, NBTRI, NBTET, NPOINT, 
     &              NPTFR,IPOBO)

      DEALLOCATE(IPOBO)

      WRITE(LU,*) 'FIN DE ELEBD31'
      ALLOCATE(NUMBER_TRIA(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,'NUMBER_TRIA')
      NUMBER_TRIA = 0
!
      MAX_TRIA=0
      DO J = 1, NBTRI
         K = 3*(J-1)+1  
         IKLE1 = IKLESTRI(K)
         IKLE2 = IKLESTRI(K+1)
         IKLE3 = IKLESTRI(K+2)
         THE_TRI=IKLE1
         IF (IKLE2 < THE_TRI) THE_TRI=IKLE2 
         IF (IKLE3< THE_TRI)  THE_TRI=IKLE3
         NUMBER_TRIA(THE_TRI)=NUMBER_TRIA(THE_TRI)+1
      END DO
      MAX_TRIA=MAXVAL(NUMBER_TRIA)
!    
      DEALLOCATE(NUMBER_TRIA)
!
      ALLOCATE(TRI_REF(NPOINT,0:MAX_TRIA),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' TRI_REF')
       TRI_REF=0 
       DO J = 1, NBTRI
         K = 3*(J-1)+1  
         IKLE1 = IKLESTRI(K)
         IKLE2 = IKLESTRI(K+1)
         IKLE3 = IKLESTRI(K+2)
         THE_TRI=IKLE1
         IF (IKLE2 < THE_TRI) THE_TRI=IKLE2 
         IF (IKLE3< THE_TRI)  THE_TRI=IKLE3
         TRI_REF(THE_TRI,0)=TRI_REF(THE_TRI,0)+1
         POS=TRI_REF(THE_TRI,0)
         TRI_REF(THE_TRI,POS)=J
      END DO
      

      DO IELEB = 1,NBTRI
         IELEM = NELBOR(IELEB)
         IKLE1 = NBOR(IKLBOR(IELEB,1))
         IKLE2 = NBOR(IKLBOR(IELEB,2))
         IKLE3 = NBOR(IKLBOR(IELEB,3))
         THE_TRI=IKLE1
         IF (IKLE2 < THE_TRI) THE_TRI=IKLE2 
         IF (IKLE3<THE_TRI)  THE_TRI=IKLE3
         POS=TRI_REF(THE_TRI,0)
         IS = .FALSE.
          M  = -1
         DO JJ = 1, POS
            J=TRI_REF(THE_TRI,JJ)
            K = 3*(J-1)+1            
            IF ((IKLE1.EQ.IKLESTRI(K)).AND.
     &          (IKLE2.EQ.IKLESTRI(K+1)).AND.
     &          (IKLE3.EQ.IKLESTRI(K+2))) THEN
               IS = .TRUE.
            ELSE IF ((IKLE1.EQ.IKLESTRI(K)).AND.
     &          (IKLE3.EQ.IKLESTRI(K+1)).AND.
     &          (IKLE2.EQ.IKLESTRI(K+2))) THEN
               IS = .TRUE.
            ELSE IF ((IKLE2.EQ.IKLESTRI(K)).AND.
     &          (IKLE1.EQ.IKLESTRI(K+1)).AND.
     &              (IKLE3.EQ.IKLESTRI(K+2))) THEN
               IS = .TRUE.
            ELSE IF ((IKLE2.EQ.IKLESTRI(K)).AND.
     &          (IKLE3.EQ.IKLESTRI(K+1)).AND.
     &          (IKLE1.EQ.IKLESTRI(K+2))) THEN
               IS = .TRUE.
            ELSE IF ((IKLE3.EQ.IKLESTRI(K)).AND.
     &          (IKLE1.EQ.IKLESTRI(K+1)).AND.
     &          (IKLE2.EQ.IKLESTRI(K+2))) THEN
               IS = .TRUE.
            ELSE IF ((IKLE3.EQ.IKLESTRI(K)).AND.
     &          (IKLE2.EQ.IKLESTRI(K+1)).AND.
     &          (IKLE1.EQ.IKLESTRI(K+2))) THEN
               IS = .TRUE.         
            ENDIF
            IF (IS) THEN
               M = J
               EXIT
            ENDIF
         ENDDO
         DO I = 1,4
            IF (IFABOR(IELEM,I).EQ.0) THEN
               IF ((IKLE1.EQ.(IKLE(NELBOR(IELEB),SOMFAC(1,I))))
     &         .AND.(IKLE2.EQ.(IKLE(NELBOR(IELEB),SOMFAC(2,I))))  
     &         .AND. (IKLE3.EQ.(IKLE(NELBOR(IELEB),SOMFAC(3,I)))))
     &         THEN
                  NI = TETTRI2(IELEM)
                  N  = 4*(IELEM-1)+NI+1
                  TETTRI(N) = M
C                  WRITE(*,*) N, '---> ',M
                  TETTRI2(IELEM) = NI + 1
               ENDIF
            ENDIF
         END DO
      ENDDO
!
      DEALLOCATE(TRI_REF)
!
!
!
!
      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(3),COUNT_RATE=PARSEC)
!------------------------------------------------------- FIN VERSION F.D




! 3B. CONSTRUCTION DE NODES1/NODES2/NODES3: CONNECTIVITE INVERSE NOEUD > TETRA
!     POUR L'ECRITURE A LA VOLEE DES UNV LOCAUX
!---------------
! PARCOURS DES MAILLES POUR CONNAITRE LE NOMBRE DE MAILLES QUI 
! LES REFERENCE
      ALLOCATE(NODES1(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NODES1')
      NODES1(:)=0
      DO I=1,NBTET
        DO K=1,4
          IKLEB=IKLESTET(4*(I-1)+K)
          NODES1(IKLEB)=NODES1(IKLEB)+1
        ENDDO
      ENDDO
! NOMBRE DE REFERENCEMENT DE POINTS ET POINTEUR NODES2 VERS NODES3
! LE IEME POINT A SA LISTE DE TETRA (EN NUMEROTATION LOCALE TETRA)
! DE NODES3(NODES2(I)) A NODES3(NODES2(I)+NODES1(I)-1)
      ALLOCATE(NODES2(NPOINT+1),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NODES2')
      COMPT=0
      NODES2(1)=1
      DO I=1,NPOINT
        COMPT=COMPT+NODES1(I)
        NODES2(I+1)=COMPT+1
      ENDDO
! POUR UN NOEUDS DONNE, QU'ELLES SONT LES MAILLES QUI LE CONCERNENT
      ALLOCATE(NODES3(COMPT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NODES3')
      NODES3(:)=-1
      DO I=1,NBTET
        DO K=1,4
          IKLEB=IKLESTET(4*(I-1)+K)
          NI=NODES2(IKLEB)
          NF=NI+NODES1(IKLEB)-1
          NT=-999
          DO N=NI,NF ! ON CHERCHE LE PREMIER INDICE DE LIBRE DE NODES3
            IF (NODES3(N)==-1) THEN
              NT=N
              EXIT
            ENDIF
          ENDDO ! EN N
          IF (NT==-999) THEN
            GOTO 146  ! PB DE DIMENSIONNEMENT DE VECTEURS NODESI
          ELSE
            NODES3(NT)=I  ! NUMERO LOCAL DU TETRA I ASSOCIE AU NOEUD NT
          ENDIF
        ENDDO
      ENDDO

! 3C. CONSTRUCTION DE NODES1T/NODES2T/NODES3T: CONNECTIVITE INVERSE NOEUD > TRIA
!     POUR LA COULEUR DES NOEUDS (DIRICHLET SUR L'INTERFACE)
!---------------
      ALLOCATE(NODES1T(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NODES1T')
      NODES1T(:)=0
      DO I=1,NBTRI
        DO K=1,3
          IKLEB=IKLESTRI(3*(I-1)+K)
          NODES1T(IKLEB)=NODES1T(IKLEB)+1
        ENDDO
      ENDDO
      ALLOCATE(NODES2T(NPOINT+1),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NODES2T')
      COMPT=0
      NODES2T(1)=1
      DO I=1,NPOINT
        COMPT=COMPT+NODES1T(I)
        NODES2T(I+1)=COMPT+1
      ENDDO
      ALLOCATE(NODES3T(COMPT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NODES3T')
      NODES3T(:)=-1
      DO I=1,NBTRI
        DO K=1,3
          IKLEB=IKLESTRI(3*(I-1)+K)
          NI=NODES2T(IKLEB)
          NF=NI+NODES1T(IKLEB)-1
          NT=-999
          DO N=NI,NF ! ON CHERCHE LE PREMIER INDICE DE LIBRE DE NODES3T
            IF (NODES3T(N)==-1) THEN
              NT=N
              EXIT
            ENDIF
          ENDDO ! EN N
          IF (NT==-999) THEN
            GOTO 146  ! PB DE DIMENSIONNEMENT DE VECTEURS NODESI
          ELSE
            NODES3T(NT)=I  ! NUMERO LOCAL DU TETRA I ASSOCIE AU NOEUD NT
          ENDIF
        ENDDO
      ENDDO
      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(4),COUNT_RATE=PARSEC)
      WRITE(LU,*)' TEMPS CONNECTIVITE INVERSE PART1/ PART2',
     &          (1.0*(TEMPS_SC(3)-TEMPS_SC(2)))/(1.0*PARSEC),'/',
     &          (1.0*(TEMPS_SC(4)-TEMPS_SC(3)))/(1.0*PARSEC),' SECONDS'
         
!----------------------------------------------------------------------
! 4. PARTITIONING
!---------------

!        DO I=1,4*NBTET
!                WRITE(LU,*) 'TETTRIALPHA',TETTRI(I)       
!        ENDDO
!        DO I=1,NBTET
!                WRITE(LU,*) 'TETTRIBETA',TETTRI2(I)
!        ENDDO

      WRITE(LU,*)' '
      WRITE(LU,*)' STARTING METIS MESH PARTITIONING------------------+'
      ALLOCATE(EPART(NBTET),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER (LU, 'EPART')
      ALLOCATE (NPART(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER (LU, 'NPART')
      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(5),COUNT_RATE=PARSEC)
! PARTITIONNEMENT DES MAILLES
      CALL METIS_PARTMESHDUAL(NBTET,NPOINT,IKLESTET,2,1,NPARTS,EDGECUT,
     &    EPART,NPART)
      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(6),COUNT_RATE=PARSEC)
      WRITE(LU,*)' '
      WRITE(LU,*)' END METIS MESH PARTITIONING------------------+'
      WRITE(LU,*)' TEMPS CONSOMME PAR  METIS ',
     &           (1.0*(TEMPS_SC(6)-TEMPS_SC(5)))/(1.0*PARSEC),' SECONDS'
      WRITE(LU,80) NELEMTOTAL,NPOINT
      WRITE(LU,81) NBTET,NBTRI
      WRITE(LU,82) EDGECUT,NPARTS
      WRITE(LU,*) 'SORTIE DE METIS CORRECTE'
CD ******************************************************
CD     LOOP  OVER THE TETRA TO COMPUTER THE NUMBER AND THE LABEL
CD     OF FINITE ELEMENTS ASSIGNED TO  EACH SUBDOMAIN
CD ******************************************************
CD     COMPUTATION OF THE MAXIMUM NUMBER OF FINITE ELEMENTS ASSIGNED TO ONE SUBDOMAIN
      NELEM_P(:)=0
      NPOIN_P(:)=0
       DO I=1,NBTET
         NELEM_P(EPART(I))=NELEM_P(EPART(I))+1
      END DO
      MAX_NELEM_P=MAXVAL(NELEM_P)
      NELEM_P(:)=0
CD     ALLOCATION OF THE ELEGL ARRAY 
      ALLOCATE(ELEGL(MAX_NELEM_P,NPARTS),STAT=IERR)
CD     ELEGL IS THE FILLED 
      IF (IERR.NE.0) CALL ALLOER(LU,'ELEGL')
      DO I=1,NBTET
         NELEM_P(EPART(I))=NELEM_P(EPART(I))+1
         ELEGL(NELEM_P(EPART(I)),EPART(I))=I
       END DO
CD     COMPUTE THE MAXIMUM OF NODES ASSIGNED TO ONE SUBDOMAIN
       NODELG(:)=0
CD     FOR EACH SUBDOMAIN IDD
       DO IDD=1,NPARTS  
          NODELG(:)=0
CD         LOOP ON THE FINITE ELEMENTS IELEM ASSIGNED TO SUBDOMAIN IDD
          DO POS=1,NELEM_P(IDD)
            IELEM=ELEGL(POS,IDD)
            N=4*(IELEM-1)+1
CD          LOOP OF THE NODE CONTAINED IN IELEM            
            DO K=0,3
               NODE=IKLESTET(N+K)
               IF (NODELG(NODE) .EQ. 0) THEN
                  NPOIN_P(IDD)=NPOIN_P(IDD)+1
                  NODELG(NODE)=NPOIN_P(IDD)
               END IF
            END DO
         END DO
      END DO
CD    ALLOCATION AND FILLING OF  THE NODEGL ARRAY      
      MAX_NPOIN_P=MAXVAL(NPOIN_P)
      NPOIN_P(:)=0
      NODELG(:)=0
!
      ALLOCATE(NODEGL(MAX_NPOIN_P,NPARTS),STAT=IERR)
       IF (IERR.NE.0) CALL ALLOER(LU,'NODEGL')
       DO IDD=1,NPARTS
          NODELG(:)=0
          DO POS=1,NELEM_P(IDD)
             IELEM=ELEGL(POS,IDD)
             N=4*(IELEM-1)+1
             DO K=0,3
                NODE=IKLESTET(N+K)
                IF (NODELG(NODE) .EQ. 0) THEN
                   NPOIN_P(IDD)=NPOIN_P(IDD)+1
                   NODELG(NODE)=NPOIN_P(IDD)
                   NODEGL(NPOIN_P(IDD),IDD)=NODE
                END IF
             END DO
          END DO
       END DO
!            
!----------------------------------------------------------------------
! 5A. ALLOCATIONS POUR ECRITURE DES FICHIERS .UNV/.LOG ASSOCIANT UN SOUS-DOMAINE
!     PAR PROC
!------------

      NAMEINP2=NAMEINP
      NAMELOG2=NAMELOG
      BLANC='    '
      MOINS1='-1'
      ALLOCATE(NODES4(NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NODES4')
C$$$      NODES4(:)=-1
      ALLOCATE(KNOLG(NPOINT),STAT=IERR)      ! C'EST SOUS-OPTIMAL EN
      IF (IERR.NE.0) CALL ALLOER(LU,' KNOLG')! TERME DE DIMENSIONNEMENT
      KNOLG(:)=-1      ! MAIS PLUS RAPIDE POUR LE REMPLISSAGE ULTERIEUR
! 
! PARAMETRE NBSDOMVOIS (NOMBRE DE SOUS DOMAINES VOISINS+2)
!      
      ALLOCATE(NACHB(NBSDOMVOIS,NPOINT),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' NACHB')
      NACHB(1,:)=0
      DO J=2,NBSDOMVOIS-1
        NACHB(J,:)=-1
      ENDDO
      ALLOCATE(TRIUNV(4*NBTRI),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER (LU, 'TRIUNV')
!
!
! 5B. RECHERCHE DE LA VRAI COULEUR AUX NOEUDS POUR EVITER LES PBS DE DIRICHLET
!     AUX INTERFACES
!---------------
      NCOLOR2(:)=-1
      DO J=1,NPOINT      ! BOUCLE SUR TOUS LES POINTS DU MAILLAGES
        NI=NODES2T(J)
        NF=NI+NODES1T(J)-1
        DO N=NI,NF       ! BOUCLE SUR LES TETRA CONTENANT LE POINT J
          NUMTET=NODES3T(N)   ! TRIA DE NUMERO LOCAL NUMTET
          NUMTRIG=CONVTRI(NUMTET)  ! NUMERO GLOBAL DU TRIANGLE
          COLOR1=ECOLOR(NUMTRIG)   ! COULEUR DU NOEUD AVEC CE TRIA
          COLOR2=NCOLOR2(J)
          IF (COLOR2 > 0) THEN   ! ON PRIORISE LES COULEURS
            PR1=0
            PR2=0
            DO L=1,NBCOLOR
              IF (PRIORITY(L)==COLOR1) THEN
                PR1=L
 1           ENDIF
              IF (PRIORITY(L)==COLOR2) THEN
                PR2=L
              ENDIF
            ENDDO
            IF ((PR1==0).OR.(PR2==0)) GOTO 154
            IF (PR1<PR2) NCOLOR2(J)=COLOR1  ! ON CHANGE DE COULEUR
          ELSE        ! PREMIERE FOIS QUE CE NOEUD EST TRAITE
            NCOLOR2(J)=COLOR1
          ENDIF
        ENDDO
      ENDDO

      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(7),COUNT_RATE=PARSEC)

!      DO IELEM = 1, NPOINT
!         WRITE(LU,*) 'NCOLOR2',NCOLOR2(IELEM)
!      ENDDO

!      DO IELEM = 1, NBCOLOR
!         WRITE(LU,*) 'PRIOR',PRIORITY(IELEM)
!      ENDDO
      
! OB D
!--------------
! RAJOUT POUR TENIR COMPTE DES COULEURS DES NOEUDS DE TETRAS LIES
! AU TRIA DE BORD ET SITUES DANS D'AUTRES SD
!--------------
      ALLOCATE(TETCOLOR(NBTET,4),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,' TETCOLOR')
      TETCOLOR(:,:)=.FALSE.
      NBRETOUCHE=0
      DO IPTFR=1,NPTFR      ! BOUCLE SUR TOUS LES POINTS DE BORD
        J=NBOR(IPTFR)
!       ON NE FAIT QQE CHOSE (EVENTUELLEMENT) QUE SI IL Y A UN TRIA
!       DE BORD (ECOLOR>0 ET NCOLOR2 !=-1). GRACE AU TRAITEMENT PRECEDENT
!       ON S'EN REND COMPTE DIRECTEMENT VIA NCOLOR2.
	LINTER=.FALSE.
        NBTETJ=NODES1(J) ! NBRE DE TETRA RATTACHES A CE NOEUD
        NI=NODES2(J)     ! ADRESSE DANS NODES3 DU PREMIER
        NF=NI+NBTETJ-1
	IF (NCOLOR2(J) > 0) THEN
	  ! ON CHERCHE A SAVOIR SI LE NOEUD EST A L'INTERFACE LINTER=.TRUE.
          DO N=NI,NF       ! BOUCLE SUR LES TETRA CONTENANT LE POINT J
            NT=NODES3(N)   ! TETRA DE NUMERO LOCAL NT
	    IF (N == NI) THEN
	      IDDNT=EPART(NT)
	    ELSE
	      IF (EPART(NT) /= IDDNT) THEN
	        LINTER=.TRUE.
		GOTO 20     ! ON A LE RENSEIGNEMENT DEMANDE, ON SORT
	      ENDIF
	    ENDIF
	  ENDDO	          ! FIN BOUCLE SUR LES TETRAS
   20     CONTINUE
!         LE NOEUD J EST UN NOEUD D'INTERFACE. ON VA COMMUNIQUER AU NOEUD
!         CORRESPONDANT DES TETRAS (SI UN TRIA DE BORD N'EST PAS SUR CETTE
!         FACE AUXQUEL CAS LE PB EST DEJA REGLE), LA BONNE COULEUR.
	  IF (LINTER) THEN  
            DO N=NI,NF       ! BOUCLE SUR LES TETRA CONTENANT LE POINT J
              NT=NODES3(N)   ! TETRA DE NUMERO LOCAL NT
!         ON VA TRIER LES CAS NON PATHOLOGIQUES ET TRES COURANT DE TETRA
!         DONT UNE FACE COINCIDE AVEC CE TRIANGLE
              IF (TETTRI2(NT)>0) THEN   !TETRA CONCERNE PAR UN TRIA
	        NIT=4*(NT-1)+1
		NFT=NIT+TETTRI2(NT)-1
		DO MT=NIT,NFT           ! BOUCLE SUR LES TRIA DU TETRA
                  NUMTRI=TETTRI(MT)     ! NUM LOCAL DU TRIA
                  NUMTRIB=3*(NUMTRI-1)+1
                  IKLE1=IKLESTRI(NUMTRIB) ! NUMERO GLOBAUX DES NOEUDS DU TRIA
                  IKLE2=IKLESTRI(NUMTRIB+1)
                  IKLE3=IKLESTRI(NUMTRIB+2)
!                 CE POINT J APPARTIENT DEJA A UN TRIA ACOLLE AU TETRA
!                 ON SAUTE LE TETRA NT
		  IF ((IKLE1==J).OR.(IKLE2==J).OR.(IKLE3==J)) THEN
! POUR TESTS
!                    WRITE(LU,*)'JE SAUTE LE TETRA ',NT,EPART(NT),
!     &                          TETTRI2(NT),' NODES ',J
                    GOTO 21
                  ENDIF		  
		ENDDO
	      ENDIF            ! FIN SI TETTRI
!             LE TETRA NT EST POTENTIELLEMENT OUBLIE, ON LE TRAITE AU CAS OU
!             LE PARTAGE SE FERA DANS ESTEL3D/READ_CONNECTIVITY
	      NUMTETB=4*(NT-1)+1
	      DO L=1,4
                IKLE1=IKLESTET(NUMTETB+L-1) ! NUMERO GLOBAUX DES NOEUDS DU TETRA
		IF (IKLE1==J) THEN
                  TETCOLOR(NT,L)=(TETCOLOR(NT,L).OR..TRUE.)
                  NBRETOUCHE=NBRETOUCHE+1
                ENDIF
	      ENDDO  ! EN L
   21        CONTINUE
	    ENDDO	          ! FIN BOUCLE SUR LES TETRAS	    		    
	  ENDIF            ! FIN SI LINTER   
	ENDIF             ! FIN SI SUR NCOLOR2
      ENDDO              ! FIN BOUCLE SUR LES POINTS DE BORD     
! OB F
      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(8),COUNT_RATE=PARSEC)
      WRITE(LU,*)' NOMBRE DE RETOUCHE DU PARTITIONNEMENT (PART2): ',
     &           NBRETOUCHE
      WRITE(LU,*)' TEMPS DE RETOUCHE DU PARTITIONNEMENT PART1/PART2',
     &            (1.0*(TEMPS_SC(7)-TEMPS_SC(6)))/(1.0*PARSEC),'/',
     &           (1.0*(TEMPS_SC(8)-TEMPS_SC(7)))/(1.0*PARSEC),' SECONDS'
C$$$      WRITE(LU,*)'IDEM VERSION DE REFERENCE'

! 5C. REMPLISSAGE EFFECTIF DU UNV PAR SD
!---------------
      IBID = 1
!
      ALLOCATE(DEJA_TROUVE(NELIN),STAT=IERR)
      IF (IERR.NE.0) CALL ALLOER(LU,'DEJA_TROUVE')
      DEJA_TROUVE(:)=.FALSE.
!      
      DO IDD=1,NPARTS  ! BOUCLE SUR LES SOUS-DOMAINES

! NOMBRE DE TRIANGLES POUR CE SOUS-DOMAINE
        NBTRIIDD=0
! NOM DU FICHIER UNV PAR SOUS-DOMAINE
        NAMEINP2(I_LENINP+1:I_LENINP+11) = EXTENS(NPARTS-1,IDD-1)
        OPEN(NINP2,FILE=NAMEINP2,STATUS='UNKNOWN',FORM='FORMATTED',
     &       ERR=132)
        REWIND(NINP2)

! NOM DU FICHIER LOG PAR SOUS-DOMAINE
        NAMELOG2(I_LENLOG+1:I_LENLOG+11) = EXTENS(NPARTS-1,IDD-1)
        OPEN(NLOG2,FILE=NAMELOG2,STATUS='UNKNOWN',FORM='FORMATTED',
     &       ERR=133)
        REWIND(NLOG2)

! TITRE (UNV PAR SD)
        WRITE(NINP2,60,ERR=112)BLANC,MOINS1
        WRITE(NINP2,61,ERR=112)NSEC1
        WRITE(NINP2,62,ERR=112)TITRE
        TITRE = ' ' 
        WRITE(NINP2,62,ERR=112)TITRE
        WRITE(NINP2,62,ERR=112)TITRE
        WRITE(NINP2,62,ERR=112)TITRE
        WRITE(NINP2,62,ERR=112)TITRE
        WRITE(NINP2,62,ERR=112)TITRE
        WRITE(NINP2,62,ERR=112)TITRE
        WRITE(NINP2,60,ERR=112)BLANC,MOINS1
!
! BLOC SUR LES COORDONNEES/COULEURS DES NOEUDS (UNV PAR SD)
        WRITE(NINP2,60,ERR=112)BLANC,MOINS1
        WRITE(NINP2,61,ERR=112)NSEC2
        COMPT=1
        NODES4(:)=-1
CD      NEW VERSION OF THE LOOP TO REDUCE THE COMPUTING TIME
        DO POS_NODE=1,NPOIN_P(IDD) ! BOUCLE SUR TOUS LES POINTS DU MAILLAGES
           J=NODEGL(POS_NODE,IDD)
CD       PREVIOUS VERSION OF THE LOOP
CD       NI=NODES2(J)
CD       NF=NI+NODES1(J)-1
CD       DO N=NI,NF       ! BOUCLE SUR LES TETRA CONTENANT LE POINT J
CD           NT=NODES3(N)   ! TETRA DE NUMERO LOCAL NT 
CD           IF (EPART(NT)==IDD) THEN     ! C'EST UNE MAILLE DU SOUS-DOMAINE
              WRITE(NINP2,63,ERR=112)COMPT,IBID,IBID,NCOLOR2(J)
              WRITE(NINP2,64,ERR=112)X1(J),Y1(J),Z1(J)
              NODES4(J)=COMPT   ! LE NOEUD J A LE NUMERO COMPT
                                ! POUR LE SOUS-DOMAINE IDD
! POUR PARALLELISME TELEMAC
              KNOLG(COMPT)=J ! CONVERSION SD (LOCAL)-->MAILLAGE ENTIER (GLOBAL)
              K=NACHB(1,J)   ! NBRE DE SD CONTENANT LE NOEUD J
              NACHBLOG=.TRUE.       
              DO L=1,K     ! NOEUD DEJA CONCERNE PAR CE SD ?
                IF (NACHB(1+L,J)==IDD) NACHBLOG=.FALSE.  ! OUI      
              ENDDO
              IF (NACHBLOG) THEN                         ! NON
                K=NACHB(1,J)+1
                IF (K.GT.NBSDOMVOIS-2) GOTO 151
                NACHB(K+1,J)=IDD  ! NOEUD GLOBAL J CONCERNE PAR IDD
                NACHB(1,J)=K      ! SA MULTIPLICITE
              ENDIF 
              COMPT=COMPT+1
!              GOTO 10 ! ON PASSE AU NOEUD SUIVANT           
!            ENDIF  ! EN EPART
!          ENDDO ! EN N
!   10     CONTINUE
        ENDDO   ! EN J
! POUR TESTS
!      DO I=1,NPOINT
!        WRITE(LU,*)'GLOBAL NUMERO POINT: ',I,' LOCAL: ',NODES4(I)
!      ENDDO
        NPOINTSD(IDD)=COMPT-1  ! NOMBRE DE NOEUDS DU SOUS-DOMAINE IDD
        WRITE(NINP2,60,ERR=112)BLANC,MOINS1

! BLOC SUR LES CONNECTIVITES/COULEURS DES MAILLES (UNV PAR SD)
        WRITE(NINP2,60,ERR=112)BLANC,MOINS1
        WRITE(NINP2,61,ERR=112)NSEC3
        COMPT=1
        IBID = 1
CD      PREVIOUS VERSION OF THE LOOP
CD      DO J=1,NELEMTOTAL
CD      IF (TYPELEM(J,1)==111) THEN ! C'EST UN TETRAEDRE
CD        NUMTET=TYPELEM(J,2) ! NUM LOCAL DU TETRA DANS LA LISTE DES TETRAS
CD            IF (EPART(NUMTET)==IDD) THEN 
        DO POS=1,NELEM_P(IDD)
                                ! BOUCLE SUR TETRA ET TRIA POUR ECOLOR
           J=ELEGL(POS,IDD)
           NUMTET=TYPELEM(J,2)  ! NUM LOCAL DU TETRA DANS LA LISTE DES TETRAS 
           ELEM=111
C OB D
! PRETRAITEMENT POUR LES EVENTUELS PB DE COULEURS DES NOEUDS DE TETRAS
! A L'INTERFACE
              IBIDC=0
	      IF (TETCOLOR(NUMTET,1)) IBIDC=IBIDC+1000
	      IF (TETCOLOR(NUMTET,2)) IBIDC=IBIDC+ 200
	      IF (TETCOLOR(NUMTET,3)) IBIDC=IBIDC+  30
	      IF (TETCOLOR(NUMTET,4)) IBIDC=IBIDC+   4
! POUR MONITORING
!              IF (IBIDC/=0) WRITE(6,*)'IDD',IDD,'PARTEL',J,COMPT,IBIDC
! IDEM VERSION DE REFERENCE
!	      IBIDC=0
! OB F
              WRITE(NINP2,65,ERR=112)COMPT,ELEM,-IBIDC,IBID,ECOLOR(J),4
              COMPT=COMPT+1
              N=4*(NUMTET-1)+1
              IKLE1=NODES4(IKLESTET(N))
              IKLE2=NODES4(IKLESTET(N+1))
              IKLE3=NODES4(IKLESTET(N+2))
              IKLE4=NODES4(IKLESTET(N+3))
              WRITE(NINP2,66,ERR=112)IKLE1,IKLE2,IKLE3,IKLE4
       IF ((IKLE1.LT.0).OR.(IKLE2.LT.0).OR.(IKLE3.LT.0).OR.(IKLE4.LT.0))
     &          GOTO 147
              IF (TETTRI2(NUMTET).NE.0) THEN
                NI=4*(NUMTET-1)+1
                NF=NI+TETTRI2(NUMTET)-1
                DO M=NI,NF   ! ON PARCOURT LES TRIANGLES DE BORD ASSOCIES
                  NUMTRI=TETTRI(M)  ! AU NUMTET TETRAEDRE; NUM LOCAL DU TRIA
                  NUMTRIG=CONVTRI(NUMTRI)  ! NUMERO GLOBAL DU TRIANGLE
                  ELEM=91
                  TRIUNV(4*NBTRIIDD+1)=ECOLOR(NUMTRIG)
                  N=3*(NUMTRI-1)+1
                  IKLE1=NODES4(IKLESTRI(N))
                  IKLE2=NODES4(IKLESTRI(N+1))
                  IKLE3=NODES4(IKLESTRI(N+2))
                  TRIUNV(4*NBTRIIDD+2)=IKLE1
                  TRIUNV(4*NBTRIIDD+3)=IKLE2
                  TRIUNV(4*NBTRIIDD+4)=IKLE3
                  NBTRIIDD=NBTRIIDD+1
!
              IF ((IKLE1.LT.0).OR.(IKLE2.LT.0).OR.(IKLE3.LT.0)) GOTO 147
!
                ENDDO  ! EN M
              ENDIF  ! EN TETTRI2
        !    ENDIF  ! EN EPART
      !    ENDIF  ! EN TYPELEM
        ENDDO ! EN J

! MAINTENANT ON PEUX RECOPIER LE BLOC DES TRIANGLES !
        ELEM=91
        DO J=1,NBTRIIDD
          WRITE(NINP2,65,ERR=112)COMPT,ELEM,IBID,IBID,
     &                           TRIUNV(4*(J-1)+1),3
          IKLE1=TRIUNV(4*(J-1)+2)
          IKLE2=TRIUNV(4*(J-1)+3)
          IKLE3=TRIUNV(4*(J-1)+4)
          WRITE(NINP2,67,ERR=112)IKLE1,IKLE2,IKLE3        
          COMPT=COMPT+1
        ENDDO  ! EN J
!
        ELEM=91
! BOUCLE SURDIMENSIONNEE, ON BOUCLE SUR LE NOMBRE DE SURFACE INTERNE DU MAILLAGE GLOBAL...                                
        DO J=1,NELIN
           IF (DEJA_TROUVE(J)) CYCLE
           IKLE1=NODES4(IKLEIN(J,2))
           IKLE2=NODES4(IKLEIN(J,3))
           IKLE3=NODES4(IKLEIN(J,4))
           IF ((IKLE1.EQ.-1).OR.(IKLE2.EQ.-1).OR.(IKLE3.EQ.-1)) CYCLE
           WRITE(NINP2,65,ERR=112) COMPT,ELEM,IBID,IBID,IKLEIN(J,1),3
           WRITE(NINP2,67,ERR=112) IKLE1,IKLE2,IKLE3
           COMPT = COMPT+1
           DEJA_TROUVE(J) = .TRUE.
        ENDDO ! EN J
!
C$$$        WRITE(LU,*) 'SUBDOMAIN',IDD,'INNERTRI',COMPT
!        
        WRITE(NINP2,60,ERR=112)BLANC,MOINS1
!        WRITE(NINP2,60,ERR=112)BLANC,MOINS1
!        WRITE(NINP2,61,ERR=112)NSEC4
!        WRITE(NINP2,68,ERR=112) 1,0,0,0,0,0,0,0
        CLOSE(NINP2)
        NELEMSD(IDD)=COMPT-1  ! NOMBRE DE MAILLES DU SOUS-DOMAINE IDD

! 5D. REMPLISSAGE EFFECTIF DU LOG PAR SD
!---------------
! ELEMENT STANDARD DU FICHIER LOG (LOG PAR SD)
        WRITE(NLOG2,51 ,ERR=113) NPOINTSD(IDD)      
        WRITE(NLOG2,52 ,ERR=113) NELEMSD(IDD)
        WRITE(NLOG2,523,ERR=113) SIZE_FLUX
        WRITE(NLOG2,53 ,ERR=113) NBFAMILY-1
        DO J=1,NBFAMILY
          WRITE(NLOG2,50,ERR=113)LOGFAMILY(J)
        ENDDO

        ! ADDITION BY JP RENAUD ON 15/02/2007
        ! AS THE LIST OF PRIORITIES HAS MOVED IN ESTEL-3D FROM
        ! THE STEERING FILE TO THE LOG FILE, WE NEED TO WRITE "A"
        ! NUMBER OF EXTERNAL FACES + PRIORITY LIST HERE. AS THESE
        ! ARE NOT USED IN PARALLEL MODE, WE MERELY COPY THE LIST
        ! FROM THE ORIGINAL LOG FILE.

        WRITE(NLOG2,531,ERR=113) NBCOLOR
        WRITE(UNIT=THEFORMAT,FMT=1000) NBCOLOR
1000    FORMAT('(''PRIORITY :'',',I3,'(X,I3,))')
        THEFORMAT=TRIM(THEFORMAT)
!        WRITE(LU,*) 'FORMATT =',THEFORMAT
        WRITE (NLOG2,FMT=THEFORMAT(1:LEN(THEFORMAT)-1))
     &  (PRIORITY(I), I=1, NBCOLOR)

        ! END ADDITION BY JP RENAUD

! KNOLG (LOG PAR SD)
        NT=NPOINTSD(IDD)
        NI=NT/6
        NF=NT-6*NI
        WRITE(NLOG2,54,ERR=113)NI,NF
        DO J=1,NI
          WRITE(NLOG2,540,ERR=113)(KNOLG(6*(J-1)+K),K=1,6)
        ENDDO
        IF (NF.EQ.1) THEN
          WRITE(NLOG2,541,ERR=113)KNOLG(6*NI+1)
        ELSE IF (NF.EQ.2) THEN
          WRITE(NLOG2,542,ERR=113)(KNOLG(6*NI+K),K=1,2)
        ELSE IF (NF.EQ.3) THEN
          WRITE(NLOG2,543,ERR=113)(KNOLG(6*NI+K),K=1,3)
        ELSE IF (NF.EQ.4) THEN
          WRITE(NLOG2,544,ERR=113)(KNOLG(6*NI+K),K=1,4)
        ELSE IF (NF.EQ.5) THEN
          WRITE(NLOG2,545,ERR=113)(KNOLG(6*NI+K),K=1,5)
        ENDIF
        WRITE(NLOG2,55,ERR=113)NPOINT  ! NOMBRE DE NOEUD DU MAILLAGE
                    ! INITIAL POUR ALLOCATION KNOGL DANS ESTEL
!
      ENDDO  ! BOUCLE SUR LES SOUS-DOMAINES

! 5E. TRAVAUX SUPPLEMENTAIRES POUR DETERMINER LE NACHB AVANT DE L'ECRIRE
!      DANS LE LOG
!---------------
      DO IDD=1,NPARTS  ! BOUCLE SUR LES SOUS-DOMAINES
! CONSTRUCTION ET DIMENSIONNEMENT DU NACHB PROPRE A CHAQUE SD
        COMPT=0
        NACHB(NBSDOMVOIS,:)=-1
        DO J=1,NPOINT      ! BOUCLE SUR TOUS LES POINTS DU MAILLAGE
          N=NACHB(1,J)
          IF (N>1) THEN    ! POINT D'INTERFACE
            N=N+1
            DO K=2,N
              IF (NACHB(K,J)==IDD) THEN ! IL CONCERNE IDD
                COMPT=COMPT+1   ! "COMPT"IEME POINT D'INTERFACE DE IDD
                NACHB(NBSDOMVOIS,J)=COMPT  ! A RETENIR COMME POINT D'INTERFACE
              ENDIF
            ENDDO            ! FIN BOUCLE SUR LES SD DU POINT J
          ENDIF   
        ENDDO              ! FIN BOUCLE POINTS
        NPOINTISD(IDD)=COMPT ! NOMBRE DE POINTS D'INTERFACE DE IDD

! 5F. ON CONTINUE L'ECRITURE DU .LOG
!-------------
        NAMELOG2(I_LENLOG+1:I_LENLOG+11) = EXTENS(NPARTS-1,IDD-1)
        OPEN(NLOG2,FILE=NAMELOG2,STATUS='OLD',FORM='FORMATTED',
     &       POSITION='APPEND',ERR=133)
        WRITE(NLOG2,56,ERR=113) NPOINTISD(IDD)
        DO J=1,NPOINT
          IF (NACHB(NBSDOMVOIS,J)>0) THEN  ! C'EST UN POINT D'INTERFACE DE IDD
            COMPT=0
            VECTNB(:)=-1
            DO K=1,NBSDOMVOIS-2    ! ON PREPARE L'INFO POUR LE NACHB TELEMAC
              IF (NACHB(K+1,J)/= IDD) THEN
                COMPT=COMPT+1
! ATTENTION A CELUI-CI, SUREMENT LIE AU NUMERO DE POINTS...
C OB D
                IF (COMPT.GT.NBSDOMVOIS-3) GOTO 152
C OB F
                IF (NACHB(K+1,J)>0) THEN
! ON STOCKE LE NUMERO DE PROC ET NON LE NUMERO DE SOUS-DOMAINE
! D'OU LA CONTRAINTE, UN PROC PAR SOUS-DOMAINE
                  VECTNB(COMPT)=NACHB(K+1,J)-1
                ENDIF
              ENDIF 
            ENDDO  ! EN K
!            WRITE(NLOG2,561,ERR=113)J,(VECTNB(K),K=1,NBSDOMVOIS-3)
             NT = NBSDOMVOIS-3	     
             NI=NT/6
             NF=NT-6*NI+1
	     WRITE(NLOG2,640,ERR=113)J,(VECTNB(K),K=1,5)
	     DO L=2,NI
	       WRITE(NLOG2,640,ERR=113)(VECTNB(6*(L-1)+K),K=0,5)
	     ENDDO
	     IF (NF.EQ.1) THEN
	       WRITE(NLOG2,641,ERR=113)VECTNB(6*NI)
	     ELSE IF (NF.EQ.2) THEN
               WRITE(NLOG2,642,ERR=113)(VECTNB(6*NI+K),K=0,1)
	     ELSE IF (NF.EQ.3) THEN
	       WRITE(NLOG2,643,ERR=113)(VECTNB(6*NI+K),K=0,2)
	     ELSE IF (NF.EQ.4) THEN
	       WRITE(NLOG2,644,ERR=113)(VECTNB(6*NI+K),K=0,3)
	     ELSE IF (NF.EQ.5) THEN
               WRITE(NLOG2,645,ERR=113)(VECTNB(6*NI+K),K=0,4)
             ENDIF
          ENDIF
        ENDDO  ! FIN BOUCLE EN J
        WRITE(NLOG2,57,ERR=113)        
        CLOSE(NLOG2)    
      ENDDO  ! BOUCLE SUR LES SOUS-DOMAINES
      CALL SYSTEM_CLOCK(COUNT=TEMPS_SC(9),COUNT_RATE=PARSEC)
      WRITE(LU,*)' REMPLISSAGE DES FICHIERS UNV ET LOG',
     &           (1.0*(TEMPS_SC(9)-TEMPS_SC(8)))/(1.0*PARSEC),' SECONDS'
!----------------------------------------------------------------------
! 6. AFFICHAGES DANS PARTEL.LOG ET TEST DE COMPLETUDE DU PARTITIONNEMENT
!------------
 
      WRITE(LU,*)' '
      COMPT1=0
      COMPT2=0
      COMPT3=0
      DO IDD=1,NPARTS
        WRITE(LU,86)IDD,NELEMSD(IDD),NPOINTSD(IDD),NPOINTISD(IDD)
        COMPT3=COMPT3+NPOINTISD(IDD)
        COMPT2=COMPT2+NPOINTSD(IDD)
        COMPT1=COMPT1+NELEMSD(IDD)
      ENDDO
      WRITE(LU,*)' ------------------------------------'
      WRITE(LU,87)COMPT1,COMPT2,COMPT3
      WRITE(LU,88)COMPT1/NPARTS,COMPT2/NPARTS,COMPT3/NPARTS
      WRITE(LU,*)' '
      WRITE(LU,83)(1.0*(TEMPS_SC(9)-TEMPS_SC(1)))/(1.0*PARSEC)
      WRITE(LU,*)' ENDING METIS MESH PARTITIONING--------------------+'
      WRITE(LU,*)' '
      WRITE(LU,*)' WRITING GEOMETRY FILE FOR EACH PROCESSOR'
      WRITE(LU,*)' WRITING LOG FILE FOR EACH PROCESSOR'

!----------------------------------------------------------------------
! 7. DIVERS
!---------------

! 7.A FORMAT DU LOG
!---------------
   50 FORMAT(A80)         ! LES AUTRES LIGNES
!             1234567890123456789012345678901234567890123456789
   51 FORMAT(' TOTAL NO. OF NODES                   :    ',I10)
   52 FORMAT(' TOTAL NO. OF ELEMENTS                :    ',I10)
  523 FORMAT(' TOTAL NO. OF USER-FLUX               :    ',I10)
   53 FORMAT(' TOTAL NO. OF FAMILIES                :    ',I10)
  531 FORMAT(' TOTAL NUMBER OF EXTERNAL FACES       :    ',I10)
   54 FORMAT(' DEBUT DE KNOLG: ',I10,' ',I10)

 5401 FORMAT(6I5)              ! PRIORITY
 5411 FORMAT(I5)               ! 
 5421 FORMAT(2I5)              ! 
 5431 FORMAT(3I5)              ! 
 5441 FORMAT(4I5)              ! 
 5451 FORMAT(5I5)              ! 

  540 FORMAT(6I10)        ! LIGNE DE BLOC KNOLG ET PRIORITY
  541 FORMAT(I10)         ! DERNIERE LIGNE DE BLOC KNOLG
  542 FORMAT(2I10)        ! DERNIERE LIGNE DE BLOC KNOLG
  543 FORMAT(3I10)        ! DERNIERE LIGNE DE BLOC KNOLG
  544 FORMAT(4I10)        ! DERNIERE LIGNE DE BLOC KNOLG
  545 FORMAT(5I10)        ! DERNIERE LIGNE DE BLOC KNOLG

  641 FORMAT(I7)         ! DERNIERE LIGNE DE BLOC NACHB
  642 FORMAT(2I7)        ! DERNIERE LIGNE DE BLOC NACHB
  643 FORMAT(3I7)        ! DERNIERE LIGNE DE BLOC NACHB
  644 FORMAT(4I7)        ! DERNIERE LIGNE DE BLOC NACHB
  645 FORMAT(5I7)        ! DERNIERE LIGNE DE BLOC NACHB
  640 FORMAT(6I7)        ! DERNIERE LIGNE DE BLOC NACHB


  
   55 FORMAT(' FIN DE KNOLG: ',I10)
   56 FORMAT(' DEBUT DE NACHB: ',I10)
  561 FORMAT(10I10)        ! LIGNE DE BLOC NACHB
   57 FORMAT(' FIN DE NACHB: ')

! 7B. FORMAT DU UNV
!---------------
   60 FORMAT(A4,A2)       ! '    -1'   
   61 FORMAT(I9)          ! LECTURE NSEC
   62 FORMAT(A80)         ! LECTURE TITRE      
   63 FORMAT(4I10)        ! LIGNE 1 BLOC COORD      
   64 FORMAT(3D25.16)     ! LIGNE 2 BLOC COORD      
   65 FORMAT(6I10)        ! LIGNE 1 BLOC CONNECTIVITE      
   66 FORMAT(4I10)        ! LIGNE 2 BLOC CONNECTIVITE SI TETRA      
   67 FORMAT(3I10)        ! LIGNE 2 BLOC CONNECTIVITE SI TRIANGLE
   68 FORMAT(8I10)        ! BLOC FANTOCHE POUR MARQUER LA FIN DU BLOC
                          ! CONNECTIVITEE
      
! 7.C AFFICHAGES DANS PARTEL.LOG
!---------------
   80 FORMAT(' #NUMBER TOTAL OF ELEMENTS: ',I8,
     &       ' #NODES                 : ',I8)
   81 FORMAT(' #TETRAHEDRONS            : ',I8,
     &       ' #TRIANGLE MESH BORDER  : ',I8)
   82 FORMAT(' #EDGECUTS                : ',I8,
     &       ' #NPARTS                : ',I8)   
   83 FORMAT('  RUNTIME                 : ',F10.2,' S')
   86 FORMAT('  DOMAIN: ',I3,' #ELEMENTS:   ',I8,' #NODES:   ',I8,
     &       ' #INTERFACENODES:   ',I8)
   87 FORMAT('  TOTAL VALUES OF ELEMENTS: ',I10,'  NODES: ',I10,
     &       '  INTERFACENODES: ',I10)
   88 FORMAT('  MEAN VALUES OF ELEMENTS :   ',I8,'  NODES:   ',I8,
     &       '  INTERFACENODES:   ',I8)
   89 FORMAT('  INPUT UNV FILE      :',A50)
   90 FORMAT('  INPUT LOG FILE      :',A50)
   91 FORMAT('  NUMBER OF PARTITIONS:',I5)
   92 FORMAT('  NUMBER OF NODES:',I10)
   93 FORMAT('  NUMBER OF ELEMENTS:',I10)
   94 FORMAT('  NUMBER OF COLORS:',I5)

! 7.D DEALLOCATE
!---------------
      DEALLOCATE(X1,Y1,Z1)
      DEALLOCATE(NCOLOR,ECOLOR)
      DEALLOCATE(IKLESTET,IKLESTRI,TYPELEM,CONVTRI,TETTRI,TETTRI2)
      DEALLOCATE(EPART,NPART)
      DEALLOCATE(NELEMSD,NPOINTSD,NPOINTISD)
      DEALLOCATE(NODES1,NODES2,NODES3,NODES4,TRIUNV)
      DEALLOCATE(NODES1T,NODES2T,NODES3T)
      DEALLOCATE(KNOLG,NACHB,PRIORITY,NCOLOR2)
      DEALLOCATE(ELEGL)
      DEALLOCATE(NODEGL)
      DEALLOCATE(NODELG)
      DEALLOCATE(NELEM_P)
      DEALLOCATE(NPOIN_P)
      RETURN

! 7.E MESSAGES D'ERREURS
!---------------
  110 TEXTERROR='! UNEXPECTED FILE FORMAT1: '//NAMELOG//' !'
      GOTO 999
  111 TEXTERROR='! UNEXPECTED FILE FORMAT2: '//NAMEINP//' !'
      GOTO 999
  112 TEXTERROR='! UNEXPECTED FILE FORMAT3: '//NAMEINP2//' !'
      GOTO 999
  113 TEXTERROR='! UNEXPECTED FILE FORMAT4: '//NAMELOG2//' !'
      GOTO 999
  120 TEXTERROR='! UNEXPECTED EOF WHILE READING: '//NAMELOG//' !'
      GOTO 999
  121 TEXTERROR='! UNEXPECTED EOF WHILE READING: '//NAMEINP//' !'
      GOTO 999
  130 TEXTERROR='! PROBLEM WHILE OPENING: '//NAMELOG//' !'
      GOTO 999
  131 TEXTERROR='! PROBLEM WHILE OPENING: '//NAMEINP//' !'
      GOTO 999
  132 TEXTERROR='! PROBLEM WHILE OPENING: '//NAMEINP2//' !'
      GOTO 999
  133 TEXTERROR='! PROBLEM WHILE OPENING: '//NAMELOG2//' !'
      GOTO 999
  140 TEXTERROR='! FILE DOES NOT EXIST: '//NAMEINP//' !'
      GOTO 999
  141 TEXTERROR='! FILE DOES NOT EXIST: '//NAMELOG//' !'
      GOTO 999
  142 TEXTERROR='! UNKNOWN TYPE OF ELEMENT IN THE MESH !'
      GOTO 999
  143 DO J = 1,NELEMTOTAL
        IF (TYPELEM(J,2)==I) WRITE(UNIT=STR8,FMT='(I8)')J
      ENDDO
      WRITE(UNIT=STR26,FMT='(I8,1X,I8,1X,I8)')IKLE1,IKLE2,IKLE3
      TEXTERROR='! BORDER SURFACE OF NUMBER '//STR8//' AND OF NODES '//
     &          STR26//' NOT LINK TO A TETRAHEDRON !'
      GOTO 999
  144 WRITE(UNIT=STR8,FMT='(I8)')MAXLENSOFT
      TEXTERROR='! NAME OF INPUT FILE '//NAMEINP//' IS LONGER THAN '//
     &           STR8(1:3)//' CHARACTERS !'
      GOTO 999
  145 WRITE(UNIT=STR8,FMT='(I8)')MAXLENSOFT
      TEXTERROR='! NAME OF INPUT FILE '//NAMELOG//' IS LONGER THAN '//
     &           STR8(1:3)//' CHARACTERS !'
      GOTO 999
  146 TEXTERROR='! PROBLEM WITH CONSTRUCTION OF INVERSE CONNECTIVITY !'
      GOTO 999
  147 TEXTERROR='! PROBLEM WHILE WRITING: '//NAMEINP2//' !'
      GOTO 999
  148 TEXTERROR='! SEVERAL ELEMENTS MAY BE FORGOTTEN BY PARTITIONNING !' 
      GOTO 999
  149 TEXTERROR='! NO INPUT UNV FILE !' 
      GOTO 999
  150 TEXTERROR='! NO INPUT LOG FILE !' 
      GOTO 999
  151 WRITE(UNIT=STR8,FMT='(I8)')J
      WRITE(UNIT=STR26,FMT='(I3,1X,I3,1X,I3,1X,I3,1X,I3,1X,I3)')
     &                 (NACHB(K,J),K=2,NBSDOMVOIS-1),IDD
      TEXTERROR='! NODE '//STR8//' BELONGS TO DOMAINS '//STR26(1:23)
     &                 //' !' 
      GOTO 999
  152 TEXTERROR='! PROBLEM WITH CONSTRUCTION OF VECTNB FOR NACHB !' 
      GOTO 999
  153 WRITE(UNIT=STR8,FMT='(I8)') CONVTET(J)
      TEXTERROR='! TETRAHEDRON '//STR8//
     &          ' LINKS TO SEVERAL BORDER TRIANGLES !'
      GOTO 999
  154 TEXTERROR='! PROBLEM WITH THE PRIORITY OF COLOR NODES !'
      GOTO 999
! END OF FILE AND FORMAT ERRORS :
 1100 TEXTERROR='ERREUR DE LECTURE DU FICHIER UNV '//
     &  'VIA MESH_CONNECTIVITY'
      GOTO 999
 1200 TEXTERROR='ERREUR DE FIN DE LECTURE DU FICHIER UNV '//
     &  'VIA MESH_CONNECTIVITY'
      GOTO 999

  999 CALL ALLOER2(LU,TEXTERROR)
!
      END SUBROUTINE PARES3D
!
!                       *************************
                        SUBROUTINE ELEBD31_PARTEL
!                       *************************
!
     *(NELBOR,NULONE,IKLBOR,IFABOR,NBOR,IKLE,
     * NBTET,NBTRI,NELMAX,NPOINT,NPTFR,IPOBO)     
!    
!***********************************************************************
! BIEF VERSION 5.5           09/04/04    J-M HERVOUET (LNH) 30 87 80 18
! 
!***********************************************************************
!              
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    NELBOR      |<-- | NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT|
! |    NULONE      |<-- | NUMERO LOCAL D'UN POINT DE BORD DANS         |
! |                |    | L'ELEMENT ADJACENT DONNE PAR NELBOR          
! |    IKLBOR      |<-- | NUMERO LOCAL DES NOEUDS A PARTIR D'UN ELEMENT
! |                |    |  DE BORD
! |    IFABOR      | -->| TABLEAU DES VOISINS DES FACES.
! |    NBOR        | -->| NUMERO GLOBAL D'UN NOEUD A PARTIR DU NÃÃÂÂÃÃÂÃÂÃÂÃÂÃÂÂ° LOCAL
! |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
! |    NBTET       | -->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
! |    NPOINT      | -->| NOMBRE TOTAL DE POINTS DU DOMAINE.
! |    NPTFR       | -->| NOMBRE DE POINTS FRONTIERES.
C |    NBTRI       | -->| NOMBRE D'ELEMENTS DE BORD.
! |    31          | -->| TYPE D'ELEMENT.
! |________________|____|______________________________________________|
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE WRITTEN BY LAM MINH-PHUONG
!
!-----------------------------------------------------------------------
!
! SUBROUTINE: ELEBD31_PARTEL
!
! FUNCTION: CONSTRUCTION DE NELBOR, NULONE, IKLBORD 
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NBTET,NBTRI,NELMAX
      INTEGER, INTENT(IN)    :: NPOINT,NPTFR
      INTEGER, INTENT(IN)    :: NBOR(NPTFR)
      INTEGER, INTENT(IN)    :: IFABOR(NELMAX,4)
      INTEGER, INTENT(IN)    :: IKLE(NBTET,4)
      INTEGER, INTENT(OUT)   :: NELBOR(NBTRI),NULONE(NBTRI,3)
      INTEGER, INTENT(OUT)   :: IKLBOR(NBTRI,3),IPOBO(NPOINT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER   :: IELEM, IELEB, J,K,IPOIN
!      INTEGER   :: IPOBO(NPOINT)
!
      INTEGER SOMFAC(3,4)
      DATA SOMFAC / 1,2,3 , 4,1,2 , 2,3,4 , 3,4,1  /
!     FACE NUMERO:    1       2       3       4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      
! CREATION DE IPOBO QUI PERMET DE PASSER DU NUMERO GLOBAL AU NUMERO LOCAL
      
      DO IPOIN=1,NPOINT
        IPOBO(IPOIN) = 0
      ENDDO
      
      DO K = 1, NPTFR
         IPOBO(NBOR(K)) = K
      ENDDO
           
       
! CONSTRUCTION DE NELBOR, NULONE, IKLBORD 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IELEB = 0
      
      DO IELEM = 1,NBTET
         DO J = 1,4
            IF (IFABOR(IELEM,J)== 0) THEN
               IELEB           = IELEB +1
               IF ( IELEB .GT. NBTRI ) THEN
                 IF(LNG.EQ.1) WRITE(LU,101)
                 IF(LNG.EQ.2) WRITE(LU,102)
101              FORMAT(1X,'ELEBD31_PARTEL : ERREUR DANS LE MAILLAGE ')
102              FORMAT(1X,'ELEBD31_PARTEL : ERROR IN MESH. BYE.')
                 CALL PLANTE2(1)
                 STOP
               END IF
               NELBOR(IELEB)   = IELEM
               NULONE(IELEB,1) = SOMFAC(1,J)
               NULONE(IELEB,2) = SOMFAC(2,J)
               NULONE(IELEB,3) = SOMFAC(3,J)
               IKLBOR(IELEB,1) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(1,J)))
               IKLBOR(IELEB,2) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(2,J)))
               IKLBOR(IELEB,3) = IPOBO(IKLE(NELBOR(IELEB),SOMFAC(3,J)))
               
            END IF
         END DO
      END DO
 
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ELEBD31_PARTEL   
! 
!                       **************************
                        SUBROUTINE VOISIN31_PARTEL
!                       **************************
!
     *(NBTET,NELMAX,SIZIKL,
     * NPOIN,NPTFR,IKLE,IFABOR,NBOR)
!
!***********************************************************************
! BIEF VERSION 5.6      02/03/06    REGINA NEBAUER (LNHE) 01 30 87 83 93
!
!***********************************************************************
!
!    FONCTION : CONSTRUCTION DU TABLEAU IFABOR, OU IFABOR(IELEM,IFACE)
!               EST LE NUMERO GLOBAL DU VOISIN DE LA FACE IFACE DE
!               L'ELEMENT IELEM SI CE VOISIN EXISTE ET 0 SI LA FACE EST
!               SUR LA FRONTIERE DU DOMAINE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    IFABOR      |<-- | TABLEAU DES VOISINS DES FACES.
! |                |    | (CAS DES MAILLAGES ADAPTATIFS)
! |    IELM        | -->| 31: TETRAEDRES NON STRUCTURES
! |    NBTET       | -->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
! |    NELMAX      | -->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
! |    NPOIN       | -->| NOMBRE TOTAL DE POINTS DU DOMAINE
! |    NPTFR       | -->| NOMBRE DE POINTS DE BORD
! |    IKLE        | -->| TABLE DE CONNECTIVITE DOMAINE
! |    SIZIKLE     | -->| ??
! |    NBOR        | -->| CORRESPONDANCE NO NOEUD DE BORD/NO GLOBAL
! |    NACHB       | -->| TABLEAU DE VOISINAGE POUR PARALLELISME
! !  IKLETR,NBTRI | -->/ CONNECTIVITE DES TRIA DE BORD POUR ESTEL3D
! |________________|____|_______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!***********************************************************************
!
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN   ) :: NBTET
      INTEGER, INTENT(IN   ) :: NELMAX
      INTEGER, INTENT(IN   ) :: SIZIKL
      INTEGER, INTENT(IN   ) :: NPOIN
      INTEGER, INTENT(IN   ) :: NPTFR
      ! NOTE : ON DONNE EXPLICITEMENT LA DEUXIEME DIMENSION DE IFABOR ET
      ! IKLE, CAR IL S'AGIT ICI TOUJOURS DE TETRAEDRES!
      INTEGER, INTENT(IN), DIMENSION(SIZIKL,4)  :: IKLE
      INTEGER, INTENT(OUT), DIMENSION(NELMAX,4) :: IFABOR
      INTEGER, INTENT(IN), DIMENSION(NPTFR) :: NBOR
!
! VARIABLES LOCALES 
!-----------------------------------------------------------------------

      ! LE TABLEAU QUI EST L'INVERSE DE NBOR (CA DONNE POUR CHAQUE NOEUD
      ! DU DOMAINE LE NUMERO DE NOEUD DE BORD, OU ZERO SI LE NOEUD EST A
      ! L'INTERIEUR DU DOMAINE.
C$$$      INTEGER, DIMENSION(NPOIN)            :: NBOR_INV
 
      ! LE TABLEAU DEFINISSANT LE NOMBRE D'ELEMENT (TETRAEDRES) VOISINS
      ! D'UN NOEUD.
      INTEGER, DIMENSION(:  ), ALLOCATABLE  :: NVOIS
      ! LE TABLEAU DEFINISSANT LES IDENTIFIANTS DES ELEMENTS VOISINS DE
      ! CHAQUE NOEUD. 
      INTEGER, DIMENSION(:  ), ALLOCATABLE :: NEIGH

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLE_TRI

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: VOIS_TRI

      ! UN TABLEAU DEFINISSANT LES ADRESSES DES DIFFERENTS ENTREES DANS
      ! LE TABLEAU NEIGH
      INTEGER, DIMENSION(:), ALLOCATABLE   :: IADR
      ! LA VALEUR D'UNE ENTREE DANS CE TABLEAU.
      INTEGER                              :: ADR

      ! LE NOMBRE MAXIMAL D'ELEMENTS VOISIN D'UN NOEUD.
      INTEGER :: NMXVOISIN
      INTEGER :: IMAX       ! DIMENSIONNEMENT TABLEAU IADR

      INTEGER :: NFACE      ! NOMBRE DE FACES DE L'ELEMENT (TETRA : 4)
      INTEGER :: NB_TRI      ! LE NOMBRE DE TRIANGLES DEFINIS

      INTEGER :: IELEM      ! COMPTEUR ELEMENTS
      INTEGER :: IELEM2     ! COMPTEUR ELEMENTS
      INTEGER :: IPOIN      ! COMPTEUR NOEUDS DOMAINE
      INTEGER :: INOEUD     ! COMPTEUR NOEUDS TETRAEDRES/TRIANGLES
      INTEGER :: IFACE      ! COMPTEUR FACE
      INTEGER :: IFACE2     ! COMPTEUR FACE
      INTEGER :: ITRI       ! COMPTEUR TRIANLGES
      INTEGER :: IVOIS      ! COMPTEUR VOISINS
      INTEGER :: NV         ! NOMBRE DE VOISINS

      INTEGER :: ERR        ! CODE D'ERREUR ALLOCATION MEMOIRE

      LOGICAL :: FOUND      ! TROUVE OU PAS ...

      INTEGER :: I1, I2, I3 ! LES TROIS NOEUDS D'UN TRIANGLE   
      INTEGER :: M1, M2, M3 ! LA MEME CHOSE EN ORDONNE.

      INTEGER :: I,J,K      ! CA SERT ...

      !????
      INTEGER :: IR1,IR2,IR3,IR4,IR5,IR6,COMPT
      LOGICAL :: BORD


!   ~~~~~~~~~~~~~~~~~~~~~~~   
!     DEFINITION DES QUATRE TRIANGLES DU TETRAEDRE : LA PREMIERE
!     DIMENSION DU TABLEAU EST LE NUMERO DU TRIANGLE, LA DEUXIEME DONNE
!     LES NUMEROS DES NOEUDS DE TETRAEDRES QUI LE DEFINISSENT.
      INTEGER SOMFAC(3,4)
      DATA SOMFAC /  1,2,3 , 4,1,2 , 2,3,4 , 3,4,1   /
!-----------------------------------------------------------------------
! DEBUT DU CODE 
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
! ETAPE 1 : COMPTAGE DU NOMBRE D'ELEMENTS VOISINS D'UN NOEUD.
!-----------------------------------------------------------------------
! CALCUL DU NOMBRE D'ELEMENTS VOISINS POUR CHAQUE NOEUD DU MAILLAGE.
! RESULTAT : NVOIS(INOEUD) DONNE LE NOMBRE D'ELEMENTS VOISINS POUR LE
! NOEUD INOEUD

      ! ON COMMENCE AVEC L'INITIALISATION A 0 DU COMPTEUR DES ELEMENTS
      ! VOISINS.

      NFACE = 4
!
      ALLOCATE(NVOIS(NPOIN),STAT=ERR)
      IF(ERR.NE.0) GOTO 999
      ALLOCATE(IADR(NPOIN),STAT=ERR)
      IF(ERR.NE.0) GOTO 999
!
      DO I = 1, NPOIN
        NVOIS(I) = 0
      ENDDO   
      ! PUIS ON COMPTE LES ELEMENTS VOISINS.
      ! EN PARCOURANT LA TABLE DE CONNECTIVITE, ON INCREMENTE LE
      ! COMPTEUR A CHAQUE FOIS QU'UN ELEMENT REFERENCE LE NOEUD IPOIN

      ! BOUCLE SUR LES 4 NOEUDS DE L'ELEMENT
      DO INOEUD = 1, 4   
        ! BOUCLE SUR LES ELEMENTS
        DO IELEM = 1,NBTET
          ! L'ID DU NOEUD I DE L'ELEMENT IELEM
          IPOIN        = IKLE( IELEM , INOEUD )
          ! INCREMENTER LE COMPTEUR.
          NVOIS(IPOIN) = NVOIS(IPOIN) + 1
        END DO
      END DO

!-----------------------------------------------------------------------
! ETAPE 2 : DETERMINATION DE LA TAILLE DU TABLEAU NEIGH() ET DE LA
! TABLE AUXILIAIRE POUR INDEXER NEIGH. ALLOCATION DE NEIGH
!-----------------------------------------------------------------------
! ON VA DANS LA SUITE CREER UN TABLEAU QUI VA CONTENIR LES IDENTIFIANT
! DES ELEMENTS VOISINS DE CHAQUE NOEUD. COMME LE NOMBRE DE VOISINS EST
! A PRIORI DIFFERENT POUR CHAQUE NOEUD, ET COMME ON NE VEUT PAS MAXIMISE
! LE TABLEAU (TROP GROS), ON A BESOIN D'UN TABLEAU AUXILIAIRE QUI VA
! NOUS DONNER L'ADRESSE DES ENTREES POUR UN NOEUD DONNEE. CE TABLEAU A
! AUTANT D'ENTREE QUE DE NOEUD.
! ET A L'OCCASION, ON VA AUSSI CALCULER LE NOMBRE MAXIMUM DE VOISINS.

      ! LA PREMIERE ENTREE DANS LE TABLEAU DES ID DES VOISINS EST 1.
      ADR       = 1
      IADR(1)   = ADR
      ! LE NOMBRE MAX D'ELEMENTS VOISINS 
      NV        = NVOIS(1)
      NMXVOISIN = NV

      DO IPOIN = 2,NPOIN
          ! CALCUL DE L'ADRESSE DES AUTRES ENTREES:
          ADR         = ADR + NV
          IADR(IPOIN) = ADR
          NV          = NVOIS(IPOIN)
          ! REPERAGE DU NOMBRE DE VOISINS MAX.
          NMXVOISIN   = MAX(NMXVOISIN,NV)
      END DO

      ! LE NOMBRE TOTAL D'ELEMENTS VOISINS POUR TOUS LES NOEUDS DONNE LA
      ! TAILLE DU TABLEAU DES VOISINS :

      IMAX = IADR(NPOIN) + NVOIS(NPOIN)

      ! ALLOCATION DE LA TABLE CONTENANT LES IDENTIFIANTS DES ELEMENTS
      ! VOISINS POUR CHAQUE NOEUD.
      ALLOCATE(NEIGH(IMAX),STAT=ERR)
      IF(ERR.NE.0) GOTO 999
!
!-----------------------------------------------------------------------
! ETAPE 3 : INITIALISATION DE NEIGH
!-----------------------------------------------------------------------
! APRES ALLOCATION DU TABLEU NEIGH, IL FAUT LE REMPLIR : DONC ON
! RECOMMENCE LA BOUCLE SUR LES QUATRE NOEUDS DE CHAQUE ELEMENT ET CETTE
! FOIS CI, ON NOTE EN MEME TEMPS L'IDENTIFIANT DANS LE TABLEAU NEIGH.
!
!
      ! REINITIALISATION DU COMPTEUR DES ELEMENTS VOISINS A 0, POUR
      ! SAVOIR OU ON EN EST.
      NVOIS(:) = 0

      ! POUR CHAQUE NOEUD DES ELEMENTS, ON RECUPERE SONT IDENTIFIANT.
      DO INOEUD = 1, 4  ! BOUCLE SUR LES NOEUDS DE L'ELEMENT
        DO IELEM=1,NBTET ! BOUCLE SUR LES ELEMENTS
          IPOIN     = IKLE( IELEM , INOEUD )
          ! ON A UN VOISIN EN PLUS.
          NV           = NVOIS(IPOIN) + 1
          NVOIS(IPOIN) = NV
          ! ON NOTE L'IDENTIFIANT DE L'ELEMENT VOISIN DANS LE TABLEAU
          NEIGH(IADR(IPOIN)+NV) = IELEM
        END DO ! FIN BOUCLE ELEMENTS
      END DO  ! FIN BOUCLE NOEUDS

!
!-----------------------------------------------------------------------
! ETAPE 4 : REPERER LES FACES COMMUNES DES TETRAEDRES ET REMPLIR LE
! TABLEAU IFABOR. 
!-----------------------------------------------------------------------
! POUR REPERER LES FACES COMMUNES AUX ELEMENTS :
! PARMIS LES ELEMENTS QUI PARTAGENT UN NOEUD, IL Y A AU MOINS
! DEUX QUI PARTAGENT UNE FACE. (S'IL NE S'AGIT PAS D'UN NOEUD DE
! BORD). 
! LE PRINCIPE DE L'ALGORITHME : EN REPERANT LES TETRAEDRES
! PARTAGENT LE NOEUD IPOIN, ON RECONSTRUIT LES TRIANGLES DE LEUR
! FACES. 
! SI ON TROUVE DEUX TRIANGLES QUI PARTAGENT LES MEMES NOEUDS, CA
! VEUT DIRE QUE LES TETRAEDRES QUI LES DEFINISSENT SONT VOISINS.
! SI ON NE TROUVE PAS DE VOISIN, CELA VEUT DIRE QUE LE TRIANGLES
! EST UNE FACE DE BORD.
! ON PART DU PRINCIPE QU'UN TRIANGLES NE PEUT ETRE DEFINIT PAR
! PLUS DE DEUX TETRAEDRES. SI C'ETAIT LE CAS, IL Y A UN PROBLEME
! DE MAILLAGE, ET CA, ON NE S'EN OCCUPE PAS ...
!
! AVANTAGES : ON ECONOMISE PAS MAL DE MEMOIRE, EN MEMORISANT QUE LES
! TRIANGLES AUTOUR D'UN NOEUD. 
! DESAVANTAGES : ON RISQUE DE FAIRE DES CALCULS EN TROP 
! (POUR ARRIVER A L'ETAPE OU ON DEFINIT LA TABLE DE CONNECTIVITE DES
! TRIANGLES)
! ON PEUT PEUT-ETRE SAUTER CETTE ETAPE EN REGARDANT SI IFABOR CONTIENT
! DEJA QQCHOSE OU PAS ...
!
! ON VA DEFINIR UNE TABLE DE CONNECTIVITE POUR LES TRIANGLES.
! CETTE TABLE DE CONNECTIVITE N'A PAS POUR BUT DE RECENSER LA
! TOTALITE DES TRIANGLES, MAIS UNIQUEMENT CEUX AUTOUR D'UN NOEUD.
! ON CONNAIT LE NOMBRE DE (TETRAEDRES) VOISINS MAXIMAL D'UN
! NOEUDS. DANS LE PIRE DES CAS, IL S'AGIT D'UN NOEUD DE BORD 
! ON VA MAXIMISER (BEAUCOUP) EN DISANT QUE LE NOMBRE MAXIMAL DE
! TRIANGELS AUTOUR D'UN NOEUD PEUT ETRE LE NOMBRE DE TETRAEDRES
! VOISINS.
! ON CREE AUSSI UN TABLEAU VOIS_TRI, QUI CONTIENT L'ID DE L'ELEMENT
! TETRAEDRE QUI L'A DEFINIT EN PREMIER (ET QUI SERA LE VOISIN DU
! TETRAEDRE QUI VA TROUVER QU'IL Y A DEJA UN AUTRE QUI LE DEFINI)
! CE TABLEAU A DEUX ENTREES : L'ID DE L'ELEMENT ET L'ID DE LA FACE.
!
      NB_TRI = NMXVOISIN * 3
!
      ALLOCATE(IKLE_TRI(NB_TRI,3),STAT=ERR)
      IF(ERR.NE.0) GOTO 999
      ALLOCATE(VOIS_TRI(NB_TRI,2),STAT=ERR)
      IF(ERR.NE.0) GOTO 999

      IFABOR(:,:) = 0
!
      ! BOUCLE SUR TOUS LES NOEUDS DU MAILLAGE.
      DO IPOIN = 1, NPOIN
          ! POUR CHAQUE NOEUD, ON REGARDE LES ELEMENTS TETRAEDRES
          ! VOISINS (PLUS PRECISEMENT : LES FACES TRIANGLES QU'IL
          ! CONSTITUE)
          ! ON REINITIALISE LA TABLE DE CONNECTIVITE DES TRIANGLES DES
          ! CES TETRAEDRES A 0 AINSI QUE LE NOMBRE DE TRIANGLES QU'ON A
          ! TROUVE :
          IKLE_TRI(:,:) = 0
          ! LA MEME CHOSE POUR LE TABLEAU QUI DIT QUEL ELEMENT A DEJA
          ! DEFINI LE TRIANGLE :
          VOIS_TRI(:,:) = 0
          ! ON RECOMMENCE A COMPTER LES TRIANGLES :
          NB_TRI         = 0
          NV            = NVOIS(IPOIN)
          ADR           = IADR(IPOIN)
          DO IVOIS = 1, NV
              ! L'IDENTIFIANT DE L'ELEMENT VOISIN NO IVOIS DU NOEUD
              ! IPOIN :
              IELEM = NEIGH(ADR+IVOIS)
              ! ON BOUCLE SUR LES QUATRE FACES DE CET ELEMENT.
              DO IFACE = 1 , NFACE
                  ! SI ON A DEJA UN VOISIN POUR CETTE FACE, ON VA PAS
                  ! PLUS LOIN ET PREND LA PROCHAINE.
                  ! SI ON N'A PAS ENCORE DE VOISIN, ON LE CHERCHE...
                  IF ( IFABOR(IELEM,IFACE) .EQ. 0 ) THEN 
                  ! CHAQUE FACE DEFINIT UN TRIANGLE. LE TRIANGLE EST
                  ! DONNE PAR TROIS NOEUDS.
                  I1 = IKLE(IELEM,SOMFAC(1,IFACE))
                  I2 = IKLE(IELEM,SOMFAC(2,IFACE))
                  I3 = IKLE(IELEM,SOMFAC(3,IFACE))
                  ! ON ORDONNE CES TROIS NOEUDS, M1 EST LE NOEUD AVEC 
                  ! L'IDENTIFIANT LE PLUS PETIT, M3 CELUI AVEC
                  ! L'IDENTIFIANT LE PLUS GRAND ET M2 EST AU MILIEU :
                  M1 = MAX(I1,(MAX(I2,I3)))
                  M3 = MIN(I1,(MIN(I2,I3)))
                  M2 = I1+I2+I3-M1-M3
                  ! ON PARCOURT LE TABLEAU DES TRIANGLES DEJA DEFINIS
                  ! POUR VOIR S'IL EN A UN QUI COMMENCE DEJA PAR M1.
                  ! SI C'EST LE CAS, ON VERIFIE S'IL A AUSSI M2 ET M3
                  ! COMME NOEUD. SI OUI, C'EST GAGNE, ON A TROUVE UN
                  ! VOISIN. SI LA RECHERCHE ECHOUE, ON CREE UN NOUVEAU
                  ! TRIANGLE.

                  FOUND = .FALSE.
                  DO ITRI = 1, NB_TRI
                      IF ( IKLE_TRI(ITRI,1) .EQ. M1 ) THEN
                          IF ( IKLE_TRI(ITRI,2) .EQ. M2 .AND.
     &                         IKLE_TRI(ITRI,3) .EQ. M3 ) THEN
                               ! ON A TROUVE ! C'EST TOUT BON.
                               ! ON RECUPERE L'INFO QU'ON A DANS
                               ! VOIS_TRI. (CAD L'ELEMENT QUI A DEJA
                               ! DEFINI LE TRIANGLE ET LA FACE)
                               IELEM2 = VOIS_TRI(ITRI,1)
                               IFACE2 = VOIS_TRI(ITRI,2)
                               IF ( IELEM2 .EQ. IELEM ) THEN
                                  IF(LNG.EQ.1) WRITE(LU,908) 31
                                  IF(LNG.EQ.2) WRITE(LU,909) 31
908                               FORMAT(1X,'VOISIN: IELM=',1I9,', 
     &                            PROBLEME DE VOISIN')
909                               FORMAT(1X,'VOISIN: IELM=',1I9,',
     &                            NEIGHBOUR PROBLEM')
                                  CALL PLANTE2(1)
                                  STOP
                               END IF
                               ! POUR ETRE SUR :
                               IF ( IELEM2 .EQ. 0 .OR.
     &                              IFACE2 .EQ. 0 ) THEN
                                IF(LNG.EQ.1) WRITE(LU,918) IELEM2,IFACE2
                                IF(LNG.EQ.2) WRITE(LU,919) IELEM2,IFACE2
918                            FORMAT(1X,'VOISIN31:TRIANGLE NON DEFINI,
     &                         IELEM=',1I9,'IFACE=',1I9)
919                            FORMAT(1X,'VOISIN31:UNDEFINED TRIANGLE,
     &                         IELEM=',1I9,'IFACE=',1I9)
                                CALL PLANTE2(1)
                                STOP
                               END IF
                               ! L'ELEMENT ET SON VOISIN : ON NOTE LA 
                               ! CORRESPONDANCE DANS IFABOR.
                               IFABOR(IELEM ,IFACE ) = IELEM2
                               IFABOR(IELEM2,IFACE2) = IELEM
                               FOUND = .TRUE.
                          END IF
                      END IF
                  END DO
                  ! ET NON, ON N'A PAS ENCORE TROUVE CE TRIANGLE, ALORS
                  ! ON EN CREE UN NOUVEAU.
                  IF ( .NOT. FOUND) THEN
                      NB_TRI             = NB_TRI + 1
                      IKLE_TRI(NB_TRI,1) = M1
                      IKLE_TRI(NB_TRI,2) = M2
                      IKLE_TRI(NB_TRI,3) = M3
                      VOIS_TRI(NB_TRI,1) = IELEM
                      VOIS_TRI(NB_TRI,2) = IFACE
                  END IF
              END IF ! IFABOR ZERO 
              END DO ! FIN BOUCLE FACES DES ELEMENTS VOISINS
!
          END DO ! FIN BOUCLE ELEMENTS VOISINS DU NOEUD
      END DO ! FIN BOUCLE NOEUDS
!
      DEALLOCATE(NEIGH)
      DEALLOCATE(IKLE_TRI)
      DEALLOCATE(VOIS_TRI)                      
!
!
!-----------------------------------------------------------------------
!  
!  DISTINCTION DANS IFABOR ENTRE LES FACES DE BORD ET LES FACES LIQUIDES
!
!  INITIALISATION DE NBOR_INV A ZERO
!
!      DO IPOIN=1,NPOIN
!        NBOR_INV(IPOIN) = 0
!      ENDDO      
!
!  PERMET DE PASSER DU NUMERO GLOBAL AU NUMERO DE BORD
!
!      DO K = 1, NPTFR
!         NBOR_INV(NBOR(K)) = K
!      ENDDO           
!
!  BOUCLE SUR TOUTES LES FACES DE TOUS LES ELEMENTS :
!
!      DO 90 IFACE = 1 , NFACE
!      DO 100 IELEM = 1 , NBTET
!
!      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
!
!      C'EST UNE VRAIE FACE DE BORD (LES FACES INTERNES EN PARALLELISME
!                                    SONT MARQUEES AVEC DES -2).
!      NUMEROS GLOBAUX DES POINTS DE LA FACE :
!
!       I1 = IKLE( IELEM , SOMFAC(1,IFACE) )
!       I2 = IKLE( IELEM , SOMFAC(2,IFACE) )
!       I3 = IKLE( IELEM , SOMFAC(3,IFACE) )
!
!      ENDIF
!
!100    CONTINUE
!90    CONTINUE
!   
!-----------------------------------------------------------------------
!  
      RETURN
!
!-----------------------------------------------------------------------
!  
999   IF(LNG.EQ.1) WRITE(LU,1000) ERR
      IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000  FORMAT(1X,'VOISIN31 : ERREUR A L''ALLOCATION DE MEMOIRE :',/,1X,
     *            'CODE D''ERREUR : ',1I9)
2000  FORMAT(1X,'VOISIN31: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     *            'ERROR CODE: ',1I9)
      CALL PLANTE2(1)
      STOP
!
!-----------------------------------------------------------------------
!  
      END
